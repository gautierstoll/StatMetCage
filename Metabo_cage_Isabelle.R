setwd("~/Git/StatMetCage")
library(tcltk)
library(tidyverse)
library(StatMetCage)
library(foreach)
library(doParallel)
source("R/RawMetaboData.R")
source("R/AnalysisMetaboData.R")
source("R/ResDailyMeanStatMetabo.R")

nbcore <- parallel::detectCores()

# load tables #####
FileList <- tk_choose.files()
# FileList <- c(
#   "~/Desktop/Last_NoNF/03112025.csv",
#   "~/Desktop/Last_NoNF/01112025.csv",
#   "~/Desktop/Last_NoNF/26102025.csv",
#   "~/Desktop/Last_NoNF/30102025.csv"
# )

# inj_time <- list(dmy_hm("27-10-2025 17:40"),dmy_hm("27-10-2025 17:40"),dmy_hm("27-10-2025 17:40"),dmy_hm("27-10-2025 17:46"),
#                  dmy_hm("27-10-2025 17:47"),dmy_hm("27-10-2025 17:47"),dmy_hm("27-10-2025 17:47"),dmy_hm("27-10-2025 17:47"),
#                  dmy_hm("31-10-2025 17:55"),dmy_hm("31-10-2025 17:55"),dmy_hm("31-10-2025 17:55"),dmy_hm("31-10-2025 17:50"),
#                  dmy_hm("31-10-2025 18:00"),dmy_hm("31-10-2025 18:00"),dmy_hm("31-10-2025 18:00"),dmy_hm("31-10-2025 17:52"),
#                  dmy_hm("02-11-2025 17:45"),dmy_hm("02-11-2025 17:45"),dmy_hm("02-11-2025 17:45"),dmy_hm("02-11-2025 17:52"),
#                  dmy_hm("02-11-2025 17:54"),dmy_hm("02-11-2025 17:54"),dmy_hm("02-11-2025 17:54"),dmy_hm("02-11-2025 17:53"),
#                  dmy_hm("04-11-2025 17:48"),dmy_hm("04-11-2025 17:48"),dmy_hm("04-11-2025 17:48"),dmy_hm("04-11-2025 17:45"),
#                  dmy_hm("04-11-2025 17:52"),dmy_hm("04-11-2025 17:52"),dmy_hm("04-11-2025 17:52"),dmy_hm("04-11-2025 17:47"))

# inj_time <- list(dmy_hm("18-10-2025 18:38"),dmy_hm("18-10-2025 18:38"),dmy_hm("18-10-2025 18:38"),dmy_hm("18-10-2025 18:44"),
#                  dmy_hm("18-10-2025 18:46"),dmy_hm("18-10-2025 18:46"),dmy_hm("18-10-2025 18:46"),dmy_hm("18-10-2025 18:45"),
#                  dmy_hm("21-10-2025 17:55"),dmy_hm("21-10-2025 17:55"),dmy_hm("21-10-2025 17:55"),dmy_hm("21-10-2025 18:02"),
#                  dmy_hm("21-10-2025 18:05"),dmy_hm("21-10-2025 18:05"),dmy_hm("21-10-2025 18:05"),dmy_hm("21-10-2025 18:04"),
#                  dmy_hm("23-10-2025 18:00"),dmy_hm("23-10-2025 18:00"),dmy_hm("23-10-2025 18:00"),dmy_hm("23-10-2025 18:05"),
#                  dmy_hm("23-10-2025 18:08"),dmy_hm("23-10-2025 18:08"),dmy_hm("23-10-2025 18:08"),dmy_hm("23-10-2025 18:07"),
#                  dmy_hm("25-10-2025 18:00"),dmy_hm("25-10-2025 18:00"),dmy_hm("25-10-2025 18:00"),dmy_hm("25-10-2025 18:05"),
#                  dmy_hm("25-10-2025 18:07"),dmy_hm("25-10-2025 18:07"),dmy_hm("25-10-2025 18:07"),dmy_hm("25-10-2025 18:06"))
# 
# names(inj_time) <- as.character(seq(1,length(inj_time)))

Folder <- paste0(str_split(FileList[1], "/")[[1]][1:length(str_split(FileList[1], "/")[[1]])-1],collapse = "/")
Folder_res <- paste0(Folder,"/Results_", format(now(), format = c("%Y-%m-%d_%H:%M:%S")))
dir.create(Folder_res)
setwd(Folder_res)

RawMetaboData <- sapply(FileList, function(File){
  tmp <- new("RawMetaboData",fileName = File,sepCSV = ";")
  tmp@data <- tmp@data[which(tmp@data$RER != "-"),]
  return(tmp)
})

# Combine tables #####
# RawMetaFull <- new("RawMetaboData") #Empty file generation on RawMetaboData format
RawMetaFull <- new("RawMetaboData")
RawMetaFull@header <- do.call("rbind", lapply(RawMetaboData, function(data) data@header))
RawMetaFull@data <- do.call("rbind", lapply(RawMetaboData, function(data) data@data))
## Combine annotation
if (any(duplicated(RawMetaFull@header$`Animal No.`))){
  RawMetaFull@header <- RawMetaFull@header %>% mutate(Animal_old = `Animal No.`, `Animal No.` = row_number())
  RawMetaFull@data <- RawMetaFull@data %>% mutate(Animal_old = `Animal No.`, `Animal No.` = forcats::fct_inorder(paste(OriginDate, `Animal No.`)))
  levels(RawMetaFull@data$`Animal No.`) <- seq(1:length(levels(RawMetaFull@data$`Animal No.`)))
}


RawMetaFull@data = RawMetaFull@data %>% mutate_at(vars(!contains("Date") & !contains("Time")), function(x) as.numeric(gsub(",",".",x)))
RawMetaFull@data <- RawMetaFull@data %>%
  group_by(`Animal No.`) %>%
  mutate(deltaDrink = if("Drink" %in% names(.)){Drink - lag(Drink)}) %>%
  mutate(deltaFeed = if("Feed" %in% names(.)) {Feed - lag(Feed)}) %>%
  ungroup()

## extract annotation table from tables
colnames(RawMetaFull@header) <- c("Box","Animal","Weight","Treat","Text2","Text3", "Date", "Time", 
if(is.na(last(colnames(RawMetaFull@header)))){"old_mice_number"})
AnnotFull = RawMetaFull@header


## Define fields of interest
FieldsOfInterest = names(RawMetaFull@data)[c(1,2,14,17,20,21,40,41,24:39)]
FieldsOfInterest <- c(FieldsOfInterest,"deltaFeed","deltaDrink")

## Instanciate an analysis object
AnalysisFull = new("AnalysisMetaboData",rawData = RawMetaFull,
                   obs = FieldsOfInterest,annotation = AnnotFull,
                   annotGroups = c("Treat"),actSwitchHour = 7)

AnalysisFull@data <- AnalysisFull@data %>% mutate(UTC = dmy_hm(paste(Date, Time))) 
# if(!is.null(inj_time)) {AnalysisFull@data <- AnalysisFull@data %>% mutate(UTC_rel = difftime(UTC, inj_time[`Animal No.`][[1]])/dminutes(x=5))}

time <- data.frame(AnalysisFull@data$MyTime, AnalysisFull@data$RelDay)
print("Full Analysis")

# metaboRawPlot2(AnalysisFull, observation = "Feed", group = "Treat",label = "Animal No.", Time_scale = "UTC_rel")
metaboRawPlot2(AnalysisFull, observation = "Feed", group = "Treat",label = "Animal No.", Time_scale = "RelDay")
# try(metaboRawPlot2(AnalysisFull, observation = "Feed", group = "Treat",label = "Animal No.", Time_scale = "UTC_rel"))
AnalysisFull_filter <- AnalysisFull

# automatic filter 
mice_rm <- AnalysisFull_filter@data %>% group_by(`Animal No.`, Treat) %>% summarize(Feed = max(Feed), .groups = "drop_last") %>% ungroup
print(mice_rm)
mice_rm <- subset(mice_rm, mice_rm$Feed < (mice_rm %>% filter(Treat == "ct") %>% select(Feed) %>% min ))$`Animal No.`

mice_rm <- c(mice_rm, 45, 46)

# filtered plot
AnalysisFull_filter@data  <- subset(AnalysisFull_filter@data, subset = !(`Animal No.` %in% mice_rm))
metaboRawPlot2(AnalysisFull_filter, observation = "Feed", group = "Treat",label = "Animal No.", Time_scale = "RelDay")
# try(metaboRawPlot2(AnalysisFull_filter, observation = "Feed", group = "Treat",label = "Animal No.", Time_scale = "UTC_rel"))


# cl <- makeCluster(24)
# registerDoParallel(cl)

# Run scripts in parallel
result <- foreach(Field = FieldsOfInterest[-c(1,2)], .packages = c('tidyverse', 'directlabels', 'rstatix', 'ggpubr')) %do% {
    print(Field)
    
    try(tmp1 <- metaboRawPlot2(AnalysisFull_filter, observation = Field, group = "Treat",labels = "Animal No.", Time_scale = "RelDay"))
    # try(tmp2 <- metaboRawPlot2(AnalysisFull_filter, observation = Field, group = "Treat",label = "Animal No.", Time_scale = "UTC_rel"))

    
    tmpResDaily = new("ResDailyMeanStatMetabo",anMetData = AnalysisFull_filter,observation = Field,
                      group = "Treat",hourWin = c(19,7),timWind=c(0,0.5),control = "ct",
                      cumul = ifelse((Field == "Feed") | (Field == "Drink"), TRUE,FALSE))
    
    tmp2 <- metaboDailyPlot2(x = tmpResDaily, mainTitle = paste(Field, "Complet"))
    
    
    # metaboDailyPlot(tmpResDaily,mainTitle = paste(Field," night",
    #                                               "\npval_d=",format(summary(tmpResDaily@lmeRes)$tTable[2,5],digit=2),
    #                                               ", pval_r=",format(summary(tmpResDaily@lmeRes)$tTable[3,5],digit=2)))
    
    # tmp <- tmpResDaily
    # tmp@rawdata <- tmp@rawdata %>% filter(Sun == "night")
    # tmp3 <- metaboDailyPlot2(x = tmp, mainTitle = paste(Field, "night"))
    # 
    # 
    # tmp <- tmpResDaily
    # tmp@rawdata <- tmp@rawdata %>% filter(Sun == "day")
    # tmp4 <- metaboDailyPlot2(x = tmp, mainTitle = paste(Field, "day"))
    
    return(list(tmp1, tmp2))
}

# Cleanup
# stopCluster(cl)
# registerDoSEQ()  # Reset to sequential mode


pdf(file=paste0(today(), "_", "ResFull_all_split","_filter",".pdf"), width = 12, height = 12)
print(result)
dev.off()
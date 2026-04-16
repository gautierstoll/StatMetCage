# ResDailyMeanStatMetabo ####

## ResDailyMeanStatMetabo ####

#' @include AnalysisMetaboData.R
library("rstatix")
library("tidyverse") # add for ResDailyMeanStatMetabo2
library("ggplot2") # add for ResDailyMeanStatMetabo2
library("ggpubr")
NULL
setOldClass("lm")
setOldClass("lme")
setOldClass("TukeyHSD")
setOldClass("multitcomp")
NULL


# function plage
plage <- function(value){
  i <- 0
  tmp <- value[1]
  res <- vector()
  for (val in value){
    if (val == tmp){
      res <- c(res, i)
    }
    else{
      tmp <- val
      i <- i+1
      res <- c(res, i)
    }
  }
  return(res)
}




#' Class for linear modeling of temporal mean
#' @slot observation name of output extracted for AnalysisMetaboData
#' @slot norm name of data in AnalysisMetaboData used for normalization
#' @slot group name of data in AnalysisMetaboData used for experimental annotation
#' @slot hourWin window (in hours) for applying mean
#' @slot lmRes result of linear model applied on mean by animals
#' @slot lmeRes result of linear mixed model applied on mean by hourWin
#' @slot lmeRes2 result of linear mixed model applied on mean by hourWin development
#' @slot tukeyPairs result of Tukey test on lmRes
#' @slot statLog if TRUE, log10 is applied to observation (after normalization) for statistical analysis
#' @slot timWind time window (in days) on which model is applied
#' @slot cumul last minus first value in time window instead of mean
#' @data all data in raw format
#' @dataProcess all processed data
#' @export
setClass("ResDailyMeanStatMetabo",
         representation = representation(
           observation = "character",
           norm = "character",
           group = "character",
           hourWin = "numeric",
           lmRes = "lm",
           lmeRes = "lme",
           lmeRes2 = "lme",
           tukeyPairs = "TukeyHSD",
           statLog = "logical",
           timWind = "numeric",
           cumul = "logical",
           data = "data.frame",
           rawdata = "data.frame",
           dataProcess = "data.frame"
         ))


#' Constructor for ResStatMetabo, perform a mixed linear statistical test
#' @param anMetData S4 object of AnalysisMetaboData
#' @param observation column of anMetData used for analysis
#' @param norm column of anMetData used for normalization (if NULL, no normalization is applied)
#' @param group column of anMetData used experimental annotation
#' @param control name of control in experimental annotation
#' @param hourWin window (in hours) for applying mean. If it is empty, night is chosen
#' @param meanAnimal true if the mean is applied onto the animal, otherwise only on time window
#' @param statLog if TRUE, log10 is applied to observation (after normalization) for statistical analysis
#' @param timWind time window (in days) on which model is applied
#' @param cumul last minus first value in time window instead of mean
#' @data all data in raw format
#' @dataProcess all processed data
#' @export
setMethod( f="initialize",
           signature = "ResDailyMeanStatMetabo",
           definition = function(.Object, anMetData, observation, norm = NULL, group, control = "control",
                                 hourWin = c("8:00","20:00"), statLog=F, timWind = c(0,max(anMetData@data$RelDay)), cumul = F){
             .Object@statLog = statLog
             if (!is.null(norm)){.Object@norm = norm} # What is normalization purpose
             .Object@hourWin = hourWin
             .Object@timWind = timWind
             .Object@group = group
             .Object@observation = observation
             .Object@cumul = cumul
             if (class(anMetData) != "AnalysisMetaboData"){stop("AnMetData is not AnalysisMetaboData")}
             if (!is.element(observation,names(anMetData@data))){stop("Observation not found")}
             if (!is.element(group,names(anMetData@data))){stop("Group not found")}
             if (!is.element(control,unlist(anMetData@data[group]))){stop("Control ",control," not found")}
             if (length(timWind) != 2){stop("Invalid time window")}
             dataDF = anMetData@data[,c(anMetData@animal,observation,group,"MyTime","RelDay","Sun")]
             names(dataDF)[1:3] = c("Animal","Observation","Group")
             dataDF$Observation = as.numeric(gsub(",",".",dataDF$Observation,fixed=T))
             dataDF$Group = factor(dataDF$Group,levels = c(control,setdiff(unique(dataDF$Group),control)))
             if(!is.null(norm)) {
               if (!is.element(norm,names(anMetData@data))){stop("Normalization not found")}
               dataDF$Observation = dataDF$Observation/as.numeric(unlist(anMetData@data[[norm]]))
             }
             if (length(timWind) == 2) {dataDF <- dataDF %>% as_tibble %>% mutate(TimeWindow = (RelDay - timWind[1] / (timWind[2]-timWind[1])))}
             .Object@group = group
             # Text time windows converter
             if (grepl(":", hourWin[1]) | grepl(":", hourWin[2])) {
               hourWin <- sapply(hourWin, function(x) {
                 tmp <- strsplit(x, ":")[[1]]
                 if (tmp[1] < 0 & tmp[1] > 23 & tmp[2] < 0 & tmp[2] >= 60) {stop("Uncorrect hourWin format")}
                 return(as.numeric(tmp[1])+as.numeric(tmp[2])/60)
                 })
             }
             if (hourWin[1] < hourWin[2]) {
               dataDF$activity = as.integer(((as.integer(dataDF$MyTime)/3600)%%24 > hourWin[1]) & ((as.integer(dataDF$MyTime)/3600)%%24 < hourWin[2]))
             } else {
               dataDF$activity = as.integer(((as.integer(dataDF$MyTime)/3600)%%24 > hourWin[1]) | ((as.integer(dataDF$MyTime)/3600)%%24 < hourWin[2]))
             }
             dataDF2 <- dataDF %>% as_tibble %>% mutate(absolutDay = as.integer((unclass(MyTime)/3600)/24), RelDay2 = as.factor(floor(RelDay)))
             dataDF2 <- dataDF2 %>% group_by(Animal) %>% mutate(period = as.factor(paste(plage(Sun), "-", Sun)))
             dataDF2$period <- factor(dataDF2$period, levels = str_sort(levels(dataDF2$period), numeric = TRUE))
              dataDF4Lm = do.call(rbind,
               by(dataDF,dataDF$Animal,function(subData){if (cumul) {
                 subDataObs = subData$Observation[which(subData$activity == 1)]
                 return(data.frame(Group = subData$Group[1],meanObs = subDataObs[length(subDataObs)] - subDataObs[1]))
               } else {
                 return(data.frame(Group = subData$Group[1],meanObs = mean(subData$Observation[which(subData$activity == 1)], na.rm = TRUE)))
                 }
                 }))
              if (statLog) {dataDF4Lm$meanObs = log10(dataDF4Lm$meanObs)}
                .Object@lmRes = lm(meanObs ~ Group, data = dataDF4Lm)
                dataDF4Lme = do.call(rbind,
                                   by(dataDF2,dataDF2[c('Animal','absolutDay')],
                                      function(subData){
                                        if (cumul) {
                                          subDataObs <- subData$Observation[which(subData$activity == 1)]
                                          data.frame(Group = subData$Group[1],
                                                     Animal = subData$Animal[1],
                                                     Days = subData$absolutDay[1],
                                                     meanObs = subDataObs[length(subDataObs)] - subDataObs[1],
                                                     RelDay = subData$RelDay[1],
                                                     RelDay2 = subData$RelDay2[1])
                                        } else {
                                        data.frame(Group = subData$Group[1],
                                                   Animal = subData$Animal[1],
                                                   Days = subData$absolutDay[1],
                                                   meanObs = mean(subData$Observation[which(subData$activity == 1)],na.rm=T),
                                                   RelDay = subData$RelDay[1],
                                                   RelDay2 = subData$RelDay2[1])
                                          }
                                        }))
                
                dataDF4Lme2 = do.call(rbind,
                                     by(dataDF2,dataDF2[c('Animal','RelDay2')],
                                        function(subData){
                                          if (cumul) {
                                            # subDataObs <- subData$Observation[which(subData$activity == 1)]
                                            subDataObs <- subData$Observation
                                            data.frame(Group = subData$Group[1],
                                                       Animal = subData$Animal[1],
                                                       Days = subData$absolutDay[1],
                                                       meanObs = subDataObs[length(subDataObs)] - subDataObs[1],
                                                       RelDay = subData$RelDay[1],
                                                       RelDay2 = subData$RelDay2[1])
                                            
                                          } else if(grepl("delta", observation)) {
                                            data.frame(Group = subData$Group[1],
                                                       Animal = subData$Animal[1],
                                                       Days = subData$absolutDay[1],
                                                       meanObs = sum(subData$Observation[which(subData$activity == 1)],na.rm=T),
                                                       RelDay = subData$RelDay[1],
                                                       RelDay2 = subData$RelDay2[1])
                                          } else {
                                            data.frame(Group = subData$Group[1],
                                                       Animal = subData$Animal[1],
                                                       Days = subData$absolutDay[1],
                                                       meanObs = mean(subData$Observation[which(subData$activity == 1)],na.rm=T),
                                                       RelDay = subData$RelDay[1],
                                                       RelDay2 = subData$RelDay2[1])
                                          }
                                        }))
              
              if (statLog) {dataDF4Lme$meanObs = log10(dataDF4Lme$meanObs)}
              .Object@lmeRes = nlme::lme(meanObs ~ Group,random = ~ 1|Animal,data = dataDF4Lme[which(is.finite(dataDF4Lme$meanObs)),])
              .Object@lmeRes2 = nlme::lme(meanObs ~ Group,random = ~ 1|Animal,data = dataDF4Lme2[which(is.finite(dataDF4Lme2$meanObs)),])
              .Object@tukeyPairs = TukeyHSD(aov(meanObs ~ Group,data = dataDF4Lm))
              .Object@data <- dataDF
              .Object@rawdata <- dataDF2
              .Object@dataProcess <- dataDF4Lme2
             return(.Object)
           })


## metaboDailyPlot ####

setGeneric(
  name = "metaboDailyPlot",
  def = function(x,signif,pvalStar = T ,mainTitle = "",cex.axis.lab = 1){standardGeneric("metaboDailyPlot")}
)


#' Plot time dependant metabolic data
#' @param x ResDailyMeanStatMetabo S4 object
#' @param signif true for significance pairwise annotation
#' @param pvalStar significant annotation with stars instead of p-value
#' @param type type of plot: data, data.model or model
#' @param cex.axis.lab cex of axis tick, label, title and pval
#' @export
setMethod( f="metaboDailyPlot",
          signature = "ResDailyMeanStatMetabo",
          definition = function(x,signif=T,pvalStar = T,mainTitle = "",cex.axis.lab=1){
            plotDf = x@lmeRes$data
            pairwisePval=t(x@tukeyPairs$Group[,4,drop=F])
            names(pairwisePval) = row.names(x@tukeyPairs$Group)
            if (pvalStar) {
            ListSignif = (lapply(1:length(pairwisePval),function(index){
              if(pairwisePval[index] < 0.0001){return(c("****",strsplit(names(pairwisePval)[index],split = "-")[[1]]))}
              else if(pairwisePval[index] < 0.001){return(c("***",strsplit(names(pairwisePval)[index],split = "-")[[1]]))}
              else if(pairwisePval[index] < 0.01){return(c("**",strsplit(names(pairwisePval)[index],split = "-")[[1]]))}
              else if(pairwisePval[index] < 0.05){return(c("*",strsplit(names(pairwisePval)[index],split = "-")[[1]]))}
              else {return(c())}
            }))} else {
              ListSignif=(lapply(1:length(pairwisePval),function(index){
                if(pairwisePval[index] < 0.05){return(c(paste("p=",format(pairwisePval[index],digits = 2),sep=""),strsplit(names(pairwisePval)[index],split = "-")[[1]]))}
                else {return(c())}
            }))}
            ListSignif = ListSignif[which(sapply(ListSignif,length) > 0)]
            ListSignifPosIndex = lapply(ListSignif,function(hit){
              return(c(which(levels(plotDf$Group) == hit[2]),which(levels(plotDf$Group) == hit[3])))})
            minTr=min(plotDf$meanObs,na.rm=T)
            maxTr=max(plotDf$meanObs,na.rm=T)
            par(mar =c(cex.axis.lab*2,cex.axis.lab*4,cex.axis.lab*2,cex.axis.lab*2))
            boxplot(meanObs ~ Group,
                    data=plotDf,main=mainTitle,cex.main = cex.axis.lab,
                    xlab="",
                    ylab=x@observation,
                    cex.axis=cex.axis.lab,
                    cex.lab=cex.axis.lab,
                    ylim=c(minTr,length(ListSignif)*abs(maxTr-minTr)*.2+maxTr)
            )
            if (length(ListSignif) > 0) {
              if (length(pairwisePval) > 1) ## more than one pair of comparison
              {
                for (signifIndex in (1:length(ListSignif))) {
                  
                  segments(y0=maxTr+(signifIndex-.4)*abs(maxTr-minTr)*.2,
                           x0= ListSignifPosIndex[[signifIndex]][1],x1=ListSignifPosIndex[[signifIndex]][2])
                  text(x=(ListSignifPosIndex[[signifIndex]][1]+ListSignifPosIndex[[signifIndex]][2])/2,y=maxTr+(signifIndex-.1)*abs(maxTr-minTr)*.2,
                       labels=ListSignif[[signifIndex]][1],cex=cex.axis.lab)
                }
              } else {
                segments(y0=maxTr+(1-.4)*abs(maxTr-minTr)*.2,
                         x0= 1,x1=2)
                text(x=1+1/2,y=maxTr+(1-.1)*abs(maxTr-minTr)*.2,
                     labels=ListSignif[1],cex=cex.axis.lab)
              }
            }
            beeswarm::beeswarm(meanObs ~ Group,data=plotDf,add=T,cex=.5,col="red")
          })


## metaboDailyPlot2 ####

setGeneric(
  name = "metaboDailyPlot2",
  def = function(x,signif,pvalStar = T,mainTitle = ""){standardGeneric("metaboDailyPlot2")}
)

#' Plot time dependant metabolic data V2
#' @param x ResDailyMeanStatMetabo S4 object
#' @param signif true for significance pairwise annotation
#' @param pvalStar significant annotation with stars instead of p-value
#' @param mainTitle title of the plot
#' @param type type of plot: data, data.model or model
#' @export
setMethod( f="metaboDailyPlot2",
           signature = "ResDailyMeanStatMetabo",
           definition = function(x,signif=T,pvalStar = T,mainTitle = ""){
             plotDf = x@lmeRes2$data
             
             plotDf_stat_0 <- plotDf %>% tukey_hsd(formula = meanObs ~ Group) %>% add_y_position
             plotDf_stat_1 <- plotDf %>% filter(!is.na(meanObs)) %>% group_by(RelDay) %>% tukey_hsd(meanObs ~ Group) %>% add_y_position
             
             # plotDf_stat_0 <- dunn_test(formula = meanObs ~ Group, data = x@dataProcess) %>% add_y_position
             # plotDf_stat_1 <- x@dataProcess %>% select(Group, meanObs, RelDay) %>% filter(RelDay < 2) %>% group_by(RelDay) %>% dunn_test(meanObs ~ Group) %>% add_y_position
               
             gg <- ggplot(plotDf, aes(x = Group, y = meanObs, color = Group)) +
               geom_boxplot(outlier.shape = NA) +
               geom_point(position = position_jitterdodge()) +
               stat_anova_test(label.y.npc = 0.9) +
               theme_bw()
             
             p1 <- gg + stat_pvalue_manual(plotDf_stat_0) + ggtitle(paste(mainTitle,"All values"))
             p2 <- gg + stat_pvalue_manual(plotDf_stat_1) + ggtitle(paste(mainTitle,"split by relative day")) + facet_wrap(~ RelDay)
             
             plotDf <- x@rawdata %>% group_by(Group, Animal, period, Sun) %>%
               summarise(meanObs = mean(Observation, na.rm = TRUE), .groups = "keep") %>%
               rename(Time = period)
             
             plotDf_stat_2 <- plotDf %>% filter(!is.na(meanObs)) %>% group_by(Time) %>% tukey_hsd(meanObs ~ Group) %>% add_y_position
             # plotDf_stat_2 <- plotDf %>% filter(!is.na(meanObs)) %>% group_by(Time) %>% dunn_test(meanObs ~ Group) %>% add_y_position
             
             p4 <- ggplot(plotDf, aes(x = Group, y = meanObs, color = Group)) +
               geom_boxplot(outlier.shape = NA) +
               geom_point(position = position_jitterdodge()) +
               ggtitle(paste(mainTitle, "values by period")) +
               stat_anova_test(label.y.npc = 0.9) +
               theme_bw() +
               ylim(c(0,max(plotDf$meanObs)*1.5)) +
               facet_wrap(~ Time) +
               stat_pvalue_manual(plotDf_stat_2)
             
             return(list(p1, p2, p4))
           })



## metaboDailyPlot3 ####

setGeneric(
  name = "metaboDailyPlot3",
  def = function(x,signif, TimeWind,pvalStar = T,mainTitle = ""){standardGeneric("metaboDailyPlot2")}
)

setGeneric(
  name = "metaboDailyPlot3",
  def = function(x,signif, TimeWind,pvalStar = T,mainTitle = ""){standardGeneric("metaboDailyPlot3")}
)

#' Plot time dependant metabolic data V2
#' @param x ResDailyMeanStatMetabo S4 object
#' @param signif true for significance pairwise annotation
#' @param TimeWind plot and analysis window
#' @param pvalStar significant annotation with stars instead of p-value
#' @param mainTitle title of the plot
#' @param type type of plot: data, data.model or model
#' @export
setMethod( f="metaboDailyPlot3",
           signature = "ResDailyMeanStatMetabo",
           definition = function(x,signif=T,TimeWind,pvalStar = T,mainTitle = ""){
             # plotDf = x@lmeRes2$data
             plotDf <- x@rawdata %>%
               filter(TimeWindow >= TimeWind[1] & TimeWindow <= TimeWind[2]) %>%
               ungroup %>%
               # group_by(Group, TimeWindow) %>%
               summarise(Mean_obs = mean(Observation), .by = c(Group, TimeWindow))
             
             # AUC <- plotDf %>% group_by(Group) %>% summarise(AUC = bayestestR::auc(TimeWindow, Mean_obs, method = "trapezoid"))
             # plotDf %>% group_by(Group) %>% summarise(AUC = pROC::auc( ~ TimeWindow, method = "trapezoid"))
             library(statmod)
             test <- x@rawdata %>% pivot_wider(id_cols = c(Animal, Group), values_from = Observation, names_from = TimeWindow) %>% ungroup()
             
             lmer2.out  <- lme4::lmer(Observation ~ TimeWindow * Group + (1|Animal),data = plotDf)
             stats::anova(lmer2.out)
            
             
             test2 <- compareGrowthCurves(group = test$Group, test %>% select(!c("Animal", "Group")) %>% as.matrix, levels=NULL, nsim=10000, fun=meanT, times=NULL,
                                          verbose=TRUE, adjust="holm", n0=0.5)
             
             lmtest::grangertest(plotDf %>% filter(Group == "c") %>% select(Observation), plotDf %>% filter(Group == "d") %>% select(Observation))
             lmtest::grangertest(plotDf %>% filter(Group == "c") %>% select(Observation), plotDf %>% filter(Group == "r") %>% select(Observation))
             lmtest::grangertest(plotDf %>% filter(Group == "r") %>% select(Observation), plotDf %>% filter(Group == "d") %>% select(Observation))
             
             # plotDf_stat_0 <- dunn_test(formula = meanObs ~ Group, data = x@dataProcess) %>% add_y_position
             # plotDf_stat_1 <- x@dataProcess %>% select(Group, meanObs, RelDay) %>% filter(RelDay < 2) %>% group_by(RelDay) %>% dunn_test(meanObs ~ Group) %>% add_y_position
             
             gg <- ggplot(plotDf, aes(x = TimeWindow, y = Observation, colour = Group)) +
               geom_line(aes(group = Animal), alpha = 0.25) +
               geom_smooth(method = "glm", se = TRUE, level = 0.99) +
               # geom_line(stat="smooth",method = "lm", formula = y ~ x, alpha = 0.5, size = 1.5) +
               # annotate("text", x=min(plotDf$TimeWindow)*1.1, y=max(plotDf$Mean_obs), label= paste(unlist(pairwise_results$group1), "vs", unlist(pairwise_results$group2), ", pval = ",formatC(pairwise_results$p.adj, format = "g", digits = 3), collapse = "\n"))+
               # annotate("text", x=min(plotDf$TimeWindow)*1.1, y=max(plotDf$Mean_obs), label= paste(unlist(test2$Group1), "vs", unlist(test2$Group2), ", pval = ",formatC(test2$adj.P.Value, format = "g", digits = 3), collapse = "\n"))+
               theme_bw()
             gg
             
             gg <- ggplot(plotDf, aes(x = TimeWindow, y = Mean_obs, colour = Group)) +
               geom_line() +
               geom_smooth(method = "loess", se = TRUE, level = 0.99) +
               # geom_line(stat="smooth",method = "lm", formula = y ~ x, alpha = 0.5, size = 1.5) +
               # annotate("text", x=min(plotDf$TimeWindow)*1.1, y=max(plotDf$Mean_obs), label= paste(unlist(pairwise_results$group1), "vs", unlist(pairwise_results$group2), ", pval = ",formatC(pairwise_results$p.adj, format = "g", digits = 3), collapse = "\n"))+
               # annotate("text", x=min(plotDf$TimeWindow)*1.1, y=max(plotDf$Mean_obs), label= paste(unlist(test2$Group1), "vs", unlist(test2$Group2), ", pval = ",formatC(test2$adj.P.Value, format = "g", digits = 3), collapse = "\n"))+
               theme_bw()
             gg
             
             test3 <- sapply(DescTools::Closest(plotDf$TimeWindow, c(1.3+(c(1.5,3,6,12)/12))), function(x) x[1])
             plotDf %>% filter(TimeWindow %in% test3) |> count(Group, TimeWindow)
             gg2 <- ggplot(plotDf %>% filter(TimeWindow %in% test3), aes(y = Observation, x = Group, colour = Group)) +
               geom_boxplot(outliers = FALSE)+
               geom_point(position = position_jitterdodge())+
               stat_compare_means(method = "kruskal") +
               theme_bw()
             gg2
             gg2 + facet_wrap(~ TimeWindow, nrow = 2)
             
             p1 <- gg + ggtitle(paste(mainTitle,"Time windows"))
             
             return(p1)
           })













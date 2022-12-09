#' @include AnalysisMetaboData.R
NULL
setOldClass("lm")
NULL

#' Class for linear modeling of temporal mean
#' @slot observation name of output extracted for AnalysisMetaboData
#' @slot norm name of data in AnalysisMetaboData used for normalization
#' @slot group name of data in AnalysisMetaboData used for experimental annotation
#' @slot hourWin window (in hours) for applying mean
#' @slot meanAnimal true if the mean is applied onto the animal, otherwise only on time window
#' @slot lmRes result of mixed linear model
#' @slot statLog if TRUE, log10 is applied to observation (after normalization) for statistical analysis
#' @slot timWind time window (in days) on which model is applied
#' @export
setClass("ResDailyMeanStatMetabo",
         representation = representation(
           observation = "character",
           norm = "character",
           group = "character",
           hourWin = "numeric",
           meanAnimal = "logical",
           lmRes = "lm",
           statLog = "logical",
           timWind = "numeric"
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
#' @export
setMethod(  f="initialize",
           signature = "ResDailyMeanStatMetabo",
           definition = function(.Object,anMetData,observation,norm = NULL,group,control = "control",hourWin = c(8,20),meanAnimal = F,statLog=F,timWind = c()){
             .Object@statLog = statLog
             .Object@norm = norm
             .Object@hourWin = hourWin
             .Object@meanAnimal = meanAnimal
             .Object@timWin = timWin
             .Object@group = group
             .Object@observation = observation
             if (class(anMetData) != "AnalysisMetaboData"){stop("AnMetData is not AnalysisMetaboData")}
             if (!is.element(observation,names(anMetData@data))){stop("Observation not found")}
             if (!is.element(group,names(anMetData@data))){stop("Group not found")}
             if (!is.element(control,unlist(anMetData@data[group]))){stop("Control ",control," not found")}
             if ((length(timWind) == 1) | length(timWind) > 2){stop("Invalid time window")}
             dataDF = anMetData@data[,c(anMetData@animal,observation,group,"MyTime","RelDay","Sun")]
             names(dataDF)[1:3] = c("Animal","Observation","Group")
             dataDF$Observation = as.numeric(gsub(",",".",dataDF$Observation,fixed=T))
             dataDF$Group = factor(dataDF$Group,levels = c(control,setdiff(unique(dataDF$Group),control)))
             if(!is.null(norm)) {
               if (!is.element(norm,names(anMetData@data))){stop("Normalization not found")}
               dataDF$Observation = dataDF$Observation/as.numeric(unlist(anMetData@data[[norm]]))
             }
             if (is.null(norm)){.Object@norm = character(0)} else {.Object@norm = norm}
             if (length(timWind) > 1) {dataDF = dataDF[which((dataDF$RelDay > timWind[1]) & (dataDF$RelDay < timWind[2])),]}
             .Object@group = group
             dataDF$activity = c(0,1)[as.integer(((dataDF$MyTime/3600)%%24 > hourWin[1]) & ((dataDF$MyTime/3600)%%24 > hourWin[1])) + 1]
             dataDF$absolutDay = as.integer((dataDF$MyTime/3600)/24)
             if (meanAnimal) {
              dataDF4Lm = do.call(rbind,
               by(dataDF,dataDF$Animal,function(subData){data.frame(Group = subData$Group[1],meanObs = mean(dataDF$Observation[which(dataDF$activity == 1)]))}))
              if (statLog) {dataDF4Lm$meanObs = log10(dataDF4Lm$meanObs)}
             .Object@lmRes = lm(meanObs ~ Group,data = dataDF4Lm)}
             else {
               dataDF4Lm = do.call(rbind,
                                   by(dataDF,dataDF$Animal,dataDF$absoluteDay,
                                      function(subData){data.frame(Group = subData$Group[1],Animal = subData$Animal[1],
                                                                  meanObs = mean(dataDF$Observation[which(dataDF$activity == 1)]))}))
               if (statLog) {dataDF4Lm$meanObs = log10(dataDF4Lm$meanObs)}
               .Objec@lmRes = nlm::lme(meanObs ~ Group,random = ~ 1|Animal)
               }
             return(.Object)
           })

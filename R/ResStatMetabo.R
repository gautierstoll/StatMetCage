#' @include AnalysisMetaboData.R
NULL
setOldClass("summary.lme")
NULL

#' Class for results of statistical analysis
#' @slot data data used for analysis
#' @slot observation name of output extracted for AnalysisMetaboData
#' @slot norm name of data in AnalysisMetaboData used for normalization
#' @slot group name of data in AnalysisMetaboData used for experimental annotation
#' @slot actSwitchHour hour at which light is switch on
#' @slot model type time model (constant, linear, quadratic, linearReg)
#' @slot lmeRes result of mixed linear model
#' @slot statLog if TRUE, log10 is applied to observation (after normalization) for statistical analysis
#' @slot timWind time window (in days) on which model is applied
#' @export
setClass("ResStatMetabo",
         representation = representation(
##           data = "data.frame", data is already in lmeRes
           observation = "character",
           norm = "character",
           group = "character",
           actSwitchHour = "numeric",
           model = "character",
           lmeRes = "summary.lme",
           statLog = "logical",
           timWind = "numeric"
         ))
#' Constructor for ResStatMetabo, perform a mixed linear statistical test
#' @param anMetData S4 object of AnalysisMetaboData
#' @param observation column of anMetData used for analysis
#' @param model type of time model (constant, linear, quadratic, linearReg)
#' @param norm column of anMetData used for normalization (if NULL, no normalization is applied)
#' @param group column of anMetData used experimental annotation
#' @param control name of control in experimental annotation
#' @param statLog if TRUE, log10 is applied to observation (after normalization) for statistical analysis
#' @param timWind time window (in days) on which model is applied
#' @export
setMethod( f="initialize",
          signature = "ResStatMetabo",
          definition = function(.Object,anMetData,observation,model = "quadratic",norm = NULL,group,control = "control",statLog=F,timWind = c() ){
            .Object@actSwitchHour = anMetData@actSwitchHour
            if (class(anMetData) != "AnalysisMetaboData"){stop("AnMetData is not AnalysisMetaboData")}
            if (!is.element(observation,names(anMetData@data))){stop("Observation not found")}
            .Object@observation = observation
            if (!is.element(group,names(anMetData@data))){stop("Group not found")}
            if (!is.element(control,unlist(anMetData@data[group]))){stop("Control ",control," not found")}
            if ((length(timWind) == 1) | length(timWind) > 2){stop("Invalid time window")}
            dataDF = anMetData@data[,c(anMetData@animal,observation,group,"RelDay","Sun","OscillActivity","SqRelDay")]
            
            
            names(dataDF)[1:3] = c("Animal","Observation","Group")
            dataDF$Observation = as.numeric(gsub(",",".",dataDF$Observation,fixed=T))
            dataDF$Group = factor(dataDF$Group,levels = c(control,setdiff(unique(dataDF$Group),control)))
            
            if(!is.null(norm)) {
              if (!is.element(norm,names(anMetData@data))){stop("Normalization not found")}
              dataDF$Observation = dataDF$Observation/as.numeric(unlist(anMetData@data[[norm]]))
            }
            if (is.null(norm)){.Object@norm = character(0)}else{.Object@norm = norm}
            if (length(timWind) > 1) {dataDF = dataDF[which((dataDF$RelDay > timWind[1]) & (dataDF$RelDay < timWind[2])),]}
            .Object@timWind = timWind
            .Object@model = model
            .Object@group = group
            if (model == "linear"){
              if(statLog) {
                dataDF$LogTObservation = log10(dataDF$Observation)
                lmeRes = summary(nlme::lme(LogTObservation ~ Group*OscillActivity+RelDay,random = ~ 1|Animal,
                                           data = dataDF[which(!is.na(dataDF$Observation)),]))
              } else {
              lmeRes = summary(nlme::lme(Observation ~ Group*OscillActivity+RelDay,random = ~ 1|Animal,
                                         data = dataDF[which(!is.na(dataDF$Observation)),]))
              }
            }
            else if (model == "quadratic"){
              if(statLog) {
                dataDF$LogTObservation = log10(dataDF$Observation)
                lmeRes = summary(nlme::lme(LogTObservation ~ (Group*OscillActivity)+RelDay+ SqRelDay,random = ~ 1|Animal,
                                           data = dataDF[which(!is.na(dataDF$Observation)),]))
              } else {
                lmeRes = summary(nlme::lme(Observation ~ (Group*OscillActivity)+RelDay+ SqRelDay,random = ~ 1|Animal,
                                           data = dataDF[which(!is.na(dataDF$Observation)),]))
              }
            }
            else if (model == "constant"){
              if(statLog) {
                dataDF$LogTObservation = log10(dataDF$Observation)
                lmeRes = summary(nlme::lme(LogTObservation ~ Group*OscillActivity,random = ~ 1|Animal,
                                           data = dataDF[which(!is.na(dataDF$Observation)),]))
              } else {
                lmeRes = summary(nlme::lme(Observation ~ Group*OscillActivity,random = ~ 1|Animal,
                                           data = dataDF[which(!is.na(dataDF$Observation)),]))
              }
            }
            else if (model == "linearReg")
              if(statLog) {
                dataDF$LogTObservation = log10(dataDF$Observation)
                lmeRes = summary(nlme::lme(LogTObservation ~ Group*RelDay+OscillActivity,random = ~ 1|Animal,
                                           data = dataDF[which(!is.na(dataDF$Observation)),]))
              } else {
                lmeRes = summary(nlme::lme(Observation ~ Group*RelDay+OscillActivity,random = ~ 1|Animal,
                                           data = dataDF[which(!is.na(dataDF$Observation)),]))
              }
            else {stop("Model not found")}
            .Object@lmeRes = lmeRes
            .Object@statLog = statLog

##            .Object@data = dataDF data is already in lmeRes
            return(.Object)
          })

setGeneric(
  name = "metaboPlot",
  def = function(x,type = "data",mainTitle = ""){standardGeneric("metaboPlot")}
)

#' Plot time dependant metabolic data
#' @param x ResStatMetabo S4 object
#' @param type type of plot: data, data.model or model
#' @param mainTitle title
#' @export
setMethod(f="metaboPlot",
          signature = "ResStatMetabo",
          definition = function(x,type="data",mainTitle = ""){
            xMinMax = c(min(x@lmeRes$data$RelDay),max(x@lmeRes$data$RelDay))
            yMinMax = c(min(x@lmeRes$data$Observation),
                        max(x@lmeRes$data$Observation)+.1*length(unique(x@lmeRes$data$Group))*(max(x@lmeRes$data$Observation) - min(x@lmeRes$data$Observation))
                        )
            tPoints = sort(unique(x@lmeRes$data$RelDay))
            mCoeff = x@lmeRes$coefficients$fixed
            predRes = rep(mCoeff[["(Intercept)"]],length(tPoints))
            if (is.element("RelDay",names(mCoeff))){ 
              predRes = predRes + mCoeff[["RelDay"]]*tPoints
            }
            if (is.element("SqRelDay",names(mCoeff))){predRes = predRes + mCoeff[["SqRelDay"]]*tPoints*tPoints}
            predOsc = sapply(tPoints,function(t){
              Osc = x@lmeRes$data$OscillActivity[which(x@lmeRes$data$RelDay == t)]
              if (length(Osc) > 1) {return(Osc[1])} else {return(Osc)}
            })
            
            ##
            if((type == "data")|(type == "data.model")){
              plot(x@lmeRes$data$RelDay[which(x@lmeRes$data$Animal == unique(x@lmeRes$data$Animal)[1])],
                   x@lmeRes$data$Observation[which(x@lmeRes$data$Animal == unique(x@lmeRes$data$Animal)[1])],
                   type = "l",lwd = .5,col = as.numeric(x@lmeRes$data$Group)[1],
                   ylim = yMinMax,xlim=xMinMax,xlab = "Relative day",ylab = x@observation,main = mainTitle)

              for (tmpAnimal in unique(x@lmeRes$data$Animal)[-1]){
                points(x@lmeRes$data$RelDay[which(x@lmeRes$data$Animal == tmpAnimal)],
                       x@lmeRes$data$Observation[which(x@lmeRes$data$Animal == tmpAnimal)],
                       col = unique(as.numeric(x@lmeRes$data$Group[which(x@lmeRes$data$Animal == tmpAnimal)])),
                       type = "l",lwd = .5)}
              legend(x=xMinMax[1],y=yMinMax[2],legend = unique(x@lmeRes$data$Group),col = as.numeric(unique(x@lmeRes$data$Group)),pch=1)
            }
            else if (type == "model"){
              predResOsc = predRes + predOsc*mCoeff[["OscillActivity"]]
              plot(tPoints,predResOsc,
                   type = "l",lty=2,lwd=2,col=1, ylim = yMinMax,xlim=xMinMax,xlab = "Relative day",ylab = x@observation,
                   main = mainTitle)}
            else {stop("Unknow type of plot")}
            if (type == "data.model"){
              predResOsc = predRes + predOsc*mCoeff[["OscillActivity"]]
              points(tPoints,predResOsc,type = "l",lty=2,lwd=2,col=1)
            }
            if ((type == "model") | (type == "data.model"))
              for (iterGr in levels(unlist(x@lmeRes$data[["Group"]]))[-1]){
                if (x@model == "linearReg") {
                  predResOsc = predRes + mCoeff[[paste("Group",iterGr,sep="")]] +
                    predOsc*(mCoeff[["OscillActivity"]]) +
                    mCoeff[[paste("Group",iterGr,":","RelDay",sep="")]] * tPoints
                } else {
                predResOsc = predRes + mCoeff[[paste("Group",iterGr,sep="")]] +
                  predOsc*(mCoeff[["OscillActivity"]] + mCoeff[[paste("Group",iterGr,":","OscillActivity",sep="")]])}
                points(tPoints,predResOsc,
                       type = "l",lty=2,lwd=2, col=which(levels(unlist(x@lmeRes$data[["Group"]])) == iterGr))
              }
          }
)
setGeneric(
  name = "predictStatMetabo",
  def = function(object,tPoint,group=""){standardGeneric("predictStatMetabo")}
)

#' @export
setMethod(f="predictStatMetabo",
          signature = "ResStatMetabo",
          definition = function(object,tPoint,group=""){
          phaseRelDay = object@lmeRes$data$RelDay[1]-asin(object@lmeRes$data$OscillActivity[1])/2/pi
           fCoeff = object@lmeRes$coefficients$fixed
           predRes = fCoeff[["(Intercept)"]]
           if (is.element(paste("Group",group,sep=""),names(fCoeff))){
             predRes = predRes + 
               fCoeff[[paste("Group",group,sep="")]] +
             (fCoeff[["OscillActivity"]]+fCoeff[[paste("Group",group,":","OscillActivity",sep="")]])*sin((tPoint-phaseRelDay)/.5*pi)
             }
           else {
             predRes = predRes + 
             +(fCoeff[["OscillActivity"]])*sin((tPoint-phaseRelDay)/.5*pi)
           }
           if (is.element("RelDay",names(fCoeff))){ 
             predRes = predRes + fCoeff[["RelDay"]]*tPoint
             }
           if (is.element("SqRelDay",names(fCoeff))){predRes = predRes + fCoeff[["SqRelDay"]]*tPoint*tPoint}
           if (object@statLog){predRes = 10^predRes}
           return(predRes)
          })


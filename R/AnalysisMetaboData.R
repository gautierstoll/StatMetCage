#' @include RawMetaboData.R
NULL

#' Class of data for statistical analysis
#' @slot data data frame that contains animal, specified observations, absolute time (my time), floating point day (RelDay), day activity (Sun), sine function based on day activity, squared floating point day (SqRelDay)
#' @slot animal name of animal column
#' @export
setClass("AnalysisMetaboData",
         representation = representation(
           data = "data.frame",
           animal = "character"
         ))
#' Constructor of AnalysisMetaboData
#' @param rawData S4 object of RawMetaboData
#' @param date name of date column
#' @param time name of time column
#' @param animal name of animal column
#' @param obs list of observation to be extracted
#' @param annotation data frame of annotation to be added to each animals, must contains an Animal column
#' @export
setMethod(f="initialize",
          signature = "AnalysisMetaboData",
          definition = function(.Object,
                                rawData,
                                date = "Date",
                                time = "Time",
                                animal = "Animal No.",
                                obs = "VO2(3)",
                                annotation = NULL,
                                annotGroups = c("Group")){
            if (class(rawData) != "RawMetaboData"){stop("Input is not RawMetaboData")}
            rawCol = c(date,time,animal,obs,norm)
            if (length(setdiff(rawCol,names(rawData@data))) > 0){stop("Cannot find ",setdiff(rawCol,names(rawData@data)))}
            dataDF = rawData@data[c(animal,obs)]
            dataDF$MyTime =  lubridate::dmy_hm(paste(unlist(rawData@data[date]),unlist(rawData@data[time]),sep=" "))

            dataDF$RelDay = unlist(by(dataDF,dataDF[animal],
                                       function(SData){(unclass(SData$MyTime) - unclass(SData$MyTime)[1])/(24*3600)}))
            dataDF$Sun = c("day","night")[as.integer((dataDF$RelDay+.125)*2)%%2+1] ## same as activity
            dataDF$OscillActivity = sin((dataDF$RelDay-0.3125)/.5*pi)
            dataDF$SqRelDay = dataDF$RelDay^2
            if (!is.null(annotation)){
              if (!is.element("Animal",names(annotation))){stop("No Animal column in annotation")}
              if (!is.element("Date",names(annotation))){stop("No Date column in annotation")}
              if (!is.element("Time",names(annotation))){stop("No Time column in annotation")}
              if (!Reduce(f = "&",is.element(annotGroups,names(annotation)))){
                stop("Mission group annotation columns in annotation")}
              if (length(intersect(unique(unlist(dataDF[[animal]])),unique(annotation$Animal))) <
                  length(unique(unlist(dataDF[[animal]])))) {stop("Missing animals in annotation")}
             .Object@data = do.call(rbind,by(dataDF,dataDF[[animal]],function(subDF){
               subAnimal = unique(unlist(subDF[[animal]]))
               subAnnot = annotation[which(annotation$Animal == subAnimal),]
               subAnnot4DF = as.data.frame(apply(subAnnot[1,!is.element(names(subAnnot),c("Animal","Date","Time"))],2,
                                              function(x){rep(x,length(subDF[,1]))}),stringsAsFactors = F)
               if (length(subAnnot[,1]) == 1){
                 for(nGr in setdiff(names(subAnnot4DF),annotGroups)){ subAnnot4DF[[nGr]] = as.numeric(unlist(subAnnot4DF[[nGr]]))}
               } else {
                  subAnnot$MyTime = lubridate::dmy_hm(paste(unlist(subAnnot$Date),unlist(subAnnot$Time),sep=" "))
                  subAnnot$RelDay = unclass(subAnnot$MyTime) - unclass(subDF$MyTime[1])
                  for(nGr in setdiff(names(subAnnot4DF),annotGroups)){ subAnnot4DF[[nGr]] = spline(subAnnot$RelDay,unlist(subAnnot[[nGr]]),xout = subDF$RelDay)$y}
               }
               names(subAnnot4DF) = lapply(names(subAnnot4DF),function(name){
                 if (is.element(name,names(subDF))) {return(paste(name,"annot",sep = "_"))}else(return(name))})
               return(cbind(subDF,subAnnot4DF))}))}
             else {.Object@data = dataDF}
            .Object@animal = animal
            return(.Object)
          })

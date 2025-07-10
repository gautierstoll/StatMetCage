#' @include RawMetaboData.R
NULL

#' Class of data for statistical analysis
#' @slot data data frame that contains animal, specified observations, absolute time (my time), floating point day (RelDay), day activity (Sun), sine function based on day activity, squared floating point day (SqRelDay)
#' @slot animal name of animal column
#' @slot actSwitchHour hour at which light is switch on
#' @export
setClass("AnalysisMetaboData",
         representation = representation(
           data = "data.frame",
           animal = "character",
           actSwitchHour = "numeric"
         ))

#' Constructor of AnalysisMetaboData
#' @param rawData S4 object of RawMetaboData
#' @param date name of date column in raw data
#' @param time name of time column in raw data
#' @param actSwitchHour hour at which light is switched on
#' @param animal name of animal column in raw data
#' @param obs list of observation to be extracted in raw data
#' @param annotation data frame of annotation to be added to each animals, must contain the column Animal, Time, Date
#' @param annotGroups list of qualitative annotation column. Other columns will be treated as numeric with a time dependence evaluated by spline.
#' @export
setMethod(f="initialize",
          signature = "AnalysisMetaboData",
          definition = function(.Object,
                                rawData,
                                date = "Date",
                                time = "Time",
                                actSwitchHour = "6",
                                animal = "Animal No.",
                                obs = "VO2(3)",
                                annotation = character(0),
                                annotGroups = c("Group")){
            if (class(rawData) != "RawMetaboData"){stop("Input is not RawMetaboData")}
            rawCol = c(date,time,animal,obs)
            if (length(setdiff(rawCol,names(rawData@data))) > 0){stop("Cannot find ",setdiff(rawCol,names(rawData@data)))}
            rawData4A = rawData@data[order(rawData@data[[animal]]),]
            dataDF = rawData4A[c(animal,obs)]
            dataDF$MyTime =  lubridate::dmy_hms(paste(unlist(rawData4A[date])," ",unlist(rawData4A[time]),":00",sep=""))
            
            dataDF$RelDay = unlist(by(dataDF,dataDF[[animal]], ## double [  create a vector
                                       function(SData){return((unclass(SData$MyTime) - unclass(SData$MyTime)[1])/(24*3600))}))
            dataDF$Sun = c("day","night")[as.integer((((unclass(dataDF$MyTime)/3600)%%24-actSwitchHour)/12)%%2)+1] ## same as activity
            dataDF$OscillActivity = sin((unclass(dataDF$MyTime)/3600-actSwitchHour)/12*pi)
              
            dataDF$SqRelDay = dataDF$RelDay^2
            if (length(annotation)>0){
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
               #
               # No more usefull due to Class change
               #
               # subAnnot4DF = as.data.frame(apply(subAnnot[1,!is.element(names(subAnnot),c("Animal","Date","Time"))],2,
               #                                function(x){rep(x,length(subDF[,1]))}),stringsAsFactors = F)
               # subAnnot4DF <- subAnnot
               #
               if (length(subAnnot[,1]) == 1){
                 for(nGr in setdiff(names(subAnnot),annotGroups)){ subAnnot[[nGr]] = as.numeric(unlist(subAnnot[[nGr]]))}
               } else {
                  subAnnot$MyTime = lubridate::dmy_hm(paste(unlist(subAnnot$Date),unlist(subAnnot$Time),sep=" "))
                  subAnnot$RelDay = unclass(subAnnot$MyTime) - unclass(subDF$MyTime[1])
                  for(nGr in setdiff(names(subAnnot),annotGroups)){ subAnnot[[nGr]] = spline(subAnnot$RelDay,unlist(subAnnot[[nGr]]),xout = subDF$RelDay)$y}
               }
               names(subAnnot) = lapply(names(subAnnot),function(name){
                 if (is.element(name,names(subDF))) {return(paste(name,"annot",sep = "_"))}else(return(name))})
    
               return(cbind(subDF,subAnnot))}))}
             else {.Object@data = dataDF}
            .Object@animal = animal
            .Object@actSwitchHour = actSwitchHour 
            return(.Object)
          })

setGeneric(
  name = "metaboRawPlot",
  def = function(x,observation,type="data",group = "Group"){standardGeneric("metaboRawPlot")}
)


#' Plot time dependant metabolic  raw data
#' @param x AnalysisMetaboData S4 object
#' @param observation parameter to be plotted
#' @param type type of plot: data, mean.sd
#' @param group Group for coloring and/or mean/sd
#' @export
setMethod(f="metaboRawPlot",
          signature = "AnalysisMetaboData",
          definition = function(x,observation,type="data",group = "Group"){
            Animals = unique(x@data[[x@animal]])
            AnnotGroups = unique(x@data[[group]])
            yMinMax = c(min(as.numeric(gsub(",",".",x@data[[observation]],fixed=T)),na.rm = T),max(as.numeric(gsub(",",".",x@data[[observation]],fixed=T)),na.rm = T)+
                         +.1*length(AnnotGroups)*(max(as.numeric(gsub(",",".",x@data[[observation]],fixed=T)),na.rm = T) - min(as.numeric(gsub(",",".",x@data[[observation]],fixed=T)),na.rm = T)))
            
            plot(x@data$RelDay[which(x@data[[x@animal]] == Animals[1])],
                 as.numeric(gsub(",",".",x@data[[observation]][which(x@data[[x@animal]] == Animals[1])],fixed=T)),
                 col = which(AnnotGroups == x@data[[group]][which(x@data[[x@animal]] == Animals[1])[1]]),
                 ylim = yMinMax,type = "l",xlab = "Relative day" ,ylab = observation)
            listCol = which(AnnotGroups == x@data[[group]][which(x@data[[x@animal]] == Animals[1])[1]])
            for (animalIndex in (1:(length(Animals)-1))){
              points(x@data$RelDay[which(x@data[[x@animal]] == Animals[animalIndex])],
                   as.numeric(gsub(",",".",x@data[[observation]][which(x@data[[x@animal]] == Animals[animalIndex])],fixed=T)),
                   col = which(AnnotGroups == x@data[[group]][which(x@data[[x@animal]] == Animals[animalIndex])[1]]),
                   type = "l")
              listCol = c(listCol,which(AnnotGroups == x@data[[group]][which(x@data[[x@animal]] == Animals[animalIndex])[1]]))
              
            }
            xMinMax = c(min(x@data$RelDay),max(x@data$RelDay))
            legend(x=xMinMax[1],y=yMinMax[2],legend = AnnotGroups,col = unique(listCol),pch=1) ## col may not be correct
          })

# Second version of metaboRawPlot #####
# Based on ggplot2
setGeneric(
  name = "metaboRawPlot2",
  def = function(x,observation,group = "Group"){standardGeneric("metaboRawPlot2")}
)

#' Plot time dependant metabolic  raw data
#' @param x AnalysisMetaboData S4 object
#' @param observations parameter to be plotted
#' @param group Group for coloring and/or mean/sd
#' @export
setMethod(f="metaboRawPlot2",
          signature = "AnalysisMetaboData",
          definition = function(x,observation,group = "Group"){
            gg <- ggplot(x@data %>% filter(!is.na(get(observation))), aes(x = RelDay, y = get(observation), color = get(group)))+
              geom_line(alpha = 0.3, if('Animal No.' %in% colnames(x@data)){aes(group = `Animal No.`)}) +
              labs(y = observation, color = group) +
              ggtitle(observation) +
              geom_smooth(method = "loess", formula = 'y ~ x', span = 0.01)+
              theme_bw()
            print(gg)
          })

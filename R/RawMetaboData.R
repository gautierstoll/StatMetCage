#' Class of raw data parsing
#' @slot Date start recording date
#' @slot Batch Batch ID of the recording
#' @slot Device Device model
#' @slot header the rest of information contained in raw data file
#' @slot data the data frame of raw data, one line is a measure, it is sort by animal
#' @export
setClass(Class = "RawMetaboData",
         representation = representation(
           Date = "Date",
           Batch = "character",
           Device = "character",
           header = "data.frame",
           data = "data.frame"
         ),
         validity = function(object){
           if (length(object@header) == 0){
             stop("Empty header")
           }
           if (length(object@data) == 0){
             stop("Empty data")
           }
           return(TRUE)
         }
)
#' Constructor of class RawMetaboData
#'
#' @param fileName .csv file(s) containing raw data
#' @param sepCSV column separator for .csv file, "," by default
#' @export
#'
setMethod(f="initialize",
          signature = "RawMetaboData",
          definition = function(.Object,fileName,sepCSV = ","){
            Data=scan(fileName,what="",sep="\n")
            tmpFirst = grep("Date",Data,fixed=T)
            .Object@header = Data[1:tmpFirst]
            DataTable = lapply(Data[c(tmpFirst,((tmpFirst+2):length(Data)))],function(l){strsplit(l,split = sepCSV)[[1]]})
            DataDF = as.data.frame(do.call("rbind",DataTable[-1]))
            names(DataDF) = DataTable[[1]]
            for (index in (1:length(DataDF[1,]))) {DataDF[,index] = as.character(DataDF[,index])}
            .Object@data = DataDF
            return(.Object)
          })

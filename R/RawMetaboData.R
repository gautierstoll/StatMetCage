#' Class of raw data parsing
#' @slot data the data frame of raw data
#' @slot header the rest of information contained in raw data file
#' @export
setClass(Class = "RawMetaboData",
         representation = representation(
           data = "data.frame",
           header = "character"
         ),
         validity = function(object){
           if (length(object@data) == 0){
             stop("Empty data")
           }
           return(TRUE)
         }
)
#' Constructor of class RawMetaboData
#'
#' @param fileName .csv file containing raw data
#' @param sepCSBV column separator for .csv file
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

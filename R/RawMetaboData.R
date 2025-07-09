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
#' If no fileName submit create an empty RawMetaboData object
#' @param fileName .csv file(s) containing raw data
#' @param sepCSV column separator for .csv file, by default is ";"
#' @param sepDec Decimal separator for .csv file, by default is ","
#' @export
#'
setMethod(f="initialize",
          signature = "RawMetaboData",
          definition = function(.Object,fileName,sepCSV = ";",sepDec=","){
            if (missingArg(fileName)){
              .Object@Date = lubridate::NA_Date_
              .Object@Batch = NA_character_
              .Object@Device = NA_character_
              .Object@data = data.frame(matrix(0,0,0))
              .Object@header = data.frame(matrix(0,0,0))
            } else {
            Data=scan(fileName,what="",sep="\n",dec=sepDec,blank.lines.skip = FALSE)
            Data <- Data[Data != ""] # Remove possible blank lines
            tmpFirst <- grep("Date;Time", Data, useBytes = TRUE) # Find Data table start with Date;Time
            # Meta info
            tmp <- strsplit(Data[1:tmpFirst-1], sepCSV)
            .Object@Date = as.Date(tmp[[1]][1], tryFormats = c("%d%m%Y","%Y-%m-%d", "%Y/%m/%d"))
            .Object@Batch = tmp[[1]][2]
            .Object@Device = tmp[[2]][2]
            # Header data
            .Object@header = as.data.frame(do.call("rbind", sapply(Data[4:tmpFirst-1], strsplit, sepCSV)))
            colnames(.Object@header) <- .Object@header[1,]
            .Object@header <- .Object@header[-c(1),]
            # Main data
            # Warning: Unit line create some error
            DataTable = lapply(Data[tmpFirst+2:length(Data)],function(l){strsplit(l,split = sepCSV)[[1]]})
            DataDF = as.data.frame(do.call("rbind",DataTable))
            colnames(DataDF) = strsplit(Data[tmpFirst],sepCSV)[[1]]
            for (index in (1:length(DataDF[1,]))) {DataDF[,index] = as.character(DataDF[,index])}
            
            .Object@data = DataDF %>% as_tibble %>% 
              mutate(OriginDate = .Object@Date)
            
            .Object@header <- .Object@header %>% as_tibble %>%
              mutate_at(3, function(x) as.numeric(gsub(",",".", x))) %>%
              mutate(Date = DataDF[1,1], Time = DataDF[1,2])
            }
            return(.Object)
          })

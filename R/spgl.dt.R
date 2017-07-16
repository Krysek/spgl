#' spgl.dt.dtDlDir
#'
#' This function creates an object of spgl.dt.dtDlDir.
#' @param id integer
#' @param forDate as_date(character())
#' @param dlDir character
#' @param dlDir.fllPath character
#' @return data.table
#' @author Christian Frei
#' @details XXX
#' @examples
#' dtDlDir1 <- spgl.dt.dtDlDir()
#' print(dtDlDir1)
#'
#' dtDlDir2 <- spgl.dt.dtDlDir(id = as.integer(1),
#'                            forDate = as_date(today()),
#'                            dlDir = "a",
#'                            dlDir.fllPath = "b")
#' print(dtDlDir2)
#' @export
#' @import data.table
#' @importFrom lubridate as_date
spgl.dt.dtDlDir <- function (id                   = integer(),
                                forDate              = as_date(character()),
                                dlDir                  = character(),
                                dlDir.fllPath          = character()) {
   dtDlDir <- data.table(id             = id,
                         forDate       = forDate,
                         dlDir         = dlDir,
                         dlDir.fllPath = dlDir.fllPath)

   dtDlDir
}




#' spgl.dt.dtHeadlineFile
#'
#' This function creates an object of spgl.dt.dtHeadlineFile.
#' @name spgl.dt.dtHeadlineFile
#' @param id integer
#' @param idDlDir integer
#' @param forDate POSIXt or character
#' @param inDate POSIXt or character
#' @param inTime POSIXt or character
#' @param dlDir character
#' @param dlDir.fllPath character
#' @param rawDir character
#' @param rawDir.fllPath character
#' @param fileHTML character
#' @param fileHTML.fllPath character
#' @param fileCSV character
#' @param fileCSV.fllPath character
#' @return data.table
#' @author Christian Frei
#' @details XXX
#' @examples
#' dtHeadlineFile1 <- spgl.dt.dtHeadlineFile()
#' print(dtHeadlineFile1)
#'
#' dtHeadlineFile2 <- spgl.dt.dtHeadlineFile(id                   = as.integer(1),
#'                                           idDlDir              = as.integer(1),
#'                                           forDate              = lubridate::as_date(today()),
#'                                           inDate               = lubridate::as_date(today()),
#'                                           inTime               = lubridate::as_datetime(now()),
#'                                           dlDir                = "a",
#'                                           dlDir.fllPath        = "b",
#'                                           rawDir               = "c",
#'                                           rawDir.fllPath       = "d",
#'                                           fileHTML             = "e",
#'                                           fileHTML.fllPath     = "f",
#'                                           fileCSV              = "g",
#'                                           fileCSV.fllPath      = "h")
#' print(dtHeadlineFile2)
#' @export
#' @import data.table
#' @importFrom lubridate as_date
#' @importFrom lubridate as_datetime
spgl.dt.dtHeadlineFile <- function (  id                   = integer(),
                                         idDlDir              = integer(),
                                         forDate              = lubridate::as_date(character()),
                                         inDate               = lubridate::as_date(character()),
                                         inTime               = lubridate::as_datetime(character()),
                                         dlDir                = character(),
                                         dlDir.fllPath        = character(),
                                         rawDir               = character(),
                                         rawDir.fllPath       = character(),
                                         fileHTML                 = character(),
                                         fileHTML.fllPath         = character(),
                                         fileCSV                 = character(),
                                         fileCSV.fllPath         = character()) {
   dtHeadlineFile <- data.table( id             = id,
                                 idDlDir        = idDlDir,
                                 forDate        = forDate,
                                 inDate         = inDate,
                                 inTime         = inTime,
                                 dlDir          = dlDir,
                                 dlDir.fllPath  = dlDir.fllPath,
                                 rawDir         = rawDir,
                                 rawDir.fllPath = rawDir.fllPath,
                                 fileHTML          = fileHTML,
                                 fileHTML.fllPath  = fileHTML.fllPath,
                                 fileCSV           = fileCSV,
                                 fileCSV.fllPath   = fileCSV.fllPath)

   dtHeadlineFile
}

#' spgl.dt.dtHeadline
#'
#' This function creates an object of spgl.dt.dtHeadline.
#' @param id integer
#' @param idHeadline integer
#' @param idHeadlineFile integer
#' @param forDate POSIXt or character
#' @param forTime POSIXt or character
#' @param isBentoLink logical
#' @param isSpiegelPlusLink logical
#' @param intro character
#' @param title character
#' @param section character
#' @param link character
#' @return data.table
#' @author Christian Frei
#' @examples
#' dtHeadline1 <- spgl.dt.dtHeadline()
#' print(dtHeadline1)
#'
#' dtHeadline2 <- spgl.dt.dtHeadline(id = as.integer(1),
#'                                   idHeadline = as.integer(2),
#'                                   idHeadlineFile = as.integer(3),
#'                                   forDate = as_date(today()),
#'                                   forTime = as_datetime(now()),
#'                                   isBentoLink = FALSE,
#'                                   isSpiegelPlusLink = FALSE,
#'                                   intro = "a",
#'                                   title = "b",
#'                                   section = "c",
#'                                   link = "d")
#' print(dtHeadline2)
#' @export
#' @import data.table
#' @importFrom lubridate as_date
#' @importFrom lubridate as_datetime
spgl.dt.dtHeadline <- function (id                   = integer(),
                                   idHeadline           = integer(),
                                   idHeadlineFile       = integer(),
                                   forDate              = lubridate::as_date(character()),
                                   forTime              = lubridate::as_datetime(character()),
                                   isBentoLink          = logical(),
                                   isSpiegelPlusLink    = logical(),
                                   intro                = character(),
                                   title                = character(),
                                   section              = character(),
                                   link                 = character()) {
   dfHeadline <- data.table(  id                = id,
                              idHeadline        = idHeadline,
                              idHeadlineFile    = idHeadlineFile,
                              forDate           = forDate,
                              forTime           = forTime,
                              isBentoLink       = isBentoLink,
                              isSpiegelPlusLink = isSpiegelPlusLink,
                              intro             = intro,
                              title             = title,
                              section           = section,
                              link              = link)
   dfHeadline
}



#' Class spgl.dt.lsFileStr
#'
#' XXX
#' @name spgl.dt.lsFileStr
#' @rdname spgl.dt.lsFileStr
#' @exportClass spgl.dt.lsFileStr
#' @slot dtDlDir dtDlDir
#' @slot dtHeadlineFile dtHeadlineFile
#' @slot dtHeadline dtHeadline
#' @author Christian Frei
#' @details XXX
#' @seealso spgl.dt.dtDlDir, spgl.dt.dtHeadlineFile, spgl.dt.dtHeadline
#' @export
#' @import data.table
spgl.dt.lsFileStr <- setClass("spgl.dt.lsFileStr",
                              representation(
                                 dtDlDir        = "data.table",
                                 dtHeadlineFile = "data.table",
                                 dtHeadline     = "data.table"),
                              prototype = prototype(
                                 dtDlDir        = spgl.dt.dtDlDir(),
                                 dtHeadlineFile = spgl.dt.dtHeadlineFile(),
                                 dtHeadline     = spgl.dt.dtHeadline()),
                              validity = function(object) {
                                 if(ncol(object@dtDlDir) != ncol(spgl.dt.dtDlDir())) {
                                    return("object@dtDlDir: Wrong number of columns")
                                 }
                                 if(all(colnames(object@dtDlDir) != colnames(spgl.dt.dtDlDir()))) {
                                    return("object@dtDlDir: Wrong column names or wrong order")
                                 }
                                 result <- all.equal(sapply(spgl.dt.dtDlDir(),
                                                            class),
                                                     sapply(object@dtDlDir,
                                                            class))
                                 if(result != TRUE) {
                                    return(paste("object@dtDlDir: Wrong column class/n", result))
                                 }
                                 if(ncol(object@dtHeadlineFile) != ncol(spgl.dt.dtHeadlineFile())) {
                                    return("object@dtHeadlineFile: Wrong number of columns")
                                 }
                                 if(all(colnames(object@dtHeadlineFile) != colnames(spgl.dt.dtHeadlineFile()))) {
                                    return("object@dtHeadlineFile: Wrong column names or wrong order")
                                 }
                                 result <- all.equal(sapply(spgl.dt.dtHeadlineFile(),
                                                            class),
                                                     sapply(object@dtHeadlineFile,
                                                            class))
                                 if(result != TRUE) {
                                    return(paste("object@dtHeadlineFile: Wrong column class/n", result))
                                 }
                                 if(ncol(object@dtHeadline) != ncol(spgl.dt.dtHeadline())) {
                                    return("object@dtHeadline: Wrong number of columns")
                                 }
                                 if(all(colnames(object@dtHeadline) != colnames(spgl.dt.dtHeadline()))) {
                                    return("object@dtHeadline: Wrong column names or wrong order")
                                 }
                                 result <- all.equal(sapply(spgl.dt.dtHeadline(),
                                                            class),
                                                     sapply(object@dtHeadline,
                                                            class))
                                 if(result != TRUE) {
                                    return(paste("object@dtHeadline: Wrong column class/n", result))
                                 }
                                 return(TRUE)
                              }
)

#' spgl.dt.lsFileStr
#'
#' Constructor of spgl.dt.lsFileStr Class
#' @rdname spgl.dt.lsFileStr
#' @param .Object .Object
#' @author Christian Frei
#' @details XXX
setMethod("initialize",
          "spgl.dt.lsFileStr",
          function(.Object) {
             .Object <- callNextMethod()
             .Object
         })



#' spgl.dt.lsFileStr.getDtDlDir
#'
#' create a method to get the value of the dtDlDir
#' @param lsFileStr spgl.dt.lsFileStr
#' @author Christian Frei
#' @details XXX
#' @examples
#' lsFileStr <- new("spgl.dt.lsFileStr")
#' print(spgl.dt.lsFileStr.getDtDlDir(lsFileStr))
#' @export
setGeneric(name="spgl.dt.lsFileStr.getDtDlDir",
           def=function(lsFileStr)
           {
              standardGeneric("spgl.dt.lsFileStr.getDtDlDir")
           }
)


#' @rdname spgl.dt.lsFileStr.getDtDlDir
#' @export
setMethod(f="spgl.dt.lsFileStr.getDtDlDir",
          signature="spgl.dt.lsFileStr",
          definition=function(lsFileStr)
          {
             return(lsFileStr@dtDlDir)
          }
)


#' spgl.dt.lsFileStr.setDtDlDir
#'
#' create a method to get the value of the dtDlDir
#' @param lsFileStr spgl.dt.lsFileStr.setDtDlDir
#' @param dtDlDir data.table
#' @return spgl.dt.lsFileStr spgl.dt.lsFileStr
#' @author Christian Frei
#' @details XXX
#' @examples
#' lsFileStr <- new("spgl.dt.lsFileStr")
#' dtDlDir <- spgl.dt.dtDlDir(id = as.integer(1),
#'                            forDate = as_date(today()),
#'                            dlDir = "a",
#'                            dlDir.fllPath = "b")
#' lsFileStr <- spgl.dt.lsFileStr.setDtDlDir(lsFileStr, dtDlDir)
#' print(spgl.dt.lsFileStr.getDtDlDir(lsFileStr))
#' @export
setGeneric(name="spgl.dt.lsFileStr.setDtDlDir",
           def=function(lsFileStr, dtDlDir)
           {
              standardGeneric("spgl.dt.lsFileStr.setDtDlDir")
           }
)


#' @rdname spgl.dt.lsFileStr.setDtDlDir
#' @export
setMethod(f="spgl.dt.lsFileStr.setDtDlDir",
          signature=c("spgl.dt.lsFileStr", "data.table"),
          definition=function(lsFileStr, dtDlDir)
          {
             lsFileStr@dtDlDir <- dtDlDir
             validObject(lsFileStr)
             return(lsFileStr)
          }
)

#' spgl.dt.lsFileStr.addDtDlDir
#'
#' create a method to get the value of the dtDlDir
#' @param lsFileStr spgl.dt.lsFileStr.setDtDlDir
#' @param dtDlDir data.table
#' @return spgl.dt.lsFileStr spgl.dt.lsFileStr
#' @author Christian Frei
#' @details XXX
#' @examples
#' lsFileStr <- new("spgl.dt.lsFileStr")
#' dtDlDir1 <- spgl.dt.dtDlDir(id = as.integer(1),
#'                            forDate = as_date(today()),
#'                            dlDir = "a",
#'                            dlDir.fllPath = "b")
#' lsFileStr <- spgl.dt.lsFileStr.setDtDlDir(lsFileStr, dtDlDir1)
#' dtDlDir2 <- spgl.dt.dtDlDir(id = as.integer(2),
#'                            forDate = as_date(today())-years(1),
#'                            dlDir = "y",
#'                            dlDir.fllPath = "z")
#' lsFileStr <- spgl.dt.lsFileStr.addDtDlDir(lsFileStr, dtDlDir2)
#' print(spgl.dt.lsFileStr.getDtDlDir(lsFileStr))
setGeneric(name="spgl.dt.lsFileStr.addDtDlDir",
           def=function(lsFileStr, dtDlDir)
           {
              standardGeneric("spgl.dt.lsFileStr.addDtDlDir")
           }
)


#' @rdname spgl.dt.lsFileStr.addDtDlDir
#' @export
setMethod(f="spgl.dt.lsFileStr.addDtDlDir",
          signature=c("spgl.dt.lsFileStr", "data.table"),
          definition=function(lsFileStr, dtDlDir)
          {
             lsFileStr@dtDlDir <- rbindlist(list(lsFileStr@dtDlDir,
                                                 dtDlDir))
             validObject(lsFileStr)
             return(lsFileStr)
          }
)


#' spgl.dt.lsFileStr.getDtHeadlineFile
#'
#' XXX
#' @param lsFileStr spgl.dt.lsFileStr
#' @return spgl.dt.lsFileStr
#' @author Christian Frei
#' @details XXX
#' @examples
#' lsFileStr <- new("spgl.dt.lsFileStr")
#' print(spgl.dt.lsFileStr.getDtHeadlineFile(lsFileStr))
#' @export
setGeneric(name="spgl.dt.lsFileStr.getDtHeadlineFile",
           def=function(lsFileStr)
           {
              standardGeneric("spgl.dt.lsFileStr.getDtHeadlineFile")
           }
)


#' @rdname spgl.dt.lsFileStr.getDtHeadlineFile
#' @export
setMethod(f="spgl.dt.lsFileStr.getDtHeadlineFile",
          signature="spgl.dt.lsFileStr",
          definition=function(lsFileStr)
          {
             return(lsFileStr@dtHeadlineFile)
          }
)

#' spgl.dt.lsFileStr.setDtHeadlineFile
#'
#' XXX
#' @param lsFileStr spgl.dt.lsFileStr
#' @param dtHeadlineFile dtHeadlineFile
#' @return spgl.dt.lsFileStr spgl.dt.lsFileStr
#' @author Christian Frei
#' @details XXX
#' @examples
#' lsFileStr <- new("spgl.dt.lsFileStr")
#' dtHeadlineFile <- spgl.dt.dtHeadlineFile(id                   = integer(1),
#'                                          idDlDir              = integer(1),
#'                                          forDate              = lubridate::as_date(today()),
#'                                          inDate               = lubridate::as_date(today()),
#'                                          inTime               = lubridate::as_datetime(now()),
#'                                          dlDir                = "a",
#'                                          dlDir.fllPath        = "b",
#'                                          rawDir               = "c",
#'                                          rawDir.fllPath       = "d",
#'                                          fileHTML             = "e",
#'                                          fileHTML.fllPath     = "f",
#'                                          fileCSV              = "g",
#'                                          fileCSV.fllPath      = "h")
#' lsFileStr <- spgl.dt.lsFileStr.setDtHeadlineFile(lsFileStr, dtHeadlineFile)
#' print(spgl.dt.lsFileStr.getDtHeadlineFile(lsFileStr))
#' @export
setGeneric(name="spgl.dt.lsFileStr.setDtHeadlineFile",
           def=function(lsFileStr, dtHeadlineFile)
           {
              standardGeneric("spgl.dt.lsFileStr.setDtHeadlineFile")
           }
)

#' @rdname spgl.dt.lsFileStr.setDtHeadlineFile
#' @export
setMethod(f="spgl.dt.lsFileStr.setDtHeadlineFile",
          signature="spgl.dt.lsFileStr",
          definition=function(lsFileStr, dtHeadlineFile)
          {
             lsFileStr@dtHeadlineFile <- dtHeadlineFile
             validObject(lsFileStr)
             return(lsFileStr)
          }
)



#' spgl.dt.lsFileStr.addDtHeadlineFile
#'
#' create a method to get the value of the dtDlDir
#' @param lsFileStr spgl.dt.lsFileStr.setDtDlDir
#' @param dtHeadlineFile data.table
#' @return spgl.dt.lsFileStr spgl.dt.lsFileStr
#' @author Christian Frei
#' @details XXX
#' @examples
#' lsFileStr <- new("spgl.dt.lsFileStr")
#' dtHeadlineFile1 <- spgl.dt.dtHeadlineFile(id                   = as.integer(1),
#'                                           idDlDir              = as.integer(1),
#'                                           forDate              = lubridate::as_date(today()),
#'                                           inDate               = lubridate::as_date(today()),
#'                                           inTime               = lubridate::as_datetime(now()),
#'                                           dlDir                = "a",
#'                                           dlDir.fllPath        = "b",
#'                                           rawDir               = "c",
#'                                           rawDir.fllPath       = "d",
#'                                           fileHTML             = "e",
#'                                           fileHTML.fllPath     = "f",
#'                                           fileCSV              = "g",
#'                                           fileCSV.fllPath      = "h")
#' lsFileStr <- spgl.dt.lsFileStr.setDtHeadlineFile(lsFileStr, dtHeadlineFile1)
#' dtHeadlineFile2 <- spgl.dt.dtHeadlineFile(id                   = as.integer(2),
#'                                           idDlDir              = as.integer(2),
#'                                           forDate              = lubridate::as_date(today()-years(1)),
#'                                           inDate               = lubridate::as_date(today()-years(1)),
#'                                           inTime               = lubridate::as_datetime(now()-years(1)),
#'                                           dlDir                = "g",
#'                                           dlDir.fllPath        = "h",
#'                                           rawDir               = "i",
#'                                           rawDir.fllPath       = "j",
#'                                           fileHTML             = "k",
#'                                           fileHTML.fllPath     = "l",
#'                                           fileCSV              = "m",
#'                                           fileCSV.fllPath      = "n")
#' lsFileStr <- spgl.dt.lsFileStr.addDtHeadlineFile(lsFileStr, dtHeadlineFile2)
#' print(spgl.dt.lsFileStr.getDtHeadlineFile(lsFileStr))
setGeneric(name="spgl.dt.lsFileStr.addDtHeadlineFile",
           def=function(lsFileStr, dtHeadlineFile)
           {
              standardGeneric("spgl.dt.lsFileStr.addDtHeadlineFile")
           }
)


#' @rdname spgl.dt.lsFileStr.addDtHeadlineFile
#' @export
setMethod(f="spgl.dt.lsFileStr.addDtHeadlineFile",
          signature=c("spgl.dt.lsFileStr", "data.table"),
          definition=function(lsFileStr, dtHeadlineFile)
          {
             lsFileStr@dtHeadlineFile <- rbindlist(list(lsFileStr@dtHeadlineFile,
                                                 dtHeadlineFile))
             validObject(lsFileStr)
             return(lsFileStr)
          }
)



#' spgl.dt.lsFileStr.setDtHeadline
#'
#' XXX
#' @param lsFileStr spgl.dt.lsFileStr
#' @param dtHeadline data.table
#' @return spgl.dt.lsFileStr
#' @author Christian Frei
#' @details XXX
#' @examples
#' lsFileStr <- new("spgl.dt.lsFileStr")
#' dtHeadline <- spgl.dt.dtHeadline(id = as.integer(1),
#'                                  idHeadline = as.integer(2),
#'                                  idHeadlineFile = as.integer(3),
#'                                  forDate = as_date(today()),
#'                                  forTime = as_datetime(now()),
#'                                  isBentoLink = FALSE,
#'                                  isSpiegelPlusLink = FALSE,
#'                                  intro = "a",
#'                                  title = "b",
#'                                  section = "c",
#'                                  link = "d")
#' lsFileStr <- spgl.dt.lsFileStr.setDtHeadline(lsFileStr, dtHeadline)
#' spgl.dt.lsFileStr.getDtHeadline(lsFileStr)
#' @export
setGeneric(name="spgl.dt.lsFileStr.setDtHeadline",
           def=function(lsFileStr, dtHeadline)
           {
              standardGeneric("spgl.dt.lsFileStr.setDtHeadline")
           }
)


#' @rdname spgl.dt.lsFileStr.setDtHeadline
#' @export
setMethod(f="spgl.dt.lsFileStr.setDtHeadline",
          signature=c("spgl.dt.lsFileStr", "data.table"),
          definition=function(lsFileStr, dtHeadline)
          {
             lsFileStr@dtHeadline <- dtHeadline
             validObject(lsFileStr)
             return(lsFileStr)
          }
)


#' spgl.dt.lsFileStr.getDtHeadline
#'
#' XXX
#' @param lsFileStr spgl.dt.lsFileStr
#' @return spgl.dt.lsFileStr
#' @author Christian Frei
#' @details XXX
#' @examples
#' lsFileStr <- new("spgl.dt.lsFileStr")
#' dtHeadline <- spgl.dt.dtHeadline(id = as.integer(1),
#'                                  idHeadline = as.integer(2),
#'                                  idHeadlineFile = as.integer(3),
#'                                  forDate = as_date(today()),
#'                                  forTime = as_datetime(now()),
#'                                  isBentoLink = FALSE,
#'                                  isSpiegelPlusLink = FALSE,
#'                                  intro = "a",
#'                                  title = "b",
#'                                  section = "c",
#'                                  link = "d")
#' lsFileStr <- spgl.dt.lsFileStr.setDtHeadline(lsFileStr, dtHeadline)
#' spgl.dt.lsFileStr.getDtHeadline(lsFileStr)
#' @export
setGeneric(name="spgl.dt.lsFileStr.getDtHeadline",
           def=function(lsFileStr)
           {
              standardGeneric("spgl.dt.lsFileStr.getDtHeadline")
           }
)


#' @rdname spgl.dt.lsFileStr.getDtHeadline
#' @export
setMethod(f="spgl.dt.lsFileStr.getDtHeadline",
          signature="spgl.dt.lsFileStr",
          definition=function(lsFileStr)
          {
             return(lsFileStr@dtHeadline)
          }
)


#' spgl.dt.lsFileStr.addDtHeadline
#'
#' xxx
#' @param lsFileStr spgl.dt.lsFileStr
#' @param dtHeadline dtHeadline
#' @return spgl.dt.lsFileStr
#' @author Christian Frei
#' @details XXX
#' @examples
#' lsFileStr <- new("spgl.dt.lsFileStr")
#' dtHeadline1 <- spgl.dt.dtHeadline(id = as.integer(1),
#'                                   idHeadline = as.integer(2),
#'                                   idHeadlineFile = as.integer(3),
#'                                   forDate = as_date(today()),
#'                                   forTime = as_datetime(now()),
#'                                   isBentoLink = FALSE,
#'                                   isSpiegelPlusLink = FALSE,
#'                                   intro = "a",
#'                                   title = "b",
#'                                   section = "c",
#'                                   link = "d")
#' lsFileStr <- spgl.dt.lsFileStr.setDtHeadline(lsFileStr, dtHeadline1)
#' dtHeadline2 <- spgl.dt.dtHeadline(id = as.integer(4),
#'                                   idHeadline = as.integer(5),
#'                                   idHeadlineFile = as.integer(6),
#'                                   forDate = as_date(today()-years(1)),
#'                                   forTime = as_datetime(now()-years(1)),
#'                                   isBentoLink = FALSE,
#'                                   isSpiegelPlusLink = FALSE,
#'                                   intro = "e",
#'                                   title = "f",
#'                                   section = "g",
#'                                   link = "h")
#' lsFileStr <- spgl.dt.lsFileStr.addDtHeadline(lsFileStr, dtHeadline2)
#' print(spgl.dt.lsFileStr.getDtHeadline(lsFileStr))
setGeneric(name="spgl.dt.lsFileStr.addDtHeadline",
           def=function(lsFileStr, dtHeadline)
           {
              standardGeneric("spgl.dt.lsFileStr.addDtHeadline")
           }
)


#' @rdname spgl.dt.lsFileStr.addDtHeadline
#' @export
setMethod(f="spgl.dt.lsFileStr.addDtHeadline",
          signature=c("spgl.dt.lsFileStr", "data.table"),
          definition=function(lsFileStr, dtHeadline)
          {
             lsFileStr@dtHeadline <- rbindlist(list(lsFileStr@dtHeadline,
                                                        dtHeadline))
             validObject(lsFileStr)
             return(lsFileStr)
          }
)




# BACKUP
#'
#' #' spgl.dt.dtDlDir
#' #'
#' #' This function creates an object of spgl.dt.dtDlDir.
#' #' @name spgl.dt.dtDlDir
#' #' @param id integer
#' #' @param forDate as_date(character())
#' #' @param dlDir character
#' #' @param dlDir.fllPath character
#' #' @return data.table
#' #' @author Christian Frei
#' #' @details XXX
#' #' @examples
#' #' dtDlDir1 <- spgl.dt.dtDlDir()
#' #' print(dtDlDir1)
#' #'
#' #' dtDlDir2 <- spgl.dt.dtDlDir(id = as.integer(1),
#' #'                            forDate = as_date(today()),
#' #'                            dlDir = "a",
#' #'                            dlDir.fllPath = "b")
#' #' print(dtDlDir2)
#' #' @export
#' #' @import data.table
#' #' @importFrom lubridate as_date
#' setGeneric(name="spgl.dt.dtDlDir",
#'            def=function(id,
#'                         forDate,
#'                         dlDir,
#'                         dlDir.fllPath)
#'            {
#'               standardGeneric("spgl.dt.dtDlDir")
#'            }
#' )
#'
#'
#' #' @docType methods
#' #' @rdname spgl.dt.dtDlDir
#' #' @export
#' setMethod(f="spgl.dt.dtDlDir",
#'           signature=signature(id = "missing", forDate = "missing",
#'                               dlDir = "missing", dlDir.fllPath = "missing"),
#'           definition=function(id,
#'                               forDate,
#'                               dlDir,
#'                               dlDir.fllPath)
#'           {
#'              dtDlDir <- data.table::data.table(
#'                 id            = integer(),
#'                 forDate       = as_date(character()),
#'                 dlDir         = character(),
#'                 dlDir.fllPath = character())
#'              dtDlDir
#'           }
#' )
#'
#'
#' #' @docType methods
#' #' @rdname spgl.dt.dtDlDir
#' #' @export
#' setMethod(f="spgl.dt.dtDlDir",
#'           signature=signature(id = "integer", forDate = "Date",
#'                               dlDir = "character", dlDir.fllPath = "character"),
#'           definition=function(id            = integer(),
#'                               forDate       = as_date(character()),
#'                               dlDir         = character(),
#'                               dlDir.fllPath = character())
#'           {
#'              dtDlDir <- data.table::data.table(
#'                 id            = id,
#'                 forDate       = forDate,
#'                 dlDir         = dlDir,
#'                 dlDir.fllPath = dlDir.fllPath)
#'              dtDlDir
#'           }
#' )
#'
#'
#'
#' #' spgl.dt.dtHeadlineFile
#' #'
#' #' This function creates an object of spgl.dt.dtHeadlineFile.
#' #' @name spgl.dt.dtHeadlineFile
#' #' @param id integer
#' #' @param idDlDir integer
#' #' @param forDate POSIXt or character
#' #' @param inDate POSIXt or character
#' #' @param inTime POSIXt or character
#' #' @param dlDir character
#' #' @param dlDir.fllPath character
#' #' @param rawDir character
#' #' @param rawDir.fllPath character
#' #' @param fileHTML character
#' #' @param fileHTML.fllPath character
#' #' @param fileCSV character
#' #' @param fileCSV.fllPath character
#' #' @return data.table
#' #' @author Christian Frei
#' #' @details XXX
#' #' @examples
#' #' dtHeadlineFile1 <- spgl.dt.dtHeadlineFile()
#' #' print(dtHeadlineFile1)
#' #'
#' #' dtHeadlineFile2 <- spgl.dt.dtHeadlineFile(id                   = as.integer(1),
#' #'                                           idDlDir              = as.integer(1),
#' #'                                           forDate              = lubridate::as_date(today()),
#' #'                                           inDate               = lubridate::as_date(today()),
#' #'                                           inTime               = lubridate::as_datetime(now()),
#' #'                                           dlDir                = "a",
#' #'                                           dlDir.fllPath        = "b",
#' #'                                           rawDir               = "c",
#' #'                                           rawDir.fllPath       = "d",
#' #'                                           fileHTML             = "e",
#' #'                                           fileHTML.fllPath     = "f",
#' #'                                           fileCSV              = "g",
#' #'                                           fileCSV.fllPath      = "h")
#' #' print(dtHeadlineFile2)
#' #' @export
#' #' @import data.table
#' #' @importFrom lubridate as_date
#' #' @importFrom lubridate as_datetime
#' setGeneric(name="spgl.dt.dtHeadlineFile",
#'            def=function(id,
#'                         idDlDir,
#'                         forDate,
#'                         inDate,
#'                         inTime,
#'                         dlDir,
#'                         dlDir.fllPath,
#'                         rawDir,
#'                         rawDir.fllPath,
#'                         fileHTML,
#'                         fileHTML.fllPath,
#'                         fileCSV,
#'                         fileCSV.fllPath)
#'            {
#'               standardGeneric("spgl.dt.dtHeadlineFile")
#'            }
#' )
#'
#'
#' #' @docType methods
#' #' @rdname spgl.dt.dtHeadlineFile
#' #' @export
#' setMethod(f="spgl.dt.dtHeadlineFile",
#'           signature=signature(id = "missing", idDlDir = "missing", forDate = "missing",
#'                               inDate = "missing", inTime = "missing", dlDir = "missing",
#'                               dlDir.fllPath = "missing", rawDir = "missing",
#'                               rawDir.fllPath = "missing", fileHTML = "missing",
#'                               fileHTML.fllPath = "missing", fileCSV = "missing",
#'                               fileCSV.fllPath = "missing"),
#'           definition=function(id,
#'                               idDlDir,
#'                               forDate,
#'                               inDate,
#'                               inTime,
#'                               dlDir,
#'                               dlDir.fllPath,
#'                               rawDir,
#'                               rawDir.fllPath,
#'                               fileHTML,
#'                               fileHTML.fllPath,
#'                               fileCSV,
#'                               fileCSV.fllPath)
#'           {
#'              dtHeadlineFile <- data.table::data.table(
#'                 id               = integer(),
#'                 idDlDir          = integer(),
#'                 forDate          = lubridate::as_date(character()),
#'                 inDate           = lubridate::as_date(character()),
#'                 inTime           = lubridate::as_datetime(character()),
#'                 dlDir            = character(),
#'                 dlDir.fllPath    = character(),
#'                 rawDir           = character(),
#'                 rawDir.fllPath   = character(),
#'                 fileHTML         = character(),
#'                 fileHTML.fllPath = character(),
#'                 fileCSV          = character(),
#'                 fileCSV.fllPath  = character())
#'              dtHeadlineFile
#'           }
#' )
#'
#'
#'
#' #' @docType methods
#' #' @rdname spgl.dt.dtHeadlineFile
#' #' @export
#' setMethod(f="spgl.dt.dtHeadlineFile",
#'           signature=signature(id = "integer", idDlDir = "integer", forDate = "Date",
#'                               inDate = "Date", inTime = "POSIXt", dlDir = "character",
#'                               dlDir.fllPath = "character", rawDir = "character",
#'                               rawDir.fllPath = "character", fileHTML = "character",
#'                               fileHTML.fllPath = "character", fileCSV = "character",
#'                               fileCSV.fllPath = "character"),
#'           definition=function(id               = integer(),
#'                               idDlDir          = integer(),
#'                               forDate          = lubridate::as_date(character()),
#'                               inDate           = lubridate::as_date(character()),
#'                               inTime           = lubridate::as_datetime(character()),
#'                               dlDir            = character(),
#'                               dlDir.fllPath    = character(),
#'                               rawDir           = character(),
#'                               rawDir.fllPath   = character(),
#'                               fileHTML         = character(),
#'                               fileHTML.fllPath = character(),
#'                               fileCSV          = character(),
#'                               fileCSV.fllPath  = character())
#'           {
#'              dtHeadlineFile <- data.table::data.table(
#'                 id                   = id,
#'                 idDlDir              = idDlDir,
#'                 forDate              = lubridate::as_date(forDate),
#'                 inDate               = lubridate::as_date(inDate),
#'                 inTime               = lubridate::as_datetime(inTime),
#'                 dlDir                = dlDir,
#'                 dlDir.fllPath        = dlDir.fllPath,
#'                 rawDir               = rawDir,
#'                 rawDir.fllPath       = rawDir.fllPath,
#'                 fileHTML             = fileHTML,
#'                 fileHTML.fllPath     = fileHTML.fllPath,
#'                 fileCSV              = fileCSV,
#'                 fileCSV.fllPath      = fileCSV.fllPath)
#'              dtHeadlineFile
#'           }
#' )
#'
#'
#' #' spgl.dt.dtHeadline
#' #'
#' #' This function creates an object of spgl.dt.dtHeadline.
#' #' @name spgl.dt.dtHeadline
#' #' @param id integer
#' #' @param idHeadline integer
#' #' @param idHeadlineFile integer
#' #' @param forDate POSIXt or character
#' #' @param forTime POSIXt or character
#' #' @param isBentoLink logical
#' #' @param isSpiegelPlusLink logical
#' #' @param intro character
#' #' @param title character
#' #' @param section character
#' #' @param link character
#' #' @return data.table
#' #' @author Christian Frei
#' #' @examples
#' #' dtHeadline1 <- spgl.dt.dtHeadline()
#' #' print(dtHeadline1)
#' #'
#' #' dtHeadline2 <- spgl.dt.dtHeadline(id = as.integer(1),
#' #'                                   idHeadline = as.integer(2),
#' #'                                   idHeadlineFile = as.integer(3),
#' #'                                   forDate = as_date(today()),
#' #'                                   forTime = as_datetime(now()),
#' #'                                   isBentoLink = FALSE,
#' #'                                   isSpiegelPlusLink = FALSE,
#' #'                                   intro = "a",
#' #'                                   title = "b",
#' #'                                   section = "c",
#' #'                                   link = "d")
#' #' print(dtHeadline2)
#' #' @export
#' #' @import data.table
#' #' @importFrom lubridate as_date
#' #' @importFrom lubridate as_datetime
#' setGeneric(name="spgl.dt.dtHeadline",
#'            def=function(id,
#'                         idHeadline,
#'                         idHeadlineFile,
#'                         forDate,
#'                         forTime,
#'                         isBentoLink,
#'                         isSpiegelPlusLink,
#'                         intro,
#'                         title,
#'                         section,
#'                         link)
#'            {
#'               standardGeneric("spgl.dt.dtHeadline")
#'            }
#' )
#'
#'
#' #' @docType methods
#' #' @rdname spgl.dt.dtHeadline
#' #' @export
#' setMethod(f="spgl.dt.dtHeadline",
#'           signature=signature(id = "missing", idHeadline="missing", idHeadlineFile="missing", forDate="missing", forTime="missing",
#'                               isBentoLink="missing", isSpiegelPlusLink="missing", intro="missing", title="missing", section="missing",
#'                               link="missing"),
#'           definition=function(id,
#'                               idHeadline,
#'                               idHeadlineFile,
#'                               forDate,
#'                               forTime,
#'                               isBentoLink,
#'                               isSpiegelPlusLink,
#'                               intro,
#'                               title,
#'                               section,
#'                               link)
#'           {
#'              dtHeadline <- data.table::data.table(
#'                 id                = integer(),
#'                 idHeadline        = integer(),
#'                 idHeadlineFile    = integer(),
#'                 forDate           = lubridate::as_date(character()),
#'                 forTime           = lubridate::as_datetime(character()),
#'                 isBentoLink       = logical(),
#'                 isSpiegelPlusLink = logical(),
#'                 intro             = character(),
#'                 title             = character(),
#'                 section           = character(),
#'                 link              = character())
#'              dtHeadline
#'           }
#' )
#'
#'
#' #' @docType methods
#' #' @rdname spgl.dt.dtHeadline
#' #' @export
#' setMethod(f="spgl.dt.dtHeadline",
#'           signature=signature(id = "integer", idHeadline="integer", idHeadlineFile="integer", forDate="Date", forTime="POSIXt",
#'                               isBentoLink="logical", isSpiegelPlusLink="logical", intro="character", title="character", section="character",
#'                               link="character"),
#'           definition=function(id = integer(),
#'                               idHeadline= integer(),
#'                               idHeadlineFile= integer(),
#'                               forDate = lubridate::as_date(character()),
#'                               forTime = lubridate::as_datetime(character()),
#'                               isBentoLink = logical(),
#'                               isSpiegelPlusLink = logical(),
#'                               intro = character(),
#'                               title = character(),
#'                               section = character(),
#'                               link = character())
#'           {
#'              dtHeadline <- data.table::data.table(
#'                 id                = as.integer(id),
#'                 idHeadline        = as.integer(idHeadline),
#'                 idHeadlineFile    = as.integer(idHeadlineFile),
#'                 forDate           = lubridate::as_date(forDate),
#'                 forTime           = lubridate::as_datetime(forTime),
#'                 isBentoLink       = as.logical(isBentoLink),
#'                 isSpiegelPlusLink = as.logical(isSpiegelPlusLink),
#'                 intro             = as.character(intro),
#'                 title             = as.character(title),
#'                 section           = as.character(section),
#'                 link              = as.character(link))
#'              dtHeadline
#'           }
#' )

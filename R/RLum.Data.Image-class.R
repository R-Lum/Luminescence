#' @include get_RLum.R set_RLum.R
NULL

#' Class \code{"RLum.Data.Image"}
#'
#' Class for luminescence image data (TL/OSL/RF).
#'
#'
#' @name RLum.Data.Image-class
#' @docType class
#' @note The class should only contain data for a set of images. For additional
#' elements the slot \code{info} can be used.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RLum.Data.Image", ...)}.
#' @author Sebastian Kreutzer, Universite Bordeaux Montaigne (France)
#' @seealso \code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data}},
#' \code{\link{plot_RLum}}
#' @references #
#' @keywords classes
#' @examples
#'
#' showClass("RLum.Data.Image")
#'
#' ##so far no further example available
#'
setClass("RLum.Data.Image",
         representation(
           recordType = "character",
           curveType = "character",
           data = "RasterBrick",
           info = "list"
         ),
         contains = "RLum.Data",
         prototype = list (
           recordType = character(),
           curveType = character(),
           data = brick(raster()),
           info = list()
         ),
         S3methods=TRUE)


# setAs - coerce methods ------------------------------------------------------

##note: This conversion will not work for multi layers!
##----------------------------------------------
##COERCE FROM AND TO data.frame

setAs("data.frame", "RLum.Data.Image",
      function(from,to){
        
        new(to,
            recordType = "unkown curve type",
            curveType = "NA",
            data = as.matrix(from),
            info = list())
      })

setAs("RLum.Data.Image", "data.frame",
      function(from){
        
        data.frame(x = from@data@values[seq(1,length(from@data@values), by = 2)],
                   y = from@data@values[seq(2,length(from@data@values), by = 2)])
        
      })


# ##----------------------------------------------
##COERCE FROM AND TO matrix

setAs("matrix", "RLum.Data.Image",
      function(from,to){
        
        new(to,
            recordType = "unkown curve type",
            curveType = "NA",
            data = brick(raster(as.matrix(from))),
            info = list())
      })

setAs("RLum.Data.Image", "matrix",
      function(from){
        
        ##only the first object is convertec
        as.matrix(from[[1]])
        
      })




# show method for object ------------------------------------------------------

setMethod("show",
          signature(object = "RLum.Data.Image"),
          function(object){
            
            x.rows <- object@data@ncols
            y.cols <- object@data@nrows
            z.range <- paste(min(object@data@data@min),":",max(object@data@data@max))
            
            ##print information
            
            cat("\n [RLum.Data.Image]")
            cat("\n\t recordType:", object@recordType)
            cat("\n\t curveType:",  object@curveType)
            cat("\n\t .. recorded frames:", length(object@data@data@names))
            cat("\n\t .. .. pixel per frame:", x.rows*y.cols)
            cat("\n\t .. .. x dimension [px]:", x.rows)
            cat("\n\t .. .. y dimension [px]:", y.cols)
            cat("\n\t .. .. full pixel value range:", z.range)
            cat("\n\t additional info elements:", length(object@info))
            #cat("\n\t\t >> names:", names(object@info))
          }
)


# # constructor (set) method for object class -----------------------------------

#' @describeIn RLum.Data.Image
#' Construction method for \code{RLum.Results} object.
#' The slot \code{originator} is optional and predefined as the function 
#' that calls the function \code{set_RLum.Results}. 
#'  
#' @param class x
#' @param recordType x
#' @param curveType x
#' @param data x
#' @param info x
setMethod("set_RLum",
          signature = signature("RLum.Data.Image"), 
          
          definition = function(class, recordType, curveType, data, info){
            
            ##check for missing curveType
            if(missing(curveType)==TRUE){
              
              curveType <- "NA"
              
            }else if (is(curveType, "character") == FALSE){
              
              stop("[set_RLum.Data.Image()] Error: 'curveType' has to be of type 'character'!")
              
            }
            
            ##check for missing arguments
            if(missing(recordType) | missing(data)){
              
              temp.error.missing <- paste(c(
                
                if(missing(recordType)){"'recordType'"}else{},
                if(missing(data)){"'data'"}else{}),
                collapse=", ")
              
              ##set error message
              temp.error.message <- paste("[set_RLum.Data.Image()] Missing required arguments " ,
                                          temp.error.missing,"!", sep="")
              stop(temp.error.message)
            }
            
            ##handle missing info argument
            if(missing(info)){
              
              info <- list()
              
            }else if (is(info, "list") == FALSE){
              
              stop("[set_RLum.Data.Image()] 'info' has to be of type 'list'!")
              
            }
            
            new("RLum.Data.Image",
                recordType = recordType,
                curveType = curveType,
                data = data,
                info = info)
            
          })

# constructor (get) method for object class -----------------------------------

#' @describeIn RLum.Data.Image
#' The argument \code{data.object} allows directly accessing
#' objects delivered within the slot \code{data}. If no \code{data.object} is 
#' specified, a preselected object is returned. The default return
#' object depends on the object originator (e.g. \code{fit_LMCurve})
#'  
#' @param object x
#' @param info.object x
setMethod("get_RLum",
          signature("RLum.Data.Image"),
          definition = function(object, info.object) {
            
            ##Check if function is of type RLum.Data.Image
            if(is(object, "RLum.Data.Image") == FALSE){
              
              stop("[get_RLum.Data.Image] Function valid for 'RLum.Data.Image' objects only!")
              
            }
            
            ##if missing info.object just show the curve values
            
            if(missing(info.object) == FALSE){
              
              if(is(info.object, "character") == FALSE){
                stop("[get_RLum.Data.Image] Error: 'info.object' has to be a character!")
              }
              
              if(info.object %in% names(object@info) == TRUE){
                
                unlist(object@info[info.object])
                
              }else{
                
                ##grep names
                temp.element.names <- paste(names(object@info), collapse = ", ")
                
                stop.text <- paste("[get_RLum.Data.Image] Error: Invalid element name. Valid names are:", temp.element.names)
                
                stop(stop.text)
                
              }
              
              
            }else{
              
              object@data
              
            }
          })

##//////////////////////////////////////////////////////////////////////////////
##//RLum.Data.Image-class.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
##author: Sebastian Kreutzer
##organisation: Universite Bordeaux Montaigne (France)
##version: 0.1
##date: 2014-07-21
##==============================================================================
##[SK]  The curve type slot is intentionally named 'recordType' against
##      the internal naming conventions.


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

setGeneric("set_RLum.Data.Image",
           function(recordType, curveType, data, info) {standardGeneric("set_RLum.Data.Image")})


setMethod("set_RLum.Data.Image",
            signature = c(recordType = "ANY", curveType = "ANY", data = "ANY", info = "ANY"),

            function(recordType, curveType, data, info){

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

setGeneric("get_RLum.Data.Image",
           function(object, info.object) {standardGeneric("get_RLum.Data.Image")})

setMethod("get_RLum.Data.Image",
          signature(object="ANY", info.object = "ANY"),
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

#' Merge function for RLum.Analysis S4 class objects
#'
#' Function allows merging of RLum.Analysis objects and adding of allowed
#' objects to an RLum.Analysis.
#'
#' This function simply allowing to merge [RLum.Analysis-class]
#' objects.  Additionally other [RLum-class] objects can be added
#' to an existing [RLum.Analysis-class] object. Supported objects
#' to be added are: [RLum.Data.Curve-class],
#' [RLum.Data.Spectrum-class] and
#' [RLum.Data.Image-class].
#'
#' The order in the new [RLum.Analysis-class] object is the object
#' order provided with the input list.
#'
#' @param objects [list] of [RLum.Analysis-class] (**required**):
#' list of S4 objects of class `RLum.Analysis`. Furthermore other objects of
#' class [RLum-class] can be added, see details.
#'
#' @return Return an [RLum.Analysis-class] object.
#'
#' @note
#' The information for the slot 'protocol' is taken from the first
#' [RLum.Analysis-class] object in the input list. Therefore at
#' least one object of type [RLum.Analysis-class] has to be provided.
#'
#' @section Function version: 0.2.0
#'
#' @author
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso [merge_RLum], [RLum.Analysis-class], [RLum.Data.Curve-class],
#' [RLum.Data.Spectrum-class], [RLum.Data.Image-class], [RLum-class]
#'
#'
#' @keywords utilities internal
#'
#' @examples
#'
#'
#' ##merge different RLum objects from the example data
#' data(ExampleData.RLum.Analysis, envir = environment())
#' data(ExampleData.BINfileData, envir = environment())
#'
#' object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
#' curve <- get_RLum(object)[[2]]
#'
#' temp.merged <- merge_RLum.Analysis(list(curve, IRSAR.RF.Data, IRSAR.RF.Data))
#'
#' @md
#' @export
merge_RLum.Analysis<- function(
  objects
){

  # Ingegrity checks ----------------------------------------------------------------------------

  ##check if object is of class RLum
  temp.class.test <- sapply(1:length(objects), function(x){

    if(is(objects[[x]], "RLum") == FALSE){

      temp.text <- paste("[merge_RLum.Analysis()]: At least element", x, "is not of class 'RLum' or a derivative class!")
      stop(temp.text)
    }




    ##provide class of objects
    is(objects[[x]])[1]

  })

  ##check if at least one object of RLum.Analysis is provided
  if(!"RLum.Analysis"%in%temp.class.test){

    stop("[merge_RLum.Analysis()] At least one input object in the list has to be of class
           'RLum.Analysis'!")

  }



  # Merge objects -------------------------------------------------------------------------------

  ##(0) get recent environment to later set variable temp.meta.data.first
  temp.environment  <- environment()
  temp.meta.data.first <- NA; rm(temp.meta.data.first) #to avoid problems with the R check routine

  ##(1) collect all elements in a list
  temp.element.list <- unlist(lapply(1:length(objects), function(x){

    ##Depending on the element the right functions is used
    if(is(objects[[x]])[1] == "RLum.Analysis"){

      ##grep export meta data from the first RLum.Analysis objects an write
      if(!exists("temp.meta.data.first")){

        assign("temp.meta.data.first", objects[[x]]@protocol, envir = temp.environment)

      }

      ##return to list
      get_RLum(objects[[x]])

    }else if((is(objects[[x]])[1] == "RLum.Data.Curve") |
               (is(objects[[x]])[1] == "RLum.Data.Image") |
               (is(objects[[x]])[1] == "RLum.Data.Spectrum")){

      ##return to list
      objects[[x]]

    }else{

      stop("[merge_RLum.Anlysis()] What ever was provided, this 'RLum' object is not supported!")

    }


  }))


  # Build new RLum.Analysis object --------------------------------------------------------------
  temp.new.RLum.Analysis <- set_RLum(
    class = "RLum.Analysis",
    originator = "merge_RLum.Analysis",
    records = temp.element.list,
    protocol = temp.meta.data.first,
    info = unlist(lapply(objects, function(x) {
      x@info
    }), recursive = FALSE),
    .pid = unlist(lapply(objects, function(x) {
      x@.uid
    }))
    )


  # Return object -------------------------------------------------------------------------------
  return( temp.new.RLum.Analysis)

}

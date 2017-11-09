#' General plot function for RLum S4 class objects
#'
#' Function calls object specific plot functions for RLum S4 class objects.
#'
#' The function provides a generalised access point for plotting specific
#' [RLum-class] objects.\cr
#' Depending on the input object, the
#' corresponding plot function will be selected.  Allowed arguments can be
#' found in the documentations of each plot function.
#'
#' \tabular{lll}{
#' **object** \tab \tab **corresponding plot function** \cr
#' [RLum.Data.Curve-class] \tab : \tab [plot_RLum.Data.Curve] \cr
#' [RLum.Data.Spectrum-class] \tab : \tab [plot_RLum.Data.Spectrum]\cr
#' [RLum.Data.Image-class] \tab : \tab [plot_RLum.Data.Image]\cr
#' [RLum.Analysis-class] \tab : \tab [plot_RLum.Analysis]\cr
#' [RLum.Results-class] \tab : \tab [plot_RLum.Results]
#' }
#'
#' @param object [RLum-class] (**required**):
#' S4 object of class `RLum`. Optional a [list] containing objects of
#' class [RLum-class] can be provided. In this case the function tries to plot
#' every object in this list according to its `RLum` class.
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to the specific plot functions. The only argument that is supported directly is `main`
#' (setting the plot title). In contrast to the normal behaviour `main` can be here provided as
#' [list] and the arguments in the list will dispatched to the plots if the `object`
#' is of type `list` as well.
#'
#' @return Returns a plot.
#'
#' @note The provided plot output depends on the input object.
#'
#' @section Function version: 0.4.3
#'
#' @author
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso [plot_RLum.Data.Curve], [RLum.Data.Curve-class], [plot_RLum.Data.Spectrum],
#' [RLum.Data.Spectrum-class], [plot_RLum.Data.Image], [RLum.Data.Image-class],
#' [plot_RLum.Analysis], [RLum.Analysis-class], [plot_RLum.Results],
#' [RLum.Results-class]
#'
#'
#' @keywords dplot
#'
#' @examples
#'
#' #load Example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' #transform data.frame to RLum.Data.Curve object
#' temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")
#'
#' #plot RLum object
#' plot_RLum(temp)
#'
#'
#' @md
#' @export
plot_RLum<- function(
  object,
  ...
){

  # Define dispatcher function ----------------------------------------------------------

  ##check if object is of class RLum
  RLum.dispatcher <- function(object, ...) {
    if (inherits(object, "RLum")) {

      ##grep object class
      object.class <- is(object)[1]

      ##select which plot function should be used and call it
      switch (
        object.class,

        RLum.Data.Curve = plot_RLum.Data.Curve(object = object, ...),
        RLum.Data.Spectrum = plot_RLum.Data.Spectrum(object = object, ...),
        RLum.Data.Image = plot_RLum.Data.Image(object = object, ...),

        ##here we have to do prevent the partial matching with 'sub' by 'subset'
        RLum.Analysis =
          if(!grepl(pattern = "subset", x = paste(deparse(match.call()), collapse = " "), fixed = TRUE)){
          plot_RLum.Analysis(object = object, subset = NULL, ...)

        }else{
          plot_RLum.Analysis(object = object, ...)

        },

        RLum.Results = plot_RLum.Results(object = object, ...)

      )

    }else{
      stop(paste0(
        "[plot_RLum()] Sorry, I don't know what to do for object of type '", is(object)[1], "'."
      ), call. = FALSE)

    }

  }


  # Run dispatcher ------------------------------------------------------------------------------

  ##call for the list, if not just proceed as normal
  if(class(object) == "list") {

    ##(0) we might have plenty of sublists before we have the list containing only
    ##RLum-objects
    object <- .unlist_RLum(object)

    ##(1) get rid of objects which are not RLum objects to avoid errors
    object.cleaned <-
      object[sapply(object, inherits, what = "RLum")]

    ##(1.1) place warning message
    if (length(object) > length(object.cleaned)) {
      warning(paste0(
        length(object) - length(object.cleaned)," non 'RLum' object(s) removed from list."
      ))

    }

    ##(2) check if empty, if empty do nothing ...
    if (length(object.cleaned) != 0) {

      ## If we iterate over a list, this might be extremly useful to have different plot titles
      if("main" %in% names(list(...))){
        if(is(list(...)$main,"list")){
          main.list <- rep(list(...)$main, length = length(object.cleaned))

        }
      }

      ##set also mtext, but in a different way
      if(!"mtext" %in% names(list(...))){


        if(is(object[[1]], "RLum.Analysis")){
          mtext <- paste("Record:", 1:length(object.cleaned))

        }else{
          mtext <- NULL

        }
      }else{
        mtext <- rep(list(...)$mtext, length.out = length(object.cleaned))

      }


      if(exists("main.list")){
        ##dispatch objects
        for (i in 1:length(object.cleaned)) {
          RLum.dispatcher(object = object[[i]],
                          main = main.list[[i]],
                          mtext = mtext[[i]],
                          ...)
        }
      }else{
        for (i in 1:length(object.cleaned)) {


          RLum.dispatcher(object = object[[i]],
                          mtext = mtext[[i]],
                          ...)
        }

      }

    }

  }else{
    ##dispatch object
    RLum.dispatcher(object = object, ...)

  }

}


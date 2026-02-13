#' @title General plot function for RLum S4 class objects
#'
#' @description
#' The function calls object specific plot functions for RLum S4 class objects.
#'
#' @details
#' The function provides a generalised access point for plotting various
#' [Luminescence::RLum-class] objects. Depending on the input object, the
#' corresponding plot function will be selected. Allowed arguments can be
#' found in the documentations of each plot function.
#'
#' \tabular{lll}{
#' **object** \tab **corresponding plot function** \cr
#' [Luminescence::RLum.Data.Curve-class] \tab [Luminescence::plot_RLum.Data.Curve] \cr
#' [Luminescence::RLum.Data.Spectrum-class] \tab [Luminescence::plot_RLum.Data.Spectrum]\cr
#' [Luminescence::RLum.Data.Image-class] \tab [Luminescence::plot_RLum.Data.Image]\cr
#' [Luminescence::RLum.Analysis-class] \tab [Luminescence::plot_RLum.Analysis]\cr
#' [Luminescence::RLum.Results-class] \tab [Luminescence::plot_RLum.Results]
#' }
#'
#' @param object [Luminescence::RLum-class] (**required**):
#' object of class [Luminescence::RLum-class] or a list of such objects. If a
#' list is provided, the function tries to plot every object in the list
#' according to its `RLum` class, after removing non-RLum objects.
#'
#' @param ... further arguments and graphical parameters to pass to the
#' specific plot functions. The only arguments that are supported directly are
#' `main` (plot title) and `mtext` (plot subtitle). Here `main` can be provided
#' as a list and the arguments in the
#' list will dispatched to the plots if `object` is of type `list` as well.
#'
#' @return
#' Produces a plot depending on the input object.
#'
#' @section Function version: 0.5
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#'
#' @seealso [Luminescence::plot_RLum.Analysis],
#' [Luminescence::plot_RLum.Data.Curve],
#' [Luminescence::plot_RLum.Data.Spectrum],
#' [Luminescence::plot_RLum.Data.Image],
#' [Luminescence::plot_RLum.Results]
#'
#' @keywords dplot
#'
#' @examples
#' #load Example data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' #transform data.frame to RLum.Data.Curve object
#' temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")
#'
#' #plot RLum object
#' plot_RLum(temp)
#'
#' @export
plot_RLum <- function(
  object,
  ...
) {
  .set_function_name("plot_RLum")
  on.exit(.unset_function_name(), add = TRUE)

  orig.list <- inherits(object, "list")
  if (orig.list) {
    ## we might have plenty of sublists before we reach the list containing
    ## only RLum-objects
    object <- .unlist_RLum(object)
    object <- .rm_nonRLum(object)

    ## return early if there is nothing to plot
    if (length(object) == 0)
      return(NULL)
  } else {
    ## transform a single object to a list, so the logic can be simplified
    object <- list(object)
  }

  extraArgs <- list(...)

  ## allow for different plot titles
  main <- NULL
  if (!is.null(extraArgs$main)) {
    main <- if (orig.list)
              .listify(extraArgs$main, length = length(object))
            else
              list(extraArgs$main)
  }

  ## allow for different subtitles
  mtext <- NULL
  if (!is.null(extraArgs$mtext)) {
    mtext <- rep_len(extraArgs$mtext, length(object))
  } else if (orig.list && inherits(object[[1]], "RLum.Analysis")) {
    mtext <- paste("Record:", 1:length(object))
  }

  for (i in seq_along(object)) {
    .validate_class(object[[i]], "RLum")
    plot_fun <- switch(
        class(object[[i]]),
        ## here we have to prevent the partial matching of 'sub' by 'subset'
        RLum.Analysis = function(object, ...) {
          if (!"subset" %in% ...names())
            plot_RLum.Analysis(object = object, subset = NULL, ...)
          else
            plot_RLum.Analysis(object = object, ...)
        },
        RLum.Data.Curve = plot_RLum.Data.Curve,
        RLum.Data.Spectrum = plot_RLum.Data.Spectrum,
        RLum.Data.Image = plot_RLum.Data.Image,
        RLum.Results = plot_RLum.Results)

    plot_fun(object = object[[i]],
             main = main[[i]],
             mtext = mtext[[i]],
             ...)
  }
}

#' @title Analyse portable CW-OSL measurements
#'
#' @description The function analyses CW-OSL curve data produced by a SUERC portable OSL reader and
#' produces a combined plot of OSL/IRSL signal intensities, OSL/IRSL depletion ratios
#' and the IRSL/OSL ratio.
#'
#' @details This function only works with [RLum.Analysis-class] objects produced by [read_PSL2R].
#' It further assumes (or rather requires) an equal amount of OSL and IRSL curves that
#' are pairwise combined for calculating the IRSL/OSL ratio. For calculating the depletion ratios
#' the cumulative signal of the last n channels (same number of channels as specified
#' by `signal.integral`) is divided by cumulative signal of the first n channels (`signal.integral`).
#'
#' @param object [RLum.Analysis-class] (**required**): [RLum.Analysis-class] object produced by [read_PSL2R].
#' The input can be a [list] of such objects, in such case each input is treated as a separate sample
#' and the results are merged.
#'
#' @param signal.integral [numeric] (**required**):
#' A vector of two values specifying the lower and upper channel used to
#' calculate the OSL/IRSL signal. Can be provided in form of `c(1, 5)` or `1:5`.
#'
#' @param invert [logical] (*with default*):
#' `TRUE` to calculate and plot the data in reverse order.
#'
#' @param normalise [logical] (*with default*):
#' `TRUE` to normalise the OSL/IRSL signals by the mean of all corresponding
#' data curves.
#'
#' @param mode [character] (*with default*): defines the analysis mode, allowed
#' are `"profile"` (the default) and `"surface"` for surface interpolation
#'
#' @param plot [logical] (*with default*):
#' enable/disable plot output
#'
#' @param ... other parameters, supported are `run` to provide the run name ,
#' if the input is a `list`, this is set automatically. Further plot parameters for
#' `mode = 'surface'`
#' `surface_values = c('ratio', 'OSL', 'IRSL')`, `legend` (`TRUE`/`FALSE`), `col_ramp`
#'
#' @return
#' Returns an S4 [RLum.Results-class] object with the following elements:
#'
#' `$data`\cr
#' `.. $summary`: [data.frame] with the results\cr
#' `.. $data`: [list] with the [RLum.Analysis-class] objects\cr
#' `.. $args`: [list] the input arguments
#'
#' @seealso [RLum.Analysis-class], [RLum.Data.Curve-class], [read_PSL2R]
#'
#' @author Christoph Burow, University of Cologne (Germany), Sebastian Kreutzer,
#' Institute of Geography, Ruprecht-Karls-University of Heidelberg, Germany
#'
#' @section Function version: 0.1.0
#'
#' @keywords datagen plot
#'
#' @examples
#'
#' # (1) load example data set
#' data("ExampleData.portableOSL", envir = environment())
#'
#' # (2) merge and plot all RLum.Analysis objects
#' merged <- merge_RLum(ExampleData.portableOSL)
#' plot_RLum(
#'  object = merged,
#'  combine = TRUE,
#'  records_max = 5,
#'  legend.pos = "outside")
#' merged
#'
#' # (3) analyse and plot
#' results <- analyse_portableOSL(
#'   merged,
#'   signal.integral = 1:5,
#'   invert = FALSE,
#'   normalise = TRUE)
#' get_RLum(results)
#'
#' @md
#' @export
analyse_portableOSL <- function(
  object,
  signal.integral = NULL,
  invert = FALSE,
  normalise = FALSE,
  mode = "profile",
  plot = TRUE,
  ...)
  {

# Self-call ---------------------------------------------------------------
  if (inherits(object, "list")) {
      temp <- .warningCatcher(lapply(1:length(object), function(x) {
        analyse_portableOSL(
          object = object[[x]],
          signal.integral = signal.integral,
          invert = invert,
          normalise = normalise,
          plot = plot,
          run = paste0("RUN #", x))
      }))

      return(merge_RLum(temp))

  }

# Start function ----------------------------------------------------------
  ## INPUT VERIFICATION ----
  if (!inherits(object, "RLum.Analysis"))
    stop("Only objects of class 'RLum.Analysis' are allowed.", call. = FALSE)
  if (!all(sapply(object, class) == "RLum.Data.Curve"))
    stop("The 'RLum.Analysis' object must only contain objects of class 'RLum.Data.Curve'.", call. = FALSE)
  if (!all(sapply(object, function(x) x@originator) == "read_PSL2R"))
    stop("Only objects originating from 'read_PSL2R()' are allowed.", call. = FALSE)

  if (is.null(signal.integral)) {
    signal.integral <- c(1, 1)
    warning("No value for 'signal.integral' provided. Only the first data point of each curve was used!",
            call. = FALSE)
  }

  ## set SAMPLE --------
  if("run" %in% names(list(...)))
    run <- list(...)$run
  else if (!is.null(object@info$Run_Name))
    run <- object@info$Run_Name
  else
    run <- "Run #1"

  ## extract  coordinates -------
  coord <- .extract_PSL_coord(object)

  ## CALCULATIONS ----
  ## Note: the list ... unlist construction is used make sure that get_RLum() always
  ## returns a list
  # OSL
  OSL <- .unlist_RLum(list(get_RLum(object, recordType = "OSL")))
  OSL <- do.call(rbind, lapply(OSL, function(x) {
    posl_get_signal(x, signal.integral)
  }))

  # IRSL
  IRSL <- .unlist_RLum(list(get_RLum(object, recordType = "IRSL")))
  IRSL <- do.call(rbind, lapply(IRSL, function(x) {
    posl_get_signal(x, signal.integral)
  }))

  ## NORMALISE ----
  if (normalise) {
    OSL <- posl_normalise(OSL)
    IRSL <- posl_normalise(IRSL)
  }

  ## INVERT ----
  if (invert) {
    OSL <- posl_invert(OSL)
    IRSL <- posl_invert(IRSL)
  }

  # OSL/IRSL Ratio
  RATIO <- IRSL$sum_signal / OSL$sum_signal

  ## PLOTTING ----
  if (plot) {
   ##
   plot_settings <- modifyList(
     x = list(
       col_ramp = grDevices::heat.colors(30, rev = TRUE, alpha = 0.5),
       surface_value = "ratio",
       legend = TRUE

     ),
     val = list(...))

   ### mode == "surface" ---------
   if(mode == "surface") {
     value <- switch(
       plot_settings$surface_value,
       "ratio" = RATIO,
       "OSL" = OSL,
       "IRSL" = IRSL
     )

     ## set matrix
     m <- cbind(coord[, 1], coord[, 2], value)

     ## interpolate
     ## TODO make better error message
     ## TODO add tests
     s <-
       try(interp::interp(
         x = m[, 1],
         y = m[, 2],
         z = m[, 3],
         nx = 200,
         ny = 200,
       ), silent = FALSE)

     if(!inherits(s, "try-error")) {
       par.default <- par(mar = c(4.5,4.5,4,2))
       on.exit(par(par.default))

       ## plot image
       graphics::image(
         s,
         col = plot_settings$col_ramp,
         xlab = "x [m]",
         ylab = "y [m]",
         main = run
       )

       ## add points
       points(m[,1:2], pch = 20)

       ## add what is shown
       mtext(side = 3, text = plot_settings$surface_value, cex = 0.9)

       ## add legend
       if(plot_settings$legend) {
         par.default <- c(par.default, par(xpd = TRUE))
         on.exit(par(par.default))

         col_grad <- plot_settings$col_ramp[
           seq(1, length(plot_settings$col_ramp), length.out = 14)]

         slices <- seq(par()$usr[3],par()$usr[4],length.out = 15)

         for(s in 1:(length(slices) - 1)){
           graphics::rect(
             xleft = par()$usr[2] * 1.01,
             xright = par()$usr[2] * 1.03,
             ybottom = slices[s],
             ytop =  slices[s + 1],
             col = col_grad[s],
             border = TRUE)
         }

         ## add legend text
         text(
           x = par()$usr[2] * 1.04,
           y = par()$usr[4],
           labels = if(is.null(plot_settings$zlim_image)) {
             format(max(m[,3]), digits = 1, scientific = TRUE)
           } else {
             format(plot_settings$zlim_image[2], digits = 1, scientific = TRUE)
           },
           cex = 0.7,
           srt = 270,
           pos = 3)
         text(
           x = par()$usr[2] * 1.04,
           y = par()$usr[3],
           labels = if(is.null(plot_settings$zlim_image)) {
             format(min(m[,3]), digits = 1, scientific = TRUE)
           } else {
             format(plot_settings$zlim_image[1], digits = 1, scientific = TRUE)
           },
           cex = 0.7,
           pos = 3,
           srt = 270)
       }

     }

   ## mode == "profile" --------
   } else {
    par.old.full <- par(no.readonly = TRUE)
    on.exit(par(par.old.full))

    # default: par(mar = c(5, 4, 4, 2) + 0.1) // bottom, left, top, right
    par(mfrow = c(1, 6))

    par(mar = c(5, 4, 4, 1) + 0.1)

    frame()

    mtext(side= 3, run, cex = 0.7, line = 2)

    par(mar = c(5, 0, 4, 1) + 0.1)

    plot(
      OSL$sum_signal,
      1:nrow(OSL),
      type = "b",
      pch = 16,
      col = "blue",
      xlim = range(pretty(OSL$sum_signal)),
      xlab = "BSL",
      ylab = "Index",
      bty = "n",
      yaxt = "n"
    )
    axis(2, line = 3, at = 1:nrow(OSL), labels = if(invert) nrow(OSL):1 else 1:nrow(OSL))
    axis(3)
    mtext("Index", side = 2, line = 6)

    plot(
      IRSL$sum_signal,
      1:nrow(IRSL),
      type = "b",
      pch = 16,
      col = "red",
      xlim = range(pretty(IRSL$sum_signal)),
      xlab = "IRSL",
      ylab = "",
      bty = "n",
      yaxt = "n"
    )
    axis(3)

    plot(
      OSL$sum_signal_depletion,
      1:nrow(OSL),
      type = "b",
      pch = 1,
      col = "blue",
      xlim = range(pretty(OSL$sum_signal_depletion)),
      xlab = "BSL depl.",
      ylab = "",
      bty = "n",
      yaxt = "n",
      lty = 2
    )
    axis(3)

    plot(
      IRSL$sum_signal_depletion,
      1:nrow(IRSL),
      type = "b",
      pch = 1,
      col = "red",
      xlim = range(pretty(IRSL$sum_signal_depletion)),
      xlab = "IRSL depl.",
      ylab = "",
      bty = "n",
      yaxt = "n",
      lty = 2
    )
    axis(3)

    plot(
      RATIO,
      1:length(RATIO),
      type = "b",
      pch = 16,
      col = "black",
      xlim = range(pretty(RATIO)),
      xlab = "IRSL/BSL",
      ylab = "",
      bty = "n",
      yaxt = "n"
    )
    axis(3)
   } ## end mode == "profile"
  }

  ## RETURN VALUE ----
  call<- sys.call()
  args <- as.list(call)[2:length(call)]
  summary <- data.frame(
    RUN = run,
    BSL = OSL$sum_signal,
    BSL_error = OSL$sum_signal_err,
    IRSL = IRSL$sum_signal,
    IRSL_error = IRSL$sum_signal_err,
    BSL_depletion = OSL$sum_signal_depletion,
    IRSL_depletion = IRSL$sum_signal_depletion,
    IRSL_BSL_RATIO = RATIO,
    COORD_X = coord[1:nrow(OSL),1],
    COORD_Y = coord[1:nrow(OSL),2]
    )

  newRLumResults <- set_RLum(
    class = "RLum.Results",
    data = list(
      summary=summary,
      data = object,
      args=args
    ),
    info = list(call = call))

  return(newRLumResults)

}

# HELPER FUNCTIONS ----------

## This extracts the relevant curve data information of the RLum.Data.Curve
## objects
posl_get_signal <- function(x, signal.integral) {
    raw_signal <- get_RLum(x)[,2]
    sigint <- range(signal.integral)
    if (sigint[2] > length(raw_signal)) {
      sigint[2] <- length(raw_signal)
      warning("'signal.integral' (", paste(range(signal.integral), collapse = ", "),") ",
              "exceeded the number of available data points (n = ", length(raw_signal),") and ",
              "has been automatically reduced to the maximum number.", call. = FALSE)
    }
    sum_signal <- sum(raw_signal[sigint[1]:sigint[2]])
    sum_signal_err <- sqrt(sum(x@info$raw_data$counts_per_cycle_error[sigint[1]:sigint[2]]^2))
    sum_signal_depletion <- sum(raw_signal[(length(raw_signal)-length(sigint[1]:sigint[2])):length(raw_signal)]) / sum_signal
    return(data.frame(sum_signal, sum_signal_err, sum_signal_depletion))
}

## This function normalises the data curve by the mean signal
posl_normalise <- function(x) {
  rel.error <- x$sum_signal_err / x$sum_signal
  x$sum_signal <- x$sum_signal / mean(x$sum_signal)
  x$sum_signal_err <- x$sum_signal * rel.error
  x$sum_signal_depletion <- x$sum_signal_depletion / mean(x$sum_signal_depletion)
  return(x)
}

## This function inverters the data.frame (useful when the sample are in inverse
## stratigraphic order)
posl_invert <- function(x) {
  x <- x[nrow(x):1, ]
}

## This function extracts the coordinates from the file name
##
.extract_PSL_coord <- function(object){
  ## get settings
  settings_sample <- vapply(object, function(x) x@info$settings$Sample, character(1)) |>
    unique()

  ## set character vector
  tmp_coord <- character(length(settings_sample))

  ## search for pattern match ... why?
  ## because otherwise the dataset becomes inconsistent
  pattern_match <- grepl(
    pattern = "\\_x\\:[0-9].+\\|y\\:[0-9].+",
    x = settings_sample, perl = TRUE)

  ## extract coordinates
  tmp_coord[pattern_match] <- regexpr(
    pattern = "\\_x\\:[0-9].+\\|y\\:[0-9].+",
    text = settings_sample[pattern_match ], perl = TRUE) |>
    regmatches(x = settings_sample[pattern_match], m = _)

  ## extract x and y
  coord_split <- strsplit(tmp_coord, split = "|y:", fixed = TRUE)

  coord <- vapply(coord_split, function(x) {
    if(length(x) == 0)
      return(c(NA_real_, NA_real_))

    c(
      as.numeric(strsplit(x, "_x:", fixed = TRUE)[[1]][[2]]),
      as.numeric(x[2]))
      },
    numeric(2))

  return(t(coord))
}

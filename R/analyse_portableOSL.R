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
#' **Note:  The function assumes the following sequence pattern: `DARK COUNT`, `IRSL`, `DARK COUNT`, `BSL`, `DARK COUNT`. If you have written a different sequence, the analysis function will (likely) not work!**.
#'
#' **Signal processing**
#' The function processes the signals as follows: `BSL` and `IRSL` signals are extracted using the
#' chosen signal integral, dark counts are taken in full.
#'
#' **Working with coordinates**
#' Usually samples are taken from a profile with a certain stratigraphy. In the past the function
#' calculated an index. With this newer version, you have two option of passing on xy-coordinates
#' to the function:
#'
#' * (1) Add coordinates to the sample name during measurement. The form is rather
#' strict and has to follow the scheme `_x:<number>|y:<number>`. Example:
#' `sample_x:0.2|y:0.4`.
#'
#' * (2) Alternatively, you can provide a [list] or [matrix] with the sample coordinates.
#' Example: `coord = list(c(0.2, 1), c(0.3,1.2))`
#'
#' Please note that the unit is meter (m) and the function expects always xy-coordinates.
#' The latter one is useful for surface interpolations. If you have measured a profile where
#' the x-coordinates to not measure, x-coordinates should be 0.
#'
#' @param object [RLum.Analysis-class] (**required**): [RLum.Analysis-class] object produced by [read_PSL2R].
#' The input can be a [list] of such objects, in such case each input is treated as a separate sample
#' and the results are merged.
#'
#' @param signal.integral [numeric] (**required**): A vector of two values specifying the lower and upper channel used to calculate the OSL/IRSL signal. Can be provided in form of `c(1, 5)` or `1:5`.
#'
#' @param invert [logical] (*with default*): `TRUE` flip the plot the data in reverse order.
#'
#' @param normalise [logical] (*with default*): `TRUE` to normalise the OSL/IRSL signals
#' to the *mean* of all corresponding data curves.
#'
#' @param mode [character] (*with default*): defines the analysis mode, allowed
#' are `"profile"` (the default) and `"surface"` for surface interpolation. If you select
#' something else, nothing will be plotted (similar to `plot = FALSE`).
#'
#' @param coord [list] [matrix] (*optional*): a list or matrix of the same length as
#' number of samples measured with coordinates for the sampling positions. Coordinates
#' are expected to be provided in meter (unit: m).
#' Expected are x and y coordinates, e.g.,
#' `coord = list(samp1 = c(0.1, 0.2)`. If you have not measured x coordinates, please x should be 0.
#'
#' @param plot [logical] (*with default*): enable/disable plot output
#'
#' @param ... other parameters to be passed to modify the plot output.
#' Supported are `run` to provide the run name ,
#' if the input is a `list`, this is set automatically. Further plot parameters are
#' `surface_values` ([character] with value to plot), `legend` (`TRUE`/`FALSE`), `col_ramp` (for
#' surface mode), `contour` (contour lines `TRUE`/`FALSE` in surface mode), `grid` (`TRUE`/`FALSE`), `col`, `pch` (for profile mode), `xlim` (a name [list] for profile mode), `ylim`,
#' `zlim` (surface mode only), `ylab`, `xlab`, `zlab` (here x-axis labelling), `main`, `bg_img` (for
#' profile mode background image, usually a profile photo; should be a raster object),
#' `bg_img_positions` (a vector with the four corner positions, cf. [graphics::rasterImage])
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
#' Institute of Geography, Ruprecht-Karl University of Heidelberg, Germany
#'
#' @section Function version: 0.1.1
#'
#' @keywords datagen plot
#'
#' @examples
#'
#' ## example profile plot
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
  coord = NULL,
  plot = TRUE,
  ...
) {
  .set_function_name("analyse_portableOSL")
  on.exit(.unset_function_name(), add = TRUE)

  ## TODO
  ## - add tests for background image option
  ## - clear docu

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
  ## only RLum.Analysis objects
  if (!inherits(object, "RLum.Analysis"))
    .throw_error("Only objects of class 'RLum.Analysis' are allowed")

  ## only curve objects
  if (!all(sapply(object, class) == "RLum.Data.Curve"))
    .throw_error("The 'RLum.Analysis' object must contain only objects ",
                 "of class 'RLum.Data.Curve'")

  ## check originator
  if (!all(sapply(object, function(x) x@originator) == "read_PSL2R"))
    .throw_error("Only objects originating from 'read_PSL2R()' are allowed")

  ## check sequence pattern
  if(!all(names(object)[1:5] == c("USER", "IRSL", "USER", "OSL", "USER")))
    .throw_error("Sequence pattern not supported, please read manual for details")

  if (is.null(signal.integral)) {
    signal.integral <- c(1, 1)
    .throw_warning("No value for 'signal.integral' provided. Only the ",
                   "first data point of each curve was used")
  }


  ## set SAMPLE --------
  if("run" %in% names(list(...)))
    run <- list(...)$run
  else if (!is.null(object@info$Run_Name))
    run <- object@info$Run_Name
  else
    run <- "Run #1"


  ## CALCULATIONS ----
  ## Note: the list ... unlist construction is used make sure that get_RLum() always
  ## returns a list
  ### get OSL -------
  OSL <- .unlist_RLum(list(get_RLum(object, recordType = "OSL")))
  OSL <- do.call(rbind, lapply(OSL, function(x) {
    .posl_get_signal(x, signal.integral)
  }))

  ### get IRSL -------
  IRSL <- .unlist_RLum(list(get_RLum(object, recordType = "IRSL")))
  IRSL <- do.call(rbind, lapply(IRSL, function(x) {
    .posl_get_signal(x, signal.integral)
  }))

  ### get DARK counts ----------
  ### we assume that USER contains the dark count measurements
  DARK_COUNT <- .unlist_RLum(list(get_RLum(object, recordType = "USER")))
  DARK_COUNT <- lapply(seq(1,length(DARK_COUNT),3), function(x) DARK_COUNT[x:(x+2)])

  DARK_COUNT <- do.call(rbind, lapply(DARK_COUNT, function(x) {
    .posl_get_dark_count(x)
  }))

  ### NORMALISE ----
  if (normalise) {
    OSL <- .posl_normalise(OSL)
    IRSL <- .posl_normalise(IRSL)
  }

  ### OSL/IRSL Ratio -------
  RATIO <- IRSL$sum_signal / OSL$sum_signal

  ### extract  coordinates -------
  if(is.null(coord)) {
    coord <- .extract_PSL_coord(object)

  } else {
    if(!inherits(coord, "matrix") && !inherits(coord, "list"))
      .throw_error("'coord' must be a matrix or a list")

    if(inherits(coord, "list"))
      coord <- do.call(rbind, coord)

    ## check length
    if(nrow(coord) != length(OSL$sum_signal))
      .throw_error("Number of coordinates differ from the number of samples")
  }

  ### GENERATE SUMMARY data.frame -----
  summary <- data.frame(
    ID = seq_along(OSL$sum_signal),
    RUN = run,
    BSL = OSL$sum_signal,
    BSL_error = OSL$sum_signal_err,
    IRSL = IRSL$sum_signal,
    IRSL_error = IRSL$sum_signal_err,
    BSL_depletion = OSL$sum_signal_depletion,
    IRSL_depletion = IRSL$sum_signal_depletion,
    IRSL_BSL_RATIO = RATIO,
    DARK =  DARK_COUNT$mean_dark_count,
    DARK_error = DARK_COUNT$sd_dark_count,
    COORD_X = coord[,1],
    COORD_Y = coord[,2]
  )

    ## if coordinates exist, sort by depth
    if(!any(is.na(coord[,2])))
      summary <- summary[order(coord[,2]),]

   ### INVERT ----------
   if(invert)
     summary <- summary[nrow(summary):1,]

  # PLOTTING -------------------------------------------------------------------
  ## generate list of plot matrices
  ## this done to have consistent settings for all plot types
  parm <-  c("BSL", "BSL_error", "IRSL", "IRSL_error",
             "BSL_depletion", "IRSL_depletion", "IRSL_BSL_RATIO", "DARK", "DARK_error")
  m_list <- lapply(parm, function(x){
     cbind(x = summary[["COORD_X"]], y = summary[["COORD_Y"]], value = summary[[x]])

  })

    ## correct names of the list
    names(m_list) <- parm

    ## add a few attributes to be used later
    attr(m_list, "xlim") <- lapply(m_list, function(x) range(x[,1]))
    attr(m_list, "ylim") <- if(invert) rev(range(m_list[[1]][,2])) else range(m_list[[1]][,2])
    attr(m_list, "zlim") <- lapply(m_list, function(x) range(x[,3]))

    ## account for surface case
    if (!is.null(mode) && mode == "surface") {
      attr(m_list, "ylim") <- if (invert) rev(range(summary$COORD_Y)) else range(summary$COORD_Y)
      attr(m_list, "xlim") <- range(summary$COORD_X)
    }

  if (!is.null(mode) && plot[1]) {
   ## account for surface case
   ## preset plot settings
   ## plot settings -------
   plot_settings <- modifyList(
     x = list(
       col_ramp = grDevices::heat.colors(30, rev = TRUE, alpha = 0.5),
       bg_img = NULL,
       bg_img_positions = NULL,
       surface_value = c("BSL", "IRSL", "IRSL_BSL_RATIO"),
       legend = TRUE,
       col = c("blue", "red", "blue", "red", "black", "grey"),
       pch = rep(16, length(m_list)),
       xlim = attr(m_list, "xlim"),
       ylim = attr(m_list, "ylim"),
       zlim = if(mode == "surface") NA else attr(m_list, "zlim"),
       ylab = if(!any(is.na(summary$COORD_Y))) "Depth [m]" else "Index",
       xlab = "x [m]",
       grid = TRUE,
       contour = FALSE,
       zlab = c("BSL", "IRSL", "BSL depl.", "IRSL depl.", "IRSL/BSL", "mean DARK"),
       main = summary$RUN[1]
     ),
     val = list(...), keep.null = TRUE)

   ## mode == "surface" ---------
   if(mode[1] == "surface") {
     ### check for validity of surface value -------
     if(!all(plot_settings$surface_value %in% names(m_list)))
       .throw_error("Unknown value to plot: Valid are: ",
                    paste(names(m_list), collapse = ", "))

     ## set par -------
     if(length(plot_settings$surface_value) > 1) {
       par.default <- par(mfrow = c(2,2))
       on.exit(par(par.default), add = TRUE)
     }

     ## loop over surface values -------
     for(i in plot_settings$surface_value) {
       ## set matrix for the plot
       m <- m_list[[i]]

       ## respect xlim and ylim range
       m <- m[m[,2] >= min(plot_settings$ylim) & m[,2] <= max(plot_settings$ylim), ]
       m <- m[m[,1] >= min(plot_settings$xlim) & m[,1] <= max(plot_settings$xlim), ]

       ## respect z_values
       if(!all(is.na(plot_settings$zlim)))
         m <- m[m[,3] >= min(plot_settings$zlim) & m[,3] <= max(plot_settings$zlim), ]

       ## interpolate ------
       s <-
         try(interp::interp(
           x = m[, 1],
           y = m[, 2],
           z = m[, 3],
           nx = 200,
           ny = 200,
         ), silent = TRUE)

       ## show only warning
       if(inherits(s, "try-error"))
         .throw_warning("Surface interpolation failed: this happens when ",
                        "all points are arranged in one line. ",
                        "Nothing plotted!")

       ## show error
       if(!inherits(s, "try-error")) {
         par.default <- c(
           if(exists("par.default")) par.default else NULL,
           par(mar = c(4.5,4.5,4,2), xpd = FALSE))
         on.exit(par(par.default), add = TRUE)

         ## open empty plot
         plot(
           x = NA,
           y = NA,
           ylim = plot_settings$ylim,
           xlim = plot_settings$xlim,
           xlab = plot_settings$xlab,
           ylab = plot_settings$ylab,
           main = plot_settings$main)

         ## add background image if available -------
         if (!is.null(plot_settings$bg_img)) {
           ## get corner positions
           if(!is.null(plot_settings$bg_img_positions))
             positions <- plot_settings$bg_img_positions[1:4]
           else
             positions <- par()$usr

           graphics::rasterImage(
             image = plot_settings$bg_img,
             xleft = positions[1],
             ybottom = positions[4],
             xright = positions[2],
             ytop = positions[3],
             interpolate = TRUE)
         }

         ## plot image -------
         graphics::image(
           s,
           col = plot_settings$col_ramp,
           add = TRUE
         )

         ## add contour
         if (plot_settings$contour)
           graphics::contour(m, add = TRUE, col = "grey")

         ## add points
         points(m[,1:2], pch = 20)

         ## add what is shown in the plot
         mtext(side = 3, text = i, cex = 0.7)

         ## add legend
         if(plot_settings$legend) {
           par.default <- c(par.default, par(xpd = TRUE))
           on.exit(par(par.default), add = TRUE)

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
             cex = 0.6,
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
             cex = 0.6,
             pos = 3,
             srt = 270)

           ## add legend labelling (central)
           text(
             x = par()$usr[2] * 1.05,
             y = (par()$usr[4] - par()$usr[3])/2 + par()$usr[3],
             labels = "Intensity [a.u.]",
             cex = 0.7,
             pos = 3,
             srt = 270)
         }
       }
     }# end for loop
   }

   ## mode == "profile" ---------
   if (!is.null(mode[1]) && mode == "profile") {
    par.old.full <- par(no.readonly = TRUE)
    on.exit(par(par.old.full), add = TRUE)

    # default: par(mar = c(5, 4, 4, 2) + 0.1) // bottom, left, top, right
    par(mfrow = c(1, 7))

    par(mar = c(5, 4, 4, 1) + 0.1)

    frame()

    mtext(side= 3, plot_settings$main, cex = 0.7, line = 2)

    par(mar = c(5, 0, 4, 1) + 0.1)

    ## make sure that wrong zlim settings do not screw up the function
    if(!inherits(plot_settings$zlim, "list")) {
      .throw_warning("In profile mode, zlim needs to be provided as a named ",
                     "list, example: list(BSL = c(0,1)). Reset to default")
      plot_settings$zlim <- attr(m_list, "zlim")
    }

    #### BSL -------
    plot(
      NA,
      NA,
      ylim = plot_settings$ylim,
      xlim = plot_settings$zlim[["BSL"]],
      xlab = plot_settings$zlab[1],
      bty = "n",
      yaxt = "n"
    )
      if(plot_settings$grid) grid()
      lines(
        x = m_list[["BSL"]][,"value"],
        y = m_list[["BSL"]][,"y"],
        type = "b",
        pch = plot_settings$pch[1],
        col = plot_settings$col[1]
      )

      ## add error bars
      segments(
        x0 = m_list[["BSL"]][,"value"] - m_list[["BSL_error"]][,"value"],
        x1 = m_list[["BSL"]][,"value"] + m_list[["BSL_error"]][,"value"],
        y0 = m_list[["BSL"]][,"y"],
        y1 = m_list[["BSL"]][,"y"],
        col = plot_settings$col[1])

        axis(2, line = 3, at = m_list[["BSL"]][,"y"], labels = m_list[["BSL"]][,"y"])
        axis(3)

    ## add general y-axis label
    mtext(plot_settings$ylab[1], side = 2, line = 6)

    ### IRSL --------------
    plot(
        NA, NA,
        ylim = plot_settings$ylim,
        xlim = plot_settings$zlim[["IRSL"]],
        xlab = plot_settings$zlab[2],
        bty = "n",
        yaxt = "n"
    )
      if(plot_settings$grid) grid()

      lines(
        x = m_list[["IRSL"]][,"value"],
        y = m_list[["IRSL"]][,"y"],
        type = "b",
        pch = plot_settings$pch[2],
        col = plot_settings$col[2])

      ## add error bars
      segments(
        x0 = m_list[["IRSL"]][,"value"] - m_list[["IRSL_error"]][,"value"],
        x1 = m_list[["IRSL"]][,"value"] + m_list[["IRSL_error"]][,"value"],
        y0 = m_list[["IRSL"]][,"y"],
        y1 = m_list[["IRSL"]][,"y"],
        col = plot_settings$col[2])

      axis(3)

    ### OSL DEPLETATION -------
    plot(
      NA, NA,
      ylim = plot_settings$ylim,
      xlim = plot_settings$zlim[["BSL_depletion"]],
      xlab = plot_settings$zlab[3],
      bty = "n",
      yaxt = "n"
    )

      if(plot_settings$grid) grid()
      lines(
        x = m_list[["BSL_depletion"]][,"value"],
        y = m_list[["BSL_depletion"]][,"y"],
        type = "b",
        lty = 2,
        pch = plot_settings$pch[3],
        col = plot_settings$col[3]
      )

      axis(3)

    ### IRSL DEPLETION ---------------
    plot(
      NA, NA,
      ylim = plot_settings$ylim,
      xlim = plot_settings$zlim[["IRSL_depletion"]],
      xlab = plot_settings$zlab[4],
      bty = "n",
      yaxt = "n"
    )

      if(plot_settings$grid) grid()

      lines(
        x = m_list[["IRSL_depletion"]][,"value"],
        y = m_list[["IRSL_depletion"]][,"y"],
        type = "b",
        lty = 2,
        pch = plot_settings$pch[4],
        col = plot_settings$col[4])

      axis(3)

    ### RATIO -----------------------------
    plot(
      NA, NA,
      ylim = plot_settings$ylim,
      xlim = plot_settings$zlim[["IRSL_BSL_RATIO"]],
      xlab = plot_settings$zlab[5],
      ylab = "",
      bty = "n",
      yaxt = "n"
    )

      if(plot_settings$grid) grid()

      lines(
        x = m_list[["IRSL_BSL_RATIO"]][,"value"],
        y = m_list[["IRSL_BSL_RATIO"]][,"y"],
        type = "b",
        pch = plot_settings$pch[5],
        col = plot_settings$col[5])

      axis(3)

    ### DARK -----------------------------
    plot(
      x = m_list[["DARK"]][,"value"],
      y = m_list[["DARK_error"]][,"y"],
      type = "b",
      pch = plot_settings$pch,
      col = plot_settings$col[6],
      ylim = plot_settings$ylim,
      xlim = range(c(
        plot_settings$zlim[["DARK"]] - plot_settings$zlim[["DARK_error"]],
        plot_settings$zlim[["DARK"]] + plot_settings$zlim[["DARK_error"]])),
      xlab = plot_settings$zlab[6],
      ylab = "",
      bty = "n",
      yaxt = "n"
    )

      ## add error bars
      segments(
        x0 = m_list[["DARK"]][,"value"] - m_list[["DARK_error"]][,"value"],
        x1 = m_list[["DARK"]][,"value"] + m_list[["DARK_error"]][,"value"],
        y0 = m_list[["DARK"]][,"y"],
        y1 = m_list[["DARK"]][,"y"],
        col = plot_settings$col[6])

        axis(3)
   } ## end mode == "profile"
  }

  ## RETURN VALUE ----
  call<- sys.call()
  args <- as.list(call)[2:length(call)]

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
.posl_get_signal <- function(x, signal.integral) {
    raw_signal <- get_RLum(x)[,2]
    sigint <- range(signal.integral)
    if (sigint[2] > length(raw_signal)) {
      sigint[2] <- length(raw_signal)
      .throw_warning("'signal.integral' (",
                     paste(range(signal.integral), collapse = ", "), ") ",
                     "exceeded the number of available data points (n = ",
                     length(raw_signal),") and has been automatically ",
                     "reduced to the maximum number.")
    }
    sum_signal <- sum(raw_signal[sigint[1]:sigint[2]])
    sum_signal_err <- sqrt(sum(x@info$raw_data$counts_per_cycle_error[sigint[1]:sigint[2]]^2))
    sum_signal_depletion <- sum(raw_signal[(length(raw_signal)-length(sigint[1]:sigint[2])):length(raw_signal)]) / sum_signal
    return(data.frame(sum_signal, sum_signal_err, sum_signal_depletion))
}

.posl_get_dark_count <- function(x) {
  ## we do assume a fixed sequence pattern, hence, we know what to
  ## expect that anything that comes in here, can be merged
  counts <- unlist(lapply(x, function(x) as.matrix(x)[,2]))

  return(data.frame(mean_dark_count = mean(counts), sd_dark_count = sd(counts)))

}

## This function normalises the data curve by the mean signal
.posl_normalise <- function(x) {
  rel.error <- x$sum_signal_err / x$sum_signal
  x$sum_signal <- x$sum_signal / mean(x$sum_signal)
  x$sum_signal_err <- x$sum_signal * rel.error
  x$sum_signal_depletion <- x$sum_signal_depletion / mean(x$sum_signal_depletion)
  return(x)
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

  ## assign values
  coord <- vapply(coord_split, function(x) {
    if(length(x) == 0)
      return(c(x = NA_real_, y = NA_real_))

    c(x = as.numeric(strsplit(x, "_x:", fixed = TRUE)[[1]][[2]]),
      y = as.numeric(x[2]))},
    numeric(2)) |> t()

  ## if NA, assign index
  if(any(is.na(coord[,1]))) coord[,1] <- 0
  if(any(is.na(coord[,2]))) coord[,2] <- 1:nrow(coord)

  return(coord)
}

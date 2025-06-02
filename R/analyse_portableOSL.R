#' @title Analyse portable CW-OSL measurements
#'
#' @description
#' The function analyses CW-OSL curve data produced by a SUERC portable OSL reader and
#' produces a combined plot of OSL/IRSL signal intensities, OSL/IRSL depletion ratios
#' and the IRSL/OSL ratio.
#'
#' @details
#' This function only works with [RLum.Analysis-class] objects produced by [read_PSL2R].
#' It further assumes (or rather requires) an equal amount of OSL and IRSL curves that
#' are pairwise combined for calculating the IRSL/OSL ratio.
#' For calculating the depletion ratios, the cumulative signal of the last *n*
#' channels (same number of channels as specified by `signal.integral`) is
#' divided by cumulative signal of the first *n* channels (`signal.integral`).
#'
#' **Note:  The function assumes the following sequence pattern:
#' `DARK COUNT`, `IRSL`, `DARK COUNT`, `BSL`, `DARK COUNT`.** Therefore, the
#' total number of curves in the input object must be a multiple of 5, and
#' there must be 3 `DARK_COUNT` records for each IRSL/BSL pair. If you have used
#' a different sequence, the function will produce an error.
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
#' * (2) Alternatively, you can provide a [list] or [matrix] with the *(x, y)*
#' coordinates of each sample in meters (m) using the `coord` argument:
#' Example: `coord = list(c(0.2, 1), c(0.3,1.2))`
#'
#' If in your profile the x-coordinates were not measured, *x* should be set
#' to 0. Note that, in such case, a surface plot cannot be produced.
#'
#' @param object [RLum.Analysis-class] (**required**):
#' object produced by [read_PSL2R]. The input can be a [list] of such objects,
#' in which case each input is treated as a separate sample and the results
#' are merged.
#'
#' @param signal.integral [numeric] (**required**):
#' A vector specifying the range of channels used to calculate the OSL/IRSL
#' signal. It can be provided as a vector of length 2 such as `c(1, 5)`, or
#' as a sequence such as `1:5`, in which case the lowest and highest values
#' define the range.
#'
#' @param invert [logical] (*with default*): `TRUE` flip the plot the data in reverse order.
#'
#' @param normalise [logical] (*with default*):
#' whether the OSL/IRSL signals should be normalised to the *mean* of all
#' corresponding data curves.
#'
#' @param mode [character] (*with default*):
#' analysis mode, one of `"profile"` (the default) or `"surface"` for surface
#' interpolation.
#'
#' @param coord [list] [matrix] (*optional*): a list or a 2-column matrix
#' with the *x* and *y* coordinates for the sampling positions in meters (m),
#' of the same length as the number of samples measured. For example, the
#' coordinates for one sample could be `coord = list(samp1 = c(0.1, 0.2)`.
#' If the *x* coordinates were not measured, *x* should be set to 0.
#' Note that, in such case, a surface plot cannot be produced.
#'
#' @param plot [logical] (*with default*): enable/disable the plot output.
#'
#' @param ... other parameters to be passed to modify the plot output.
#' Supported are `run` to provide the run name (if the input is a `list`, this
#' is set automatically). Further plot parameters accepted are `main`,
#' `col`, `xlim` (a named [list] for profile mode), `ylim`, `ylab`, `xlab`.
#' Additional parameters for `mode = "profile"` are  `type`, `pch`, `grid`
#' (`TRUE`/`FALSE`), `bg_img` (a raster object for the background image,
#' usually a profile photo), `bg_img_positions` (a vector with the four corner
#' positions, see [graphics::rasterImage]), `zlab` (here x-axis labelling).
#' Additional parameters for `mode = "surface"` are `surface_value`
#' ([character] with names of the surfaces to plot), `col_ramp`, `legend`
#' (`TRUE`/`FALSE`), `contour` (`TRUE`/`FALSE`),` `contour_nlevels`,
#' `contour_col`, ' zlim`.
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
#' @author Christoph Burow, University of Cologne (Germany) \cr
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @section Function version: 0.1.3
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

  ## Self-call --------------------------------------------------------------
  if (inherits(object, "list")) {
      temp <- .warningCatcher(lapply(seq_along(object), function(x) {
        analyse_portableOSL(
          object = object[[x]],
          signal.integral = signal.integral,
          invert = invert,
          normalise = normalise,
          mode = mode,
          plot = plot,
          run = paste0("RUN #", x))
      }))

      return(merge_RLum(temp))
  }

  ## Start function ---------------------------------------------------------

  ## only RLum.Analysis objects
  .validate_class(object, "RLum.Analysis")
  .validate_not_empty(object)

  ## only curve objects
  if (!all(sapply(object, class) == "RLum.Data.Curve"))
    .throw_error("The 'RLum.Analysis' object must contain only objects ",
                 "of class 'RLum.Data.Curve'")

  ## check originator
  if (!all(sapply(object, function(x) x@originator) == "read_PSL2R"))
    .throw_error("Only objects originating from 'read_PSL2R()' are allowed")

  ## check length and start of the sequence pattern, we check it further below
  if (length(object) %% 5 != 0 ||
      !all(names(object)[1:5] == c("USER", "IRSL", "USER", "OSL", "USER")))
    .throw_error("Sequence pattern not supported: see the manual for details")

  if (is.null(signal.integral)) {
    signal.integral <- c(1, 1)
    .throw_warning("No value for 'signal.integral' provided. Only the ",
                   "first data point of each curve was used")
  } else {
    signal.integral <- range(signal.integral)
  }

  ## set the maximum signal_integral allowed: as this must be valid across all
  ## records, we cap it to the minimum number of points
  num.points <- min(sapply(get_RLum(object, recordType = c("OSL", "IRSL")),
                             length))
  if (max(signal.integral) > num.points || min(signal.integral) < 1) {
    orig.signal.int <- signal.integral
    signal.integral <- pmin(pmax(signal.integral, 1), num.points)
    .throw_warning("'signal.integral' (",
                   .collapse(orig.signal.int, quote = FALSE), ") ",
                   "exceeds the number of data points, reset to (",
                   .collapse(signal.integral, quote = FALSE), ")")
  }

  .validate_args(mode, c("profile", "surface"))
  .validate_logical_scalar(invert)
  .validate_logical_scalar(normalise)
  .validate_logical_scalar(plot)

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

  if (nrow(OSL) != nrow(IRSL)) {
    .throw_error("Sequence pattern not supported: the number of OSL records ",
                 "does not match the number of IRSL records")
  }

  ### get DARK counts ----------
  ### we assume that USER contains the dark count measurements
  DARK_COUNT <- .unlist_RLum(list(get_RLum(object, recordType = "USER")))

  ## we expect a sequence pattern with 3 DARK_COUNT records for each OSL/IRSL pair
  num.dark.count <- length(DARK_COUNT)
  if (num.dark.count %% 3 != 0) {
    .throw_error("Sequence pattern not supported: expected ", nrow(OSL) * 3,
                 " DARK_COUNT records, but found ", num.dark.count)
  }

  DARK_COUNT <- lapply(seq(1, num.dark.count, 3), function(x) DARK_COUNT[x:(x+2)])
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
    settings_sample <- unique(
        vapply(object, function(x) x@info$settings$Sample, character(1)))
    num.names <- length(settings_sample)
    if (num.names != length(RATIO)) {
      .throw_error("'object' references ", num.names, " sample names, but ",
                   "only ", length(RATIO), " IRSL/OSL pairs found")
    }
    coord <- .extract_PSL_coord(settings_sample)

  } else {
    .validate_class(coord, c("matrix", "list"))
    if(inherits(coord, "list"))
      coord <- do.call(rbind, coord)

    ## check length
    if(nrow(coord) != length(OSL$sum_signal))
      .throw_error("The number of coordinates in 'coord' should match the ",
                   "number of samples (", length(OSL$sum_signal), ")")
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
    if (!anyNA(coord[, 2]))
      summary <- summary[order(coord[,2]),]

   ### INVERT ----------
   if(invert)
     summary <- summary[nrow(summary):1,]

  if (mode == "surface" && plot && all(range(summary$COORD_X) == c(0, 0))) {
    plot <- FALSE
    message("[analyse_portableOSL()] Surface plot is not available when ",
            "all x-coordinates are 0, plot reset to FALSE")
  }

  # PLOTTING -------------------------------------------------------------------
  ## generate list of plot matrices
  ## this done to have consistent settings for all plot types
  parm <- grep("^(BSL|IRSL|DARK)", colnames(summary), value = TRUE)
  m_list <- lapply(parm, function(x){
     cbind(x = summary[["COORD_X"]], y = summary[["COORD_Y"]], value = summary[[x]])
  })

    ## correct names of the list
    names(m_list) <- parm

  if (plot) {

    ## add a few attributes to be used later
    attr(m_list, "xlim") <- lapply(m_list, function(x) range(x[,1]))
    attr(m_list, "ylim") <- if(invert) rev(range(m_list[[1]][,2])) else range(m_list[[1]][,2])
    attr(m_list, "zlim") <- lapply(m_list, function(x) range(x[,3]))

    ## account for surface case
    if (mode == "surface") {
      attr(m_list, "ylim") <- if (invert) rev(range(summary$COORD_Y)) else range(summary$COORD_Y)
      attr(m_list, "xlim") <- range(summary$COORD_X)
    }

   ## preset plot settings
   ## plot settings -------
   plot_settings <- modifyList(
     x = list(
       col_ramp = grDevices::heat.colors(30, rev = TRUE, alpha = 0.5),
       bg_img = NULL,
       bg_img_positions = NULL,
       surface_value = c("BSL", "IRSL", "IRSL_BSL_RATIO"),
       legend = TRUE,
       type = "b",
       cex = 1,
       col = c("blue", "red", "blue", "red", "black", "grey"),
       pch = rep(16, length(m_list)),
       xlim = attr(m_list, "xlim"),
       ylim = attr(m_list, "ylim"),
       zlim = if(mode == "surface") NA else attr(m_list, "zlim"),
       ylab = if (!anyNA(summary$COORD_Y)) "Depth [m]" else "Index",
       xlab = "x [m]",
       grid = TRUE,
       contour = FALSE,
       contour_nlevels = 10,
       contour_col = "grey",
       zlab = c("BSL", "IRSL", "BSL depl.", "IRSL depl.", "IRSL/BSL", "mean DARK"),
       main = summary$RUN[1]
     ),
     val = list(...), keep.null = TRUE)

    par.default <- par(no.readonly = TRUE)
    on.exit(par(par.default), add = TRUE)

    ## mode == "surface" ---------
    if (mode == "surface") {
     ### check for validity of surface value -------
     if(!all(plot_settings$surface_value %in% names(m_list)))
       .throw_error("Unknown value to plot, valid values are: ",
                    .collapse(names(m_list)))

     ## set par -------
     if(length(plot_settings$surface_value) > 1) {
       par(mfrow = c(2, 2))
     }

     ## loop over surface values -------
     for(i in plot_settings$surface_value) {
       ## set matrix for the plot
       m <- m_list[[i]]

       ## respect xlim and ylim range
       m <- m[m[, 2] >= min(plot_settings$ylim) &
              m[, 2] <= max(plot_settings$ylim), , drop = FALSE]
       m <- m[m[, 1] >= min(plot_settings$xlim) &
              m[, 1] <= max(plot_settings$xlim), , drop = FALSE]

       ## respect z_values
       if(!all(is.na(plot_settings$zlim)))
         m <- m[m[, 3] >= min(plot_settings$zlim) &
                m[, 3] <= max(plot_settings$zlim), , drop = FALSE]

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
                        "all points are arranged in one line or xlim/ylim/zlim ",
                        "are too tight. Nothing plotted!")

       ## show error
       if(!inherits(s, "try-error")) {
         par(mar = c(4.5, 4.5, 4, 3), xpd = FALSE)

         ## open empty plot
         plot(
           x = NA,
           y = NA,
           ylim = plot_settings$ylim,
           xlim = plot_settings$xlim,
           xlab = plot_settings$xlab,
           ylab = plot_settings$ylab,
           cex.lab = plot_settings$cex,
           cex.axis = plot_settings$cex,
           main = plot_settings$main)

         ## add background image if available -------
         if (!is.null(plot_settings$bg_img)) {
           ## get corner positions
           positions <- plot_settings$bg_img_positions[1:4]
           if (is.null(positions))
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
           graphics::contour(
             x = s$x,
             y = s$y,
             z = s$z,
             add = TRUE,
             nlevels = plot_settings$contour_nlevels,
             col = plot_settings$contour_col)

         ## add points
         points(m[, 1:2], pch = 20, cex = plot_settings$cex)

         ## add what is shown in the plot
         mtext(side = 3, text = i, cex = plot_settings$cex * 0.7)

         ## add legend
         if(plot_settings$legend) {
           par(xpd = NA)

           col_grad <- plot_settings$col_ramp[
             seq(1, length(plot_settings$col_ramp), length.out = 14)]

           slices <- seq(par()$usr[3],par()$usr[4],length.out = 15)

           for(s in 1:(length(slices) - 1)){
             graphics::rect(
               xleft = par()$usr[2] * 1,
               xright = par()$usr[2] * 1.02,
               ybottom = slices[s],
               ytop =  slices[s + 1],
               col = col_grad[s],
               border = TRUE)
           }

           ## add legend text
           text(
             x = par()$usr[2] * 1.03,
             y = par()$usr[4],
             labels = if(is.null(plot_settings$zlim_image)) {
               format(max(m[,3]), digits = 1, scientific = TRUE)
             } else {
               format(plot_settings$zlim_image[2], digits = 1, scientific = TRUE)
             },
             cex = plot_settings$cex * 0.6,
             srt = 270,
             pos = 3)

           text(
             x = par()$usr[2] * 1.03,
             y = par()$usr[3],
             labels = if(is.null(plot_settings$zlim_image)) {
               format(min(m[,3]), digits = 1, scientific = TRUE)
             } else {
               format(plot_settings$zlim_image[1], digits = 1, scientific = TRUE)
             },
             cex = plot_settings$cex * 0.6,
             pos = 3,
             srt = 270)

           ## add legend labelling (central)
           text(
             x = par()$usr[2] * 1.035,
             y = (par()$usr[4] - par()$usr[3])/2 + par()$usr[3],
             labels = "Intensity [a.u.]",
             cex = plot_settings$cex * 0.7,
             pos = 3,
             srt = 270)
         }
       }
     }# end for loop
    }

    ## mode == "profile" ---------
    if (mode == "profile") {

    # default: par(mar = c(5, 4, 4, 2) + 0.1) // bottom, left, top, right
    par(mfrow = c(1, 7))
    par(mar = c(5, 4, 4, 1) + 0.1)

    graphics::frame()

      mtext(side = 3, plot_settings$main, line = 2,
            cex = plot_settings$cex * 0.7)

    par(mar = c(5, 0, 4, 1) + 0.1)

    ## make sure that wrong zlim settings do not screw up the function
    if(!inherits(plot_settings$zlim, "list")) {
      .throw_warning("In profile mode, zlim needs to be provided as a named ",
                     "list, example: list(BSL = c(0,1)). Reset to default")
      plot_settings$zlim <- attr(m_list, "zlim")
    }

      ## plot the profile for each measure
      profile.parts <- c("BSL", "IRSL", "BSL_depletion", "IRSL_depletion",
                         "IRSL_BSL_RATIO", "DARK")
      for (prof in profile.parts) {
        idx <- match(prof, profile.parts)

        xlim <- plot_settings$zlim[[prof]]
        if (prof == "DARK") {
          xlim <- range(c(
              plot_settings$zlim[["DARK"]] - plot_settings$zlim[["DARK_error"]],
              plot_settings$zlim[["DARK"]] + plot_settings$zlim[["DARK_error"]]))
        }
        plot(
            NA,
            NA,
            ylim = plot_settings$ylim,
            xlim = xlim,
            xlab = plot_settings$zlab[idx],
            ylab = "",
            cex.lab = plot_settings$cex,
            cex.axis = plot_settings$cex,
            bty = "n",
            yaxt = "n"
        )
        if (plot_settings$grid)
          graphics::grid()

        x.val <- m_list[[prof]][, "value"]
        y.val <- m_list[[prof]][, "y"]
        lines(
            x = x.val,
            y = y.val,
            type = plot_settings$type,
            cex = plot_settings$cex,
            lty = ifelse(prof %in% c("BSL_depletion", "IRSL_depletion"), 2, 1),
            pch = plot_settings$pch[idx],
            col = plot_settings$col[idx]
        )

        ## add error bars
        if (prof %in% c("BSL", "IRSL", "DARK")) {
          x.err <- m_list[[paste0(prof, "_error")]][, "value"]
          segments(
              x0 = x.val - x.err,
              x1 = x.val + x.err,
              y0 = y.val,
              y1 = y.val,
              col = plot_settings$col[idx])
        }

        axis(3, cex.axis = plot_settings$cex)

        ## add general axis labels
        if (idx == 1) {
          labs <- pretty(y.val, n = 10)
          axis(2, line = 3, at = labs, labels = labs,
               cex.axis = plot_settings$cex)
          mtext(plot_settings$ylab[1], side = 2, line = 6,
                cex = plot_settings$cex * 0.8)
        }
      }

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
.posl_get_signal <- function(x, sigint) {
    raw_signal <- get_RLum(x)[,2]
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

## This function extracts the coordinates from the sample names
.extract_PSL_coord <- function(settings_sample) {
  ## set character vector
  tmp_coord <- character(length(settings_sample))

  ## search for pattern match ... why?
  ## because otherwise the dataset becomes inconsistent
  pattern <- "_x:[0-9].*\\|y:[0-9].*"
  pattern_match <- grepl(
    pattern = pattern,
    x = settings_sample, perl = TRUE)

  ## extract coordinates
  tmp_coord[pattern_match] <- regexpr(
    pattern = pattern,
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
  if (anyNA(coord[, 1])) coord[, 1] <- 0
  if (anyNA(coord[, 2])) coord[, 2] <- 1:nrow(coord)

  return(coord)
}

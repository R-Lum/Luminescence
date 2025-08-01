#' @title Create Regions of Interest (ROI) Graphic
#'
#' @description The function creates ROI graphic with data extracted from the
#' data imported via [read_RF2R]. This function might be of use to work with
#' reduced data from spatially resolved measurements.
#' The plot dimensions mimic the original image dimensions.
#'
#' @param object [RLum.Analysis-class], [RLum.Results-class] or a [list] of such objects (**required**):
#' input data created either by [read_RF2R] or [extract_ROI].
#'
#'@param exclude_ROI [numeric] (*with default*): option to remove particular ROIs from the
#'analysis. Those ROIs are plotted but not coloured and not taken into account
#'in distance analysis. `NULL` excludes nothing.
#'
#'@param dist_thre [numeric] (*optional*): euclidean distance threshold in pixel
#'distance. All ROI for which the euclidean distance is smaller are marked. This
#'helps to identify ROIs that might be affected by signal cross-talk. Note:
#'the distance is calculated from the centre of an ROI, e.g., the threshold
#'should include consider the ROIs or grain radius.
#'
#'@param dim.CCD [numeric] (*optional*): metric x and y for the recorded (chip)
#'surface in µm. For instance `c(8192,8192)`, if set additional x and y-axes are shown
#'
#' @param bg_image [RLum.Data.Image-class] (*optional*): background image object
#' (please note that dimensions are not checked).
#'
#' @param plot [logical] (*with default*): enable/disable the plot output.
#'
#'@param ... further parameters to manipulate the plot. On top of all arguments of
#'[graphics::plot.default] the following arguments are supported: `lwd.ROI`, `lty.ROI`,
#'`col.ROI`, `col.pixel`, `text.labels`, `text.offset`, `grid` (`TRUE/FALSE`), `legend` (`TRUE/FALSE`),
#'`legend.text`, `legend.pos`
#'
#'@return An ROI plot and an [RLum.Results-class] object with a matrix containing
#'the extracted ROI data and a object produced by [stats::dist] containing
#'the euclidean distance between the ROIs.
#'
#'@section Function version: 0.2.0
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [read_RF2R], [extract_ROI]
#'
#'@keywords datagen plot
#'
#'@examples
#'
#'## simple example
#'file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
#'temp <- read_RF2R(file)
#'plot_ROI(temp)
#'
#'## in combination with extract_ROI()
#'m <- matrix(runif(100,0,255), ncol = 10, nrow = 10)
#'roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
#'t <- extract_ROI(object = m, roi = roi)
#'plot_ROI(t, bg_image = m)
#'
#'@export
plot_ROI <- function(
  object,
  exclude_ROI = c(1),
  dist_thre = -Inf,
  dim.CCD = NULL,
  bg_image = NULL,
  plot = TRUE,
  ...
) {
  .set_function_name("plot_ROI")
  on.exit(.unset_function_name(), add = TRUE)

  ##helper function to extract content
  .spatial_data <- function(x) {
    .validate_class(x, c("RLum.Analysis", "RLum.Results"),
                    extra = "a 'list' of such objects",
                    name = "'object'")

    ##ignore all none RLum.Analysis
    if (!inherits(x, "RLum.Analysis") || x@originator != "read_RF2R")
      .throw_error("Object originator '", x@originator, "' not supported")

    ##extract some of the elements
    info <- x@info
    info$ROI <- strsplit(split = " ", info$ROI, fixed = TRUE)[[1]][2]
    info <- info[c("ROI", "x", "y", "area", "width", "height",
                    "image_width", "image_height", "grain_d")]
    names(info) <- gsub("image_", "img_", names(info))
    unlist(lapply(info, as.numeric))
  }

  if (inherits(object, "RLum.Results") &&
      ## use %in% instead of == to support the case when originator is NULL
      object@originator %in% "extract_ROI") {
    m <- object@data$roi_coord

  } else {
    ## make sure the object is a list
    if(!is.list(object)) object <- list(object)
    .validate_not_empty(object, "list")

    ##extract values and convert to numeric matrix
    m <- t(vapply(object, .spatial_data, numeric(length = 9)))

    ## correct coordinates (they come in odd from the file)
    m[,"x"] <- m[,"x"] + m[,"width"] / 2
    m[,"y"] <- m[,"y"] + m[,"height"] / 2
  }

  .validate_positive_scalar(exclude_ROI, int = TRUE, null.ok = TRUE)

  ##make sure the ROI selection works
  if (is.null(exclude_ROI))
    exclude_ROI <- nrow(m) + 1

  ## distance calculation
  euc_dist <- sel_euc_dist <- stats::dist(m[-exclude_ROI])

  ## add half_width and half_height
  m <- cbind(m, half_width = m[, "width"] / 2, half_height = m[, "height"] / 2)
  rownames(m) <- m[, "ROI"]

  ## distance threshold selector
  sel_euc_dist[sel_euc_dist < dist_thre[1]] <- NA
  sel_euc_dist <- suppressWarnings(as.numeric(rownames(na.exclude(as.matrix(sel_euc_dist)))))

  ## add information to matrix
  m <- cbind(m, dist_sel = FALSE)
  m[m[,"ROI"]%in%sel_euc_dist,"dist_sel"] <- TRUE

  ## --- Plotting ---
  if(plot) {
    plot_settings <- modifyList(x = list(
      xlim = c(0, max(m[, "img_width"])),
      ylim = c(max(m[, "img_height"]), 0),
      xlab = "width [px]",
      ylab = "height [px]",
      main = "Spatial ROI Distribution",
      frame.plot = FALSE,
      lwd.ROI = 0.75,
      lty.ROI = 2,
      col.ROI = "black",
      col.pixel = rgb(0,1,0,0.6),
      text.labels = m[,"ROI"],
      text.offset = 0.3,
      grid = FALSE,
      legend = TRUE,
      legend.text = c("ROI", "sel. pixel", "> dist_thre"),
      legend.pos = "topright"
    ), val = list(...))

    ## set plot area
    do.call(
      what = graphics::plot.default,
      args = c(x = NA, y = NA,
               plot_settings[names(plot_settings) %in%
                             methods::formalArgs(graphics::plot.default)])
    )

    ## add background image if available
    if(!is.null(bg_image)){
      a <- try({
        as(bg_image, "RLum.Data.Image")
      }, silent = TRUE)
      if(inherits(a, "try-error")) {
        .throw_warning("'bg_image' is not of class 'RLum.Data.Image' and ",
                       "cannot be converted into it, background image plot skipped")
      } else {
        a <- a@data
        graphics::image(
          x = 1:nrow(a[, , 1]),
          y = 1:ncol(a[, , 1]),
          z = a[,order(1:dim(a)[2], decreasing = TRUE),1],
          add = TRUE,
          col = grDevices::hcl.colors(20, "inferno", rev = FALSE),
          useRaster = TRUE)
      }
   }

    if (plot_settings$grid) graphics::grid(nx = max(m[, "img_width"]),
                                           ny = max(m[, "img_height"]))

    ## plot metric scale
    if (!is.null(dim.CCD)) {
      axis(
        side = 1,
        at = axTicks(1),
        labels = paste(floor(dim.CCD[1] / max(m[,"img_width"]) * axTicks(1)), "\u00b5m"),
        lwd = -1,
        lwd.ticks = -1,
        line = -2.2,
        cex.axis = 0.8
      )
      axis(
        side = 2,
        at = axTicks(2)[-1],
        labels = paste(floor(dim.CCD[2] / max(m[,"img_height"]) * axTicks(2)), "\u00b5m")[-1],
        lwd = -1,
        lwd.ticks = -1,
        line = -2.2,
        cex.axis = 0.8
      )
    }

    ## add circles and rectangles
    for (i in 1:nrow(m)) {
      if (!i%in%exclude_ROI) {
        ## mark selected pixels
        x0 <- m[i, "x"] - m[i, "half_width"]
        x1 <- m[i, "x"] + m[i, "half_width"]
        y0 <- m[i, "y"] - m[i, "half_height"]
        y1 <- m[i, "y"] + m[i, "half_height"]
        polygon(
          x = c(x0, x0, x1, x1),
          y = c(y0, y1, y1, y0),
          col = plot_settings$col.pixel
        )
      }

      ## add ROIs
      shape::plotellipse(
        rx = m[i, "half_width"],
        ry = m[i, "half_width"],
        mid = c(m[i, "x"], m[i, "y"]),
        lcol = plot_settings$col.ROI,
        lty = plot_settings$lty.ROI,
        lwd = plot_settings$lwd.ROI)
    }

    ## add distance marker
    points(
      x = m[!m[,"ROI"]%in%sel_euc_dist & !m[,"ROI"]%in%exclude_ROI, "x"],
      y = m[!m[,"ROI"]%in%sel_euc_dist & !m[,"ROI"]%in%exclude_ROI, "y"],
      pch = 4,
      col = "red")

    ## add text
    if(length(m[-exclude_ROI,"x"]) > 0) {
      graphics::text(
         x = m[-exclude_ROI, "x"],
         y = m[-exclude_ROI, "y"],
         labels = plot_settings$text.labels[-exclude_ROI],
         cex = 0.6,
         col = if(!is.null(bg_image)) "white" else "black",
         pos = 1,
         offset = plot_settings$text.offset
       )
    }

    ##add legend
    if(plot_settings$legend) {
      legend(
        plot_settings$legend.pos,
        bty = "",
        pch  = c(1, 15, 4),
        box.lty = 0,
        bg = rgb(1,1,1,0.7),
        legend = plot_settings$legend.text,
        col = c(plot_settings$col.ROI, plot_settings$col.pixel, "red")
      )
    }

  }##end if plot

  ## return results
  invisible(set_RLum(
    class = "RLum.Results",
    data = list(
      ROI = m,
      euc_dist = euc_dist),
    info = list(
      call = sys.call()
    )))
}

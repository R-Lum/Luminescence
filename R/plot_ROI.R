#'@title Create Regions of Interest (ROI) Graphic
#'
#'@decription Create ROI graphic with data extracted from the data imported
#'via [read_RF2R]. This function is used internally by [analyse_IRSAR.RF] but
#'might be of use to work with reduced data from spatially resolved measurements.
#'The plot dimensions mimic the original image dimensions
#'
#'@param object [RLum.Analysis-class] or a [list] of such objects (**required**):
#'data input. Please note that to avoid function errors, only input created
#'by the function [read_RF2R] is accepted
#'
#'@param plot [logical] (*with default*): enable or disable plot output to use
#'the function only to extract the ROI data
#'
#'@param ... further parameters to manipulate the plot. On top of all arguments of
#'[graphics::plot.default] the following arguments are supported: `lwd.ROI`, `lty.ROI`,
#'`col.ROI`, `col.pixed`, `text.labels`, `text.offset`, `grid` (`TRUE/FALSE`), `legend` (`TRUE/FALSE`),
#'`legend.text`, `legend.pos`
#'
#'@return An ROI plot and an [RLum.Results-class] object with a matrix containing
#'the extracted ROI data.
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Department of Geography & Earth Sciences, Aberystwyth University
#' (United Kingdom)
#'
#'@seealso [read_RF2R], [analyse_IRSAR.RF]
#'
#'@keywords datagen plot
#'
#'@examples
#'file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
#'temp <- read_RF2R(file)
#'plot_ROI(temp)
#'
#'@md
#'@export
plot_ROI <- function(
  object,
  plot = TRUE,
  ...) {
  ##extract content helper function
  .spatial_data <- function(x) {
    ##ignore all none RLum.Analysis
    if (class(x) != "RLum.Analysis" || x@originator != "read_RF2R") {
      stop("[plot_RegionsOfInterest()] At least one input element is not of type 'RLum.Analysis' and/or does
      was not produced by 'read_RF2R()`!", call. = FALSE)
    }

    ##extract some of the elements
    info <- x@info
    info$ROI <- strsplit(split = " ", info$ROI, fixed = TRUE)[[1]][2]

    c(ROI = info$ROI,
      x = info$x,
      y = info$y,
      width = info$width,
      height = info$height,
      img_width = info$image_width,
      img_height = info$image_height,
      grain_d = info$grain_d)

  }

  ## make sure the object is a list
  if(!is.list(object)) object <- list(object)

  ##extract values and convert to numeric matrix
  m <- t(vapply(object, .spatial_data, character(length = 8)))

  ##make numeric
  storage.mode(m) <- "numeric"

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
      col.pixel = "green",
      text.labels = m[,"ROI"],
      text.offset = 0.3,
      grid = FALSE,
      legend = TRUE,
      legend.text = c("ROI", "sel. pixel"),
      legend.pos = "topright"
    ), val = list(...))

    ## set plot area
    do.call(
      what = plot.default,
      args = c(x = NA, y = NA,
               plot_settings[names(plot_settings) %in% methods::formalArgs(plot.default)])
    )

    if (plot_settings$grid) grid(nx = max(m[,"img_width"]), ny = max(m[,"img_height"]))

    ## add circles and rectangles
    for (i in 1:nrow(m)) {
      if (i > 1) {
        ## mark selected pixels
        polygon(
          x = c(m[i, "x"], m[i, "x"], m[i, "x"] + m[i, "width"], m[i, "x"] + m[i, "width"]),
          y = c(m[i, "y"], m[i, "y"] + m[i, "height"], m[i, "y"] + m[i, "height"], m[i, "y"]),
          col = plot_settings$col.pixel
        )
      }

      ## add ROIs
      shape::plotellipse(
        rx = m[i, "width"] / 2,
        ry = m[i, "width"] / 2,
        mid = c(m[i, "x"] + m[i, "width"] / 2, m[i, "y"] + m[i, "height"] / 2),
        lcol = plot_settings$col.ROI,
        lty = plot_settings$lty.ROI,
        lwd = plot_settings$lwd.ROI
      )

      if (i > 1) {
        ## add text
        graphics::text(
          x = m[i, "x"],
          y = m[i, "y"],
          labels = plot_settings$text.labels[i],
          cex = 0.6,
          pos = 3,
          offset = plot_settings$text.offset
        )
      }
    }

    ##add legend
    if(plot_settings$legend) {
      legend(
        plot_settings$legend.pos,
        bty = "n",
        pch  = c(1, 15),
        legend = plot_settings$legend.text,
        col = c(plot_settings$col.ROI, plot_settings$col.pixel)
      )
    }

  }##end if plot

  ## return results
  return(set_RLum("RLum.Results", data = list(ROI = m)))

}

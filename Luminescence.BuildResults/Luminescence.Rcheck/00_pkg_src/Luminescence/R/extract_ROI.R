#'@title Extract Pixel Values through Circular Region-of-Interests (ROI) from an Image
#'
#'@description Light-weighted function to extract pixel values from pre-defined regions-of-interest (ROI) from
#'[RLum.Data.Image-class], [array] or [matrix] objects and provide simple image processing
#'capacity. The function is limited to circular ROIs.
#'
#'@details The function uses a cheap approach to decide whether a pixel lies within
#'a circle or not. It assumes that pixel coordinates are integer values and
#'that a pixel centring within the circle is satisfied by:
#'
#'\deqn{x^2 + y^2 <= (d/2)^2}
#'
#'where \eqn{x} and \eqn{y} are integer pixel coordinates and \eqn{d} is the integer
#'diameter of the circle in pixel.
#'
#'@param object [RLum.Data.Image-class], [array] or [matrix] (**required**): input image data
#'
#'@param roi [matrix] (**required**): matrix with three columns containing the centre coordinates
#'of the ROI (first two columns) and the diameter of the circular ROI. All numbers must by of type [integer]
#'and will forcefully coerced into such numbers using `as.integer()` regardless.
#'
#'@param roi_summary (**with default**): if `"mean"` (the default) defines what is returned
#'in the element `roi_summary`; alternatively `"mean"`, `"median"`, `"sd"` or `"sum"` can be chosen.
#'Pixel values are conveniently summarised using the above defined keyword.
#'
#'@param plot [logical] (*optional*): enables/disables control plot. Only the first
#'image frame is shown
#'
#'@return [RLum.Results-class] object with the following elements:
#'`..$roi_signals`: a named [list] with all ROI values and their coordinates
#'`..$roi_summary`: an [matrix] where rows are frames from the image, and columns are different ROI
#'The element has two attributes: `summary` (the method used to summarise pixels) and `area` (the pixel area)
#'`..$roi_coord`: a [matrix] that can be passed to [plot_ROI]
#'
#'If `plot = TRUE` a control plot is returned.
#'
#'@section Function version: 0.1.0
#'
#'@author
#'Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@seealso [RLum.Data.Image-class]
#'
#'@keywords manip
#'
#'@examples
#'
#' m <- matrix(runif(100,0,255), ncol = 10, nrow = 10)
#' roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
#' extract_ROI(object = m, roi = roi, plot = TRUE)
#'
#'@md
#'@export
extract_ROI <- function(
  object,
  roi,
  roi_summary = "mean",
  plot = FALSE
){

# Self call ---------------------------------------------------------------
  if (is(object, "list"))
    return(merge_RLum(lapply(object, extract_ROI, roi = roi, plot = plot)))

# Check input -------------------------------------------------------------
  ## check input for ROIs
  if (!is.matrix(roi) || nrow(roi) < 1 || ncol(roi) < 3)
    stop("[extract_ROI()] Please check the format of roi, it looks wrong!", call. = FALSE)

  ## check input for object
  if (!is(object, "matrix") && !is(object, "array") &&  !is(object, "RLum.Data.Image"))
    stop("[extract_ROI()] Input for argument 'object' not supported!", call. = FALSE)

  ## calculate the radius
  roi <- roi[,1:3]
  roi[,3] <- ceiling(roi[,3]/2)

  ## make sure that we have integer values only in the matrix
  roi[] <- as.integer(roi)

  ## copy object (to not work on the input data)
  a <- object

  ## try to convert into something meaningful
  if (is(object, "RLum.Data.Image"))
    a <- object@data

  if (is(object, "matrix"))
    a <- array(data = object, dim = c(nrow(object), ncol(object), 1))

# Helper function ---------------------------------------------------------
  .extract_pixel <- function(m, r, mid) {
    ## get row - column combinations
    grid <- as.matrix(expand.grid(x = 1:nrow(m), y = 1:ncol(m)))

    ## adjust values for mid point
    ## + get pixel coordinates if within the circle
    px_id <- grid[(grid[,"x"] - mid[1])^2 + (grid[,"y"]  - mid[2])^2 <= r[1]^2,]

    ## extract values from matrix
    px_extract <- NA
    if (nrow(px_id) > 0) {
      px_extract <- vapply(1:nrow(px_id), function(x) {
        m[px_id[x,1],px_id[x,2]]

      }, numeric(1))
    }

    attr(px_extract, "coord") <- px_id
    return(px_extract)
  }

# Extract ROIs ------------------------------------------------------------
   roi_signals <- lapply(1:nrow(roi), function(x){
    ## iterate through a stack if needed
    temp <- lapply(1:(dim(a)[3]), function(z){
      .extract_pixel(a[,,z], roi[x,3], mid = roi[x,1:2])

    })

    ## compile into matrix
    m <- matrix(unlist(temp), nrow = length(temp[[1]]))

    ## add attributes ... including coordinates; but only one time
    colnames(m) <- paste0("frame_", 1:ncol(m))
    attr(m, "px_coord") <- attr(temp[[1]], "coord")
    return(m)

  })

  ## add names
  names(roi_signals) <- paste0("ROI_", 1:nrow(roi))

# Plot check --------------------------------------------------------------
  if (plot) {
    ## this is a control plot, so we plot only the first image; nothing more

    ## image
    graphics::image(
      x = 1:nrow(a[, , 1]),
      y = 1:ncol(a[, , 1]),
      a[, , 1],
      ylab = "y-dim [px]",
      xlab = "x-dim [px]",
      useRaster = TRUE,
      main = "extract_ROIs() - control plot")
    box()

    ## visualise ROIs
    overlay <- a[,,1]
    overlay[] <- 0
    for (i in 1:length(roi_signals))
      overlay[attr(roi_signals[[i]], "px_coord")[,1], attr(roi_signals[[i]], "px_coord")[,2]] <- 1

    ## marked ROIs
    graphics::image(
      x = 1:nrow(a[, , 1]),
      y = 1:ncol(a[, , 1]),
      overlay,
      axes = FALSE,
      add = TRUE,
      useRaster = TRUE,
      col = c(rgb(1, 1, 1, 0), rgb(0, 1, 0, 0.5)))

    ## add circles and points
    for (i in 1:nrow(roi)) {
      lines(shape::getellipse(rx = roi[i, 3], mid = c(roi[i, 1:2], dr = 0.1)), col = "red", lwd = 1.5)
      text(x = roi[i,1], y = roi[i,2], i, col = "black", cex = 1.2)

    }

  }

# ROI summary -------------------------------------------------------------
  ## set roi fun and avoid add input
  if(!any(roi_summary[1]%in%c("mean", "median", "sd", "sum")))
    stop("[extract_ROI()] roi_summary method not supported, check manual!", call. = FALSE)

  ## set function
  roi_fun <- roi_summary[1]

  ## create summary using matrixStats
  roi_summary <- matrix(unlist(
    switch(roi_fun,
      "mean" = lapply(roi_signals,  matrixStats::colMeans2),
      "median" = lapply(roi_signals,  matrixStats::colMedians),
      "sd" = lapply(roi_signals,  matrixStats::colSds),
      "sum" = lapply(roi_signals,  matrixStats::colSums2))),
    ncol = length(roi_signals))

  ## set names to make it easier
  colnames(roi_summary) <- names(roi_signals)
  rownames(roi_summary) <- paste0("frame_", 1:nrow(roi_summary))
  attr(roi_summary, "summary") <- roi_fun
  attr(roi_summary, "area") <- vapply(roi_signals, nrow, numeric(1))

  ## add more roi information to the output for further processing
  roi <- cbind(
    ROI = 1:nrow(roi),
    x = roi[,1],
    y = roi[,2],
    area = attr(roi_summary, "area"),
    width = vapply(roi_signals, function(x) diff(range(attr(x, "px_coord")[,"x"])), numeric(1)),
    height = vapply(roi_signals, function(x) diff(range(attr(x, "px_coord")[,"y"])), numeric(1)),
    img_width = nrow(a[, , 1]),
    img_height = ncol(a[, , 1]),
    grain_d = roi[,3])

# Return ------------------------------------------------------------------
  return(
    set_RLum(
      class = "RLum.Results",
      data = list(
        roi_signals = roi_signals,
        roi_summary = roi_summary,
        roi_coord = roi),
      info = list(
        call = sys.call())))

}

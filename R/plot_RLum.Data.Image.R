#' @title  Plot function for an `RLum.Data.Image` S4 class object
#'
#' @description The function provides a standardised plot output for image data of an
#' `RLum.Data.Image`S4 class object, mainly using the plot functions
#' provided by the [raster] package.
#'
#' **Details on the plot functions**
#'
#' Image is visualised as 2D plot using generic plot types provided by other
#' packages.
#'
#' Supported plot types:
#'
#' **`plot.type = "plot.raster"`**
#'
#' Uses the standard plot function for raster data from the package
#' [raster::raster]: [raster::plot]. For each raster layer in a
#' raster brick one plot is produced.
#'
#' Arguments that are passed through the function call:
#'
#' `main`,`axes`, `xlab`, `ylab`, `xlim`, `ylim`,`zlim`, `col`, `stretch` (`NULL` no
#' stretch)
#'
#' **`plot.type = "plotRGB"`**
#'
#' Uses the function [raster::plotRGB] from the
#' [raster::raster] package. Only one image plot is produced as all layers
#' in a brick a combined.  This plot type is useful to see whether any signal
#' is recorded by the camera.\cr
#' Arguments that are passed through the function call:
#'
#' `main`,`axes`, `xlab`, `ylab`, `ext`, `interpolate`, `maxpixels`,
#' `alpha`, `colNA`, `stretch`
#'
#' **`plot.type = "contour"`**
#'
#' Uses the function contour plot function from the [raster]
#' function ([raster::contour]). For each raster layer one contour
#' plot is produced. Arguments that are passed through the function call:\cr
#'
#' `main`,`axes`, `xlab`, `ylab`, `xlim`, `ylim`,
#' `col`
#'
#' @param object [RLum.Data.Image-class] (**required**): S4
#' object of class `RLum.Data.Image`
#'
#' @param par.local [logical] (*with default*): use local graphical
#' parameters for plotting, e.g. the plot is shown in one column and one row.
#' If `par.local = FALSE` global parameters are inherited.
#'
#' @param plot.type [character] (*with default*): plot types.
#' Supported types are `plot.raster`, `plotRGB` or `contour`
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to the specific plot functions.
#'
#' @return Returns a plot.
#'
#' @section Function version: 0.1.1
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @seealso [RLum.Data.Image-class], [plot], [plot_RLum], [raster::raster]
#'
#' @keywords aplot
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.RLum.Data.Image, envir = environment())
#'
#' ##plot data
#' plot_RLum.Data.Image(ExampleData.RLum.Data.Image)
#'
#' @md
#' @export
plot_RLum.Data.Image <- function(
  object,
  par.local = TRUE,
  plot.type = "plot.raster",
  ...
){

# Integrity check -----------------------------------------------------------
  ##check if object is of class RLum.Data.Image
  if(class(object) != "RLum.Data.Image")
    stop("[plot_RLum.Data.Image()] Input object is not of type RLum.Data.Image.", call. = FALSE)

  ## extract object
  object <- object@data

# Plot settings -----------------------------------------------------------
plot_settings <- modifyList(x = list(
    main = "RLum.Data.Image",
    axes = TRUE,
    xlab = "Length [px]",
    ylab = "Height [px]",
    xlim = c(0,dim(object)[2]),
    ylim = c(0,dim(object)[1]),
    zlim = range(c(object@data@min, object@data@max)),
    ext = NULL,
    interpolate = FALSE,
    stretch = "hist",
    maxpixels = dim(object)[1]*dim(object)[2],
    alpha = 255,
    colNA = "white",
    col = grDevices::hcl.colors(50, palette = "Inferno"),
    cex = 1
  ), val = list(...), keep.null = TRUE)


  ##par setting for possible combination with plot method for RLum.Analysis objects
  if(par.local) par(mfrow=c(1,1), cex = plot_settings$cex)

  if(plot.type == "plotRGB"){
    ## plot.type: plotRGB -----
    raster::plotRGB(
      x = object,
      main = plot_settings$main,
      axes = plot_settings$axes,
      xlab = plot_settings$xlab,
      ylab = plot_settings$ylab,
      ext = plot_settings$ext,
      interpolate = plot_settings$interpolate,
      maxpixels = plot_settings$maxpixels,
      alpha = plot_settings$alpha,
      colNA = plot_settings$colNA,
      stretch = plot_settings$stretch)

  }else if(plot.type == "plot.raster"){
    # plot.type: plot.raster -----
    if(!is.null(plot_settings$stretch)) {
    object <- raster::stretch(
      object,
      minq = .02,
      maxq = .98,
      minv = plot_settings$zlim[1],
      maxv = plot_settings$zlim[2],
      )
    }

    if(class(object)[1] != "RasterBrick") object <- raster::brick(object)

    raster::plot(
      object,
      main = paste(plot_settings$main, "#", 1:object@data@nlayers),
      xlim = plot_settings$xlim,
      ylim = plot_settings$ylim,
      zlim = plot_settings$zlim,
      xlab = plot_settings$xlab,
      ylab = plot_settings$ylab,
      col = plot_settings$col,
      interpolate = plot_settings$interpolate
    )

  }else if(plot.type == "contour"){
    ## plot.type: contour ----
    for(i in 1:raster::nlayers(object)){
      if(!is.null(plot_settings$stretch)) {
        object <- raster::stretch(
          object,
          minq = .02,
          maxq = .98,
          minv = plot_settings$zlim[1],
          maxv = plot_settings$zlim[2]
        )
      }

      raster::contour(
        raster::raster(object, layer = i),
        main = plot_settings$main,
        xlim = plot_settings$xlim,
        ylim = plot_settings$ylim,
        xlab = plot_settings$xlab,
        ylab = plot_settings$ylab,
        col = plot_settings$col
      )
    }

  }else{
    stop("[plot_RLum.Data.Image()] Unknown plot type.", call. = FALSE)

  }

}

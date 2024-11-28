#' @title  Plot function for an `RLum.Data.Image` S4 class object
#'
#' @description The function provides very basic plot functionality for image data of an
#' [RLum.Data.Image-class] object. For more sophisticated plotting it is recommended
#' to use other very powerful packages for image processing.
#'
#'
#' **Details on the plot functions**
#'
#' Supported plot types:
#'
#' **`plot.type = "plot.raster"`**
#'
#' Uses the standard plot function of R [graphics::image]. If wanted, the image
#' is enhanced, using the argument `stretch`. Possible values are `hist`, `lin`, and
#' `NULL`. The latter does nothing. The argument `useRaster = TRUE` is used by default, but
#' can be set to `FALSE`.
#'
#' **`plot.type = "contour"`**
#'
#' This uses the function [graphics::contour]
#'
#' @param object [RLum.Data.Image-class] (**required**): S4
#' object of class `RLum.Data.Image`
#'
#' @param par.local [logical] (*with default*): use local graphical
#' parameters for plotting, e.g. the plot is shown in one column and one row.
#' If `par.local = FALSE` global parameters are inherited.
#'
#' @param frames [numeric] (*optional*): sets the frames to be set, by default all
#' frames are plotted. Can be sequence of numbers, as long as the frame number is valid.
#'
#' @param plot.type [character] (*with default*): plot types.
#' Supported types are `plot.raster`, `contour`
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to the specific plot functions. Standard supported parameters are `xlim`, `ylim`, `zlim`,
#' `xlab`, `ylab`, `main`, `legend` (`TRUE` or `FALSE`), `col`, `cex`, `axes` (`TRUE` or `FALSE`),
#' `zlim_image` (adjust the z-scale over different images), `stretch`
#'
#' @return Returns a plot
#'
#' @note The axes limitations (`xlim`, `zlim`, `zlim`) work directly on the object,
#' so that regardless of the chosen limits the image parameters can be adjusted for
#' best visibility. However, in particular for z-scale limitations this is not always
#' wanted, please use `zlim_image` to maintain a particular value range over a
#' series of images.
#'
#' @section Function version: 0.2.1
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Data.Image-class], [plot], [plot_RLum], [graphics::image], [graphics::contour]
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
  frames = NULL,
  par.local = TRUE,
  plot.type = "plot.raster",
  ...
) {
  .set_function_name("plot_RLum.Data.Image")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(object, "RLum.Data.Image")

  ## extract object
  object <- object@data
  if (length(dim(object)) != 3) {
    ## the object is empty or malformed
    return(NULL)
  }

# Define additional functions ---------------------------------------------
.stretch <- function(x, type = "lin"){
  if(is.null(type[1])) return(x[,,1])

  if(type[1] == "lin") {
    x <- x[,,1]
    r <- range(x)
    q <- stats::quantile(x, c(0.05, 0.95), na.rm = TRUE)

    ## consider special case for q == 0
    if(sum(q) > 0) {
      x <- (r[2] * (x - q[1])) / (q[2] - q[1])
      x[x < 0] <- r[1]
      x[x > r[2]] <- r[2]
    }
  }

  if(type[1] == "hist")
    x <- matrix(stats::ecdf(x)(x) * 255, ncol = ncol(x))

  return(x)
}

 plot.type <- .validate_args(plot.type, c("plot.raster", "contour"))

# Plot settings -----------------------------------------------------------
plot_settings <- modifyList(x = list(
    main = "RLum.Data.Image",
    axes = TRUE,
    xlab = "Length [px]",
    ylab = "Height [px]",
    xlim = c(1,dim(object)[1]),
    ylim = c(1,dim(object)[2]),
    zlim = range(object),
    zlim_image = NULL,
    legend = TRUE,
    useRaster = TRUE,
    stretch = "hist",
    col = c(grDevices::hcl.colors(50, palette = "Inferno")),
    cex = 1
  ), val = list(...), keep.null = TRUE)

  ## set frames
  if(!is.null(frames)) {
    frames[1] <- max(1,min(frames))
    frames[length(frames)] <- min(dim(object)[3],max(frames))
    object <- object[,,frames,drop = FALSE]
  }

  ## enforce xlim, ylim and zlim directly here
  ## xlim, ylim
  object[] <- object[
    max(plot_settings$xlim[1], 1):min(plot_settings$xlim[2], dim(object)[1]),
    max(plot_settings$ylim[1], 1):min(plot_settings$ylim[2], dim(object)[2]),,
    drop = FALSE]

  ## zlim
  object[object <= plot_settings$zlim[1]] <- max(0,plot_settings$zlim[1])
  object[object >= plot_settings$zlim[2]] <- min(max(object),plot_settings$zlim[2])

  ##par setting for possible combination with plot method for RLum.Analysis objects
  if(par.local) par(mfrow=c(1,1), cex = plot_settings$cex)

  if (plot.type == "plot.raster") {
# plot.raster -------------------------------------------------------------
    for(i in 1:dim(object)[3]) {
      par.default <- par(mar = c(4.5,4.5,4,3))
      on.exit(par(par.default), add = TRUE)
      x <- object[, , i, drop = FALSE]
      image <-.stretch(x, type = plot_settings$stretch)

      graphics::image(
        x = image,
        useRaster = plot_settings$useRaster,
        axes = FALSE,
        zlim = if(is.null(plot_settings$zlim_image)) range(image) else plot_settings$zlim_image,
        xlab = plot_settings$xlab,
        ylab = plot_settings$ylab,
        main = paste0(plot_settings$main, " #",i),
        col = plot_settings$col)
      graphics::box()

      ## axes
      if(plot_settings$axes) {
        xlab <- pretty(1:dim(x)[1])
        xlab[c(1,length(xlab))] <- c(0,dim(x)[1])
        xat <- seq(0,1,length.out = length(xlab))
        graphics::axis(side = 1, at = xat, labels = xlab)

        ylab <- pretty(1:dim(x)[2])
        ylab[c(1,length(ylab))] <- c(0,dim(x)[2])
        yat <- seq(0,1,length.out = length(ylab))
        graphics::axis(side = 2, at = yat, labels = ylab)
      }

      ## add legend
      if(plot_settings$legend) {
        par.default <- c(par.default, par(xpd = TRUE))
        on.exit(par(par.default), add = TRUE)
        col_grad <- plot_settings$col[seq(1, length(plot_settings$col), length.out = 14)]
        slices <- seq(0,1,length.out = 15)
        for(s in 1:(length(slices) - 1)){
          graphics::rect(
            xleft = par()$usr[4] * 1.01,
            xright = par()$usr[4] * 1.03,
            ybottom = slices[s],
            ytop =  slices[s + 1],
            col = col_grad[s],
            border = TRUE)
        }

        text(
          x = par()$usr[4] * 1.04,
          y = par()$usr[2],
          labels = if(is.null(plot_settings$zlim_image)) {
            format(max(x), digits = 1, scientific = TRUE)
            } else {
            format(plot_settings$zlim_image[2], digits = 1, scientific = TRUE)
            },
          cex = 0.7,
          srt = 270,
          pos = 3)
        text(
          x = par()$usr[4] * 1.04,
          y = par()$usr[3],
          labels = if(is.null(plot_settings$zlim_image)) {
            format(min(x), digits = 1, scientific = TRUE)
          } else {
            format(plot_settings$zlim_image[1], digits = 1, scientific = TRUE)
          },
          cex = 0.7,
          pos = 3,
          srt = 270)
      }
    }

  }else if(plot.type == "contour"){
     for(i in 1:dim(object)[3]) {
      x <- object[, , i, drop = FALSE]
      graphics::contour(
        x = x[,,1],
        axes = FALSE,
        zlim = if(is.null(plot_settings$zlim_image)) range(x) else plot_settings$zlim_image,
        xlab = plot_settings$xlab,
        ylab = plot_settings$ylab,
        main = paste0(plot_settings$main, " #",i),
        col = plot_settings$col)
      graphics::box()
     }

    ## axes
    if(plot_settings$axes) {
      xlab <- pretty(1:dim(x)[1])
      xlab[c(1,length(xlab))] <- c(0,dim(x)[1])
      xat <- seq(0,1,length.out = length(xlab))
      graphics::axis(side = 1, at = xat, labels = xlab)

      ylab <- pretty(1:dim(x)[2])
      ylab[c(1,length(ylab))] <- c(0,dim(x)[1])
      yat <- seq(0,1,length.out = length(ylab))
      graphics::axis(side = 2, at = yat, labels = ylab)
    }
  }
}

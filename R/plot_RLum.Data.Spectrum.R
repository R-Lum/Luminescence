#' @title Plot function for an RLum.Data.Spectrum S4 class object
#'
#' @description
#' The function provides a standardised plot output for spectrum data of an
#' [Luminescence::RLum.Data.Spectrum-class] class object. The purpose of this function is to
#' provide easy and straightforward spectra plotting, not a fully customised
#' access to all plot parameters. If this is wanted, standard R plot
#' functionality should be used instead.
#'
#' **Matrix structure** \cr (cf. [Luminescence::RLum.Data.Spectrum-class])
#'
#' - `rows` (x-values): wavelengths/channels (`xlim`, `xlab`)
#' - `columns` (y-values): time/temperature (`ylim`, `ylab`)
#' - `cells` (z-values): count values (`zlim`, `zlab`)
#'
#' *Note: This nomenclature is valid for all plot types of this function!*
#'
#' **Nomenclature for value limiting**
#'
#' - `xlim`: Limits values along the wavelength axis
#' - `ylim`: Limits values along the time/temperature axis
#' - `zlim`: Limits values along the count value axis
#'
#' **Details on the plot functions**
#'
#' Spectrum is visualised as 3D or 2D plot. Both plot types are based on
#' internal R plot functions.
#'
#'**`plot.type = "persp"`**
#'
#' Arguments that will be passed to [graphics::persp]:
#'
#' - `shade`: default is `0.4`
#' - `phi`: default is `15`
#' - `theta`: default is `-30`
#' - `lphi`: default is `15`
#' - `ltheta`: default is `-30`
#' - `expand`: default is `1`
#' - `axes`: default is `TRUE`
#' - `box`: default is `TRUE`; accepts `"alternate"` for a custom plot design
#' - `ticktype`: default is `detailed`, `r`: default is `10`
#'
#' **Note:** Further parameters can be adjusted via `par`. For example
#' to set the background transparent and reduce the thickness of the lines use:
#' `par(bg = NA, lwd = 0.7)` before the function call.
#'
#'**`plot.type = "single"`**
#'
#' Per frame a single curve is returned. Frames are time or temperature
#' steps.
#'
#' -`frames`: pick the frames to be plotted (depends on the binning!). Check without
#' this setting before plotting.
#'
#'**`plot.type = "multiple.lines"`**
#'
#' All frames plotted in one frame.
#'
#'-`frames`: pick the frames to be plotted (depends on the binning!). Check without
#' this setting before plotting.
#'
#'**`plot.type = "image"` or `plot.type = "contour"`**
#'
#' These plot types use the R functions [graphics::image] or [graphics::contour].
#' The advantage is that many plots can be arranged conveniently using standard
#' R plot functionality. If `plot.type = "image"` a contour is added by default,
#' which can be disabled using the argument `contour = FALSE` to add own contour
#' lines of choice.
#'
#'**`plot.type = "transect"`**
#'
#' Depending on the selected wavelength/channel range a transect over the
#' time/temperature (y-axis) will be plotted along the wavelength/channels
#' (x-axis). If the range contains more than one channel, values (z-values) are
#' summed up. To select a transect use the `xlim` argument, e.g.
#' `xlim = c(300,310)` plot along the summed up count values of channel
#' 300 to 310.
#'
#' **Further arguments that will be passed (depending on the plot type)**
#'
#' `xlab`, `ylab`, `zlab`, `xlim`, `ylim`, `zlim`, `main`, `mtext`,
#' `box`, `pch`, `type` (`"single"`, `"multiple.lines"`, `"interactive"`),
#' `col`, `border`, `lwd`, `bty`, `showscale` (`"interactive"`, `"image"`)
#' `contour`, `contour.col` (`"image"`), `labcex` (`"image"`, `"contour"`),
#' `n_breaks` (`"image"`), `legend` (`TRUE`/`FALSE`),
#' `legend.pos` (`"image"`), `legend.horiz` (`TRUE`/`FALSE` | `"image"`)
#'
#' @param object [Luminescence::RLum.Data.Spectrum-class] or [matrix] (**required**):
#' S4 object of class [Luminescence::RLum.Data.Spectrum-class] or a `matrix` containing count
#' values of the spectrum.\cr
#' Please note that in case of a matrix row names and col names are set
#' automatically if not provided.
#'
#' @param par.local [logical] (*with default*):
#' use local graphical parameters for plotting, e.g. the plot is shown in one
#' column and one row. If `par.local = FALSE` global parameters are inherited.
#'
#' @param plot.type [character] (*with default*):
#' for a 3D-plot use `"persp"` or `"interactive"`; for a 2D-plot you can use
#' `"image"`, `"contour"`, `"single"` or `"multiple.lines"` (along the time or
#' temperature axis) or `"transect"` (along the wavelength axis).
#'
#' @param optical.wavelength.colours [logical] (*with default*):
#' use optical wavelength colour palette. Note: For this, the spectrum range is
#' limited: `c(350,750)`. Own colours can be set with the argument `col`. If
#' you provide already binned spectra, the colour assignment is likely to be
#' wrong, since the colour gradients are calculated using the bin number.
#'
#' @param bg.spectrum [Luminescence::RLum.Data.Spectrum-class] or [matrix] (*optional*):
#' spectrum used for the background subtraction. The background spectrum should
#' be measured using the same setting as the signal spectrum. The argument
#' `bg.channels` controls how the subtraction is performed: if `bg.channels`
#' is not specified or the number of channels is identical between the signal
#' and background spectra, a channel-wise subtraction is performed; otherwise,
#' the *arithmetic mean* is calculated and subtracted from the signal.
#'
#' @param bg.channels [vector] (*optional*):
#' channels used for background subtraction. If the number of channels is
#' identical between the signal and background spectra, a channel-wise
#' subtraction is performed; otherwise this number is used to select channels
#' for calculating the *arithmetic mean*  If a spectrum is provided via
#' `bg.spectrum`, this argument only works on the background spectrum.
#'
#' **Note:** Background subtraction is applied prior to channel binning!
#'
#' @param bin.rows [integer] (*with default*):
#' allow summing-up wavelength channels (horizontal binning),
#' e.g. `bin.rows = 2` two channels are summed up.
#' Binning is applied after the background subtraction.
#'
#' @param bin.cols [integer] (*with default*):
#' allow summing-up channel counts (vertical binning) for plotting,
#' e.g. `bin.cols = 2` two channels are summed up.
#' Binning is applied after the background subtraction.
#'
#' @param norm [logical], [character] (*optional*):
#' if logical, whether curve normalisation should occur (`FALSE` by default);
#' alternatively, one of the values detailed in [Luminescence::plot_RLum.Data.Curve].
#' The normalisation is applied after binning.
#'
#' @param rug [logical] (*with default*):
#' enable/disable colour rug. Currently only implemented for plot
#' type `multiple.lines` and `single`.
#'
#' @param limit_counts [numeric] (*optional*):
#' value to limit all count values to this value, i.e. all count values above
#' this threshold will be replaced by this threshold. This is helpful
#' especially in case of TL-spectra.
#'
#' @param xaxis.energy [logical] (*with default*): enable/disable using energy
#' instead of wavelength on the x-axis. Function [Luminescence::convert_Wavelength2Energy]
#' is used to perform the conversion.
#'
#' **Note:** Besides being used in setting the axis, with this option the
#' the spectrum is recalculated in terms of intensity, see details.
#'
#' @param legend.text [character] (*with default*):
#' possibility to provide own legend text. This argument is only considered for
#' plot types providing a legend, e.g. `plot.type = "transect"`.
#'
#' @param plot [logical] (*with default*): enable/disable the plot output. If
#' the plot output is disabled, the [matrix] used for the plotting and the
#' calculated colour values (as attributes) are returned. This way, the
#' (binned, transformed etc.) output can be used in other functions and
#' packages, such as plotting with the package `'plot3D'`.
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to the `plot` function.
#'
#' @return
#' Returns a plot and the transformed `matrix` used for plotting with some
#' useful attributes such as `colour` and `pmat` (the transpose matrix from
#' [graphics::persp]).
#'
#' @note Not all additional arguments (`...`) will be passed similarly!
#'
#' @section Function version: 0.6.13
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)
#'
#' @seealso [Luminescence::RLum.Data.Spectrum-class],
#' [Luminescence::convert_Wavelength2Energy], [Luminescence::plot_RLum],
#' [graphics::persp], [plotly::plot_ly], [graphics::contour], [graphics::image]
#'
#' @keywords aplot
#'
#' @examples
#'
#' ##load example data
#' data(ExampleData.XSYG, envir = environment())
#'
#' ##(1)plot simple spectrum (2D) - image
#' plot_RLum.Data.Spectrum(
#'  TL.Spectrum,
#'  plot.type="image",
#'  xlim = c(310,750),
#'  ylim = c(0,300),
#'  bin.rows=10,
#'  bin.cols = 1)
#'
#' ##(2) plot spectrum (3D)
#' plot_RLum.Data.Spectrum(
#'   TL.Spectrum,
#'   plot.type="persp",
#'   xlim = c(310,750),
#'   ylim = c(0,100),
#'   bin.rows=10,
#'   bin.cols = 1)
#'
#'##(3) plot spectrum on energy axis
#'##please note the background subtraction
#'plot_RLum.Data.Spectrum(TL.Spectrum,
#' plot.type="persp",
#' ylim = c(0,200),
#' bin.rows=10,
#' bg.channels = 10,
#' bin.cols = 1,
#' xaxis.energy = TRUE)
#'
#' ##(4) plot multiple lines (2D) - multiple.lines (with ylim)
#' plot_RLum.Data.Spectrum(
#'  TL.Spectrum,
#'  plot.type="multiple.lines",
#'  xlim = c(310,750),
#'  ylim = c(0,100),
#'  bin.rows=10,
#'  bin.cols = 1)
#'
#' \dontrun{
#'  ##(4) interactive plot using the package plotly ("surface")
#'  plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="interactive",
#'  xlim = c(310,750), ylim = c(0,300), bin.rows=10,
#'  bin.cols = 1)
#'
#'  ##(5) interactive plot using the package plotly ("contour")
#'  plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="interactive",
#'  xlim = c(310,750), ylim = c(0,300), bin.rows=10,
#'  bin.cols = 1,
#'  type = "contour",
#'  showscale = TRUE)
#'
#'  ##(6) interactive plot using the package plotly ("heatmap")
#'  plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="interactive",
#'  xlim = c(310,750), ylim = c(0,300), bin.rows=10,
#'  bin.cols = 1,
#'  type = "heatmap",
#'  showscale = TRUE)
#' }
#'
#' @export
plot_RLum.Data.Spectrum <- function(
  object,
  par.local = TRUE,
  plot.type = "contour",
  optical.wavelength.colours = TRUE,
  bg.spectrum = NULL,
  bg.channels = NULL,
  bin.rows = 1,
  bin.cols = 1,
  norm = FALSE,
  rug = TRUE,
  limit_counts = NULL,
  xaxis.energy = FALSE,
  legend.text = NULL,
  plot = TRUE,
  ...
) {
  .set_function_name("plot_RLum.Data.Spectrum")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_class(object, c("RLum.Data.Spectrum", "matrix"))

  if (inherits(object, "matrix")) {
    if (is.null(colnames(object))) {
      colnames(object) <- seq_len(ncol(object))
    }
    if (is.null(rownames(object))) {
      rownames(object) <- seq_len(nrow(object))
    }

    object <- set_RLum(class = "RLum.Data.Spectrum", data = object)
    .throw_message("Input has been converted to an 'RLum.Data.Spectrum' ",
                   "object using set_RLum()", error = FALSE)
  }

  if (length(object@data) < 2) {
    .throw_error("'object' contains no data")
  }
  ## `norm` is not validated here but will be validated by normalise_RLum()
  .validate_class(bg.spectrum, c("RLum.Data.Spectrum", "matrix"), null.ok = TRUE)
  .validate_args(plot.type, c("contour", "persp", "single", "multiple.lines",
                              "image", "transect", "interactive"))
  .validate_positive_scalar(bin.rows, int = TRUE)
  .validate_positive_scalar(bin.cols, int = TRUE)

  ##XSYG
  ##check for curveDescripter
  if ("curveDescripter" %in% names(object@info)) {
    temp.lab <- strsplit(object@info$curveDescripter, split = ";")[[1]]
    xlab <- temp.lab[2]
    ylab <- temp.lab[1]
    zlab <- temp.lab[3]
  }else{
    xlab <- "Row values [a.u.]"
    ylab <- "Column values [a.u.]"
    zlab <- "Cell values [a.u.]"
  }

  # Do energy axis conversion -------------------------------------------------------------------
  if (xaxis.energy){
    ##conversion
    object <- convert_Wavelength2Energy(object, digits = 5)

    ##modify row order (otherwise subsequent functions, like persp, have a problem)
    object@data[] <- object@data[order(as.numeric(rownames(object@data))),]
    rownames(object@data) <- sort(as.numeric(rownames(object@data)))

    ## update axis label
    xlab <- "Energy [eV]"
  }

  ## check for duplicated column names (e.g., temperature not increasing)
  if (anyDuplicated(colnames(object@data)) > 0) {
    .throw_warning("Duplicated column names found, replaced by index")
    colnames(object@data) <- 1:ncol(object@data[])
  }

  ##deal with addition arguments
  extraArgs <- list(...)

  main <- extraArgs$main %||% "RLum.Data.Spectrum"
  zlab <- extraArgs$zlab %||% ifelse(plot.type == "multiple.lines", ylab, zlab)
  xlab <- extraArgs$xlab %||% xlab
  ylab <- extraArgs$ylab %||% ifelse(plot.type %in% c("single", "multiple.lines"),
                                     "Luminescence [cts/channel]", ylab)
  xlim <- extraArgs$xlim %||% range(as.numeric(rownames(object@data)))
  ylim <- extraArgs$ylim %||% range(as.numeric(colnames(object@data)))
  #for zlim see below

  mtext <- extraArgs$mtext %||% ""
  cex <- extraArgs$cex %||% 1
  phi <- extraArgs$phi %||% 15
  theta <- extraArgs$theta %||% -30
  lphi <- extraArgs$lphi %||% 15
  ltheta <- extraArgs$ltheta %||% -30
  r <- extraArgs$r %||% 10
  shade <- extraArgs$shade %||% 0.4
  expand <- extraArgs$expand %||% 0.6
  border <- extraArgs$border
  box <- extraArgs$box %||% TRUE
  axes <- extraArgs$axes %||% TRUE
  ticktype <- extraArgs$ticktype %||% "detailed"
  log <- extraArgs$log %||% ""
  labcex <- extraArgs$labcex %||% 0.6
  type <- extraArgs$type %||% ifelse(plot.type == "interactive", "surface", "l")
  pch <- extraArgs$pch %||% 1
  lwd <- extraArgs$lwd %||% 1
  bty <- extraArgs$bty
  sub <- extraArgs$sub %||% ""
  #for plotly::plot_ly
  showscale <- extraArgs$showscale %||% FALSE

  ## further plot settings
  plot_settings <- modifyList(
    x = list(
      legend = TRUE,
      legend.pos = "topright",
      legend.horiz = FALSE,
      n_breaks = 50
    ),
    val = extraArgs)

  # prepare values for plot ---------------------------------------------------
  ##copy data
  temp.xyz <- object@data

  ##check for NULL column names
  if(is.null(colnames(temp.xyz)))
    colnames(temp.xyz) <- 1:ncol(temp.xyz)

  if(is.null(rownames(temp.xyz)))
    rownames(temp.xyz) <- 1:nrow(temp.xyz)

  ##check for the case of a single column matrix
  if(ncol(temp.xyz)>1){
    x.vals <- as.numeric(rownames(temp.xyz))
    y.vals <- as.numeric(colnames(temp.xyz))

    ## limit the data according to xlim and ylim
    temp.xyz <- temp.xyz[x.vals >= xlim[1] & x.vals <= xlim[2],
                         y.vals >= ylim[1] & y.vals <= ylim[2],
                         drop = FALSE]
    if (nrow(temp.xyz) == 0 || ncol(temp.xyz) == 0) {
      .throw_error("No data left after applying 'xlim' and 'ylim'")
    }
  }

  ## wavelength
  x <- as.numeric(rownames(temp.xyz))

  ## time/temp
  y <- as.numeric(colnames(temp.xyz))

  # Background spectrum -------------------------------------------------------------------------
  if(!is.null(bg.spectrum)){
    bg.xyz <- if (inherits(bg.spectrum, "RLum.Data.Spectrum"))
                bg.spectrum@data
              else
                bg.spectrum

    ## set row and column names
    if (is.null(rownames(bg.xyz)))
      rownames(bg.xyz) <- 1:nrow(bg.xyz)
    if (is.null(colnames(bg.xyz)))
      colnames(bg.xyz) <- 1:ncol(bg.xyz)

    ## convert to energy scale if needed
    if (xaxis.energy) {
        #conversion
        bg.xyz <- convert_Wavelength2Energy(cbind(as.numeric(rownames(bg.xyz)), bg.xyz), digits = 5)
        rownames(bg.xyz) <- bg.xyz[,1]
        bg.xyz <- bg.xyz[,-1, drop = FALSE]

        ##modify row order (otherwise subsequent functions, like persp, have a problem)
        bg.xyz <- bg.xyz[order(as.numeric(rownames(bg.xyz))),,drop = FALSE]
    }

    ## reduce for xlim
    bg.xyz <- bg.xyz[as.numeric(rownames(bg.xyz)) >= xlim[1] &
                     as.numeric(rownames(bg.xyz)) <= xlim[2], , drop = FALSE]

    ## reduce for ylim by only if channels is NULL
    if (is.null(bg.channels)) {
      bg.xyz <- bg.xyz[, as.numeric(colnames(bg.xyz)) >= ylim[1] &
                         as.numeric(colnames(bg.xyz)) <= ylim[2], drop = FALSE]
      if (ncol(bg.xyz) == 0) {
        .throw_error("No background channels left after applying 'ylim'")
      }

      bg.channels <- 1:ncol(bg.xyz)
    }
  }

  # Background subtraction ---------------------------------------------------
  if(!is.null(bg.channels)){
    ##set background object if not available
    if(is.null(bg.spectrum)) bg.xyz <- temp.xyz

    if(max(bg.channels) > ncol(bg.xyz) || any(bg.channels <= 0)){
      ##correct the mess
      bg.channels <- sort(unique(bg.channels))
      bg.channels[bg.channels <= 0] <- 1
      bg.channels[bg.channels >= ncol(bg.xyz)] <- ncol(bg.xyz)

      .throw_warning("'bg.channels' out of range, corrected to: ",
                     .format_range(bg.channels))
    }

    ## the challenge we have here is that we want to maintain
    ## a channel-wise subtraction if the spectra are identical.
    if(length(bg.channels) > 1 &&
       (length(bg.channels) != ncol(bg.xyz) ||
       length(bg.channels) != ncol(temp.xyz)))
      temp.xyz <- temp.xyz - rowMeans(bg.xyz[,bg.channels])
    else
      temp.xyz <- temp.xyz - bg.xyz[,bg.channels]

    ##set values < 0 to 0
    temp.xyz[temp.xyz < 0] <- 0

    ##check worst case
    if(sum(temp.xyz) == 0){
      .throw_message("After background subtraction all counts are negative: ",
                     "nothing plotted, NULL returned")
      return(NULL)
    }
  }

  ## Channel binning --------------------------------------------------------
  if(bin.rows > 1){
    temp.xyz <- .matrix_binning(temp.xyz, bin_size = bin.rows, bin_col = FALSE, names = "mean")
    x <- as.numeric(rownames(temp.xyz))

    ##remove last channel (this is the channel that included less data)
    if (length(x) %% bin.rows != 0 && length(x) > bin.rows) {
      .throw_warning(length(x) %% bin.rows,
                     " channels removed due to row (wavelength) binning")
      temp.xyz <- temp.xyz[-length(x),]
      x <- x[-length(x)]
    }
  }

  if(bin.cols > 1){
    temp.xyz <- .matrix_binning(temp.xyz, bin_size = bin.cols, bin_col = TRUE, names = "groups")
    y <- as.numeric(colnames(temp.xyz))

    ##remove last channel (this is the channel that included less data)
    if (length(y) %% bin.cols != 0 && length(y) > bin.cols) {
      .throw_warning(length(y) %% bin.cols,
                     " channels removed due to column (frame) binning")
      temp.xyz <- temp.xyz[,-length(y)]
      y <- y[-length(y)]
    }
  }

  ##limit z-values if requested, this idea was taken from the Diss. by Thomas Schilles, 2002
  if(!is.null(limit_counts[1])) {
    if(min(temp.xyz) > limit_counts[1]) {
      limit_counts <- floor(limit_counts[1] + min(temp.xyz))
      .throw_warning("'limit_counts' is smaller than the lowest count value, ",
                     "'limit_counts' reset to ", limit_counts)
    }

    temp.xyz[temp.xyz[] > max(min(temp.xyz), limit_counts[1])] <- limit_counts[1]
  }

  ## data normalisation -----------------------------------------------------
  if (!isFALSE(norm)) {
    ## this is a bit cumbersome, but we cannot call `normalise_RLum()` directly
    ## on a matrix (and calling `.normalise_curve()` ourselves means that we
    ## would have to duplicate the input validation steps here), see #1303
    temp.spectrum <- set_RLum("RLum.Data.Spectrum", data = temp.xyz)
    temp.xyz <- normalise_RLum(temp.spectrum, norm)@data

    ## check if all values were replaced by 0 because normalisation produced Inf/Nan
    if (sum(range(temp.xyz)) == 0) {
      .throw_message("Insufficient data for plotting, NULL returned")
      return(NULL)
    }
  }

  ## don't apply a logarithmic transformation if non-positive values are present
  if (grepl("z", log) && any(temp.xyz <= 0)) {
    log <- gsub("z", "", log, fixed = TRUE)
    .throw_warning("Data contains non-positive values, 'log' reset to '", log, "'")
  }

  ##check for zlim
  zlim <- extraArgs$zlim %||% range(temp.xyz)

  # set colour values --------------------------------------------------------
  if (is.null(extraArgs$col) || plot.type %in% c("single", "multiple.lines")) {
    if (optical.wavelength.colours ||
        (rug && (!plot.type %in% c("persp", "interactive")))) {

      col.labels <- c(violet = "#EE82EE",
                      blue   = "#0000FF",
                      green  = "#00FF00",
                      yellow = "#FFFF00",
                      orange = "#FFA500",
                      red    = "#FF0000",
                      infra  = "#BEBEBE")

      ##make different colour palette for energy values
      if (xaxis.energy) {
        ## as thresholds are descending, we must reverse the labels, as cut()
        ## will sort the cutpoints in increasing order
        col.breaks <- c(min(max(xlim), 4.13), 2.76, 2.52, 2.18, 2.10, 2.00, 1.57, 0)
        col.labels <- rev(col.labels)
      }else{
        ## colour thresholds for wavelength axis
        col.breaks <- c(min(xlim, 300), 450, 495, 570, 590, 620, 790, Inf)
      }

      ## assign colours according to the defined thresholds
      col <- cut(x, col.breaks, labels = col.labels, right = FALSE)

      ## when using labels, cut generates a factor variable
      col <- as.character(col)

      ##if only one colour value, then skip gradient calculation as it causes
      ##an error
      col.unique <- unique(col)
      if(length(col.unique) > 1){
        ##set colour function for replacement
        colfunc <- grDevices::colorRampPalette(col.unique)

        ## index of the first occurrence of each colour besides the first,
        ## as we are trying to find where transition between colours occur
        col.first.idx <- match(col.unique, col)[-1]

        ## size of the colour transition band
        band.size <- 50 / bin.rows

        ##set borders for colour gradient recalculation
        grad.min <- pmax(floor(col.first.idx - band.size), 1)
        grad.max <- ceiling(col.first.idx + band.size)

        ##build up new index sequence (might be better)
        gradient.idx <- unlist(lapply(seq_along(grad.min),
                                      function(j) grad.min[j]:grad.max[j]))

        ##generate colour ramp and replace values
        col[gradient.idx] <- colfunc(length(gradient.idx))

        ##correct for overcharged colour values (causes zebra colour pattern)
        size.diff <- length(col) - nrow(temp.xyz)
        if (size.diff != 0) {
          col <- col[1:(length(col) + size.diff)]
        }
      }

    }else{
      col <- "black"
    }

  }else{
    col <- extraArgs$col
  }

  # Do log scaling if needed -------------------------------------------------
  ##x
  if(grepl("x", log)[1]) x <- log10(x)

  ##y
  if(grepl("y", log)[1]) y <- log10(y)

  ##z
  if(grepl("z", log)[1]) temp.xyz <- log10(temp.xyz)

# PLOT --------------------------------------------------------------------

## set variables we need later
pmat <- NA

if(plot){

  ##par setting for possible combination with plot method for RLum.Analysis objects
  if(par.local) {
    par.default <- par()[c("mfrow", "mar", "mgp", "xpd")]
    on.exit(par(par.default), add = TRUE)
    add.bottom <- if (plot.type %in% c("single", "transect")) 1 else 0
    par(mfrow = c(1, 1), mar = c(3.1 + add.bottom, 3.1, 2, 1),
        mgp = c(2, 0.5, 0), cex = cex)
  }

  ##rest plot type for 1 column matrix
  if(ncol(temp.xyz) == 1 && plot.type != "single"){
    plot.type <- "single"
    .throw_warning("Single column matrix, 'plot.type' reset to 'single'")
  }

  if (nrow(temp.xyz) == 1 && plot.type != "single") {
    .throw_message("Insufficient data for plotting, NULL returned")
    return(NULL)
  }

  if (plot.type == "persp") {

    ## Plot: perspective plot ----
    ## ==========================================================================#
    pmat <- graphics::persp(
      x, y, temp.xyz,
      shade = shade,
      axes = if(box[1] == "alternate") FALSE else axes,
      phi = phi,
      theta = theta,
      ltheta = ltheta,
      lphi = lphi,
      xlab = xlab,
      ylab = ylab,
      zlab = zlab,
      zlim = zlim,
      scale = TRUE,
      col = col[1:(length(x)-1)], ##needed due to recycling of the colours
      main = main,
      expand = expand,
      border = border,
      box = if(box[1] == "alternate") FALSE else box,
      r = r,
      ticktype = ticktype)

    ## this is custom plot output that might come in handy from time to time
    if(axes & box[1] == "alternate") {
      ## add axes manually
      x_axis <- seq(min(x), max(x), length.out = 20)
      y_axis <- seq(min(y), max(y), length.out = 20)
      z_axis <- seq(min(temp.xyz), max(temp.xyz), length.out = 20)

      lines(grDevices::trans3d(x_axis,min(y) - 5, min(temp.xyz),pmat))
      lines(grDevices::trans3d(min(x) - 5,y_axis, min(temp.xyz),pmat))
      lines(grDevices::trans3d(min(x) - 5,max(y), z_axis,pmat))

      ## x-axis
      px_axis <- pretty(x_axis)
      px_axis <- px_axis[(px_axis) > min(x_axis) & px_axis < max(x_axis)]

      tick_start <- grDevices::trans3d(px_axis, min(y_axis), min(z_axis), pmat)
      tick_end <- grDevices::trans3d(
        px_axis, min(y_axis) - max(y_axis) * 0.05, min(z_axis), pmat)

      ## calculate slope angle for xlab and ticks
      m <- (tick_start$y[2] - tick_start$y[1]) / (tick_start$x[2]  - tick_start$x[1])
      m <- atan(m) * 360 / (2 * pi)

      segments(tick_start$x, tick_start$y, tick_end$x, tick_end$y)
      text(
        tick_end$x,
        tick_end$y,
        adj = c(0.5,1.2),
        px_axis,
        xpd = TRUE,
        cex = 0.85,
        srt = m)

      ## x-axis label
      text(
        mean(tick_end$x),
        min(tick_end$y),
        adj = c(0.5, 1),
        xlab,
        srt = m,
        xpd = TRUE)

      ## y-axis
      py_axis <- pretty(y_axis)
      py_axis <- py_axis[(py_axis) > min(y_axis) & py_axis < max(y_axis)]

      tick_start <- grDevices::trans3d(min(x_axis), py_axis, min(z_axis), pmat)
      tick_end <- grDevices::trans3d(
        min(x_axis) - max(x_axis) * 0.025, py_axis, min(z_axis), pmat)
      segments(tick_start$x, tick_start$y, tick_end$x, tick_end$y)

      ## calculate slope angle for xlab and ticks
      m <- (tick_start$y[2] - tick_start$y[1]) / (tick_start$x[2]  - tick_start$x[1])
      m <- atan(m) * 360 / (2 * pi)

      text(
        tick_end$x,
        tick_end$y,
        py_axis,
        adj = c(0.6,1.2),
        srt = m,
        cex = 0.85,
        xpd = TRUE)

      ## y-axis label
      text(
        min(tick_end$x),
        mean(tick_end$y),
        adj = c(0.5, 1),
        ylab,
        srt = m,
        xpd = TRUE)

      ## z-axis
      pz_axis <- pretty(z_axis)
      pz_axis <- pz_axis[(pz_axis) > min(z_axis) & pz_axis < max(z_axis)]

      tick_start <- grDevices::trans3d(min(x_axis), max(y_axis), pz_axis, pmat)
      tick_end <- grDevices::trans3d(
        min(x_axis) - max(x_axis) * 0.015, max(y_axis), pz_axis, pmat)
      segments(tick_start$x, tick_start$y, tick_end$x, tick_end$y)

      ## calculate slope angle for xlab and ticks
      m <- (tick_start$y[2] - tick_start$y[1]) / (tick_start$x[2]  - tick_start$x[1])
      m <- atan(m) * 360 / (2 * pi)

      text(
        tick_end$x,
        tick_end$y,
        format(pz_axis, scientific = TRUE, digits = 1),
        adj = c(0.5,1.2),
        srt = m,
        xpd = TRUE,
        cex = 0.85)

      ## z-axis label
      text(
        min(tick_end$x),
        mean(tick_end$y),
        adj = c(0.5, 2.5),
        zlab,
        srt = m,
        xpd = TRUE)
    }

  } else if (plot.type == "interactive") {
    ## ==========================================================================#
    ##interactive plot and former persp3d
    ## ==========================================================================#

    ## Plot: interactive ----
    .require_suggested_package("plotly", "Displaying interactive plots")

       ##set up plot
        p <- plotly::plot_ly(
          z = temp.xyz,
          x = as.numeric(colnames(temp.xyz)),
          y = as.numeric(rownames(temp.xyz)),
          type = type,
          showscale = showscale
          #colors = col[1:(length(col)-1)],
        )

       ##change graphical parameters
       p <-  plotly::layout(
         p = p,
         scene = list(
           xaxis = list(title = ylab),
           yaxis = list(title = xlab),
           zaxis = list(title = zlab)
         ),
         title = main
       )

       print(p)
       on.exit(return(p), add = TRUE)

  } else if (plot.type == "contour") {
    ## Plot: contour plot ----
    ## ==========================================================================#
    graphics::contour(x,y,temp.xyz,
            xlab = xlab,
            ylab = ylab,
            main = main,
            labcex = labcex * cex,
            col = "black"
    )

  } else if (plot.type == "image") {
    ## Plot: image plot ----
    ## ==========================================================================#

    ## set breaks
    n_breaks <- plot_settings$n_breaks

    ## get colours
    col <- extraArgs$col %||% grDevices::hcl.colors(n_breaks, palette = "Inferno")

    ## set break vector
    breaks <- seq(min(temp.xyz), max(temp.xyz), length.out = length(col) + 1)

    ## render graphic
    graphics::image(
      x,y,temp.xyz,
      xlab = xlab,
      ylab = ylab,
      main = main,
      breaks = breaks,
      col = col
    )

    if (is.null(extraArgs$contour) || !isFALSE(extraArgs$contour)) {
      graphics::contour(x, y, temp.xyz,
              col = extraArgs$contour.col %||% rgb(1, 1, 1, 0.8),
              labcex = labcex * cex,
              add = TRUE)
    }

    if(plot_settings$legend[1]) {
      ## add legend
      legend_scale_id <- seq(1,length(breaks),length.out = c(min(c(length(breaks), 6))))

      legend(
          plot_settings$legend.pos,
          legend = ceiling(breaks[legend_scale_id]/10) * 10,
          fill = col[legend_scale_id],
          bg = grDevices::rgb(1,1,1,0.7),
          title = "Intensity",
          cex = 0.9,
          horiz = plot_settings$legend.horiz)
    }

  } else if(plot.type == "single") {
    ## Plot: single plot ----
    ## ==========================================================================#

    ## set colour rug
    col.rug <- col
    col <- extraArgs$col %||% "black"
    box <- extraArgs$box[1] %||% TRUE
    frames <- extraArgs$frames %||% 1:length(y)

    for(i in frames) {
      if (!"zlim" %in% names(extraArgs))
        zlim <- range(temp.xyz[, i])
      plot(x, temp.xyz[,i],
           xlab = xlab,
           ylab = ylab,
           main = main,
           xlim = xlim,
           ylim = zlim,
           frame = box,
           xaxt = "n",
           col = col,
           sub = paste0(
             "(frame ",i, " | ",
             paste(ifelse(i == 1, "0.0", round(y[i - 1], digits = 1)),
                    ":", round(y[i], digits = 1)), ")"),
           type = type,
           pch = pch)

      ## add colour rug
      if(rug){
        par.usr <- par("usr")
          ##rug as continuous rectangle
          i <- floor(seq(1,length(x), length.out = 300))
          graphics::rect(
            xleft = x[i[-length(i)]],
            xright = x[i[-1]],
            ytop = par.usr[3] + diff(c(par.usr[3], min(zlim))) * 0.9,
            ybottom = par.usr[3],
            col = col.rug[i],
            border = NA,
            lwd = 1)

          ## add rectangle from zero to first value
          graphics::rect(
            xleft = par.usr[1],
            xright = x[i[1]],
            ytop = par.usr[3] + diff(c(par.usr[3], min(zlim))) * 0.9,
            ybottom = par.usr[3],
            col = col.rug[1],
            density = 50,
            border = NA,
            lwd = 1)

          ## add rectangle from the last value to end of plot
          graphics::rect(
            xleft = x[i[length(i)]],
            xright = par.usr[2],
            ytop = par.usr[3] + diff(c(par.usr[3], min(zlim))) * 0.9,
            ybottom = par.usr[3],
            col = col.rug[length(col.rug)],
            density = 50,
            border = NA,
            lwd = 1)
      }

      ## add y axis to prevent overplotting
      graphics::axis(side = 1)

      ## add box if needed
      if(box) graphics::box()
    }

  } else if (plot.type == "multiple.lines") {
    ## Plot: multiple.lines ----
    ## ========================================================================#
    col.rug <- col
    col <- extraArgs$col %||% "black"
    box <- extraArgs$box[1] %||% TRUE
    frames <- extraArgs$frames %||% 1:length(y)

    max.frames <- ncol(temp.xyz)
    if (any(frames > max.frames)) {
      .throw_message("Skipped frames exceeding the maximum (", max.frames, ")",
                     error = FALSE)
      frames <- pmin(frames, max.frames)
    }

    ##change graphic settings
    par(mar = c(3.1, 3.1, 2, 7), xpd = TRUE)

    ##grep zlim
    if (!"zlim" %in% names(extraArgs))
      zlim <- range(temp.xyz)

    ##open plot area
    plot(NA, NA,
         xlab = xlab,
         ylab = ylab,
         main = main,
         xlim = xlim,
         ylim = zlim,
         frame = box,
         xaxt = "n",
         sub = sub,
         bty = bty)

    ## add colour rug
    if(rug){
      par.usr <- par("usr")
      ##rug as continuous rectangle
      i <- floor(seq(1,length(x), length.out = 300))
      graphics::rect(
        xleft = x[i[-length(i)]],
        xright = x[i[-1]],
        ytop = par.usr[3] + diff(c(par.usr[3], min(zlim))) * 0.9,
        ybottom = par.usr[3],
        col = col.rug[i],
        border = NA,
        lwd = NA)

      ## add rectangle from zero to first value
      graphics::rect(
        xleft = par.usr[1],
        xright = x[i[1]],
        ytop = par.usr[3] + diff(c(par.usr[3], min(zlim))) * 0.9,
        ybottom = par.usr[3],
        col = col.rug[1],
        density = 50,
        border = NA,
        lwd = 1)

      ## add rectangle from the last value to end of plot
      graphics::rect(
        xleft = x[i[length(i)]],
        xright = par.usr[2],
        ytop = par.usr[3] + diff(c(par.usr[3], min(zlim))) * 0.9,
        ybottom = par.usr[3],
        col = col.rug[length(col.rug)],
        density = 50,
        border = NA,
        lwd = 1)
    }

    ##add lines
    for(i in frames){
      lines(x,
            temp.xyz[,i],
            lty = i,
            lwd = lwd,
            type = type,
            col = col)
    }

    ## add y axis to prevent overplotting
    graphics::axis(side = 1)

    ## add box if needed
    if(box) graphics::box()

    ##for missing values - legend.text
    if (is.null(legend.text))
      legend.text <- as.character(paste(round(y[frames],digits=1), zlab))

    ##legend
    if(plot_settings$legend) {
      legend(x = par()$usr[2],
             y = par()$usr[4],
             legend = legend.text,
             lwd= lwd,
             lty = frames,
             bty = "n",
             cex = 0.6 * cex)
    }

  } else if (plot.type == "transect") {
    ## Plot: transect plot ----
    ## ========================================================================#

    ##sum up rows (column sum)
    temp.xyz <- colSums(temp.xyz)

    ##consider differences within the arguments
    zlim <- extraArgs$zlim %||% c(0, max(temp.xyz))
    zlab <- extraArgs$ylab %||% "Counts [1/summed channels]"

    plot(y, temp.xyz,
         xlab = ylab,
         ylab = zlab,
         main = main,
         xlim = ylim,
         ylim = zlim,
         col = col,
         sub = paste0("(channel range: ", .format_range(xlim, sep = " : "), ")"),
         type = type,
         pch = pch)
  }

  ## plot additional mtext
  mtext(mtext, side = 3, cex = cex * 0.8)
}

# Return ------------------------------------------------------------------
## add some attributes
attr(temp.xyz, "colour") <- col
attr(temp.xyz, "pmat") <- pmat

  ## return visible or not
  if (plot)
    return(invisible(temp.xyz))
  return(temp.xyz)
}

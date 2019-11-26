#' Plot function for an RLum.Data.Spectrum S4 class object
#'
#' The function provides a standardised plot output for spectrum data of an
#' RLum.Data.Spectrum S4 class object
#'
#' **Matrix structure** \cr (cf. [RLum.Data.Spectrum-class])
#'
#' - `rows` (x-values): wavelengths/channels (xlim, xlab)
#' - `columns` (y-values): time/temperature (ylim, ylab)
#' - `cells` (z-values): count values (zlim, zlab)
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
#' Arguments that will be passed to [persp]:
#'
#' - `shade`: default is `0.4`
#' - `phi`: default is `15`
#' - `theta`: default is `-30`
#' - `expand`: default is `1`
#' - `ticktype`: default is `detailed`, `r`: default is `10`
#'
#' **Note:** Further parameters can be adjusted via `par`. For example
#' to set the background transparent and reduce the thickness of the lines use:
#' `par(bg = NA, lwd = 0.7)` previous the function call.
#'
#'**`plot.type = "single"`**
#'
#' Per frame a single curve is returned. Frames are time or temperature
#' steps.
#'
#'**`plot.type = "multiple.lines"`**
#'
#' All frames plotted in one frame.
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
#' `xlab`, `ylab`, `zlab`, `xlim`, `ylim`,
#' `zlim`, `main`, `mtext`, `pch`, `type` ("single", "multiple.lines", "interactive"),
#' `col`, `border`, `box` `lwd`, `bty`, `showscale` ("interactive")
#'
#' @param object [RLum.Data.Spectrum-class] or [matrix] (**required**):
#' S4 object of class `RLum.Data.Spectrum` or a `matrix` containing count
#' values of the spectrum.\cr
#' Please note that in case of a matrix rownames and colnames are set
#' automatically if not provided.
#'
#' @param par.local [logical] (*with default*):
#' use local graphical parameters for plotting, e.g. the plot is shown in one column and one row.
#' If `par.local = FALSE` global parameters are inherited.
#' @param plot.type [character] (*with default*): plot type, for
#' 3D-plot use `persp`, or `interactive`, for a 2D-plot `contour`,
#' `single` or `multiple.lines` (along the time or temperature axis)
#' or `transect` (along the wavelength axis) \cr
#'
#' @param optical.wavelength.colours [logical] (*with default*):
#' use optical wavelength colour palette. Note: For this, the spectrum range is
#' limited: `c(350,750)`. Own colours can be set with the argument `col`. If you provide already
#' binned spectra, the colour assignment is likely to be wrong, since the colour gradients are calculated
#' using the bin number.
#'
#' @param bg.spectrum [RLum.Data.Spectrum-class] or [matrix] (*optional*): Spectrum
#' used for the background subtraction. By definition, the background spectrum should have been
#' measured with the same setting as the signal spectrum. If a spectrum is provided, the
#' argument `bg.channels` works only on the provided background spectrum.
#'
#' @param bg.channels [vector] (*optional*):
#' defines channel for background subtraction If a vector is provided the mean
#' of the channels is used for subtraction. If a spectrum is provided via `bg.spectrum`, this
#' argument only works on the `bg.spectrum`.
#'
#' **Note:** Background subtraction is applied prior to channel binning
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
#' @param norm [character] (*optional*): Normalise data to the maximum (`norm = "max"`) or
#' minimum (`norm = "min"`) count values. The normalisation is applied after the binning.
#'
#' @param rug [logical] (*with default*):
#' enables or disables colour rug. Currently only implemented for plot
#' type `multiple.lines` and `single`
#'
#' @param limit_counts [numeric] (*optional*):
#' value to limit all count values to this value, i.e. all count values above
#' this threshold will be replaced by this threshold. This is helpful
#' especially in case of TL-spectra.
#'
#' @param xaxis.energy [logical] (*with default*): enables or disables energy instead of
#' wavelength axis. For the conversion the function [convert_Wavelength2Energy] is used.
#'
#' **Note:** This option means not only simnply redrawing the axis,
#' instead the spectrum in terms of intensity is recalculated, s. details.
#'
#' @param legend.text [character] (*with default*):
#' possiblity to provide own legend text. This argument is only considered for
#' plot types providing a legend, e.g. `plot.type="transect"`
#'
#' @param ... further arguments and graphical parameters that will be passed
#' to the `plot` function.
#'
#' @return Returns a plot.
#'
#' @note Not all additional arguments (`...`) will be passed similarly!
#'
#' @section Function version: 0.6.2
#'
#' @author
#' Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @seealso [RLum.Data.Spectrum-class], [convert_Wavelength2Energy], [plot], [plot_RLum], [persp],
#' [plotly::plot_ly], [contour]
#'
#' @keywords aplot
#'
#' @examples
#'
#' ##load example data
#' data(ExampleData.XSYG, envir = environment())
#'
#' ##(1)plot simple spectrum (2D) - contour
#' plot_RLum.Data.Spectrum(TL.Spectrum,
#'                         plot.type="contour",
#'                         xlim = c(310,750),
#'                         ylim = c(0,300),
#'                         bin.rows=10,
#'                         bin.cols = 1)
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
#'
#'  ##(7) alternative using the package fields
#'  fields::image.plot(get_RLum(TL.Spectrum))
#'  contour(get_RLum(TL.Spectrum), add = TRUE)
#'
#' }
#'
#' @md
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
  norm = NULL,
  rug = TRUE,
  limit_counts = NULL,
  xaxis.energy = FALSE,
  legend.text,
  ...
){


  # Integrity check -----------------------------------------------------------

  ##check if object is of class RLum.Data.Spectrum
  if(class(object)[1] != "RLum.Data.Spectrum"){

    if(class(object)[1] == "matrix"){

      if(is.null(colnames(object))){
        colnames(object) <- 1:ncol(object)

      }

      if(is.null(rownames(object))){
        rownames(object) <- 1:nrow(object)

      }


      object <- set_RLum(class = "RLum.Data.Spectrum",
                         data = object)

      message("[plot_RLum.Data.Spectrum()] Input has been converted to a RLum.Data.Spectrum object using set_RLum()")


    }else{
      stop("[plot_RLum.Data.Spectrum()] Input object neither of class 'RLum.Data.Spectrum' nor 'matrix'.",
           call. = FALSE)

    }

  }

  ##XSYG
  ##check for curveDescripter
  if("curveDescripter" %in% names(object@info) == TRUE){

    temp.lab <- strsplit(object@info$curveDescripter, split = ";")[[1]]
    xlab <- if(xaxis.energy == FALSE){
      temp.lab[2]}else{"Energy [eV]"}
    ylab <- temp.lab[1]
    zlab <- temp.lab[3]

  }else{

    xlab <- if(xaxis.energy == FALSE){
      "Row values [a.u.]"}else{"Energy [eV]"}
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

  }


  ##deal with addition arguments
  extraArgs <- list(...)

  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else
  {"RLum.Data.Spectrum"}

  zlab <- if("zlab" %in% names(extraArgs)) {extraArgs$zlab} else
  {ifelse(plot.type == "multiple.lines", ylab, zlab)}

  xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else
  {xlab}

  ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} else
  {ifelse(plot.type == "single" | plot.type == "multiple.lines",
          "Luminescence [cts/channel]", ylab)}

  xlim <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else
  {c(min(as.numeric(rownames(object@data))),
     max(as.numeric(rownames(object@data))))}

  ylim <- if("ylim" %in% names(extraArgs)) {extraArgs$ylim} else
  {c(min(as.numeric(colnames(object@data))),
     max(as.numeric(colnames(object@data))))}

  #for zlim see below

  mtext <- if("mtext" %in% names(extraArgs)) {extraArgs$mtext} else
  {""}

  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else
  {1}

  phi <- if("phi" %in% names(extraArgs)) {extraArgs$phi} else
  {15}

  theta <- if("theta" %in% names(extraArgs)) {extraArgs$theta} else
  {-30}

  r <- if("r" %in% names(extraArgs)) {extraArgs$r} else
  {10}

  shade <- if("shade" %in% names(extraArgs)) {extraArgs$shade} else
  {0.4}

  expand <- if("expand" %in% names(extraArgs)) {extraArgs$expand} else
  {0.6}

  border <- if("border" %in% names(extraArgs)) {extraArgs$border} else
  {NULL}

  box <- if("box" %in% names(extraArgs)) {extraArgs$box} else
  {TRUE}

  ticktype <- if("ticktype" %in% names(extraArgs)) {extraArgs$ticktype} else
  {"detailed"}

  log<- if("log" %in% names(extraArgs)) {extraArgs$log} else
  {""}

  type<- if("type" %in% names(extraArgs)) {extraArgs$type} else
  {
    if (plot.type == "interactive") {
      "surface"

    } else{
      "l"

    }
  }

  pch<- if("pch" %in% names(extraArgs)) {extraArgs$pch} else
  {1}

  lwd<- if("lwd" %in% names(extraArgs)) {extraArgs$lwd} else
  {1}

  bty <- if("bty" %in% names(extraArgs)) {extraArgs$bty} else
  {NULL}

  sub<- if("sub" %in% names(extraArgs)) {extraArgs$sub} else
  {""}

  #for plotly::plot_ly
  showscale<- if("showscale" %in% names(extraArgs)) {extraArgs$showscale} else
  {FALSE}



  # prepare values for plot ---------------------------------------------------
  ##copy data
  temp.xyz <- object@data

  ##check for NULL column names
  if(is.null(colnames(temp.xyz))){
    colnames(temp.xyz) <- 1:ncol(temp.xyz)

  }

  if(is.null(rownames(temp.xyz))){
    rownames(temp.xyz) <- 1:nrow(temp.xyz)

  }

  ##check for the case of a single column matrix
  if(ncol(temp.xyz)>1){

    ##reduce for xlim
    temp.xyz <- temp.xyz[as.numeric(rownames(temp.xyz)) >= xlim[1] &
                           as.numeric(rownames(temp.xyz)) <= xlim[2],]

    ##reduce for ylim
    temp.xyz <- temp.xyz[, as.numeric(colnames(temp.xyz)) >= ylim[1] &
                           as.numeric(colnames(temp.xyz)) <= ylim[2]]

  }

  ## wavelength
  x <- as.numeric(rownames(temp.xyz))

  ## time/temp
  y <- as.numeric(colnames(temp.xyz))

  # Background spectrum -------------------------------------------------------------------------
  if(!is.null(bg.spectrum)){
    if(class(bg.spectrum)[1] == "RLum.Data.Spectrum" || class(bg.spectrum)[1] == "matrix"){
      ##case RLum
      if(class(bg.spectrum)[1] == "RLum.Data.Spectrum") bg.xyz <- bg.spectrum@data

      ##case matrix
      if(class(bg.spectrum)[1] == "matrix") bg.xyz <- bg.spectrum

      ##take care of channel settings, otherwise set bg.channels
      if(is.null(bg.channels))
        bg.channels <- c(1:ncol(bg.xyz))

      ##set rownames
      if(is.null(rownames(bg.xyz)))
        rownames(bg.xyz) <- 1:nrow(bg.xyz)

      ##convert to energy scale if needed
      if(xaxis.energy){

        #conversion
        bg.xyz <- convert_Wavelength2Energy(cbind(as.numeric(rownames(bg.xyz)), bg.xyz), digits = 5)
        rownames(bg.xyz) <- bg.xyz[,1]
        bg.xyz <- bg.xyz[,-1, drop = FALSE]

        ##modify row order (otherwise subsequent functions, like persp, have a problem)
        bg.xyz <- bg.xyz[order(as.numeric(rownames(bg.xyz))),,drop = FALSE]
        rownames(bg.xyz) <- sort(as.numeric(rownames(bg.xyz)))

      }

      ##reduce for xlim
      bg.xyz <- bg.xyz[as.numeric(rownames(bg.xyz)) >= xlim[1] &
                             as.numeric(rownames(bg.xyz)) <= xlim[2],,drop = FALSE]

    }else{
      stop("[plot_RLum.Data.Spectrum()] Input for 'bg.spectrum' not supported, please check manual!", call. = FALSE)

    }

  }

  # Background subtraction ---------------------------------------------------
  if(!is.null(bg.channels)){
    ##set background object if not available
    if(is.null(bg.spectrum)) bg.xyz <- temp.xyz

    if(max(bg.channels) > ncol(bg.xyz) || bg.channels <= 0){
      ##correct the mess
      bg.channels <- sort(unique(bg.channels))
      bg.channels[bg.channels <= 0] <- 1
      bg.channels[bg.channels >= ncol(bg.xyz)] <- ncol(bg.xyz)

      warning(
        paste0(
          "[plot_RLum.Data.Spectrum()] 'bg.channels' out of range, corrected to: ",
          min(bg.channels),
          ":",
          max(bg.channels)
        ), call. = FALSE)
    }

    if(length(bg.channels) > 1){
      temp.bg.signal <- rowMeans(bg.xyz[,bg.channels])
      temp.xyz <- temp.xyz - temp.bg.signal

    }else{
      temp.xyz <- temp.xyz - bg.xyz[,bg.channels]

    }

    ##set values < 0 to 0
    temp.xyz[temp.xyz < 0] <- 0

    ##check worst case
    if(sum(temp.xyz) == 0){
      message("[plot_RLum.Data.Spectrum()] After background subtraction all counts < 0. Nothing plotted, NULL returned!")
      return(NULL)
    }

  }


  # Channel binning ---------------------------------------------------------
  ##rewrite arguments; makes things easier
  bin.cols <- bin.cols[1]
  bin.rows <- bin.rows[1]

  ##fatal check (not needed anymore, but never change running code)
  if(bin.cols < 1 | bin.rows < 1)
    stop("[plot_RLum.Data.Spectrum()] 'bin.cols' and 'bin.rows' have to be > 1!", call. = FALSE)

  if(bin.rows > 1){
    temp.xyz <- .matrix_binning(temp.xyz, bin_size = bin.rows, bin_col = FALSE, names = "mean")
    x <- as.numeric(rownames(temp.xyz))

    ##remove last channel (this is the channel that included less data)
    if(length(x)%%bin.rows != 0){
      ##return warning
      warning(
        paste0("[plot_RLum.Data.Spectrum()] ",length(x)%%bin.rows,
               " channel(s) removed due to row (wavelength) binning."),
        call. = FALSE)

      ##do it
      temp.xyz <- temp.xyz[-length(x),]
      x <- x[-length(x)]

    }

  }

  if(bin.cols > 1){
    temp.xyz <- .matrix_binning(temp.xyz, bin_size = bin.cols, bin_col = TRUE, names = "groups")
    y <- as.numeric(colnames(temp.xyz))

    ##remove last channel (this is the channel that included less data)
    if(length(y)%%bin.cols != 0){

      ##return warning
      warning(
        paste0("[plot_RLum.Data.Spectrum()] ",length(y)%%bin.cols,
               " channel(s) removed due to column (frame) binning."),
        call. = FALSE)

      ##do it
      temp.xyz <- temp.xyz[,-length(y)]
      y <- y[-length(y)]

    }

  }

  ##limit z-values if requested, this idea was taken from the Diss. by Thomas Schilles, 2002
  if(!is.null(limit_counts)){
    temp.xyz[temp.xyz[]>limit_counts] <- limit_counts

  }


  # Normalise if wanted -------------------------------------------------------------------------
  if(!is.null(norm)){
    if(norm == "min")
      temp.xyz <- temp.xyz/min(temp.xyz)

    if(norm == "max")
      temp.xyz <- temp.xyz/max(temp.xyz)

  }


  ##check for zlim
  zlim <- if("zlim" %in% names(extraArgs)) {extraArgs$zlim} else
  {range(temp.xyz)}


  # set color values --------------------------------------------------------
  if("col" %in% names(extraArgs) == FALSE | plot.type == "single" | plot.type == "multiple.lines"){

    if(optical.wavelength.colours == TRUE | (rug == TRUE & (plot.type != "persp" & plot.type != "interactive"))){

      ##make different colour palette for energy values
      if (xaxis.energy) {
        col.violet <- c(2.76, ifelse(max(xlim) <= 4.13, max(xlim), 4.13))
        col.blue <- c(2.52, 2.76)
        col.green <- c(2.18, 2.52)
        col.yellow <- c(2.10, 2.18)
        col.orange <- c(2.00, 2.10)
        col.red <- c(1.57, 2.00)
        col.infrared <-
          c(1.55, ifelse(min(xlim) >= 1.55, min(xlim), 1.57))


        #set colour palette
        col <- unlist(sapply(1:length(x), function(i){

          if(x[i] >= col.violet[1] & x[i] < col.violet[2]){"#EE82EE"}
          else if(x[i] >= col.blue[1] & x[i] < col.blue[2]){"#0000FF"}
          else if(x[i] >= col.green[1] & x[i] < col.green[2]){"#00FF00"}
          else if(x[i] >= col.yellow[1] & x[i] < col.yellow[2]){"#FFFF00"}
          else if(x[i] >= col.orange[1] & x[i] < col.orange[2]){"#FFA500"}
          else if(x[i] >= col.red[1] & x[i] < col.red[2]){"#FF0000"}
          else if(x[i] <= col.infrared[2]){"#BEBEBE"}

        }))


      }else{
        ##wavelength colours for wavelength axis
        col.violet <- c(ifelse(min(xlim) <= 300, min(xlim), 300),450)
        col.blue <- c(450,495)
        col.green <- c(495,570)
        col.yellow <- c(570,590)
        col.orange <- c(590,620)
        col.red <- c(620,790)
        col.infrared <-
          c(790, ifelse(max(xlim) >= 800, max(xlim), 800))

        #set colour palette
        col <- unlist(sapply(1:length(x), function(i){

          if(x[i] >= col.violet[1] & x[i] < col.violet[2]){"#EE82EE"}
          else if(x[i] >= col.blue[1] & x[i] < col.blue[2]){"#0000FF"}
          else if(x[i] >= col.green[1] & x[i] < col.green[2]){"#00FF00"}
          else if(x[i] >= col.yellow[1] & x[i] < col.yellow[2]){"#FFFF00"}
          else if(x[i] >= col.orange[1] & x[i] < col.orange[2]){"#FFA500"}
          else if(x[i] >= col.red[1] & x[i] < col.red[2]){"#FF0000"}
          else if(x[i] >= col.infrared[1]){"#BEBEBE"}

        }))


      }

      ##find unique colours
      col.unique <- unique(col)

      ##if only one colour value, then skip gradient calculation as it causes
      ##an error
      if(length(col.unique) > 1){

        ##set colour function for replacement
        colfunc <- colorRampPalette(col.unique)

        ##get index for colour values to be cut from the current palette
        col.unique.index <-
          vapply(col.unique, function(i) {
            max(which(col == i))

          }, numeric(1))

        ##remove last index (no colour gradient needed), for energy axis use the first value
        col.unique.index <- col.unique.index[-length(col.unique.index)]

        ##set borders for colour gradient recalculation
        col.unique.index.min <- col.unique.index - (50/bin.rows)
        col.unique.index.max <- col.unique.index + (50/bin.rows)

        ##set negative values to the lowest index
        col.unique.index.min[col.unique.index.min<=0] <- 1

        ##build up new index sequence (might be better)
        col.gradient.index <- as.vector(unlist((
          sapply(1:length(col.unique.index.min), function(j){

            seq(col.unique.index.min[j],col.unique.index.max[j], by = 1)

          }))))


        ##generate colour ramp and replace values
        col.new <- colfunc(length(col.gradient.index))
        col[col.gradient.index] <- col.new

        ##correct for overcharged colour values (causes zebra colour pattern)
        if (diff(c(length(col), nrow(temp.xyz))) < 0) {
          col <- col[1:c(length(col) - diff(c(length(col), nrow(temp.xyz))))]

        }else if(diff(c(length(col), nrow(temp.xyz))) > 0){
          col <- col[1:c(length(col) + diff(c(length(col), nrow(temp.xyz))))]


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
  if(grepl("x", log)==TRUE){x <- log10(x)}

  ##y
  if(grepl("y", log)==TRUE){y <- log10(y)}

  ##z
  if(grepl("z", log)==TRUE){temp.xyz <- log10(temp.xyz)}


  # PLOT --------------------------------------------------------------------

  ##par setting for possible combination with plot method for RLum.Analysis objects
  if(par.local == TRUE){par(mfrow=c(1,1), cex = cex)}

  ##rest plot type for 1 column matrix
  if(ncol(temp.xyz) == 1 && plot.type != "single"){
    plot.type <- "single"
    warning("[plot_RLum.Data.Spectrum()] Single column matrix: plot.type has been automatically reset to 'single'", call. = FALSE)
  }

  ##do not let old code break down ...
  if(plot.type == "persp3d"){
    plot.type <- "interactive"
    warning("[plot_RLum.Data.Spectrum()] 'plot.type' has been automatically reset to interactive!", call. = FALSE)

  }

  if(plot.type == "persp" && ncol(temp.xyz) > 1){
    ## ==========================================================================#
    ##perspective plot
    ## ==========================================================================#
    persp(x, y, temp.xyz,
          shade = shade,
          phi = phi,
          theta = theta,
          xlab = xlab,
          ylab = ylab,
          zlab = zlab,
          zlim = zlim,
          scale = TRUE,
          col = col[1:(length(x)-1)], ##needed due to recycling of the colours
          main = main,
          expand = expand,
          border = border,
          box = box,
          r = r,
          ticktype = ticktype)

    ##plot additional mtext
    mtext(mtext, side = 3, cex = cex*0.8)

  }else if(plot.type == "interactive" && ncol(temp.xyz) > 1) {
    ## ==========================================================================#
    ##interactive plot and former persp3d
    ## ==========================================================================#

    ## Plot: interactive ----
    ##http://r-pkgs.had.co.nz/description.html
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("[plot_RLum.Data.Spectrum()] Package 'plotly' needed for this plot type. Please install it.",
           call. = FALSE)
    }

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
           xaxis = list(
             title = ylab

           ),
           yaxis = list(
             title = xlab
           ),
           zaxis = list(title = zlab)

         ),
         title = main
       )

       print(p)
       on.exit(return(p))


  }else if(plot.type == "contour" && ncol(temp.xyz) > 1) {
    ## ==========================================================================#
    ##contour plot
    ## ==========================================================================#
    contour(x,y,temp.xyz,
            xlab = xlab,
            ylab = ylab,
            main = main,
            col = "black"
    )

    ##plot additional mtext
    mtext(mtext, side = 3, cex = cex*0.8)


  } else if(plot.type == "single") {
    ## ==========================================================================#
    ## single plot
    ## ==========================================================================#

    col.rug <- col

    col<- if("col" %in% names(extraArgs)) {extraArgs$col} else
    {"black"}



    for(i in 1:length(y)){

      if("zlim" %in% names(extraArgs) == FALSE){zlim <- range(temp.xyz[,i])}

      plot(x, temp.xyz[,i],
           xlab = xlab,
           ylab = ylab,
           main = main,
           xlim = xlim,
           ylim = zlim,
           col = col,
           sub = paste(
             "(frame ",i, " | ",
             ifelse(i==1,
                    paste("0.0 :", round(y[i], digits = 1)),
                    paste(round(y[i-1], digits = 1),":",
                          round(y[i], digits =1))),")",
             sep = ""),
           type = type,
           pch = pch)

      if(rug == TRUE){
        ##rug als continous polygons
        for(i in 1:length(x)){
          polygon(x = c(x[i],x[i+1],x[i+1],x[i]),
                  y = c(min(zlim),min(zlim), par("usr")[3], par("usr")[3]),
                  border = col.rug[i], col = col.rug[i])
        }
      }

    }

    ##plot additional mtext
    mtext(mtext, side = 3, cex = cex*0.8)


  }else if(plot.type == "multiple.lines" && ncol(temp.xyz) > 1) {
    ## ========================================================================#
    ## multiple.lines plot
    ## ========================================================================#

    col.rug <- col

    col<- if("col" %in% names(extraArgs)) {extraArgs$col} else
    {"black"}

    ##change graphic settings
    par.default <- par()[c("mfrow", "mar", "xpd")]
    par(mfrow = c(1,1), mar=c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)

    ##grep zlim
    if("zlim" %in% names(extraArgs) == FALSE){zlim <- range(temp.xyz)}

    ##open plot area
    plot(NA, NA,
         xlab = xlab,
         ylab = ylab,
         main = main,
         xlim = xlim,
         ylim = zlim,
         sub = sub,
         bty = bty)

    if(rug == TRUE){
      ##rug als continous polygons
      for(i in 1:length(x)){
        polygon(x = c(x[i],x[i+1],x[i+1],x[i]),
                y = c(min(zlim),min(zlim), par("usr")[3], par("usr")[3]),
                border = col.rug[i], col = col.rug[i])
      }
    }

    ##add lines
    for(i in 1:length(y)){

      lines(x,
            temp.xyz[,i],
            lty = i,
            lwd = lwd,
            type = type,
            col = col)
    }

    ##for missing values - legend.text
    if(missing(legend.text)){

      legend.text <- as.character(paste(round(y,digits=1), zlab))

    }

    ##legend
    legend(x = par()$usr[2],
           y = par()$usr[4],

           legend = legend.text,

           lwd= lwd,
           lty = 1:length(y),
           bty = "n",
           cex = 0.6*cex)

    ##plot additional mtext
    mtext(mtext, side = 3, cex = cex*0.8)

    ##reset graphic settings
    par(par.default)
    rm(par.default)

  }else if(plot.type == "transect" && ncol(temp.xyz) > 1) {
    ## ========================================================================#
    ## transect plot
    ## ========================================================================#

    ##sum up rows (column sum)
    temp.xyz <- colSums(temp.xyz)

    ##consider differences within the arguments
    #check for zlim
    zlim <- if("zlim" %in% names(extraArgs)) {extraArgs$zlim} else
    {c(0,max(temp.xyz))}

    #check for zlim
    zlab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} else
    {paste("Counts [1/summed channels]")}

    plot(y, temp.xyz,
         xlab = ylab,
         ylab = zlab,
         main = main,
         xlim = ylim,
         ylim = zlim,
         col = col,
         sub = paste("(channel range: ", min(xlim), " : ", max(xlim), ")", sep=""),
         type = type,
         pch = pch)

    ##plot additional mtext
    mtext(mtext, side = 3, cex = cex*0.8)


  }else{
    stop("[plot_RLum.Data.Spectrum()] Unknown plot type.", call. = FALSE)

  }

}

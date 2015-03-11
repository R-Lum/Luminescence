plot_RLum.Data.Spectrum<- structure(function(#Plot function for an RLum.Data.Spectrum S4 class object
  ### The function provides a standardised plot output for spectrum data of an
  ### RLum.Data.Spectrum S4 class object

  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr

  ##section<<
  ## version 0.3.5
  # ===========================================================================

  object,
  ### \code{\linkS4class{RLum.Data.Spectrum}} (\bold{required}):
  ### S4 object of class \code{RLum.Data.Spectrum}

  par.local = TRUE,
  ### \code{\link{logical}} (with default): use local graphical parameters for plotting, e.g.
  ### the plot is shown in one column and one row. If \code{par.local = FALSE}
  ### global parameters are inherited.

  plot.type = "contour",
  ### \code{\link{character}} (with default): plot type, for 3D-plot use
  ### \code{persp}, or \code{persp3d}, for a 2D-plot \code{contour},
  ### \code{single} or \code{multiple.lines}
  ### (along the time or temperature axis) or \code{transect} (along the wavelength
  ### axis) \cr
  ###
  ### Note: The use of \code{persp3d} will produce a dynamic 3D surface plot
  ### on the screen.

  optical.wavelength.colours = TRUE,
  ### \code{\link{logical}} (with default): use optical wavelength colour palette.
  ### Note: For this, the spectrum range is limited: \code{c(350,750)}.
  ### Own colours can be set with the argument \code{col}.

  bg.channels,
  ### \code{\link{vector}} (optional): defines channel for background subtraction
  ### If a vector is provided the mean of the channels is used for subtraction.
  ### Note: Background subtraction is applied prior to channel binning

  bin.rows = 1,
  ### \code{\link{integer}} (with defaul): allow summing-up wavelength channels
  ### (horizontal binning), e.g. \code{bin.rows = 2} two channels are summed up

  bin.cols = 1,
  ### \code{\link{integer}} (with default): allow summing-up channel counts
  ### (vertical binning) for plotting,
  ### e.g. \code{bin.cols = 2} two channels are summed up

  rug = TRUE,
  ### \code{\link{logical}} (with default): enables or disables colour rug.
  ### Currently only implemented for plot type \code{multiple.lines}.

  xaxis.energy = FALSE,
  ### \code{\link{logical}} (with default): enables or disables energy instead
  ### of wavelength axis. Axis labelling are changed accordingly, so far no
  ### manual axis labelling is choosen.

  legend.text,
  ### \code{\link{character}} (with default): possiblity to provide own legend text.
  ### This argument is only considered for plot types providing
  ### a legend, e.g. \code{plot.type="transect"}

  ...
  ### further arguments and graphical parameters that will be passed to the
  ### \code{plot} function.
){


  # Integrity check -----------------------------------------------------------

  ##check if object is of class RLum.Data.Spectrum
  if(class(object) != "RLum.Data.Spectrum"){

    stop("[plot_RLum.Data.Spectrum] Input object is not of type RLum.Data.Spectrum")

  }

  ##XSYG
  ##check for curveDescripter
  if("curveDescripter" %in% names(object@info) == TRUE){

    temp.lab <- strsplit(object@info$curveDescripter, split = ";")[[1]]
    xlab <- if(xaxis.energy == FALSE | plot.type == "persp" | plot.type == "persp3d"){
      temp.lab[2]}else{"Energy [eV]"}
    ylab <- temp.lab[1]
    zlab <- temp.lab[3]

  }else{

    xlab <- if(xaxis.energy == FALSE | plot.type == "persp" | plot.type == "persp3d"){
      "Row values [a.u.]"}else{"Energy [eV]"}
    ylab <- "Column values [a.u.]"
    zlab <- "Cell values [a.u.]"

  }

  ##warning for energy curve conversion
  if(xaxis.energy == TRUE & (plot.type == "persp" | plot.type == "persp3d")){

    warning("Energy axis conversion not supported for this plot.type, nothing was done.")

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
  {0}

  theta <- if("theta" %in% names(extraArgs)) {extraArgs$theta} else
  {-150}

  shade <- if("shade" %in% names(extraArgs)) {extraArgs$shade} else
  {0.4}

  expand <- if("expand" %in% names(extraArgs)) {extraArgs$expand} else
  {1}

  border <- if("border" %in% names(extraArgs)) {extraArgs$border} else
  {NULL}

  box <- if("box" %in% names(extraArgs)) {extraArgs$box} else
  {TRUE}

  ticktype <- if("ticktype" %in% names(extraArgs)) {extraArgs$ticktype} else
  {"detailed"}

  log<- if("log" %in% names(extraArgs)) {extraArgs$log} else
  {""}

  type<- if("type" %in% names(extraArgs)) {extraArgs$type} else
  {"l"}

  pch<- if("pch" %in% names(extraArgs)) {extraArgs$pch} else
  {1}

  lwd<- if("lwd" %in% names(extraArgs)) {extraArgs$lwd} else
  {1}

  bty <- if("bty" %in% names(extraArgs)) {extraArgs$bty} else
  {NULL}

  sub<- if("sub" %in% names(extraArgs)) {extraArgs$sub} else
  {""}




  # prepare values for plot ---------------------------------------------------
  temp.xyz <- as(object, "matrix")

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


  # Background subtraction ---------------------------------------------------

  if(missing(bg.channels) == FALSE){

    if(length(bg.channels) > 1){

      temp.bg.signal <- rowMeans(temp.xyz[,bg.channels])
      temp.xyz <- temp.xyz[,1:ncol(temp.xyz)] - temp.bg.signal

    }else{

      temp.xyz <- temp.xyz[,1:ncol(temp.xyz)] - temp.xyz[,bg.channels]
      temp.xyz <- ifelse(temp.xyz < 0, mean(temp.xyz[,bg.channels]), temp.xyz)

    }

    ##set values < 0 to 0
    temp.xyz <- ifelse(temp.xyz < 0, mean(temp.xyz[,bg.channels[1]]), temp.xyz)

  }


  # Channel binning ---------------------------------------------------------

  if(missing(bin.rows) == FALSE){

      ##calculate n.rows
      n.rows <- nrow(temp.xyz)

      ##modulo operation for the number of groups
      bin.group.rest <- n.rows%%bin.rows

      ##define groups for binning
      bin.group <- rep(1:(n.rows/bin.rows), 1, each = bin.rows)

      ##add last group
      bin.group <- c(bin.group, rep(n.rows/bin.rows + 1, 1, each = bin.group.rest))

      ##sum up rows
      temp.xyz <- rowsum(temp.xyz, bin.group)

      ##correct labeling
      x <- x[seq(1, n.rows, bin.rows)]

      ## to avoid odd plots remove last group if bin.rows is not a multiple
      ## of the row number
      if(bin.group.rest != 0){

        temp.xyz <- temp.xyz[-nrow(temp.xyz),]
        x <- x[-length(x)]

        warning("Last wavelength channel has been removed due to binning.")

      }


      rm(bin.group.rest)

  }


  if(missing(bin.cols) == FALSE){

    ##calculate n.cols
    n.cols <- ncol(temp.xyz)

      ##check for validity
      if(bin.cols > n.cols){

        bin.cols <- n.cols

        warning("bin.cols > the number of columns. Value reduced to number of cols.")

      }

    ##modulo operation for the number of groups
    bin.group.rest <- n.cols%%bin.cols

    ##define groups for binning
    bin.group <- rep(1:(n.cols/bin.cols), 1, each = bin.cols)

    ##add last group
    bin.group <- c(bin.group, rep(n.cols/bin.cols + 1, 1, each = bin.group.rest))

    ##sum up cols
    temp.xyz <- rowsum(t(temp.xyz), bin.group)
    temp.xyz <- t(temp.xyz)

    ##correct labeling
    y <- y[seq(1, n.cols, bin.cols)]

    ## to avoid odd plots remove last group if bin.cols is not a multiple
    ## of the col number
    if(bin.group.rest != 0){

      temp.xyz <- temp.xyz[,-ncol(temp.xyz)]
      y <- y[-length(y)]

      warning("Last count channel has been removed due to column binning.")

    }

  }

  ##check for zlim
  zlim <- if("zlim" %in% names(extraArgs)) {extraArgs$zlim} else
  {range(temp.xyz)}


  # set color values --------------------------------------------------------

  if("col" %in% names(extraArgs) == FALSE){

    if(optical.wavelength.colours == TRUE && (type == "b" | type == "p" |
                                              (plot.type != "single" &&
                                               plot.type != "transect"))){

      col.violet <- c(ifelse(min(xlim) <= 300, min(xlim), 300),450)
      col.blue <- c(450,495)
      col.green <- c(495,570)
      col.yellow <- c(570,590)
      col.orange <- c(590,620)
      col.red <- c(620,790)
      col.infrared <- c(790, ifelse(max(xlim) >= 800, max(xlim), 800))

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

        ##find unique colours
        col.unique <- unique(col)

        ##if only one colour value, then skip gradient calculation as it causes
        ## an error

        if(length(col.unique) > 1){

          ##set colour function for replacement
          colfunc <- colorRampPalette(col.unique)

          ##get index for cut colour values from current palette
          col.unique.index <- sapply(1:length(col.unique), function(i){

            col.unique.max <- max(which(col == col.unique[i]))

          })

          ##remove last index (no colour gradient needed)
          col.unique.index <- col.unique.index[-length(col.unique.index)]

          ##set borders for colour gradient recalculation
          col.unique.index.min <- col.unique.index - (50)/bin.rows
          col.unique.index.max <- col.unique.index + (50)/bin.rows

          ##build up new index sequence (might be better)
          col.gradient.index <- as.vector(
            sapply(1:length(col.unique.index.min), function(j){

            seq(col.unique.index.min[j],col.unique.index.max[j], by = 1)

         }))

        ##generate colour ramp and replace values
        col.new <- colfunc(length(col.gradient.index))
        col[col.gradient.index] <- col.new

        ##correct for overcharged colour values (causes zebra colour pattern)
        if(diff(c(length(col), nrow(temp.xyz))) < 0){

          col <- col[1:c(length(col)+diff(c(length(col), nrow(temp.xyz))))]

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
  if(ncol(temp.xyz) == 1){
      plot.type = "single"
     warning("Single column matrix: plot.type has been automatically reset to 'single'")
  }

  if(plot.type == "persp3d" && ncol(temp.xyz) > 1){

  ## ==========================================================================#
  ##perspective plot 3D screen (package rgl)
  ## ==========================================================================#
  persp3d(x, y, temp.xyz,
            xlab = xlab,
            ylab = ylab,
            zlab = zlab,
            zlim = zlim,
            col = col,
            main = main)

  }else if(plot.type == "persp" && ncol(temp.xyz) > 1){
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
        col = col[1:(length(col)-1)], ##needed due to recycling of the colours
        main = main,
        expand = expand,
        border = border,
        box = box,
        ticktype = ticktype)


    ##plot additional mtext
    mtext(mtext, side = 3, cex = cex*0.8)


   }else if(plot.type == "contour" && ncol(temp.xyz) > 1) {
   ## ==========================================================================#
   ##contour plot
   ## ==========================================================================#
   contour(x,y,temp.xyz,
           xlab = xlab,
           ylab = ylab,
           main = main,
           col = "black",
           xaxt = "n"
           )

   ##plot additional mtext
   mtext(mtext, side = 3, cex = cex*0.8)

   ##add normal or energy axsis
   if(xaxis.energy == FALSE){

     axis(side =1)

   }else{

     axis.ticks <- seq(min(xlim), max(xlim), length = 10)
     axis(side = 1, at = axis.ticks,
          labels = round(c((4.13566733e-015 * 299792458e+09)/axis.ticks), digits =1))
   }


   } else if(plot.type == "single") {
  ## ==========================================================================#
  ## single plot
  ## ==========================================================================#

    for(i in 1:length(y)){

      if("zlim" %in% names(extraArgs) == FALSE){zlim <- range(temp.xyz[,i])}

      plot(x, temp.xyz[,i],
           xlab = xlab,
           ylab = ylab,
           main = main,
           xlim = xlim,
           ylim = zlim,
           col = col,
           xaxt = "n",
           sub = paste(
             "(frame ",i, " | ",
             ifelse(i==1,
                    paste("0.0 :", round(y[i], digits = 1)),
                    paste(round(y[i-1], digits = 1),":",
                          round(y[i], digits =1))),")",
             sep = ""),
           type = type,
           pch = pch)

      ##add normal or energy axsis
      if(xaxis.energy == FALSE){

        axis(side =1)

      }else{

        axis.ticks <- seq(min(xlim), max(xlim), length = 10)
        axis(side = 1, at = axis.ticks,
             labels = round(c((4.13566733e-015 * 299792458e+09)/axis.ticks), digits =1))
      }



    }

    ##plot additional mtext
    mtext(mtext, side = 3, cex = cex*0.8)


    }else if(plot.type == "multiple.lines" && ncol(temp.xyz) > 1) {
    ## ========================================================================#
    ## multiple.lines plot
    ## ========================================================================#

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
           bty = bty,
           xaxt = "n")

      if(rug == TRUE){
      ##rug als continous polygons
      for(i in 1:length(x)){
        polygon(x = c(x[i],x[i+1],x[i+1],x[i]),
                y = c(min(zlim),min(zlim), par("usr")[3], par("usr")[3]),
                border = col[i], col = col[i])
        }
      }

      ##add normal or energy axsis
      if(xaxis.energy == FALSE){

        axis(side =1)

      }else{

      axis.ticks <- seq(min(xlim), max(xlim), length = 10)
      axis(side = 1, at = axis.ticks,
           labels = round(c((4.13566733e-015 * 299792458e+09)/axis.ticks), digits =1))
      }


       ##add lines
       for(i in 1:length(y)){

         lines(x,
               temp.xyz[,i],
               lty = i,
               lwd = lwd,
               type = type)
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

    stop("[plot_RLum.Data.Spectrum()] Unknown plot type.")

  }

  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------

  ##details<<
  ## \bold{Matrix structure} \cr
  ## (cf. \code{\linkS4class{RLum.Data.Spectrum}}) \cr
  ##
  ## \itemize{
  ## \item \code{rows} (x-values): wavelengths/channels (xlim, xlab)
  ## \item \code{columns} (y-values): time/temperature  (ylim, ylab)
  ## \item \code{cells} (z-values): count values (zlim, zlab)
  ##}
  ##
  ## \emph{Note: This nomenclature is valid for all plot types of this function!}
  ##
  ##
  ## \bold{Nomenclature for value limiting}\cr
  ##
  ## \code{xlim}: Limits values along the wavelength axis\cr
  ## \code{ylim}: Limits values along the time/temperature axis\cr
  ## \code{zlim}: Limits values along the count value axis
  ##
  ## \bold{Details on the plot functions} \cr
  ##
  ## Spectrum is visualised as 3D or 2D plot. Both plot types are based on internal
  ## R plot functions. \cr
  ##
  ## \bold{\code{plot.type = "persp"}}
  ##
  ## Arguments that will be passed to \code{\link{persp}}:
  ## \itemize{
  ## \item \code{shade}: default is \code{0.4}
  ## \item \code{phi}: default is \code{30}
  ## \item \code{theta}: default is \code{30}
  ## \item \code{expand}: default is \code{1}
  ## \item \code{ticktype}: default is \code{detailed}
  ## }
  ##
  ## \emph{Note: Further parameters can be adjusted via \code{par}. For example
  ## to set the background transparent and reduce the thickness of the lines use:
  ## \code{par(bg = NA, lwd = 0.7)} previous the function call.}
  ##
  ## \bold{\code{plot.type = "single"}}\cr
  ## Per frame a single curve is returned. Frames are time or temperature steps.\cr
  ##
  ## \bold{\code{plot.type = "multiple.lines"}}\cr
  ## All frames drawn in one frame.\cr
  ##
  ## \bold{\code{plot.type = "transect"}}\cr
  ## Depending on the selected wavelength/channel range a transect over the
  ## time/temperature (y-axis) will be plotted along the wavelength/channels (x-axis).
  ## If the range contains more than one channel, values (z-values) are summed up.
  ## To select a transect use the \code{xlim} argument, e.g. \code{xlim = c(300,310)}
  ## plot along the summed up count values of channel 300 to 310.
  ##
  ##
  ## \bold{Further arguments that will be passed (depending on the plot type)}\cr
  ## \code{xlab}, \code{ylab}, \code{zlab}, \code{xlim}, \code{ylim}, \code{zlim},
  ## \code{main}, \code{mtext}, \code{pch}, \code{type}, \code{border}, \code{box}
  ## \code{lwd}, \code{bty} \cr


  ##value<<
  ## Returns a plot.

  ##references<<
  ## #

  ##note<<
  ## Not all additional arguments (\code{...}) will be passed similarly!

  ##seealso<<
  ## \code{\linkS4class{RLum.Data.Spectrum}},
  ## \code{\link{plot}}, \code{\link{plot_RLum}}, \code{\link{persp}},
  ## \code{\link{persp3d}}, \code{\link{contour}}

  ##keyword<<
  ## aplot

}, ex=function(){

 ##load example data
 data(ExampleData.XSYG, envir = environment())

 ##(1)plot simple spectrum (2D) - contour
 plot_RLum.Data.Spectrum(TL.Spectrum,
                         plot.type="contour",
                         xlim = c(310,750),
                         ylim = c(0,300),
                         bin.rows=10,
                         bin.cols = 1)

 ##(2) plot simple spectrum (2D) - multiple.lines (with ylim)
 plot_RLum.Data.Spectrum(TL.Spectrum,
                         plot.type="multiple.lines",
                         xlim = c(310,750),
                         ylim = c(0,100),
                         bin.rows=10,
                         bin.cols = 1)

 ##(3) plot 3d spectrum (uncomment for usage)
 # plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="persp",
 # xlim = c(310,750), ylim = c(0,300), bin.rows=10,
 # bin.cols = 1)


})#END OF STRUCTURE
#' Plot function for an RLum.Results S4 class object
#'
#' The function provides a standardised plot output for data of an RLum.Results
#' S4 class object
#'
#' The function produces a multiple plot output.  A file output is recommended
#' (e.g., [pdf]).
#'
#' @param object [RLum.Results-class] (**required**):
#' S4 object of class `RLum.Results`
#'
#' @param single [logical] (*with default*):
#' single plot output (`TRUE/FALSE`) to allow for plotting the results in as
#' few plot windows as possible.
#'
#' @param ... further arguments and graphical parameters will be passed to
#' the `plot` function.
#'
#' @return Returns multiple plots.
#'
#' @note
#' Not all arguments available for [plot] will be passed!
#' Only plotting of `RLum.Results` objects are supported.
#'
#' @section Function version: 0.2.1
#'
#' @author
#' Christoph Burow, University of Cologne (Germany) \cr
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [plot], [plot_RLum]
#'
#' @keywords aplot
#'
#' @examples
#'
#'
#' ###load data
#' data(ExampleData.DeValues, envir = environment())
#'
#' # apply the un-logged minimum age model
#' mam <- calc_MinDose(data = ExampleData.DeValues$CA1, sigmab = 0.2, log = TRUE, plot = FALSE)
#'
#' ##plot
#' plot_RLum.Results(mam)
#'
#' # estimate the number of grains on an aliquot
#' grains<- calc_AliquotSize(grain.size = c(100,150), sample.diameter = 1, plot = FALSE, MC.iter = 100)
#'
#' ##plot
#' plot_RLum.Results(grains)
#'
#'
#' @md
#' @export
plot_RLum.Results<- function(
  object,
  single = TRUE,
  ...
) {
  .set_function_name("plot_RLum.Results")
  on.exit(.unset_function_name(), add = TRUE)

  ##============================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ##============================================================================##

  .validate_class(object, "RLum.Results")
  if (is.null(object@originator) || is.na(object@originator)) {
    .throw_error("Object originator not supported")
  }

  ##============================================================================##
  ## SAFE AND RESTORE PLOT PARAMETERS ON EXIT
  ##============================================================================##
  par.old <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(par.old)), add = TRUE)

  ##============================================================================##
  ## ... ARGUMENTS
  ##============================================================================##

  ##deal with addition arguments
  extraArgs <- list(...)

  ##main
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else
  {""}
  ##mtext
  mtext <- if("mtext" %in% names(extraArgs)) {extraArgs$mtext} else
  {""}
  ##log
  log <- if("log" %in% names(extraArgs)) {extraArgs$log} else
  {""}
  ##lwd
  lwd <- if("lwd" %in% names(extraArgs)) {extraArgs$lwd} else
  {1}
  ##lty
  lty <- if("lty" %in% names(extraArgs)) {extraArgs$lty} else
  {1}
  ##type
  type <- if("type" %in% names(extraArgs)) {extraArgs$type} else
  {"l"}
  ##pch
  pch <- if("pch" %in% names(extraArgs)) {extraArgs$pch} else
  {1}
  ##col
  col <- if("col" %in% names(extraArgs)) {extraArgs$col} else
  {"black"}

  ##============================================================================##
  ## PLOTTING
  ##============================================================================##

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  ## CASE 0: General plot dispatcher ----------
  switch(object@originator,
      "analyse_SAR.CWOSL" = plot_AbanicoPlot(object),
      "analyse_pIRIRSequence" = plot_AbanicoPlot(object),
      "analyse_IRSAR.RF" = plot_AbanicoPlot(object),
      "calc_FiniteMixture" = do.call(calc_FiniteMixture, c(object, extraArgs)),
    NULL
    )

  ## CASE 1: Minimum Age Model / Maximum Age Model -------
  if (object@originator %in% c("calc_MinDose", "calc_MaxDose")) {

    ## single MAM estimate
    # plot profile log likelihood

    profiles <- object@data$profile
    if (object@data$args$log) {
      profiles@profile$gamma$par.vals[ ,"gamma"] <- exp(profiles@profile$gamma$par.vals[ ,"gamma"])
      profiles@profile$sigma$par.vals[ ,"sigma"] <- exp(profiles@profile$sigma$par.vals[ ,"sigma"])

      if (object@data$args$par == 4)
        profiles@profile$mu$par.vals[ ,"mu"] <- exp(profiles@profile$mu$par.vals[ ,"mu"])
    }

    if (single)
      par(mfrow=c(2, 2))

    param <- c("gamma", "sigma", "p0", "mu")

    for (i in param) {
      if (object@data$summary$par == 3 && i == "mu")
        break

      tryCatch({

        xvals <- as.data.frame(profiles@profile[[i]]$par.vals)[[i]]
        xlim <- range(xvals[xvals > 0])
        suppressWarnings(
          bbmle::plot(profiles, which = i, xlab = "", xaxt = "n", xlim = xlim)
        )

        axis(1, mgp = c(3, 0.5, 0))
        title(xlab = i, line = 1.2)

        if (i %in% c("gamma", "sigma", "mu") && object@data$args$log && object@data$args$log.output) {
          axis(1, at = axTicks(1),
               labels = format(round(log(axTicks(1)), 2), nsmall = 2),
               line = 2.5, mgp = c(3, 0.5, 0))
          title(xlab = paste0("log(", i, ")"), line = 4)
        }

      }, error = function(e)  {
        message("Unable to plot the likelihood profile for: ", i,
                " (likelihood probably infinite)")
      })
    }
    par(mfrow=c(1,1))


    # })

    ## bootstrap MAM estimates
    if(object@data$args$bootstrap==TRUE) {

      # save previous plot parameter and set new ones
      .pardefault<- par(no.readonly = TRUE)

      # get De-llik pairs
      pairs<- object@data$bootstrap$pairs$gamma

      # get polynomial fit objects
      poly.lines<- list(poly.three=object@data$bootstrap$poly.fits$poly.three,
                        poly.four=object@data$bootstrap$poly.fits$poly.four,
                        poly.five=object@data$bootstrap$poly.fits$poly.five,
                        poly.six=object@data$bootstrap$poly.fits$poly.six)

      # define polynomial curve functions for plotting
      poly.curves<- list(poly.three.curve=function(x) { poly.lines$poly.three$coefficient[4]*x^3 + poly.lines$poly.three$coefficient[3]*x^2 + poly.lines$poly.three$coefficient[2]*x + poly.lines$poly.three$coefficient[1] },
                         poly.four.curve=function(x) { poly.lines$poly.four$coefficient[5]*x^4 + poly.lines$poly.four$coefficient[4]*x^3 + poly.lines$poly.four$coefficient[3]*x^2 + poly.lines$poly.four$coefficient[2]*x + poly.lines$poly.four$coefficient[1] },
                         poly.five.curve=function(x) { poly.lines$poly.five$coefficient[6]*x^5 + poly.lines$poly.five$coefficient[5]*x^4 + poly.lines$poly.five$coefficient[4]*x^3 + poly.lines$poly.five$coefficient[3]*x^2 + poly.lines$poly.five$coefficient[2]*x + poly.lines$poly.five$coefficient[1] },
                         poly.six.curve=function(x) { poly.lines$poly.six$coefficient[7]*x^6 + poly.lines$poly.six$coefficient[6]*x^5 + poly.lines$poly.six$coefficient[5]*x^4 + poly.lines$poly.six$coefficient[4]*x^3 + poly.lines$poly.six$coefficient[3]*x^2 + poly.lines$poly.six$coefficient[2]*x + poly.lines$poly.six$coefficient[1] })

      ## --------- PLOT "RECYCLE" BOOTSTRAP RESULTS ------------ ##

      if(single==TRUE) {
        graphics::layout(cbind(c(1, 1, 2, 5, 5, 6), c(3, 3, 4, 7, 7, 8)))
        par(cex = 0.6)
      } else {
        graphics::layout(matrix(c(1, 1, 2)), 2, 1)
        par(cex = 0.8)
      }

      for(i in 1:4) {
        ## ----- LIKELIHOODS

        # set margins (bottom, left, top, right)
        par(mar=c(0,5,5,3))

        # sort De and likelihoods by De (increasing)
        pairs<- pairs[order(pairs[,1]),]

        # remove invalid NA values
        pairs <- stats::na.omit(pairs)

        plot(x=pairs[,1],
             y=pairs[,2],
             xlab="Equivalent Dose [Gy]",
             ylab="Likelihood",
             xlim=range(pretty(pairs[,1])),
             ylim=range(pretty(c(0, as.double(quantile(pairs[,2],probs=0.98))))),
             xaxt = "n",
             xaxs = "i",
             yaxs = "i",
             bty = "l",
             main="Recycled bootstrap MAM-3")

        axis(side = 1, labels = FALSE, tick = FALSE)

        # add subtitle
        mtext(as.expression(bquote(italic(M) == .(object@data$args$bs.M) ~ "|" ~
                                     italic(N) == .(object@data$args$bs.N) ~ "|" ~
                                     italic(sigma[b])  == .(object@data$args$sigmab) ~
                                     "\u00B1" ~ .(object@data$args$sigmab.sd) ~ "|" ~
                                     italic(h) == .(round(object@data$args$bs.h,1))
        )
        ),
        side = 3, line = 0.3, adj = 0.5,
        cex = if(single){0.5}else{0.8})

        # add points
        points(x=pairs[,1], y=pairs[,2], pch=1, col = "grey80")

        # get polynomial function
        poly.curve<- poly.curves[[i]]

        # add curve to plot
        curve(poly.curve, from = min(pairs[,1]), to = (max(pairs[,1])),
              col = "black", add = TRUE, type = "l")

        # add legend
        legend<- c("Third degree", "Fourth degree", "Fifth degree", "Sixth degree")
        legend("topright",  xjust = 0,
               legend = legend[i],
               y.intersp = 1.2,
               bty = "n",
               title = "Polynomial Fit",
               lty = 1,
               lwd= 1.5)

        ## ----- RESIDUALS

        # set margins (bottom, left, top, right)
        par(mar=c(5,5,0,3))

        plot(x = pairs[,1],
             y = residuals(poly.lines[[i]]),
             ylim = c(min(residuals(poly.lines[[i]]))*1.2,
                      as.double(quantile(residuals(poly.lines[[i]]),probs=0.99))),
             xlim=range(pretty(pairs[,1])),
             xaxt = "n",
             bty = "l",
             xaxs = "i",
             col = "grey80",
             ylab = "Fit residual",
             xlab = "Equivalent dose [Gy]")

        axis(side = 1, labels = TRUE, tick = TRUE)

        # add horizontal line
        abline(h = 0, lty=2)

        # calculate residual sum of squares (RSS) and add to plot
        rss<- sum(residuals(poly.lines[[i]])^2)
        mtext(text = paste("RSS =",round(rss,3)), adj = 1,
              side = 3, line = -2,
              cex = if(single){0.6}else{0.8})

        ## ----- PROPORTIONS

      }##EndOf::Plot_loop

      # restore previous plot parameters
      par(.pardefault)

      ### TODO: plotting of the LOESS fit needs to be fleshed out
      ### possibly integrate this in the prior polynomial plot loop

      ### LOESS PLOT
      if (!anyNA(object@data$bootstrap$loess.fit)) {
      pairs<- object@data$bootstrap$pairs$gamma
      pred<- predict(object@data$bootstrap$loess.fit)
      loess<- cbind(pairs[,1], pred)
      loess<- loess[order(loess[,1]),]

      # plot gamma-llik pairs
      plot(pairs,
           ylim = c(0, as.double(quantile( pairs[,2],probs=0.99))),
           ylab = "Likelihood",
           xlab = "Equivalent dose [Gy]",
           col = "gray80")

      # add LOESS line
      lines(loess, type = "l", col = "black")
      }

      ### ------ PLOT BOOTSTRAP LIKELIHOOD FIT

      par(mar=c(5,4,4,4))

      xlim<- range(pretty(object@data$data[,1]))
      xlim[1]<- xlim[1]-object@data$data[which.min(object@data$data[,1]),2]
      xlim[2]<- xlim[2]+object@data$data[which.max(object@data$data[,1]),2]
      xlim<- range(pretty(xlim))

      # empty plot
      plot(NA,NA,
           xlim=xlim,
           ylim=c(0,2),
           xlab="Equivalent dose [Gy]",
           ylab="",
           bty="l",
           axes=FALSE,
           xaxs="i",
           yaxs="i",
           yaxt="n")

      axis(side = 1)
      axis(side = 2, at = c(0,0.5,1))

      mtext(text = "Normalised likelihood / density", side = 2, line = 2.5, adj = 0)

      # set the polynomial to plot
      poly.curve<- poly.curves[[1]] # three degree poly

      # plot a nice grey polygon as in the publication
      step<- 0.1
      x<- seq(min(pairs[,1]), max(pairs[,1]), step)
      y<- poly.curve(x)
      # normalise y-values
      y<- y/max(y)

      x<- c(min(pairs[,1]), x, max(pairs[,1]))
      y<- c(0, y, 0)

      # cutoff negative y values
      y.idx <- !is.na(y) & y >= 0
      x <- x[y.idx]
      y <- y[y.idx]

      # add bootstrap likelihood polygon to plot
      polygon(x, y, col = "grey80", border = NA)

      if (all(x > max(xlim)) || all(x < min(xlim)))
        .throw_warning("Bootstrap estimates out of x-axis range")


      ### ----- PLOT MAM SINGLE ESTIMATE

      # symmetric errors, might not be appropriate
      mean<- object@data$summary$de
      sd<- object@data$summary$de_err


      if (anyNA(c(mean, sd))) {
        .throw_warning("Unable to plot the MAM single estimate (NA value)")

      } else {

        x<- seq(mean-5*sd, mean+5*sd, 0.001)
        y<- dnorm(seq(mean-5*sd, mean+5*sd, 0.001), mean, sd)
        # normalise y-values
        y<- y/max(y)

        points(x, y,
               type="l",
               col="red")

        ## asymmetric errors
        x<- unlist(object@data$profile@profile$gamma$par.vals[,1])
        y<- abs(unlist(object@data$profile@profile$gamma$z))

        if(object@data$args$log == TRUE) {
          x<- exp(x)
        }

        # now invert the data by shifting
        y<- -y
        y<- y-min(y)
        y<- y/max(y)

        # fit a smoothing spline
        l<- stats::spline(x = x, y = y, method = "n", n = 1000)

        # make the endpoints zero
        l$y[1]<- l$y[length(l$y)]<- 0

        # add profile log likelihood curve to plot
        lines(l, col="blue", lwd=1)

        # add vertical lines of the mean values
        #points(x = 80, y = 100,type = "l")
      }

      #### ------ PLOT DE
      par(new = TRUE)

      # sort the data in ascending order
      dat<- object@data$data[order(object@data$data[,1]),]

      x<- dat[,1]
      y<- 1:length(object@data$data[,1])

      plot(x = x, y = y,
           xlim=xlim,
           ylim=c(0, max(y)+1),
           axes = FALSE,
           pch = 16,
           xlab = "",
           ylab="",
           xaxs="i",
           yaxs="i")

      axis(side = 4)
      mtext(text = "# Grain / aliquot", side = 4, line = 2.5)

      # get sorted errors
      err<- object@data$data[order(object@data$data[,1]),2]

      # fancy error bars
      graphics::arrows(x0 = x-err, y0 = y,
             x1 =  x+err, y1 = y,
             code = 3, angle = 90, length = 0.05)

      ### ---- AUXILLARY

      # add legend
      legend("bottomright",
             bty = "n",
             col = c("grey80", "red", "blue", "black"),
             pch = c(NA,NA,NA,16),
             lty = c(1,1,1,1),
             lwd=c(10,2,2,2),
             legend = c("Bootstrap likelihood", "Profile likelihood (gaussian fit)","Profile likelihood", "Grain / aliquot"),
      )

    }##EndOf::Bootstrap_plotting
  }#EndOf::CASE1_MinimumAgeModel-3


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  ## CASE 2: Central Age Model ---------
  if(object@originator=="calc_CentralDose") {

    # get profile log likelihood data
    sig<- object@data$profile$sig*100
    llik<- object@data$profile$llik

    # save previous plot parameter and set new ones
    .pardefault<- par(no.readonly = TRUE)

    # plot the profile log likeihood
    par(oma=c(2,1,2,1),las=1,cex.axis=1.2, cex.lab=1.2)
    plot(sig,llik,type="l",xlab=as.expression(bquote(sigma[OD]~"[%]")),ylab="Log likelihood",lwd=1.5)
    abline(h=0,lty=3)
    abline(h=-1.92,lty=3)
    title(as.expression(bquote("Profile log likelihood for" ~ sigma[OD])))

    # find upper and lower confidence limits for sigma
    sigmax<- sig[which.max(llik)]
    tf<- abs(llik+1.92) < 0.05
    sig95<- sig[tf]
    ntf<- length(sig95)
    sigL<- sig95[1]
    sigU<- sig95[ntf]

    # put them on the graph
    abline(v=sigL)
    abline(v=sigmax)
    abline(v=sigU)
    dx<- 0.006
    dy<- 0.2
    ytext<- min(llik) + dy
    res<- c(sigL,sigmax,sigU)
    text(res+dx,rep(ytext,3),round(res,2),adj=0)

    # restore previous plot parameters
    par(.pardefault)
    rm(.pardefault)
  }##EndOf::Case 2 - calc_CentralDose()


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  ## CASE 3: Fuchs & Lang 2001 --------
  if(object@originator=="calc_FuchsLang2001") {

    ##deal with addition arguments
    extraArgs <- list(...)

    main <- if("main" %in% names(extraArgs)) {extraArgs$main} else {"Fuchs & Lang (2001)"}
    xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else {expression(paste(D[e]," [s]"))}
    ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} else {"# Aliquots"}
    sub <-  if("sub" %in% names(extraArgs)) {extraArgs$sub} else {""}
    cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else {1}
    lwd <- if("lwd" %in% names(extraArgs)) {extraArgs$lwd} else {1}
    pch <- if("pch" %in% names(extraArgs)) {extraArgs$pch} else {19}
    ylim <- if("ylim" %in% names(extraArgs)) {extraArgs$ylim} else {c(1,length(object@data$data[,1])+3)}
    xlim <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else {c(min(object@data$data[,1])-max(object@data$data[,2]), max(object@data$data[,1])+max(object@data$data[,2]))}
    mtext <- if("mtext" %in% names(extraArgs)) {extraArgs$mtext} else {"unknown sample"}

    # extract relevant plotting parameters
    o<- order(object@data$data[[1]])
    data_ordered<- object@data$data[o,]
    usedDeValues<- object@data$usedDeValues
    n.usedDeValues<- object@data$summary$n.usedDeValues

    par(cex = cex, mfrow=c(1,1))

    ##PLOT
    counter<-seq(1,max(o))

    plot(NA,NA,
         ylim = ylim,
         xlim = xlim,
         xlab = xlab,
         ylab = ylab,
         main = main,
         sub = sub)

    ##SEGMENTS
    segments(data_ordered[,1]-data_ordered[,2],1:length(data_ordered[,1]),
             data_ordered[,1]+data_ordered[,2],1:length(data_ordered[,1]),
             col="gray")


    ##POINTS
    points(data_ordered[,1], counter,pch=pch)

    ##LINES
    ##BOUNDARY INFORMATION
    ##lower boundary
    lines(c(
      usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1], #boundary_counter for incorporate skipped values
      usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1]),
      c(min(o)-0.5,max(o)+0.5),
      col="red",
      lty="dashed", lwd = lwd)


    #upper boundary
    lines(c(max(usedDeValues[,1]),max(usedDeValues[,1])),c(min(o)-0.5,max(o)+0.5),
          col="red",lty="dashed", lwd = lwd)

    #plot some further informations into the grafik
    graphics::arrows(
      usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1]+usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1]*0.02, #x1
      max(o)+0.5, #y1
      max(usedDeValues[,1]-usedDeValues[,1]*0.02), #x2
      max(o)+0.5, #y2,
      code=3,
      length=0.03)

    text(
      c(
        usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1],
        usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1]),
      c(max(o)+2,max(o)+2),
      labels=paste("used values = ", n.usedDeValues),
      cex=0.6*cex,
      adj=0)

    ##MTEXT
    mtext(side=3,mtext,cex=cex)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  ## CASE 5: Aliquot Size ---------
  if(object@originator=="calc_AliquotSize") {
    ## FIXME(mcol): The following code has been ported to calc_AliquotSize(),
    ## where it has been further updated. This version is preserved to support
    ## the plotting of an RLum.Results object directly, which calc_AliquotSize()
    ## doesn't yet support.
    if(!is.null(object@data$MC$estimates)) {
      extraArgs <- list(...)

      main <- if("main" %in% names(extraArgs)) { extraArgs$main } else { "Monte Carlo Simulation"  }
      xlab <- if("xlab" %in% names(extraArgs)) { extraArgs$xlab } else { "Amount of grains on aliquot" }

      # extract relevant data
      MC.n<- object@data$MC$estimates
      MC.n.kde<- object@data$MC$kde
      MC.stats<- object@data$MC$statistics
      MC.q<- object@data$MC$quantile
      MC.iter<- object@data$args$MC.iter

      # set layout of plotting device
      graphics::layout(matrix(c(1, 1, 2)), 2, 1)
      par(cex = 0.8)

      ## plot MC estimate distribution

      # set margins (bottom, left, top, right)
      par(mar=c(2,5,5,3))

      # plot histogram
      hist(MC.n, freq=FALSE, col = "gray90",
           main="", xlab=xlab,
           xlim = c(min(MC.n)*0.95, max(MC.n)*1.05),
           ylim = c(0, max(MC.n.kde$y)*1.1))

      # add rugs to histogram
      graphics::rug(MC.n)

      # add KDE curve
      lines(MC.n.kde, col = "black", lwd = 1)

      # add mean, median and quantils (0.05,0.95)
      abline(v=c(MC.stats$mean, MC.stats$median, MC.q),
             lty=c(2, 4, 3,3), lwd = 1)

      # add main- and subtitle
      mtext(main, side = 3, adj = 0.5,
            line = 3, cex = 1)
      mtext(as.expression(bquote(italic(n) == .(MC.iter) ~ "|" ~
                                   italic(hat(mu)) == .(round(MC.stats$mean)) ~ "|" ~
                                   italic(hat(sigma))  == .(round(MC.stats$sd.abs)) ~ "|" ~
                                   italic(frac(hat(sigma),sqrt(n))) == .(round(MC.stats$se.abs))  ~ "|" ~
                                   italic(v) == .(round(MC.stats$skewness, 2))
      )
      ),
      side = 3, line = 0.3, adj = 0.5,
      cex = 0.9)

      # add legend
      legend("topright", legend = c("mean","median", "0.05 / 0.95 quantile"),
             lty = c(2, 4, 3), bg = "white", box.col = "white", cex = 0.9)

      ## BOXPLOT
      # set margins (bottom, left, top, right)
      par(mar=c(5,5,0,3))

      plot(NA, type="n", xlim=c(min(MC.n)*0.95, max(MC.n)*1.05),
           xlab=xlab,  ylim=c(0.5,1.5),
           xaxt="n", yaxt="n", ylab="")
      par(bty="n")
      graphics::boxplot(MC.n, horizontal = TRUE, add = TRUE, bty = "n")
    }
  }#EndOf::Case 5 - calc_AliquotSize()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  ## CASE 6: calc_SourceDoseRate() ----------
  if(object@originator=="calc_SourceDoseRate") {

    ##prepare data
    ##get data
    df <- get_RLum(object = object, data.object = "dose.rate")

    ##reduce the size for plotting, more than 100 points make no sense
    if(nrow(df)>100)
      df <- df[seq(1,nrow(df), length = 100),]

    ##plot settings
    plot.settings <- list(
      main = "Source Dose Rate Prediction",
      xlab = "Date",
      ylab = paste0(
        "Dose rate/(",get_RLum(object = object, data.object = "parameters")$dose.rate.unit,")"),
      log = "",
      cex = 1,
      xlim = NULL,
      ylim = c(min(df[,1]) - max(df[,2]), max(df[,1]) + max(df[,2])),
      pch = 1,
      mtext = paste0(
        "source type: ", get_RLum(object = object, data.object = "parameters")$source.type,
        " | ",
        "half-life: ", get_RLum(object = object, data.object = "parameters")$halflife,
        " a"
      ),
      grid = expression(nx = 10, ny = 10),
      col = 1,
      type = "b",
      lty = 1,
      lwd = 1,
      segments = ""
    )

    ##modify list if something was set
    plot.settings <- modifyList(plot.settings, list(...))


    ##plot
    plot(
      df[,3], df[,1],
      main = plot.settings$main,
      xlab = plot.settings$xlab,
      ylab = plot.settings$ylab,
      xlim = plot.settings$xlim,
      ylim = plot.settings$ylim,
      log = plot.settings$log,
      pch = plot.settings$pch,
      col = plot.settings$pch,
      type = plot.settings$type,
      lty = plot.settings$lty,
      lwd = plot.settings$lwd
    )

    if(!is.null(plot.settings$segments)){
      segments(
        x0 = df[,3], y0 = df[,1] + df[,2],
        x1 = df[,3], y1 = df[,1] - df[,2]
      )
    }

    mtext(side = 3, plot.settings$mtext)

    if(!is.null(plot.settings$grid)){
      graphics::grid(eval(plot.settings$grid))
    }

  }#EndOf::Case 6 - calc_SourceDoseRate()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  ## CASE 7: Fast Ratio ----------
  if (object@originator=="calc_FastRatio") {

    # graphical settings
    settings <- list(main = "Fast Ratio",
                     xlab = "t/s",
                     ylab = "Signal/cts",
                     type = "b",
                     log = "",
                     pch = 16,
                     cex = 1.0,
                     col = "black")
    settings <- modifyList(settings, list(...))

    par(cex = settings$cex)

    # fetch data from RLum.Results object
    curve <- get_RLum(object, "data")
    if (inherits(curve, "RLum.Data.Curve"))
      curve <- get_RLum(curve)
    res <- get_RLum(object, "summary")
    fit <- get_RLum(object, "fit")

    # calculate the dead channel time offset
    offset <- res$dead.channels.start * res$channel.width

    # plot the OSL curve
    plot(curve, type = "n", main = settings$main,
         xlab = settings$xlab, ylab = settings$ylab, log = settings$log)

    # plot points to show measured data points (i.e., the channels)
    if (settings$type == "p" || settings$type == "b")
      points(curve[(res$dead.channels.start + 1):(nrow(curve) - res$dead.channels.end), ],
             pch = settings$pch, col = settings$col)

    # plot dead channels as empty circles
    if (res$dead.channels.start > 0)
      points(curve[1:res$dead.channels.start,])
    if (res$dead.channels.end > 0)
      points(curve[(nrow(curve) - res$dead.channels.end):nrow(curve), ])

    if (settings$type == "l" || settings$type == "b")
      lines(curve, col = settings$col)

    # optional: plot fitted CW curve
    if (!is.null(fit)) {
      nls.fit <- get_RLum(fit, "fit")
      if (!inherits(fit, "try-error") & "fitCW.curve" %in% names(object@data$args)) {
        if (object@data$args$fitCW.curve == "T" | object@data$args$fitCW.curve == TRUE) {
          lines(curve[(res$dead.channels.start + 1):(nrow(curve) - res$dead.channels.end), 1],
                predict(nls.fit), col = "red", lty = 1)

          ##plot curve for additional parameters
          col_components <- c("red", "green", "blue")
          for (i in 1:3) {
            if (!is.na(fit@data$data[[paste0("I0", i)]]))
              curve(fit@data$data[[paste0("I0", i)]] * fit@data$data[[paste0("lambda", i)]] * exp(-fit@data$data[[paste0("lambda", i)]] * x),
                    lwd = 1, lty = 4, add = TRUE, col = col_components[i])
          }

        }
      }
    }

    # add vertical lines and labels for L1, L2, L3
    L_times <- c(curve[res$Ch_L1, 1],
                 curve[res$Ch_L2, 1],
                 curve[res$Ch_L3_start, 1],
                 curve[res$Ch_L3_end, 1]) + offset
    abline(v = L_times,
           lty = 2)
    text(L_times, max(curve[ ,2]) * 0.95, pos = c(4,4,2,2),
         labels = expression('L'[1], 'L'[2], 'L'[3['start']], 'L'[3['end']]))

  }#EndOf::Case7 - calc_FastRatio()
}

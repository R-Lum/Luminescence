#' @title Estimate the amount of grains on an aliquot
#'
#' @description
#' This function can be used to either estimate the number of grains on an
#' aliquot or to compute the packing density, depending on the arguments
#' provided.
#'
#' The following formula is used to estimate the number of grains `n`:
#'
#' \deqn{n = (\pi*x^2)/(\pi*y^2)*d}
#'
#' where `x` is the radius of the aliquot size (microns), `y` is the mean
#' radius of the mineral grains (mm), and `d` is the packing density
#' (a value between 0 and 1).
#'
#' **Packing density**
#'
#' The default value for `packing.density` is 0.65, which is the mean of
#' empirical values determined by Heer et al. (2012) and unpublished data from
#' the Cologne luminescence laboratory. If `packing.density = Inf`, a maximum
#' density of \eqn{\pi / \sqrt{12} \approx 0.9069} is used. However, note that
#' this value is not appropriate as the standard preparation procedure of
#' aliquots resembles a PECC (*Packing Equal Circles in a Circle*) problem,
#' where the maximum packing density is asymptotic to about 0.87.
#'
#' **Monte Carlo simulation**
#'
#' The number of grains on an aliquot can be estimated by Monte Carlo simulation
#' when setting `MC = TRUE`. All parameters necessary to calculate
#' `n` (`x`, `y`, `d`) are assumed to be normally distributed with means
#' \eqn{\mu_x, \mu_y, \mu_d} and standard deviations \eqn{\sigma_x, \sigma_y, \sigma_d}.
#'
#' For the mean grain size, random samples are taken first from
#' \eqn{N(\mu_y, \sigma_y)}, where \eqn{\mu_y = mean.grain.size} and
#' \eqn{\sigma_y = (max.grain.size-min.grain.size)/4}, so that 95% of all
#' grains are within the provided the grain size range. This effectively takes
#' into account that after sieving the sample there is still a small chance of
#' having grains smaller or larger than the used mesh sizes. For each random
#' sample the mean grain size is calculated, from which random subsamples are
#' drawn for the Monte Carlo simulation.
#'
#' The packing density is assumed
#' to be normally distributed with an empirically determined \eqn{\mu = 0.65}
#' (or provided value) and \eqn{\sigma = 0.18}. The normal distribution is
#' truncated at `d = 0.87` as this is approximately the maximum packing
#' density that can be achieved in a PECC problem.
#'
#' The sample diameter has
#' \eqn{\mu = sample.diameter} and \eqn{\sigma = 0.2} to take into account
#' variations in sample disc preparation (i.e. applying silicon spray to the
#' disc). A lower truncation point at `x = 0.5` is used, which assumes
#' that aliquots with sample diameter smaller than 0.5 mm are discarded.
#' Likewise, the normal distribution is truncated at the diameter of the
#' sample carrier (9.8 mm by default, but controllable via the
#' `sample_carrier.diameter` argument).
#'
#' For each random sample drawn from the
#' normal distribution, the amount of grains on the aliquot is calculated. By
#' default, `10^4` iterations are used, but the can be controlled with
#' option `MC.iter` (see `...`). Results are visualised in a bar- and
#' boxplot together with a statistical summary.
#'
#' @param grain.size [numeric] (**required**):
#' mean grain size (microns) or a range of grain sizes from which the
#' mean grain size is computed (e.g. `c(100,200)`).
#'
#' @param sample.diameter [numeric] (**required**):
#' diameter (mm) of the targeted area on the sample carrier.
#'
#' @param packing.density [numeric] (*with default*):
#' empirical value for the mean packing density. \cr
#' If `packing.density = Inf`, a hexagonal structure on an infinite
#' plane with a packing density of \eqn{\pi / \sqrt{12} \approx 0.9069}
#' is assumed.
#'
#' @param MC [logical] (*optional*):
#' if `TRUE` the function performs a Monte Carlo simulation for estimating the
#' amount of grains on the sample carrier and assumes random errors in grain
#' size distribution and packing density. Requires `grain.size` to be specified
#' as a 2-element vector with the range (min and max) of grain sizes.
#' For more information see details.
#'
#' @param grains.counted [numeric] (*optional*):
#' grains counted on a sample carrier. If a non-zero positive integer is
#' provided, this function will calculate the packing density of the aliquot.
#' If more than one value is provided, the mean packing density and its
#' standard deviation are calculated.
#' Note that this overrides `packing.density`.
#'
#' @param sample_carrier.diameter [numeric] (*with default*):
#' diameter (mm) of the sample carrier.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param ... further arguments and graphical parameters to be passed. In
#' particular, `MC.iter` to control the number of Monte Carlo iterations,
#' `main`, `xlab`, `col`, `line_col`, `line_lwd`, `cex`, `rug` (`TRUE/FALSE`),
#' `boxplot` (`TRUE/FALSE`), `summary` (`TRUE/FALSE`), `legend` (`TRUE/FALSE`).
#'
#' @return
#' Returns a terminal output. In addition an
#' [RLum.Results-class] object is returned containing the
#' following element:
#'
#' \item{.$summary}{[data.frame] summary of all relevant calculation results.}
#' \item{.$args}{[list] used arguments}
#' \item{.$call}{[call] the function call}
#' \item{.$MC}{[list] results of the Monte Carlo simulation}
#'
#' The output should be accessed using the function [get_RLum].
#'
#' @section Function version: 0.33
#'
#' @author
#' Christoph Burow, University of Cologne (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)
#'
#' @references
#' Duller, G.A.T., 2008. Single-grain optical dating of Quaternary
#' sediments: why aliquot size matters in luminescence dating. Boreas 37,
#' 589-612.
#'
#' Heer, A.J., Adamiec, G., Moska, P., 2012. How many grains
#' are there on a single aliquot?. Ancient TL 30, 9-16.
#'
#' **Further reading**
#'
#' Chang, H.-C., Wang, L.-C., 2010. A simple proof of Thue's
#' Theorem on Circle Packing. [https://arxiv.org/pdf/1009.4322v1](),
#' 2013-09-13.
#'
#' Graham, R.L., Lubachevsky, B.D., Nurmela, K.J.,
#' Oestergard, P.R.J., 1998.  Dense packings of congruent circles in a circle.
#' Discrete Mathematics 181, 139-154.
#'
#' Huang, W., Ye, T., 2011. Global
#' optimization method for finding dense packings of equal circles in a circle.
#' European Journal of Operational Research 210, 474-481.
#'
#' @examples
#'
#' ## Estimate the amount of grains on a small aliquot
#' calc_AliquotSize(grain.size = c(100,150), sample.diameter = 1, MC.iter = 100)
#'
#' ## Calculate the mean packing density of large aliquots
#' calc_AliquotSize(grain.size = c(100,200), sample.diameter = 8,
#'                  grains.counted = c(2525,2312,2880), MC.iter = 100)
#'
#' @md
#' @export
calc_AliquotSize <- function(
  grain.size,
  sample.diameter,
  packing.density = 0.65,
  MC = TRUE,
  grains.counted,
  sample_carrier.diameter = 9.8,
  plot = TRUE,
  ...
) {
  .set_function_name("calc_AliquotSize")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  if (missing(grain.size) ||
      length(grain.size) == 0 || length(grain.size) > 2) {
    .throw_error("Please provide the mean grain size or the range ",
                 "of grain sizes (in microns)")
  }

  .validate_positive_scalar(packing.density)
  if (is.infinite(packing.density)) {
    # use ~0.907... from Thue's Theorem as packing density
    packing.density <- pi / sqrt(12)
  }
  if (packing.density > 1) {
    .throw_error("'packing.density' should be a value between 0 and 1")
  }

  .validate_positive_scalar(sample.diameter)
  .validate_positive_scalar(sample_carrier.diameter)
  if (sample.diameter > sample_carrier.diameter)
    .throw_warning("A sample diameter of ", sample.diameter,  " mm was ",
                   "specified for a sample carrier of ", sample_carrier.diameter,
                   " mm, values will be capped to the sample carrier size")

  if (!missing(grains.counted) && MC == TRUE) {
    MC = FALSE
    message("\nMonte Carlo simulation is only available for estimating the ",
            "amount of grains on the sample disc, 'MC' reset to FALSE\n")
  }

  if(MC == TRUE && length(grain.size) != 2) {
    .throw_error("'grain.size' must be a vector containing the min and max ",
                 "grain size when using Monte Carlo simulations")
  }


  ##==========================================================================##
  ## ... ARGUMENTS
  ##==========================================================================##

  # set default parameters
  settings <- list(MC.iter = 10^4,
                   verbose = TRUE)

  # override settings with user arguments
  settings <- modifyList(settings, list(...))

  MC.n <- MC.stats <- MC.n.kde <- MC.t.test <- MC.q <- NULL

  ##==========================================================================##
  ## CALCULATIONS
  ##==========================================================================##

  # calculate the mean grain size
  range.flag<- FALSE
  if(length(grain.size) == 2) {
    gs.range<- grain.size
    grain.size<- mean(grain.size)
    range.flag<- TRUE
  }

  # function to calculate the amount of grains
  ## original formula: n <- ((pi * (sd / 2)^2) / (pi * (gs / 2000)^2)) * d
  calc_n <- function(sd, gs, d) {
    (1000 * sd / gs)^2 * d
  }

  # calculate the amount of grains on the aliquot
  if(missing(grains.counted) == TRUE) {
    n.grains<- calc_n(sample.diameter, grain.size, packing.density)

    ##========================================================================##
    ## MONTE CARLO SIMULATION

    if(MC == TRUE && range.flag == TRUE) {

      # create a random set of packing densities assuming a normal
      # distribution with the empirically determined standard deviation of
      # 0.18.
      d.mc<- rnorm(settings$MC.iter, packing.density, 0.18)

      # in a PECC the packing density cannot be larger than ~0.87
      d.mc <- pmin(d.mc, 0.87)
      d.mc <- pmax(d.mc, 0.25)

      # create a random set of sample diameters assuming a normal
      # distribution with an assumed standard deviation of
      # 0.2. For a more conservative estimate this is divided by 2.
      sd.mc<- rnorm(settings$MC.iter, sample.diameter, 0.2)

      # it is assumed that sample diameters < 0.5 mm either do not
      # occur, or are discarded. Either way, any smaller sample
      # diameter is capped at 0.5.
      # Also, the sample diameter can not be larger than the sample
      # disc.
      sd.mc <- pmax(sd.mc, 0.5)
      sd.mc <- pmin(sd.mc, sample_carrier.diameter)

      # create random samples assuming a normal distribution
      # with the mean grain size as mean and half the range (min:max)
      # as standard deviation. For a more conservative estimate this
      # is further devided by 2, so half the range is regarded as
      # two sigma.
      gs.mc<- rnorm(settings$MC.iter, grain.size, diff(gs.range)/4)

      # draw random samples from the grain size spectrum (gs.mc) and calculate
      # the mean for each sample. This gives an approximation of the variation
      # in mean grain size on the sample disc
      gs.mc.sampleMean <-
        sapply(1:length(gs.mc),
               function(x) mean(sample(gs.mc,
                                       calc_n(
                                           sample(sd.mc, size = 1),
                                           grain.size,
                                           sample(d.mc, size = 1)
                                       ), replace = TRUE)))

      # calculate n for each MC data set
      MC.n <- apply(data.frame(sd.mc, gs.mc.sampleMean, d.mc), 1,
                    function(x) calc_n(x[1], x[2], x[3]))

      # summarize MC estimates
      MC.q<- quantile(MC.n, c(0.05,0.95))
      MC.n.kde<- density(MC.n, n = 10000)

      # apply student's t-test
      MC.t.test<- stats::t.test(MC.n)
      MC.t.lower<- MC.t.test["conf.int"]$conf.int[1]
      MC.t.upper<- MC.t.test["conf.int"]$conf.int[2]
      MC.t.se<- (MC.t.upper-MC.t.lower)/3.92

      # get unweighted statistics from calc_Statistics() function
      MC.stats<- calc_Statistics(as.data.frame(cbind(MC.n,0.0001)))$unweighted
    }
  }#EndOf:estimate number of grains


  ##========================================================================##
  ## CALCULATE PACKING DENSITY

  if (!missing(grains.counted)) {
    area.container <- pi * (sample.diameter / 2)^2
    area.grains <- pi * (grain.size / 1000 / 2)^2 * grains.counted
    packing.density <- area.grains / area.container
    std.d <- sd(packing.density)
  }

  ##==========================================================================##
  ##TERMINAL OUTPUT
  ##==========================================================================##
  if (settings$verbose) {

    cat("\n [calc_AliquotSize]")
    cat("\n\n ---------------------------------------------------------")
    cat("\n mean grain size (microns)  :", grain.size)
    cat("\n sample diameter (mm)       :", sample.diameter)
    if(missing(grains.counted) == FALSE) {
      if(length(grains.counted) == 1) {
        cat("\n counted grains             :", grains.counted)
      } else {
        cat("\n mean counted grains        :", round(mean(grains.counted)))
      }
    }
    if(missing(grains.counted) == TRUE) {
      cat("\n packing density            :", round(packing.density, 3))
    }
    if(missing(grains.counted) == FALSE) {
      if(length(grains.counted) == 1) {
        cat("\n packing density            :", round(packing.density, 3))
      } else {
        cat("\n mean packing density       :", round(mean(packing.density), 3))
        cat("\n standard deviation         :", round(std.d, 3))
      }
    }
    if(missing(grains.counted) == TRUE) {
      cat("\n number of grains           :", round(n.grains, 0))
    }

    if(MC == TRUE && range.flag == TRUE) {
      cat("\n\n --------------- Monte Carlo Estimates -------------------")
      cat("\n number of iterations (n)     :", settings$MC.iter)
      cat("\n median                       :", round(MC.stats$median))
      cat("\n mean                         :", round(MC.stats$mean))
      cat("\n standard deviation (mean)    :", round(MC.stats$sd.abs))
      cat("\n standard error (mean)        :", round(MC.stats$se.abs, 1))
      cat("\n 95% CI from t-test (mean)    :", round(MC.t.lower), "-", round(MC.t.upper))
      cat("\n standard error from CI (mean):", round(MC.t.se, 1))
    }
    cat("\n ---------------------------------------------------------\n")
  }

  ##==========================================================================##
  ##RETURN VALUES
  ##==========================================================================##

  if (missing(grains.counted))
    grains.counted <- NA
  else
    n.grains <- NA
  summary <- data.frame(grain.size = grain.size,
                        sample.diameter = sample.diameter,
                        packing.density = packing.density,
                        n.grains = round(n.grains, 0),
                        grains.counted = grains.counted)

  call<- sys.call()
  args<- as.list(sys.call())[-1]

  # create S4 object
  object <- set_RLum(
    class = "RLum.Results",
    originator = "calc_AliquotSize",
    data = list(
      summary=summary,
      MC=list(estimates=MC.n,
              statistics=MC.stats,
              kde=MC.n.kde,
              t.test=MC.t.test,
              quantile=MC.q)),
    info = list(call=call,
                args=args))

  ##=========##
  ## PLOTTING
  if (plot && !is.null(object@data$MC$estimates)) {

    ## default graphical settings
    settings <- list(
        main = "Monte Carlo Simulation",
        xlab = "Amount of grains on aliquot",
        col = "gray90",
        line_col = "black",
        line_lwd = 1,
        cex = 0.8,
        rug = TRUE,
        boxplot = TRUE,
        summary = TRUE,
        legend = TRUE,
        NULL
    )
    settings <- modifyList(settings, list(...))

    ## extract relevant data
    MC.n <- object@data$MC$estimates
    MC.n.kde <- object@data$MC$kde
    MC.stats <- object@data$MC$statistics
    MC.q <- object@data$MC$quantile
    MC.iter <- object@data$args$MC.iter

    ## set layout of plotting device
    nrow.splits <- if (settings$boxplot) 2 else 1
    graphics::layout(matrix(c(1, 1, nrow.splits)), nrow.splits, 1)
    par(cex = settings$cex)

    ## plot MC estimate distribution

    ## set margins (bottom, left, top, right)
    par(mar = c(if (settings$boxplot) 2 else 5, 5, 5, 3))

    ## plot histogram
    hist(MC.n, freq = FALSE, col = settings$col,
         main = settings$main, xlab = settings$xlab, cex = settings$cex,
         xlim = c(min(MC.n) * 0.95, max(MC.n) * 1.05),
         ylim = c(0, max(MC.n.kde$y) * 1.1))

    ## add rugs to histogram
    if (settings$rug) {
      graphics::rug(MC.n)
    }

    ## add KDE curve
    lines(MC.n.kde, col = settings$line_col, lwd = settings$line_lwd)

    ## add mean, median and quantils (0.05,0.95)
    abline(v = c(MC.stats$mean, MC.stats$median, MC.q),
           lty = c(2, 4, 3, 3), lwd = 1)

    ## add title and subtitle
    if (settings$summary) {
      mtext(as.expression(bquote(italic(n) == .(MC.iter) ~ "|" ~
                                 italic(hat(mu)) == .(round(MC.stats$mean)) ~ "|" ~
                                 italic(hat(sigma)) == .(round(MC.stats$sd.abs)) ~ "|" ~
                                 italic(frac(hat(sigma), sqrt(n))) == .(round(MC.stats$se.abs)) ~ "|" ~
                                 italic(v) == .(round(MC.stats$skewness, 2)))),
            side = 3, line = -0.5, adj = 0.5, cex = 0.9 * settings$cex)
    }

    ## add legend
    if (settings$legend) {
      legend("topright", legend = c("mean","median", "0.05 / 0.95 quantile"),
             lty = c(2, 4, 3), bg = "white", box.col = "white",
             cex = 0.9 * settings$cex)
    }

    ## BOXPLOT
    ## set margins (bottom, left, top, right)
    if (settings$boxplot) {
      par(mar = c(5, 5, 0, 3))
      plot(NA, type = "n", xlim = c(min(MC.n) * 0.95, max(MC.n) * 1.05),
           xlab = settings$xlab, ylim = c(0.5, 1.5),
           xaxt = "n", yaxt = "n", ylab = "")
      graphics::boxplot(MC.n, horizontal = TRUE, add = TRUE, bty = "n")
    }
  }

  # Return values
  invisible(object)
}

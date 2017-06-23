#' Estimate the amount of grains on an aliquot
#'
#' Estimate the number of grains on an aliquot. Alternatively, the packing
#' density of an aliquot is computed.
#'
#' This function can be used to either estimate the number of grains on an
#' aliquot or to compute the packing density depending on the the arguments
#' provided.
#' 
#' The following function is used to estimate the number of grains `n`:
#' 
#' \deqn{n = (\pi*x^2)/(\pi*y^2)*d} 
#' 
#' where `x` is the radius of the aliquot size (microns), `y` is the mean 
#' radius of the mineral grains (mm) and `d` is the packing density 
#' (value between 0 and 1).
#'
#' **Packing density** 
#' 
#' The default value for `packing.density` is 0.65, which is the mean of 
#' empirical values determined by Heer et al. (2012) and unpublished data from 
#' the Cologne luminescence laboratory. If `packing.density = "Inf"` a maximum 
#' density of  \eqn{\pi/\sqrt12 = 0.9068\ldots} is used. However, note that 
#' this value is not appropriate as the standard preparation procedure of
#' aliquots resembles a  PECC (*"Packing Equal Circles in a Circle"*) problem 
#' where the maximum packing density is asymptotic to about 0.87.
#'
#' **Monte Carlo simulation** 
#'
#' The number of grains on an aliquot can be estimated by Monte Carlo simulation 
#' when setting `MC = TRUE`. Each of the parameters necessary to calculate 
#' `n` (`x`, `y`, `d`) are assumed to be normally distributed with means 
#' \eqn{\mu_x, \mu_y, \mu_d} and standard deviations \eqn{\sigma_x, \sigma_y, \sigma_d}. 
#'
#' For the mean grain size random samples are taken first from
#' \eqn{N(\mu_y, \sigma_y)}, where \eqn{\mu_y = mean.grain.size} and
#' \eqn{\sigma_y = (max.grain.size-min.grain.size)/4} so that 95\% of all
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
#' density that can be achieved in PECC problem. 
#'
#' The sample diameter has
#' \eqn{\mu = sample.diameter} and \eqn{\sigma = 0.2} to take into account
#' variations in sample disc preparation (i.e. applying silicon spray to the
#' disc). A lower truncation point at `x = 0.5` is used, which assumes
#' that aliqouts with smaller sample diameters of 0.5 mm are discarded.
#' Likewise, the normal distribution is truncated at 9.8 mm, which is the
#' diameter of the sample disc. 
#'
#' For each random sample drawn from the
#' normal distributions the amount of grains on the aliquot is calculated. By
#' default, `10^5` iterations are used, but can be reduced/increased with
#' `MC.iter` (see `...`). The results are visualised in a bar- and
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
#' empirical value for mean packing density. \cr 
#' If `packing.density = "Inf"` a hexagonal structure on an infinite plane with 
#' a packing density of \eqn{0.906\ldots} is assumed.
#'
#' @param MC [logical] (*optional*): 
#' if `TRUE` the function performs a monte carlo simulation for estimating the 
#' amount of grains on the sample carrier and assumes random errors in grain 
#' size distribution and packing density. Requires a vector with min and max 
#' grain size for `grain.size`. For more information see details.
#'
#' @param grains.counted [numeric] (*optional*):
#' grains counted on a sample carrier. If a non-zero positive integer is provided this function
#' will calculate the packing density of the aliquot. If more than one value is
#' provided the mean packing density and its standard deviation is calculated.
#' Note that this overrides `packing.density`.
#'
#' @param plot [logical] (*with default*): 
#' plot output (`TRUE`/`FALSE`)
#'
#' @param ... further arguments to pass (`main, xlab, MC.iter`).
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
#' @section Function version: 0.31
#'
#' @author Christoph Burow, University of Cologne (Germany)
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
#' Theorem on Circle Packing. [http://arxiv.org/pdf/1009.4322v1.pdf](),
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
  plot=TRUE,
  ...
){
  ##==========================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ##==========================================================================##

  if(length(grain.size) == 0 | length(grain.size) > 2) {
    cat(paste("\nPlease provide the mean grain size or a range",
              "of grain sizes (in microns).\n"), fill = FALSE)
    stop(domain=NA)
  }

  if(packing.density < 0 | packing.density > 1) {
    if(packing.density == "inf") {
    } else {
      cat(paste("\nOnly values between 0 and 1 allowed for packing density!\n"))
      stop(domain=NA)
    }
  }

  if(sample.diameter < 0) {
    cat(paste("\nPlease provide only positive integers.\n"))
    stop(domain=NA)
  }

  if (sample.diameter > 9.8)
    warning("\n A sample diameter of ", sample.diameter ," mm was specified, but common sample discs are 9.8 mm in diameter.", call. = FALSE)

  if(missing(grains.counted) == FALSE) {
    if(MC == TRUE) {
      MC = FALSE
      cat(paste("\nMonte Carlo simulation is only available for estimating the",
                "amount of grains on the sample disc. Automatically set to",
                "FALSE.\n"))
    }
  }

  if(MC == TRUE && length(grain.size) != 2) {
    cat(paste("\nPlease provide a vector containing the min and max grain",
              "grain size(e.g. c(100,150) when using Monte Carlo simulations.\n"))
    stop(domain=NA)
  }


  ##==========================================================================##
  ## ... ARGUMENTS
  ##==========================================================================##

  # set default parameters
  settings <- list(MC.iter = 10^4,
                   verbose = TRUE)

  # override settings with user arguments
  settings <- modifyList(settings, list(...))


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

  # use ~0.907... from Thue's Theorem as packing density
  if(packing.density == "inf") {
    packing.density = pi/sqrt(12)
  }

  # function to calculate the amount of grains
  calc_n<- function(sd, gs, d) {
    n<- ((pi*(sd/2)^2)/
           (pi*(gs/2000)^2))*d
    return(n)
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

      # in a PECC the packing density can not be larger than ~0.87
      d.mc[which(d.mc > 0.87)]<- 0.87
      d.mc[which(d.mc < 0.25)]<- 0.25

      # create a random set of sample diameters assuming a normal
      # distribution with an assumed standard deviation of
      # 0.2. For a more conservative estimate this is divided by 2.
      sd.mc<- rnorm(settings$MC.iter, sample.diameter, 0.2)

      # it is assumed that sample diameters < 0.5 mm either do not
      # occur, or are discarded. Either way, any smaller sample
      # diameter is capped at 0.5.
      # Also, the sample diameter can not be larger than the sample
      # disc, i.e. 9.8 mm.
      sd.mc[which(sd.mc <0.5)]<- 0.5
      if (sample.diameter <= 9.8)
        sd.mc[which(sd.mc >9.8)]<- 9.8

      # create random samples assuming a normal distribution
      # with the mean grain size as mean and half the range (min:max)
      # as standard deviation. For a more conservative estimate this
      # is further devided by 2, so half the range is regarded as
      # two sigma.
      gs.mc<- rnorm(settings$MC.iter, grain.size, diff(gs.range)/4)

      # draw random samples from the grain size spectrum (gs.mc) and calculate
      # the mean for each sample. This gives an approximation of the variation
      # in mean grain size on the sample disc
      gs.mc.sampleMean<- vector(mode = "numeric")


      for(i in 1:length(gs.mc)) {
        gs.mc.sampleMean[i]<- mean(sample(gs.mc, calc_n(
          sample(sd.mc, size = 1),
          grain.size,
          sample(d.mc, size = 1)
        ), replace = TRUE))
      }

      # create empty vector for MC estimates of n
      MC.n<- vector(mode="numeric")

      # calculate n for each MC data set
      for(i in 1:length(gs.mc)) {
        MC.n[i]<- calc_n(sd.mc[i],
                         gs.mc.sampleMean[i],
                         d.mc[i])
      }

      # summarize MC estimates
      MC.q<- quantile(MC.n, c(0.05,0.95))
      MC.n.kde<- density(MC.n, n = 10000)

      # apply student's t-test
      MC.t.test<- t.test(MC.n)
      MC.t.lower<- MC.t.test["conf.int"]$conf.int[1]
      MC.t.upper<- MC.t.test["conf.int"]$conf.int[2]
      MC.t.se<- (MC.t.upper-MC.t.lower)/3.92


      # get unweighted statistics from calc_Statistics() function
      MC.stats<- calc_Statistics(as.data.frame(cbind(MC.n,0.0001)))$unweighted

    }
  }#EndOf:estimate number of grains


  ##========================================================================##
  ## CALCULATE PACKING DENSITY

  if(missing(grains.counted) == FALSE) {

    area.container<- pi*sample.diameter^2

    if(length(grains.counted) == 1) {
      area.grains<- (pi*(grain.size/1000)^2)*grains.counted
      packing.density<- area.grains/area.container
    }
    else {
      packing.densities<- length(grains.counted)
      for(i in 1:length(grains.counted)) {
        area.grains<- (pi*(grain.size/1000)^2)*grains.counted[i]
        packing.densities[i]<- area.grains/area.container
      }
      std.d<- sd(packing.densities)
    }
  }

  ##==========================================================================##
  ##TERMINAL OUTPUT
  ##==========================================================================##
  if (settings$verbose) {

    cat("\n [calc_AliquotSize]")
    cat(paste("\n\n ---------------------------------------------------------"))
    cat(paste("\n mean grain size (microns)  :", grain.size))
    cat(paste("\n sample diameter (mm)       :", sample.diameter))
    if(missing(grains.counted) == FALSE) {
      if(length(grains.counted) == 1) {
        cat(paste("\n counted grains             :", grains.counted))
      } else {
        cat(paste("\n mean counted grains        :", round(mean(grains.counted))))
      }
    }
    if(missing(grains.counted) == TRUE) {
      cat(paste("\n packing density            :", round(packing.density,3)))
    }
    if(missing(grains.counted) == FALSE) {
      if(length(grains.counted) == 1) {
        cat(paste("\n packing density            :", round(packing.density,3)))
      } else {
        cat(paste("\n mean packing density       :", round(mean(packing.densities),3)))
        cat(paste("\n standard deviation         :", round(std.d,3)))
      }
    }
    if(missing(grains.counted) == TRUE) {
      cat(paste("\n number of grains           :", round(n.grains,0)))
    }



    if(MC == TRUE && range.flag == TRUE) {
      cat(paste(cat(paste("\n\n --------------- Monte Carlo Estimates -------------------"))))
      cat(paste("\n number of iterations (n)     :", settings$MC.iter))
      cat(paste("\n median                       :", round(MC.stats$median)))
      cat(paste("\n mean                         :", round(MC.stats$mean)))
      cat(paste("\n standard deviation (mean)    :", round(MC.stats$sd.abs)))
      cat(paste("\n standard error (mean)        :", round(MC.stats$se.abs, 1)))
      cat(paste("\n 95% CI from t-test (mean)    :", round(MC.t.lower), "-", round(MC.t.upper)))
      cat(paste("\n standard error from CI (mean):", round(MC.t.se, 1)))
      cat(paste("\n ---------------------------------------------------------\n"))

    } else {
      cat(paste("\n ---------------------------------------------------------\n"))
    }

  }
  ##==========================================================================##
  ##RETURN VALUES
  ##==========================================================================##


  # prepare return values for mode: estimate grains
  if(missing(grains.counted) == TRUE) {
    summary<- data.frame(grain.size = grain.size,
                         sample.diameter = sample.diameter,
                         packing.density = packing.density,
                         n.grains = round(n.grains,0),
                         grains.counted = NA)
  }

  # prepare return values for mode: estimate packing density/densities
  if(missing(grains.counted) == FALSE) {

    # return values if only one value for counted.grains is provided
    if(length(grains.counted) == 1) {
      summary<- data.frame(grain.size = grain.size,
                           sample.diameter = sample.diameter,
                           packing.density = packing.density,
                           n.grains = NA,
                           grains.counted = grains.counted)
    } else {
      # return values if more than one value for counted.grains is provided
      summary<- data.frame(rbind(1:5))
      colnames(summary)<- c("grain.size", "sample.diameter", "packing.density",
                            "n.grains","grains.counted")
      for(i in 1:length(grains.counted)) {
        summary[i,]<- c(grain.size, sample.diameter, packing.densities[i],
                        n.grains = NA, grains.counted[i])
      }
    }
  }

  if(!MC) {
    MC.n<- NULL
    MC.stats<- NULL
    MC.n.kde<- NULL
    MC.t.test<- NULL
    MC.q<- NULL
  }

  if(missing(grains.counted)) grains.counted<- NA

  call<- sys.call()
  args<- as.list(sys.call())[-1]

  # create S4 object
  newRLumResults.calc_AliquotSize <- set_RLum(
    class = "RLum.Results",
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
  if(plot==TRUE) {
    try(plot_RLum.Results(newRLumResults.calc_AliquotSize, ...))
  }

  # Return values
  invisible(newRLumResults.calc_AliquotSize)

}

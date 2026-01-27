#' @title (Un-)logged minimum age model (MAM) after Galbraith et al. (1999)
#'
#' @description
#' Function to fit the (un-)logged three or four parameter minimum dose model
#' (MAM-3/4) to De data.
#'
#' **Parameters**
#'
#' This model has four parameters:
#' \tabular{rl}{
#' `gamma`: \tab minimum dose on the log scale \cr
#' `mu`: \tab mean of the non-truncated normal distribution \cr
#' `sigma`: \tab spread in ages above the minimum \cr
#' `p0`: \tab proportion of grains at gamma \cr }
#'
#' If `par=3` (default) the 3-parameter minimum age model is applied,
#' where `gamma=mu`. For `par=4` the 4-parameter model is applied instead.
#'
#' **(Un-)logged model**
#'
#' In the original version of the minimum dose model, the basic data are the natural
#' logarithms of the De estimates and relative standard errors of the De
#' estimates. The value for `sigmab` must be provided as a ratio
#' (e.g, 0.2 for 20 %). This model will be applied if `log = TRUE`.
#'
#' If `log=FALSE`, the modified un-logged model will be applied instead. This
#' has essentially the same form as the original version.  `gamma` and
#' `sigma` are in Gy and `gamma` becomes the minimum true dose in the
#' population.
#' **Note** that the un-logged model requires `sigmab` to be in the same
#' absolute unit as the provided De values (seconds or Gray).
#'
#' While the original (logged) version of the minimum dose
#' model may be appropriate for most samples (i.e. De distributions), the
#' modified (un-logged) version is specially designed for modern-age and young
#' samples containing negative, zero or near-zero De estimates (Arnold et al.
#' 2009, p. 323).
#'
#' **Initial values & boundaries**
#'
#' The log-likelihood calculations use the [nlminb] function for box-constrained
#' optimisation using PORT routines.  Accordingly, initial values for the four
#' parameters can be specified via `init.values`. If no values are
#' provided for `init.values`, reasonable starting values are estimated
#' from the input data.  If the final estimates of *gamma*, *mu*,
#' *sigma* and *p0* are totally off target, consider providing custom
#' starting values via `init.values`.
#'
#' The boundaries for the individual model parameters are not required to be
#' explicitly specified. If you want to override the default boundary values,
#' use arguments `gamma.lower`, `gamma.upper`, `sigma.lower`, `sigma.upper`,
#' `p0.lower`, `p0.upper`, `mu.lower` and `mu.upper`.
#'
#' **Bootstrap**
#'
#' When `bootstrap=TRUE` the function applies the bootstrapping method as
#' described in Cunningham & Wallinga (2012). By default, the minimum age model
#' produces 1000 first level and 3000 second level bootstrap replicates. The
#' uncertainty on `sigmab` is 0.04 by default. These values can be changed by
#' using the arguments `bs.M` (first level replicates), `bs.N`
#' (second level replicates) and `sigmab.sd` (error on `sigmab`). The bandwidth
#' of the kernel density estimate can be specified with `bs.h`. By default,
#' this is calculated as:
#'
#' \deqn{h = (2*\sigma_{DE})/\sqrt{n}}
#'
#' **Multicore support**
#'
#' Parallel computations can be activated by setting `multicore=TRUE`.
#' By default, the number of available logical CPU cores is determined
#' automatically, but can be changed with `cores`. The multicore support
#' is only available when `bootstrap=TRUE` and spawns `n` R instances
#' for each core to get MAM estimates for each of the N and M bootstrap
#' replicates. Note that this option is highly experimental and may or may not
#' work for your machine. The performance gain increases for larger number
#' of bootstrap replicates. However, note that with each additional core (and
#' hence R instance) the memory usage can significantly increase, depending
#' on the number of bootstrap replicates. When insufficient memory is available,
#' there will be a massive performance hit.
#'
#' **Likelihood profiles**
#'
#' The likelihood profiles are generated and plotted by the `bbmle` package.
#' The profile likelihood plots look different to ordinary profile likelihood as
#'
#' "`[...]` the plot method for likelihood profiles displays the square root of
#' the deviance difference (twice the difference in negative log-likelihood from
#' the best fit), so it will be V-shaped for cases where the quadratic approximation
#' works well `[...]`." (Bolker 2016).
#'
#' For more details on the profile likelihood
#' calculations and plots please see the vignettes of the `bbmle` package
#' (also available here: [https://CRAN.R-project.org/package=bbmle]().
#'
#' @param data [Luminescence::RLum.Results-class] or [data.frame] (**required**):
#' for [data.frame]: two columns for De and De error.
#'
#' @param sigmab [numeric] (**required**):
#' additional spread in De values, representing the expected overdispersion in
#' the data should the sample be well-bleached (Cunningham & Wallinga 2012, p. 100).
#' **NOTE**: For the logged model (`log = TRUE`) this value must be
#' a fraction, e.g. 0.2 (= 20 %). If the un-logged model is used (`log = FALSE`),
#' `sigmab` must be provided in the same absolute units of the De values (seconds or Gray).
#' See details.
#'
#' @param log [logical] (*with default*):
#' whether the logged minimum dose model should be fit to De data.
#'
#' @param par [numeric] (*with default*):
#' number of parameters in the minimum age model, either 3 (default) or 4.
#'
#' @param bootstrap [logical] (*with default*):
#' apply the recycled bootstrap approach of Cunningham & Wallinga (2012).
#'
#' @param init.values [numeric] (*optional*):
#' a named list with starting values for `gamma`, `sigma`, `p0` and `mu`
#' (e.g. `list(gamma=100, sigma=1.5, p0=0.1, mu=100)`). If no values are
#' provided, reasonable values will be estimated from the data.
#' **NOTE**: the initial values must always be given in the absolute units.
#' If a logged model is applied (`log = TRUE`), the provided `init.values`
#' are automatically log-transformed.
#'
#' @param level [logical] (*with default*):
#' the confidence level required (defaults to 0.95).
#'
#' @param log.output [logical] (*with default*):
#' if `TRUE` and `log = TRUE`, the console output will also show the logged
#' values of the final parameter estimates and confidence intervals.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param multicore [logical] (*with default*):
#' parallelize the computation of the bootstrap by creating a multicore cluster
#' (only considered if `bootstrap = TRUE`). By default, it uses all available
#' logical CPU cores, but this can be changed with option `cores`. Note that
#' this option is highly experimental and may not work on all machines.
#'
#' @param ... (*optional*) further arguments for bootstrapping
#' (`bs.M`, `bs.N`, `bs.h`, `sigmab.sd`). See details for their usage.
#' Further arguments are
#' - `verbose`: enable/disable output to the terminal
#' - `debug`: enable/disable extended console output
#' - `cores`: number of cores to be used when `multicore=TRUE`
#'
#' @return Returns a plot (*optional*) and terminal output. In addition an
#' [Luminescence::RLum.Results-class] object is returned containing the
#' following elements:
#'
#' \item{$summary}{[data.frame] summary of all relevant model results.}
#' \item{$data}{[data.frame] original input data}
#' \item{$args}{[list] used arguments}
#' \item{$call}{[call] the function call}
#' \item{$mle}{[bbmle::mle2] object containing the maximum log likelihood functions for all parameters}
#' \item{$BIC}{[numeric] BIC score}
#' \item{$confint}{[data.frame] confidence intervals for all parameters}
#' \item{$profile}{[stats::profile] the log likelihood profiles}
#' \item{$bootstrap}{[list] bootstrap results}
#'
#' The output should be accessed using the function [Luminescence::get_RLum].
#'
#' @note
#' The default starting values for *gamma*, *mu*, *sigma*
#' and *p0* may only be appropriate for some De data sets and may need to
#' be changed for other data. This is especially true when the un-logged
#' version is applied. \cr
#' Also note that all R warning messages are suppressed
#' when running this function. If the results seem odd consider re-running the
#' model with `debug=TRUE` which provides extended console output and
#' forwards all internal warning messages.
#'
#' @section Function version: 0.4.6
#'
#' @author
#' Christoph Burow, University of Cologne (Germany) \cr
#' Based on a rewritten S script of Rex Galbraith, 2010 \cr
#' The bootstrap approach is based on a rewritten MATLAB script of Alastair Cunningham. \cr
#' Alastair Cunningham is thanked for his help in implementing and cross-checking the code.
#'
#' @seealso [Luminescence::calc_CentralDose], [Luminescence::calc_CommonDose], [Luminescence::calc_FiniteMixture],
#' [Luminescence::calc_FuchsLang2001], [Luminescence::calc_MaxDose]
#'
#' @references
#' Arnold, L.J., Roberts, R.G., Galbraith, R.F. & DeLong, S.B.,
#' 2009. A revised burial dose estimation procedure for optical dating of young
#' and modern-age sediments. Quaternary Geochronology 4, 306-325.
#'
#' Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission
#' track ages. Nuclear Tracks Radiation Measurements 4, 459-470.
#'
#' Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley, J.M.,
#' 1999. Optical dating of single grains of quartz from Jinmium rock shelter,
#' northern Australia. Part I: experimental design and statistical models.
#' Archaeometry 41, 339-364.
#'
#' Galbraith, R.F., 2005. Statistics for
#' Fission Track Analysis, Chapman & Hall/CRC, Boca Raton.
#'
#' Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose and error
#' calculation and display in OSL dating: An overview and some recommendations.
#' Quaternary Geochronology 11, 1-27.
#'
#' Olley, J.M., Roberts, R.G., Yoshida, H., Bowler, J.M., 2006. Single-grain optical dating of grave-infill
#' associated with human burials at Lake Mungo, Australia. Quaternary Science
#' Reviews 25, 2469-2474.
#'
#' **Further reading**
#'
#' Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain equivalent dose
#' (De) distributions: Implications for OSL dating of sediment mixtures.
#' Quaternary Geochronology 4, 204-230.
#'
#' Bolker, B., 2016. Maximum likelihood estimation analysis with the bbmle package.
#' In: Bolker, B., R Development Core Team, 2016. bbmle: Tools for General Maximum Likelihood Estimation.
#' R package version 1.0.18. [https://CRAN.R-project.org/package=bbmle]()
#'
#' Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain quartz De distributions and an
#' assessment of procedures for estimating burial dose. Quaternary Science
#' Reviews 25, 2475-2502.
#'
#' Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial archives using robust OSL chronologies.
#' Quaternary Geochronology 12, 98-106.
#'
#' Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the reproducibility and accuracy
#' of optical dating of fluvial deposits.  Quaternary Geochronology 1, 109-120.
#'
#' Rodnight, H., 2008. How many equivalent dose values are needed to
#' obtain a reproducible distribution?. Ancient TL 26, 3-10.
#'
#' @examples
#'
#' ## Load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' # (1) Apply the minimum age model with minimum required parameters.
#' # By default, this will apply the un-logged 3-parameter MAM.
#' calc_MinDose(data = ExampleData.DeValues$CA1, sigmab = 0.1)
#'
#' \dontrun{
#' # (2) Re-run the model, but save results to a variable and turn
#' # plotting of the log-likelihood profiles off.
#' mam <- calc_MinDose(
#'  data = ExampleData.DeValues$CA1,
#'  sigmab = 0.1,
#'  plot = FALSE)
#'
#' # Show structure of the RLum.Results object
#' mam
#'
#' # Show summary table that contains the most relevant results
#' res <- get_RLum(mam, "summary")
#' res
#'
#' # Plot the log likelihood profiles retroactively, because before
#' # we set plot = FALSE
#' plot_RLum(mam)
#'
#' # Plot the dose distribution in an abanico plot and draw a line
#' # at the minimum dose estimate
#' plot_AbanicoPlot(data = ExampleData.DeValues$CA1,
#'                  main = "3-parameter Minimum Age Model",
#'                  line = mam,polygon.col = "none",
#'                  hist = TRUE,
#'                  rug = TRUE,
#'                  summary = c("n", "mean", "mean.weighted", "median", "in.ci"),
#'                  centrality = res$de,
#'                  line.col = "red",
#'                  grid.col = "none",
#'                  line.label = paste0(round(res$de, 1), "\U00B1",
#'                                      round(res$de_err, 1), " Gy"),
#'                  bw = 0.1,
#'                  ylim = c(-25, 18),
#'                  summary.pos = "topleft",
#'                  mtext = bquote("Parameters: " ~
#'                                   sigma[b] == .(get_RLum(mam, "args")$sigmab) ~ ", " ~
#'                                   gamma == .(round(log(res$de), 1)) ~ ", " ~
#'                                   sigma == .(round(res$sig, 1)) ~ ", " ~
#'                                   rho == .(round(res$p0, 2))))
#'
#'
#'
#' # (3) Run the minimum age model with bootstrap
#' # NOTE: Bootstrapping is computationally intensive
#' # (3.1) run the minimum age model with default values for bootstrapping
#' calc_MinDose(data = ExampleData.DeValues$CA1,
#'              sigmab = 0.15,
#'              bootstrap = TRUE)
#'
#' # (3.2) Bootstrap control parameters
#' mam <- calc_MinDose(data = ExampleData.DeValues$CA1,
#'                     sigmab = 0.15,
#'                     bootstrap = TRUE,
#'                     bs.M = 300,
#'                     bs.N = 500,
#'                     bs.h = 4,
#'                     sigmab.sd = 0.06,
#'                     plot = FALSE)
#'
#' # Plot the results
#' plot_RLum(mam)
#'
#' # save bootstrap results in a separate variable
#' bs <- get_RLum(mam, "bootstrap")
#'
#' # show structure of the bootstrap results
#' str(bs, max.level = 2, give.attr = FALSE)
#'
#' # print summary of minimum dose and likelihood pairs
#' summary(bs$pairs$gamma)
#'
#' # Show polynomial fits of the bootstrap pairs
#' bs$poly.fits$poly.three
#'
#' # Plot various statistics of the fit using the generic plot() function
#' par(mfcol=c(2,2))
#' plot(bs$poly.fits$poly.three, ask = FALSE)
#'
#' # Show the fitted values of the polynomials
#' summary(bs$poly.fits$poly.three$fitted.values)
#' }
#'
#' @export
calc_MinDose <- function(
  data,
  sigmab,
  log = TRUE,
  par = 3,
  bootstrap = FALSE,
  init.values = NULL,
  level = 0.95,
  log.output = FALSE,
  plot = TRUE,
  multicore = FALSE,
  ...
) {
  .set_function_name("calc_MinDose")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------
  .validate_class(data, c("data.frame", "RLum.Results"))
  .validate_not_empty(data)
  if (inherits(data, "RLum.Results")) {
    data <- get_RLum(data, "data")
  }
  if (ncol(data) < 2) {
    .throw_error("'data' should have 2 columns")
  }

  if (!all(stats::complete.cases(data))) {
    .throw_message("Warning: Input data contained NA/NaN values, ",
                   "which were removed prior to calculations", error = FALSE)
    data <- data[stats::complete.cases(data), ]
    if (nrow(data) == 0)
      .throw_error("After NA removal, nothing is left from the data set")
  }

  .validate_positive_scalar(sigmab)
  if (!is.null(init.values)) {
    if (!is.list(init.values)) {
      .throw_error("'init.values' is expected to be a named list")
    }
    exp.names <- c("gamma", "sigma", "p0", "mu")
    mis.names <- setdiff(exp.names, names(init.values))
    if (length(init.values) < length(exp.names) || length(mis.names) > 0) {
      .throw_error("Please provide initial values for all model parameters. ",
                   "\nMissing parameters: ",
                   toString(mis.names))
    }
  }

  ## par can only be 3 or 4
  .validate_positive_scalar(par, int = TRUE)
  if (!par %in% c(3, 4)) {
    .throw_error("'par' can only be set to 3 or 4")
  }

  .validate_logical_scalar(log)
  .validate_logical_scalar(bootstrap)
  .validate_logical_scalar(log.output)
  .validate_logical_scalar(plot)
  .validate_logical_scalar(multicore)

  ##============================================================================##
  ## ... ARGUMENTS
  ##============================================================================##

  extraArgs <- list(...)

  ## check if this function is called by calc_MaxDose()
  if ("invert" %in% names(extraArgs)) {
    invert <- .validate_logical_scalar(extraArgs$invert, name = "'invert'")
    if (invert && !log) {
      log <- TRUE # overwrite user choice as max dose model currently only supports the logged version
      .throw_warning("The maximum dose model only supports the logged version, ",
                     "'log' reset to TRUE\n")
    }
  } else {
    invert <- FALSE
  }

  ## console output
  verbose <- extraArgs$verbose %||% TRUE
  .validate_logical_scalar(verbose)

  ## bootstrap replications
  # first level bootstrap
  if ("bs.M" %in% names(extraArgs)) {
    M <- .validate_positive_scalar(as.integer(extraArgs$bs.M),
                                   int = TRUE, name = "'bs.M'")
    M <- max(M, 2) # issue 900
  } else {
    M <- 1000
  }

  # second level bootstrap
  if ("bs.N" %in% names(extraArgs)) {
    N <- .validate_positive_scalar(as.integer(extraArgs$bs.N),
                                   int = TRUE, name= "'bs.N'")
  } else {
    N <- 3*M
  }

  # KDE bandwith
  if ("bs.h" %in% names(extraArgs)) {
    h <- .validate_positive_scalar(extraArgs$bs.h, name = "'bs.h'")
  } else {
    h <- sd(data[, 1]) / sqrt(nrow(data)) * 2
  }

  # standard deviation of sigmab
  if ("sigmab.sd" %in% names(extraArgs)) {
    sigmab.sd <- .validate_positive_scalar(extraArgs$sigmab.sd,
                                           name = "'sigmab.sd'")
  } else {
    sigmab.sd <- 0.04
  }

  if ("debug" %in% names(extraArgs)) {
    debug <- .validate_logical_scalar(extraArgs$debug, name = "'debug'")
  } else {
    debug <- FALSE
  }

  if ("cores" %in% names(extraArgs)) {
    cores <- .validate_positive_scalar(extraArgs$cores,
                                       int = TRUE, name = "'cores'")
  } else {
    cores <- parallel::detectCores()
    if (multicore)
      message("Logical CPU cores detected: ", cores) # nocov
  }

  ## remove non-positive values if using log-transformation
  if (log) {
    nonpos <- data[, 1] <= 0
    if (sum(nonpos) > 0) {
      .throw_warning("De values must be positive with 'log = TRUE', ",
                     sum(nonpos), " values set to NA")
      data[nonpos, 1] <- NA
    }
  }


  ##============================================================================##
  ## START VALUES
  ##============================================================================##

  if (is.null(init.values)) {
    start <- list(gamma = ifelse(log, log(quantile(data[ ,1], probs = 0.25, na.rm = TRUE)),
                                 quantile(data[ ,1], probs = 0.25, na.rm = TRUE)),
                  sigma = 1.2,
                  p0 = 0.01,
                  mu = ifelse(log, log(quantile(data[ ,1], probs = 0.25, na.rm = TRUE)),
                              mean(data[ ,1])))
  } else {
    start <- list(gamma = ifelse(log, log(init.values$gamma), init.values$gamma),
                  sigma = ifelse(log, log(init.values$sigma), init.values$sigma),
                  p0 = init.values$p0,
                  mu = ifelse(log, log(init.values$mu), init.values$mu))
  }

  ##============================================================================##
  ## ESTIMATE BOUNDARY PARAMETERS
  ##============================================================================##

  boundaries <- list(
    # gamma.lower = min(data[ ,1]/10),
    # gamma.upper = max(data[ ,1]*1.1),
    # sigma.lower = 0,
    # sigma.upper = 5,
    # mu.lower = min(data[ ,1])/10,
    # mu.upper = max(data[ ,1]*1.1)
    gamma.lower = -Inf,
    gamma.upper = Inf,

    sigma.lower = 0,
    sigma.upper = Inf,

    p0.lower = 0,
    p0.upper = 1,

    mu.lower = -Inf,
    mu.upper = Inf
  )

  boundaries <- modifyList(boundaries, list(...))

  ##============================================================================##
  ## AUXILIARY FUNCTIONS
  ##============================================================================##

  # THIS FUNCTION CALCULATES THE NEGATIVE LOG LIKELIHOOD OF THE DATA
  Neglik_f <- function(gamma, sigma, p0, mu, data) {
    # this calculates the negative of the log likelihood of the
    # data (data) for a given set of parameters (gamma, sigma, p0)
    # data is a 2x2 matrix of data: De, rel_error (including sigma_b)

    # recover the data
    zi <- data[ ,1]
    si <- data[ ,2]

    # in the MAM-3 gamma and mu are assumed to be equal
    if (par == 3)
      mu <- gamma

    # calculate sigma^2 + seld^2, mu0 and sigma0
    s2 <- sigma^2 + si^2
    sigma0 <- 1/sqrt(1/sigma^2 + 1/si^2)
    mu0 <- (mu/sigma^2 + zi/si^2)/(1/sigma^2 + 1/si^2)

    # calculate the log-likelihood
    logsqrt2pi <- 0.5*log(2*pi)
    res0 <- (gamma - mu0)/sigma0
    res1 <- (gamma - mu)/sigma
    lf1i <- log(p0) - log(si) - 0.5*((zi-gamma)/si)^2   - logsqrt2pi
    lf2i <- log(1-p0) - 0.5*log(s2) - 0.5*(zi-mu)^2/s2  - logsqrt2pi
    lf2i <- lf2i + log(1 - stats::pnorm(res0)) - log(1 - stats::pnorm(res1))
    llik <- log( exp(lf1i) + exp(lf2i) )
    negll <- -sum(llik)

    return(negll)
  }

  # THIS MAXIMIZES THE Neglik_f LIKELIHOOD FUNCTION AND RETURNS AN MLE OBJECT
  Get_mle <- function(data) {
    tryCatch({
      suppressWarnings(
        mle <- bbmle::mle2(data = list(data = data),
                           optimizer = "nlminb",
                           lower=c(gamma = boundaries$gamma.lower,
                                   sigma = boundaries$sigma.lower,
                                   p0 = boundaries$p0.lower,
                                   mu = boundaries$mu.lower),
                           upper=c(gamma = boundaries$gamma.upper,
                                   sigma = boundaries$sigma.upper,
                                   p0 = boundaries$p0.upper,
                                   mu = boundaries$mu.upper),
                           minuslogl = Neglik_f,
                           control = list(iter.max = 1000L),
                           start = start)
      )
    }, error = function(e) {
      .throw_error("Maximizing the negative log-likelihood failed: ", e$message) # nocov
    })
    return(mle)
  }

  ##============================================================================##
  ## MAIN PROGRAM
  ##============================================================================##

  # combine errors
  if (log) {
    lcd <- log(data[, 1]) * ifelse(invert, -1, 1)
    if (invert) {
      x.offset <- abs(min(lcd, na.rm = TRUE))
      lcd <- lcd+x.offset
    }
    lse <- sqrt((data[ ,2]/data[ ,1])^2 + sigmab^2)
  } else {
    lcd <- data[ ,1]
    lse <- sqrt(data[ ,2]^2 + sigmab^2)
  }

  # create new data frame with DE and combined relative error
  dat <- cbind(lcd, lse)

  ## remove NAs which may be present if we had non-positive De values with
  ## log = TRUE
  dat <- dat[!is.na(dat[, 1]), ]

  # get the maximum likelihood estimate
  ests <- Get_mle(dat)

  # check if any standard errors are NA or NaN
  coef_err <- suppressWarnings(
    t(as.data.frame(bbmle::summary(ests)@coef[ ,2]))
  )

  if (debug)
    print(suppressWarnings(bbmle::summary(ests)))

  ## deal with NA/NaN values
  if (anyNA(coef_err))
    coef_err[which(is.na(coef_err))] <- t(as.data.frame(ests@coef))[which(is.na(coef_err))] / 100

  which <- c("gamma", "sigma", "p0", if (par == 4) "mu")
  prof.lower <- c(gamma = -Inf, sigma = 0, p0 = 0, mu = -Inf)
  prof.upper <- c(gamma = Inf, sigma = Inf, p0 = 1, mu = Inf)

  # calculate profile log likelihoods
  prof <- suppressWarnings(
    bbmle::profile(ests,
                   which = which,
                   std.err = as.vector(coef_err),
                   #try_harder = TRUE,
                   quietly = TRUE,
                   tol.newmin = Inf,
                   skiperrs = TRUE,
                   prof.lower = prof.lower,
                   prof.upper = prof.upper)
  )
  # Fallback when profile() returns a 'better' fit
  maxsteps <- 100
  cnt <- 1
  while (!inherits(prof, "profile.mle2")) {
    if (maxsteps == 0L)
      .throw_error("Couldn't find a converging fit for the profile log-likelihood")
    if (verbose)
      message("## Trying to find a better fit (", cnt, "/10) ##")
    prof <- suppressWarnings(
      bbmle::profile(ests,
                     which = which,
                     std.err = as.vector(coef_err),
                     # try_harder = TRUE,
                     quietly = TRUE,
                     maxsteps = maxsteps,
                     tol.newmin = Inf,
                     skiperrs = TRUE,
                     prof.lower = prof.lower,
                     prof.upper = prof.upper)
    )
    maxsteps <- maxsteps - 10
    cnt <- cnt + 1
  }

  ## delete rows where z = -Inf/Inf or NaN
  prof@profile$gamma <- prof@profile$gamma[is.finite(prof@profile$gamma$z), ]
  prof@profile$sigma <- prof@profile$sigma[is.finite(prof@profile$sigma$z), ]
  prof@profile$p0 <- prof@profile$p0[is.finite(prof@profile$p0$z), ]
  if (par == 4) {
    prof@profile$mu <- prof@profile$mu[is.finite(prof@profile$mu$z), ]
  }

  # calculate Bayesian Information Criterion (BIC)
  BIC <- BIC(ests)

  # retrieve results from mle2-object
  pal <- if (log) {
    if (invert) {
      exp((bbmle::coef(ests)[["gamma"]]-x.offset)*-1)
    } else {
      exp(bbmle::coef(ests)[["gamma"]])
    }
  } else {
    bbmle::coef(ests)[["gamma"]]
  }
  sig <- bbmle::coef(ests)[["sigma"]]
  p0end <- bbmle::coef(ests)[["p0"]]

  if (par == 4) {
    muend <- ifelse(log, exp(bbmle::coef(ests)[["mu"]]), bbmle::coef(ests)[["mu"]])
  } else {
    muend <- NA
  }

  ##============================================================================##
  ## ERROR CALCULATION

  #### METHOD 1: follow the instructions of Galbraith & Roberts (2012) ####
  # "If the likelihood profile is symmetrical about the parameter, an approximate standard error
  #  can be calculated by dividing the length of this interval by 3.92"
  conf <- suppressWarnings(
    as.data.frame(bbmle::confint(prof, tol.newmin = Inf, quietly = TRUE, level = level))
  )
  class(conf[,1]) <- class(conf[,2]) <- "numeric"

  if (invert) {
    conf[1, ] <- (conf[1, ]-x.offset)*-1
    t <- conf[1,1]
    conf[1,1] <- conf[1,2]
    conf[1,2] <- t
  }

  ## keep a copy of the original conf object to be returned at the end
  conf.orig <- conf
  if (log) {
    logged.rows <- row.names(conf) != "p0"
    conf[logged.rows, ] <- exp(conf[logged.rows, ])
  }

  gamma_err <- (conf["gamma", 2] - conf["gamma", 1]) / 3.92


  ##============================================================================##
  ## AGGREGATE RESULTS
  summary <- data.frame(de=pal,
                        de_err=gamma_err,
                        ci_level = level,
                        ci_lower = conf["gamma", 1],
                        ci_upper = conf["gamma", 2],
                        par=par,
                        sig=ifelse(log, exp(sig), sig),
                        p0=p0end,
                        mu=muend,
                        Lmax=-ests@min,
                        BIC=BIC)
  call <- sys.call()
  args <- list(log=log, sigmab=sigmab, par = par, bootstrap=bootstrap,
               init.values=start, log.output = log.output,
               bs.M=M, bs.N=N, bs.h=h, sigmab.sd=sigmab.sd)

  if (!is.na(summary$mu) && !is.na(summary$de) &&
    ## equivalent to log(summary$de) > summary$mu, but also valid if de < 0
    summary$de > exp(summary$mu)) {
      .throw_warning("Gamma is larger than mu, consider running the model ",
                     "with new boundary values (see details '?calc_MinDose')")
  }

  ##============================================================================##
  ## BOOTSTRAP
  ##============================================================================##
  if (bootstrap) {

    ## BOOTSTRAP FUNCTIONS ----
    # Function that draws N+M sets of integer values from 1:n and returns
    # both the indices and frequencies
    draw_Freq <- function() {
      f <- R <- matrix(0L, N+M, n)
      for (i in seq_len(N+M)) {
        R[i, ] <- sample(x = n, size = n, replace = TRUE)
        f[i, ] <- tabulate(R, n)
      }
      return(list(R = R, freq = f))
    }

    # Function that adds the additional error sigmab to each individual DE error
    combine_Errors <- function(d, e) {
      if (log) {
        d[ ,2] <- sqrt((d[ ,2]/d[ ,1])^2 + e^2)
        d[ ,1] <- log(d[ ,1])
      } else {
        d[ ,2] <- sqrt(d[ ,2]^2 + e^2)
      }
      return(d)
    }

    # Function that produces N+M replicates from the original data set using
    # randomly sampled indices with replacement and adding a randomly drawn
    # sigmab error
    create_Replicates <- function(f, s) {
      d <- apply(f$R, 1, function(x) data[x, ])
      r <- mapply(function(x, y) combine_Errors(x, y), d, s, SIMPLIFY = FALSE)
      return(r)
    }

    # Function to extract the estimate of gamma from mle2 objects and converting
    # it back to the 'normal' scale
    save_Gamma <- function(d) {
      m <- bbmle::coef(d)[["gamma"]]
      if (log && invert) {
        m <- exp(-(m - x.offset))
      }
      return(m)
    }

    # Function that takes each of the N replicates and produces a kernel density
    # estimate of length n.
    get_KDE <- function(d) {
      f <- approx(density(x = d[, 1], kernel = "gaussian", bw = h, na.rm = TRUE),
                  xout = d[, 1])
      pStarTheta <- as.vector(f$y / sum(f$y))
      pStarTheta / (1 / n)
    }

    # Function that calculates the product term of the recycled bootstrap
    get_ProductTerm <- function(Pmat, b2Pmatrix) {
      prodterm <- apply(Pmat^b2Pmatrix$freq[1:N, ], 1, prod)
      return(prodterm)
    }

    # Function that calculates the pseudo likelihoods for M replicates and
    # returns the dose-likelihood pairs
    make_Pairs <- function(theta, b2mam, prodterm) {
      kdthis <- vapply(theta, function(x) (x - b2mam) / h, numeric(N))
      kd1 <- dnorm(kdthis)
      kd2 <- kd1 * prodterm
      kd <- colSums(kd2, na.rm = TRUE)
      likelihood <- kd / (N * h)
      cbind(theta, likelihood)
    }

    ## START BOOTSTRAP ----
    msg <- sprintf(paste("\n [calc_MinDose] \n\nRecycled Bootstrap",
                         "\n\nParameters:",
                         "\n M = %d",
                         "\n N = %d",
                         "\n sigmab = %.2f \U00B1 %.2f",
                         "\n h = %.2f",
                         "\n\nCreating %d bootstrap replicates%s..."),
                   M, N, sigmab, sigmab.sd, h, N+M,
                   if (multicore) paste(" using", cores, "cores") else "")
    if (verbose)
      message(msg)

    n <- length(data[ ,1])
    # Draw N+M samples from a normally distributed sigmab
    sigmab <- rnorm(N + M, sigmab, sigmab.sd)
    # Draw N+M random indices and their frequencies
    b2Pmatrix <- draw_Freq()
    # Finally draw N+M bootstrap replicates
    replicates <- create_Replicates(b2Pmatrix, sigmab)

    # MULTICORE: The call to 'Get_mle' is the bottleneck of the function.
    # Using multiple CPU cores can reduce the computation cost, but may
    # not work for all machines.
    if (verbose)
      message("Applying the model to all replicates, this may take a while...")
    if (multicore) {
      cl <- parallel::makeCluster(cores)
      mle <- parallel::parLapply(cl, replicates, Get_mle)
      parallel::stopCluster(cl)
    } else {
      mle <- lapply(replicates, Get_mle)
    }

    # Final bootstrap calculations
    if (verbose)
      message("Calculating likelihoods...")
    # Save 2nd- and 1st-level bootstrap results (i.e. estimates of gamma)
    b2mam <- sapply(mle[1:N], save_Gamma)
    theta <- sapply(mle[c(N+1):c(N+M)], save_Gamma)
    # Calculate the probability/pseudo-likelihood
    Pmat <- lapply(replicates[c(N+1):c(N+M)], get_KDE)
    prodterm <- vapply(Pmat, get_ProductTerm, b2Pmatrix, FUN.VALUE = numeric(N))
    # Save the bootstrap results as dose-likelihood pairs
    pairs <- make_Pairs(theta, b2mam, prodterm)

    ## --------- FIT POLYNOMIALS -------------- ##
    if (verbose)
      message("Fitting curves to dose-likelihood pairs...")
    # polynomial fits of increasing degrees

    ## if the input values are too close to zero, we may get
    ## Inf values >>> we remove them here with a warning
    is.inf <- is.infinite(pairs[, 2])
    if (any(is.inf)) {
      # nocov start
      .throw_warning("Inf values produced by bootstrapping removed for loess ",
                     "fitting (", round(sum(is.inf) / nrow(pairs) * 100, 2),
                     "% of the total dataset). This message usually indicates ",
                     "that your values are close to 0.")
      pairs <- pairs[!is.inf, ]
      # nocov end
    }

    poly.three <- stats::lm(pairs[, 2] ~ poly(pairs[, 1], degree = 3, raw = TRUE))
    poly.four <- stats::lm(pairs[, 2] ~ poly(pairs[, 1], degree = 4, raw = TRUE))
    poly.five <- stats::lm(pairs[, 2] ~ poly(pairs[, 1], degree = 5, raw = TRUE))
    poly.six <- stats::lm(pairs[, 2] ~ poly(pairs[, 1], degree = 6, raw = TRUE))

    ## --------- FIT LOESS -------------- ##
    # Polynomials are probably not reasonable and often suffer badly from
    # overfitting, especially towards the margins of the fitted data. In this
    # particular use case polynomials may suggest a multimodal likelihood
    # distribution where actually none is given. The non-parametric
    # LOESS (LOcal polynomial regrESSion) often yields better results than
    # standard polynomials.
    if (nrow(pairs) >= 7) {
      loess <- loess(pairs[, 2] ~ pairs[, 1])
    } else {
      loess <- NA
      .throw_warning("Not enough bootstrap replicates for loess fitting, try ",
                     "increasing `bs.M`")
    }

  }#EndOf::Bootstrap

  ##============================================================================##
  ## CONSOLE PRINT
  ##============================================================================##
  if (verbose && !bootstrap) {
      cat("\n----------- meta data -----------\n")
      print(data.frame(n=length(data[ ,1]),
                       par=par,
                       sigmab=sigmab,
                       logged=log,
                       Lmax=-ests@min,
                       BIC=BIC,
                       row.names = ""))

      cat("\n--- final parameter estimates ---\n")
      tmp <- round(data.frame(
        gamma=ifelse(!invert,
                     ifelse(log, exp(bbmle::coef(ests)[["gamma"]]), bbmle::coef(ests)[["gamma"]]),
                     ifelse(log, exp((bbmle::coef(ests)[["gamma"]]-x.offset)*-1),(bbmle::coef(ests)[["gamma"]]-x.offset)*-1)
        ),
        sigma=ifelse(log, exp(bbmle::coef(ests)[["sigma"]]), bbmle::coef(ests)[["sigma"]]),
        p0=bbmle::coef(ests)[["p0"]],
        mu=ifelse(par==4,
                  muend,
                  0),
        row.names="", check.names = FALSE), 2)

      if (log && log.output) {
        tmp$`log(gamma)` <- round(log(tmp$gamma),2)
        tmp$`log(sigma)` <- round(log(tmp$sigma),2)
        if (par == 4)
          tmp$`log(mu)` <- round(log(tmp$mu),2)
      }

      print(tmp)

      cat("\n------ confidence intervals -----\n")
      conf_print <- round(conf, 2)
      if (log && log.output) {
          conf_tmp <- round(conf.orig, 2)
          conf_tmp[which(rownames(conf_tmp) == "p0"), ] <- "-"
          conf_print <- cbind(conf_print,
                              setNames(conf_tmp, names(conf_tmp)))
          conf_print <- rbind(
            setNames(data.frame("", "", "(logged)", "(logged)", row.names = "", stringsAsFactors = FALSE), names(conf_print)),
            conf_print)
      }
      print(conf_print)

      cat("\n------ De (asymmetric error) -----\n")
      print(round(data.frame(De=pal,
                             lower = conf["gamma", 1],
                             upper = conf["gamma", 2],
                             row.names=""), 2))

      cat("\n------ De (symmetric error) -----\n")
      print(round(data.frame(De=pal,
                             error=gamma_err,
                             row.names=""), 2))
  }

  ##============================================================================##
  ## RETURN VALUES
  ##============================================================================##

  if (invert)
    prof@profile$gamma$par.vals[ ,"gamma"] <- rev((prof@profile$gamma$par.vals[ ,"gamma"] - x.offset)*-1)

  if (!bootstrap)
    pairs <- poly.three <- poly.four <- poly.five <- poly.six <- loess <- NULL

  results <- set_RLum(
    class = "RLum.Results",
    originator = "calc_MinDose",
    data = list(summary = summary,
                data = data,
                args = args,
                call = call,
                mle = ests,
                BIC = BIC,
                confint = conf.orig,
                profile = prof,
                bootstrap = list(
                  pairs = list(gamma=pairs),
                  poly.fits = list(poly.three = poly.three,
                                   poly.four = poly.four,
                                   poly.five = poly.five,
                                   poly.six = poly.six),
                  loess.fit = loess)))

  ##=========##
  ## PLOTTING
  if (plot)
    try(plot_RLum.Results(results, ...))

  results
}

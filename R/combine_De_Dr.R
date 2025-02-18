#'@title Estimate Individual Age using Bayesian Inference
#'
#'@description A Bayesian robust estimation of central age from equivalent dose
#' measurements under the assumption that the dose rate is modelled by a
#' finite Gaussian mixture model.
#'
#'@param theta [numeric] (**required**): the weight vector of the Gaussian mixture
#'
#'@param mu [numeric] (**required**): is the mean vector of the Gaussian mixture
#'
#'@param sigma [numeric] (**required**): is the standard deviation vector of the Gaussian mixture
#'
#'@param De [numeric] (**required**): the equivalent dose sample
#'
#'@param s [numeric] (**required**): the vector of measurement errors on De.
#'
#'@param sig0 [numeric] (**required**): the prior shrinkage parameter
#'
#'@param Age_range [numeric] (*with default*): the age range to investigate
#'
#'@param method_control [list] (*with default*): parameters passed down
#' to the jags process
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#'@return An [RLum.Results-class] object to be used in [combine_De_Dr]
#'
#'@section Function version: 0.1.0
#'
#'@note The function is intended to be called by [combine_De_Dr], however, for
#' reasons of transparency
#'
#'@author Anne Philippe, Université de Nantes (France),
#' Jean-Michel Galharret, Université de Nantes (France),
#' Norbert Mercier, IRAMAT-CRP2A, Université Bordeaux Montaigne (France),
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@examples
#' n <- 1000
#' sdt <- 0.3
#' Dr <- stats::rlnorm (n, 0, sdt)
#' int_OD <-  0.1
#' tildeDr = Dr * (1 + rnorm(length(Dr), 0, int_OD))
#' De <-  c(50 * sample(Dr, 50, replace = TRUE), 10, 12, 200, 250)
#' k <- length(De)
#' s <- stats::rnorm(k, 10, 2)
#' a <- De / mean(tildeDr)
#' sig_a2 <- a ^ 2 * (s / De) ^ 2
#' sig0 <- sqrt(1 / mean(1 / sig_a2))
#' fit <- mclust::Mclust(tildeDr, model = "V")
#' theta <- fit$parameters$pro
#' mu <- fit$parameters$mean
#' sigma <- sqrt(fit$parameters$variance$sigmasq)
#' Age_range <- c(0, Dr * (1 + rnorm(length(Dr), 0, int_OD[1])))
#' res <- .calc_IndividualAgeModel(theta, mu, sigma, De, s, sig0, Age_range = Age_range)
#'@md
#'@noRd
.calc_IndividualAgeModel <- function(
  theta,
  mu,
  sigma,
  De,
  s,
  sig0,
  Age_range,
  method_control = list(),
  verbose = TRUE
){

  # Set parameters and models -----------------------------------------------
  nobs <- length(De)
  event1 <- "model{
            for( i in 1 : N ) {
              D_e[i] ~ dnorm(a[i] * mu, tau[i])
              tau[i] <- 1 / (a[i] * sigma) ^ 2
              De[i] ~ dnorm(D_e[i], prec2[i])
              a[i] ~ dnorm(A, prec_a[i])
              u[i] ~ dunif(0,1)
              prec_a[i] <- 1 / s02 * u[i] / (1 - u[i])
              prec2[i] <- 1 / (s2[i])
              sig_a[i] <- 1 / sqrt(prec_a[i])
            }
          A ~ dunif(Amin, Amax)
        }"

  event2 <- "model{
            for( i in 1 : N ) {
              D_e[i] ~ dnorm(a[i] * mu[z[i]], tau[i])
              tau[i] <- 1 / (a[i] * sigma[z[i]]) ^ 2
              z[i] ~ dcat(theta)
              De[i] ~ dnorm(D_e[i], prec2[i])
              a[i] ~ dnorm(A, prec_a[i])
              u[i] ~ dunif(0, 1)
              prec_a[i] <- 1 / s02 * u[i] / (1 - u[i])
              prec2[i] <- 1 / (s2[i])
              sig_a[i] <- 1 / sqrt(prec_a[i])
            }
            A ~ dunif(Amin, Amax)
          }"

  data1 <- list(
    'theta' = theta,
    'mu' = mu,
    'sigma' = sigma,
    'N' = nobs ,
    'De' = De,
    's2' = s ^ 2,
    's02' = sig0[1] ^ 2,
    'Amin' = Age_range[1],
    'Amax' = Age_range[2]
  )

  # Run Bayesian model ------------------------------------------------------
  method_control <- modifyList(
    x = list(
      variable.names = c('A', 'a', 'sig_a'),
      n.chains = 4,
      n.adapt = 1000,
      n.iter = 5000,
      thin = 1,
      progress.bar = if(verbose) "text" else "none",
      quiet = if(verbose) FALSE else TRUE,
      diag = if(verbose) TRUE else FALSE,
      return_mcmc = FALSE
    ),
    val = method_control)

  on.exit(close(model), add = TRUE)
  ## select model
  if(length(theta) == 1) {
    data1$theta <- NULL
    model <- textConnection(event1)

  } else {
    model <- textConnection(event2)

  }

  ## run model
  if(verbose) cat("(1) Running Bayesian modelling 'Individual Age Model' ... ")

  jags <- rjags::jags.model(
    file = model,
    data = data1,
    n.chains = method_control$n.chains,
    n.adapt = method_control$n.adapt,
    quiet = method_control$quiet
  )

  stats::update(
    jags,
    n.iter = method_control$n.iter,
    progress.bar = method_control$progress.bar,
    quiet = method_control$quiet
  )

  samp <-
    rjags::coda.samples(
      model = jags,
      variable.names = method_control$variable.names,
      n.iter = method_control$n.iter,
      thin = method_control$thin,
      progress.bar = method_control$progress.bar
    )

  if(verbose & method_control$quiet) cat("DONE")
  if(method_control$diag) {
    cat("\n[.calc_IndividualAgeModel()]\n")
    print(coda::gelman.diag(samp))

  }

  # Return ------------------------------------------------------------------
  return(set_RLum(
    "RLum.Results",
    data = list(
      A = unlist(samp[, "A"]),
      a = do.call(rbind, samp[, 2:(nobs + 1)]),
      sig_a = do.call(rbind, samp[, (2 + nobs):(2 * nobs + 1)]),
      model = paste(jags$model(), ""),
      mcmc_IAM = if(method_control$return_mcmc) samp else NULL),
    info = list(call = sys.call())
  ))
}


#'@title Central Bayesian Central Age Model
#'
#'@description A Bayesian estimation of central age from equivalent dose measurements
#'under the assumption that the dose rate is modelled by finite Gaussian mixture model.
#'MCMC outputs provide to JAGS program.
#'
#'@param theta [numeric] (**required**): the weight vector of the Gaussian mixture
#'
#'@param mu [numeric] (**required**): is the mean vector of the Gaussian mixture
#'
#'@param sigma [numeric] (**required**): is the standard deviation vector of the Gaussian mixture
#'
#'@param De [numeric] (**required**): the equivalent dose sample
#'
#'@param s [numeric] (**required**): the vector of measurement errors on De.
#'
#'@param Age_range [numeric] (*with default*): the age range to investigate
#'
#'@param method_control [list] (*with default*): parameters passed down to the jags process
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#'@return An [RLum.Results-class] object
#'
#'@section Function version: 0.1.0
#'
#'@author Anne Philippe, Université de Nantes (France),
#'Jean-Michel Galharret, Université de Nantes (France),
#'Norbert Mercier, IRAMAT-CRP2A, Université Bordeaux Montaigne (France),
#'Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@md
#'@noRd
.calc_BayesianCentralAgeModel <- function(
  theta,
  mu,
  sigma,
  De,
  s,
  Age_range,
  method_control = list(),
  verbose = TRUE
){

  # Set models --------------------------------------------------------------
  central_age_model1 <- "
    model{
        for( i in 1:J) {
        D_e[i] ~ dnorm(A*mu,1/(A*sigma)^2)
        De[i] ~ dnorm(D_e[i], prec2[i])
        prec2[i] <- 1/(s2[i])
        }

        A ~ dunif(Amin,Amax)
    }"

  central_age_model2 <- "
    model{
        for( i in 1:J) {
        D_e[i] ~ dnorm(A*mu[z[i]],tau[i])
        tau[i]<-1/(A*sigma[z[i]])^2
        z[i] ~ dcat(theta)
        De[i] ~ dnorm(D_e[i], prec2[i])
        prec2[i] <- 1/(s2[i])
        }

        A ~ dunif(Amin,Amax)
    }"

  # Run Bayesian modelling --------------------------------------------------
  method_control <- modifyList(
    x = list(
      variable.names = c('A', 'D_e'),
      n.chains = 4,
      n.adapt = 1000,
      n.iter = 5000,
      thin = 1,
      progress.bar = if(verbose) "text" else "none",
      quiet = if(verbose) FALSE else TRUE,
      diag = if(verbose) TRUE else FALSE,
      return_mcmc = FALSE
    ),
    val = method_control)

  on.exit(close(model), add = TRUE)
  data <-
    list(
      'theta' = theta,
      'mu' = mu,
      'sigma' = sigma,
      'De' = De,
      'J' =  length(De),
      's2' = s ^ 2,
      'Amin' = Age_range[1],
      'Amax' = Age_range[2]
    )

  ## select model
  if(length(theta) == 1) {
    data$theta <- NULL
    model <- textConnection(central_age_model1)

  } else {
    model <- textConnection(central_age_model2)

  }

  ## run modelling
  if(verbose) cat("\n(2) Running Bayesian modelling 'Bayesian Central Age Model' ... ")

  jags2 <- rjags::jags.model(
    file = model,
    data = data,
    n.chains = method_control$n.chains,
    n.adapt = method_control$n.adapt,
    quiet = method_control$quiet
  )

  stats::update(
    object = jags2,
    n.iter = method_control$n.iter,
    progress.bar = method_control$progress.bar,
    quiet = method_control$quiet
  )

  samp2 <- rjags::coda.samples(
    model = jags2,
    variable.names = method_control$variable.names,
    n.iter = method_control$n.iter,
    thin = method_control$thin,
    progress.bar = method_control$progress.bar
  )

  if(verbose & method_control$quiet) cat("DONE\n")

  if(method_control$diag) {
    cat("\n[.calc_BayesianCentralAgeModel()]\n")
    print(coda::gelman.diag(samp2))

  }

  # Return ------------------------------------------------------------------
  return(set_RLum(
    "RLum.Results",
    data = list(
      A = unlist(samp2[, "A"]),
      D_e = do.call(rbind, samp2[, -1]),
      model = paste(jags2$model(), ""),
      mcmc_BCAM = if(method_control$return_mcmc) samp2 else NULL),
    info = list(call = sys.call())
  ))

}

#'@title Combine Dose Rate and Equivalent Dose Distribution
#'
#'@description A Bayesian statistical analysis of OSL age requiring dose rate sample.
#'Estimation contains a preliminary step for detecting outliers in the equivalent
#'dose sample.
#'
#'@details
#'
#'**Outlier detection**
#'
#'Two different outlier detection methods are implemented (full details are given
#'in the cited literature).
#'
#'1. The *default* and recommend method, uses quantiles to compare prior and
#'posterior distributions of the individual variances of the equivalent doses.
#'If the corresponding quantile in the corresponding posterior distribution is larger
#'than the quantile in the prior distribution, the value is marked
#'as outlier (cf. Galharret et al., preprint)
#'
#'2. The alternative method employs the method suggested by Rousseeuw and Croux (1993)
#'using the absolute median distance.
#'
#'**Parameters available for `method_control`**
#'
#'The parameters listed below are used to granular control Bayesian modelling using
#'[rjags::rjags]. Internally the functions `.calc_IndividualAgeModel()` and
#'`.calc_BayesianCentraAgelModel()`. The parameter settings affect both models.
#'Note: `method_control` expects a **named** list of parameters
#'
#'\tabular{llll}{
#' **PARAMETER** \tab **TYPE** \tab **DEFAULT** \tab **REMARKS** \cr
#' `variable.names_IAM` \tab [character] \tab `c('A', 'a', 'sig_a')` \tab variables names to be monitored in the modelling process using the internal function `.calc_IndividualAgeModel()`\cr
#' `variable.names_BCAM` \tab [character] \tab `c('A', 'D_e')` \tab variables names to be monitored in the modelling process using the internal function `.calc_BayesianCentraAgelModel()`\cr
#' `n.chains` \tab [integer] \tab `4` \tab number of MCMC chains\cr
#' `n.adapt` \tab [integer] \tab `1000` \tab number of iterations for the adaptation\cr
#' `n.iter` \tab [integer] \tab `5000` \tab number of iterations to monitor cf. [rjags::coda.samples]\cr
#' `thin` \tab [numeric] \tab `1` \tab thinning interval for the monitoring cf. [rjags::coda.samples]\cr
#' `diag` \tab [logical] \tab `FALSE` \tab additional terminal convergence diagnostic.
#' `FALSE` if `verbose = FALSE`\cr
#' `progress.bar` \tab [logical] \tab `FALSE` \tab enable/disable progress bar. `FALSE` if `verbose = FALSE`\cr
#' `quiet` \tab [logical] \tab `TRUE` \tab silence terminal output. Set to `TRUE` if `verbose = FALSE`\cr
#' `return_mcmc`\tab [logical] \tab `FALSE` \tab return additional MCMC diagnostic information\cr
#'}
#'
#'@param De [numeric] (**required**): a equivalent dose sample
#'
#'@param s [numeric] (**required**): a vector of measurement errors on the equivalent dose
#'
#'@param Dr [numeric] (**required**): a dose rate sample
#'
#'@param int_OD [numeric] (**required**): the intrinsic overdispersion, typically the standard deviation
#'characterizing a dose-recovery test distribution
#'
#'@param Age_range [numeric] (*with default*): the age range to be investigated by the algorithm, the larger
#'the value the more iterations are needed and the longer it takes. Should not be set too narrow, cut
#'the algorithm some slack.
#'
#'@param outlier_threshold [numeric] (*with default*): the required significance level used
#'for the outlier detection. If set to `1`, no outliers are removed. If
#'`outlier_method = "RousseeuwCroux1993"`, the median distance is used as outlier threshold.
#'Please see details for further information.
#'
#'@param outlier_method [character] (*with default*): select the outlier detection
#'method, either `"default"` or `"RousseeuwCroux1993"`. See details for further information.
#'
#' @param outlier_analysis_plot [logical] (*with default*): enable/disable the
#' outlier analysis plot. Note: the outlier analysis will happen independently
#' of the plot output.
#'
#'@param method_control [list] (*with default*): named [list] of further parameters passed down
#' to the [rjags::rjags] modelling
#'
#'@param par_local [logical] (*with default*): if set to `TRUE` the function uses its
#'own [graphics::par] settings (which will end in two plots next to each other)
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#' @param plot [logical] (*with default*): enable/disable the plot output.
#'
#'@param ... a few further arguments to fine-tune the plot output such as
#'`cdf_ADr_quantiles` (`TRUE`/`FALSE`), `legend.pos`, `legend` (`TRUE`/`FALSE`)
#'
#'@return The function returns a plot if `plot = TRUE` and an [RLum.Results-class]
#'object with the following slots:
#'
#' `@data`\cr
#' `.. $Ages`: a [numeric] vector with the modelled ages to be further analysed or visualised\cr
#' `.. $Ages_stats`: a [data.frame] with sum HPD, CI 68% and CI 95% for the ages \cr
#' `.. $outliers_index`: the index with the detected outliers\cr
#' `.. $cdf_ADr_mean` : empirical cumulative density distribution A * Dr (mean)\cr
#' `.. $cdf_ADr_quantiles` : empirical cumulative density distribution A * Dr (quantiles .025,.975)\cr
#' `.. $cdf_De_no_outlier` : empirical cumulative density distribution of the De with no outliers\cr
#' `.. $cdf_De_initial` : empirical cumulative density distribution of the initial De\cr
#' `.. $mcmc_IAM` : the MCMC list of the Individual Age Model, only of `method_control = list(return_mcmc = TRUE)` otherwise `NULL`\cr
#' `.. $mcmc_BCAM` : the MCMC list of the Bayesian Central Age Model, only of `method_control = list(return_mcmc = TRUE)` otherwise `NULL`\cr
#'
#' `@info`\cr
#' `.. $call`: the original function call\cr
#' `.. $model_IAM`: the BUGS model used to derive the individual age\cr
#' `.. $model_BCAM`: the BUGS model used to calculate the Bayesian Central Age\cr
#'
#'@references
#'
#'Mercier, N., Galharret, J.-M., Tribolo, C., Kreutzer, S., Philippe, A., preprint.
#'Luminescence age calculation through Bayesian convolution of equivalent dose and
#'dose-rate distributions: the De_Dr model. Geochronology, 1-22.
#'
#'Galharret, J-M., Philippe, A., Mercier, N., preprint. Detection of outliers with
#'a Bayesian hierarchical model: application to the single-grain luminescence dating method.
#'Electronic Journal of Applied Statistics
#'
#'**Further reading**
#'
#'Rousseeuw, P.J., Croux, C., 1993. Alternatives to the median absolute deviation.
#'Journal of the American Statistical Association 88, 1273–1283. \doi{10.2307/2291267}
#'
#'Rousseeuw, P.J., Debruyne, M., Engelen, S., Hubert, M., 2006. Robustness and outlier detection in chemometrics.
#'Critical Reviews in Analytical Chemistry 36, 221–242. \doi{10.1080/10408340600969403}
#'
#'@author Anne Philippe, Université de Nantes (France),
#'Jean-Michel Galharret, Université de Nantes (France),
#'Norbert Mercier, IRAMAT-CRP2A, Université Bordeaux Montaigne (France),
#'Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@seealso [plot_OSLAgeSummary], [rjags::rjags], [mclust::mclust-package]
#'
#'@section Function version: 0.1.0
#'
#'@keywords dplot distribution datagen
#'
#'@examples
#'## set parameters
#' Dr <- stats::rlnorm (1000, 0, 0.3)
#' De <- 50*sample(Dr, 50, replace = TRUE)
#' s <- stats::rnorm(50, 10, 2)
#'
#'## run modelling
#'## note: modify parameters for more realistic results
#'\dontrun{
#'results <- combine_De_Dr(
#'  Dr = Dr,
#'  int_OD = 0.1,
#'  De,
#'  s,
#'  Age_range = c(0,100),
#'  method_control = list(
#'   n.iter = 100,
#'   n.chains = 1))
#'
#'## show models used
#'writeLines(results@info$model_IAM)
#'writeLines(results@info$model_BCAM)
#'}
#'
#'@md
#'@export
combine_De_Dr <- function(
  De,
  s,
  Dr,
  int_OD,
  Age_range = c(1,300),
  outlier_threshold = .05,
  outlier_method = "default",
  outlier_analysis_plot = FALSE,
  method_control = list(),
  par_local = TRUE,
  verbose = TRUE,
  plot = TRUE,
  ...
) {
  .set_function_name("combine_De_Dr")
  on.exit(.unset_function_name(), add = TRUE)

  ## check package availability ---------------------------------------------
  .require_suggested_package("rjags")
  .require_suggested_package("coda")
  .require_suggested_package("mclust")

  ## Integrity checks -------------------------------------------------------

  if (length(De) != length(s))
    .throw_error("'De' and 's' should have the same length")
  .validate_logical_scalar(verbose)
  .validate_logical_scalar(plot)

# Prepare data ------------------------------------------------------------
  ## we have to fetch the function otherwise
  ## we would need it in import instead of suggests
  mclustBIC <- mclust::mclustBIC

  ##  Estimation of the rate dose Dr1 by a Gaussian Mixture Model
  tildeDr <- Dr * (1 + rnorm(length(Dr), 0, int_OD[1]))
  fit <-
    mclust::Mclust(data = tildeDr,
                   modelNames = "V",
                   verbose = FALSE)

  theta <- fit$parameters$pro
  mu <- fit$parameters$mean
  sigma <- sqrt(fit$parameters$variance$sigmasq)
  a <- De / mean(tildeDr)
  sig_a2 <- a ^ 2 * (s / De) ^ 2
  sig0 <- sqrt(1 / mean(1 / sig_a2))

# Set parameters ----------------------------------------------------------
  method_control <- modifyList(
    x = list(
      variable.names_IAM = c('A', 'a', 'sig_a'),
      variable.names_BCAM = c('A', 'D_e'),
      n.chains = 4,
      n.adapt = 1000,
      n.iter = 5000,
      thin = 1,
      progress.bar = "none",
      quiet = TRUE,
      diag = FALSE,
      return_mcmc = FALSE
    ),
    val = method_control)

# Bayesian Modelling IAM --------------------------------------------------
if(verbose)  cat("\n[combine_De_Dr()]\n")

fit_IAM <- .calc_IndividualAgeModel(
      theta = theta,
      mu = mu,
      sigma = sigma,
      De = De,
      s = s,
      sig0 = sig0,
      Age_range = Age_range[1:2],
      verbose = verbose,
      method_control = list(
          variable.names = method_control$variable.names_IAM,
          n.chains = method_control$n.chains,
          n.adapt = method_control$n.adapt,
          n.iter = method_control$n.iter,
          thin = method_control$thin,
          progress.bar = method_control$progress.bar,
          quiet = method_control$quiet,
          diag = method_control$diag,
          return_mcmc = method_control$return_mcmc)
      )

# Outlier detection -------------------------------------------------------
  ## set threshold for outliers
  alpha <- outlier_threshold[1]

  ## apply method ... default is method develop by Jean-Michel and Anne
  if(outlier_method == "RousseeuwCroux1993") {
    ## calculate the median of the sig_a
    xj <- log(matrixStats::colMedians(fit_IAM$sig_a))
    MAD <- 1.483 * median(abs(xj - median(xj)))
    test <- (xj - median(xj)) / MAD
    out <- sort(which(test > alpha))

  } else {
    sig_max <- sig0 * ((1 - alpha) / alpha) ^ .5
    test <- vapply(1:length(De), function(j){
      mean(fit_IAM$sig_a[, j] >= sig_max)

    }, numeric(1))

    out <- sort(which(test > alpha))
  }

  ##some terminal output
  if(verbose){
    if (length(out) > 0) {
      cat(
        paste0(
          "\n    >> Outliers detected: ",
          length(out), "/", length(De),
          " (", round(length(out) / length(De) * 100, 1), "%)"
        )
      )
    }
  }

  ## apply the removal
  if (length(out) == 0) {
      De1 <- De
      s1 <- s

   } else {
      De1 <- De[-out]
      s1 <- s[-out]
   }

# Bayesian modelling BCAM -------------------------------------------------
  fit_BCAM <- .calc_BayesianCentralAgeModel(
      theta,
      mu,
      sigma,
      De = De1,
      s = s1,
      Age_range = Age_range,
      verbose = verbose,
      method_control = list(
        variable.names = method_control$variable.names_BCAM,
        n.chains = method_control$n.chains,
        n.adapt = method_control$n.adapt,
        n.iter = method_control$n.iter,
        thin = method_control$thin,
        progress.bar = method_control$progress.bar,
        quiet = method_control$quiet,
        diag = method_control$diag,
        return_mcmc = method_control$return_mcmc)
    )

# Calculate EDFC -------------------------------------------------
  ## calculate various parameters
  D_e <- fit_BCAM$D_e
  A2 <- fit_BCAM$A

  ## calculate bandwidths
  h <- density(De)$bw
  h1 <- density(De1)$bw

  t <- seq(min(D_e), max(D_e), length.out = min(1000, round(max(D_e) - min(D_e), 0)))

  ind <- min(5000, length(A2))
  subsamp <- sample(1:length(A2), ind, replace = FALSE)
  cdf_ADr <- matrix(0, nrow = ind, ncol = length(t))

  ## De distribution re-sampled without outliers -> De2
  ## De distribution re-sampled initial -> De3
  De2 <-
    rnorm(length(subsamp), sample(De1, size = length(subsamp), replace = TRUE), h1)
  De3 <-
    rnorm(length(subsamp), sample(De, size = length(subsamp), replace = TRUE), h)

  ## calculate ecdf
  cdf_De_no_outlier<- stats::ecdf(De2)(t)
  cdf_De_initial <- stats::ecdf(De3)(t)

  for (i in 1:ind)
    cdf_ADr[i, ] <- stats::ecdf(A2[subsamp[i]] * tildeDr)(t)

  ## calculate mean value and quantiles for the ecdf A * Dr
  cdf_ADr_mean <- matrixStats::colMeans2(cdf_ADr)
  cdf_ADr_quantiles <- matrixStats::colQuantiles(cdf_ADr, probs = c(.025,.975))

  ## further values to ease the interpretation
  d <- density(fit_BCAM$A)
  HPD <- d$x[which.max(d$y)[1]]
  CI_68 <- .calc_HPDI(fit_BCAM$A, prob = 0.68)
  CI_95 <- .calc_HPDI(fit_BCAM$A, prob = 0.95)

# Additional terminal output ----------------------------------------------
if(verbose){
  cat("(3) Age results (presumably in ka) \n")
  cat("    -----------------------------------\n")
  cat("    Age (HPD)   :\t", format(round(HPD,2), nsmall = 2), "\n")
  cat("    Age (CI 68%):\t", paste(format(round(range(CI_68),2), nsmall =2), collapse = " : "), "\n")
  cat("    Age (CI 95%):\t", paste(format(round(range(CI_95),2), nsmall =2), collapse = " : "), "\n")
  cat("    -----------------------------------\n")

}

# Plotting ----------------------------------------------------------------
if(plot){
  ##check incoming plot settings
  plot_settings <- modifyList(x = list(
    cdf_ADr_quantiles = FALSE,
    legend = TRUE,
    legend.pos = "bottomright"
  ), list(...))

  ##make sure we reset plots
  if(par_local) {
    old.par <- par(mfrow = c(1, 2))
    on.exit(par(old.par), add = TRUE)

  }

  if(outlier_analysis_plot){
    N <- length(De)

    ##plot with outliers
    graphics::boxplot(fit_IAM$sig_a, outline = FALSE,
            col = (abs(as.numeric(
              1:length(De) %in% out
            ) - 1) + 2),
    main = "Outlier detection",
    xaxt = "n",
    xlab = expression(paste("Index of ", sigma[a])))

    ## add axis
    axis(side = 1, at = 1:length(De), labels = 1:length(De), )
    mtext(
      text = paste0(length(out), "/", N, " (", round(length(out) / N * 100, 1), "%)"),
      side = 3,
      cex = 0.8
    )
    abline(h = sig0, col = "violet")

    ##plot sd of outliers
    if(length(out) > 0){
      graphics::boxplot(fit_IAM$sig_a[, out],
              outline = FALSE,
              names = out,
              ylab = "Individual sd [a.u.]",
              main = "Outliers: posterior distr.")

    abline(h = sig0, col = "violet")


    } else {
      shape::emptyplot()
      text(0.5, 0.5, "No outlier detected!")

    }
  }

  ##plot age summary
  plot_OSLAgeSummary(
    object = fit_BCAM,
    level = 0.68,
    rug = FALSE,
    polygon_col = rgb(100, 149, 237, 75, maxColorValue = 255),
    verbose = FALSE
  )

  ## open plot area
  plot(NA,
    xlim = range(t),
    ylim = c(0, 1),
    ylab = "ecdf (mean)",
    xlab = "Dose [Gy]",
    main= "ECDF")

  ## add quantile range (only for A * Dr)
  if(plot_settings$cdf_ADr_quantiles){
    polygon(
     x = c(t, rev(t)),
     y = c(cdf_ADr_quantiles[,1], rev(cdf_ADr_quantiles[,2])),
     col = rgb(1,0,0,0.2), lty = 0)
  }

  ##add mean lines for the ecdfs
  lines(t, cdf_ADr_mean, col = 2, lty = 1, lwd = 2)
  lines(t, cdf_De_no_outlier, type = "l", col = 3, lty = 2, lwd = 2)
  lines(t, cdf_De_initial, type = "l", col = 4, lty = 3, lwd = 2)

  if(plot_settings$legend){
    legend(
      plot_settings$legend.pos,
      legend = c(
        expression(A %*% Dr),
        expression(paste(D[e], " no outliers")),
        expression(paste(D[e], " initial"))),
      lty = c(1,2,3),
      bty = "n",
      col = c(2,3,4),
      cex = 0.8)
  }
}

# Return results ----------------------------------------------------------
  return(set_RLum(
    "RLum.Results",
    data = list(
      Ages = fit_BCAM$A,
      Ages_stats = data.frame(
        HPD = HPD,
        CI_68_lower = CI_68[1],
        CI_68_upper = CI_68[2],
        CI_95_lower = CI_95[1],
        CI_95_upper = CI_95[2]),
      outliers_index = out,
      cdf_ADr_mean = cdf_ADr_mean,
      cdf_ADr_quantiles = cdf_ADr_quantiles,
      cdf_De_no_outlier = cdf_De_no_outlier,
      cdf_De_initial = cdf_De_initial,
      mcmc_IAM = fit_IAM$mcmc_IAM,
      mcmc_BCAM = fit_BCAM$mcmc_BCAM
      ),
    info = list(
      call = sys.call(),
      model_IAM = fit_IAM$model,
      model_BCAM = fit_BCAM$model)
  ))

}

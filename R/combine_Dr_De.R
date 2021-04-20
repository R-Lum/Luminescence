#'@title Estimate Individual Age using Bayesian Inference
#'
#'@description A Bayesian robust estimation of central age from equivalent dose
#' measurements under the assumption that the dose rate is modelled by
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
#'@param verbose [logical] (*with default*): enable/disable terminal feedback
#'
#'@return An [RLum.Results-class] object to be used in [combine_Dr_De]
#'
#'@section Function version: 0.1.0
#'
#'@note The function is intended to be called by [combine_Dr_De], however, for
#' reasons of transparency
#'
#'@author Anne Philippe, Université de Nantes (France),
#' Jean-Michel Galharret, Université de Nantes (France),
#' Norbert Mercier, IRAMAT-CRP2A, Université Bordeaux Montaigne (France),
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
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
      diag = if(verbose) TRUE else FALSE
    ),
    val = method_control)

  on.exit(close(model))
  ## select model
  if(length(theta) == 1) {
    data1$theta <- NULL
    model <- textConnection(event1)

  } else {
    model <- textConnection(event2)

  }

  ## run model
  if(verbose) cat("\n(1) Running Bayesian modelling Individual Age Model ... ")

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
      model = paste(jags$model(), "")),
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
#'@param verbose [logical] (*with default*): enable/disable terminal feedback
#'
#'@return An [RLum.Results-class] object
#'
#'@section Function version: 0.1.0
#'
#'@author Anne Philippe, Université de Nantes (France),
#'Jean-Michel Galharret, Université de Nantes (France),
#'Norbert Mercier, IRAMAT-CRP2A, Université Bordeaux Montaigne (France),
#'Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@examples
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
      diag = if(verbose) TRUE else FALSE
    ),
    val = method_control)

  on.exit(close(model))
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
  if(verbose) cat("\n(2) Running Bayesian modelling Bayesian Central Age Model ... ")

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

  if(verbose & method_control$quiet) cat("DONE")

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
      model = paste(jags2$model(), "")
    ),
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
#' `diag` \tab [logical] \tab `FALSE` \tab additional terminal output for convergence diagnostic.
#' `FALSE` if `verbose = FALSE`\cr
#' `progress.bar` \tab [logical] \tab `FALSE` \tab enable/disable progress bar. `FALSE` if `verbose = FALSE`\cr
#' `quiet` \tab [logical] \tab `TRUE` \tab silence terminal output. Set to `TRUE` if `verbose = FALSE`
#'}
#'
#'@param Dr [numeric] (**required**): a dose rate sample
#'
#'@param int_OD [numeric] (**required**): the intrinsic overdispersion, typically the standard deviation characterizing a dose-recovery test distribution
#'
#'@param De [numeric] (**required**): a equivalent dose sample
#'
#'@param s [numeric] (**required**): a vector of measurement errors on the equivalent dose
#'
#'@param Age_range [numeric] (**required**): the age range to be investigated
#'
#'@param alpha [numeric] (*with default*): the required significance level used
#'for the outlier detection. If set to `1`, no outliers are removed.
#'
#'@param method_control [list] (*with default*): named [list] of further parameters passed down
#' to the [rjags::rjags] modelling
#'
#'@param outlier_analysis_plot [logical] (*with default*): enables/disables the outlier analysis plot. Note: the outlier analysis will happen with or without plot output
#'
#'@param par_local [logical] (*with default*): if set to `TRUE` the function uses its
#'own [graphics::par] settings (which will end in two plots next to each other)
#'
#'@param verbose [logical] (*with default*): enable/disable terminal feedback
#'
#'@param plot [logical] (*with default*): enable/disable plot output
#'
#'@return The function returns a plot if `plot = TRUE` and an [RLum.Results-class]
#'object with the following slots:
#'
#' `@data`\cr
#' `.. $Ages`: a [numeric] vector with the modelled \cr
#' `.. $outliers_index`: the index with the detected outliers\cr
#'
#' `@info`\cr
#' `.. $call`: the original function call\cr
#' `.. $model_IAM`: the BUGS model used to derive the individual age\cr
#' `.. $model_BCAM`: the BUGS model used to calculate the Bayesian Central Age\cr
#'
#'@references ##TODO
#'
#'@author Anne Philippe, Université de Nantes (France),
#'Jean-Michel Galharret, Université de Nantes (France),
#'Norbert Mercier, IRAMAT-CRP2A, Université Bordeaux Montaigne (France),
#'Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@section Function version: 0.1.0
#'
#'@keywords dplot distribution 	datagen
#'
#'@examples
#'## set parameters
#' Dr <- stats::rlnorm (1000, 0, 0.3)
#' De <-  50*sample(Dr, 50, replace = TRUE)
#' s <- stats::rnorm(50, 10, 2)
#'
#'## run modelling
#'## note: modify parameters for more realistic results
#'results <- combine_Dr_De(
#' Dr = Dr,
#' int_OD = 0.1,
#' De,
#' s,
#' Age_range = c(0,100),
#'  method_control = list(
#'   n.iter = 100,
#'   n.chains = 1))
#'
#'## show models used
#'writeLines(results@info$model_IAM)
#'writeLines(results@info$model_BCAM)
#'
#'@md
#'@export
combine_Dr_De <- function(
  Dr,
  int_OD,
  De,
  s,
  Age_range,
  alpha = .05,
  method_control = list(),
  outlier_analysis_plot = FALSE,
  par_local = TRUE,
  verbose = TRUE,
  plot = TRUE
) {

# Check input data --------------------------------------------------------
if (!requireNamespace(c("rjags", "coda", "mclust"), quietly = TRUE)) {
    stop("[combine_Dr_De()] To use this function you have to first
         install the package 'rjags', 'coda', and 'mclust'.", call. = FALSE)
}

# Integrity checks --------------------------------------------------------
 if(length(De) != length(s))
   stop("[combine_Dr_De()] 'De' and 's' are not of similar length!", call. = FALSE)

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
      diag = FALSE
    ),
    val = method_control)

# Bayesian Modelling IAM --------------------------------------------------
if(verbose)  cat("\n[combine_Dr_De()]")

fit_IAM <- .calc_IndividualAgeModel(
      theta = theta,
      mu = mu,
      sigma = sigma,
      De = De,
      s = s,
      sig0 = sig0,
      Age_range = Age_range,
      method_control = list(
          variable.names = method_control$variable.names_IAM,
          n.chains = method_control$n.chains,
          n.adapt = method_control$n.adapt,
          n.iter = method_control$n.iter,
          thin = method_control$thin,
          progress.bar = method_control$progress.bar,
          quiet = method_control$quiet,
          diag = method_control$diag)
      )

# Outlier detection -------------------------------------------------------
  sig_max <- sig0 * ((1 - alpha) / alpha) ^ .5
  test <- vapply(1:length(De), function(j){
    mean(fit_IAM$sig_a[, j] >= sig_max)

  }, numeric(1))

  out <- sort(which(test > alpha))
  ##age <- matrixStats::colMedians(fit_IAM$a) ##TODO has no use

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

# Central age estimation ---------------------------------------------------
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
      method_control = list(
        variable.names = method_control$variable.names_BCAM,
        n.chains = method_control$n.chains,
        n.adapt = method_control$n.adapt,
        n.iter = method_control$n.iter,
        thin = method_control$thin,
        progress.bar = method_control$progress.bar,
        quiet = method_control$quiet,
        diag = method_control$diag)
    )

# Plotting ----------------------------------------------------------------
if(plot){
  ##make sure we reset plots
  if(par_local) {
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    par(mfrow = c(1, 2))
  }

  if(outlier_analysis_plot){
    N <- length(De)

    ##plot with outliers
    boxplot(fit_IAM$sig_a, outline = FALSE,
            col = (abs(as.numeric(
              1:length(De) %in% out
            ) - 1) + 2),
    main = "Outlier detection",
    xaxt = "n",
    xlab = "Index of sig_a")
    axis(side = 1, at = 1:length(De), labels = 1:length(De), )
    mtext(
      text = paste0(length(out), "/", N, " (", round(length(out) / N * 100, 1), "%)"),
      side = 3,
      cex = 0.8
    )
    abline(h = sig0, col = 3)

    ##plot sd of outliers
    if(length(out) > 0){
      boxplot(fit_IAM$sig_a[, out],
              outline = FALSE,
              names = out,
              ylab = "Individual sd [a.u.]",
              main = "Outliers: posterior distr.")
      abline(h = sig0, col = 3)
    } else {
      shape::emptyplot()
      text(0.5, 0.5, "No outlier detected!")

    }

  }

  ##plot age summary
  plot_OSLAgeSummary(
    object = fit_BCAM,
    level = 0.68,
    polygon_col = rgb(100, 149, 237, 75, maxColorValue = 255),
    verbose = FALSE
  )

  ##plot ECDF
  D_e <- fit_BCAM$D_e
  A2 <- fit_BCAM$A

  t <- seq(
      min(D_e), max(D_e),
      length.out = min(1000, round(max(D_e) - min(D_e), 0)))

  ind <- min(5000, length(A2))
  subsamp <- sample(1:length(A2), ind, replace = FALSE)
  cdf_De <- matrix(0, nrow = ind, ncol = length(t))
  cdf_ADr <- matrix(0, nrow = ind, ncol = length(t))
  for (i in 1:ind) {
    cdf_De[i, ] <- stats::ecdf(D_e[subsamp[i], ])(t)
    cdf_ADr[i, ] <- stats::ecdf(A2[subsamp[i]] * tildeDr)(t)
  }

  ## calculate various parameters
  cdf_De_mean <- matrixStats::colMeans2(cdf_De)
  cdf_ADr_mean <- matrixStats::colMeans2(cdf_ADr)
  cdf_De_quantiles <- matrixStats::colQuantiles(cdf_De, probs = c(.025,.975))
  cdf_ADr_quantiles <- matrixStats::colQuantiles(cdf_ADr, probs = c(.025,.975))

  ## open plot area
  plot(
    NA,
    NA,
    xlim = range(t),
    ylim = c(0, 1),
    ylab = "ecdf (mean)",
    xlab = "Dose [Gy]",
    main= "ECDF")

  ##add polygon for De
  polygon(
    x = c(t, rev(t)),
    y = c(cdf_De_quantiles[,1], rev(cdf_De_quantiles[,2])),
    col = rgb(1,0,0,0.3), lty = 0)

  ##add polygon for A * Dr
  polygon(
    x = c(t, rev(t)),
    y = c(cdf_ADr_quantiles[,1], rev(cdf_ADr_quantiles[,2])),
    col = rgb(0,1,0,0.3), lty = 0)

  ##add mean lines
  lines(t, cdf_De_mean, col = "red", lty = 1)
  lines(t, cdf_ADr_mean, col = "green", lty = 2)

  legend(
    "bottomright",
    legend = c("De", "A * Dr"),
    lty = c(1,2),
    bty = "n",
    col = c("green", "red"),
    cex = 0.8)
  }

# Return results ----------------------------------------------------------
  return(set_RLum(
    "RLum.Results",
    data = list(
      Ages = fit_BCAM$A,
      outliers_index = out),
    info = list(
      call = sys.call(),
      model_IAM = fit_IAM$model,
      model_BCAM = fit_BCAM$model)
  ))

}

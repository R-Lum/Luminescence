calc_MinDose <- structure(function( # Apply the (un-)logged minimum age model (MAM) after Galbraith et al. (1999) to a given De distribution
  ### Function to fit the (un-)logged three or four parameter minimum dose model 
  ### (MAM-3/4) to De data.
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany) \cr
  ## Based on a rewritten S script of Rex Galbraith, 2010 \cr
  ## The bootstrap approach is based on a rewritten MATLAB script of Alastair Cunningham. \cr
  ## Alastair Cunningham is thanked for his help in implementing and cross-checking the code.\cr
  
  ##section<<
  ## version 0.4.1
  # ===========================================================================
  
  data, 
  ### \code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
  ### for \code{data.frame}: two columns with De \code{(data[ ,1])} and
  ### De error \code{(values[ ,2])}
  sigmab, 
  ### \code{\link{numeric}}  (\bold{required}): spread in De values given as a 
  ### fraction (e.g. 0.2). This value represents the expected overdispersion in
  ### the data should the sample be well-bleached (Cunningham & Walling 2012, p. 100).
  log = TRUE, 
  ### \code{\link{logical}} (with default): fit the (un-)logged 
  ### minimum dose model to De data
  par = 3,
  ### \code{\link{numeric}} (with default): apply the 3- or 4-parametric minimum age
  ### model (\code{par=3} or \code{par=4}). The MAM-3 is used by default.
  bootstrap = FALSE,
  ### \code{\link{logical}} (with default): apply the recycled bootstrap approach of Cunningham & Wallinga (2012).
  init.values, 
  ### \code{\link{numeric}} (optional): a named list with starting values for gamma, sigma, p0 and mu
  ### (e.g. \code{list(gamma=100 sigma=1.5, p0=0.1, mu=100)}).
  ### If no values are provided reasonable values are tried to be estimated from the data.
  plot = TRUE, 
  ### \code{\link{logical}} (with default): plot output
  ### (\code{TRUE}/\code{FALSE})
  ...
  ### (optional) further arguments for bootstrapping (\code{bs.M, bs.N, bs.h, sigmab.sd}). 
  ### See details for their usage.
){ 
  
  ##============================================================================##
  ## ... ARGUMENTS
  ##============================================================================##
  
  extraArgs <- list(...)
  
  ## check if this function is called by calc_MaxDose()
  if ("invert" %in% names(extraArgs)) {
    invert <- extraArgs$invert
    if (!log) {
      log <- TRUE # overwrite user choice as max dose model currently only supports the logged version
      cat(paste("\n[WARNING] The maximum dose model only supports the logged version.",
                "'log' was automatically changed to TRUE.\n\n"))
    }
  } else {
    invert <- FALSE
  }
  
  ## console output
  if ("verbose" %in% names(extraArgs)) {
    verbose <- extraArgs$verbose
  } else {
    verbose <- TRUE
  }
  
  ## bootstrap replications
  # first level bootstrap
  if ("bs.M" %in% names(extraArgs)) {
    M <- as.integer(extraArgs$bs.M)
  } else {
    M <- 1000
  }
  
  # second level bootstrap
  if ("bs.N" %in% names(extraArgs)) {
    N <- as.integer(extraArgs$bs.N)
  } else {
    N <- 3*M
  }
  
  # KDE bandwith
  if ("bs.h" %in% names(extraArgs)) {
    h <- extraArgs$bs.h
  } else {
    h <- (sd(data[ ,1])/sqrt(length(data[ ,1])))*2
  }
  
  # standard deviation of sigmab 
  if ("sigmab.sd" %in% names(extraArgs)) {
    sigmab.sd <- extraArgs$sigmab.sd
  } else {
    sigmab.sd <- 0.04
  }
  
  if ("debug" %in% names(extraArgs)) {
    debug <- extraArgs$debug
  } else {
    debug <- FALSE
  }
  
  ## WARNINGS ----
  if (!debug)
    options(warn = -1)
  
  ##============================================================================##
  ## START VALUES
  ##============================================================================##
  
  if (missing(init.values)) {
    gamma.init <- ifelse(log, log(quantile(data[ ,1], probs = 0.25)), quantile(data[ ,1], probs = 0.25))
    sigma.init <- 1.2
    p0.init <- 0.01
    mu.init <- ifelse(log, log(quantile(data[ ,1], probs = 0.25)), mean(data[ ,1]))
    init.values <- NA
  } else {
    gamma.init <- init.values$gamma
    sigma.init <- init.values$sigma
    p0.init <- init.values$p0
    mu.init <- init.values$mu
  }
  
  ##============================================================================##
  ## ESTIMATE BOUNDARY PARAMETERS
  ##============================================================================##
  
  gamma.xlb <- min(data[ ,1]/10)
  gamma.xub <- max(data[ ,1]*1.1)
  sigma.xlb <- 0
  sigma.xub <- 5
  mu.xlb <- min(data[ ,1])/10
  mu.xub <- max(data[ ,1]*1.1)
  
  # combine lower and upper boundary values to vectors
  if (log) { 
    xlb <- c(log(gamma.xlb), sigma.xlb, 0)
    xub <- c(log(gamma.xub), sigma.xub, 1)
  } else { 
    xlb <- c(gamma.xlb, sigma.xlb, 0)
    xub <- c(gamma.xub, exp(sigma.xub), 1)
  }
  
  if (par==4) {
    xlb <- c(xlb, ifelse(log, log(mu.xlb), mu.xlb))
    xub <- c(xub, ifelse(log, log(mu.xub), mu.xub))
  }
  
  ##============================================================================##
  ## AUXILLARY FUNCTIONS
  ##============================================================================##
  
  # THIS FUNCTION CALCULATES THE NEGATIVE LOG LIKELIHOOD OF THE DATA
  Neglik_f <- function(gamma, sigma, p0, mu, data) {
    # this calculates the negative of the log likelihood of the  
    # data (data) for a given set of parameters (gamma, sigma, p0)
    # data is a 2x2 matrix of data: De, rel_error (including sigma_b)
    
    # recover the data
    zi <- data[ ,1]
    si <- data[ ,2]
    n <- length(zi)
    
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
    lf2i <- lf2i + log(1-pnorm(res0)) - log(1-pnorm(res1))
    llik <- log( exp(lf1i) + exp(lf2i) )
    negll <- -sum(llik)
    
    return(negll)
  }
  
  # THIS MAXIMIZES THE Neglik_f LIKELIHOOD FUNCTION AND RETURNS AN MLE OBJECT
  Get_mle <- function(data, gamma, sigma, p0, mu) {
    
    start <- list(gamma=gamma, sigma=sigma, p0=p0, mu=mu)
    
    t.args <- list(data = list(data=data),
                  optimizer = "nlminb",
                  #method = "L-BFGS-B",
                  lower=c(gamma = -Inf, sigma = 0, p0 = 0, mu = -Inf),
                  upper=c(gamma = Inf, sigma = Inf, p0 = 1, mu = Inf),
                  minuslogl = Neglik_f,
                  control = list(iter.max = 1000L),
                  start = start
                  )
    
    # TODO: PROPER ERROR HANDLING
    tryCatch({
        suppressWarnings(
          mle <- do.call("mle2", t.args)
          )
    }, error = function(e) {
      stop(paste("Sorry, seems like I encountered an error...:", e), call. = FALSE)
    })
    
    return(mle)
  }
  
  ##============================================================================##
  ## MAIN PROGRAM
  ##============================================================================##
  
  # combine errors
  if (log) { 
    if (invert) {
      lcd <- log(data[ ,1])*-1
      x.offset <- abs(min(lcd))
      lcd <- lcd+x.offset
    } else {
      lcd <- log(data[ ,1])
    }
    lse <- sqrt((data[ ,2]/data[ ,1])^2 + sigmab^2)
  } else {
    lcd <- data[ ,1]
    lse <- sqrt(data[ ,2]^2 + sigmab^2)
  }
  
  # create new data frame with DE and combined relative error
  dat <- cbind(lcd, lse)
  
  # get the maximum likelihood estimate
  ests <- Get_mle(dat, gamma.init, sigma.init, p0.init, mu.init)
  
  # check if any standard errors are NA or NaN
  coef_err <- t(as.data.frame(summary(ests)@coef[ ,2]))

  if (debug) 
    print(summary(ests))

  if (any(is.nan(coef_err))) 
    coef_err[which(is.nan(coef_err))] <- t(as.data.frame(ests@coef))/100
  if (any(is.na(coef_err)))
    coef_err[which(is.na(coef_err))] <- t(as.data.frame(ests@coef))/100
  
  if (par == 3)
    which <- c("gamma", "sigma", "p0")
  if (par == 4)
    which <- c("gamma", "sigma", "p0", "mu")
  
  # calculate profile log likelihoods
  prof <- profile(ests,
                 which = which,
                 std.err = as.vector(coef_err),
                 #try_harder = TRUE,
                 quietly = TRUE,
                 tol.newmin = Inf,
                 skiperrs = TRUE,
                 prof.lower=c(gamma = -Inf, sigma = 0, p0 = 0, mu = -Inf),
                 prof.upper=c(gamma = Inf, sigma = Inf, p0 = 1, mu = Inf)
                 )
  # Fallback when profile() returns a 'better' fit
  maxsteps <- 100
  cnt <- 1
  while (!inherits(prof, "profile.mle2")) {
    message(paste0("## Trying to find a better fit (", cnt, "/10) ##"))
    if (maxsteps == 0L) 
      stop(paste("Sorry, but I can't find a converging fit for the profile log-likelihood."), 
           call. = FALSE)
    
    prof <- profile(ests,
                   which = which,
                   std.err = as.vector(coef_err),
                   try_harder = TRUE,
                   quietly = TRUE,
                   maxsteps = maxsteps,
                   tol.newmin = Inf,
                   skiperrs = TRUE,
                   prof.lower = xlb,
                   prof.upper = xub
                   )
    
    maxsteps <- maxsteps - 10
    cnt <- cnt+1
  }
  
  ## DELETE rows where z = -Inf/Inf
  prof@profile$gamma <-  prof@profile$gamma[which(prof@profile$gamma["z"] != Inf), ]
  prof@profile$gamma <-  prof@profile$gamma[which(prof@profile$gamma["z"] != -Inf), ]
  prof@profile$sigma <-  prof@profile$sigma[which(prof@profile$sigma["z"] != Inf), ]
  prof@profile$sigma <-  prof@profile$sigma[which(prof@profile$sigma["z"] != -Inf), ]
  prof@profile$p0 <-  prof@profile$p0[which(prof@profile$p0["z"] != Inf), ]
  prof@profile$p0 <-  prof@profile$p0[which(prof@profile$p0["z"] != -Inf), ]
  
  if (par == 4) {
    prof@profile$mu <-  prof@profile$mu[which(prof@profile$mu["z"] != Inf), ]
    prof@profile$mu <-  prof@profile$mu[which(prof@profile$mu["z"] != -Inf), ]
  }
  
  # calculate Bayesian Information Criterion (BIC)
  BIC <- BIC(ests)
  
  # retrieve results from mle2-object
  pal <- if (log) {
    if (invert) {
      exp((coef(ests)[["gamma"]]-x.offset)*-1)
    } else {
      exp(coef(ests)[["gamma"]])
    }
  } else {
    coef(ests)[["gamma"]]
  }
  sig <- coef(ests)[["sigma"]]
  p0end <- coef(ests)[["p0"]]
  
  if (par==4) {
    muend <- ifelse(log, exp(coef(ests)[["mu"]]), coef(ests)[["mu"]])
  } else {
    muend <- NA
  }
  
  ##============================================================================##
  ## ERROR CALCULATION
  
  #### METHOD 1: follow the instructions of Galbraith & Roberts (2012) ####
  # "If the likelihood profile is symmetrical about the parameter, an approximate standard error 
  #  can be calculated by dividing the length of this interval by 3.92"
  conf <- as.data.frame(confint(prof, tol.newmin = Inf, quietly = TRUE))
  
  if (invert) {
    conf[1, ] <- (conf[1, ]-x.offset)*-1
    t <- conf[1,1]
    conf[1,1] <- conf[1,2]
    conf[1,2] <- t
  }
  
  gamma_err <- if (log) {
    (exp(conf["gamma",2])-exp(conf["gamma",1]))/3.92 
  } else {
    (conf["gamma",2]-conf["gamma",1])/3.92
  }
 
  
  ##============================================================================##
  ## AGGREGATE RESULTS
  summary <- data.frame(de=pal, 
                       de_err=gamma_err, 
                       "ci_lower"=ifelse(log, exp(conf["gamma",1]), conf["gamma",1]),
                       "ci_upper"=ifelse(log, exp(conf["gamma",2]), conf["gamma",2]),
                       par=par,
                       sig=sig, 
                       p0=p0end,
                       mu=muend,
                       Lmax=-ests@min, 
                       BIC=BIC)
  call <- sys.call()
  args <- list(log=log, sigmab=sigmab, bootstrap=bootstrap, 
              init.values=init.values, 
              bs.M=M, bs.N=N, bs.h=h, sigmab.sd=sigmab.sd)
  
  ##============================================================================##
  ## BOOTSTRAP
  ##============================================================================##
  
  # create normal distribution for sigmab
  if (bootstrap == TRUE) {
    # header
    cat(paste("\n   --- RECYCLED BOOTSTRAP ---- \n"))
    
    # save original sigmab value
    sigmab.global <- sigmab
    
    # define normal dist. parameters for sigmab
    sigmab.mean <- sigmab
    sigmab.sd <- sigmab.sd
    
    # empty variables
    n <- length(data[ ,1])
    b2Pmatrix <- matrix(NA, N, n)
    b2mamvec <- matrix(NA, N, 1)
    pairs <- matrix(NA, M, 2)
    p0.pairs <- matrix(NA, M, 2)
    
    # KDE bandwidth
    h <- h
    
    ## --------- N SECOND LEVEL BOOTSTRAPS -------------- ##
    for(i in 1:N) {
      if (i == 1) {
        cat(paste("\n"))
        cat(paste("   Calculating", N, "second level bootstraps. Please wait... \n"))
        
        #get starting time
        time.start <- Sys.time()
      }
      if (i == 10) {
        #get stop time
        time.stop <- Sys.time()
        # calculate time needed for one iteration
        time.elapsed <- abs(as.double(time.start - time.stop))/10
        # estimate time till second level bootstraps are finished
        ETA <- Sys.time()+time.elapsed*N
        # tell user about time
        write(paste("   Estimated time needed:", round(time.elapsed*N), "s | Finished at", ETA), file = "")
        # create progress bar
        pb <- txtProgressBar(min = 0, max = N, char = "#", style = 3)
      }
      # draw random sigmab
      sigmab <- rnorm(1, sigmab.mean, sigmab.sd)
      # n random integers with replacement
      R <- sample(x=n, size=n, replace=TRUE)
      # recored frequencies of each n, used in calc of product term
      for(k in 1:n) {
        b2Pmatrix[i, k] <- sum(R == k)
      }
      
      # create bootstrap replicate
      Rde <- data[R, ]
      de2 <- Rde
      
      # combine errors
      if (log) { 
        lcd <- log(de2[ ,1])
        lse <- sqrt((de2[ ,2]/de2[ ,1])^2 + sigmab^2)
      } else {
        lcd <- de2[ ,1]
        lse <- sqrt(de2[ ,2]^2 + sigmab^2)
      }
      
      # create new data frame with DE and combined relative error
      dat <- cbind(lcd, lse)
      
      # get the maximum likelihood estimate
      ests <- Get_mle(dat, gamma.init, sigma.init, p0.init, mu.init)
      
      # check if any standard errors are NA or NaN
      coef_err <- t(as.data.frame(summary(ests)@coef[ ,2]))
      if (any(is.nan(coef_err)))
        coef_err[which(is.nan(coef_err))] <- t(as.data.frame(ests@coef))/100
      if (any(is.na(coef_err)))
        coef_err[which(is.na(coef_err))] <- t(as.data.frame(ests@coef))/100
      
      # save gamma to storage matrix
      if (log) {
        if (invert) {
          b2mamvec[i,1] <- exp((coef(ests)[["gamma"]]-x.offset)*-1)
        } else {
          b2mamvec[i,1] <- exp(coef(ests)[["gamma"]])
        }
      } else {
        b2mamvec[i,1] <- coef(ests)[["gamma"]]
      }
      
      if (i > 10) {
        # update progress bar
        setTxtProgressBar(pb, i)
      }
    }
    # close progress bar
    close(pb)
    
    ## --------- M FIRST LEVEL BOOTSTRAPS -------------- ##
    for(i in 1:M) {
      if (i == 1) {
        cat(paste("\n"))
        cat(paste("   Calculating", M, "first level bootstraps. Please wait... \n"))
        #get starting time
        time.start <- Sys.time()
      }
      if (i == 10) {
        #get stop time
        time.stop <- Sys.time()
        # calculate time needed for one iteration
        time.elapsed <- abs(as.double(time.start - time.stop))/10
        # estimate time till second level bootstraps are finished
        ETA <- Sys.time()+time.elapsed*N
        # tell user about time
        write(paste("   Estimated time needed:", round(time.elapsed*N), "s | Finished at", ETA), file = "")
        # create progress bar
        pb <- txtProgressBar(min = 0, max = M, char = "#", style = 3)
      }
      # draw random sigmab
      sigmab <- rnorm(1, sigmab.mean, sigmab.sd)
      # n random integers with replacement
      R <- sample(x=n, size=n, replace=TRUE)
      # create bootstrap replicates
      Rde <- data[R, ]
      de2 <- Rde
      
      # combine errors
      if (log) { 
        lcd <- log(de2[ ,1])
        lse <- sqrt((de2[ ,2]/de2[ ,1])^2 + sigmab^2)
      } else {
        lcd <- de2[ ,1]
        lse <- sqrt(de2[ ,2]^2 + sigmab^2)
      }
      
      # create new data frame with DE and combined relative error
      dat <- cbind(lcd, lse)
      
      # get the maximum likelihood estimate
      ests <- Get_mle(dat, gamma.init, sigma.init, p0.init, mu.init)
      
      # check if any standard errors are NA or NaN
      coef_err <- t(as.data.frame(summary(ests)@coef[ ,2]))
      if (any(is.nan(coef_err)))
        coef_err[which(is.nan(coef_err))] <- t(as.data.frame(ests@coef))/100
      if (any(is.na(coef_err)))
        coef_err[which(is.na(coef_err))] <- t(as.data.frame(ests@coef))/100
      
      # save gamma to storage matrix
      if (log) {
        if (invert) {
          theta <- exp((coef(ests)[["gamma"]]-x.offset)*-1)
        } else {
          theta <- exp(coef(ests)[["gamma"]]) 
        }
      } else {
        theta <- coef(ests)[["gamma"]]
      }
      
      bs.p0 <- coef(ests)[["p0"]]
      
      # kernel density estimate
      f <- density(x=de2[ ,1], kernel="gaussian", bw=h)
      f <- approx(x=f$x, y=f$y, xout=de2[ ,1])
      
      # convert to matrix
      f <- cbind(f$x,f$y)
      pStarTheta <- as.vector(f[ ,2]/sum(f[ ,2]))
      thetavec <- matrix(theta, N, 1)    
      kdthis <- (thetavec-b2mamvec)/h
      kd1 <- dnorm(kdthis)
      
      # the product term
      Pmat <- pStarTheta/(1/n)
      Pmat <- matrix(t(Pmat), N, n, byrow=TRUE)
      prodterm <- apply(Pmat^b2Pmatrix, 1, prod)
      
      kd2 <- kd1*prodterm
      kd <- sum(kd2)
      likelihood <- (1/(N*h))*kd
      pairs[i, ] <- c(theta, likelihood)
      p0.pairs[i, ] <- c(theta, bs.p0)
      
      if (i > 10) {
        # update progress bar
        setTxtProgressBar(pb, i)
      }
    }
    # close progress bar
    close(pb)
    
    ## --------- FIT POLYNOMIALS -------------- ##
    # polynomial fits of increasing degrees
    poly.three <- lm(pairs[ ,2] ~ poly(pairs[ ,1], degree=3, raw=TRUE)) # 3-deg. poly
    poly.four <- lm(pairs[ ,2] ~ poly(pairs[ ,1], degree=4, raw=TRUE))  # 4-deg. poly
    poly.five <- lm(pairs[ ,2] ~ poly(pairs[ ,1], degree=5, raw=TRUE))  # 5-deg. poly
    poly.six <- lm(pairs[ ,2] ~ poly(pairs[ ,1], degree=6, raw=TRUE))   # 6-deg. poly
    
    ## --------- FIT LOESS -------------- ##
    loess <- loess(pairs[ ,2] ~ pairs[ ,1])     
  }
  
  ##============================================================================##
  ## CONSOLE PRINT
  ##============================================================================##
  
  if (verbose) {
    if (bootstrap==FALSE) {
      cat("\n----------- meta data -----------\n")
      print(data.frame(n=length(data[ ,1]),
                       par=par,
                       sigmab=sigmab, 
                       logged=log, 
                       Lmax=-ests@min,
                       BIC=BIC,
                       row.names = ""))
      
      cat("\n--- final parameter estimates ---\n")
      print(round(data.frame(gamma=ifelse(!invert, coef(ests)[["gamma"]], (coef(ests)[["gamma"]]-x.offset)*-1), 
                             sigma=coef(ests)[["sigma"]],
                             p0=coef(ests)[["p0"]],
                             mu=ifelse(par==4, ifelse(log,log(muend),muend),0),
                             row.names=""), 2))
      
      cat("\n------ confidence intervals -----\n")
      print(round(conf, 2))
      
      cat("\n------ De (asymmetric error) -----\n")
      print(round(data.frame(De=pal, 
                             "lower"=ifelse(log, ifelse(!invert, exp(conf["gamma",1]), exp((conf["gamma",2]-x.offset)*-1)), conf["gamma",1]),
                             "upper"=ifelse(log, ifelse(!invert, exp(conf["gamma",2]), exp((conf["gamma",1]-x.offset)*-1)), conf["gamma",2]),
                             row.names=""), 2))
      
      cat("\n------ De (symmetric error) -----\n")
      print(round(data.frame(De=pal, 
                             error=gamma_err, 
                             row.names=""), 2))
      
    } else if (bootstrap==TRUE) {
      # TODO: at least some text output would be nice for bootstrap
    }
  }
  
  ##============================================================================##
  ## RETURN VALUES
  ##============================================================================##
  
  if (!bootstrap) 
    pairs <- p0.pairs <- poly.three <- poly.four <- poly.five <- poly.six <- loess <- NULL
  
  newRLumResults.calc_MinDose <- set_RLum.Results(
    data = list(summary = summary,
                data = data,
                args = args,
                call = call,
                mle = ests,
                BIC = BIC,
                confint = conf,
                profile = prof,
                bootstrap = list(
                  pairs = list(gamma=pairs,
                               p0=p0.pairs),
                  poly.fits = list(poly.three = poly.three,
                                   poly.four = poly.four,
                                   poly.five = poly.five,
                                   poly.six = poly.six),
                  loess.fit = loess)))
  
  ##=========##
  ## PLOTTING
  if (plot==TRUE) {
    try(plot_RLum.Results(newRLumResults.calc_MinDose, ...))
  }
  
  if (!debug)
    options(warn = 0)
  
  invisible(newRLumResults.calc_MinDose)
  ### Returns a plot (optional) and terminal output. In addition an 
  ### \code{\linkS4class{RLum.Results}} object is 
  ### returned containing the following elements:
  ###
  ### \item{summary}{\link{data.frame} summary of all relevant model results.}
  ### \item{data}{\link{data.frame} original input data}
  ### \item{args}{\link{list} used arguments}
  ### \item{call}{\link{call} the function call}
  ### \item{mle}{\link{mle2} \code{mle2} object containing the maximum log likelhood functions for all parameters}
  ### \item{BIC}{\link{numeric} BIC score}
  ### \item{confint}{\link{data.frame} confidence intervals for all parameters}
  ### \item{profile}{\link{profile.mle2} the log likelihood profiles}
  ### \item{bootstrap}{\link{list} bootstrap results}
  ###
  ### The output should be accessed using the function 
  ### \code{\link{get_RLum.Results}}  
  
  ##details<<
  ## \bold{Parameters} \cr\cr
  ## This model has four parameters: \cr\cr
  ## \tabular{rl}{
  ## \code{gamma}: \tab minimum dose on the log scale \cr
  ## \code{mu}: \tab mean of the non-truncated normal distribution \cr
  ## \code{sigma}: \tab spread in ages above the minimum \cr
  ## \code{p0}: \tab proportion of grains at gamma \cr }
  ## If \code{par=3} (default) the 3-parametric minimum age model is applied,
  ## where \code{gamma=mu}. For \code{par=4} the 4-parametric model is applied
  ## instead.\cr\cr
  ## \bold{(Un-)logged model} \cr\cr
  ## In the original version of the three-parameter minimum dose model, the 
  ## basic data are the natural logarithms of the De estimates and relative 
  ## standard errors of the De estimates. This model will be applied if 
  ## \code{log=TRUE}. \cr\cr
  ## If \code{log=FALSE}, the modified un-logged model will be applied 
  ## instead. This has essentially the same form as the original version. 
  ## \code{gamma} and \code{sigma} are in Gy and \code{gamma} becomes the
  ## minimum true dose in the population. \cr\cr
  ## While the original (logged) version of the mimimum dose model may be
  ## appropriate for most samples (i.e. De distributions), the modified 
  ## (un-logged) version is specially designed for modern-age and young
  ## samples containing negative, zero or near-zero De estimates (Arnold 
  ## et al. 2009, p. 323). \cr\cr
  ## \bold{Initial values & boundaries} \cr\cr
  ## The log likelihood calculations use the \link{nlminb} function for
  ## box-constrained optimisation using PORT routines. 
  ## Accordingly, initial values for the four parameters can be specified via
  ## \code{init.values}. If no values are provided for \code{init.values} reasonable starting 
  ## values are estimated from the input data. 
  ## If the final estimates of \emph{gamma}, \emph{mu}, 
  ## \emph{sigma} and \emph{p0} are totally off target, consider providing custom
  ## starting values via \code{init.values}. \cr
  ## In contrast to previous versions of this function the boundaries for the
  ## individual model parameters can no longer be specified. Appropriate boundary
  ## are now hard-coded and are valid for all input data sets. \cr\cr
  ## \bold{Bootstrap} \cr\cr
  ## When \code{bootstrap=TRUE} the function applies the bootstrapping method as
  ## described in Wallinga & Cunningham (2012). By default, the minimum age model 
  ## produces 1000 first level and 3000 second level bootstrap replicates
  ## (actually, the number of second level bootstrap replicates is three times 
  ## the number of first level replicates unless specified otherwise). 
  ## The uncertainty on sigmab is 0.04 by default. These values can be changed by
  ## using the arguments \code{bs.M} (first level replicates), \code{bs.N}
  ## (second level replicates) and \code{sigmab.sd} (error on sigmab). With \code{bs.h}
  ## the bandwidth of the kernel density estimate can be specified. By default, \code{h} 
  ## is calculated as \cr
  ## \deqn{h = (2*\sigma_{DE})/\sqrt{n}}
  
  
  ##references<<
  ## Arnold, L.J., Roberts, R.G., Galbraith, R.F. & DeLong, S.B., 2009. A revised
  ## burial dose estimation procedure for optical dating of young and modern-age 
  ## sediments. Quaternary Geochronology 4, 306-325. \cr\cr
  ## Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission 
  ## track ages. Nuclear Tracks Radiation Measurements 4, 459-470. \cr\cr
  ## Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley, J.M., 
  ## 1999. Optical dating of single grains of quartz from Jinmium rock shelter, 
  ## northern Australia. Part I: experimental design and statistical models. 
  ## Archaeometry 41, 339-364. \cr\cr
  ## Galbraith, R.F., 2005. Statistics for Fission Track Analysis, Chapman & 
  ## Hall/CRC, Boca Raton. \cr\cr
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose
  ## and error calculation and display in OSL dating: An overview and some
  ## recommendations. Quaternary Geochronology 11, 1-27. \cr\cr
  ## \bold{Further reading} \cr\cr
  ## Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain 
  ## equivalent dose (De) distributions: Implications for OSL dating of sediment 
  ## mixtures. Quaternary Geochronology 4, 204-230. \cr\cr
  ## Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain 
  ## quartz De distributions and an assessment of procedures for estimating burial
  ## dose. Quaternary Science Reviews 25, 2475-2502. \cr\cr
  ## Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial
  ## archives using robust OSL chronologies. Quaternary Geochronology 12, 
  ## 98-106. \cr\cr
  ## Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the
  ## reproducibility and accuracy of optical dating of fluvial deposits. 
  ## Quaternary Geochronology 1, 109-120. \cr\cr
  ## Rodnight, H., 2008. How many equivalent dose values are needed to obtain a
  ## reproducible distribution?. Ancient TL 26, 3-10. \cr\cr
  
  ##note<<
  ## The default starting values for \emph{gamma}, \emph{mu}, 
  ## \emph{sigma} and \emph{p0} may only be appropriate for some De data sets 
  ## and may need to be changed for other data. This is especially true when
  ## the un-logged version is applied. \cr 
  ## Also note that all R warning messages are suppressed when running this
  ## function. If the results seem odd consider re-running the model with
  ## \code{debug=TRUE} which provides extended console output and forwards
  ## all internal warning messages.
  
  ##seealso<<
  ## \code{\link{calc_CentralDose}},
  ## \code{\link{calc_CommonDose}}, \code{\link{calc_FiniteMixture}},
  ## \code{\link{calc_FuchsLang2001}}, \code{\link{calc_MaxDose}}
  
},ex=function(){
  
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  
  # apply the un-logged, 3-parametric minimum age model
  calc_MinDose(data = ExampleData.DeValues, 
               par = 3, 
               sigmab = 0.05, 
               log = FALSE)
  
  # re-run the model, but save results to a variable
  mam <- calc_MinDose(data = ExampleData.DeValues, 
                     par = 3, 
                     sigmab = 0.05, 
                     log = FALSE, 
                     plot = FALSE)
  
  # show summary table
  get_RLum.Results(mam, "summary")
  
  # plot the log likelihood profiles retroactively
  plot_RLum.Results(mam)
})



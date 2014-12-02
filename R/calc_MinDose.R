calc_MinDose<- structure(function( # Apply the (un-)logged three parameter minimum age model (MAM 3) after Galbraith et al. (1999) to a given De distribution
  ### Function to fit the (un-)logged three parameter minimum dose model 
  ### (MAM 3) to De data.
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany) \cr
  ## Based on a rewritten S script of Rex Galbraith, 2010 \cr
  ## The bootstrap approach is based on a rewritten MATLAB script of Alastair Cunningham. \cr
  ## Alastair Cunningham is thanked for his help in implementing and cross-checking the code.\cr
  
  ##section<<
  ## version 0.4 
  # ===========================================================================
  
  data, 
  ### \code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
  ### for \code{data.frame}: two columns with De \code{(data[,1])} and
  ### De error \code{(values[,2])}
  sigmab, 
  ### \code{\link{numeric}}  (\bold{required}): spread in De values given as a 
  ### fraction (e.g. 0.2). This value represents the expected overdispersion in
  ### the data should the sample be well-bleached (Cunningham & Walling 2012, p. 100).
  log=TRUE, 
  ### \code{\link{logical}} (with default): fit the (un-)logged three parameter 
  ### minimum dose model to De data
  par=3,
  ### \code{\link{numeric}} (with default): apply the 3- or 4-parametric minimum age
  ### model (\code{par=3} or \code{par=4}).
  bootstrap=FALSE,
  ### code{\link{logical}} (with default): apply the recycled bootstrap approach of Cunningham & Wallinga (2012).
  boundaries,
  ### \code{\link{list}}: a named list of boundary values for gamma, sigma and mu
  ### to be used in the optimisation routine 
  ### (e.g. \code{list(gamma=c(0.01,100), sigma=c(0.01,5), p0=c(0.01,0.99), mu=c(10, 100))}). 
  ### If no values are provided reasonable values are tried to be estimated from the data.
  init.values, 
  ### \code{\link{numeric}}: a named list with starting values for gamma, sigma, p0 and mu
  ### (e.g. \code{list(gamma=100 sigma=1.5, p0=0.1, mu=100)}).
  ### If no values are provided reasonable values are tried to be estimated from the data.
  plot=TRUE, 
  ### \code{\link{logical}} (with default): plot output
  ### (\code{TRUE}/\code{FALSE})
  ...
  ### further arguments for bootstrapping (\code{bs.M, bs.N, bs.h, sigmab.sd}). 
  ### See details for their usage.
){ 
  
  ##============================================================================##
  ## ... ARGUMENTS
  ##============================================================================##
  
  extraArgs <- list(...)
  
  ## check if this function is called by calc_MaxDose()
  if("invert" %in% names(extraArgs)) {
    invert<- extraArgs$invert
    if(!log) {
      log<- TRUE # overwrite user choice as max dose model currently only supports the logged version
      cat(paste("\n[WARNING] The maximum dose model only supports the logged version.",
                "'log' was automatically changed to TRUE.\n\n"))
    }
  } else {
    invert<- FALSE
  }
  
  ## console output
  if("verbose" %in% names(extraArgs)) {
    verbose<- extraArgs$verbose
  } else {
    verbose<- TRUE
  }
  
  ## bootstrap replications
  # first level bootstrap
  if("bs.M" %in% names(extraArgs)) {
    M<- as.integer(extraArgs$bs.M)
  } else {
    M<- 1000
  }
  
  # second level bootstrap
  if("bs.N" %in% names(extraArgs)) {
    N<- as.integer(extraArgs$bs.N)
  } else {
    N<- 3*M
  }
  
  # KDE bandwith
  if("bs.h" %in% names(extraArgs)) {
    h<- extraArgs$bs.h
  } else {
    h<- (sd(data[,1])/sqrt(length(data[,1])))*2
  }
  
  # standard deviation of sigmab 
  if("sigmab.sd" %in% names(extraArgs)) {
    sigmab.sd<- extraArgs$sigmab.sd
  } else {
    sigmab.sd<- 0.04
  }
  
  if("debug" %in% names(extraArgs)) {
    debug<- extraArgs$debug
  } else {
    debug<- FALSE
  }
  
  ##============================================================================##
  ## START VALUES
  ##============================================================================##
  
  if(missing(init.values)) {
    gamma.init<- ifelse(log, log(min(data[,1])), min(data[,1]))
    sigma.init<- 1.2
    p0.init<- 0.01
    mu.init<- ifelse(log, log(mean(data[,1])), mean(data[,1]))
    init.values<- NA
  } else {
    gamma.init<- init.values$gamma
    sigma.init<- init.values$sigma
    p0.init<- init.values$p0
    mu.init<- init.values$mu
  }
  
  ##============================================================================##
  ## ESTIMATE BOUNDARY PARAMETERS
  ##============================================================================##
  
  ## BOUNDARIES FOR GAMMA
  if(missing(boundaries) == TRUE) {
    gamma.xlb<- min(data[,1]/10)
    gamma.xub<- max(data[,1]*1.1)
    sigma.xlb<- 1e-8
    sigma.xub<- 5
    mu.xlb<- min(data[,1])
    mu.xub<- max(data[,1]*1.1)
  } else {
    gamma.xlb<- boundaries$gamma[1]
    gamma.xub<- boundaries$gamma[2]
    sigma.xlb<- boundaries$sigma[1]
    sigma.xub<- boundaries$sigma[2]
    mu.xlb<- boundaries$mu[1]
    mu.xub<- boundaries$mu[2]
  }
  
  boundary.gamma<- c(gamma.xlb, gamma.xub)
  boundary.sigma<- c(sigma.xlb, sigma.xub)
  boundary.mu<- c(mu.xlb, mu.xub)
  
  # combine lower and upper boundary values to vectors
  if(log==TRUE) { 
    xlb<- c(log(gamma.xlb), sigma.xlb, 1e-8)
    xub<- c(log(gamma.xub), sigma.xub, 0.99999999)
  } else { 
    xlb<- c(gamma.xlb, sigma.xlb, 1e-8)
    xub<- c(gamma.xub, exp(sigma.xub), 0.99999999)
  }
  
  if(par==4) {
    xlb<- c(xlb, ifelse(log, log(mu.xlb), mu.xlb))
    xub<- c(xub, ifelse(log, log(mu.xub), mu.xub))
  }
  
  ##============================================================================##
  ## AUXILLARY FUNCTIONS
  ##============================================================================##
  
  # THIS FUNCTION CALCULATES THE NEGATIVE LOG LIKELIHOOD OF THE DATA
  Neglik_f<- function(gamma, sigma, p0, mu, data) {
    # this calculates the negative of the log likelihood of the  
    # data (data) for a given set of parameters (gamma, sigma, p0)
    #data is a 2x2 matrix of data: De, rel_error (including sigma_b)
    
    # recover the data
    zi<- data[,1]
    si<- data[,2]
    n<- length(zi)
    
    # in the MAM-3 gamma and mu are assumed to be equal
    if(par==3) {
      mu<- gamma
    }
    
    # calculate sigma^2 + seld^2, mu0 and sigma0
    s2<- sigma^2 + si^2
    sigma0<- 1/sqrt(1/sigma^2 + 1/si^2)
    mu0<- (mu/sigma^2 + zi/si^2)/(1/sigma^2 + 1/si^2) 
    
    # calculate the log-likelihood
    logsqrt2pi<- 0.5*log(2*pi)
    res0<- (gamma - mu0)/sigma0
    res1<- (gamma - mu)/sigma
    lf1i<- log(p0) - log(si) - 0.5*((zi-gamma)/si)^2   - logsqrt2pi
    lf2i<- log(1-p0) - 0.5*log(s2) - 0.5*(zi-mu)^2/s2  - logsqrt2pi
    lf2i<- lf2i + log(1-pnorm(res0)) - log(1-pnorm(res1))
    llik<- log( exp(lf1i) + exp(lf2i) )
    negll<- -sum(llik)
    
    # check if negative log likelihood is a valid estimate 
    if(negll == Inf || negll == -Inf || is.na(negll)==TRUE) {
      negll=0
    }
    
    return(negll)
  }
  
  ## THIS EVALUATES WHETER ANY OF THE PARAMETERS IS ON THE BOUNDARY
  Eval_boundaries<- function(mle, args) {
    coef<- as.vector(mle@coef)
    lower<- as.vector(args$lower)
    upper<- as.vector(args$upper)
    flag<- FALSE
    
    if(debug) {
      print(paste("coef:", paste(round(coef,4), collapse = " | ")))
      print(paste("lower:", paste(round(lower,4), collapse = " | ")))
      print(paste("upper:", paste(round(upper,4), collapse = " | ")))
    }
    
    for(i in 1:length(coef)) {
      if(any(coef[i]==c(lower[i],upper[i]))) {
        flag<- TRUE
        break
      }
    }
    return(flag)
  }
  
  ## THIS FUNCTION IS CALLED WHEN ONE OR MORE ESTIMATES REACHED THE BOUNDARY
  Readjust_boundaries<- function(args) {
    args$upper["sigma"]<- args$upper["sigma"]*1.1
    
    # TODO: implement check which parameter is actually on the boundary
    return(args)
  }
  
  # THIS MAXIMIZES THE Neglik_f LIKELIHOOD FUNCTION AND RETURNS AN MLE OBJECT
  Get_mle<- function(data, xlb, xub, gamma, sigma, p0, mu) {
    
    if(par==3) {
      lower<- c(gamma=xlb[1], sigma=xlb[2], p0=xlb[3])
      upper<- c(gamma=xub[1], sigma=xub[2], p0=xub[3])
      start<- list(gamma=gamma, sigma=sigma, p0=p0)
    } else {
      lower<- c(gamma=xlb[1], sigma=xlb[2], p0=xlb[3], mu=xlb[4])
      upper<- c(gamma=xub[1], sigma=xub[2], p0=xub[3], mu=xub[4])
      start<- list(gamma=gamma, sigma=sigma, p0=p0, mu=mu)
    }
    
    t.args<- list(data = list(data=data),
                  optimizer = "optim",
                  method = "L-BFGS-B",
                  lower=lower,
                  upper=upper,
                  minuslogl = Neglik_f,
                  control = list(maxit = 1000),
                  start = start)
    
    # TODO: PROPER ERROR HANDLING
    # check if values are on the boundary and change accordingly;
    # else, use latest working set of parameters
    
    tryCatch({
      for(i in 1:10) {
        suppressWarnings(
          mle<- do.call("mle2", t.args)
        )
        
        flag.b<- Eval_boundaries(mle, t.args)
        if(flag.b==TRUE) {
          t.args<- Readjust_boundaries(t.args)
          if(debug){print("Readjusting boundaries")}
        } else {
          break
        }
      }
    }, error = function(e) {
      print(paste("Sorry, seems like I encountered an error...:", e))
    })
    
    if(class(mle)!="mle2") {
      mle<- do.call("mle2", t.args)
    }
    
    return(mle)
  }
  
  ##============================================================================##
  ## MAIN PROGRAM
  ##============================================================================##
  
  # combine errors
  if(log==TRUE) { 
    if(invert==TRUE) {
      lcd<- log(data[,1])*-1
      x.offset<- abs(min(lcd))
      lcd<- lcd+x.offset
    } else {
      lcd<- log(data[,1])
    }
    lse<- sqrt((data[,2]/data[,1])^2 + sigmab^2)
  } else {
    lcd<- data[,1]
    lse<- sqrt(data[,2]^2 + sigmab^2)
  }
  
  # create new data frame with DE and combined relative error
  dat<- cbind(lcd, lse)
  
  # get the maximum likelihood estimate
  ests<- Get_mle(dat, xlb, xub, gamma.init, sigma.init, p0.init, mu.init)
  
  # check if any standard errors are NA or NaN
  coef_err<- t(as.data.frame(summary(ests)@coef[,2]))
  if(debug) print(summary(ests))
  if(any(is.nan(coef_err))) {
    coef_err[which(is.nan(coef_err))]<- t(as.data.frame(ests@coef))/100
  }
  
  # calculate profile log likelihoods
  prof<- profile(ests,
                 std.err = as.vector(coef_err),
                 try_harder = TRUE,
                 quietly = TRUE,
                 tol.newmin = Inf,
                 skiperrs = TRUE,
                 control = list(maxit = 1000),
                 prof.upper=xub, 
                 prof.lower=xlb)
  
  # calculate Bayesian Information Criterion (BIC)
  BIC<- BIC(ests)
  
  # retrieve results from mle2-object
  pal<- if(log==TRUE) {
    if(invert==TRUE) {
      exp((coef(ests)[["gamma"]]-x.offset)*-1)
    } else {
      exp(coef(ests)[["gamma"]])
    }
  } else {
    coef(ests)[["gamma"]]
  }
  sig<- coef(ests)[["sigma"]]
  p0end<- coef(ests)[["p0"]]
  
  if(par==4) {
    muend<- ifelse(log, exp(coef(ests)[["mu"]]), coef(ests)[["mu"]])
  } else {
    muend<- NA
  }
  
  ##============================================================================##
  ## ERROR CALCULATION
  
  #### METHOD 1: follow the instructions of Galbraith & Roberts (2012) ####
  # "If the likelihood profile is symmetrical about the parameter, an approximate standard error 
  #  can be calculated by dividing the length of this interval by 3.92"
  conf<- as.data.frame(confint(ests, tol.newmin = Inf, quietly=TRUE))
  
  if(invert==TRUE) {
    conf[1,]<- (conf[1,]-x.offset)*-1
    t<- conf[1,1]
    conf[1,1]<- conf[1,2]
    conf[1,2]<- t
  }
  
  gamma_err<- if(log==TRUE) {
    (exp(conf["gamma",2])-exp(conf["gamma",1]))/3.92 
  }
  else {
    (conf["gamma",2]-conf["gamma",1])/3.92
  }
  
  ##============================================================================##
  ## AGGREGATE RESULTS
  
  summary<- data.frame(de=pal, 
                       de_err=gamma_err, 
                       "ci_lower"=ifelse(log, exp(conf["gamma",1]), conf["gamma",1]),
                       "ci_upper"=ifelse(log, exp(conf["gamma",2]), conf["gamma",2]),
                       par=par,
                       sig=sig, 
                       p0=p0end,
                       mu=muend,
                       Lmax=-ests@min, 
                       BIC=BIC)
  call<- sys.call()
  args<- list(log=log, sigmab=sigmab, bootstrap=bootstrap, 
              boundary.gamma=boundary.gamma, 
              boundary.sigma=boundary.sigma,
              boundary.mu=boundary.mu,
              init.values=init.values, 
              bs.M=M, bs.N=N, bs.h=h, sigmab.sd=sigmab.sd)
  
  ##============================================================================##
  ## BOOTSTRAP
  ##============================================================================##
  
  # create normal distribution for sigmab
  if(bootstrap == TRUE) {
    # header
    cat(paste("\n   --- RECYCLED BOOTSTRAP ---- \n"))
    
    # save original sigmab value
    sigmab.global<- sigmab
    
    # define normal dist. parameters for sigmab
    sigmab.mean<- sigmab
    sigmab.sd<- sigmab.sd
    
    # empty variables
    n<- length(data[,1])
    b2Pmatrix<- matrix(NA, N, n)
    b2mamvec<- matrix(NA, N, 1)
    pairs<- matrix(NA, M, 2)
    p0.pairs<- matrix(NA, M, 2)
    
    # KDE bandwidth
    h<- h
    
    ## --------- N SECOND LEVEL BOOTSTRAPS -------------- ##
    for(i in 1:N) {
      if(i == 1) {
        cat(paste("\n"))
        cat(paste("   Calculating", N, "second level bootstraps. Please wait... \n"))
        
        #get starting time
        time.start<- Sys.time()
      }
      if(i == 10) {
        #get stop time
        time.stop<- Sys.time()
        # calculate time needed for one iteration
        time.elapsed<- abs(as.double(time.start - time.stop))/10
        # estimate time till second level bootstraps are finished
        ETA<- Sys.time()+time.elapsed*N
        # tell user about time
        write(paste("   Estimated time needed:", round(time.elapsed*N), "s | Finished at", ETA), file = "")
        # create progress bar
        pb <- txtProgressBar(min = 0, max = N, char = "#", style = 3)
      }
      # draw random sigmab
      sigmab<- rnorm(1, sigmab.mean, sigmab.sd)
      # n random integers with replacement
      R<- sample(x=n, size=n, replace=TRUE)
      # recored frequencies of each n, used in calc of product term
      for(k in 1:n) {
        b2Pmatrix[i, k]<- sum(R == k)
      }
      
      # create bootstrap replicate
      Rde<- data[R,]
      de2<- Rde
      
      # combine errors
      if(log==TRUE) { 
        lcd<- log(de2[,1])
        lse<- sqrt((de2[,2]/de2[,1])^2 + sigmab^2)
      } else {
        lcd<- de2[,1]
        lse<- sqrt(de2[,2]^2 + sigmab^2)
      }
      
      # create new data frame with DE and combined relative error
      dat<- cbind(lcd, lse)
      
      # get the maximum likelihood estimate
      ests<- Get_mle(dat, xlb, xub, gamma.init, sigma.init, p0.init)
      
      # check if any standard errors are NA or NaN
      coef_err<- t(as.data.frame(summary(ests)@coef[,2]))
      if(any(is.nan(coef_err))) {
        coef_err[which(is.nan(coef_err))]<- t(as.data.frame(ests@coef))/100
      }
      
      # save gamma to storage matrix
      if(log == TRUE) {
        if(invert==TRUE) {
          b2mamvec[i,1]<- exp((coef(ests)[["gamma"]]-x.offset)*-1)
        } else {
          b2mamvec[i,1]<- exp(coef(ests)[["gamma"]])
        }
      } else {
        b2mamvec[i,1]<- coef(ests)[["gamma"]]
      }
      
      if(i > 10) {
        # update progress bar
        setTxtProgressBar(pb, i)
      }
    }
    # close progress bar
    close(pb)
    
    ## --------- M FIRST LEVEL BOOTSTRAPS -------------- ##
    for(i in 1:M) {
      if(i == 1) {
        cat(paste("\n"))
        cat(paste("   Calculating", M, "first level bootstraps. Please wait... \n"))
        #get starting time
        time.start<- Sys.time()
      }
      if(i == 10) {
        #get stop time
        time.stop<- Sys.time()
        # calculate time needed for one iteration
        time.elapsed<- abs(as.double(time.start - time.stop))/10
        # estimate time till second level bootstraps are finished
        ETA<- Sys.time()+time.elapsed*N
        # tell user about time
        write(paste("   Estimated time needed:", round(time.elapsed*N), "s | Finished at", ETA), file = "")
        # create progress bar
        pb <- txtProgressBar(min = 0, max = M, char = "#", style = 3)
      }
      # draw random sigmab
      sigmab<- rnorm(1, sigmab.mean, sigmab.sd)
      # n random integers with replacement
      R<- sample(x=n, size=n, replace=TRUE)
      # create bootstrap replicates
      Rde<- data[R,]
      de2<- Rde
      
      # combine errors
      if(log==TRUE) { 
        lcd<- log(de2[,1])
        lse<- sqrt((de2[,2]/de2[,1])^2 + sigmab^2)
      } else {
        lcd<- de2[,1]
        lse<- sqrt(de2[,2]^2 + sigmab^2)
      }
      
      # create new data frame with DE and combined relative error
      dat<- cbind(lcd, lse)
      
      # get the maximum likelihood estimate
      ests<- Get_mle(dat, xlb, xub, gamma.init, sigma.init, p0.init)
      
      # check if any standard errors are NA or NaN
      coef_err<- t(as.data.frame(summary(ests)@coef[,2]))
      if(any(is.nan(coef_err))) {
        coef_err[which(is.nan(coef_err))]<- t(as.data.frame(ests@coef))/100
      }
      
      # save gamma to storage matrix
      if(log == TRUE) {
        if(invert == TRUE) {
          theta<- exp((coef(ests)[["gamma"]]-x.offset)*-1)
        } else {
          theta<- exp(coef(ests)[["gamma"]]) 
        }
      } else {
        theta<- coef(ests)[["gamma"]]
      }
      
      bs.p0<- coef(ests)[["p0"]]
      
      # kernel density estimate
      f<- density(x=de2[,1], kernel="gaussian", bw=h)
      f<- approx(x=f$x, y=f$y, xout=de2[,1])
      
      # convert to matrix
      f<- cbind(f$x,f$y)
      pStarTheta<- as.vector(f[,2]/sum(f[,2]))
      thetavec<- matrix(theta, N, 1)    
      kdthis<- (thetavec-b2mamvec)/h
      kd1<- dnorm(kdthis)
      
      # the product term
      Pmat<- pStarTheta/(1/n)
      Pmat<- matrix(t(Pmat), N, n, byrow=TRUE)
      prodterm<- apply(Pmat^b2Pmatrix, 1, prod)
      
      kd2<- kd1*prodterm
      kd<- sum(kd2)
      likelihood<- (1/(N*h))*kd
      pairs[i,]<- c(theta, likelihood)
      p0.pairs[i,]<- c(theta, bs.p0)
      
      if(i > 10) {
        # update progress bar
        setTxtProgressBar(pb, i)
      }
    }
    # close progress bar
    close(pb)
    
    ## --------- FIT POLYNOMIALS -------------- ##
    # polynomial fits of increasing degrees
    poly.three<- lm(pairs[,2] ~ poly(pairs[,1], degree=3, raw=TRUE)) # 3-deg. poly
    poly.four<- lm(pairs[,2] ~ poly(pairs[,1], degree=4, raw=TRUE))  # 4-deg. poly
    poly.five<- lm(pairs[,2] ~ poly(pairs[,1], degree=5, raw=TRUE))  # 5-deg. poly
    poly.six<- lm(pairs[,2] ~ poly(pairs[,1], degree=6, raw=TRUE))   # 6-deg. poly
  }
  
  ##============================================================================##
  ## CONSOLE PRINT
  ##============================================================================##
  
  if(verbose==TRUE) {
    if(bootstrap==FALSE) {
      cat("\n----------- meta data -----------\n")
      print(data.frame(n=length(data[,1]),
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
      
    } else if(bootstrap==TRUE) {
      # TODO: at least some text output would be nice for bootstrap
    }
  }
  
  ##============================================================================##
  ## RETURN VALUES
  ##============================================================================##
  
  if(bootstrap==FALSE) {
    pairs<- NULL
    p0.pairs<- NULL
    poly.three<- NULL
    poly.four<- NULL
    poly.five<- NULL
    poly.six<- NULL
  }
  
  newRLumResults.calc_MinDose<- set_RLum.Results(
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
                                   poly.six = poly.six))))
  
  ##=========##
  ## PLOTTING
  if(plot==TRUE) {
    try(plot_RLum.Results(newRLumResults.calc_MinDose, ...))
  }
  
  invisible(newRLumResults.calc_MinDose)
  ### Returns a plot (optional) and terminal output. In addition an 
  ### \code{\linkS4class{RLum.Results}} object is 
  ### returned containing the following elements:
  ###
  ### \item{summary}{\link{data.frame} summary of all relevant model results.}
  ### \item{data}{\link{data.frame} original input data}
  ### \item{args}{\link{list} used arguments}
  ### \item{call}{\link{call} the function call}
  ### \item{mle}{\link{mle2} mle2 object containing the maximum log likelhood functions for all parameters}
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
  ## where \code{gamma = mu}. For \code{par=4} the 4-parametric model is applied
  ## instead.\cr\cr
  ## \bold{(Un-)logged model} \cr\cr
  ## In the original version of the three-parameter minimum dose model, the 
  ## basic data are the natural logarithms of the De estimates and relative 
  ## standard errors of the De estimates. This model will be applied if 
  ## \code{log = TRUE}. \cr\cr
  ## If \code{log = FALSE}, the modified un-logged model will be applied 
  ## instead. This has essentially the same form as the original version. 
  ## \code{gamma} and \code{sigma} are in Gy and \code{gamma} becomes the
  ## minimum true dose in the population. \cr\cr
  ## While the original (logged) version of the mimimum dose model may be
  ## appropriate for most samples (i.e. De distributions), the modified 
  ## (un-logged) version is specially designed for modern-age and young
  ## samples containing negative, zero or near-zero De estimates (Arnold 
  ## et al. 2009, p. 323). \cr\cr
  ## \bold{Boundaries} \cr\cr  
  ## Depending on the data, the upper and lower bounds for \emph{gamma}, \emph{mu}, 
  ## \emph{sigma} and \emph{p0} need to be specified. If no values are provided
  ## for \code{boundaris} the function tries to estimate reasonable boundaries.
  ## However, these might not be appropriate in all cases and 
  ## if the final estimate of any of these parameters is on the boundary, 
  ## make sure to adjust the boundaries via \code{boundaries}
  ## \cr\cr
  ## \bold{Initial values} \cr\cr
  ## The log likelihood calculations use the \link{optim} function with the
  ## \code{L-BFGS-B} method. 
  ## Accordingly, initial values for the four parameters need to be specified.
  ## IF no values are provided for \code{init.values} reasonable starting 
  ## values are estimated. If the final estimates of \emph{gamma}, \emph{mu}, 
  ## \emph{sigma} and \emph{p0} are totally of, consider providing custom
  ## starting values via \code{init.values}.
  
  
  ##references<<
  ## Arnold, L.J., Roberts, R.G., Galbraith, R.F. & DeLong, S.B., 2009. A revised
  ## burial dose estimation procedure for optical dating of young and modern-age 
  ## sediments. Quaternary Geochronology, 4, pp. 306-325. \cr\cr
  ## Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission 
  ## track ages. Nuclear Tracks Radiation Measurements, 4, pp. 459-470. \cr\cr
  ## Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley, J.M., 
  ## 1999. Optical dating of single grains of quartz from Jinmium rock shelter, 
  ## northern Australia. Part I: experimental design and statistical models. 
  ## Archaeometry, 41, pp. 339-364. \cr\cr
  ## Galbraith, R.F., 2005. Statistics for Fission Track Analysis, Chapman & 
  ## Hall/CRC, Boca Raton. \cr\cr
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose
  ## and error calculation and display in OSL dating: An overview and some
  ## recommendations. Quaternary Geochronology, 11, pp. 1-27. \cr\cr
  ## \bold{Further reading} \cr\cr
  ## Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain 
  ## equivalent dose (De) distributions: Implications for OSL dating of sediment 
  ## mixtures. Quaternary Geochronology, 4, pp. 204-230. \cr\cr
  ## Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain 
  ## quartz De distributions and an assessment of procedures for estimating burial
  ## dose. Quaternary Science Reviews, 25, pp. 2475-2502. \cr\cr
  ## Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial
  ## archives using robust OSL chronologies. Quaternary Geochronology, 12, 
  ## pp. 98-106. \cr\cr
  ## Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the
  ## reproducibility and accuracy of optical dating of fluvial deposits. 
  ## Quaternary Geochronology, 1, pp. 109-120. \cr\cr
  ## Rodnight, H., 2008. How many equivalent dose values are needed to obtain a
  ## reproducible distribution?. Ancient TL, 26, pp. 3-10. \cr\cr
  
  ##note<<
  ## The default boundary and starting values for \emph{gamma}, \emph{mu}, 
  ## \emph{sigma} and \emph{p0} may only be appropriate for some De data sets 
  ## and may need to be changed for other data. This is especially true when
  ## the un-logged version is applied.
  
  ##seealso<<
  ## \code{\link{calc_CentralDose}},
  ## \code{\link{calc_CommonDose}}, \code{\link{calc_FiniteMixture}},
  ## \code{\link{calc_FuchsLang2001}}, \code{\link{calc_MaxDose}}
  
  
  #### EXAMPLE ####
},ex=function(){
  
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  
  # apply the un-logged, 3-parametric minimum age model
  calc_MinDose(data = ExampleData.DeValues, par = 3, sigmab = 0.2, log = FALSE)
  
  # re-run the model, but save results to a variable
  mam<- calc_MinDose(data = ExampleData.DeValues, par = 3, sigmab = 0.2, log = FALSE, plot = FALSE)
  
  # show summary table
  get_RLum.Results(mam, "summary")
  
  # plot the log likelihood profiles retroactively
  plot_RLum.Results(mam)
})



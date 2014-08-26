calc_MinDose4<- structure(function( # Apply the (un-)logged four parameter minimum age model (MAM 4) after Galbraith et al. (1999) to a given De distribution
  ### Function to fit the (un-)logged four parameter minimum dose model (MAM 4)
  ### to De data.
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany) \cr
  ## Based on a rewritten S script of Rex Galbraith, 2010 \cr\cr
  ## The bootstrap approach is based on a rewritten MATLAB script of Alastair Cunningham. \cr
  ## Alastair Cunningham is thanked for his help in implementing and cross-checking the code.\cr
  
  ##section<<
  ## version 0.3
  # ===========================================================================
  
  input.data, 
  ### \code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
  ### for \code{data.frame}: two columns with De \code{(input.data[,1])} and
  ### De error \code{(values[,2])}
  sigmab, 
  ### \code{\link{numeric}}  (\bold{required}): spread in De values given as a 
  ### fraction (e.g. 0.2). This value represents the expected overdispersion in
  ### the data should the sample be well-bleached (Cunningham & Walling 2012, 
  ### p. 100).
  log=TRUE, 
  ### \code{\link{logical}} (with default): fit the (un-)logged three parameter 
  ### minimum dose model to De data
  sample.id="unknown sample",
  ### \code{\link{character}} (with default): sample id
  bootstrap,
  ## code{\link{logical}}: Apply the recycled bootstrap approach of Cunningham & Wallinga (2012).
  boundary.gamma,
  ### \code{\link{numeric}}: a vector of in the form of
  ### \code{c(lower.boundary, upper.boundary)}. If no values are provided 
  ### reasonable values are tried to be estimated by the input data.
  boundary.sigma,
  ### \code{\link{numeric}}: a vector of in the form of
  ### \code{c(lower.boundary, upper.boundary)}. If no values are provided 
  ### reasonable values are tried to be estimated by the input data.
  boundary.mu,
  ### \code{\link{numeric}}: a vector of in the form of
  ### \code{c(lower.boundary, upper.boundary)}. If no values are provided 
  ### reasonable values are tried to be estimated by the input data.
  init.values=c(10, 10, 0.6, 0.01), 
  ### \code{\link{numeric}} (with default): starting values for gamma, sigma and p0.
  ### Custom values need to be provided in a vector of length three in the form of
  ### \code{c(gamma, mu, sigma, p0)}.
  ignore.NA = FALSE,
  ### \code{\link{logical}} (with default): ignore NA values during log
  ### likelihood calculations. See details.
  calc.ProfileLikelihoods=TRUE, 
  ### \code{\link{logical}} (with default): calculate profile log likelihood
  ### functions for gamma, mu, sigma, p0. See \code{output.indices}.
  console.ProfileLikelihoods=FALSE, 
  ### \code{\link{logical}} (with default): print profile log likelihood
  ### functions for gamma, mu, sigma, p0 to console.
  console.extendedOutput=FALSE, 
  ### \code{\link{logical}} (with default): extended terminal output
  output.plot=TRUE, 
  ### \code{\link{logical}} (with default): plot output
  ### (\code{TRUE}/\code{FALSE})
  ...
  ### further arguments for bootstrapping (\code{bs.M, bs.N, bs.h, sigmab.sd}). 
  ### See details for their usage.
  ){                      
  
  ##=============================================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ##=============================================================================================##
  
  if(missing(input.data)==FALSE){
    
    if(is(input.data, "data.frame") == FALSE & is(input.data,
                                                  "RLum.Results") == FALSE){
      
      stop("[calc_FiniteMixture] Error: 'input.data' object has to be of type 
           'data.frame' or 'RLum.Results'!")
      
    }else{
      
      if(is(input.data, "RLum.Results") == TRUE){
        
        input.data <- get_RLum.Results(input.data, 
                                       signature(object = "De.values"))
        
      }
    }
  }  
  
  try(colnames(input.data)<- c("ED","ED_Error"),silent=TRUE)
  
  if(colnames(input.data[1])!="ED" || colnames(input.data[2])!="ED_Error") { 
    cat(paste("Columns must be named 'ED' and 'ED_Error'"), fill = FALSE)
    stop(domain = NA)
  }
  
  if(sigmab <0 | sigmab >1) { 
    cat(paste("sigmab needs to be given as a fraction between", 
              "0 and 1 (e.g. 0.2)"), fill = FALSE)
    stop(domain = NA)
  }
  
  
  
  if(missing(boundary.gamma) == FALSE) {
    
    if(length(boundary.gamma) != 2) {
      cat(paste("boundary.gamma needs to be a vector of length = 2, i.e. ",
                "c(lower.boundary, upper.boundary"), fill = FALSE)
      stop(domain=NA)
    }
    else{
      if(boundary.gamma[1] >= boundary.gamma[2]) {
        cat(paste("the lower boundary for gamma needs to be smaller than",
                  "the upper boundary"), fill = FALSE)
        stop(domain=NA)
      }
    }
  }
  
  if(missing(boundary.sigma) == FALSE) {
    if(length(boundary.sigma) != 2) {
      cat(paste("boundary.sigma needs to be a vector of length = 2, i.e. ",
                "c(lower.boundary, upper.boundary"), fill = FALSE)
      stop(domain=NA)
    }
    else{
      if(boundary.sigma[1] >= boundary.sigma[2]) {
        cat(paste("the lower boundary for sigma needs to be smaller than",
                  "the upper boundary"), fill = FALSE)
        stop(domain=NA)
      }
    }
  }
  
  
  ##============================================================================##
  ## ESTIMATE START PARAMETERS
  ##============================================================================##
  
  # TO BE DONE
  
  ##============================================================================##
  ## ESTIMATE BOUNDARY PARAMETERS
  ##============================================================================##
  
  ## BOUNDARIES FOR GAMMA
  
  if(missing(boundary.gamma) == FALSE) {
    
    # retrieve lower and upper boundary for gamma
    gamma.xlb<- boundary.gamma[1]
    gamma.xub<- boundary.gamma[2]
    
    
  } 
  else { # 0.1, max De
    
    gamma.xlb<- min(input.data[,1]/10)
    gamma.xub<- max(input.data[,1]*1.1)
    
  }
  
  ## BOUNDARIES FOR SIGMA
  if(missing(boundary.sigma) == FALSE) {
    
    # retrieve lower and upper boundary for sigma
    sigma.xlb<- boundary.sigma[1]
    sigma.xub<- boundary.sigma[2]
  }
  else { # default values by Rex Galbraith: 0.001, 5
    
    sigma.xlb<- 0.001
    sigma.xub<- 5
    
  }
  
  ## BOUNDARIES FOR MU
  if(missing(boundary.mu) == FALSE) {
    
    # retrieve lower and upper boundary for sigma
    mu.xlb<- boundary.mu[1]
    sigma.xub<- boundary.mu[2]
  }
  else { 
    
    mu.xlb<- 10
    mu.xub<- max(input.data[,1]*1.1)
    
  }
  
  
  ## THIS FUNCTION IS CALLED WHEN ONE OR MORE ESTIMATES REACHED THE BOUNDARY
  readjust.boundaries<- function (s.xlb, s.xub,
                                  g.xlb, g.xub,
                                  m.xlb, m.xub,
                                  Bcheck) {
    
    if(Bcheck[".xlb","sigma"]==TRUE) {
      sigma.xlb<- s.xlb*0.9
    }
    if(Bcheck[".xub","sigma"]==TRUE) {
      sigma.xub<- s.xub*1.1
    }
    if(Bcheck[".xlb","gamma"]==TRUE) {
      gamma.xlb<- g.xlb*0.9
    }
    if(Bcheck[".xub","gamma"]==TRUE) {
      gamma.xub<- g.xub*1.1
    }
    if(Bcheck[".xlb","mu"]==TRUE) {
      mu.xlb<- m.xlb*0.9
    }
    if(Bcheck[".xub","mu"]==TRUE) {
      mu.xub<- m.xub*1.1
    }
    
    return(data.frame(sigma.xlb, sigma.xub,
                      gamma.xlb, gamma.xub,
                      mu.xlb, mu.xub))
  }
  
  
  ##============================================================================##
  ## CALCULATIONS
  ##============================================================================##
  
  # this calculates ln(1-Phi(x)) robustly, where 
  # Phi(x) is the standard normal c.d.f  
  
  lnnprob.f<- function(x) {
    
    
    
    logsqrt2pi<- 0.5*log(2*pi)
    
    b1  <-  3.8052e-8
    b2  <-  1.00000615302
    b3  <-  3.98064794e-4
    b4  <-  1.98615381364
    b5  <-  0.151679116635
    b6  <-  5.29330324926 
    b7  <-  4.8385912808
    b8  <- 15.1508972451
    b9  <-  0.742380924027
    b10 <- 30.789933034
    b11 <-  3.99019417010 
    
    lnnp<- x
    tf<-  (x<1.28)
    
    
    if(ignore.NA == FALSE) {
      if(any(is.na(tf) == TRUE)) {
        cat(paste("\n WARNING: log likelihood calculation produced NA values.",
                  "\n  To omit NA values set ignore.NA = TRUE. Use with caution!"),
            fill = FALSE)    
      }
    }
    
    if(ignore.NA == TRUE) {
      if(any(is.na(tf) == TRUE)) {
        tf<- na.omit(tf)
        options(warn = -1)
      }
    }
    
    if(sum(tf)>0) {
      z<- x[tf]
      lnnp[tf]<- log(1-pnorm(z))
    }
    
    tf<- (!tf)
    
    if(sum(tf)>0) {
      z<- x[tf]
      fz <- 1/(z+b3+b4/(z-b5+b6/(z+b7-b8/(z+b9+b10/(z+b11)))))
      hz <- 1.0/(1.0-b1/z+b2*fz/z)
      lnnp[tf] <-  log(hz)-log(z)-0.5*z*z-logsqrt2pi
    }
    return(lnnp)
  }
  
  # neglik.f calculates minus the log-likelihood of the data from 
  # the parameters and the data
  
  neglik.f<- function(param,dat,rep){
    
    # this calculates the negative of the log likelihood of the  
    # data (dat,rep) for a given set of parameters (param)
    # param is the vector of model parameters: 
    #        (gamma, mu, sigma, p0)
    # datmat is a nx2 matrix of data: ld, seld (including sigma_b)
    # recover the data
    zi<- dat[,1]
    si<- dat[,2]
    n<- length(zi)
    
    # recover the parameters
    gamma<- param[1]
    mu<-    param[2]
    sigma<- param[3]
    p0<-    param[4]
    
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
    lf2i<- lf2i + lnnprob.f(res0) - lnnprob.f(res1)
    llik<- log( exp(lf1i) + exp(lf2i) )
    negll<- -sum(llik)
    
    return(negll)
  }
  
  ##============================================================================##
  ## MAIN PROGRAM
  ##============================================================================##
  apply.MAM<- function(input.data, sigmab, ...) {
    
    extraArgs <- list(...)
    
    ## set plot main title
    if("new.bounds" %in% names(extraArgs)) {
      #print(extraArgs) # DEBUG
      sigma.xlb<- as.double(extraArgs$new.bounds[1,"sigma.xlb"])
      sigma.xub<- as.double(extraArgs$new.bounds[1,"sigma.xub"])
      gamma.xlb<- as.double(extraArgs$new.bounds[1,"gamma.xlb"])
      gamma.xub<- as.double(extraArgs$new.bounds[1,"gamma.xub"])
      mu.xlb<- as.double(extraArgs$new.bounds[1,"mu.xlb"])
      mu.xub<- as.double(extraArgs$new.bounds[1,"mu.xub"])
    } 
    
    # read in the data and print the number of grains
    if(log==TRUE) {
      lcd<- log(input.data$ED)
      lse<- sqrt( (input.data$ED_Error/input.data$ED)^2 + sigmab^2 )
    }
    else {
      lcd<- input.data$ED
      lse<- sqrt( (input.data$ED_Error)^2 + sigmab^2 )
    }
    
    datmat<- cbind(lcd,lse)
    
    if(log==TRUE) {
      gamma.init<- log(init.values[1])
      mu.init<- log(init.values[2])
    }
    else {
      gamma.init<- init.values[1]
      mu.init<- init.values[2]
    }
    
    sigma.init<- init.values[3]
    
    p0.init<- init.values[4]
    
    x0<- c(gamma.init, mu.init, sigma.init, p0.init)
    
    if(log==TRUE) {
      xlb<- c( log(gamma.xlb), log(mu.xlb), sigma.xlb, 0.0001 )
      xub<- c( log(gamma.xub), log(mu.xub), sigma.xub, 0.9999 )
    }
    else {
      xlb<- c( gamma.xlb, mu.xlb, sigma.xlb, 0.0001 )
      xub<- c( gamma.xub, mu.xub, sigma.xub, 0.9999 )
    }
    
    
    # parameters to control printing from nlminb
    ncalls<- 0
    ncmod<- 0
    ipr<- 50
    prstat<- c(ncalls,ncmod,ipr)
    prfile<- F
    
    # maximise the likelihood
    opt.param<- nlminb(start = x0, objective = neglik.f, scale = 1,
                       lower = xlb, upper = xub, dat = datmat,
                       control = list(iter.max = 1000, eval.max = 1000))
    
    # print out the maxmimum likelihood estimates
    mlest<- opt.param$par
    gamma<- mlest[1]
    mu<- mlest[2]
    sigma<-  mlest[3]
    p0<-  mlest[4]
    
    maxlik<- -opt.param$objective
    bic<- -2*maxlik + 4*log(length(lcd))
    get("prstat",mode="numeric")
    ncalls<- prstat[1]
    
    # return values
    MAM.res<- data.frame(maxlik=maxlik,
                         bic=bic,
                         gamma=gamma,
                         sigma=sigma,
                         p0=p0,
                         mu=mu)
    
    # this is a dummy variable for compatibility reasons
    Bmess<- NA
    
    # check if any of the estimates are on the boundary
    bcheck<- data.frame(gamma=c(all.equal(gamma.xub,if(log==TRUE){exp(gamma)}
                                          else{gamma})==TRUE,
                                all.equal(gamma.xlb,if(log==TRUE){exp(gamma)}
                                          else{gamma})==TRUE),
                        sigma=c(all.equal(sigma.xub,sigma)==TRUE,
                                all.equal(sigma.xlb,sigma)==TRUE),
                        mu=c(all.equal(mu.xub,if(log==TRUE){exp(mu)}
                                       else{mu})==TRUE,
                             all.equal(mu.xlb,if(log==TRUE){exp(mu)}
                                       else{mu})==TRUE))
    rownames(bcheck)<- c(".xub",".xlb")
    
    
    return(list(lcd, MAM.res, datmat, x0, xlb, mlest, xub, Bmess, bcheck, 
                sigma.xlb, sigma.xub, gamma.xlb, gamma.xub, mu.xlb, mu.xub))
  }
  
  
  
  ##============================================================================##
  ## BOOTSTRAP
  ##============================================================================##
  
  # create normal distribution for sigmab
  
  if(missing(bootstrap) == FALSE) {
    
    extraArgs <- list(...)
    
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
      h<- (sd(input.data[,1])/sqrt(length(input.data[,1])))*2
    }
    
    # standard deviation of sigmab 
    if("sigmab.sd" %in% names(extraArgs)) {
      sigmab.sd<- extraArgs$sigmab.sd
    } else {
      sigmab.sd<- 0.04
    }
    
    if(bootstrap == TRUE) {
      
      # header
      cat(paste("\n   --- RECYCLED BOOTSTRAP ---- \n"))
      
      # save original sigmab value
      sigmab.global<- sigmab
      
      # define normal dist. parameters for sigmab
      sigmab.mean<- sigmab
      sigmab.sd<- sigmab.sd
      
      # empty variables
      n<- length(input.data[,1])
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
        Rde<- input.data[R,]
        de2<- Rde
        
        # apply MAM and get results
        mam<- apply.MAM(de2, sigmab) 
        
        # save gamma to storage matrix
        if(log == TRUE) {
          b2mamvec[i,1]<- exp(mam[[2]]$gamma)
        } else {
          b2mamvec[i,1]<- mam[[2]]$gamma
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
        Rde<- input.data[R,]
        de2<- Rde
        
        # apply MAM and get results
        mam<- apply.MAM(de2, sigmab)
        
        
        ## --------- CHECK AND READJUST BOUNDARIS ------------ ##
        
        # get logical matrix, which indicates whether any parameter 
        # is on the boundary
        Bcheck<- mam[[9]]
        
        # check if any parameter is on the boundary
        if(any(Bcheck == TRUE)) {
          
          # try 1000 times to produce a valid result
          for(j in 1:1000) {  
            
            if(any(Bcheck == TRUE)) {
              
              
              new.bounds<- readjust.boundaries(s.xlb = sigma.xlb, 
                                               s.xub = sigma.xub,
                                               g.xlb = gamma.xlb,
                                               g.xub = gamma.xub,
                                               m.xlb = mu.xlb,
                                               m.xub = mu.xub,
                                               Bcheck)
              
              #mam<- apply.MAM(input.data, sigmab, new.bounds=new.bounds) #ORIGINAL
              mam<- apply.MAM(de2, sigmab, new.bounds=new.bounds)
              
              # get boundary values
              sigma.xlb<- mam[[10]]
              sigma.xub<- mam[[11]]
              gamma.xlb<- mam[[12]]
              gamma.xub<- mam[[13]]
              mu.xlb<- mam[[14]]
              mu.xub<- mam[[15]]
              
              # get TRUE/FALSE boundary matrix
              Bcheck<- mam[[9]]
            }
            else {
              # break loop if none of the parameters is on the loop
              break
            }
          } #EndOf::LOOP (1000 iter)
        }#EndOf::Bcheck matrix check
        
        
        # save gamma
        if(log == TRUE) {
          theta<- exp(mam[[2]]$gamma)
        } else {
          theta<- mam[[2]]$gamma
        }
        
        bs.p0<- mam[[2]]$p0
        
        
        # kernel density estimate
        f<- density(x=de2[,1], kernel="gaussian", bw=h)
        f<- approx(x=f$x, y=f$y, xout=de2[,1])
        
        # convert to matrix
        f<- cbind(f$x,f$y)
        
        pStarTheta<- as.vector(f[,2]/sum(f[,2]))
        
        
        thetavec<- matrix(theta, N, 1)    
        #kdthis<- (b2mamvec-thetavec)/h # SHOULD IT BE THE OTHER WAY AROUND?
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
      
      
      ## --------- PLOT "RECYCLE" BOOTSTRAP RESULTS ------------ ##
      
      for(i in 1:4) {
        
        # save previous plot parameter and set new ones
        .pardefault<- par(no.readonly = TRUE)
        
        # set plot layout
        layout(matrix(c(1,1,2,3)),3,1)
        par(cex = 0.8)
        
        ## ----- LIKELIHOODS
        
        # set margins (bottom, left, top, right)
        par(mar=c(1,5,5,3))
        
        # sort De and likelihoods by De (increasing)
        pairs<- pairs[order(pairs[,1]),]
        
        # remove invalid NA values
        pairs<- na.omit(pairs)
        
        # sort corresponding p0 values
        p0.pairs<- p0.pairs[order(p0.pairs[,1]),]
        
        # remove invalid NA values
        p0.pairs<- na.omit(p0.pairs)
        
        plot(x=pairs[,1],
             y=pairs[,2],
             xlab="Equivalent Dose [Gy]",
             ylab="Likelihood",
             ylim=c(0, as.double(quantile(pairs[,2],probs=0.98))),
             main="Recycled bootstrap MAM-4")
        
        # add subtitle
        mtext(as.expression(bquote(italic(M) == .(M) ~ "|" ~
                                     italic(N) == .(N) ~ "|" ~
                                     italic(sigma[b])  == .(sigmab.global) ~ 
                                     "\u00B1" ~ .(sigmab.sd) ~ "|" ~
                                     italic(h) == .(round(h,1))
        )
        ),           
        side = 3, line = 0.3, adj = 0.5, 
        cex = 0.9)
        
        
        points(x=pairs[,1], y=pairs[,2], pch=1, col = "grey66")
        
        
        
        # polynomial fits of increasing degrees
        poly.three<- lm(pairs[,2] ~ poly(pairs[,1], degree=3, raw=TRUE)) # 3-deg. poly
        poly.four<- lm(pairs[,2] ~ poly(pairs[,1], degree=4, raw=TRUE))  # 4-deg. poly
        poly.five<- lm(pairs[,2] ~ poly(pairs[,1], degree=5, raw=TRUE))  # 5-deg. poly
        poly.six<- lm(pairs[,2] ~ poly(pairs[,1], degree=6, raw=TRUE))   # 6-deg. poly
        
        if(i == 1) {
          poly.curve<- function(x) poly.three$coefficient[4]*x^3 + poly.three$coefficient[3]*x^2 + poly.three$coefficient[2]*x + poly.three$coefficient[1]
        } 
        if(i == 2) {
          poly.curve<- function(x) poly.four$coefficient[5]*x^4 + poly.four$coefficient[4]*x^3 + poly.four$coefficient[3]*x^2 + poly.four$coefficient[2]*x + poly.four$coefficient[1]
        }
        if(i == 3) {
          poly.curve<- function(x) poly.five$coefficient[6]*x^5 + poly.five$coefficient[5]*x^4 + poly.five$coefficient[4]*x^3 + poly.five$coefficient[3]*x^2 + poly.five$coefficient[2]*x + poly.five$coefficient[1]
        }
        if(i == 4) {
          poly.curve<- function(x) poly.six$coefficient[7]*x^6 + poly.six$coefficient[6]*x^5 + poly.six$coefficient[5]*x^4 + poly.six$coefficient[4]*x^3 + poly.six$coefficient[3]*x^2 + poly.six$coefficient[2]*x + poly.six$coefficient[1]
        }
        
        poly.lines<- list(poly.three,poly.four,poly.five,poly.six)
        
        # add polynomials to plot
        curve(poly.curve, col = "black", add = TRUE, type = "l")
        
        legend<- c("Third degree", "Fourth degree", "Fifth degree", "Sixth degree")
        
        # add legend
        legend("topright", legend = legend[i], 
               y.intersp = 1.2,
               bty = "n", 
               title = "Polynomial Fit",
               lty = 1,
               lwd= 1.5)
        
        
        ## ----- RESIDUALS
        
        # set margins (bottom, left, top, right)
        par(mar=c(3,5,2,3))
        
        plot(x = pairs[,1],
             y = residuals(poly.lines[[i]]),
             ylim = c(min(residuals(poly.lines[[i]]))*1.2,
                      as.double(quantile(residuals(poly.lines[[i]]),probs=0.99))),
             xlab = "",
             col = "grey66",
             ylab = "Fit Residual")
        
        abline(h = 0)
        
        ## ----- PROPORTIONS
        
        # set margins (bottom, left, top, right)
        par(mar=c(5,5,0,3))
        
        # plot p0 values
        plot(x=p0.pairs[,1],
             y=p0.pairs[,2]*100,
             xlab="Equivalent Dose [Gy]",
             ylab="Proportion [%]",
             ylim=c(0, max(p0.pairs[,2])*100),
             col = "grey66")
        
        
        
        # polynomial fits of increasing degrees
        #poly.three.p0<- predict(lm(p0.pairs[,2] ~ poly(p0.pairs[,1], degree=3, raw=TRUE)))
        #lines(x=p0.pairs[,1], poly.three.p0, col="black", lwd=1.5)
        
        # pair min dose and likelihoods of poly fits
        #poly.three<- data.frame(min_dose = pairs[,1], poly.three$fitted.values)
        poly.four<- data.frame(min_dose = pairs[,1], poly.four$fitted.values)
        poly.five<- data.frame(min_dose = pairs[,1], poly.five$fitted.values)
        poly.six<- data.frame(min_dose = pairs[,1], poly.six$fitted.values)
        
        # restore previous plot parameters
        par(.pardefault)
        
      }##EndOf::Plot_loop
      
    }##EndOf::Bootstrap
  }##EndOf::Bootstrap
  
  else {
    
    ## --------- NORMAL MAM-3 W/O BOOTSTRAP ------------ ##
    
    # apply MAM-3
    MAM.res<- apply.MAM(input.data, sigmab)
    
    
    
    ## --------- CHECK AND READJUST BOUNDARIS ------------ ##
    
    # get logical matrix, which indicates whether any parameter 
    # is on the boundary
    Bcheck<- MAM.res[[9]]
    
    # check if any parameter is on the boundary
    if(any(Bcheck == TRUE)) {
      
      # try 1000 times to produce a valid result
      for(i in 1:1000) {  
        
        if(any(Bcheck == TRUE)) {
          
          new.bounds<- readjust.boundaries(s.xlb = sigma.xlb, 
                                           s.xub = sigma.xub,
                                           g.xlb = gamma.xlb,
                                           g.xub = gamma.xub,
                                           m.xlb = mu.xlb,
                                           m.xub = mu.xub,
                                           Bcheck)
          
          MAM.res<- apply.MAM(input.data, sigmab, new.bounds=new.bounds)  
          
          # get boundary values
          sigma.xlb<- MAM.res[[10]]
          sigma.xub<- MAM.res[[11]]
          gamma.xlb<- MAM.res[[12]]
          gamma.xub<- MAM.res[[13]]
          mu.xlb<- MAM.res[[14]]
          mu.xub<- MAM.res[[15]]
          
          # get TRUE/FALSE boundary matrix
          Bcheck<- MAM.res[[9]]
        }
        else {
          # break loop if none of the parameters is on the boundary
          break
        }
      } #EndOf::LOOP (1000 iter)
    }#EndOf::Bcheck matrix check
    
    
    # retrieve results from MAM.res object
    # (for compatibility reasons the results are stored in individual variables)
    
    # slot 1
    lcd<- MAM.res[[1]]
    
    # slot 2
    maxlik<- MAM.res[[2]]$maxlik
    bic<- MAM.res[[2]]$bic
    gamma<- MAM.res[[2]]$gamma
    sigma<- MAM.res[[2]]$sigma
    mu<- MAM.res[[2]]$mu
    p0<- MAM.res[[2]]$p0
    x0<- MAM.res[[2]]$x0
    xlb<- MAM.res[[2]]$xlb
    xub<- MAM.res[[2]]$xub
    mlest<- MAM.res[[2]]$mlest
    
    # slot 3-8
    datmat<- MAM.res[[3]]
    x0<- MAM.res[[4]]
    xlb<- MAM.res[[5]]
    mlest<- MAM.res[[6]]
    xub<- MAM.res[[7]]
    Bmess<- MAM.res[[8]]
  } #EndOf::Normal MAM-3
  
  ##============================================================================##
  ## PROFILE LOG LIKELIHOODS
  ##============================================================================##
  
  if(calc.ProfileLikelihoods == TRUE && missing(bootstrap) == TRUE) {
    
    
    if(output.plot == TRUE) {
      # save previous plot parameter and set new ones
      .pardefault<- par(no.readonly = TRUE)
      
      par(mfrow=c(2,2),
          oma=c(9,2,9,1),
          mar=c(3.7,3.1,1.1,0.2),
          mgp=c(1.75,0.5,0),
          las=1,
          cex.axis=1.1,
          cex.lab=1.3)
    }
    
    # calculate the required profiles
    lbpar<- c("gamma","mu","sigma","p0")
    npar<- length(x0)
    
    for(j in 1:4) {
      
      
      ##============================================================================##    
      ## first pass at the profile likelihood
      
      # this does a broad sweep of the possible parameter values and
      # estimates where more intensive calculations should be made
      lowpar<- xlb[j]+(mlest[j]-xlb[j])*c(0.01,0.5,0.8,0.9,0.95,0.99)
      highpar<- mlest[j]+(xub[j]-mlest[j])*c(0.01,0.05,0.1,0.2,0.5,0.99)
      prfpar<- c(lowpar,mlest[j],highpar)
      plk<- numeric(0)
      
      if(console.ProfileLikelihoods == TRUE) {
        cat("\n\n ------calculating profiles: broad sweep-------")
        cat(paste("\n trial parameter value","|","profile log likelihood \n"))
      }
      
      for(par in prfpar )
      { 
        
        # set up the initial values and the bounds so that the 
        # profile parameter is fixed i.e. lower bd = upper bd
        if(j == 1) {
          px0<- c(par,mlest[2:npar])
          pxlb<- c(par,xlb[2:npar])
          pxub<- c(par,xub[2:npar])
        }
        if(j >1 & j <npar) {
          j1<- j-1
          j2<- j+1
          px0<- c(mlest[1:j1],par,mlest[j2:npar])
          pxlb<- c(xlb[1:j1],par,xlb[j2:npar])
          pxub<- c(xub[1:j1],par,xub[j2:npar])
        }
        if(j == npar) {
          npar1<- npar-1
          px0<- c(mlest[1:npar1],par)
          pxlb<- c(xlb[1:npar1],par)
          pxub<- c(xub[1:npar1],par)
        }
        
        # parameters to control printing from nlminb
        ncalls<-0
        ncmod<- 0
        ipr<- 0
        prstat<- c(ncalls,ncmod,ipr)
        
        
        # maximise the likelihood
        opt.param<- nlminb(start = px0, objective = neglik.f, scale = 1, 
                           lower = pxlb, upper=pxub, dat=datmat, 
                           control = list(iter.max = 1000, eval.max = 1000))
        
        # save and print the results
        proflik<- -maxlik-opt.param$objective
        plk<- c(plk,proflik)
        
        if(console.ProfileLikelihoods == TRUE) {
          cat(c("",format(round(j,0)),"   ", 
                format(round(c(par,proflik),3), nsmall=3), "\n"))
        }
      }#END OF LOOP
      
      ##=============================================================================================##
      ## second pass
      
      # this does a fine sweep of the parameter values between
      # limits derived from the broad sweep
      n<- length(prfpar)
      cnt<- (1:n)
      tf<- plk > -2.0
      n1<- min(cnt[tf])
      n2<- max(cnt[tf])
      
      if(n1>1) {
        tf[n1-1]<- TRUE
      }
      
      if(n2<n) {
        tf[n2+1]<- TRUE
      }
      
      minpar<- xlb[j]
      
      if(n1>1) {
        minpar<- min(prfpar[tf])
      }
      
      maxpar<- xub[j]
      
      if(n2<n) {
        maxpar<- max(prfpar[tf])
      }
      
      prfpar2<- seq(minpar,maxpar,length=20)
      plk2<- numeric(0)
      
      if(console.ProfileLikelihoods == TRUE) {
        cat("\n ------calculating profiles: fine sweep-------")
        cat(paste("\n trial parameter value","|","profile log likelihood \n"))
      }
      
      for(par in prfpar2 ) { 
        
        # set up the initial values and the bounds so that the 
        # profile parameter is fixed i.e. lower bd = upper bd
        if(j == 1) {
          px0<- c(par,mlest[2:npar])
          pxlb<- c(par,xlb[2:npar])
          pxub<- c(par,xub[2:npar])
        }
        if(j >1 & j <npar) {
          j1<- j-1
          j2<- j+1
          px0<- c(mlest[1:j1],par,mlest[j2:npar])
          pxlb<- c(xlb[1:j1],par,xlb[j2:npar])
          pxub<- c(xub[1:j1],par,xub[j2:npar])
        }
        if(j == npar) {
          npar1<- npar-1
          px0<- c(mlest[1:npar1],par)
          pxlb<- c(xlb[1:npar1],par)
          pxub<- c(xub[1:npar1],par)
        }
        
        # parameters to control printing from nlminb
        ncalls<-0
        ncmod<- 0
        ipr<- 0
        prstat<- c(ncalls,ncmod,ipr)
        
        # maximise the likelihood
        opt.param<- nlminb(start=px0,objective=neglik.f,scale=1,lower=pxlb,upper=pxub,
                           dat=datmat,control=list(iter.max = 1000, 
                                                   eval.max = 1000))
        
        # save and print the results
        proflik<- -maxlik-opt.param$objective
        plk2<- c(plk2,proflik)
        
        if(console.ProfileLikelihoods == TRUE) {
          cat(c("", format(round(j,0)), "   ", 
                format(round(c(par,proflik),3),nsmall=3), "\n"))
        }
      }#END OF LOOP
      
      ##============================================================================##
      ## POOLING & PLOTTING
      ##============================================================================##  
      
      # the results from the broad sweep and the fine sweep
      # are pooled, and only the data values near the  maximum likelihood
      # solution are kept and the results are plotted
      prfpar<- c(prfpar,prfpar2)
      plk<- c(plk,plk2)
      ord<- order(prfpar)
      prfpar<- prfpar[ord]
      plk<- plk[ord]
      n<- length(prfpar)
      cnt<- (1:n)
      tf<- plk>=-1.92
      n1<- min(cnt[tf])
      n2<- max(cnt[tf])
      if(n1>1) tf[n1-1]<- TRUE
      if(n2<n) tf[n2+1]<- TRUE
      prfpar<- prfpar[tf]
      plk<- plk[tf]
      
      # plotting
      if(output.plot==TRUE) {
        plot(prfpar,plk,type="n",xlab=lbpar[j],ylab="",ylim=c(-2.5,0),cex=0.8)
        lines(prfpar,plk)
        abline(h=-1.92,lty=3)
        abline(h=-0.5,lty=3)
      }
      
      # the 95% upper and lower confidence limits are calculated from the
      # profile results and the results are added to the plot
      n<- length(prfpar)
      cnt<- (1:n)
      tf<- plk>=-1.92
      n1<- min(cnt[tf])
      n2<- max(cnt[tf])
      delu<- (maxpar-minpar)/100
      
      # the lower confidence limit	
      if(n1 >1){ 
        uci<-prfpar[(n1-1):n1]
        lci<- plk[(n1-1):n1]
        u1<- approx(lci,uci,-1.92)$y
        
        if(output.plot == TRUE) {
          text(u1+delu,-2.12,format(round(u1,2)),adj=0,cex=1)
          
          if(c(j == 1 | j == 2) & log == TRUE)
          {
            text(u1+delu,-2.32,format(round(exp(u1),2)),adj=0,cex=1)
          }
        }
      }
      
      # the upper confidence limit
      if(n2 <n) {
        uci<-prfpar[n2:(n2+1)]
        lci<- plk[n2:(n2+1)]
        u2<- approx(lci,uci,-1.92)$y
        
        if(output.plot == TRUE) {
          text(u2-delu,-2.12,format(round(u2,2)),adj=1,cex=1)
          
          if(c(j == 1 | j == 2) & log == TRUE)
          {
            text(u2-delu,-2.32,format(round(exp(u2),2)),adj=1,cex=1)
          }
        }
      }
      
      if(j==1) {
        try(gul<-u1, silent=TRUE)
        try(guu<-u2, silent=TRUE)
      }
      if(j==2) {
        try(mul<-u1, silent=TRUE)
        try(muu<-u2, silent=TRUE)
      }
      
      
      # the 68% upper and lower confidence limits are calculated from the
      # profile results and the results are added to the plot
      n<- length(prfpar)
      cnt<- (1:n)
      tf<- plk>=-0.5
      n1<- min(cnt[tf])
      n2<- max(cnt[tf])
      delu<- (maxpar-minpar)/100
      
      # the lower confidence limit
      if(n1 >1){
        uci<-prfpar[(n1-1):n1]
        lci<- plk[(n1-1):n1]
        u1<- approx(lci,uci,-0.5)$y
        
        if(output.plot == TRUE) {
          text(u1+delu,-0.7,format(round(u1,2)),adj=0,cex=1)
          
          if(c(j==1 | j==2) & log == TRUE)
          {
            text(u1+delu,-0.9,format(round(exp(u1),2)),adj=0,cex=1)
          }
        }
      }
      
      # the upper confidence limit
      if(n2 <n){
        uci<-prfpar[n2:(n2+1)]
        lci<- plk[n2:(n2+1)]
        u2<- approx(lci,uci,-0.5)$y
        
        if(output.plot == TRUE) {
          text(u2-delu,-0.7,format(round(u2,2)),adj=1,cex=1)
          
          if(c(j==1 | j==2) & log == TRUE)
          {
            text(u2-delu,-0.9,format(round(exp(u2),2)),adj=1,cex=1)
          }
        }
      }
      
      if(j==1) {
        try(gll<-u1, silent=TRUE)
        try(glu<-u2, silent=TRUE)
      }
      if(j==2) {
        try(mll<-u1, silent=TRUE)
        try(mlu<-u2, silent=TRUE)
      }
      
      
      # printing the maximum likelihood estimate on the graph
      if(output.plot == TRUE) {
        text(mlest[j],-0.17,format(round(mlest[j],2)),cex=1)
        if(c(j == 1 | j == 2) & log == TRUE)
        {
          text(mlest[j],-0.35,format(round(exp(mlest[j]),2)),cex=1)
        }
      }
    }
    if(output.plot==TRUE) {
      mtext(side=2,line=0,"Relative profile log likelihood",cex=1.2,outer=T,las=0) 
      mtext(side=3,line=0,paste(sample.id,if(log==TRUE){"   MAM 4"}else{"   MAM 4-UL"}),
            cex=1.4,outer=T)
    }
  }#EndOf IF (PROFILE LOG LIKELIHOODS)
  
  if(missing(bootstrap) == TRUE) {
    
    
    # prepare return values
    results<- data.frame(id=sample.id,n=length(lcd),log=log,Lmax=maxlik,BIC=bic,
                         gamma=gamma, mu=mu, sigma=sigma, p0=p0,
                         mindose=if(log==TRUE){exp(gamma)}else{gamma},
                         centdose=if(log==TRUE){exp(mu)}else{mu},
                         "gamma_68ci_lower"=NA,"gamma_68ci_upper"=NA,
                         "gamma_95ci_lower"=NA,"gamma_95ci_upper"=NA,
                         "mu_68ci_lower"=NA,"mu_68ci_upper"=NA,
                         "mu_95ci_lower"=NA,"mu_95ci_upper"=NA)
    
    
    try(results$"gamma_68ci_lower"<- if(log==TRUE){exp(gll)}else{gll},silent=TRUE)
    try(results$"gamma_68ci_upper"<- if(log==TRUE){exp(glu)}else{glu},silent=TRUE)
    try(results$"gamma_95ci_lower"<- if(log==TRUE){exp(gul)}else{gul},silent=TRUE)
    try(results$"gamma_95ci_upper"<- if(log==TRUE){exp(guu)}else{guu},silent=TRUE)
    
    try(results$"mu_68ci_lower"<- if(log==TRUE){exp(mll)}else{mll},silent=TRUE)
    try(results$"mu_68ci_upper"<- if(log==TRUE){exp(mlu)}else{mlu},silent=TRUE)
    try(results$"mu_95ci_lower"<- if(log==TRUE){exp(mul)}else{mul},silent=TRUE)
    try(results$"mu_95ci_upper"<- if(log==TRUE){exp(muu)}else{muu},silent=TRUE)
    
    # print out the maxmimum likelihood estimates  
    
    cat(paste("\n\n----------- meta data ------------"))                                 
    cat(paste("\n Sample ID:      ",sample.id))
    cat(paste("\n n:              ",length(lcd)))
    cat(paste("\n sigmab:         ",sigmab))
    cat(paste("\n log ED:         ",log))
    cat(paste("\n Lmax:           ",round(maxlik,3)))
    cat(paste("\n BIC:            ",round(bic,3)))
    
    cat(paste("\n\n--------- final parameter estimates ---------"))
    cat(paste("\n gamma:    ",round(gamma,4)),
        "\t\t minimum dose: ",if(log==TRUE){round(exp(gamma),3)}
        else{round(gamma,3)})
    cat(paste("\n mu:       ",round(mu,4)),
        "\t\t cent dose:    ",if(log==TRUE){round(exp(mu),3)}
        else{round(mu,3)})
    cat(paste("\n sigma:    ",round(sigma,4))) 
    cat(paste("\n p0:       ",round(p0,4)))
    
    if(log == TRUE) {
      cat(paste("\n\n------- confidence intervals for gamma -------"))
      try(cat(paste("\n  95% ci: ", round(exp(gul),3), "-", round(exp(guu),3),
                    " (-", round(exp(gamma)-exp(gul),2), " +",
                    round(exp(guu)-exp(gamma),2),")"), sep=""), silent=TRUE)
      try(cat(paste("\n  68% ci: ", round(exp(gll),3), "-", round(exp(glu),3), 
                    " (-", round(exp(gamma)-exp(gll),2), " +", 
                    round(exp(glu)-exp(gamma),2),")"), sep=""),silent=TRUE)
      
      if(any(is.na(results[12:15]))==TRUE){
        cat("\n # Couldn't calculate confidence intervals.")
      }
      
      cat(paste("\n\n------- confidence intervals for mu ----------"))
      try(cat(paste("\n  95% ci: ", round(exp(mul),3), "-", round(exp(muu),3),
                    " (-", round(exp(mu)-exp(mul),2), " +",
                    round(exp(muu)-exp(mu), 2),")"),sep=""),silent=TRUE)
      try(cat(paste("\n  68% ci: ", round(exp(mll),3), "-", round(exp(mlu),3),
                    " (-", round(exp(mu)-exp(mll),2), " +",
                    round(exp(mlu)-exp(mu), 2),")"),sep=""),silent=TRUE)
      
      if(any(is.na(results[16:19]))==TRUE){
        cat("\n # Couldn't calculate confidence intervals.")
      }
      
    }
    else {
      cat(paste("\n\n------- confidence intervals for gamma -------"))
      try(cat(paste("\n  95% ci: ", round(gul,3), "-", round(guu,3), " (-",
                    round(gamma-gul,2), " +",round(guu-gamma,2),")")
              ,sep=""),silent=TRUE)
      try(cat(paste("\n  68% ci: ", round(gll,3), "-", round(glu,3), " (-",
                    round(gamma-gll,2), " +",round(glu-gamma,2),")")
              ,sep=""),silent=TRUE)
      
      if(any(is.na(results[12:15]))==TRUE){
        cat("\n # Couldn't calculate confidence intervals.")
      }
      
      cat(paste("\n\n------- confidence intervals for mu ----------"))
      try(cat(paste("\n  95% ci: ", round(mul,3), "-", round(muu,3), " (-",
                    round(mu-mul,2), " +",round(muu-mu,2),")")
              ,sep=""),silent=TRUE)
      try(cat(paste("\n  68% ci: ", round(mll,3), "-", round(mlu,3), " (-",
                    round(mu-mll,2), " +",round(mlu-mu,2),")")
              ,sep=""),silent=TRUE)
      
      if(any(is.na(results[16:19]))==TRUE){
        cat("\n # Couldn't calculate confidence intervals.")
      }
      
      
      
      
      if(any(is.na(results))==TRUE){
        cat("\n # Couldn't calculate confidence intervals.")}
    }
    
    
    cat(paste("\n----------------------------------------------"))
    
    
    
    
    # restore previous plot parameters
    if(output.plot==TRUE && missing(bootstrap) == TRUE) {
      par(.pardefault)
    }
    
    # return values
    newRLumResults.calc_MinDose3 <- set_RLum.Results(
      data = list(
        results = results))
    
    invisible(newRLumResults.calc_MinDose3)
    
  } else {
    
    colnames(pairs)<- c("min_dose","likelihood")
    
    newRLumResults.calc_MinDose3 <- set_RLum.Results(
      data = list(results = 
                    list(
                      pairs = pairs,
                      poly.fits = list(poly.three = poly.three,
                                       poly.four = poly.four,
                                       poly.five = poly.five,
                                       poly.six = poly.six)))
      
    )
    
    invisible(newRLumResults.calc_MinDose3)
    
  }
  ### Returns a plot (optional) and terminal output. A file 
  ### containing statistical results is provided if desired. In addition an 
  ### \code{\linkS4class{RLum.Results}} object is 
  ### returned containing the following element:
  ###
  ### \item{results}{\link{data.frame} with statistical parameters.}
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
  ## Depending on the data, the upper and lower bounds for gamma
  ## (\code{gamma.xlb} and \code{gamma.xub}) and mu (\code{mu.xlb} and 
  ## \code{mu.xub}) need to be specified. If the final estimate of gamma or
  ## mu is on the boundary, \code{gamma.xlb} and \code{gamma.xub} 
  ## (\code{mu.xlb} and \code{mu.xub} respectively) need to be adjusted 
  ## appropriately, so that gamma and mu lie within the bounds. The same
  ## applies for sigma boundaries (\code{sigma.xlb} and \code{sigma.xub})
  ## \cr\cr
  ## \bold{Initial values} \cr\cr
  ## The log likelihood calculations use the \link{nlminb} function. 
  ## Accordingly, initial values for the four parameters \code{init.gamma},
  ## \code{init.sigma}, \code{init.mu} and \code{init.p0} need to be specified.
  ## \cr\cr
  ## \bold{Ignore NA values} \cr\cr
  ## In some cases during the calculation of the log likelihoods NA values 
  ## are produced instantly terminating the minimum age model. It is advised to 
  ## adjust some of the values provided for any argument. If the model still
  ## produces NA values it is possible to omit these values by setting 
  ## \code{ignore.NA = TRUE}. While the model is then usually able to finish
  ## all calculations the integrity of the final estimates cannot be ensured.
  ## Use this argument at own risk.
  
  
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
  ## \code{\link{nlminb}},
  ## \code{\link{calc_CentralDose}},
  ## \code{\link{calc_CommonDose}}, \code{\link{calc_FiniteMixture}},
  ## \code{\link{calc_FuchsLang2001}}, \code{\link{calc_MinDose3}}
  
  
}, ex=function(){
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  
  ## apply the logged minimum dose model
  #calc_MinDose4(ExampleData.DeValues, 
  #              sigmab = 0.05,
  #              ignore.NA = TRUE,
  #              output.plot = FALSE)
})
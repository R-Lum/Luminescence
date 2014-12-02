##======================================
calc_MaxDose3<- structure(function( # Apply the maximum age model to a given De distribution 
  ### Function to fit the maximum age model to De data. This function is a
  ### modified version of the three parameter minimum age model after Galbraith
  ### et al. (1999) using a similiar approach described in Olley et al. (2006).
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany) \cr
  ## Based on a rewritten S script of Rex Galbraith, 2010 \cr\cr
  
  ##section<<
  ## version 0.22
  # ===========================================================================
  
  input.data, 
  ### \code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
  ### for \code{data.frame}: two columns with De \code{(input.data[,1])} and
  ### De error \code{(values[,2])}
  sigmab, 
  ### \code{\link{numeric}}  (\bold{required}): spread in De values given as a 
  ### fraction (e.g. 0.2). This value represents the expected overdispersion in
  ### the data should the sample be well-bleached (Cunningham & Walling 2012, p. 100).
  log=TRUE, 
  ### \code{\link{logical}} (with default): fit the (un-)logged three parameter 
  ### maximum dose model to De data. An un-logged version is currently not
  ### supported
  sample.id="unknown sample",
  ### \code{\link{character}} (with default): sample id
  gamma.xlb=0.1, 
  ### \code{\link{numeric}} (with default): lower boundary of gamma
  gamma.xub=100,
  ### \code{\link{numeric}} (with default): upper boundary of gamma
  sigma.xlb= 0.001, 
  ### \code{\link{numeric}} (with default): lower boundary of sigma 
  sigma.xub= 5.00, 
  ### \code{\link{numeric}} (with default): upper boundary of sigma 
  init.gamma=10, 
  ### \code{\link{numeric}} (with default): starting value of gamma 
  init.sigma=1.2, 
  ### \code{\link{numeric}} (with default): starting value of sigma
  init.p0=0.01,
  ### \code{\link{numeric}} (with default): starting value of p0 
  ignore.NA = FALSE,
  ### \code{\link{logical}} (with default): ignore NA values during log
  ### likelihood calculations. See details.
  calc.ProfileLikelihoods=TRUE, 
  ### \code{\link{logical}} (with default): calculate profile log likelihood
  ### functions for gamma, sigma, p0. See \code{output.indices}.
  console.ProfileLikelihoods=FALSE, 
  ### \code{\link{logical}} (with default): print profile log likelihood
  ### functions for gamma, sigma, p0 to console.
  console.extendedOutput=FALSE, 
  ### \code{\link{logical}} (with default): extended terminal output 
  output.plot=FALSE, 
  ### \code{\link{logical}} (with default): plot output
  ### (\code{TRUE}/\code{FALSE})
  output.indices=3
  ### \code{\link{numeric}} (with default): requires 
  ### \code{calc.ProfileLikelihoods} = \code{TRUE}. Indices: 1 = gamma, 
  ### 2 = gamma/sigma, 3 = gamma/sigma/p0.
  ){                     
  
  
##============================================================================##
## CONSISTENCY CHECK OF INPUT DATA
##============================================================================##
  
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
  
  if(colnames(input.data[1])!="ED"||colnames(input.data[2])!="ED_Error") { 
    cat(paste("Columns must be named 'ED' and 'ED_Error'"), fill = FALSE)
    stop(domain=NA) 
  }
  
  if(sigmab <0 | sigmab >1) { 
    cat(paste("sigmab needs to be given as a fraction between", 
              "0 and 1 (e.g. 0.2)"), fill = FALSE)
    stop(domain=NA)
  }
  
  if(output.indices >3 | output.indices <1) { 
    cat(paste("Invalid number of indices. Only 1, 2 or 3",
              "(gamma, gamma/sigma, gamma/sigma/p0) allowed."), fill = FALSE)
    stop(domain=NA)
  }
  
  if(log == FALSE) { 
    cat(paste("Warning! The maximum age model currently only works with",
              "logarithmic De values."), fill = FALSE)
    stop(domain=NA)
  }
  
  if(output.plot == TRUE) {
    cat(paste("\n\n CAUTION: Graph labels are erroneous in this version! \n"), 
        fill = FALSE)
    #stop(domain=NA)
  }
  
##============================================================================##
## CALCULATIONS
##============================================================================##

# this calculates ln(1-Phi(x)) robustly, where 
# Phi(x) is the standard normal c.d.f

lnnprob.f<- function(x)
{ 
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

neglik.f<- function(param,input.data,rep){

# this calculates the negative of the log likelihood of the  
# data (dat,rep) for a given set of parameters (param)
#
# param is the vector of model parameters: 
#        (gamma, sigma, p0)
# datmat is a nx2 matrix of data: ld, seld (including sigma_b)
	
# recover the data
	zi<- input.data[,1]
	si<- input.data[,2]
	n<- length(zi)

# recover the parameters
	gamma<- param[1]
	mu<- gamma
	sigma<-  param[2]
	p0<-  param[3]

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

# printing the results to screen
  get("prstat",mode="numeric")
	ncalls<- prstat[1]
	ncmod<-  prstat[2]
	ipr<-    prstat[3]
	ncalls<- ncalls+1
	ncmod<- ncmod+1
  if(console.extendedOutput==TRUE) {
	if( ncmod==ipr | (ncalls==1 & ipr>0) )
	   {
	    cat(paste("\n\n\n ------ # function calls:", ncalls,"------"))
	    cat(paste("\n neg. log likelihood: ",round(negll,3)))
	    cat(paste("\n parameters"))
      cat(paste("\n  gamma: ",round(gamma,3),"  maxdose:",
              if(log==TRUE){round(exp(gamma),3)}else{round(gamma,3)}))
	    cat(paste("\n  sigma: ",round(sigma,4)))
	    cat(paste("\n     p0: ",round(p0,3)))
	    cat(paste("\n      n: ",length(zi)))
	    ncmod<- 0
	   }
  }
  
	assign("prstat",c(ncalls,ncmod,ipr))

  return(negll)
}

##============================================================================##
## MAIN PROGRAM
##============================================================================##

# read in the data and print the number of grains
  if(log==TRUE) { 
    lcd<- log(input.data$ED)*-1
    x.offset<- abs(min(lcd))
    lcd<- lcd+x.offset
    lse<-   sqrt( (input.data$ED_Error/input.data$ED)^2 + sigmab^2 )
  }
  else {
    lcd<- input.data$ED
    lse<-   sqrt( (input.data$ED_Error)^2 + sigmab^2 )
    }

	datmat<- cbind(lcd,lse)

  cat("\n [Calc_MaxDose3] \n")

# supply the starting values and bounds for the parameters
  if(log==TRUE) { gamma.init<- log(init.gamma) }
  else {          gamma.init<- init.gamma }
  sigma.init<- init.sigma
  p0.init<- init.p0

  x0<- c(gamma.init, sigma.init, p0.init)
  if(log==TRUE) { xlb<- c( log(gamma.xlb), sigma.xlb, 0.0001 )
                  xub<- c( log(gamma.xub), sigma.xub, 0.9999 )
  }
  else { xlb<- c( gamma.xlb, sigma.xlb, 0.0001 )
         xub<- c( gamma.xub, sigma.xub, 0.9999 )
  }

# parameters to control printing from nlminb
	ncalls<- 0
	ncmod<- 0
	ipr<- 50
	prstat<- c(ncalls,ncmod,ipr)
	prfile<- F
	if(log==TRUE) { Bmess<- 0 }

# maximise the likelihood
  opt.param<- nlminb(start=x0,objective=neglik.f,scale=1,lower=xlb,upper=xub,
                     input.data=datmat,control=list(iter.max = 1000, 
                                                    eval.max = 1000))

# print out the maxmimum likelihood estimates
  mlest<- opt.param$par
  gamma<- mlest[1]
  sigma<-  mlest[2]
  p0<-  mlest[3]

  maxlik<- -opt.param$objective
	bic<- -2*maxlik + 3*log(length(lcd))
  get("prstat",mode="numeric")
  ncalls<- prstat[1]
  if(log==TRUE){
    get("Bmess",)
  }

##============================================================================##
## PROFILE LOG LIKELIHOODS
##============================================================================##

  if(calc.ProfileLikelihoods==TRUE)
  {

# read in the indices of the parameters for which profile
# likelihoods are to be calculated
  
  profind<- as.integer(output.indices)

  # save previous plot parameter and set new ones
  .pardefault<- par(no.readonly = TRUE)
  
  if(output.plot==TRUE) {
    par(mfrow=c(2,2),oma=c(9,2,9,1),mar=c(3.7,3.1,1.1,0.2),
        mgp=c(1.75,0.5,0),las=1,cex.axis=1.1,cex.lab=1.3)
  }
  
# calculate the required profiles
  lbpar<- c("gamma","sigma","p0")
  npar<- length(x0)
  for(j in 1:profind)
  {
    
##============================================================================##    
## first pass at the profile likelihood

# this does a broad sweep of the possible parameter values and
# estimates where more intensive calculations should be made
  lowpar<- xlb[j]+(mlest[j]-xlb[j])*c(0.01,0.5,0.8,0.9,0.95,0.99)
  highpar<- mlest[j]+(xub[j]-mlest[j])*c(0.01,0.05,0.1,0.2,0.5,0.99)
  prfpar<- c(lowpar,mlest[j],highpar)
  plk<- numeric(0)

  if(console.ProfileLikelihoods==TRUE){
    cat("\n ------calculating profiles: broad sweep-------")
    cat(paste("\n trial parameter value","|","profile log likelihood \n"))
  }
  for(par in prfpar)
    {
    
# set up the initial values and the bounds so that the 
# profile parameter is fixed i.e. lower bd = upper bd
  if(j==1) 
    {
      px0<- c(par,mlest[2:npar])
      pxlb<- c(par,xlb[2:npar])
      pxub<- c(par,xub[2:npar])
     }
  if(j>1 & j<npar) 
    { 
      j1<- j-1
      j2<- j+1
      px0<- c(mlest[1:j1],par,mlest[j2:npar])
      pxlb<- c(xlb[1:j1],par,xlb[j2:npar])
      pxub<- c(xub[1:j1],par,xub[j2:npar])
     }
  if(j==npar) 
    { 
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
                     input.data=datmat,control=list(iter.max = 1000, 
                                                    eval.max = 1000))

  # save and print the results
  proflik<- -maxlik-opt.param$objective
  plk<- c(plk,proflik)
  #
  if(console.ProfileLikelihoods==TRUE){
  cat(c("",format(round(j,0)),"   ",format(round(c(par,proflik),3),nsmall=3),
        "\n"))
  }
}#END OF LOOP

##============================================================================##
## second pass

# this does a fine sweep of the parameter values between
# limits derived from the broad sweep
  n<- length(prfpar)
  cnt<- (1:n)
  tf<- plk > -2.0
  n1<- min(cnt[tf])
  n2<- max(cnt[tf])
  if(n1>1) tf[n1-1]<- T
  if(n2<n) tf[n2+1]<- T
  minpar<- xlb[j]
  if(n1>1) minpar<- min(prfpar[tf])
  maxpar<- xub[j]
  if(n2<n) maxpar<- max(prfpar[tf])
  prfpar2<- seq(minpar,maxpar,length=20)
  plk2<- numeric(0)

  if(console.ProfileLikelihoods==TRUE){
  cat("\n ------calculating profiles: fine sweep-------")
  cat(paste("\n trial parameter value","|","profile log likelihood \n"))
  }
  for(par in prfpar2 )
    { 
    
# set up the initial values and the bounds so that the 
# profile parameter is fixed i.e. lower bd = upper bd 
  if(j==1) 
    {
      px0<- c(par,mlest[2:npar])
      pxlb<- c(par,xlb[2:npar])
      pxub<- c(par,xub[2:npar])
    }
  if(j>1 & j<npar) 
    {
      j1<- j-1
      j2<- j+1
      px0<- c(mlest[1:j1],par,mlest[j2:npar])
      pxlb<- c(xlb[1:j1],par,xlb[j2:npar])
      pxub<- c(xub[1:j1],par,xub[j2:npar])
    }
  if(j==npar) 
    {
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
                     input.data=datmat,control=list(iter.max = 1000, 
                                                    eval.max = 1000))

# save and print the results
  proflik<- -maxlik-opt.param$objective
  plk2<- c(plk2,proflik)
  
  if(console.ProfileLikelihoods==TRUE){
    cat(c("",format(round(j,0)),"   ",format(round(c(par,proflik),3),nsmall=3),
          "\n"))
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
  if(n1>1) tf[n1-1]<- T
  if(n2<n) tf[n2+1]<- T
  prfpar<- prfpar[tf]
  plk<- plk[tf]

  true.prfpar<- (prfpar-x.offset)*-1
  
# plotting
  if(output.plot==TRUE) {
  
  plot(true.prfpar,plk,type="n",xlab=lbpar[j],ylab="",ylim=c(-2.5,0),cex=0.8)
  lines(true.prfpar,plk)
  abline(h=-1.92,lty=3)
  abline(h=-0.5,lty=3)
  
  }

# the 95% upper and lower confidence limits are calculated from the
# profile results  and the results are added to the plot
  n<- length(prfpar)
  cnt<- (1:n)
  tf<- plk>=-1.92
  n1<- min(cnt[tf])
  n2<- max(cnt[tf])
  delu<- (maxpar-minpar)/100

  # save "true" gamma value
  true.gamma<- (gamma-x.offset)*-1
  
# the lower confidence limit
if(n1>1)
  {
    uci<-prfpar[(n1-1):n1]
    lci<- plk[(n1-1):n1]
    u1<- approx(lci,uci,-1.92)$y
    
    if(output.plot==TRUE) {
      text(u1+delu,-2.12,format(round(u1,2)),adj=0,cex=1)
      if(j==1 & log==TRUE)
        {
	        text(u1+delu,-2.32,format(round(exp(u1),2)),adj=0,cex=1)
        }
      }
    }
# the upper confidence limit
  if(n2<n)
    {
      uci<-prfpar[n2:(n2+1)]
      lci<- plk[n2:(n2+1)]
      u2<- approx(lci,uci,-1.92)$y
     	
      if(output.plot==TRUE) {
        text(u2-delu,-2.12,format(round(u2,2)),adj=1,cex=1)
     	  if(j==1 & log==TRUE)
         {
	         text(u2-delu,-2.32,format(round(exp(u2),2)),adj=1,cex=1)
         }
       }
     }
  if(j==1) {
  try(gul<- u1, silent=TRUE)
  try(guu<- u2, silent=TRUE) }
  
  
# the 68% upper and lower confidence limits are calculated from the
# profile results  and the results are added to the plot
  n<- length(prfpar)
  cnt<- (1:n)
  tf<- plk>=-0.5
  n1<- min(cnt[tf])
  n2<- max(cnt[tf])
  delu<- (maxpar-minpar)/100

# the lower confidence limit
  if(n1>1)
    {
      uci<-prfpar[(n1-1):n1]
      lci<- plk[(n1-1):n1]
      u1<- approx(lci,uci,-0.5)$y
     	
      if(output.plot==TRUE) {
        text(u1+delu,-0.7,format(round(u1,2)),adj=0,cex=1)
     	
        if(j==1 & log==TRUE)
         {
          text(u1+delu,-0.9,format(round(exp(u1),2)),adj=0,cex=1)
         }
       }
     }

# the upper confidence limit
  if(n2<n)
    {
      uci<-prfpar[n2:(n2+1)]
      lci<- plk[n2:(n2+1)]
      u2<- approx(lci,uci,-0.5)$y
     	
      if(output.plot==TRUE) {
        text(u2-delu,-0.7,format(round(u2,2)),adj=1,cex=1)
    	  if(j==1 & log==TRUE)
        {
	        text(u2-delu,-0.9,format(round(exp(u2),2)),adj=1,cex=1)
        }
      }
    }
  
  if(j==1) {
  try(gll<- u1, silent=TRUE)
  try(glu<- u2, silent=TRUE) }
  

  
  
# printing the maximum likelihood estimate on the graph
  if(output.plot==TRUE) {
    text(mlest[j],-0.17,format(round(mlest[j],2)),cex=1)
       	 if(j==1 & log==TRUE)
            {
  	          text(mlest[j],-0.35,format(round(exp(mlest[j]),2)),cex=1)
            } 
        }
    }
  if(output.plot==TRUE) {
    mtext(side=2,line=0,"Relative profile log likelihood",cex=1.2,outer=T,las=0)
    mtext(side=3,line=0,paste(sample.id,if(log==TRUE){"   MAM 3"}
                              else{"   MAM 3-UL"}),cex=1.4,outer=T)
  }
# Bmess is a message: it is the number ofs times 
# B1<=B2 
  if(log==TRUE) {
  get("Bmess")
  cat("\n -----------------------------")
  cat(paste("\n # number of times B1<=B2:",Bmess,"#"))
  cat("\n -----------------------------")
  }
}#EndOf IF (PROFILE LOG LIKELIHOODS)

# prepare return values
results<- data.frame(id=sample.id,n=length(lcd),log=log,Lmax=maxlik,BIC=bic,
                     gamma=gamma, sigma=sigma, p0=p0,
                     mindose=if(log==TRUE){exp(gamma)}else{gamma},
                     "68ci_lower"=NA,
                     "68ci_upper"=NA,
                     "95ci_lower"=NA,
                     "95ci_upper"=NA)


try(results$"X68ci_lower"<- if(log==TRUE){exp(gll)}else{gll},silent=TRUE)
try(results$"X68ci_upper"<- if(log==TRUE){exp(glu)}else{glu},silent=TRUE)
try(results$"X95ci_lower"<- if(log==TRUE){exp(gul)}else{gul},silent=TRUE)
try(results$"X95ci_upper"<- if(log==TRUE){exp(guu)}else{guu},silent=TRUE)


# print out the maxmimum likelihood estimates

  cat(paste("\n\n----------- meta data ------------"))                                 
  cat(paste("\n Sample ID:      ",sample.id))
  cat(paste("\n n:              ",length(lcd)))
  cat(paste("\n sigmab:         ",sigmab))
  cat(paste("\n log ED:         ",log))
  cat(paste("\n Lmax:           ",round(maxlik,3)))
  cat(paste("\n BIC:            ",round(bic,3)))

  cat(paste("\n\n--------- final parameter estimates ---------"))
  cat(paste("\n gamma:   ",round(true.gamma,4)),
        "\t\t maximum dose: ",if(log==TRUE){round(exp(true.gamma),3)}
      else{round(gamma,3)})
  cat(paste("\n sigma:   ",round(sigma,4))) 
  cat(paste("\n p0:      ",round(p0,4)))


  cat(paste("\n\n------- confidence intervals for gamma -------"))
  if(log==TRUE) {
    try(cat(paste("\n 95% ci:  ", round(exp((guu-x.offset)*-1),2),
                  "-", round(exp((gul-x.offset)*-1),2),
                  " (-", round(exp(true.gamma)-exp((guu-x.offset)*-1),2), " +",
                  round(exp((gul-x.offset)*-1)-exp(true.gamma),2),")"),
            sep=""),silent=TRUE)
    
    try(cat(paste("\n 68% ci:  ", round(exp((glu-x.offset)*-1),2),
                  "-", round(exp((gll-x.offset)*-1),2),
                  " (-", round(exp(true.gamma)-exp((glu-x.offset)*-1),2), " +",
                  round(exp((gll-x.offset)*-1)-exp(true.gamma),2),")") ,
            sep=""),silent=TRUE)
  }
  else {
    try(cat(paste("\n 95% ci:  ", round(gul,2), "-", round(guu,2), " (-",
                    round(gamma-gul,2), " +",round(guu-gamma,2),")")
              ),silent=TRUE)
    
    try(cat(paste("\n 68% ci:  ", round(gll,2), "-", round(glu,2), " (-",
                    round(gamma-gll,2), " +",round(glu-gamma,2),")")
              ),silent=TRUE)
  }

  if(any(is.na(results))==TRUE){
    cat("\n # Couldn't calculate confidence intervals.")
  }

  cat(paste("\n----------------------------------------------"))
  
# Print out warnings with regard to parameter boundaries

bcheck<- data.frame(gamma=c(all.equal(gamma.xub,if(log==TRUE){exp(gamma)}
                                      else{gamma})==TRUE,
                            all.equal(gamma.xlb,if(log==TRUE){exp(gamma)}
                                      else{gamma})==TRUE),
                    sigma=c(all.equal(sigma.xub,sigma)==TRUE,
                            all.equal(sigma.xlb,sigma)==TRUE))
rownames(bcheck)<- c(".xub",".xlb")


                    
  if(any(bcheck==TRUE)) {
    cat(paste("\n\n #---------------------------------#"))
    cat(paste("\n # Warning! One or more parameters #",
              "\n # are on the boundary. Check the  #",
              "\n # logical matrix below to see     #",
              "\n # which ones and where.           #"), fill = FALSE)
    cat(paste("\n #---------------------------------#\n\n"))
    print(bcheck)
  }

  if(ignore.NA == TRUE) {
    cat(paste("\n CAUTION: By ignoring NA values the validity of results",
              "is not ensured. \n"), fill = FALSE)
  }

  # restore previous plot parameters
  par(.pardefault)
  
# return values
  
  newRLumResults.calc_MaxDose3 <- set_RLum.Results(
    data = list(
      results = results))

invisible(newRLumResults.calc_MaxDose3)
### Returns a plot (optional) and terminal output. In addition an 
### \code{\linkS4class{RLum.Results}} object is 
### returned containing the following element:
###
### \item{results}{\link{data.frame} with statistical parameters.}
###
### The output should be accessed using the function 
### \code{\link{get_RLum.Results}}  
  
##details<<
## \bold{Parameters} \cr\cr
## This model has three parameters: \cr
## \tabular{rl}{
## \code{gamma}: \tab maximum dose on the log scale \cr
## \code{sigma}: \tab spread in ages above the maximum \cr
## \code{p0}: \tab proportion of grains at gamma \cr }
## \bold{Data transformation} \cr\cr
## To estimate the maximum dose population and its standard error, the three 
## parameter minimum age model of Galbraith et al. (1999) is adapted. The
## measured De values are transformed as follows: \cr\cr
## 1. convert De values to natural logs \cr
## 2. multiply the logged data to creat a mirror image of the De distribution\cr
## 3. shift De values along x-axis by the smallest x-value found to obtain 
## only positive values \cr
## 4. combine in quadrature the measurement error associated with each De value
## with a relative error specified by sigmab
## 5. apply the MAM to these data \cr\cr
## When all calculations are done the results are then converted
## as follows\cr\cr
## 1. subtract the x-offset \cr
## 2. multiply the natural logs by -1 \cr
## 3. take the exponent to obtain the maximum dose estimate in Gy \cr\cr
## \bold{(Un-)logged model} \cr\cr
## In the original version of the three-parameter maximum dose model, the basic
## data are the natural logarithms of the De estimates and relative standard 
## errors of the De estimates. This model will be applied if \code{log = TRUE}.
## \cr\cr
## If \code{log = FALSE}, the modified un-logged model will be applied instead.
## This has essentially the same form as the original version. \code{gamma} and 
## \code{sigma} are in Gy and \code{gamma} becomes the maximum true dose in 
## the population. NOTE: This option is disabled for the maximum dose 
## model, as the data transformation requires logged De values! \cr\cr
## While the original (logged) version of the mimimum dose model may be 
## appropriate for most samples (i.e. De distributions), the modified 
## (un-logged) version is specially designed for modern-age and young samples 
## containing negative, zero or near-zero De estimates (Arnold et al. 2009,
## p. 323). \cr\cr
## \bold{Boundaries} \cr\cr
## Depending on the data, the upper and lower bounds for gamma (\code{gamma.xlb}
## and \code{gamma.xub}) need to be specified. If the final estimate of gamma is
## on the boundary, \code{gamma.xlb} and \code{gamma.xub} need to be adjusted 
## appropriately, so that gamma lies within the bounds. The same applies for 
## sigma boundaries (\code{sigma.xlb} and \code{sigma.xub}). \cr\cr
## \bold{Initial values} \cr\cr
## The log likelihood calculations use the \link{nlminb} function. Accordingly,
## initial values for the three parameters \code{init.gamma}, \code{init.sigma} 
## and \code{init.p0} need to be specified. \cr\cr
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
## Olley, J.M., Roberts, R.G., Yoshida, H., Bowler, J.M., 2006. Single-grain
## optical dating of grave-infill associated with human burials at Lake Mungo,
## Australia. Quaternary Science Reviews, 25, pp. 2469-2474.\cr\cr
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
## The default boundary and starting values for \emph{gamma}, \emph{sigma} and 
## \emph{p0} may only be appropriate for some De data sets and may need to be 
## changed for other data. An un-logged version is currently not supported.

##seealso<<
## \code{\link{nlminb}}, 
## \code{\link{calc_CentralDose}},
## \code{\link{calc_CommonDose}}, \code{\link{calc_FiniteMixture}},
## \code{\link{calc_FuchsLang2001}}, \code{\link{calc_MinDose4}}

}, ex=function(){
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  
  ## apply the logged maximum dose model
  ## NOTE THAT THE EXAMPLE DATA SET IS NOT SUITABLE FOR THE
  ## MAXIMUM DOSE MODEL.
  calc_MaxDose3(ExampleData.DeValues,
                sigmab = 0.3, gamma.xub = 4000)
})
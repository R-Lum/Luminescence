calc_CentralDose<- structure(function( # Apply the central age model (CAM) after Galbraith et al. (1999) to a given De distribution
  ### This function calculates the central dose and dispersion of the De 
  ### distribution, their standard errors and the profile log likelihood
  ### function for sigma.
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany) \cr
  ## Based on a rewritten S script of Rex Galbraith, 2010 \cr \cr
  
  ##section<<
  ## version 1.23 [2014-04-29]
  # ===========================================================================
  
  input.data,
  ### \code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
  ### for \code{data.frame}: two columns with De \code{(input.data[,1])} and
  ### De error \code{(values[,2])}
  sigmab = 0,
  ### \code{\link{numeric}} (with default): spread in De values given as
  ### a fraction (e.g. 0.2). This value represents the expected overdispersion
  ### in the data should the sample be well-bleached (Cunningham & Walling 2012, 
  ### p. 100).
  sample.id="unknown sample",
  ### \code{\link{character}} (with default): sample id
  print.iterations=FALSE,
  ### \code{\link{logical}} (with default): terminal output of
  ### calculation iterations
  output.plot=TRUE
  ### \code{\link{logical}} (with default): plot output
  ) {                     
                              

##============================================================================##
## CONSISTENCY CHECK OF INPUT DATA
##============================================================================##
  
  if(missing(input.data)==FALSE){
    
    if(is(input.data, "data.frame") == FALSE & is(input.data,
                                                  "RLum.Results") == FALSE){
      
      stop("[calc_CentralDose] Error: 'input.data' object has to be of type 
           'data.frame' or 'RLum.Results'!")
      
    }else{
      
      if(is(input.data, "RLum.Results") == TRUE){
        
        input.data <- get_RLum.Results(input.data, 
                                       signature(object = "De.values"))
        
      }
    }
  }  
  
  try(colnames(input.data)<- c("ED","ED_Error"), silent = TRUE)
  
  if(colnames(input.data[1])!="ED"||colnames(input.data[2])!="ED_Error") { 
    cat(paste("Columns must be named 'ED' and 'ED_Error'"), fill = FALSE)
    stop(domain=NA) 
  }
  
  if(sigmab <0 | sigmab >1) { 
    cat(paste("sigmab needs to be given as a fraction between", 
              "0 and 1 (e.g. 0.2)"), fill = FALSE)
    stop(domain=NA)
  }
  
##============================================================================##
## CALCULATIONS
##============================================================================##
		          
# calculate  yu = log(ED) and su = se(logED)
	yu<- log(input.data$ED)
	su<- sqrt( (input.data$ED_Error/input.data$ED)^2 + sigmab^2 )

# calculate starting values and weights 
  sigma<- 0.15
	wu<- 1/(sigma^2 + su^2)
	delta<- sum(wu*yu)/sum(wu)
	n<- length(yu)

# compute mle's
  for(j in 1:200){
    
  	delta<- sum(wu*yu)/sum(wu)
  	sigma<- sigma*sqrt(sum( (wu^2)*(yu-delta)^2/sum(wu) ))
  	wu<- 1/(sigma^2 + su^2)
  
# print iterations
      if(print.iterations==TRUE) {
        
      	print(round(c(delta, sigma),4))
      	
      }
  }

# save parameters for terminal output
out.delta<- exp(delta)
out.sigma<- sigma
	
# log likelihood	
	llik<-  0.5*sum(log(wu)) - 0.5*sum(wu*(yu-delta)^2)
    # save parameter for terminal output
    out.llik<- round(llik,4)
  Lmax<- llik

# standard errors
	sedelta<- 1/sqrt(sum(wu))
	sesigma<- 1/sqrt(2*sigma*sum(wu^2))

# save parameters for terminal output	
  out.sedelta<- sedelta
  out.sesigma<- sesigma

# profile log likelihood
  sigmax<- sigma
	llik<- 0
	sig0<- max(0,sigmax-8*sesigma)
	sig1<- sigmax + 9.5*sesigma
	sig<- try(seq(sig0,sig1,0.0001), silent = TRUE)
  
  if(class(sig) != "try-error") {
 
	for(sigma in sig) {
    
   	wu<- 1/(sigma^2 + su^2)
  	mu<- sum(wu*yu)/sum(wu)
  	ll<-  0.5*sum(log(wu)) - 0.5*sum(wu*(yu-mu)^2)
  	llik<- c(llik,ll)

  }
  
	llik<- llik[-1] - Lmax
  
  }#endif::try-error

##============================================================================##  
##TERMINAL OUTPUT
##============================================================================##  
  
  cat("\n [calc_CentralDose]")
  cat(paste("\n\n ---------------------------------------"))
  cat(paste("\n sample ID:              ",sample.id))
  cat(paste("\n n:                      ",n))
  cat(paste("\n log ED:                 ","TRUE"))
  cat(paste("\n ---------------------------------------"))
  cat(paste("\n central dose (delta):   ",round(out.delta,4)))
  cat(paste("\n rse (delta):            ",round(out.sedelta,4)))
  cat(paste("\n se (delta):             ",round(out.delta*out.sedelta,4)))
  cat(paste("\n ---------------------------------------"))
  cat(paste("\n overdispersion (sigma): ",round(out.sigma,4)))
  cat(paste("\n se (sigma):             ",if(class(sig) != "try-error") {
    round(out.sesigma,4) } else {"-"}))
  cat(paste("\n ---------------------------------------\n\n"))  

  
##============================================================================##  
##PLOTTING
##============================================================================##
  
if(output.plot==TRUE) {
  
  if(class(sig) != "try-error") {
    
    # save previous plot parameter and set new ones
    .pardefault<- par(no.readonly = TRUE)
    
    # plot the profile log likeihood
    par(oma=c(2,1,2,1),las=1,cex.axis=1.2, cex.lab=1.2)
    plot(sig,llik,type="l",xlab="Sigma",ylab="Log likelihood",lwd=1.5)
    abline(h=0,lty=3)
    abline(h=-1.92,lty=3)
    title("Profile log likelihood for sigma")
    
    # find upper and lower confidence limits for sigma
    tf<- abs(llik+1.92) < 0.005
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
    
  }#endif::try-error
}#endif::output.plot
  
  
  
#return value
  
  if(class(sig) == "try-error") {
    out.sigma<- 0
    out.sesigma<- NA
  }
  
  results<- data.frame(id=sample.id,n=n,log_ED="TRUE",central_dose=out.delta,
                       rse_delta=out.sedelta, se_delta=out.delta*out.sedelta,
                       OD=out.sigma,se_sigma=out.sesigma)
  
  newRLumResults.calc_CentralDose <- set_RLum.Results(
    data = list(
      results = results))
  
  invisible(newRLumResults.calc_CentralDose)
  ### Returns a plot (optional) and terminal output. In addition an 
  ### \code{\linkS4class{RLum.Results}} object is 
  ### returned containing the following element:
  ###
  ### \item{results}{\link{data.frame} with statistical parameters.}
  ###
  ### The output should be accessed using the function 
  ### \code{\link{get_RLum.Results}}
  
  
  ##details<<
  ## This function uses the equations of Galbraith et al. (1999, pp. 358-359). 
  ## The parameter \code{sigma} is estimated using the maximum likelihood 
  ## approach. A detailed explanation on maximum likelihood estimation can be 
  ## found in the appendix of Galbraith & Laslett (1993, pp. 468-470)
  
  ##references<<
  ## Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission
  ## track ages. Nuclear Tracks Radiation Measurements, 4, pp. 459-470. \cr \cr
  ## Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley, J.M., 
  ## 1999. Optical dating of single grains of quartz from Jinmium rock shelter, 
  ## northern Australia. Part I: experimental design and statistical models. 
  ## Archaeometry, 41, pp. 339-364. \cr \cr
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
  ## dose and error calculation and display in OSL dating: An overview and some 
  ## recommendations. Quaternary Geochronology, 11, pp. 1-27. \cr \cr
  ## \bold{Further reading} \cr \cr
  ## Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain 
  ## equivalent dose (De) distributions: Implications for OSL dating of sediment
  ## mixtures. Quaternary Geochronology, 4, pp. 204-230. \cr \cr
  ## Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain 
  ## quartz De distributions and an assessment of procedures for estimating 
  ## burial dose. Quaternary Science Reviews, 25, pp. 2475-2502. \cr \cr
  ## Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial 
  ## archives using robust OSL chronologies. Quaternary Geochronology, 12,
  ## pp. 98-106. \cr \cr
  ## Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing 
  ## the reproducibility and accuracy of optical dating of fluvial deposits. 
  ## Quaternary Geochronology, 1, pp. 109-120. \cr \cr
  ## Rodnight, H., 2008. How many equivalent dose values are needed to obtain a 
  ## reproducible distribution?. Ancient TL, 26, pp. 3-10.
  
  ##seealso<<
  ## \code{\link{plot}}, \code{\link{calc_CommonDose}}, 
  ## \code{\link{calc_FiniteMixture}}, \code{\link{calc_FuchsLang2001}},
  ## \code{\link{calc_MinDose3}}, \code{\link{calc_MinDose4}}

},  ex=function(){
  ##load example data
  data(ExampleData.DeValues, envir = environment())
  
  ##apply the central dose model
  calc_CentralDose(ExampleData.DeValues)
  
})#END OF STRUCTURE
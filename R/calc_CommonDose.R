calc_CommonDose <-
structure(function( # Apply the (un-)logged common age model after Galbraith et al. (1999) to a given De distribution
  ### Function to calculate the common dose of a De distribution.
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany) \cr
  
  ##section<<
  ## version 1.2
  # ===========================================================================
  
  input.data,
  ### \code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
  ### for \code{data.frame}: two columns with De \code{(input.data[,1])} and
  ### De error \code{(values[,2])}
  sigmab = 0,
  ### \code{\link{numeric}} (with default): spread in De values given as a 
  ### fraction (e.g. 0.2). This value represents the expected overdispersion in 
  ### the data should the sample be well-bleached (Cunningham & Walling 2012, 
  ### p. 100).
  log = TRUE,
  ### \code{\link{logical}} (with default): fit the (un-)logged common age 
  ### model to De data
  sample.id = "unknown sample"
  ### \code{\link{character}} (with default): sample id
  ) {                     
                              

##============================================================================##
## CONSISTENCY CHECK OF INPUT DATA
##============================================================================##
  
  if(missing(input.data)==FALSE){
    
    if(is(input.data, "data.frame") == FALSE & is(input.data,
                                                 "RLum.Results") == FALSE){
      
      stop("[calc_CommonDose] Error: 'input.data' object has to be of type 
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
	if(log==TRUE) {
    yu<- log(input.data$ED)
  	su<- sqrt( (input.data$ED_Error/input.data$ED)^2 + sigmab^2 )
  }
  else {
    yu<- input.data$ED
    su<- sqrt((input.data$ED_Error)^2 + sigmab^2)
  }

# calculate weights 
	wu<- 1/su^2
	delta<- sum(wu*yu)/sum(wu)
	n<- length(yu)
  
#standard error
  sedelta<- 1/sqrt(sum(wu))
  if(log==FALSE) {
    sedelta<- sedelta/delta
  }  
  
  cat("\n [calc_CommonDose]")
  cat(paste("\n\n ---------------------------------"))
  cat(paste("\n sample ID:              ",sample.id))
  cat(paste("\n n:                      ",n))
  cat(paste("\n log ED:                 ",if(log==TRUE){"TRUE"}else{"FALSE"}))
  cat(paste("\n ---------------------------------"))
  cat(paste("\n common dose:            ",if(log==TRUE){round(exp(delta),4)}
            else{round(delta,4)}))
  cat(paste("\n rse:                    ",round(sedelta,4)))
  cat(paste("\n se:                     ",round(if(log==TRUE){
    exp(delta)*sedelta}else{delta*sedelta},4)))
  cat(paste("\n ---------------------------------\n\n"))  
  
#return value
  results<- data.frame(id=sample.id,n=n,log_ED=log,common_dose=if(log==TRUE){
    round(exp(delta),4)}else{round(delta,4)},
                       rse=round(sedelta,4),se=round(if(log==TRUE){
                         exp(delta)*sedelta}else{delta*sedelta},4))
  
  newRLumResults.calc_CommonDose <- set_RLum.Results(
    data = list(
      results = results))
  
  invisible(newRLumResults.calc_CommonDose)
  ### Returns a terminal output. In addition an 
  ### \code{\linkS4class{RLum.Results}} object is 
  ### returned containing the following element:
  ###
  ### \item{results}{\link{data.frame} with statistical parameters.}
  ###
  ### The output should be accessed using the function 
  ### \code{\link{get_RLum.Results}}
  
  ##details<<
  ##  \bold{(Un-)logged model} \cr\cr
  ## When \code{log = TRUE} this function calculates the weighted mean of 
  ## logarithmic De values. Each of the estimates is weighted by the inverse 
  ## square of its relative standard error. The weighted mean is then 
  ## transformed back to the dose scale (Galbraith & Roberts 2012, p. 14).\cr\cr
  ## The log transformation is not applicable if the De estimates are close to 
  ## zero or negative. In this case the un-logged model can be applied instead
  ## (\code{log = FALSE}). The weighted mean is then calculated using the 
  ## un-logged estimates of De and their absolute standard error 
  ## (Galbraith & Roberts 2012, p. 14).
  
  ##references<<
  ## Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission
  ## track ages. Nuclear Tracks Radiation Measurements, 4, pp. 459-470. \cr\cr
  ## Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley, J.M., 
  ## 1999. Optical dating of single grains of quartz from Jinmium rock shelter, 
  ## northern Australia. Part I: experimental design and statistical models. 
  ## Archaeometry, 41, pp. 339-364. \cr\cr
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
  ## dose and error calculation and display in OSL dating: An overview and some 
  ## recommendations. Quaternary Geochronology, 11, pp. 1-27. \cr\cr
  ## \bold{Further reading} \cr\cr
  ## Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain
  ## equivalent dose (De) distributions: Implications for OSL dating of sediment
  ## mixtures. Quaternary Geochronology, 4, pp. 204-230. \cr\cr
  ## Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain
  ## quartz De distributions and an assessment of procedures for estimating 
  ## burial dose. Quaternary Science Reviews, 25, pp. 2475-2502. \cr\cr
  ## Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial
  ## archives using robust OSL chronologies. Quaternary Geochronology, 12, 
  ## pp. 98-106. \cr\cr
  ## Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the
  ## reproducibility and accuracy of optical dating of fluvial deposits.
  ## Quaternary Geochronology, 1, pp. 109-120.\cr\cr
  ## Rodnight, H., 2008. How many equivalent dose values are needed to obtain
  ## a reproducible distribution?. Ancient TL, 26, pp. 3-10.
  
  ##seealso<<
  ## \code{\link{calc_CentralDose}},
  ## \code{\link{calc_FiniteMixture}}, \code{\link{calc_FuchsLang2001}},
  ## \code{\link{calc_MinDose3}}, \code{\link{calc_MinDose4}}   
  
}, ex = function(){
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  
  ## apply the common dose model
  calc_CommonDose(ExampleData.DeValues)                         
})

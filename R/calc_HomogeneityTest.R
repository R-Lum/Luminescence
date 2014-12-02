calc_HomogeneityTest<- structure(function( # Apply a simple homogeneity test after Galbraith (2003)
  ### A simple homogeneity test for De estimates
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany), \cr
  
  ##section<<
  ## version 0.2
  # ===========================================================================
  
  input.data,
  ### \code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
  ### for \code{data.frame}: two columns with De \code{(input.data[,1])} and
  ### De error \code{(values[,2])}
  log = TRUE,
  ### \code{\link{logical}} (with default): peform the homogeniety test with
  ### (un-)logged data
  sample.id="unknown sample", 
  ### \code{\link{character}} (with default): sample id
  ...
  ### further arguments (for internal compatibility only).
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
  
  
##==========================================================================##
## ... ARGUMENTS
##==========================================================================##
  
  extraArgs <- list(...)
  
  ## set plot main title
  if("output.console" %in% names(extraArgs)) {
    output.console<- extraArgs$output.console
  } else {
    output.console<- TRUE
  }
  
##============================================================================##
## CALCULATIONS
##============================================================================##

  if(log==TRUE){
    input.data<- log(input.data)
  }
  
  wi<- 1/input.data[2]^2
  wizi<- wi*input.data[1]
  mu<- sum(wizi)/sum(wi)
  gi<- wi*(input.data[1]-mu)^2
  
  G<- sum(gi)
  df<- length(wi)-1
  n<- length(wi)
  P<- pchisq(G, df, lower.tail = FALSE)

##============================================================================##
## OUTPUT
##============================================================================##

if(output.console == TRUE) {
  
  cat("\n [calc_HomogeneityTest]")
  cat(paste("\n\n ---------------------------------"))
  cat(paste("\n sample ID:         ", sample.id))
  cat(paste("\n n:                 ", n))
  cat(paste("\n ---------------------------------"))
  cat(paste("\n mu:                ", round(mu,4)))
  cat(paste("\n G-value:           ", round(G,4)))
  cat(paste("\n Degrees of freedom:", df))
  cat(paste("\n P-value:           ", round(P,4)))
  cat(paste("\n ---------------------------------\n\n"))
  
}

  #return value
  results<- data.frame(id=sample.id,n=n,g.value=G,df=df,P.value=P)
  
  newRLumResults.calc_HomogeneityTest <- set_RLum.Results(
    data = list(
      results = results))
  
  
  invisible(newRLumResults.calc_HomogeneityTest)
  ### Returns a terminal output. In addition an 
  ### \code{\linkS4class{RLum.Results}} object is 
  ### returned containing the following element:
  ###
  ### \item{results}{\link{data.frame} with statistical parameters.}
  ###
  ### The output should be accessed using the function 
  ### \code{\link{get_RLum.Results}}  
  
  ##details<<
  ## For details see Galbraith (2003).
  
  ##references<<
  ## Galbraith, R.F., 2003. A simple homogeneity test for estimates of dose
  ## obtained using OSL. Ancient TL, 21, pp. 75-77.
  
  ##seealso<<
  ## \code{\link{pchisq}}
      
}, ex=function(){
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  
  ## apply the homogeneity test
  calc_HomogeneityTest(ExampleData.DeValues)
})
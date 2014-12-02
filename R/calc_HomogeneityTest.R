calc_HomogeneityTest<- structure(function( # Apply a simple homogeneity test after Galbraith (2003)
  ### A simple homogeneity test for De estimates
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany), \cr
  
  ##section<<
  ## version 0.2 
  # ===========================================================================
  
  data,
  ### \code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
  ### for \code{data.frame}: two columns with De \code{(data[,1])} and
  ### De error \code{(values[,2])}
  log=TRUE,
  ### \code{\link{logical}} (with default): peform the homogeniety test with
  ### (un-)logged data
  ...
  ### further arguments (for internal compatibility only).
){                     
  
  
  ##============================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ##============================================================================##
  
  if(missing(data)==FALSE){
    if(is(data, "data.frame") == FALSE & is(data, "RLum.Results") == FALSE){
      stop("[calc_FiniteMixture] Error: 'data' object has to be of type 
           'data.frame' or 'RLum.Results'!")
    } else {
      if(is(data, "RLum.Results") == TRUE){
        data <- get_RLum.Results(data, signature(object = "De.values"))
        
      }
    }
  }  
  
  ##==========================================================================##
  ## ... ARGUMENTS
  ##==========================================================================##
  
  extraArgs <- list(...)
  
  ## set plot main title
  if("verbose" %in% names(extraArgs)) {
    verbose<- extraArgs$verbose
  } else {
    verbose<- TRUE
  }
  
  ##============================================================================##
  ## CALCULATIONS
  ##============================================================================##
  
  if(log==TRUE){
    dat<- log(data)
  } else {
    dat<- data
  }
  
  wi<- 1/dat[2]^2
  wizi<- wi*dat[1]
  mu<- sum(wizi)/sum(wi)
  gi<- wi*(dat[1]-mu)^2
  
  G<- sum(gi)
  df<- length(wi)-1
  n<- length(wi)
  P<- pchisq(G, df, lower.tail = FALSE)
  
  ##============================================================================##
  ## OUTPUT
  ##============================================================================##
  
  if(verbose == TRUE) {
    cat("\n [calc_HomogeneityTest]")
    cat(paste("\n\n ---------------------------------"))
    cat(paste("\n n:                 ", n))
    cat(paste("\n ---------------------------------"))
    cat(paste("\n mu:                ", round(mu,4)))
    cat(paste("\n G-value:           ", round(G,4)))
    cat(paste("\n Degrees of freedom:", df))
    cat(paste("\n P-value:           ", round(P,4)))
    cat(paste("\n ---------------------------------\n\n"))
  }
  
  ##============================================================================##
  ## RETURN VALUES
  ##============================================================================##
  
  summary<- data.frame(n=n,g.value=G,df=df,P.value=P)
  
  call<- sys.call()
  args<- list(log=log)
  
  newRLumResults.calc_HomogeneityTest <- set_RLum.Results(
    data = list(
      summary=summary,
      data=data,
      args=args,
      call=call
    ))
  
  invisible(newRLumResults.calc_HomogeneityTest)
  ### Returns a terminal output. In addition an 
  ### \code{\linkS4class{RLum.Results}} object is 
  ### returned containing the following element:
  ###
  ### \item{summary}{\link{data.frame} summary of all relevant model results.}
  ### \item{data}{\link{data.frame} original input data}
  ### \item{args}{\link{list} used arguments}
  ### \item{call}{\link{call} the function call}
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
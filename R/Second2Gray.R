Second2Gray <- structure(function(#Converting values from seconds (s) to gray (Gy)
  ### Conversion of absorbed radiation dose in seconds (s) to the SI unit gray (Gy) 
  ### including error propagation. Normally used for equivalent dose data.
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany), 
  ## Michael Dietze, GFZ Potsdam (Germany),
  ## Margret C. Fuchs, AWI Potsdam (Germany), \cr
  
  ##section<<
  ## version 0.3
  # ===========================================================================

  values,
  ### \link{data.frame} (\bold{required}): measured data (\code{values[,1]}) and data error 
  ### (\code{values [,2]})
  
  dose_rate,
  ### \link{vector} (\bold{required}): dose rate in Gy/s and dose rate error in Gy/s
  
  method = "gaussian"
  ### \link{character} (with default): method used for error calculation 
  ### (\code{gaussian} or \code{absolute}), see details for further information
){ 
  
  De.seconds <- values[,1]
  De.error.seconds <- values[,2]
  
  De.gray <- NA
  De.error.gray <- NA
  
  De.gray <- round(De.seconds*dose_rate[1], digits=2) 
  
  if(method == "gaussian"){
    
    De.error.gray <- round(sqrt((De.seconds*dose_rate[2])^2+(dose_rate[1]*De.error.seconds)^2), digits=2)
    
  }else if (method == "absolute"){
        
    De.error.gray <- round(abs(dose_rate[1] * De.error.seconds) + abs(De.seconds * dose_rate[2]), digits=2)
    
  }else{
    
    stop("[Second2Gray] Error: unknown error calculation method!" )
    
  }
    
  values <- data.frame(De=De.gray, De.error=De.error.gray)
	return(values)
  
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
  
  ##details<<
  ## Calculation of De values from seconds (s) to gray (Gy)
  ## \deqn{De [Gy] = De [s] * Dose Rate [Gy/s])} \cr
  ## Provided calculation methods for error calculation:  
  ## \bold{gaussian} error propagation \cr
  ## \deqn{De.error.gray = \sqrt(dose.rate * De.error.seconds)^2 + (De.seconds * dose.rate.error)^2 ))}
  ## \bold{absolute} error propagation \cr
  ## \deqn{De.error.gray = abs(dose.rate * De.error.seconds) + abs(De.seconds * dose.rate.error)}
  
  ##value<<
  ## Returns a \link{data.frame} with converted values.
  
  ##references<<
  ## #
  
  ##note<<
  ## If no or a wrong method is given, the execution of the function is stopped. 
  
  ##seealso<<
  ## # 
  
  ##keyword<<
  ## manip
  
}, ex=function(){
  
  ##(1) for dose taken from the example data help file
  data(ExampleData.DeValues, envir = environment())
  Second2Gray(ExampleData.DeValues, c(0.0438,0.0019))
  
})  

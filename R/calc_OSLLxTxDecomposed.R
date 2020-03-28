#' Calculate Lx/Tx ratio for CW-OSL signal components
#'
#' Calculate Lx/Tx ratios from a given set of CW-OSL curves which was decomposed by
#' [OSLdecomposition::decompose_RLumData]
#' 
#' @param OSL.component [character] (*optional*):
#' an [numeric] index or a name describing which OSL signal component shall be evaluated.
#' It presumes, that the data set was manipulated by function [OSLdecomposition::decompose_RLumData].
#' This argument can either be the name of the OSL component assigned by [OSLdecomposition::fit_OSLcurve]
#' or the index of component. Then "1" selects the fastest decaying component, "2" the second fastest
#' and so on.
#' 
#' 

calc_OSLLxTxDecomposed <- function(
  Lx.data,
  Tx.data = NULL,
  OSL.component = 1,
  sig0 = 0,
  digits = NULL
){
  
  #
  # ToDo's:
  # - Integrity checks for the component table
  # - Handle background-signal-component if present
  # - add Tx.data integrity checks
  # - add previous-residual-subtraction functionality
  # - add list with decomposition algorithm parameters to return object
  #
  #
  # Example of the 'object@records[[...]]@info$COMPONENTS' Data.frame:
  #
  # name      lambda cross.section cross.relative SAR.compatible   t.start     t.end ch.start
  # 1 Component 1 0.988274358  1.392307e-17         1.0000              1  0.000000  2.097898        1
  # 2 Component 2 0.141741931  1.996898e-18         0.1434              1  2.097898 14.185786       22
  # 3       Slow3 0.002876973  4.053156e-20         0.0029              0 14.185786 40.059861      143
  # ch.end          n    n.error n.residual  bin bin.error
  # 1     21  3733.3078   84.68497          0 3951  60.23047
  # 2    142   720.6614  162.03757          2 3842  71.17842
  # 3    401 85018.4793 1409.87313      75764 5910  84.88269
  #
  #


  ##--------------------------------------------------------------------------##
  ## (1) - integrity checks
  
 
  

  # ... insert all the data.frame checks here ...
  
  # define the component
  component_index <- NA
  
  if (!(is.numeric(OSL.component) || is.character(OSL.component))) {
    stop("[calc_OSLLxTxDecomposed()] Type error! No valid data type for OSL.component")}
  
  # get component index from component name
  if (is.character(OSL.component)) {
    
    if (tolower(OSL.component) %in% tolower(Lx.data$name)) {
      
      component_index <- which(tolower("OSL.component") == tolower(Lx.data$name))
      
    } else {
      
      stop(paste0("[calc_OSLLxTxDecomposed()] Invalid OSL component name! Valid names are: ", 
                  paste(Lx.data$name, collapse = ", ")))
    }
  }
  
  # if a numeric is given, check if it matches with any component index
  if (is.numeric(OSL.component)) {
    if (OSL.component %in% 1:nrow(Lx.data)) {
      
      component_index <- OSL.component
      
      # insert background-signal-component check here
      
    } else {
      stop(paste0("[calc_OSLLxTxDecomposed()] Invalid OSL component index! Component table has ", nrow(Lx.data), " rows."))
    
    }
    
  }
  
  ##--------------------------------------------------------------------------##
  ## (2) - extract Lx and Tx values
  
  LnLx <- Lx.data$n[component_index]
  LnLx.Error <- Lx.data$n.error[component_index]

  TnTx <- 1
  TnTx.Error <- 0
  if (!is.null(Tx.data)) {
    
    TnTx <- Tx.data$n[component_index]
    TnTx.Error <- Tx.data$n.error[component_index]
  }
  
  ##combine results
  LnLxTnTx <- cbind(
    LnLx,
    LnLx.Error,
    TnTx,
    TnTx.Error
  )
  
  # THE FOLLOWING CODE IS MOSTLY IDENTICAL WITH (4) IN calc_OSLLxTxRatio()  
  
  ##--------------------------------------------------------------------------##
  ##(4) Calculate LxTx error according Galbraith (2014)
  
  #transform results in a data.frame
  LnLxTnTx <- as.data.frame((LnLxTnTx))
  
  #add col names
  colnames(LnLxTnTx)<-c("Net_LnLx", "Net_LnLx.Error",
                        "Net_TnTx", "Net_TnTx.Error")
  
  ##calculate Ln/Tx
  LxTx <- LnLxTnTx$Net_LnLx/LnLxTnTx$Net_TnTx
  
  ##set NaN
  if(is.nan(LxTx)) LxTx <- 0
  
  ##calculate Ln/Tx error
  LxTx.relError <- sqrt((LnLx.Error / LnLx)^2 + (TnTx.Error / TnTx)^2)
  LxTx.Error <- abs(LxTx * LxTx.relError)
  
  ##set NaN
  if(is.nan(LxTx.Error)) LxTx.Error <- 0
  
  ##add an extra component of error
  LxTx.Error <- sqrt(LxTx.Error^2 + (sig0 * LxTx)^2)
  
  ##return combined values
  temp <- cbind(LnLxTnTx,LxTx,LxTx.Error)
  
  
  ##apply digits if wanted
  if(!is.null(digits)){
    temp[1,] <- round(temp[1,], digits = digits)
    
  }
  
  # ToDo: Add decomposition algorithm parameters here
  # calc.parameters <- list(...)
  
  ##set results object
  temp.return <-
    set_RLum(
      class = "RLum.Results",
      data = list(
        LxTx.table = temp),
      #  calc.parameters = calc.parameters),
      info = list(call = sys.call())
    )
  
  invisible(temp.return)
  

}
#' Calculate Lx/Tx ratio for CW-OSL signal components
#'
#' Calculate Lx/Tx ratios from a given set of CW-OSL curves which was decomposed by
#' [OSLdecomposition::decompose_RLumData]
#' 
#' 

calc_OSLLxTxDecomposed <- function(
  Lx.data,
  Tx.data = NULL,
  OSL.component = 1,
  allow.negative.values = TRUE,
  digits = NULL
){
  
 

  ##--------------------------------------------------------------------------##
  ##(1) - component related integrity checks
  
  if (!(is.numeric(OSL.component) || is.character(OSL.component))) {
    stop("[calc_OSLLxTxDecomposed()] Type error! No valid data type for OSL.component")}
  
  

  
  

   
}
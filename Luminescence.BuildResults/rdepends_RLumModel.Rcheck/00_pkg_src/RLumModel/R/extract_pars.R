#' Helper function to extract parms in one single list to submit to Rcpp ODE function
#'
#' @param parameters.step \code{\link{list}} (\bold{required}): input list with different classes.
#'
#' @param \dots further arguments and graphical parameters passed to
#'
#' @return This function returns an \code{\link{list}}.
#'
#' @section Function version: 0.1.0 [2016-09-02]
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#'
#' @references
#'
#' @seealso \code{\link{simulate_CW_OSL}},\code{\link{simulate_heating}}, \code{\link{simulate_illumination}},
#' \code{\link{simulate_irradiation}}, \code{\link{simulate_LM_OSL}}, \code{\link{simulate_pause}},
#' \code{\link{simulate_RF}}, \code{\link{simulate_TL}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.extract_pars <- function(parameters.step,...){
  
  R <- parameters.step$R
  P <- parameters.step$P
  temp <- parameters.step$temp
  b <- parameters.step$b
  if("a" %in% names(parameters.step)){
    a <- parameters.step$a
  } else {
    a <- NULL
  }

  N <- parameters.step$parms$N
  E <- parameters.step$parms$E
  s <- parameters.step$parms$s
  A <- parameters.step$parms$A
  B <- parameters.step$parms$B
  Th <- parameters.step$parms$Th
  E_th <- parameters.step$parms$E_th
  n <- parameters.step$parms$n$n
  k_B <- parameters.step$parms$k_B
  W <- parameters.step$parms$W
  K <- parameters.step$parms$K
  
  times <- parameters.step$times
  
  model <- parameters.step$parms$model
  
  return(list(
    a = a,
    R = R,
    P = P,
    b = b,
    temp = temp,
    N = N,
    E = E,
    s = s,
    A = A,
    B = B,
    Th = Th,
    E_th = E_th,
    n = n,
    k_B = k_B,
    W = W,
    K = K,
    times = times,
    model = model))

}
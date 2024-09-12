#' @title sequence step irradiation
#'
#' @description This function simulates the irradiation of quartz in the energy-band-model.
#'
#' @param temp \code{\link{cnumeric}} (\bold{required}): temperature [deg. C] at which the dose should be applied
#'
#' @param dose \code{\link{numeric}} (\bold{required}): dose to apply in Gray
#'
#' @param dose_rate \code{\link{numeric}} (\bold{required}): Dose rate in Gy/s
#'
#' @param n \code{\link{numeric}} or \code{\linkS4class{RLum.Results}} (\bold{required}):
#' concentration of electron-/holetraps, valence- and conduction band
#' from step before. This is necessary to get the boundary condition for the ODEs.
#'
#' @param parms \code{\linkS4class{RLum.Results}} (\bold{required}): The specific model parameters are used to simulate
#' numerical quartz luminescence results.
#'
#' @return This function returns an RLum.Results object.
#'
#' @section Function version: 0.1.2 [2017-11-20]
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#'
#' @references
#'
#' Bailey, R.M., 2001. Towards a general kinetic model for optically and thermally stimulated
#' luminescence of quartz. Radiation Measurements 33, 17-45.
#'
#' @seealso \code{\link{model_LuminescenceSignals}}, \code{\link{simulate_RL}}
#'
#' @examples
#'
#' #so far no example available
#'
#' @noRd
.simulate_irradiation <- function(
  temp,
  dose,
  dose_rate,
  n,
  parms
){

# check input arguments ---------------------------------------------------

  ##check if temperature is > 0 K (-273 degree celsius)
  if(temp < -273){
    stop("\n [.simulate_irradiation()] Argument 'temp' has to be > 0 K!")
  }
  ##check if dose_rate is a positive number
  if(dose_rate < 0){
    stop("\n [.simulate_irradiation()] Argument 'dose_rate' has to be a positive number!")
  }

  ##check if dose is a positive number
  if(dose < 0){
    stop("\n [.simulate_irradiation()] Argument 'dose' has to be a positive number!")
  }

  ##check if n is a RLum object
  if(class(n)[1] != "RLum.Results"){
    n <- n
  } else {
    n <- n$n
  }

# solving ODE ---------------------------------------------------
  if(dose != 0){

  ##============================================================================##
  # SETTING PARAMETERS FOR IRRADIATION
  #
  # R: electron-hole-production-rate (in Bailey 2004: 2.5e10, Bailey 2002: 3e10, else: 5e7)
  # P: Photonflux (in Bailey 2004: wavelength [nm])
  # b: heating rate [deg. C/s]
  ##============================================================================##
  ## check if R is given in customized parameter sets
  if("R" %in% names(parms) && parms$R != 0){
    R <- dose_rate*parms$R

  } else {
    R <- dose_rate * 5e7  # all other simulations

    if(parms$model == "Bailey2004")
      R <- dose_rate * 2.5e10

    if(parms$model == "Bailey2002")
      R <- dose_rate * 3e10

    if(parms$model == "Friedrich2018")
      R <- dose_rate * 6.3e7
  }

  P <- 0
  b <- 0

  ##============================================================================##
  # SETTING PARAMETERS FOR ODE
  ##============================================================================##
  times   <- seq(0, dose/(dose_rate), by = (dose/dose_rate)/100)
  parameters.step <- .extract_pars(parameters.step = list(
    R = R,
    P = P,
    temp = temp,
    b = b,
    times = times,
    parms = parms))

  ##============================================================================##
  # SOLVING ODE (deSolve required)
  ##============================================================================##
  out <- deSolve::lsoda(
    y = n,
    times = times,
    parms = parameters.step,
    func = .set_ODE_Rcpp,
    rtol = 1e-6,
    atol = 1e-6);

  ##============================================================================##
  # TAKING THE LAST LINE OF "OUT" TO COMMIT IT TO THE NEXT STEP
  ##============================================================================##
  return(Luminescence::set_RLum(
    class = "RLum.Results",
    data = list(
    n = out[length(times),-1],temp = temp)))

  } else {
  return(Luminescence::set_RLum(
    class = "RLum.Results",
    data = list(
    n = n,
    temp = temp)))

 }
}

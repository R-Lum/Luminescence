#' Apply a fading correction according to Huntley & Lamothe (2001) for a given
#' g-value and a given tc
#'
#' This function solves the equation used for correcting the fading affected age
#' including the error for a given g-value according to Huntley & Lamothe (2001).
#'
#' As the g-value sligthly depends on the time between irradiation and the prompt measurement,
#' this is tc, always a tc value needs to be provided. If the g-value was normalised to a distinct
#' time or evaluated with a different tc value (e.g., external irradiation), also the tc value
#' for the g-value needs to be provided (argument \code{tc.g_value} and then the g-value is recalcualted
#' to tc of the measurement used for estimating the age applying the following equation:
#'
#' \deqn{\kappa_{tc} = \kappa_{tc.g} / (1 - \kappa_{tc.g} * log(tc/tc.g))}
#'
#' where
#'
#' \deqn{\kappa_{tc.g} = g / 100 / log(10)}
#'
#' with \eqn{log} the natural logarithm.
#'
#'
#' The error of the fading-corrected age is determined using a Monte Carlo
#' simulation approach. Solving of the equation is realised using
#' \code{\link{uniroot}}. Large values for \code{n.MC} will significantly
#' increase the computation time.\cr
#'
#' \bold{\code{n.MC = 'auto'}}
#'
#' The error estimation based on a stochastic process, i.e. for a small number of MC runs the calculated
#' error varies considerably every time the function is called, even with the same input values.
#' The argument option \code{n.MC = 'auto'} tries to find a stable value for the standard error, i.e.
#' the standard deviation of values calculated during the MC runs (\code{age.corr.MC}),
#' within a given precision (2 digits) by increasing the number of MC runs stepwise and
#' calculating the corresponding error.
#'
#' If the determined error does not differ from the 9 values calculated previously
#' within a precision of (here) 3 digits the calculation is stopped as it is assumed that the error
#' is stable. Please note that (a) the duration depends on the input values as well as on
#' the provided computation ressources and it may take a while, (b) the length (size) of the output
#' vector \code{age.corr.MC}, where all the single values produced during the MC runs are stored,
#' equals the number of MC runs (here termed observations).
#'
#' To avoid an endless loop the calculation is stopped if the number of observations exceeds 10^7.
#' This limitation can be overwritten by setting the number of MC runs manually,
#' e.g. \code{n.MC = 10000001}. Note: For this case the function is not checking whether the calculated
#' error is stable.\cr
#'
#'
#' \bold{\code{seed}}
#'
#' This option allows to recreate previously calculated results by setting the seed
#' for the R random number generator (see \code{\link{set.seed}} for details). This option
#' should not be mixed up with the option \bold{\code{n.MC = 'auto'}}. The results may
#' appear similar, but they are not comparable!\cr
#'
#' \bold{FAQ}\cr
#' Q: Which tc value is expected?\cr
#' A: tc is the time in seconds between irradiation and the prompt measurement applied during your
#' De measurement. However, this tc might differ from the tc used for estimating the g-value. In the
#' case of an SAR measurement tc should be similar, however, if it differs, you have to provide this
#' tc value (the one used for estimating the g-value) using the argument \code{tc.g_value}.\cr
#'
#' @param age.faded \code{\link{numeric}} \code{\link{vector}} (\bold{required}): uncorrected
#' age with error in ka (see example)
#'
#' @param g_value \code{\link{vector}} (\bold{required}): g-value and error obtained
#' from separate fading measurements (see example). Alternatively an \code{\linkS4class{RLum.Results}} object
#' can be provided produced by the function \code{analyse_FadingMeasurement}, in this case tc is set
#' automatically
#'
#' @param tc \code{\link{numeric}} (\bold{required}): time in seconds between
#' irradiation and the prompt measurement (cf. Huntley & Lamothe 2001). Argument will be ignored
#' if \code{g_value} was an \code{RLum.Results} object
#'
#' @param tc.g_value \code{\link{numeric}} (with default): the time in seconds between irradiation
#' and the prompt measurement used for estimating the g-value. If the g-value was normalised
#' to, e.g., 2 days, this time in seconds (i.e., 172800) should be given here. If nothing is provided
#' the time is set to tc, which is usual case for g-values obtained using the SAR method and g-values
#' that had been not normalised to 2 days.
#'
#' @param n.MC \code{\link{integer}} (with default): number of Monte Carlo
#' simulation runs for error estimation. If \code{n.MC = 'auto'} is used the function
#' tries to find a 'stable' error for the age. Note: This may take a while!
#'
#' @param seed \code{\link{integer}} (optional): sets the seed for the random number generator
#' in R using \code{\link{set.seed}}
#'
#' @param interval \code{\link{numeric}} (with default): a vector containing the end-points (age interval) of the
#' interval to be searched for the root in 'ka'. This argument is passed to the function \code{\link[stats]{uniroot}}
#' used for solving the equation.
#'
#' @param txtProgressBar \link{logical} (with default): enables or disables
#' \code{\link{txtProgressBar}}
#'
#' @param verbose \code{\link{logical}} (with default): enables or disables terminal output
#'
#'
#' @return Returns an S4 object of type \code{\linkS4class{RLum.Results}}.\cr
#'
#' Slot: \bold{@data}\cr
#' \tabular{lll}{
#' \bold{Object} \tab \bold{Type} \tab \bold{Comment}\cr
#'  \code{age.corr} \tab \code{data.frame} \tab Corrected age \cr
#'  \code{age.corr.MC} \tab \code{numeric} \tab MC simulation results with all possible ages from
#'  that simulation\cr
#' }
#'
#' Slot: \bold{@info}\cr
#'
#' \tabular{lll}{
#' \bold{Object} \tab \bold{Type} \tab \bold{Comment}\cr
#'  \code{info} \tab \code{character} \tab the original function call
#'
#' }
#'
#'
#' @note Special thanks to Sebastien Huot for his support and clarification via e-mail.
#'
#'
#' @section Function version: 0.4.2
#'
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#'
#' @seealso \code{\linkS4class{RLum.Results}}, \code{\link{get_RLum}},
#' \code{\link{uniroot}}
#'
#'
#' @references Huntley, D.J., Lamothe, M., 2001. Ubiquity of anomalous fading
#' in K-feldspars and the measurement and correction for it in optical dating.
#' Canadian Journal of Earth Sciences, 38, 1093-1106.
#'
#'
#' @keywords datagen
#'
#'
#' @examples
#'
#' ##run the examples given in the appendix of Huntley and Lamothe, 2001
#'
#' ##(1) faded age: 100 a
#' results <- calc_FadingCorr(
#'    age.faded = c(0.1,0),
#'    g_value = c(5.0, 1.0),
#'    tc = 2592000,
#'    tc.g_value = 172800,
#'    n.MC = 100)
#'
#' ##(2) faded age: 1 ka
#' results <- calc_FadingCorr(
#'    age.faded = c(1,0),
#'    g_value = c(5.0, 1.0),
#'    tc = 2592000,
#'    tc.g_value = 172800,
#'    n.MC = 100)
#'
#' ##(3) faded age: 10.0 ka
#' results <- calc_FadingCorr(
#'    age.faded = c(10,0),
#'    g_value = c(5.0, 1.0),
#'    tc = 2592000,
#'    tc.g_value = 172800,
#'    n.MC = 100)
#'
#' ##access the last output
#' get_RLum(results)
#'
#' @export
calc_FadingCorr <- function(
  age.faded,
  g_value,
  tc = NULL,
  tc.g_value = tc,
  n.MC = 10000,
  seed = NULL,
  interval = c(0.01,500),
  txtProgressBar = TRUE,
  verbose = TRUE
){

  ##TODO set link after the function analyse_FadingMeasurement was released
  ## ... this option should be tested as well

  # Integrity checks ---------------------------------------------------------------------------
  stopifnot(!missing(age.faded), !missing(g_value))

  ##check input
  if(class(g_value)[1] == "RLum.Results"){
    if(g_value@originator == "analyse_FadingMeasurement"){

      tc <- get_RLum(g_value)[["TC"]]
      g_value <- as.numeric(get_RLum(g_value)[,c("FIT", "SD")])

    }else{
      try(stop("[calc_FadingCorr()] Unknown originator for the provided RLum.Results object via 'g_value'!", call. = FALSE))
      return(NULL)


    }


  }

  ##check if tc is still NULL
  if(is.null(tc)){
    try(stop("[calc_FadingCorr()] 'tc' needs to be set!", call. = FALSE))
    return(NULL)

  }


  ##============================================================================##
  ##DEFINE FUNCTION
  ##============================================================================##


  f <- function(x, af,kappa,tc){1-kappa*(log(x/tc)-1) - (af/x)}

  ##============================================================================##
  ##CALCULATION
  ##============================================================================##

  ##recalculate the g-value to the given tc ... should be similar
  ##of tc = tc.g_value
  ##re-calulation thanks to the help by Sebastien Huot, e-mail: 2016-07-19
  ##Please note that we take the vector for the g_value here
  k0 <- g_value / 100 / log(10)
  k1 <- k0 / (1 - k0 * log(tc[1]/tc.g_value[1]))
  g_value <-  100 * k1 * log(10)

  ##calculate kappa (equation [5] in Huntley and Lamothe, 2001)
  kappa <- g_value / log(10) / 100

  ##transform tc in ka years
  ##duration of the year over a long term taken from http://wikipedia.org
  tc <- tc[1] / 60 / 60 / 24 / 365.2425  / 1000
  tc.g_value <- tc.g_value[1] / 60 / 60 / 24 / 365.2425  / 1000

  ##calculate mean value
  temp <-
    uniroot(
      f,
      interval = interval,
      tol = 0.001,
      tc = tc,
      af = age.faded[1],
      kappa = kappa[1],
      check.conv = FALSE
    )

  ##--------------------------------------------------------------------------##
  ##Monte Carlo simulation for error estimation
  tempMC.sd.recent <- NA
  tempMC.sd.count <- 1:10
  counter <- 1

  ##show some progression bar of the process
  if (n.MC == 'auto') {
    n.MC.i <- 10000

    cat("\n[calc_FadingCorr()] ... trying to find stable error value ...")
    if (txtProgressBar) {
      cat("\n -------------------------------------------------------------\n")
      cat(paste0("   ",paste0("(",0:9,")", collapse = "   "), "\n"))
    }
  }else{
    n.MC.i <- n.MC

  }



  # Start loop  ---------------------------------------------------------------------------------

  ##set object and preallocate memory
  tempMC <- vector("numeric", length = 1e+07)
  tempMC[] <- NA
  i <- 1
  j <- n.MC.i

  while(length(unique(tempMC.sd.count))>1 | j > 1e+07){

    ##set previous
    if(!is.na(tempMC.sd.recent)){
      tempMC.sd.count[counter] <- tempMC.sd.recent

    }

    ##set seed
    if (!is.null(seed)) set.seed(seed)

    ##pre-allocate memory
    g_valueMC <- vector("numeric", length = n.MC.i)
    age.fadeMC <- vector("numeric", length = n.MC.i)
    kappaMC <- vector("numeric", length = n.MC.i)

    ##set-values
    g_valueMC <- rnorm(n.MC.i,mean = g_value[1],sd = g_value[2])
    age.fadedMC <- rnorm(n.MC.i,mean = age.faded[1],sd = age.faded[2])
    kappaMC <- g_valueMC / log(10) / 100



    ##calculate for all values
    tempMC[i:j] <- suppressWarnings(vapply(X = 1:length(age.fadedMC), FUN = function(x) {
      temp <- try(uniroot(
        f,
        interval = interval,
        tol = 0.001,
        tc = tc,
        af = age.fadedMC[[x]],
        kappa = kappaMC[[x]],
        check.conv = TRUE,
        maxiter = 1000,
        extendInt = "yes"
      ), silent = TRUE)

      ##otherwise the automatic error value finding
      ##will never work
      if(!is(temp,"try-error") && temp$root<1e8) {
        return(temp$root)
      } else{
        return(NA)
      }

    }, FUN.VALUE = 1))

    i <- j + 1
    j <- j + n.MC.i

    ##stop here if a fixed value is set
    if(n.MC != 'auto'){
      break
    }

    ##set recent
    tempMC.sd.recent <- round(sd(tempMC, na.rm = TRUE), digits = 3)

    if (counter %% 10 == 0) {
      counter <- 1

    }else{
      counter <- counter + 1

    }

    ##show progress in terminal
    if (txtProgressBar) {
      text <- rep("CHECK",10)
      if (counter %% 2 == 0) {
        text[1:length(unique(tempMC.sd.count))] <- "-----"
      }else{
        text[1:length(unique(tempMC.sd.count))] <- " CAL "
      }



      cat(paste("\r ",paste(rev(text), collapse = " ")))
    }

  }

  ##--------------------------------------------------------------------------##

  ##remove all NA values from tempMC
  tempMC <- tempMC[!is.na(tempMC)]

  ##obtain corrected age
  age.corr <- data.frame(
    AGE = round(temp$root, digits = 4),
    AGE.ERROR = round(sd(tempMC), digits = 4),
    AGE_FADED = age.faded[1],
    AGE_FADED.ERROR = age.faded[2],
    G_VALUE = g_value[1],
    G_VALUE.ERROR = g_value[2],
    KAPPA = kappa[1],
    KAPPA.ERROR = kappa[2],
    TC = tc,
    TC.G_VALUE = tc.g_value,
    n.MC = n.MC,
    OBSERVATIONS = length(tempMC),
    SEED = ifelse(is.null(seed), NA, seed)
  )

  ##============================================================================##
  ##OUTPUT VISUAL
  ##============================================================================##
  if(verbose) {
    cat("\n\n[calc_FadingCorr()]\n")
    cat("\n >> Fading correction according to Huntley & Lamothe (2001)")

    if (tc != tc.g_value) {
      cat("\n >> g-value re-calculated for the given tc")

    }

    cat(paste(
      "\n\n .. used g-value:\t",
      round(g_value[1], digits = 3),
      " \u00b1 ",
      round(g_value[2], digits = 3),
      " %/decade",
      sep = ""
    ))
    cat(paste(
      "\n .. used tc:\t\t",
      format(tc, digits = 4, scientific = TRUE),
      " ka",
      sep = ""
    ))
    cat(paste0(
      "\n .. used kappa:\t\t",
      round(kappa[1], digits = 4),
      " \u00b1 ",
      round(kappa[2], digits = 4)
    ))
    cat("\n ----------------------------------------------")
    cat(paste0("\n seed: \t\t\t", ifelse(is.null(seed), NA, seed)))
    cat(paste0("\n n.MC: \t\t\t", n.MC))
    cat(paste0(
      "\n observations: \t\t",
      format(length(tempMC), digits = 2, scientific = TRUE),
      sep = ""
    ))
    cat("\n ----------------------------------------------")
    cat(paste0(
      "\n Age (faded):\t\t",
      round(age.faded[1], digits = 4),
      " ka \u00b1 ",
      round(age.faded[2], digits = 4),
      " ka"
    ))
    cat(paste0(
      "\n Age (corr.):\t\t",
      round(age.corr[1], digits = 4),
      " ka \u00b1 ",
      round(age.corr[2], digits = 4),
      " ka"
    ))
    cat("\n ---------------------------------------------- \n")

  }

  ##============================================================================##
  ##OUTPUT RLUM
  ##============================================================================##
  return(set_RLum(
    class = "RLum.Results",
    data = list(age.corr = age.corr,
                age.corr.MC = tempMC),
    info = list(call = sys.call())
  ))

}

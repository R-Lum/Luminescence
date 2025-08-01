#'@title Fading Correction after Huntley & Lamothe (2001)
#'
#'@description Apply a fading correction according to Huntley & Lamothe (2001) for a given
#'\eqn{g}-value and a given \eqn{t_{c}}
#'
#'@details
#'This function solves the equation used for correcting the fading affected age
#'including the error for a given \eqn{g}-value according to Huntley & Lamothe (2001):
#'
#'\deqn{
#'\frac{A_{f}}{A} = 1 - \kappa * \Big[ln(\frac{A}{t_c}) - 1\Big]
#'}
#'
#'with \eqn{\kappa} defined as
#'
#'\deqn{
#'\kappa = \frac{\frac{\mathrm{g\_value}}{ln(10)}}{100}
#'}
#'
#' \eqn{A} and \eqn{A_{f}} are given in ka. \eqn{t_c} is given in s, however, it
#' is internally recalculated to ka.
#'
#' As the \eqn{g}-value slightly depends on the time \eqn{t_{c}} between
#' irradiation and the prompt measurement, a value for `tc` must always be
#' provided.
#' If the \eqn{g}-value was normalised to a distinct
#' time or evaluated with a different tc value (e.g., external irradiation), also
#' the \eqn{t_{c}} value for the \eqn{g}-value needs to be provided (argument `tc.g_value`
#' and then the \eqn{g}-value is recalculated
#' to \eqn{t_{c}} of the measurement used for estimating the age applying the
#' following equation:
#'
#' \deqn{\kappa_{tc} = \kappa_{tc.g} / (1 - \kappa_{tc.g} * ln(tc/tc.g))}
#'
#' where
#'
#' \deqn{\kappa_{tc.g} = g / 100 / ln(10)}
#'
#' The error of the fading-corrected age is determined using a Monte Carlo
#' simulation approach. Solving of the equation is performed using
#' [uniroot]. Large values for `n.MC` will significantly
#' increase the computation time.\cr
#'
#' **`n.MC = 'auto'`**
#'
#' The error estimation based on a stochastic process, i.e. for a small number of
#' MC runs the calculated error varies considerably every time the function is called,
#' even with the same input values.
#' The argument option `n.MC = 'auto'` tries to find a stable value for the standard error, i.e.
#' the standard deviation of values calculated during the MC runs (`age.corr.MC`),
#' within a given precision (2 digits) by increasing the number of MC runs stepwise and
#' calculating the corresponding error.
#'
#' If the determined error does not differ from the 9 values calculated previously
#' within a precision of (here) 3 digits the calculation is stopped as it is assumed
#' that the error is stable. Please note that (a) the duration depends on the input
#' values as well as on the provided computation resources and it may take a while,
#' (b) the length (size) of the output
#' vector `age.corr.MC`, where all the single values produced during the MC runs
#' are stored, equals the number of MC runs (here termed observations).
#'
#' To avoid an endless loop the calculation is stopped if the number of observations
#' exceeds 10^7.
#' This limitation can be overwritten by setting the number of MC runs manually,
#' e.g. `n.MC = 10000001`. Note: For this case the function is not checking whether the calculated
#' error is stable.\cr
#'
#' **`seed`**
#'
#' This option allows to recreate previously calculated results by setting the seed
#' for the R random number generator (see [set.seed] for details). This option
#' should not be mixed up with the option **`n.MC = 'auto'`**. The results may
#' appear similar, but they are not comparable!\cr
#'
#' **FAQ**\cr
#'
#' **Q**: Which \eqn{t_{c}} value is expected?\cr
#'
#' **A**: \eqn{t_{c}} is the time in seconds between irradiation and the prompt measurement
#' applied during your \eqn{D_{e}} measurement. However, this \eqn{t_{c}} might
#' differ from the \eqn{t_{c}} used for estimating the \eqn{g}-value. In the
#' case of an SAR measurement \eqn{t_{c}} should be similar, however,
#' if it differs, you have to provide this
#' \eqn{t_{c}}  value (the one used for estimating the \eqn{g}-value) using
#' the argument `tc.g_value`.\cr
#'
#' **Q**: The function could not find a solution, what should I do?\cr
#'
#' **A**: This usually happens for model parameters exceeding the boundaries of the
#' fading correction model (e.g., very high \eqn{g}-value). Please check
#' whether another fading correction model might be more appropriate.
#'
#' @param age.faded [numeric] [vector] (**required**):
#' vector of length 2 containing the uncorrected age and the error in ka
#' (see example).
#'
#' @param g_value [vector] or [RLum.Results-class] (**required**):
#' either a vector of length 2 containing the g-value and error obtained from
#' separate fading measurements (see example), or an [RLum.Results-class]
#' object produced by [analyse_FadingMeasurement]. If the latter, the `tc`
#' argument is set automatically.
#'
#' @param tc [numeric] (**required**):
#' time in seconds between irradiation and the prompt measurement (cf. Huntley & Lamothe 2001).
#' The argument is ignored when `g_value` is an [RLum.Results-class] object.
#'
#' @param tc.g_value [numeric] (*with default*):
#' time in seconds between irradiation and the prompt measurement used in the
#' estimation of the g-value. If the g-value was normalised, the normalisation
#' time (in seconds) should be given, e.g., for a g-value normalised to 2 days,
#' the value 172800 should be used. If nothing is provided the time is set to
#' `tc`, which is usual case for g-values obtained using the
#' SAR method and \eqn{g}-values that have been not normalised to 2 days.
#'
#' @param n.MC [integer] (*with default*):
#' number of Monte Carlo simulation runs for error estimation.
#' If `n.MC = 'auto'` is used the function tries to find a 'stable' error for
#' the age. See details for further information.
#' **Note:** This may take a while!
#'
#' @param seed [integer] (*optional*):
#' sets the seed for the random number generator in R using [set.seed]
#'
#' @param interval [numeric] (*with default*):
#' a vector containing the end-points (age interval) of the interval to be searched for the root in 'ka'.
#' This argument is passed to the function [stats::uniroot] used for solving the equation.
#'
#' @param txtProgressBar [logical] (*with default*):
#' enable/disable the progress bar.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @return Returns an S4 object of type [RLum.Results-class].\cr
#'
#' Slot: **`@data`**\cr
#' \tabular{lll}{
#' **Object** \tab **Type** \tab **Comment** \cr
#'  `age.corr` \tab [data.frame] \tab Corrected age \cr
#'  `age.corr.MC` \tab [numeric] \tab MC simulation results with all possible ages from that simulation \cr
#' }
#'
#' Slot: **`@info`**\cr
#'
#' \tabular{lll}{
#' **Object** \tab **Type** \tab **Comment** \cr
#'  `info` \tab [character] \tab the original function call
#' }
#'
#'
#' @note Special thanks to Sébastien Huot for his support and clarification via e-mail.
#'
#'
#' @section Function version: 0.4.4
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Results-class], [analyse_FadingMeasurement], [get_RLum], [uniroot]
#'
#' @references
#' Huntley, D.J., Lamothe, M., 2001. Ubiquity of anomalous fading
#' in K-feldspars and the measurement and correction for it in optical dating.
#' Canadian Journal of Earth Sciences, 38, 1093-1106.
#'
#' @keywords datagen
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
  tc,
  tc.g_value = tc,
  n.MC = 10000,
  seed = NULL,
  interval = c(0.01,500),
  txtProgressBar = TRUE,
  verbose = TRUE
){
  .set_function_name("calc_FadingCorr")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(age.faded, c("numeric", "integer"))
  .validate_length(age.faded, 2)
  .validate_class(g_value, c("numeric", "integer", "RLum.Results"))
  if(inherits(g_value, "RLum.Results")){
    if(g_value@originator == "analyse_FadingMeasurement"){
      tc <- get_RLum(g_value)[["TC"]]
      g_value <- as.numeric(get_RLum(g_value)[,c("FIT", "SD")])
    }else{
      .throw_message("Unknown originator for the 'g_value' object provided")
      return(NULL)
    }
  } else {
    .validate_length(g_value, 2)
  }

  ## tc is validated only now, as it may be set in the previous block
  .validate_class(tc, c("numeric", "integer"))
  .validate_positive_scalar(tc)
  .validate_positive_scalar(tc.g_value)
  .validate_class(interval, "numeric")
  .validate_length(interval, 2)
  .validate_logical_scalar(txtProgressBar)
  .validate_logical_scalar(verbose)


  ##============================================================================##
  ##DEFINE FUNCTION
  ##============================================================================##
  f <- function(x, af, kappa, tc) {
    1 - kappa * (log(x / tc) - 1) - (af / x)
  }

  ##============================================================================##
  ##CALCULATION
  ##============================================================================##

  ##recalculate the g-value to the given tc ... should be similar
  ##of tc = tc.g_value
  ##re-calculation thanks to the help by Sébastien Huot, e-mail: 2016-07-19
  ##Please note that we take the vector for the g_value here
  k0 <- g_value / 100 / log(10)
  k1 <- k0 / (1 - k0 * log(tc[1]/tc.g_value[1]))
  g_value <- 100 * k1 * log(10)

  ##calculate kappa (equation [5] in Huntley and Lamothe, 2001)
  kappa <- g_value / log(10) / 100

  ## transform tc from s to ka years
  tc <- tc / (1000 * .const$year_s)
  tc.g_value <- tc.g_value / (1000 * .const$year_s)

  ##calculate mean value
  temp <-
    try(suppressWarnings(uniroot(
      f,
      interval = interval,
      tol = 0.0001,
      tc = tc,
      extendInt = "yes",
      af = age.faded[1],
      kappa = kappa[1],
      check.conv = TRUE
    )), silent = TRUE)

  if(inherits(temp, "try-error")){
    .throw_message("No solution found, NULL returned: this usually happens ",
                   "for very large, unrealistic g-values, please consider ",
                   "another model for the fading correction")
    return(NULL)
  }

  ##--------------------------------------------------------------------------##
  ##Monte Carlo simulation for error estimation
  tempMC.sd.recent <- NA
  tempMC.sd.count <- 1:10
  counter <- 1

  ## show a progress bar of the process
  if (n.MC == 'auto') {
    n.MC.i <- 10000

    cat("\n[calc_FadingCorr()] ... trying to find stable error value ...")
    if (txtProgressBar) {
      cat("\n -------------------------------------------------------------\n")
      cat(paste0("   ",paste0("(",0:9,")", collapse = "   "), "\n"))
    }
  }else{
    n.MC.i <- .validate_positive_scalar(n.MC, int = TRUE)
  }

  # Start loop  ---------------------------------------------------------------------------------

  ##set object and preallocate memory
  tempMC <- vector("numeric", length = if (n.MC == "auto") 1e+07 else n.MC)
  tempMC[] <- NA
  i <- 1
  j <- n.MC.i

  while (length(unique(tempMC.sd.count)) > 1 || j > length(tempMC)) {

    ##set previous
    if(!is.na(tempMC.sd.recent)){
      tempMC.sd.count[counter] <- tempMC.sd.recent
    }

    ##set seed
    if (!is.null(seed)) set.seed(seed)

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
      res <- NA
      if (!inherits(temp, "try-error") && temp$root < 1e8) {
        res <- temp$root
      }
      return(res)

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

  ## discard wild outliers, as they will bias the error if present
  tempMC <- tempMC[tempMC < 100 * stats::IQR(tempMC)]

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

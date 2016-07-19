#' Apply a fading correction according to Huntley & Lamothe (2001) for a given
#' g-value.
#'
#' This function runs the iterations that are needed to calculate the corrected
#' age including the error for a given g-value according to Huntley & Lamothe
#' (2001).
#'
#'
#' The error of the fading-corrected age is determined using a Monte Carlo
#' simulation approach. Solving of the equation is realised using
#' \code{\link{uniroot}}. Large values for \code{n.MCruns} will significantly
#' increase the computation time.\cr
#'
#' \bold{\code{n.MCruns = 'auto'}}
#'
#' The error estimation based on a stochastic process, i.e. for a small number of MC runs the calculated
#' error varies considerably every time the function is called, even with the same input values.
#' The argument option \code{n.MCruns = 'auto'} tries to find a stable value for the standard error, i.e.
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
#' e.g. \code{n.MCruns = 10000001}. Note: For this case the function is not checking whether the calculated
#' error is stable.\cr
#'
#'
#' \bold{\code{seed}}
#'
#' This option allows to recreate previously calculated results by setting the seed
#' for the R random number generator (see \code{\link{set.seed}} for details). This option
#' should not be mixed up with the option \bold{\code{n.MCruns = 'auto'}}. The results may
#' appear similar, but they are not comparable!
#'
#'
#' @param g_value \code{\link{vector}} (\bold{required}): g-value and error obtained
#' from separate fading measurements (see example)
#'
#' @param tc \code{\link{numeric}} (\bold{required}): time in seconds between
#' irradiation and the prompt measurement (cf. Huntley & Lamothe 2001).
#'
#' @param tc.g_value \code{\link{numeric}} (with default): the time in seconds between irradiation
#' and the prompt measurement used for estimating the g-value. If the g-value was normalised
#' to, e.g., 2 days, this time in seconds (i.e., 172800) should be given here. If nothing is provided
#' the time is set to tc, which is usual case for g-values obtained using the SAR method and g-values
#' that had been not normalised to 2 days.
#'
#' @param age.faded \code{\link{numeric}} \code{\link{vector}} (\bold{required}): uncorrected
#' age with error in ka (see example)
#'
#' @param n.MCruns \code{\link{integer}} (with default): number of Monte Carlo
#' simulation runs for error estimation. If \code{n.MCruns = 'auto'} is used the function
#' tries to find a 'stable' error for the age. Note: This may take a while!
#'
#' @param seed \code{\link{integer}} (optional): sets the seed for the random number generator
#' in R using \code{\link{set.seed}}
#'
#' @param txtProgressBar \link{logical} (with default): enables or disables
#' \code{\link{txtProgressBar}}
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
#'
#' @note The upper age limit is set to 500 ka! \cr
#' Special thanks to Sebastien Huot for his support via e-mail.
#'
#'
#' @section Function version: 0.4.0
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
#' ##run the examples given in Huntley and Lamothe, 2001
#' ##(1) calculate g-value for given kappa
#' g_value <- c(100 * 0.0231 * log(10), 100 * 0.0044 * log(10))
#'
#' ##run examples from the appendix
#' ##100 a
#' results <- calc_FadingCorr(
#'    g_value,
#'    tc = 2592000,
#'    age.faded = c(0.1,0),
#'    n.MCruns=100)
#'
#' ## 1 ka
#' results <- calc_FadingCorr(
#'    g_value,
#'    tc = 2592000,
#'    age.faded = c(1,0),
#'    n.MCruns=100)
#'
#' ## 10.0 ka
#' results <- calc_FadingCorr(
#'    g_value,
#'    tc = 2592000,
#'    age.faded = c(10,0),
#'    n.MCruns=100)
#'
#' get_RLum(results)
#'
#' @export
calc_FadingCorr <- function(
  g_value,
  tc,
  tc.g_value = tc,
  age.faded,
  n.MCruns = 10000,
  seed = NULL,
  txtProgressBar = TRUE
){

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
  k0 <- g_value / log(10) / 100
  k1 <- k0 / (1 - k0 * log(tc.g_value/tc))
  g_value <-  100 * k1 * log(10)

  ##calculate kappa (equation [5] in Huntley and Lamothe, 2001)
  kappa <- g_value[1] / log(10) / 100

  ##transform tc in ka years
  ##duration of the year over a long term taken from Wikipedia
  tc <- tc / 60 / 60 / 24 / 365.2425  / 1000

  ##calculate mean value
  temp <-
    uniroot(
      f,
      c(0.1, 500),
      tol = 0.001,
      tc = tc,
      af = age.faded[1],
      kappa = kappa,
      check.conv = FALSE
    )

  ##--------------------------------------------------------------------------##
  ##Monte Carlo simulation for error estimation
  tempMC.sd.recent <- NA
  tempMC.sd.count <- 1:10
  counter <- 1

  ##show some progression bar of the process
  if (n.MCruns == 'auto') {
    n.MCruns.i <- 10000

    cat("\n[calc_FadingCorr()] ... trying to find stable error value ...")
    if (txtProgressBar) {
      cat("\n -------------------------------------------------------------\n")
      cat(paste0("   ",paste0("(",0:9,")", collapse = "   "), "\n"))
    }
  }else{
    n.MCruns.i <- n.MCruns

  }



  # Start loop  ---------------------------------------------------------------------------------

  ##set object and preallocate memory
  tempMC <- vector("numeric", length = 1e+07)
  tempMC[] <- NA
  i <- 1
  j <- n.MCruns.i

  while(length(unique(tempMC.sd.count))>1 | j > 1e+07){

    ##set previous
    if(!is.na(tempMC.sd.recent)){
      tempMC.sd.count[counter] <- tempMC.sd.recent

    }

    ##set seed
    if (!is.null(seed)) set.seed(seed)

    ##pre-allocate memory
    g_valueMC <- vector("numeric", length = n.MCruns.i)
    age.fadeMC <- vector("numeric", length = n.MCruns.i)
    kappaMC <- vector("numeric", length = n.MCruns.i)

    ##set-values
    g_valueMC <- rnorm(n.MCruns.i,mean = g_value[1],sd = g_value[2])
    age.fadedMC <- rnorm(n.MCruns.i,mean = age.faded[1],sd = age.faded[2])
    kappaMC <- g_valueMC / log(10) / 100



    ##calculate for all values
    tempMC[i:j] <- suppressWarnings(vapply(X = 1:length(age.fadedMC), FUN = function(x) {
      temp <- try(uniroot(
        f,
        c(0.1,500),
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
    j <- j + n.MCruns.i

    ##stop here if a fixed value is set
    if(n.MCruns != 'auto'){
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
    TC = tc,
    TC.G_VALUE = tc.g_value,
    N.MCRUNS = n.MCruns,
    OBSERVATIONS = length(tempMC),
    SEED = ifelse(is.null(seed), NA, seed)
  )

  ##============================================================================##
  ##OUTPUT VISUAL
  ##============================================================================##

  cat("\n\n[calc_FadingCorr()]\n")
  cat("\n Fading correction according to Huntley & Lamothe (2001):\n")
  cat(paste("\n Age (faded):\t\t",age.faded[1]," ka \u00b1 ",
            age.faded[2]," ka",sep=""))
  cat(paste("\n .. used g-value:\t",round(g_value[1], digits = 3), " \u00b1 ",
            round(g_value[2], digits = 3)," %/decade",sep=""))
  cat(paste("\n .. used tc:\t\t",format(tc, digits = 4, scientific = TRUE), " ka",sep=""))
  cat(paste("\n .. used kappa:\t\t",mean(kappa),sep=""))
  cat("\n ------------------------------------")
  cat(paste0("\n seed: \t\t\t", ifelse(is.null(seed), NA, seed)))
  cat(paste0("\n n.MCruns: \t\t",n.MCruns))
  cat(paste0("\n observations: \t\t",
            format(length(tempMC), digits = 2, scientific =TRUE),sep=""))
  cat("\n ------------------------------------")
  cat(paste("\n Age (corr.): ",age.corr[1]," ka \u00b1 ",age.corr[2]," ka",sep=""))
  cat("\n ------------------------------------\n")

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


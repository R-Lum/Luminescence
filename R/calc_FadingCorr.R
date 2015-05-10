#' Apply a fading correction according to Huntley & Lamothe (2001) for a given
#' g-value.
#'
#' This function runs the iterations that are needed to calculate the corrected
#' age including the error for a given g-value according to Huntley & Lamothe
#' (2001).
#'
#' The error of the fading-corrected age is determined using a Monte Carlo
#' simulation approach. Solving of the equation is realised using
#' \code{\link{uniroot}} Large values for \code{n.MCruns} will significantly
#' increase the computation time.
#'
#' @param g_value \link{vector} (\bold{required}): g-value and error obtained
#' from separate fading measurements (see example)
#' @param tc \link{numeric} (\bold{required}): time in seconds (time between
#' irradiation and the prompt measurement, cf. Huntely & Lamothe 2001)
#' @param age.faded \link{numeric} \link{vector} (\bold{required}): uncorrected
#' age with error in ka (see example)
#' @param n.MCruns \link{integer} (with default): number of Monte Carlo
#' simulation runs for error estimation
#' @return Returns an S4 object of type \code{\linkS4class{RLum.Results}}. Slot
#' \code{data} contains a \code{\link{list}} with the following structure:\cr $
#' age.corr (data.frame) .. $ age \cr .. $ age.error \cr $ age.corr.MC
#' (numeric)\cr
#'
#' \code{Age.corr.MC} contain all possible ages from the Monte Carlo (error)
#' simulation.
#' @note The upper age limit is set to 500 ka!
#' @section Function version: 0.2
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' @seealso \code{\linkS4class{RLum.Results}}, \code{\link{get_RLum.Results}},
#' \code{\link{uniroot}}
#' @references Huntley, D.J., Lamothe, M., 2001. Ubiquity of anomalous fading
#' in K-feldspars and the measurement and correction for it in optical dating.
#' Canadian Journal of Earth Sciences, 38, 1093-1106.
#' @keywords datagen
#' @examples
#'
#'
#' results <- calc_FadingCorr(g_value = c(3.3,0.03), tc = 752,
#'                 age.faded = c(100,10),
#'                 n.MCruns=100)
#'
#' get_RLum.Results(results)
#'
#'
calc_FadingCorr <- function(
  g_value,
  tc,
  age.faded,
  n.MCruns = 10000
){

  ##============================================================================##
  ##DEFINE FUNCTION
  ##============================================================================##

  f <- function(x, af,kappa,tc){1-kappa*(log(x/tc)-1) - (af/x)}

  ##============================================================================##
  ##CALCULATION
  ##============================================================================##

  ##calculate kappa
  kappa<-g_value[1]/log(10)/100

  ##transform tc in ka years
  tc<-tc/60/60/24/365/1000

  ##calculate mean value
  temp <- uniroot(f, c(0.1,500), tol = 0.001, tc = tc, af = age.faded[1], kappa = kappa,
                  check.conv = TRUE)

  ##--------------------------------------------------------------------------##
  ##Monte Carlo simulation for error estimation

    ##pre-allocate memory
    g_valueMC <- vector("numeric", length = n.MCruns)
    age.fadeMC <- vector("numeric", length = n.MCruns)
    kappaMC <- vector("numeric", length = n.MCruns)

    ##set-values
    g_valueMC <- rnorm(n.MCruns,mean=g_value[1],sd=g_value[2])
    age.fadedMC <- rnorm(n.MCruns,mean=age.faded[1],sd=age.faded[2])
    kappaMC <- g_valueMC/log(10)/100


    ##calculate for all values
    tempMC <- lapply(1:length(age.fadedMC),function(x) {
      uniroot(
        f,
        c(0.1,500),
        tol = 0.001,
        tc = tc,
        af = age.fadedMC[[x]],
        kappa = kappaMC[[x]],
        check.conv = TRUE
      )$root

    })

  ##--------------------------------------------------------------------------##

  ##obtain corrected age
  age.corr<-data.frame(age=round(temp$root, digits = 2),
                       age.error=round(sd(unlist(tempMC)),digits=2))

  ##============================================================================##
  ##OUTPUT VISUAL
  ##============================================================================##

  cat("\n[calc_FadingCorr()]")
  cat("\n\t Fading correction according to Huntley & Lamothe (2001):\n")
  cat(paste("\n\t Age (faded): ",age.faded[1]," ka \u00b1 ",
            age.faded[2]," ka",sep=""))
  cat(paste("\n\t g-value: ",g_value[1], "%/decade \u00b1 ",
            g_value[2]," %/decade",sep=""))
  cat(paste("\n\t tc: ",format(tc, digits = 4, scientific = TRUE), " ka",sep=""))
  cat(paste("\n\t kappa: ",mean(kappa),sep=""))
  cat(paste("\n\t observations: ",
            format(length(unlist(tempMC)), digits = 2, scientific =TRUE),sep=""))
  cat("\n\n\t ----------------------------------")
  cat(paste("\n\t Age (corr.): ",age.corr[1]," ka \u00b1 ",age.corr[2]," ka",sep=""))
  cat("\n\t ----------------------------------\n")

  ##============================================================================##
  ##OUTPUT RLUM
  ##============================================================================##

  temp.RLum.Results <- set_RLum.Results(data = list(
    age.corr = age.corr,
    age.corr.MC = unlist(tempMC)
  ))

  return(temp.RLum.Results)

}

#' Apply the (un-)logged common age model after Galbraith et al. (1999) to a
#' given De distribution
#'
#' Function to calculate the common dose of a De distribution.
#'
#' **(Un-)logged model** 
#'
#' When `log = TRUE` this function
#' calculates the weighted mean of logarithmic De values. Each of the estimates
#' is weighted by the inverse square of its relative standard error. The
#' weighted mean is then transformed back to the dose scale (Galbraith &
#' Roberts 2012, p. 14).
#' 
#' The log transformation is not applicable if the
#' De estimates are close to zero or negative. In this case the un-logged model
#' can be applied instead (`log = FALSE`). The weighted mean is then
#' calculated using the un-logged estimates of De and their absolute standard
#' error (Galbraith & Roberts 2012, p. 14).
#'
#' @param data [RLum.Results-class] or [data.frame] (**required**): 
#' for [data.frame]: two columns with De `(data[,1])` and De error `(data[,2])`
#' 
#' @param sigmab [numeric] (*with default*): 
#' additional spread in De values.
#' This value represents the expected overdispersion in the data should the sample be 
#' well-bleached (Cunningham & Walling 2012, p. 100).
#' **NOTE**: For the logged model (`log = TRUE`) this value must be
#' a fraction, e.g. 0.2 (= 20 \%). If the un-logged model is used (`log = FALSE`),
#' sigmab must be provided in the same absolute units of the De values (seconds or Gray).
#' 
#' @param log [logical] (*with default*): 
#' fit the (un-)logged central age model to De data
#' 
#' @param ... currently not used.
#' 
#' @return 
#' Returns a terminal output. In addition an
#' [RLum.Results-class] object is returned containing the
#' following element:
#'
#' \item{.$summary}{[data.frame] summary of all relevant model results.}
#' \item{.$data}{[data.frame] original input data} 
#' \item{.$args}{[list] used arguments} 
#' \item{.$call}{[call] the function call}
#'
#' The output should be accessed using the function [get_RLum]
#' 
#' @section Function version: 0.1.1
#' 
#' @author 
#' Christoph Burow, University of Cologne (Germany)
#' 
#' @seealso [calc_CentralDose], [calc_FiniteMixture],
#' [calc_FuchsLang2001], [calc_MinDose]
#' 
#' @references 
#' Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for
#' mixed fission track ages. Nuclear Tracks Radiation Measurements 4, 459-470. 
#'
#' Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley,
#' J.M., 1999. Optical dating of single grains of quartz from Jinmium rock
#' shelter, northern Australia. Part I: experimental design and statistical
#' models.  Archaeometry 41, 339-364. 
#'
#' Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose and error calculation and
#' display in OSL dating: An overview and some recommendations. Quaternary
#' Geochronology 11, 1-27. 
#'
#' **Further reading** 
#'
#' Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain equivalent dose
#' (De) distributions: Implications for OSL dating of sediment mixtures.
#' Quaternary Geochronology 4, 204-230. 
#' 
#' Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain quartz De distributions and an
#' assessment of procedures for estimating burial dose. Quaternary Science
#' Reviews 25, 2475-2502. 
#'
#' Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial archives using robust OSL chronologies.
#' Quaternary Geochronology 12, 98-106. 
#'
#' Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the reproducibility and accuracy
#' of optical dating of fluvial deposits.  Quaternary Geochronology, 1 109-120. 
#'
#' Rodnight, H., 2008. How many equivalent dose values are needed to
#' obtain a reproducible distribution?. Ancient TL 26, 3-10.
#' 
#' @examples
#'
#' ## load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## apply the common dose model
#' calc_CommonDose(ExampleData.DeValues$CA1)
#'
#' @md
#' @export
calc_CommonDose <- function(
  data,
  sigmab,
  log=TRUE,
  ...
) {
  
  ##============================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ##============================================================================##

  if (!is.data.frame(data) && !is(data,"RLum.Results")) {
    stop("[calc_CentralDose] Error: 'data' object has to be of type ",
         "'data.frame' or 'RLum.Results'!")
  }
  if (is(data, "RLum.Results")) {
    data <- get_RLum(data, "data")
  }
  if (ncol(data) < 2) {
    stop("[calc_FiniteMixture()] 'data' object must have two columns",
         call. = FALSE)
  }
  if(!missing(sigmab)) {
    if (sigmab < 0 || sigmab > 1) {
      stop("[calc_FiniteMixture()] 'sigmab' must be a value between 0 and 1",
           call. = FALSE)
    }
  }

  ## set expected column names
  colnames(data)[1:2] <- c("ED", "ED_Error")


  ##============================================================================##
  ## ADDITIONAL ARGUMENTS
  ##============================================================================##
  settings <- list(verbose = TRUE)
  settings <- modifyList(settings, list(...))
  
  ##============================================================================##
  ## CALCULATIONS
  ##============================================================================##
  
  # set default value of sigmab
  if (missing(sigmab)) sigmab<- 0
  
  # calculate  yu = log(ED) and su = se(logED)
  if (log) {
    yu<- log(data$ED)
    su<- sqrt( (data$ED_Error/data$ED)^2 + sigmab^2 )
  }
  else {
    yu<- data$ED
    su<- sqrt((data$ED_Error)^2 + sigmab^2)
  }
  
  # calculate weights
  wu<- 1/su^2
  delta<- sum(wu*yu)/sum(wu)
  n<- length(yu)
  
  #standard error
  sedelta<- 1/sqrt(sum(wu))
  if (!log) {
    sedelta<- sedelta/delta
  }
  
  if (log){
    delta<- exp(delta)
  }
  
  ##============================================================================##
  ## TERMINAL OUTPUT
  ##============================================================================##
  
  if (settings$verbose) {
    cat("\n [calc_CommonDose]")
    cat(paste("\n\n----------- meta data --------------"))
    cat(paste("\n n:                      ",n))
    cat(paste("\n log:                    ",if(log==TRUE){"TRUE"}else{"FALSE"}))
    cat(paste("\n----------- dose estimate ----------"))
    cat(paste("\n common dose:            ", round(delta,2)))
    cat(paste("\n SE:                     ", round(delta*sedelta, 2)))
    cat(paste("\n rel. SE [%]:            ", round(sedelta*100,2)))
    cat(paste("\n------------------------------------\n\n"))
  }
  
  ##============================================================================##
  ## RETURN VALUES
  ##============================================================================##
  
  summary<- data.frame(de=delta,
                       de_err=delta*sedelta)
  
  call<- sys.call()
  args<- list(log=log, sigmab=sigmab)
  
  newRLumResults.calc_CommonDose<- set_RLum(
    class = "RLum.Results",
    data = list(summary = summary,
                data = data,
                args = args,
                call = call))
  
  invisible(newRLumResults.calc_CommonDose)
  
}

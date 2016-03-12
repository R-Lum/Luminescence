#' Calculate the Fast Ratio for CW-OSL curves
#' 
#' This function calculates the fast ratio of CW-OSL curves after
#' Durcan & Duller (2001). 
#' 
#' Some more details on the function.
#'
#' @param object \code{\linkS4class{RLum.Analysis}}, 
#' \code{\linkS4class{RLum.Data.Curve}} or \code{\link{data.frame}} 
#' (\bold{required}):
#' 
#' @param stimulation.power \code{\link{numeric}}:
#' 
#' @param wavelength \code{\link{numeric}}:
#' 
#' @param sigmaF \code{\link{numeric}}:
#' 
#' @param sigmaM \code{\link{numeric}}:
#' 
#' @param Ch_L1 \code{\link{numeric}}:
#' 
#' @param x \code{\link{numeric}}:
#' 
#' @param x2 \code{\link{numeric}}:
#' 
#' @param dead.channels \code{\link{numeric}}: Vector of length 2 in the form of
#' \code{c(x, y)}.
#' 
#' @param ... available options: \code{verbose} \code{\link{logical}}.
#'
#' @return Returns an S4 object of type \code{\linkS4class{RLum.Results}} and the slot
#' \code{data} contains a \code{\link{list}} with the following structure:\cr
#' $ selection (data.frame) \cr
#' .. $ summary \cr
#' .. $ data \cr
#' .. $ args \cr
#' .. $ call \cr
#' 
#' @section Function version: 0.1.0
#'
#' @author 
#' Georgina King, University of Cologne (Germany) \cr
#' Julie A. Durcan, Institute of Geography and Earth Sciences, Aberystwyth University (United Kingdom) \cr
#' Christoph Burow, University of Cologne (Germany) \cr
#'
#' @references 
#' Durcan, J.A. & Duller, G.A.T., 2011. The fast ratio: A rapid measure for testing
#' the dominance of the fast component in the initial OSL signal from quartz.
#' Radiation Measurements 46, 1065-1072. \cr
#'
#' @examples
#' # load example CW-OSL curve
#' data("ExampleData.CW_OSL_Curve")
#' 
#' # calculate the fast ratio w/o further adjustments
#' res <- calc_FastRatio(ExampleData.CW_OSL_Curve)
#' 
#' # show the summary table
#' get_RLum(res)
#'
#' @export
calc_FastRatio <- function(object, 
                           stimulation.power = 30.6, 
                           wavelength = 470,
                           sigmaF = 2.6E-17,
                           sigmaM = 4.28E-18,
                           Ch_L1 = 1,
                           x = 1,
                           x2 = 0.1,
                           dead.channels = c(0,0),
                           ...) {
  
  ## Input object handling -----------------------------------------------------
  if (inherits(object, "RLum.Analysis"))
    object <- get_RLum(object)
  
  if (!inherits(object, "list"))
    object <-list(object)
  
  ## Settings ------------------------------------------------------------------
  settings <- list()
  
  # override defaults with args in ... [currently not used]
  settings <- modifyList(settings, list(...))
  
  
  ## Calculations --------------------------------------------------------------
  # iterate over all user provided objects and calculate the FR
  fast.ratios <- lapply(object, function(obj) {
    
    if (inherits(obj, "RLum.Data.Curve"))
      A <- get_RLum(obj)
    else
      A <- obj
    
    ## Energy calculation
    # P = user defined stimulation power in mW
    # lambdaLED = wavelength of stimulation source in nm
    P <- stimulation.power 
    lamdaLED <- wavelength
    
    ## Constants
    # h = speed of light, h = Planck's constant
    h <- 6.62607004E-34
    c <- 299792458
    
    I0 <- (P / 1000) / (h * c / (lamdaLED * 10^-9))
    Ch_width <- max(A[ ,1]) / length(A[ ,1])
    
    # remove dead channels
    A <- A[(dead.channels[1] + 1):(nrow(A)-dead.channels[2]), ]
    
    ## The equivalent time in s of L1, L2, L3
    # Use these values to look up the channel
    t_L1 <- 0
    t_L2 <- (log(x / 100)) / (-sigmaF * I0)
    t_L3_start <- (log(x / 100)) / (-sigmaM * I0)
    t_L3_end <- (log(x2 / 100)) / (-sigmaM * I0)
    
    ## Channel number(s) of L2 and L3
    Ch_L2 <- floor(t_L2 / Ch_width)
    if (dead.channels[1] > 0)
      Ch_L2 <- Ch_L2 - dead.channels[1]
    
    Ch_L3st<- floor(t_L3_start / Ch_width)
    Ch_L3end <- floor(t_L3_end / Ch_width)
    
    ## Counts in channels L1, L2, L3
    Cts_L1 <- A[Ch_L1, 2]
    
    if (Ch_L2 > nrow(A)) {
      warning(sprintf(paste("The calculated channel for L2 (%i)", 
                            "is larger than available channels (%i).",
                            "Returned NULL."), Ch_L2, nrow(A)), 
              call. = FALSE)
      return(NULL)
    } else {
      Cts_L2 <- A[Ch_L2, 2]
    }
    
    if (Ch_L3st >= nrow(A) | Ch_L3end > nrow(A)) {
      warning(sprintf(paste("The calculated channels for L3 (%i, %i)", 
                            "are larger than available channels (%i).",
                            "The background was estimated from the last",
                            "5 channels instead."), Ch_L3st, Ch_L3end, nrow(A)), 
              call. = FALSE)
      Cts_L3 <- mean(A[(nrow(A)-5):(nrow(A)), 2])
    } else {
      Cts_L3 <- mean(A[Ch_L3st:Ch_L3end, 2])
    }
    
    ## Fast Ratio
    FR <- (Cts_L1-Cts_L3) / (Cts_L2-Cts_L3)
    if (length(FR) != 1)
      FR <- NA
    
    ## Return values -----------------------------------------------------------
    summary <- data.frame(fast.ratio = FR,
                          channels = nrow(A),
                          t_L1 = t_L1,
                          t_L2 = t_L2,
                          t_L3_start = t_L3_start,
                          t_L3_end = t_L3_end,
                          Ch_L1 = Ch_L1,
                          Ch_L2 = Ch_L2,
                          Ch_L3_start = Ch_L3st,
                          Ch_L3_end = Ch_L3end,
                          Cts_L1 = Cts_L1,
                          Cts_L2 = Cts_L2,
                          Cts_L3 = Cts_L3)
    
    fast.ratio <- set_RLum(class = "RLum.Results",
                           originator = "calc_FastRatio",
                           data = list(summary = summary,
                                       data = obj,
                                       args = as.list(sys.call(-2L)[-1]),
                                       call = sys.call(-2L))
    )
    
    return(fast.ratio)
    
  })
  if (length(fast.ratios) == 1)
    fast.ratios <- fast.ratios[[1]]
  invisible(fast.ratios)
}

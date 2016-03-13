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
#' @param plot \code{\link{numeric}}: plot output (\code{TRUE}/\code{FALSE})
#' 
#' @param ... available options: \code{verbose} \code{\link{logical}}.
#'
#' @return Returns a plot (optional) and an S4 object of type \code{\linkS4class{RLum.Results}}. 
#' The slot \code{data} contains a \code{\link{list}} with the following structure:\cr
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
                           plot = TRUE,
                           ...) {
  
  ## Input object handling -----------------------------------------------------
  if (inherits(object, "RLum.Analysis"))
    object <- get_RLum(object)
  
  if (!inherits(object, "list"))
    object <-list(object)
  
  ## Settings ------------------------------------------------------------------
  settings <- list(verbose = TRUE,
                   info = list())
  
  # override defaults with args in ...
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
    A <- as.data.frame(A[(dead.channels[1] + 1):(nrow(A)-dead.channels[2]), ])
    A[ ,1] <- A[ ,1] - A[1,1]
    
    ## The equivalent time in s of L1, L2, L3
    # Use these values to look up the channel
    t_L1 <- 0
    t_L2 <- (log(x / 100)) / (-sigmaF * I0)
    t_L3_start <- (log(x / 100)) / (-sigmaM * I0)
    t_L3_end <- (log(x2 / 100)) / (-sigmaM * I0)
    
    ## Channel number(s) of L2 and L3
    Ch_L2 <- which.min(abs(A[,1] - t_L2))
    
    if (Ch_L2 <= 1) {
      msg <- sprintf("Calculated time/channel for L2 is too small (%.f, %.f). Returned NULL.", 
              t_L2, Ch_L2)
      settings$info <- modifyList(settings$info, list(L2 = msg))
      warning(msg, call. = FALSE)
      return(NULL)
    }
    
    Ch_L3st<- which.min(abs(A[,1] - t_L3_start))
    Ch_L3end <- which.min(abs(A[,1] - t_L3_end))
    
    ## Counts in channels L1, L2, L3
    Cts_L1 <- A[Ch_L1, 2]
    
    if (Ch_L2 > nrow(A)) {
      msg <- sprintf(paste("The calculated channel for L2 (%i)", 
                           "is larger than available channels (%i).",
                           "Returned NULL."), Ch_L2, nrow(A))
      settings$info <- modifyList(settings$info, list(L2 = msg))
      warning(msg, call. = FALSE)
      return(NULL)
    } else {
      Cts_L2 <- A[Ch_L2, 2]
    }
    
    if (Ch_L3st >= nrow(A) | Ch_L3end > nrow(A)) {
      msg <- sprintf(paste("The calculated channels for L3 (%i, %i)", 
                           "are larger than available channels (%i).",
                           "The background was estimated from the last",
                           "5 channels instead."), Ch_L3st, Ch_L3end, nrow(A))
      settings$info <- modifyList(settings$info, list(L3 = msg))
      warning(msg, call. = FALSE)
      Ch_L3st <- nrow(A) - 5
      Ch_L3end <- nrow(A)
    }
    Cts_L3 <- mean(A[Ch_L3st:Ch_L3end, 2])
    
    # Warn if counts are not in decreasing order
    if (Cts_L3 >= Cts_L2)
      warning(sprintf("L3 contains more counts (%.f) than L2 (%.f).",
                      Cts_L3, Cts_L2), call. = FALSE)
    
    
    ## Fast Ratio
    FR <- (Cts_L1-Cts_L3) / (Cts_L2-Cts_L3)
    if (length(FR) != 1)
      FR <- NA
    
    ## Return values -----------------------------------------------------------
    summary <- data.frame(fast.ratio = FR,
                          channels = nrow(A),
                          channel.width = Ch_width,
                          dead.channels.start = dead.channels[1],
                          dead.channels.end = dead.channels[2],
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
                                       call = sys.call(-2L)),
                           info = settings$info
    )
    
    ## Console Output ----------------------------------------------------------
    if (settings$verbose) {
      
      table.names <- c(
        "Fast Ratio\t", "Channels\t", "Channel width (s)", "Dead channels start", "Dead channels end",
        "-\n Time L1 (s)\t", "Time L2 (s)\t", "Time L3 start (s)", "Time L3 end (s)",
        "-\n Channel L1\t", "Channel L2\t", "Channel L3 start", "Channel L3 end\t",
        "-\n Counts L1\t", "Counts L2\t", "Counts L3\t")
      
      cat("\n[calc_FastRatio()]\n")
      cat("\n -------------------------------")
      for (i in 1:ncol(summary)) {
        cat(paste0("\n ", table.names[i],"\t: ",
                   format(summary[1, i], digits = 2, nsmall = 2)))
      }
      cat("\n -------------------------------\n\n")
      
    }
    ## Plotting ----------------------------------------------------------------
    if (plot) 
      try(plot_RLum.Results(fast.ratio, ...))
    
    # return
    return(fast.ratio)
  }) # End of lapply
  
  if (length(fast.ratios) == 1)
    fast.ratios <- fast.ratios[[1]]
  
  invisible(fast.ratios)
}

#' @title Apply the model after Fuchs & Lang (2001) to a given De distribution
#'
#' @description
#' This function applies the method according to Fuchs & Lang (2001) for
#' heterogeneously bleached samples with a given coefficient of variation
#' threshold.
#'
#' @details
#'
#' **Used values**
#'
#' If the coefficient of variation (`c[v]`) of the first
#' two values is larger than the threshold `c[v_threshold]`, the first value is
#' skipped.  Use the `startDeValue` argument to define a start value for
#' calculation (e.g. 2nd or 3rd value).
#'
#' **Basic steps of the approach**
#'
#' 1. Estimate natural relative variation of the sample using a dose recovery test
#' 2. Sort the input values in ascending order
#' 3. Calculate a running mean, starting with the lowermost two values and add values iteratively.
#' 4. Stop if the calculated `c[v]` exceeds the specified `cvThreshold`
#'
#' @param data [Luminescence::RLum.Results-class] or [data.frame] (**required**):
#' for [data.frame]: two columns with De `(data[,1])` and De error `(values[,2])`
#'
#' @param cvThreshold [numeric] (*with default*):
#' coefficient of variation in percent, as threshold for the method,
#' e.g. `cvThreshold = 3`. See details
#' .
#' @param startDeValue [numeric] (*with default*):
#' number of the first aliquot that is used for the calculations
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param ... further arguments and graphical parameters passed to [plot]
#'
#' @return
#' Returns a plot (*optional*) and terminal output. In addition an
#' [Luminescence::RLum.Results-class] object is returned containing the
#' following elements:
#'
#' \item{summary}{[data.frame] summary of all relevant model results.}
#' \item{data}{[data.frame] original input data}
#' \item{args}{[list] used arguments}
#' \item{call}{[call] the function call}
#' \item{usedDeValues}{[data.frame] containing the used values for the calculation}
#'
#' @note Please consider the requirements and the constraints of this method
#' (see Fuchs & Lang, 2001)
#'
#' @section Function version: 0.4.1
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany) \cr
#' Christoph Burow, University of Cologne (Germany)
#'
#' @seealso [plot], [Luminescence::calc_MinDose], [Luminescence::calc_FiniteMixture],
#' [Luminescence::calc_CentralDose], [Luminescence::calc_CommonDose], [Luminescence::RLum.Results-class]
#'
#' @references
#' Fuchs, M. & Lang, A., 2001. OSL dating of coarse-grain fluvial
#' quartz using single-aliquot protocols on sediments from NE Peloponnese,
#' Greece. In: Quaternary Science Reviews 20, 783-787.
#'
#' Fuchs, M. & Wagner, G.A., 2003. Recognition of insufficient bleaching by
#' small aliquots of quartz for reconstructing soil erosion in Greece.
#' Quaternary Science Reviews 22, 1161-1167.
#'
#' @keywords dplot
#'
#'
#' @examples
#' ## load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## calculate De according to Fuchs & Lang (2001)
#' temp<- calc_FuchsLang2001(ExampleData.DeValues$BT998, cvThreshold = 5)
#'
#' @export
calc_FuchsLang2001 <- function(
  data,
  cvThreshold = 5,
  startDeValue = 1,
  plot = TRUE,
  ...
) {
  .set_function_name("calc_FuchsLang2001")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(data, c("data.frame", "RLum.Results"))
  .validate_not_empty(data)
  if (inherits(data, "RLum.Results")) {
    data <- get_RLum(data, "data")
  }
  if (ncol(data) < 2) {
    .throw_error("'data' should have 2 columns")
  }
  .validate_positive_scalar(cvThreshold)
  .validate_positive_scalar(startDeValue, int = TRUE)
  if (startDeValue > nrow(data)) {
    .throw_error("'startDeValue' exceeds the number of rows in 'data' (",
                 nrow(data), ")")
  }
  .validate_logical_scalar(plot)

  # Deal with extra arguments -----------------------------------------------
  ##deal with addition arguments
  extraArgs <- list(...)
  verbose <- extraArgs$verbose %||% TRUE
  .validate_logical_scalar(verbose)

  ##============================================================================##
  ##PREPARE DATA
  ##============================================================================##

  ##1. order values in ascending order write used D[e] values in data.frame
  o <- order(data[[1]]) # o is only an order parameter
  data_ordered <- data[o,] # sort values after o and write them into a new variable

  ##2. estimate D[e]
  # set variables
  usedDeValues <- data.frame(De = NA, De_Error = NA, cv = NA)
  endDeValue <- startDeValue
  cv.col <- NULL

  ## write skipped values
  if (startDeValue > 1) {
    idx.skipped <- seq(startDeValue - 1)
    usedDeValues[idx.skipped, 1:2] <- data_ordered[idx.skipped, 1:2]
    cv.col[idx.skipped] <- "skipped"
  }

  ##=================================================================================================##
  ##LOOP FOR MODEL
  ##=================================================================================================##
  # repeat loop (run at least one time)
  for (endDeValue in startDeValue:nrow(data_ordered)) {
    ## calculate mean, sd and cv from ordered De values
    Des <- data_ordered[startDeValue:endDeValue, 1]
    mean <- round(mean(Des), digits = 2)
    sd <- round(sd(Des), digits = 2)
    cv <- round(sd / mean * 100, digits = 2) #calculate coefficient of variation

    ## avoid crashes if the both mean and sd are zero
    if (is.na(cv))
      cv <- 0

    ## write used De values
    usedDeValues[endDeValue, 1:2] <- data_ordered[endDeValue, 1:2]

    # break if cv > cvThreshold
    if (cv > cvThreshold && endDeValue > startDeValue) {

      # if the first two D[e] values give a cv > cvThreshold, than skip the first D[e] value
      if (endDeValue-startDeValue<2) {
        cv.col[endDeValue - 1] <- "not used"

        # go to the next D[e] value
        startDeValue <- startDeValue + 1
      } else {
        cv.col[endDeValue] <- paste("#", cv, "%")
        break #break loop
      }

    } else {
      cv.col[endDeValue] <- paste(cv, "%")
    }
  }

  ##=================================================================================================##
  ##ADDITIONAL CALCULATIONS and TERMINAL OUTPUT
  ##=================================================================================================##

  # additional calculate weighted mean
  usedValues <- data_ordered[startDeValue:endDeValue, ]
  w <- 1 / usedValues[, 2]^2 # weights for weighted mean
  weighted_mean <- round(stats::weighted.mean(usedValues[, 1], w),
                         digits = 2)
  weighted_sd <- round(sqrt(1 / sum(w)), digits = 2)
  n.usedDeValues <- nrow(usedValues)

  ## append column
  usedDeValues[1:length(cv.col), 3] <- cv.col
  rownames(usedDeValues) <- NULL

  # standard error
  se <- round(sd / sqrt(n.usedDeValues), digits = 2)

  if(verbose){
    cat("\n[calc_FuchsLang2001]")
    cat("\n\n----------- meta data --------------")
    cat("\n cvThreshold:            ", cvThreshold, "%")
    cat("\n used values:            ", n.usedDeValues)
    cat("\n----------- dose estimate ----------")
    cat("\n mean:                   ", mean)
    cat("\n sd:                     ", sd)
    cat("\n weighted mean:          ", weighted_mean)
    cat("\n weighted sd:            ", weighted_sd)
    cat("\n se:                     ", se)
    cat("\n------------------------------------\n\n")
  }

  ##===========================================================================#
  ##RETURN  VALUES
  ##==========================================================================##
  summary <- data.frame(
    de = mean,
    de_err = sd,
    de_weighted = weighted_mean,
    de_weighted_err = weighted_sd,
    se = se,
    n.usedDeValues = n.usedDeValues
  )

  args <- list(cvThreshold = cvThreshold, startDeValue = startDeValue)
  results <- set_RLum(
    class = "RLum.Results",
    data = list(
      summary = summary,
      data = data,
      args = args,
      usedDeValues = usedDeValues
    ),
    info = list(call = sys.call())
  )

  ##=========##
  ## PLOTTING
  if(plot) {
    try(plot_RLum.Results(results, ...),
        outFile = stdout()) # redirect error messages so they can be silenced
  }#endif::plot

  results
}

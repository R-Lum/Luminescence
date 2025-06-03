#' @title Apply the model after Fuchs & Lang (2001) to a given De distribution
#'
#' @description This function applies the method according to Fuchs & Lang (2001) for
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
#' @param data [RLum.Results-class] or [data.frame] (**required**):
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
#' [RLum.Results-class] object is returned containing the
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
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany) \cr
#' Christoph Burow, University of Cologne (Germany)
#'
#' @seealso [plot], [calc_MinDose], [calc_FiniteMixture], [calc_CentralDose],
#' [calc_CommonDose], [RLum.Results-class]
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
#' @md
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

  # Deal with extra arguments -----------------------------------------------
  ##deal with addition arguments
  extraArgs <- list(...)
  verbose <- if("verbose" %in% names(extraArgs)) {extraArgs$verbose} else {TRUE}

  ##============================================================================##
  ##PREPARE DATA
  ##============================================================================##

  ##1. order values in ascending order write used D[e] values in data.frame
  o <- order(data[[1]]) # o is only an order parameter
  data_ordered <- data[o,] # sort values after o and write them into a new variable

  ##2. estimate D[e]
  # set variables
  usedDeValues <- data.frame(De = NA, De_Error = NA, cv = NA)
  endDeValue <- startDeValue[1]

  # if the first D[e] values are not used write this information in the data.frame
  if (startDeValue[1] != 1) {
    n <- abs(1 - startDeValue[1])

    #  write used D[e] values in data.frame
    usedDeValues[1:n, 1] <- data_ordered[1:n, 1]
    usedDeValues[1:n, 2] <- data_ordered[1:n, 2]
    usedDeValues[1:n, 3] <- "skipped"
  }

  ##=================================================================================================##
  ##LOOP FOR MODEL
  ##=================================================================================================##
  # repeat loop (run at least one time)
  repeat {
    #calculate mean, sd and cv
    mean<-round(mean(data_ordered[startDeValue:endDeValue,1]),digits=2) #calculate mean from ordered D[e] values
    sd<-round(sd(data_ordered[startDeValue:endDeValue,1]),digits=2)		#calculate sd from ordered D[e] values
    cv <- round(sd / mean * 100, digits = 2) #calculate coefficient of variation

    ## avoid crashes if the both mean and sd are zero
    if (is.nan(cv))
      cv <- 0

    # break if cv > cvThreshold
    if (cv > cvThreshold[1] & endDeValue > startDeValue) {
      # if the first two D[e] values give a cv > cvThreshold, than skip the first D[e] value
      if (endDeValue-startDeValue<2) {
        #  write used D[e] values in data.frame
        usedDeValues[endDeValue, 1] <- data_ordered[endDeValue, 1]
        usedDeValues[endDeValue, 2] <- data_ordered[endDeValue, 2]
        usedDeValues[endDeValue - 1, 3] <- "not used"

        # go to the next D[e] value
        startDeValue <- startDeValue + 1

      } else {
        usedDeValues[endDeValue, 1] <- data_ordered[endDeValue, 1]
        usedDeValues[endDeValue, 2] <- data_ordered[endDeValue, 2]
        usedDeValues[endDeValue, 3] <- paste("# ", cv, " %", sep = "")

        break #break loop
      }

    }#EndIf
    else {

      # write used D[e] values in data.frame
      usedDeValues[endDeValue,1]<-data_ordered[endDeValue,1]
      usedDeValues[endDeValue,2]<-data_ordered[endDeValue,2]

      # first cv values alway contains NA to ensure that NA% is not printed test
      if(is.na(cv)==TRUE) {
        usedDeValues[endDeValue,3]<-cv
      } else {
        usedDeValues[endDeValue,3]<-paste(cv," %",sep="")
      }
    }#EndElse

    # go the next D[e] value until the maximum number is reached
    if (endDeValue<length(data_ordered[,1])) {
      endDeValue<-endDeValue+1
    } else {break}

  }#EndRepeat

  ##=================================================================================================##
  ##ADDITIONAL CALCULATIONS and TERMINAL OUTPUT
  ##=================================================================================================##

  # additional calculate weighted mean
  w <- 1 / (data_ordered[startDeValue:endDeValue, 2]) ^ 2 #weights for weighted mean
  weighted_mean <- round(stats::weighted.mean(data_ordered[startDeValue:endDeValue, 1], w),
                         digits = 2)
  weighted_sd <- round(sqrt(1 / sum(w)), digits = 2)
  n.usedDeValues <- endDeValue - startDeValue + 1

  # standard error
  se <- round(sd / sqrt(endDeValue - startDeValue + 1), digits = 2)

  if(verbose){
    cat("\n[calc_FuchsLang2001]")
    cat("\n\n----------- meta data --------------")
    cat("\n cvThreshold:            ", cvThreshold[1], "%")
    cat("\n used values:            ", n.usedDeValues)
    cat("\n----------- dose estimate ----------")
    cat("\n mean:                   ", mean)
    cat("\n sd:                     ", sd)
    cat("\n weighted mean:          ", weighted_mean)
    cat("\n weighted sd:            ", weighted_sd)
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
    n.usedDeValues = n.usedDeValues
  )

  args <- list(cvThreshold = cvThreshold, startDeValue = startDeValue)
  newRLumResults.calc_FuchsLang2001 <- set_RLum(
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
    try(plot_RLum.Results(newRLumResults.calc_FuchsLang2001, ...))
  }#endif::plot

  invisible(newRLumResults.calc_FuchsLang2001)
}

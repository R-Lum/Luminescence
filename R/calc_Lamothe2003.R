#' Apply fading correction after Lamothe et al., 2003
#'
#' This function applies the fading correction for the prediction of long-term fading as suggested
#' by Lamothe et atl., 2003. The function basically adjusts the Ln/Tn values and fit a new dose-response
#' curve using the function [plot_GrowthCurve].
#'
#'
#' @param object [RLum.Results-class] [data.frame] (**required**): Input data for applying the
#' fading correction. Alow are (1) [data.frame] with three columns (dose, De, De error), (2)
#' [RLum.Results-class] object created by the function [analyse_SAR.CWOSL] or [analyse_pIRIRSequence]
#'
#' @param dose_rate.envir [numeric] vector of length 2 (**required**): Environmental dose rate in mGy/a
#'
#' @param dose_rate.source [numeric] vector of length 2 (**required**): Irradiation source dose rate in Gy/s
#'
#' @param g_value [numeric] vector of length 2 (**required**): g_value in \%/decade recalculated at the moment
#' the De was calculated, i.e. tc is either similar for the g-value measurement and the De measurement or
#' needs be recalculated (cf. [calc_FadingCorr])
#'
#' @param plot [logical] (with default): Enables/disables plot output
#'
#' @param verbose [logical] (with default): Enables/disables terminal verbose mode
#'
#' @param ... further arguments passed to the function [plot_GrowthCurve]
#'
#' @return The function returns are graphical output produced by the function [plot_GrowthCurve] and
#' an [RLum.Results-class].
#'
#' -----------------------------------\cr
#' `[ NUMERICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#'
#' **`RLum.Results`**-object
#'
#' **slot:** **`@data`**
#'
#' \tabular{lll}{
#'  **Element** \tab **Type** \tab **Description**\cr
#'  `$data` \tab `data.frame` \tab the fading corrected values \cr
#'  `$fit` \tab `nls` \tab the object returned by the dose response curve fitting \cr
#' }
#'
#' '**slot:** **`@info`**
#'
#' The original function call
#'
#' @references
#'
#' Lamothe, M., Auclair, M., Hamzaoui, C., Huot, S., 2003.
#' Towards a prediction of long-term anomalous fadingof feldspar IRSL. Radiation Measurements 37,
#' 493-498.
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France), Norbert Mercier,
#' IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
#'
#' @keywords datagen
#'
#' @example
#'
#'##load data
#'##ExampleData.BINfileData contains two BINfileData objects
#'##CWOSL.SAR.Data and TL.SAR.Data
#'data(ExampleData.BINfileData, envir = environment())
#'
#'##transform the values from the first position in a RLum.Analysis object
#'object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
#'
#'##perform SAR analysis and set rejection criteria
#'results <- analyse_SAR.CWOSL(
#' object = object,
#' signal.integral.min = 1,
#' signal.integral.max = 2,
#' background.integral.min = 900,
#' background.integral.max = 1000,
#' verbose = FALSE,
#' plot = FALSE,
#' onlyLxTxTable = TRUE
#' )
#'
#' ##run fading correction
#' resutls_corr <- calc_Lamothe2003(
#'   object = results,
#'   dose_rate.envir =  c(1.676 , 0.180),
#'   dose_rate.source = c(0.184, 0.003),
#'   g_value =  c(2.36, 0.6),
#'   plot = TRUE,
#'   fit.method = "EXP")
#'
#'@md
#'@export
calc_Lamothe2003 <- function(
  object,
  dose_rate.envir,
  dose_rate.source,
  g_value,
  verbose = TRUE,
  plot = TRUE,
  ...
){

  # Input parameter test ------------------------------------------------------------------------
  ##object
    if(missing(object)){
      stop("[calc_Lamothe2003()] Input for 'object' missing but required!", call. = FALSE)
    }

  ##dose_rate.envir
  if(missing(dose_rate.envir)){
    stop("[calc_Lamothe2003()] Input for 'dose_rate.envir' missing but required!", call. = FALSE)
  }

  ##dose_rate.source
  if(missing(dose_rate.source)){
    stop("[calc_Lamothe2003()] Input for 'dose_rate.source' missing but required!", call. = FALSE)
  }

  ##g_value
  if(missing(g_value)){
    stop("[calc_Lamothe2003()] Input for 'g_value' missing but required!", call. = FALSE)
  }

  ##check input type and length
  ##dose_rate.envir
  if(class(dose_rate.envir) != "numeric" || length(dose_rate.envir) < 2){
    stop("[calc_Lamothe2003()] Input for 'dose_rate.envir' is not of type 'numeric' and/or of length < 2!", call. = FALSE)

  }else{
    if(length(dose_rate.envir) > 2){
      warning("[calc_Lamothe2003()] 'dose_rate.envir' has length > 2. Take only the first two entries.",call. = FALSE, immediate. = TRUE)
      dose_rate.envir <- dose_rate.envir[1:2]
    }

  }

  ##dose_rate.source
  if(class(dose_rate.source) != "numeric" || length(dose_rate.source) < 2){
    stop("[calc_Lamothe2003()] Input for 'dose_rate.source' is not of type 'numeric' and/or of length < 2!", call. = FALSE)

  }else{
    if(length(dose_rate.source) > 2){
      warning("[calc_Lamothe2003()] 'dose_rate.source' has length > 2. Take only the first two entries.",call. = FALSE, immediate. = TRUE)
      dose_rate.source <- dose_rate.source[1:2]
    }
  }


  # Input assignment -----------------------------------------------------------------------------
  ## We allow input as data.frame() and RLum.Results objects ... the output from functions listed
  ## below .. if we allow a data.frame it should have at least Dose, Lx/Tx, Lx/Tx Error
  if(class(object) == "data.frame"){
    data <- object[,1:3]

    ##add signal information
    if(any(grepl(pattern = "Signal", x = colnames(object), fixed = TRUE))){
      SIGNAL <- object[[which(grepl(pattern = "Signal", colnames(object), fixed = TRUE))[1]]]

    }else{
      SIGNAL <- NA

    }

  }else if(class(object) == "RLum.Results"){
    if(object@originator == "analyse_SAR.CWOSL" || object@originator == "analyse_pIRIRSequence"){
      ##now we do crazy stuff, we make a self-call here since this file can contain a lot of information

        ##get number of datasets; we have to search for the word natural, everthing else is not safe enough
        full_table <- object@data$LnLxTnTx.table
        set_start <- which(grepl(full_table$Name, pattern = "Natural", fixed = TRUE))
        set_end <- c(set_start[-1] - 1, nrow(full_table))

        ##signal column if available
        if(object@originator == "analyse_pIRIRSequence"){
          object <- full_table[,c("Dose", "LxTx", "LxTx.Error", "Signal")]
        }else{
          object <- full_table[,c("Dose", "LxTx", "LxTx.Error")]

        }

        ##now run the function
        results <- lapply(1:length(set_start), function(x){
          calc_Lamothe2003(
            object = object[set_start[x]:set_end[x], ],
            dose_rate.envir = dose_rate.envir,
            dose_rate.source = dose_rate.source,
            g_value = g_value,
            verbose = verbose,
            plot = plot,
            ...
          )
        })

        ##merge output
        return(merge_RLum(results))
    }else{
      stop(paste0("[calc_Lamothe2003()] Input for 'object' created by function ",object@originator, "() not supported!"), call. = FALSE)

    }


  }else{
    stop("[calc_Lamothe2003()] Unsupported data type for 'object'!", call. = FALSE)

  }

  # Apply correction----------------------------------------------------------------------------

  # transform irradiation times to dose values
  data[[1]] <- data[[1]] * dose_rate.source[1]

  # fading correction
  rr <-  31.5576 * 10^9 * dose_rate.source[1] / (exp(1) * dose_rate.envir[1])
  s_rr <-  (sqrt ((100*dose_rate.source[2]/dose_rate.source[1])^2 + (100*dose_rate.envir[2]/dose_rate.envir[1])^2))  * rr / 100
  Fading_C <-  1 - (g_value[1])/100 * log10(rr)
  sFading_C <-  sqrt ((log10(rr) )^2 * ((g_value[2])/100)^2 + (g_value[1]/(100*rr))^2 * (s_rr)^2 )

  #apply to input data
  LnTn_BEFORE <- data[[2]][1]
  LnTn_BEFORE.ERROR <- data[[3]][1]
  data[[2]][1] <-  data[[2]][1] / Fading_C
  data[[3]][1] <-  (sqrt( (100*data[[3]][1]/data[[2]][1])^2  + ((1/Fading_C - 1)*100*sFading_C/Fading_C)^2 )) * data[[2]][1] / 100


  # Fitting ---------------------------------------------------------------------------------
  ##set arguments
  argument_list <- list(
    sample = data,
    verbose = FALSE,
    main = "Corrected Dose Response Curve",
    xlab = "Dose [Gy]",
    txtProgressBar = verbose,
    output.plotExtended = FALSE,
    output.plot = plot

  )

  ##filter doubled arguments
  argument_list <- modifyList(x = argument_list, val = list(...))

  ##run plot function
  fit_results <- do.call(what = plot_GrowthCurve,args = argument_list)


  # Age calculation -----------------------------------------------------------------------------
  Age <-  get_RLum(fit_results)[["De"]] / dose_rate.envir[1]
  s_Age <-  sqrt (  (100*get_RLum(fit_results)[["De.Error"]]/get_RLum(fit_results)[["De"]])^2 + (100*dose_rate.envir[2]/dose_rate.envir[1])^2   ) *Age/100



  # Terminal output -----------------------------------------------------------------------------
  if(verbose){
    cat("\n[calc_Lamothe2003()] \n\n")
    cat(" Fading_C:\t\t", Fading_C, " \u00b1 ", sFading_C,"\n")
    cat(" Corrected Ln/Tn:\t", data[[2]][1], " \u00b1 ", data[[3]][1],"\n")
    cat(" Corrected De:\t\t", get_RLum(fit_results)[["De"]], " \u00b1 ", get_RLum(fit_results)[["De.Error"]]," Gy \n")
    cat(" Corrected Age:\t\t", Age, " \u00b1 ", s_Age," ka \n")
    cat("--------------------------------------------------------\n")

  }

  # Compile output ------------------------------------------------------------------------------
  return(
    set_RLum(
      class = "RLum.Results",
      data = list(
        data = data.frame(
          FADING_C = Fading_C,
          FADING_C.ERROR = sFading_C,
          LnTn_BEFORE = LnTn_BEFORE,
          LnTn_BEFORE.ERROR = LnTn_BEFORE.ERROR,
          LnTn_AFTER = data[[2]][1],
          LnTn_AFTER.ERROR = data[[3]][1],
          DE = get_RLum(fit_results)[["De"]],
          DE = get_RLum(fit_results)[["De"]],
          DE.ERROR = get_RLum(fit_results)[["De.Error"]],
          AGE = Age,
          AGE.ERROR = s_Age,
          SIGNAL = SIGNAL
          ),
        fit = get_RLum(fit_results, data.object = "Fit")

      ),
      info = list(
        call = sys.call()
      )
    )

  )

}

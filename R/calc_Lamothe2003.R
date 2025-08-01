#' @title Apply fading correction after Lamothe et al., 2003
#'
#' @description
#' This function applies the fading correction for the prediction of long-term
#' fading as suggested by Lamothe et al., 2003. The function basically adjusts
#' the $L_n/T_n$ values and fits a new dose-response curve using function
#' [plot_GrowthCurve].
#'
#' @details
#'
#' **Format of `object` if `data.frame`**
#'
#' If `object` is a [data.frame], all input values must be of type [numeric].
#' Dose values are expected in seconds (s) not Gray (Gy). No `NA` values are
#' allowed and the value for the natural dose (first row) should be `0`.
#' Example for three dose points (column names are arbitrary):
#'
#' ```
#'  object <- data.frame(
#'  dose = c(0,25,50),
#'  LxTx = c(4.2, 2.5, 5.0),
#'  LxTx_error = c(0.2, 0.1, 0.2))
#'  ```
#'
#'  **Note on the g-value and `tc`**
#'
#' Users new to R and fading measurements are often confused about what to
#' enter for `tc` and why it may differ from `tc.g_value`. By convention
#' (Huntley & Lamothe 2001), the `tc` value is the time elapsed between the
#' end of the irradiation and the prompt measurement. Usually there is no
#' reason for having a `tc` value different for the equivalent dose measurement
#' and the *g*-value measurement, except if different equipment was used.
#' However, if, for instance, the *g*-value measurement sequence was analysed
#' with the *Analyst* (Duller 2015) and `Luminescence` is used to correct for
#' fading, there is a high chance that the value returned by the *Analyst*
#' comes normalised to 2-days, even if the `tc` values of the measurement were
#' identical. In such cases, the fading correction cannot be correct until the
#' `tc.g_value` is manually set to 2-days (`172800` s) because the function
#' will internally recalculate values to an identical `tc` value.
#'
#' @param object [RLum.Results-class] [data.frame] (**required**):
#' Input data for applying the fading correction, can be (1) a [data.frame]
#' with three columns (`dose`, `LxTx`, `LxTx error`; see details), or (2) an
#' [RLum.Results-class] object created by [analyse_SAR.CWOSL] or
#' [analyse_pIRIRSequence].
#'
#' @param dose_rate.envir [numeric] vector of length 2 (**required**):
#' Environmental dose rate in mGy/a.
#'
#' @param dose_rate.source [numeric] vector of length 2 (**required**):
#' Irradiation source dose rate in Gy/s, which is, according to Lamothe et al.
#' (2003) De/t.
#'
#' @param g_value [numeric] vector of length 2 (**required**): g_value in
#' %/decade *recalculated at the moment* the equivalent dose was calculated,
#' i.e. `tc` is either similar for the *g*-value measurement **and** the
#' De measurement or needs be to recalculated (cf. [calc_FadingCorr]).
#' Inserting a normalised g-value, e.g., normalised to 2-days , will
#' lead to wrong results.
#'
#' @param tc [numeric] (*optional*): time in seconds between the **end** of
#' the irradiation and the prompt measurement used in the equivalent dose
#' estimation (cf. Huntley & Lamothe 2001).
#' If set to `NULL`, it is assumed that `tc` is similar for the equivalent
#' dose estimation and the *g*-value estimation.
#'
#' @param tc.g_value [numeric] (*with default*):
#' time in seconds between irradiation and the prompt measurement estimating
#' the *g*-value. If the *g*-value was normalised to, e.g., 2 days, this time
#' in seconds (i.e., `172800`) should be entered here along with the time used
#' for the equivalent dose estimation. If nothing is provided the time is set
#' to `tc`, which is the usual case for *g*-values obtained using the SAR
#' method and *g*-values that had been not normalised to 2 days.
#' Note: If this value is not `NULL` the functions expects a [numeric] value for `tc`.
#'
#' @param plot [logical] (*with default*): enable/disable the plot output.
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#' @param ... further arguments passed to function [plot_GrowthCurve].
#'
#' @return
#' The function returns an [RLum.Results-class] object and the graphical
#' output produced by [plot_GrowthCurve].
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
#' Huntley, D.J., Lamothe, M., 2001. Ubiquity of anomalous fading in K-feldspars and the measurement
#' and correction for it in optical dating. Canadian Journal of Earth Sciences 38, 1093-1106.
#'
#' Duller, G.A.T., 2015. The Analyst software package for luminescence data: overview and recent improvements.
#' Ancient TL 33, 35–42.
#'
#' Lamothe, M., Auclair, M., Hamzaoui, C., Huot, S., 2003.
#' Towards a prediction of long-term anomalous fading of feldspar IRSL. Radiation Measurements 37,
#' 493-498.
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany), Norbert Mercier,
#' IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
#'
#' @keywords datagen
#'
#' @seealso [plot_GrowthCurve], [calc_FadingCorr], [analyse_SAR.CWOSL], [analyse_pIRIRSequence]
#'
#' @examples
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
#' results_corr <- calc_Lamothe2003(
#'   object = results,
#'   dose_rate.envir =  c(1.676 , 0.180),
#'   dose_rate.source = c(0.184, 0.003),
#'   g_value =  c(2.36, 0.6),
#'   plot = TRUE,
#'   fit.method = "EXP")
#'
#'
#'@export
calc_Lamothe2003 <- function(
  object,
  dose_rate.envir,
  dose_rate.source,
  g_value,
  tc = NULL,
  tc.g_value = tc,
  verbose = TRUE,
  plot = TRUE,
  ...
) {
  .set_function_name("calc_Lamothe2003")
  on.exit(.unset_function_name(), add = TRUE)

  ## Input parameter test ---------------------------------------------------

  .validate_class(object, c("data.frame", "RLum.Results"))

  .validate_length_2_vector <- function(vec) {
    name <- sprintf("'%s'", all.vars(match.call())[1])
    .validate_class(vec, "numeric", name = name)
    if (length(vec) < 2)
      .throw_error(name, " should contain 2 elements")
    if (length(vec) > 2) {
      .throw_warning(name, " has length > 2, taking only the first two entries")
      vec <- vec[1:2]
    }
    return(vec)
  }
  .validate_length_2_vector(dose_rate.envir)
  .validate_length_2_vector(dose_rate.source)
  .validate_length_2_vector(g_value)

  ##tc
  if(is.null(tc) && !is.null(tc.g_value))
    .throw_error("If you set 'tc.g_value' you have to provide a value for 'tc' too")


  # Input assignment -----------------------------------------------------------------------------
  ## We allow input as data.frame() and RLum.Results objects ... the output from functions listed
  ## below .. if we allow a data.frame it should have at least Dose, Lx/Tx, Lx/Tx Error
  if(inherits(object, "data.frame")){
    data <- object[,1:3]

    ##add signal information
    if(any(grepl(pattern = "Signal", x = colnames(object), fixed = TRUE))){
      SIGNAL <- object[[grep(pattern = "Signal", colnames(object), fixed = TRUE)[1]]]
    }else{
      SIGNAL <- NA
    }

  }else if(inherits(object, "RLum.Results")){
    if (!object@originator %in% c("analyse_SAR.CWOSL", "analyse_pIRIRSequence"))
      .throw_error("Input for 'object' created by function ",
                   object@originator, "() not supported")

    ## get number of datasets; we have to search for the word "Natural",
    ## everything else is not safe enough
    full_table <- object@data$LnLxTnTx.table
    set_start <- grep(full_table$Name, pattern = "Natural", fixed = TRUE)
    set_end <- c(set_start[-1] - 1, nrow(full_table))

    ## columns of interest
    cols <- c("Dose", "LxTx", "LxTx.Error",
              if (object@originator == "analyse_pIRIRSequence") "Signal")
    object <- full_table[, cols]

    ## we make a self-call here since this file can contain a lot of information
    results <- lapply(1:length(set_start), function(x){
          calc_Lamothe2003(
            object = object[set_start[x]:set_end[x], ],
            dose_rate.envir = dose_rate.envir,
            dose_rate.source = dose_rate.source,
            g_value = g_value,
            tc = tc,
            tc.g_value = tc.g_value,
            verbose = verbose,
            plot = plot,
            ...
          )
        })

    ## merge output
    return(merge_RLum(results))
  }

  # Apply correction----------------------------------------------------------------------------

  ##recalculate the g-value to the given tc ...
  ##re-calculation thanks to the help by Sébastien Huot, e-mail: 2016-07-19
  if(!is.null(tc)){
    k0 <- g_value / 100 / log(10)
    k1 <- k0 / (1 - k0 * log(tc[1]/tc.g_value[1]))
    g_value <-  100 * k1 * log(10)
  }

  # transform irradiation times to dose values
  data[[1]] <- data[[1]] * dose_rate.source[1]

  ## fading correction (including dose rate conversion from Gy/s to Gy/ka)
  ## and error calculation
  ## the formula in Lamothe et al. (2003) reads:
  ## I_faded = I_unfaded*(1-g*log((1/e)*DR_lab/DR_soil)))
  rr <-  31.5576e+09 * dose_rate.source[1] / (exp(1) * dose_rate.envir[1])
  s_rr <- sqrt((dose_rate.source[2]/dose_rate.source[1])^2 + (dose_rate.envir[2]/dose_rate.envir[1])^2) * rr
  Fading_C <- 1 - g_value[1] / 100 * log10(rr)
  sFading_C <- sqrt((log10(rr) * g_value[2]/100)^2 + (g_value[1]/(100 * rr) * s_rr)^2)

  # store original Lx/Tx in new object
  LnTn_BEFORE <- data[[2]][1]
  LnTn_BEFORE.ERROR <- data[[3]][1]

  # apply to input data
  data[[2]][1] <-  data[[2]][1] / Fading_C
  data[[3]][1] <-  sqrt((data[[3]][1]/data[[2]][1])^2 +
                        (sFading_C/Fading_C)^2) * data[[2]][1]


  # Fitting ---------------------------------------------------------------------------------
  ##set arguments
  argument_list <- modifyList(list(
    sample = data,
    verbose = FALSE,
    main = "Corrected Dose Response Curve",
    xlab = "Dose [Gy]",
    txtProgressBar = verbose,
    output.plotExtended = FALSE,
    output.plot = plot
  ), val = list(...))

  ##run plot function
  fit_results <- do.call(what = plot_GrowthCurve, args = argument_list)


  # Age calculation -----------------------------------------------------------------------------
  res <- get_RLum(fit_results)
  Age <- res[["De"]] / dose_rate.envir[1]
  s_Age <- sqrt((100 * res[["De.Error"]] / res[["De"]])^2 +
                (100 * dose_rate.envir[2] / dose_rate.envir[1])^2) * Age / 100

  # Terminal output -----------------------------------------------------------------------------
  if(verbose){
    cat("\n[calc_Lamothe2003()] \n\n")
    cat(" Used g_value:\t\t", round(g_value[1],3)," \u00b1 ",round(g_value[2],3),"%/decade \n")
    if(!is.null(tc)){
      cat(" tc for g_value:\t", tc.g_value, " s\n")
    }
    cat("\n")
    cat(" Fading_C:\t\t", round(Fading_C,3), " \u00b1 ", round(sFading_C,3),"\n")
    cat(" Corrected Ln/Tn:\t", round(data[[2]][1],3), " \u00b1 ", round(data[[3]][1],3),"\n")
    cat(" Corrected De:\t\t", round(res[["De"]], 2), " \u00b1 ", round(res[["De.Error"]], 2)," Gy \n")
    cat("--------------------------------------------------------\n")
    cat(" Corrected Age:\t\t", round(Age,2), " \u00b1 ", round(s_Age,2)," ka \n")
    cat("--------------------------------------------------------\n")
  }

  # Compile output ------------------------------------------------------------------------------
  return(
    set_RLum(
      class = "RLum.Results",
      data = list(
        data = data.frame(
          g_value = g_value[1],
          g_value.ERROR = g_value[2],
          tc = ifelse(is.null(tc), NA, tc),
          tc.g_value = ifelse(is.null(tc.g_value), NA, tc.g_value),
          FADING_C = Fading_C,
          FADING_C.ERROR = sFading_C,
          LnTn_BEFORE = LnTn_BEFORE,
          LnTn_BEFORE.ERROR = LnTn_BEFORE.ERROR,
          LnTn_AFTER = data[[2]][1],
          LnTn_AFTER.ERROR = data[[3]][1],
          DE = res[["De"]],
          DE.ERROR = res[["De.Error"]],
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

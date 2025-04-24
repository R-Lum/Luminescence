#' @title Bayesian models (baSAR) applied on luminescence data
#'
#' @description This function allows the application of Bayesian models on luminescence data, measured
#' with the single-aliquot regenerative-dose (SAR, Murray and Wintle, 2000) protocol. In particular,
#' it follows the idea proposed by Combès et al., 2015 of using an hierarchical model for estimating
#' a central equivalent dose from a set of luminescence measurements. This function is (I) the adoption
#' of this approach for the R environment and (II) an extension and a technical refinement of the
#' published code.
#'
#' @details
#' Internally the function consists of two parts: (I) The Bayesian core for the Bayesian calculations
#' and applying the hierarchical model and (II) a data pre-processing part. The Bayesian core can be run
#' independently, if the input data are sufficient (see below). The data pre-processing part was
#' implemented to simplify the analysis for the user as all needed data pre-processing is done
#' by the function, i.e. in theory it is enough to provide a BIN/BINX-file with the SAR measurement
#' data. For the Bayesian analysis for each aliquot the following information are needed from the SAR analysis.
#' `LxTx`, the `LxTx` error and the dose values for all regeneration points.
#'
#' **How is the systematic error contribution calculated?**
#'
#' Standard errors (so far) provided with the source dose rate are considered as systematic uncertainties
#' and added to final central dose by:
#'
#' \deqn{systematic.error = 1/n \sum SE(source.doserate)}
#'
#' \deqn{SE(central.dose.final) = \sqrt{SE(central.dose)^2 + systematic.error^2}}
#'
#' Please note that this approach is rather rough and can only be valid if the source dose rate
#' errors, in case different readers had been used, are similar. In cases where more than
#' one source dose rate is provided a warning is given.
#'
#' **Input / output scenarios**
#'
#' Various inputs are allowed for this function. Unfortunately this makes the function handling rather
#' complex, but at the same time very powerful. Available scenarios:
#'
#' **(1) - `object` is BIN-file or link to a BIN-file**
#'
#' Finally it does not matter how the information of the BIN/BINX file are provided. The function
#' supports **(a)** either a path to a file or directory or a `list` of file names or paths or
#' **(b)** a [Risoe.BINfileData-class] object or a list of these objects. The latter one can
#' be produced by using the function [read_BIN2R], but this function is called automatically
#' if only a file name and/or a path is provided. In both cases it will become the data that can be
#' used for the analysis.
#'
#' `[CSV_file = NULL]`
#'
#' If no CSV file (or data frame with the same format) is provided, the
#' function runs an automatic process that consists of the following steps:
#'
#'  1. Select all valid aliquots using the function [verify_SingleGrainData]
#'  2. Calculate `Lx/Tx` values using the function [calc_OSLLxTxRatio]
#'  3. Calculate De values using the function [plot_GrowthCurve]
#'
#' These proceeded data are subsequently used in for the Bayesian analysis
#'
#' `[CSV_file != NULL]`
#'
#' If a CSV file is provided (or a `data.frame` containing similar information)
#' the pre-processing phase consists of the following steps:
#'
#'  1. Calculate `Lx/Tx` values using the function [calc_OSLLxTxRatio]
#'  2. Calculate De values using the function [plot_GrowthCurve]
#'
#' The CSV file should contain the BIN-file names and the aliquots selected
#' for the further analysis. This allows a manual selection of input data, as the automatic selection
#' by [verify_SingleGrainData] might be not totally sufficient.
#'
#'
#' **(2) - `object` `RLum.Results object`**
#'
#' If an [RLum.Results-class] object is provided as input and(!) this object was
#' previously created by the function `analyse_baSAR()` itself, the pre-processing part
#' is skipped and the function starts directly with the Bayesian analysis. This option is very powerful
#' as it allows to change parameters for the Bayesian analysis without the need to repeat
#' the data pre-processing. If furthermore the argument `aliquot_range` is set, aliquots
#' can be manually excluded based on previous runs.
#'
#' **`method_control`**
#'
#' These are arguments that can be passed directly to the Bayesian calculation core, supported arguments
#' are:
#'
#' \tabular{lll}{
#' **Parameter** \tab **Type** \tab **Description**\cr
#' `lower_centralD` \tab [numeric] \tab sets the lower bound for the expected De range. Change it only if you know what you are doing!\cr
#' `upper_centralD` \tab [numeric] \tab sets the upper bound for the expected De range. Change it only if you know what you are doing!\cr
#' `n.chains` \tab [integer] \tab sets number of parallel chains for the model (default = 3) (cf. [rjags::jags.model])\cr
#' `inits` \tab [list] \tab option to set initialisation values (cf. [rjags::jags.model]) \cr
#' `thin` \tab [numeric] \tab thinning interval for monitoring the Bayesian process (cf. [rjags::jags.model])\cr
#' `variable.names` \tab [character] \tab set the variables to be monitored during the MCMC run, default:
#' `'central_D'`, `'sigma_D'`, `'D'`, `'Q'`, `'a'`, `'b'`, `'c'`, `'g'`.
#' Note: only variables present in the model can be monitored.
#' }
#'
#' **User defined models**\cr
#'
#' The function provides the option to modify and to define own models that can be used for
#' the Bayesian calculation. In the case the user wants to modify a model, a new model
#' can be piped into the function via the argument `baSAR_model` as `character`.
#' The model has to be provided in the JAGS dialect of the BUGS language (cf. [rjags::jags.model])
#' and parameter names given with the pre-defined names have to be respected, otherwise the function
#' will break.
#'
#' **FAQ**
#'
#' Q: How can I set the seed for the random number generator (RNG)?
#'
#' A: Use the argument `method_control`, e.g., for three MCMC chains
#' (as it is the default):
#'
#' ```
#' method_control = list(
#' inits = list(
#'  list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1),
#'  list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 2),
#'  list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 3)
#' ))
#' ```
#'
#' This sets a reproducible set for every chain separately.\cr
#'
#' Q: How can I modify the output plots?
#'
#' A: You can't, but you can use the function output to create own, modified plots.
#'
#'
#' Q: Can I change the boundaries for the central_D?
#'
#' A: Yes, we made it possible, but we DO NOT recommend it, except you know what you are doing!\cr
#' Example: `method_control = list(lower_centralD = 10))`
#'
#' Q: The lines in the baSAR-model appear to be in a wrong logical order?\cr
#'
#' A: This is correct and allowed (cf. JAGS manual)
#'
#'
#' **Additional arguments support via the `...` argument**
#'
#' This list summarizes the additional arguments that can be passed to the internally used
#' functions.
#'
#' \tabular{llll}{
#' **Supported argument** \tab **Corresponding function** \tab **Default** \tab **Short description **\cr
#' `threshold` \tab [verify_SingleGrainData] \tab `30` \tab change rejection threshold for curve selection \cr
#' `skip` \tab [data.table::fread] \tab `0` \tab number of rows to be skipped during import\cr
#' `n.records` \tab [read_BIN2R] \tab `NULL` \tab limit records during BIN-file import\cr
#' `duplicated.rm` \tab [read_BIN2R] \tab `TRUE` \tab remove duplicated records in the BIN-file\cr
#' `pattern` \tab [read_BIN2R] \tab `TRUE` \tab select BIN-file by name pattern\cr
#' `position` \tab [read_BIN2R] \tab `NULL` \tab limit import to a specific position\cr
#' `background.count.distribution` \tab [calc_OSLLxTxRatio] \tab `"non-poisson"` \tab set assumed count distribution\cr
#' `fit.weights` \tab [plot_GrowthCurve] \tab `TRUE` \tab enable/disable fit weights\cr
#' `fit.bounds` \tab [plot_GrowthCurve] \tab `TRUE` \tab enable/disable fit bounds\cr
#' `n.MC` \tab [plot_GrowthCurve] \tab `100` \tab number of MC runs for error calculation\cr
#' `output.plot` \tab [plot_GrowthCurve] \tab `TRUE` \tab enable/disable dose response curve plot\cr
#' `output.plotExtended` \tab [plot_GrowthCurve] \tab `TRUE` \tab enable/disable extended dose response curve plot\cr
#' `recordType` \tab [get_RLum] \tab `c(OSL (UVVIS), irradiation (NA)` \tab helps for the curve selection\cr
#' }
#'
#'
#' @param object [Risoe.BINfileData-class], [RLum.Results-class], [list] of [RLum.Analysis-class],
#' [character] or [list] (**required**):
#' input object used for the Bayesian analysis. If a `character` is provided the function
#' assumes a file connection and tries to import a BIN/BINX-file using the provided path. If a `list` is
#' provided the list can only contain either `Risoe.BINfileData` objects or `character`s
#' providing a file connection. Mixing of both types is not allowed. If an [RLum.Results-class]
#' is provided the function directly starts with the Bayesian Analysis (see details)
#'
#' @param CSV_file [character] or [data.frame] (*optional*):
#' if a `character`, it must be the path to a CSV file with data for the
#' analysis. Either way, data should contain 3 columns:
#' the name of the file, the disc position and the grain position
#' (the last being 0 for multi-grain measurements).\cr
#'
#' @param aliquot_range [numeric] (*optional*):
#' allows to limit the range of the aliquots used for the analysis.
#' This argument has only an effect if the argument `CSV_file` is used or
#' the input is the previous output (i.e. is [RLum.Results-class]). In this case the
#' new selection will add the aliquots to the removed aliquots table.
#'
#' @param source_doserate [numeric] (**required**):
#' source dose rate of beta-source used for the measurement and its uncertainty
#' in Gy/s, e.g., `source_doserate = c(0.12, 0.04)`. Parameter can be provided
#' as `list`, for the case that more than one BIN-file is provided, e.g.,
#' `source_doserate = list(c(0.04, 0.004), c(0.05, 0.004))`.
#'
#' @param signal.integral [vector] (**required**):
#' vector with the limits for the signal integral used for the calculation,
#' e.g., `signal.integral = c(1:5)`. Ignored if `object` is an [RLum.Results-class] object.
#' The parameter can be provided as `list`, see `source_doserate`.
#'
#' @param signal.integral.Tx [vector] (*optional*):
#' vector with the limits for the signal integral for the Tx curve. I
#' f nothing is provided the value from `signal.integral` is used and it is ignored
#' if `object` is an [RLum.Results-class] object.
#' The parameter can be provided as `list`, see `source_doserate`.
#'
#' @param background.integral [vector] (**required**):
#' vector with the bounds for the background integral.
#' Ignored if `object` is an [RLum.Results-class] object.
#' The parameter can be provided as `list`, see `source_doserate`.
#'
#' @param background.integral.Tx [vector] (*optional*):
#' vector with the limits for the background integral for the Tx curve.
#' If nothing is provided the value from `background.integral` is used.
#' Ignored if `object` is an [RLum.Results-class] object.
#' The parameter can be provided as `list`, see `source_doserate`.
#'
#' @param irradiation_times [numeric] (*optional*): if set this vector replaces all irradiation
#' times for one aliquot and one cycle (Lx and Tx curves) and recycles it for all others cycles and aliquots.
#' Please note that if this argument is used, for every(!) single curve
#' in the dataset an irradiation time needs to be set.
#'
#' @param sigmab [numeric] (*with default*):
#' option to set a manual value for the overdispersion (for `LnTx` and `TnTx`),
#' used for the `Lx`/`Tx` error calculation. The value should be provided as
#' absolute squared count values, cf. [calc_OSLLxTxRatio].
#' The parameter can be provided as `list`, see `source_doserate`.
#'
#' @param sig0 [numeric] (*with default*):
#' allow adding an extra component of error to the final Lx/Tx error value
#' (e.g., instrumental error, see details is [calc_OSLLxTxRatio]).
#' The parameter can be provided as `list`, see `source_doserate`.
#'
#' @param distribution [character] (*with default*):
#' type of distribution that is used during Bayesian calculations for
#' determining the Central dose and overdispersion values.
#' Allowed inputs are `"cauchy"`, `"normal"` and `"log_normal"`.
#'
#' @param baSAR_model [character] (*optional*):
#' option to provide an own modified or new model for the Bayesian calculation
#' (see details). If an own model is provided the argument `distribution` is
#' ignored and set to `'user_defined'`
#'
#' @param n.MCMC [integer] (*with default*):
#' number of iterations for the Markov chain Monte Carlo (MCMC) simulations
#'
#' @param fit.method [character] (*with default*):
#' equation used for the fitting of the dose-response curve using the function
#' [plot_GrowthCurve] and then for the Bayesian modelling. Here supported methods: `EXP`, `EXP+LIN` and `LIN`
#'
#' @param fit.force_through_origin [logical] (*with default*):
#' force fitting through origin
#'
#' @param fit.includingRepeatedRegPoints [logical] (*with default*):
#' includes the recycling point (assumed to be measured during the last cycle)
#'
#' @param method_control [list] (*optional*):
#' named list of control parameters that can be directly
#' passed to the Bayesian analysis, e.g., `method_control = list(n.chains = 4)`.
#' See details for further information
#'
#' @param digits [integer] (*with default*):
#' round output to the number of given digits
#'
#' @param distribution_plot [character] (*with default*): sets the final distribution plot that
#' shows equivalent doses obtained using the frequentist approach and sets in the central dose
#' as comparison obtained using baSAR. Allowed input is `'abanico'` or `'kde'`. If set to `NULL` nothing is plotted.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param plot_reduced [logical] (*with default*):
#' enable/disable the advanced plot output.
#'
#' @param plot_singlePanels [logical] (*with default*):
#' enable/disable single plots or plots arranged by `analyse_baSAR`.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param ... parameters that can be passed to the function [calc_OSLLxTxRatio]
#' (almost full support), [data.table::fread] (`skip`), [read_BIN2R] (`n.records`,
#' `position`, `duplicated.rm`), see details.
#'
#'
#' @return Function returns results numerically and graphically:
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
#'  `$summary` \tab `data.frame` \tab statistical summary, including the central dose \cr
#'  `$mcmc` \tab `mcmc` \tab [coda::mcmc.list] object including raw output \cr
#'  `$models` \tab `character` \tab implemented models used in the baSAR-model core \cr
#'  `$input_object` \tab `data.frame` \tab summarising table (same format as the XLS-file) including, e.g., Lx/Tx values\cr
#'  `$removed_aliquots` \tab `data.frame` \tab table with removed aliquots (e.g., `NaN`, or `Inf` `Lx`/`Tx` values). If nothing was removed `NULL` is returned
#' }
#'
#'**slot:** **`@info`**
#'
#' The original function call
#'
#' ------------------------\cr
#' `[ PLOT OUTPUT ]`\cr
#' ------------------------\cr
#'
#'  - (A) Ln/Tn curves with set integration limits,
#'  - (B) trace plots are returned by the baSAR-model, showing the convergence of the parameters (trace)
#'  and the resulting kernel density plots. If `plot_reduced = FALSE` for every(!) dose a trace and
#'  a density plot is returned (this may take a long time),
#'  - (C) dose plots showing the dose for every aliquot as boxplots and the marked
#'  HPD in within. If boxes are coloured 'orange' or 'red' the aliquot itself should be checked,
#'  - (D) the dose response curve resulting from the monitoring of the Bayesian modelling are
#'  provided along with the Lx/Tx values and the HPD. Note: The amount for curves displayed
#'  is limited to 1000 (random choice) for performance reasons,
#'  - (E) the final plot is the De distribution as calculated using the conventional (frequentist) approach
#'  and the central dose with the HPDs marked within. This figure is only provided for a comparison,
#'  no further statistical conclusion should be drawn from it.
#'
#'
#' **Please note: If distribution was set to `log_normal` the central dose is given as geometric mean!**
#'
#'
#' @section Function version: 0.1.36
#'
#' @author
#' Norbert Mercier, Archaésciences Bordeaux, CNRS-Université Bordeaux Montaigne (France) \cr
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany) \cr
#' The underlying Bayesian model based on a contribution by Combès et al., 2015.
#'
#' @seealso [read_BIN2R], [calc_OSLLxTxRatio], [plot_GrowthCurve],
#' [data.table::fread], [verify_SingleGrainData],
#' [rjags::jags.model], [rjags::coda.samples], [boxplot.default]
#'
#'
#' @references
#'
#' Combès, B., Philippe, A., Lanos, P., Mercier, N., Tribolo, C., Guerin, G., Guibert, P., Lahaye, C., 2015.
#' A Bayesian central equivalent dose model for optically stimulated luminescence dating.
#' Quaternary Geochronology 28, 62-70. doi:10.1016/j.quageo.2015.04.001
#'
#' Mercier, N., Kreutzer, S., Christophe, C., Guerin, G., Guibert, P., Lahaye, C., Lanos, P., Philippe, A.,
#' Tribolo, C., 2016. Bayesian statistics in luminescence dating: The 'baSAR'-model and its implementation
#' in the R package 'Luminescence'. Ancient TL 34, 14-21.
#'
#' **Further reading**
#'
#' Gelman, A., Carlin, J.B., Stern, H.S., Dunson, D.B., Vehtari, A., Rubin, D.B., 2013.
#' Bayesian Data Analysis, Third Edition. CRC Press.
#'
#' Murray, A.S., Wintle, A.G., 2000. Luminescence dating of quartz using an improved single-aliquot
#' regenerative-dose protocol. Radiation Measurements 32, 57-73. doi:10.1016/S1350-4487(99)00253-X
#'
#' Plummer, M., 2017. JAGS Version 4.3.0 user manual. `https://sourceforge.net/projects/mcmc-jags/files/Manuals/4.x/jags_user_manual.pdf/download`
#'
#' @note
#' **If you provide more than one BIN-file**, it is **strongly** recommended to provide
#' a `list` with the same number of elements for the following parameters:
#'
#' `source_doserate`, `signal.integral`, `signal.integral.Tx`, `background.integral`,
#' `background.integral.Tx`, `sigmab`, `sig0`.
#'
#' Example for two BIN-files: `source_doserate = list(c(0.04, 0.006), c(0.05, 0.006))`
#'
#' **The function is currently limited to work with standard Risoe BIN-files only!**
#'
#' @keywords datagen
#'
#' @examples
#'
#' ##(1) load package test data set
#' data(ExampleData.BINfileData, envir = environment())
#'
#' ##(2) selecting relevant curves, and limit dataset
#' CWOSL.SAR.Data <- subset(
#'   CWOSL.SAR.Data,
#'   subset = POSITION%in%c(1:3) & LTYPE == "OSL")
#'
#' \dontrun{
#' ##(3) run analysis
#' ##please not that the here selected parameters are
#' ##choosen for performance, not for reliability
#' results <- analyse_baSAR(
#'   object = CWOSL.SAR.Data,
#'   source_doserate = c(0.04, 0.001),
#'   signal.integral = c(1:2),
#'   background.integral = c(80:100),
#'   fit.method = "LIN",
#'   plot = FALSE,
#'   n.MCMC = 200
#'
#' )
#'
#' print(results)
#'
#'
#' ##CSV_file template
#' ##copy and paste this the code below in the terminal
#' ##you can further use the function write.csv() to export the example
#'
#' CSV_file <-
#' structure(
#' list(
#'  BIN_FILE = NA_character_,
#'  DISC = NA_real_,
#'  GRAIN = NA_real_),
#'    .Names = c("BIN_FILE", "DISC", "GRAIN"),
#'    class = "data.frame",
#'    row.names = 1L
#' )
#'
#' }
#'
#' @md
#' @export
analyse_baSAR <- function(
  object,
  CSV_file = NULL,
  aliquot_range = NULL,
  source_doserate = NULL,
  signal.integral,
  signal.integral.Tx = NULL,
  background.integral,
  background.integral.Tx = NULL,
  irradiation_times = NULL,
  sigmab = 0,
  sig0 = 0.025,
  distribution = "cauchy",
  baSAR_model = NULL,
  n.MCMC = 100000,
  fit.method = "EXP",
  fit.force_through_origin = TRUE,
  fit.includingRepeatedRegPoints = TRUE,
  method_control = list(),
  digits = 3L,
  distribution_plot = "kde",
  plot = TRUE,
  plot_reduced = TRUE,
  plot_singlePanels = FALSE,
  verbose = TRUE,
  ...
) {
  .set_function_name("analyse_baSAR")
  on.exit(.unset_function_name(), add = TRUE)

  ##////////////////////////////////////////////////////////////////////////////////////////////////
  ##FUNCTION TO BE CALLED to RUN the Bayesian Model
  ##////////////////////////////////////////////////////////////////////////////////////////////////
  ##START
  .baSAR_function <-
    function(Nb_aliquots,
             distribution,
             data.Dose,
             data.Lum,
             data.sLum,
             fit.method,
             n.MCMC,
             fit.force_through_origin,
             fit.includingRepeatedRegPoints,
             method_control,
             baSAR_model,
             verbose)
    {

      ##lower and uppder De, grep from method_control ... for sure we find it here,
      ##as it was set before the function call
      lower_centralD <- method_control[["lower_centralD"]]
      upper_centralD <- method_control[["upper_centralD"]]

      ##number of MCMC
      n.chains <-  if (is.null(method_control[["n.chains"]])) {
        3
      } else{
        method_control[["n.chains"]]
      }

      ##inits
      inits <-  if (is.null(method_control[["inits"]])) {
        NULL
      } else{
        method_control[["inits"]]
      }

      ##thin
      thin <-  if (is.null(method_control[["thin"]])) {
        if(n.MCMC >= 1e+05){
          thin <- n.MCMC/1e+05 * 250

        }else{
          thin <- min(10, n.MCMC / 2)
        }
      } else{
        .validate_positive_scalar(method_control[["thin"]], int = TRUE,
                                  name = "'thin' in 'method_control'")
        method_control[["thin"]]
      }

      ## jags reports ugly errors if thin exceeds n.MCMC / 2, as that
      ## would correspond to producing just one posterior sample, see #407
      if (!is.null(method_control[["thin"]]) && thin > n.MCMC / 2) {
        thin <- n.MCMC / 2
        .throw_warning("'thin = ", method_control[["thin"]],
                       "' is too high for 'n.MCMC = ", n.MCMC,
                       "', reset to ", thin)
      }

      #check whether this makes sense at all, just a direty and quick test
      stopifnot(lower_centralD >= 0)

      Limited_cycles <- vector()

      if (fit.method == "EXP") {ExpoGC <- 1 ; LinGC <-  0 }
      if (fit.method == "LIN") {ExpoGC <- 0 ; LinGC <-  1 }
      if (fit.method == "EXP+LIN") {ExpoGC <- 1 ; LinGC <-  1 }
      if (fit.force_through_origin == TRUE) {GC_Origin <- 1} else {GC_Origin <- 0}

      ##Include or exclude repeated dose points
      if (fit.includingRepeatedRegPoints) {
        for (i in 1:Nb_aliquots) {
          Limited_cycles[i] <- length(stats::na.exclude(data.Dose[,i]))
        }

      }else{

        for (i in 1:Nb_aliquots) {

          temp.logic <- !duplicated(data.Dose[,i], incomparables=c(0))  # logical excluding 0
          m <- length(which(!temp.logic))

          data.Dose[,i] <-  c(data.Dose[,i][temp.logic], rep(NA, m))
          data.Lum[,i] <-  c(data.Lum[,i][temp.logic], rep(NA, m))
          data.sLum[,i]  <-  c(data.sLum[,i][temp.logic], rep(NA, m))

          rm(m, temp.logic)
        }

        for (i in 1:Nb_aliquots) {
          Limited_cycles[i] <- length(data.Dose[, i]) - length(which(is.na(data.Dose[, i])))
        }
      }

      ##check and correct for distribution name
      if (!is.null(baSAR_model) && distribution != "user_defined") {
        distribution <- "user_defined"
        message("[analyse_basAR()] 'baSAR_model' provided, setting ",
                "distribution to 'user_defined'")
      }

      # Bayesian Models ----------------------------------------------------------------------------
      # INFO: >
      #  > sometimes lines apear to be in a wrong logical order, however, this is allowed in the
      #  > model definition since:
      #  > "The data block is not limited to logical relations, but may also include stochastic relations."
      #  > (Plummer, 2017. JAGS Version 4.3.0 user manual, p. 9)
      baSAR_models <- list(
        cauchy = "model {

            central_D ~  dunif(lower_centralD,upper_centralD)

            precision_D ~ dt(0, pow(0.16*central_D, -2), 1)T(0, )
            sigma_D <-  1/sqrt(precision_D)

            for (i in 1:Nb_aliquots) {
              a[i] ~  dnorm(6.5 , 1/(9.2^2) ) T(0, )
              b[i] ~  dnorm(50 , 1/(1000^2) )  T(0, )
              c[i] ~  dnorm(1.002 , 1/(0.9^2) ) T(0, )
              g[i] ~  dnorm(0.5 , 1/(2.5^2) ) I(-a[i], )
              sigma_f[i]  ~  dexp (20)

              D[i] ~ dt ( central_D , precision_D, 1)    #  Cauchy distribution

              S_y[1,i] <-  1/(sLum[1,i]^2 + sigma_f[i]^2)
              Lum[1,i] ~ dnorm ( Q[1,i] , S_y[1,i])
              Q[1,i]  <-  GC_Origin * g[i] + LinGC * (c[i] * D[i] ) + ExpoGC * (a[i] * (1 - exp (-D[i] /b[i])) )

              for (m in 2:Limited_cycles[i]) {
                S_y[m,i] <-  1/(sLum[m,i]^2 + sigma_f[i]^2)
                Lum[m,i] ~ dnorm( Q[m,i] , S_y[m,i] )
                Q[m,i]  <-  GC_Origin * g[i] + LinGC * (c[i] * Dose[m,i]) + ExpoGC * (a[i] * (1 - exp (-Dose[m,i]/b[i])) )
              }
            }
          }",

       normal = "model {
            central_D ~  dunif(lower_centralD,upper_centralD)

            sigma_D ~ dunif(0.01, 1 * central_D)

            for (i in 1:Nb_aliquots) {
              a[i] ~  dnorm(6.5 , 1/(9.2^2) ) T(0, )
              b[i] ~  dnorm(50 , 1/(1000^2) )  T(0, )
              c[i] ~  dnorm(1.002 , 1/(0.9^2) ) T(0, )
              g[i] ~  dnorm(0.5 , 1/(2.5^2) ) I(-a[i], )
              sigma_f[i]  ~  dexp (20)

              D[i] ~ dnorm ( central_D , 1/(sigma_D^2) )   #   Normal distribution

              S_y[1,i] <-  1/(sLum[1,i]^2 + sigma_f[i]^2)
              Lum[1,i] ~ dnorm ( Q[1,i] , S_y[1,i])
              Q[1,i]  <-  GC_Origin * g[i] + LinGC * (c[i] * D[i] ) + ExpoGC * (a[i] * (1 - exp (-D[i] /b[i])) )


              for (m in 2:Limited_cycles[i]) {
                S_y[m,i] <-  1/(sLum[m,i]^2 + sigma_f[i]^2)
                Lum[m,i] ~ dnorm( Q[m,i] , S_y[m,i] )
                Q[m,i]  <-  GC_Origin * g[i] + LinGC * (c[i] * Dose[m,i]) + ExpoGC * (a[i] * (1 - exp (-Dose[m,i]/b[i])) )
              }
            }
            }",

       log_normal = "model {
            central_D ~  dunif(lower_centralD,upper_centralD)

            log_central_D <-  log(central_D) - 0.5 * l_sigma_D^2
            l_sigma_D ~ dunif(0.01, 1 * log(central_D))
            sigma_D <-  sqrt((exp(l_sigma_D^2) -1) * exp( 2*log_central_D + l_sigma_D^2) )

            for (i in 1:Nb_aliquots) {
              a[i] ~  dnorm(6.5 , 1/(9.2^2) ) T(0, )
              b[i] ~  dnorm(50 , 1/(1000^2) )  T(0, )
              c[i] ~  dnorm(1.002 , 1/(0.9^2) ) T(0, )
              g[i] ~  dnorm(0.5 , 1/(2.5^2) ) I(-a[i], )
              sigma_f[i]  ~  dexp (20)

              log_D[i] ~ dnorm ( log_central_D , 1/(l_sigma_D^2) )  #   Log-Normal distribution
              D[i] <-  exp(log_D[i])

              S_y[1,i] <-  1/(sLum[1,i]^2 + sigma_f[i]^2)
              Lum[1,i] ~ dnorm ( Q[1,i] , S_y[1,i])
              Q[1,i]  <-  GC_Origin * g[i] + LinGC * (c[i] * D[i] ) + ExpoGC * (a[i] * (1 - exp (-D[i] /b[i])) )

            for (m in 2:Limited_cycles[i]) {
                S_y[m,i] <-  1/(sLum[m,i]^2 + sigma_f[i]^2)
                Lum[m,i] ~ dnorm( Q[m,i] , S_y[m,i] )
                Q[m,i]  <-  GC_Origin * g[i] + LinGC * (c[i] * Dose[m,i]) + ExpoGC * (a[i] * (1 - exp (-Dose[m,i]/b[i])) )
              }
            }
        }",

        user_defined = baSAR_model
       )

      ##check whether the input for distribution was sufficient
      if (!distribution %in% names(baSAR_models)) {
        .throw_error("No pre-defined model for the requested distribution. ",
                     "Please select one of ",
                     .collapse(rev(names(baSAR_models))[-1]),
                     ", or define an own model using argument 'baSAR_model'")
      }

      if (distribution == "user_defined" && is.null(baSAR_model)) {
        .throw_error("You specified a 'user_defined' distribution, ",
                     "but did not provide a model via 'baSAR_model'")
      }

      ### Bayesian inputs
      data_Liste  <- list(
        'Dose' = data.Dose,
        'Lum' = data.Lum,
        'sLum' = data.sLum,
        'LinGC' = LinGC,
        'ExpoGC' = ExpoGC,
        'GC_Origin' = GC_Origin,
        'Limited_cycles' = Limited_cycles,
        'lower_centralD' = lower_centralD,
        'upper_centralD' = upper_centralD,
        'Nb_aliquots' = Nb_aliquots
      )

      if(verbose){
        cat("\n[analyse_baSAR()] ---- baSAR-model ---- \n")
        cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
        cat("[analyse_baSAR()] Bayesian analysis in progress ...\n")
        message(".. >> bounds set to: lower_centralD = ", lower_centralD,
                " | upper_centralD = ", upper_centralD)
      }

      Nb_Iterations <- n.MCMC

      if (verbose) {
        message(
          ".. >> calculation will be done assuming a '",
          distribution,
          "' distribution\n"
        )
      }

      ##set model
      jagsfit <- rjags::jags.model(
          file = textConnection(baSAR_models[[distribution]]),
          data = data_Liste,
          inits = inits,
          n.chains = n.chains,
          n.adapt = Nb_Iterations,
          quiet = !verbose
       )

      ##update jags model (it is a S3-method)
      stats::update(
        object = jagsfit,
        n.iter = Nb_Iterations,
        progress.bar = if(verbose){"text"}else{NULL}
        )

      ##get data ... full and reduced, the reduced one to limit the plot output
      sampling <- rjags::coda.samples(
        model = jagsfit,
        variable.names = variable.names,
        n.iter = Nb_Iterations,
        thin = thin
      )

      ##this we need for the output of the terminal
      ##Why sampling reduced? Because the summary() method produces a considerable overhead while
      ##running over all the variables
      sampling_reduced <- rjags::coda.samples(
        model = jagsfit,
        variable.names = c('central_D', 'sigma_D'),
        n.iter = Nb_Iterations,
        thin = thin
      )

      pt_zero <- 0
      nb_decal <-  2
      pt_zero <- Nb_aliquots

      ##standard error and mean
      output.mean <-
        round(summary(sampling_reduced)[[1]][c("central_D", "sigma_D"), 1:2], digits)

        ##calculate geometric mean for the case that the distribution is log-normal
        if(distribution == "log_normal"){
          temp.vector <- unlist(lapply(sampling_reduced, function(x){as.vector(x[,1])}))
          gm <- round(exp(sum(log(temp.vector))/length(temp.vector)),digits)
          rm(temp.vector)
        }else{
          gm <- NULL
        }

      ##quantiles
      ##68% + 95%
      output.quantiles <-
        round(summary(sampling_reduced, quantiles = c(0.025, 0.16, 0.84, 0.975))[[2]][c("central_D", "sigma_D"), 1:4], digits)

      #### output data.frame with results
      baSAR.output <- data.frame(
        DISTRIBUTION = distribution,
        NB_ALIQUOTS = Nb_aliquots,
        N.CHAINS = n.chains,
        N.MCMC = n.MCMC,
        FIT_METHOD = fit.method,
        CENTRAL = if(is.null(gm)){output.mean[1,1]}else{gm},
        CENTRAL.SD = output.mean[1,2],
        SIGMA = output.mean[2,1],
        SIGMA.SD = output.mean[2,2],
        CENTRAL_Q_.16 = output.quantiles[1,2],
        CENTRAL_Q_.84 = output.quantiles[1,3],
        SIGMA_Q_.16 = output.quantiles[2,2],
        SIGMA_Q_.84 = output.quantiles[2,3],
        CENTRAL_Q_.025 = output.quantiles[1,1],
        CENTRAL_Q_.975 = output.quantiles[1,4],
        SIGMA_Q_.025 = output.quantiles[2,1],
        SIGMA_Q_.975 = output.quantiles[2,4]
      )

      return(
        baSAR.output = list(
          baSAR.output_summary = baSAR.output,
          baSAR.output_mcmc = sampling,
          models = list(
            cauchy = baSAR_models[["cauchy"]],
            normal = baSAR_models[["normal"]],
            log_normal = baSAR_models[["log_normal"]],
            user_defined = baSAR_models[["user_defined"]]
          )
        )
      )
    }
  ##END
  ##////////////////////////////////////////////////////////////////////////////////////////////////


  ## Integrity checks -------------------------------------------------------

  .require_suggested_package("rjags")
  .require_suggested_package("coda")
  .validate_class(object, c("Risoe.BINfileData", "RLum.Results", "character", "list"))
  .validate_not_empty(object)
  .validate_positive_scalar(n.MCMC, int = TRUE)
  fit.method <- .validate_args(fit.method, c("EXP", "EXP+LIN", "LIN"))
  distribution_plot <- .validate_args(distribution_plot, c("kde", "abanico"),
                                      null.ok = TRUE)
  if (is.null(distribution_plot))
    distribution_plot <- ""

  #capture additional piped arguments
  additional_arguments <- list(

    ##verify_SingleGrainData
    threshold = 30,

    ##calc_OSLLxTxRatio()
    background.count.distribution = "non-poisson",

    ## data.table::fread()
    skip = 0,

    ##read_BIN2R()
    n.records = NULL,
    duplicated.rm = TRUE,
    position = NULL,
    pattern = NULL,

    ## plot_GrowthCurve()
    fit.weights = TRUE,
    fit.bounds = TRUE,
    n.MC = 100,
    output.plot = plot,
    output.plotExtended = plot,

    ## get_RLum
    recordType = c("OSL (UVVIS)", "irradiation (NA)")
  )

  #modify this list on purpose
  additional_arguments <- modifyList(x = additional_arguments,
                                     val = list(...))

  ##set function arguments
  function_arguments <- NULL

  ## variable names to monitor
  variable.names <- c('central_D', 'sigma_D', 'D', 'Q', 'a', 'b', 'c', 'g')
  if (!is.null(method_control[["variable.names"]])) {
    ## take variable from the user but remove nonsense values
    variable.names <- setdiff(method_control[["variable.names"]],
                              c("", NA))
  }

  ## always monitor the D variable
  variable.names <- unique(c(variable.names, "D"))


  # Set input -----------------------------------------------------------------------------------

  ##if the input is already of type RLum.Results, use the input and do not run
  ##all pre-calculations again
  if(is(object, "RLum.Results")){
    if(object@originator == "analyse_baSAR"){

      ##We want to use previous function arguments and recycle them

        ##(1) get information you need as input from the RLum.Results object
        function_arguments <- as.list(object@info$call)

        ##(2) overwrite by current provided arguments
        ##by using a new argument we have the choise which argument is allowed for
        ##changes
        function_arguments.new <- modifyList(x = function_arguments, val = as.list(match.call()))

     ##get maximum cycles
     max_cycles <- max(object$input_object[["CYCLES_NB"]])

     ##set Nb_aliquots
     Nb_aliquots <- nrow(object$input_object)

     ## return NULL if not at least three aliquots are used for the calculation
     if (Nb_aliquots < 3) {
       .throw_message("Number of aliquots < 3, NULL returned")
       return(NULL)
     }

     ##set variables
     ##Why is.null() ... it prevents that in case of a function crash is nothing is provided ...

     ##set changeable function arguments

       ##distribution
       if(!is.null(function_arguments.new$distribution)){
         distribution <- function_arguments.new$distribution
       }

       ##n.MCMC
       if(!is.null(function_arguments.new$n.MCMC)){
         n.MCMC <- function_arguments.new$n.MCMC
       }

       ##fit.method
       if(!is.null(function_arguments.new$fit.method)){
         fit.method <- function_arguments.new$fit.method
       }

       ## fit.force_through_origin
       if(!is.null(function_arguments.new$fit.force_through_origin)){
          fit.force_through_origin <- function_arguments.new$fit.force_through_origin
       }

       ##fit.includingRepeatedRegPoints
       if(!is.null(function_arguments.new$fit.includingRepeatedRegPoints)){
          fit.includingRepeatedRegPoints <- function_arguments.new$fit.includingRepeatedRegPoints
       }

       ##source_doserate
       if(length(as.list(match.call())$source_doserate) > 0){
         .throw_warning("'source_doserate' is ignored in this mode as ",
                        "it was already set")
       }

       ##aliquot_range
       if(!is.null(function_arguments.new$aliquot_range)){
         aliquot_range <- eval(function_arguments.new$aliquot_range)
       }

       ##method_control
       if(!is.null(function_arguments.new$method_control)){
         method_control <- eval(function_arguments.new$method_control)
       }

       ##baSAR_model
       if(!is.null(function_arguments.new$baSAR_model)){
         baSAR_model <- eval(function_arguments.new$baSAR_model)
       }

       ##plot
       if(!is.null(function_arguments.new$plot)){
         plot <- function_arguments.new$plot
       }

       ##verbose
       if(!is.null(function_arguments.new$verbose)){
         verbose <- function_arguments.new$verbose
       }


     ##limit according to aliquot_range
     ##TODO Take care of the case that this was provided, otherwise more and more is removed!
     if (!is.null(aliquot_range)) {
       if (max(aliquot_range) <= nrow(object$input_object)) {
         input_object <- object$input_object[aliquot_range, ]

         ##update list of removed aliquots
         removed_aliquots <-rbind(object$removed_aliquots, object$input_object[-aliquot_range,])

         ##correct Nb_aliquots
         Nb_aliquots <- nrow(input_object)

       } else{
         .throw_message("'aliquot_range' out of bounds, input ignored")

         ##reset aliquot range
         aliquot_range <- NULL

         ##take entire object
         input_object <- object$input_object

         ##set removed aliquots
         removed_aliquots <- object$removed_aliquots
       }

     } else{
       ##set the normal case
       input_object <- object$input_object

       ##set removed aliquots
       removed_aliquots <- object$removed_aliquots
     }

     ##set non function arguments
     Doses <- t(input_object[,9:(8 + max_cycles)])
     LxTx <- t(input_object[,(9 + max_cycles):(8 + 2 * max_cycles)])
     LxTx.error <-  t(input_object[,(9 + 2 * max_cycles):(8 + 3 * max_cycles)])

     rm(max_cycles)

    }else{
      .throw_error("'object' is of type 'RLum.Results', ",
                   "but was not produced by analyse_baSAR()")
    }

  }else{

    if(verbose){
      cat("\n[analyse_baSAR()] ---- PRE-PROCESSING ----\n")
    }

    ##Supported input types are:
    ##  (1) BIN-file
    ##      .. list
    ##      .. character
    ##  (2) RisoeBINfileData object
    ##      .. list
    ##      .. S4
    ##  (3) RLum.Analysis objects
    ##      .. list
    ##      .. S4

    ##In case an RLum.Analysis object is provided we try an ugly conversion only
    if(inherits(object, "list") && all(vapply(object, function(x){inherits(x, "RLum.Analysis")}, logical(1)))){
     if(verbose)
       cat("[analyse_baSAR()] List of RLum.Analysis-objects detected ..\n")

      ## set number of objects
      n_objects <- length(object)

      ##stop for only one element
      if (n_objects < 2)
        .throw_error("At least two aliquots are needed for the calculation")

      ##extract wanted curves
      if(verbose)
        cat(paste0("\t\t  .. extract '", additional_arguments$recordType ,"'\n"))
      object <- get_RLum(object, recordType = additional_arguments$recordType, drop = FALSE)

      ## check that we are not left with empty records
      if (length(object[[1]]@records) == 0) {
        .throw_error("No records of the appropriate type were found")
      }

      ##extract irradiation times
      if(is.null(irradiation_times)){
        if(verbose)
          cat("\t\t  .. extract irradiation times\n")
        irradiation_times <- extract_IrradiationTimes(object[[1]])$irr.times$IRR_TIME
      }

      ##run conversion
      if(verbose)
        cat("\t\t  .. run conversion\n")
      object <- try(convert_RLum2Risoe.BINfileData(object),
                    outFile = stdout()) # redirect error messages so they can be silenced

      ##create fallback
       if(inherits(object, "try-error")){
         .throw_message("Object conversion failed, NULL returned")
         return(NULL)
       }

      ##remove none-OSL curves
      if(!all(object@METADATA[["LTYPE"]] %in% "OSL")){
        if(verbose)
          cat("\t\t  .. remove non-OSL curves\n")
        rm_id <- which(object@METADATA[["LTYPE"]] != "OSL")
        object@METADATA <- object@METADATA[-rm_id,]
        object@DATA[rm_id] <- NULL

        ##reset index
        object@METADATA[["ID"]] <- 1:length(object@METADATA[["ID"]])

        ##delete objects
        rm(rm_id)
      }
    }

    if (inherits(object, "Risoe.BINfileData")) {
      fileBIN.list <- list(object)

    } else if (is(object, "list")) {
      ##check what the list containes ...
      object_type <- unique(sapply(object, function(x) {
        .validate_class(x, c("Risoe.BINfileData", "character"),
                        name = "All elements of 'object'")
        class(x)[1]
      }))

      if (length(object_type) > 1) {
        .throw_error("'object' only accepts a list of objects of the same type")
      }
        if (object_type == "Risoe.BINfileData") {
          fileBIN.list <- object

        } else if (object_type == "character") {
          fileBIN.list <- read_BIN2R(
            file = object,
            position = additional_arguments$position,
            duplicated.rm = additional_arguments$duplicated.rm,
            n.records = additional_arguments$n.records,
            pattern = additional_arguments$pattern,
            verbose = verbose
          )
        }

    } else if (is(object, "character")) {
      fileBIN.list <- list(
        read_BIN2R(
          file = object,
          position = additional_arguments$position,
          duplicated.rm = additional_arguments$duplicated.rm,
          n.records = additional_arguments$n.records,
          pattern = additional_arguments$pattern,
          verbose = verbose
        )
      )
    }

    ##Problem ... the user might have made a pre-selection in the Analyst software, if this the
    ##we respect this selection
    record.selected <- unlist(
      lapply(fileBIN.list,
      FUN = function(x) x@METADATA[["SEL"]] ))

    if (!all(record.selected)) {
      if (verbose) {
        message("[analyse_baSAR()] Record pre-selection in BIN-file detected,",
                "record reduced to selection\n")
      }
      if (sum(record.selected) == 0) {
        .throw_warning("No records selected, NULL returned")
        return(NULL)
      }

      fileBIN.list <- lapply(fileBIN.list, function(x){
            ##reduce data
            x@DATA <- x@DATA[x@METADATA[["SEL"]]]
            x@METADATA <- x@METADATA[x@METADATA[["SEL"]], ]

            ##reset index
            x@METADATA[["ID"]] <- 1:nrow(x@METADATA)
            return(x)
      })
    }

    # Declare variables ---------------------------------------------------------------------------

    Disc <-  list()
    Grain <- list()
    Disc_Grain.list <- list()

    Nb_aliquots <-  0
    previous.Nb_aliquots <- 0
    object.file_name <- list()

    Mono_grain <-  TRUE

    Limited_cycles <- vector()

    ##set information
    for (i in 1 : length(fileBIN.list)) {
      Disc[[i]] <-  list()
      Grain[[i]] <-  list()

      ##get BIN-file name
      object.file_name[[i]] <- unique(fileBIN.list[[i]]@METADATA[["FNAME"]])
    }

    ## remove duplicated entries
    is.duplicated <- duplicated(unlist(object.file_name))
    if (any(is.duplicated)) {
      msg <- paste(.collapse(object.file_name[is.duplicated]),
                   "is a duplicate and therefore removed from the input")
      if(verbose){
        message("[analyse_baSAR()] ", msg)
      }
      .throw_warning(msg)

      ##remove entry
      Disc[is.duplicated] <- NULL
      Grain[is.duplicated] <- NULL
      fileBIN.list[is.duplicated] <- NULL
      object.file_name[is.duplicated] <- NULL
    }

  ## Expand input arguments -------------------------------------------------
  rep.length <- length(fileBIN.list)

  if (is.null(source_doserate)) {
    .throw_error("'source_doserate' is missing, but the current ",
                 "implementation expects dose values in Gy")
  }
  source_doserate <- .listify(source_doserate, rep.length)
  sigmab <- .listify(sigmab, rep.length)
  sig0 <- .listify(sig0, rep.length)
  signal.integral <- .listify(signal.integral, rep.length)
  background.integral <- .listify(background.integral, rep.length)

  if (!is.null(signal.integral.Tx)) {
    signal.integral.Tx <- .listify(signal.integral.Tx, rep.length)
  }
  if (!is.null(background.integral.Tx)) {
    background.integral.Tx <- .listify(background.integral.Tx, rep.length)
  }

  # Read CSV file -----------------------------------------------------------
  if (is.null(CSV_file)) {
    ##select aliquots giving light only, this function accepts also a list as input
    if(verbose){
      cat("[analyse_baSAR()] 'CSV_file' not provided, running automatic grain selection ...\n")
    }

    for (k in 1:length(fileBIN.list)) {
      ##if the uses provides only multiple grain data (GRAIN == 0), the verification
      ##here makes not really sense and should be skipped
      if(length(unique(fileBIN.list[[k]]@METADATA[["GRAIN"]])) > 1){
        aliquot_selection <-
          verify_SingleGrainData(
            object = fileBIN.list[[k]],
            cleanup_level = "aliquot",
            threshold = additional_arguments$threshold,
            cleanup = FALSE
          )

        ## remove grain position 0 (this are usually TL measurements
        ## on the cup or we are talking about multiple aliquot)
        num.grain.pos0 <- sum(aliquot_selection$unique_pairs[["GRAIN"]] == 0,
                              na.rm = TRUE)
        if (sum(num.grain.pos0) > 0) {
          .throw_warning("Automatic grain selection: ", num.grain.pos0,
                         " curves with grain index 0 have been removed ",
                         "from the dataset")
        }

        datalu <-
          aliquot_selection$unique_pairs[!aliquot_selection$unique_pairs[["GRAIN"]] == 0,]

        if(nrow(datalu) == 0){
          .throw_message("Nothing left after the automatic grain selection, ",
                         "NULL returned")
          return(NULL)
        }

      }else{
          .throw_warning("Only multiple grain data provided, ",
                         "automatic selection skipped")
          datalu <- unique(fileBIN.list[[k]]@METADATA[, c("POSITION", "GRAIN")])

          ##set mono grain to FALSE
          Mono_grain <- FALSE
          aliquot_selection <- NA
      }

      ##get number of aliquots (one aliquot has a position and a grain number)
      Nb_aliquots <- nrow(datalu)

      ##write information in variables
      Disc[[k]] <- datalu[["POSITION"]]
      Grain[[k]] <- datalu[["GRAIN"]]

      ##free memory
      rm(datalu, aliquot_selection)
    }
    rm(k)

  } else {

    .validate_class(CSV_file, c("data.frame", "character"), extra = "NULL")

    ## error message used multiple times
    err.msg <- paste("'CSV_file' should have at least 3 columns for the name",
                     "of the file, the disc position and the grain position")

    ##load file if we have a filename
    if (is.character(CSV_file)) {
      ##test for valid file
      if(!file.exists(CSV_file)){
        .throw_error("'CSV_file' does not exist")
      }

      ## import CSV file
      datalu <- data.table::fread(CSV_file, data.table = FALSE,
                                  skip = additional_arguments$skip)

      ###check whether data format is somehow odd, check only the first three columns
      if (ncol(datalu) < 3) {
        .throw_error(err.msg)
      }
      if(!all(grepl(colnames(datalu), pattern = " ")[1:3])){
        .throw_error("One of the first 3 columns in 'CSV_file' has no ",
                     "header. ", err.msg)
      }

      ##get rid of empty rows if the BIN_FILE name column is empty
      datalu <- datalu[!is.na(datalu[[1]]), ]

    } else{

      datalu <- CSV_file

      ##check number of number of columns in data.frame
      if(ncol(datalu) < 3){
        .throw_error(err.msg)
      }

      ##problem: the first column should be of type character, the others are
      ##of type numeric, unfortunately it is too risky to rely on the user, we do the
      ##proper conversion by ourself ...
      datalu[[1]] <- as.character(datalu[[1]])
      datalu[[2]] <- as.numeric(datalu[[2]])
      datalu[[3]] <- as.numeric(datalu[[3]])
    }

    ##limit aliquot range
    if (!is.null(aliquot_range)) {
      datalu <- datalu[aliquot_range,]
    }

    Nb_ali <-  0
    k <- NULL

    for (nn in 1:length((datalu[, 1]))) {
      if (!is.na(datalu[nn, 1]))  {

        ##check whether one file fits
        file.basename <- strsplit(basename(datalu[nn, 1]),
                                  split = ".", fixed = TRUE)[[1]][1]
        matches <- grep(pattern = file.basename, x = unlist(object.file_name))
        if (length(matches) > 0) {
          k <- matches[1]
          nj <- length(Disc[[k]]) + 1

          Disc[[k]][nj] <-  as.numeric(datalu[nn, 2])
          Grain[[k]][nj] <-  as.numeric(datalu[nn, 3])
          Nb_ali <-  Nb_ali + 1
          if (is.na(Grain[[k]][nj]) || Grain[[k]][nj] == 0) {
            Mono_grain <- FALSE
          }

        }else{
          .throw_warning("'", datalu[nn, 1], "' not recognised ",
                         "or not loaded, skipped")
        }

      } else{

        if (Nb_ali == 0) {
          .throw_error("Number of discs/grains = 0")
        }

        break()
      }
    }

    ##if k is NULL it means it was not set so far, so there was
    ##no corresponding BIN-file found
    if(is.null(k)){
      .throw_error("The BIN-file names provided via 'CSV_file' do not match ",
                   "the loaded BIN-files")
    }
  }

  ###################################### loops on files_number
  for (k in 1:length(fileBIN.list)) {
    Disc_Grain.list[[k]] <- list()   # data.file number
    n_aliquots_k <- length(Disc[[k]])

      if(n_aliquots_k == 0){
        fileBIN.list[[k]] <- NULL
        .throw_warning("No data selected from BIN-file ", k,
                       ", BIN-file removed from input")
        next()
      }

    for (d in 1:n_aliquots_k) {
      dd <-  as.integer(unlist(Disc[[k]][d]))
      gg <- if (Mono_grain) as.integer(unlist(Grain[[k]][d])) else 1
      Disc_Grain.list[[k]][[dd]] <- list()  # data.file number ,  disc_number

        Disc_Grain.list[[k]][[dd]][[gg]] <- list()  # data.file number ,  disc_number, grain_number
        for (z in 1:6) {
          Disc_Grain.list[[k]][[dd]][[gg]][[z]] <- list()
          # 1 = index numbers, 2 = irradiation doses,  3 = LxTx , 4 = sLxTx,  5 = N d'aliquot, 6 = De +- D0 +- (4 values)
        }
    }
  }

  if(verbose){
    cat("\n[analyse_baSAR()] Preliminary analysis in progress ... ")
    cat("\n[analyse_baSAR()] Hang on, this may take a while ... \n")
  }

  for (k in 1:length(fileBIN.list)) {

    stopifnot(length(fileBIN.list[[k]]) == nrow(fileBIN.list[[k]]@METADATA))

    ## check that the data available is consistent
    length.data <- nrow(fileBIN.list[[k]]@METADATA)
    length.disc <- length(Disc[[k]])
    if (length.data %% length.disc != 0) {
      ## this can happen if the input data was subset incorrectly (#517)
      .throw_error("In input ", k, " the number of data points (",
                   length.data, ") is not a multiple of the number of ",
                   "positions (", length.disc, ")")
    }

    ### METADATA
    n_index.vector <- fileBIN.list[[k]]@METADATA[["ID"]]
    measured_discs.vector <- fileBIN.list[[k]]@METADATA[["POSITION"]]
    measured_grains.vector <- fileBIN.list[[k]]@METADATA[["GRAIN"]]
    fname <- fileBIN.list[[k]]@METADATA[["FNAME"]]

    ## always get irradiation times
    irrad_time.vector <- fileBIN.list[[k]]@METADATA[["IRR_TIME"]]

    ## now we override, keep in mind that we do not care about the pattern
    if(!is.null(irradiation_times))
      irrad_time.vector <- rep(irradiation_times, length.out = length(irrad_time.vector))

    ##if all irradiation times are 0 we should stop here
    if (length(unique(irrad_time.vector)) == 1) {
      .throw_message("All irradiation times are identical, NULL returned")
      return(NULL)
    }

    disc_pos <- as.integer(unlist(Disc[[k]]))
    grain_pos <- as.integer(unlist(Grain[[k]]))

    ### Automatic Filling - Disc_Grain.list
    for (i in 1: length(Disc[[k]])) {
      disc_selected <-  as.integer(Disc[[k]][i])
      grain_selected <- if (Mono_grain) as.integer(Grain[[k]][i]) else 0

      ## hard break if the disc number or grain number does not fit

         ##disc (position)
         disc_logic <- (disc_selected == measured_discs.vector)
         if (!any(disc_logic)) {
           .throw_message("In BIN-file '", unique(fname), "' position number ",
                          disc_selected, " does not exist, NULL returned")
            return(NULL)
          }

          ##grain
          grain_logic <- (grain_selected == measured_grains.vector)
          if (!any(grain_logic)) {
            .throw_message("In BIN-file '", unique(fname), "' grain number ",
                           grain_selected, " does not exist, NULL returned")
            return(NULL)
          }

          ##if the test passed, compile index list
          index_liste <- n_index.vector[disc_logic & grain_logic]

      if (Mono_grain == FALSE)  {grain_selected <-1}

          for (kn in 1: length(index_liste)) {

              t <- index_liste[kn]

              dose.value <- irrad_time.vector[t]
              if(!is.null(unlist(source_doserate))){
                dose.value <- dose.value * unlist(source_doserate[[k]][1])
              }

              s <- 1 + length( Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[1]] )
              Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[1]][s] <- n_index.vector[t]  # indexes
              if ( s%%2 == 1) { Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[2]][as.integer(1+s/2)] <- dose.value  }      # irradiation doses
          }
    }
  }


  ######################  Data associated with a single Disc/Grain
  max_cycles <-  0
  count <- 1
  calc_OSLLxTxRatio_warning <- list()

  for (k in 1:length(fileBIN.list)) {

    ##plot Ln and Tn curves if wanted
    ##we want to plot the Ln and Tn curves to get a better feeling
    ##The approach here is rather rough coded, but it works
    if (plot) {
      curve_index <- vapply(1:length(Disc[[k]]), function(i) {
        dd <- as.integer(Disc[[k]][i])
        gg <- if (Mono_grain) as.integer(Grain[[k]][i]) else 1

        Ln_index <- as.numeric(Disc_Grain.list[[k]][[dd]][[gg]][[1]][1])
        Tn_index <- as.numeric(Disc_Grain.list[[k]][[dd]][[gg]][[1]][2])
        return(c(Ln_index, Tn_index))
      }, FUN.VALUE = vector(mode = "numeric", length = 2))


      ## data.tables for Ln and Tn values
      Ln_dt <- rbindlist(list(fileBIN.list[[k]]@DATA[curve_index[1, ]]))
      Tn_dt <- rbindlist(list(fileBIN.list[[k]]@DATA[curve_index[2, ]]))

      ##open plot are
      if (!plot_singlePanels) {
        par.default <- par()$mfrow
        par(mfrow = c(1, 2))
      }

      ##get natural curve and combine them in matrix
      graphics::matplot(
        x = 1:nrow(Ln_dt),
        y = Ln_dt,
        col = rgb(0, 0, 0, 0.3),
        ylab = "Luminescence [a.u.]",
        xlab = "Channel",
        main = expression(paste(L[n], " - curves")),
        type = "l"
      )

      ##add integration limits
      abline(v = range(signal.integral[[k]]), lty = 2, col = "green")
      abline(v = range(background.integral[[k]]), lty = 2, col = "red")
      mtext(paste0("ALQ: ",count, ":", count + ncol(curve_index)))

      graphics::matplot(
        x = 1:nrow(Tn_dt),
        y = Tn_dt,
        col = rgb(0, 0, 0, 0.3),
        ylab = "Luminescence [a.u.]",
        xlab = "Channel",
        main = expression(paste(T[n], " - curves")),
        type = "l"
      )

      ##add integration limits depending on the choosen value
      if(is.null(signal.integral.Tx[[k]])){
        abline(v = range(signal.integral[[k]]), lty = 2, col = "green")

      }else{
        abline(v = range(signal.integral.Tx[[k]]), lty = 2, col = "green")
      }

      if(is.null(background.integral.Tx[[k]])){
        abline(v = range(background.integral[[k]]), lty = 2, col = "red")

      }else{
        abline(v = range(background.integral.Tx[[k]]), lty = 2, col = "red")
      }

      mtext(paste0("ALQ: ",count, ":", count + ncol(curve_index)))

      ##reset par
      if (!plot_singlePanels) {
        par(mfrow = par.default)
      }

      ##remove some variables
      rm(curve_index, Ln_dt, Tn_dt)
    }


    for (i in 1:length(Disc[[k]])) {
      dd <- as.integer(Disc[[k]][i])
      gg <- if (Mono_grain) as.integer(Grain[[k]][i]) else 1
      sel.disc.grain <- Disc_Grain.list[[k]][[dd]][[gg]]

      # Data for the selected Disc-Grain
      for (nb_index in 1:(length(sel.disc.grain[[1]]) / 2)) {

        index1 <- as.numeric(sel.disc.grain[[1]][2 * nb_index - 1])
        index2 <- as.numeric(sel.disc.grain[[1]][2 * nb_index])
        this.data <- fileBIN.list[[k]]@DATA
        Lx.data <- data.frame(seq_along(this.data[[index1]]),
                              this.data[[index1]])
        Tx.data <- data.frame(seq_along(this.data[[index2]]),
                              this.data[[index2]])

        ## call calc_OSLLxTxRatio()
        ## we run this function with a warnings catcher to reduce the load of warnings for the user
        temp_LxTx <- withCallingHandlers(
          calc_OSLLxTxRatio(
            Lx.data = Lx.data,
            Tx.data = Tx.data,
            signal.integral = signal.integral[[k]],
            signal.integral.Tx = signal.integral.Tx[[k]],
            background.integral = background.integral[[k]],
            background.integral.Tx = background.integral.Tx[[k]],
            background.count.distribution = additional_arguments$background.count.distribution,
            sigmab = sigmab[[k]],
            sig0 = sig0[[k]]
          ),
          warning = function(c) {
            calc_OSLLxTxRatio_warning[[i]] <<- c
            invokeRestart("muffleWarning")
          }
        )

        ##get LxTx table
        LxTx.table <- temp_LxTx$LxTx.table

        Disc_Grain.list[[k]][[dd]][[gg]][[3]][nb_index] <- LxTx.table[[9]]
        Disc_Grain.list[[k]][[dd]][[gg]][[4]][nb_index] <- LxTx.table[[10]]
        Disc_Grain.list[[k]][[dd]][[gg]][[5]][nb_index] <- LxTx.table[[7]]

        ##free memory
        rm(LxTx.table)
        rm(temp_LxTx)
      }

      ## reset `sel.disc.grain` because the data it pointed to has changed
      sel.disc.grain <- Disc_Grain.list[[k]][[dd]][[gg]]

      # Fitting Growth curve and Plot
      sample_dose <- unlist(sel.disc.grain[[2]])
      sample_LxTx <- unlist(sel.disc.grain[[3]])
      sample_sLxTx <- unlist(sel.disc.grain[[4]])
      TnTx <- unlist(sel.disc.grain[[5]])

      ##create needed data.frame (this way to make sure that rows are doubled if something is missing)
      selected_sample <- as.data.frame(cbind(sample_dose, sample_LxTx, sample_sLxTx, TnTx))

      ## call plot_GrowthCurve() to get De and De value
      fitcurve <-
        suppressWarnings(plot_GrowthCurve(
          sample = selected_sample,
          na.rm = TRUE,
          fit.method = fit.method,
          fit.force_through_origin = fit.force_through_origin,
          fit.weights = additional_arguments$fit.weights,
          fit.includingRepeatedRegPoints = fit.includingRepeatedRegPoints,
          fit.bounds = additional_arguments$fit.bounds,
          n.MC = additional_arguments$n.MC,
          output.plot = additional_arguments$output.plot,
          output.plotExtended = additional_arguments$output.plotExtended,
          txtProgressBar = FALSE,
          verbose = verbose,
          main = paste0("ALQ: ", count," | POS: ", Disc[[k]][i], " | GRAIN: ", Grain[[k]][i])
        ))

        ##get data.frame with De values
        if(!is.null(fitcurve)){
          fitcurve_De <- get_RLum(fitcurve, data.object = "De")

          Disc_Grain.list[[k]][[dd]][[gg]][[6]][1] <- fitcurve_De[["De"]]
          Disc_Grain.list[[k]][[dd]][[gg]][[6]][2] <- fitcurve_De[["De.Error"]]
          Disc_Grain.list[[k]][[dd]][[gg]][[6]][3] <- fitcurve_De[["D01"]]
          Disc_Grain.list[[k]][[dd]][[gg]][[6]][4] <- fitcurve_De[["D01.ERROR"]]
        }else{
          ##we have to do this, otherwise the grains will be sorted out
          Disc_Grain.list[[k]][[dd]][[gg]][[6]][1:4] <- NA
        }

      ## reset `sel.disc.grain` because the data it pointed to has changed
      sel.disc.grain <- Disc_Grain.list[[k]][[dd]][[gg]]

      Limited_cycles[previous.Nb_aliquots + i] <- length(sel.disc.grain[[2]])

      max_cycles <- max(length(sel.disc.grain[[2]]), max_cycles)

        previous.Nb_aliquots <-
            length(stats::na.exclude(Limited_cycles)) # Total count of aliquots

      count <- count + 1
    }

  }   ##  END of loop on BIN files
  rm(count)

  ##evaluate warnings from calc_OSLLxTxRatio()
  if(length(calc_OSLLxTxRatio_warning)>0){
    w_table <- table(unlist(calc_OSLLxTxRatio_warning))
    for(w in 1:length(w_table)){
      .throw_warning(names(w_table)[w], " This warning occurred ",
                     w_table[w], " times")
    }
  }
  rm(calc_OSLLxTxRatio_warning)

  Nb_aliquots <- previous.Nb_aliquots

  ##create results matrix
  OUTPUT_results <-
    matrix(nrow = Nb_aliquots,
           ncol = (8 + 3 * max_cycles),
           byrow = TRUE)

  ## set column name (this makes it much easier to debug)
  colnames(OUTPUT_results) <- c(
    "INDEX_BINfile",
    "DISC",
    "GRAIN",
    "DE",
    "DE.SD",
    "D0",
    "D0.SD",
    "CYCLES_NB",
    paste0("DOSE_", 1:max_cycles),
    paste0("LxTx_", 1:max_cycles),
    paste0("LxTx_", 1:max_cycles, ".SD")
  )

  comptage <- 0
  for (k in 1:length(fileBIN.list)) {

    for (i in 1:length(Disc[[k]])) {
      dd <- as.numeric(Disc[[k]][i])
      gg <- if (Mono_grain) as.numeric(Grain[[k]][i]) else 1

      comptage <- comptage + 1
      OUTPUT_results[comptage, 1] <- k
      OUTPUT_results[comptage, 2] <- as.numeric(dd)
      OUTPUT_results[comptage, 3] <- if (Mono_grain) gg else 0

      sel.disc.grain <- Disc_Grain.list[[k]][[dd]][[gg]]
      if (length(sel.disc.grain[[6]]) != 0) {
        ##DE
        OUTPUT_results[comptage, 4] <- as.numeric(sel.disc.grain[[6]][1])

        ##DE.SD
        OUTPUT_results[comptage, 5] <- as.numeric(sel.disc.grain[[6]][2])

        ##D0
        OUTPUT_results[comptage, 6] <- as.numeric(sel.disc.grain[[6]][3])

        ##D0.SD
        OUTPUT_results[comptage, 7] <- as.numeric(sel.disc.grain[[6]][4])

        ##CYCLES_NB
        OUTPUT_results[comptage, 8] <- length(sel.disc.grain[[2]])

        ## auxiliary variable
        llong <- length(sel.disc.grain[[2]])

        ##Dose
        OUTPUT_results[comptage, 9:(8 + llong)] <- as.numeric(sel.disc.grain[[2]])

        ##LxTx values
        OUTPUT_results[comptage, (9 + max_cycles):(8 + max_cycles + llong)] <-
          as.numeric(sel.disc.grain[[3]])

        ##LxTx SD values
         OUTPUT_results[comptage, (9 + 2*max_cycles):(8 + 2*max_cycles + llong)] <-
           as.numeric(sel.disc.grain[[4]])
      }
    }
  }


  ##Clean matrix and remove all unwanted entries

    ##remove all NA columns, means all NA columns in POSITION and DISC
    ##this NA values are no calculation artefacts, but coming from the data processing and have
    ##no further value
    OUTPUT_results <- OUTPUT_results[!is.na(OUTPUT_results[,2]),]

    ##clean up NaN values in the LxTx and corresponding error values
    ##the transposition of the matrix may increase the performance for very large matrices
    OUTPUT_results_reduced <- t(OUTPUT_results)
    selection <- vapply(X = 1:ncol(OUTPUT_results_reduced), FUN = function(x){
        !any(is.nan(OUTPUT_results_reduced[9:(8+3*max_cycles), x]) | is.infinite(OUTPUT_results_reduced[9:(8+3*max_cycles), x]))

    }, FUN.VALUE = vector(mode = "logical", length = 1))

    removed_aliquots <- t(OUTPUT_results_reduced[,!selection])
    OUTPUT_results_reduced <- t(OUTPUT_results_reduced[,selection])

    ##finally, check for difference in the number of dose points ... they should be the same
    if(length(unique(OUTPUT_results_reduced[,"CYCLES_NB"])) > 1){
      .throw_warning("The number of dose points differs across ",
                     "your data set. Check your data!")
    }

  ##correct number of aliquots if necessary
  if(Nb_aliquots > nrow(OUTPUT_results_reduced)) {
    Nb_aliquots <- nrow(OUTPUT_results_reduced)
    .throw_warning("'Nb_aliquots' corrected due to NaN or Inf values ",
                   "in Lx and/or Tx to ", Nb_aliquots, ". You might want ",
                   "to check 'removed_aliquots' in the function output.")
  }

  ##Prepare for Bayesian analysis
  Doses <- t(OUTPUT_results_reduced[,9:(8 + max_cycles)])
  LxTx <- t(OUTPUT_results_reduced[, (9 + max_cycles):(8 + 2 * max_cycles)])
  LxTx.error <- t(OUTPUT_results_reduced[, (9 + 2 * max_cycles):(8 + 3 * max_cycles)])

  ##prepare data frame for output that can used as input
  input_object <- data.frame(
    BIN_FILE = unlist(object.file_name)[OUTPUT_results_reduced[[1]]],
    OUTPUT_results_reduced[, -1],
    stringsAsFactors = FALSE
  )


  ##prepare data frame for output that shows rejected aliquots
  if (length(removed_aliquots) > 0) {
    removed_aliquots <-
      as.data.frame(removed_aliquots,  stringsAsFactors = FALSE)
    removed_aliquots <- cbind(BIN_FILE = unlist(object.file_name)[removed_aliquots[[1]]],
                              removed_aliquots[, -1])

  }else{
    removed_aliquots <- NULL
  }
}

  # Call baSAR-function -------------------------------------------------------------------------

  ##check for the central_D bound settings
  ##Why do we use 0 and 1000: Combes et al., 2015 wrote
  ## that "We set the bounds for the prior on the central dose D, Dmin = 0 Gy and
  ## Dmax = 1000 Gy, to cover the likely range of possible values for D.


    ##check if something is set in method control, if not, set it
    if (is.null(method_control[["upper_centralD"]])) {
      method_control <- c(method_control, upper_centralD = 1000)

    }else{
      if(distribution == "normal" | distribution == "cauchy" | distribution == "log_normal"){
        .throw_warning("You have modified the upper central_D boundary ",
                       "while applying a predefined model. This is ",
                       "possible but not recommended!")
      }
    }

    ##we do the same for the lower_centralD, just to have everthing in one place
    if (is.null(method_control[["lower_centralD"]])) {
      method_control <- c(method_control, lower_centralD = 0)

    }else{
      if(distribution == "normal" | distribution == "cauchy" | distribution == "log_normal"){
        .throw_warning("You have modified the lower central_D boundary ",
                       "while applying a predefined model. This is ",
                       "possible but not recommended!")
      }
    }

    if(min(input_object[["DE"]][input_object[["DE"]] > 0], na.rm = TRUE) < method_control$lower_centralD |
       max(input_object[["DE"]], na.rm = TRUE) > method_control$upper_centralD){
      .throw_warning("Your lower_centralD and/or upper_centralD values ",
                     "seem not to fit to your input data. This may indicate ",
                     "a wronlgy set 'source_doserate'.")
    }

  ##>> try here is much better, as the user might run a very long preprocessing and do not
  ##want to fail here
  results <-
    try(.baSAR_function(
      Nb_aliquots = Nb_aliquots,
      distribution = distribution,
      data.Dose = Doses,
      data.Lum = LxTx,
      data.sLum = LxTx.error,
      fit.method = fit.method,
      n.MCMC = n.MCMC,
      fit.force_through_origin = fit.force_through_origin,
      fit.includingRepeatedRegPoints = fit.includingRepeatedRegPoints,
      method_control = method_control,
      baSAR_model = baSAR_model,
      verbose = verbose
    ), outFile = stdout()) # redirect error messages so they can be silenced

  ##check whether this became NULL
  if(!is(results, "try-error")){
    ##how do we add the systematic error?
    ##(1) source_doserate is a list, not a vector, but the user can
    ##provide many source dose rates and he can provide only a single vector (no error)

    systematic_error <- 0
    if(!is.null(unlist(source_doserate)) || !is.null(function_arguments$source_doserate)){
      ##if it comes from the previous call, it is, unfortunately not that simple
      if(!is.null(function_arguments$source_doserate)){
        source_doserate <- eval(function_arguments$source_doserate)

        if(!is(source_doserate, "list")){
          source_doserate <- list(source_doserate)
        }
      }

      systematic_error <- unlist(lapply(source_doserate, function(x){
        if(length(x) == 2) x[2] else 0
       }))
    }

    ##state are warning for very different errors
    if(mean(systematic_error) != systematic_error[1]){
      .throw_warning("Provided source dose rate errors differ. The mean ",
                     "was taken, but the calculated systematic error ",
                     "might not be valid")
    }

    ##add to the final de
    DE_FINAL.ERROR <- sqrt(results[[1]][["CENTRAL.SD"]]^2 + mean(systematic_error)^2)

    ##consider the case that we get NA and this might be confusing
    if(is.na(DE_FINAL.ERROR)){
      DE_FINAL.ERROR <- results[[1]][["CENTRAL.SD"]]
    }

    ##combine
    results[[1]] <- cbind(results[[1]], DE_FINAL = results[[1]][["CENTRAL"]], DE_FINAL.ERROR = DE_FINAL.ERROR)

  }else{
    results <- NULL
    verbose <- FALSE
    plot <- FALSE
  }

  # Terminal output -----------------------------------------------------------------------------
  if(verbose){
    cat("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n")
    cat("\n[analyse_baSAR()] ---- RESULTS ---- \n")
    cat("------------------------------------------------------------------\n")
    cat(paste0("Used distribution:\t\t", results[[1]][["DISTRIBUTION"]],"\n"))
    num.aliquots <- results[[1]][["NB_ALIQUOTS"]]
    tot.aliquots <- num.aliquots
    if (!is.null(removed_aliquots))
      tot.aliquots <- tot.aliquots + nrow(removed_aliquots)
    cat(paste0("Number of aliquots used:\t", num.aliquots, "/", tot.aliquots))
    if (!is.null(aliquot_range)) {
      cat(" (manually removed: ", length(aliquot_range), ")\n")
    } else {
      cat("\n")
    }

    extra <- if (!is.null(baSAR_model)) " (user defined)" else ""
    cat("Considered fitting method:\t", results[[1]][["FIT_METHOD"]],
        extra, "\n")
    cat("Number of independent chains:\t", results[[1]][["N.CHAINS"]], "\n")
    cat("Number MCMC iterations/chain:\t", results[[1]][["N.MCMC"]], "\n")
    cat("------------------------------------------------------------------\n")
    if(distribution == "log_normal"){
      cat("\t\t\t\tmean*\tsd\tHPD\n")

    }else{
      cat("\t\t\t\tmean\tsd\tHPD\n")
    }


    cat(paste0(">> Central dose:\t\t", results[[1]][["CENTRAL"]],"\t",
               results[[1]][["CENTRAL.SD"]],"\t",
               "[", results[[1]][["CENTRAL_Q_.16"]]," ; ", results[[1]][["CENTRAL_Q_.84"]], "]**\t"))
    cat(paste0("\n\t\t\t\t\t\t[", results[[1]][["CENTRAL_Q_.025"]]," ; ", results[[1]][["CENTRAL_Q_.975"]],"]***"))

    cat(paste0("\n>> sigma_D:\t\t\t", results[[1]][["SIGMA"]],"\t", results[[1]][["SIGMA.SD"]], "\t",
               "[",results[[1]][["SIGMA_Q_.16"]]," ; ", results[[1]][["SIGMA_Q_.84"]], "]**\t"))
    cat(paste0("\n\t\t\t\t\t\t[",results[[1]][["SIGMA_Q_.025"]]," ; ", results[[1]][["SIGMA_Q_.975"]], "]***"))
    cat(paste0("\n>> Final central De:\t\t", results[[1]][["DE_FINAL"]],"\t", round(results[[1]][["DE_FINAL.ERROR"]], digits = digits), "\t",
               " - \t -"))
    cat("\n------------------------------------------------------------------\n")
    cat(
      paste("(systematic error contribution to final De:",
            format((1-results[[1]][["CENTRAL.SD"]]/results[[1]][["DE_FINAL.ERROR"]])*100, scientific = TRUE), "%)\n")
    )
    if(distribution == "log_normal"){
     cat("* mean of the central dose is the geometric mean\n")
    }
    cat("** 68 % level | *** 95 % level\n")
  }


  # Plotting ------------------------------------------------------------------------------------
  if(plot){

    ## deprecated argument
    if ("plot.single" %in% names(list(...))) {
      plot_singlePanels <- list(...)$plot.single
      .throw_warning("'plot.single' is deprecated, use 'plot_singlePanels' ",
                     "instead")
    }

    ##get colours from the package Luminescence
    col <- get("col", pos = .LuminescenceEnv)

    ##get list of variable names (we need them later)
    varnames <- coda::varnames(results[[2]])

    ##////////////////////////////////////////////////////////////////////////////////////////////
    ##TRACE AND DENSITY PLOT
    ####//////////////////////////////////////////////////////////////////////////////////////////
    if(plot_reduced){
      if (!all(c("central_D", "sigma_D") %in% variable.names)) {
        var.missing <- setdiff(c("central_D", "sigma_D"), variable.names)
        .throw_message("Plots for 'central_D' and 'sigma_D' could not be ",
                       "produced as 'variable.names' does not include ",
                       .collapse(var.missing))
      } else {
        try(plot(results[[2]][, c("central_D", "sigma_D"), drop = FALSE]),
            silent = TRUE)
      }
    }else{
      try(plot(results[[2]]))
    }


    ##////////////////////////////////////////////////////////////////////////////////////////////
    ##TRUE DOSE PLOT AND DECISION MAKER
    ####//////////////////////////////////////////////////////////////////////////////////////////
    if (!plot_singlePanels) {
      par(mfrow = c(2, 2))
    }

    ##get list with D values
    ##get list out of it
    plot_matrix <- as.matrix(results[[2]][,grep(x = varnames, pattern = "D[", fixed = TRUE)])
    aliquot_quantiles <- t(matrixStats::colQuantiles(x = plot_matrix, probs = c(0.25,0.75)))

    ##define boxplot colours ... we have red and orange
    box.col <- vapply(1:ncol(aliquot_quantiles), function(x){
      if(aliquot_quantiles[2,x] < results[[1]][,c("CENTRAL_Q_.025")] |
         aliquot_quantiles[1,x] > results[[1]][,c("CENTRAL_Q_.975")]
      ){
        col[2]
      }else if(aliquot_quantiles[2,x] < results[[1]][,c("CENTRAL_Q_.16")] |
               aliquot_quantiles[1,x] > results[[1]][,c("CENTRAL_Q_.84")]){
        "orange"
      }else{
        "white"
      }
    }, FUN.VALUE = vector(mode = "character", length = 1))

    ## to assure a minimum of quality not more then 15 boxes are plotted in each plot
    i <- 1

    while(i < ncol(plot_matrix)){
      step <- min(ncol(plot_matrix), i + 14)
      plot_check <- try(graphics::boxplot(
        x = plot_matrix[,i:step],
        use.cols = TRUE,
        horizontal = TRUE,
        outline = TRUE,
        col = box.col[i:step],
        xlab = if(is.null(unlist(source_doserate))){"Dose [s]"}else{"Dose [Gy]"},
        ylab = "Aliquot index",
        yaxt = "n",
        xlim = c(1,19),
        main = paste0("Individual Doses | ALQ: ", i,":",step)
      ))

      if(!is(plot_check, "try-error")){
      if(step == ncol(plot_matrix)){
        axis(side = 2, at = 1:15, labels = as.character(c(i:step, rep(" ", length = 15 - length(i:step)))),
             cex.axis = 0.8
        )

      }else{
        axis(side = 2, at = 1:15, labels = as.character(i:step), cex.axis = 0.8)
      }

      ##add HPD with text
      ##HPD - 68%
      lines(
        x = c(
          results[[1]][, c("CENTRAL_Q_.16")], results[[1]][, c("CENTRAL_Q_.16")],
          results[[1]][, c("CENTRAL_Q_.84")], results[[1]][, c("CENTRAL_Q_.84")]),
        y = c(par()$usr[3], 16, 16, par()$usr[3]),
        lty = 3,
        col = col[3],
        lwd = 1.5
      )
      text(
        x = results[[1]][, c("CENTRAL")],
        y = 16,
        labels = "68 %",
        pos = 3,
        col = col[3],
        cex = 0.9 * par()$cex
      )

      ##HPD - 98 %%
      lines(
        x = c(
          results[[1]][, c("CENTRAL_Q_.025")], results[[1]][, c("CENTRAL_Q_.025")],
          results[[1]][, c("CENTRAL_Q_.975")], results[[1]][, c("CENTRAL_Q_.975")]),
        y = c(par()$usr[3], 17.5, 17.5, par()$usr[3]),
        lty = 3,
        col = col[2],
        lwd = 1.5
      )

      text(
        x = results[[1]][, c("CENTRAL")],
        y = 17.5,
        labels = "95 %",
        pos = 3,
        col = col[2],
        cex = 0.9 * par()$cex)
      }
      ##update counter
      i <- i + 15
    }
    rm(plot_matrix)

    if (!plot_singlePanels) {
      par(mfrow = c(1,2))
      on.exit(par(mfrow = c(1,1), bg = "white", xpd = FALSE), add = TRUE)
    }
    ##////////////////////////////////////////////////////////////////////////////////////////////
    ##DOSE RESPONSE CURVES AND Lx/Tx VALUES
    ####//////////////////////////////////////////////////////////////////////////////////////////

      ##define selection vector
      selection <- c("a[", "b[", "c[", "g[", "Q[1,")

      ##get list out of it
      list_selection <- lapply(X = selection, FUN = function(x){
        unlist(results[[2]][,grep(x = varnames, pattern = x, fixed = TRUE)])
      })

      ## assign only the first letter to avoid `[` in the names
      names(list_selection) <- strtrim(selection, 1)

      ##create matrix
      plot_matrix <- do.call(what = "cbind", args = list_selection)

      ##free memory
      rm(list_selection)


      ##make selection according to the model for the curve plotting
      if (fit.method == "EXP") {ExpoGC <- 1 ; LinGC <-  0 }
      if (fit.method == "LIN") {ExpoGC <- 0 ; LinGC <-  1 }
      if (fit.method == "EXP+LIN") {ExpoGC <- 1 ; LinGC <-  1 }
      if (fit.force_through_origin) {GC_Origin <- 0} else {GC_Origin <- 1}

      ##add choise for own provided model
      fit.method_plot <- fit.method
      if(!is.null(baSAR_model)){
        fit.method_plot <- paste(fit.method_plot, "(user defined)")
      }

       ##open plot area
        ##for the xlim and ylim we have to identify the proper ranges based on the input
        xlim <- c(0, max(input_object[,grep(x = colnames(input_object), pattern = "DOSE")], na.rm = TRUE)*1.1)
        ylim <- c(
          min(input_object[,grep(x = colnames(input_object), pattern = "LxTx")], na.rm = TRUE),
          max(input_object[,grep(x = colnames(input_object), pattern = "LxTx")], na.rm = TRUE)*1.1)

        ##check for position of the legend ... we can do better
        if(results[[1]][["CENTRAL_Q_.975"]] < max(xlim)/2){
          legend_pos <- "topright"

        }else{
          legend_pos <- "topleft"
        }

        ##set plot area
        plot_check <- try(plot(
          NA,
          NA,
          ylim = ylim,
          xlim = xlim,
          ylab = expression(paste(L[x] / T[x])),
          xlab = if(is.null(unlist(source_doserate))){"Dose [s]"}else{"Dose [Gy]"},
          main = "baSAR Dose Response Curves"
        ))


        if (!is(plot_check, "try-error")) {
          ##add mtext
          mtext(side = 3, text = paste("Fit:", fit.method_plot))

          ##check whether we have all data we need (might be not the case of the user
          ##selects own variables)
          var.required <- c("a", "b", "c", "g")
          if (nrow(plot_matrix) != 0 && all(var.required %in% variable.names)) {
            ##plot individual dose response curves
            x <- NA
            for (i in seq(1, nrow(plot_matrix), length.out = 1000)) {
              curve(
                  GC_Origin * plot_matrix[i, "g"] +
                  LinGC * (plot_matrix[i, "c"] * x) +
                  ExpoGC * (plot_matrix[i, "a"] *
                            (1 - exp (-x / plot_matrix[i, "b"]))),
                  add = TRUE,
                  col = rgb(0, 0, 0, .1)
              )
            }
          }else{
            var.missing <- setdiff(var.required, variable.names)
            .throw_message("Dose-response curves could not be plotted as ",
                           "'variable.names' does not include ",
                           .collapse(var.missing))
          }

          ##add dose points
          n.col <-
            length(input_object[, grep(x = colnames(input_object), pattern = "DOSE")])

          ##add rug with natural Lx/Tx
          graphics::rug(side = 2, x = input_object[[9 + n.col]])

          ##plot Lx/Tx values .. without errors ... this is enough here
          for (i in 2:length(input_object[, grep(x = colnames(input_object), pattern = "DOSE")])) {
            ##add error bars
            segments(
              x0 = input_object[[8 + i]],
              x1 = input_object[[8 + i]],
              y0 = input_object[[8 + n.col + i]] - input_object[[8 + 2 * n.col + i]],
              y1 = input_object[[8 + n.col + i]] + input_object[[8 + 2 * n.col + i]],
              col = "grey"
            )

            ##add points in the top of it
            points(
              x = input_object[[8 + i]],
              y = input_object[[8 + n.col + i]],
              pch = 21,
              col = col[11],
              bg = "grey"
            )
          }

          ##add ablines
          abline(
            v = results[[1]][, c("CENTRAL_Q_.16", "CENTRAL_Q_.84")],
            lty = 3,
            col = col[3],
            lwd = 1.2
          )
          abline(v = results[[1]][, c("CENTRAL_Q_.025", "CENTRAL_Q_.975")], lty = 2, col = col[2])

          ##add legend1
          legend(
            legend_pos,
            bty = "n",
            horiz = FALSE,
            lty = c(3, 2),
            col = c(col[3], col[2]),
            legend = c("HPD - 68 %", "HPD - 95 %")
          )

          ##add legend2
          legend(
            "bottomright",
            bty = "n",
            horiz = FALSE,
            pch = 21,
            col = col[11],
            bg = "grey",
            legend = "measured dose points"
          )
        }
      ##remove object, it might be rather big
      rm(plot_matrix)

      ##03 Abanico Plot
      if(distribution_plot == "abanico"){
        plot_check <- plot_AbanicoPlot(
          data = input_object[, c("DE", "DE.SD")],
          zlab = if(is.null(unlist(source_doserate))){expression(paste(D[e], " [s]"))}else{expression(paste(D[e], " [Gy]"))},
          log.z = distribution == "log_normal",
          z.0 = results[[1]]$CENTRAL,
          y.axis = FALSE,
          polygon.col = FALSE,
          line = results[[1]][,c(
            "CENTRAL_Q_.16", "CENTRAL_Q_.84", "CENTRAL_Q_.025", "CENTRAL_Q_.975")],
          line.col = c(col[3], col[3], col[2], col[2]),
          line.lty = c(3,3,2,2),
          output = TRUE,
          mtext = paste0(
            nrow(input_object) - length(which(is.na(input_object[, c("DE", "DE.SD")]))),
            "/",
            nrow(input_object),
            " plotted (removed are NA values)"
          )
        )

        if (!is.null(plot_check)) {
          legend(
            "topleft",
            legend = c("Central dose", "HPD - 68%", "HPD - 95 %"),
            lty = c(2, 3, 2),
            col = c("black", col[3], col[2]),
            bty = "n",
            cex = par()$cex * 0.8
          )

        }
      }else{
        plot_check <- NULL
      }

      ##In case the Abanico plot will not work because of negative values
      ##provide a KDE
      if(is.null(plot_check) && distribution_plot == "kde"){
        plot_check <- try(suppressWarnings(plot_KDE(
          data = input_object[, c("DE", "DE.SD")],
          xlab = if(is.null(unlist(source_doserate))){expression(paste(D[e], " [s]"))}else{expression(paste(D[e], " [Gy]"))},
          mtext =   paste0(
            nrow(input_object) - length(which(is.na(input_object[, c("DE", "DE.SD")]))),
            "/",
            nrow(input_object),
            " (removed are NA values)"
          )
        )))

        if(!is(plot_check, "try-error")) {
          abline(v = results[[1]]$CENTRAL, lty = 2)
          abline(
            v = results[[1]][, c("CENTRAL_Q_.16", "CENTRAL_Q_.84")],
            lty = 3,
            col = col[3],
            lwd = 1.2
          )
          abline(v = results[[1]][, c("CENTRAL_Q_.025", "CENTRAL_Q_.975")], lty = 2, col = col[2])

          ##check for position of the legend
          if(results[[1]][["CENTRAL_Q_.975"]] < max(xlim)/2){
            legend_pos <- "right"

          }else{
            legend_pos <- "topleft"
          }

          legend(
            legend_pos,
            legend = c("Central dose", "HPD - 68%", "HPD - 95 %"),
            lty = c(2, 3, 2),
            col = c("black", col[3], col[2]),
            bty = "n",
            cex = par()$cex * 0.8
          )
        }
      }
  }

  # Return --------------------------------------------------------------------------------------
  return(set_RLum(
    class = "RLum.Results",
    data = list(
      summary = results[[1]],
      mcmc = results[[2]],
      models = results[[3]],
      input_object = input_object,
      removed_aliquots = removed_aliquots
      ),
    info = list(call = sys.call())
  ))
}

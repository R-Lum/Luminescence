#' Bayesian models (baSAR) applied on luminescence data
#'
#' This function allows the application of Bayesian models on luminescence data, measured
#' with the single-aliquot regenerative-dose (SAR, Murray and Wintle, 2000) protocol. In particular,
#' it follows the idea proposed by Combes et al., 2015 of using an hierarchical model for estimating
#' a central equivalent dose from a set of luminescence measurements. This function is (I) the adaption
#' of this approach for the R environement and (II) an extension and a technical refinement of the
#' published code. \cr
#'
#' Internally the function consists of two parts: (I) The Bayesian core for the bayesian calculations
#' and applying the hierchical model and (II) a data pre-processing part. The Bayesian core can be run
#' independently, if the input data are sufficient (see below). The data pre-processing part was
#' implemented to simplify the analysis for the user as all needed data pre-processing is done
#' by the function, i.e. in theory it is enough to provide a BIN/BINX-file with the SAR measurement
#' data. For the Bayesian analysis for each aliquot the following information are needed from the SAR analysis.
#' LxTx, the LxTx error and the dose values for all regeneration points.
#'
#'
#' \bold{Allowed input data}\cr
#'
#' Various inputs are allowed for this function. Unfortunately this makes the function handling rather
#' complex, but at the same time very powerful. Available scenarios:\cr
#'
#' \bold{(1) - \code{object} is BIN-file or link to a BIN-file}
#'
#' Finally it does not matter how the information of the BIN/BINX file are provided. The function
#' supports (a) either a path to a file or directory or a \code{list} of file names or paths or (b)
#' a \code{\linkS4class{Risoe.BINfileData}} object or a list of these objects. The latter one can
#' be produced by using the function \code{\link{read_BIN2R}}, but this function is called automatically
#' if only a filename and/or a path is provided. In both cases it will become the data that can be
#' used for the analysis.
#'
#' \code{XLS_file = NULL}\cr
#'
#' If no XLS file (or data frame with the same format) is provided the functions runs an automatic process that
#' consists of the following steps:
#'
#' \itemize{
#'  \item Select all valid aliquots using the function \code{\link{verify_SingleGrainData}}
#'  \item Calculate Lx/Tx values using the function \code{\link{calc_OSLLxTxRatio}}
#'  \item Calculate De values using the function \code{\link{plot_GrowthCurve}}
#' }
#'
#' These proceded data are subsequently used in for the Bayesian analysis
#'
#' \code{XLS_file != NULL}\cr
#'
#' If an XLS-file is provided or a \code{data.frame} providing similar information the pre-processing
#' steps consists of the following steps:
#'
#' \itemize{
#'  \item Calculate Lx/Tx values using the function \code{\link{calc_OSLLxTxRatio}}
#'  \item Calculate De values using the function \code{\link{plot_GrowthCurve}}
#' }
#'
#' Means, the XLS file should contain a selection of the BIN-file names and the aliquots selected
#' for the further analysis. This allows a manual selection of input data, as the automatic selection
#' by \code{\link{verify_SingleGrainData}} might be not totally suffcient.\cr
#'
#'
#' \bold{(2) - \code{object} \code{RLum.Results object}}
#'
#' If an \code{\linkS4class{RLum.Results}} object is provided as input and(!) this object was
#' previously created by the function \code{analyse_baSAR()} itself, the pre-processing part
#' is skipped and the function starts directly the Bayesian analysis. This option is very powerfull
#' as it allows to change parameters for the Bayesian analysis without the need to repeat
#' the data pre-processing.\cr
#'
#' \bold{\code{method_control}}\cr
#'
#' This are arguments that can be passed directly to the Bayesian calculation core, supported arguments
#' are:
#'
#' \tabular{lll}{
#' \bold{Parameter} \tab \bold{Type} \tab \bold{Descritpion}\cr
#' \code{lower_De} \tab \code{\link{numeric}} \tab sets the lower bound for the expected De range\cr
#' \code{upper_De} \tab \code{\link{numeric}} \tab sets the upper bound for the expected De range\cr
#' }
#'
#' \bold{Additional arguments support via the \code{...} argument}\cr
#'
#' This list summarizes the additional arguments that can be passed to the internally used
#' funtions.
#'
#' \tabular{llll}{
#' \bold{Supported argument} \tab \bold{Corresponding function} \tab \bold{Default} \tab \bold{Short description}\cr
#' \code{threshold} \tab \code{\link{verify_SingleGrainData}} \tab \code{30} \tab change rejection threshold for curve selection \cr
#' \code{sheet} \tab \code{\link[readxl]{read_excel}} \tab \code{1} \tab select XLS-sheet for import\cr
#' \code{col_names} \tab \code{\link[readxl]{read_excel}} \tab \code{TRUE} \tab first row in XLS-file is header\cr
#' \code{col_types} \tab \code{\link[readxl]{read_excel}} \tab \code{NULL} \tab limit import to specific columns\cr
#' \code{skip} \tab \code{\link[readxl]{read_excel}} \tab \code{0} \tab number of rows to be skipped during import\cr
#' \code{n.records} \tab \code{\link{read_BIN2R}} \tab \code{NULL} \tab limit records during BIN-file import\cr
#' \code{duplicated.rm} \tab \code{\link{read_BIN2R}} \tab \code{TRUE} \tab remove duplicated records in the BIN-file\cr
#' \code{pattern} \tab \code{\link{read_BIN2R}} \tab \code{TRUE} \tab select BIN-file by name pattern\cr
#' \code{position} \tab \code{\link{read_BIN2R}} \tab \code{NULL} \tab limit import to a specific position\cr
#' \code{background.count.distribution} \tab \code{\link{calc_OSLLxTxRatio}} \tab \code{"non-poisson"} \tab set assumed count distribution\cr
#' \code{fit.weights} \tab \code{\link{plot_GrowthCurve}} \tab \code{TRUE} \tab enable/disable fit weights\cr
#' \code{fit.bounds} \tab \code{\link{plot_GrowthCurve}} \tab \code{TRUE} \tab enable/disable fit bounds\cr
#' \code{NumberIterations.MC} \tab \code{\link{plot_GrowthCurve}} \tab \code{100} \tab number of MC runs for error calculation\cr
#' \code{output.plot} \tab \code{\link{plot_GrowthCurve}} \tab \code{TRUE} \tab enables/disables dose repsonse curve plot\cr
#' \code{output.plotExtended} \tab \code{\link{plot_GrowthCurve}} \tab \code{TRUE} \tab enables/disables extended dose repsonse curve plot\cr
#' }
#'
#'
#'
#' @param object \code{\linkS4class{Risoe.BINfileData}} or \code{\linkS4class{RLum.Results}} or
#' \code{\link{character}} or \code{\link{list}} (\bold{required}):
#' input object used for the Bayesian analysis. If a \code{character} is provided the function
#' assumes a file connection and tries to import a BIN-file using the provided path. If a \code{list} is
#' provided the list can only contain either \code{Risoe.BINfileData} objects or \code{character}s
#' providing a file connection. Mixing of both types is not allowed. If an \code{\linkS4class{RLum.Results}}
#' is provided the function direclty starts with the Bayesian Analysis (see details)
#'
#' @param XLS_file \code{\link{character}} (optional): XLS_file with data for the analysis. This file must contain 3 columns: the name of the file, the disc position and the grain position (the last being 0 for multi-grain measurements)
#' @param aliquot_range \code{\link{numeric}} (optional): allows to limit the range of the aliquots
#' used for the analysis. This argument has only an effect if the argument \code{XLS_file} is used as
#' well
#'
#' @param source_doserate \code{\link{numeric}} (optional): source dose rate of beta-source used
#' for the measuremnt and its uncertainty in Gy/s, e.g., \code{source_doserate = c(0.12, 0.04)}.
#' If nothing is provided the results are returned in the same domain as the input values.
#' Paramater can be provided as \code{list}, for the case that more than one BIN-file is provided, e.g.,
#' \code{source_doserate = list(c(0.04, 0.004), c(0.05, 0.004))}.
#'
#' @param signal.integral \code{\link{vector}} (\bold{required}): vector with the
#' limits for the signal integral used for the calculation, e.g., \code{signal.integral = c(1:5)}
#' Ignored if \code{object} is an \code{\linkS4class{RLum.Results}} object.
#' The parameter can be provided as \code{list}, \code{source_doserate}.
#'
#' @param signal.integral.Tx \code{\link{vector}} (optional): vector with the
#' limits for the signal integral for the Tx curve. If nothing is provided the
#' value from \code{signal.integral} is used and it is ignored
#' if \code{object} is an \code{\linkS4class{RLum.Results}} object.
#' The parameter can be provided as \code{list}, \code{source_doserate}.
#'
#' @param background.integral \code{\link{vector}} (\bold{required}): vector with the
#' bounds for the background integral.
#' Ignored if \code{object} is an \code{\linkS4class{RLum.Results}} object.
#' The parameter can be provided as \code{list}, \code{source_doserate}.
#'
#' @param background.integral.Tx \code{\link{vector}} (optional): vector with the
#' limits for the background integral for the Tx curve. If nothing is provided the
#' value from \code{background.integral} is used.
#' Ignored if \code{object} is an \code{\linkS4class{RLum.Results}} object.
#' The parameter can be provided as \code{list}, \code{source_doserate}.
#'
#' @param sigmab \code{\link{numeric}} (with default): option to set a manual value for
#' the overdispersion (for LnTx and TnTx), used for the Lx/Tx error
#' calculation. The value should be provided as absolute squared count values, cf. \code{\link{calc_OSLLxTxRatio}}.
#' The parameter can be provided as \code{list}, \code{source_doserate}.
#'
#' @param sig0 \code{\link{numeric}} (with default): allow adding an extra component of error
#' to the final Lx/Tx error value (e.g., instrumental errror, see details is \code{\link{calc_OSLLxTxRatio}}).
#' The parameter can be provided as \code{list}, \code{source_doserate}.
#'
#' @param distribution \code{\link{character}} (with default): type of distribution that is used during
#' Bayesian calculations for determining the Central dose and overdispersion values.
#' Allowed inputs are \code{"cauchy"}, \code{"normal"} and \code{"log_normal"}.
#'
#' @param n.MCMC \code{\link{integer}} (with default): number of iterations for the Markov chain Monte Carlo (MCMC)
#' simulations
#'
#' @param fit.method \code{\link{character}} (with default): fit method used for fitting the growth
#' curve using the function \code{\link{plot_GrowthCurve}}. Here supported methods: \code{EXP},
#' \code{EXP+LIN} and \code{LIN}
#'
#' @param fit.force_through_origin \code{\link{logical}} (with default): force fitting through origin
#'
#' @param fit.includingRepeatedRegPoints \code{\link{logical}} (with default):
#' includes the recycling point (assumed to be measured during the last cycle)
#'
#' @param method_control \code{\link{list}} (optional): named list of control parameters that can be directly
#' passed to the Bayesian analysis, e.g., \code{method_control = list(lower_De = 0.01)}.
#' See details for further information
#'
#' @param plot \code{\link{logical}} (with default): enables or disables plot output
#'
#' @param plot_reduced \code{\link{logical}} (with default): enables or disables the advanced plot output
#'
#' @param verbose \code{\link{logical}} (with default): enables or disables verbose mode
#'
#' @param ... parameters that can be passed to the function \code{\link{calc_OSLLxTxRatio}} (almost full support)
#' \code{\link[readxl]{read_excel}} (full support), \code{\link{read_BIN2R}} (\code{n.records},
#' \code{position}, \code{duplicated.rm}), see details.
#'
#' @section Function version: 0.1.0
#'
#' @author Norbert Mercier, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), Sebastian Kreutzer,
#' IRAMAT-CRP2A, Universite Bordeaux Montaigne (France) \cr
#'
#' The underlying Bayesian model based on a contribution by Combes et al., 2015.
#'
#' @seealso \code{\link{read_BIN2R}}, \code{\link{calc_OSLLxTxRatio}}, \code{\link{plot_GrowthCurve}},
#' \code{\link[readxl]{read_excel}}, \code{\link{verify_SingleGrainData}},
#' \code{\link[rjags]{jags.model}}, \code{\link[rjags]{coda.samples}}
#'
#' @references
#'
#' Combes, B., Philippe, A., Lanos, P., Mercier, N., Tribolo, C., Guerin, G., Guibert, P., Lahaye, C., 2015.
#' A Bayesian central equivalent dose model for optically stimulated luminescence dating.
#' Quaternary Geochronology 28, 62-70. doi:10.1016/j.quageo.2015.04.001
#'
#' \bold{Further reading}
#'
#' Gelman, A., Carlin, J.B., Stern, H.S., Dunson, D.B., Vehtari, A., Rubin, D.B., 2013.
#' Bayesian Data Analysis, Third Edition. CRC Press.
#'
#' Murray, A.S., Wintle, A.G., 2000. Luminescence dating of quartz using an improved single-aliquot
#' regenerative-dose protocol. Radiation Measurements 32, 57-73. doi:10.1016/S1350-4487(99)00253-X
#'
#' @note \bold{If you provide more than one BIN-file}, it is \bold{strongly} recommanded to provide
#' a \code{list} with the same number of elements for the following parameters:\cr
#' \code{source_doserate}, \code{signal.integral}, \code{signal.integral.Tx}, \code{background.integral},
#' \code{background.integral.Tx}, \code{sigmab}, \code{sig0}.\cr
#'
#' Example for two BIN-files: \code{source_doserate = list(c(0.04, 0.006), c(0.05, 0.006))}\cr
#'
#' \bold{This function has beta status!} and is limited to work with
#' standard Risoe BIN-files only!
#'
#' @keywords datagen
#'
#'
#' @examples
#'
#'##(1) load package test data set
#'data(ExampleData.BINfileData, envir = environment())
#'
#'##(2) selecting relevant curves, and limit dataset
#'CWOSL.SAR.Data <- subset(
#'  CWOSL.SAR.Data,
#'  subset = POSITION == c(1:3) & LTYPE == "OSL")
#'
#'\dontrun{
#'##(3) run analysis
#'##please not that the here selected parameters are
#'##choosen for performance, not for reliability
#'analyse_baSAR(
#'  object = CWOSL.SAR.Data,
#'  signal.integral = c(1:2),
#'  background.integral = c(80:100),
#'  fit.method = "LIN",
#'  plot = FALSE,
#'  n.MCMC = 200
#')
#'
#'
#' ##XLS_file template
#' ##copy and paste this the code below in the terminal
#' ##you can further use the function write.csv() to export the example
#'
#' XLS_file <-
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
#' @export
analyse_baSAR <- function(
  object,
  XLS_file = NULL,
  aliquot_range = NULL,
  source_doserate = NULL,
  signal.integral,
  signal.integral.Tx = NULL,
  background.integral,
  background.integral.Tx = NULL,
  sigmab = 0,
  sig0 = 0.025,
  distribution = "cauchy",
  n.MCMC = 100000,
  fit.method = "EXP",
  fit.force_through_origin = TRUE,
  fit.includingRepeatedRegPoints = TRUE,
  method_control = list(),
  plot = TRUE,
  plot_reduced = TRUE,
  verbose = TRUE,
  ...
){


  ##TODO :
  ## - data.frame as input is still untested

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
             plot,
             verbose)
    {


      ##we have to do that this way, as otherwise the Rjags function chrashes
      lower_De <-
        if (is.null(method_control[["lower_De"]])) {
          0.01
        } else{
          method_control[["lower_De"]]
        }
      upper_De <-
        if (is.null(method_control[["upper_De"]])) {
          1000
        } else{
          method_control[["upper_De"]]
        }

      #check whether this makes sense at all, just a direty and quick test
      stopifnot(lower_De > 0)


      Limited_cycles <- vector()

      if (fit.method == "EXP") {ExpoGC <- 1 ; LinGC <-  0 }
      if (fit.method == "LIN") {ExpoGC <- 0 ; LinGC <-  1 }
      if (fit.method == "EXP+LIN") {ExpoGC <- 1 ; LinGC <-  1 }
      if (fit.force_through_origin == TRUE) {GC_Origin <- 1} else {GC_Origin <- 0}

      if (fit.includingRepeatedRegPoints == TRUE) {
        for (i in 1:Nb_aliquots) {
          Limited_cycles[i] <- length(na.exclude(data.Dose[,i]))}
      }
      else {
        for (i in 1:Nb_aliquots) {
        Limited_cycles[i] <- length(na.exclude(data.Dose[,i])) - 1}
      }

      # Bayesian Models ----------------------------------------------------------------------------
      baSARc_model.bug <- "model {

            central_D ~  dunif(lower_De,upper_De)

            precision_D ~ dt (0, 0.16 * central_D, 1) T(0, )    #    Alternative plus directe proposee par Philippe L.
            sigma_D <-  1/sqrt(precision_D)

            for (i in 1:Nb_aliquots) {
            a[i] ~  dnorm(6.5 , 1/(9.2^2) ) T(0, )
            b[i] ~  dnorm(50 , 1/(1000^2) )  T(0, )
            c[i] ~  dnorm(1.002 , 1/(0.9^2) ) T(0, )
            g[i] ~  dnorm(0.5 , 1/(2.5^2) ) I(-a[i], )
            sigma_f[i]  ~  dexp (20)

            D[i] ~ dt ( central_D , precision_D, 1)    #      Cauchy distribution

            S_y[1,i] <-  1/(sLum[1,i]^2 + sigma_f[i]^2)
            Lum[1,i] ~ dnorm ( Q[1,i] , S_y[1,i])
            Q[1,i]  <-  GC_Origin * g[i] + LinGC * (c[i] * D[i] ) + ExpoGC * (a[i] * (1 - exp (-D[i] /b[i])) )

            for (m in 2:Limited_cycles[i]) {
            S_y[m,i] <-  1/(sLum[m,i]^2 + sigma_f[i]^2)
            Lum[m,i] ~ dnorm( Q[m,i] , S_y[m,i] )
            Q[m,i]  <-  GC_Origin * g[i] + LinGC * (c[i] * Dose[m,i]) + ExpoGC * (a[i] * (1 - exp (-Dose[m,i]/b[i])) )
            }
            }
          }"

      # Normal distribution
      baSARn_model.bug <- "model {

            central_D ~  dunif(lower_De,upper_De)

            sigma_D ~ dunif(0.01, 1 * central_D)

            for (i in 1:Nb_aliquots) {
            a[i] ~  dnorm(6.5 , 1/(9.2^2) ) T(0, )
            b[i] ~  dnorm(50 , 1/(1000^2) )  T(0, )
            c[i] ~  dnorm(1.002 , 1/(0.9^2) ) T(0, )
            g[i] ~  dnorm(0.5 , 1/(2.5^2) ) I(-a[i], )
            sigma_f[i]  ~  dexp (20)

            D[i] ~ dnorm ( central_D , 1/(sigma_D^2) )   #           Normal distribution

            S_y[1,i] <-  1/(sLum[1,i]^2 + sigma_f[i]^2)
            Lum[1,i] ~ dnorm ( Q[1,i] , S_y[1,i])
            Q[1,i]  <-  GC_Origin * g[i] + LinGC * (c[i] * D[i] ) + ExpoGC * (a[i] * (1 - exp (-D[i] /b[i])) )

            for (m in 2:Limited_cycles[i]) {
            S_y[m,i] <-  1/(sLum[m,i]^2 + sigma_f[i]^2)
            Lum[m,i] ~ dnorm( Q[m,i] , S_y[m,i] )
            Q[m,i]  <-  GC_Origin * g[i] + LinGC * (c[i] * Dose[m,i]) + ExpoGC * (a[i] * (1 - exp (-Dose[m,i]/b[i])) )
            }
            }
            }"

      # Log-Normal distribution
      baSARl_model.bug <- "model {

            central_D ~  dunif(lower_De,upper_De)

            log_central_D <-  log(central_D) - 0.5 * l_sigma_D^2
            l_sigma_D ~ dunif(0.01, 1 * log(central_D))
            sigma_D <-  sqrt((exp(l_sigma_D^2) -1) * exp( 2*log_central_D + l_sigma_D^2) )

            for (i in 1:Nb_aliquots) {
            a[i] ~  dnorm(6.5 , 1/(9.2^2) ) T(0, )
            b[i] ~  dnorm(50 , 1/(1000^2) )  T(0, )
            c[i] ~  dnorm(1.002 , 1/(0.9^2) ) T(0, )
            g[i] ~  dnorm(0.5 , 1/(2.5^2) ) I(-a[i], )
            sigma_f[i]  ~  dexp (20)

            log_D[i] ~ dnorm ( log_central_D , 1/(l_sigma_D^2) )  #          Log-Normal distribution
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
        }"

      ### Bayesian inputs
      data_Liste  <- list(
        'Dose' = data.Dose,
        'Lum' = data.Lum,
        'sLum' = data.sLum,
        'LinGC' = LinGC,
        'ExpoGC' = ExpoGC,
        'GC_Origin' = GC_Origin,
        'Limited_cycles' = Limited_cycles,
        'lower_De' = lower_De,
        'upper_De' = upper_De,
        'Nb_aliquots' = Nb_aliquots
      )

      if(verbose){
        cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
        cat("[analyse_baSAR()] Bayesian analysis in progress ... ")
        message(paste(".. >> bounds set to: lower_De =", lower_De, "| upper_De =", upper_De))
      }

      Nb_Iterations <- n.MCMC

      if (distribution == "cauchy") {

        if(verbose){message(".. >> calculation will be done assuming a Cauchy distribution\n")}
        distribution <-  "Cauchy distribution"
        jagsfit <- rjags::jags.model(
          textConnection(baSARc_model.bug),
          data = data_Liste,
          n.chains = 3,
          n.adapt = Nb_Iterations,
          quiet = if(verbose){FALSE}else{TRUE}
         )

      }else if (distribution == "normal") {
        if(verbose){message(".. >> calculation will be done assuming a Normal distribution\n")}
        distribution <-  "Normal distribution"
        jagsfit <- rjags::jags.model(
          textConnection(baSARn_model.bug),
          data = data_Liste,
          n.chains = 3,
          n.adapt= Nb_Iterations,
          quiet = if(verbose){FALSE}else{TRUE}
          )

      }else if (distribution == "log_normal") {
        if(verbose){message(".. >> calculation will be done assuming a Log-Normal distribution")}
        distribution <-  "Log-Normal distribution"
        jagsfit <- rjags::jags.model(
          textConnection(baSARl_model.bug),
          data = data_Liste,
          n.chains = 3,
          n.adapt = Nb_Iterations,
          quiet = if(verbose){FALSE}else{TRUE}
        )
      }else{
        stop("[analyse_baSAR()] unknown input for 'distribution'. Allowed are: 'cauchy', 'normal' or 'log_normal'")

      }

      ##update jags model (it is a S3-method)
      update(
        object = jagsfit,
        n.iter = Nb_Iterations,
        progress.bar = if(verbose){"text"}else{NULL}
        )

      ##get data ... full and reduced, the reduced one to limit the plot output
      sampling <- rjags::coda.samples(
        model = jagsfit,
        variable.names = c('central_D', 'sigma_D', 'D'),
        n.iter = Nb_Iterations / 10,
        thin = 10,
        progress.bar = if(verbose){"text"}else{NULL}
      )

      sampling_reduced <- rjags::coda.samples(
        model = jagsfit,
        variable.names = c('central_D', 'sigma_D'),
        n.iter = Nb_Iterations / 10,
        thin = 10,
        progress.bar = if(verbose){"text"}else{NULL}
      )

      ##CHECK FOR RJAGS
      if(verbose){print(summary(sampling)[[1]])}

      if(plot){

        if(plot_reduced){
          plot(sampling_reduced)

        }else{
          plot(sampling)
        }

      }

      ###############  Screen output
      pt_zero <- 0
      nb_decal <-  2
      pt_zero <- Nb_aliquots
      output.mean <- vector("numeric")

      output.mean[1] <-  round(summary(sampling)[[1]][(pt_zero+1)], 2)
      output.mean[2] <- round(summary(sampling)[[1]][(2*pt_zero+3)], 2)
      output.mean[3] <-  round(summary(sampling)[[1]][(pt_zero+2)], 2)
      output.mean[4] <- round(summary(sampling)[[1]][(2*pt_zero+4)], 2)


      ##show Abanico Plot
      if(plot){
        df <- as.data.frame(summary(sampling)[[1]])
        plot_AbanicoPlot(
          data = df[-c(nrow(df),nrow(df)-1),1:2],
          z.0 = output.mean[1],
          summary = c("n"),
          summary.pos = "topleft",
          zlab = expression(paste(D[e], " [a.u.]")),
          mtext = paste("Central dose: ", output.mean[1], "\u00b1", output.mean[2])
          )

        rm(df)

      }



      #### output data.frame with results
      baSAR.output <- data.frame(
        DISTRIBUTION = distribution,
        NB_ALIQUOTS = Nb_aliquots,
        N.MCMC = n.MCMC,
        FIT_METHOD = fit.method,
        CENTRAL = output.mean[1],
        CENTRAL.SD = output.mean[2],
        SIGMA = output.mean[3],
        SIGMA.SD = output.mean[4]
      )

      return(baSAR.output = list(
        baSAR.output_summary = baSAR.output,
        baSAR.output_matrix = sampling,
        models = list(
          cauchy = baSARc_model.bug,
          normal = baSARn_model.bug,
          log_normal = baSARl_model.bug
          )
      ))

    }
  ##END
  ##////////////////////////////////////////////////////////////////////////////////////////////////

  # Integrity tests -----------------------------------------------------------------------------

  ##check whether rjags is available
  ##code snippet taken from
  ##http://r-pkgs.had.co.nz/description.html
  if (!requireNamespace("rjags", quietly = TRUE)) {
    stop("[analyse_baSAR()] To use this function you have to first install the package 'rjags'.",
         call. = FALSE)
  }

  #capture additional piped arguments
  additional_arguments <- list(

    ##verify_SingleGrainData
    threshold = 30,

    ##calc_OSLLxTxRatio()
    background.count.distribution = "non-poisson",

    ##readxl::read_excel()
    sheet = 1,
    col_names = TRUE,
    col_types = NULL,
    skip = 0,

    ##read_BIN2R()
    n.records = NULL,
    duplicated.rm = TRUE,
    position = NULL,
    pattern = NULL,

    ##plot_GrowthCurve()
    fit.weights = TRUE,
    fit.bounds = TRUE,
    NumberIterations.MC = 100,
    output.plot = if(plot){TRUE}else{FALSE},
    output.plotExtended = if(plot){TRUE}else{FALSE}

  )

  #modify this list on purpose
  additional_arguments <- modifyList(x = additional_arguments,
                                     val = list(...))


  # Set input -----------------------------------------------------------------------------------

  ##if the input is alreayd of type RLum.Results, use the input and do not run
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

     ##return NULL if not a minium of three aliquots are used for the calculation
     if(Nb_aliquots < 2){
       warning("[analyse_baSAR()] number of aliquots < 3, this makes no sense, NULL returned!")
       return(NULL)

     }

     ##set variables
     ##Why is.null() ... it prevents that the function crashed is nothing is provided ...

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
       if(!is.null(function_arguments.new$source_doserate)){
         source_doserate <- eval(function_arguments.new$source_doserate)

       }

       ##method_control
       if(!is.null(function_arguments.new$method_control)){
         method_control <- eval(function_arguments.new$method_control)
       }

       ##plot
       if(!is.null(function_arguments.new$plot)){
         plot <- function_arguments.new$plot
       }


       ##verbose
       if(!is.null(function_arguments.new$verbose)){
         verbose <- function_arguments.new$verbose
       }


     ##set non function arguments
     Doses <- t(object$input_object[,9:(8 + max_cycles)])
     LxTx <- t(object$input_object[,(9 + max_cycles):(8 + 2 * max_cycles)])
     LxTx.error <-  t(object$input_object[,(9 + 2 * max_cycles):(8 + 3 * max_cycles)])

     ##set input object as new input_object
     input_object <- object$input_object

     rm(max_cycles)

    }else{
      stop("[analyse_baSAR()] 'object' is of type 'RLum.Results', but has not been produced by analyse_baSAR()!")

    }


  }else{

    ##Supported input types are:
    ##  (1) BIN-file
    ##      .. list
    ##      .. character
    ##  (2) RisoeBINfileData object
    ##      .. list
    ##      .. S4

    if (is(object, "Risoe.BINfileData")) {
      fileBIN.list <- list(object)

    } else if (is(object, "list")) {
      ##check what the list containes ...
      object_type <-
        unique(unlist(lapply(
          1:length(object),
          FUN = function(x) {
            is(object[[x]])[1]
          }
        )))

      if (length(object_type)  == 1) {
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
        } else{
          stop(
            "[analyse_baSAR()] data type in the input list provided for 'object' is not supported!"
          )
        }

      } else{
        stop("[analyse_baSAR()] 'object' only accepts a list with objects of similar type!")
      }

    } else if (is(object, "character")) {
      fileBIN.list <- list(
        read_BIN2R(
          file = object,
          position = additional_arguments$position,
          duplicated.rm = additional_arguments$duplicated.rm,
          n.records = additional_arguments$n.records,
          verbose = verbose
        )
      )

    } else{
      stop(
        paste0(
          "[analyse_baSAR()] '",
          is(object)[1],
          "' as input is not supported. Check manual for allowed input objects."
        )
      )
    }

  ##################################### Extent parameters to lists ... and expand if necessary

  ##test_parameter = source_doserate
  if(is(source_doserate, "list")){
    source_doserate <- rep(source_doserate, length = length(fileBIN.list))
  }else{
    source_doserate <- rep(list(source_doserate), length = length(fileBIN.list))
  }

  ##sigmab
  if(is(sigmab, "list")){
    sigmab <- rep(sigmab, length = length(fileBIN.list))
    }else{
    sigmab <- rep(list(sigmab), length = length(fileBIN.list))
    }

  ##sig0
  if(is(sig0, "list")){
    sig0 <- rep(sig0, length = length(fileBIN.list))
  }else{
    sig0 <- rep(list(sig0), length = length(fileBIN.list))
  }


  ##test_parameter = signal.integral
  if(is(signal.integral, "list")){
    signal.integral <- rep(signal.integral, length = length(fileBIN.list))
  }else{
    signal.integral <- rep(list(signal.integral), length = length(fileBIN.list))
  }


  ##test_parameter = signal.integral.Tx
  if (!is.null(signal.integral.Tx)) {
    if (is(signal.integral.Tx, "list")) {
      signal.integral.Tx <- rep(signal.integral.Tx, length = length(fileBIN.list))
    } else{
      signal.integral.Tx <- rep(list(signal.integral.Tx), length = length(fileBIN.list))
    }
  }

  ##test_parameter = background.integral
  if(is(background.integral, "list")){
    background.integral <- rep(background.integral, length = length(fileBIN.list))
  }else{
    background.integral <- rep(list(background.integral), length = length(fileBIN.list))
  }


  ##test_parameter = background.integral
  if(is(background.integral, "list")){
    background.integral <- rep(background.integral, length = length(fileBIN.list))
  }else{
    background.integral <- rep(list(background.integral), length = length(fileBIN.list))
  }



  ##test_parameter = background.integral.Tx
  if (!is.null(background.integral.Tx)) {
    if (is(background.integral.Tx, "list")) {
      background.integral.Tx <-
        rep(background.integral.Tx, length = length(fileBIN.list))
    } else{
      background.integral.Tx <-
        rep(list(background.integral.Tx), length = length(fileBIN.list))
    }
  }
  #############################################################################

    ##SET fit.method
    if (fit.method == "EXP" |
        fit.method == "EXP+LIN" |
        fit.method == "LIN") {

    } else{
      stop("[analyse_baSAR()] Unsupported fit method. Supported: 'EXP', 'EXP+LIN' and 'LIN'")
    }


  #################################        DECLARE VARIABLES
  Dose <-  list()
  LxTx <-  list()
  sLxTx <-  list()

  Disc <-  list()
  Grain <- list()
  Disc_Grain.list <- list()

  Nb_aliquots <-  0
  previous.Nb_aliquots <- 0
  object.file_name <- list()

  Mono_grain <-  TRUE

  ##TODO
  Limited_cycles <- vector()

  ##set information
  for (i in 1 : length(fileBIN.list)) {
    Disc[[i]] <-  list()
    Grain[[i]] <-  list()

    ##get BIN-file name
    object.file_name[[i]] <- unique(fileBIN.list[[i]]@METADATA[["FNAME"]])

  }

  ##########################################################   READ Excel sheet

  if(is.null(XLS_file)){

    ##select aliquots giving light only, this function accepts also a list as input
    if(verbose){
      cat("\n[analyse_baSAR()] No XLS file provided, running automatic grain selection ...")

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


        ##remove grain position 0 (this are usually TL measurements on the cup or we are talking about multipe aliquot)
        warning(
          paste(
            "[analyse_baSAR()] Automatic grain selection:",
            sum(aliquot_selection$unique_pairs[["GRAIN"]] == 0, na.rm = TRUE),
            "curve(s) with grain index 0 had been removed from the dataset."
          ),
          call. = FALSE
        )

        datalu <-
          aliquot_selection$unique_pairs[!aliquot_selection$unique_pairs[["GRAIN"]] == 0,]

        if(nrow(datalu) == 0){

          warning("[analyse_baSAR()] Sorry, nothing was left after the automatic grain selection! NULL returned!", call. = FALSE)
          return(NULL)

        }

      }else{

          warning("[analyse_baSAR()] Only multiple grain data provided, automatic selection skipped!")
          datalu <- unique(fileBIN.list[[k]]@METADATA[, c("POSITION", "GRAIN")])

          ##set mono grain to FALSE
          Mono_grain <- FALSE
          aliquot_selection <- NA


      }

      ##get number of aliquots (one aliquot has a position and a grain number)
      Nb_aliquots <- nrow(datalu[, 1])

      ##write information in variables
      Disc[[k]] <-  datalu[["POSITION"]]
      Grain[[k]] <- datalu[["GRAIN"]]

      ##free memory
      rm(datalu, aliquot_selection)
    }
    rm(k)


  } else if (is(XLS_file, "data.frame") || is(XLS_file, "character")) {

    ##load file if we have an XLS file
    if (is(XLS_file, "character")) {
      ##test for valid file
      if(!file.exists(XLS_file)){
        stop("[analyse_baSAR()] defined XLS_file does not exists!")

      }

      ##import Excel sheet
      datalu <- readxl::read_excel(
        path = XLS_file,
        sheet = additional_arguments$sheet,
        col_names = additional_arguments$col_names,
        col_types = additional_arguments$col_types,
        skip = additional_arguments$skip
      )

      ##get rid of empty rows if the BIN_FILE name column is empty
      datalu <- datalu[!is.na(datalu[[1]]), ]


    } else{
      datalu <- XLS_file

    }

    ##limit aliquot range
    if (!is.null(aliquot_range)) {
      datalu <- datalu[aliquot_range,]

    }

    Nb_ali <-  0
    k <- NULL

    for (nn in 1:length((datalu[, 1]))) {
      if (!is.na(datalu[nn, 1]))  {

        ##check wether one file fits
        if (any(grepl(
          pattern = strsplit(
            x = basename(datalu[nn, 1]),
            split = ".",
            fixed = TRUE
          )[[1]][1],
          x = unlist(object.file_name)
        ))) {

          k <- grep(pattern = strsplit(
            x = basename(datalu[nn, 1]),
            split = ".",
            fixed = TRUE
          )[[1]][1],
          x = object.file_name)

          nj <-  length(Disc[[k]]) + 1
          Disc[[k]][nj] <-  as.numeric(datalu[nn, 2])
          Grain[[k]][nj] <-  as.numeric(datalu[nn, 3])
          Nb_ali <-  Nb_ali + 1
          if (is.na(Grain[[k]][nj]) || Grain[[k]][nj] == 0) {
            Mono_grain <- FALSE
          }

        }else{
          warning(
            paste0("[analyse_baSAR] '", (datalu[nn, 1]), "' not recognized or not loaded; skipped!"),
            call. = FALSE
          )
        }


      } else{

        if (Nb_ali == 0) {
          stop("[analyse_baSAR()] Nb. discs/grains  = 0 !")
        }

        break()
      }
    }

    ##if k is NULL it means it was not set so far, so there was
    ##no corresponding BIN file found
    if(is.null(k)){
      stop("[analyse_baSAR()] BIN-file names in XLS-file do not fit to the loaded BIN-files!")

    }

  } else{
    stop("[analyse_baSAR()] input type for 'XLS_file' not supported!")
  }


  ###################################### loops on files_number
  for (k in 1:length(fileBIN.list)) {

    Disc_Grain.list[[k]] <- list()   # data.file number
    n_aliquots_k <- length((Disc[[k]]))

    for (d in 1:n_aliquots_k) {
      dd <-  as.integer(unlist(Disc[[k]][d]))
      Disc_Grain.list[[k]][[dd]] <- list()  # data.file number ,  disc_number
    }

    for (d in 1:n_aliquots_k) {
      dd <-  as.integer(unlist(Disc[[k]][d]))
      if (Mono_grain == FALSE) {
        gg <- 1
      }
      if (Mono_grain == TRUE)  {
        gg <-  as.integer(unlist(Grain[[k]][d]))}

        Disc_Grain.list[[k]][[dd]][[gg]] <- list()  # data.file number ,  disc_number, grain_number
        for (z in 1:6) {
          Disc_Grain.list[[k]][[dd]][[gg]][[z]] <- list()
          # 1 = index numbers, 2 = irradiation doses,  3 = LxTx , 4 = sLxTx,  5 = N d'aliquot, 6 = De +- D0 +- (4 values)
        }
    }
  }


  if(verbose){
    cat("\n[analyse_baSAR()] Preliminary analysis in progress ... ")
    cat("\n[analyse_baSAR()] Hang on, this may take a long time ... \n")
  }


  for (k in 1:length(fileBIN.list)) {

    n_index.vector <- vector("numeric")
    logical_selection.vector <- vector("logical")

    measured_discs.vector <- vector("numeric")
    measured_grains.vector <- vector("numeric")
    measured_grains.vector_list <- vector("numeric")
    irrad_time.vector <- vector("numeric")

    disc_pos <- vector("numeric")
    grain_pos <- vector("numeric")

    ### META_DATA
    length_BIN <-  length(fileBIN.list[[k]])
    n_index.vector <- fileBIN.list[[k]]@METADATA[["ID"]][1:length_BIN]              #  curves indexes vector
    logical_selection.vector <- fileBIN.list[[k]]@METADATA[["SEL"]][1:length_BIN]    # TRUE / FALSE vector

    measured_discs.vector <-  fileBIN.list[[k]]@METADATA[["POSITION"]][1:length_BIN] # measured discs vector
    measured_grains.vector <- fileBIN.list[[k]]@METADATA[["GRAIN"]][1:length_BIN]    # measured grains vector
    irrad_time.vector <- fileBIN.list[[k]]@METADATA[["IRR_TIME"]][1:length_BIN]      # irradiation durations vector

    ##if all irradiation times are 0 we should stop here
    if(length(unique(irrad_time.vector)) == 1){
      warning("[analyse_baSAR()] It appears the the irradiation times are all the same. Analysis stopped an NULL returned!")
      return()
    }

    disc_pos <- as.integer(unlist(Disc[[k]]))
    grain_pos <- as.integer(unlist(Grain[[k]]))

    ### Automatic Filling - Disc_Grain.list

    for (i in 1: length(Disc[[k]])) {

      disc_selected <-  as.integer(Disc[[k]][i])
      if (Mono_grain == TRUE) {grain_selected <- as.integer(Grain[[k]][i])} else { grain_selected <-0}

          disc_logic <-   (disc_selected == measured_discs.vector)
          grain_logic <-  (grain_selected == measured_grains.vector)
          index_liste <- n_index.vector[disc_logic & grain_logic]
      if (Mono_grain == FALSE)  { grain_selected <-1}

          for (kn in 1: length(index_liste)) {

            if (logical_selection.vector[index_liste[kn]] == TRUE){
              t <-  index_liste[kn]

              ##check if the source_doserate is NULL or not
              if(!is.null(unlist(source_doserate))){
                dose.value <-  irrad_time.vector[t] * unlist(source_doserate[[k]][1])

              }else{
                dose.value <-  irrad_time.vector[t]

              }

              s <- 1 + length( Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[1]] )
              Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[1]][s] <- n_index.vector[t]  # indexes
              if ( s%%2 == 1) { Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[2]][as.integer(1+s/2)] <- dose.value  }      # irradiation doses
            }

          }
    }
  }


  ######################  Data associated with a single Disc/Grain

  max_cycles <-  0

  for (k in 1:length(fileBIN.list)) {

    if (Mono_grain == TRUE) (max.grains <- 100) else (max.grains <- 1)

    for (i in 1:length(Disc[[k]])) {

      disc_selected <-  as.integer(Disc[[k]][i])
      if (Mono_grain == TRUE) {
        grain_selected <- as.integer(Grain[[k]][i])
      } else {
        grain_selected <- 1
      }

      # Data for the selected Disc-Grain
      for (nb_index in 1:((length(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[1]]))/2 )) {

        index1 <- as.numeric(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[1]][2*nb_index-1])
        index2 <- as.numeric(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[1]][2*nb_index])
        Lx.data <- data.frame(seq(1:length( fileBIN.list[[k]]@DATA[[index1]])), fileBIN.list[[k]]@DATA[[index1]])
        Tx.data <- data.frame(seq(1:length( fileBIN.list[[k]]@DATA[[index2]])), fileBIN.list[[k]]@DATA[[index2]])

        # call calc_OSLLxTxRatio()
        temp_LxTx <- calc_OSLLxTxRatio(
          Lx.data = Lx.data,
          Tx.data = Tx.data,
          signal.integral = signal.integral[[k]],
          signal.integral.Tx = signal.integral.Tx[[k]],
          background.integral = background.integral[[k]],
          background.integral.Tx = background.integral.Tx[[k]],
          background.count.distribution = additional_arguments$background.count.distribution,
          sigmab = sigmab[[k]],
          sig0 = sig0[[k]]
        )


        ##get LxTx table
        LxTx.table <- temp_LxTx$LxTx.table

        Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[3]][nb_index] <- LxTx.table[[9]]
        Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[4]][nb_index] <- LxTx.table[[10]]
        Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[5]][nb_index] <- LxTx.table[[7]]

        ##free memory
        rm(LxTx.table)
      }

      # Fitting Growth curve and Plot
      sample_dose <-  unlist(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[2]])
      sample_LxTx <-  unlist(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[3]])
      sample_sLxTx <- unlist(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[4]])

      TnTx <- unlist(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[5]])

      ##create needed data.frame
      selected_sample <- data.frame (sample_dose, sample_LxTx, sample_sLxTx, TnTx)


      ##call plot_GrowthCurve() to get De and De value
      fitcurve <-
        suppressWarnings(plot_GrowthCurve(
          sample = selected_sample,
          na.rm = TRUE,
          fit.method = fit.method,
          fit.force_through_origin = fit.force_through_origin,
          fit.weights = additional_arguments$fit.weights,
          fit.includingRepeatedRegPoints = fit.includingRepeatedRegPoints,
          fit.bounds = additional_arguments$fit.bounds,
          NumberIterations.MC = additional_arguments$NumberIterations.MC,
          output.plot = additional_arguments$output.plot,
          output.plotExtended = additional_arguments$output.plotExtended,
          txtProgressBar = FALSE,
          verbose = verbose
        ))


      if(!is.null(fitcurve)){

        ##get data.frame with De values
        fitcurve_De <- get_RLum(fitcurve, data.object = "De")

        Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[6]][1] <-
          fitcurve_De[["De"]]
        Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[6]][2] <-
          fitcurve_De[["De.Error"]]
        Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[6]][3] <-
          fitcurve_De[["D01"]]
        Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[6]][4] <-
          fitcurve_De[["D01.ERROR"]]

        Limited_cycles[previous.Nb_aliquots + i] <-
          length(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[2]])

        if (length(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[2]]) > max_cycles) {
          max_cycles <-
            length(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[2]])
        }



      previous.Nb_aliquots <-
        length(Limited_cycles) # Total count of aliquots

      }

    }

  }    ##  END of loop on BIN files ################################################################

  Nb_aliquots <-  previous.Nb_aliquots

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

      disc_selected <-  as.numeric(Disc[[k]][i])

      if (Mono_grain == TRUE) {
        grain_selected <-
          as.numeric(Grain[[k]][i])
      } else {
        grain_selected <- 1
      }
      comptage <- comptage + 1

      OUTPUT_results[comptage, 1] <- k
      OUTPUT_results[comptage, 2] <- as.numeric(disc_selected)
      if (Mono_grain == TRUE) {
        OUTPUT_results[comptage, 3] <- grain_selected
      }
      else {
        OUTPUT_results[comptage, 3] <- 0
      }


     if (length(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[6]]) != 0) {

        ##DE
        OUTPUT_results[comptage, 4] <-
          as.numeric(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[6]][1])

        ##DE.SD
        OUTPUT_results[comptage, 5] <-
          as.numeric(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[6]][2])

        ##D0
        OUTPUT_results[comptage, 6] <-
          as.numeric(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[6]][3])

        ##D0.SD
        OUTPUT_results[comptage, 7] <-
          as.numeric(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[6]][4])

        ##CYCLES_NB
        OUTPUT_results[comptage, 8] <-
          length(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[2]])

          ##auxillary variable
          llong <-
            length(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[2]])

        ##Dose
        OUTPUT_results[comptage, 9:(8 + llong)] <-
          as.numeric(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[2]])

        ##LxTx values
        OUTPUT_results[comptage, (9 + max_cycles):(8 + max_cycles + llong)] <-
          as.numeric(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[3]])

        ##LxTx SD values
         OUTPUT_results[comptage, (9 + 2*max_cycles):(8 + 2*max_cycles + llong)] <-
          as.numeric(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[4]])

     }

    }
  }


  ##Clean matrix and remove all unwanted entries

    ##remove NA values in DE
    OUTPUT_results_reduced <- OUTPUT_results[!is.na(OUTPUT_results[,4]),]

    ##remove DE values <= 0
    OUTPUT_results_reduced <- OUTPUT_results_reduced[OUTPUT_results_reduced[,4] > 0,]

    ##clean up NaN values in the LxTx and corresponding error values
    ##the transposition of the matrix may increase the performance for very large matricies
    OUTPUT_results_reduced <- t(OUTPUT_results_reduced)
    selection <- vapply(X = 1:ncol(OUTPUT_results_reduced), FUN = function(x){
        !any(is.nan(OUTPUT_results_reduced[9:(8+2*max_cycles), x]))

    }, FUN.VALUE = vector(mode = "logical", length = 1))

    OUTPUT_results_reduced <- t(OUTPUT_results_reduced[,selection])

    ##finally, check for difference in the number of dose points ... they should be the same
    if(unique(OUTPUT_results_reduced[,"CYCLES_NB"])>1){
       warning("[analyse_baSAR()] the number of dose points differs across your data set. Check your data!")

    }

  ##correct number of aliquots if necessary
  if(Nb_aliquots > nrow(OUTPUT_results_reduced)) {
    Nb_aliquots <- nrow(OUTPUT_results_reduced)
    warning(paste("[analyse_baSAR()] 'Nb_aliquots' corrected to ", Nb_aliquots), call. = FALSE)

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

  }

  ##CALL internal baSAR function
  results <-
    .baSAR_function(
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
      plot = plot,
      verbose = verbose
    )

  ##add error from the source_doserate
  if(!is.null(unlist(source_doserate))){
    DE_FINAL.ERROR <- sqrt(results[[1]][["CENTRAL.SD"]]^2 + sum(unlist(
      source_doserate[1:length(source_doserate)][2])^2))

  }else{
    DE_FINAL.ERROR <- NA

  }

  ##consider the case that we get NA and this might be confusing
  if(is.na(DE_FINAL.ERROR)){
    DE_FINAL.ERROR <- results[[1]][["CENTRAL.SD"]]

  }

  ##combine
  results[[1]] <- cbind(results[[1]], DE_FINAL = results[[1]][["CENTRAL"]], DE_FINAL.ERROR = DE_FINAL.ERROR)

  if(verbose){
    cat("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n")
    cat("\n[analyse_baSAR()] - RESULTS\n")
    cat("---------------------------------------------------------------\n")
    cat(paste0("Used distribution:\t\t\t", results[[1]][["DISTRIBUTION"]],"\n"))
    cat(paste0("Number of aliquots used:\t\t", results[[1]][["NB_ALIQUOTS"]],"\n"))
    cat(paste0("Considered fitting method:\t\t", results[[1]][["FIT_METHOD"]],"\n"))
    cat(paste0("Number MCMC iterations:\t\t\t", results[[1]][["N.MCMC"]],"\n"))

    cat("---------------------------------------------------------------\n")
    cat(paste0(">> Central dose:\t\t\t", results[[1]][["CENTRAL"]]," \u00b1 ", results[[1]][["CENTRAL.SD"]]))
    cat(paste0("\n>> Overdispersion (sigma):\t\t", results[[1]][["SIGMA"]]," \u00b1 ", results[[1]][["SIGMA.SD"]]))
    cat(paste0("\n>> Final central De:\t\t\t", results[[1]][["DE_FINAL"]]," \u00b1 ", round(results[[1]][["DE_FINAL.ERROR"]], digits = 2)))
    cat("\n---------------------------------------------------------------\n")
    cat(
      paste("(systematic error contribution to final De:",
            format((1-results[[1]][["CENTRAL.SD"]]/results[[1]][["DE_FINAL.ERROR"]])*100, scientific = TRUE), "%)\n")
      )
  }

  # Return --------------------------------------------------------------------------------------
  return(set_RLum(
    class = "RLum.Results",
    data = list(
      summary = results[[1]],
      mcmc = results[[2]],
      models = results[[3]],
      input_object = input_object),
    info = list(call = sys.call())
  ))

}

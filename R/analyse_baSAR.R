#' Bayesian models (baSAR) applied on luminescence data
#'
#' This function allows the application of Bayesian models (baSAR) on luminescecence data
#'
#' @param object \code{\linkS4class{Risoe.BINfileData}} or \code{{character}} or \code{\link{list}} (\bold{required}): input object used for the Bayesian analysis. If a \code{character} is provided the function
#' assumes a file connection and tries to import a BIN-file using the provided path. If a \code{list} is
#' provided the list can only contain either \code{Risoe.BINfileData} objects or \code{character}s
#' providing a file connection. Mixing of both types is not allowed.
#'
#' @param source_doserate \code{\link{numeric}} (with default): source dose rate of beta-source used
#' for the measuremnt in Gy/s
#'
#' @inheritParams calc_OSLLxTxRatio
#'
#' @param distribution \code{\link{character}} (with default): type of distribution that is used for
#' the Bayesian calculation. Allowed inputs are  \code{cauchy},\code{normal} and \code{log_normal}
#'
#' @param fit.method \code{\link{character}} (with default): fit method used for fitting the growth
#' curve using the function \code{\link{plot_GrowthCurve}}. Here supported methods: \code{EXP},
#' \code{EXP+LIN} and \code{LIN}
#'
#' @param fit.force_through_origin \code{\link{character}} (with default): force fitting through origin
#'
#' @param fit.includingRecyclingPoints \code{\link{character}} (with default): includes the recycling point (assuming measured during last cycle)
#'
#' @param XLS_file \code{\link{character}} (with default): XLS_file with data for the analysis
#'
#' @param plot \code{\link{logical}} (with default): enables or disables plot output
#'
#' @param plot_reduced \code{\link{logical}} (with default): enables or disables the advanced plot output
#'
#' @param verbose \code{\link{logical}} (with default): enables or disables verbose mode
#'
#' @param ... parameters that can be passed to the function
#' \code{\link[readxl]{read_excel}} (all arguments are supported), \code{\link{read_BIN2R}} (\code{n.records},
#' \code{position}, \code{duplicated.rm})
#'
#' @section Function version: 0.1.0
#'
#' @authors Norbert Mercier, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), Sebastian Kreutzer,
#' IRAMAT-CRP2A, Universite Bordeaux Montaigne (France) \cr
#'
#' The underlying Bayesian model based on a contribution by Combes et al., 2015.
#'
#' @seealso \code{\link{read_BIN2R}}, \code{\link{plot_GrowthCurve}},
#' \code{\link[rjags]{jags.model}}, \code{\link[rjags]{coda.samples}}
#'
#' @references
#'
#' Combes, B., Philippe, A., Lanos, P., Mercier, N., Tribolo, C., Guerin, G., Guibert, P., Lahaye, C., 2015.
#' A Bayesian central equivalent dose model for optically stimulated luminescence dating.
#' Quaternary Geochronology 28, 62-70. doi:10.1016/j.quageo.2015.04.001
#'
#' @note The current version of the function works with standard Risoe BIN-files only!
#'
#' @keywords datagen
#'
#'
#' @examples
#'
#' #nothing so far
#'
#' @export
analyse_baSAR <- function(
  object,
  source_doserate = 1,
  Lx.data,
  Tx.data,
  signal.integral,
  signal.integral.Tx = NULL,
  background.integral,
  background.integral.Tx = NULL,
  background.count.distribution = "non-poisson",
  sigmab = 0,
  sig0 = 0.025,
  digits = NULL,
  distribution = "cauchy",
  fit.method = "EXP",
  fit.force_through_origin = TRUE,
  fit.includingRecyclingPoints = TRUE,
  XLS_file = NULL,
  plot = TRUE,
  plot_reduced = TRUE,
  verbose = TRUE,
  ...
){


  ##TODO :
  ##
  ## - argument description should be changed, as the copy and past from calc_OSLLxTxRatio()
  ## makes not really sense ...
  ## - add ... support for more functions used to get the user the maximum of control
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
             fit.force_through_origin,
             fit.includingRecyclingPoints,
             plot,
             verbose)
    {

      Limited_cycles <- vector()

      if (fit.method == "EXP") {ExpoGC <- 1 ; LinGC <-  0 }
      if (fit.method == "LIN") {ExpoGC <- 0 ; LinGC <-  1 }
      if (fit.method == "EXP+LIN") {ExpoGC <- 1 ; LinGC <-  1 }
      if (fit.force_through_origin == TRUE) {GC_Origin <- 1} else {GC_Origin <- 0}

      if (fit.includingRecyclingPoints == TRUE) {
        for (i in 1:Nb_aliquots) {
          Limited_cycles[i] <- length(na.exclude(data.Dose[,i]))}
      }
      else {
        for (i in 1:Nb_aliquots) {
        Limited_cycles[i] <- length(na.exclude(data.Dose[,i])) - 1}
      }
      low_De <-  0.01 ; up_De <-  500   # Gy

      ########################################        MODELS      ########################################

      #   Cauchy distribution
      baSARc_model.bug <- "model {

            central_D ~  dunif(low_De,up_De)

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

            central_D ~  dunif(low_De,up_De)

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

            central_D ~  dunif(low_De,up_De)

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

      ########################################     END   MODELS      ########################################

      ### Bayesian inputs
      data_Liste  <- list(
        'Dose' = data.Dose,
        'Lum' = data.Lum,
        'sLum' = data.sLum,
        'LinGC' = LinGC,
        'ExpoGC' = ExpoGC,
        'GC_Origin' = GC_Origin,
        'Limited_cycles' = Limited_cycles,
        'low_De' = low_De,
        'up_De' = up_De,
        'Nb_aliquots' = Nb_aliquots
      )

      if(verbose){
        cat("[analyse_baSAR()] Bayesian analysis in progress ... ")
      }

      Nb_Iterations <-  1000

      if (distribution == "cauchy") {

        if(verbose){cat("\n[analyse_baSAR()] Calculation assuming a Cauchy distribution:\n")}
        distribution <-  "Cauchy distribution"
        jagsfit <- rjags::jags.model(
          textConnection(baSARc_model.bug),
          data = data_Liste,
          n.chains = 3,
          n.adapt = Nb_Iterations,
          quiet = if(verbose){FALSE}else{TRUE}
         )

      }else if (distribution == "normal") {
        if(verbose){cat("\n[analyse_baSAR()] Calculation assuming a Normal distribution:\n")}
        distribution <-  "Normal distribution"
        jagsfit <- rjags::jags.model(
          textConnection(baSARn_model.bug),
          data = data_Liste,
          n.chains = 3,
          n.adapt= Nb_Iterations,
          quiet = if(verbose){FALSE}else{TRUE}
          )

      }else if (distribution == "log_normal") {
        if(verbose){cat("\n[analyse_baSAR()] Calculation assuming a Log-Normal distribution:\n")}
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
      if ( nb_decal != length(summary(sampling)[[1]])/4 ) {
        histo_points <-  summary(sampling)[[1]] [1:((length(summary(sampling)[[1]])/4)-nb_decal), 1]

        plot_ViolinPlot(
          histo_points,
          main = paste("Distribution sample :" , XLS_file),
          xlab = "Dose [Gy]")
        pt_zero <- Nb_aliquots
      }
      output.mean <- vector("numeric")

      output.mean[1] <-  round(summary(sampling)[[1]][(pt_zero+1)], 2)
      output.mean[2] <- round(summary(sampling)[[1]][(2*pt_zero+3)], 2)
      output.mean[3] <-  round(summary(sampling)[[1]][(pt_zero+2)], 2)
      output.mean[4] <- round(summary(sampling)[[1]][(2*pt_zero+4)], 2)

      #### output data.frame with results
      baSAR.output <- data.frame(
        DISTRIBUTION = distribution,
        NB_ALIQUOTS = Nb_aliquots,
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

    ##readxl::read_excel()
    sheet = 1,
    col_names = TRUE,
    col_types = NULL,
    na = "",
    skip = 0,

    ##read_BIN2R()
    n.records = NULL,
    duplicated.rm = TRUE,
    position = NULL


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

     ##check whether this makes sense at all TODO
     ##set variables
     ##Why is.null() ... it prevents that the function crashed is nothing is provided ...
     Nb_aliquots <- nrow(object$input_object) #TODO here we need more flexibility

     ##set changeable function arguments

       ##distribution
       if(!is.null(function_arguments.new$distribution)){
         distribution <- function_arguments.new$distribution
       }

       ##fit.method
       if(!is.null(function_arguments.new$fit.method)){
         fit.method <- function_arguments.new$fit.method
       }

       ## fit.force_through_origin
       if(!is.null(function_arguments.new$fit.force_through_origin)){
          fit.force_through_origin <- function_arguments.new$fit.force_through_origin
       }

       ##fit.includingRecyclingPoints
       if(!is.null(function_arguments.new$fit.includingRecyclingPoints)){
          fit.includingRecyclingPoints <- function_arguments.new$fit.includingRecyclingPoints
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

  ##################################### Extending parameters to lists ...

  ##test_parameter = source_doserate
  if(is(source_doserate[[1]], "list")){
    source_doserate <- rep(source_doserate, length = length(fileBIN.list))
  }else{
    source_doserate <- rep(list(source_doserate), length = length(fileBIN.list))
  }

  ##test_parameter = signal.integral
  if(is(signal.integral[[1]], "list")){
    signal.integral <- rep(signal.integral, length = length(fileBIN.list))
  }else{
    signal.integral <- rep(list(signal.integral), length = length(fileBIN.list))
  }


  ##test_parameter = signal.integral.Tx
  if (!is.null(signal.integral.Tx)) {
    if (is(signal.integral.Tx[[1]], "list")) {
      signal.integral.Tx <- rep(signal.integral.Tx, length = length(fileBIN.list))
    } else{
      signal.integral.Tx <- rep(list(signal.integral.Tx), length = length(fileBIN.list))
    }
  }

  ##test_parameter = background.integral
  if(is(background.integral[[1]], "list")){
    background.integral <- rep(background.integral, length = length(fileBIN.list))
  }else{
    background.integral <- rep(list(background.integral), length = length(fileBIN.list))
  }



  ##test_parameter = background.integral.Tx
  if (!is.null(background.integral.Tx)) {
    if (is(background.integral.Tx[[1]], "list")) {
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
      aliquot_selection <-
        verify_SingleGrainData(
          object = fileBIN.list[[k]],
          cleanup_level = "aliquot",
          threshold = 30,
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

      ##get number of aliquots (one aliquot has a position and a grain number)
      Nb_aliquots <- nrow(datalu[, 1])

      ##write information in variables
      Disc[[k]] <-  datalu[["POSITION"]]
      Grain[[k]] <- datalu[["GRAIN"]]

      ##free memory
      rm(datalu, aliquot_selection)
    }
    rm(k)


  }else if(is(XLS_file, "data.frame")){

    Nb_ali <-  0
    datalu <- XLS_file

    for (nn in 1:length((datalu[,1]))) {
      if ( is.na(datalu[nn,1]) == FALSE)  {

        if ( length(intersect ((datalu[nn,1]), object)) == 1) {

          k <- which(datalu[nn,1] == object)
          nj <-  length(Disc[[k]]) + 1
          Disc[[k]][nj] <-  as.numeric(datalu[nn,2])
          Grain[[k]][nj] <-  as.numeric(datalu[nn,3])
          Nb_ali <-  Nb_ali + 1
          if (is.na(Grain[[k]][nj]) == TRUE) {Mono_grain =  FALSE}

        }else{
          stop(paste0(" '",(datalu[nn,1]), "' not known or not loaded."))
        }

      }else{
        break
        if (Nb_ali == 0) {  stop("[analyse_baSAR()] Nb. discs/grains  = 0 !") }
      }
    }



  }else if(is(XLS_file, "character")){

    Nb_ali <-  0

    datalu <- readxl::read_excel(
      path = XLS_file,
      sheet = additional_arguments$sheet,
      col_names = additional_arguments$col_names,
      col_types = additional_arguments$col_types,
      na = additional_arguments$na,
      skip = additional_arguments$skip
    )

    for (nn in 1:length((datalu[,1]))) {

      if (!is.na(datalu[nn,1]))  {

        if (any(grepl(
              pattern = strsplit(x = basename(datalu[nn, 1]),split = ".", fixed = TRUE)[[1]][1],
              x = unlist(object.file_name)))) {

          k <- grep(
            pattern = strsplit(x = basename(datalu[nn, 1]), split = ".", fixed = TRUE)[[1]][1],
            x = object.file_name)

          nj <-  length(Disc[[k]]) + 1
          Disc[[k]][nj] <-  as.numeric(datalu[nn, 2])
          Grain[[k]][nj] <-  as.numeric(datalu[nn, 3])
          Nb_ali <-  Nb_ali + 1
          if (is.na(Grain[[k]][nj])) {
            Mono_grain <- FALSE
          }

        } else{
          warning(paste0("[analyse_baSAR] '", (datalu[nn, 1]), "' not recognized or not loaded; skipped!"),
                  call. = FALSE)
        }

      }else{
             break
             if (Nb_ali == 0) {  stop("[analyse_baSAR()] Nb. discs/grains  = 0 !") }
      }
    }

    }else{
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
      if (Mono_grain == FALSE) {gg <-  1}
      if (Mono_grain == TRUE)  {gg <-  as.integer(unlist(Grain[[k]][d]))}
        Disc_Grain.list[[k]][[dd]][[gg]] <- list()  # data.file number ,  disc_number, grain_number
        for (z in 1:6) {
          Disc_Grain.list[[k]][[dd]][[gg]][[z]] <- list()
          # 1 = index numbers, 2 = irradiation doses,  3 = LxTx , 4 = sLxTx,  5 = N d'aliquot, 6 = De +- D0 +- (4 values)
        }
    }
  }

  if(verbose){
    cat("\n[analyse_baSAR()] Preliminary analysis in progress ... ")
    cat("\n[analyse_baSAR()] Hang on, this may take a long time ... \n\n")
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
    n_index.vector <- fileBIN.list[[k]]@METADATA[[1]][1:length_BIN]             #  curves indexes vector
    logical_selection.vector <- fileBIN.list[[k]]@METADATA[[2]][1:length_BIN]   # TRUE / FALSE vector

    measured_discs.vector <-  fileBIN.list[[k]]@METADATA[[9]][1:length_BIN]     # measured discs vector
    measured_grains.vector <- fileBIN.list[[k]]@METADATA[[10]][1:length_BIN]    # measured grains vector
    irrad_time.vector <- fileBIN.list[[k]]@METADATA[[45]][1:length_BIN]         # irradiation durations vector

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
              dose.value <-  irrad_time.vector[t] * unlist(source_doserate[[k]])
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
          background.count.distribution = "non-poisson",
          sigmab = sigmab,
          sig0 = sig0,
          digits = digits
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


      ##TODO support ... for this function
      fitcurve <-
        plot_GrowthCurve(
          selected_sample,
          na.rm = TRUE,
          fit.method = "LIN", #"EXP OR LIN",
      #    fit.force_through_origin = TRUE,
          fit.weights = TRUE,
      #    fit.includingRepeatedRegPoints = FALSE,
          fit.bounds = TRUE,
          NumberIterations.MC = 10,
          output.plot = FALSE,
          output.plotExtended = TRUE,
          output.plotExtended.single = FALSE,
          cex.global = 1,
          txtProgressBar = FALSE,
          verbose = FALSE
        )


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


  ##ASK NORBERT _ TODO
  # ### Prepare Matrix data for baSAR function
  # Limited_cycles <- as.integer(na.omit(Limited_cycles))
  # max_cycle <-  max(Limited_cycles)
  #
  # Doses <- matrix(NA, nrow = max_cycle, ncol = Nb_aliquots)
  # LxTx <- matrix(NA, nrow = max_cycle, ncol = Nb_aliquots)
  # LxTx.error <- matrix(NA, nrow = max_cycle, ncol = Nb_aliquots)
  #
  # p <-  0
  #
  # tempo_1 <-   vector("numeric")
  # for (k in 1:length(fileBIN.list)) {
  #   for (i in 1:length(Disc[[k]])) {
  #     p  <-  p + 1
  #     d <- as.numeric(Disc[[k]][i])
  #     if (Mono_grain == TRUE)
  #       (g <- as.numeric(Grain[[k]][i]))
  #     else
  #       (g <- 1)
  #     tempo_1 <-  as.numeric(Disc_Grain.list[[k]][[d]][[g]][[2]])
  #
  #     for (h in 1:length(tempo_1)) {
  #       Doses[h, p] <- as.numeric(Disc_Grain.list[[k]][[d]][[g]][[2]][h])
  #       LxTx[h, p] <-
  #         as.numeric(Disc_Grain.list[[k]][[d]][[g]][[3]][h])
  #       LxTx.error[h, p] <-
  #         as.numeric(Disc_Grain.list[[k]][[d]][[g]][[4]][h])
  #     }
  #   }
  # }


  # Doses <-  apply(Doses, MARGIN = 2, FUN = "as.single")
  # LxTx <-  apply(LxTx, MARGIN = 2, FUN = "as.single")
  # LxTx.error <-  apply(LxTx.error, MARGIN = 2, FUN = "as.single")

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
      fit.force_through_origin = fit.force_through_origin,
      fit.includingRecyclingPoints = fit.includingRecyclingPoints,
      plot = plot,
      verbose = verbose
    )

  if(verbose){
    cat("\n[analyse_baSAR()] - RESULTS\n")
    cat("---------------------------------------------------------------\n")
    cat(paste0("Used distribution:\t\t\t", results[[1]][["DISTRIBUTION"]],"\n"))
    cat(paste0("Number of aliquots used:\t\t", results[[1]][["NB_ALIQUOTS"]],"\n"))
    cat(paste0("Considered fitting method:\t\t", results[[1]][["FIT_METHOD"]],"\n"))
    cat("---------------------------------------------------------------\n")
    cat(paste0(">> Central dose:\t\t\t", results[[1]][["CENTRAL"]]," \u00b1 ", results[[1]][["SIGMA"]]))
    cat(paste0("\n   .. SD central dose:\t\t\t", results[[1]][["CENTRAL.SD"]]))
    cat(paste0("\n   .. SD sigma:\t\t\t\t", results[[1]][["SIGMA.SD"]]))
    cat("\n---------------------------------------------------------------\n")
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

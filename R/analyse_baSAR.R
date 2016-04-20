#' Bayesian models (baSAR) applied on luminescence data
#'
#' This function allows the application of Bayesian models (baSAR) on luminescecence data
#'
#' @param object \code{\link{character}} (\bold{required}): BIN-file, either a single file or a list
#' of file names
#'
#' @param source_doserate \code{\link{numeric}} (with default): source dose rate of beta-source used
#' for the measuremnt in Gy/s
#'
#' @inheritParams calc_OSLLxTxRatio
#'
#' @param distribution \code{\link{character}} (with default): type of distribution that is used for
#' the Bayesian calculation. Allowed inputs are \code{normal}, \code{cauchy} and \code{log_normal}
#'
#' @param fit.method \code{\link{character}} (with default): fit method used for fitting the growth
#' curve using the function \code{\link{plot_GrowthCurve}}. Here supported methods: \code{EXP},
#' \code{EXP+LIN} and \code{LIN}
#'
#' @param XLS_file \code{\link{character}} (with default): XLS_file with data for the analysis
#'
#' @param plot \code{\link{logical}} (with default): enables or disables plot output
#'
#' @param plot_reduced \code{\link{logical}} (with default): enables or disables the advanced plot output
#'
#' @param verbose \code{\link{logical}} (with default): enables or disables verbose mode
#'
#' @section Function version: 0.1.0
#'
#' @author Norbert Mercier, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), Sebastian Kreutzer,
#' IRAMAT-CRP2A, Universite Bordeaux Montaigne (France) \cr
#'
#' The underlying Bayesian model based on a contribution by Combes et al., 2015.
#'
#' @seealso \code{\link{read_BIN2R}}, \code{\link[rjags]{jags.model}}, \code{\link[rjags]{coda.samples}}
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
  sigmab,
  sig0 = 0,
  digits = NULL,
  distribution = "normal",
  fit.method = "EXP",
  XLS_file = NULL,
  plot = TRUE,
  plot_reduced = TRUE,
  verbose = TRUE
){

  ##TODO
  ## - Check for input data, plain R should be possible
  ## - add running example ... a short one
  ## - implement a selfcall (RLum.Results) along with the XLS sheet
  ## - there is something missing in the code for calculating the De ... ask Norbert


  ##FUNCTION TO BE CALLED to RUN the Bayesian Model
  ###############  START : baSAR_function
  baSAR_function <-
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


      if (fit.method == "e") {ExpoGC <- 1 ; LinGC <-  0 ; fit.method <-  "EXP"}
      if (fit.method == "l") {ExpoGC <- 0 ; LinGC <-  1 ; fit.method <-  "LIN"}
      if (fit.method == "el") {ExpoGC <- 1 ; LinGC <-  1 ; fit.method <-  "EXP+LIN"}
      if (fit.force_through_origin == TRUE) {GC_Origin <- 1} else {GC_Origin <- 0}

      if (fit.includingRecyclingPoints == TRUE) {
        for (i in 1:Nb_aliquots) {
          Limited_cycles[i] <- length(na.exclude(data.Dose[,i]))}
      }
      else
      {for (i in 1:Nb_aliquots) {
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

            D[i] ~ dt ( central_D , precision_D, 1)    #     loi Cauchy

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

            D[i] ~ dnorm ( central_D , 1/(sigma_D^2) )   #          loi Normale

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

            log_D[i] ~ dnorm ( log_central_D , 1/(l_sigma_D^2) )  #          loi log-normale
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
        'low_De' = low_De,
        'up_De' = up_De,
        'Nb_aliquots' = Nb_aliquots
      )


      if(verbose){
        cat("\n[analyse_baSAR()] hang on, this may take a long time ... \n\n")
        cat(" $$$$$$$$$$$$         Bayesian Analysis in progress ...         $$$$$$$$$$$\n")
      }

      Nb_Iterations <-  100000

      if (distribution == "cauchy") {

        if(verbose){cat("\n>> Calculation assuming a Cauchy distribution:\n")}
        distribution <-  "Cauchy distribution"
        jagsfit <- rjags::jags.model(
          textConnection(baSARc_model.bug),
          data = data_Liste,
          n.chains = 3,
          n.adapt = Nb_Iterations,
          quiet = if(verbose){FALSE}else{TRUE}
         )

      }else if (distribution == "normal") {
        if(verbose){cat("\n>> Calculation assuming a Normal distribution:\n")}
        distribution <-  "Normal distribution"
        jagsfit <- rjags::jags.model(
          textConnection(baSARn_model.bug),
          data = data_Liste,
          n.chains = 3,
          n.adapt= Nb_Iterations,
          quiet = if(verbose){FALSE}else{TRUE}
          )

      }else if (distribution == "log_normal") {
        if(verbose){cat("\n>> Calculation assuming a Log-Normal distribution:\n")}
        distribution <-  "Log-Normal distribution"
        jagsfit <- rjags::jags.model(
          textConnection(baSARl_model.bug),
          data = data_Liste,
          n.chains = 3,
          n.adapt = Nb_Iterations,
          quiet = if(verbose){FALSE}else{TRUE}
        )
      }else{
        stop("[analyse_baSAR()] unknown input for 'distribution'. Allowed are: 'cauchy', 'normal' or 'log-normal'")


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

      pt_zero <- 0
      nb_decal <-  2
      if ( nb_decal != length(summary(sampling)[[1]])/4 ) {
        histo_points <-  summary(sampling)[[1]] [1:((length(summary(sampling)[[1]])/4)-nb_decal), 1]
        plot_ViolinPlot(
          histo_points,
          main = paste("Distribution sample :" , fichier),
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
  ###############  End : baSAR_function
  ####++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####


  # Integrity tests -----------------------------------------------------------------------------

  ##check whether rjags is available
  ##code snippet taken from
  ##http://r-pkgs.had.co.nz/description.html
  if (!requireNamespace("rjags", quietly = TRUE)) {
    stop("[analyse_baSAR()] To use this function you have to first install the package 'rjags'.",
         call. = FALSE)
  }


  # Set input -----------------------------------------------------------------------------------

  if(is(object, "character")){
    fileBIN.list <- list(object)

  }else if(is(object, "list")){
    fileBIN.list <- object


  }else{
    stop("[analyse_baSAR()] input for 'object' not supported!")

  }

  ##SET fit.method
  if(fit.method == "EXP"){
    fit <- "e"

  }else if(fit.method == "LIN"){
    fit <- "l"

  }else if(fit.method == "EXP+LIN"){
    fit <- "EXP+LIN"

  }else{
    stop(
      "[analyse_baSAR()] Unsupported fit method. Supported: 'EXP', 'EXP+LIN' and 'LIN'"
    )

  }

  ##SET xls file
  fichier <- XLS_file

  ##TODO: data should be provided also without(!) XLS sheet ...

  #################################        DECLARE
  Dose <-  list()
  LxTx <-  list()
  sLxTx <-  list()

  Disc <-  list()
  Grain <- list()
  Disc_Grain.list <- list()

  Limited_cycles <- vector()

  Nb_aliquots <-  0 ; previous.Nb_aliquots <- 0 ;


  for (i in 1 : length(fileBIN.list)) {
    Disc[[i]] <-  list()
    Grain[[i]] <-  list()
  }

  ### Read BIN file
  fileBIN.list <- read_BIN2R(object,
                             duplicated.rm = TRUE,
                             verbose = verbose)


    ##we need a list, so take care that we get one
    if(!is(fileBIN.list, "list")){
      fileBIN.list <- list(fileBIN.list)

    }


  ###################################### loop on files_number
  for (k in 1:length(fileBIN.list)) {

    ##set number of aliquots
    n_aliquots <- length(unique(fileBIN.list[[k]]@METADATA[["POSITION"]]))

    Disc_Grain.list[[k]] <- list()   # data.file number
    for (d in 1:n_aliquots) {

      ##set number of grains for position d
      n_grains <- length(
        unique(fileBIN.list[[k]]@METADATA[fileBIN.list[[k]]@METADATA[["POSITION"]] == d, "GRAIN"])
        )
      Disc_Grain.list[[k]][[d]] <- list()  # data.file number , disc_number

      for (g in 1:n_grains) {
        Disc_Grain.list[[k]][[d]][[g]] <- list()  # data.file number ,  disc_number, grain_number
        for (z in 1:5) {
          Disc_Grain.list[[k]][[d]][[g]][[z]] <- list()  # 1 = index numbers, 2 = irradiation doses,  3 = LxTx , 4 = sLxTx,  5 = N d'aliquot
        }
      }
    }

    Mono_grain <- as.logical
    n_index.vector <- vector("numeric")
    logical_selection.vector <- vector("logical")
    nb_points.vector <- vector("numeric")
    measured_discs.vector <- vector("numeric")
    measured_grains.vector <- vector("numeric")
    measured_grains.vector_list <- vector("numeric")
    irrad_time.vector <- vector("numeric")

    ### Parameters for Lx/Tx calculations
    sigma_b <- 0 ; sig_0<-0.025    # by default

    datalu <- readxl::read_excel(fichier,sheet = 1)

    Nb_aliquots <- length(datalu[,1])

    Disc[[k]] <-  datalu[,1]
    Grain[[k]] <- datalu[,2]
    rm(datalu)

    ### META_DATA
    length_BIN <-  length(fileBIN.list[[k]])
    n_index.vector <- fileBIN.list[[k]]@METADATA[[1]][1:length_BIN]             #  curves indexes vector
    logical_selection.vector <- fileBIN.list[[k]]@METADATA[[2]][1:length_BIN]   # TRUE / FALSE vector                                              #   selection a revoir

    nb_points.vector  <- fileBIN.list[[k]]@METADATA[[6]][1:length_BIN]          # recorded points vector
    measured_discs.vector <-  fileBIN.list[[k]]@METADATA[[9]][1:length_BIN]     # measured discs vector
    measured_grains.vector <- fileBIN.list[[k]]@METADATA[[10]][1:length_BIN]    # measured grains vector
    irrad_time.vector <- fileBIN.list[[k]]@METADATA[[45]][1:length_BIN]         # irradiation durations vector

    ### Test if Single-Grain  file
    Mono_grain <- TRUE    # by default
    grains_numbers <-  1:100 #TODO, check whether this might become problematic
    measured_grains.vector_list <-  intersect(grains_numbers,measured_grains.vector)   # vector with list of measured grains
    if ( length(measured_grains.vector_list) < 10) {Mono_grain <- FALSE}

    ### Automatic Filling - Disc_Grain.list
    for (t in 1:length(n_index.vector)) {
      if (logical_selection.vector[t] == TRUE) {
        disc.value <- measured_discs.vector[t]
        if (Mono_grain == FALSE) {grain.value <-  1}
        if (Mono_grain == TRUE)  {grain.value <-  measured_grains.vector[t]}
        dose.value <-  irrad_time.vector[t] * source_doserate
        s <- 1 + length( Disc_Grain.list[[k]][[disc.value]][[grain.value]][[1]] )
        Disc_Grain.list[[k]][[disc.value]][[grain.value]][[1]][s] <- n_index.vector[t]  # indexes
        if ( s%%2 == 1) { Disc_Grain.list[[k]][[disc.value]][[grain.value]][[2]][as.integer(1+s/2)] <- dose.value  }      # irradiation doses
      }
    }

    if(verbose){cat("\n[analyse_baSAR()] Preliminary analysis in progress ...")}

    ######################  Data associated with a single Disc/Grain

    if (Mono_grain == TRUE) (max.grains <- 100) else (max.grains <- 1)

    for (i in 1: length(Disc[[k]])) {

      disc_selected <-  as.integer(Disc[[k]][i]) ; grain_selected <- as.integer(Grain[[k]][i])

      # Data for the selected Disc-Grain
      s <-  0
      for (nb_index in 1:(length(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[1]])-1)) {
        s <-  as.integer(1+nb_index/2)

        # Calculation of Lx/Tx ratios
        index1 <- as.integer(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[1]][nb_index])
        index2 <- as.integer(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[1]][nb_index+1])
        Lx.data <- data.frame(seq(1:nb_points.vector[index1]), fileBIN.list[[k]]@DATA[[index1]])
        Tx.data <- data.frame(seq(1:nb_points.vector[index2]), fileBIN.list[[k]]@DATA[[index2]])

        # call calc_OSLLxTxRatio()
        temp_LxTx <- calc_OSLLxTxRatio(
          Lx.data = Lx.data,
          Tx.data = Tx.data,
          signal.integral = signal.integral,
          signal.integral.Tx = signal.integral.Tx,
          background.integral = background.integral,
          background.integral.Tx = background.integral.Tx,
          background.count.distribution = "non-poisson",
          sigmab = sigma_b,
          sig0 = sig_0,
          digits = digits
        )

        Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[3]][s] <- temp_LxTx$LxTx.table[9]
        Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[4]][s] <- temp_LxTx$LxTx.table[10]
        Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[5]] <- i                           }

      Limited_cycles[previous.Nb_aliquots + i]<- length(Disc_Grain.list[[k]][[disc_selected]][[grain_selected]][[2]])
    }

    previous.Nb_aliquots <-  previous.Nb_aliquots + Nb_aliquots # Total count of aliquots

  }    ##  END of loop on files

  Nb_aliquots <-  previous.Nb_aliquots

  ### Prepare Matrix data for baSAR function

  Limited_cycles <- as.integer(Limited_cycles)
  max_cycle <-  max(Limited_cycles)

  Doses <- matrix(NA, nrow=max_cycle, ncol=Nb_aliquots)
  LxTx <- matrix(NA, nrow=max_cycle, ncol=Nb_aliquots)
  LxTx.error <- matrix(NA, nrow=max_cycle, ncol=Nb_aliquots)

  p <-  0

  tempo_1 <-   vector("numeric"); tempo_2 <- vector("numeric"); tempo_3 <- vector("numeric")
  for (k in 1 : length(fileBIN.list)) {
    for (i in 1:length(Disc[[k]])) {
      p  <-  p + 1;  d <- as.numeric(Disc[[k]][i]);  g <- as.numeric(Grain[[k]][i])
      tempo_1 <-  as.numeric(Disc_Grain.list[[k]][[d]][[g]][[2]])
      tempo_2 <-  as.numeric(Disc_Grain.list[[k]][[d]][[g]][[3]])
      tempo_3 <-  as.numeric(Disc_Grain.list[[k]][[d]][[g]][[4]])
      for (h in 1: length(tempo_1)) {
        Doses[h,p]<- tempo_1[h]
        LxTx[h,p]<- tempo_2[h]
        LxTx.error[h,p]<- tempo_3[h]
      }
    }
  }

  Doses <-  apply(Doses, MARGIN = 2, FUN = "as.single")
  LxTx <-  apply(LxTx, MARGIN = 2, FUN = "as.single")
  LxTx.error <-  apply(LxTx.error, MARGIN = 2, FUN = "as.single")

  ##CALL internal baSAR function
  results <-
    baSAR_function(
      Nb_aliquots = Nb_aliquots,
      distribution = distribution,
      data.Dose = Doses,
      data.Lum = LxTx,
      data.sLum = LxTx.error,
      fit.method = fit,
      fit.force_through_origin = TRUE,
      fit.includingRecyclingPoints = FALSE,
      plot = plot,
      verbose = verbose
    )


  if(verbose){
    cat(paste0("Everything was done!"))

  }

  # Return --------------------------------------------------------------------------------------
  return(set_RLum(
    class = "RLum.Results",
    data = list(
      summary = results[[1]],
      mcmc = results[[2]],
      models = results[[3]]
      ),
    info = list(call = sys.call())
  ))

}

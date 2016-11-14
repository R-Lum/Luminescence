#' Calculation of the dose rate for a ceramic sample
#'
#' This function allows to estimating the dose rate for a ceramic which was surrounded by sediment.
#' It call the \link{use_DRAC} and \link{calc_CosmicDoseRate} functions from the R package '\link{Luminescence}'.
#' The function 'use_DRAC' is only compatible wth DRAC version 1.1.
#'
#' @param data
#'  \link{list}: data object create throught the function \link{template_DRAC4flint}.
#'
#' @param notification
#'  \link{logical} (default): set to \code{FALSE} if you don't want to see the legal notification.
#'
#' @return
#'  This function return a \linkS4class{TLum.Results} object containing the Age estimation, the dose rates
#'  (total, internal, external, environmental, alpha, beta, gamma and cosmic), the equivalent dose used and their uncertainties.
#'
#' @author David Strebler
#'
#' @export use_DRAC4ceramic


use_DRAC4ceramic <- function (

  data,

  notification = TRUE
){
  ## ------------------------------------------------------------------------------------------

  if(!is(data,"DRAC4ceramic.list")){
    stop("[use_DRAC4ceramic] Error: data has to be create using the 'template_DRAC4ceramic' function.")
  }
  ## ------------------------------------------------------------------------------------------

  ## LEGAL NOTICE ----

  legal.notice <- paste("\t	-------------------- IMPORTANT NOTE ------------------------ \n",
                        "\t This function relies on the functions 'use_DRAC' and 'calc_CosmicDoseRate' \n",
                        "\t from the R package 'Luminescence' to estimate the different dose rate. \n",
                        "\t The function 'use_DRAC' is only compatible with DRAC version 1.1. \n",
                        "\t Before using this function make sure that this is the correct version, otherwise expect unspecified errors.\n",
                        "\t Please ensure you cite the use of DRAC in your work, published or otherwise. Please cite the website name and  \n",
                        "\t version (e.g. DRAC v1.1) and the accompanying journal article:  \n",
                        "\t Durcan, J.A., King, G.E., Duller, G.A.T., 2015. DRAC: Dose rate and age calculation for trapped charge  \n",
                        "\t dating. Quaternary Geochronology 28, 54-61. \n",
                        "\t Please ensure you also cite the use of the R package 'Luminescence' (cf. citation('Luminescence')). \n")

  if(notification){
    message(legal.notice)
  }

  comment <- list(legal=legal.notice)


  project <- data$info$project
  sample <- data$info$sample
  mineral <- data$info$mineral
  conversion.factors <- data$info$conversion.factors

  int.alpha.attenuation <- data$info$alpha.size.attenuation
  int.beta.attenuation <- data$info$beta.size.attenuation
  int.etch.beta.attenuation <- data$info$beta.etch.attenuation

  date <- data$info$date

  ## ------------------------------------------------------------------------------------------
  # Check values
  if(is.null(project)){
    project <- "Unknown"
    warning("[use_DRAC4ceramic] Warning: no project name")

  }else if(!is.character(project)){
    stop("[use_DRAC4ceramic] Error: data$info$project has to be of type 'character'.")
  }

  if(is.null(sample)){
    sample <- "Unknown"
    warning("[use_DRAC4ceramic] Warning: no sample name")

  }else if(!is.character(sample)){
    stop("[use_DRAC4ceramic] Error: data$info$sample has to be of type 'character'.")
  }

  if(is.null(mineral)){
    stop("[use_DRAC4ceramic] Error: mineral unknown")
  }else if(!is.character(mineral)){
    stop("[use_DRAC4ceramic] Error: data$info$mineral has to be of type 'character'.")
  }else if(!(mineral %in% c("Q", "F", "PM"))){
    stop("[use_DRAC4ceramic] Error: data$info$mineral can only be 'Q', 'F' or 'PM'.")
  }

  if(is.null(conversion.factors)){
    stop("[use_DRAC4ceramic] Error: no conversion factors selected")
  }else if(!is.character(conversion.factors)){
    stop("[use_DRAC4ceramic] Error: data$info$conversion.factors has to be of type 'character'.")
  }else if(!(conversion.factors %in% c("AdamiecAitken1998", "Guerinetal2011", "Liritzisetal2013"))){
    stop("[use_DRAC4ceramic] Error: data$info$conversion.factors can only be 'AdamiecAitken1998', 'Guerinetal2011' or 'Liritzisetal2013'.")
  }

  if(is.null(int.alpha.attenuation)){
    stop("[use_DRAC4ceramic] Error: 'data$info$alpha.size.attenuation' is null.")
  }else if(!is.character(int.alpha.attenuation)){
    stop("[use_DRAC4ceramic] Error: 'data$info$alpha.size.attenuation' is not of type 'character'.")
  }else if(!(int.alpha.attenuation %in% c("Bell1980","Brennanetal1991"))){
    stop("[use_DRAC4ceramic] Error: 'data$info$alpha.size.attenuation' can only be 'Bell1980' or 'Brennanetal1991'.")
  }

  if(is.null(int.beta.attenuation)){
    stop("[use_DRAC4ceramic] Error: 'data$info$beta.size.attenuation' is null.")
  }else if(!is.character(int.beta.attenuation)){
    stop("[use_DRAC4ceramic] Error: 'data$info$beta.size.attenuation' is not of type 'character'.")
  }else if(!(int.beta.attenuation %in% c("Mejdahl1979","Brennan2003", "Guerinetal2012-Q", "Guerinetal2012-F"))){
    stop("[use_DRAC4ceramic] Error: 'data$info$beta.size.attenuation' can only be 'Mejdahl1979', 'Brennan2003', 'Guerinetal2012-Q' or 'Guerinetal2012-F'.")
  }else if(int.beta.attenuation == "Guerinetal2012-Q" && mineral != "Q"){
    warning("[use_DRAC4ceramic]  Warning you use 'Guerinetal2012-Q' for 'data$info$beta.size.attenuation' but material is not 'Q'.")
  }else if(int.beta.attenuation == "Guerinetal2012-F" && mineral != "F"){
    warning("[use_DRAC4ceramic]  Warning you use 'Guerinetal2012-F' for 'info$beta.size.attenuation' but material is not 'F'.")
  }

  if(is.null(int.etch.beta.attenuation)){
    stop("[use_DRAC4ceramic] Error: 'data$info$beta.etch.attenuation' is null.")
  }else if(!is.character(int.etch.beta.attenuation)){
    stop("[use_DRAC4ceramic] Error: 'data$info$beta.etch.attenuation' is not of type 'character'.")
  }else if(!(int.etch.beta.attenuation %in% c("Bell1979","Brennan2003"))){
    stop("[use_DRAC4ceramic] Error: 'data$info$beta.etch.attenuation' can only be 'Bell1979' or 'Brennan2003'.")
  }

  if(is.null(date)){
    date <- as.numeric(format(Sys.Date(),"%Y"))
  }else if(!is.numeric(date)){
    stop("[use_DRAC4ceramic] Error: 'data$info$date' is not of type 'numeric'.")
  }

  ## ------------------------------------------------------------------------------------------

  # Equivalent dose
  De <- data$De$De
  De.err <- data$De$De.err

  ## ------------------------------------------------------------------------------------------
  # Check values
  if(is.null(De) || is.null(De.err)){
    De <- 0
    De.err <- 0
    warning("[use_DRAC4ceramic] Warning: data$De$De or data$De$De.err is missing. De and De.err are considered as null")

  }else if(!is.numeric(De) || !is.numeric(De)){
    stop("[use_DRAC4ceramic] Error: data$De$De has to be of type 'numeric'.")
  }else if(De < 0 || De.err < 0){
    stop("[use_DRAC4ceramic] Error: data$De$De or data$De$De.err < 0.")
  }
  ## ------------------------------------------------------------------------------------------

  # Internal dose rate
  # By concentration
  int.U <- data$grain$Dr$U
  int.U.err <- data$grain$Dr$U.err
  int.Th <- data$grain$Dr$Th
  int.Th.err <- data$grain$Dr$Th.err
  int.K <- data$grain$Dr$K
  int.K.err <- data$grain$Dr$K.err
  int.Rb <- data$grain$Dr$Rb
  int.Rb.err <- data$grain$Dr$Rb.err

  int.K2Rb <- data$grain$Dr$K2Rb


  # Direct evaluation
  int.alpha <- data$grain$Dr$alpha
  int.alpha.err <- data$grain$Dr$alpha.err
  int.beta <- data$grain$Dr$beta
  int.beta.err <- data$grain$Dr$beta.err
  int.gamma <- data$grain$Dr$gamma
  int.gamma.err <- data$grain$Dr$gamma.err


  ## ------------------------------------------------------------------------------------------
  # Check values

  # Dose rate direct estimation
  if(is.null(int.alpha) || is.null(int.alpha.err)){
    int.alpha <- "X"
    int.alpha.err <- "X"
  }else if(!is.numeric(int.alpha) || !is.numeric(int.alpha.err)){
    stop("[use_DRAC4ceramic] Error: data$grain$Dr$alpha or data$grain$Dr$alpha.err is not of type 'numeric'.")
  }else if(int.alpha < 0 || int.alpha.err < 0){
    stop("[use_DRAC4ceramic] Error: data$grain$Dr$alpha or data$grain$Dr$alpha.err is  < 0.")
  }

  if(is.null(int.beta) || is.null(int.beta.err)){
    int.beta <- "X"
    int.beta.err <- "X"
  }else if(!is.numeric(int.beta) || !is.numeric(int.beta.err)){
    stop("[use_DRAC4ceramic] Error: data$grain$Dr$beta or data$grain$Dr$beta.err is not of type 'numeric'.")
  }else if(int.beta < 0 || int.beta.err < 0){
    stop("[use_DRAC4ceramic] Error: data$grain$Dr$beta or data$grain$Dr$beta.err is  < 0.")
  }

  if(is.null(int.gamma) || is.null(int.gamma.err)){
    int.gamma <- "X"
    int.gamma.err <- "X"
  }else if(!is.numeric(int.gamma) || !is.numeric(int.gamma.err)){
    stop("[use_DRAC4ceramic] Error: data$grain$Dr$gamma or data$grain$Dr$gamma.err is not of type 'numeric'.")
  }else if(int.gamma < 0 || int.gamma.err < 0){
    stop("[use_DRAC4ceramic] Error: data$grain$Dr$gamma or data$grain$Dr$gamma.err is  < 0.")
  }

  # Concentration
  if (is.null(int.U) || is.null(int.Th) || is.null(int.K)){
    int.U <- "X"
    int.U.err <- "X"
    int.Th <- "X"
    int.Th.err <- "X"
    int.K <- "X"
    int.K.err <- "X"
    int.Rb <- "X"
    int.Rb.err <- "X"

    int.K2Rb <- "N"

  }else{
    if(!is.numeric(int.U) || !is.numeric(int.U.err)){
      stop("[use_DRAC4ceramic] Error: data$grain$Dr$U or data$grain$Dr$U.err is not of type 'numeric'.")
    }else if(int.U < 0 || int.U.err < 0){
      stop("[use_DRAC4ceramic] Error: data$grain$Dr$U or data$grain$Dr$U.err is  < 0.")
    }

    if(!is.numeric(int.Th) || !is.numeric(int.Th.err)){
      stop("[use_DRAC4ceramic] Error: data$grain$Dr$Th or data$grain$Dr$Th.err is not of type 'numeric'.")
    }else if(int.Th < 0 || int.Th.err < 0){
      stop("[use_DRAC4ceramic] Error: data$grain$Dr$Th or data$grain$Dr$Th.err is  < 0.")
    }

    if(!is.numeric(int.K) || !is.numeric(int.K.err)){
      stop("[use_DRAC4ceramic] Error: data$grain$Dr$K or data$grain$Dr$K.err is not of type 'numeric'.")
    }else if(int.K < 0 || int.K.err < 0){
      stop("[use_DRAC4ceramic] Error: data$grain$Dr$K or data$grain$Dr$K.err is  < 0.")
    }

    if(is.null(int.Rb) || is.null(int.Rb.err)){
      int.K2Rb <- "Y"
      int.Rb <- "X"
      int.Rb.err <- "X"

    }else if(!is.numeric(int.Rb) || !is.numeric(int.Rb.err)){
      stop("[use_DRAC4ceramic] Error: data$grain$Dr$Rb or data$grain$Dr$Rb.err is not of type 'numeric'.")
    }else if(int.Rb < 0 || int.Rb.err < 0){
      stop("[use_DRAC4ceramic] Error: data$grain$Dr$Rb or data$grain$Dr$Rb.err is  < 0.")
    }else if(is.null(int.K2Rb)){
      int.K2Rb <- "N"
    }else if(!is.logical(int.K2Rb)){
      stop("[use_DRAC4ceramic] Error: 'data$grain$Dr$K2Rb' is not of type 'logical'.")
    }else if(int.K2Rb){
      int.K2Rb <- "Y"
    }else{
      int.K2Rb <- "N"
    }
  }

  ## ------------------------------------------------------------------------------------------

  # External dose rate
  # By concentration
  ext.U <- data$ceramic$Dr$U
  ext.U.err <- data$ceramic$Dr$U.err
  ext.Th <- data$ceramic$Dr$Th
  ext.Th.err <- data$ceramic$Dr$Th.err
  ext.K <- data$ceramic$Dr$K
  ext.K.err <- data$ceramic$Dr$K.err
  ext.Rb <- data$ceramic$Dr$Rb
  ext.Rb.err <- data$ceramic$Dr$Rb.err

  ext.K2Rb <- data$ceramic$Dr$K2Rb


  # Direct evaluation
  ext.alpha <- data$ceramic$Dr$alpha
  ext.alpha.err <- data$ceramic$Dr$alpha.err
  ext.beta <- data$ceramic$Dr$beta
  ext.beta.err <- data$ceramic$Dr$beta.err
  ext.gamma <- data$ceramic$Dr$gamma
  ext.gamma.err <- data$ceramic$Dr$gamma.err


  ## ------------------------------------------------------------------------------------------
  # Check values

  # Dose rate direct estimation
  if(is.null(ext.alpha) || is.null(ext.alpha.err)){
    ext.alpha <- "X"
    ext.alpha.err <- "X"
  }else if(!is.numeric(ext.alpha) || !is.numeric(ext.alpha.err)){
    stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$alpha or data$ceramic$Dr$alpha.err is not of type 'numeric'.")
  }else if(ext.alpha < 0 || ext.alpha.err < 0){
    stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$alpha or data$ceramic$Dr$alpha.err is  < 0.")
  }

  if(is.null(ext.beta) || is.null(ext.beta.err)){
    ext.beta <- "X"
    ext.beta.err <- "X"
  }else if(!is.numeric(ext.beta) || !is.numeric(ext.beta.err)){
    stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$beta or data$ceramic$Dr$beta.err is not of type 'numeric'.")
  }else if(ext.beta < 0 || ext.beta.err < 0){
    stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$beta or data$ceramic$Dr$beta.err is  < 0.")
  }

  if(is.null(ext.gamma) || is.null(ext.gamma.err)){
    ext.gamma <- "X"
    ext.gamma.err <- "X"
  }else if(!is.numeric(ext.gamma) || !is.numeric(ext.gamma.err)){
    stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$gamma or data$ceramic$Dr$gamma.err is not of type 'numeric'.")
  }else if(ext.gamma < 0 || ext.gamma.err < 0){
    stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$gamma or data$ceramic$Dr$gamma.err is  < 0.")
  }

  # Concentration
  if (is.null(ext.U) || is.null(ext.Th) || is.null(ext.K)){
    ext.U <- "X"
    ext.U.err <- "X"
    ext.Th <- "X"
    ext.Th.err <- "X"
    ext.K <- "X"
    ext.K.err <- "X"
    ext.Rb <- "X"
    ext.Rb.err <- "X"

    ext.K2Rb <- "N"

  }else{
    if(!is.numeric(ext.U) || !is.numeric(ext.U.err)){
      stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$U or data$ceramic$Dr$U.err is not of type 'numeric'.")
    }else if(ext.U < 0 || ext.U.err < 0){
      stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$U or data$ceramic$Dr$U.err is  < 0.")
    }

    if(!is.numeric(ext.Th) || !is.numeric(ext.Th.err)){
      stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$Th or data$ceramic$Dr$Th.err is not of type 'numeric'.")
    }else if(ext.Th < 0 || ext.Th.err < 0){
      stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$Th or data$ceramic$Dr$Th.err is  < 0.")
    }

    if(!is.numeric(ext.K) || !is.numeric(ext.K.err)){
      stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$K or data$ceramic$Dr$K.err is not of type 'numeric'.")
    }else if(ext.K < 0 || ext.K.err < 0){
      stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$K or data$ceramic$Dr$K.err is  < 0.")
    }

    if(is.null(ext.Rb) || is.null(ext.Rb.err)){
      ext.K2Rb <- "Y"
      ext.Rb <- "X"
      ext.Rb.err <- "X"

    }else if(!is.numeric(ext.Rb) || !is.numeric(ext.Rb.err)){
      stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$Rb or data$ceramic$Dr$Rb.err is not of type 'numeric'.")
    }else if(ext.Rb < 0 || ext.Rb.err < 0){
      stop("[use_DRAC4ceramic] Error: data$ceramic$Dr$Rb or data$ceramic$Dr$Rb.err is  < 0.")
    }else if(is.null(ext.K2Rb)){
      ext.K2Rb <- "N"
    }else if(!is.logical(ext.K2Rb)){
      stop("[use_DRAC4ceramic] Error: 'data$ceramic$Dr$K2Rb' is not of type 'logical'.")
    }else if(ext.K2Rb){
      ext.K2Rb <- "Y"
    }else{
      ext.K2Rb <- "N"
    }
  }

  ## ------------------------------------------------------------------------------------------

  # Environmental dose rate
  # By concentration
  env.U <- data$sediment$Dr$U
  env.U.err <- data$sediment$Dr$U.err
  env.Th <- data$sediment$Dr$Th
  env.Th.err <- data$sediment$Dr$Th.err
  env.K <- data$sediment$Dr$K
  env.K.err <- data$sediment$Dr$K.err
  env.Rb <- data$sediment$Dr$Rb
  env.Rb.err <- data$sediment$Dr$Rb.err

  env.K2Rb <- data$sediment$Dr$K2Rb


  # Direct evaluation
  env.alpha <- data$sediment$Dr$alpha
  env.alpha.err <- data$sediment$Dr$alpha.err
  env.beta <- data$sediment$Dr$beta
  env.beta.err <- data$sediment$Dr$beta.err
  env.gamma <- data$sediment$Dr$gamma
  env.gamma.err <- data$sediment$Dr$gamma.err


  ## ------------------------------------------------------------------------------------------
  # Check values

  # Dose rate direct estimation
  if(is.null(env.alpha) || is.null(env.alpha.err)){
    env.alpha <- "X"
    env.alpha.err <- "X"
  }else if(!is.numeric(env.alpha) || !is.numeric(env.alpha.err)){
    stop("[use_DRAC4ceramic] Error: data$sediment$Dr$alpha or data$sediment$Dr$alpha.err is not of type 'numeric'.")
  }else if(env.alpha < 0 || env.alpha.err < 0){
    stop("[use_DRAC4ceramic] Error: data$sediment$Dr$alpha or data$sediment$Dr$alpha.err is  < 0.")
  }

  if(is.null(env.beta) || is.null(env.beta.err)){
    env.beta <- "X"
    env.beta.err <- "X"
  }else if(!is.numeric(env.beta) || !is.numeric(env.beta.err)){
    stop("[use_DRAC4ceramic] Error: data$sediment$Dr$beta or data$sediment$Dr$beta.err is not of type 'numeric'.")
  }else if(env.beta < 0 || env.beta.err < 0){
    stop("[use_DRAC4ceramic] Error: data$sediment$Dr$beta or data$sediment$Dr$beta.err is  < 0.")
  }

  if(is.null(env.gamma) || is.null(env.gamma.err)){
    env.gamma <- "X"
    env.gamma.err <- "X"
  }else if(!is.numeric(env.gamma) || !is.numeric(env.gamma.err)){
    stop("[use_DRAC4ceramic] Error: data$sediment$Dr$gamma or data$sediment$Dr$gamma.err is not of type 'numeric'.")
  }else if(env.gamma < 0 || env.gamma.err < 0){
    stop("[use_DRAC4ceramic] Error: data$sediment$Dr$gamma or data$sediment$Dr$gamma.err is  < 0.")
  }

  # Dose rate base on concentration
  if (is.null(env.U) || is.null(env.Th) || is.null(env.K)){
    env.U <- "X"
    env.U.err <- "X"
    env.Th <- "X"
    env.Th.err <- "X"
    env.K <- "X"
    env.K.err <- "X"
    env.Rb <- "X"
    env.Rb.err <- "X"

    env.K2Rb <- "N"

  }else{
    if(!is.numeric(env.U) || !is.numeric(env.U.err)){
      stop("[use_DRAC4ceramic] Error: data$sediment$Dr$U or data$sediment$Dr$U.err is not of type 'numeric'.")
    }else if(env.U < 0 || env.U.err < 0){
      stop("[use_DRAC4ceramic] Error: data$sediment$Dr$U or data$sediment$Dr$U.err is  < 0.")
    }

    if(!is.numeric(env.Th) || !is.numeric(env.Th.err)){
      stop("[use_DRAC4ceramic] Error: data$sediment$Dr$Th or data$sediment$Dr$Th.err is not of type 'numeric'.")
    }else if(env.Th < 0 || env.Th.err < 0){
      stop("[use_DRAC4ceramic] Error: data$sediment$Dr$Th or data$sediment$Dr$Th.err is  < 0.")
    }

    if(!is.numeric(env.K) || !is.numeric(env.K.err)){
      stop("[use_DRAC4ceramic] Error: data$sediment$Dr$K or data$sediment$Dr$K.err is not of type 'numeric'.")
    }else if(env.K < 0 || env.K.err < 0){
      stop("[use_DRAC4ceramic] Error: data$sediment$Dr$K or data$sediment$Dr$K.err is  < 0.")
    }

    if(is.null(env.Rb)){
      env.K2Rb <- "Y"
      env.Rb <- "X"
      env.Rb.err <- "X"

    }else if(!is.numeric(env.Rb) || !is.numeric(env.Rb.err)){
      stop("[use_DRAC4ceramic] Error: data$sediment$Dr$Rb or data$sediment$Dr$Rb.err is not of type 'numeric'.")
    }else if(env.Rb < 0 || env.Rb.err < 0){
      stop("[use_DRAC4ceramic] Error: data$sediment$Dr$Rb or data$sediment$Dr$Rb.err is  < 0.")
    }else if(is.null(env.K2Rb)){
      env.K2Rb <- "N"
    }else if(!is.logical(env.K2Rb)){
      stop("[use_DRAC4ceramic] Error: 'data$sediment$Dr$K2Rb' is not of type 'logical'.")
    }else if(env.K2Rb){
      env.K2Rb <- "Y"
    }else{
      env.K2Rb <- "N"
    }
  }
  ## ------------------------------------------------------------------------------------------

  # Flint information

  # Internal information
  int.size.min <- data$grain$info$grain.size.min
  int.size.max <- data$grain$info$grain.size.max

  int.etch.min <- data$grain$info$grain.etch.min
  int.etch.max <- data$grain$info$grain.etch.max

  int.a.value <- data$grain$info$a.value
  int.a.value.err <- data$grain$info$a.value.err

  ## ------------------------------------------------------------------------------------------
  # Check values

  if(is.null(int.size.min) || is.null(int.size.max)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.size.min' or 'data$grain$info$grain.size.max' is null.")
  }else if(!is.numeric(int.size.min) || !is.numeric(int.size.max)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.size.min' or 'data$grain$info$grain.size.max' is not of type 'numeric'.")
  }else if(int.size.min > int.size.max){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.size.min' > 'data$grain$info$grain.size.max'.")
  }else if(int.size.min < 1){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.size.min' < 1 [um].")
  }else if(int.size.max > 1000){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.size.min' > 1000 [um].")
  }

  if(is.null(int.etch.min) || is.null(int.etch.max)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.etch.min' or 'data$grain$info$grain.etch.max' is null.")
  }else if(!is.numeric(int.etch.min) || !is.numeric(int.etch.max)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.etch.min' or 'data$grain$info$grain.etch.max' is not of type 'numeric'.")
  }else if(int.etch.min > int.etch.max){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.etch.min' > 'data$grain$info$grain.etch.max'.")
  }else if(int.etch.min < 0){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.etch.min' < 0 [um].")
  }else if(int.etch.max > 30){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$grain.etch.min' > 30 [um].")
  }

  if(is.null(int.a.value) || is.null(int.a.value.err)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$a.value' or 'data$grain$info$a.value.err' is null.")
  }else if(!is.numeric(int.a.value) || !is.numeric(int.a.value.err)){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$a.value' or 'data$grain$info$a.value.err' is not of type 'numeric'.")
  }else if(int.a.value < 0){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$a.value' < 0 [um].")
  }else if(int.a.value.err < 0){
    stop("[use_DRAC4ceramic] Error: 'data$grain$info$a.value.err' < 0 [um].")
  }
  ## ------------------------------------------------------------------------------------------

  # 'External' information
  max.etch <- 30

  ext.etch.min <- max.etch
  ext.etch.max <- max.etch

  ext.water <- data$ceramic$info$water.content
  ext.water.err <- data$ceramic$info$water.content.err

  ext.density <- data$ceramic$info$density
  ext.density.err <- data$ceramic$info$density.err

  ## ------------------------------------------------------------------------------------------
  # Check values

  if(is.null(ext.water) || is.null(ext.water.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$water.content' or 'data$ceramic$info$water.content.err' is null.")
  }else if(!is.numeric(ext.water) || !is.numeric(ext.water.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$water.content' or 'data$ceramic$info$water.content.err' is not of type 'numeric'.")
  }else if(ext.water < 0){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$water.content' < 0 [um].")
  }else if(ext.water.err < 0){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$water.content.err' < 0 [um].")
  }

  if(is.null(ext.density) || is.null(ext.density.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$density' or 'data$ceramic$info$density.err' is null.")
  }else if(!is.numeric(ext.density) || !is.numeric(ext.density.err)){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$density' or 'data$ceramic$info$density.err' is not of type 'numeric'.")
  }else if(ext.density < 0){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$density' < 0 [um].")
  }else if(ext.density.err < 0){
    stop("[use_DRAC4ceramic] Error: 'data$ceramic$info$density.err' < 0 [um].")
  }
  ## ------------------------------------------------------------------------------------------

  # Sediment information

  # Env information
  env.water <- data$sediment$info$water.content
  env.water.err <- data$sediment$info$water.content.err

  env.density <- data$sediment$info$density
  env.density.err <- data$sediment$info$density.err

  env.scale4shallow.depth <- data$sediment$info$scale4shallow.depth

  ## ------------------------------------------------------------------------------------------
  # Check values

  if(is.null(env.water) || is.null(env.water.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$water.content' or 'data$sediment$info$water.content.err' is null.")
  }else if(!is.numeric(env.water) || !is.numeric(env.water.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$water.content' or 'data$sediment$info$water.content.err' is not of type 'numeric'.")
  }else if(env.water < 0){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$water.content' < 0 [um].")
  }else if(env.water.err < 0){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$water.content.err' < 0 [um].")
  }

  if(is.null(env.density) || is.null(env.density.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$density' or 'data$sediment$info$density.err' is null.")
  }else if(!is.numeric(env.density) || !is.numeric(env.density.err)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$density' or 'data$sediment$info$density.err' is not of type 'numeric'.")
  }else if(env.density < 0){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$density' < 0 [um].")
  }else if(env.density.err < 0){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$density.err' < 0 [um].")
  }

  if(is.null(env.scale4shallow.depth)){
    env.scale4shallow.depth <- 'N'
    warning("[use_DRAC4ceramic] Error: 'data$sediment$info$scale4shallow.depth' is null.")

  }else if(!is.logical(env.scale4shallow.depth)){
    stop("[use_DRAC4ceramic] Error: 'data$sediment$info$scale4shallow.depth' is not of type 'logical'.")
  }else if(env.scale4shallow.depth){
    env.scale4shallow.depth <- "Y"
  }else{
    env.scale4shallow.depth <- "N"
  }
  ## ------------------------------------------------------------------------------------------

  # Cosmic dose rate

  #Theoretical

  depth <- data$cosmic$depth
  depth.err <- data$cosmic$depth.err

  latitude <- data$cosmic$latitude
  longitude <- data$cosmic$longitude
  altitude <- data$cosmic$altitude

  #Direct measurement
  cosmic <- data$cosmic$Dr
  cosmic.err <- data$cosmic$Dr.err

  # For calc_CosmicDoseRate
  corr.fieldChanges <- data$cosmic$corr.fieldChanges

  ## ------------------------------------------------------------------------------------------
  # Check values

  if(is.null(depth) || is.null(depth.err)){
    depth <- "X"
    depth.err <- "X"

  }else if(!is.numeric(depth) || !is.numeric(depth.err)){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$depth' or 'data$cosmic$depth.err' is not of type 'numeric'.")
  }else if(depth < 0){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$depth' < 0 [m].")
  }else if(depth.err < 0){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$depth.err' < 0 [m].")
  }

  if(is.null(latitude) || is.null(longitude) || is.null(altitude)){
    latitude <- "X"
    longitude <- "X"
    altitude <- "X"

  }else if(!is.numeric(latitude) || !is.numeric(longitude) || !is.numeric(altitude) ){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$latitude', 'data$cosmic$longitude' or 'data$cosmic$altitude' is not of type 'numeric'.")
  }else if(latitude < -90 || latitude > 90){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$latitude' has to be between -90\u00b0 and +90\u00b0.")
  }else if(longitude < -180 || longitude > 180){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$longitude' has to be between -180\u00b0 and +180\u00b0.")
  }else if(altitude > 5000){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$altitude' has to be < 5000 m.")
  }

  if(is.null(cosmic) || is.null(cosmic.err)){
    cosmic <- "X"
    cosmic.err <- "X"
  }else if(!is.numeric(cosmic) || !is.numeric(cosmic.err)){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$Dr' or 'data$cosmic$Dr.err' is not of type 'numeric'.")
  }else if(cosmic < 0){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$Dr' < 0 [Gy].")
  }else if(cosmic.err < 0){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$Dr.err' < 0 [Gy].")
  }

  if(is.null(corr.fieldChanges)){
    corr.fieldChanges <- FALSE
  }else if(!is.logical(corr.fieldChanges)){
    stop("[use_DRAC4ceramic] Error: 'data$cosmic$corr.fieldChanges' is not of type 'logical'.")
  }

  ## ------------------------------------------------------------------------------------------
  # First run, Internal & External dose (flint)

  int.input <- template_DRAC(notification = notification)

  int.input$`Project ID` <- project
  int.input$`Sample ID` <- sample
  int.input$Mineral <- mineral
  int.input$`Conversion factors` <- conversion.factors

  int.input$`Internal U (ppm)` <-  int.U
  int.input$`errInternal U (ppm)` <- int.U.err
  int.input$`Internal Th (ppm)` <- int.Th
  int.input$`errInternal Th (ppm)` <- int.Th.err
  int.input$`Internal K (%)` <- int.K
  int.input$`errInternal K (%)` <- int.K.err
  int.input$`Rb (ppm)` <- int.Rb
  int.input$`errRb (ppm)` <- int.Rb.err
  int.input$`Calculate internal Rb from K conc?`<- int.K2Rb

  int.input$`ExternalU (ppm)` <-  ext.U
  int.input$`errExternal U (ppm)` <- ext.U.err
  int.input$`External Th (ppm)` <- ext.Th
  int.input$`errExternal Th (ppm)` <- ext.Th.err
  int.input$`External K (%)` <- ext.K
  int.input$`errExternal K (%)` <- ext.K.err
  int.input$`External Rb (ppm)` <- ext.Rb
  int.input$`errExternal Rb (ppm)` <- ext.Rb.err
  int.input$`Calculate external Rb from K conc?` <- ext.K2Rb

  int.input$`Grain size min (microns)` <- int.size.min
  int.input$`Grain size max (microns)` <- int.size.max

  int.input$`alpha-Grain size attenuation` <- int.alpha.attenuation
  int.input$`beta-Grain size attenuation ` <- int.beta.attenuation

  int.input$`Etch depth min (microns)` <- int.etch.min
  int.input$`Etch depth max (microns)` <- int.etch.max

  int.input$`beta-Etch depth attenuation factor` <- int.etch.beta.attenuation

  int.input$`a-value` <- int.a.value
  int.input$`erra-value` <- int.a.value.err

  int.input$`Water content ((wet weight - dry weight)/dry weight) %` <- ext.water
  int.input$`errWater content %` <- ext.water.err

  int.input$`Depth (m)` <- depth
  int.input$`errDepth (m)` <- depth.err

  int.input$`Overburden density (g cm-3)` <- env.density
  int.input$`errOverburden density (g cm-3)` <- env.density.err

  int.input$`Latitude (decimal degrees)` <- latitude
  int.input$`Longitude (decimal degrees)` <- longitude
  int.input$`Altitude (m)` <- altitude

  int.input$`De (Gy)` <- De
  int.input$`errDe (Gy)` <- De.err

  int.output <- use_DRAC(int.input,verbose=notification)

  # --------------------------------------------------------------------------------------------------------------------------

  # Second run, Evironmental dose (sediment)

  env.input <- template_DRAC(notification = FALSE)

  env.input$`Project ID` <- project
  env.input$`Sample ID` <- sample
  env.input$Mineral <- mineral
  env.input$`Conversion factors` <- conversion.factors

  env.input$`ExternalU (ppm)` <-  env.U
  env.input$`errExternal U (ppm)` <- env.U.err
  env.input$`External Th (ppm)` <- env.Th
  env.input$`errExternal Th (ppm)` <- env.Th.err
  env.input$`External K (%)` <- env.K
  env.input$`errExternal K (%)` <- env.K.err
  int.input$`External Rb (ppm)` <- env.Rb
  int.input$`errExternal Rb (ppm)` <- env.Rb.err
  env.input$`Calculate external Rb from K conc?` <- env.K2Rb

  env.input$`Scale gammadoserate at shallow depths?` <- env.scale4shallow.depth

  env.input$`Grain size min (microns)` <- int.size.min
  env.input$`Grain size max (microns)` <- int.size.max

  env.input$`a-value` <- int.a.value
  env.input$`erra-value` <- int.a.value.err

  env.input$`Water content ((wet weight - dry weight)/dry weight) %` <- env.water
  env.input$`errWater content %` <- env.water.err

  env.input$`Depth (m)` <- depth
  env.input$`errDepth (m)` <- depth.err
  env.input$`Overburden density (g cm-3)` <- env.density
  env.input$`errOverburden density (g cm-3)` <- env.density.err
  env.input$`Latitude (decimal degrees)` <- latitude
  env.input$`Longitude (decimal degrees)` <- longitude
  env.input$`Altitude (m)` <- altitude

  env.input$`De (Gy)` <- De
  env.input$`errDe (Gy)` <- De.err

  env.output <- use_DRAC(env.input,verbose=FALSE)

  # --------------------------------------------------------------------------------------------------------------------------

  # Combining the 2 runs...

  temp.int.alpha <- as.numeric(int.output$DRAC$highlights$`Internal Dry alphadoserate (Gy.ka-1)`[1])
  temp.int.alpha.err <- as.numeric(int.output$DRAC$highlights$`Internal Dry erralphadoserate (Gy.ka-1)`[1])

  temp.int.beta <- as.numeric(int.output$DRAC$highlights$`Internal Dry betadoserate (Gy.ka-1)`[1])
  temp.int.beta.err <- as.numeric(int.output$DRAC$highlights$`Internal Dry errbetadoserate (Gy.ka-1)`[1])

  temp.ext.alpha <- as.numeric(int.output$DRAC$highlights$`Water corrected alphadoserate`[1])
  temp.ext.alpha.err <- as.numeric(int.output$DRAC$highlights$`Water corrected erralphadoserate`[1])

  temp.ext.beta <- as.numeric(int.output$DRAC$highlights$`Water corrected betadoserate`[1])
  temp.ext.beta.err <- as.numeric(int.output$DRAC$highlights$`Water corrected errbetadoserate`[1])

  temp.ext.gamma <- as.numeric(int.output$DRAC$highlights$`Water corrected gammadoserate (Gy.ka-1)`[1])
  temp.ext.gamma.err <- as.numeric(int.output$DRAC$highlights$`Water corrected errgammadoserate (Gy.ka-1)`[1])

  temp.env.alpha <- as.numeric(env.output$DRAC$highlights$`Water corrected alphadoserate`[1])
  temp.env.alpha.err <- as.numeric(env.output$DRAC$highlights$`Water corrected erralphadoserate`[1])

  temp.env.beta <-  as.numeric(env.output$DRAC$highlights$`Water corrected betadoserate`[1])
  temp.env.beta.err <- as.numeric(env.output$DRAC$highlights$`Water corrected errbetadoserate`[1])

  temp.env.gamma <- as.numeric(env.output$DRAC$highlights$`Water corrected gammadoserate (Gy.ka-1)`[1])
  temp.env.gamma.err <- as.numeric(env.output$DRAC$highlights$`Water corrected errgammadoserate (Gy.ka-1)`[1])

  temp.cosmic <- as.numeric(env.output$DRAC$highlights$`Cosmicdoserate (Gy.ka-1)`[1])
  temp.cosmic.err <- as.numeric(env.output$DRAC$highlights$`errCosmicdoserate (Gy.ka-1)`[1])

  # --------------------------------------------------------------------------------------------------------------------------

  # Estimation of external gamma over/under.estimation

  if(temp.env.gamma > temp.ext.gamma){
    gamma.over <- abs(temp.env.gamma-temp.ext.gamma)/temp.ext.gamma*100

    message.gamma <- paste("The gamma dose rate coming from the flint is probably overestimated (+",round(gamma.over,2)," %).",sep="")
  }else if(temp.env.gamma > temp.ext.gamma){
    gamma.under <- abs(temp.env.gamma-temp.ext.gamma)/temp.ext.gamma

    message.gamma <- paste("The gamma dose rate coming from the flint is probably underestimated (-",round(gamma.under,2)," %).",sep="")
  }else{
    message.gamma <- "The environment and the flint produce similar gamma dose rate. \n"
  }

  comment <- c(comment, gamma=message.gamma)

  # DRAC results
  # Partial dose rate
  DRAC.int.Dr <- temp.int.alpha+temp.int.beta
  DRAC.int.Dr.err <- sqrt(sum(temp.int.alpha.err^2, temp.int.beta^2))

  DRAC.ext.Dr <- temp.ext.alpha+temp.ext.beta
  DRAC.ext.Dr.err <- sqrt(sum(temp.ext.alpha.err^2, temp.ext.beta^2))

  DRAC.env.Dr <- temp.env.gamma+temp.cosmic
  DRAC.env.Dr.err <- sqrt(sum(temp.env.gamma.err^2, temp.cosmic.err^2))

  DRAC.alpha.Dr <- temp.int.alpha+temp.ext.alpha
  DRAC.alpha.Dr.err <- sqrt(sum(temp.int.alpha.err^2, temp.ext.alpha^2))

  DRAC.beta.Dr <- temp.int.beta+temp.ext.beta
  DRAC.beta.Dr.err <- sqrt(sum(temp.int.beta.err^2, temp.ext.beta^2))

  DRAC.gamma.Dr <- temp.env.gamma
  DRAC.gamma.Dr.err <- temp.env.gamma.err

  DRAC.cosmic.Dr <- temp.cosmic
  DRAC.cosmic.Dr.err <- temp.cosmic.err

  # Total dose rate
  DRAC.Dr <- sum(DRAC.alpha.Dr,
                 DRAC.beta.Dr,
                 DRAC.gamma.Dr,
                 DRAC.cosmic.Dr)

  DRAC.Dr.err <- sqrt(sum(DRAC.alpha.Dr.err^2,
                          DRAC.beta.Dr.err^2,
                          DRAC.gamma.Dr.err^2,
                          DRAC.cosmic.Dr.err^2))

  # Age
  DRAC.age <- De/DRAC.Dr
  DRAC.age.err <- DRAC.age*sqrt(sum((De.err/De)^2,(DRAC.Dr.err/DRAC.Dr)^2))


  # output
  DRAC.result <- list(Age = DRAC.age,
                      Age.err =DRAC.age.err,
                      De=De,
                      De.err = De.err,
                      Dr = DRAC.Dr,
                      Dr.err = DRAC.Dr.err,
                      int.Dr = DRAC.int.Dr,
                      int.Dr.err = DRAC.int.Dr.err,
                      ext.Dr = DRAC.ext.Dr,
                      ext.Dr.err = DRAC.ext.Dr.err,
                      env.Dr = DRAC.env.Dr,
                      env.Dr.err = DRAC.env.Dr.err,
                      alpha.Dr = DRAC.alpha.Dr,
                      alpha.Dr.err = DRAC.alpha.Dr.err,
                      beta.Dr = DRAC.beta.Dr,
                      beta.Dr.err = DRAC.beta.Dr.err,
                      gamma.Dr = DRAC.gamma.Dr,
                      gamma.Dr.err = DRAC.gamma.Dr.err,
                      cosmic.Dr = DRAC.cosmic.Dr,
                      cosmic.Dr.err = DRAC.cosmic.Dr.err)

  # Correction of the cosmic dose rate using 'calc_CosmicDoseRate'

  R.cosmic <- calc_CosmicDoseRate(depth = depth,
                                  density = env.density,
                                  latitude = latitude,
                                  longitude = longitude,
                                  altitude = altitude,
                                  est.age = DRAC.age,
                                  error = DRAC.cosmic.Dr.err/DRAC.cosmic.Dr,
                                  corr.fieldChanges = corr.fieldChanges)

  R.cosmic.Dr <- R.cosmic$summary$dc
  R.cosmic.Dr.err <- R.cosmic$summary$dc*R.cosmic$args$error

  # R results

  # Partial dose rate
  R.int.Dr <- temp.int.alpha+temp.int.beta
  R.int.Dr.err <- sqrt(sum(temp.int.alpha.err^2, temp.int.beta^2))

  R.ext.Dr <- temp.ext.alpha+temp.ext.beta
  R.ext.Dr.err <- sqrt(sum(temp.ext.alpha.err^2, temp.ext.beta^2))

  R.env.Dr <- temp.env.gamma+R.cosmic.Dr
  R.env.Dr.err <- sqrt(sum(temp.env.gamma.err^2, R.cosmic.Dr.err^2))

  R.alpha.Dr <- temp.int.alpha+temp.ext.alpha
  R.alpha.Dr.err <- sqrt(sum(temp.int.alpha.err^2, temp.ext.alpha^2))

  R.beta.Dr <- temp.int.beta+temp.ext.beta
  R.beta.Dr.err <- sqrt(sum(temp.int.beta.err^2, temp.ext.beta^2))

  R.gamma.Dr <- temp.env.gamma
  R.gamma.Dr.err <- temp.env.gamma.err

  # Total dose rate
  R.Dr <- sum(R.alpha.Dr,
              R.beta.Dr,
              R.gamma.Dr,
              R.cosmic.Dr)

  R.Dr.err <- sqrt(sum(R.alpha.Dr.err^2,
                       R.beta.Dr.err^2,
                       R.gamma.Dr.err^2,
                       R.cosmic.Dr.err^2))

  # Age
  R.age <- De/R.Dr
  R.age.err <- R.age*sqrt(sum((De.err/De)^2,(R.Dr.err/R.Dr)^2))



  # Output
  R.result <- list(Age = R.age,
                   Age.err =R.age.err,
                   De=De,
                   De.err = De.err,
                   Dr = R.Dr,
                   Dr.err = R.Dr.err,
                   int.Dr = R.int.Dr,
                   int.Dr.err = R.int.Dr.err,
                   ext.Dr = R.ext.Dr,
                   ext.Dr.err = R.ext.Dr.err,
                   env.Dr = R.env.Dr,
                   env.Dr.err = R.env.Dr.err,
                   alpha.Dr = R.alpha.Dr,
                   alpha.Dr.err = R.alpha.Dr.err,
                   beta.Dr = R.beta.Dr,
                   beta.Dr.err = R.beta.Dr.err,
                   gamma.Dr = R.gamma.Dr,
                   gamma.Dr.err = R.gamma.Dr.err,
                   cosmic.Dr = R.cosmic.Dr,
                   cosmic.Dr.err = R.cosmic.Dr.err)


  age.CE <- date - R.age*1000
  age.CE.err <- R.age.err*1000

  if(age.CE > 0){
    message.CE <- paste("The heating of the ceramic occured around", round(age.CE), "\u00b1", round(age.CE.err), "CE.")
  }else if(abs(age.CE)<100){
    message.CE <- paste("The heating of the ceramic occured around", round(abs(age.CE),-1), "\u00b1", round(age.CE.err,-1), " BCE.")
  }else if(abs(age.CE)<1000){
    message.CE <- paste("The heating of the ceramic occured around", round(abs(age.CE),-2), "\u00b1", round(age.CE.err,-2), " BCE.")
  }else{
    message.CE <- paste("The heating of the ceramic occured around", round(abs(age.CE),-3), "\u00b1", round(age.CE.err,-3), " BCE.")
  }

  message.project <- paste("For the sample", sample, "of the project", project)
  message.De <- paste("The equivalent dose is:", round(De,3), "\u00b1", round(De.err,3), "Gy.")
  message.Dr <- paste("The dose rate is:", round(R.Dr,3), "\u00b1", round(R.Dr.err,3), "Gy/ka.")
  message.Age <- paste("The age is estimated as:", round(R.age,3), "\u00b1", round(R.age.err,3), "ka.")

  output <- paste("\t [use_DRAC4ceramic] \n ",
                  "\t \n",
                  paste("\t", message.project, "\n"),
                  "\t --------------------------------------------------------- \n ",
                  paste("\t", message.De, "\n"),
                  paste("\t", message.Dr, "\n"),
                  paste("\t", message.gamma, "\n"),
                  "\t --------------------------------------------------------- \n",
                  paste("\t", message.Age,"\n"),
                  paste("\n \t", message.CE, "\n"))


  cat(output)

  result <- list(age=R.age,
                 age.err=R.age.err,
                 Dr=R.Dr,
                 Dr.err=R.Dr.err,
                 DRAC = DRAC.result,
                 R = R.result,
                 comment=comment)

  return(result)
}

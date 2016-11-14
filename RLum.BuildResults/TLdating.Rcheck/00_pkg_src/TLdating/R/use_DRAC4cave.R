#' Calculation of the dose rate for a sediment sample coming from a cave
#'
#' This function allows to estimating the dose rate for a grain which was surrounded by sediment and rocks.
#' It call the \link{use_DRAC} and \link{calc_CosmicDoseRate} functions from the R package '\link{Luminescence}'.
#' The function 'use_DRAC' is only compatible wth DRAC version 1.1.
#'
#' @param data
#'  \link{list}: data object create throught the function \link{template_DRAC4cave}.
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
#' @export use_DRAC4cave


use_DRAC4cave <- function (

  data,

  notification = TRUE
){
  ## ------------------------------------------------------------------------------------------

  if(!is(data,"DRAC4cave.list")){
    stop("[use_DRAC4cave] Error: data has to be create using the 'template_DRAC4cave' function.")
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
    warning("[use_DRAC4cave] Warning: no project name")

  }else if(!is.character(project)){
    stop("[use_DRAC4cave] Error: data$info$project has to be of type 'character'.")
  }

  if(is.null(sample)){
    sample <- "Unknown"
    warning("[use_DRAC4cave] Warning: no sample name")

  }else if(!is.character(sample)){
    stop("[use_DRAC4cave] Error: data$info$sample has to be of type 'character'.")
  }

  if(is.null(mineral)){
    stop("[use_DRAC4cave] Error: mineral unknown")
  }else if(!is.character(mineral)){
    stop("[use_DRAC4cave] Error: data$info$mineral has to be of type 'character'.")
  }else if(!(mineral %in% c("Q", "F", "PM"))){
    stop("[use_DRAC4cave] Error: data$info$mineral can only be 'Q', 'F' or 'PM'.")
  }

  if(is.null(conversion.factors)){
    stop("[use_DRAC4cave] Error: no conversion factors selected")
  }else if(!is.character(conversion.factors)){
    stop("[use_DRAC4cave] Error: data$info$conversion.factors has to be of type 'character'.")
  }else if(!(conversion.factors %in% c("AdamiecAitken1998", "Guerinetal2011", "Liritzisetal2013"))){
    stop("[use_DRAC4cave] Error: data$info$conversion.factors can only be 'AdamiecAitken1998', 'Guerinetal2011' or 'Liritzisetal2013'.")
  }

  if(is.null(int.alpha.attenuation)){
    stop("[use_DRAC4cave] Error: 'data$info$alpha.size.attenuation' is null.")
  }else if(!is.character(int.alpha.attenuation)){
    stop("[use_DRAC4cave] Error: 'data$info$alpha.size.attenuation' is not of type 'character'.")
  }else if(!(int.alpha.attenuation %in% c("Bell1980","Brennanetal1991"))){
    stop("[use_DRAC4cave] Error: 'data$info$alpha.size.attenuation' can only be 'Bell1980' or 'Brennanetal1991'.")
  }

  if(is.null(int.beta.attenuation)){
    stop("[use_DRAC4cave] Error: 'data$info$beta.size.attenuation' is null.")
  }else if(!is.character(int.beta.attenuation)){
    stop("[use_DRAC4cave] Error: 'data$info$beta.size.attenuation' is not of type 'character'.")
  }else if(!(int.beta.attenuation %in% c("Mejdahl1979","Brennan2003", "Guerinetal2012-Q", "Guerinetal2012-F"))){
    stop("[use_DRAC4cave] Error: 'data$info$beta.size.attenuation' can only be 'Mejdahl1979', 'Brennan2003', 'Guerinetal2012-Q' or 'Guerinetal2012-F'.")
  }else if(int.beta.attenuation == "Guerinetal2012-Q" && mineral != "Q"){
    warning("[use_DRAC4cave]  Warning you use 'Guerinetal2012-Q' for 'data$info$beta.size.attenuation' but material is not 'Q'.")
  }else if(int.beta.attenuation == "Guerinetal2012-F" && mineral != "F"){
    warning("[use_DRAC4cave]  Warning you use 'Guerinetal2012-F' for 'info$beta.size.attenuation' but material is not 'F'.")
  }

  if(is.null(int.etch.beta.attenuation)){
    stop("[use_DRAC4cave] Error: 'data$info$beta.etch.attenuation' is null.")
  }else if(!is.character(int.etch.beta.attenuation)){
    stop("[use_DRAC4cave] Error: 'data$info$beta.etch.attenuation' is not of type 'character'.")
  }else if(!(int.etch.beta.attenuation %in% c("Bell1979","Brennan2003"))){
    stop("[use_DRAC4cave] Error: 'data$info$beta.etch.attenuation' can only be 'Bell1979' or 'Brennan2003'.")
  }

  if(is.null(date)){
    date <- as.numeric(format(Sys.Date(),"%Y"))
  }else if(!is.numeric(date)){
    stop("[use_DRAC4cave] Error: 'data$info$date' is not of type 'numeric'.")
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
    warning("[use_DRAC4cave] Warning: data$De$De or data$De$De.err is missing. De and De.err are considered as null")

  }else if(!is.numeric(De) || !is.numeric(De)){
    stop("[use_DRAC4cave] Error: data$De$De has to be of type 'numeric'.")
  }else if(De < 0 || De.err < 0){
    stop("[use_DRAC4cave] Error: data$De$De or data$De$De.err < 0.")
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
    stop("[use_DRAC4cave] Error: data$grain$Dr$alpha or data$grain$Dr$alpha.err is not of type 'numeric'.")
  }else if(int.alpha < 0 || int.alpha.err < 0){
    stop("[use_DRAC4cave] Error: data$grain$Dr$alpha or data$grain$Dr$alpha.err is  < 0.")
  }

  if(is.null(int.beta) || is.null(int.beta.err)){
    int.beta <- "X"
    int.beta.err <- "X"
  }else if(!is.numeric(int.beta) || !is.numeric(int.beta.err)){
    stop("[use_DRAC4cave] Error: data$grain$Dr$beta or data$grain$Dr$beta.err is not of type 'numeric'.")
  }else if(int.beta < 0 || int.beta.err < 0){
    stop("[use_DRAC4cave] Error: data$grain$Dr$beta or data$grain$Dr$beta.err is  < 0.")
  }

  if(is.null(int.gamma) || is.null(int.gamma.err)){
    int.gamma <- "X"
    int.gamma.err <- "X"
  }else if(!is.numeric(int.gamma) || !is.numeric(int.gamma.err)){
    stop("[use_DRAC4cave] Error: data$grain$Dr$gamma or data$grain$Dr$gamma.err is not of type 'numeric'.")
  }else if(int.gamma < 0 || int.gamma.err < 0){
    stop("[use_DRAC4cave] Error: data$grain$Dr$gamma or data$grain$Dr$gamma.err is  < 0.")
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
      stop("[use_DRAC4cave] Error: data$grain$Dr$U or data$grain$Dr$U.err is not of type 'numeric'.")
    }else if(int.U < 0 || int.U.err < 0){
      stop("[use_DRAC4cave] Error: data$grain$Dr$U or data$grain$Dr$U.err is  < 0.")
    }

    if(!is.numeric(int.Th) || !is.numeric(int.Th.err)){
      stop("[use_DRAC4cave] Error: data$grain$Dr$Th or data$grain$Dr$Th.err is not of type 'numeric'.")
    }else if(int.Th < 0 || int.Th.err < 0){
      stop("[use_DRAC4cave] Error: data$grain$Dr$Th or data$grain$Dr$Th.err is  < 0.")
    }

    if(!is.numeric(int.K) || !is.numeric(int.K.err)){
      stop("[use_DRAC4cave] Error: data$grain$Dr$K or data$grain$Dr$K.err is not of type 'numeric'.")
    }else if(int.K < 0 || int.K.err < 0){
      stop("[use_DRAC4cave] Error: data$grain$Dr$K or data$grain$Dr$K.err is  < 0.")
    }

    if(is.null(int.Rb) || is.null(int.Rb.err)){
      int.K2Rb <- "Y"
      int.Rb <- "X"
      int.Rb.err <- "X"

    }else if(!is.numeric(int.Rb) || !is.numeric(int.Rb.err)){
      stop("[use_DRAC4cave] Error: data$grain$Dr$Rb or data$grain$Dr$Rb.err is not of type 'numeric'.")
    }else if(int.Rb < 0 || int.Rb.err < 0){
      stop("[use_DRAC4cave] Error: data$grain$Dr$Rb or data$grain$Dr$Rb.err is  < 0.")
    }else if(is.null(int.K2Rb)){
      int.K2Rb <- "N"
    }else if(!is.logical(int.K2Rb)){
      stop("[use_DRAC4cave] Error: 'data$grain$Dr$K2Rb' is not of type 'logical'.")
    }else if(int.K2Rb){
      int.K2Rb <- "Y"
    }else{
      int.K2Rb <- "N"
    }
  }

  ## ------------------------------------------------------------------------------------------

  # External dose rate
  # By concentration
  ext1.U <- data$sediment$Dr$U
  ext1.U.err <- data$sediment$Dr$U.err
  ext1.Th <- data$sediment$Dr$Th
  ext1.Th.err <- data$sediment$Dr$Th.err
  ext1.K <- data$sediment$Dr$K
  ext1.K.err <- data$sediment$Dr$K.err
  ext1.Rb <- data$sediment$Dr$Rb
  ext1.Rb.err <- data$sediment$Dr$Rb.err

  ext1.K2Rb <- data$sediment$Dr$K2Rb


  # Direct evaluation
  ext1.alpha <- data$sediment$Dr$alpha
  ext1.alpha.err <- data$sediment$Dr$alpha.err
  ext1.beta <- data$sediment$Dr$beta
  ext1.beta.err <- data$sediment$Dr$beta.err
  ext1.gamma <- data$sediment$Dr$gamma
  ext1.gamma.err <- data$sediment$Dr$gamma.err


  ## ------------------------------------------------------------------------------------------
  # Check values

  # Dose rate direct estimation
  if(is.null(ext1.alpha) || is.null(ext1.alpha.err)){
    ext1.alpha <- "X"
    ext1.alpha.err <- "X"
  }else if(!is.numeric(ext1.alpha) || !is.numeric(ext1.alpha.err)){
    stop("[use_DRAC4cave] Error: data$sediment$Dr$alpha or data$sediment$Dr$alpha.err is not of type 'numeric'.")
  }else if(ext1.alpha < 0 || ext1.alpha.err < 0){
    stop("[use_DRAC4cave] Error: data$sediment$Dr$alpha or data$sediment$Dr$alpha.err is  < 0.")
  }

  if(is.null(ext1.beta) || is.null(ext1.beta.err)){
    ext1.beta <- "X"
    ext1.beta.err <- "X"
  }else if(!is.numeric(ext1.beta) || !is.numeric(ext1.beta.err)){
    stop("[use_DRAC4cave] Error: data$sediment$Dr$beta or data$sediment$Dr$beta.err is not of type 'numeric'.")
  }else if(ext1.beta < 0 || ext1.beta.err < 0){
    stop("[use_DRAC4cave] Error: data$sediment$Dr$beta or data$sediment$Dr$beta.err is  < 0.")
  }

  if(is.null(ext1.gamma) || is.null(ext1.gamma.err)){
    ext1.gamma <- "X"
    ext1.gamma.err <- "X"
  }else if(!is.numeric(ext1.gamma) || !is.numeric(ext1.gamma.err)){
    stop("[use_DRAC4cave] Error: data$sediment$Dr$gamma or data$sediment$Dr$gamma.err is not of type 'numeric'.")
  }else if(ext1.gamma < 0 || ext1.gamma.err < 0){
    stop("[use_DRAC4cave] Error: data$sediment$Dr$gamma or data$sediment$Dr$gamma.err is  < 0.")
  }

  # Concentration
  if (is.null(ext1.U) || is.null(ext1.Th) || is.null(ext1.K)){
    ext1.U <- "X"
    ext1.U.err <- "X"
    ext1.Th <- "X"
    ext1.Th.err <- "X"
    ext1.K <- "X"
    ext1.K.err <- "X"
    ext1.Rb <- "X"
    ext1.Rb.err <- "X"

    ext1.K2Rb <- "N"

  }else{
    if(!is.numeric(ext1.U) || !is.numeric(ext1.U.err)){
      stop("[use_DRAC4cave] Error: data$sediment$Dr$U or data$sediment$Dr$U.err is not of type 'numeric'.")
    }else if(ext1.U < 0 || ext1.U.err < 0){
      stop("[use_DRAC4cave] Error: data$sediment$Dr$U or data$sediment$Dr$U.err is  < 0.")
    }

    if(!is.numeric(ext1.Th) || !is.numeric(ext1.Th.err)){
      stop("[use_DRAC4cave] Error: data$sediment$Dr$Th or data$sediment$Dr$Th.err is not of type 'numeric'.")
    }else if(ext1.Th < 0 || ext1.Th.err < 0){
      stop("[use_DRAC4cave] Error: data$sediment$Dr$Th or data$sediment$Dr$Th.err is  < 0.")
    }

    if(!is.numeric(ext1.K) || !is.numeric(ext1.K.err)){
      stop("[use_DRAC4cave] Error: data$sediment$Dr$K or data$sediment$Dr$K.err is not of type 'numeric'.")
    }else if(ext1.K < 0 || ext1.K.err < 0){
      stop("[use_DRAC4cave] Error: data$sediment$Dr$K or data$sediment$Dr$K.err is  < 0.")
    }

    if(is.null(ext1.Rb) || is.null(ext1.Rb.err)){
      ext1.K2Rb <- "Y"
      ext1.Rb <- "X"
      ext1.Rb.err <- "X"

    }else if(!is.numeric(ext1.Rb) || !is.numeric(ext1.Rb.err)){
      stop("[use_DRAC4cave] Error: data$sediment$Dr$Rb or data$sediment$Dr$Rb.err is not of type 'numeric'.")
    }else if(ext1.Rb < 0 || ext1.Rb.err < 0){
      stop("[use_DRAC4cave] Error: data$sediment$Dr$Rb or data$sediment$Dr$Rb.err is  < 0.")
    }else if(is.null(ext1.K2Rb)){
      ext1.K2Rb <- "N"
    }else if(!is.logical(ext1.K2Rb)){
      stop("[use_DRAC4cave] Error: 'data$sediment$Dr$K2Rb' is not of type 'logical'.")
    }else if(ext1.K2Rb){
      ext1.K2Rb <- "Y"
    }else{
      ext1.K2Rb <- "N"
    }
  }

  ## ------------------------------------------------------------------------------------------

  # Rock dose rate
  # By concentration
  ext2.U <- data$sediment$Dr$U
  ext2.U.err <- data$sediment$Dr$U.err
  ext2.Th <- data$sediment$Dr$Th
  ext2.Th.err <- data$sediment$Dr$Th.err
  ext2.K <- data$sediment$Dr$K
  ext2.K.err <- data$sediment$Dr$K.err
  ext2.Rb <- data$sediment$Dr$Rb
  ext2.Rb.err <- data$sediment$Dr$Rb.err

  ext2.K2Rb <- data$sediment$Dr$K2Rb


  # Direct evaluation
  ext2.alpha <- data$rock$Dr$alpha
  ext2.alpha.err <- data$rock$Dr$alpha.err
  ext2.beta <- data$rock$Dr$beta
  ext2.beta.err <- data$rock$Dr$beta.err
  ext2.gamma <- data$rock$Dr$gamma
  ext2.gamma.err <- data$rock$Dr$gamma.err


  ## ------------------------------------------------------------------------------------------
  # Check values

  # Dose rate direct estimation
  if(is.null(ext2.alpha) || is.null(ext2.alpha.err)){
    ext2.alpha <- "X"
    ext2.alpha.err <- "X"
  }else if(!is.numeric(ext2.alpha) || !is.numeric(ext2.alpha.err)){
    stop("[use_DRAC4cave] Error: data$rock$Dr$alpha or data$rock$Dr$alpha.err is not of type 'numeric'.")
  }else if(ext2.alpha < 0 || ext2.alpha.err < 0){
    stop("[use_DRAC4cave] Error: data$rock$Dr$alpha or data$rock$Dr$alpha.err is  < 0.")
  }

  if(is.null(ext2.beta) || is.null(ext2.beta.err)){
    ext2.beta <- "X"
    ext2.beta.err <- "X"
  }else if(!is.numeric(ext2.beta) || !is.numeric(ext2.beta.err)){
    stop("[use_DRAC4cave] Error: data$rock$Dr$beta or data$rock$Dr$beta.err is not of type 'numeric'.")
  }else if(ext2.beta < 0 || ext2.beta.err < 0){
    stop("[use_DRAC4cave] Error: data$rock$Dr$beta or data$rock$Dr$beta.err is  < 0.")
  }

  if(is.null(ext2.gamma) || is.null(ext2.gamma.err)){
    ext2.gamma <- "X"
    ext2.gamma.err <- "X"
  }else if(!is.numeric(ext2.gamma) || !is.numeric(ext2.gamma.err)){
    stop("[use_DRAC4cave] Error: data$rock$Dr$gamma or data$rock$Dr$gamma.err is not of type 'numeric'.")
  }else if(ext2.gamma < 0 || ext2.gamma.err < 0){
    stop("[use_DRAC4cave] Error: data$rock$Dr$gamma or data$rock$Dr$gamma.err is  < 0.")
  }

  # Dose rate base on concentration
  if (is.null(ext2.U) || is.null(ext2.Th) || is.null(ext2.K)){
    ext2.U <- "X"
    ext2.U.err <- "X"
    ext2.Th <- "X"
    ext2.Th.err <- "X"
    ext2.K <- "X"
    ext2.K.err <- "X"
    ext2.Rb <- "X"
    ext2.Rb.err <- "X"

    ext2.K2Rb <- "N"

  }else{
    if(!is.numeric(ext2.U) || !is.numeric(ext2.U.err)){
      stop("[use_DRAC4cave] Error: data$rock$Dr$U or data$rock$Dr$U.err is not of type 'numeric'.")
    }else if(ext2.U < 0 || ext2.U.err < 0){
      stop("[use_DRAC4cave] Error: data$rock$Dr$U or data$rock$Dr$U.err is  < 0.")
    }

    if(!is.numeric(ext2.Th) || !is.numeric(ext2.Th.err)){
      stop("[use_DRAC4cave] Error: data$rock$Dr$Th or data$rock$Dr$Th.err is not of type 'numeric'.")
    }else if(ext2.Th < 0 || ext2.Th.err < 0){
      stop("[use_DRAC4cave] Error: data$rock$Dr$Th or data$rock$Dr$Th.err is  < 0.")
    }

    if(!is.numeric(ext2.K) || !is.numeric(ext2.K.err)){
      stop("[use_DRAC4cave] Error: data$rock$Dr$K or data$rock$Dr$K.err is not of type 'numeric'.")
    }else if(ext2.K < 0 || ext2.K.err < 0){
      stop("[use_DRAC4cave] Error: data$rock$Dr$K or data$rock$Dr$K.err is  < 0.")
    }

    if(is.null(ext2.Rb)){
      ext2.K2Rb <- "Y"
      ext2.Rb <- "X"
      ext2.Rb.err <- "X"

    }else if(!is.numeric(ext2.Rb) || !is.numeric(ext2.Rb.err)){
      stop("[use_DRAC4cave] Error: data$rock$Dr$Rb or data$rock$Dr$Rb.err is not of type 'numeric'.")
    }else if(ext2.Rb < 0 || ext2.Rb.err < 0){
      stop("[use_DRAC4cave] Error: data$rock$Dr$Rb or data$rock$Dr$Rb.err is  < 0.")
    }else if(is.null(ext2.K2Rb)){
      ext2.K2Rb <- "N"
    }else if(!is.logical(ext2.K2Rb)){
      stop("[use_DRAC4cave] Error: 'data$rock$Dr$K2Rb' is not of type 'logical'.")
    }else if(ext2.K2Rb){
      ext2.K2Rb <- "Y"
    }else{
      ext2.K2Rb <- "N"
    }
  }
  ## ------------------------------------------------------------------------------------------

  # grain information

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
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.size.min' or 'data$grain$info$grain.size.max' is null.")
  }else if(!is.numeric(int.size.min) || !is.numeric(int.size.max)){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.size.min' or 'data$grain$info$grain.size.max' is not of type 'numeric'.")
  }else if(int.size.min > int.size.max){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.size.min' > 'data$grain$info$grain.size.max'.")
  }else if(int.size.min < 1){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.size.min' < 1 [um].")
  }else if(int.size.max > 1000){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.size.min' > 1000 [um].")
  }

  if(is.null(int.etch.min) || is.null(int.etch.max)){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.etch.min' or 'data$grain$info$grain.etch.max' is null.")
  }else if(!is.numeric(int.etch.min) || !is.numeric(int.etch.max)){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.etch.min' or 'data$grain$info$grain.etch.max' is not of type 'numeric'.")
  }else if(int.etch.min > int.etch.max){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.etch.min' > 'data$grain$info$grain.etch.max'.")
  }else if(int.etch.min < 0){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.etch.min' < 0 [um].")
  }else if(int.etch.max > 30){
    stop("[use_DRAC4cave] Error: 'data$grain$info$grain.etch.min' > 30 [um].")
  }

  if(is.null(int.a.value) || is.null(int.a.value.err)){
    stop("[use_DRAC4cave] Error: 'data$grain$info$a.value' or 'data$grain$info$a.value.err' is null.")
  }else if(!is.numeric(int.a.value) || !is.numeric(int.a.value.err)){
    stop("[use_DRAC4cave] Error: 'data$grain$info$a.value' or 'data$grain$info$a.value.err' is not of type 'numeric'.")
  }else if(int.a.value < 0){
    stop("[use_DRAC4cave] Error: 'data$grain$info$a.value' < 0 [um].")
  }else if(int.a.value.err < 0){
    stop("[use_DRAC4cave] Error: 'data$grain$info$a.value.err' < 0 [um].")
  }
  ## ------------------------------------------------------------------------------------------

  # 'External' information

  ext1.water <- data$sediment$info$water.content
  ext1.water.err <- data$sediment$info$water.content.err

  ext1.density <- data$sediment$info$density
  ext1.density.err <- data$sediment$info$density.err

  ext1.scale4shallow.depth <- data$sediment$info$scale4shallow.depth

  ## ------------------------------------------------------------------------------------------
  # Check values

  if(is.null(ext1.water) || is.null(ext1.water.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$water.content' or 'data$sediment$info$water.content.err' is null.")
  }else if(!is.numeric(ext1.water) || !is.numeric(ext1.water.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$water.content' or 'data$sediment$info$water.content.err' is not of type 'numeric'.")
  }else if(ext1.water < 0){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$water.content' < 0 [um].")
  }else if(ext1.water.err < 0){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$water.content.err' < 0 [um].")
  }

  if(is.null(ext1.density) || is.null(ext1.density.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$density' or 'data$sediment$info$density.err' is null.")
  }else if(!is.numeric(ext1.density) || !is.numeric(ext1.density.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$density' or 'data$sediment$info$density.err' is not of type 'numeric'.")
  }else if(ext1.density < 0){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$density' < 0 [um].")
  }else if(ext1.density.err < 0){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$density.err' < 0 [um].")
  }

  if(is.null(ext1.scale4shallow.depth)){
    ext1.scale4shallow.depth <- 'N'
    warning("[use_DRAC4cave] Error: 'data$sediment$info$scale4shallow.depth' is null.")

  }else if(!is.logical(ext1.scale4shallow.depth)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$scale4shallow.depth' is not of type 'logical'.")
  }else if(ext1.scale4shallow.depth){
    ext1.scale4shallow.depth <- "Y"
  }else{
    ext1.scale4shallow.depth <- "N"
  }
  ## ------------------------------------------------------------------------------------------

  # rock information

  # ext2 information
  ext2.water <- data$rock$info$water.content
  ext2.water.err <- data$rock$info$water.content.err

  ext2.density <- data$rock$info$density
  ext2.density.err <- data$rock$info$density.err

  ext2.ratio <- data$rock$info$ratio
  ext2.ratio.err <- data$rock$info$ratio.err

  ## ------------------------------------------------------------------------------------------
  # Check values

  if(is.null(ext2.water) || is.null(ext2.water.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$water.content' or 'data$sediment$info$water.content.err' is null.")
  }else if(!is.numeric(ext2.water) || !is.numeric(ext2.water.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$water.content' or 'data$sediment$info$water.content.err' is not of type 'numeric'.")
  }else if(ext2.water < 0){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$water.content' < 0 [um].")
  }else if(ext2.water.err < 0){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$water.content.err' < 0 [um].")
  }

  if(is.null(ext2.density) || is.null(ext2.density.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$density' or 'data$sediment$info$density.err' is null.")
  }else if(!is.numeric(ext2.density) || !is.numeric(ext2.density.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$density' or 'data$sediment$info$density.err' is not of type 'numeric'.")
  }else if(ext2.density < 0){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$density' < 0 [um].")
  }else if(ext2.density.err < 0){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$density.err' < 0 [um].")
  }

  if(is.null(ext2.ratio) || is.null(ext2.ratio.err)){
    ext2.ratio <- 0
    ext2.ratio.err <- 0
  }else if(!is.numeric(ext2.ratio) || !is.numeric(ext2.ratio.err)){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$ratio' or 'data$sediment$info$ratio.err' is not of type 'numeric'.")
  }else if(ext2.ratio < 0){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$ratio' < 0.")
  }else if(ext2.ratio.err < 0){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$ratio.err' < 0.")
  }else if(ext2.ratio > 1){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$ratio' > 1.")
  }else if(ext2.ratio.err > 1){
    stop("[use_DRAC4cave] Error: 'data$sediment$info$ratio.err' > 1.")
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
    stop("[use_DRAC4cave] Error: 'data$cosmic$depth' or 'data$cosmic$depth.err' is not of type 'numeric'.")
  }else if(depth < 0){
    stop("[use_DRAC4cave] Error: 'data$cosmic$depth' < 0 [m].")
  }else if(depth.err < 0){
    stop("[use_DRAC4cave] Error: 'data$cosmic$depth.err' < 0 [m].")
  }

  if(is.null(latitude) || is.null(longitude) || is.null(altitude)){
    latitude <- "X"
    longitude <- "X"
    altitude <- "X"

  }else if(!is.numeric(latitude) || !is.numeric(longitude) || !is.numeric(altitude) ){
    stop("[use_DRAC4cave] Error: 'data$cosmic$latitude', 'data$cosmic$longitude' or 'data$cosmic$altitude' is not of type 'numeric'.")
  }else if(latitude < -90 || latitude > 90){
    stop("[use_DRAC4cave] Error: 'data$cosmic$latitude' has to be between -90\u00b0 and +90\u00b0.")
  }else if(longitude < -180 || longitude > 180){
    stop("[use_DRAC4cave] Error: 'data$cosmic$longitude' has to be between -180\u00b0 and +180\u00b0.")
  }else if(altitude > 5000){
    stop("[use_DRAC4cave] Error: 'data$cosmic$altitude' has to be < 5000 m.")
  }

  if(is.null(cosmic) || is.null(cosmic.err)){
    cosmic <- "X"
    cosmic.err <- "X"
  }else if(!is.numeric(cosmic) || !is.numeric(cosmic.err)){
    stop("[use_DRAC4cave] Error: 'data$cosmic$Dr' or 'data$cosmic$Dr.err' is not of type 'numeric'.")
  }else if(cosmic < 0){
    stop("[use_DRAC4cave] Error: 'data$cosmic$Dr' < 0 [Gy].")
  }else if(cosmic.err < 0){
    stop("[use_DRAC4cave] Error: 'data$cosmic$Dr.err' < 0 [Gy].")
  }

  if(is.null(corr.fieldChanges)){
    corr.fieldChanges <- FALSE
  }else if(!is.logical(corr.fieldChanges)){
    stop("[use_DRAC4cave] Error: 'data$cosmic$corr.fieldChanges' is not of type 'logical'.")
  }

  ## ------------------------------------------------------------------------------------------
  # First run, Internal & External dose (grain)

  run1.input <- template_DRAC(notification = notification)

  run1.input$`Project ID` <- project
  run1.input$`Sample ID` <- sample
  run1.input$Mineral <- mineral
  run1.input$`Conversion factors` <- conversion.factors

  run1.input$`Internal U (ppm)` <-  int.U
  run1.input$`errInternal U (ppm)` <- int.U.err
  run1.input$`Internal Th (ppm)` <- int.Th
  run1.input$`errInternal Th (ppm)` <- int.Th.err
  run1.input$`Internal K (%)` <- int.K
  run1.input$`errInternal K (%)` <- int.K.err
  run1.input$`Rb (ppm)` <- int.Rb
  run1.input$`errRb (ppm)` <- int.Rb.err
  run1.input$`Calculate internal Rb from K conc?`<- int.K2Rb

  run1.input$`ExternalU (ppm)` <-  ext1.U
  run1.input$`errExternal U (ppm)` <- ext1.U.err
  run1.input$`External Th (ppm)` <- ext1.Th
  run1.input$`errExternal Th (ppm)` <- ext1.Th.err
  run1.input$`External K (%)` <- ext1.K
  run1.input$`errExternal K (%)` <- ext1.K.err
  run1.input$`External Rb (ppm)` <- ext1.Rb
  run1.input$`errExternal Rb (ppm)` <- ext1.Rb.err
  run1.input$`Calculate external Rb from K conc?` <- ext1.K2Rb

  run1.input$`Grain size min (microns)` <- int.size.min
  run1.input$`Grain size max (microns)` <- int.size.max

  run1.input$`alpha-Grain size attenuation` <- int.alpha.attenuation
  run1.input$`beta-Grain size attenuation ` <- int.beta.attenuation

  run1.input$`Etch depth min (microns)` <- int.etch.min
  run1.input$`Etch depth max (microns)` <- int.etch.max

  run1.input$`beta-Etch depth attenuation factor` <- int.etch.beta.attenuation

  run1.input$`a-value` <- int.a.value
  run1.input$`erra-value` <- int.a.value.err

  run1.input$`Water content ((wet weight - dry weight)/dry weight) %` <- ext1.water
  run1.input$`errWater content %` <- ext1.water.err

  run1.input$`Depth (m)` <- depth
  run1.input$`errDepth (m)` <- depth.err

  run1.input$`Overburden density (g cm-3)` <- ext2.density
  run1.input$`errOverburden density (g cm-3)` <- ext2.density.err

  run1.input$`Latitude (decimal degrees)` <- latitude
  run1.input$`Longitude (decimal degrees)` <- longitude
  run1.input$`Altitude (m)` <- altitude

  run1.input$`De (Gy)` <- De
  run1.input$`errDe (Gy)` <- De.err

  run1.output <- use_DRAC(run1.input,verbose=notification)

  # --------------------------------------------------------------------------------------------------------------------------

  # Second run, Evironmental dose (sediment)

  run2.input <- template_DRAC(notification = notification)

  run2.input$`Project ID` <- project
  run2.input$`Sample ID` <- sample
  run2.input$Mineral <- mineral
  run2.input$`Conversion factors` <- conversion.factors

  run2.input$`Internal U (ppm)` <-  int.U
  run2.input$`errInternal U (ppm)` <- int.U.err
  run2.input$`Internal Th (ppm)` <- int.Th
  run2.input$`errInternal Th (ppm)` <- int.Th.err
  run2.input$`Internal K (%)` <- int.K
  run2.input$`errInternal K (%)` <- int.K.err
  run2.input$`Rb (ppm)` <- int.Rb
  run2.input$`errRb (ppm)` <- int.Rb.err
  run2.input$`Calculate internal Rb from K conc?`<- int.K2Rb

  run2.input$`ExternalU (ppm)` <-  ext2.U
  run2.input$`errExternal U (ppm)` <- ext2.U.err
  run2.input$`External Th (ppm)` <- ext2.Th
  run2.input$`errExternal Th (ppm)` <- ext2.Th.err
  run2.input$`External K (%)` <- ext2.K
  run2.input$`errExternal K (%)` <- ext2.K.err
  run2.input$`External Rb (ppm)` <- ext2.Rb
  run2.input$`errExternal Rb (ppm)` <- ext2.Rb.err
  run2.input$`Calculate external Rb from K conc?` <- ext2.K2Rb

  run2.input$`Grain size min (microns)` <- int.size.min
  run2.input$`Grain size max (microns)` <- int.size.max

  run2.input$`alpha-Grain size attenuation` <- int.alpha.attenuation
  run2.input$`beta-Grain size attenuation ` <- int.beta.attenuation

  run2.input$`Etch depth min (microns)` <- int.etch.min
  run2.input$`Etch depth max (microns)` <- int.etch.max

  run2.input$`beta-Etch depth attenuation factor` <- int.etch.beta.attenuation

  run2.input$`a-value` <- int.a.value
  run2.input$`erra-value` <- int.a.value.err

  run2.input$`Water content ((wet weight - dry weight)/dry weight) %` <- ext2.water
  run2.input$`errWater content %` <- ext2.water.err

  run2.input$`Depth (m)` <- depth
  run2.input$`errDepth (m)` <- depth.err

  run2.input$`Overburden density (g cm-3)` <- ext2.density
  run2.input$`errOverburden density (g cm-3)` <- ext2.density.err

  run2.input$`Latitude (decimal degrees)` <- latitude
  run2.input$`Longitude (decimal degrees)` <- longitude
  run2.input$`Altitude (m)` <- altitude

  run2.input$`De (Gy)` <- De
  run2.input$`errDe (Gy)` <- De.err

  run2.output <- use_DRAC(run2.input,verbose=notification)

  # --------------------------------------------------------------------------------------------------------------------------

  # Combining the 2 runs...


  temp.int.alpha <- as.numeric(run1.output$DRAC$highlights$`Internal Dry alphadoserate (Gy.ka-1)`[1])
  temp.int.alpha.err <- as.numeric(run1.output$DRAC$highlights$`Internal Dry erralphadoserate (Gy.ka-1)`[1])

  temp.int.beta <- as.numeric(run1.output$DRAC$highlights$`Internal Dry betadoserate (Gy.ka-1)`[1])
  temp.int.beta.err <- as.numeric(run1.output$DRAC$highlights$`Internal Dry errbetadoserate (Gy.ka-1)`[1])

  temp.ext1.alpha <- as.numeric(run1.output$DRAC$highlights$`Water corrected alphadoserate`[1])
  temp.ext1.alpha.err <- as.numeric(run1.output$DRAC$highlights$`Water corrected erralphadoserate`[1])

  temp.ext1.beta <- as.numeric(run1.output$DRAC$highlights$`Water corrected betadoserate`[1])
  temp.ext1.beta.err <- as.numeric(run1.output$DRAC$highlights$`Water corrected errbetadoserate`[1])

  temp.ext1.gamma <- as.numeric(run1.output$DRAC$highlights$`Water corrected gammadoserate (Gy.ka-1)`[1])
  temp.ext1.gamma.err <- as.numeric(run1.output$DRAC$highlights$`Water corrected errgammadoserate (Gy.ka-1)`[1])

  temp.ext2.alpha <- as.numeric(run2.output$DRAC$highlights$`Water corrected alphadoserate`[1])
  temp.ext2.alpha.err <- as.numeric(run2.output$DRAC$highlights$`Water corrected erralphadoserate`[1])

  temp.ext2.beta <-  as.numeric(run2.output$DRAC$highlights$`Water corrected betadoserate`[1])
  temp.ext2.beta.err <- as.numeric(run2.output$DRAC$highlights$`Water corrected errbetadoserate`[1])

  temp.ext2.gamma <- as.numeric(run2.output$DRAC$highlights$`Water corrected gammadoserate (Gy.ka-1)`[1])
  temp.ext2.gamma.err <- as.numeric(run2.output$DRAC$highlights$`Water corrected errgammadoserate (Gy.ka-1)`[1])

  temp.cosmic <- as.numeric(run1.output$DRAC$highlights$`Cosmicdoserate (Gy.ka-1)`[1])
  temp.cosmic.err <- as.numeric(run1.output$DRAC$highlights$`errCosmicdoserate (Gy.ka-1)`[1])

  ratio <- ext2.ratio
  ratio.err <- ext2.ratio.err

  temp.ext.alpha <- temp.ext1.alpha
  temp.ext.alpha.err <- temp.ext1.alpha.err

  temp.ext.beta <- temp.ext1.beta
  temp.ext.beta.err <- temp.ext1.beta.err

  corr.ext1.gamma <- (1-ext2.ratio)*temp.ext1.gamma
  corr.ext1.gamma.err <- sqrt((ext2.ratio.err/ext2.ratio)^2 + (temp.ext1.gamma.err(temp.ext1.gamma))^2)*corr.ext1.gamma

  corr.ext2.gamma <- ext2.ratio*temp.ext2.gamma
  corr.ext2.gamma.err <- sqrt((ext2.ratio.err/ext2.ratio)^2 + (temp.ext1.gamma.err(temp.ext1.gamma))^2)*corr.ext2.gamma

  temp.env.gamma <-  corr.ext1.gamma + corr.ext2.gamma
  temp.env.gamma.err <- sqrt( corr.ext1.gamma.err^2 + corr.ext2.gamma.err^2)
  # --------------------------------------------------------------------------------------------------------------------------

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
                                  density = ext2.density,
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

  message.project <- paste("For the sample", sample, "of the project", project)
  message.De <- paste("The equivalent dose is:", round(De,3), "\u00b1", round(De.err,3), "Gy.")
  message.Dr <- paste("The dose rate is:", round(R.Dr,3), "\u00b1", round(R.Dr.err,3), "Gy/ka.")
  message.Age <- paste("The age is estimated as:", round(R.age,3), "\u00b1", round(R.age.err,3), "ka.")

  if(age.CE > 0){
    message.CE <- paste("The heating of the sediment occured around", round(age.CE), "\u00b1", round(age.CE.err), "CE.")
  }else if(abs(age.CE)<100){
    message.CE <- paste("The heating of the sediment occured around", round(abs(age.CE),-1), "\u00b1", round(age.CE.err,-1), " BCE.")
  }else if(abs(age.CE)<1000){
    message.CE <- paste("The heating of the sediment occured around", round(abs(age.CE),-2), "\u00b1", round(age.CE.err,-2), " BCE.")
  }else{
    message.CE <- paste("The heating of the sediment occured around", round(abs(age.CE),-3), "\u00b1", round(age.CE.err,-3), " BCE.")
  }

  output <- paste("\t [use_DRAC4cave] \n ",
                  "\t \n",
                  paste("\t", message.project, "\n"),
                  "\t --------------------------------------------------------- \n ",
                  paste("\t", message.De, "\n"),
                  paste("\t", message.Dr, "\n"),
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

#' @title Modelling Exponential Exposure Distribution
#'
#' @description Modelling incomplete and heterogeneous bleaching of mobile grains partially
#' exposed to the light, an implementation of the EED model proposed by Guibert et al. (2019)
#'
#' @details The function is an implementation and enhancement of the scripts used for
#' the work by Guibert et al. (2019).
#'
#'
#' **Method control parameters**
#'
#' \tabular{llll}{
#'  **ARGUMENT** \tab **FUNCTION** \tab **DEFAULT** \tab **DESCRIPTION**\cr
#'  `lower`  \tab  [DEoptim::DEoptim] \tab `c(0,0)` \tab set lower bounds for kappa and sigma auto
#'  parameter finding \cr
#'  `upper` \tab  [DEoptim::DEoptim] \tab `c(1000,2)` \tab set upper bounds for kapp and sigma
#'  auto parameter finding \cr
#'  `itermax` \tab [DEoptim::DEoptim.control] \tab `500` \tab maximum number for iterations to
#'  find the parameters \cr
#'  `VTR` \tab [DEoptim::DEoptim.control] \tab 1e-05 \tab stop value to reached for the
#'  optimisation \cr
#'  `trace` \tab [DEoptim::DEoptim.control] \tab `FALSE` \tab enable/disable travce mode \cr
#'  `trave_plot` \tab - \tab `FALSE` \tab enable/disable additional trace plot output \cr
#'
#' }
#'
#' @param data [data.frame] (**required**): input data consisting of two columns, the De and the
#' SE(De). Values are expected in Gy
#'
#' @param D0 [integer] (*with default*): D0 value, defining the characteristation behaviour
#' of the the quartz. Value are expected in Gy
#'
#' @param expected_dose [integer] (): TODO
#'
#' @param kappa [numeric] (*optional*): positive dimensionless exposure parameter
#' characterising the bleaching state of the grains. Low values (< 10) indicate
#' poor bleaching
#'
#' @param sigma_distr [numeric] (*optional*): positive dose rate parameter, representing the
#' dose variability to which the grains were exposed ##TODO perhaps it should be renamed
#'
#' @param n.simul [integer] (*with default*): number of simulations
#'
#' @param n.minSimExp [integer] (*with default*): number of MC runs for calculating the uncertainty
#' contribution from the sampling
#'
#' @param sample_name [character] (*with default*): name of the sample
#'
#' @param method_control [list] (*with default*): additional deep control parameters, parameters
#' need to be provided as names list, see details
#'
#' @param verbose [logical] (with default): enable/disable verbose mode
#'
#' @param plot [logical] (with default): enable/disable plot output
#'
#' @param ... further parameters that can be passed to better control the plot output. Support arguments
#' are `xlab`, `xlim`.
#'
#' @author Pierre Guibert, IRAMAT-CRP2A, UMR 5060, Université Bordeaux Montaigne (France),
#' Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, Université Bordeaux Montaigne (France)
#'
#' @section Function version: 0.1.0
#'
#' @references Guibert, P., Christophe, C., Urbanova, P., Guérin, G., Blain, S., 2017.
#' Modeling incomplete and heterogeneous bleaching of mobile grains partially exposed to the
#' light - Towards a new tool for single grain OSL dating of poorly bleached mortars.
#' Radiation Measurements 107, 48–57. \doi{10.1016/j.radmeas.2017.10.003}
#'
#' @seealso [DEoptim::DEoptim], [RLum.Results-class], [calc_MinDose], [calc_FuchsLang2001], [calc_IEU],
#' [calc_FiniteMixture]
#'
#' @keywords datagen
#'
#' @examples
#'
#' ##TODO
#'
#' @md
#' @export
calc_EED_Model <- function(
  data,
  D0 = 120L,
  expected_dose = 12L,
  kappa = NULL,
  sigma_distr = NULL,
  n.simul = 5000L,
  n.minSimExp = 50L,
  sample_name = "",
  method_control = list(),
  verbose = TRUE,
  plot = TRUE,
  ...

){


  ##TODO
  ## add docu examples
  ## add docu details
  ##
  ## TODO Questions to Pierre
  ##  - what the user is doing of the real dose/age is not known, can it be assesed?
  ##  - the underlying assumption is obviously an exponential distribution, what
  ##  - so far the parameter uncertainty estimation does not work nicely.x
  ##  happens to data where this is not observd? I cannot work?

# Integrity tests  ----------------------------------------------------------------------------

  ##check input data and make it a hard stop
  if(missing(data) || class(data) != 'data.frame')
    stop("[calc_EED_Model()] 'data' needs to be a two column data.frame, cf. manual!", call. = FALSE)


# Helper functions ----------------------------------------------------------------------------

  ##the helper functions base on ode by Pierre, each helper was only a little bit
  ##optimised and then tested separatly

  # Calcul de la variance du plateau sur la base des doses arch?o
  # corrig?es de la dose r?siduelle uniquement (modif 31.5.2018)
  .calc_Plateau_Variance <- function(M_Data, MinDose_Index, MaxDose_Index){
    var_ratio <- var(M_Data[MinDose_Index:MaxDose_Index, 3]) # doses nettes corrig?es r?siduel uniquement
    mean_ratio <- mean(M_Data[MinDose_Index:MaxDose_Index, 3])
    return(var_ratio / (mean_ratio ^ 2))
  }

  # Calcul de la variance du plateau      modifi? le 31.5.2018
  # sur la base des rapport observ?/simul? des doses totales brutes
  .calc_Plateau_Variance_uncorr <- function (M_Data, MinDose_Index, MaxDose_Index){
    var_ratio <-
      var (M_Data[MinDose_Index:MaxDose_Index, 4]) # ratio observ?/simul? brut
    mean_ratio <- mean(M_Data[MinDose_Index:MaxDose_Index, 4])
    return (var_ratio / (mean_ratio ^ 2))
  }

  .EED_Simul_Matrix <- function (M_Simul, Expected_Dose, sigma_distr,D0, kappa, Iinit, Nsimul){
    ## g?n?re une liste de Nsimul valeurs distribu?es selon une loi log normale de moyenne Expected_Dose ##
    M_Simul[,1] <- Expected_Dose * exp(sigma_distr * rnorm(Nsimul)) *
      exp(-0.5 * (sigma_distr ^ 2))

    ## g?n?re une liste de Nsimul valeurs de dose r?siduelle selon une distribution expoentielle de l'exposition ##
    M_Simul[, 2] <-
      (-D0 * log(1 - Iinit * exp(-stats::rexp(Nsimul, rate = 1 / kappa))))

    ## g?n?re la liste des doses individuelles ##
    M_Simul[,3] <- M_Simul[,1] + M_Simul[,2]

    ## g?n?re la liste class?e des doses individuelles ##
    M_Simul[,4] <- sort(M_Simul[,3], decreasing = FALSE)

    ## g?n?re une liste d'index and sort it
    index <- sort.list(rank(M_Simul[,3], ties.method = "first"))

    ## la liste d'index est utilis?e pour remettre les colonnes 1 et 2 dans l'ordre du classement des doses totales ##
    M_Simul[,5:6] <- M_Simul[index,1:2]

    ##calculate cumulative mean values
    M_Simul[,7:9] <- matrixStats::colCumsums(M_Simul[,4:6]) / seq_len(Nsimul)

    ##return value
    return(M_Simul)
  }


  #  Calcul de la matrice de M_Data : donnees experimentales traitees
  .EED_Data_Matrix <- function(M_Data, Dosedata, M_Simul, Expected_Dose, Ndata, Nsimul){
    ##set index
    index_SimRes <- as.integer((1:Ndata)*Nsimul/Ndata)

    # recopie Dosedata dans les colonnes 9 et 10
    M_Data[, 9] <- Dosedata[, 1]
    M_Data[, 10] <- Dosedata[, 2]

    #colonne 1 : moyenne cumulative des ED individuels
    M_Data[, 1] <- cumsum(Dosedata[, 1]) / (1:Ndata)

    # colonne 2 : incertitude statistique sur les valeurs de la moyenne cumulative
    M_Data[, 2] <- (1 / (1:Ndata)) * sqrt(cumsum(Dosedata[, 2] ^ 2))

    # colonne 3 : dose nette : dose moyenne corrigee de la moyenne des doses residuelles
    M_Data[, 3] <- M_Data[, 1] - M_Simul[index_SimRes, 9]

    # colonne 4 : ratio observed / simulated of uncorrected doses
    M_Data[, 4] <- M_Data[, 1] / M_Simul[index_SimRes, 7]

    # colonne 5 :erreur stat sur les rapports
    M_Data[, 5] <- M_Data[, 2] / M_Simul[index_SimRes, 7]

    # par(mfrow = c(3,1))
    # matplot(Dosedata[,1],  M_Data[,4], type = "p", pch = 1, log = "x")
    # matplot(Dosedata[,1],  M_Data[,5], type = "p", pch = 1, log = "x")

    # colonne 6 : dose nette corrigee des r?siduels ET de l'int?gration partielle de la
    # distribution log-normale des burial doses
    M_Data[, 6] <- M_Data[, 3] * (Expected_Dose / M_Simul[index_SimRes, 8])

    # colonne 7 : erreur sur dose nette corrigee (premier calcul :
    # incertitude uniquement bas?e sur erreur exp?riementale)
    M_Data[, 7] <- abs((M_Data[, 6] / M_Data[, 3]) * M_Data[, 2])

    # colonne 8 : ratio dose residuelle / dose nette corrigee
    M_Data[M_Data[, 6] != 0, 8] <- M_Simul[index_SimRes, 9] / M_Data[, 6]
    M_Data[M_Data[, 6] == 0, 8] <- 0

    return(M_Data)
  }

  .Initial_State_of_OSL <- function(Dosedata, D0, method){
    SaturationState <-  (1 - exp(-max(Dosedata[, 1]) / D0))
    if (method == "max") {return(SaturationState)}
    if( !is.numeric(method)) {return(1)}
    if (method >1 | method<0)  {return(1)}
    if (method < SaturationState) {return(SaturationState)}
    if (method > SaturationState) {return(method)}
  }


  # Calcul des incertitudes statistiques liees au tirage des Ndata grains              #
  # calcul incertitude statistique li?e au nombre limit? de grains
  #
  # On se base sur un tirage de Ndata donn?es ? partir de la matrice de simlation
  # on choisit les Ndata premi?res donn?es, on calcule les moyennes cumulatives de dose totale
  # on prend les Ndata suivantes etc de telle sorte ? ce qu'on base le calcul d'?cart-type sur au moins 50 tirages
  # Pour cela on va calculer le nb d'exp?riences simul?es possibles ? partir du rapport Nsimul/Ndata
  # on teste ce rapport et s'il est inf?rieur ? une valeur limite inf?rieure, on demande de relancer le calcul
  # avec davantage de simulations.
  .EED_Calc_Overall_StatUncertainty <- function (M_Data, M_Simul, Ndata, Nsimul){
    Nsimexp <- as.integer(Nsimul/Ndata)

    ##initialise matrix matrix
    M_SimExpResults <- matrix(nrow = Ndata, ncol = 2)

    if (Nsimexp > MinNbSimExp){
      M_StoreData <- matrix(nrow = Ndata, ncol = Nsimexp)
      M_CurSimExp <- matrix(nrow = Ndata, ncol = 2)

      ##initialise values (outsite of the loop)
      cur_mean_id <- numeric(length = Ndata)

      for (j in 1:Nsimexp){
        ##fill matrix
        M_CurSimExp[,1] <- M_Simul[(1+(j-1)*Ndata):(j*Ndata),3]
        M_CurSimExp[,2] <- sort(M_CurSimExp[,1], decreasing = FALSE, method = "quick")

        ##calculate cummulative mean
        cur_mean <- cumsum(M_CurSimExp[, 2]) / (1:Ndata)

        # on cherche l'indice ic correspondant a la valeur de la moyenne brute
        # et on extrait de la matrice de simulation la valeur de la moyenne des doses résiduelles
        ##this loop does not look much efficient, but it is, if we use the usual way,
        ##we hand the full object which can take very long, here we can stop after the
        ##mean is greater than the reference value

        #find index of first larger values, using the C++ code gives an order
        #of magnitude speed plus
        cur_mean_id <- src_find_first_larger_value(x = cur_mean, y = M_Simul[,7])

        ##store data in matrix
        M_StoreData[, j] <- cur_mean - M_Simul[cur_mean_id, 9]

      }

      ##calculate rowMeans and row standard deviations
      M_SimExpResults[,1] <- rowMeans(M_StoreData)
      M_SimExpResults[,2] <- matrixStats::rowSds(M_StoreData)

    }##end if

    if (Nsimexp<MinNbSimExp)
      print("pas assez de simulations, merci d'augmenter Nsimul")

    # colonne 7 : calcule l'erreur totale sur la dose nette corrig?e M_Data[i,7]
    M_Data[,7] <- abs((M_Data[,6]/M_Data[,3])*sqrt((M_SimExpResults[,2]^2)+(M_Data[,2]^2)))

    return (M_Data)
  }


  # fonction d'initialisation de l'etat initial
  # si absence de valeur, par defaut prend comme etat initial la valeur 1 (saturation)
  # MaJ le 9 novembre 2018
  .Initial_State_of_OSL <- function(Dosedata, D0, method) {
    SaturationState = (1 - exp(-max(Dosedata[, 1]) / D0))
    if (method == "max") {
      return  (SaturationState)
    }
    if (!is.numeric(method)) {
      return(1)
    }
    if (method > 1 | method < 0)  {
      return(1)
    }
    if (method < SaturationState) {
      return (SaturationState)
    }
    if (method > SaturationState) {
      return(method)
    }
  }

  # fonction permettant de retourner l'indice de la valeur minimale de la  dose individuelle
  # prise en compte pour le calcul du plateau
  .Get_Plateau_MinDoseIndex <-
    function(M_data, Ndata, MinIndivDose) {
      if (MinIndivDose == "all") {
        return(1)

      } else {
        if (is.numeric(MinIndivDose)) {
          current_index <- 1
          while ((MinIndivDose > M_Data[current_index, 9]) &
                 (current_index < Ndata)) {
            current_index <- current_index + 1
          }
        }
        return(current_index)
      }
    }


  # fonction permettant de retourner l'indice de la valeur maximale de la  dose individuelle
  # prise en compte pour le calcul du plateau
  .Get_Plateau_MaxDoseIndex <- function(M_data, Ndata, MaxIndivDose) {
    if (MaxIndivDose == "all") {
      return(Ndata)
    }
    else {
      if (is.numeric(MaxIndivDose))
      {
        current_index = 1
        while ((MaxIndivDose > M_Data[current_index, 9]) &
               (current_index < Ndata)) {
          current_index = current_index + 1
        }
      }
      return(current_index)
    }

  }

  # # allow automated kapp and sigma_distr parameter estimation
  .guess_EED_parameters <- function(
    Expected_Dose, D0, Iinit, Nsimul, Dosedata, M_Data, M_Simul, Ndata,
    method_control_intern = method_control){

    ##settings to control the what needs to be controlled
    method_control <- modifyList(
      list(
        lower = c(0, 0),
        upper = c(1000, 2),
        itermax = 500,
        VTR = 1e-05,
        trace = FALSE,
        trace_plot = FALSE
      ),
      method_control_intern)

    ##define function for the differential evolution
    fn <- function(x, M_Simul, Expected_Dose, D0, Iinit, Nsimul, Dosedata, M_Data, Ndata){
      kappa <- x[1]
      sigma_distr <- x[2]
      M_Simul <- .EED_Simul_Matrix (M_Simul, Expected_Dose, sigma_distr, D0, kappa, Iinit, Nsimul)
      M_Data <- .EED_Data_Matrix(M_Data, Dosedata, M_Simul, Expected_Dose, Ndata, Nsimul)
      M_Data <- .EED_Calc_Overall_StatUncertainty(M_Data = M_Data,
        M_Simul = M_Simul, Ndata = Ndata, Nsimul = Nsimul)

      .calc_Plateau_Variance_uncorr(M_Data, MinDose_Index = 1, MaxDose_Index = nrow(Dosedata))

    }

    ##run differential evolution
    o <- DEoptim::DEoptim(
      fn,
      lower = method_control$lower,
      upper = method_control$upper,
      control = DEoptim::DEoptim.control(
        VTR = method_control$VTR,
        itermax = method_control$itermax,
        c = 0.5,
        strategy = 6,
        trace = method_control$trace
      ),
      M_Simul = M_Simul,
      Expected_Dose = Expected_Dose,
      D0 = D0,
      Iinit = Iinit,
      Nsimul = Nsimul,
      Dosedata = Dosedata,
      M_Data = M_Data,
      Ndata = Ndata
    )

    ##additional control plots
    if(method_control$trace_plot){
      par(mfrow = c(2,1))
      plot(o$member$bestmemit, type = "b", xlab = "kapp", ylab = "sigma")
      plot(o$member$bestvalit, type = "b", xlab = "Run index", ylab = "Variance")
      summary(o)

    }

    return(c(
      kappa = o$optim$bestmem[1],
      sigma_distr = o$optim$bestmem[2],
      min_var = o$optim$bestval))

  }

# Input data ----------------------------------------------------------------------------------

##some parameters are rewritten the leave the code by Pierre untouched

Dosedata <- data[,1:2]
Ndata <- nrow(Dosedata)

# Nsimul est le nb de tirages de la variable al?atoire qui va g?n?rer Nsimul valeurs de dose résiduelle
# et Nsimul valeurs de dose arch?ologique (burial dose), donc Nsimul valeurs de dose equivalente
# individuelle. Pour une approche finale et précise de la dose moyenne, on peut monter ce paramétre ?
# des valeurs hautes : 100000 par exemple.
# our une premiére approche rapide d'un échantillon inconnu, il faut limiter les temps de calculs
# pour explorer l'espace kappa, sigma dans lequel on va trouver les param?tres optimaux.
# On peut proposer alors une valeur minimale par défaut de 5000 simulations
# Le mieux est de laisser votre libert? de choix et d'analyse ...
Nsimul <- n.simul

# MinNbSimExp définit le nombre de tirages minimal pour calculer l'incertitude d'echantillonnage,
# 2e membre de l'incertitude statistique globale. J'ai pense que 50 valeurs pour calculer un écart-type ?
# tait le minimum. ce paramétre ajuste le nombre de simulations en l'augmentant ? une valeur
# superieure au produit du nb de donnees et du nombre de tirages minimal au cas où Nsimul est très
# faible. Si l'on souhaite vraiment faire une étude rapide on peut diminuer MinNbSimExp.
# Pensez cependant ? conserver en commentaires cette instruction por remettre ? jour
# le code une fois l'exploration faite :
# MinNbSimExp = 50
MinNbSimExp <- n.minSimExp

if(MinNbSimExp * Ndata > Nsimul)
  Nsimul <- (MinNbSimExp + 1) * Ndata

## valeur en Gy de la dose de saturation selon une loi de croissance exponentielle saturante ##
D0 <- D0 #valeur en Gy

## introduire la dose attendue en Gy ##
Expected_Dose <- expected_dose #2.7

## initialise la valeur de l'intensit? initiale (1 = saturation) ##
#si le param?tre method est "max", l'initialisation se fait selon Iinit = 1-exp(-max(Dosedata[,1])/D0)
# sinon si l'on donne une valeur comprise entre 0 et 1, la valeur introduite est prise en compte,
# sinon on affecte 1 dans tous les autres cas
# Iinit = 1 #par exemple
# ATTENTION / aucun test de coh?rence n'est fait pour savoir
# si la valeur num?rique introduite est compatible ou non avec les donn?es

Iinit <- .Initial_State_of_OSL(Dosedata, D0, "max")
# Iinit = .Initial_State_of_OSL(Dosedata, D0, method = 1)

## on définit une matrice dans laquelle on va stocker et utiliser les données de la simulation ##
M_Simul <- matrix(nrow = Nsimul, ncol = 9)

# génère une matrice à partir des donnés expérimentales #
M_Data <- matrix(nrow = Ndata, ncol = 10)
colnames(M_Data) <- c("CUM_MEAN_DE", "CUM_MEAN_DE_X", "NET_CUM_MEAN_DE_NET", "NET_CUM_MEAN_DE_X",
                      "RATIO_DE_SIM", "RATIO_DE_SIM_X", "INT_NET_DOSE", "INT_NET_DOSE_X",
                      "DE", "DE_X")

# Guess parameters if needed ------------------------------------------------------------------
if(verbose) cat("\n[calc_EED_Model()]\n")

## introduire le facteur d'eclairement kappa : kappa = 2 : très faible éclairement, kappa >100 bon
##  blanchiment ##
## TODO - this is not really what Pierre had in mind, he wanted to have the variance, not an automated
## esstimation
if(is.null(kappa) || is.null(sigma_distr)){

  if(verbose)
    cat("\n>> Running differential evolution optimization to find 'kappa' and 'sigma' ... \n")

  temp_guess <- .guess_EED_parameters(
    Expected_Dose = Expected_Dose,
    D0 = D0,
    Iinit = Iinit,
    Nsimul = Nsimul,
    Dosedata = Dosedata,
    M_Data = M_Data,
    M_Simul = M_Simul,
    Ndata = Ndata
    )

  kappa <- temp_guess[1]
  sigma_distr <- temp_guess[2]
  min_var <- temp_guess[3]

  if(verbose){
    cat(">> min. variance:", min_var)
    cat("\n>> kappa: ", kappa, " | sigma: ",sigma_distr)
  }

}

# Calculation ---------------------------------------------------------------------------------

M_Simul <- .EED_Simul_Matrix (M_Simul, Expected_Dose, sigma_distr,D0, kappa, Iinit, Nsimul)
M_Data <- .EED_Data_Matrix(M_Data, Dosedata, M_Simul, Expected_Dose, Ndata, Nsimul)
M_Data <- .EED_Calc_Overall_StatUncertainty(M_Data = M_Data, M_Simul = M_Simul, Ndata = Ndata, Nsimul = Nsimul)

max_dose_simul <- max(M_Simul[,3])
index_min_uncert <- sort.list(index_min_uncert <-
              rank(M_Data[, 7], ties.method = "first"))


# Terminal output -----------------------------------------------------------------------------
if(verbose){
cat("\n------------------------------------ \n")

cat("\n Maximal Individual Equivalent Dose integrated: ", round(M_Data[index_min_uncert[1],9],2), "Gy")
cat("\n Averaged Corrected Equivalent Dose: ",
    round(M_Data[index_min_uncert[1],6],2), "\u00b1", round(M_Data[index_min_uncert[1],7],2), "Gy")


}
##################################################################################################################
#### THE ONE SHOT : calcule les valeurs de dose moyenne

##TODO discuss with Pierre

# "recherche du minimum d'incertitude statistique"
# #min(M_Data[,7])
# index_min_uncert <- rank(M_Data[,7], ties.method = "first")
# index_min_uncert<-sort.list(index_min_uncert)
# "Minimal error for maximal equivalent Dose (Gy) equal to:  "
# M_Data[index_min_uncert[1],9]
# "cumulative mean corrected equivalent dose (Gy)"
# M_Data[index_min_uncert[1],6]
#
#
# #liste_type = c(9, 10, 6, 7)
# #M_Data[,liste_type[1:4]]


# Plotting ------------------------------------------------------------------------------------
if(plot) {

  ##store and restore par settings
  par_default <- par()
  on.exit(par(mfrow = par_default$mfrow))

  ##plot settings
  plot_settings <- modifyList(
    x = list(
      xlab = "Individual dose [Gy]",
      xlim = c(0,max(M_Simul[,2:3]))),
    val = list(...))

  ##set box width
  box_width <- 1.5

  ##set output par
  par(mfrow = c(1,2))

  ## Histograms with the data
  ##(1) first histogram showing the natural distribution
  hist(
    x = rep(Dosedata[,1], length.out = length(M_Simul[,3])),
    freq = FALSE,
    breaks = seq(0, box_width * (1 + as.integer(max(M_Simul[,3])/box_width)), box_width),
    xlim = plot_settings$xlim,
    border = rgb(0.7,0.1,0,.5),
    col = rgb(240,154,149,alpha = 255, maxColorValue = 255),
    main = paste0("Distribution De: ", sample_name),
    xlab = plot_settings$xlab
   )

  ##(2) overplotting the first histogram with simulated data
  hist(
    x = M_Simul[,3],
    breaks = seq(0, box_width*(1 + as.integer(max(M_Simul[,3])/box_width)), box_width),
    freq = FALSE,
    border = rgb(0,0.1,0.8,.5),
    col = rgb(122,206,209,alpha = 140, maxColorValue = 255),
    add = TRUE,
    xlab = plot_settings$xlab
  )

  ##add legend
  legend(
    "topright",
    bty = "n",
    legend = c("Data", "Simulation"),
    col = c(rgb(240,154,149,alpha = 255, maxColorValue = 255),rgb(122,206,209,alpha = 140, maxColorValue = 255)),
    pch = 15)

  ##add mtext
  mtext(side = 3, text = paste0("n_sim = ", length(M_Simul[,3])))

  ## Histograms with the error for the data
  ##(3) first histogram showing the natural distribution
  hist(
    x = rep(Dosedata[,2], length.out = length(M_Simul[,2])),
    freq = FALSE,
    breaks = seq(0, box_width*(1 + as.integer(max(M_Simul[,2])/box_width)), box_width),
    xlim = plot_settings$xlim,
    border = rgb(0.7,0.1,0,.5),
    col = rgb(240,154,149,alpha = 255, maxColorValue = 255),
    main = paste0("Distribution SE(De):", sample_name),
    xlab = plot_settings$xlab
  )

  ##(4) overplotting the first histogram with simulated data
  hist(
    x = M_Simul[,2],
    breaks = seq(0, box_width*(1 + as.integer(max(M_Simul[,2])/box_width)), box_width),
    freq = FALSE,
    border = rgb(0,0.1,0.8,.5),
    col = rgb(122,206,209,alpha = 140, maxColorValue = 255),
    add = TRUE,
    xlab = plot_settings$xlab
  )

  ##add legend
  legend(
    "topright",
    bty = "n",
    legend = c("Data", "Simulation"),
    col = c(rgb(240,154,149,alpha = 255, maxColorValue = 255),rgb(122,206,209,alpha = 140, maxColorValue = 255)),
    pch = 15)

  ##add mtext
  mtext(side = 3, text = paste0("n_sim = ", length(M_Simul[,2])))


  par(mfrow = c(1,3))
  liste_type <- c(4,8)

  ##1st plateau plot
  graphics::matplot(
     Dosedata[,1],
     M_Data[,liste_type],
     type = "p",
     pch = 1,
     log = "x",
     main = "Observed/simulated \n Residual Dose Fraction Ratio",
     xlab = plot_settings$xlab,
     ylab = "Ratios",
     xlim = c(min(Dosedata[,1]), plot_settings$xlim[2]),
     ylim = c(0, 1.5))
   abline(h = 1)
   for (i in 1:Ndata) {
     lines(
       x = c(Dosedata[i, 1], Dosedata[i, 1]),
       y = c((M_Data[i, 4] - M_Data[i, 5]), (M_Data[i, 4] + M_Data[i, 5])))
   }

  liste_type <- c(1, 6, 3)
  ##2nd plateau plot
  graphics::matplot(
    x = Dosedata[,1],
    y = M_Data[,liste_type],
    type = "p",
    pch = 1,
    log = "x",
    main = paste("sample ", sample_name, sep = ""),
    xlab = plot_settings$xlab,
    ylab = "Cumulative mean doses [Gy]",
    xlim = c(min(Dosedata[,1]), plot_settings$xlim[2]),
    ylim = c(0, 2*Expected_Dose))

    # tracer les valeurs de la liste classee des doses archeologiques de la matrice de simulation
    # contrainte : ne tracer qu'une partie des points sinon on va passer un temps fou
    # par exemple on veut tracer un nb de points egal a :
    NbSimPtsDisplayed <- 1000
    if (NbSimPtsDisplayed > Nsimul)
      NbSimPtsDisplayed <- Nsimul
    XY_psimul <- matrix(nrow = NbSimPtsDisplayed, ncol = 2)
    delta_index <- as.integer(Nsimul / NbSimPtsDisplayed)
    plot_index <- 1
    cur_ind <- 1

   while(plot_index < Nsimul ){
     XY_psimul[cur_ind, 1] <-  M_Simul[plot_index, 4]
     XY_psimul[cur_ind, 2] <-  M_Simul[plot_index, 8]
     plot_index <- plot_index + delta_index
     cur_ind <- cur_ind + 1
   }

   abline(h = Expected_Dose)
   points(XY_psimul[, 1], XY_psimul[, 2], pch = 1, col = rgb(0,0,0,0.2))

   ##add error bars
   ##error bars for corrected De
   segments(
     x0 = Dosedata[, 1],
     x1 = Dosedata[, 1],
     y0 = M_Data[, 6] - M_Data[, 7],
     y1 = M_Data[, 6] + M_Data[, 7],
     col = "red"
   )

   ##error bars cum mean De
   segments(
     x0 = Dosedata[, 1],
     x1 = Dosedata[, 1],
     y0 = M_Data[, 3] - M_Data[, 2],
     y1 = M_Data[, 3] + M_Data[, 2],
     col = "green"
   )


   ##add legend
   legend(
     "topright",
     legend = c(
       "Cum Mean (uncorr.) De",
       "Cum Mean (corr.) De_sim",
       "Corr. De (resid., partial integ.)",
       "Cum mean (corr.) De"
      ),
     bty = "n",
     pch = 20,
     col = c("black", "grey", "red", "green"))


   ##add standard deviation plot
   plot(
     Dosedata[, 1],
     M_Data[, 7],
     main = "Std Dev on average corrected doses \n ",
     xlab = plot_settings$xlab,
     ylab = "Standard deviation [Gy]",
     xlim = c(min(Dosedata[,1]), plot_settings$xlim[2]),
     type = "p",
     pch = 1,
     log = "x"
   )
 }#end if plot


# Output --------------------------------------------------------------------------------------
set_RLum(
  class = "RLum.Results",
  data = list(
    M_Data = M_Data ##TODO: we should name the columns
  ),
  info = list(
    call = sys.call()
  ))

}


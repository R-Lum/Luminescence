#' @title Modelling Exponential Exposure Distribution
#'
#' @description Modelling incomplete and heterogeneous bleaching of mobile grains partially
#' exposed to the light, an implementation of the EED model proposed by Guibert et al. (2019).
#'
#' @details The function is an implementation and enhancement of the scripts used in
#' Guibert et al. (2019). The implementation supports a semi-automated estimation
#' of the parameters `kappa` and `sigma_distr`. If set to `NULL`, a surface interpolation
#' is used to estimated those values.
#'
#' **Method control parameters**
#'
#' \tabular{llll}{
#'  **ARGUMENT** \tab **FUNCTION** \tab **DEFAULT** \tab **DESCRIPTION**\cr
#'  `lower`  \tab  - \tab `c(0.1,0,0)` \tab set lower bounds for kappa, sigma, and the expected De in auto mode \cr
#'  `upper` \tab  - \tab `c(1000,2)` \tab set upper bounds for kappa, sigma, and the expected De in auto mode \cr
#'  `iter_max` \tab - \tab `1000` \tab maximum number for iterations for used to find kappa and sigma \cr
#'  `trace` \tab - \tab `FALSE` \tab enable/disable terminal trace mode; overwritten by global argument `verbose`\cr
#'  `trace_plot` \tab - \tab `FALSE` \tab enable/disable additional trace plot output; overwritten by global argument `verbose` \cr
#'
#' }
#'
#' @param data [data.frame] (**required**): input data consisting of two columns, the De and the
#' SE(De). Values are expected in Gy
#'
#' @param D0 [integer] (*with default*): D0 value (in Gy), defining the
#' characterisation behaviour of the quartz.
#'
#' @param expected_dose [numeric] (**required**): expected equivalent dose
#'
#' @param MinIndivDose [numeric] (*with default*): value specifying the minimum dose taken into
#' account for the plateau. `NULL` applies all values.
#'
#' @param MaxIndivDose [numeric] (*with default*): value specifying the maximum dose taken into
#' account for the plateau. `NULL` applies all values.
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
#' need to be provided as named list, see details
#'
#' @param verbose [logical] (*with default*): enable/disable output to the
#' terminal.
#'
#' @param plot [logical] (*with default*): enable/disable the plot output.
#'
#' @param ... further parameters that can be passed to better control the plot output. Support arguments
#' are `xlab`, `xlim`.
#'
#' @author Pierre Guibert, IRAMAT-CRP2A, UMR 5060, Université Bordeaux Montaigne (France),
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @section Function version: 0.1.0
#'
#' @references Guibert, P., Christophe, C., Urbanova, P., Guérin, G., Blain, S., 2017.
#' Modelling incomplete and heterogeneous bleaching of mobile grains partially exposed to the
#' light - Towards a new tool for single grain OSL dating of poorly bleached mortars.
#' Radiation Measurements 107, 48–57. \doi{10.1016/j.radmeas.2017.10.003}
#'
#' @seealso [RLum.Results-class], [calc_MinDose], [calc_FuchsLang2001], [calc_IEU],
#' [calc_FiniteMixture]
#'
#' @keywords datagen
#'
#' @examples
#'
#' data(ExampleData.MortarData, envir = environment())
#' calc_EED_Model(
#'  data = MortarData,
#'  kappa = 14,
#'  sigma_distr = 0.37,
#'  expected_dose = 11.7)
#'
#' ## automated estimation of
#' ## sigma_distribution and
#' ## kappa
#' \dontrun{
#'  calc_EED_Model(
#'  data = MortarData,
#'  kappa = NULL,
#'  sigma_distr = NULL,
#'  expected_dose = 11.7)
#' }
#'
#' @md
#' @export
calc_EED_Model <- function(
  data,
  D0 = 120L,
  expected_dose,
  MinIndivDose = NULL,
  MaxIndivDose = NULL,
  kappa = NULL,
  sigma_distr = NULL,
  n.simul = 5000L,
  n.minSimExp = 50L,
  sample_name = "",
  method_control = list(),
  verbose = TRUE,
  plot = TRUE,
  ...
) {
  .set_function_name("calc_EED_Model")
  on.exit(.unset_function_name(), add = TRUE)

  ##TODO
  ## add docu examples
  ## add docu details
  ##
  ## TODO Questions to Pierre
  ##  - what the user is doing of the real dose/age is not known, can it be assessed?
  ##  - the underlying assumption is obviously an exponential distribution, what
  ##  - so far the parameter uncertainty estimation does not work nicely.x
  ##  happens to data where this is not observed? I cannot work?

  ## Integrity checks -------------------------------------------------------

  .validate_class(data, "data.frame")
  .validate_class(expected_dose, "numeric")
  if (!is.null(MinIndivDose))
    .validate_class(MinIndivDose, "numeric")
  if (!is.null(MaxIndivDose))
    .validate_class(MaxIndivDose, "numeric")

  ##store and restore par settings
  par_default <- par(no.readonly = TRUE)
  on.exit(par(mfrow = par_default$mfrow), add = TRUE)

# Helper functions ----------------------------------------------------------------------------

  ##the helper functions base on ode by Pierre, each helper was only a little bit
  ## optimised and then tested separately

  # Calcul de la variance du plateau sur la base des doses arch?o
  # corrig?es de la dose r?siduelle uniquement (modif 31.5.2018)
  # nocov start
  .calc_Plateau_Variance <- function(M_Data, MinDose_Index, MaxDose_Index){
    var_ratio <- stats::var(M_Data[MinDose_Index:MaxDose_Index, 3]) # doses nettes corrigées résiduel uniquement
    mean_ratio <- mean(M_Data[MinDose_Index:MaxDose_Index, 3])
    return(var_ratio / (mean_ratio ^ 2))
  }
  # nocov end

  # Calcul de la variance du plateau      modifi? le 31.5.2018
  # sur la base des rapport observ?/simul? des doses totales brutes
  .calc_Plateau_Variance_uncorr <- function (M_Data, MinDose_Index, MaxDose_Index){
    var_ratio <-
      stats::var(M_Data[MinDose_Index:MaxDose_Index, 4]) # ratio observé/simulé brut
    mean_ratio <- mean(M_Data[MinDose_Index:MaxDose_Index, 4])
    return (var_ratio / (mean_ratio ^ 2))
  }

  # Calcul de la variance du plateau      ajout le 20.8.2018
  # sur la base des doses archeologiques
  # ##TODO not yet included
  # nocov start
  .calc_Plateau_Variance_AD <- function (M_Data, MinDose_Index, MaxDose_Index) {
      var_ratio <- stats::var(M_Data[MinDose_Index:MaxDose_Index, 6])
      mean_ratio <- mean(M_Data[MinDose_Index:MaxDose_Index, 6])
      return (var_ratio / (mean_ratio ^ 2))
    }
  # nocov end

  .EED_Simul_Matrix <- function (M_Simul, expected_dose, sigma_distr,D0, kappa, Iinit, Nsimul){

    ## génére une liste de Nsimul valeurs distribu?es selon une loi log normale de moyenne expected_dose ##
    M_Simul[,1] <- expected_dose * exp(sigma_distr * rnorm(Nsimul)) *
      exp(-0.5 * (sigma_distr ^ 2))

    ## génére une liste de Nsimul valeurs de dose r?siduelle selon une distribution expoentielle de l'exposition ##
    M_Simul[, 2] <-
      (-D0 * log(1 - Iinit * exp(-stats::rexp(Nsimul, rate = 1 / kappa))))

    ## génére la liste des doses individuelles ##
    M_Simul[,3] <- M_Simul[,1] + M_Simul[,2]

    ## génére la liste class?e des doses individuelles ##
    M_Simul[,4] <- sort(M_Simul[,3], decreasing = FALSE)

    ## génére une liste d'index and sort it
    index <- sort.list(rank(M_Simul[,3], ties.method = "first"))

    ## la liste d'index est utilis?e pour remettre les colonnes 1 et 2 dans l'ordre du classement des doses totales ##
    M_Simul[,5:6] <- M_Simul[index,1:2]

    ##calculate cumulative mean values
    M_Simul[,7:9] <- matrixStats::colCumsums(M_Simul[,4:6]) / seq_len(Nsimul)

    ##return value
    return(M_Simul)
  }


  #  Calcul de la matrice de M_Data : donnees experimentales traitees
  .EED_Data_Matrix <- function(M_Data, Dosedata, M_Simul, expected_dose, Ndata, Nsimul){
    ##set index
    index_SimRes <- as.integer((1:Ndata) * Nsimul / Ndata)

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

    # colonne 6 : dose nette corrigee des r?siduels ET de l'int?gration partielle de la
    # distribution log-normale des burial doses
    M_Data[, 6] <- M_Data[, 3] * (expected_dose / M_Simul[index_SimRes, 8])

    # colonne 7 : erreur sur dose nette corrigee (premier calcul :
    # incertitude uniquement bas?e sur erreur exp?riementale)
    M_Data[, 7] <- abs((M_Data[, 6] / M_Data[, 3]) * M_Data[, 2])

    # colonne 8 : ratio dose residuelle / dose nette corrigee
    M_Data[M_Data[, 6] != 0, 8] <- M_Simul[index_SimRes, 9] / M_Data[, 6]
    M_Data[M_Data[, 6] == 0, 8] <- 0

    return(M_Data)
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

  .EED_Calc_Overall_StatUncertainty <- function (M_Data, M_Simul, Ndata, Nsimul, MinNbSimExp){
    M_SimExpResults <- src_EED_Calc_Overall_StatUncertainty(M_Simul, Ndata, Nsimul, MinNbSimExp)

    # colonne 7 : calcule l'erreur totale sur la dose nette corrig?e M_Data[i,7]
    M_Data[,7] <- abs((M_Data[,6]/M_Data[,3])*sqrt((M_SimExpResults[,2]^2)+(M_Data[,2]^2)))
    return (M_Data)
  }

  # fonction d'initialisation de l'etat initial
  # si absence de valeur, par defaut prend comme etat initial la valeur 1 (saturation)
  # MaJ le 9 novembre 2018
  .Initial_State_of_OSL <- function(Dosedata, D0, method) {
    SaturationState <- (1 - exp(-max(Dosedata[, 1]) / D0))
    if (method == "max") {
      return  (SaturationState)
    }
    # nocov start
    if (!is.numeric(method) || method > 1 || method < 0) {
      return(1)
    }
    return(max(method, SaturationState))
    # nocov end
  }

  # fonction permettant de retourner l'indice de la valeur minimale de la  dose individuelle
  # prise en compte pour le calcul du plateau
  # here combined to one single function returning the minium or the maxixumx
  .Get_Plateau_MinDoseIndex <- function(M_Data, Ndata, MinIndivDose) {
      if (is.null(MinIndivDose))
        return(1)
    # nocov start
      current_index <- 1
      while ((MinIndivDose > M_Data[current_index, 9]) &
             (current_index < Ndata)) {
        current_index <- current_index + 1
      }

      return(current_index)
    # nocov end
  }

  # fonction permettant de retourner l'indice de la valeur maximale de la  dose individuelle
  # prise en compte pour le calcul du plateau
  .Get_Plateau_MaxDoseIndex <- function(M_Data, Ndata, MaxIndivDose) {
    if (is.null(MaxIndivDose))
      return(Ndata)
    # nocov start
        current_index <- 1
        while ((MaxIndivDose > M_Data[current_index, 9]) &
               (current_index < Ndata)) {
          current_index <- current_index + 1
        }

      return(current_index)
    # nocov end
  }

  ## allow automated kapp and sigma_distr parameter estimation
  .guess_EED_parameters <- function(set_kappa, set_sigma_distr,
    set_expected_dose, D0, Iinit, Nsimul, Dosedata, M_Data, M_Simul, Ndata,
    set_MinDose_Index, set_MaxDose_Index, method_control_intern = method_control){

    ##settings to control the what needs to be controlled
    method_control <- modifyList(
      list(
        lower = c(0.1, 0, 1),
        upper = c(100, 1, 100),
        iter_max = 1000,
        trace = FALSE,
        trace_plot = FALSE
      ),
      method_control_intern)

    ##define function for the parameter estimation which will be part of
    ##the interpolation
    fn <- function(
      set_kappa, set_sigma_distr,
      M_Simul, set_expected_dose,
      D0, Iinit, Nsimul, Dosedata, M_Data, Ndata){

      M_Simul <- .EED_Simul_Matrix (M_Simul, set_expected_dose, set_sigma_distr, D0, set_kappa, Iinit, Nsimul)
      M_Data <- .EED_Data_Matrix(M_Data, Dosedata, M_Simul, set_expected_dose, Ndata, Nsimul)

      M_Data <- .EED_Calc_Overall_StatUncertainty(
          M_Data = M_Data,
          M_Simul = M_Simul,
          Ndata = Ndata,
          Nsimul = Nsimul,
          MinNbSimExp = MinNbSimExp
        )

      ##return variance and the mean DE
      return(
        c(
        VAR = .calc_Plateau_Variance_uncorr(M_Data, MinDose_Index = set_MinDose_Index,
                                            MaxDose_Index = set_MaxDose_Index),
        RESIDUAL = sum((M_Data[,6] - rep(set_expected_dose, nrow(M_Data)))^2)
        ))
      }

    par(mfrow = c(3,3))
    test_var <- Inf
    n_iter <- 0
    while(test_var > 1e-04 && n_iter < method_control$iter_max){
      ##define parameter matrix
      m <- matrix(NA, nrow = 16, ncol = 5)

      ##fill matrix if parameter is not NULL
      #kappa
      m[,1] <- rep(
        exp(seq(log(method_control$lower[1]), log(method_control$upper[1]), length.out = 4)), 4)

      ##sigma_distr
      m[,2] <- rep(seq(method_control$lower[2],method_control$upper[2], length.out = 4), each = 4)

      ##expected dose
      m[,3] <- rep(expected_dose[1], 16)

      ##calculate the variance
      for(i in 1:nrow(m)){
        m[i, 4:5] <-
          fn(
            set_kappa = m[i, 1],
            set_sigma_distr = m[i, 2],
            M_Simul,
            set_expected_dose = m[i,3],
            D0,
            Iinit,
            Nsimul,
            Dosedata,
            M_Data,
            Ndata
          )
      }

      ##surface interpolation
      s <-
        try(interp::interp(
          x = m[, 1],
          y = m[, 2],
          z = m[, 4],
          nx = 200,
          ny = 200,
          duplicate = "strip" #does not seem to work
        ), silent = FALSE)

      if (inherits(s, "try-error")) {
        .throw_error("Surface interpolation failed, you may want to try it again")
      }

      ##graphical output
      if(plot & method_control$trace_plot){
        graphics::image(
          s,
          col = grDevices::heat.colors(30, rev = TRUE),
          xlab = "kappa",
          ylab = "sigma_distr"
        )
        graphics::contour(s, add= TRUE, nlevels = 10)

        abline(h = s$y[which(s$z == min(s$z, na.rm = TRUE), arr.ind = TRUE)[,2]], lty = 2)
        abline(v = s$x[which(s$z == min(s$z, na.rm = TRUE), arr.ind = TRUE)[,1]], lty = 2)
      }

      ##our decision is the 5 % quantile
      q10 <- which(s$z<= quantile(s$z, probs = 0.05, na.rm = TRUE), arr.ind = TRUE)

      ##calculate
      optim_kappa <- c(median(s$x[unique(q10[,1])], na.rm = TRUE), sd(s$x[unique(q10[,1])]))
      optim_sigm_distr <- c(median(s$y[unique(q10[,1])]), sd(s$y[unique(q10[,1])]))

      ##write output
      if(verbose && method_control$trace){
        cat("\n\n variance threshold: ", min(s$z, na.rm = TRUE))
        cat("\n >> kappa: ",   optim_kappa[1], "\u00b1", optim_kappa[2])
        cat("\n >> sigma_distr: ", optim_sigm_distr[1] , "\u00b1", optim_sigm_distr[2])
      }

        ##reset parameters
        method_control$lower[1:2] <- c(min(s$x[unique(q10[,1])]), min(s$y[unique(q10[,2])]))
        method_control$upper[1:2] <- c(max(s$x[unique(q10[,1])]), max(s$y[unique(q10[,2])]))

      if(verbose && method_control$trace){
        cat("\n >> lower: ",  paste(method_control$lower[1:2], collapse = ", "))
        cat("\n >> upper: ",  paste(method_control$upper[1:2], collapse = ", "))
      }

      ##update threshold
      test_var <- min(s$z, na.rm = TRUE)
      n_iter <- n_iter + 1

      ##implement differential break if the search area is already smaller than the area
      if(diff(c(method_control$lower[1], method_control$upper[1])) < 0.1)
         break()
    }

    return(list(
       kappa = optim_kappa ,
       sigma_distr = optim_sigm_distr,
       min_var = test_var,
       expected_dose = m[1,3]
       ))
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

# set limits ... ##TODO M_data is still NA here ... double check, so far only NULL works
# The problem is that this call comes to early and it should be somehow integrated in the guess_EED_parameter
# function to get evaluated everytime on the other hand we just want to limit the search range, maybe this
# can be done better.
# The code in .Get_Plateau_MinDoseIndex is rather inefficient and loops over the vector, however,
# this is not out main problem. In fact we have to first understand what this function is really
# doing and whether we can achive this better and easier. So far it gets called a lot of times
Min_plateau <- .Get_Plateau_MinDoseIndex(M_Data, Ndata, MinIndivDose = NULL)
Max_plateau <- .Get_Plateau_MaxDoseIndex(M_Data, Ndata, MaxIndivDose = NULL)
if (Max_plateau <= Min_plateau) {
  Min_plateau <- 1
} #priorité max dose

# Guess parameters if needed ------------------------------------------------------------------
if(verbose) cat("\n[calc_EED_Model()]\n")

## introduire le facteur d'eclairement kappa : kappa = 2 : très faible éclairement, kappa >100 bon
##  blanchiment ##
## TODO - this is not really what Pierre had in mind, he wanted to have the variance, not an automated
## esstimation
if(is.null(kappa) || is.null(sigma_distr)){

  if(verbose)
    cat("\n>> Running automated parameter estimation... \n")

  temp_guess <- .guess_EED_parameters(
    set_kappa = kappa,
    set_sigma_distr = sigma_distr,
    set_expected_dose = expected_dose,
    D0 = D0,
    Iinit = Iinit,
    Nsimul = Nsimul,
    Dosedata = Dosedata,
    M_Data = M_Data,
    M_Simul = M_Simul,
    Ndata = Ndata,
    set_MinDose_Index = Min_plateau,
    set_MaxDose_Index = Max_plateau
    )

  kappa <- temp_guess[[1]][1]
  sigma_distr <- temp_guess[[2]][1]
  min_var <- temp_guess[[3]]
  expected_dose <- temp_guess[[4]]

  if(verbose){
    cat(">> min. variance:", min_var)
    cat("\n>> kappa: ", kappa, " | sigma: ",sigma_distr)
  }
}

# Calculation ---------------------------------------------------------------------------------
M_Simul <- .EED_Simul_Matrix (M_Simul, expected_dose, sigma_distr,D0, kappa, Iinit, Nsimul)
M_Data <- .EED_Data_Matrix(M_Data, Dosedata, M_Simul, expected_dose, Ndata, Nsimul)

M_Data <- .EED_Calc_Overall_StatUncertainty(M_Data = M_Data, M_Simul = M_Simul, Ndata = Ndata, Nsimul = Nsimul, MinNbSimExp)

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

  ##2nd plateau plot
   graphics::matplot(
     x = Dosedata[, 1],
     y = M_Data[, c(1, 6, 3)],
     type = "p",
     pch = 1,
     log = "x",
     main = paste("sample ", sample_name, sep = ""),
     xlab = plot_settings$xlab,
     ylab = "Cumulative mean doses [Gy]",
     xlim = c(min(Dosedata[, 1]), plot_settings$xlim[2]),
     ylim = c(0, 2 * expected_dose)
   )

    # tracer les valeurs de la liste classee des doses archeologiques de la matrice de simulation
    # contrainte : ne tracer qu'une partie des points sinon on va passer un temps fou
    # par exemple on veut tracer un nb de points egal a :
    NbSimPtsDisplayed <- min(Nsimul, 1000)
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

   abline(h = expected_dose)
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

#'Calculate the Average Dose and the dose rate dispersion
#'
#'This functions calculates the Average Dose and their extrinsic dispersion and estimates
#'the standard errors by bootstrapping based on the Average Dose Model by Guerin et al., 2016
#'
#'\bold{\code{sigma_m}}\cr
#'
#'The program requires the input of a known value of sigma_m,
#'which corresponds to the intrinsic overdispersion, as determined
#'by a dose recovery experiment. Then the dispersion in doses (sigma_d)
#'will be that over and above sigma_m (and individual uncertainties sigma_wi).
#'
#' @param data \code{\linkS4class{RLum.Results}} or \link{data.frame}
#' (\bold{required}): for \code{data.frame}: two columns with De
#' \code{(data[,1])} and De error \code{(values[,2])}
#'
#' @param sigma_m \code{\link{numeric}} (\bold{required}): the overdispersion resulting from a dose recovery
#' experiment, i.e. when all grains have  received the same dose. Indeed in such a case, any
#' overdispersion (i.e. dispersion on top of analytical uncertainties) is, by definition, an
#' unrecognised measurement uncertainty.
#'
#' @param Nb_BE \code{\link{integer}} (with default): sample size used for the bootstrapping
#'
#' @param na.rm \code{\link{logical}} (with default): exclude NA values
#' from the data set prior to any further operation.
#'
#' @param plot \code{\link{logical}} (with default): enables/disables plot output
#'
#' @param verbose \code{\link{logical}} (with default): enables/disables terminal output
#'
#' @param ... further arguments that can be passed to \code{\link[graphics]{hist}}. As three plots
#' are returned all arguments need to be provided as \code{\link{list}},
#' e.g., \code{main = list("Plot 1", "Plot 2", "Plot 3")}. Note: not all arguments of \code{hist} are
#' supported, but the output of \code{hist} is returned and can be used of own plots. \cr
#'
#' Further supported arguments: \code{mtext} (\code{character}), \code{rug} (\code{TRUE/FALSE}).
#'
#' @section Function version: 0.1.3
#'
#' @author Claire Christophe, IRAMAT-CRP2A, Universite de Nantes (France),
#' Anne Philippe, Universite de Nantes, (France),
#' Guillaume Guerin, IRAMAT-CRP2A, Universite Bordeaux Montaigne, (France),
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne, (France)
#'
#' @seealso \code{\link{read.table}}, \code{\link[graphics]{hist}}
#'
#' @references
#' TODO: Add Guerin et al., 2016 once it has been published
#'
#' \bold{Further reading}\cr
#'
#' Efron, B., Tibshirani, R., 1986. Bootstrap Methods for Standard Errors, Confidence Intervals,
#' and Other Measures of Statistical Accuracy. Statistical Science 1, 54-75.
#'
#' @note This function has beta status!
#'
#' @keywords datagen
#'
#' @examples
#'
#'##Example 01 using package example data
#'##load example data
#'data(ExampleData.DeValues, envir = environment())
#'
#'##calculate Average dose
#'##(use only the first 56 values here)
#'AD <- calc_AverageDose(ExampleData.DeValues$CA1[1:56,],
#'sigma_m = 0.1)
#'
#'##plot De and set Average dose as central value
#'plot_AbanicoPlot(
#'  data = ExampleData.DeValues$CA1[1:56,],
#'  z.0 = AD$summary$Average_DOSE)
#'
#'@export
calc_AverageDose <- function(
  data,
  sigma_m = NULL,
  Nb_BE = 500,
  na.rm = TRUE,
  plot = TRUE,
  verbose = TRUE,
  ...
){

  # Define internal functions ------------------------------------------------------------------

  # function which compute mle's for data (yu,su)
  .mle <- function(yu , su, wu.start, sigma_d.start, delta.start){

    ##set start parameters, otherwise the function will try to get them
    ##from the parent environment, which is not wanted ...
    delta.temp <- 0
    sigma_d.temp <- 0

    sigma_d <- sigma_d.start
    delta <- delta.start
    wu <- wu.start

    j <- 0
    iteration_limit <- 10000

    ##loop until convergence or the iteration limit is reached
    while(j < iteration_limit) {

      ##code by Claire; in the 2nd and 3rd line delta and sigma_d are replaced by delta.temp and
      ##sigma_d.temp; otherwise the iteration and its test for convergence will not work
      delta.temp <- exp( sum(wu*(yu+(0.5*(sigma_d^2)))) / sum(wu) )
      sigma_d.temp <- sigma_d*sum( (wu^2) * (yu-log(delta.temp)+0.5*sigma_d^2)^2) / (sum( wu*(1+yu-log(delta.temp)+0.5*sigma_d^2)))
      wu <- 1/(sigma_d.temp^2 + su^2)

      ##break loop if convergence is reached ... if not update values
      if(is.infinite(delta.temp) | is.infinite(sigma_d.temp)){
        break()

      }else if (
        ##compare values ... if they are equal we have convergence
        all(
          c(round(c(delta, sigma_d), 4)) == c(round(c(delta.temp, sigma_d.temp), 4))
          )
        ) {
        break()

      } else{
        ##update input values
        delta <- delta.temp
        sigma_d <- sigma_d.temp
        j <- j + 1

      }

    }

    ##if no convergence was reached stop entire function; no stop as this may happen during the
    ##bootstraping procedure
    if(j == iteration_limit){
      warning("[calc_AverageDoseModel()] .mle() no convergence reached for the given limits. NA returned!")
      return(c(NA,NA))

    }else if(is.infinite(delta.temp) | is.infinite(sigma_d.temp)){
      warning("[calc_AverageDoseModel()] .mle() gaves Inf values. NA returned!")
      return(c(NA,NA))

    }else{
      return(c(round(c(delta, sigma_d),4)))

    }

  }

  .CredibleInterval <- function(a_chain, level = 0.95) {
    ## Aim : estimation of the shortest credible interval of the sample of parameter a
    # A level % credible interval is an interval that keeps N*(1-level) elements of the sample
    # The level % credible interval is the shortest of all those intervals.
    ## Parameters :
    # a_chain : the name of the values of the parameter a
    # level : the level of the credible interval expected
    ## Returns : the level and the endpoints

    sorted_sample <- sort(a_chain)
    N <- length(a_chain)
    OutSample <- N * (1 - level)

    I <- cbind(sorted_sample[1:(OutSample + 1)] , sorted_sample[(N - OutSample):N])

    l <-  I[, 2] - I[, 1] # length of intervals
    i <- which.min(l) # look for the shortest interval

    return(c(
      level = level,
      CredibleIntervalInf = I[i, 1],
      CredibleIntervalSup = I[i, 2]
    ))

  }

  ##////////////////////////////////////////////////////////////////////////////////////////////////
  ##HERE THE MAIN FUNCTION STARTS
  ##////////////////////////////////////////////////////////////////////////////////////////////////

  # Integrity checks ----------------------------------------------------------------------------

  if(!is(data, "RLum.Results") & !is(data, "data.frame")){
    stop("[calc_AverageDose()] input is neither of type 'RLum.Results' nor of type 'data.frame'!")

  }else {

    if(is(data, "RLum.Results")){
     data <- get_RLum(data)

    }

  }

  if(is.null(sigma_m)){
    stop("[calc_AverageDose()] 'sigma_m' is missing but required")

  }

  # Data preparation -----------------------------------------------------------------------------

  ##problem: the entire code refers to column names the user may not provide...
  ##  >> to avoid changing the entire code, the data will shape to a format that
  ##  >> fits to the code

    ##check for number of columns
    if(ncol(data)<2){
      try(stop("[calc_AverageDose()] data set contains < 2 columns! NULL returned!", call. = FALSE))
      return(NULL)

    }

    ##used only the first two colums
    if(ncol(data)>2){
      data <- data[,1:2]
      warning("[calc_AverageDose()] number of columns in data set > 2. Only the first two columns were used.", call. = FALSE)
    }

    ##exclude NA values
    if(any(is.na(data))){
      data <- na.exclude(data)
      warning("[calc_AverageDose()] NA values in data set detected. Rows with NA values removed!", call. = FALSE)

    }

    ##check data set
    if(nrow(data) == 0){
      try(stop("[calc_AverageDose()] data set contains 0 rows! NULL returned!", call. = FALSE))
      return(NULL)

    }

    ##data becomes to dat (thus, make the code compatible with the code by Claire and Anne)
    dat <- data

    ##preset column names, as the code refers to it
    colnames(dat) <- c("cd", "se")


  # Pre calculation -----------------------------------------------------------------------------

  ##calculate  yu = log(CD) and su = se(logCD)
  yu <- log(dat$cd)

  su <- sqrt((dat$se / dat$cd) ^ 2 + sigma_m ^ 2)

  # calculate starting values and weights
  sigma_d <- sd(dat$cd) / mean(dat$cd)
  wu <- 1 / (sigma_d ^ 2 + su ^ 2)

  delta <- mean(dat$cd)
  n <- length(yu)

  ##terminal output
  if (verbose) {
    cat("\n[calc_AverageDose()]")
    cat("\n\n>> Initialisation <<")
    cat(paste("\nn:\t\t", n))
    cat(paste("\ndelta:\t\t", delta))
    cat(paste("\nsigma_m:\t", sigma_m))
    cat(paste("\nsigma_d:\t", sigma_d))
  }


  # mle's computation
  dhat <- .mle(yu, su, wu.start = wu, sigma_d.start = sigma_d, delta.start = delta)
  delta <- dhat[1]
  sigma_d <- dhat[2]
  wu <- 1 / (sigma_d ^ 2 + su ^ 2)

  # maximum log likelihood
  llik <- sum(-log(sqrt(2 * pi / wu)) - (wu / 2) * ((yu - log(delta) + 0.5 * (sigma_d ^ 2)) ^ 2))

  ##terminal output
  if(verbose){
    cat(paste("\n\n>> Calculation <<\n"))
    cat(paste("log likelihood:\t", round(llik, 4)))

  }


  # standard errors obtained by bootstrap, we refer to Efron B. and Tibshirani R. (1986)
  # est ce qu'il faut citer l'article ici ou tout simplement dans la publi ?
  n <- length(yu)

  ##calculate dstar
  ##set matrix for I
  I <- matrix(data = sample(x = 1:n, size = n * Nb_BE, replace = TRUE), ncol = Nb_BE)

  ##iterate over the matrix and produce dstar
  ##(this looks a little bit complicated, but is far more efficient)
  dstar <- t(vapply(
    X = 1:Nb_BE,
    FUN = function(x) {
      .mle(yu[I[, x]], su[I[, x]], sigma_d.start = sigma_d, delta.start = delta, wu.start = wu)

    },
    FUN.VALUE = vector(mode = "numeric", length = 2)
  ))

  ##exclude NA values
  dstar <- na.exclude(dstar)


  ##calculate confidence intervalls
  IC_delta <- .CredibleInterval(dstar[,1],0.95)
  IC_sigma_d <- .CredibleInterval(dstar[,2],0.95)
  IC <- rbind(IC_delta, IC_sigma_d)

  # standard errors
  sedelta <- sqrt ((1/(Nb_BE-1))*sum((dstar[,1]-mean(dstar[,1]))^2))
  sesigma_d <- sqrt ((1/(Nb_BE-1))*sum((dstar[,2]-mean(dstar[,2]))^2))


  ##Terminal output
  if (verbose) {
    cat("\nconfidence intervals\n")
    cat("--------------------------------------------------\n")
    print(t(IC), print.gap = 6, digits = 4)
    cat("--------------------------------------------------\n")

    cat(paste("\n>> Results <<\n"))
    cat("----------------------------------------------------------\n")
    cat(paste(
      "Average dose:\t ",
      round(delta, 4),
      "\tse(Aver. dose):\t",
      round(sedelta, 4)
    ))
    if(sigma_d == 0){
      cat(paste(
        "\nsigma_d:\t ",
        round(sigma_d, 4),
        "\t\tse(sigma_d):\t",
        round(sesigma_d, 4)
      ))

    }else{
    cat(paste(
      "\nsigma_d:\t ",
      round(sigma_d, 4),
      "\tse(sigma_d):\t",
      round(sesigma_d, 4)
    ))
    }
    cat("\n----------------------------------------------------------\n")

  }

  ##compile final results data frame
  results_df <- data.frame(
    Average_DOSE = delta,
    Average_DOSE.SE = sedelta,
    SIGMA_D = sigma_d,
    SIGMA_D.SE = sesigma_d,
    IC_Average_DOSE.LEVEL = IC_delta[1],
    IC_Average_DOSE.LOWER = IC_delta[2],
    IC_Average_DOSE.UPPER = IC_delta[3],
    IC_SIGMA_D.LEVEL = IC_sigma_d[1],
    IC_SIGMA_D.LOWER = IC_sigma_d[2],
    IC_SIGMA_D.UPPER = IC_sigma_d[3],
    L_MAX = llik,
    row.names = NULL

  )

  # Plotting ------------------------------------------------------------------------------------

    ##the plotting (enable/disable) is controlled below, as with this
    ##we always get a histogram object

    ##set data list
    data_list <- list(dat$cd, dstar[,1], dstar[,2])

    ##preset plot arguments
    plot_settings <- list(
      breaks = list("FD", "FD", "FD"),
      probability = list(FALSE, TRUE, TRUE),
      main = list(
        "Observed: Equivalent dose",
        "Bootstrapping: Average Dose",
        "Bootstrapping: Sigma_d"),
      xlab = list(
       "Equivalent dose [a.u.]",
       "Average dose [a.u.]",
       "Sigma_d"),
      axes = list(TRUE, TRUE, TRUE),
      col = NULL,
      border = NULL,
      density = NULL,
      freq = NULL,
      mtext = list(
        paste("n = ", length(data_list[[1]])),
        paste("n = ", length(data_list[[2]])),
        paste("n = ", length(data_list[[3]]))),
      rug = list(TRUE, TRUE, TRUE)

    )

    ##modify this list by values the user provides

      ##expand all elements in the list
      ##problem: the user might provid only one item, then the code will break
      plot_settings.user <- lapply(list(...), function(x){
        rep(x, length = 3)

      })

      ##modify
      plot_settings <- modifyList(x =  plot_settings.user, val = plot_settings)


    ##get change par setting and reset on exit
    par.default <- par()$mfrow
    on.exit(par(mfrow = par.default))
    par(mfrow = c(1,3))

    ##Produce plots
    ##(1) - histogram of the observed equivalent dose
    ##(2) - histogram of the bootstrapped De
    ##(3) - histogram of the bootstrapped sigma_d

    ##with lapply we get fetch also the return of hist, they user might want to use this later
    hist <- lapply(1:length(data_list), function(x){
      temp <- suppressWarnings(hist(
        x = data_list[[x]],
        breaks = plot_settings$breaks[[x]],
        probability = plot_settings$probability[[x]],
        main = plot_settings$main[[x]],
        xlab = plot_settings$xlab[[x]],
        axes = plot_settings$axes[[x]],
        freq = plot_settings$freq[[x]],
        plot = plot,
        col = plot_settings$col[[x]],
        border = plot_settings$border[[x]],
        density = plot_settings$density[[x]]

      ))

      if (plot) {
        ##add rug
        if (plot_settings$rug[[x]]) {
          rug(data_list[[x]])

        }

        ##plot mtext
        mtext(side = 3,
              text = plot_settings$mtext[[x]],
              cex = par()$cex)
      }

      return(temp)

    })

  # Return --------------------------------------------------------------------------------------
  set_RLum(
    class = "RLum.Results",
    data = list(
      summary = results_df,
      dstar = dstar,
      hist = hist
      ),
    info = list(call = sys.call())

  )

}

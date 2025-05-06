#' @title Apply the finite mixture model (FMM) after Galbraith (2005) to a given De
#' distribution
#'
#' @description
#' This function fits a k-component mixture to a De distribution with differing
#' known standard errors. Parameters (doses and mixing proportions) are
#' estimated by maximum likelihood assuming that the log dose estimates come
#' from a mixture of normal distributions.
#'
#' @details
#' This model uses the maximum likelihood and Bayesian Information Criterion
#' (BIC) approaches.
#'
#' Indications of overfitting are:
#'
#' - increasing BIC
#' - repeated dose estimates
#' - covariance matrix not positive definite
#' - covariance matrix produces `NaN`
#' - convergence problems
#'
#' **Plot**
#'
#' If `n.components` is a vector (`c(k.min:k.max)`), a plot is generated
#' showing the *k* components equivalent doses as normal distributions.
#' By default `pdf.weight` is set to `TRUE`, so that the probability density
#' functions are weighted by the components proportion for each iteration of
#' *k* components, so the sum of areas of each component equals 1.
#' If `pdf.weight` is set to `FALSE`, the area under each normal distribution
#' is always 1. While the density values are on the same scale when no weights
#' are used, the y-axis are individually scaled if the probability density are
#' weighted by the components proportion.\cr
#' The standard deviation (sigma) of the normal distributions is by default
#' determined by a common `sigmab` (see `pdf.sigma`). For
#' `pdf.sigma = "se"` the standard error of each component is taken
#' instead.\cr
#' The stacked [graphics::barplot] shows the proportion of each component (in
#' per cent) calculated by the FMM. The last plot shows the achieved BIC scores
#' and maximum log-likelihood estimates for each value of *k*.
#'
#' @param data [RLum.Results-class] or [data.frame] (**required**):
#' for [data.frame]: two columns with De `(data[,1])` and De error `(values[,2])`
#'
#' @param sigmab [numeric] (**required**):
#' spread in De values given as a fraction (e.g. 0.2). This value represents
#' the expected overdispersion in the data should the sample be well-bleached
#' (Cunningham & Wallinga 2012, p. 100).
#'
#' @param n.components [numeric] (**required**):
#' number of components to be fitted. If a vector is provided (e.g. `c(2:8)`) the
#' finite mixtures for 2, 3 ... 8 components are calculated and a plot and a
#' statistical evaluation of the model performance (BIC score and maximum
#' log-likelihood) is provided.
#'
#' @param grain.probability [logical] (*with default*):
#' prints the estimated probabilities of which component each grain is in
#'
#' @param pdf.weight [logical] (*with default*):
#' weight the probability density functions by the components proportion.
#' Ignored if `n.components` has length 1.
#'
#' @param pdf.sigma [character] (*with default*):
#' if `"sigmab"` the components normal distributions are plotted with a common standard
#' deviation (i.e. `sigmab`) as assumed by the FFM. Alternatively,
#' `"se"` takes the standard error of each component for the sigma
#' parameter of the normal distribution
#'
#' @param pdf.colors [character] (*with default*):
#' colour coding of the components in the plot.
#' Possible options are `"gray"`, `"colors"` and `"none"`.
#'
#' @param plot.proportions [logical] (*with default*):
#' plot a [graphics::barplot] showing the proportions of components.
#' Ignored if `n.components` has length 1.
#'
#' @param plot.criteria [logical] (*with default*):
#' plot the statistical criteria (BIC and log-likelihood).
#' Ignored if `n.components` has length 1.
#'
#' @param plot [logical] (*with default*): enable/disable the  plot output.
#'
#' @param ... other parameters to control the plot output. Supported are
#' `cex`, `main`, `main.densities`, `main.proportions`, `main.criteria`,
#' `pdf.scale`, `dose.scale`.
#'
#' @return
#' Returns a plot (*optional*) and terminal output. In addition an
#' [RLum.Results-class] object is returned containing the
#' following elements:
#'
#' \item{.$summary}{[data.frame] summary of all relevant model results.}
#' \item{.$data}{[data.frame] original input data}
#' \item{.$args}{[list] used arguments}
#' \item{.$call}{[call] the function call}
#' \item{.$mle}{ covariance matrices of the log likelihoods}
#' \item{.$BIC}{ BIC score}
#' \item{.$llik}{ maximum log likelihood}
#' \item{.$grain.probability}{ probabilities of a grain belonging to a component}
#' \item{.$components}{[matrix] estimates of the de, de error and proportion for each component}
#' \item{.$single.comp}{[data.frame] single component FFM estimate}
#'
#' If a vector for `n.components` is provided (e.g.  `c(2:8)`),
#' `mle` and `grain.probability` are lists containing matrices of the
#' results for each iteration of the model.
#'
#' The output should be accessed using the function [get_RLum].
#'
#' @section Function version: 0.4.3
#'
#' @author
#' Christoph Burow, University of Cologne (Germany) \cr
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany) \cr
#' Based on a rewritten S script of Rex Galbraith, 2006.
#'
#' @seealso [calc_CentralDose], [calc_CommonDose],
#' [calc_FuchsLang2001], [calc_MinDose]
#'
#' @references
#' Galbraith, R.F. & Green, P.F., 1990. Estimating the component
#' ages in a finite mixture. Nuclear Tracks and Radiation Measurements 17,
#' 197-206.
#'
#' Galbraith, R.F. & Laslett, G.M., 1993. Statistical models
#' for mixed fission track ages. Nuclear Tracks Radiation Measurements 4,
#' 459-470.
#'
#' Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of
#' equivalent dose and error calculation and display in OSL dating: An overview
#' and some recommendations. Quaternary Geochronology 11, 1-27.
#'
#' Roberts, R.G., Galbraith, R.F., Yoshida, H., Laslett, G.M. & Olley, J.M., 2000.
#' Distinguishing dose populations in sediment mixtures: a test of single-grain
#' optical dating procedures using mixtures of laboratory-dosed quartz.
#' Radiation Measurements 32, 459-465.
#'
#' Galbraith, R.F., 2005. Statistics for Fission Track Analysis, Chapman & Hall/CRC, Boca Raton.
#'
#' **Further reading**
#'
#' Arnold, L.J. & Roberts, R.G., 2009. Stochastic
#' modelling of multi-grain equivalent dose (De) distributions: Implications
#' for OSL dating of sediment mixtures. Quaternary Geochronology 4,
#' 204-230.
#'
#' Cunningham, A.C. & Wallinga, J., 2012. Realizing the
#' potential of fluvial archives using robust OSL chronologies. Quaternary
#' Geochronology 12, 98-106.
#'
#' Rodnight, H., Duller, G.A.T., Wintle, A.G. &
#' Tooth, S., 2006. Assessing the reproducibility and accuracy of optical
#' dating of fluvial deposits.  Quaternary Geochronology 1, 109-120.
#'
#' Rodnight, H. 2008. How many equivalent dose values are needed to obtain a
#' reproducible distribution?. Ancient TL 26, 3-10.
#'
#'
#' @examples
#'
#' ## load example data
#' data(ExampleData.DeValues, envir = environment())
#'
#' ## (1) apply the finite mixture model
#' ## NOTE: the data set is not suitable for the finite mixture model,
#' ## which is why a very small sigmab is necessary
#' calc_FiniteMixture(ExampleData.DeValues$CA1,
#'                    sigmab = 0.2, n.components = 2,
#'                    grain.probability = TRUE)
#'
#' ## (2) repeat the finite mixture model for 2, 3 and 4 maximum number of fitted
#' ## components and save results
#' ## NOTE: The following example is computationally intensive. Please un-comment
#' ## the following lines to make the example work.
#' FMM<- calc_FiniteMixture(ExampleData.DeValues$CA1,
#'                          sigmab = 0.2, n.components = c(2:4),
#'                          pdf.weight = TRUE)
#'
#' ## show structure of the results
#' FMM
#'
#' ## show the results on equivalent dose, standard error and proportion of
#' ## fitted components
#' get_RLum(object = FMM, data.object = "components")
#'
#' @md
#' @export
calc_FiniteMixture <- function(
  data,
  sigmab,
  n.components,
  grain.probability = FALSE,
  pdf.weight = TRUE,
  pdf.sigma = "sigmab",
  pdf.colors = "gray",
  plot.proportions = TRUE,
  plot.criteria = TRUE,
  plot=TRUE,
  ...
) {
  .set_function_name("calc_FiniteMixture")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(data, c("data.frame", "RLum.Results"))
  if (inherits(data, "RLum.Results")) {
    if (data@originator == "calc_FiniteMixture") {
      do.call(.plot_FiniteMixture, c(data, as.list(sys.call())[-(1:2)]))
      return(invisible(data))
    }
    data <- get_RLum(data, "data")
  }
  if (ncol(data) < 2) {
    .throw_error("'data' object must have two columns")
  }
  if (sigmab < 0 || sigmab > 1) {
    .throw_error("'sigmab' must be a value between 0 and 1")
  }
  .validate_class(n.components, c("integer", "numeric"))
  if (min(n.components) < 2) {
    .throw_error("'n.components' should be at least 2")
  }
  .validate_logical_scalar(grain.probability)
  .validate_logical_scalar(pdf.weight)
  pdf.sigma <- .validate_args(pdf.sigma, c("sigmab", "se"))
  pdf.colors <- .validate_args(pdf.colors, c("gray", "colors", "none"))
  .validate_logical_scalar(plot.proportions)
  .validate_logical_scalar(plot.criteria)
  .validate_logical_scalar(plot)

  ## ensure that the chosen components are sorted
  n.components <- sort(n.components)
  multiple.components <- length(n.components) > 1

  ## set expected column names
  colnames(data)[1:2] <- c("ED", "ED_Error")

  ## ... ARGUMENTS ------------
  ##============================================================================##

  extraArgs <- list(...)

  ## console output
  verbose <- TRUE
  if("verbose" %in% names(extraArgs)) {
    verbose<- extraArgs$verbose
    .validate_logical_scalar(verbose)
  }

  ##============================================================================##
  ## CALCULATIONS
  ##============================================================================##

  ## create storage variables if more than one k is provided
  if (multiple.components) {

    # counter needed for various purposes
    cnt<- 1

    # create summary matrix containing DE, standard error (se) and proportion
    # for each component
    comp.n<- matrix(data = NA, ncol = length(n.components),
                    nrow = n.components[length(n.components)] * 3,
                    byrow = TRUE)

    # create empty vector as storage for BIC and LLIK scores
    BIC.n<- vector(mode = "double")
    LLIK.n<- vector(mode = "double")

    # create empty vectors of type "lists" as storage for mle matrices and
    # grain probabilities
    vmat.n<- vector(mode = "list", length = length(n.components))
    grain.probability.n<- vector(mode = "list", length = length(n.components))
  }

  ## start actual calculation (loop) for each provided maximum components to
  ## be fitted.
  for(i in 1:length(n.components)) {

    k<- n.components[i]

    # calculate yu = log(ED),  su = se(logED),  n = number of grains
    yu<- log(data$ED)
    su<- data$ED_Error/data$ED
    n<- length(yu)

    # compute starting values
    pui<- matrix(0,n,k)
    nui<- matrix(0,n,k)
    pii<- rep(1/k,k)
    mu<- min(yu) + (max(yu)-min(yu))*(1:k)/(k+1)

    # remove the # in the line below to get alternative starting values
    # (useful to check that the algorithm converges to the same values)
    #	mu<- quantile(yu,(1:k)/(k+1))

    # compute maximum log likelihood estimates
    nit<- 499L
    wu<- 1/(sigmab^2 + su^2)
    rwu<- sqrt(wu)

    for(j in 1:nit){
      for(i in 1:k)
      {
        fui <- rwu * exp(-0.5 * wu * (yu - mu[i])^2)
        nui[, i] <- pii[i] * fui
      }
      pui <- nui / rowSums(nui)
      mu <- colSums(wu * yu * pui) / colSums(wu * pui)
      pii <- colMeans(pui)
    }

    # calculate the log likelihood and BIC
    llik <- sum(log( 1 / sqrt(2 * pi) * rowSums(nui) ))
    bic<- -2*llik + (2*k - 1)*log(n)

    # calculate the covariance matrix and standard errors of the estimates
    # i.e., the dose estimates in Gy and relative standard errors, and
    # the mixing proportions and standard errors.
    aui<- matrix(0,n,k)
    bui<- matrix(0,n,k)
    for(i in 1:k)
    {
      aui[,i]<- wu*(yu-mu[i])
      bui[, i] <- -wu + aui[, i]^2
    }
    delta<- diag(rep(1,k))

    Au<- matrix(0,k-1,k-1)
    Bu<- matrix(0,k-1,k)
    Cu<- matrix(0,k,k)

    for(i in 1:(k-1)){ for(j in 1:(k-1)){
      Au[i,j]<- sum( (pui[,i]/pii[i] - pui[,k]/pii[k])*(pui[,j]/pii[j] -
                                                          pui[,k]/pii[k]) )}}

    for(i in 1:(k-1)){ for(j in 1:k){
      Bu[i,j]<- sum( pui[,j]*aui[,j]*(pui[,i]/pii[i] - pui[,k]/pii[k] -
                                        delta[i,j]/pii[i] + delta[k,j]/pii[k] ) )}}

    for(i in 1:k){ for(j in 1:k){
      Cu[i,j]<- sum( pui[,i]*pui[,j]*aui[,i]*aui[,j] - delta[i,j]*bui[,i]*
                       pui[,i] ) }}

    invvmat<- rbind(cbind(Au,Bu),cbind(t(Bu),Cu))
    vmat<- solve(invvmat, tol=.Machine$double.xmin)

    # calculate DE, relative standard error, standard error
    dose<- exp(mu)
    re <- suppressWarnings(sqrt(diag(vmat)))[-c(1:(k-1))]
    re[is.nan(re)] <- NA

    sed<- dose*re
    estd<- rbind(dose,re,sed)

    # this calculates the proportional standard error of the proportion of grains
    # in the fitted components. However, the calculation is most likely erroneous.
    # rek<- sqrt(sum(vmat[1:(k-1),1:(k-1)]))
    # sep<-  c(sqrt(diag(vmat))[c(1:(k-1))],rek)

    # rename proportion
    estp <- pii

    # merge results to a data frame
    blk<- rep("    ",k)
    comp<- rbind(blk,round(estd,4),blk,round(estp,4))
    comp<- data.frame(comp,row.names=c("","dose (Gy)    ","rse(dose)    ",
                                       "se(dose)(Gy)"," ","proportion   "))
    names(comp) <- paste0("comp", 1:k)

    # calculate the log likelihood and BIC for a single component -- can
    # be useful to see if there is evidence of more than one component
    mu0<- sum(wu*yu)/sum(wu)
    fu0<-  rwu*exp(-0.5*wu*(yu-mu0)^2)
    L0<- sum( log((1/sqrt(2*pi))*fu0 ) )
    bic0<- -2*L0 + log(n)
    comp0<- round(c(exp(mu0),sigmab,L0,bic0),4)

    ## save results for k components in storage variables
    if (multiple.components) {

      # vector of indices needed for finding the dose rows of the summary
      # matrix - position 1,4,7...n
      pos.n<- seq(from = 1, to = n.components[cnt]*3, by = 3)

      # save results of each iteration to summary matrix
      for(i in 1:n.components[cnt]) {
        ## store De, SE, Proportion
        comp.n[pos.n[i] + 0:2, cnt] <- round(c(dose[i], sed[i], estp[i]), 4)
      }

      # save BIC and llik of each iteration to corresponding vector
      BIC.n[cnt]<- bic
      LLIK.n[cnt]<- llik

      # save mle matrix and grain probabilities to corresponding vector
      vmat.n[[cnt]]<- vmat
      grain.probability.n[[cnt]]<- as.data.frame(pui)

      # increase counter by one for next iteration
      cnt<- cnt+1
    }#EndOf::save intermediate results
  }##EndOf::calculation loop

  ##============================================================================##
  ## STATISTICAL CHECK
  ##============================================================================##

  if (multiple.components) {

    ## Likelihood ratio test: the difference in deviance is compared to a
    ## chi-squared distribution with degrees of freedom corresponding to the
    ## difference in number of parameters (number of components) to obtain a
    ## p-value.
    ## We also consider the likelihood of the single component model, so we
    ## can determine the significance of the 2-component model.
    LRT <- -2 * (c(L0, LLIK.n) - c(LLIK.n, 0))[1:length(n.components)]
    dfs <- diff(c(1, n.components))
    p.vals <- 1 - stats::pchisq(LRT, dfs)

    ## Add significance stars
    stars <- as.character(cut(p.vals,
                              breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                              labels = c("***", "**", "*", ".", " ")))

    ## merge BIC and llik scores to a single data frame, including those
    ## for the single component model
    results.n <- rbind(BIC = round(c(bic0, BIC.n), 3),
                       loglik = round(c(L0, LLIK.n), 3),
                       signif = c("", stars))
    colnames(results.n) <- c(1, n.components)
  }

  ## OUTPUT ---------
  ##============================================================================##
  if(verbose) {

    ## HEADER (always printed)
    cat("\n [calc_FiniteMixture]")

    # general information on sample and model performance
    cat("\n\n----------- meta data ------------")
    cat("\n n:                    ", n)
    cat("\n sigmab:               ", sigmab)
    cat("\n number of components: ", .collapse(n.components, quote = FALSE))

    ## OUTPUT WHEN ONLY ONE VALUE FOR n.components IS PROVIDED
    if (!multiple.components) {

      cat("\n llik:                 ", round(llik,4))
      cat("\n BIC:                  ", round(bic,3))

      # covariance matrix
      cat("\n\n--- covariance matrix of mle's ---\n\n")
      print(round(vmat,6))

      # fitted components
      cat("\n\n----------- components -----------\n\n")
      print(comp)

      # print (to 2 decimal places) the estimated probabilities of which component
      # each grain is in -- sometimes useful for diagnostic purposes
      if(grain.probability==TRUE) {
        cat("\n-------- grain probability -------\n\n")
        print(round(pui,2))
      }
    }#EndOf::Output for length(n.components) == 1

    ## output for single component
    cat("\n\n-------- single component --------")
    cat("\n mu:                    ", comp0[1])
    cat("\n sigmab:                ", comp0[2])
    cat("\n llik:                  ", comp0[3])
    cat("\n BIC:                   ", comp0[4])
    cat("\n\n")

    ##----------------------------------------------------------------------------
    ## OUTPUT WHEN ONLY >1 VALUE FOR n.components IS PROVIDED
    if (multiple.components) {

      ## final labeling of component and BIC/llik matrices

      ## create row labels in correct order (dose, se, prop)
      prefix <- paste0("c", 1:n.components[length(n.components)])
      n.lab <- unlist(lapply(prefix,
                             function(x) paste0(x, c("_dose", "_se", "_prop"))))

      # label columns and rows of summary matrix and BIC/LLIK data frame
      colnames(comp.n) <- n.components
      rownames(comp.n) <- n.lab

      ## CONSOLE OUTPUT

      # print component matrix
      cat("\n---------- k components ----------\n")
      print(round(comp.n, 2), na.print = "<NA>")

      # print BIC scores and LLIK estimates
      cat("\n------ statistical criteria ------\n")
      print(results.n, quote = FALSE, right = TRUE)

      ## print evaluation of statistical criteria
      # lowest BIC score
      cat("\n Lowest BIC score for k =", n.components[which.min(BIC.n)])

      # first significant increase in LLIK estimates
      if (!any(p.vals < 0.05)) {
        cat("\n No significant increase in maximum log-likelihood estimates.\n")
      } else {
        cat("\n First significant increase in maximum log-likelihood for",
            "k =", n.components[(p.vals < 0.05)][1], "\n")
      }

      cat("\n")
    }#EndOf::Output for length(n.components) > 1
  }

  ## RETURN VALUES --------
  ##============================================================================##

  # .@data$meta
  BIC <- data.frame(n.components = n.components,
                    BIC = if (multiple.components) BIC.n else bic)
  llik <- data.frame(n.components = n.components,
                     llik = if (multiple.components) LLIK.n else llik)

  # .@data$components
  comp.re<- t(rbind(round(estd,4),round(estp,4)))
  colnames(comp.re)<- c("de","rel_de_err","de_err","proportion")
  comp.re<- comp.re[,-2] # remove the relative error column

  # .@data$grain.probability
  grain.probability<- round(pui, 2)

  summary<- data.frame(comp.re)
  call<- sys.call()
  args<- list(sigmab = sigmab, n.components = n.components)

  # create S4 object
  results <- set_RLum(
    class = "RLum.Results",
    originator = "calc_FiniteMixture",
    data = list(
      summary=summary,
      data=data,
      args=args,
      call=call,
      mle = if (multiple.components) vmat.n else vmat,
      BIC = BIC,
      llik = llik,
      grain.probability = if (multiple.components) grain.probability.n else grain.probability,
      components = if (multiple.components) comp.n else comp.re,
      single.comp = data.frame(mu = comp0[1], sigmab = comp0[2],
                               llik = comp0[3], BIC = comp0[4])))

  if (anyNA(unlist(summary)) && verbose)
    .throw_warning("The model produced NA values: either the input data are ",
                   "inapplicable for the model, or the model parameters ",
                   "need to be adjusted (e.g. 'sigmab')")

  ##=========##
  ## PLOTTING -----------
  if (plot)
    try(do.call(.plot_FiniteMixture, c(results, as.list(sys.call())[-c(1,2)])))

  # Return values
  invisible(results)
}

## Function to plot an RLum.Results object generated by calc_FiniteMixture().
## This is a separate function to allows calling plot_RLum.Results() directly
## on the same object.
.plot_FiniteMixture <- function(object, ...) {
  if (length(object@data$args$n.components) == 1) {
    return()
  }

  ## deal with additional arguments
  extraArgs <- list(...)
  settings <- list(
      main = "Finite Mixture Model",
      pdf.weight = TRUE,
      pdf.sigma = "sigmab",
      pdf.colors = "gray",
      cex = 1,
      main.densities = "Normal distributions",
      main.proportions = "Proportion of components",
      main.criteria = "Statistical criteria",
      plot.proportions = TRUE,
      plot.criteria = TRUE
  )
  settings <- modifyList(settings, extraArgs)

  ## extract relevant data from object
  n.components <- object@data$args$n.components
  comp.n <- object@data$components
  sigmab <- object@data$args$sigmab
  BIC.n <- object@data$BIC$BIC
  LLIK.n <- object@data$llik$llik

  ## save previous plot parameter and set new ones
  par.default <- par(no.readonly = TRUE)
  on.exit(par(par.default), add = TRUE)

  ## DEVICE AND PLOT LAYOUT
  n.plots <- length(n.components) #number of PDF plots in plot area #1
  seq.matrix <- rbind(c(1:n.plots), c(1:n.plots))
  if (settings$plot.proportions)
    seq.matrix <- rbind(seq.matrix, rep(max(seq.matrix) + 1))
  if (settings$plot.criteria)
    seq.matrix <- rbind(seq.matrix, rep(max(seq.matrix) + 1))

  ## create device layout
  graphics::layout(seq.matrix)

  ## outer margins (bottom, left, top, right)
  par(oma = c(1, 5, 3, 5))

  ## general plot parameters (global scaling, allow overplotting)
  par(cex = 0.8 * settings$cex, xpd = NA)

  ## define colour palette for prettier output
  if (settings$pdf.colors == "colors") {
    col.n <- c("red3", "slateblue3", "seagreen", "tan3", "yellow3",
               "burlywood4", "magenta4", "mediumpurple3", "brown4", "grey",
               "aquamarine")
    poly.border <- FALSE
  }
  else if (settings$pdf.colors == "gray") {
    col.n <- grDevices::gray.colors(length(n.components)*2)
    poly.border <- FALSE
  }
  else if (settings$pdf.colors == "none") {
    col.n <- rgb(0, 0, 0, 0)
    poly.border <- TRUE
  }

  ##--------------------------------------------------------------------------
  ## PLOT 1: EQUIVALENT DOSES OF COMPONENTS

  ## calculate weighted density values
  .calc.density <- function(x, mu, sd, weights) {
    dnorm(x, mean = mu, sd = sd) * weights
  }

  max.dose <- max(object@data$data[, 1]) + sd(object@data$data[, 1]) / 2
  min.dose <- min(object@data$data[, 1]) - sd(object@data$data[, 1]) / 2

  ## determine y-axis scaling
  y.scale <- c(min.dose, max.dose)
  if ("dose.scale" %in% names(extraArgs)) {
    y.scale <- extraArgs$dose.scale
  }

  ## create empty plot without x-axis
  for (i in 1:n.plots) {
    pos.n <- seq(from = 1, to = n.components[i] * 3, by = 3)

    ## set margins (bottom, left, top, right)
    par(mar = c(2, 0, 2, 0))

    ## empty plot area
    plot(NA, NA,
         xlim = c(min(n.components)-0.2, max(n.components)+0.2),
         ylim = c(min(comp.n[pos.n, ] - comp.n[pos.n + 1, ], na.rm = TRUE),
                  max((comp.n[pos.n, ] + comp.n[pos.n + 1, ]) * 1.1, na.rm = TRUE)),
         ylab = "",
         xaxt = "n",
         yaxt = "n",
         xlab = "")

    ## add y-axis label (only for the first plot)
    if (i == 1) {
      mtext(expression(paste("D"[e]," [Gy]")), side = 2, line = 2.7,
            cex = settings$cex)
    }

    ## NORMAL DISTR. OF EACH COMPONENT

    if (settings$pdf.weight) {
      dens <- 0
      for (b in 1:max(n.components)) {
        comp.mu <- comp.n[pos.n[b], i]
        comp.sd <- if (settings$pdf.sigma == "se") comp.n[pos.n[b] + 1, i]
                   else comp.mu * sigmab
        if (!is.na(comp.mu) && !is.na(comp.sd)) {
          wi <- comp.n[pos.n[b] + 2, i]
          dens <- dens + sapply(0:max.dose, .calc.density,
                                comp.mu, comp.sd, wi)
        }
      }
    } else {
      ## estimate the y-scaling if no weights are used
      dens <- sapply(0:max.dose, .calc.density,
                     na.exclude(c(comp.n[pos.n, ])),
                     na.exclude(c(comp.n[pos.n + 1, ])), 1)
    }

    ## x-axis (density)
    if ("pdf.scale" %in% names(extraArgs)) {
      x.scale <- extraArgs$pdf.scale
    } else {
      x.scale <- max(dens) * 1.1
    }

    ## LOOP - iterate over number of components
    dens.sum <- 0
    for (j in 1:max(n.components)) {
      comp.mu <- comp.n[pos.n[j], i]
      comp.sd <- if (settings$pdf.sigma == "se") comp.n[pos.n[j] + 1, i]
                 else comp.mu * sigmab
      if (!is.na(comp.mu) && !is.na(comp.sd)) {
        ## calculate density values for 0 to maximum dose
        wi <- if (settings$pdf.weight) comp.n[pos.n[j] + 2, i] else 1
        dens <- sapply(0:max.dose, .calc.density,
                       comp.mu, comp.sd, wi)

        ## save density values in list for sum curve of gaussians
        dens.sum <- dens.sum + dens

        ## PLOT Normal Distributions
        par(new = TRUE)
        plot(dens, 1:length(dens) - 1,
             type = "l", yaxt = "n", xaxt = "n", col = col.n[j], lwd = 1,
             ylim = y.scale,
             xlim = c(0, x.scale),
             xaxs = "i", yaxs = "i",
             ann = FALSE, xpd = FALSE)

        ## add x-axis with corrected tick positions
        if (j == 1)
          axis(side = 1, labels = paste("k =", n.components[i]),
               at = x.scale / 2, tick = FALSE, line = -1)

        # draw coloured polygons under curve
        polygon(x = c(0, min(dens), dens, 0),
                y = c(0, 0, 0:max.dose, max.dose),
                col = adjustcolor(col.n[j], alpha.f = 0.66),
                yaxt = "n", border = poly.border, xpd = FALSE, lty = 2, lwd = 1.5)
      }
    }##EndOf::Component loop

    ## Add sum of Gaussian curve
    par(new = TRUE)
    plot(dens.sum, 1:length(dens) - 1,
         type = "l", yaxt = "n", xaxt = "n", col = "black",
         lwd = 1.5, lty = 1,
         ylim = y.scale,
         xlim = c(0, x.scale),
         xaxs = "i", yaxs = "i", ann = FALSE, xpd = FALSE)

    ## draw additional info during first k-cycle
    if (i == 1) {

      ## plot title
      mtext(settings$main.densities,
            side = 3, font = 2, line = 0, adj = 0, cex = 0.8 * settings$cex)

      ## main title
      mtext(settings$main,
            side = 3, font = 2, line = 3.5, adj = 0.5, cex = settings$cex,
            at = graphics::grconvertX(0.5, from = "ndc", to = "user"))

      ## subtitle
      has.nas <- anyNA(unlist(object$summary))
      subtitle <- as.expression(bquote(italic(sigma[b]) == .(sigmab) ~
                                         "|" ~ n == .(length(object@data$data[, 1])) ~
                                         .(if (has.nas) "| The model produced NA values"
                                           else "")
                                       ))
      mtext(subtitle,
            side = 3, font = 1, line = 2.2, adj = 0.5,
            at = graphics::grconvertX(0.5, from = "ndc", to = "user"),
            col = ifelse(has.nas, 2, 1),
            cex = 0.9 * settings$cex)

      ## x-axis label
      mtext("Density [a.u.]",
            side = 1, line = 1.5, adj = 0.5, cex = settings$cex,
            at = graphics::grconvertX(0.5, from = "ndc", to = "user"))

      ## draw y-axis with proper labels
      axis(side = 2, labels = TRUE)
    }

    if (settings$pdf.colors == "colors") {
      ## create legend labels
      dose.lab.legend <- paste0("c", 1:n.components[length(n.components)])

      ncol.temp <- min(max(n.components), 8)
      yadj <- if (max(n.components) > 8) 1.025 else 0.93

      ## add legend
      if (i == n.plots) {
        legend(graphics::grconvertX(0.55, from = "ndc", to = "user"),
               graphics::grconvertY(yadj, from = "ndc", to = "user"),
               legend = dose.lab.legend,
               col = col.n[1:max(n.components)],
               pch = 15, adj = c(0,0.2), pt.cex = 1.4,
               bty = "n", ncol = ncol.temp, x.intersp = 0.4)

        mtext("Components: ", cex = 0.8 * settings$cex,
              at = graphics::grconvertX(0.5, from = "ndc", to = "user"))
      }
    }

  } ## EndOf::k-loop and Plot 1

  ## this value (found by trial and error) should be summed to xlim to remove
  ## the empty gaps before the first bar and after the last in the proportions
  ## plot, and for better centring of points in the statistical criteria plot
  xlim.adj <- length(n.components) / (length(n.components) + 18) * c(1, -1)

  ##--------------------------------------------------------------------------
  ## PLOT 2: PROPORTION OF COMPONENTS
  if (settings$plot.proportions) {

    ## create matrix with proportions from a subset of the summary matrix
    prop.matrix <- comp.n[pos.n + 2, ] * 100

    ## stacked barplot of proportions without x-axis
    graphics::barplot(prop.matrix,
                      width = 1,
                      xlim = c(0, length(n.components)) + xlim.adj,
                      ylim = c(0, 100),
                      axes = TRUE,
                      space = 0,
                      col = col.n,
                      xpd = FALSE,
                      xaxt = "n")

    ## y-axis label
    mtext("Proportion [%]",
          side = 2,line = 3, cex = settings$cex)

    ## add x-axis with corrected tick positions
    axis(side = 1, labels = paste("k =", n.components),
         at = 1:length(n.components) - 0.5, tick = FALSE, line = -1)

    ## draw a box (not possible with barplot())
    graphics::box(lty = 1, col = "black")

    ## add subtitle
    mtext(settings$main.proportions,
          side = 3, font = 2, line = 0, adj = 0, cex = 0.8 * settings$cex)
  }

  ##--------------------------------------------------------------------------
  ## PLOT 3: BIC & LLIK

  if (settings$plot.criteria) {
  ## prepare scaling for both y-axes
  BIC.scale <- c(min(BIC.n) * if (min(BIC.n) < 0) 1.2 else 0.8,
                max(BIC.n) * if (max(BIC.n) < 0) 0.8 else 1.2)
  LLIK.scale <- c(min(LLIK.n) * if (min(LLIK.n) < 0) 1.2 else 0.8,
                  max(LLIK.n) * if (max(LLIK.n) < 0) 0.8 else 1.2)

  ## plot BIC scores
  plot(1:length(n.components) - 0.5, BIC.n,
       main = "",
       type = "b",
       pch = 22,
       cex = 1.5,
       xlim = c(0, length(n.components)) + xlim.adj,
       ylim = BIC.scale,
       xlab = "", xaxt = "n",
       ylab = expression(paste("BIC")),
       cex.lab = 1.25)

  ## following plot should be added to previous
  par(new = TRUE)

  ## plot LLIK estimates
  plot(1:length(n.components) - 0.5, LLIK.n,
       xlim = c(0, length(n.components)) + xlim.adj,
       ylim = LLIK.scale,
       axes = FALSE, type = "b", pch = 16, ylab = "", xlab = "", lty = 2, cex = 1.5)

  ## add x-axis with corrected tick positions
  axis(side = 1, labels = paste("k =", n.components),
       at = 1:length(n.components) - 0.5, tick = FALSE, line = -1)

  ## subtitle
  mtext(settings$main.criteria,
        side = 3, font = 2, line = 0, adj = 0, cex = 0.8 * settings$cex)

  ## second y-axis (LLIK) with label
  axis(side = 4)
  mtext(bquote(italic(L)[max]),
        side = 4,line = 3, cex = 1.3 * settings$cex)

  ## legend
  legend("topleft",
         legend = c("BIC", as.expression(bquote(italic(L)[max]))),
         pch = c(22, 16), pt.bg = c("white", "black"), pt.cex = 1.3,
         bty = "n", horiz = FALSE, inset = 0.01)
  }
}

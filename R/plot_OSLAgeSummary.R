#'@title Plot Posterior OSL-Age Summary
#'
#'@description A graphical summary of the statistical inference of an OSL age
#'
#'@details The function is called automatically by [combine_De_Dr]
#'
#'@param object [RLum.Results-class], [numeric] (**required**): an object produced
#' by [combine_De_Dr]. Alternatively, a [numeric] vector of a parameter from an MCMC process
#'
#'@param level [numeric] (*with default*): probability of shown credible interval
#'
#'@param digits [integer] (*with default*): number of digits considered for the calculation
#'
#'@param verbose [logical] (*with default*): enable/disable additional terminal output
#'
#'@param ... further arguments to modify the plot, supported: `xlim`, `ylim`, `xlab`, `ylab`,
#' `main`, `lwd`, `lty`, `col`, `polygon_col`, `polygon_density`, `rug`
#'
#'@return A posterior distribution plot and an [RLum.Results-class]
#' object with the credible interval.
#'
#'@author Anne Philippe, Université de Nantes (France),
#' Jean-Michel Galharret, Université de Nantes (France),
#' Norbert Mercier, IRAMAT-CRP2A, Université Bordeaux Montaigne (France),
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#'@section Function version: 0.1.0
#'
#'@seealso [combine_De_Dr], [plot.default], [rjags::rjags]
#'
#'@keywords hplot dplot
#'
#'@examples
#'##generate random data
#'set.seed(1234)
#'object <- rnorm(1000, 100, 10)
#'plot_OSLAgeSummary(object)
#'
#'@md
#'@export
plot_OSLAgeSummary <- function(
  object,
  level = 0.95,
  digits = 1L,
  verbose = TRUE,
  ...
){
# Integrity tests ---------------------------------------------------------
  if(is(object, "RLum.Results") &&
     object@originator %in% c(".calc_BayesianCentralAgeModel", ".calc_IndividualAgeModel"))
    object <- get_RLum(object, data.object = "A")

  if(is(object, "RLum.Results") && object@originator == "combine_De_Dr")
    object <- get_RLum(object, data.object = "Ages")


  if(!is(object, "numeric")) {
    stop(paste0("[plot_OSLAgeSummary()] class ", class(object)[1],
                " not supported as input for object!"),call. = FALSE)
  }

  ## A should be a matrix
  A <- as.matrix(object, ncol = 1)

# Run calculations --------------------------------------------------------
  ## use our internal function instead of Archaeophase to avoid the decency hell
  CI <- round(.calc_HPDI(A, prob = level[1]), digits[1])
  Bayes_est_mean <- round(mean(A), digits = digits)
  Bayes_est_sd <- round(sd(A), digits = digits)

# Terminal output ---------------------------------------------------------
  if(verbose){
    cat("\n[plot_OSLAgeSummary()]\n")
    cat(paste0(" Credible Interval (", level * 100 ),"%): ",paste(CI[1,], collapse = " : "), "\n")
    cat(paste0(" Bayes estimate (posterior mean \u00b1 sd): ", Bayes_est_mean[1], " \u00b1 ", Bayes_est_sd[1]),"\n")

  }

# Plot output -------------------------------------------------------------
  density_A <- density(A)

  plot_settings <- modifyList(x = list(
    xlim = range(A),
    ylim = range(density_A$y),
    main = "Posterior distr. of A",
    xlab = "Age [ka]",
    ylab = "Density",
    lwd = 1,
    lty = 1,
    col = "black",
    polygon_col = rgb(1,0,0,0.3),
    polygon_density = 20,
    rug = FALSE

  ), val = list(...))

  plot(
    x = density_A$x,
    y = density_A$y,
    xlim = plot_settings$xlim,
    ylim = plot_settings$ylim * 1.07,
    xlab = plot_settings$xlab,
    ylab = plot_settings$ylab,
    main = plot_settings$main,
    type = "l",
    lwd = plot_settings$lwd,
    lty = plot_settings$lty,
    col = plot_settings$col
  )

  ## add lines on the top for the CI
  lines(x = c(CI[1,]), y = rep(par()$usr[4] * 0.92, 2))
  lines(x = rep(CI[1,1], 2), y = c(par()$usr[4] * 0.91, par()$usr[4] * 0.92))
  lines(x = rep(CI[1,2], 2), y = c(par()$usr[4] * 0.91, par()$usr[4] * 0.92))

  ## add polygon fill
  polygon(
    x = c(density_A$x, rev(density_A$x)),
    y = c(density_A$y, rep(0, length(density_A$y))),
    col = plot_settings$polygon_col,
    lty = 0,
    density = NULL
  )

  ## add CI
  xy_id <- density_A$x >= CI[1,1] & density_A$x <= CI[1,2]
  polygon(
    x = c(density_A$x[xy_id], rev(density_A$x[xy_id])),
    y = c(density_A$y[xy_id], rep(0, length(density_A$y[xy_id]))),
    col = "black",
    lwd = 0.5,
    border = TRUE,
    density = plot_settings$polygon_density
  )

  ##add rug
  if(plot_settings$rug) rug(A)

  ## add text
  text(x = density_A$x[xy_id][1], y = density_A$y[xy_id][2], CI[1,1], pos = 2, cex = 0.6)
  text(x = max(density_A$x[xy_id]), y = rev(density_A$y[xy_id])[1], CI[1,2], pos = 4, cex = 0.6)
  text(
    x = median(density_A$x[xy_id]),
    y = par()$usr[4] * 0.91,
    labels = paste0("CI: ", level[1] * 100, "%"),
    pos = 3,
    cex = 0.6
  )

# Return ------------------------------------------------------------------
  return(set_RLum("RLum.Results",
    data = list(
      Estimate = Bayes_est_mean,
      Credible_Interval = CI,
      level = level),
  info = list(call = sys.call())))

}

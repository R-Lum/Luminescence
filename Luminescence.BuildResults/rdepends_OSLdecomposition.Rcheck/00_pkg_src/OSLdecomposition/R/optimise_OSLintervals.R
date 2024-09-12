#' Find adequate integration intervals for CW-OSL decomposition
#'
#' This function defines integration intervals for CW-OSL component separation with [decompose_OSLcurve].
#' The underlying iterative optimisation process aims for minimum cross-correlation between the signal components.
#'
#' The precision of the component separation with [decompose_OSLcurve] and the impact of
#' systematic decay rate errors on the component separation depends on the integration interval definition.
#' This function minimises the influence of an under/over-estimated decay rate to the
#' signal intensity calculation of other component. This is done by maximizing the denominator
#' determinant in Cramers rule, see Mittelstraß (2019) for details. For maximisation, the iterative
#' evolutionary algorithm of Storn and Price (1997) is used, available in *R* through [DEoptim::DEoptim].
#'
#' The inclusion of a background component is supported, see [decompose_OSLcurve] for details.
#'
#'
#' @param components [data.frame] or [numeric] vector (**required**):
#' Table or vector containing the decay constants of the signal components.
#' A [data.frame] must contain a column `$lambda`. Usually the [data.frame] is provided
#' by [fit_OSLcurve].
#'
#' @param curve [data.frame] or [matrix] or [RLum.Data.Curve-class] (*optional*):
#' OSL signal curve which serves as time axis template.
#' The input curve will be used to define `channel.width` and `channel.number`
#'
#' @param channel.width [numeric] (*optional*):
#' Channel width in seconds. Necessary if `curve` is not given.
#'
#' @param channel.number [numeric] (*optional*):
#' Number of channels resp. data points. Necessary if `curve` is not given.
#'
#' @param t.start [numeric] (*with default*):
#' Starting time of the first interval, per default the start of the measurement.
#'
#' @param t.end [numeric] (*optional*):
#' End time of the last interval, per default the end of the measurement.
#'
#' @param background.component [logical] (*with default*):
#' If `TRUE`, an additional interval for a component with a decay rate of zero will be
#' determined. This enables the calculation of the signal background level during the signal
#' decomposition with [decompose_OSLcurve].
#'
#' @param verbose [logical] (*with default*):
#' Enables console text output.
#'
#' @param parallel.computing [logical] (*with default*):
#' Enables the use of multiple CPU cores. This increases the execution speed significantly
#' but may need administrator rights and/or a firewall exception.
#' See [DEoptim::DEoptim.control] for further information.
#'
#' @return
#' The input table `components` [data.frame] will be returned with four additional columns:
#' `$t.start`, `$t.end` defining the time intervals and `$ch.start`, `$ch.end` assigning those intervals to channel indicies.
#' If a [numeric] vector is given as input, a new [data.frame] will be returned.
#'
#' @section Last updates:
#'
#' 2020-08-23, DM: Replaced previous maximum searching algorithm with [DEoptim::DEoptim]
#'  (**update may have changed analysis results**)
#'
#' 2020-10-29, DM: Added `parallel.computing` argument; enhanced roxygen documentation (*minor update*)
#'
#' @author
#' Dirk Mittelstraß, \email{dirk.mittelstrass@@luminescence.de}
#'
#' Please cite the package the following way:
#'
#' Mittelstraß, D., Schmidt, C., Beyer, J., Heitmann, J. and Straessner, A.:
#' R package OSLdecomposition: Automated identification and separation of quartz CW-OSL signal components, *in preparation*.
#'
#' @seealso [decompose_OSLcurve], [RLum.OSL_decomposition], [DEoptim::DEoptim], [fit_OSLcurve]
#'
#' @references
#' Mittelstraß, D., 2019. Decomposition of weak optically stimulated luminescence signals and its application in retrospective dosimetry at quartz (Master thesis). TU Dresden, Dresden.
#'
#' Storn, R., Price, K., 1997. Differential Evolution – A Simple and Efficient Heuristic for global Optimization over Continuous Spaces. Journal of Global Optimization 11, 341–359.
#'
#' @examples
#'
#' A <- optimise_OSLintervals(c(2, 0.5, 0.02), channel.width = 0.1, channel.number = 200)
#' print(A, row.names = FALSE)
#'
#' @md
#' @export

optimise_OSLintervals <- function(
  components,
  curve = NULL,
  channel.width = NA,
  channel.number = NA,
  t.start = 0,
  t.end = NA,
  background.component = FALSE,
  verbose = TRUE,
  parallel.computing = FALSE
){

  # Changelog:
  # * 2018-04-05, DM: first running version
  # * 2018-06-16, DM: added 2-component and 1-component case
  # * 2018-06-19, DM: changed t0 and t3 parameter to t.start and t.end and added full increment proof
  # * 2018-06-24, DM: changed data structure to get static tables
  # * 2019-03-21, DM: Rewritten for arbitrary component numbers and changed data structure again
  # * 2019-10-02, DM: added optional determination of background-fitting interval
  # * 2020-08-23, DM: Replaced own minimum searching algorithm with DEoptim
  # * 2020-08-27, DM: Renamed function; More input classes allowed
  # * 2020-10-29, DM: Added `parallel.computing` argument; enhanced roxygen documentation
  #
  # ToDo:
  # * alter argument `curve` to `template.curve`

  # Hidden parameters
  silent <- FALSE

  # Check if 'components' is of valid type
  if (inherits(components, "numeric")) {
    components <- data.frame(name = paste0("Component ", 1:length(components)),
                             lambda = sort(components, decreasing = TRUE))

  }else if(inherits(components, "data.frame")){
    if (!("lambda" %in% colnames(components)) | !("name" %in% colnames(components))) {
      stop("[decompose_OSLcurve()] Error: Input object 'components' needs at least a column '$lambda' and a column '$name'!")}

  }else{
    stop("[optimise_OSLintervals()] Error: Input object 'components' is not of type 'numeric vector' or 'data.frame' !")}


  # check if template curve or channel parameters are given
  if (!is.null(curve)) {

    if(!inherits(curve, c("RLum.Data.Curve", "data.frame", "matrix"))){
      stop("[optimise_OSLintervals()] Error: Input object 'curve' is not of type 'RLum.Data.Curve' or 'data.frame' or 'matrix'!")}

    if(inherits(curve, "RLum.Data.Curve")) curve <- as.data.frame(Luminescence::get_RLum(curve))

    if (!("time" %in% colnames(curve)) ||
        !("signal" %in% colnames(curve))) {
      curve <- data.frame(time = curve[,1],
                          signal = curve[,2])}

    # now take the parameters of interest
    dt <- curve$time[2] - curve$time[1]
    N <- length(curve$time)

  } else if ((!is.na(channel.width)) && (!is.na(channel.number))) {

    dt <- channel.width
    N <- channel.number

  } else {
    stop("[optimise_OSLintervals()] Error: No template curve or channel parameters are given")}



  # If the background also shall be fitted, add another component without lambda value
  if (background.component) {

    if (!is.na(components$lambda[nrow(components)])) {
      new.row <- components[nrow(components),]
      rownames(new.row) <- "Background"
      new.row[1:length(new.row)] <- NA
      new.row[1]  <- "Background"
      components <- rbind(components, new.row)}

  } else {

    if (is.na(components$lambda[nrow(components)])) {
      components <- components[1:(nrow(components) - 1),]}}

  K <- nrow(components)
  lambdas <- components$lambda

  # round start and end point to full increments
  t0 <- floor(t.start / dt) * dt

  ## set t.end if not preset
  if (is.na(t.end) | (t.end > N * dt) | (t.end < K * dt)) {
    t.end <- N * dt
  } else {
    t.end <- floor(t.end / dt) * dt}


  ########## is there just one component? ###########

  if (K == 1) {

    # define full curve as integration interval
    components$t.start <- t0
    components$t.end <- t.end
    components$ch.start <- 1 + floor(t0 /  dt)
    components$ch.end <- ceiling(t.end / dt)

    if (verbose) {
      cat("Just one component defined: Full measurement duration set as integration interval\n" )}

    invisible(components)}

  ########## Create matrix ###########

  ## channels: just the end of an interval. For example: 1:3 / 4:5 / 6:7 becomes c(0,3,5,7)
  calc_determinant <- function(input, lamb = lambdas, k = K, n = N, DT = dt, t_start = t0, t_end = t.end) {

    channels <- round(input)
    if (any(duplicated(channels))) return(Inf)

    # Add first and last interval border
    channels <- c(t_start / DT,
                  sort(channels),
                  t_end / DT)

    # Create square matrix
    M <- matrix(0, k, k)

    # Build matrix elements
    for (i in c(1:k)) {
      for (j in c(1:k)) {

        if (is.na(lamb[j])) {

          # Background interval
          M[i, j] <- channels[i + 1] * DT - channels[i] * DT

        } else {

          # P <- exp(-t0 * f.fast) - exp(-t1 * f.fast)
          M[i, j] <- exp(- channels[i] * DT * lamb[j]) - exp(- channels[i + 1] * DT * lamb[j])}}}

    # calc determinant; we want to maximize the determinant, so the minimisation value has to be the reziproke
    goal <- 1 / abs(det(M))

    return(goal)}



  ########################################### DEoptim ##############################################

  # start parameters for the choosable intervals divider
  min.ch <-  1:(K - 1) + t0 / dt
  max.ch <- (t.end / dt - K + 1):(t.end / dt - 1)

  # Perform differential evolution (DE). As minimisation function, use calc_RSS()
  det_min <- try(DEoptim::DEoptim(
    fn = calc_determinant,
    lower = min.ch,
    upper = max.ch,
    control = DEoptim::DEoptim.control(
      NP = K * 30,
      strategy = 2,
      itermax = 100,
      c = 0.2,
      reltol = 1e-6,
      steptol = 20,
      parallelType = parallel.computing,
      packages = c("OSLdecomposition"),
      parVar = c("lambdas", "K", "N", "dt", "t0", "t.end"),
      trace = FALSE)),
    silent = silent)

  # Did the DE algorithm break?
  if (methods::is(det_min)[1] == "try-error"){

    #if (verbose & (is(DE_min)[1] == "try-error")) cat(DE_min[1])
    stop("[decompose_OSLcurve()] Error: Differential evolution failed. Please reconsider input parameter\n")}

  # Otherwise, extract results
  interval_divider <- sort(round(det_min$optim$bestmem))
  det_value <- 1 / det_min$optim$bestval



  if (verbose) {
    cat("Maximum determinant =", formatC(det_value, digits = 4),
        "with interval dividing channels at i =", paste0(interval_divider, collapse = ", " ), "\n" )}

  components$t.start <- unlist(c(t0, interval_divider * dt))
  components$t.end <- unlist(c(interval_divider * dt, t.end))
  components$ch.start <- unlist(c(1 + floor(t0 /  dt), interval_divider + 1))
  components$ch.end <- unlist(c(interval_divider, ceiling(t.end / dt)))

  invisible(components)
}

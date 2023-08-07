#' Multi-exponential CW-OSL decomposition
#'
#' Function for determining the signal component amplitudes of a multi-exponential decay curve if
#' the signal component decay parameters are already given. Thus, this function decomposes CW-OSL
#' curves with known components of unknown intensity.
#'
#' The function assumes multiple exponentially decaying signal components with first-order kinetics:
#'
#' \deqn{I(t) = n_1 \lambda_1 exp(-\lambda_1 t) + n_2 \lambda_2 exp(-\lambda_2 t) + ... + n_K \lambda_K exp(-\lambda_K t)}
#'
#' with \eqn{I(t)} the CW-OSL signal, \eqn{n} the signal component intensity,
#' \eqn{\lambda} the signal component decay constant and \eqn{K} the number of signal components.
#' For the actual decomposition procedure, the integrated version of this formula is used, see Mittelstrass et al. (2021) for details.
#'
#' **Decomposition algorithm**
#'
#' The calculation procedure depends on the function argument `algorithm`.
#' This function includes two different decomposition algorithms: `"det"` for **det**erminant solution
#' and `"nls"` for **n**onlinear **l**east **s**quares estimate
#'
#' `algorithm = "det"` (default)
#'
#' The function calculates the CW-OSL component intensities by building an equation system
#' which is then solved by a determinant-based approach (Cramers rule). This purely analytical
#' approach gives the algorithm a solution in all possible cases, even if the measurement consists just of noise
#' or the wrong model is used. There are also no 'false minima' events.
#' The statistical error is calculated by applying the *propagation of uncertainty* method on Cramers rule.
#'
#' The precision of this algorithm as well as the propagation of eventual systematic errors of the decay rate values,
#' depend on the integration intervals, given by the columns `$t.start`, `$t.end`, `$ch.start` and `$ch.end`
#' of the [data.frame] used as input for the argument `components`.
#' In principle, these can be chosen freely. Reasonable integration intervals are defined by [optimise_OSLintervals].
#' If not defined, the logarithmic mean values between life times (reciprocal decay rate) of subsequent components are
#' used as interval borders.
#'
#' `algorithm = "nls"`
#'
#' As alternative algorithm, Levenberg-Marquardt nonlinear regression is available, see [minpack.lm::nlsLM] for details.
#' The results are identical to that of the `"det"` algorithm in accuracy and precision. But there is the slight chance (< 1 %)
#' of fitting failure when using the `"nls"` algorithm. Also, the statistical errors are underestimated by 20-80 %
#' in most cases. As advantage, the `"nls"` algorithm is less sensitive against systematic errors
#' caused by uncorrected signal background.
#'
#' `algorithm = "det+nls"`
#'
#' Both algorithms can be combined. Then, `"det"` provides the startings values and the error estimations for
#' `"nls"` and returns replacement results, in case `"nls"` fails. `"nls"` compensates for potential systematic
#' errors in the fast and medium components intensity values due to uncorrected signal background. However, the
#' background signal will still affect slow component results. The slowest component will be overestimated while
#' the second slowest component will be underestimated. If these components are of particular interest,
#' it is recommended to set `background.fitting = TRUE`
#'
#' All three methods were tested at 5x10^6 simulated CW-OSL curves by Mittelstrass (2019) for their performance
#' (+++ reliable results in all cases; ++ reliable in >95% of cases: + reliable in most cases):
#'
#' \tabular{llll}{
#'  **Characteristics** \tab **det** \tab **nls** \tab **det+nls** \cr
#'  Decomposition success rate \tab 100 % \tab >99 % \tab 100 % \cr
#'  Component intensity accuracy \tab +++ \tab +++ \tab +++ \cr
#'  Accuracy in case of uncorrected background \tab + \tab ++ \tab ++ \cr
#'  Error estimation accuracy \tab +++ \tab + \tab ++
#' }
#'
#' In summary, `algorithm = "det"` is recommended for the most cases. If the signal background level is
#' significant (> 2 % of initial signal) but was not corrected, `algorithm = "det+nls"` is the better choice.
#' Setting `background.fitting = TRUE` is usually not recommended, only in case slow components shall
#' be investigated in measurements with uncorrected background.
#'
#' **Error estimation**
#'
#' In case of `algorithm = "det"` or `"det+nls"` the Propagation of Uncertainty method is used to
#' transform signal bin error values (column `$bin.error`) into component intensity error values (column `$n.error`). The signal bin error
#' calculation depends on the argument `error.estimation`, see below.
#' If `algorithm = "nls"` is used, the error values provided by [minpack.lm::nlsLM] are returned.
#'
#' `error.estimation = "empiric"` (default)
#'
#' The standard deviation of each signal bin (signal bin = signal value of an integrated time interval) is
#' calculated from the *corrected sample variance* between the CW-OSL model and the actual CW-OSL curve
#' for that interval. Thus, statistical errors are monitored accurately without any prior knowledge required.
#' However, potential systematic errors are monitored insufficiently. Also, at least two (better more) data points
#' per signal bin are needed to estimate its standard deviation. If a signal bin consists just of one data point,
#' its square root value is taken as standard deviation, in accordance to the Poisson distribution.
#'
#' `error.estimation = "poisson"` or [numeric] value
#'
#' Alternatively the standard error can be calculated by approximating a **Poisson** distributed signal error,
#' known as *Shot noise*. This is suitable if the lack of data points on the x-axis circumvents an empiric error
#' estimation, like with spatially or spectrally resolved CCD measurements. Also the parameter can be set to a [numeric]
#' value, which  represents the detector noise in *cts / s* and is assumed to be normal distributed.
#' The detector noise will be added on top of the Poisson distributed shot noise.
#'
#' `error.estimation = "only.bin.RSS"`
#'
#' The error estimation is omitted but the residual sum of squares (RSS) between input curve and combined
#' signal component curves is calculated. However, the RSS value is divided into sections according to
#' the signal bins (column `$bin.RSS`). The full RSS value can be calculated by summing over the complete column.
#' The RSS value is usually used a minimization target in fitting algorithms, like done in [fit_OSLcurve].
#' The values of the `$bin.RSS` column allows for weighted fitting by applying pre-factors to the bin RSS values.
#' For further speed advance, the calculation of `$components$n.residual` and `$components$initial.signal` is
#' also omitted.
#'
#' `error.estimation = "none"`
#'
#' The error estimation is omitted. This option saves significant computing time, if the error estimation is
#' not of significance. For further speed advance, the calculation of `$components$n.residual` and
#' `$components$initial.signal` is also omitted.
#'
#' *Systematic errors*
#'
#' The ratio of the error values of both error estimation methods can be used to detect (but not quantify) systematic
#' errors. `"poisson"` error values are not affected by systematic errors, while `"empiric"` errors are.
#' If the detector noise is known and taken into account, the relation between both values for a given
#' signal bin should be about \eqn{empiric / poisson = 1}. In case of systematic errors, this ratio increases.
#'
#'
#'
#' @param curve [data.frame] or [matrix] or [RLum.Data.Curve-class] (**required**):
#' CW-OSL curve x-Axis: `$time` or first column as measurement time (must have constant time intervals);
#' y-Axis: `$signal` or second column as luminescence signal. Further columns will be ignored.
#'
#' @param components [data.frame] or [numeric] vector (**required**):
#' Either a vector containing the decay parameters of the CW-OSL components or a table (data.frame), usually the table returned by [fit_OSLcurve].
#' In case of a vector: It is recommended to use less than 7 parameters. The parameters will be sorted in decreasing order.
#' In case of a data.frame, one column must be named `$lambda`.
#' It is recommended to provide also integration interval parameters (columns `$t.start`, `$t.end`, `$ch.start`, `$ch.end`),
#' which can be found by applying [optimise_OSLintervals] to the global mean curve, calculated by [sum_OSLcurves].
#' If one or more column is missing, a simple interval definition algorithm is run automatically, see section **Details**.
#'
#' @param background.fitting [logical] (*with default*):
#' if `TRUE`, an additional signal component with a decay rate of \eqn{\lambda = 0} is included.
#' This allows for an accurate estimation of slow component intensities if the data is not background
#' corrected. However, the additional component reduces the overall precision of the algorithm.
#' It can also cause implausible slow component results if the measurement duration is not sufficiently long (> 30 s).
#'
#' @param algorithm [character] string (*with default*):
#' Choice of curve decomposition approach. Either `"det"` or `"det+nls"` or `"nls"`, see section **Details**.
#'^^
#' @param error.estimation [character] string (*with default*):
#' integral error estimation approach, either `"empiric"` or `"poisson"` or a [numeric] value or `"none"`,
#' see section **Details**. This argument has no effect if `algorithm = "nls"`.
#'
#' @param verbose [logical] (*with default*):
#' enables console text output
#'
#' @return
#' The input table **components** [data.frame] will be returned with added or overwritten
#' columns: `$n`, `$n.error`, `$n.residual`, `$bin`, `$bin.error`, `$bin.RSS`, `$initial.signal`.
#' Which columns are written depends on the selected parameters. If an input data.frame contains already
#' one of the above columns but parameters are selected which do not re-calculate the values, the values
#' of the columns are set to `NA`.
#'
#' @section Last updates:
#'
#' 2022-07-25, DM: Extended algorithm for bin-wise RSS calculation and added error estimation option "only.bin.RSS"
#'
#' @author
#' Dirk Mittelstraß, \email{dirk.mittelstrass@@luminescence.de}
#'
#' Please cite the package the following way:
#'
#' Mittelstraß, D., Schmidt, C., Beyer, J., Heitmann, J. and Straessner, A.:
#' R package OSLdecomposition: Automated identification and separation of quartz CW-OSL signal components, *in preparation*.
#'
#' @seealso [fit_OSLcurve], [optimise_OSLintervals], [RLum.OSL_decomposition], [minpack.lm::nlsLM]
#'
#' @references
#'
#' Mittelstraß, D., 2019. Decomposition of weak optically stimulated luminescence signals and
#' its application in retrospective dosimetry at quartz (Master thesis). TU Dresden, Dresden.
#'
#' @examples
#'
#' # Set some arbitrary decay parameter for a dim CW-OSL measurement of quartz
#' components <- data.frame(name = c("fast", "medium", "slow"),
#'                          lambda = c(2, 0.5, 0.02),
#'                          n = c(1000, 1000, 10000))
#'
#' # Simulate the CW-OSL curve and add some signal noise and some detection background
#' curve <- simulate_OSLcomponents(components, simulate.curve = TRUE,
#'                                 add.poisson.noise = TRUE, add.background = 40)
#'
#' # Decompose the simulated curve
#' components <- decompose_OSLcurve(curve, components)
#'
#' # Display the component separation results
#' plot_OSLcurve(curve, components)
#'
#' ### Decomposition including signal background fitting:
#'
#' # Define optimized integration intervals, including an interval for the background
#' components <- optimise_OSLintervals(components, curve, background.component = TRUE)
#'
#' # Decompose again and view results
#' components <- decompose_OSLcurve(curve, components, background.fitting = TRUE)
#' plot_OSLcurve(curve, components)
#'
#' @md
#' @export
decompose_OSLcurve <- function(
    curve,
    components,
    background.fitting = FALSE,
    algorithm = "det", # "det", "nls", "det+nls"
    error.estimation = "empiric",   # "poisson", "empiric", "only.bin.RSS", "none", numeric value
    verbose = TRUE
){

  # Changelog:
  # * winter 2012/13: first basic version, used for Bachelor thesis DM
  # * autumn 2013   : added empiric error estimation, shown in germanLED Freiberg 2013
  # * 2014-11-??, SK: formatted into RLuminecence package standard
  # * 2014-11-07, DM: Binomial error propagation added
  # * 2018-05-04, DM: added residuals for n values (necessary for slow component dosimetry) and many little tweaks
  # * 2018-06-22, DM: added decomposition of data sets with just 1 or 2 components
  # * 2018-06-22, DM: added negative.values.to.zero and set it on TRUE as default (on request of C. Schmidt) (later removed)
  # * 2018-07-05, DM: overworked error estimation; replaced binomial with poisson error approach; added auto-switch to poisson if integral length = 1; integrated background.noise into error
  # * 2019-03-29, DM: Rewritten function for several purposes: 1. working now with any number of components  2. shorter and more elegant code 3. data format more suitable for markdown/shiny applications.
  # * 2019-09-09, DM: Added anticorrelating covariance terms into error estimation (later removed)
  # * 2019-09-25, DM: Merged function with decompose_OSLalternatively() and added algorithm argument
  # * 2019-09-25, DM: Deleted unnecessary functions (negative.values.to.zero, offset, anticorrelation)
  # * 2019-10-02, DM: Added optional background fitting
  # * 2020-04-06, DM: Added 'initial.signal' column in output data.frame; cleaned print output
  # * 2020-07-20, DM: Added algorithm for fast interval definition based on logarithmic means; More input data checks
  # * 2020-08-27, DM: Replaced [nls] function in the optional refinement fitting with the more robust [minpack.lm::nlsLM]
  # * 2020-08-30, DM: Renamed 'error.calculation' into 'error.estimation'; changed [numeric] value unit from cts/ch to cts/s
  # * 2022-07-25, DM: Extended algorithm for bin-wise RSS calculation and added error estimation option "only.bin.RSS"
  #
  # ToDo:
  # * Enable the input of a list of curves
  # * Replace Cramers rule equations with 'solve()' to increase performance and to increase precision if a
  #   determinant converges towards zero, see https://en.wikipedia.org/wiki/Numerical_stability
  # * In some very rare cases, negative values for n.error are returned. How can that happen?
  # * Test and expand interval determination algorithm in case of very few (N ~ K) data points
  # * Enhance Auto-interval finder to work when 'background.fitting = TRUE'
  # * Check the background fitting algorithm carefully, especially its behavior in interaction with the possible function parameters.
  # * It should be sufficient if t.start OR ch.start is given

  ########## Input checks ###########

  if(!inherits(curve, c("RLum.Data.Curve", "data.frame", "matrix"))){
    stop("[decompose_OSLcurve()] Error: Input object 'curve' is not of type 'RLum.Data.Curve' or 'data.frame' or 'matrix'!")}

  if(inherits(curve, "RLum.Data.Curve")) curve <- as.data.frame(Luminescence::get_RLum(curve))

  if (!("time" %in% colnames(curve)) |
      !("signal" %in% colnames(curve))) {
    curve <- data.frame(time = curve[,1],
                        signal = curve[,2])}

  if (algorithm == "nls") {
    if (error.estimation != "empiric" && error.estimation != "none") {
      warning("Specific error.estimation argument without effect when algorithm = nls")
    }
    error.estimation <- "none"
  }



  # What is the channel width?
  dt <- curve$time[2] - curve$time[1]

  # check if time begins with zero and add dt if the case
  if (curve$time[1] == 0)  curve$time <- curve$time + dt

  # Check if 'components' is of valid type
  if (inherits(components, "numeric")) {
    components <- data.frame(name = paste0("Component ", 1:length(components)),
                             lambda = sort(components, decreasing = TRUE))

  }else if(inherits(components, "data.frame")){
    if (!("lambda" %in% colnames(components)) | !("name" %in% colnames(components))) {
      stop("[decompose_OSLcurve()] Error: Input object 'components' needs at least a column '$lambda' and a column '$name'!")}

  }else{
    stop("[decompose_OSLcurve()] Error: Input object 'components' is not of type 'numeric vector' or 'data.frame' !")}

  # if background.fitting = FALSE (recommended), remove last row
  # this removes also the last integration interval (which is good)
  if (is.na(components$lambda[nrow(components)]) & (background.fitting==FALSE)) {
    components <- components[1:(nrow(components)-1),]}

  # and now the other case
  if (!is.na(components$lambda[nrow(components)]) & (background.fitting==TRUE)) {
    stop(paste0("Background fitting is activated but no background integration interval is given.\n",
                "Background integration interval can be defined by running: \n",
                "<components> <- optimise_OSLintervals(<components>, <curve>, background.fitting = TRUE)"))}

  lambda <- components$lambda
  K <- nrow(components)
  X <- c(1:K)

  if (K > nrow(curve)) {
    stop("[decompose_OSLcurve()] Error: Number of decay rates in 'components' exceeds number of data points in 'curve'!")}


  # are the integration intervals given?
  if (!("t.start" %in% colnames(components)) ||
      !("t.end" %in% colnames(components)) ||
      !("ch.start" %in% colnames(components)) ||
      !("ch.end" %in% colnames(components))) {

    if (background.fitting) {
      stop("[decompose_OSLcurve()] Integration intervals are not provided! Auto-assignment works only if background.fitting = FALSE. Please change argument or run optimise_OSLintervals() prior data decomposition")}

    # Define the K = 1 case first:
    ch.start <- 1
    ch.end <- nrow(curve)

    if (K > 1) {

      # Calculate the logarithmic means between following lambdas
      intervals <- diff(log(lambda)) / diff(lambda)

      # Test if each interval starts before k/K
      intervals <- pmin(intervals, curve$time[nrow(curve)] * c(1:(K-1)) / K)

      # Round values up to full channels
      ch.end <- ceiling(intervals / dt)
      ch.start <- c(1, ch.end + 1)
      ch.end <- c(ch.end, nrow(curve))

      # Test if each interval is at least one channel wide
      for (i in 1:(K-1)) {
        if ((ch.end[i] - ch.start[i]) < 1) {
          ch.end[i] <- ch.start[i] + 1
          ch.start[i + 1] <- ch.end[i] + 1}}

      # In the very unlikely event that the last interval is shifted out of the measurement
      if (ch.start[K] >= ch.end[K]) {
        stop("[decompose_OSLcurve()] Error: Last interval is shifted out of the measurement.")}

    } # ToDo: all okay in case of just one component?

    t.start <- (ch.start - 1) * dt
    t.end <- ch.end * dt

    components$t.start <- t.start
    components$t.end <- t.end
    components$ch.start <- ch.start
    components$ch.end <- ch.end

  } else {

    t.start <- components$t.start
    t.end <- components$t.end
    ch.start <- components$ch.start
    ch.end <- components$ch.end}

  # preset some basic objects
  signal <- curve$signal[1:components$ch.end[K]]
  n <- NULL
  components$n <- rep(NA, K)

  # Set optional columns whose values rely on the decomposition outcome to NA
  # Reason: Depending on the chosen error estimation, they may not be overwritten otherwise.
  for (col_name in c("n.error", "bin.RSS", "bin.error", "initial.signal", "n.residual")) {
    if(col_name %in% colnames(components)) components[col_name] <- rep(NA, K)
  }


  ### calculate bin signal values ###
  I <- NULL
  for (i in X) I <- c(I, sum(signal[c(ch.start[i]:ch.end[i])]))
  components$bin <- I

  ######################### DET ###########################

  if ((algorithm == "det")||(algorithm == "det+nls")) {

    ### Calculate the signal intensities with Cramer's rule ###

    ## Build the denominator matrix

    # build an empty K x K matrix
    D <- matrix(0, K, K)

    # the component index k increases along the x-axis
    # the signal bin index i increases along the y-axis
    for (i in X) {
      for (k in X) {

        # is a decay parameter given for component k?
        if (!is.na(lambda[k])) {

          # YES:
          # calculated the decay probability during the interval of signal bin i
          # and assign it to the matrix element i, k
          D[i, k] <- exp(-t.start[i] * lambda[k]) - exp(- t.end[i] * lambda[k])

        } else {

          # NO:
          # assume that component k represents the signal background level
          D[i, k] <- t.end[i] - t.start[i]}}}

    ## Build the enumerator matrices

    # prepare a list object to store the matrices
    A <- list(NULL)

    for (k in X) {

      # each enumerator matrix is equal to the denominator matrix but with
      # the column k replaced by the vector I build of the signal bin values
      A.temp <- D
      A.temp[,k] <- I
      A[[k]] <- A.temp}

    for (k in X) {

      # the signal intensities are calculated by the ratios of the determinants
      n.temp <- det(A[[k]])/det(D)
      n <- c(n, n.temp)}

    # write the vector n containing the results
    # into the single record results data.frame 'components'
    components$n <- n

  }  # end DET

  ######################### NLS ###########################

  if ((algorithm == "nls")||(algorithm == "det+nls")) {

    # use outcome from DET as start parameters. If not given, use integral values
    if(is.null(n)) n <- I

    ### Create fit formula ###
    n.names <- paste0("n.",1:K)

    # is there a background component?
    if (is.na(components$lambda[K])) {

      lambda <- components$lambda[1:(K - 1)]
      decays <- paste(n.names[1:(K - 1)],
                      " * (exp(-",lambda," * (time - ", dt,")) - exp(-",lambda," * time))"
                      , collapse=" + ")
      decays <- paste0(decays, " + ", n.names[K], " * ", dt)

    } else {

      decays <- paste(n.names," * (exp(-",components$lambda," * (time - ", dt,")) - exp(-",components$lambda," * time))"
                      , collapse=" + ")}

    fit.formula <- stats::as.formula(paste0("signal ~ ", decays))

    names(n) <- n.names

    ### Apply Levenberg-Marquardt fitting algorithm  ###
    fit <- try(minpack.lm::nlsLM(fit.formula,
                                 data = curve,
                                 start = c(n)),
               silent = TRUE)

    if (attr(fit,"class") == "try-error") {

      if (algorithm == "nls") {

        warning("nls-fit failed. Input component table returned")
        return(components)
      } else {

        if (verbose) cat("Levenberg-Marquardt fitting failed. Returning equation system solution instead")
        algorithm <- "det-fallback"}

    } else {

      n <- stats::coef(fit)
      components$n <- n

      # add error estimations of fit as default and 'error.estimation=nls'-result
      components$n.error <- summary(fit)$parameters[, "Std. Error"][X]}

  } ########### end NLS ############



  ################### ERROR CALC ##################

  if ((error.estimation == "empiric")
      || (error.estimation == "poisson")
      || (error.estimation == "only.bin.RSS")
      || is.numeric(error.estimation)) {

    ### Calculate signal bin variances  ###
    I.err <- NULL
    bin.RSS <- NULL
    if ((error.estimation == "empiric")
        || (error.estimation == "only.bin.RSS")) {

      # Calc reconstructed noise-free curve
      #curve <- simulate_OSLcomponents(components, curve, simulate.curve = FALSE)
      ##########################

      # Use the signal vector as residual vector to save memory allocations

      # Speed up things here by calculating "exp(-lambda*time)" vector and applying it
      # on the component intensity n. This is an obscure but faster variant of the formula
      # component$A <- n * (exp(-lambda*(time - channel.width)) - exp(-lambda*time))

      for (k in X) {
        signal <- signal + n[k] * diff(exp(-lambda[k] * c(0, curve$time)))
      }
      # ToDo: Add special case lambda = NA

      # Calculate RSS per segment for the calc_RSS algorithm in fit_OSLcurve()
      for (i in X) {
        bin.RSS <- c(bin.RSS, sum(signal[ch.start[i]:ch.end[i]]^2))
      }
      components$bin.RSS <- bin.RSS

      # Calc corrected sample variance
      if (error.estimation == "empiric"){
        for (i in X) {


          if (ch.start[i] == ch.end[i]) {
            # if signal bin consists just of one channel, assume Poisson statistics:
            I.err <- sqrt(I[i])
          } else {

            # in all other cases: Use the corrected sample variance formula
            korrektor <- length(ch.start[i]:ch.end[i]) / (length(ch.start[i]:ch.end[i]) - 1)
            I.err <- c(I.err, sqrt(korrektor * bin.RSS[i]))
          }
        }
        components$bin.error <- I.err
      }
    } else {

      # Use poisson approach, add instrumental noise if defined
      if (!is.numeric(error.estimation)) {
        error.estimation <- 0
      }

      for (i in X) {

        I.err[i] <- sqrt(I[i] + (t.end[i] - t.start[i]) * error.estimation^2 )}}

    components$bin.error <- I.err

    ### Propagation of uncertainty ###
    if (error.estimation != "only.bin.RSS") {
      for (k in X) {
        sum.err <- 0

        for (i in X) {

          A.k <- A[[k]]

          # Differentiate the determinant term after I[j]
          A.k[i,] <- 0
          A.k[,k] <- 0
          A.k[i,k] <- 1
          sum.err <- sum.err + (det(A.k) * I.err[i])^2}

        components$n.error[k] <- sqrt(sum.err) / det(D)}
    }
    ############ end ERROR CALC ############
  } else {
    if (error.estimation != "none") {
      warning("Invalid error.estimation argument.")
    }
  }


  ########## component residuals  ###########
  if((error.estimation != "only.bin.RSS" && error.estimation != "none")||(algorithm == "nls")){

    # set the end of the record as the end of stimulation. Need not to be the same value as t.end
    stim.end <- curve$time[length(curve$time)]
    for (i in X) {

      components$n.residual[i] <- round(n[i] * exp(- stim.end * lambda[i]))}


    # Calculate average share of each component at initial signal
    first_signal <- X
    for (i in X) {
      if (is.na(lambda[i])) {

        first_signal[i] <- n[i] * dt
      } else {
        first_signal[i] <- n[i] * (1 - exp(- lambda[i] * dt))}}

    components$initial.signal <- round(first_signal / sum(first_signal), digits = 4)
  }

  # Printing the whole data.frame is too long, so we display just the important columns
  if (verbose) {
    col_set <- c("name", "n")
    for (col_name in c("n.error", "bin.RSS", "bin.error", "initial.signal", "n.residual")) {
      if(col_name %in% colnames(components)) col_set <- c(col_set, col_name)
    }
    col_set <- col_set[col_set %in% colnames(components)]
    print.data.frame(subset(components, select = col_set), row.names = FALSE)}

  invisible(components)
}

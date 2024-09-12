#' Multi-exponential CW-OSL curve fitting
#'
#' Fitting function for multi-exponentially decaying CW-OSL measurements,
#' based on the algorithm described by Bluszcz & Adamiec (2006).
#'
#' The function assumes multiple exponentially decaying signal components with first-order kinetics:
#'
#' \deqn{I(t) = n_1 \lambda_1 exp(-\lambda_1 t) + n_2 \lambda_2 exp(-\lambda_2 t) + ... + n_K \lambda_K exp(-\lambda_K t)}
#'
#' with \eqn{I(t)} the CW-OSL signal, \eqn{n} the signal component intensity,
#' \eqn{\lambda} the signal component decay constant and \eqn{K} the number of signal components.
#' For actual fitting, the integrated version of this formula is used, see Mittelstraß et al. (2021) for details.
#'
#' The fitting algorithm is an implementation of the *hybrid evolutionary-linear algorithm* (HELA)
#' by Bluszcz & Adamiec (2006). See there or Mittelstraß et al. (in preparation) for details.
#' The differential evolution part of HELA is performed by [DEoptim::DEoptim].
#' The linear regression part of HELA is performed by [decompose_OSLcurve].
#' The parameter refinement by Levenberg-Marquardt fitting is performed by [minpack.lm::nlsLM].
#'
#' **F-test**
#'
#' Bluszcz & Adamiec (2006) suggest the use of an F-test to determine the correct number of signal components.
#' This function compares the residual square sum (*RSS_K*) value of each
#' fitting with the *RSS_{K-1}* value of the previous fitting and calculates
#' an *Improvement-in-fitting-quality* criterion:
#'
#' \deqn{F_K = {(RSS_{K-1} - RSS_K)/2} / {RSS_K(N - 2K)}}
#'
#' Here, *N* is the number data points (channels) of the measurement and *K* is the number of OSL components
#' in the fitting model. If *F_K* falls below the threshold value (`F.threshold`), the fitting model
#' with *K* components is apparently not significantly better than the *K* - 1 model of the previous fitting cycle.
#' Thus, the *K* - 1 model will be recommended as fitting solution.
#'
#'
#' **Photoionisation cross sections**
#'
#' While the function is suited for the analysis of a wide variety of multi-exponential decay problems,
#' it is targeted to CW-OSL measurements of quartz under SAR protocol conditions (470 nm stimulation at 125 °C).
#' To compare the calculated OSL components with OSL components reported in published literature,
#' photoionisation cross sections are calculated using the `stimulation.wavelength` \eqn{\lambda_{stim}}  and
#' `stimulation.intensity` \eqn{\Phi_{stim}}:
#'
#' \deqn{\sigma_k=\lambda_k {hc / \Phi_{stim}\lambda_{stim}}}
#'
#' Here \eqn{\sigma_k} is the photoionisation cross section of component *k* in cm^2,
#' \eqn{\lambda_k} the CW-OSL decay constant in s^-1, *h* the Planck constant and *c* the speed of light.
#'
#' If a `stimulation.intensity` between 460 nm and 485 nm is defined,
#' the components are named automatically in accordance to the
#' cross-sections published by Durcan and Duller (2011), Jain et al. (2003) and Singarayer and Bailey (2003).
#' For the Ultrafast and the Slow4 component, no consistent literature values could be found, so their range
#' is tentatively assigned:
#'
#' \tabular{lll}{
#'  **Component** \tab **Lower limit (cm^2)** \tab **Upper limit (cm^2)**\cr
#'  Ultrafast \tab 1e-16 \tab 1e-15 \cr
#'  Fast \tab 1.9e-17 \tab 3.1e-17 \cr
#'  Medium \tab 3e-18 \tab 9e-18 \cr
#'  Slow1 \tab 1e-18 \tab 1.85e-18 \cr
#'  Slow2 \tab 1.1e-19 \tab 4e-19 \cr
#'  Slow3 \tab 1e-20 \tab 4.67e-20 \cr
#'  Slow4 \tab 1e-21 \tab 1e-20
#' }
#'
#' @param curve [RLum.Data.Curve-class] or [data.frame] or [matrix] (**required**):
#' CW-OSL record or average CW-OSL curve created by [sum_OSLcurves]. If no column `$time` exists, the first column is defined
#' as measurement time (x-axis). Time intervals must be constant. If no column `$signal` exists, the second column is defined
#' as signal values (y-axis). Further columns will be ignored
#'
#' @param K.max [numeric] (*with default*):
#' Maximum number of components *K*. The computing time increases exponentially with the component number.
#' *K* < 7 is recommended
#'
#' @param F.threshold [numeric] (*with default*):
#' Fitting stop criterion. If the F-value is lower than this threshold, the fitting procedure stops and the K - 1 fit is returned
#'
#' @param stimulation.intensity [numeric] (*with default*):
#' Intensity of optical stimulation in *mW / cm²*. Used to calculate photoionisation cross sections.
#'
#' @param stimulation.wavelength [numeric] (*with default*):
#' Wavelength of optical stimulation in *nm*. Used to calculate photoionisation cross sections.
#' If a wavelength between 465 and 480 nm is chosen, the cross sections are set into
#' relation with literature values to name the signal components automatically.
#'
#' @param verbose [logical] (*with default*):
#' Enables console text output.
#'
#' @param output.complex [logical] (*with default*):
#' If `TRUE`, the function returns a list of objects, see section **Value** for further information.
#' If `FALSE`, the function returns a [data.frame] with the CW-OSL model parameters of the fitting chosen by the F-test.
#' Setting the parameter to `FALSE` is not recommended when fitting a global average curve created by [sum_OSLcurves] as over-fitting is likely in such cases.
#'
#' @param parallel.computing [logical] (*with default*):
#' Enables the use of multiple CPU cores. This increases the execution speed significantly
#' but may need administrator rights and/or a firewall exception.
#' See [DEoptim::DEoptim.control] for further information.
#'
#' @section Last update:
#'
#' 2022-07-27, DM: Moved residual sum of squares (RSS) calculation during DE-optimization cycle to decompose_OSLcurve() to improve computing time by factor 3 to 4
#'
#' @author
#' Dirk Mittelstraß, \email{dirk.mittelstrass@@luminescence.de}
#'
#' Please cite the package the following way:
#'
#' Mittelstraß, D., Schmidt, C., Beyer, J., Heitmann, J. and Straessner, A.:
#' R package OSLdecomposition: Automated identification and separation of quartz CW-OSL signal components, *in preparation*.
#'
#' @seealso [RLum.OSL_decomposition], [sum_OSLcurves], [decompose_OSLcurve], [plot_OSLcurve],
#' [plot_PhotoCrosssections], [minpack.lm::nlsLM], [DEoptim::DEoptim]
#'
#' @references
#'
#' Bluszcz, A., Adamiec, G., 2006. Application of differential evolution to fitting OSL
#' decay curves. Radiation Measurements 41, 886–891.
#'
#' Durcan, J.A., Duller, G.A.T., 2011. The fast ratio: A rapid measure for testing the dominance of the fast component in the initial OSL signal from quartz. Radiation Measurements 46, 1065–1072.
#'
#' Jain, M., Murray, A.S., Bøtter-Jensen, L., 2003. Characterisation of blue-light stimulated luminescence components in different quartz samples: implications for dose measurement. Radiation Measurements 37, 441–449.
#'
#' Mittelstraß, D., 2019. Decomposition of weak optically stimulated luminescence signals and
#' its application in retrospective dosimetry at quartz (Master thesis). TU Dresden, Dresden.
#'
#' Singarayer, J.S., Bailey, R.M., 2003. Further investigations of the quartz optically stimulated luminescence components using linear modulation.
#' Radiation Measurements, Proceedings of the 10th international Conference on Luminescence and Electron-Spin Resonance Dating (LED 2002) 37, 451–458.
#'
#'
#' @return
#'
#' If `output.complex = FALSE`, a [data.frame] is returned. It contains the signal decay rates
#' and signal intensities of the best fit. The best fit was either chosen by the F-test or
#' the last successful fitting iteration.
#'
#' If `output.complex = TRUE`, a [list] of objects is returned:
#'
#' \tabular{lll}{
#'  **Element** \tab **Type** \tab **Description**\cr
#'  `decay.rates` \tab `numeric` \tab [vector] of the best suiting decay rates \cr
#'  `K.selected` \tab `numeric` \tab number of components of the best fit \cr
#'  `F.test` \tab `data.frame` \tab table containing the F-test parameter and the decay rates of each fitting model \cr
#'  `F.test.print` \tab `data.frame` \tab the same table as above, but formated for pretty console and report output \cr
#'  `info.text` \tab `character` \tab collected messages from the algorithms \cr
#'  `component.tables` \tab `list` \tab result [data.frame]s for all tested models \cr
#'  `curve` \tab `data.frame` \tab fitted time-signal-curve \cr
#'  `components` \tab `data.frame` \tab best fit; same object as `output.complex = FALSE` returns \cr
#'  `fit.results` \tab `list` \tab [list] of [nls] objects for all tested models \cr
#'  `plot.data` \tab `data.frame` \tab factorized results for overview plotting with [plot_PhotoCrosssections] \cr
#'  `parameters` \tab `list` \tab function arguments and the needed computing time
#' }
#'
#'
#' @examples
#'
#' # Create a simple curve with just one component
#' curve <- data.frame(
#'   X = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
#'   Y = c(377, 244, 163, 93, 59, 28, 17, 13, 10, 8, 9, 5))
#' # Perform fitting
#' components <- fit_OSLcurve(curve, F.threshold = 3)
#'
#' # Display results
#' plot_OSLcurve(curve, components)
#'
#' @md
#' @export
fit_OSLcurve <- function(
  curve,
  K.max = 5,
  F.threshold = 150,
  stimulation.intensity = 30,
  stimulation.wavelength = 470,
  verbose = TRUE,
  output.complex = FALSE,
  parallel.computing = FALSE
){

  # Changelog:
  # * 2019-02-14, DM: First version
  # * 2019-03-15, DM: Separated 'decompose_OSLalternatively()'
  # * 2019-04-29, DM: Added Blucszs & Adamiec-like approach using numOSL::decomp (Peng et al. 2014)
  # * 2019-05-14, DM: Added "fit_OSLLifeTimes" approach from Luminescence package 0.9; Corrected and improved numOSL approach; Deleted nls.default approach
  # * 2019-06-28, DM: Deleted "fit_OSLLifeTimes" approach. Added stretched exponentials for testing. Added overview plot
  # * 2019-10-07, DM: Streamlined function; added optional background fitting
  # * 2019-10-08, DM: Separated plotting to plot_PhotoCrosssections()
  # * 2020-04-04, DM: Extended output list (curve & arguments)
  # * 2020-04-06, DM: Extended print output and made some  tweaks. Replaced 'SAR.compatible' with 'fully.bleached'
  # * 2020-05-05, DM: Replaced boolean 'fully.bleached' with numeric 'bleaching.grade'
  # * 2020-08-05, DM: Added DEoptim + nlsLM algorithm
  # * 2020-08-10, DM: Optional parallel computing enabled
  # * 2020-10-26, DM: Roxygen documentation
  # * 2020-11-25, DM: Reworked console output
  # * 2022-07-27, DM: Moved residual sum of squares (RSS) calculation during DE-optimization cycle to decompose_OSLcurve() to improve computing time by factor 3 to 4
  #
  # ToDo:
  # * Enhance documentation with more algorithm info and some F.threshold recommendation
  # * Reactivate optional background level fitting
  # * Introduce 'fit_OSLcurve.control' which forwards algorithm parameters to DEoptim.control and nls.lm.control
  # * Enable optional weighted fitting and give out reduced Chi²

  # Internal parameter (for later use in fit_OSLcurve.control)
  silent <- TRUE # don't display warnings or not-fatal errors
  LM <- TRUE

  ################### Prepare input data ###########################

  if(!inherits(curve, c("RLum.Data.Curve", "data.frame", "matrix"))){
    stop("[fit_OSLcurve()] Error: Input object 'curve' is not of type 'RLum.Data.Curve' or 'data.frame' or 'matrix'!")}

  if(inherits(curve, "RLum.Data.Curve")) curve <- as.data.frame(Luminescence::get_RLum(curve))

  if (!("time" %in% colnames(curve)) |
      !("signal" %in% colnames(curve))) {
    curve <- data.frame(time = curve[,1],
                        signal = curve[,2])}

  channel_width <- curve$time[2] - curve$time[1]

  # check if curve$time beginns with zero and add channel_width if the case
  if (curve$time[1] == 0)  {
    if (verbose) cat("Time axis begins with t_1 = 0, which is not allowed. All time values increased by", channel_width,"\n")
    curve$time <- curve$time + channel_width}

  # check if any signal = 0 or smaller and set them to 0.1
  if (any(curve$signal <= 0)) {
    if (verbose) cat("One or more signal values are equal or smaller than zero: Replaced with 0.1\n")
    curve$signal[curve$signal <= 0] <- 0.1}

  # Some presets ...
  K_selected <- 0
  X <- c(1:K.max)
  RSS <- NA
  RSS_old <- Inf
  fittings <- list(NULL)
  component_tables <- list(NULL)
  F_table <- data.frame(NULL)
  F_table_print <- data.frame(NULL)
  plot_data <- data.frame(NULL)
  lambda <- c(NULL)
  info_text <- ""

  # prepare printed table
  if (verbose) cat("\nDecay rates (s^-1):\n")
  if (verbose) cat("Cycle \t", paste(paste0("   Comp. ", X), collapse = "  "),
                   "        RSS     F-value\n")

  ###################### Subfunction for DE minimisation ###########################################
  calc_RSS <- function(lambda_vector, RSScurve = curve){

    # The linear part of HELA (see Bluszcz & Adamiec 2006) is performed by decompose_OSLcurve()
    RSScomponents <- decompose_OSLcurve(RSScurve,
                                        lambda_vector,
                                        algorithm = "det",
                                        error.estimation = "only.bin.RSS",
                                        verbose = FALSE)

    # ... and calculate the residual sum of squares (RSS)
    RSS <- sum(RSScomponents$bin.RSS)
    if (is.na(RSS) || (RSS <= 0)) RSS <- Inf
    return(RSS)
  }

  ###################### Reduced Chi² ###############################################################
  # calc_Chi2 <- function(components, CHIcurve = curve, N_curves = M, detec_noise = 0, K = K){

  #   CHIcurve <- simulate_OSLcomponents(components,
  #                                      curve = CHIcurve,
  #                                      simulate.curve = FALSE)

  #   RS <- CHIcurve$residual^2 * N_curves / (abs(curve$sum) + detec_noise)
  #   Chi2 <- sum(RS) / (length(RS) - K * 2)
  #   return(Chi2)}

  ###################### Photo-Ionisation Crosssections #############################################
  build_component_table <- function(lambda_vector, lambda_err, BCTcurve = curve){

    Y <- 1:length(lambda_vector)

    # Calc photon energy: E = h*v  [W*s^2 * s^-1 = W*s = J]
    E <-6.62606957e-34 * 299792458 * 10^9 / stimulation.wavelength

    # Calc photon flux of stimulation light: Flux = I / E  [W/cm^2 / W*s = 1/s*cm^2]
    Flux <- stimulation.intensity / (E * 1000)

    # Calc cross-sections: Sigma = lambda / Flux  [s^-1 / 1/s*cm^2 = cm^2]
    cross.section <- lambda_vector / Flux
    cross.relative <- round(cross.section / cross.section[1], digits=4)

    ### NAME COMPONENTS ###
    # default names:
    name <- paste0("Component ", Y)

    if ((stimulation.wavelength >= 460) && (stimulation.wavelength <= 485)  ) {

      # Rename components according to literature
      for (i in Y) {

        c <- cross.section[i]

        # Autonaming uses Table 1 in Durcan & Duller (2011)
        # Minimum = lowest value - 2-sigma
        # Maximum = highest value + 2-sigma
        # Exception are Ultrafast and Slow4 which are not well defined in literature and guessed freely
        if ((c > 1e-16) && (c < 1e-15)) name[i] <- "Ultrafast"
        if ((c > 1.9e-17) && (c < 3.1e-17)) name[i] <- "Fast"
        if ((c > 3e-18) && (c < 9e-18)) name[i] <- "Medium"
        if ((c > 1e-18) && (c < 1.85e-18)) name[i] <- "Slow1"
        if ((c > 1.1e-19) && (c < 4e-19)) name[i] <- "Slow2"
        if ((c > 1e-20) && (c < 4.67e-20)) name[i] <- "Slow3"
        if ((c > 1e-21) && (c < 1e-20)) name[i] <- "Slow4"}}


    # Check for double-naming
    name[duplicated(name)] <- paste0(name[duplicated(name)], ".a")

    # Check again
    name[duplicated(name)] <- paste0(substr(name[duplicated(name)], 1, nchar(name[duplicated(name)]) - 2), ".b")

    # And again
    name[duplicated(name)] <- paste0(substr(name[duplicated(name)], 1, nchar(name[duplicated(name)]) - 2), ".c")

    # How much is the component bleached during stimulation?
    bleaching.grade <- round(1 - exp(- lambda_vector * max(BCTcurve$time)), digits = 4)

    # Decay with zero or negative error had no correct error estimation
    lambda_err[lambda_err <= 0] <- NA

    ##### Build result table #####
    components <- data.frame(name = name,
                             lambda = lambda_vector,
                             lambda.error = lambda_err,
                             cross.section = cross.section,
                             cross.relative = cross.relative,
                             bleaching.grade = bleaching.grade)

    row.names(components) <- Y

    # Go the easy way to extract additional information from the fitting
    components <- decompose_OSLcurve(curve = BCTcurve,
                                     components = components,
                                     verbose = FALSE)

  } ### END building tables for the various cases ###



  #----------------------------------------------------------------------------------------------------#
  #------------------------------------- K = K + 1 cycle ----------------------------------------------#
  #----------------------------------------------------------------------------------------------------#
  computing_time <- Sys.time()
  for (K in X) {

    lambda_error <- rep(0, K)
    fit <- list()

    ########################################### DEoptim ##############################################

    # Divide the DE parameter space a the decay values of the previous cycle
    # Additional constraints:
    # - no negative values (decay >= 0)
    # - no superfast decays, that the channel frequency couldn't resolve it (decay <= 2 / channel_width)
    lower_lambda <- c(lambda, 0)
    upper_lambda <- c(2 / channel_width, lambda)

    # Perform differential evolution (DE). As minimisation function, use calc_RSS()
    DE_min <- try(DEoptim::DEoptim(
      fn = calc_RSS,
      lower = lower_lambda,
      upper = upper_lambda,
      control = DEoptim::DEoptim.control(
        NP = K * 15,
        strategy = 2,
        itermax = 80 + K * 10,
        c = 0.2,
        reltol = 1e-4,
        steptol = 10,
        parallelType = parallel.computing,
        packages = c("OSLdecomposition"),
        parVar = c("curve"),
        trace = FALSE)),
      silent = silent)

    # Did the DE algorithm break?
    if (methods::is(DE_min)[1] == "try-error"){

      if (verbose) cat("-> Differential evolution failed at K =", K, ". Algorithm stopped.\n")

      # Discard the current component number
      K <- K - 1

      # leave loop
      break}

    # Otherwise, extract results
    fit[["DE"]] <- DE_min
    lambda <- DE_min$optim$bestmem
    RSS <- DE_min$optim$bestval

    ########################################### nlsLM ##############################################

    if (LM) {

      # We need the signal intensities of the components as start values for the LM fitting
      DE_components <- decompose_OSLcurve(curve,
                                          lambda,
                                          algorithm = "det",
                                          error.estimation = "none",
                                          verbose = FALSE)
      n <- DE_components$n

      ### Create fit formula ###
      n.names <- paste0("n.",1:K)
      lambda.names <- paste0("lambda.",1:K)

      # now create the optimization formula
      fit.formula <- stats::formula(paste0("signal ~ ",
                                       paste(n.names," * (exp(-", lambda.names," * (time - ", channel_width,")) - exp(-", lambda.names," * time))",
                                             collapse=" + ")))

      # Name the vectors to allow the correct value allocation
      names(n) <- n.names
      names(lambda) <- lambda.names

      ### Apply LM algorithm  ###
      LM_fit <- try(minpack.lm::nlsLM(fit.formula,
                                      data = curve,
                                      start = c(n, lambda),
                                      control = minpack.lm::nls.lm.control(
                                        maxiter = 50 + K * 20)),
                    silent = silent)

      if (methods::is(LM_fit)[1] == "try-error") {

        #if (verbose) cat(LM_fit[1])
        if (verbose) cat("Levenberg-Marquardt fitting failed at K =", K, ". Differential evolution result:\n")

      } else {

        fit[["LM"]] <- LM_fit
        lambda <- summary(LM_fit)$parameters[paste0("lambda.", 1:K),"Estimate"]
        RSS <- LM_fit$m$deviance()
        lambda_error <- summary(LM_fit)$parameters[paste0("lambda.", 1:K),"Std. Error"]}}



    # save the raw fitting objects
    fittings[[K]] <- fit

    # identify the components and build the Component table
    component_table <- try(build_component_table(lambda, lambda_error),
                                 silent = silent)

    if (methods::is(component_table)[1] == "try-error") {

      if (verbose) cat(component_table[1])
      break

    } else {
      component_tables[[K]] <- component_table}




    # Add values to [plot_Photocrosssections()] ploting table
    plot_data <- rbind(plot_data,
                       data.frame(lambda = lambda,
                                  lambda.low = lambda - lambda_error,
                                  lambda.up = lambda + lambda_error,
                                  name = factor(paste0("Best fit with K = ", K)),
                                  x = K))
    #x <- x + 1

    ### F-test ###
    F_value <- 0.5*(RSS_old - RSS) / (RSS / (length(curve$signal) - 2 * K))
    RSS_old <- RSS

    # Create live console output
    table_row <- c(rep("          ", K.max),
                   formatC(RSS, digits = 4, width = 10),
                   formatC(F_value, digits = 4, width = 10))
    table_row[1:length(lambda)] <- formatC(lambda, digits = 4, width = 10)
    if (verbose) cat("K =", K, "\t", paste(table_row, collapse = "  "), "\n")

    # Build output F-test table for [report_Step2.rmd] script
    F_table_print <- rbind(F_table_print, table_row, stringsAsFactors = FALSE)
    F_table <- rbind(F_table, (c(lambda[X], RSS, F_value)))

    # Stop fitting if K - 1 was the correct model
    if (F_value <= F.threshold) {
      info_line <- paste0("Left loop because F-test value (F = ", formatC(F_value),
                          ") fell below threshold value (F = ", F.threshold,")\n")
      info_text <- paste0(info_text, info_line)
      if (verbose) cat(info_line)
      break}

    # If the current iteration cycle succeed until this point, it must be the best fit so far. Therefore:
    K_selected <- K

    if (K == K.max) {
      info_line <- paste0("Left loop because maximum number of allowed components K is reached\n")
      info_text <- paste0(info_text, info_line)
      if (verbose) cat(info_line)}

  } #---------------------------------------- End cycle -----------------------------------------------

  if ((K_selected == 0) | (nrow(F_table_print) == 0)) stop("[fit_OSLcurve] No sucessful fit")

  # How long did the algorithm need?
  computing_time <- as.numeric(difftime(Sys.time(), computing_time, units = "s"))


  # Give F tables approbiate headers
  colnames(F_table) <- c(paste0("f_", X),"RSS","F_value")
  colnames(F_table_print) <- c(paste0("f_", X),"RSS","F_value")

  # Delete unused columns
  if (nrow(F_table) < K.max) {
  #  F_table <- F_table[,-c((nrow(F_table) + 1):K.max)]
    F_table_print <- F_table_print[,-c((nrow(F_table) + 1):K.max)]}

  # Standard set of components is the on chosen by the F-test
  components <- component_tables[[K_selected]]

  ######### Further console output ######
  if (verbose) {

    cat(paste0("-> The F-test suggests the K = ", K_selected," model"), "\n")


    # Build a nove looking photoionisation cross section table for the console output
    cat("\nPhotoionisation cross sections (cm^2):\n")
    cat("Cycle \t", sprintf(" %-19s", paste0("Comp. ", X)), "\n")

    most_identified <- NULL
    for (k in 1:K) {

      cross_section <- prettyNum(component_tables[[k]]$cross.section, digits = 3)
      name <- component_tables[[k]]$name
      identified <- !grepl("ompo", name, fixed = TRUE)

      cross_section[identified] <- paste0(cross_section[identified], " (", name[identified], ")")
      cat("K =", k, "\t", sprintf("%-20s", cross_section), "\n")

      most_identified <- c(most_identified, sum(identified) - k/100)}

    if (sum(most_identified) > 0) {
      cat(paste0("-> Most known quartz OSL components found in the K = ",
                 which(max(most_identified) == most_identified)," model"), "\n")}


  }

  ######################### Return values #######################

  if (output.complex) {

    algorithm <- "DE"
    if (LM) algorithm <- "DE+LM"

    output <- list(decay.rates = components$lambda,
                   K.selected = K_selected,
                   F.test = F_table,
                   F.test.print = F_table_print,
                   info.text = info_text,
                   component.tables = component_tables,
                   curve = curve,
                   components = components,
                   fit.results = fittings,
                   plot.data = plot_data,
                   parameters = list(K.max = K.max,
                                     F.threshold = F.threshold,
                                     stimulation.intensity = stimulation.intensity,
                                     stimulation.wavelength = stimulation.wavelength,
                                     algorithm = algorithm,
                                     computing.time.s = computing_time))
    invisible(output)

  } else {
    invisible(components)}
}

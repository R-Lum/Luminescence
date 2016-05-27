#' Analyse IRSAR RF measurements
#'
#' Function to analyse IRSAR RF measurements on K-feldspar samples, performed
#' using the protocol according to Erfurt et al. (2003) and beyond.
#'
#' The function performs an IRSAR analysis described for K-feldspar samples by
#' Erfurt et al. (2003) assuming a negligible sensitivity change of the RF
#' signal.\cr
#'
#' \bold{General Sequence Structure} (according to Erfurt et al.
#' (2003)) \enumerate{
#'
#' \item Measuring IR-RF intensity of the natural dose for a few seconds (\eqn{RF_{nat}})
#' \item Bleach the samples under solar conditions for at least 30 min without changing the geometry
#' \item Waiting for at least one hour
#' \item Regeneration of the IR-RF signal to at least the natural level (measuring (\eqn{RF_{reg}})
#' \item Fitting data with a stretched exponential function
#' \item Calculate the the palaeodose \eqn{D_{e}} using the parameters from the
#' fitting}
#'
#' Actually two methods are supported to obtain the \eqn{D_{e}}: \code{method = "FIT"} and
#' \code{method = "SLIDE"}:
#'
#' \bold{\code{method = "FIT"}}\cr
#'
#' The principle is described above and follows the original suggestions by
#' Erfurt et al., 2003. For the fitting the mean count value of the RF_nat curve is used.
#'
#' Function used for the fitting (according to Erfurt et al. (2003)): \cr
#'
#' \deqn{\phi(D) = \phi_{0}-\Delta\phi(1-exp(-\lambda*D))^\beta}
#' with \eqn{\phi(D)} the dose dependent IR-RF flux, \eqn{\phi_{0}} the inital
#' IR-RF flux, \eqn{\Delta\phi} the dose dependent change of the IR-RF flux,
#' \eqn{\lambda} the exponential parameter, \eqn{D} the dose and \eqn{\beta}
#' the dispersive factor.\cr\cr To obtain the palaeodose \eqn{D_{e}} the
#' function is changed to:\cr \deqn{D_{e} = ln(-(\phi(D) -
#' \phi_{0})/(-\lambda*\phi)^{1/\beta}+1)/-\lambda}\cr The fitting is done
#' using the \code{port} algorithm of the \code{\link{nls}} function.\cr
#'
#'
#' \bold{\code{method = "SLIDE"}}\cr
#'
#' For this method the natural curve is slided along the x-axis until
#' congruence with the regenerated curve is reached. Instead of fitting this
#' allows to work with the original data without the need of any physical
#' model. This approach was introduced for RF curves by Buylaert et al., 2012
#' and Lapp et al., 2012.
#'
#' Here the sliding is done by searching for the minimum of the squared residuals.\cr
#'
#' \bold{\code{method.control}}\cr
#'
#' To keep the generic argument list as clear as possible, arguments to control the methods
#' for De estimation are all preset with meaningful default parameters and can be
#' handled using the argument \code{method.control} only, e.g.,
#' \code{method.control = list(trace = TRUE)}. Supported arguments are:\cr
#'
#' \tabular{lll}{
#' ARGUMENT       \tab METHOD               \tab DESCRIPTION\cr
#' \code{trace}   \tab \code{FIT}, \code{SLIDE} \tab as in \code{\link{nls}}; shows sum of squared residuals\cr
#' \code{maxiter} \tab \code{FIT}            \tab as in \code{\link{nls}}\cr
#' \code{warnOnly} \tab \code{FIT}           \tab as in \code{\link{nls}}\cr
#' \code{minFactor} \tab \code{FIT}            \tab as in \code{\link{nls}}\cr
#' \code{correct_onset} \tab \code{SLIDE}      \tab The logical argument literally spoken,
#' shifts the curves along the x-axis by the first channel, as light is expected in the first channel.
#'  The default value is \code{TRUE}.\cr
#' \code{show_density} \tab \code{SLIDE}       \tab \code{\link{logical}} (with default)
#' enables or disables KDE plots for MC run results. If the distribution is too narrow nothing is shown.\cr
#' \code{show_fit} \tab \code{SLIDE}       \tab \code{\link{logical}} (with default)
#' enables or disables the plot of the fitted curve rountinly obtained during the evaluation.\cr
#'\code{n.MC}                  \tab \code{SLIDE}       \tab    \code{\link{integer}} (wiht default):
#' This controls the number of MC runs within the sliding (assesing the possible minimum values).
#' The default \code{n.MC = 1000}. Note: This parameter is not the same as controlled by the
#' function argument \code{n.MC} \cr
#' }
#'
#'
#' \bold{Error estimation}\cr
#'
#' For \bold{\code{method = "FIT"}} the asymmetric error range is obtained by using the 2.5 \% (lower) and
#' the 97.5 \% (upper) quantiles of the \eqn{RF_{nat}} curve for calculating the \eqn{D_{e}} error range.\cr
#'
#' For \bold{\code{method = "SLIDE"}} the error is obtained by bootstrapping the residuals of the slided
#' curve to construct new natural curves for a Monte Carlo simulation. The error is returned in two
#' ways: (a) the standard deviation of the herewith obtained \eqn{D_{e}} from the MC runs and (b) the confidence
#' interval using the  2.5 \% (lower) and the 97.5 \% (upper) quantiles. The results of the MC runs
#' are returned with the function output. \cr
#'
#' \bold{Test parameters}\cr
#'
#' The argument \code{test_parameters} allows to pass some thresholds for several test parameters,
#' which will be evaluated during the function run. If a threshold is set and it will be exceeded the
#' test parameter status will be set to "FAILED". Intentionally this parameter is not termed
#' 'rejection criteria' as not all test parameters are evaluated for both methods and some parameters
#' are calculated by not evaluated by default. Common for all parameters are the allowed argument options
#' \code{NA} and \code{NULL}. If the parameter is set to \code{NA} the value is calculated but the
#' result will not be evaluated, means it has no effect on the status ("OK" or "FAILED") of the parameter.
#' Setting the parameter to \code{NULL} disables the parameter entirely and the parameter will be
#' also removed from the function output. This might be useful in cases where a particular parameter
#' asks for long computation times. Currently supported parameters are:
#'
#' \code{curves_ratio} \code{\link{numeric}} (default: \code{1.001}):\cr
#'
#' The ratio of \eqn{RF_{nat}} over \eqn{RF_{reg}} in the range of\eqn{RF_{nat}} of is calculated
#' and should not exceed the threshold value. \cr
#'
#' \code{intersection_ratio} \code{\link{numeric}} (default: \code{NA}):\cr
#'
#' Calculated as absolute difference from 1 of the ratio of the integral of the normalised RF-curves,
#' This value indicates intersection of the RF-curves and should be close to 0 if the curves
#' have a similar shape. For this calculation first the corresponding time-count pair value on the RF_reg
#' curve is obtained using the maximum count value of the RF_nat curve and only this segment (fitting to
#' the RF_nat curve) on the RF_reg curve is taken for further calculating this ratio. If nothing is
#' found at all, \code{Inf} is returned. \cr
#'
#' \code{residuals_slope} \code{\link{numeric}} (default: \code{NA}; only for \code{method = "SLIDE"}): \cr
#'
#' A linear function is fitted on the residuals after sliding.
#' The corresponding slope can be used to discard values as a high (positive, negative) slope
#' may indicate that both curves are fundamentally different and the method cannot be applied at all.
#' Per default the value of this parameter is calculated but not evaluated. \cr
#'
#'\code{curves_bounds} \code{\link{numeric}} (default: \eqn{max(RF_{reg_counts})}:\cr
#'
#'This measure uses the maximum time (x) value of the regenerated curve.
#'The maximum time (x) value of the natural curve cannot be larger than this value. However, although
#'this is not recommended the value can be changed or disabled.\cr
#'
#'\code{dynamic_ratio} \code{\link{numeric}} (default: \code{NA}):\cr
#'
#'The dynamic ratio of the regenerated curve is calculated as ratio of the minimum and maximum count values.
#'
#'\code{lambda}, \code{beta} and \code{delta.phi}
#'\code{\link{numeric}} (default: \code{NA}; \code{method = "SLIDE"}): \cr
#'
#'The stretched exponential function suggested by Erfurt et al. (2003) describing the decay of
#'the RF signal, comprises several parameters that might be useful to evaluate the shape of the curves.
#'For \code{method = "FIT"} this parameter is obtained during the fitting, for \code{method = "SLIDE"} a
#'rather rough estimation is made using the function \code{\link[minpack.lm]{nlsLM}} and the equation
#'given above. Note: As this procedure requests more computation time, setting of one of these three parameters
#'to \code{NULL} also prevents a calculation of the remaining two.
#'
#'
#' @param object \code{\linkS4class{RLum.Analysis}} or a \code{\link{list}} of \code{RLum.Analysis} objects (\bold{required}): input
#' object containing data for protocol analysis. The function expects to find at least two curves in the
#' \code{\linkS4class{RLum.Analysis}} object: (1) RF_nat, (2) RF_reg. If a \code{list} is provided as
#' input all other parameters can be provided as \code{list} as well to gain full control.
#'
#' @param sequence_structure \code{\link{vector}} \link{character} (with
#' default): specifies the general sequence structure. Allowed steps are
#' \code{NATURAL}, \code{REGENERATED}. In addition any other character is
#' allowed in the sequence structure; such curves will be ignored during the analysis.
#'
#' @param RF_nat.lim \code{\link{vector}} (with default): set minimum and maximum
#' channel range for natural signal fitting and sliding. If only one value is provided this
#' will be treated as minimum value and the maximum limit will be added automatically.
#'
#' @param RF_reg.lim \code{\link{vector}} (with default): set minimum and maximum
#' channel range for regenerated signal fitting and sliding. If only one value is provided this
#' will be treated as minimum value and the maximum limit will be added automatically.
#'
#' @param method \code{\link{character}} (with default): setting method applied
#' for the data analysis. Possible options are \code{"FIT"} or \code{"SLIDE"}.
#'
#' @param method.control \code{\link{list}} (optional): parameters to control the method, that can
#' be passed to the choosen method. These are for (1) \code{method = "FIT"}: 'trace', 'maxiter', 'warnOnly',
#' 'minFactor' and for (2) \code{method = "SLIDE"}: 'correct_onset', 'show_density',  'show_fit', 'trace'.
#' See details.
#'
#' @param test_parameters \code{\link{list} (with default)}: set test parameters.
#' Supported parameters are: \code{curves_ratio}, \code{residuals_slope} (only for
#' \code{method = "SLIDE"}), \code{curves_bounds}, \code{dynamic_ratio},
#' \code{lambda}, \code{beta} and \code{delta.phi}. All input: \code{\link{numeric}}
#' values, \code{NA} and \code{NULL} (s. Details)
#'
#' (see Details for further information)
#'
#' @param n.MC \code{\link{numeric}} (with default): set number of Monte
#' Carlo runs for start parameter estimation (\code{method = "FIT"}) or
#' error estimation (\code{method = "SLIDE"}). Note: Large values will
#' significantly increase the computation time
#'
#' @param txtProgressBar \code{\link{logical}} (with default): enables \code{TRUE} or
#' disables \code{FALSE} the progression bar during MC runs
#'
#' @param plot \code{\link{logical}} (with default): plot output (\code{TRUE}
#' or \code{FALSE})
#'
#' @param plot_reduced \code{\link{logical}} (optional): provides a reduced plot output if enabled
#' to allow common R plot combinations, e.g., \code{par(mfrow(...))}. If \code{TRUE} no residual plot
#' is returned; it has no effect if \code{plot = FALSE}
#'
#' @param \dots further arguments that will be passed to the plot output.
#' Currently supported arguments are \code{main}, \code{xlab}, \code{ylab},
#' \code{xlim}, \code{ylim}, \code{log}, \code{legend} (\code{TRUE/FALSE}),
#' \code{legend.pos}, \code{legend.text} (passes argument to x,y in
#' \code{\link[graphics]{legend}}), \code{xaxt}
#'
#'
#' @return A plot (optional) and an \code{\linkS4class{RLum.Results}} object is
#' returned:\cr
#'
#' \bold{@data}\cr
#' $ De.values: \code{\link{data.frame}} table with De and corresponding values\cr
#' ..$ DE : \code{numeric}: the obtained equivalent dose\cr
#' ..$ DE.ERROR : \code{numeric}: (only method = "SLIDE") standard deviation obtained from MC runs \cr
#' ..$ DE.LOWER : \code{numeric}: 2.5\% quantile for De values obtained by MC runs \cr
#' ..$ DE.UPPER : \code{numeric}: 97.5\% quantile for De values obtained by MC runs  \cr
#' ..$ DE.STATUS  : \code{character}: test parameter status\cr
#' ..$ RF_NAT.LIM  : \code{charcter}: used RF_nat curve limits \cr
#' ..$ RF_REG.LIM : \code{character}: used RF_reg curve limits\cr
#' ..$ POSITION : \code{integer}: (optional) position of the curves\cr
#' ..$ DATE : \code{character}: (optional) measurement date\cr
#' ..$ SEQUENCE_NAME : \code{character}: (optional) sequence name\cr
#' ..$ UID : \code{character}: unique data set ID \cr
#' $ test_parameters : \code{\link{data.frame}} table test parameters \cr
#' $ fit : {\code{\link{nls}} \code{nlsModel} object} \cr
#' $ slide : \code{\link{list}} data from the sliding process, including the sliding matrix\cr
#'
#' \bold{@info}\cr
#' $ call : \code{\link[methods]{language-class}}: the orignal function call \cr
#'
#' The output (\code{De.values}) should be accessed using the
#' function \code{\link{get_RLum}}
#'
#' @note \bold{[THIS FUNCTION HAS BETA-STATUS]}\cr
#'
#' This function assumes that there is no sensitivity change during the
#' measurements (natural vs. regenerated signal), which is in contrast to the
#' findings from Buylaert et al. (2012). Furthermore: In course of ongoing research this function has
#' been almost fully re-written, but further thoughtful tests are still pending!
#' However, as a lot new package functionality was introduced with the changes made
#' for this function and to allow a part of such tests the re-newed code was made part
#' of the current package.\cr
#'
#' @section Function version: 0.6.9
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
#'
#' @seealso \code{\linkS4class{RLum.Analysis}},
#' \code{\linkS4class{RLum.Results}}, \code{\link{get_RLum}},
#' \code{\link{nls}}, \code{\link[minpack.lm]{nlsLM}}
#'
#'
#' @references Buylaert, J.P., Jain, M., Murray, A.S., Thomsen, K.J., Lapp, T.,
#' 2012. IR-RF dating of sand-sized K-feldspar extracts: A test of accuracy.
#' Radiation Measurements 44 (5-6), 560-565. doi: 10.1016/j.radmeas.2012.06.021
#'
#' Erfurt, G., Krbetschek, M.R., 2003. IRSAR - A single-aliquot
#' regenerative-dose dating protocol applied to the infrared radiofluorescence
#' (IR-RF) of coarse- grain K-feldspar. Ancient TL 21, 35-42.
#'
#' Erfurt, G., 2003. Infrared luminescence of Pb+ centres in potassium-rich
#' feldspars. physica status solidi (a) 200, 429-438.
#'
#' Erfurt, G., Krbetschek, M.R., 2003. Studies on the physics of the infrared
#' radioluminescence of potassium feldspar and on the methodology of its
#' application to sediment dating. Radiation Measurements 37, 505-510.
#'
#' Erfurt, G., Krbetschek, M.R., Bortolot, V.J., Preusser, F., 2003. A fully
#' automated multi-spectral radioluminescence reading system for geochronometry
#' and dosimetry. Nuclear Instruments and Methods in Physics Research Section
#' B: Beam Interactions with Materials and Atoms 207, 487-499.
#'
#' Lapp, T., Jain, M., Thomsen, K.J., Murray, A.S., Buylaert, J.P., 2012. New
#' luminescence measurement facilities in retrospective dosimetry. Radiation
#' Measurements 47, 803-808. doi:10.1016/j.radmeas.2012.02.006
#'
#' Trautmann, T., 2000. A study of radioluminescence kinetics of natural
#' feldspar dosimeters: experiments and simulations. Journal of Physics D:
#' Applied Physics 33, 2304-2310.
#'
#' Trautmann, T., Krbetschek, M.R., Dietrich, A., Stolz, W., 1998.
#' Investigations of feldspar radioluminescence: potential for a new dating
#' technique. Radiation Measurements 29, 421-425.
#'
#' Trautmann, T., Krbetschek, M.R., Dietrich, A., Stolz, W., 1999. Feldspar
#' radioluminescence: a new dating method and its physical background. Journal
#' of Luminescence 85, 45-58.
#'
#' Trautmann, T., Krbetschek, M.R., Stolz, W., 2000. A systematic study of the
#' radioluminescence properties of single feldspar grains. Radiation
#' Measurements 32, 685-690.
#'
#'
#' @keywords datagen
#'
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.RLum.Analysis, envir = environment())
#'
#' ##(1) perform analysis using the method 'FIT'
#' results <- analyse_IRSAR.RF(object = IRSAR.RF.Data)
#'
#' ##show De results and test paramter results
#' get_RLum(results, data.object = "De.values")
#' get_RLum(results, data.object = "test_parameters")
#'
#' ##(2) perform analysis using the method 'SLIDE'
#' results <- analyse_IRSAR.RF(object = IRSAR.RF.Data, method = "SLIDE", n.MC = 1)
#'
#' \dontrun{
#' ##(3) perform analysis using the method 'SLIDE' and method control option
#' ## 'trace
#' results <- analyse_IRSAR.RF(
#'  object = IRSAR.RF.Data,
#'  method = "SLIDE",
#'  method.control = list(trace = TRUE))
#'
#' }
#'
#' @export
analyse_IRSAR.RF<- function(
  object,
  sequence_structure = c("NATURAL", "REGENERATED"),
  RF_nat.lim = NULL,
  RF_reg.lim = NULL,
  method = "FIT",
  method.control = NULL,
  test_parameters = NULL,
  n.MC = 10,
  txtProgressBar = TRUE,
  plot = TRUE,
  plot_reduced = FALSE,
  ...
){

  ##TODO
  ## - if a file path is given, the function should try to find out whether an XSYG-file or
  ##   a BIN-file is provided

  # SELF CALL -----------------------------------------------------------------------------------
  if(is.list(object)){

    ##extent the list of arguments if set

    ##sequence_structure
    sequence_structure <- rep(list(sequence_structure), length = length(object))

    ##RF_nat.lim
    RF_nat.lim <- rep(list(RF_nat.lim), length = length(object))

    ##RF_reg.lim
    RF_reg.lim <- rep(list(RF_reg.lim), length = length(object))

    ##method
    method <- rep(list(method), length = length(object))

    ##method.control
    method.control <- rep(list(method.control), length = length(object))

    ##test_parameters
    if(is(test_parameters[[1]], "list")){
      test_parameters <- rep(test_parameters, length = length(object))

    }else{
     test_parameters <- rep(list(test_parameters), length = length(object))

    }


    ##n.MC
    n.MC <- rep(list(n.MC), length = length(object))

    ##main
    if("main"%in% names(list(...))){

      if(is(list(...)$main, "list")){
        temp_main <- rep(list(...)$main, length = length(object))

      }else{
        temp_main <- rep(list(list(...)$main), length = length(object))

      }

    }else{
      temp_main <- as.list(paste0("ALQ #",1:length(object)))

    }


    ##run analysis
    temp <- lapply(1:length(object), function(x){

      analyse_IRSAR.RF(
        object = object[[x]],
        sequence_structure = sequence_structure[[x]],
        RF_nat.lim = RF_nat.lim[[x]],
        RF_reg.lim = RF_reg.lim[[x]],
        method = method[[x]],
        method.control = method.control[[x]],
        test_parameters = test_parameters[[x]],
        n.MC = n.MC[[x]],
        txtProgressBar = txtProgressBar,
        plot = plot,
        plot_reduced = plot_reduced,
        main = temp_main[[x]],
        ...)
    })

    ##combine everything to one RLum.Results object as this as what was written ... only
    ##one object

    ##merge results and check if the output became NULL
    results <- merge_RLum(temp)

    ##DO NOT use invisible here, this will stop the function from stopping
    if(length(results) == 0){
      return(NULL)

    }else{
      return(results)

    }

  }


  ##===============================================================================================#
  ## INTEGRITY TESTS AND SEQUENCE STRUCTURE TESTS
  ##===============================================================================================#

  ##MISSING INPUT
  if(missing("object")){
    stop("[analyse_IRSAR.RF()] No input 'object' set!")
  }

  ##INPUT OBJECTS
  if(!is(object, "RLum.Analysis")){
    stop("[analyse_IRSAR.RF()] Input object is not of type 'RLum.Analysis'!")
  }

  ##CHECK OTHER ARGUMENTS
  if(!is(sequence_structure, "character")){
    stop("[analyse_IRSAR.RF()] argument 'sequence_structure' needs to be of type character.")
  }

    ##n.MC
    if(!is(n.MC, "numeric") || n.MC <= 0){
      stop("[analyse_IRSAR.RF()] argument 'n.MC' has to be of type integer and >= 0")
    }



  ##SELECT ONLY MEASURED CURVES
  ## (this is not really necessary but rather user friendly)
  if(!length(suppressWarnings(get_RLum(object, curveType= "measured"))) == 0){
    object <- get_RLum(object, curveType= "measured", drop = FALSE)

  }

  ##INVESTIGATE SEQUENCE OBJECT STRUCTURE

  ##grep object strucute
  temp.sequence_structure <- structure_RLum(object)

  ##grep name of the sequence and the position this will be useful later on
  ##name
  if (!is.null(suppressWarnings(get_RLum(get_RLum(object, record.id = 1), info.object = "name")))) {
    aliquot.sequence_name <-
      get_RLum(get_RLum(object, record.id = 1), info.object = "name")

  }else{
    aliquot.sequence_name <- NA

  }


  ##position
  if (!is.null(suppressWarnings(get_RLum(get_RLum(object, record.id = 1), info.object = "position")))){
    aliquot.position <-
      get_RLum(get_RLum(object, record.id = 1), info.object = "position")

  }else{
    aliquot.position <- NA

  }

  ##date
  if (!is.null(suppressWarnings(get_RLum(get_RLum(object, record.id = 1), info.object = "startDate")))){
    aliquot.date <-
      get_RLum(get_RLum(object, record.id = 1), info.object = "startDate")

    ##transform so far the format can be identified
    if (nchar(aliquot.date) == 14) {
      aliquot.date <-
        paste(c(
          substr(aliquot.date, 1,4),substr(aliquot.date, 5,6), substr(aliquot.date, 7,8)
        ), collapse = "-")

    }

  }else{
    aliquot.date <- NA

  }




  ##set structure values
  temp.sequence_structure$protocol.step <-
    rep(sequence_structure, length_RLum(object))[1:length_RLum(object)]

  ##check if the first curve is shorter than the first curve
  if (temp.sequence_structure[1,"n.channels"] > temp.sequence_structure[2,"n.channels"]) {
    stop(
      "[analyse_IRSAR.RF()] Number of data channels in RF_nat > RF_reg. This is not supported!"
    )

  }

  ##===============================================================================================#
  ## SET CURVE LIMITS
  ##===============================================================================================#
  ##the setting here will be valid for all subsequent operations

  ##01
  ##first get allowed curve limits, this makes the subsequent checkings easier and the code
  ##more easier to read
  RF_nat.lim.default <- c(1,max(
    subset(
      temp.sequence_structure,
      temp.sequence_structure$protocol.step == "NATURAL"
    )$n.channels
  ))

  RF_reg.lim.default <- c(1,max(
    subset(
      temp.sequence_structure,
      temp.sequence_structure$protocol.step == "REGENERATED"
    )$n.channels
  ))


  ##02 - check boundaris
  ##RF_nat.lim
  if (is.null(RF_nat.lim) || is.na(RF_nat.lim)) {
    RF_nat.lim <- RF_nat.lim.default

  }else {
    ##this allows to provide only one boundary and the 2nd will be added automatically
    if (length(RF_nat.lim) == 1) {
      RF_nat.lim <- c(RF_nat.lim, RF_nat.lim.default[2])

    }

    if (min(RF_nat.lim) < RF_nat.lim.default[1] |
        max(RF_nat.lim) > RF_nat.lim.default[2]) {
      RF_nat.lim <- RF_nat.lim.default

      warning(paste0(
        "RF_nat.lim out of bounds, reset to: RF_nat.lim = c(",
        paste(range(RF_nat.lim), collapse = ":")
      ),")")
    }

  }

  ##RF_reg.lim
  ##
  if (is.null(RF_reg.lim)) {
    RF_reg.lim <- RF_reg.lim.default

  }else {
    ##this allows to provide only one boundary and the 2nd will be added automatically
    if (length(RF_reg.lim) == 1) {
      RF_reg.lim <- c(RF_reg.lim, RF_reg.lim.default[2])

    }

    if (min(RF_reg.lim) < RF_reg.lim.default[1] |
        max(RF_reg.lim) > RF_reg.lim.default[2]) {
      RF_reg.lim <- RF_reg.lim.default

      warning(paste0(
        "RF_reg.lim out of bounds, reset to: RF_reg.lim = c(",
        paste(range(RF_reg.lim), collapse = ":")
      ),")")

    }
  }

  ##check if intervalls make sense at all
  if(length(RF_reg.lim[1]:RF_reg.lim[2]) < RF_nat.lim[2]){
    RF_reg.lim[2] <- RF_reg.lim[2] + abs(length(RF_reg.lim[1]:RF_reg.lim[2]) - RF_nat.lim[2]) + 1

    warning(paste0("Length intervall RF_reg.lim < length RF_nat. Reset to RF_reg.lim = c(",
                   paste(range(RF_reg.lim), collapse=":")),")")

  }


  ##===============================================================================================#
  ## SET METHOD CONTROL PARAMETER - FOR BOTH METHODS
  ##===============================================================================================#
  ##
  ##set supported values with default
  method.control.settings <- list(
    trace = FALSE,
    maxiter = 500,
    warnOnly = FALSE,
    minFactor = 1 / 4096,
    correct_onset = TRUE,
    show_density = TRUE,
    show_fit = FALSE,
    n.MC = 1000
  )

  ##modify list if necessary
  if(!is.null(method.control)){

    if(!is(method.control, "list")){
      stop("[analyse_IRSAR.RF()] 'method.control' has to be of type 'list'!")

    }

    ##check whether this arguments are supported at all
    if (length(which(
      names(method.control) %in% names(method.control.settings) == FALSE
    ) != 0)) {
      temp.text <- paste0(
        "[analyse_IRSAR.RF()] Argument(s) '",
        paste(names(method.control)[which(names(method.control) %in% names(method.control.settings) == FALSE)], collapse = " and "),
        "' are not supported for 'method.control'. Supported arguments are: ",
        paste(names(method.control.settings), collapse = ", ")
      )

      warning(temp.text)
      rm(temp.text)

    }

    ##modify list
    method.control.settings <- modifyList(x = method.control.settings, val = method.control)

  }


  ##===============================================================================================#
  ## SET PLOT PARAMETERS
  ##===============================================================================================#

  ##get channel resolution (should be equal for all curves, but if not the mean is taken)
  resolution.RF <- round(mean((temp.sequence_structure$x.max/temp.sequence_structure$n.channels)),digits=1)


  plot.settings <- list(
    main = "IR-RF",
    xlab = "Time [s]",
    ylab = paste0("IR-RF [cts/", resolution.RF," s]"),
    log = "",
    cex = 1,
    legend = TRUE,
    legend.text = c("RF_nat","RF_reg"),
    legend.pos = "top",
    xaxt = "s"
    ##xlim and ylim see below as they has to be modified differently
  )

  ##modify list if something was set
  plot.settings <- modifyList(plot.settings, list(...))

  ##=============================================================================#
  ## ANALYSIS
  ##=============================================================================#

  ##grep first regenerated curve
  RF_reg <- as.data.frame(object@records[[
    temp.sequence_structure[temp.sequence_structure$protocol.step=="REGENERATED","id"]]]@data)

    ##correct of the onset of detection by using the first time value
    if (method == "SLIDE" &
        method.control.settings$correct_onset == TRUE) {
      RF_reg[,1] <- RF_reg[,1] - RF_reg[1,1]

    }


  RF_reg.x <- RF_reg[RF_reg.lim[1]:RF_reg.lim[2],1]
  RF_reg.y <- RF_reg[RF_reg.lim[1]:RF_reg.lim[2],2]


  ##grep values from natural signal
  RF_nat <- as.data.frame(object@records[[
    temp.sequence_structure[temp.sequence_structure$protocol.step=="NATURAL","id"]]]@data)

    ##correct of the onset of detection by using the first time value
  if (method == "SLIDE" &
      method.control.settings$correct_onset == TRUE) {
    RF_nat[,1] <- RF_nat[,1] - RF_nat[1,1]
  }


  ##limit values to fit range (at least to the minimum)
  RF_nat.limited<- RF_nat[min(RF_nat.lim):max(RF_nat.lim),]

  ##calculate some useful parameters
  RF_nat.mean <- mean(RF_nat.limited[,2])
  RF_nat.sd <- sd(RF_nat.limited[,2])

  RF_nat.error.lower <- quantile(RF_nat.limited[,2], 0.975)
  RF_nat.error.upper <- quantile(RF_nat.limited[,2], 0.025)


  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  ##METHOD FIT
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    ## REGENERATED SIGNAL
    # set function for fitting ------------------------------------------------

    fit.function <-
      as.formula(y ~ phi.0 - (delta.phi * ((1 - exp(
        -lambda * x
      )) ^ beta)))

    ##stretched expontial function according to Erfurt et al. (2003)
    ## + phi.0 >> initial IR-RF flux
    ## + delta.phi >> dose dependent change of the IR-RF flux
    ## + lambda >> exponential parameter
    ## + beta >> dispersive factor

    # set start parameter estimation ------------------------------------------

    fit.parameters.start <- c(
      phi.0 = max(RF_reg.y),
      lambda = 0.0001,
      beta = 1,
      delta.phi = 1.5 * (max(RF_reg.y) - min(RF_reg.y))
    )

  if(method == "FIT"){

    # start nls fitting -------------------------------------------------------

    ##Monte Carlo approach for fitting
    fit.parameters.results.MC.results <- data.frame()

    ##produce set of start paramters
    phi.0.MC <- rep(fit.parameters.start["phi.0"], n.MC)
    lambda.MC <- seq(0.0001, 0.001, by=(0.001-0.0001)/n.MC) ##TODO
    beta.MC <- rep(fit.parameters.start["beta"], n.MC)
    delta.phi.MC <- rep(fit.parameters.start["delta.phi"], n.MC)

    ##start fitting loop for MC runs
    for(i in 1:n.MC){

      fit.MC <- try(nls(
        fit.function,
        trace = FALSE,
        data = list(x = RF_reg.x, y = RF_reg.y),
        algorithm = "port",
        start = list(
          phi.0 = phi.0.MC[i],
          delta.phi = delta.phi.MC[i],
          lambda = lambda.MC[i],
          beta = beta.MC[i]
        ),
        nls.control(
          maxiter = 100,
          warnOnly = FALSE,
          minFactor = 1 / 1024
        ),
        lower = c(
          phi.0 = .Machine$double.xmin,
          delta.phi = .Machine$double.xmin,
          lambda = .Machine$double.xmin,
          beta = .Machine$double.xmin
        ),
        upper = c(
          phi.0 = max(RF_reg.y),
          delta.phi = max(RF_reg.y),
          lambda = 1,
          beta = 100
        )
      ),
      silent = TRUE
      )

      if(inherits(fit.MC,"try-error") == FALSE) {
        temp.fit.parameters.results.MC.results <- coef(fit.MC)

        fit.parameters.results.MC.results[i,"phi.0"] <-
          temp.fit.parameters.results.MC.results["phi.0"]
        fit.parameters.results.MC.results[i,"lambda"] <-
          temp.fit.parameters.results.MC.results["lambda"]
        fit.parameters.results.MC.results[i,"delta.phi"] <-
          temp.fit.parameters.results.MC.results["delta.phi"]
        fit.parameters.results.MC.results[i,"beta"] <-
          temp.fit.parameters.results.MC.results["beta"]

      }
    }

    ##FINAL fitting after successful MC
    if(length(na.omit(fit.parameters.results.MC.results)) != 0){

      ##choose median as final fit version
      fit.parameters.results.MC.results <- sapply(na.omit(fit.parameters.results.MC.results), median)

      ##try final fitting
      fit <- try(nls(
        fit.function,
        trace = method.control.settings$trace,
        data = data.frame(x = RF_reg.x, y = RF_reg.y),
        algorithm = "port",
        start = list(
          phi.0 = fit.parameters.results.MC.results["phi.0"],
          delta.phi = fit.parameters.results.MC.results["delta.phi"],
          lambda = fit.parameters.results.MC.results["lambda"],
          beta = fit.parameters.results.MC.results["beta"]
        ),
        nls.control(
          maxiter = method.control.settings$maxiter,
          warnOnly = method.control.settings$warnOnly,
          minFactor = method.control.settings$minFactor
        ),
        lower = c(
          phi.0 = .Machine$double.xmin,
          delta.phi = .Machine$double.xmin,
          lambda = .Machine$double.xmin,
          beta = .Machine$double.xmin
        ),
        upper = c(
          phi.0 = max(RF_reg.y),
          delta.phi = max(RF_reg.y),
          lambda = 1, beta = 100
        )
      ),
      silent = FALSE
      )
    }else{

      fit <- NA
      class(fit) <- "try-error"

    }

    # get parameters ----------------------------------------------------------
    # and with that the final De

    if (!inherits(fit,"try-error")) {
      fit.parameters.results <- coef(fit)

    }else{
      fit.parameters.results <- NA

    }

    ##calculate De value
    if (!is.na(fit.parameters.results[1])) {
      De <- suppressWarnings(round(log(
        -((RF_nat.mean - fit.parameters.results["phi.0"]) /
            -fit.parameters.results["delta.phi"]
        ) ^ (1 / fit.parameters.results["beta"]) + 1
      ) /
        -fit.parameters.results["lambda"], digits =
        2))

      ##This could be solved with a MC simulation, but for this the code has to be adjusted
      ##The question is: Where the parameters are coming from?
      ##TODO
      De.error <- NA

      De.lower <- suppressWarnings(round(log(
        -((RF_nat.error.lower - fit.parameters.results["phi.0"]) /
            -fit.parameters.results["delta.phi"]
        ) ^ (1 / fit.parameters.results["beta"]) + 1
      ) /
        -fit.parameters.results["lambda"],digits = 2))

      De.upper <- suppressWarnings(round(log(
        -((RF_nat.error.upper - fit.parameters.results["phi.0"]) /
            -fit.parameters.results["delta.phi"]
        ) ^ (1 / fit.parameters.results["beta"]) + 1
      ) /
        -fit.parameters.results["lambda"],digits = 2))

    }else{
      De <- NA
      De.error <- NA
      De.lower <- NA
      De.upper <- NA

    }
  }

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  ##METHOD SLIDE - ANALYSIS
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  else if(method == "SLIDE"){

    ##convert to matrix (in fact above the matrix data were first transfered to data.frames ... here
    ##we correct this ... again)  ##TODO
    RF_nat.limited <- as.matrix(RF_nat.limited)
    RF_reg.limited <- matrix(c(RF_reg.x, RF_reg.y), ncol = 2)
    RF_nat <- as.matrix(RF_nat)

    ##DEFINE FUNCTION FOR SLIDING
    ##FIND MINIMUM - this is done in a function so that it can be further used for MC simulations
    sliding <- function(RF_nat,
                        RF_nat.limited,
                        RF_reg.limited,
                        n.MC = method.control.settings$n.MC,
                        numerical.only = FALSE){


      ##(0) set objects ... nomenclature as used in Frouin et al., please note that here the index
      ##is used instead the real time values
      t_max.id <- nrow(RF_reg.limited)
      t_max_nat.id <- nrow(RF_nat.limited)
      t_min.id <- 1
      t_min <- RF_nat.limited[1,1]

      ##(1) calculate sum of residual squares using internal Rcpp function

      #pre-allocate object
      temp.sum.residuals <- vector("numeric", length = t_max.id - t_max_nat.id)

      ##calculate sum of squared residuals ... for the entire set
      temp.sum.residuals <-
        .analyse_IRSARRF_SRS(
          values_regenerated_limited =  RF_reg.limited[,2],
          values_natural_limited = RF_nat.limited[,2],
          n_MC =  n.MC
        )


      #(2) get minimum value (index and time value)
      t_n.id <- which.min(temp.sum.residuals$sliding_vector)

      temp.sliding.step <- RF_reg.limited[t_n.id] - t_min

      ##(3) slide curve graphically ... full data set we need this for the plotting later
      RF_nat.slided <- matrix(data = c(RF_nat[,1] + temp.sliding.step, RF_nat[,2]), ncol = 2)
      t_n <- RF_nat.slided[1,1]

      ##the same for the MC runs of the minimum values
      t_n.MC <-
        vapply(X = 1:length(temp.sum.residuals$sliding_vector_min_MC), FUN = function(x) {
          t_n.id.MC <-
            which(temp.sum.residuals$sliding_vector == temp.sum.residuals$sliding_vector_min_MC[x])
          temp.sliding.step.MC <- RF_reg.limited[t_n.id.MC] - t_min
          t_n.MC <- (RF_nat[,1] + temp.sliding.step.MC)[1]
          return(t_n.MC)

        }, FUN.VALUE = vector(mode = "numeric", length = 1))


      ##(4) get residuals (needed to be plotted later)
      ## they cannot be longer than the RF_reg.limited curve
      if((t_n.id+length(RF_nat.limited[,2])-1) >= nrow(RF_reg.limited)){
        residuals <- RF_nat.limited[1:length(t_n.id:nrow(RF_reg.limited)),2]
        - RF_reg.limited[t_n.id:nrow(RF_reg.limited), 2]

      }else{
        residuals <- RF_nat.limited[,2] - RF_reg.limited[t_n.id:(t_n.id+length(RF_nat.limited[,2])-1), 2]

      }

      ##(4.1) calculate De from the first channel ... which is t_n here
      De <- round(t_n, digits = 2)
      De.MC <- round(t_n.MC, digits = 2)
      temp.trend.fit <- NA

      ##(5) calculate trend fit
      if(length(RF_nat.limited[,1]) > length(residuals)){
        temp.trend.fit <- coef(lm(y~x,
                                  data.frame(x = RF_nat.limited[1:length(residuals),1], y = residuals)))

      }else{
        temp.trend.fit <- coef(lm(y~x, data.frame(x = RF_nat.limited[,1], y = residuals)))

      }



      ##return values and limited if they are not needed
      if (numerical.only == FALSE) {
        return(
          list(
            De = De,
            De.MC = De.MC,
            residuals = residuals,
            trend.fit = temp.trend.fit,
            RF_nat.slided = RF_nat.slided,
            t_n.id = t_n.id,
            squared_residuals = temp.sum.residuals$sliding_vector
          )
        )
      }else{
        return(list(De = De, De.MC = De.MC))
      }

    }##end of function sliding()


    ##PERFORM sliding and overwrite values
    slide <-  sliding(
      RF_nat = RF_nat,
      RF_nat.limited = RF_nat.limited,
      RF_reg.limited = RF_reg.limited
    )


    ##write results in variables
    De <- slide$De
    residuals <- slide$residuals
    RF_nat.slided <-  slide$RF_nat.slided


    # ERROR ESTIMATION
    # MC runs for error calculation ---------------------------------------------------------------

    ##set residual matrix for MC runs, i.e. set up list of pseudo RF_nat curves as function
    ##(i.e., bootstrap from the natural curve distribution)

    slide.MC.list <- lapply(1:n.MC,function(x) {

      ##also here we have to account for the case that user do not understand
      ##what they are doing ...
      if(slide$t_n.id + nrow(RF_nat.limited)-1 > nrow(RF_reg.limited)){
        cbind(
          RF_nat.limited[1:length(slide$t_n.id:nrow(RF_reg.limited)),1],
          (RF_reg.limited[slide$t_n.id:nrow(RF_reg.limited) ,2]
           + sample(residuals,
                    size = length(slide$t_n.id:nrow(RF_reg.limited)),
                    replace = TRUE)
          )
        )

      }else{
        cbind(
          RF_nat.limited[,1],
          (RF_reg.limited[slide$t_n.id:(slide$t_n.id + nrow(RF_nat.limited)-1) ,2]
           + sample(residuals, size = nrow(RF_nat.limited), replace = TRUE)
          )
        )
      }

    })


    if(txtProgressBar){
      ##terminal output fo MC
      cat("\n\t Run Monte Carlo loops for error estimation\n")

      ##progress bar
      pb<-txtProgressBar(min=0, max=n.MC, initial=0, char="=", style=3)
    }


    De.MC <- c(vapply(X = 1:n.MC,
                    FUN.VALUE = vector("numeric", length = method.control.settings$n.MC),
                    FUN = function(i){

      temp.slide.MC <- sliding(
        RF_nat = RF_nat,
        RF_reg.limited = RF_reg.limited,
        RF_nat.limited = slide.MC.list[[i]],
        numerical.only = TRUE
      )

      ##update progress bar
      if (txtProgressBar) {
        setTxtProgressBar(pb, i)
      }

       ##do nothing else, just report all possible values
       return(temp.slide.MC[[2]])

    }))

    ##close
    if(txtProgressBar){close(pb)}

    ##calculate absolute deviation between De and the here newly calculated De.MC
    ##this is, e.g. ^t_n.1* - ^t_n in Frouin et al.
    De.diff <- diff(x = c(De, De.MC))
    De.error <- round(sd(De.MC), digits = 2)
    De.lower <- De - quantile(De.diff, 0.975)
    De.upper <- De - quantile(De.diff, 0.025)

  }else{

    warning("Analysis skipped: Unknown method or threshold of test parameter exceeded.")

  }

  ##===============================================================================================#
  ## TEST PARAMETER
  ##===============================================================================================#
  ## Test parameter are evaluated after all the calculations have been done as
  ## it should be up to the user to decide whether a value should be taken into account or not.

  ##(0)
  ##set default values and overwrite them if there was something new
  ##set defaults
  TP <- list(
    curves_ratio = 1.001,
    intersection_ratio = NA,
    residuals_slope = NA,
    curves_bounds = ceiling(max(RF_reg.x)),
    dynamic_ratio = NA,
    lambda = NA,
    beta = NA,
    delta.phi = NA
  )

    ##modify default values by given input
    if(!is.null(test_parameters)){TP <- modifyList(TP, test_parameters)}

    ##remove NULL elements from list
    TP <- TP[!sapply(TP, is.null)]

    ##set list with values we want to evaluate
    TP <- lapply(TP, function(x){
      data.frame(THRESHOLD = as.numeric(x), VALUE = NA, STATUS = "OK", stringsAsFactors = TRUE)

    })


  ##(1) check if RF_nat > RF_reg, considering the fit range
  ##TP$curves_ratio
    if ("curves_ratio" %in% names(TP)) {
      TP$curves_ratio$VALUE <-
        sum(RF_nat.limited[,2]) / sum(RF_reg[RF_nat.lim[1]:RF_nat.lim[2], 2])

      if (!is.na(TP$curves_ratio$THRESHOLD)) {
        TP$curves_ratio$STATUS <-
          ifelse(TP$curves_ratio$VALUE >= TP$curves_ratio$THRESHOLD, "FAILED", "OK")
      }
    }

   ##(1.1) check if RF_nat > RF_reg, considering the fit range
   ##TP$intersection_ratio
    if ("intersection_ratio" %in% names(TP)) {

      ##It is, as always, a little bit more complicated ...
      ##We cannot just normalise both curves and compare ratios. With increasing De the curve
      ##shape of the RF_nat curve cannot be the same as the RF_reg curve at t = 0. Therefore we
      ##have to find the segment in the RF_reg curve that fits to the RF_nat curve
      ##
      ##(1) get maximum count value for RF_nat
      IR_RF_nat.max <- max(RF_nat.limited[,2])

      ##(2) find corresponding time value for RF_reg (here no limited)
      IR_RF_reg.corresponding_id <- which.min(abs(RF_reg[,2] - IR_RF_nat.max))

      ##(3) calculate ratio, but just starting from the point where both curves correspond
      ##in terms of intensiy, otherwise the ratio cannot be correct

      ##the boundary check is necessary to avoid errors
      if((IR_RF_reg.corresponding_id + length(RF_nat.lim[1]:RF_nat.lim[2])) > length(RF_reg[,2])){
        TP$intersection_ratio$VALUE <- Inf

      }else{

      TP$intersection_ratio$VALUE <-
        abs(1 - sum((RF_nat.limited[, 2] / max(RF_nat.limited[, 2]))) /
              sum(RF_reg[IR_RF_reg.corresponding_id:(IR_RF_reg.corresponding_id + length(RF_nat.lim[1]:RF_nat.lim[2]) - 1), 2] /
                    max(RF_reg[IR_RF_reg.corresponding_id:(IR_RF_reg.corresponding_id + length(RF_nat.lim[1]:RF_nat.lim[2]) - 1), 2])))

      if (!is.na(TP$intersection_ratio$THRESHOLD)) {
        TP$intersection_ratio$STATUS <-
          ifelse(TP$intersection_ratio$VALUE >= TP$intersection_ratio$THRESHOLD, "FAILED", "OK")
      }

      rm(IR_RF_nat.max, IR_RF_reg.corresponding_id)

      }
    }

  ##(2) check slop of the residuals using a linear fit
  ##TP$residuals_slope
    if ("residuals_slope" %in% names(TP)) {
      if (exists("slide")) {
        TP$residuals_slope$VALUE <- abs(slide$trend.fit[2])

        if (!is.na(TP$residuals_slope$THRESHOLD)) {
          TP$residuals_slope$STATUS <- ifelse(
            TP$residuals_slope$VALUE >= TP$residuals_slope$THRESHOLD, "FAILED", "OK")

        }
      }
    }

  ##(3) calculate dynamic range of regenrated curve
  ##TP$dynamic_ratio
  if ("dynamic_ratio"%in%names(TP)){
    TP.dynamic_ratio <- subset(temp.sequence_structure,
                               temp.sequence_structure$protocol.step == "REGENERATED")
    TP$dynamic_ratio$VALUE <- TP.dynamic_ratio$y.max/TP.dynamic_ratio$y.min

    if (!is.na(TP$dynamic_ratio$THRESHOLD)){
      TP$dynamic_ratio$STATUS  <- ifelse(
        TP$dynamic_ratio$VALUE <= TP$dynamic_ratio$THRESHOLD , "FAILED", "OK")
    }
  }


  ##(4) decay parameter
  ##TP$lambda
  if ("lambda"%in%names(TP) & "beta"%in%names(TP) & "delta.phi"%in%names(TP)){

    fit.lambda <- try(minpack.lm::nlsLM(
        fit.function,
        data = data.frame(x = RF_reg.x, y = RF_reg.y),
        algorithm = "LM",
        start = list(
          phi.0 = fit.parameters.start["phi.0"],
          delta.phi = fit.parameters.start["delta.phi"],
          lambda = fit.parameters.start["lambda"],
          beta = fit.parameters.start["beta"]
        ),
        lower = c(
          phi.0 = .Machine$double.xmin,
          delta.phi = .Machine$double.xmin,
          lambda = .Machine$double.xmin,
          beta = .Machine$double.xmin
        ),
        upper = c(
          phi.0 = max(RF_reg.y),
          delta.phi = max(RF_reg.y),
          lambda = 1, beta = 100
        )
      ),
    silent = TRUE
    )

    if(!inherits(fit.lambda, "try-error")){
       temp.coef <- coef(fit.lambda)

       TP$lambda$VALUE <- temp.coef["lambda.lambda"]
       TP$beta$VALUE <- temp.coef["beta.beta"]
       TP$delta.phi$VALUE <- temp.coef["delta.phi.delta.phi"]

       if (!is.na( TP$lambda$THRESHOLD)){
        TP$lambda$STATUS <- ifelse(TP$lambda$VALUE <= TP$lambda$THRESHOLD, "FAILED", "OK")
       }

       if (!is.na( TP$beta$THRESHOLD)){
         TP$beta$STATUS <- ifelse(TP$beta$VALUE <= TP$beta$THRESHOLD, "FAILED", "OK")
       }

       if (!is.na( TP$delta.phi$THRESHOLD)){
         TP$delta.phi$STATUS <- ifelse(TP$delta.phi$VALUE <= TP$delta.phi$THRESHOLD, "FAILED", "OK")
       }

    }
  }

  ##(99) check whether after sliding the
  ##TP$curves_bounds
  if (!is.null(TP$curves_bounds)) {
    if(exists("slide")){
      TP$curves_bounds$VALUE <- max(RF_nat.slided[RF_nat.lim,1])

       if (!is.na(TP$curves_bounds$THRESHOLD)){
        TP$curves_bounds$STATUS <- ifelse(TP$curves_bounds$VALUE >= floor(max(RF_reg.x)), "FAILED", "OK")
       }


    }else if(exists("fit")){
      TP$curves_bounds$VALUE <- De.upper

      if (!is.na(TP$curves_bounds$THRESHOLD)){
        TP$curves_bounds$STATUS <- ifelse(TP$curves_bounds$VALUE  >= max(RF_reg.x), "FAILED", "OK")
      }
    }
  }


  ##Combine everything in a data.frame
    if(length(TP) != 0) {
      TP.data.frame <- as.data.frame(
        cbind(
          POSITION =  as.integer(aliquot.position),
          PARAMETER = c(names(TP)),
          do.call(data.table::rbindlist, args = list(l = TP)),
          SEQUENCE_NAME = aliquot.sequence_name,
          UID = NA
        )
      )

      ##set De.status to indicate whether there is any problem with the De according to the test parameter
      if ("FAILED" %in% TP.data.frame$STATUS) {
        De.status <- "FAILED"
      }else{
        De.status <- "OK"
      }

    }else{
      De.status <- "OK"
      TP.data.frame <- NULL

    }

  ##===============================================================================================#
  ## PLOTTING
  ##===============================================================================================#
  if(plot){


    ##get internal colour definition
    col <- get("col", pos = .LuminescenceEnv)

    if (!plot_reduced) {

      ##grep par default and define reset
      def.par <- par(no.readonly = TRUE)
      on.exit(par(def.par))

      ##set plot frame, if a method was choosen
      if (method == "SLIDE" | method == "FIT") {
        layout(matrix(c(1, 2), 2, 1, byrow = TRUE), c(2), c(1.3, 0.4), TRUE)
        par(
          oma = c(1, 1, 1, 1),
          mar = c(0, 4, 3, 0),
          cex = plot.settings$cex
        )

      }
    }else{

      if(plot.settings[["cex"]] != 1){

        def.par <- par()[["cex"]]
        on.exit(par(def.par))

        par(cex = plot.settings[["cex"]])

      }

    }

    ##here control xlim and ylim behaviour
    ##xlim
    xlim  <- if ("xlim" %in% names(list(...))) {
      list(...)$xlim
    } else
    {
      if (plot.settings$log == "x" | plot.settings$log == "xy") {
        c(min(temp.sequence_structure$x.min),max(temp.sequence_structure$x.max))

      }else{
        c(0,max(temp.sequence_structure$x.max))

      }

    }

    ##ylim
    ylim  <- if("ylim" %in% names(list(...))) {list(...)$ylim} else
    {c(min(temp.sequence_structure$y.min), max(temp.sequence_structure$y.max))}


    ##open plot area
    plot(
      NA,NA,
      xlim = xlim,
      ylim = ylim,
      xlab = ifelse((method != "SLIDE" & method != "FIT") | plot_reduced, plot.settings$xlab," "),
      xaxt = ifelse((method != "SLIDE" & method != "FIT") | plot_reduced, plot.settings$xaxt,"n"),
      yaxt = "n",
      ylab = plot.settings$ylab,
      main = plot.settings$main,
      log = plot.settings$log,

    )

    if(De.status == "FAILED"){

      ##build list of failed TP
      mtext.message <- paste0(
        "Threshold exceeded for:  ",
        paste(subset(TP.data.frame, TP.data.frame$STATUS == "FAILED")$PARAMETER, collapse = ", "),". For details see manual.")

      ##print mtext
      mtext(text = mtext.message,
            side = 3, outer = TRUE, col = "red",
            cex = 0.8 * par()[["cex"]])
      warning(mtext.message)

    }

    ##use scientific format for y-axis
    labels <- axis(2, labels = FALSE)
    axis(side = 2, at = labels, labels = format(labels, scientific = TRUE))

    ##(1) plot points that have been not selected
    points(RF_reg[-(min(RF_reg.lim):max(RF_reg.lim)),1:2], pch=3, col=col[19])

    ##(2) plot points that has been used for the fitting
    points(RF_reg.x,RF_reg.y, pch=3, col=col[10])

    ##show natural points if no analysis was done
    if(method != "SLIDE" & method != "FIT"){

      ##add points
      points(RF_nat, pch = 20, col = "grey")
      points(RF_nat.limited, pch = 20, col = "red")

      ##legend
      if (plot.settings$legend) {
        legend(
          plot.settings$legend.pos,
          legend = plot.settings$legend.text,
          pch = c(19, 3),
          col = c("red", col[10]),
          horiz = TRUE,
          bty = "n",
          cex = .9 * par()[["cex"]]
        )
      }


    }


    ##Add fitted curve, if possible. This is a graphical control that might be considered
    ##as useful before further analysis will be applied
    if (method.control.settings$show_fit) {

      if(!is(fit.lambda, "try-error")){
        fit.lambda_coef <- coef(fit.lambda)

        curve(fit.lambda_coef[[1]]-
                (fit.lambda_coef[[2]]*
                   ((1-exp(-fit.lambda_coef[[3]]*x))^fit.lambda_coef[[4]])),
              add=TRUE,
              lty = 2,
              col="red")

        rm(fit.lambda_coef)
      }else{
        warning("[analyse_IRSAR.RF()] No fit possible, no fit shown.")


      }

    }

    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    ## PLOT - METHOD FIT
    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    if(method == "FIT"){

      ##dummy to cheat R CMD check
      x<-NULL; rm(x)

      ##plot fitted curve
      curve(fit.parameters.results["phi.0"]-
              (fit.parameters.results["delta.phi"]*
                 ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])),
            add=TRUE,
            from = RF_reg[min(RF_reg.lim), 1],
            to = RF_reg[max(RF_reg.lim), 1],
            col="red")

      ##plotting to show the limitations if RF_reg.lim was chosen
      ##show fitted curve GREY (previous red curve)
      curve(fit.parameters.results["phi.0"]-
              (fit.parameters.results["delta.phi"]*
                 ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])),
            add=TRUE,
            from = min(RF_reg[, 1]),
            to = RF_reg[min(RF_reg.lim), 1],
            col="grey")

      ##show fitted curve GREY (after red curve)
      curve(fit.parameters.results["phi.0"]-
              (fit.parameters.results["delta.phi"]*
                 ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])),
            add=TRUE,
            from = RF_reg[max(RF_reg.lim), 1],
            to = max(RF_reg[, 1]),
            col="grey")

      ##add points
      points(RF_nat, pch = 20, col = col[19])
      points(RF_nat.limited, pch = 20, col = col[2])

      ##legend
      if (plot.settings$legend) {
        legend(
          plot.settings$legend.pos,
          legend = plot.settings$legend.text,
          pch = c(19, 3),
          col = c("red", col[10]),
          horiz = TRUE,
          bty = "n",
          cex = .9 * par()[["cex"]]
        )
      }

      ##plot range choosen for fitting
      abline(v=RF_reg[min(RF_reg.lim), 1], lty=2)
      abline(v=RF_reg[max(RF_reg.lim), 1], lty=2)

      ##plot De if De was calculated
      if(is.na(De) == FALSE & is.nan(De) == FALSE){

        lines(c(0,De.lower), c(RF_nat.error.lower,RF_nat.error.lower), lty=2, col="grey")
        lines(c(0,De), c(RF_nat.mean,RF_nat.mean), lty=2, col="red")
        lines(c(0,De.upper), c(RF_nat.error.upper,RF_nat.error.upper), lty=2, col="grey")

        lines(c(De.lower, De.lower),
              c(0,RF_nat.error.lower), lty=2, col="grey")
        lines(c(De,De), c(0, RF_nat.mean), lty=2, col="red")
        lines(c(De.upper, De.upper),
              c(0,RF_nat.error.upper), lty=2, col="grey")

      }

      ##Insert fit and result
      if(is.na(De) != TRUE & (is.nan(De) == TRUE |
                              De > max(RF_reg.x) |
                              De.upper > max(RF_reg.x))){

        try(mtext(side=3, substitute(D[e] == De,
                                     list(De=paste(
                                       De," (",De.lower," ", De.upper,")", sep=""))),
                  line=0, cex=0.8 * par()[["cex"]], col="red"), silent=TRUE)

        De.status <- "VALUE OUT OF BOUNDS"

      } else{

        if ("mtext" %in% names(list(...))) {
          mtext(side = 3, list(...)$mtext)
        }else{
          try(mtext(
            side = 3,
            substitute(D[e] == De,
                       list(
                         De = paste(De," [",De.lower," ; ", De.upper,"]", sep =
                                      "")
                       )),
            line = 0,
            cex = 0.7 * par()[["cex"]]
          ),
          silent = TRUE)
        }

        De.status <- "OK"
      }


      if (!plot_reduced) {

        ##==lower plot==##
        par(mar = c(4.2, 4, 0, 0))

        ##plot residuals
        if (is.na(fit.parameters.results[1]) == FALSE) {
          plot(
            RF_reg.x,
            residuals(fit),
            xlim = c(0, max(temp.sequence_structure$x.max)),
            xlab = plot.settings$xlab,
            yaxt = "n",
            xaxt = plot.settings$xaxt,
            type = "p",
            pch = 20,
            col = "grey",
            ylab = "E",
            log = ""
          )

          ##add 0 line
          abline(h = 0)
        } else{
          plot(
            NA,
            NA,
            xlim = c(0, max(temp.sequence_structure$x.max)),
            ylab = "E",
            xlab = plot.settings$xlab,
            xaxt = plot.settings$xaxt,
            ylim = c(-1, 1)
          )
          text(x = max(temp.sequence_structure$x.max) / 2,
               y = 0,
               "Fitting Error!")
        }

      }
    }

    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    ## PLOT - METHOD SLIDE
    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    else if(method == "SLIDE"){

      ##(0) density plot
      if (method.control.settings$show_density) {
        ##showing the density makes only sense when we see at least 10 data points
        if (length(unique(De.MC)) >= 15) {
          ##calculate density De.MC
          density.De.MC <- density(De.MC)

          ##calculate transformation function
          x.1 <- max(density.De.MC$y)
          x.2 <- min(density.De.MC$y)

          ##with have to limit the scaling a little bit
          if (RF_nat.limited[1,2] >
            max(RF_reg.limited[,2]) - (max(RF_reg.limited[,2]) - min(RF_reg.limited[,2]))*.5) {

            y.1 <- max(RF_reg.limited[,2]) - (max(RF_reg.limited[,2]) - min(RF_reg.limited[,2]))*.5

          }else{
            y.1 <- RF_nat.limited[1,2]

          }

          y.2 <- par("usr")[3]

          m <- (y.1 - y.2) / (x.1 + x.2)
          n <- y.1 - m * x.1

          density.De.MC$y <- m * density.De.MC$y + n
          rm(x.1,x.2,y.1,y.2,m,n)

          polygon(density.De.MC$x,
                  density.De.MC$y,
                  col = rgb(0,0.4,0.8,0.5))

        }else{
          warning("Narrow density distribution, no density distribution plotted!")

        }

      }

      ##(1) plot unused points in grey ... unused points are points outside of the set limit
      points(
        matrix(RF_nat.slided[-(min(RF_nat.lim):max(RF_nat.lim)),1:2], ncol = 2),
        pch = 21, col = col[19]
      )

      ##(2) add used points
      points(RF_nat.slided[min(RF_nat.lim):max(RF_nat.lim),], pch = 21, col = col[2],
             bg = col[2])

      ##(3) add line to show the connection between the first point and the De
      lines(x = c(RF_nat.slided[1,1], RF_nat.slided[1,1]),
            y = c(.Machine$double.xmin,RF_nat.slided[1,2]),
            lty = 2,
            col = col[2]
      )

      ##(4) add arrow at the lowest y-coordinate possible to show the sliding
      if (plot.settings$log != "y" & plot.settings$log != "xy") {
        shape::Arrows(
          x0 = 0,
          y0 = ylim[1],
          y1 = ylim[1],
          x1 = RF_nat.slided[1,1],
          arr.type = "triangle",
          arr.length = 0.3 * par()[["cex"]],
          code = 2,
          col = col[2],
          arr.adj = 1,
          arr.lwd = 1
        )
      }

      ##TODO
      ##uncomment here to see all the RF_nat curves produced by the MC runs
      ##could become a polygone for future versions
      #lapply(1:n.MC, function(x){lines(slide.MC.list[[x]], col = rgb(0,0,0, alpha = 0.2))})

      ##plot range choosen for fitting
      abline(v=RF_reg[min(RF_reg.lim), 1], lty=2)
      abline(v=RF_reg[max(RF_reg.lim), 1], lty=2)

      if (plot.settings$legend) {
        legend(
          plot.settings$legend.pos,
          legend = plot.settings$legend.text,
          pch = c(19, 3),
          col = c("red", col[10]),
          horiz = TRUE,
          bty = "n",
          cex = .9 * par()[["cex"]]
        )

      }



      ##write information on the De in the plot
      if("mtext" %in% names(list(...))) {

        mtext(side = 3, list(...)$mtext)

      }else{

        try(mtext(side=3,
                  substitute(D[e] == De, list(De=paste0(De," [", De.lower, " ; ", De.upper, "]"))),
                  line=0,
                  cex=0.7 * par()[["cex"]]),
            silent=TRUE)

      }

      if (!plot_reduced) {
        ##==lower plot==##
        ##RESIDUAL PLOT
        par(mar = c(4, 4, 0, 0))

        plot(
          NA,
          NA,
          ylim = range(residuals),
          xlim = xlim,
          xlab = plot.settings$xlab,
          type = "p",
          pch = 1,
          col = "grey",
          xaxt = plot.settings$xaxt,
          ylab = "E",
          yaxt = "n",
          log = ifelse(
            plot.settings$log == "y" |
              plot.settings$log == "xy",
            "",
            plot.settings$log
          )
        )

        ##add axis for 0 ... means if the 0 is not visible there is labelling
        axis(side = 4,
             at = 0,
             labels = 0)

        ##add residual indicator (should circle around 0)
        col.ramp <- colorRampPalette(c(col[19], "white", col[19]))
        col.polygon <- col.ramp(100)

        if (plot.settings$log != "x") {
          shape::filledrectangle(
            mid = c((xlim[2]) + (par("usr")[2] - xlim[2]) / 2,
                    max(residuals) - diff(range(residuals)) / 2),
            wx = par("usr")[2] - xlim[2],
            wy = diff(range(residuals)),
            col = col.polygon
          )

        }
        ##add 0 line
        abline(h = 0, lty = 3)

        ##0-line indicator and arrows if this is not visible
        ##red colouring here only if the 0 point is not visible to avoid too much colouring
        if (max(residuals) < 0 &
            min(residuals) < 0) {
          shape::Arrowhead(
            x0 =   xlim[2] + (par("usr")[2] - xlim[2]) / 2,
            y0 = max(residuals),
            angle = 270,
            lcol = col[2],
            arr.length = 0.4,
            arr.type = "triangle",
            arr.col = col[2]
          )

        } else if (max(residuals) > 0 & min(residuals) > 0) {
          shape::Arrowhead(
            x0 =   xlim[2] + (par("usr")[2] - xlim[2]) / 2,
            y0 = min(residuals),
            angle = 90,
            lcol = col[2],
            arr.length = 0.4,
            arr.type = "triangle",
            arr.col = col[2]
          )


        } else{
          points(xlim[2], 0, pch = 3)

        }


        ##add residual points
        if (length(RF_nat.slided[c(min(RF_nat.lim):max(RF_nat.lim)), 1]) > length(residuals)) {
          temp.points.diff <-
            length(RF_nat.slided[c(min(RF_nat.lim):max(RF_nat.lim)), 1]) -
            length(residuals)

          points(RF_nat.slided[c(min(RF_nat.lim):(max(RF_nat.lim) - temp.points.diff)), 1],
                 residuals,
                 pch = 20,
                 col = rgb(0, 0, 0, 0.4))

        } else{
          points(RF_nat.slided[c(min(RF_nat.lim):max(RF_nat.lim)), 1],
                 residuals,
                 pch = 20,
                 col = rgb(0, 0, 0, 0.4))

        }

        ##add vertical line to mark De (t_n)
        abline(v = De, lty = 2, col = col[2])

        ##add numeric value of De ... t_n
        axis(
          side = 1,
          at = De,
          labels = De,
          cex.axis = 0.8 * plot.settings$cex,
          col = "blue",
          padj = -1.55,
        )


        ##TODO- CONTROL PLOT! ... can be implemented in appropriate form in a later version
        if (method.control.settings$trace) {
          par(new = TRUE)
          plot(
            RF_reg.limited[1:length(slide$squared_residuals),1],
            slide$squared_residuals,
            ylab = "",
            type = "l",
            xlab = "",
            xaxt = plot.settings$xaxt,
            axes = FALSE,
            xlim = xlim,
            log = "y"
          )


        }

      }

    }

  }#endif::plot
  ##=============================================================================#
  ## RETURN
  ##=============================================================================#

  ##catch up worst case scenarios ... means something went wrong
  if(!exists("De")){De  <- NA}
  if(!exists("De.error")){De.error  <- NA}
  if(!exists("De.MC")){De.MC  <- NA}
  if(!exists("De.lower")){De.lower  <- NA}
  if(!exists("De.upper")){De.upper  <- NA}
  if(!exists("De.status")){De.status  <- NA}
  if (!exists("fit")) {
  if (exists("fit.lambda")) {
      fit <- fit.lambda

    }else{
      fit  <- list()

    }
  }
  if(!exists("slide")){slide <- list()}

  ##combine values for De into a data frame
  De.values <- data.frame(
      DE = De,
      DE.ERROR = De.error,
      DE.LOWER = De.lower,
      DE.UPPER = De.upper,
      DE.STATUS = De.status,
      RF_NAT.LIM = paste(RF_nat.lim, collapse = ":"),
      RF_REG.LIM = paste(RF_reg.lim, collapse = ":"),
      POSITION =  as.integer(aliquot.position),
      DATE = aliquot.date,
      SEQUENCE_NAME = aliquot.sequence_name,
      UID = NA,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  ##generate unique identifier
  UID <- .create_UID()

    ##update data.frames accordingly
    De.values$UID <- UID

    if(!is.null(TP.data.frame)){
      TP.data.frame$UID <- UID

    }


  ##produce results object
    newRLumResults.analyse_IRSAR.RF <- set_RLum(
      class = "RLum.Results",
      data = list(
        De.values = De.values,
        De.MC = De.MC,
        test_parameters = TP.data.frame,
        fit = fit,
        slide = slide
      ),
      info = list(call = sys.call())
    )

  invisible(newRLumResults.analyse_IRSAR.RF)

}


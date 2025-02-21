#' @title Analyse IRSAR RF measurements
#'
#' @description Function to analyse IRSAR RF measurements on K-feldspar samples, performed
#' using the protocol according to Erfurt et al. (2003) and beyond.
#'
#' @details The function performs an IRSAR analysis described for K-feldspar samples by
#' Erfurt et al. (2003) assuming a negligible sensitivity change of the RF
#' signal.
#'
#' **General Sequence Structure** (according to Erfurt et al., 2003)
#'
#'
#' 1. Measuring IR-RF intensity of the natural dose for a few seconds (\eqn{RF_{nat}})
#' 2. Bleach the samples under solar conditions for at least 30 min without changing the geometry
#' 3. Waiting for at least one hour
#' 4. Regeneration of the IR-RF signal to at least the natural level (measuring (\eqn{RF_{reg}})
#' 5. Fitting data with a stretched exponential function
#' 6. Calculate the palaeodose \eqn{D_{e}} using the parameters from the fitting
#'
#'
#' Actually three methods are supported to obtain the \eqn{D_{e}}:
#' `method = "FIT"`, `method = "SLIDE"` and `method = "VSLIDE"`:
#'
#' **`method = "FIT"`**
#'
#' The principle is described above and follows the original suggestions by
#' Erfurt et al., 2003. For the fitting the mean count value of the `RF_nat` curve is used.
#'
#' Function used for the fitting (according to Erfurt et al. (2003)):
#'
#' \deqn{\phi(D) = \phi_{0}-\Delta\phi(1-exp(-\lambda*D))^\beta}
#'
#' with
#' \eqn{\phi(D)} the dose dependent IR-RF flux,
#' \eqn{\phi_{0}} the initial IR-RF flux,
#' \eqn{\Delta\phi} the dose dependent change of the IR-RF flux,
#' \eqn{\lambda} the exponential parameter, \eqn{D} the dose and
#' \eqn{\beta} the dispersive factor.
#'
#' To obtain the palaeodose
#' \eqn{D_{e}} the function is changed to:
#'
#' \deqn{D_{e} = ln(-(\phi(D) - \phi_{0})/(-\lambda*\phi)^{1/\beta}+1)/-\lambda}
#'
#' The fitting is done using the `port` algorithm of the [nls] function.
#'
#'
#' **`method = "SLIDE"`**
#'
#' For this method, the natural curve is slid along the x-axis until
#' congruence with the regenerated curve is reached. As opposed to fitting, this
#' allows working with the original data without the need for any physical
#' model. This approach was introduced for RF curves by Buylaert et al., 2012
#' and Lapp et al., 2012.
#'
#' Here the sliding is done by searching for the minimum of the sum of
#' squared residuals.
#' For the mathematical details of the implementation see Frouin et al., 2017
#'
#' **`method = "VSLIDE"`**
#'
#' Same as `"SLIDE"` but searching also vertically for the best match (i.e. in xy-direction.)
#' See Kreutzer et al. (2017) and Murari et al. (2021). By default the vertical sliding
#' range is set automatically, but can be set manually by changing the
#' `vslide_range` parameter (see `method_control`).
#'
#' **`method_control`**
#'
#' To keep the generic argument list as clear as possible, parameters to control
#' the methods for De estimation are preset with meaningful default values,
#' which can however be modified using the `method_control` argument, e.g.,
#' `method_control = list(trace = TRUE)`. Supported parameters are:
#'
#' \tabular{lll}{
#' **PARAMETER** \tab **METHOD** \tab **DESCRIPTION**\cr
#' `trace`   \tab `FIT`, `SLIDE` or `VSLIDE` \tab as in [nls]; shows sum of squared residuals\cr
#' `trace_vslide` \tab `SLIDE` or `VSLIDE` \tab [logical] argument to enable or disable the tracing of the vertical sliding\cr
#' `maxiter` \tab `FIT` \tab as in [nls]\cr
#' `warnOnly` \tab `FIT` \tab as in [nls]\cr
#' `minFactor` \tab `FIT` \tab as in [nls]\cr
#' `correct_onset` \tab `SLIDE` or `VSLIDE` \tab The logical argument shifts the curves along the x-axis by the first channel,
#' as light is expected in the first channel. The default value is `TRUE`.\cr
#' `show_density` \tab `SLIDE` or `VSLIDE` \tab [logical] (*with default*)
#' enable/disable KDE plots for MC run results. If the distribution is too narrow nothing is shown.\cr
#' `show_fit` \tab `SLIDE` or `VSLIDE` \tab [logical] (*with default*)
#' enable/disable the plot of the fitted curve routinely obtained during the evaluation.\cr
#' `n.MC` \tab `SLIDE` or `VSLIDE` \tab [integer] (*with default*):
#' This controls the number of MC runs within the sliding (assessing the possible minimum values).
#' The default `n.MC = 1000`. Note: This parameter is not the same as controlled by the
#' function argument `n.MC`. \cr
#' `vslide_range` \tab `SLIDE` or `VSLIDE` \tab [logical] or [numeric] or [character] (*with default*):
#' This argument sets the boundaries for a vertical curve
#' sliding. The argument expects a vector with an absolute minimum and a maximum (e.g., `c(-1000,1000)`).
#' Alternatively the values `NULL` and `'auto'` are allowed. The automatic mode detects the
#' reasonable vertical sliding range (**recommended**). `NULL` applies no vertical sliding.
#' The default is `NULL`.\cr
#' `num_slide_windows` \tab `SLIDE` or `VSLIDE` \tab [integer] (*with default*):
#' This argument controls how many differently-sized windows are tested when
#' sliding: the higher the value (up to a maximum of 10), the more time is
#' spent in searching the global optimum. The default is 3, which attempts to
#' strike a balance between quality of the fit and computation speed.\cr
#' `cores` \tab `SLIDE` or `VSLIDE` \tab `number` or `character` (*with default*): set number of cores to be allocated
#' for a parallel processing of the Monte-Carlo runs. The default value is `NULL` (single thread),
#' the recommended values is `'auto'`. An optional number (e.g., `cores` = 8) assigns a value manually.
#' }
#'
#'
#' **Error estimation**
#'
#' For **`method = "FIT"`** the asymmetric error range is obtained by using the 2.5 % (lower) and
#' the 97.5 % (upper) quantiles of the \eqn{RF_{nat}} curve for calculating the \eqn{D_{e}} error range.
#'
#' For **`method = "SLIDE"`** and **`method = "VSLIDE"`** the error is obtained
#' by bootstrapping the residuals of the slid
#' curve to construct new natural curves for a Monte Carlo simulation. The error is returned in two
#' ways: (a) the standard deviation of the \eqn{D_{e}} obtained from the MC
#' runs and (b) the confidence
#' interval using the  2.5 % (lower) and the 97.5 % (upper) quantiles. The results of the MC runs
#' are returned with the function output.
#'
#' **Test parameters**
#'
#' The argument `test_parameters` allows to pass some thresholds for several test parameters,
#' which will be evaluated during the function run. If a threshold is set and
#' it is exceeded, the
#' test parameter status will be set to `"FAILED"`. Intentionally this parameter is not termed
#' `'rejection criteria'` as not all test parameters are evaluated for both methods and some parameters
#' are calculated but not evaluated by default. Common for all parameters are the allowed argument options
#' `NA` and `NULL`. If the parameter is set to `NA` the value is calculated but the
#' result will not be evaluated, therefore it will have no effect on the
#' status (`"OK"` or `"FAILED"`) of the parameter.
#' Setting the parameter to `NULL` disables the parameter entirely and the parameter will be
#' also removed from the function output. This might be useful in cases where a particular parameter
#' requires a long computation time. Currently supported parameters are:
#'
#' `curves_ratio` [numeric] (default: `1.001`):
#'
#' The ratio of \eqn{RF_{nat}} over \eqn{RF_{reg}} in the range of\eqn{RF_{nat}} of is calculated
#' and should not exceed the threshold value.
#'
#' `intersection_ratio` [numeric] (default: `NA`):
#'
#' Calculated as absolute difference from 1 of the ratio of the integral of the normalised RF-curves,
#' This value indicates intersection of the RF-curves and should be close to 0 if the curves
#' have a similar shape. For this calculation first the corresponding time-count pair value on the RF_reg
#' curve is obtained using the maximum count value of the `RF_nat` curve and only this segment (fitting to
#' the `RF_nat` curve) on the RF_reg curve is taken for further calculating this ratio. If nothing is
#' found at all, `Inf` is returned.
#'
#' `residuals_slope` [numeric] (default: `NA`; only for `method = "SLIDE"`):
#'
#' A linear function is fitted on the residuals after sliding.
#' The corresponding slope can be used to discard values as a high (positive, negative) slope
#' may indicate that both curves are fundamentally different and the method cannot be applied at all.
#' Per default the value of this parameter is calculated but not evaluated.
#'
#' `curves_bounds` [numeric] (default: \eqn{max(RF_{reg_counts})}:
#'
#' This measure uses the maximum time (x) value of the regenerated curve.
#' The maximum time (x) value of the natural curve cannot be larger than this value. However, although
#' this is not recommended the value can be changed or disabled.
#'
#' `dynamic_ratio` [numeric] (default: `NA`):
#'
#' The dynamic ratio of the regenerated curve is calculated as ratio of the minimum and maximum count values.
#'
#' `lambda`, `beta` and `delta.phi`
#' [numeric] (default: `NA`; `method = "SLIDE"`):
#'
#' The stretched exponential function suggested by Erfurt et al. (2003) describing the decay of
#' the RF signal, comprises several parameters that might be useful to evaluate the shape of the curves.
#' For `method = "FIT"` this parameter is obtained during the fitting, for `method = "SLIDE"` a
#' rather rough estimation is made using the function [minpack.lm::nlsLM] and the equation
#' given above. Note: As this procedure requests more computation time, setting of one of these three parameters
#' to `NULL` also prevents a calculation of the remaining two.
#'
#'
#' @param object [RLum.Analysis-class] or a [list] of [RLum.Analysis-class]-objects (**required**):
#' input object containing data for protocol analysis. The function expects to
#' find at least two curves in the [RLum.Analysis-class] object: (1) `RF_nat`, (2) `RF_reg`.
#' If a `list` is provided as input all other parameters can be provided as
#' `list` as well to gain full control.
#'
#' @param sequence_structure [vector] [character] (*with default*):
#' specifies the general sequence structure. Allowed steps are `NATURAL` and
#' `REGENERATED`, and one of each must appear.
#' In addition any other character is allowed in the sequence structure;
#' such curves will be ignored during the analysis.
#'
#' @param RF_nat.lim [vector] (*with default*):
#' set minimum and maximum channel range for natural signal fitting and sliding.
#' If only one value is provided this will be treated as minimum value and the
#' maximum limit will be added automatically.
#'
#' @param RF_reg.lim [vector] (*with default*):
#' set minimum and maximum channel range for regenerated signal fitting and sliding.
#' If only one value is provided this will be treated as minimum value and the
#' maximum limit will be added automatically.
#'
#' @param method [character] (*with default*): select method applied for the
#' data analysis. Possible options are `"FIT"`, `"SLIDE"`, `"VSLIDE"`;
#' `"NONE"` can be used to disable the analysis and plot the natural points
#' at their original position.
#'
#' @param method_control [list] (*optional*):
#' parameters to control the method, that can be passed to the chosen method.
#' These are for (1) `method = "FIT"`: `'trace'`, `'maxiter'`, `'warnOnly'`, `'minFactor'` and for
#' (2) `method = "SLIDE"`: `'correct_onset'`, `'show_density'`,  `'show_fit'`, `'trace'`.
#' See details.
#'
#' @param test_parameters [list] (*with default*):
#' set test parameters. Supported parameters are: `curves_ratio`, `residuals_slope` (only for
#' `method = "SLIDE"`), `curves_bounds`, `dynamic_ratio`,
#' `lambda`, `beta` and `delta.phi`. All input: [numeric]
#' values, `NA` and `NULL` (see Details).
#'
#' (see Details for further information)
#'
#' @param n.MC [numeric] (*with default*):
#' set number of Monte Carlo runs for start parameter estimation (`method = "FIT"`) or
#' error estimation (`method = "SLIDE"`). This value can be set to `NULL` to skip the
#' MC runs. Note: Large values will significantly increase the computation time
#'
#' @param txtProgressBar [logical] (*with default*):
#' enable/disable the progress bar during MC runs.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param plot_reduced [logical] (*optional*):
#' provides a reduced plot output if enabled to allow common R plot combinations,
#' e.g., `par(mfrow(...))`. If `TRUE` no residual plot
#' is returned; it has no effect if `plot = FALSE`
#'
#' @param ... further arguments that will be passed to the plot output.
#' Currently supported arguments are `main`, `xlab`, `ylab`,
#' `xlim`, `ylim`, `log`, `legend` (`TRUE/FALSE`),
#' `legend.pos`, `legend.text` (passes argument to x,y in
#' [graphics::legend]), `xaxt`
#'
#'
#' @return
#' The function returns numerical output and an (*optional*) plot.
#'
#' -----------------------------------\cr
#' `[ NUMERICAL OUTPUT ]`\cr
#' -----------------------------------\cr
#'
#' **`RLum.Results`**-object
#'
#' **slot:** **`@data`**
#'
#' `[.. $data : data.frame]`
#'
#' \tabular{lll}{
#'  **Column** \tab **Type** \tab **Description**\cr
#'  `DE` \tab `numeric` \tab the obtained equivalent dose\cr
#'  `DE.ERROR` \tab `numeric` \tab (only `method = "SLIDE"`) standard deviation obtained from MC runs \cr
#'  `DE.LOWER` \tab `numeric`\tab 2.5% quantile for De values obtained by MC runs \cr
#'  `DE.UPPER` \tab `numeric`\tab 97.5% quantile for De values obtained by MC runs  \cr
#'  `DE.STATUS`  \tab `character`\tab test parameter status\cr
#'  `RF_NAT.LIM`  \tab `character`\tab used `RF_nat` curve limits \cr
#'  `RF_REG.LIM` \tab `character`\tab used `RF_reg` curve limits\cr
#'  `POSITION` \tab `integer`\tab (*optional*) position of the curves\cr
#'  `DATE` \tab `character`\tab (*optional*) measurement date\cr
#'  `SEQUENCE_NAME` \tab `character`\tab (*optional*) sequence name\cr
#'  `UID` \tab `character`\tab unique data set ID
#' }
#'
#' `[.. $De.MC : numeric]`
#'
#' A `numeric` vector with all the De values obtained by the MC runs.
#'
#' `[.. $test_parameters : data.frame]`
#'
#' \tabular{lll}{
#'  **Column** \tab **Type** \tab **Description**\cr
#'  `POSITION` \tab `numeric` \tab aliquot position \cr
#'  `PARAMETER` \tab `character` \tab test parameter name \cr
#'  `THRESHOLD` \tab `numeric` \tab set test parameter threshold value \cr
#'  `VALUE` \tab `numeric` \tab the calculated test parameter value (to be compared with the threshold)\cr
#'  `STATUS` \tab `character` \tab test parameter status either `"OK"` or `"FAILED"` \cr
#'  `SEQUENCE_NAME` \tab `character` \tab name of the sequence, so far available \cr
#'  `UID` \tab `character`\tab unique data set ID
#' }
#'
#' `[.. $fit : data.frame]`
#'
#' An [nls] object produced by the fitting.
#'
#' `[.. $slide : list]`
#'
#' A [list] with data produced during the sliding. Some elements are previously
#' reported with the summary object data. List elements are:
#'
#' \tabular{lll}{
#'  **Element** \tab **Type** \tab **Description**\cr
#'  `De` \tab `numeric` \tab the final De obtained with the sliding approach \cr
#'  `De.MC` \tab `numeric` \tab all De values obtained by the MC runs \cr
#'  `residuals` \tab `numeric` \tab the obtained residuals for each channel of the curve \cr
#'  `trend.fit` \tab `lm` \tab fitting results produced by the fitting of the residuals \cr
#'  `RF_nat.slid` \tab `matrix` \tab the slid `RF_nat` curve \cr
#'  `t_n.id` \tab `numeric` \tab the index of the t_n offset \cr
#'  `I_n` \tab `numeric` \tab the vertical intensity offset if a vertical slide was applied \cr
#'  `algorithm_error` \tab `numeric` \tab the vertical sliding suffers from a systematic effect induced by the used
#'  algorithm. The returned value is the standard deviation of all obtained De values while expanding the
#'  vertical sliding range. It can be added as systematic error to the final De error; so far wanted.\cr
#'  `vslide_range` \tab `numeric` \tab the range used for the vertical sliding \cr
#'  `num_slide_windows` \tab `integer` \tab the number of windows used for the vertical sliding \cr
#'  `squared_residuals` \tab `numeric` \tab the squared residuals (horizontal sliding)
#' }
#'
#'
#' **slot:** **`@info`**
#'
#' The original function call ([methods::language-class]-object)
#'
#' The output (`data`) should be accessed using the function [get_RLum].
#'
#' ------------------------\cr
#' `[ PLOT OUTPUT ]`\cr
#' ------------------------\cr
#'
#' The slid IR-RF curves with the finally obtained De
#'
#' @note
#' This function assumes that there is no sensitivity change during the
#' measurements (natural vs. regenerated signal), which is in contrast to the
#' findings by Buylaert et al. (2012).
#'
#' @section Function version: 0.7.10
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Analysis-class], [RLum.Results-class], [get_RLum],
#' [nls], [minpack.lm::nlsLM], `parallel::mclapply`
#'
#'
#' @references
#' Buylaert, J.P., Jain, M., Murray, A.S., Thomsen, K.J., Lapp, T.,
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
#' Frouin, M., Huot, S., Kreutzer, S., Lahaye, C., Lamothe, M., Philippe, A., Mercier, N., 2017.
#' An improved radiofluorescence single-aliquot regenerative dose protocol for K-feldspars.
#' Quaternary Geochronology 38, 13-24. doi:10.1016/j.quageo.2016.11.004
#'
#' Kreutzer, S., Murari, M.K., Frouin, M., Fuchs, M., Mercier, N., 2017.
#' Always remain suspicious: a case study on tracking down a technical artefact while measuring IR-RF.
#' Ancient TL 35, 20–30.
#'
#' Murari, M.K., Kreutzer, S., Fuchs, M., 2018. Further investigations on IR-RF:
#' Dose recovery and correction. Radiation Measurements 120, 110–119.
#' doi: 10.1016/j.radmeas.2018.04.017
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
#' ** Further reading**
#'
#' Murari, M.K., Kreutzer, S., King, G.E., Frouin, M., Tsukamoto, S., Schmidt, C., Lauer, T.,
#' Klasen, N., Richter, D., Friedrich, J., Mercier, N., Fuchs, M., 2021.
#' Infrared radiofluorescence (IR-RF) dating: A review. Quaternary Geochronology 64,
#' 101155. doi: 10.1016/j.quageo.2021.101155
#'
#' @keywords datagen
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
#' get_RLum(results, data.object = "data")
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
#'  method_control = list(trace = TRUE))
#' }
#'
#' @md
#' @export
analyse_IRSAR.RF<- function(
  object,
  sequence_structure = c("NATURAL", "REGENERATED"),
  RF_nat.lim = NULL,
  RF_reg.lim = NULL,
  method = "FIT",
  method_control = NULL,
  test_parameters = NULL,
  n.MC = 10,
  txtProgressBar = TRUE,
  plot = TRUE,
  plot_reduced = FALSE,
  ...
) {
  .set_function_name("analyse_IRSAR.RF")
  on.exit(.unset_function_name(), add = TRUE)

  ##TODO
  ## - if a file path is given, the function should try to find out whether an XSYG-file or
  ##   a BIN-file is provided
  ##  - add NEWS for vslide_range
  ##  - update documentary ... if it works as expected.

  # SELF CALL -----------------------------------------------------------------------------------
  if(is.list(object)){
    ## expand input arguments
    rep.length <- length(object)

    sequence_structure <- .listify(sequence_structure, rep.length)
    RF_nat.lim <- .listify(RF_nat.lim, rep.length)
    RF_reg.lim <- .listify(RF_reg.lim, rep.length)
    method <- .listify(method, rep.length)
    n.MC <- .listify(n.MC, rep.length)

    ##test_parameters
    if(is(test_parameters[[1]], "list")){
      test_parameters <- rep(test_parameters, rep.length)

    }else{
     test_parameters <- rep(list(test_parameters), rep.length)
    }

    ##main
    if("main"%in% names(list(...))){
      temp_main <- .listify(list(...)$main, rep.length)

    }else{
      originator <- if (length(object) > 0) object[[1]]@originator else NA
      if (!is.na(originator) && originator == "read_RF2R") {
        temp_main <- lapply(object, function(x) x@info$ROI)
      } else {
        temp_main <- as.list(paste0("ALQ #",1:length(object)))
      }
    }

    ##run analysis
    temp <- lapply(seq_along(object), function(x) {
      analyse_IRSAR.RF(
        object = object[[x]],
        sequence_structure = sequence_structure[[x]],
        RF_nat.lim = RF_nat.lim[[x]],
        RF_reg.lim = RF_reg.lim[[x]],
        method = method[[x]],
        method_control = method_control,
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


  ## Integrity checks -------------------------------------------------------

  .validate_class(object, "RLum.Analysis")
  .validate_class(sequence_structure, "character")
  .validate_not_empty(object)
  if (length(sequence_structure) < 2) {
    .throw_error("'sequence_structure' should contain at least two elements")
  }
  if (!all(c("NATURAL", "REGENERATED") %in% sequence_structure)) {
    .throw_error("'sequence_structure' must contain one each of 'NATURAL' ",
                 "and 'REGENERATED'")
  }
  if (!is.null(RF_nat.lim))
    .validate_class(RF_nat.lim, c("numeric", "integer"))
  if (!is.null(RF_reg.lim))
    .validate_class(RF_reg.lim, c("numeric", "integer"))
  method <- .validate_args(toupper(method), c("FIT", "SLIDE", "VSLIDE", "NONE"))
  .validate_positive_scalar(n.MC, int = TRUE, null.ok = TRUE)

  ##SELECT ONLY MEASURED CURVES
  ## (this is not really necessary but rather user friendly)
  if(!length(suppressWarnings(get_RLum(object, curveType= "measured"))) == 0){
    object <- get_RLum(object, curveType= "measured", drop = FALSE)
  }

  ##INVESTIGATE SEQUENCE OBJECT STRUCTURE

  ##grep object structure
  temp.sequence_structure <- structure_RLum(object)

  ##check whether both curve have the same length, in this case we cannot proceed (sliding
  ##is not allowed)
  if(length(unique(temp.sequence_structure[["x.max"]])) == 1 &&
     grepl("SLIDE", method) &&
     (is.null(RF_nat.lim) & is.null(RF_reg.lim))) {
    .throw_error("There is no further sliding space left. All curves have ",
                 "the same length and no limitation was set")
  }

  ##grep name of the sequence and the position this will be useful later on
  ##name
  record1 <- get_RLum(object, record.id = 1)
  aliquot.sequence_name <- suppressWarnings(get_RLum(record1,
                                                     info.object = "name"))
  if (is.null(aliquot.sequence_name)) {
    aliquot.sequence_name <- NA
  }

  ##position
  aliquot.position <- suppressWarnings(get_RLum(record1,
                                                info.object = "position"))
  if (is.null(aliquot.position)) {
    aliquot.position <- NA
  }

  ##date
  aliquot.date <- suppressWarnings(get_RLum(record1,
                                            info.object = "startDate"))
  if (!is.null(aliquot.date)) {
    ##transform so far the format can be identified
    if (nchar(aliquot.date) == 14) {
      aliquot.date <- paste(c(substr(aliquot.date, 1, 4),
                              substr(aliquot.date, 5, 6),
                              substr(aliquot.date, 7, 8)), collapse = "-")
    }
  }else{
    aliquot.date <- NA
  }

  ##set structure values
  temp.sequence_structure$protocol.step <-
    rep(sequence_structure, length_RLum(object))[1:length_RLum(object)]

  ## check that we both natural and regenerated are still there
  nat.idx <- which(temp.sequence_structure$protocol.step == "NATURAL")
  reg.idx <- which(temp.sequence_structure$protocol.step == "REGENERATED")
  if (length(nat.idx) == 0 || length(reg.idx) == 0) {
    .throw_error("'sequence_structure' is missing one of 'NATURAL' or ",
                 "'REGENERATED'")
  }

  ##check if the first curve is shorter than the first curve
  num.channels.nat <- temp.sequence_structure$n.channels[nat.idx]
  num.channels.reg <- temp.sequence_structure$n.channels[reg.idx]
  if (num.channels.nat > num.channels.reg) {
    .throw_error("Number of data channels in RF_nat > RF_reg is not supported")
  }

  ##===============================================================================================#
  ## SET CURVE LIMITS
  ##===============================================================================================#
  ##the setting here will be valid for all subsequent operations

  ##01
  ## get the allowed curve limits
  RF_nat.lim.default <- c(1, max(num.channels.nat))
  RF_reg.lim.default <- c(1, max(num.channels.reg))

  ## 02 - check boundaries
  ##RF_nat.lim
  if (is.null(RF_nat.lim) || anyNA(RF_nat.lim)) {
    RF_nat.lim <- RF_nat.lim.default

  }else {
    ##this allows to provide only one boundary and the 2nd will be added automatically
    if (length(RF_nat.lim) == 1) {
      RF_nat.lim <- c(RF_nat.lim, RF_nat.lim.default[2])
    }

    if (min(RF_nat.lim) < RF_nat.lim.default[1] |
        max(RF_nat.lim) > RF_nat.lim.default[2]) {
      RF_nat.lim <- RF_nat.lim.default
      .throw_warning("'RF_nat.lim' out of bounds, reset to c(",
                     paste(range(RF_nat.lim), collapse = ":"),")")
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
      .throw_warning("'RF_reg.lim' out of bounds, reset to c(",
                     paste(range(RF_reg.lim), collapse = ":"), ")")
    }
  }

  ## check if intervals make sense at all
  len.RF_reg.lim <- length(RF_reg.lim[1]:RF_reg.lim[2])
  if (len.RF_reg.lim < RF_nat.lim[2]) {
    RF_reg.lim[2] <- RF_reg.lim[2] + abs(len.RF_reg.lim - RF_nat.lim[2]) + 1

    ## check that now we haven't gone beyond the size of RF_reg.lim
    if (RF_reg.lim[2] > RF_reg.lim.default[2]) {
      .throw_error("'RF_reg.lim' defines too short an interval and it's not ",
                   "possible to extend it")
    }

    .throw_warning("'RF_reg.lim' defines too short an interval, reset to c(",
                   paste(range(RF_reg.lim), collapse=":"), ")")
  }

  ## check again that we can actually slide the curve
  if (diff(RF_nat.lim) == diff(RF_reg.lim)) {
    .throw_error("No sliding space left after limitations were applied")
  }

  # Method Control Settings ---------------------------------------------------------------------
  ##===============================================================================================#
  ## SET METHOD CONTROL PARAMETER - FOR BOTH METHODS
  ##===============================================================================================#
  ##
  ##set supported values with default
  method_control.settings <- list(
    trace = FALSE,
    trace_vslide = FALSE,
    maxiter = 500,
    warnOnly = FALSE,
    minFactor = 1 / 4096,
    correct_onset = TRUE,
    show_density = TRUE,
    show_fit = FALSE,
    n.MC = if(is.null(n.MC)) NULL else 1000,
    vslide_range = if(method[1] == "VSLIDE") "auto" else NULL,
    num_slide_windows = 3,
    cores = NULL
  )

  ## deprecated argument
  if ("method.control" %in% names(list(...))) {
    method_control <- list(...)$method.control
    .throw_warning("'method.control' is deprecated, use ",
                   "'method_control' instead")
  }

  ##modify list if necessary
  if (!is.null(method_control)) {
    .validate_class(method_control, "list")

    ##check whether this arguments are supported at all
    unsupported.idx <- which(!names(method_control) %in%
                             names(method_control.settings))
    if (length(unsupported.idx) > 0) {
      .throw_warning("'",  paste(names(method_control)[unsupported.idx],
                                 collapse = ", "),
                     "' not supported for 'method_control'. Supported arguments are: ",
                     .collapse(names(method_control.settings)))
    }

    ## check for odd user input
    temp.vslide_range <- method_control$vslide_range
    if (length(temp.vslide_range) > 0) {
      .validate_class(temp.vslide_range, c("character", "integer", "numeric"),
                      name = "'vslide_range' in 'method_control'")
    }
    if (is.character(temp.vslide_range) && temp.vslide_range[1] != "auto") {
      .throw_error("'vslide_range' in 'method_control' should be either ",
                   "'auto' or a 2-element numeric vector")
    }
    if (length(temp.vslide_range) > 2) {
      method_control$vslide_range <- method_control$vslide_range[1:2]
      .throw_warning("'vslide_range' in 'method_control' has more ",
                     "than 2 elements, only the first two were used")
    }

    temp.num_slide_windows <- method_control$num_slide_windows
    if (length(temp.num_slide_windows) > 0) {
      .validate_positive_scalar(temp.num_slide_windows, int = TRUE,
                                name = "'num_slide_windows' in 'method_control'")
      if (temp.num_slide_windows > 10) {
        method_control$num_slide_windows <- 10
        .throw_warning("'num_slide_windows' in 'method_control' should be ",
                       "between 1 and 10, reset to 10 ")
      }
    }

    ##modify list
    method_control.settings <- modifyList(
      x = method_control.settings,
      val = method_control,
      keep.null = TRUE)
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
    if (grepl("SLIDE", method) &&
        method_control.settings$correct_onset == TRUE) {
      RF_reg[,1] <- RF_reg[,1] - RF_reg[1,1]
    }

  RF_reg.x <- RF_reg[RF_reg.lim[1]:RF_reg.lim[2],1]
  RF_reg.y <- RF_reg[RF_reg.lim[1]:RF_reg.lim[2],2]

  ##grep values from natural signal
  RF_nat <- as.data.frame(object@records[[
    temp.sequence_structure[temp.sequence_structure$protocol.step=="NATURAL","id"]]]@data)

  ## correct the onset of detection by using the first time value
  if (grepl("SLIDE", method) &&
      method_control.settings$correct_onset == TRUE) {
    RF_nat[,1] <- RF_nat[,1] - RF_nat[1,1]
  }

  ##limit values to fit range (at least to the minimum)
  RF_nat.limited<- RF_nat[min(RF_nat.lim):max(RF_nat.lim),]

  ##calculate some useful parameters
  RF_nat.mean <- mean(RF_nat.limited[,2])
  RF_nat.sd <- sd(RF_nat.limited[,2])

  RF_nat.error.lower <- quantile(RF_nat.limited[,2], 0.975, na.rm = TRUE)
  RF_nat.error.upper <- quantile(RF_nat.limited[,2], 0.025, na.rm = TRUE)


  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  ##METHOD FIT
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    ## REGENERATED SIGNAL
    # set function for fitting ------------------------------------------------
    fit.function <- y ~ phi.0 - (delta.phi * ((1 - exp(
        -lambda * x
      )) ^ beta))

    ## stretched exponential function according to Erfurt et al. (2003)
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
    lambda.MC <- seq(0.0001, 0.001, by=(0.001-0.0001)/n.MC)
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
        stats::nls.control(
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
    if (length(stats::na.omit(fit.parameters.results.MC.results)) != 0) {

      ##choose median as final fit version
      fit.parameters.results.MC.results <- sapply(stats::na.omit(fit.parameters.results.MC.results), median)

      ##try final fitting
      fit <- try(nls(
        fit.function,
        trace = method_control.settings$trace,
        data = data.frame(x = RF_reg.x, y = RF_reg.y),
        algorithm = "port",
        start = list(
          phi.0 = fit.parameters.results.MC.results["phi.0"],
          delta.phi = fit.parameters.results.MC.results["delta.phi"],
          lambda = fit.parameters.results.MC.results["lambda"],
          beta = fit.parameters.results.MC.results["beta"]
        ),
        stats::nls.control(
          maxiter = method_control.settings$maxiter,
          warnOnly = method_control.settings$warnOnly,
          minFactor = method_control.settings$minFactor
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
    fit.parameters.results <- NA
    if (!inherits(fit,"try-error")) {
      fit.parameters.results <- coef(fit)
    }

    ##calculate De value
    De <- NA
    De.error <- NA
    De.lower <- NA
    De.upper <- NA
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
    }
  }

  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  ##METHOD SLIDE - ANALYSIS
  ##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  else if(method == "SLIDE" || method == "VSLIDE"){
    ##convert to matrix (in fact above the matrix data were first transferred to
    ##data.frames ... here
    ##we correct this ... again)
    RF_nat.limited <- as.matrix(RF_nat.limited)
    RF_reg.limited <- matrix(c(RF_reg.x, RF_reg.y), ncol = 2)
    RF_nat <- as.matrix(RF_nat)

    ##DEFINE FUNCTION FOR SLIDING
    ##FIND MINIMUM - this is done in a function so that it can be further used for MC simulations
    # sliding() -----------------------------------------------------------------------------------
    sliding <- function(RF_nat,
                        RF_nat.limited,
                        RF_reg.limited,
                        n.MC = method_control.settings$n.MC,
                        vslide_range = method_control.settings$vslide_range,
                        num_slide_windows = method_control.settings$num_slide_windows,
                        trace = method_control.settings$trace_vslide,
                        numerical.only = FALSE){

      ##(0) set objects ... nomenclature as used in Frouin et al., please note that here the index
      ##is used instead the real time values
      t_max.id <- nrow(RF_reg.limited)
      t_max_nat.id <- nrow(RF_nat.limited)
      t_min.id <- 1
      t_min <- RF_nat.limited[1,1]

      ##(1) calculate sum of residual squares using internal Rcpp function

      ##initialise slide range for specific conditions, namely NULL and "auto"
      if (is.null(vslide_range)) {
        vslide_range <- 0

      } else if (vslide_range[1] == "auto") {
        range <- max(RF_reg.limited[, 2]) - min(RF_reg.limited[, 2])
        vslide_range <- -range:range
        algorithm_error <- NA

      } else{
        vslide_range <- vslide_range[1]:vslide_range[2]
        algorithm_error <- NULL
      }

      ##problem: the optimisation routine slightly depends on the chosen input sliding vector
      ##and it might get trapped in a local minimum
      ##therefore we run the algorithm by expanding the sliding vector
      if(!is.null(vslide_range) && any(vslide_range != 0)){

        ##construct list of vector ranges we want to check for, this should avoid that we
        ##got trapped in a local minimum
        mid.idx <- floor((length(vslide_range) + 1) / 2)
        steps <- as.integer(seq(min(t_max_nat.id / 2, mid.idx / 16), mid.idx,
                                length.out = num_slide_windows))
        vslide_range.list <- lapply(steps, function(x) mid.idx + c(-x, x))

        ##correct for out of bounds problem; it might occur
        vslide_range.list[[num_slide_windows]] <- c(0, length(vslide_range))

        ##TODO ... this is not really optimal, but ok for the moment, better would be
        ##the algorithm finds sufficiently the global minimum.
        ##now run it in a loop and expand the range from the inner to the outer part
        ##at least this is considered for the final error range ...
        temp_minimum_list <- lapply(1:num_slide_windows, function(x) {
          src_analyse_IRSARRF_SRS(
            values_regenerated_limited =  RF_reg.limited[,2],
            values_natural_limited = RF_nat.limited[,2],
            vslide_range = vslide_range[vslide_range.list[[x]][1]:vslide_range.list[[x]][2]],
            n_MC = 0, #we don't need MC runs here, so make it quick
            trace = trace)[c("sliding_vector_min_index","vslide_minimum", "vslide_index")]
        })

        ##get all horizontal index value for the local minimum (corresponding to the vslide)
        temp_hslide_indices <- vapply(temp_minimum_list, function(x) {
          x$sliding_vector_min_index}, FUN.VALUE = numeric(length = 1))

        ##get also the vertical slide indices
        temp_vslide_indices <- vapply(temp_minimum_list, function(x) {
          x$vslide_index}, FUN.VALUE = numeric(length = 1))

        ##get all the minimum values
        temp_minimum <- vapply(temp_minimum_list, function(x) {
          x$vslide_minimum}, FUN.VALUE = numeric(length = 1))

        ##get minimum and set it to the final range
        opt.range <- vslide_range.list[[which.min(temp_minimum)]]
        vslide_range <- vslide_range[opt.range[1]:opt.range[2]]

        ##get all possible t_n values for the range expansion ... this can be considered
        ##as somehow systematic uncertainty, but it will be only calculated if the full range
        ##is considered, otherwise it is too biased by the user's choice
        ##ToDo: So far the algorithm error is not sufficiently documented
        if(!is.null(algorithm_error)){
          algorithm_error <- sd(vapply(1:length(temp_vslide_indices), function(k){
            temp.sliding.step <- RF_reg.limited[temp_hslide_indices[k]] - t_min
            ## return the offset of the t_n values
            RF_nat[1, 1] + temp.sliding.step
          }, FUN.VALUE = numeric(length = 1)))

        }else{
         algorithm_error <- NA
        }

      }else{
        algorithm_error <- NA
      }

      ##now run the final sliding with the identified range that corresponds to the minimum value
      temp.sum.residuals <-
        src_analyse_IRSARRF_SRS(
          values_regenerated_limited =  RF_reg.limited[,2],
          values_natural_limited = RF_nat.limited[,2],
          vslide_range = vslide_range,
          n_MC = if(is.null(n.MC)) 0 else n.MC,
          trace = trace
      )

      #(2) get minimum value (index and time value)
      index_min <- which.min(temp.sum.residuals$sliding_vector)
      if(length(index_min) == 0) t_n.id <- 1 else t_n.id <- index_min

      I_n <- 0
      if (!is.null(vslide_range)) {
        I_n <- vslide_range[temp.sum.residuals$vslide_index]
      }

      temp.sliding.step <- RF_reg.limited[t_n.id] - t_min

      ##(3) slide curve graphically ... full data set we need this for the plotting later
      RF_nat.slid <- matrix(data = c(RF_nat[,1] + temp.sliding.step, RF_nat[,2] + I_n), ncol = 2)
      t_n <- RF_nat.slid[1,1]

      ##the same for the MC runs of the minimum values
      if(!is.null(n.MC)) {
        t_n.MC <-
          vapply(
            X = 1:length(temp.sum.residuals$sliding_vector_min_MC),
            FUN = function(x) {
              ##get minimum for MC
              t_n.id.MC <- which(temp.sum.residuals$sliding_vector ==
                                 temp.sum.residuals$sliding_vector_min_MC[x])

              ## there is a non-zero chance that we have got two indices for
              ## the minimun, so we take the mean
              temp.sliding.step.MC <- RF_reg.limited[t_n.id.MC] - t_min
              t_n.MC <- (RF_nat[, 1] + mean(temp.sliding.step.MC))[1]

              return(t_n.MC)
            },
            FUN.VALUE = vector(mode = "numeric", length = 1)
          )

      } else{
        t_n.MC <- NA_integer_
      }

      ##(4) get residuals (needed to be plotted later)
      ## they cannot be longer than the RF_reg.limited curve
      reg.limited.idx <- t_n.id:nrow(RF_reg.limited)
      len.shorter <- min(nrow(RF_nat.limited), length(reg.limited.idx))
      residuals <- (RF_nat.limited[1:len.shorter, 2] + I_n) -
        RF_reg.limited[reg.limited.idx[1:len.shorter], 2]

      ##(4.1) calculate De from the first channel ... which is t_n here
      De <- round(t_n, digits = 2)
      De.MC <- round(t_n.MC, digits = 2)

      ##(5) calculate trend fit
      max.rows <- min(nrow(RF_nat.limited), length(residuals))
      temp.fit <- stats::lm(y ~ x, data.frame(x = RF_nat.limited[1:max.rows, 1],
                                              y = residuals))
      temp.trend.fit <- coef(temp.fit)

      ##return values and limited if they are not needed
      if (numerical.only == FALSE) {
        return(
          list(
            De = De,
            De.MC = De.MC,
            residuals = residuals,
            trend.fit = temp.trend.fit,
            RF_nat.slid = RF_nat.slid,
            t_n.id = t_n.id,
            I_n = I_n,
            algorithm_error = algorithm_error,
            vslide_range = if(is.null(vslide_range)){NA}else{range(vslide_range)},
            num_slide_windows = num_slide_windows,
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
    RF_nat.slid <-  slide$RF_nat.slid
    I_n <- slide$I_n

    # ERROR ESTIMATION
    # MC runs for error calculation ---------------------------------------------------------------

    ##set residual matrix for MC runs, i.e. set up list of pseudo RF_nat curves as function
    ##(i.e., bootstrap from the natural curve distribution)

    De.diff <- De.error <- De.lower <- De.upper <- De.MC <- NA_integer_
    if(!is.null(n.MC)){
      slide.MC.list <- lapply(1:n.MC,function(x) {

        reg.limited.idx <- slide$t_n.id:nrow(RF_reg.limited)
        len.shorter <- min(nrow(RF_nat.limited), length(reg.limited.idx))
        cbind(RF_nat.limited[1:len.shorter, 1],
              RF_reg.limited[reg.limited.idx[1:len.shorter], 2] +
              sample(residuals, len.shorter, replace = TRUE))
      })

      ##set parallel calculation if wanted
      if (is.null(method_control.settings$cores)) {
        cores <- 1

      } else {
        available.cores <- parallel::detectCores()
        requested.cores <- method_control.settings$cores[1]

        ##case 'auto'
        if (requested.cores == "auto") {
          cores <- max(available.cores - 2, 1) # nocov

        } else if (is.numeric(requested.cores)) {
          .validate_positive_scalar(requested.cores, int = TRUE,
                                    name = "method_control.settings$cores")
          if (requested.cores > available.cores) {
            ##assign all they have, it is not our problem
            # nocov start
            .throw_warning("Number of cores limited to the maximum ",
                           "available (", available.cores, ")")
            # nocov end
          }
          cores <- min(requested.cores, available.cores)

        }else{
          .throw_message("Invalid value for control argument 'cores', ",
                         "value set to 1")
          cores <- 1
        }

        ##return message
        message("[analyse_IRSAR.RF()] Using ", cores,
                ifelse(cores == 1, " core", " cores"), " ...")
      }

      ## SINGLE CORE -----
      if (cores[1] == 1) {
        if(txtProgressBar){
          ##progress bar
          cat("\n\t Run Monte Carlo loops for error estimation\n")
          pb <- txtProgressBar(min = 0, max = n.MC, initial = 0, char = "=", style = 3)
        }

        De.MC <- sapply(1:n.MC, function(i) {
          # update progress bar
          if (txtProgressBar) setTxtProgressBar(pb, i)

          sliding(
            RF_nat = RF_nat,
            RF_reg.limited = RF_reg.limited,
            RF_nat.limited = slide.MC.list[[i]],
            numerical.only = TRUE
          )[[2]]
        })

        ## close progress bar
        if (txtProgressBar) close(pb)

      ## MULTICORE -----
      } else {
        ## Create the determined number of R copies
        cl <- parallel::makeCluster(cores)

        ##run MC runs
        De.MC <- parallel::parSapply(cl, X = slide.MC.list,
                                     FUN = function(x){
                                       sliding(
                                         RF_nat = RF_nat,
                                         RF_reg.limited = RF_reg.limited,
                                         RF_nat.limited = x,
                                         numerical.only = TRUE
                                       )[[2]]
                                     })
        ##destroy multicore cluster
        parallel::stopCluster(cl)
      }
      ##calculate absolute deviation between De and the here newly calculated De.MC
      ##this is, e.g. ^t_n.1* - ^t_n in Frouin et al.
      De.diff <- diff(x = c(De, De.MC))
      De.error <- round(sd(De.MC), digits = 2)
      De.lower <- De - quantile(De.diff, 0.975, na.rm = TRUE)
      De.upper <- De - quantile(De.diff, 0.025, na.rm = TRUE)
    }
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
    TP <- .rm_NULL_elements(TP)

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
          ifelse(TP$curves_ratio$VALUE > TP$curves_ratio$THRESHOLD, "FAILED", "OK")
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
          ifelse(TP$intersection_ratio$VALUE > TP$intersection_ratio$THRESHOLD, "FAILED", "OK")
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
            TP$residuals_slope$VALUE > TP$residuals_slope$THRESHOLD, "FAILED", "OK")
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
        TP$dynamic_ratio$VALUE < TP$dynamic_ratio$THRESHOLD , "FAILED", "OK")
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
      ## add one channel on the top to make sure that it works
      TP$curves_bounds$VALUE <- max(RF_nat.slid[RF_nat.lim,1]) + (RF_nat[2,1] - RF_nat[1,1])

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
  De.status <- "OK"
  TP.data.frame <- NULL
  if (length(TP) != 0) {
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
      }
  }

  ##===============================================================================================#
  # Plotting ------------------------------------------------------------------------------------
  ##===============================================================================================#
  if (plot) {

    ##get internal colour definition
    col <- get("col", pos = .LuminescenceEnv)

    if (!plot_reduced) {

      ##grep par default and define reset
      def.par <- par(no.readonly = TRUE)
      on.exit(par(def.par), add = TRUE)

      ##set plot frame, if a method was chosen
      if (any(method %in% c("SLIDE", "FIT", "VSLIDE"))) {
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
        on.exit(par(def.par), add = TRUE)

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
      xlab = ifelse((!any(method %in% c("SLIDE", "FIT", "VSLIDE"))) | plot_reduced, plot.settings$xlab," "),
      xaxt = ifelse((!any(method %in% c("SLIDE", "FIT", "VSLIDE"))) | plot_reduced, plot.settings$xaxt,"n"),
      yaxt = "n",
      ylab = plot.settings$ylab,
      main = plot.settings$main,
      log = plot.settings$log,
    )

    if(De.status == "FAILED"){

      ##build list of failed TP
      mtext.message <- paste0(
        "Threshold exceeded for: ",
        .collapse(TP.data.frame$PARAMETER[TP.data.frame$STATUS == "FAILED"]),
                  ". For details see manual.")

      ##print mtext
      mtext(text = mtext.message,
            side = 3, outer = TRUE, col = "red",
            cex = 0.8 * par()[["cex"]])
      .throw_warning(mtext.message)
    }

    ##use scientific format for y-axis
    labels <- axis(2, labels = FALSE)
    axis(side = 2, at = labels, labels = format(labels, scientific = TRUE))

    ##(1) plot points that have been not selected
    points(RF_reg[-(min(RF_reg.lim):max(RF_reg.lim)),1:2], pch=3, col=col[19])

    ##(2) plot points that has been used for the fitting
    points(RF_reg.x,RF_reg.y, pch=3, col=col[10])

    ##show natural points if no analysis was done
    if(!any(method %in% c("SLIDE", "FIT", "VSLIDE"))){
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
    if (method_control.settings$show_fit) {

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
        .throw_warning("No fit possible, no fit shown.")
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
      mtext.txt <-  substitute(D[e] == De,
                               list(De = paste0(De, " [", De.lower,
                                                " ; ", De.upper,"]")))
      if(is.na(De) != TRUE & (is.nan(De) == TRUE |
                              De > max(RF_reg.x) |
                              De.upper > max(RF_reg.x))){

        try(mtext(side=3, mtext.txt,
                  line=0, cex=0.8 * par()[["cex"]], col="red"), silent=TRUE)
        De.status <- "VALUE OUT OF BOUNDS"

      } else{

        if ("mtext" %in% names(list(...))) {
          mtext(side = 3, list(...)$mtext)
        }else{
          try(mtext(side = 3, mtext.txt,
                    line = 0, cex = 0.7 * par()[["cex"]]), silent = TRUE)
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
          plot(NA, NA,
            xlim = c(0, max(temp.sequence_structure$x.max)),
            ylab = "E",
            xlab = plot.settings$xlab,
            xaxt = plot.settings$xaxt,
            ylim = c(-1, 1)
          )
          text(x = max(temp.sequence_structure$x.max) / 2,
               y = 0, "Fitting Error!")
        }
      }
    }

    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    ## PLOT - METHOD SLIDE
    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    else if(method == "SLIDE" || method == "VSLIDE"){
      ##(0) density plot
      if (method_control.settings$show_density) {
        ##showing the density makes only sense when we see at least 10 data points
        if (!anyNA(De.MC) && length(unique(De.MC)) >= 15) {

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
          .throw_warning("Narrow density distribution, ",
                         "no density distribution plotted")
        }
      }

      ##(1) plot unused points in grey ... unused points are points outside of the set limit
      points(
        matrix(RF_nat.slid[-(min(RF_nat.lim):max(RF_nat.lim)),1:2], ncol = 2),
        pch = 21, col = col[19]
      )

      ##(2) add used points
      points(RF_nat.slid[min(RF_nat.lim):max(RF_nat.lim),], pch = 21, col = col[2],
             bg = col[2])

      ##(3) add line to show the connection between the first point and the De
      lines(x = c(RF_nat.slid[1,1], RF_nat.slid[1,1]),
            y = c(.Machine$double.xmin,RF_nat.slid[1,2]),
            lty = 2,
            col = col[2]
      )

      ##(4) add arrow at the lowest y-coordinate possible to show the sliding
      if (plot.settings$log != "y" & plot.settings$log != "xy") {
        shape::Arrows(
          x0 = 0,
          y0 = ylim[1],
          y1 = ylim[1],
          x1 = RF_nat.slid[1,1],
          arr.type = "triangle",
          arr.length = 0.3 * par()[["cex"]],
          code = 2,
          col = col[2],
          arr.adj = 1,
          arr.lwd = 1
        )
      }

      ##(5) add vertical shift as arrow; show nothing if nothing was shifted
      if (plot.settings$log != "y" & plot.settings$log != "xy" & I_n != 0) {
        shape::Arrows(
          x0 = (0 + par()$usr[1])/2,
          y0 = RF_nat[1,2],
          y1 = RF_nat[1,2] + I_n,
          x1 = (0 + par()$usr[1])/2,
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
        col.ramp <- grDevices::colorRampPalette(c(col[19], "white", col[19]))
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
          angle <- 270
          y0 <- max(residuals)
        } else if (max(residuals) > 0 & min(residuals) > 0) {
          angle <- 90
          y0 <- min(residuals)
        } else {
          angle <- NA
          points(xlim[2], 0, pch = 3)
        }
        if (!is.na(angle)) {
          shape::Arrowhead(
            x0 = xlim[2] + (par("usr")[2] - xlim[2]) / 2,
            y0 = y0,
            angle = angle,
            lcol = col[2],
            arr.length = 0.4,
            arr.type = "triangle",
            arr.col = col[2]
          )
        }

        ##add residual points
        temp.points.diff <- max(length(min(RF_nat.lim):max(RF_nat.lim)) -
                                length(residuals), 0)
        points(RF_nat.slid[c(min(RF_nat.lim):(max(RF_nat.lim) - temp.points.diff)), 1],
                 residuals,
                 pch = 20,
                 col = rgb(0, 0, 0, 0.4))

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
        if (method_control.settings$trace) {
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
            ylim = ylim,
            log = "y"
          )
        }
      }
    }

  }#endif::plot

  # Return --------------------------------------------------------------------------------------
  ##catch up worst case scenarios ... means something went wrong
  if(!exists("De")){De  <- NA}
  if(!exists("De.error")){De.error  <- NA}
  if(!exists("De.MC")){De.MC  <- NA}
  if(!exists("De.lower")){De.lower  <- NA}
  if(!exists("De.upper")){De.upper  <- NA}
  if(!exists("De.status")){De.status  <- NA}
  if (!exists("fit")) {
    fit  <- list()
    if (exists("fit.lambda")) {
      fit <- fit.lambda
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
  UID <- create_UID()

    ##update data.frames accordingly
    De.values$UID <- UID

    if(!is.null(TP.data.frame)){
      TP.data.frame$UID <- UID
    }

  ##produce results object
    newRLumResults.analyse_IRSAR.RF <- set_RLum(
      class = "RLum.Results",
      data = list(
        data = De.values,
        De.MC = De.MC,
        test_parameters = TP.data.frame,
        fit = fit,
        slide = slide
      ),
      info = list(call = sys.call())
    )

  invisible(newRLumResults.analyse_IRSAR.RF)
}

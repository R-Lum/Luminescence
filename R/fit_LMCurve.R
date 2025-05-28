#' @title Non-linear Least Squares Fit for LM-OSL curves
#'
#' @description
#' The function determines weighted non-linear least-squares estimates of the
#' component parameters of an LM-OSL curve (Bulur 1996) for a given number of
#' components and returns various component parameters. The fitting procedure
#' uses the Levenberg-Marquardt algorithm as implemented in function `nlsLM`
#' from package `minpack.lm`.
#'
#' @details
#' **Fitting function**
#'
#' The function for the fitting has the general
#' form:
#'
#' \deqn{y = (exp(0.5)*Im_1*x/xm_1)*exp(-x^2/(2*xm_1^2)) + ,\ldots, + exp(0.5)*Im_i*x/xm_i)*exp(-x^2/(2*xm_i^2))}
#'
#' where \eqn{1 < i < 8}
#'
#' This function and the equations for the conversion to b (detrapping probability)
#' and n0 (proportional to initially trapped charge) have been taken from Kitis
#' et al. (2008):
#'
#' \deqn{xm_i=\sqrt{max(t)/b_i}}
#' \deqn{Im_i=exp(-0.5)n0/xm_i}
#'
#' **Background subtraction**
#'
#' When a background signal is provided with the `values.bg` argument, the
#' user can choose among three methods for background subtraction by setting
#' the `bg.subtraction` argument to one of these:
#'
#' - `"polynomial"` (default): a polynomial function is fitted using [glm]
#' and the resulting function is used for background subtraction:
#' \deqn{y = a*x^4 + b*x^3 + c*x^2 + d*x + e}
#'
#' - `"linear"`: a linear function is fitted using [glm] and the resulting
#' function is used for background subtraction:
#' \deqn{y = a*x + b}
#'
#' - `"channel"`: the measured background signal is subtracted channel-wise
#' from the measured signal.
#'
#' - `"none"`: this disables background subtraction even if `values.bg` is
#' provided.
#'
#' **Start values**
#'
#' The choice of the initial parameters for the `nls`-fitting is a crucial
#' point and the fitting procedure may mainly fail due to ill chosen start
#' parameters. Here, three options are provided:
#'
#' **(a)**
#' If `start_values` is not provided by the user, a cheap guess is made
#' by using the detrapping values found by Jain et al. (2003) for quartz for a
#' maximum of 7 components. Based on these values, the pseudo start parameters
#' `xm` and `Im` are recalculated for the given data set. In all cases, fitting
#' starts with the ultra-fast component and (depending on `n.components`)
#' steps through the following values. If no fit could be achieved, an error
#' plot (for `plot = TRUE`) with the pseudo curve (based on the
#' pseudo start parameters) is provided. This may give the opportunity to
#' identify appropriate start parameters visually.
#'
#' **(b)**
#' If start values are provided, the function works like a simple [nls]
#' fitting approach.
#'
#' **Goodness of fit**
#'
#' The goodness of the fit is given by a pseudo-R² value (pseudo coefficient of
#' determination). According to Lave (1970), the value is calculated as:
#'
#' \deqn{pseudoR^2 = 1 - RSS/TSS}
#'
#' where \eqn{RSS = Residual~Sum~of~Squares}
#' and \eqn{TSS = Total~Sum~of~Squares}
#'
#' **Error of fitted component parameters**
#'
#' The 1-sigma error for the components is calculated using
#' the function [stats::confint]. Due to considerable calculation time, this
#' option is disabled by default. In addition, the error for the components
#' can be estimated by using internal R functions like [summary]. See the
#' [nls] help page for more information.
#'
#' *For more details on the nonlinear regression in R, see Ritz & Streibig (2008).*
#'
#' @param values [RLum.Data.Curve-class] or [data.frame] (**required**):
#' x,y data of measured values (time and counts).
#'
#' @param values.bg [RLum.Data.Curve-class] or [data.frame] (*optional*):
#' x,y data of measured values (time and counts) for background subtraction.
#'
#' @param n.components [integer] (*with default*):
#' fixed number of components that are to be recognised during fitting
#' (min = 1, max = 7).
#'
#' @param start_values [data.frame] (*optional*):
#' start parameters for `lm` and `xm` data for the fit. If no start values are given,
#' an automatic start value estimation is attempted (see details).
#'
#' @param input.dataType [character] (*with default*):
#' alter the plot output depending on the input data: `"LM"` or `"pLM"` (pseudo-LM).
#' See: [convert_CW2pLM]
#'
#' @param sample_code [character] (*optional*):
#' sample code used for the plot and the optional output table (mtext).
#'
#' @param sample_ID [character] (*optional*):
#' additional identifier used as column header for the table output.
#'
#' @param LED.power [numeric] (*with default*):
#' LED power (max.) used for intensity ramping in mW/cm².
#' **Note:** This value is used for the calculation of the absolute
#' photoionisation cross section.
#'
#' @param LED.wavelength [numeric] (*with default*):
#' LED wavelength in nm used for stimulation.
#' **Note:** This value is used for the calculation of the absolute
#' photoionisation cross section.
#'
#' @param fit.trace [logical] (*with default*):
#' traces the fitting process on the terminal.
#'
#' @param fit.calcError [logical] (*with default*):
#' calculate 1-sigma error range of components using [stats::confint].
#'
#' @param bg.subtraction [character] (*with default*):
#' specifies method for background subtraction (one of `"polynomial"`,
#' `"linear"`, `"channel"`, or `"none"`, see Details). Only considered if
#' `values.bg` is specified.
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param plot.BG [logical] (*with default*):
#' enable/disable a plot of the background values with the fit used for the
#' background subtraction.
#'
#' @param plot.residuals [logical] (*with default*):
#' enable/disable the plot of the residuals.
#'
#' @param plot.contribution [logical] (*with default*):
#' enable/disable the plot of the component contribution.
#'
#' @param legend [logical] (*with default*):
#' enable/disable the plot legend.
#'
#' @param legend.pos [character] (*with default*):
#' keyword specifying the position of the legend.
#'
#' @param method_control [list] (*optional*): options to control the output
#' produced. Currently only the 'export.comp.contrib.matrix' (logical) option
#' is supported, to enable/disable export of the component contribution
#' matrix.
#'
#' @param ... Further arguments that may be passed to the plot output, e.g.
#' `main`, `xlab`, `xlab`, `xlim`, `ylim`, `cex`, `log`.
#'
#' @return
#' Various types of plots are returned. For details see above. Furthermore an
#' `RLum.Results` object is returned with the following structure:
#'
#' **`@data:`**
#'
#' `.. $data` : [data.frame] with fitting results\cr
#' `.. $fit` : nls ([nls] object)\cr
#' `.. $component_matrix` : [matrix] with numerical xy-values of the single fitted components with the resolution of the input data
#' `.. $component.contribution.matrix` : [list] component distribution matrix
#'  (produced only if `method_control$export.comp.contrib.matrix = TRUE`)
#'
#' **`info:`**
#'
#' `.. $call` : [call] the original function call
#'
#' Matrix structure for the distribution matrix:
#'
#' Column 1 and 2: time and `rev(time)` values\cr
#' Additional columns are used for the components, two for each component,
#' containing I0 and n0. The last columns `cont.` provide information on
#' the relative component contribution for each time interval including the row
#' sum for this values.
#'
#' @note
#' The pseudo-R² may not be the best parameter to describe the goodness
#' of the fit. The trade off between the `n.components` and the pseudo-R²
#' value currently remains unconsidered.
#'
#' The function **does not** ensure that the fitting procedure has reached a
#' global minimum rather than a local minimum! In any case of doubt, the use of
#' manual start values is highly recommended.
#'
#' @section Function version: 0.3.6
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [fit_CWCurve], [plot], [nls], [minpack.lm::nlsLM], [get_RLum]
#'
#' @references
#' Bulur, E., 1996. An Alternative Technique For Optically
#' Stimulated Luminescence (OSL) Experiment. Radiation Measurements, 26, 5,
#' 701-709.
#'
#' Jain, M., Murray, A.S., Boetter-Jensen, L., 2003. Characterisation of
#' blue-light stimulated luminescence components in different quartz samples:
#' implications for dose measurement. Radiation Measurements, 37 (4-5),
#' 441-449.
#'
#' Kitis, G. & Pagonis, V., 2008. Computerized curve deconvolution analysis for
#' LM-OSL. Radiation Measurements, 43, 737-741.
#'
#' Lave, C.A.T., 1970. The Demand for Urban Mass Transportation. The Review of
#' Economics and Statistics, 52 (3), 320-323.
#'
#' Ritz, C. & Streibig, J.C., 2008. Nonlinear Regression with R. R. Gentleman,
#' K. Hornik, & G. Parmigiani, eds., Springer, p. 150.
#'
#' @keywords dplot models
#'
#' @examples
#'
#' ##(1) fit LM data without background subtraction
#' data(ExampleData.FittingLM, envir = environment())
#' fit_LMCurve(values = values.curve, n.components = 3, log = "x")
#'
#' ##(2) fit LM data with background subtraction and export as JPEG
#' ## -alter file path for your preferred system
#' ##jpeg(file = "~/Desktop/Fit_Output\%03d.jpg", quality = 100,
#' ## height = 3000, width = 3000, res = 300)
#' data(ExampleData.FittingLM, envir = environment())
#' fit_LMCurve(values = values.curve, values.bg = values.curveBG,
#'             n.components = 2, log = "x", plot.BG = TRUE)
#' ##dev.off()
#'
#' ##(3) fit LM data with manual start parameters
#' data(ExampleData.FittingLM, envir = environment())
#' fit_LMCurve(values = values.curve,
#'             values.bg = values.curveBG,
#'             n.components = 3,
#'             log = "x",
#'             start_values = data.frame(Im = c(170,25,400), xm = c(56,200,1500)))
#'
#' @md
#' @export
fit_LMCurve<- function(
  values,
  values.bg,
  n.components = 3,
  start_values,
  input.dataType = "LM",
  sample_code = "",
  sample_ID = "",
  LED.power = 36,
  LED.wavelength = 470,
  fit.trace = FALSE,
  fit.calcError = FALSE,
  bg.subtraction = "polynomial",
  verbose = TRUE,
  plot = TRUE,
  plot.BG = FALSE,
  plot.residuals = TRUE,
  plot.contribution = TRUE,
  legend = TRUE,
  legend.pos = "topright",
  method_control = list(),
  ...
) {
  .set_function_name("fit_LMCurve")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  ##(1) data.frame or RLum.Data.Curve object?
  .validate_class(values, c("data.frame", "RLum.Data.Curve"))
  .validate_not_empty(values)

  if (inherits(values, "RLum.Data.Curve")) {
    if (!values@recordType %in% c("RBR", "LM-OSL")) {
      .throw_error("recordType should be 'RBR' or 'LM-OSL'. ",
                   "Consider using `as(object, 'data.frame')` if you ",
                   "have used a pseudo transformation function")
    }

    values <- as(values,"data.frame")
  }

  ##(2) data.frame or RLum.Data.Curve object?
  if (!missing(values.bg)) {
    .validate_class(values.bg, c("data.frame", "RLum.Data.Curve"))

    if (inherits(values.bg, "RLum.Data.Curve") && (is.na(values.bg@recordType) || values.bg@recordType != "RBR"))
      .throw_error("'recordType' for values.bg should be 'RBR'!")

    if (inherits(values.bg, "RLum.Data.Curve"))
      values.bg <- as(values.bg, "data.frame")

    ## check if length of bg and signal is consistent
    if (nrow(values) != nrow(values.bg))
      .throw_error("'values' and 'values.bg' have different lengths")
  }

  .validate_positive_scalar(n.components, int = TRUE)
  input.dataType <- .validate_args(input.dataType, c("LM", "pLM"))
  bg.subtraction <- .validate_args(bg.subtraction,
                                   c("polynomial", "linear", "channel", "none"))
  .validate_logical_scalar(verbose)
  .validate_logical_scalar(plot)
  .validate_logical_scalar(plot.BG)
  .validate_logical_scalar(plot.residuals)
  .validate_logical_scalar(plot.contribution)
  .validate_logical_scalar(legend)
  .validate_class(method_control, "list")

  ## remove missing values
  if (anyNA(values)) {
    na.idx <- unique(which(is.na(values), arr.ind = TRUE)[, 1])
    values <- values[-na.idx, ]
    if (!missing(values.bg))
      values.bg <- values.bg[-na.idx, ]
  }
  if (!missing(values.bg) && anyNA(values.bg)) {
    na.idx <- unique(which(is.na(values.bg), arr.ind = TRUE)[, 1])
    values <- values[-na.idx, ]
    values.bg <- values.bg[-na.idx, ]
  }

  ## Set plot format parameters -----------------------------------------------
  extraArgs <- list(...) # read out additional arguments list

  settings <- modifyList(list(
      main = "Default",
      xlim = range(values[, 1]),
      ylim = c(if (input.dataType == "LM") min(values[, 2]) else 0,
               max(values[, 2] * 1.1)),
      xlab = if (input.dataType == "LM") "Time [s]" else "u [s]",
      ylab = if (input.dataType == "LM") {
               paste0("LM-OSL [cts/",
                      round(max(values[, 1]) / nrow(values), digits = 2), " s]")
             } else "pLM-OSL [a.u.]",
      log = "",
      cex = 0.8,
      fun = FALSE
  ), extraArgs)

  method_control <- modifyList(x = list(export.comp.contrib.matrix = FALSE),
                               val = method_control)

  ## deprecated argument
  if ("fit.method" %in% names(extraArgs) && extraArgs$fit.method == "port") {
    .throw_warning("`fit.method = 'port'` is deprecated, fitting always ",
                   "occurs with the 'LM' method")
  }

  # layout safety settings
  if (plot || plot.BG) {
    par.default <- par()[c("mfrow", "cex", "mar", "omi", "oma")]
    on.exit(par(par.default), add = TRUE)
  }

  ##============================================================================##
  ##  BACKGROUND SUBTRACTION
  ##============================================================================##
  if (!missing(values.bg) && bg.subtraction != "none") {
    if (bg.subtraction %in% c("polynomial", "linear")) {
      if (bg.subtraction == "polynomial") {
        ## fit polynomial function to background
        glm.fit <- stats::glm(values.bg[, 2] ~ values.bg[, 1]
                                         + I(values.bg[, 1]^2)
                                         + I(values.bg[, 1]^3))
      } else {
        ## fit linear function to background
        glm.fit <- stats::glm(values.bg[, 2] ~ values.bg[, 1])
      }

      ## subtract background with fitted function
      values[, 2] <- values[, 2] - glm.fit$fitted.values

    }else if(bg.subtraction=="channel"){
      values[,2]<-values[,2]-values.bg[,2]
    }

    if (plot.BG) {
      par(mfrow = c(1, 1), cex = 1.5 * settings$cex)
      plot(values.bg, main = "Background",
           ylab = "LM-OSL [a.u.]", xlab = "Time [s]")
      mtext(side = 3, sample_code, cex = 0.8 * settings$cex)

      if (bg.subtraction %in% c("polynomial", "linear")) {
        lines(values.bg[, 1], glm.fit$fitted.values, col = "red", lwd = 1.5)

        ## extract and round coefficients to produce the fitted equation
        glm.coef <- round(coef(glm.fit), 2)
        text(0,max(values.bg[,2]),
             paste0("y = ",
                    if (bg.subtraction == "polynomial")
                      paste0(glm.coef[4], "*x^3+", glm.coef[3], "*x^2+"),
                    glm.coef[2], "*x+", glm.coef[1]), pos = 4)
      }
    }

    if (verbose) {
      message("[fit_LMCurve()] >> Background subtracted (method = '",
              bg.subtraction, "')")
    }
  }


  ##============================================================================##
  ##  FITTING
  ##============================================================================##

  ##------------------------------------------------------------------------##
  ##set function for fit equation (according Kitis and Pagonis, 2008)
  ##////equation used for fitting////(start)
  fit.equation<-function(Im.i,xm.i){
    equation<-parse(
      text=paste("exp(0.5)*Im[",Im.i,"]*(values[,1]/xm[",xm.i,"])*exp(-values[,1]^2/(2*xm[",xm.i,"]^2))",
                 collapse="+",sep=""))
    return(equation)
  }
  ##////equation used for fitting///(end)
  ##------------------------------------------------------------------------##

  ##set formula elements for fitting functions
  ## the upper two functions should be removed ... but chances are needed ... TODO
  ##////equation used for fitting////(start)
  fit.formula <- function(n.components){
    Im <- paste0("Im.",1:n.components)
    xm <- paste0("xm.",1:n.components)

    stats::as.formula(paste0("y ~ ", paste("(exp(0.5) * ", Im, "* x /", xm,
                                           ") * exp(-x^2 / (2 *", xm, "^2))",
                                           collapse = " + ")))
  }
  ##////equation used for fitting///(end)

  ##------------------------------------------------------------------------##
  ##automatic start parameter estimation

  ##set fit function
  fit.function <- fit.equation(Im.i = 1:n.components, xm.i = 1:n.components)

  if(missing(start_values)){

    ##set b (detrapping) values for a 7-component function taken from Jain et al. (2003)
    b.pseudo<-c(32,2.5,0.65,0.15,0.025,0.0025,0.00030)

    ##calculate xm parameters from values set based on the pseudo curves
    xm.pseudo<-sqrt(max(values[,1])/b.pseudo)

    ##the Im values obtaind by calculating residuals
    Im.pseudo <- sapply(1:length(b.pseudo), function(x) {
      xm.residual <- abs(values[, 1] - xm.pseudo[x])
      values[which.min(xm.residual), 1] # time value of minimum residual
    })

    ##set additional variables
    b.pseudo_start<-1
    b.pseudo_end<-0
    fit.found <- FALSE

    while(!fit.found) {
      xm <- xm.pseudo[b.pseudo_start:(n.components + b.pseudo_end)]
      Im <- Im.pseudo[b.pseudo_start:(n.components + b.pseudo_end)]

          ##re-name for method == "LM"
          names(Im) <- paste0("Im.", 1:n.components)
          names(xm) <- paste0("xm.", 1:n.components)
          start.list <- c(as.list(Im), as.list(xm))

          fit <- try(minpack.lm::nlsLM(
            fit.formula(n.components),
            data = data.frame(x = values[,1],
                              y = values[,2]),
            start = start.list,
            lower = rep(0, length(start.list)),
            trace = fit.trace,
            control = minpack.lm::nls.lm.control(maxiter = 500)
          ), silent = TRUE)

      if (!inherits(fit, "try-error") || n.components + b.pseudo_end == 7) {
        fit.found <- TRUE
      }
      b.pseudo_start <- b.pseudo_start + 1
      b.pseudo_end <- b.pseudo_end + 1
    }#end:while loop fit found

  }else{#endif::missing start values
    ##------------------------------------------------------------------------##
    Im <- start_values[, 1]
    names(Im) <- paste0("Im.", 1:n.components)
    xm <- start_values[, 2]
    names(xm) <- paste0("xm.", 1:n.components)
    start.list <- c(as.list(Im), as.list(xm))
    fit <- try(minpack.lm::nlsLM(
                       fit.formula(n.components),
                       data = data.frame(x = values[, 1],
                                         y = values[, 2]),
                       start = start.list,
                       lower = rep(0, length(start.list)),
                       trace = fit.trace,
                       control = minpack.lm::nls.lm.control(maxiter = 500)),
               outFile = stdout()) # redirect error messages so they can be silenced
  }#endif::startparameter

  ##------------------------------------------------------------------------##

  ##grep parameters
  if (!inherits(fit, "try-error")) {
    parameters<-coef(fit)

    ##write parameters in vectors and order parameters
    Im<-parameters[1:(length(parameters)/2)]
    Im.names <- names(Im)
    xm<-parameters[(1+(length(parameters)/2)):length(parameters)]
    xm.names <- names(xm)

    ##order parameters
    o <- order(xm)
    xm <- xm[o]
    names(xm) <- xm.names
    Im <- Im[o]
    names(Im) <- Im.names

    if (verbose){
      ##print rough fitting information - use the nls() control for more information
      writeLines("\n[fit_LMCurve()]")
      writeLines(paste0("\nFitting was done using a ", n.components,
                        "-component function:\n"))

      ##print parameters
      print(c(xm, Im))

      #print some additional information
      writeLines("\n(equation used for fitting according to Kitis & Pagonis, 2008)")
    }#end if

    ##============================================================================##
    ##  Additional Calculations
    ##============================================================================##

    ##calculate stimulation intensity Schmidt (2008)

    ##Energy - E = h*v
    h <- .const$h # Planck constant (W*s^2)
    ny <- .const$c / (LED.wavelength / 10^9) # frequency of the light
    E<-h*ny

    ##transform LED.power in W/cm^2
    LED.power<-LED.power/1000

    stimulation_intensity<-LED.power/E

    ##calculate b and n from the equation of Bulur(1996) to compare results
    ##Using Equation 5 and 6 from Kitis (2008)
    b<-as.vector(max(values[,1])/xm^2) #detrapping probability
    n0<-as.vector((Im/exp(-0.5))*xm)


    ##CALCULATE 1- sigma CONFIDENCE INTERVAL
    ##------------------------------------------------------------------------##
    b.error <- rep(NA, n.components)
    n0.error <- rep(NA, n.components)

    if(fit.calcError){
      ##option for confidence interval
      values.confint <- try(stats::confint(fit, level = 0.68), silent = TRUE)

      if(!inherits(values.confint, "try-error")) {
        Im.confint <- values.confint[1:(length(values.confint[, 1]) / 2), ]
        xm.confint <- values.confint[((length(values.confint[,1])/2)+1):length(values.confint[,1]),]

        ##error calculation
        b.error <- as.vector(abs((max(values[, 1]) / xm.confint[, 1]^2) -
                                 (max(values[, 1]) / xm.confint[, 2]^2)))
        n0.error <- as.vector(abs(((Im.confint[,1]/exp(-0.5))*xm.confint[,1]) - ((Im.confint[,2]/exp(-0.5))*xm.confint[,2])))

      } else {
        .throw_warning("The computation of the parameter confidence intervals ",
                       "failed. Please try to run stats::confint() manually ",
                       "on the $fit output object")
      }
    }
    ##------------------------------------------------------------------------##


    ##calculate photoionisation cross section and print on terminal
    ##using EQ (5) in Kitis
    cs<-as.vector((max(values[,1])/xm^2)/stimulation_intensity)
    rel_cs<-round(cs/cs[1],digits=4)

    ##coefficient of determination after law
    RSS <- sum(residuals(fit)^2) #residual sum of squares
    TSS <- sum((values[,2] - mean(values[,2]))^2) #total sum of squares
    pR<-round(1-RSS/TSS,digits=4)

    ##============================================================================##
    ## COMPONENT TO SUM CONTRIBUTION MATRIX
    ##============================================================================##

    ##+++++++++++++++++++++++++++++++
    ##set matrix
    ##set polygon matrix for optional plot output
    component.contribution.matrix <- matrix(NA,
                                            nrow = length(values[,1]),
                                            ncol = (2*length(xm)) + 2)

    ##set x-values
    component.contribution.matrix[,1] <- values[,1]
    component.contribution.matrix[,2] <- rev(values[,1])

    ##+++++++++++++++++++++++++++++++
    ##set 1st polygon
    ##1st polygon (calculation)
    y.contribution_first <- (exp(0.5)*Im[1]*values[,1]/
                               xm[1]*exp(-values[,1]^2/(2*xm[1]^2))/
                               (eval(fit.function))*100)

    ##avoid NaN values (might happen with synthetic curves)
    y.contribution_first[is.nan(y.contribution_first)==TRUE] <- 0

    ##set values in matrix
    component.contribution.matrix[,3] <- 100
    component.contribution.matrix[,4] <- 100-rev(y.contribution_first)

    ##+++++++++++++++++++++++++++++++
    ##set polygons in between
    ##polygons in between (calculate and plot)
    if (length(xm)>2){
      y.contribution_prev <- y.contribution_first
      i<-2

      ##matrix stepping
      k <- seq(3, ncol(component.contribution.matrix), by=2)

      while (i<=length(xm)-1) {
        y.contribution_next<-(exp(0.5)*Im[i]*values[,1]/
                                xm[i]*exp(-values[,1]^2/(2*xm[i]^2))/
                                (eval(fit.function))*100)

        ##avoid NaN values
        y.contribution_next[is.nan(y.contribution_next)==TRUE] <- 0

        ##set values in matrix
        component.contribution.matrix[, k[i]] <- 100-y.contribution_prev
        component.contribution.matrix[, k[i]+1] <- rev(100-y.contribution_prev-
                                                         y.contribution_next)

        y.contribution_prev <- y.contribution_prev + y.contribution_next

        i<-i+1
      }#end while loop
    }#end if

    ##+++++++++++++++++++++++++++++++
    ##set last polygon

    ##last polygon (calculation)
    y.contribution_last<-(exp(0.5)*Im[length(xm)]*values[,1]/
                            xm[length(xm)]*exp(-values[,1]^2/
                                                 (2*xm[length(xm)]^2))/
                            (eval(fit.function))*100)

    ##avoid NaN values
    y.contribution_last[is.nan(y.contribution_last)==TRUE]<-0

    component.contribution.matrix[,((2*length(xm))+1)] <- y.contribution_last
    component.contribution.matrix[,((2*length(xm))+2)] <- 0

    ##change names of matrix to make more easy to understand
    component.contribution.matrix.names <- c("x", "rev.x",
                                             paste(c("y.c","rev.y.c"),rep(1:n.components,each=2), sep=""))

    ##calculate area for each component, for each time interval
    component.contribution.matrix.area <- sapply(
      seq(3,ncol(component.contribution.matrix),by=2),
      function(x){
        matrixStats::rowDiffs(cbind(rev(component.contribution.matrix[,(x+1)]),
                       component.contribution.matrix[,x]))
      })

    ##append to existing matrix
    component.contribution.matrix <- cbind(
      component.contribution.matrix,
      component.contribution.matrix.area,
      rowSums(component.contribution.matrix.area)
    )

    ##set final column names
    colnames(component.contribution.matrix) <- c(
      component.contribution.matrix.names,
      paste(c("cont.c"),rep(1:n.components,each=1), sep=""),
      "cont.sum")

    ##============================================================================##
    ##  Terminal Output (advanced)
    ##============================================================================##
    if (verbose){
      ##write fill lines
      writeLines("------------------------------------------------------------------------------")
      writeLines("(1) Corresponding values according to the equation in Bulur, 1996 for b and n0:\n")
      for (i in 1:length(b)){
        cat(paste0("b", i), "=", format(b[i], scientific = TRUE),
            "\u00B1", format(b.error[i], scientific = TRUE), "\n")
        cat(paste0("n0", i), "=", format(n0[i], scientific = TRUE),
            "\u00B1", format(n0.error[i], scientific = TRUE), "\n\n")
      }#end for loop

      ##write photoionisation cross section on terminal
      for (i in 1:length(cs)){
        cat(paste0("cs from component.", i), "=",
            format(cs[i], scientific = TRUE, digits = 4),
            "cm^2\t >> relative: ", round(cs[i] / cs[1], 4), "\n")
      }#end for loop

      cat("\n(stimulation intensity value used for calculation: ",
          format(stimulation_intensity, scientific = TRUE), " 1/s 1/cm^2)\n")
      writeLines("(errors quoted as 1-sigma uncertainties)")
      writeLines("------------------------------------------------------------------------------\n")

      #sum of squares
      cat("pseudo-R^2 =", pR, "\n")
    }#end if

    ##============================================================================##
    ##  COMPOSE RETURN VALUES (data.frame)
    ##============================================================================##

    ##write output table if values exists
    if (exists("fit")){
      ## build column names according to a pattern
      pat <- c("Im%d", "xm%d", "b%d", "b%d.error",
               "n%02d", "n%02d.error", "cs%d", "rel_cs%d")
      output.tableColNames <- unlist(lapply(1:7, function(x) sprintf(pat, x)))

      ## set data.frame for a max value of 7 components
      output.table <- rep(NA_real_, length(pat) * 7)
      for (k in 1:n.components) {
        output.table[(k - 1) * 8 + 1:8] <- c(Im[k], xm[k], b[k], b.error[k],
                                             n0[k], n0.error[k], cs[k], rel_cs[k])
      }

      ##add pR and n.components
      output.table <- cbind(sample_ID, sample_code, n.components,
                            data.frame(as.list(output.table)), pR)
      colnames(output.table)<-c("ID","sample_code","n.components",output.tableColNames,"pseudo-R^2")

      ##----------------------------------------------------------------------------##
    }#endif::exists fit

  }else{
    output.table <- NA
    component.contribution.matrix <- NA
    .throw_message("Fitting failed, plot without fit produced")
  }

  # Calculate component curves ----------------------------------------------
  component_matrix <- NA
  if(!inherits(fit,"try-error")){
    component_matrix <- matrix(NA, nrow = nrow(values), ncol = 2 + length(Im))
    colnames(component_matrix) <- c("TIME", "SUM", paste("COMP_", 1:length(Im)))
    component_matrix[, 1] <- values[, 1]
    component_matrix[, 2] <- eval(fit.function)

    ## add single components
    for(i in 1:length(Im)){
      component_matrix[, 2 + i] <-
        exp(0.5) * Im[i] * values[, 1] /
        xm[i] * exp(-values[, 1] ^ 2 / (2 * xm[i] ^ 2))
    }
  }

  # Plotting ----------------------------------------------------------------
  if(plot){
    ##cheat the R check routine
    x <- NULL; rm(x)

    ##grep package colour gallery
    col <- get("col", pos = .LuminescenceEnv)

    ## change xlim/ylim values in case of log plot to avoid problems
    if (settings$log %in% c("x", "xy") && settings$xlim[1] == 0) {
      .throw_warning("'xlim' changed to avoid 0 values for log-scale")
      xlim <- c(2^0.5 / 2 * max(values[, 1]) / nrow(values), settings$xlim[2])
    }
    if (settings$log %in% c("y", "xy") && settings$ylim[1] == 0) {
      settings$ylim[1] <- 0.01
    }

    ##set plot frame
    graphics::layout(matrix(seq(1 + plot.residuals + plot.contribution), ncol = 1),
                     heights = c(10, if (plot.residuals) 3,
                                 if (plot.contribution) 4))
    par(oma = c(1, 1, 1, 1), mar = c(0.5, 2.5, 2, 0), cex = settings$cex)
    mgp <- c(1.5, 0.5, 0)

    ##==upper plot==##
    ##open plot area
    plot_check <- try(plot(
      NA,
      NA,
      xlim = settings$xlim,
      ylim = settings$ylim,
      xlab = "",
      xaxt = "n",
      main = settings$main,
      mgp = mgp,
      log = settings$log,
      cex.axis = 0.8,
      cex.lab = 0.9,
      ylab = settings$ylab
    ), silent = TRUE)

    if (inherits(plot_check, "try-error")) {
      ## reset the graphic device if plotting failed
      .throw_message("Figure margins too large or plot area too small, ",
                     "nothing plotted")
      grDevices::dev.off()
    } else {

    mtext(side = 3, sample_code, cex = 0.8 * settings$cex)

    ##plotting measured signal
    points(values[, 1],
           values[, 2],
           pch = 20,
           col = rgb(0.4, 0.4, 0.4, 0.5))

    ##==pseudo curve==##------------------------------------------------------#

    ##curve for used pseudo values
    if (inherits(fit, "try-error") && missing(start_values)) {
      fit.function<-fit.equation(Im.i=1:n.components,xm.i=1:n.components)
      Im<-Im.pseudo[1:n.components]
      xm<-xm.pseudo[1:n.components]

      ##draw pseudo curve
      lines(values[,1],eval(fit.function), lwd=2, col="red", lty=2)

      axis(side=1)
      mtext(side = 1, settings$xlab, cex = 0.9 * settings$cex, line = 2)

      mtext(side = 4, paste(n.components, "component pseudo function is shown"),
            cex = 0.7, col = "blue")

      ##draw information text on plot
      text(min(values[,1]),max(values[,2]),"FITTING ERROR!",pos=4)

      ##additional legend
      legend("topright",c("pseudo sum function"),lty=2,lwd=2,col="red",bty="n")
    }
    ##==pseudo curve==##------------------------------------------------------##

    ##plot sum function
    if (!inherits(fit, "try-error")) {
      lines(values[,1],eval(fit.function), lwd=2, col="black")
      legend.caption <- c("Sum curve", paste("Component", 1:n.components))
      curve.col <- seq(n.components + 1)

      ##plot signal curves

      ##plot curve for additional parameters
      for (i in 1:length(xm)) {
        curve(exp(0.5) * Im[i] * x / xm[i] * exp(-x^2 / (2 * xm[i]^2)),
              col = col[i + 1], lwd = 2, add = TRUE)
      }

      ## plot legend
      if (legend)
        legend(legend.pos, legend.caption, lty = 1, lwd = 2, bty = "n",
               col = col[curve.col])

      ##==lower plot==##
      ##plot residuals
      if (plot.residuals) {
      par(mar = c(0.5, 2.5, 0, 0))
      plot(values[,1],residuals(fit),
           xlim = settings$xlim,
           xlab = "",
           xaxt = "n",
           type="l",
           col="grey",
           ylab="Residual",
           lwd=2,
           xpd = NA,
           mgp = mgp,
           cex.axis = 0.8,
           cex.lab = 0.9,
           log = gsub("y", "", settings$log))

      ##ad 0 line
      abline(h=0)
      }

      ##------------------------------------------------------------------------#
      ##++component to sum contribution plot ++##
      ##------------------------------------------------------------------------#

      ##plot component contribution to the whole signal
      #open plot area
      if (plot.contribution) {
      par(mar = c(2, 2.5, 0, 0))
      plot(NA,NA,
           xlim = settings$xlim,
           ylim=c(0,100),
           ylab = "Component\ncontrib. [%]",
           xlab = settings$xlab,
           main = "",
           xpd = NA,
           mgp = mgp,
           cex.axis = 0.8,
           cex.lab = 0.9,
           log = gsub("y", "", settings$log))

      stepping <- seq(3,length(component.contribution.matrix),2)

      for(i in 1:length(xm)){

        polygon(c(component.contribution.matrix[,1],
                  component.contribution.matrix[,2]),
                c(component.contribution.matrix[,stepping[i]],
                  component.contribution.matrix[,stepping[i]+1]),
                col = col[i+1])
      }
      }
      ##------------------------------------------------------------------------##
    }#end if try-error for fit

    if (settings$fun == TRUE) sTeve() # nocov

    } # end if (plot_check)
  }
  ##-----------------------------------------------------------------------------
  ##remove objects
  try(unlist("parameters"))

  ##============================================================================#
  ## Return Values
  ##============================================================================#

  if (!method_control$export.comp.contrib.matrix) {
    component.contribution.matrix <- NA
  }
  newRLumResults.fit_LMCurve <- set_RLum(
    class = "RLum.Results",
    data = list(
      data = output.table,
      fit = fit,
      component_matrix = component_matrix,
      component.contribution.matrix = list(component.contribution.matrix)
    ),
    info = list(call = sys.call())
  )

  invisible(newRLumResults.fit_LMCurve)
}

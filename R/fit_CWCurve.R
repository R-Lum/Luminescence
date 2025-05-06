#' @title Nonlinear Least Squares Fit for CW-OSL curves -beta version-
#'
#' @description
#' The function determines the weighted least-squares estimates of the
#' component parameters of a CW-OSL signal for a given maximum number of
#' components and returns various component parameters. The fitting procedure
#' uses the [nls] function with the `port` algorithm.
#'
#' **Fitting function**
#'
#' The function for the CW-OSL fitting has the general form:
#'
#' \deqn{y = I0_{1}*\lambda_{1}*exp(-\lambda_1*x) + ,\ldots, + I0_{i}*\lambda_{i}*exp(-\lambda_i*x) }
#'
#' where \eqn{0 < i < 8}
#'
#' and \eqn{\lambda} is the decay constant \cr
#' and \eqn{I0} the initial number of trapped electrons.
#'
#' *(for the used equation cf. Boetter-Jensen et al., 2003, Eq. 2.31)*
#'
#' **Start values**
#'
#' Start values are estimated automatically by fitting a linear function to the
#' logarithmized input data set. Currently, there is no option to manually
#' provide start parameters.
#'
#' **Goodness of fit**
#'
#' The goodness of the fit is given as pseudoR² value (pseudo coefficient of
#' determination). According to Lave (1970), the value is calculated as:
#'
#' \deqn{pseudoR^2 = 1 - RSS/TSS}
#'
#' where \eqn{RSS = Residual~Sum~of~Squares} \cr
#' and \eqn{TSS = Total~Sum~of~Squares}
#'
#'
#' **Error of fitted component parameters**
#'
#' The 1-sigma error for the
#' components is calculated using the function [stats::confint]. Due to
#' considerable calculation time, this option is deactivated by default. In
#' addition, the error for the components can be estimated by using internal R
#' functions like [summary]. See the [nls] help page
#' for more information.
#'
#' *For details on the nonlinear regression in R, see Ritz & Streibig (2008).*
#'
#' @param values [RLum.Data.Curve-class] or [data.frame] (**required**):
#' x, y data of measured values (time and counts). See examples.
#'
#' @param n.components.max [vector] (*optional*):
#' maximum number of components that are to be used for fitting.
#' The upper limit is 7.
#'
#' @param fit.failure_threshold [integer] (*with default*):
#' limits the failed fitting attempts.
#'
#' @param fit.method [character] (*with default*):
#' select fit method, allowed values: `'port'` and `'LM'`. `'port'` uses the 'port'
#' routine from the function [nls] `'LM'` utilises the function `nlsLM` from
#' the package `minpack.lm` and with that the Levenberg-Marquardt algorithm.
#'
#' @param fit.trace [logical] (*with default*):
#' traces the fitting process on the terminal.
#'
#' @param fit.calcError [logical] (*with default*):
#' calculate 1-sigma error range of components using [stats::confint]
#'
#' @param LED.power [numeric] (*with default*):
#' LED power (max.) used for intensity ramping in mW/cm².
#' **Note:** The value is used for the calculation of the absolute
#' photoionisation cross section.
#'
#' @param LED.wavelength [numeric] (*with default*):
#' LED wavelength used for stimulation in nm.
#' **Note:** The value is used for the calculation of the absolute
#' photoionisation cross section.
#'
#' @param cex.global [numeric] (*with default*):
#' global scaling factor.
#'
#' @param sample_code [character] (*optional*):
#' sample code used for the plot and the optional output table (`mtext`).
#'
#' @param verbose [logical] (*with default*):
#' enable/disable output to the terminal.
#'
#' @param output.terminalAdvanced [logical] (*with default*):
#' enhanced terminal output. Only valid if `verbose = TRUE`.
#'
#' @param plot [logical] (*with default*):
#' enable/disable the plot output.
#'
#' @param method_control [list] (*optional*): options to control the output
#' produced. Currently only the 'export.comp.contrib.matrix' (logical) option
#' is supported, to enable/disable export of the component contribution
#' matrix.
#'
#' @param ... further arguments and graphical parameters passed to [plot].
#'
#' @return
#' **plot (*optional*)**
#'
#' the fitted CW-OSL curves are returned as plot.
#'
#' **RLum.Results object**
#'
#' Beside the plot and table output options, an [RLum.Results-class] object is
#' returned.
#'
#' `fit`:
#' an `nls` object (`$fit`) for which generic R functions are
#' provided, e.g. [summary], [stats::confint], [profile]. For more
#' details, see [nls].
#'
#' `output.table`:
#' a [data.frame] containing the summarised parameters including the error
#'
#' `component.contribution.matrix`:
#' [matrix] containing the values for the component to sum contribution plot
#' (`$component.contribution.matrix`).
#' Produced only if `method_control$export.comp.contrib.matrix = TRUE`).
#'
#' Matrix structure:\cr
#' Column 1 and 2: time and `rev(time)` values \cr
#' Additional columns are used for the components, two for each component,
#' containing I0 and n0. The last columns `cont.` provide information on
#' the relative component contribution for each time interval including the row
#' sum for this values.
#'
#' @note
#'
#' **Beta version - This function has not been properly tested yet and**
#' **should therefore not be used for publication purposes!**
#'
#' The pseudo-R² may not be the best parameter to describe the goodness of the
#' fit. The trade off between the `n.components` and the pseudo-R² value
#' is currently not considered.
#'
#' The function **does not** ensure that the fitting procedure has reached a
#' global minimum rather than a local minimum!
#'
#' @section Function version: 0.5.4
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [fit_LMCurve], [plot],[nls], [RLum.Data.Curve-class],
#' [RLum.Results-class], [get_RLum], [minpack.lm::nlsLM]
#'
#' @references
#' Boetter-Jensen, L., McKeever, S.W.S., Wintle, A.G., 2003.
#' Optically Stimulated Luminescence Dosimetry. Elsevier Science B.V.
#'
#' Lave, C.A.T., 1970. The Demand for Urban Mass Transportation. The Review of
#' Economics and Statistics, 52 (3), 320-323.
#'
#' Ritz, C. & Streibig, J.C., 2008. Nonlinear Regression with R. In: R.
#' Gentleman, K. Hornik, G. Parmigiani, eds., Springer, p. 150.
#'
#' @keywords dplot models
#'
#' @examples
#'
#' ##load data
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#'
#' ##fit data
#' fit <- fit_CWCurve(values = ExampleData.CW_OSL_Curve,
#'                    main = "CW Curve Fit",
#'                    n.components.max = 4,
#'                    log = "x")
#'
#' @md
#' @export
fit_CWCurve<- function(
  values,
  n.components.max,
  fit.failure_threshold = 5,
  fit.method = "port",
  fit.trace = FALSE,
  fit.calcError = FALSE,
  LED.power = 36,
  LED.wavelength = 470,
  cex.global = 0.6,
  sample_code = "Default",
  verbose = TRUE,
  output.terminalAdvanced = TRUE,
  plot = TRUE,
  method_control = list(),
  ...
) {
  .set_function_name("fit_CWCurve")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(values, c("RLum.Data.Curve", "data.frame"))
  .validate_not_empty(values)
  if (inherits(values, "RLum.Data.Curve")) {
    values <- as.data.frame(values@data[, 1:2, drop = FALSE])
  }

  ## set x and y values
  x <- values[, 1]
  y <- values[, 2]

  if (sum(y > 0) == 0) {
    .throw_error("'values' contains no positive counts")
  }
  if (any(order(x) != seq_along(x))) {
    .throw_error("Time values are not ordered")
  }
  fit.method <- .validate_args(fit.method, c("port", "LM"))
  .validate_positive_scalar(n.components.max, int = TRUE)
  .validate_positive_scalar(fit.failure_threshold, int = TRUE)
  .validate_logical_scalar(verbose)
  .validate_logical_scalar(output.terminalAdvanced)
  .validate_logical_scalar(plot)
  .validate_class(method_control, "list")

  # Deal with extra arguments -----------------------------------------------

  ##deal with addition arguments
  extraArgs <- list(...)

  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else
  {"CW-OSL Curve Fit"}

  log <- if("log" %in% names(extraArgs)) {extraArgs$log} else
  {""}

  xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else
  {"Time [s]"}

  ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} else
  {paste("OSL [cts/",round(max(x)/length(x), digits = 2)," s]",sep="")}

  method_control <- modifyList(x = list(export.comp.contrib.matrix = FALSE),
                               val = method_control)

  if ("output.path" %in% names(extraArgs))
    .throw_warning("Argument 'output.path' no longer supported, ignored")

  ##============================================================================##
  ## FITTING
  ##============================================================================##
  ##
  ##////equation used for fitting////(start)
  fit.equation <- function(I0.i,lambda.i){
    equation<-parse(
      text=paste("I0[",I0.i,"]*lambda[",lambda.i,"]*exp(-lambda[",lambda.i,"]*x)",
                 collapse="+",sep=""))
    return(equation)
  }
  ##////equation used for fitting///(end)

  ##set formula elements for fitting functions
  ## the upper two funtions should be removed ... but chances are needed ... TODO
  ##////equation used for fitting////(start)
  fit.formula <- function(n.components){
    I0 <- paste0("I0.",1:n.components)
    lambda <- paste0("lambda.",1:n.components)
    stats::as.formula(paste0("y ~ ", paste(I0," * ", lambda,
                                           "* exp(-",lambda," * x)",
                                           collapse = " + ")))
  }
  ##////equation used for fitting///(end)

  ##////equation used for fitting////(start)
  fit.formula.simple <- function(n.components){
    I0 <- paste0("I0.",1:n.components)
    lambda <- paste0("lambda.",1:n.components)
    stats::as.formula(paste0("y ~ ", paste(I0," * exp(-",lambda," * x)",
                             collapse = " + ")))
  }
  ##////equation used for fitting///(end)

  ##set variables
  keep.fitting <- TRUE # set to FALSE if the fitting should be stopped early
  n.components <- 1 #number of components used for fitting - start with 1
  fit.failure_counter <- 0 #counts the failed fitting attempts

  ##if n.components_max is missing, then it is Inf
  if(missing(n.components.max)==TRUE){n.components.max<-Inf}

  ##
  ##++++Fitting loop++++(start)
  while(keep.fitting && n.components <= n.components.max) {
    ##(0) START PARAMETER ESTIMATION
    ##rough automatic start parameter estimation

    ##I0
    I0<-rep(values[1,2]/3,n.components)
    names(I0) <- paste0("I0.",1:n.components)

    ##lambda
    ##ensure that no values <=0 are included remove them for start parameter
    ##estimation and fit an linear function a first guess
    temp.values <- data.frame(x[y > 0], log(y[y > 0]))

    temp <- stats::lm(temp.values)
    lambda<-abs(temp$coefficient[2])/nrow(values)

    k<-2
    while(k<=n.components){
      lambda[k]<-lambda[k-1]/100
      k<-k+1
    }
    names(lambda) <- paste0("lambda.",1:n.components)

    ##(1) FIRST FIT WITH A SIMPLE FUNCTION
    if(fit.method == "LM"){

      ##try fit simple
      fit.try<-suppressWarnings(try(minpack.lm::nlsLM(fit.formula.simple(n.components),
                                          data=values,
                                          start=c(I0,lambda),
                                          na.action = "na.exclude",
                                          trace = fit.trace,
                                          control = minpack.lm::nls.lm.control(
                                            maxiter = 500
                                          )),
                                    silent = TRUE
      ))#end try


    }else if(fit.method == "port"){

      ##try fit simple
      fit.try<-suppressWarnings(try(nls(fit.formula.simple(n.components),
                                        data=values,
                                        trace = fit.trace,
                                        algorithm="port",
                                        na.action = "na.exclude",
                                        start=c(I0,lambda),
                                        stats::nls.control(
                                          tol = 1,
                                          maxiter=100,
                                          warnOnly=FALSE,
                                          minFactor=1/1024
                                        ),
                                        lower=rep(0,n.components * 2)# set lower boundaries for components
      ), silent=TRUE# nls
      ))#end try
    }

    ##(3) FIT WITH THE FULL FUNCTION
    if (!inherits(fit.try, "try-error")) {

      ##grep parameters from simple fit to further work with them
      parameters <- coef(fit.try)

      ##grep parameters an set new starting parameters, here just lambda is choosen as
      ##it seems to be the most valuable parameter
      lambda <- parameters[(n.components+1):length(parameters)]

      if(fit.method == "LM"){

        ##try fit simple
        fit.try<-suppressWarnings(try(minpack.lm::nlsLM(fit.formula(n.components),
                                            data=values,
                                            start=c(I0,lambda),
                                            trace = fit.trace,
                                            na.action = "na.exclude",
                                            lower = rep(0,n.components * 2),
                                            control = minpack.lm::nls.lm.control(
                                              maxiter = 500
                                            )),
                                      silent = TRUE))

        ## HACK:
        # minpack.lm::nlsLM() stores the 'lower' argument as class "call" rather
        # than "numeric" as nls() does. Before running confint() on this object
        # we overwrite the "lower" slot with the numeric values again.
        if (!inherits(fit.try, "try-error")) {
          fit.try$call$lower <- rep(0,n.components * 2)
        }

      }else{
        ##try fit
        fit.try<-suppressWarnings(try(nls(fit.formula(n.components),
                                          trace=fit.trace,
                                          data=values,
                                          algorithm="port",
                                          na.action = "na.exclude",
                                          start=c(I0,lambda),
                                          stats::nls.control(
                                            maxiter = 500,
                                            warnOnly = FALSE,
                                            minFactor = 1/4096
                                          ),
                                          lower=rep(0,n.components * 2)# set lower boundaries for components
        ), silent=TRUE# nls
        ))#end try

      }#fit.method
    }
    n.components <- n.components + 1

    ##count failed attempts for fitting
    if (!inherits(fit.try, "try-error")) {
      fit <- fit.try

    }else{
      fit.failure_counter <- fit.failure_counter+1
      if (!exists("fit")) {
        fit <- fit.try
      }
    }

    ##stop fitting after a given number of wrong attempts
    if(fit.failure_counter>=fit.failure_threshold){
      keep.fitting <- FALSE
    }

  }##end while
  ##++++Fitting loop++++(end)

  ##============================================================================##
  ## FITTING OUTPUT
  ##============================================================================##

  ##grep parameters
  output.table <- component.contribution.matrix <- NA
  if (!inherits(fit, "try-error")) {

    parameters <- coef(fit)

    ##correct fit equation for the de facto used number of components
    I0.i<-1:(length(parameters)/2)
    lambda.i<-1:(length(parameters)/2)
    fit.function<-fit.equation(I0.i=I0.i,lambda.i=lambda.i)
    n.components<-length(I0.i)

    ##write parameters in vectors and order by decreasing lambda value
    I0<-parameters[1:(length(parameters)/2)]
    lambda<-parameters[(1+(length(parameters)/2)):length(parameters)]

    o<-order(lambda,decreasing=TRUE)
    I0<-I0[o]
    lambda<-lambda[o]

    ##============================================================================##
    ## Additional Calculation
    ##============================================================================##

    ## ---------------------------------------------
    ##calculate stimulation intensity Schmidt (2008)

    ##Energy - E = h*v
    h <- .const$h # Planck constant (W*s^2)
    ny <- .const$c / (LED.wavelength / 10^9) # frequency of light
    E<-h*ny

    ## transform LED.power in W/cm²
    LED.power<-LED.power/1000

    ##gets stimulation intensity
    stimulation_intensity<-LED.power/E

    ## ---------------------------------------------
    ##calculate photoionisation cross section and print on terminal

    ##using EQ (5) in Kitis
    cs<-as.vector(lambda/stimulation_intensity)
    cs.rel<-round(cs/cs[1],digits=4)

    ## ---------------------------------------------
    ##coefficient of determination after law

    RSS <- sum(residuals(fit)^2) #residual sum of squares
    TSS <- sum((y - mean(y))^2) #total sum of squares
    pR<-round(1-RSS/TSS,digits=4)

    if(pR<0){
      .throw_warning("pseudo-R^2 < 0!") # nocov
    }

    ## ---------------------------------------------
    ##calculate 1- sigma CONFIDENCE INTERVALL

    lambda.error<-rep(NA, n.components)
    I0.error<-rep(NA, n.components)

    ## option for confidence interval
    if (fit.calcError) {
      tryCatch({
        values.confint <- stats::confint(fit, level = 0.68)
        half <- nrow(values.confint) / 2
        I0.confint <- values.confint[1:half, ]
        lambda.confint <- values.confint[half + 1:half, ]

        ## error calculation
        I0.error <- abs(I0.confint[, 1] - I0.confint[, 2])
        lambda.error <- abs(lambda.confint[, 1] - lambda.confint[, 2])
      }, error = function(e) {
        ## report the error from confint()
        .throw_message("Computation of confidence interval failed: ",
                       e$message)
      })
    }#endif::fit.calcError

    ##============================================================================##
    ## Terminal Output
    ##============================================================================##

    if (verbose) {

      ##print rough fitting information - use the nls() control for more information
      writeLines("\n[fit_CWCurve()]")
      cat(paste0("\nFitting was finally done using a ", n.components,
                 "-component function (max=", n.components.max, "):\n"))
      writeLines("------------------------------------------------------------------------------")
      writeLines(paste0("y ~ ", as.character(fit.formula(n.components))[3], "\n"))

      ##combine values and change rows names
      fit.results<-cbind(I0,I0.error,lambda,lambda.error,cs, cs.rel)
      row.names(fit.results)<-paste("c", 1:(length(parameters)/2), sep="")

      ##print parameters
      print(fit.results)

      #print some additional information
      if (fit.calcError)
        cat("(errors quoted as 1-sigma values)\n")
      writeLines("------------------------------------------------------------------------------")
    }#end if

    ##============================================================================##
    ## Terminal Output (advanced)
    ##============================================================================##
    if (verbose && output.terminalAdvanced) {

      ##sum of squares
      writeLines(paste("pseudo-R^2 = ",pR,sep=""))
    }#end if
    ##============================================================================##
    ## Table Output
    ##============================================================================##

    ##write output table if values exists
    if (exists("fit")){

      ##set data.frame for a max value of 7 components
      output.table<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
      output.tableColNames<-c("I01","I01.error","lambda1", "lambda1.error",
                              "cs1","cs1.rel",
                              "I02","I02.error","lambda2", "lambda2.error",
                              "cs2","cs2.rel",
                              "I03","I03.error","lambda3", "lambda3.error",
                              "cs3","cs3.rel",
                              "I04","I04.error","lambda4", "lambda4.error",
                              "cs4","cs4.rel",
                              "I05","I05.error","lambda5", "lambda5.error",
                              "cs5","cs5.rel",
                              "I06","I06.error","lambda6", "lambda6.error",
                              "cs6","cs6.rel",
                              "I07","I07.error","lambda7", "lambda7.error",
                              "cs7","cs7.rel"
      )

      ##write components in output table
      i<-0
      k<-1
      while(i<=n.components*6){
        output.table[1,i+1]<-I0[k]
        output.table[1,i+2]<-I0.error[k]
        output.table[1,i+3]<-lambda[k]
        output.table[1,i+4]<-lambda.error[k]
        output.table[1,i+5]<-cs[k]
        output.table[1,i+6]<-cs.rel[k]
        i<-i+6
        k<-k+1
      }

      ##add pR and n.components
      output.table<-cbind(sample_code,n.components,output.table,pR)

      ##alter column names
      colnames(output.table)<-c("sample_code","n.components",
                                output.tableColNames,"pseudo-R^2")

      ##============================================================================##
      ## COMPONENT TO SUM CONTRIBUTION PLOT
      ##============================================================================##

      ##+++++++++++++++++++++++++++++++
      ##set matrix
      ##set polygon matrix for optional plot output
      component.contribution.matrix <- matrix(NA,
                                              nrow = length(values[,1]),
                                              ncol = (2*length(I0)) + 2)

      ##set x-values
      component.contribution.matrix[,1] <- values[,1]
      component.contribution.matrix[,2] <- rev(values[,1])

      ##+++++++++++++++++++++++++++++++
      ##set 1st polygon
      ##1st polygon (calculation)
      y.contribution_first<-(I0[1]*lambda[1]*exp(-lambda[1]*x))/(eval(fit.function))*100

      ##avoid NaN values (might happen with synthetic curves)
      y.contribution_first[is.nan(y.contribution_first)==TRUE] <- 0

      ##set values in matrix
      component.contribution.matrix[,3] <- 100
      component.contribution.matrix[,4] <- 100 - rev(y.contribution_first)

      ##+++++++++++++++++++++++++++++++
      ##set polygons in between
      ##polygons in between (calculate and plot)
      if (length(I0)>2){

        y.contribution_prev <- y.contribution_first
        i<-2

        ##matrix stepping
        k <- seq(3, ncol(component.contribution.matrix), by=2)

        while (i<=length(I0)-1) {

          y.contribution_next<-I0[i]*lambda[i]*exp(-lambda[i]*x)/(eval(fit.function))*100

          ##avoid NaN values
          y.contribution_next[is.nan(y.contribution_next)==TRUE] <- 0

          ##set values in matrix
          component.contribution.matrix[,k[i]] <- 100 - y.contribution_prev
          component.contribution.matrix[, k[i]+1] <- rev(100-y.contribution_prev-
                                                           y.contribution_next)

          y.contribution_prev <- y.contribution_prev + y.contribution_next

          i <- i+1

        }#end while loop
      }#end if

      ##+++++++++++++++++++++++++++++++
      ##set last polygon

      ##last polygon (calculation)
      y.contribution_last <- I0[length(I0)]*lambda[length(lambda)]*exp(-lambda[length(lambda)]*x)/
        (eval(fit.function))*100

      ##avoid NaN values
      y.contribution_last[is.nan(y.contribution_last)==TRUE]<-0

      component.contribution.matrix[,((2*length(I0))+1)] <- y.contribution_last
      component.contribution.matrix[,((2*length(I0))+2)] <- 0

      ##change names of matrix to make more easy to understand
      component.contribution.matrix.names <- c(
        "x", "rev.x",
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

    }#endif :: (exists("fit"))
  }

  ##============================================================================##
  ## PLOTTING
  ##============================================================================##
  if(plot==TRUE){

    ##grep par parameters
    par.default <- par()[c("mfrow", "cex", "mar", "omi", "oma")]
    on.exit(par(par.default), add = TRUE)

    ##set colors gallery to provide more colors
    col <- get("col", pos = .LuminescenceEnv)

    ##set plot frame
    par(cex = cex.global)
    if(!inherits(fit, "try-error")){
      graphics::layout(matrix(c(1, 2, 3), 3, 1, byrow = TRUE),
                       c(1.6, 1, 1), c(1, 0.3, 0.4), TRUE)
      par(oma = c(1, 1, 1, 1), mar = c(0, 4, 3, 0))
    }
    ##== upper plot ==##
    ##open plot area
    plot_check <- try(plot(NA, NA,
         xlim=c(min(x),max(x)),
         ylim = c(ifelse(log == "xy", 1, 0), max(y)),
         xlab = ifelse(inherits(fit, "try-error"), xlab, ""),
         xaxt = ifelse(inherits(fit, "try-error"), "s", "n"),
         ylab=ylab,
         main=main,
         log = log), silent = TRUE)

    if (inherits(plot_check, "try-error")) {
      ## reset the graphic device if plotting failed
      .throw_message("Figure margins too large or plot area too small, ",
                     "nothing plotted")
      grDevices::dev.off()
    } else {

    ##plotting measured signal
    points(x,y,pch=20, col="grey")

    ##add additional labeling (fitted function)
    mtext(side=3, sample_code, cex=0.7*cex.global)

    ##plot sum function
    if (!inherits(fit, "try-error")) {
      lines(x,eval(fit.function), lwd=2, col="black")
      legend.caption<-"sum curve"
      curve.col <- 1

      ##plot signal curves

      ##plot curve for additional parameters
      if(length(I0)>1){

        for (i in 1:length(I0)) {
          curve(I0[i]*lambda[i]*exp(-lambda[i]*x),col=col[i+1],
                lwd = 2,
                add = TRUE)
          legend.caption<-c(legend.caption,paste("component ",i,sep=""))
          curve.col<-c(curve.col,i+1)
        }
      }#end if
      ##plot legend
      #legend(y=max(y)*1,"measured values",pch=20, col="gray", bty="n")
      legend("topright",legend.caption,lty=rep(1,n.components+1,NA),lwd=2,col=col[curve.col], bty="n")

      ##==lower plot==##
      ##plot residuals
      par(mar=c(4.2,4,0,0))
      plot_check2 <- try(plot(x,residuals(fit),
           xlim=c(min(x),max(x)),
           xlab=xlab,
           type="l",
           col="grey",
           ylab="Residual [a.u.]",
           lwd=2,
           log=if(log=="x" | log=="xy"){log="x"}else{""}
      ), silent = TRUE)

      if (inherits(plot_check2, "try-error")) {
        ## reset the graphic device if plotting failed
        .throw_message("Figure margins too large or plot area too small, ",
                       "nothing plotted")
        grDevices::dev.off()
      } else {

      ##add 0 line
      abline(h=0)

      ##------------------------------------------------------------------------##
      ##++component to sum contribution plot ++##
      ##------------------------------------------------------------------------##

      ##plot component contribution to the whole signal
      #open plot area
      par(mar=c(4,4,3.2,0))
      plot(NA,NA,
           xlim=c(min(x),max(x)),
           ylim=c(0,100),
           ylab="Contribution [%]",
           xlab=xlab,
           main="Component contribution to sum curve",
           log=if(log=="x" | log=="xy"){log="x"}else{""})

      stepping <- seq(3,length(component.contribution.matrix[1,]),2)

      for(i in 1:length(I0)){

        polygon(c(component.contribution.matrix[,1],
                  component.contribution.matrix[,2]),
                c(component.contribution.matrix[,stepping[i]],
                  component.contribution.matrix[,stepping[i]+1]),
                col = col[i+1])
      }
      rm(stepping)
      } # end if (plot_check2)

    } else {
      if (verbose)
        .throw_message("Fitting failed, plot without fit produced")
    }#end if try-error for fit

    } # end if (plot_check)
  }

  ##============================================================================##
  ## Return Values
  ##============================================================================##

  if (!method_control$export.comp.contrib.matrix) {
    component.contribution.matrix <- NA
  }
  newRLumResults.fit_CWCurve <- set_RLum(
    class = "RLum.Results",
    data = list(
      data = output.table,
      fit = fit,
      component.contribution.matrix = list(component.contribution.matrix)
    ),
    info = list(call = sys.call())
  )

  rm(fit)
  rm(output.table)
  rm(component.contribution.matrix)

  invisible(newRLumResults.fit_CWCurve)
}

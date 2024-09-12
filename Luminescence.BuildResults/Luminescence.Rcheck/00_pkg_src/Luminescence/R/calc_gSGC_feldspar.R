#'@title Calculate Global Standardised Growth Curve (gSGC) for Feldspar MET-pIRIR
#'
#'@description Implementation of the gSGC approach for feldspar MET-pIRIR by Li et al. (2015)
#'
#'@details ##TODO
#'
#'@param data [data.frame] (**required**): data frame with five columns per sample
#'`c("LnTn", "LnTn.error", "Lr1Tr1", "Lr1Tr1.error","Dr1")`
#'
#'@param gSGC.type [character] (*with default*): growth curve type to be selected
#'according to Table 3 in Li et al. (2015). Allowed options are
#'`"50LxTx"`, `"50Lx"`, `"50Tx"`, `"100LxTx"`, `"100Lx"`, `"100Tx"`, `"150LxTx"`,
#' `"150Lx"`, `"150Tx"`, `"200LxTx"`, `"200Lx"`, `"200Tx"`, `"250LxTx"`, `"250Lx"`,
#' `"250Tx"`
#'
#'@param gSGC.parameters [data.frame] (*optional*): an own parameter set for the
#'gSGC with the following columns `y1`, `y1_err`, `D1`
#'`D1_err`, `y2`, `y2_err`, `y0`, `y0_err`.
#'
#'@param n.MC [numeric] (*with default*): number of Monte-Carlo runs for the
#'error calculation
#'
#'@param plot [logical] (*with default*): enables/disables the control plot output
#'
#'@return Returns an S4 object of type [RLum.Results-class].
#'
#' **`@data`**\cr
#' `$ df` ([data.frame]) \cr
#'  `.. $DE` the calculated equivalent dose\cr
#'  `.. $DE.ERROR` error on the equivalent dose, which is the standard deviation of the MC runs\cr
#'  `.. $HPD95_LOWER` lower boundary of the highest probability density (95%)\cr
#'  `.. $HPD95_UPPER` upper boundary of the highest probability density (95%)\cr
#' `$ m.MC` ([list]) numeric vector with results from the MC runs.\cr
#'
#' **`@info`**\cr
#' `$ call`` ([call]) the original function call
#'
#' @section Function version: 0.1.0
#'
#' @author Harrison Gray, USGS (United States),
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [RLum.Results-class], [get_RLum], [uniroot], [calc_gSGC]
#'
#' @references Li, B., Roberts, R.G., Jacobs, Z., Li, S.-H., Guo, Y.-J., 2015.
#' Construction of a “global standardised growth curve” (gSGC) for infrared
#' stimulated luminescence dating of K-feldspar 27, 119–130. \doi{10.1016/j.quageo.2015.02.010}
#'
#' @keywords datagen
#'
#' @examples
#'
#' ##test on a generated random sample
#' n_samples <- 10
#' data <- data.frame(
#'   LnTn = rnorm(n=n_samples, mean=1.0, sd=0.02),
#'   LnTn.error = rnorm(n=n_samples, mean=0.05, sd=0.002),
#'   Lr1Tr1 = rnorm(n=n_samples, mean=1.0, sd=0.02),
#'   Lr1Tr1.error = rnorm(n=n_samples, mean=0.05, sd=0.002),
#'   Dr1 = rep(100,n_samples))
#'
#' results <- calc_gSGC_feldspar(
#'   data = data, gSGC.type = "50LxTx",
#'   plot = FALSE)
#'
#' plot_AbanicoPlot(results)
#'
#'@md
#'@export
calc_gSGC_feldspar <- function (
  data,
  gSGC.type = "50LxTx",
  gSGC.parameters,
  n.MC = 100,
  plot = FALSE
){

# Integrity checks --------------------------------------------------------
  if (!is(data, "data.frame")) {
    stop("[calc_gSGC_feldspar()] 'data' needs to be of type data.frame.", call. = FALSE)
  }
  if (!is(gSGC.type[1], "character")) {
    stop("[calc_gSGC_feldspar()] 'gSGC.type' needs to be of type character.", call. = FALSE)
  }
  if (ncol(data) != 5) {
    stop("[calc_gSGC_feldspar()] Structure of 'data' does not fit the expectations.", call. = FALSE)
  }
  colnames(data) <- c("LnTn", "LnTn.error", "Lr1Tr1", "Lr1Tr1.error",
                      "Dr1")

# Parametrize -------------------------------------------------------------
  params <- data.frame( # this is the data from Table 3 of Li et al., 2015
    Type = c("50LxTx", "50Lx", "50Tx", "100LxTx", "100Lx", "100Tx", "150LxTx", "150Lx", "150Tx", "200LxTx", "200Lx", "200Tx", "250LxTx", "250Lx", "250Tx"),
    y1 = c( 0.57, 0.36, 0.2, 0.39, 0.41, 0.28, 0.43, 0.4, 0.31, 0.3, 0.34, 0.37, 0.37, 0.17, 0.48),
    y1_err = c( 0.19, 0.25, 0.24, 0.12, 0.28, 0.22, 0.11, 0.27, 0.33, 0.06, 0.28, 0.28, 0.1, 0.12, 0.37),
    D1 = c( 241, 276, 259, 159, 304, 310, 177, 327, 372, 119, 316, 372, 142, 197, 410),
    D1_err = c( 66, 137, 279, 48, 131, 220, 41, 132, 300, 32, 145, 218, 35, 116, 210),
    y2 = c( 0.88, 1.37, 0.34, 0.91, 1.22, 0.42, 0.88, 1.26, 0.45, 0.95, 1.24, 0.43, 0.74, 1.32, 0.45),
    y2_err = c( 0.15, 0.19, 0.15, 0.1, 0.23, 0.26, 0.09, 0.23, 0.18, 0.05, 0.25, 0.24, 0.09, 0.1, 0.15),
    D2 = c( 1115, 1187, 1462, 741, 1146, 2715, 801, 1157, 2533, 661, 1023, 2792, 545, 830, 2175),
    D2_err = c( 344, 287, 191, 105, 288, 639, 109, 263, 608, 49, 205, 709, 62, 79, 420),
    y0 = c( 0.008, 0.003, 0.685, 0.018, 0.01, 0.64, 0.026, 0.015, 0.61, 0.034, 0.02, 0.573, 0.062, 0.028, 0.455),
    y0_err = c( 0.009, 0.009, 0.014, 0.008, 0.008, 0.015, 0.006, 0.007, 0.014, 0.006, 0.006, 0.013, 0.005, 0.005, 0.011),
    D0_2.3 = c( 2000, 2450, 1420, 1420, 2300, 2900, 1500, 2340, 2880, 1320, 2080, 2980, 1000, 1780, 2500),
    D0_3 = c( 2780, 3280, 2520, 1950, 3100, 4960, 2060, 3130, 4760, 1780, 2800, 5120, 1380, 2360, 4060)
  )

  # these are user specified parameters if they so desire
  if (!missing(gSGC.parameters)){
    y1 <- gSGC.parameters$y1
    y1_err <- gSGC.parameters$y1_err
    D1 <- gSGC.parameters$D1
    D1_err <- gSGC.parameters$D1_err
    y2 <- gSGC.parameters$y2
    y2_err <- gSGC.parameters$y2_err
    y0 <- gSGC.parameters$y0
    y0_err <- gSGC.parameters$y0_err

  } else {
   if (gSGC.type[1] %in% params$Type){
   # take the user input pIRSL temperature and assign the correct parameters
   index <- match(gSGC.type,params$Type)

      y1 <- params$y1[index]
      y1_err <- params$y1_err[index]

      D1 <- params$D1[index]
      D1_err <- params$D1_err[index]

      y2 <- params$y2[index]
      y2_err <- params$y2_err[index]

      D2 <- params$D2[index]
      D2_err <- params$D2_err[index]

      y0 <- params$y0[index]
      y0_err <- params$y0_err[index]

    } else {
      # give error if input is wrong
      stop(
        paste0("[calc_gSGC_feldspar()] 'gSGC.type' needs to be one of the accepted values, such as: ",
          paste(params$Type, collapse = ", ")),
        call. = FALSE)
    }
  }

  ##set function for uniroot
  ## function from Li et al., 2015 eq: 3
  ## function that equals zero when the correct De is found.
  ## This is so uniroot can find the correct value or 'root'
  f <- function(De, Dr1, Lr1Tr1, LnTn, y1, D1, y2, D2, y0){
    f_D <-  y1 * (1 - exp(-De / D1)) + y2 * (1 - exp(-De / D2)) + y0
    f_Dr <-  y1 * (1 - exp(-Dr1 / D1)) + y2 * (1 - exp(-Dr1 / D2)) + y0
    ##return(f_D/Lr1Tr1 - f_Dr/LnTn) ##TODO double check seems to be wrong
    return(f_Dr/Lr1Tr1 - f_D/LnTn)

  }

# Run calculation ---------------------------------------------------------
  l <- lapply(1:nrow(data), function(i) {
    Lr1Tr1 <- data[i, "Lr1Tr1"] #assign user's input data
    Lr1Tr1.error <- data[i, "Lr1Tr1.error"]
    Dr1 <- data[i, "Dr1"]
    LnTn <- data[i, "LnTn"]
    LnTn.error <- data[i, "LnTn.error"]

    ## uniroot solution
    temp <- try({
        uniroot(
          f,
          interval = c(0.1, 3000),
          tol = 0.001,
          Dr1 = Dr1,
          Lr1Tr1 = Lr1Tr1,
          LnTn = LnTn,
          y1 = y1,
          D1 = D1,
          y2 = y2,
          D2 = D2,
          y0 = y0,
          extendInt = "yes",
          check.conv = TRUE,
          maxiter = 1000)
       }, silent = TRUE) # solve for the correct De

      ## in case the initial uniroot solve does not work
      if(inherits(temp, "try-error")) {
        message("[calc_gSGC_feldspar()] Error: No solution found for ",
                "dataset #", i, ", NA returned")
        return(NA)
      }

      De <- temp$root
      temp.MC.matrix <- matrix(nrow = n.MC, ncol = 8)

        # to estimate the error, use a monte carlo simulation. assume error in input data is gaussian
        # create a matrix
        colnames(temp.MC.matrix) <- c("LnTn", "Lr1Tr1","y1", "D1", "y2", "D2", "y0", "De")

        # simulate random values for each parameter
        temp.MC.matrix[, 1:7] <- matrix(
          rnorm(n.MC * 7,
            mean = c(LnTn, Lr1Tr1, y1, D1, y2, D2, y0),
            sd = c(LnTn.error, Lr1Tr1.error, y1_err, D1_err, y2_err, D2_err, y0_err)),
          ncol = 7,
          byrow = TRUE)

        # now use the randomly generated parameters to calculate De's with uniroot
        for (j in 1:n.MC){
          temp2 <- try({
              uniroot(
                f,
                interval = c(0.1, 3000),
                tol = 0.001,
                LnTn = temp.MC.matrix[j, 1],
                Lr1Tr1 =  temp.MC.matrix[j, 2],
                y1 = temp.MC.matrix[j, 3],
                D1 = temp.MC.matrix[j, 4],
                y2 = temp.MC.matrix[j, 5],
                D2 = temp.MC.matrix[j, 6],
                y0 = temp.MC.matrix[j, 7],
                Dr1 = Dr1,
                extendInt = "yes",
                check.conv = TRUE,
                maxiter = 1000
              )
            }, silent = TRUE)

          if (!inherits(temp2, "try-error")){
            temp.MC.matrix[j,8] <- temp2$root

          } else {
            # give an NA if uniroot cannot find a root (usually due to bad random values)
            temp.MC.matrix[j,8] <- NA
          }

        }

        # set the De uncertainty as the standard deviations of the randomly generated des
        De.error <- sd(temp.MC.matrix[, 8], na.rm = TRUE)

        return(list(
          DE = De,
          DE.ERROR = De.error,
          m.MC = temp.MC.matrix))
    })

# Plotting ----------------------------------------------------------------
  if(plot){
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))

    par(mfrow = c(mfrow = c(3,3)))
    for (i in 1:length(l)) {
      if(is.na(l[[i]][1])) next();

      y_max <- max(l[[i]]$m.MC[, 1:2])
      plot(NA, NA,
           xlab = "Dose [a.u.]",
           ylab = "Norm. Signal",
           xlim = c(0, 3000),
           main = paste0("Dataset #", i),
           ylim = c(0, y_max)
           )

      for(j in 1:nrow(l[[i]]$m.MC)){
        #y1 * (1 - exp(-De / D1)) + y2 * (1 - exp(-De / D2)) + y0
        x <- NA
        curve(
           l[[i]]$m.MC[j, 3] * (1 - exp(-x  / l[[i]]$m.MC[j, 4])) +
           l[[i]]$m.MC[j, 5] * (1 - exp(-x  / l[[i]]$m.MC[j, 6])) +
           l[[i]]$m.MC[j, 7],
        col = rgb(0,0,0,0.4),
        add = TRUE)
      }

       par(new = TRUE)
       hist <- hist(na.exclude(l[[i]]$m.MC[, 8]),
           plot = FALSE
         )

       hist$counts <- ((y_max/max(hist$counts)) * hist$counts) / 2
       plot(
         hist,
         xlab = "",
         ylab = "",
         axes = FALSE,
         xlim = c(0, 3000),
         ylim = c(0, y_max),
         main = ""
       )

    }

  }


# Return ------------------------------------------------------------------

  ##output matrix
  m <- matrix(ncol = 4, nrow = nrow(data))

  ##calculate a few useful parameters
  for(i in 1:nrow(m)){
    if(is.na(l[[i]][1])) next();

    m[i,1] <- l[[i]]$DE
    m[i,2] <- l[[i]]$DE.ERROR

    HPD <- .calc_HPDI(na.exclude(l[[i]]$m.MC[,8]))
    m[i,3] <- HPD[1,1]
    m[i,4] <- HPD[1,2]

  }

  df <- data.frame(
    DE = m[, 1],
    DE.ERROR = m[, 2],
    HPD95_LOWER = m[, 3],
    HPD95_UPPER = m[, 4]
  )

  return(
    set_RLum("RLum.Results",
      data = list(
        data = df,
        m.MC = lapply(l, function(x) {if(is.na(x[[1]])) {return(x)} else {x$m.MC} })
      ),
      info = list(
        call = sys.call()
      )
    ))

}

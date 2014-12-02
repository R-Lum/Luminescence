calc_Statistics <- structure(function(# Function to calculate statistic measures
  ### This function calculates a number of descriptive statistics for De-data,
  ### most fundamentally using error-weighted approaches.
  
  # ===========================================================================
  ##author<<
  ## Michael Dietze, GFZ Potsdam (Germany),\cr
  
  ##section<<
  ## version 0.1
  # ===========================================================================
  
  data,
  ### \code{\link{data.frame}} or \code{\linkS4class{RLum.Results}} object 
  ### (required): for \code{data.frame} two columns: De (\code{data[,1]})
  ### and De error (\code{data[,2]}). To plot several data sets in one plot
  ### the data sets must be provided as \code{list}, e.g. 
  ### \code{list(data.1, data.2)}.
  
  weight.calc = "reciprocal"
  ### \code{\link{character}}: type of weight calculation. One out of
  ### \code{"reciprocal"} (weight is 1/error), \code{"square"} (weight is
  ### 1/error^2).
) {
  ## Check input data
  if(is(data, "RLum.Results") == FALSE & 
       is(data, "data.frame") == FALSE) {
    stop(paste("[calc_Statistics] Error: Input data format is neither",
               "'data.frame' nor 'RLum.Results'"))
  } else {
    if(is(data, "RLum.Results") == TRUE) {
      data <- get_RLum.Results(data)[,1:2]
    }
  }

  ## check/set weight calculation type
  if(sum(data[,2]) == 0) {
    warning("All errors are zero! Automatically set to 10^-9!")
    data[,2] <- rep(10^-9, length(data[,2]))
  }
  
  if(weight.calc == "reciprocal") {
    S.weights <- 1 / data[,2]
  } else if(weight.calc == "square") {
    S.weights <- 1 / data[,2]^2
  } else {
    stop ("[calc_Statistics] Error: Weight calculation type not supported!")
  }
  
  S.weights <- S.weights / sum(S.weights)
  
  ## define function after isotone::weighted.median
  median.w <- function (y, w)
  {
    ox <- order(y)
    y <- y[ox]
    w <- w[ox]
    k <- 1
    low <- cumsum(c(0, w))
    up <- sum(w) - low
    df <- low - up
    repeat {
      if (df[k] < 0) 
        k <- k + 1
      else if (df[k] == 0) 
        return((w[k] * y[k] + w[k - 1] * y[k - 1]) / (w[k] + w[k - 1]))
      else return(y[k - 1])
    }
  }
  
  ## calculate n
  S.n <- nrow(data)
  
  ## calculate mean
  S.mean <- mean(x = data[,1], 
                 na.rm = TRUE)
  
  S.wg.mean <- weighted.mean(x = data[,1],
                             w = S.weights,
                             n.rm = TRUE)
  
  ## calculate median
  S.median <- median(x = data[,1],
                     na.rm = TRUE)
  
  S.wg.median <- median.w(y = data[,1],
                          w = S.weights)
  
  ## calculate absolute standard deviation
  S.sd.abs <- sd(x = data[,1],
                 na.rm = TRUE)
  
  S.wg.sd.abs <- sqrt(sum(S.weights * (data[,1] - S.wg.mean)^2) / 
                        (((S.n - 1) * sum(S.weights)) / S.n))
  
  ## calculate relative standard deviation
  S.sd.rel <- S.sd.abs / S.mean * 100

  S.wg.sd.rel <- S.wg.sd.abs / S.wg.mean * 100
  
  ## calculate absolute standard error of the mean
  S.se.abs <- S.sd.abs / sqrt(S.n)
  
  S.wg.se.abs <- S.wg.sd.abs / sqrt(S.n)  
  
  ## calculate relative standard error of the mean
  S.se.rel <- S.se.abs / S.mean * 100
  
  S.wg.se.rel <- S.wg.se.abs / S.wg.mean * 100
  
  ## calculate skewness
  S.skewness <- 1 / S.n * sum(((data[,1] - S.mean) / S.sd.abs)^3)
  
  ## calculate kurtosis
  S.kurtosis <- 1 / S.n * sum(((data[,1] - S.mean) / S.sd.abs)^4)
  
  ## create list objects of calculation output
  S.weighted <- list(n = S.n,
                     mean = S.wg.mean,
                     median = S.wg.median,
                     sd.abs = S.sd.abs,
                     sd.rel = S.sd.rel,
                     se.abs = S.se.abs,
                     se.rel = S.se.rel)

  S.unweighted <- list(n = S.n,
                       mean = S.mean,
                       median = S.median,
                       sd.abs = S.wg.sd.abs,
                       sd.rel = S.wg.sd.rel,
                       se.abs = S.wg.se.abs,
                       se.rel = S.wg.se.rel,
                       skewness = S.skewness,
                       kurtosis = S.kurtosis)

  list(weighted = S.weighted,
       unweighted = S.unweighted)
  ### Returns a list with weighted and unweighted statistic measures.

}, ex=function(){
  ## load example data
  data(ExampleData.DeValues, envir = environment())

  ## show a rough plot of the data to illustrate the non-normal distribution
  plot_KDE(ExampleData.DeValues)
  
  ## calculate statistics and show output
  str(calc_Statistics(ExampleData.DeValues))
  
  ## now the same for 10000 normal distributed random numbers with equal errors
  x <- as.data.frame(cbind(rnorm(n = 10^5, mean = 0, sd = 1),
                           rep(0.001, 10^5)))
  
  ## note the congruent results for weighted and unweighted measures
  str(calc_Statistics(x)) 
})
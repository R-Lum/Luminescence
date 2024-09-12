#' @title Probabilistic Numerical Modelling of Sediment Properties
#' \cr
#' 
#' \if{html}{
#' \figure{logo_sandbox.png}{options: width="50" alt="sandbox logo"}
#' }
#' 
#' @description Flexible framework for definition and application of time/depth-based 
#' rules for sets of parameters for single grains that can be used to 
#' create synthetic samples, used for synthetic preparation and synthetic 
#' measurements.
#' 
#' @md
#' @name sandbox-package
#' @aliases sandbox
#' @docType package
#' @author Michael Dietze (GFZ Potsdam, Germany), 
#'  Sebastian Kreutzer (Geography & Earth Sciences, Aberystwyth University, United Kingdom)
#' @keywords package
#' @importFrom stats rnorm runif rgamma splinefun
#' @importFrom methods is
NULL

#' Example Grain Size Data
#' 
#' Example data set of a virtual loess-like sample.
#' 
#' The sample was created using the rule book book_1, a depth of 5 m and
#' a cuboid sample geometry with 2 mm edge length.
#' 
#' @name sample
#' @docType data
#' @format The format is: 'data.frame':	1000 obs. of  12 variables:
#' $ ID               : int  33107 33108 33109 33110 33111 33112 ...
#' $ depth            : num  5 5 5 5 5 ...
#' $ population       : num  3 1 3 2 1 3 1 3 3 3 ...
#' $ age              : num  25711 25710 25712 25709 25710 ...
#' $ dose_rate        : num  7.163 -1.083 -0.929 3.541 5.732 ...
#' $ water_content    : num  13.29 10.99 3.65 8.98 3.29 ...
#' $ population       : num  0.3 0.586 0.3 0.114 0.586 ...
#' $ grainsize        : num  4.01 6.22 5.16 5.47 5.57 ...
#' $ density          : num  1.92 1.91 1.9 1.88 1.9 ...
#' $ packing          : num  0.708 0.702 0.698 0.702 0.688 ...
#' $ photon_equivalent: num  1.017 0.993 1.005 1 0.995 ...
#' $ predose          : num  2020 3106 1983 191 2387 ...
#' @keywords datasets
#' @examples
#' 
#' ## load example data set
#' data(sample, envir = environment())
#' 
#' ## plot grain-size distribution
#' plot(density(sample$grainsize))
#' 
NULL

#' Aliquots Prepared to Measured Virtually
#' 
#' Example data of virtually prepared aliquots 
#' ready to be measured
#' 
#' @name sample_osl_aliquots
#' @docType data
#' @format The format is: 'data.frame':	2 obs. of  65 variables:
#' ..$ grains      : num [1:2] 1 2
#' ..$ d_sample    : num [1:2] 2 2
#' ..$ population  : num [1:2] 1 1
#' ..$ age         : num [1:2] 1574 1578
#' ..$ population  : num [1:2] 2 2
#' ..$ grainsize   : num [1:2] 2.52 2.48
#' ..$ packing     : num [1:2] 1.32 4.82
#' ..$ density     : num [1:2] 3.24 2.13
#' ..$ osl_doserate: num [1:2] 0.00875 0.0046
#' ..$ osl_N1      : num [1:2] 1.5e+07 1.5e+07
#' ..$ osl_N2      : num [1:2] 1e+07 1e+07
#' ..$ osl_N3      : num [1:2] 1e+09 1e+09
#' ..$ osl_N4      : num [1:2] 2.5e+08 2.5e+08
#' ..$ osl_N5      : num [1:2] 5e+10 5e+10
#' ..$ osl_N6      : num [1:2] 3e+08 3e+08
#' ..$ osl_N7      : num [1:2] 1e+10 1e+10
#' ..$ osl_N8      : num [1:2] 5e+09 5e+09
#' ..$ osl_N9      : num [1:2] 1e+11 1e+11
#' ..$ osl_E1      : num [1:2] 0.97 0.97
#' ..$ osl_E2      : num [1:2] 1.55 1.55
#' ..$ osl_E3      : num [1:2] 1.7 1.7
#' ..$ osl_E4      : num [1:2] 1.72 1.72
#' ..$ osl_E5      : num [1:2] 2 2
#' ..$ osl_E6      : num [1:2] 1.43 1.43
#' ..$ osl_E7      : num [1:2] 1.75 1.75
#' ..$ osl_E8      : num [1:2] 5 5
#' ..$ osl_E9      : num [1:2] 5 5
#' ..$ osl_s1      : num [1:2] 5e+12 5e+12
#' ..$ osl_s2      : num [1:2] 5e+14 5e+14
#' ..$ osl_s3      : num [1:2] 5e+13 5e+13
#' ..$ osl_s4      : num [1:2] 5e+14 5e+14
#' ..$ osl_s5      : num [1:2] 1e+10 1e+10
#' ..$ osl_s6      : num [1:2] 5e+13 5e+13
#' ..$ osl_s7      : num [1:2] 5e+14 5e+14
#' ..$ osl_s8      : num [1:2] 1e+13 1e+13
#' ..$ osl_s9      : num [1:2] 1e+13 1e+13
#' ..$ osl_A1      : num [1:2] 1e-08 1e-08
#' ..$ osl_A2      : num [1:2] 1e-08 1e-08
#' ..$ osl_A3      : num [1:2] 1e-09 1e-09
#' ..$ osl_A4      : num [1:2] 5e-10 5e-10
#' ..$ osl_A5      : num [1:2] 1e-10 1e-10
#' ..$ osl_A6      : num [1:2] 5e-07 5e-07
#' ..$ osl_A7      : num [1:2] 1e-09 1e-09
#' ..$ osl_A8      : num [1:2] 1e-10 1e-10
#' ..$ osl_A9      : num [1:2] 1e-09 1e-09
#' ..$ osl_B1      : num [1:2] 0 0
#' ..$ osl_B2      : num [1:2] 0 0
#' ..$ osl_B3      : num [1:2] 0 0
#' ..$ osl_B4      : num [1:2] 0 0
#' ..$ osl_B5      : num [1:2] 0 0
#' ..$ osl_B6      : num [1:2] 5e-09 5e-09
#' ..$ osl_B7      : num [1:2] 5e-10 5e-10
#' ..$ osl_B8      : num [1:2] 1e-10 1e-10
#' ..$ osl_B9      : num [1:2] 1e-10 1e-10
#' ..$ osl_Th1     : num [1:2] 0.75 0.75
#' ..$ osl_Th2     : num [1:2] 0 0
#' ..$ osl_Th3     : num [1:2] 6 6
#' ..$ osl_Th4     : num [1:2] 4.5 4.5
#' ..$ osl_Th5     : num [1:2] 0 0
#' ..$ osl_E_th1   : num [1:2] 0.1 0.1
#' ..$ osl_E_th2   : num [1:2] 0 0
#' ..$ osl_E_th3   : num [1:2] 0.1 0.1
#' ..$ osl_E_th4   : num [1:2] 0.13 0.13
#' ..$ osl_E_th5   : num [1:2] 0 0
#' ..$ osl_R       : num [1:2] 5e+07 5e+07
#' @keywords datasets
#' @examples
#' 
#' ## load example data set
#' data(sample_osl_aliquots, envir = environment())
#' 
#' ## plot grain-size distribution
#' plot(density(sample_osl_aliquots[[1]]$age))
#' 
NULL



#' Set parameters for different quartz luminescence models
#'
#' This function provides all necessary model parameters to the calculation of the ODEs.
#'
#' The common model parameters are:
#'
#' \bold{N}: concentrations of electron/hole traps [cm^(-3)]
#' \bold{E}: depth of the electron/hole trap [eV]
#' \bold{s}: frequency factor [s^(-1)]
#' \bold{A}: conduction band to electron/hole trap transition probability [s^(-1)]
#' \bold{B}: valence band to hole trap transition probability [s^(-1)]
#' \bold{Th}: photo-ionisation cross-section [s^(-1)]
#' \bold{E_th}: `thermal assistance' energy [eV]
#' \bold{n}: concentrations of electron/hole traps after sample history [cm^(-3)]
#'
#' @note \bold{n} are the saved concentrations of the last step of the sample history
#' of the used model. They will be loaded, if 'simulate_sample_history = FALSE' in
#' \code{\link{model_LuminescenceSignals}} is chosen.
#'
#'
#' @param model \code{\link{character}} (\bold{required}): set model to be used.
#' Available models are:
#' "Bailey2001", "Bailey2002", "Bailey2004", "Pagonis2007", "Pagonis2008"
#'
#' @return This function returns a \code{\link{list}} with all neccessary parameters for
#' the used model.
#'
#' @note The order of the energy-band-levels is sometimes in an different order than in the original model.
#' This was necessary, because
#' in the simulations the luminescence center always has to be the last entry in every parameter.
#' Another reason was the clear division between electron traps and hole centers.
#' When a user wants to create his/her own parameter sets he/she only has to take care that the luminescence center is the last
#' entry in every vector and that the first entries are the electron traps and the last entries the hole centres.
#'
#' @section Function version: 0.1.3 [2017-11-20]
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#'
#' @references
#'
#' Bailey, R.M., 2001. Towards a general kinetic model for optically and thermally stimulated
#' luminescence of quartz. Radiation Measurements 33, 17-45.
#'
#' Bailey, R.M., 2002. Simulations of variability in the luminescence characteristics of natural
#' quartz and its implications for estimates of absorbed dose.
#' Radiation Protection Dosimetry 100, 33-38.
#'
#' Bailey, R.M., 2004. Paper I-simulation of dose absorption in quartz over geological timescales
#' and it simplications for the precision and accuracy of optical dating.
#' Radiation Measurements 38, 299-310.
#' 
#' Friedrich, J., Pagonis, V., Chen, R., Kreutzer, S., Schmidt, C., 2017: Quartz radiofluorescence: a modelling approach.
#' Journal of Luminescence 186, 318-325.
#'
#' Pagonis, V., Chen, R., Wintle, A.G., 2007: Modelling thermal transfer in optically
#' stimulated luminescence of quartz. Journal of Physics D: Applied Physics 40, 998-1006.
#'
#' Pagonis, V., Wintle, A.G., Chen, R., Wang, X.L., 2008. A theoretical model for a new dating protocol
#' for quartz based on thermally transferred OSL (TT-OSL).
#' Radiation Measurements 43, 704-708.
#'
#' @examples
#'
#' pars <- .set_pars("Bailey2001")
#'
#' @noRd
.set_pars <- function(model){

# check input arguments ---------------------------------------------------

  #Check if model is supported
  model.allowed_keywords <- c("Bailey2001", 
                              "Bailey2004", 
                              "Pagonis2008", 
                              "Pagonis2007", 
                              "Bailey2002", 
                              "Friedrich2017", 
                              "Friedrich2018",
                              "customized", 
                              "customised")

  if(!model%in%model.allowed_keywords){
    stop(paste0("[.set_Pars()] Model not supported. Supported models are: ", paste(model.allowed_keywords, collapse = ", ")))
  }

  ##============================================================================##
  ## natural constants
  ##============================================================================##

  # Boltzmann constant [eV/K]
  k_B <- 8.617e-5

  # activiation Energy [eV]
  W <- 0.64

  # dimensionless constant (for Details see Wintle (1975))
  K <- 2.8e7

  parameter.list = list(

    Bailey2001 = list(

      N = c(1.5e7, 1e7, 1e9, 2.5e8, 5e10, 3e8, 1e10, 5e9, 1e11),
      E = c(0.97, 1.55, 1.7, 1.72, 2, 1.43, 1.75, 5, 5),
      s = c(5e12, 5e14, 5e13, 5e14, 1e10, 5e13, 5e14, 1e13, 1e13),
      A = c(1e-8, 1e-8, 1e-9, 5e-10, 1e-10, 5e-7, 1e-9, 1e-10, 1e-9),
      B = c(0, 0, 0, 0, 0, 5e-9, 5e-10, 1e-10, 1e-10),
      Th = c(0.75, 0, 6, 4.5, 0),
      E_th = c(0.1, 0, 0.1, 0.13, 0),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(9.169767e-03, 7.619894e+04, 1.291564e+08, 7.432290e+06, 2.690423e+10, 5.741230e+06, 6.779304e+07, 1.591234e+08, 2.680824e+10, 2.450977e-07, 4.263486e-07),
      temp = 20)),
      k_B = k_B,
      W = W,
      K = K,
      model = model
      ),

    Bailey2002 = list(

      N = c(9e9, 1e9, 1.5e11, 5e10, 1e11, 1.5e11, 2e12, 5e12, 1.2e11, 1e12, 5e11, 1e13),
      E = c(0.97, 1.55, 1.7, 1.72, 1.8, 1.65, 2.6, 2, 1.43, 1.75, 5, 5),
      s = c(5e12, 5e14, 5e12, 5e13, 5e13, 5e13, 5e13, 1e10, 5e13, 5e14, 1e13, 1e13),
      A = c(1e-8, 1e-8, 1e-9, 8e-10, 8e-10, 5e-10, 2e-10, 1e-10, 5e-8, 1e-9, 1e-10, 1e-9),
      B = c(0, 0, 0, 0, 0, 0, 0, 0, 5e-9, 5e-10, 1e-10, 1e-10),
      Th = c(1e-19, 0, 1e-16, 3e-17, 4e-18, 3e-19, 2e-21, 0),
      E_th = c(0.1, 0, 0.1, 0.13, 0.2, 0.2, 0.2,0),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(1.304051e+01, 2.474416e+07, 3.791273e+10, 9.871927e+09, 2.088693e+10, 1.506005e+10, 1.989412e+12, 4.962146e+12, 2.696001e+09, 5.054229e+10, 8.974516e+10, 6.892331e+12, 1.266365e-06, 3.534302e-06), 
      temp = 20)),
      k_B = k_B,
      W = W,
      K = K,
      model = model
    ),

    Bailey2004 = list(

      N = c(1.42e10, 1.5e9, 2.05e11, 7.04e10, 1.77e11, 2.53e11, 3.58e12, 1.28e13, 4.16e12, 4.2e11, 1.15e14, 8.83e13),
      E = c(0.97, 1.55, 1.7, 1.72, 1.8, 1.65, 2.6, 2, 1.75, 1.43, 5, 5),
      s = c(5e12, 5e14, 5e12, 5e13, 5e13, 5e13, 5e13, 1e10, 5e14,  5e13, 1e13, 1e13),
      A = c(1e-8, 1e-8, 1e-9, 8e-10, 8e-10, 5e-10, 2e-10, 1e-10, 1e-9, 5e-8, 1e-10, 1e-9),
      B = c(0, 0, 0, 0, 0, 0, 0, 0, 5e-10, 5e-9, 1e-10, 1e-10),
      Th = c(1e-19, 0, 1e-16, 3e-17, 4e-18, 3e-19, 2e-21, 0),
      E_th = c(0.1, 0, 0.1, 0.13, 0.2, 0.2, 0.2, 0),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(5.150601e+02, 1.164635e+09, 4.270964e+10, 1.201018e+10, 5.712265e+10, 2.065021e+11, 3.573812e+12, 1.280000e+13, 1.496238e+11, 1.214026e+10, 2.194172e+12,1.433739e+13, 3.743729e-04, 8.625906e-06), 
      temp = 20)),
      k_B = k_B,
      W = W,
      K = K,
      model = model
    ),

    Pagonis2007 = list(

      N = c(5.1e9, 1e7, 1e11, 2.5e8, 5e10, 3e8, 1e10, 5e9, 1e8),
      E = c(0.97, 1.55, 1.73, 1.8, 2, 1.43, 1.75, 5, 5),
      s = c(5e12, 5e14, 36e13, 1.5e13, 1e10, 5e13, 5e14, 1e13, 1e13),
      A = c(1e-8, 1e-9, 0.5e-9, 5e-10, 1e-10, 5e-7, 1e-9, 1e-10, 1e-9),
      B = c(0, 0, 0, 0, 0, 5e-9, 5e-10, 1e-10, 1e-10),
      Th = c(0.75, 0, 6, 4.5, 0),
      E_th = c(0.1, 0, 0.1, 0.13, 0),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(3.788751e+00, 1.931482e+03, 7.322346e+08, 3.233415e+06, 1.902026e+10, 3.000000e+08, 1.000000e+10, 5.000000e+09, 1.000000e+08, 3.554883e-06, 4.355730e+09), 
      temp = 20)),      
      k_B = k_B,
      W = W,
      K = K,
      model = model
    ),

    Pagonis2008 = list(

      N = c(1.5e7, 1e7, 4e7, 2.5e8, 5e10, 5e9, 4e9, 3e8, 1e10, 1.2e12, 3e10),
      E = c(0.97, 1.55, 1.73, 1.8, 2.0, 1.65, 1.6, 1.43, 1.75, 5.0, 5.0),
      s = c(5.0e12, 5.0e14, 6.5e13, 1.5e13, 1.0e10, 6.5e13, 5.0e12, 5.0e13, 5.0e14, 1.0e13, 1.0e13),
      A = c(1e-08, 1e-08, 5e-09, 5e-10, 1e-10, 1e-11, 6e-12, 5e-07, 1e-09, 1e-14, 1e-10),
      B = c(0, 0, 0, 0, 0, 0, 0,5e-09, 5e-10, 3e-10, 1e-10),
      Th = c(0.75, 0.00, 6.00, 4.50, 0.00, 0.01, 0.00),
      E_th = c(0.10, 0.00, 0.10, 0.13, 0.00, 0.20, 0.00),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(3.401581e-03, 5.718477e+04, 2.879822e+07, 1.235043e+08, 2.556071e+10, 3.881049e+06, 7.550608e+06, 1.734105e+08, 3.332680e+09, 1.294818e+08, 2.208893e+10, 6.329367e-08, 3.137732e-05),
      temp = 20)),
      k_B = k_B,
      W = W,
      K = K,
      model = model
    ),
    
    Friedrich2017 = list(
      
      N = c(1.5e7, 1e7, 1e9, 2.5e8, 5e10, 3e9, 1e10, 5e9, 1e11),
      E = c(0.97, 1.55, 1.7, 1.72, 1.95, 1.8, 1.75, 5, 5),
      s = c(5e12, 5e14, 5e13, 5e14, 1e10, 5e13, 5e14, 1e13, 1e13),
      A = c(1e-8, 1e-8, 1e-9, 5e-10, 1e-10, 5e-7, 1e-9, 1e-10, 1e-9),
      B = c(0, 0, 0, 0, 0, 5e-9, 5e-10, 1e-10, 1e-10),
      Th = c(0.75, 0, 6, 4.5, 0),
      E_th = c(0.1, 0, 0.1, 0.13, 0),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(4.282981e-02, 2.165932e+06, 1.464513e+08, 1.898261e+07, 1.372718e+10, 2.215388e+09, 4.107361e+07, 6.146246e+07, 1.157685e+10, 4.006611e-07, 5.763883e-11),
           temp = 20)),
      k_B = k_B,
      K = K,
      W = W,
      model = model
    ),
    
    Friedrich2018 = list(
      
      N = c(1.5e7, 1e7, 1e9, 2.5e8, 5e10, 1e10, 1e10, 5e9, 1e11),
      E = c(0.97, 1.55, 1.7, 1.72, 1.95, 1.8, 1.75, 5, 5),
      s = c(5e12, 5e14, 5e13, 5e14, 1e10, 5e13, 5e14, 1e13, 1e13),
      A = c(1e-8, 1e-8, 1e-9, 5e-10, 1e-10, 5e-7, 1e-9, 1e-10, 1e-9),
      B = c(0, 0, 0, 0, 0, 5e-9, 5e-10, 1e-10, 5e-10),
      Th = c(0.75, 0, 6, 4.5, 0),
      E_th = c(0.1, 0, 0.1, 0.13, 0),
      n =  set_RLum(class = "RLum.Results", data = list(n = c(2.025417e-02, 1.048967e+06, 2.789634e+07, 3.508166e+06, 8.606323e+09, 5.796382e+09, 5.274192e+06, 1.648926e+07, 2.820631e+09, 9.047145e-08, 1.525008e-11),
                                                        temp = 20)),
      k_B = k_B,
      K = K,
      W = W,
      model = model
    ),
    
    
    customized = list(
      n =  set_RLum(class = "RLum.Results", data = list(n = rep(0,4),
                                                        temp = 20,
                                                        model = model)),
      k_B = k_B,
      W = W,
      K = K,
      model = model
    )
  )


  switch(model,
        "Bailey2001" = {
          return(parameter.list$Bailey2001)
        },
        
        "Bailey2002" = {
          return(parameter.list$Bailey2002)
        },

        "Bailey2004" = {
          return(parameter.list$Bailey2004)
        },

        "Pagonis2007" = {
          return(parameter.list$Pagonis2007)
        },

        "Pagonis2008" = {
          return(parameter.list$Pagonis2008)
        },
        
        "Friedrich2017" = {
          return(parameter.list$Friedrich2017)
        },
        
        "Friedrich2018" = {
          return(parameter.list$Friedrich2018)
        },
        
        "customized" = {
          return(parameter.list$customized)
        }
  )#end switch

}

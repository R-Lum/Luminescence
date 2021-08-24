#' Model Luminescence Signals
#'
#' This function models luminescence signals for quartz based on published physical models.
#' It is possible to simulate TL, (CW-) OSL, RF measurements in a arbitrary sequence. This
#' sequence is definded as a \code{\link{list}} of certain abrivations. Furthermore it is possible to
#' load a sequence direct from the Riso Sequence Editor.
#' The output is an \code{\linkS4class{RLum.Analysis}}object and so the plots are done
#' by the \code{\link{plot_RLum.Analysis}} function. If a SAR sequence is simulated the plot output can be disabled and SAR analyse functions
#' can be used.
#'
#'
#' Defining a \bold{sequence}\cr
#'
#' \tabular{lll}{
#' \bold{Arguments} \tab \bold{Description} \tab \bold{Sub-arguments}\cr
#' TL \tab thermally stimulated luminescence \tab 'temp begin' [\eqn{^{\circ}}C], 'temp end' [\eqn{^{\circ}}C], 'heating rate' [\eqn{^{\circ}}C/s]\cr
#' OSL\tab optically stimulated luminescence \tab 'temp' [\eqn{^{\circ}}C], 'duration' [s], 'optical_power' [\%]\cr
#' ILL\tab illumination \tab 'temp' [\eqn{^{\circ}}C], 'duration' [s], 'optical_power' [\%]\cr
#' LM_OSL\tab linear modulated OSL \tab 'temp' [\eqn{^{\circ}}C], 'duration' [s], optional: 'start_power' [\%], 'end_power' [\%]\cr
#' RL/RF\tab radioluminescence\tab 'temp' [\eqn{^{\circ}}C], 'dose' [Gy], 'dose_rate' [Gy/s] \cr
#' RF_heating\tab RF during heating/cooling\tab 'temp begin' [\eqn{^{\circ}}C], 'temp end' [\eqn{^{\circ}}C], 'heating rate' [\eqn{^{\circ}}C/s], 'dose_rate' [Gy/s] \cr
#' IRR\tab irradiation \tab 'temp' [\eqn{^{\circ}}C], 'dose' [Gy], 'dose_rate' [Gy/s] \cr
#' CH \tab cutheat \tab 'temp' [\eqn{^{\circ}}C], optional: 'duration' [s], 'heating_rate' [\eqn{^{\circ}}C/s]\cr
#' PH  \tab preheat \tab 'temp' [\eqn{^{\circ}}C], 'duration' [s], optional: 'heating_rate' [\eqn{^{\circ}}C/s]\cr
#' PAUSE \tab pause \tab 'temp' [\eqn{^{\circ}}C], 'duration' [s]
#' }
#'
#' Note: 100 \% illumination power equates to 20 mW/cm^2
#'
#'
#' Defining a \bold{SAR-sequence}\cr
#'
#' \tabular{lll}{
#' \bold{Abrivation} \tab \bold{Description} \tab \bold{examples} \cr
#' RegDose \tab Dose points of the regenerative cycles [Gy]\tab c(0, 80, 140, 260, 320, 0, 80)\cr
#' TestDose\tab Test dose for the SAR cycles  [Gy]\tab 50 \cr
#' PH\tab Temperature of the preheat [\eqn{^{\circ}}C]\tab 240 \cr
#' CH\tab Temperature of the cutheat [\eqn{^{\circ}}C]\tab 200 \cr
#' OSL_temp\tab Temperature of OSL read out [\eqn{^{\circ}}C]\tab  125 \cr
#' OSL_duration\tab  Duration of OSL read out [s]\tab default: 40 \cr
#' Irr_temp \tab Temperature of irradiation [\eqn{^{\circ}}C]\tab default: 20\cr
#' PH_duration  \tab Duration of the preheat [s]\tab default: 10 \cr
#' dose_rate \tab Dose rate of the laboratory irradiation source [Gy/s]\tab default: 1 \cr
#' optical_power \tab Percentage of the full illumination power [\%]\tab default: 90 \cr
#' Irr_2recover \tab Dose to be recovered in a dose-recovery-test [Gy]\tab 20
#' }
#'
#' @param sequence \code{\link{list}} (\bold{required}): set sequence to model as \code{\link{list}} or as *.seq file from the
#' Riso sequence editor. To simulate SAR measurements there is an extra option to set the sequence list (cf. details).
#
#' @param model \code{\link{character}} (\bold{required}): set model to be used. Available models are:
#' "Bailey2001", "Bailey2002", "Bailey2004", "Pagonis2007", "Pagonis2008", "Friedrich2017", "Friedrich2018" and for own models "customized" (or "customised").
#' Note: When model = "customized" is set, the argument 'own_parameters' has to be set.
#'
#' @param lab.dose_rate \code{\link{numeric}} (with default): laboratory dose rate in XXX
#' Gy/s for calculating seconds into Gray in the *.seq file.
#'
#' @param simulate_sample_history \code{\link{logical}} (with default): FALSE (with default): simulation begins at laboratory conditions, 
#' TRUE: simulations begins at crystallization (all levels 0) process
#'
#' @param plot \code{\link{logical}} (with default): Enables or disables plot output
#'
#' @param verbose \code{\link{logical}} (with default): Verbose mode on/off
#'
#' @param show_structure \code{\link{logical}} (with default): Shows the structure of the result.
#' Recommended to show record.id to analyse concentrations.
#' 
#' @param own_parameters \code{\link{list}} (with default): This argument allows the user to submit own parameter sets. The \code{\link{list}}
#' has to contain the following items:
#' \itemize{
#'  \item{N: Concentration of electron- and hole traps [cm^(-3)]}
#'  \item{E: Electron/Hole trap depth [eV}
#'  \item{s: Frequency factor [s^(-1)]}
#'  \item{A: Conduction band to electron trap and valence band to hole trap transition probability [s^(-1) * cm^(3)]. 
#'  \bold{CAUTION: Not every publication uses 
#'  the same definition of parameter A and B! See vignette "RLumModel - Usage with own parameter sets" for further details}}
#'  \item{B: Conduction band to hole centre transition probability [s^(-1) * cm^(3)].}
#'  \item{Th: Photo-eviction constant or photoionisation cross section, respectively}
#'  \item{E_th: Thermal assistence energy [eV]}
#'  \item{k_B: Boltzman constant 8.617e-05 [eV/K]}
#'  \item{W: activation energy 0.64 [eV] (for UV)}
#'  \item{K: 2.8e7 (dimensionless constant)}
#'  \item{model: "customized"}
#'  \item{R (optional): Ionisation rate (pair production rate) equivalent to 1 Gy/s [s^(-1) * cm^(-3)]}
#'  }
#' 
#' For further details see Bailey 2001, Wintle 1975, vignette "RLumModel - Using own parameter sets" 
#' and example 3. 
#' 
#' @param own_state_parameters \code{\link{numeric}} (with default): Some publications (e.g. Pagonis 2009)
#' offer state parameters. With this argument the user can submit this state parameters. For further details
#' see vignette ""RLumModel - Using own parameter sets" and example 3.
#' 
#' @param own_start_temperature \code{\link{numeric}} (with default): Parameter to control the start temperature (in deg. C) of
#' a simulation. This parameter takes effect only when 'model = "customized"' is choosen. 
#' 
#' @param \dots further arguments and graphical parameters passed to
#' \code{\link{plot.default}}. See details for further information.
#'
#' @return This function returns an \code{\linkS4class{RLum.Analysis}} object with all TL, (LM-) OSL and RF/RL steps
#' in the sequence. Every entry is an \code{\linkS4class{RLum.Data.Curve}} object and can be plotted, analysed etc. with
#' further \code{RLum}-functions.
#'
#' @section Function version: 0.1.5 
#'
#' @author Johannes Friedrich, University of Bayreuth (Germany),
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
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
#' Friedrich, J., Kreutzer, S., Schmidt, C., 2016. Solving ordinary differential equations to understand luminescence:
#' 'RLumModel', an advanced research tool for simulating luminescence in quartz using R. Quaternary Geochronology 35, 88-100.
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
#' Pagonis, V., Lawless, J., Chen, R., Anderson, C., 2009. Radioluminescence in Al2O3:C - analytical and numerical 
#' simulation results. Journal of Physics D: Applied Physics 42, 175107 (9pp).
#'
#' Soetaert, K., Cash, J., Mazzia, F., 2012. Solving differential equations in R.
#' Springer Science & Business Media.
#' 
#' Wintle, A., 1975. Thermal Quenching of Thermoluminescence in Quartz. Geophysical Journal International 41, 107-113.
#'
#' @seealso \code{\link{plot}}, \code{\linkS4class{RLum}},
#' \code{\link{read_SEQ2R}}
#'
#' @examples
#'
#'
#' ##================================================================##
#' ## Example 1: Simulate Bailey2001
#' ## (cf. Bailey, 2001, Fig. 1)
#' ##================================================================##
#'
#' ##set sequence with the following steps
#' ## (1) Irradiation at 20 deg. C with a dose of 10 Gy and a dose rate of 1 Gy/s
#' ## (2) TL from 20-400 deg. C with a rate of 5 K/s
#'
#' sequence <-
#'   list(
#'     IRR = c(20, 10, 1),
#'     TL = c(20, 400, 5)
#'   )
#'
#' ##model sequence
#' model.output <- model_LuminescenceSignals(
#'   sequence = sequence,
#'   model = "Bailey2001"
#' )
#' 
#' ##get all TL concentrations
#' 
#' TL_conc <- get_RLum(model.output, recordType = "(TL)", drop = FALSE)
#' 
#' plot_RLum(TL_conc)
#' 
#' ##plot 110 deg. C trap concentration
#' 
#' TL_110 <- get_RLum(TL_conc, recordType = "conc. level 1") 
#' plot_RLum(TL_110)
#' 
#' ##============================================================================##
#' ## Example 2: compare different optical powers of stimulation light
#' ##============================================================================##
#'
#' # call function "model_LuminescenceSignals", model = "Bailey2004"
#' # and simulate_sample_history = FALSE (default),
#' # because the sample history is not part of the sequence
#' # the optical_power of the LED is varied and then compared.
#'
#' optical_power <- seq(from = 0,to = 100,by = 20)
#'
#' model.output <- lapply(optical_power, function(x){
#'
#'  sequence <- list(IRR = c(20, 50, 1),
#'                   PH = c(220, 10, 5),
#'                   OSL = c(125, 50, x)
#'                   )
#'
#'  data <- model_LuminescenceSignals(
#'            sequence = sequence,
#'            model = "Bailey2004",
#'            plot = FALSE,
#'            verbose = FALSE
#'            )
#'
#'  return(get_RLum(data, recordType = "OSL$", drop = FALSE))
#' })
#'
#' ##combine output curves
#' model.output.merged <- merge_RLum(model.output)
#'
#' ##plot
#' plot_RLum(
#'  object = model.output.merged,
#'  xlab = "Illumination time [s]",
#'  ylab = "OSL signal [a.u.]",
#'  main = "OSL signal dependency on optical power of stimulation light",
#'  legend.text = paste("Optical power density", 20*optical_power/100, "mW/cm^2"),
#'  combine = TRUE)
#'  
#' ##============================================================================##
#' ## Example 3: Usage of own parameter sets (Pagonis 2009)
#' ##============================================================================##
#' 
#' own_parameters <- list(
#'   N = c(2e15, 2e15, 1e17, 2.4e16),
#'   E = c(0, 0, 0, 0),
#'   s = c(0, 0, 0, 0),
#'   A = c(2e-8, 2e-9, 4e-9, 1e-8),
#'   B = c(0, 0, 5e-11, 4e-8),
#'   Th = c(0, 0),
#'   E_th = c(0, 0),
#'   k_B = 8.617e-5,
#'   W = 0.64,
#'   K = 2.8e7,
#'   model = "customized",
#'   R = 1.7e15
#'  )
#'  ## Note: In Pagonis 2009 is B the valence band to hole centre probability, 
#'  ## but in Bailey 2001 this is A_j. So the values of B (in Pagonis 2009)
#'  ## are A in the notation above. Also notice that the first two entries in N, A and 
#'  ## B belong to the electron traps and the last two entries to the hole centres.
#'  
#'  own_state_parameters <- c(0, 0, 0, 9.4e15)
#'  
#'  ## calculate Fig. 3 in Pagonis 2009. Note: The labels for the dose rate in the original 
#'  ## publication are not correct.
#'  ## For a dose rate of 0.1 Gy/s belongs a RF signal to ~ 1.5e14 (see Fig. 6).
#'  
#'  sequence <- list(RF = c(20, 0.1, 0.1))
#'  
#'  model_LuminescenceSignals(
#'    model = "customized", 
#'    sequence = sequence, 
#'    own_parameters = own_parameters, 
#'    own_state_parameters = own_state_parameters)
#'  
#'  
#'  
#'  
#' \dontrun{  
#' ##============================================================================##
#' ## Example 4: Simulate Thermal-Activation-Characteristics (TAC)
#' ##============================================================================##
#'  
#'  ##set temperature
#'  act.temp <- seq(from = 80, to = 600, by = 20)
#'  
#'  ##loop over temperature
#'  model.output <- vapply(X = act.temp, FUN = function(x) {
#'  
#'  ##set sequence, note: sequence includes sample history
#'    sequence <- list(
#'      IRR = c(20, 1, 1e-11),
#'      IRR = c(20, 10, 1),
#'      PH = c(x, 1),
#'      IRR = c(20, 0.1, 1),
#'      TL = c(20, 150, 5)
#'  )
#'  ##run simulation
#'    temp <- model_LuminescenceSignals(
#'      sequence = sequence,
#'      model = "Pagonis2007",
#'      simulate_sample_history = TRUE,
#'      plot = FALSE,
#'      verbose = FALSE
#'    )
#'      ## "TL$" for exact matching TL and not (TL)
#'    TL_curve <- get_RLum(temp, recordType = "TL$")
#'    ##return max value in TL curve
#'    return(max(get_RLum(TL_curve)[,2]))
#'  }, FUN.VALUE = 1)
#'  
#'  ##plot resutls
#'  plot(
#'    act.temp[-(1:3)],
#'    model.output[-(1:3)],
#'    type = "b",
#'    xlab = "Temperature [\u00B0C]",
#'    ylab = "TL [a.u.]"
#'  )
#'
#' ##============================================================================##
#' ## Example 5: Simulate SAR sequence
#' ##============================================================================##
#'
#' ##set SAR sequence with the following steps
#' ## (1) RegDose: set regenerative dose [Gy] as vector
#' ## (2) TestDose: set test dose [Gy]
#' ## (3) PH: set preheat temperature in deg. C
#' ## (4) CH: Set cutheat temperature in deg. C
#' ## (5) OSL_temp: set OSL reading temperature in deg. C
#' ## (6) OSL_duration: set OSL reading duration in s
#'
#' sequence <- list(
#'  RegDose = c(0,10,20,50,90,0,10),
#'  TestDose = 5,
#'  PH = 240,
#'  CH = 200,
#'  OSL_temp = 125,
#'  OSL_duration = 70)
#'
#' # call function "model_LuminescenceSignals", set sequence = sequence,
#' # model = "Pagonis2007" (palaeodose = 20 Gy) and simulate_sample_history = FALSE (default),
#' # because the sample history is not part of the sequence
#'
#'  model.output <- model_LuminescenceSignals(
#'    sequence = sequence,
#'    model = "Pagonis2007",
#'    plot = FALSE
#'  )
#'
#' # in environment is a new object "model.output" with the results of
#' # every step of the given sequence.
#' # Plots are done at OSL and TL steps and the growth curve
#'
#' # call "analyse_SAR.CWOSL" from RLum package
#'  results <- analyse_SAR.CWOSL(model.output,
#'                             signal.integral.min = 1,
#'                             signal.integral.max = 15,
#'                             background.integral.min = 601,
#'                             background.integral.max = 701,
#'                             fit.method = "EXP",
#'                             dose.points = c(0,10,20,50,90,0,10))
#'
#'
#' ##============================================================================##
#' ## Example 6: generate sequence from *.seq file and run SAR simulation
#' ##============================================================================##
#'
#' # load example *.SEQ file and construct a sequence.
#' # call function "model_LuminescenceSignals", load created sequence for sequence,
#' # set model = "Bailey2002" (palaeodose = 10 Gy)
#' # and simulate_sample_history = FALSE (default),
#' # because the sample history is not part of the sequence
#'
#' path <- system.file("extdata", "example_SAR_cycle.SEQ", package="RLumModel")
#'
#' sequence <- read_SEQ2R(file = path)
#'
#' model.output <- model_LuminescenceSignals(
#'   sequence = sequence,
#'   model = "Bailey2001",
#'   plot = FALSE
#' )
#'
#'
#' ## call RLum package function "analyse_SAR.CWOSL" to analyse the simulated SAR cycle
#'
#' results <- analyse_SAR.CWOSL(model.output,
#'                              signal.integral.min = 1,
#'                              signal.integral.max = 10,
#'                              background.integral.min = 301,
#'                              background.integral.max = 401,
#'                              dose.points = c(0,8,14,26,32,0,8),
#'                              fit.method = "EXP")
#'
#' print(get_RLum(results))
#'
#'
#' ##============================================================================##
#' ## Example 7: Simulate sequence at laboratory without sample history
#' ##============================================================================##
#'
#' ##set sequence with the following steps
#' ## (1) Irraditation at 20 deg. C with a dose of 100 Gy and a dose rate of 1 Gy/s
#' ## (2) Preheat to 200 deg. C and hold for 10 s
#' ## (3) LM-OSL at 125 deg. C. for 100 s
#' ## (4) Cutheat at 200 dec. C.
#' ## (5) Irraditation at 20 deg. C with a dose of 10 Gy and a dose rate of 1 Gy/s
#' ## (6) Pause at 200 de. C. for 100 s
#' ## (7) OSL at 125 deg. C for 100 s with 90 % optical power
#' ## (8) Pause at 200 deg. C for 100 s
#' ## (9) TL from 20-400 deg. C with a heat rate of 5 K/s
#' ## (10) Radiofluorescence at 20 deg. C with a dose of 200 Gy and a dose rate of 0.01 Gy/s
#'
#' sequence <-
#'  list(
#'    IRR = c(20, 100, 1),
#'    PH = c(200, 10),
#'    LM_OSL = c(125, 100),
#'    CH = c(200),
#'    IRR = c(20, 10, 1),
#'    PAUSE = c(200, 100),
#'    OSL = c(125, 100, 90),
#'    PAUSE = c(200, 100),
#'    TL = c(20, 400, 5),
#'    RF = c(20, 200, 0.01)
#' )
#'
#' # call function "model_LuminescenceSignals", set sequence = sequence,
#' # model = "Pagonis2008" (palaeodose = 200 Gy) and simulate_sample_history = FALSE (default),
#' # because the sample history is not part of the sequence
#'
#' model.output <- model_LuminescenceSignals(
#'    sequence = sequence,
#'    model = "Pagonis2008"
#'    )
#'
#'}
#' @export
model_LuminescenceSignals <- function(
  model,
  sequence,
  lab.dose_rate = 1,
  simulate_sample_history = FALSE,
  plot = TRUE,
  verbose = TRUE,
  show_structure = FALSE,
  own_parameters = NULL,
  own_state_parameters = NULL,
  own_start_temperature = NULL,
  ...
) {

# Integrity tests and conversion --------------------------------------------------------------
  
  if(missing(model))
    stop("[model_LuminescenceSignals()] Argument 'model' not given!", call. = FALSE)
  
  if(missing(sequence))
    stop("[model_LuminescenceSignals()] Argument 'sequence' not given!", call. = FALSE)

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

  if(!model%in%model.allowed_keywords)
    stop(paste0("[model_LuminescenceSignals()] Model not supported. Supported models are: ", 
                paste(model.allowed_keywords, collapse = ", ")))

  #Check sequence
  if(is(sequence, "character")){
    
    if(!tools::file_ext(sequence) %in% c("SEQ", "seq"))
      stop("[model_LuminescenceSignals()] Argument 'sequence' is not a *.SEQ file!", call. = FALSE)
  
    sequence <- read_SEQ2R(
        file = sequence,
        lab.dose_rate = lab.dose_rate,
        txtProgressBar = ifelse(verbose, TRUE, FALSE))
  }

  else if(is.list(sequence)){

    if(!is.numeric(unlist(sequence)))
      stop("[model_LuminescenceSignals()] Sequence comprises non-numeric arguments!")

    # test if .create_SAR.sequence is requiered
    if("RegDose"%in%names(sequence)){
      
      RegDose = sequence$RegDose
      TestDose = sequence$TestDose
      PH = sequence$PH
      CH = sequence$CH
      OSL_temp = sequence$OSL_temp

      Irr_temp = sequence$Irr_temp
      if(is.null(Irr_temp))
        Irr_temp <- 20

      OSL_duration = sequence$OSL_duration
      if(is.null(sequence$OSL_duration))
        OSL_duration <- 40

      PH_duration = sequence$PH_duration
      if(is.null(PH_duration))
        PH_duration <- 10

      dose_rate = sequence$dose_rate
      if(is.null(dose_rate))
        dose_rate <- lab.dose_rate

      optical_power = sequence$optical_power
      if(is.null(optical_power))
        optical_power <- 90
      
      if(!"Irr_2recover" %in% names(sequence)){# SAR sequence

        sequence <- .create_SAR.sequence(
          RegDose = RegDose,
          TestDose = TestDose,
          PH = PH,
          CH = CH,
          OSL_temp = OSL_temp,
          Irr_temp = Irr_temp,
          OSL_duration = OSL_duration,
          PH_duration = PH_duration,
          dose_rate = dose_rate,
          optical_power = optical_power)
        
      } else {# DRT sequence

        sequence <- .create_DRT.sequence(
          RegDose = RegDose,
          TestDose = TestDose,
          PH = PH,
          CH = CH,
          OSL_temp = OSL_temp,
          Irr_temp = Irr_temp,
          OSL_duration = OSL_duration,
          PH_duration = PH_duration,
          dose_rate = dose_rate,
          optical_power = optical_power,
          Irr_2recover = sequence$Irr_2recover)
    }
  } else {
    
   sequence <- sequence
   
  }
  } else { # end if(is.list(sequence))
    stop("[model_LuminescenceSignals()] Sequence has to be of class list or a *.seq file", call. = FALSE)
  }

  #check for wrong elements in the sequence

  ##allowed keywords
  sequence.allowed_keywords <- c("IRR","PH", "CH", "TL", "OSL", "PAUSE", "LM_OSL", "RL", "RF", "ILL", "RF_heating")

  ##check
  if(!all(names(sequence)%in%sequence.allowed_keywords)){
    stop(paste0("[model_LuminescenceSignals()] Unknow sequence arguments: Allowed arguments are: ", 
                paste(sequence.allowed_keywords, collapse = ", ")), call. = FALSE)
    }

  #check if lab.dose_rate > 0
  if(lab.dose_rate <= 0)
    stop("[model_LuminescenceSignals()] lab.dose_rate has to be a positive number!", call. = FALSE)

  extraArgs <- list(...)

# Load model parameters ------------------------------------------------------------------------------------
  
  ## check if "parms" in extra arguments for fitting data to model parameters

    if(model == "customized" || model == "customised"){
        if(is.null(own_parameters)){
          stop("[model_LuminescenceSignals()] Argument 'model' set to 'customized', but no own parameters are given!", 
               call. = FALSE)}
    
        parms <- own_parameters
        
        ##check if "Th", "E_th", "k_B", "W" or "K" are set
        if("Th" %in% names(parms)){
          parms$Th <- unname(unlist(parms["Th"]))
        } else {
          parms$Th <- rep(0, length(parms$N))
        }
        
        if("E_th" %in% names(parms)){
          parms$E_th <- unname(unlist(parms["E_th"]))
        } else {
          parms$E_th <- rep(0, length(parms$N))
        }
        
        parms["k_B"] <- ifelse("k_B" %in% names(parms), unname(unlist(parms["k_B"])), 8.617e-05)
        parms["W"] <- ifelse("W" %in% names(parms), unname(unlist(parms["W"])), 0.64)
        parms["K"] <- ifelse("K" %in% names(parms), unname(unlist(parms["K"])), 2.8e7)
        
        ## set start temperature
        start_temp <- ifelse(is.null(own_start_temperature), 20, own_start_temperature)

        if(!is.null(own_state_parameters)){ ## state parameters submitted
          
          own_state_parameters <- c(own_state_parameters, 0, 0)
          
          n <- Luminescence::set_RLum(class = "RLum.Results",
                                      data = list(n = own_state_parameters,
                                                  temp = start_temp,
                                                  model = model))
          
        } else { ## no state parameters submitted
          
          n <- Luminescence::set_RLum(class = "RLum.Results",
                                      data = list(n = rep(0,length(parms$N)+2),
                                                  temp = start_temp,
                                                  model = model))
        }
        
    } else { ## model not customized and parms not set
      
      if(!is.null(own_parameters)){
        warning(paste0("[model_LuminescenceSignals()] Argument 'own_parameters' set, but argument 'model' not set to 'customized'. Used '", model, "' as argument for 'model'."), call. = FALSE)
      }
      
      if(!is.null(own_state_parameters)){
        warning(paste0("[model_LuminescenceSignals()] Argument 'own_sate_parameters' set, but argument 'model' not set to 'customized'. Ignored argument 'own_state_parameters'."), call. = FALSE)
      }
      
      if(!is.null(own_start_temperature)){
        warning(paste0("[model_LuminescenceSignals()] Argument 'own_start_temperature' set, but argument 'model' not set to 'customized'. Ignored argument 'own_start_temperature'."), call. = FALSE)
      }
        
        parms <- .set_pars(model)

        if(simulate_sample_history){
          n <- Luminescence::set_RLum(class = "RLum.Results",
                                      data = list(n = rep(0,length(parms$N)+2),
                                                  temp = 20,
                                                  model = model))
          } else {
            n <- parms$n
          }
    }

# sequence ------------------------------------------------------------------------------------

  #sequence, n and parms as arguments for the SequenceTranslator, who translates the sequence to different model steps
    model.output <- .translate_sequence(
        sequence = sequence,
        n = n,
        model = model,
        parms = parms,
        verbose = verbose
      )


# Plot settings -------------------------------------------------------------------------------

  if(plot){

    plot.data <- get_RLum(model.output, 
                          recordType = c("RF$", "TL$", "OSL$", "LM-OSL$", "RF_heating$", "pause$"), 
                          drop = FALSE)
    
    Luminescence::plot_RLum(plot.data, ...)
  }

# model.output structure --------------------------------------------------

  if(show_structure){
    cat("[model_LuminescenceSignals()] \n\t>> Structure of output from 'model_LuminescenceSignals()' \n\n")
    print(Luminescence::structure_RLum(model.output))
  }

# return model.output -------------------------------------------------------------------------
  return(model.output)

}#end function

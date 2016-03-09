pkgname <- "RLumModel"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "RLumModel-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('RLumModel')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("ExampleData.ModelOutput")
### * ExampleData.ModelOutput

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.ModelOutput
### Title: Example data (TL curve) simulated from Bailey (2001 ,fig. 1)
### Aliases: ExampleData.ModelOutput model.output
### Keywords: datasets

### ** Examples


data(ExampleData.ModelOutput)
TL_curve <- get_RLum(model.output, recordType = "TL$", drop = FALSE)

##plot TL curve
plot_RLum(TL_curve)

TL_concentrations <- get_RLum(model.output, recordType = "(TL)", drop = FALSE)
plot_RLum(TL_concentrations)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.ModelOutput", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("model_LuminescenceSignals")
### * model_LuminescenceSignals

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: model_LuminescenceSignals
### Title: Model Luminescence Signals
### Aliases: model_LuminescenceSignals

### ** Examples



##================================================================##
## Example 1: Simulate sample history of Bailey2001
## (cf. Bailey, 2001, Fig. 1)
##================================================================##

##set sequence with the following steps
## (1) Irradiation at 20 deg. C with a dose of 10 Gy and a dose rate of 1 Gy/s
## (2) TL from 20-400 deg. C with a rate of 5 K/s

sequence <-
  list(
    IRR = c(20, 10, 1),
    TL = c(20, 400, 5)
  )

##model sequence
model.output <- model_LuminescenceSignals(
  sequence = sequence,
  model = "Bailey2001"
)

## Not run: 
##D ##============================================================================##
##D ## Example 2: Simulate sequence at labour without sample history
##D ##============================================================================##
##D 
##D ##set sequence with the following steps
##D ## (1) Irraditation at 20 deg. C with a dose of 100 Gy and a dose rate of 1 Gy/s
##D ## (2) Preheat to 200 deg. C and hold for 10 s
##D ## (3) LM-OSL at 125 deg. C. for 100 s
##D ## (4) Cutheat at 200 dec. C.
##D ## (5) Irraditation at 20 deg. C with a dose of 10 Gy and a dose rate of 1 Gy/s
##D ## (6) Pause at 200 de. C. for 100 s
##D ## (7) OSL at 125 deg. C for 100 s with 90 % optical power
##D ## (8) Pause at 200 deg. C for 100 s
##D ## (9) TL from 20-400 deg. C with a heat rate of 5 K/s
##D ## (10) Radiofluorescence at 20 deg. C with a dose of 200 Gy and a dose rate of 0.01 Gy/s
##D 
##D sequence <-
##D  list(
##D    IRR = c(20, 100, 1),
##D    PH = c(200, 10),
##D    LM_OSL = c(125, 100),
##D    CH = c(200),
##D    IRR = c(20, 10, 1),
##D    PAUSE = c(200, 100),
##D    OSL = c(125, 100, 90),
##D    PAUSE = c(200, 100),
##D    TL = c(20, 400, 5),
##D    RF = c(20, 200, 0.01)
##D )
##D 
##D # call function "model_LuminescenceSignals", set sequence = sequence,
##D # model = "Pagonis2008" (palaeodose = 200 Gy) and simulate_sample_history = FALSE (default),
##D # because the sample history is not part of the sequence
##D 
##D model.output <- model_LuminescenceSignals(
##D    sequence = sequence,
##D    model = "Pagonis2008"
##D    )
##D 
##D 
##D 
##D ##============================================================================##
##D ## Example 3: Simulate SAR sequence
##D ##============================================================================##
##D 
##D ##set SAR sequence with the following steps
##D ## (1) RegDose: set regenerative dose [Gy] as vector
##D ## (2) TestDose: set test dose [Gy]
##D ## (3) PH: set preheat temperature in deg. C
##D ## (4) CH: Set cutheat temperature in deg. C
##D ## (5) OSL_temp: set OSL reading temperature in deg. C
##D ## (6) OSL_duration: set OSL reading duration in s
##D 
##D sequence <- list(
##D  RegDose = c(0,10,20,50,90,0,10),
##D  TestDose = 5,
##D  PH = 240,
##D  CH = 200,
##D  OSL_temp = 125,
##D  OSL_duration = 70)
##D 
##D # call function "model_LuminescenceSignals", set sequence = sequence,
##D # model = "Pagonis2007" (palaeodose = 20 Gy) and simulate_sample_history = FALSE (default),
##D # because the sample history is not part of the sequence
##D 
##D  model.output <- model_LuminescenceSignals(
##D 
##D  sequence = sequence,
##D  model = "Pagonis2007",
##D  plot = FALSE
##D  )
##D 
##D # in environment is a new object "model.output" with the results of
##D # every step of the given sequence.
##D # Plots are done at OSL and TL steps and the growth curve
##D 
##D # call "analyse_SAR.CWOSL" from RLum package
##D  results <- analyse_SAR.CWOSL(model.output,
##D                             signal.integral.min = 1,
##D                             signal.integral.max = 15,
##D                             background.integral.min = 601,
##D                             background.integral.max = 701,
##D                             fit.method = "EXP",
##D                             dose.points = c(0,10,20,50,90,0,10))
##D 
##D 
##D ##============================================================================##
##D ## Example 4: generate sequence from *.seq file and run SAR simulation
##D ##============================================================================##
##D 
##D # load example *.SEQ file and construct a sequence.
##D # call function "model_LuminescenceSignals", load created sequence for sequence,
##D # set model = "Bailey2002" (palaeodose = 10 Gy)
##D # and simulate_sample_history = FALSE (default),
##D # because the sample history is not part of the sequence
##D 
##D path <- system.file("extdata", "example_SAR_cycle.SEQ", package="RLumModel")
##D 
##D sequence <- read_SEQ2R(file = path)
##D 
##D model.output <- model_LuminescenceSignals(
##D   sequence = sequence,
##D   model = "Bailey2001",
##D   plot = FALSE
##D )
##D 
##D 
##D ## call RLum package function "analyse_SAR.CWOSL" to analyse the simulated SAR cycle
##D 
##D results <- analyse_SAR.CWOSL(model.output,
##D                              signal.integral.min = 1,
##D                              signal.integral.max = 10,
##D                              background.integral.min = 301,
##D                              background.integral.max = 401,
##D                              dose.points = c(0,8,14,26,32,0,8),
##D                              fit.method = "EXP")
##D 
##D print(get_RLum(results))
##D 
##D 
##D ##============================================================================##
##D ## Example 5: compare different optical powers of stimulation light
##D ##============================================================================##
##D 
##D # call function "model_LuminescenceSignals", model = "Bailey2004"
##D # and simulate_sample_history = FALSE (default),
##D # because the sample history is not part of the sequence
##D # the optical_power of the LED is varied and then compared.
##D 
##D optical_power <- seq(from = 0,to = 100,by = 20)
##D 
##D model.output <- lapply(1:length(optical_power), function(x){
##D 
##D  sequence <- list(IRR = c(20, 50, 1),
##D                   PH = c(220, 10, 5),
##D                   OSL = c(125, 50, optical_power[x])
##D                   )
##D 
##D  data <- model_LuminescenceSignals(
##D            sequence = sequence,
##D            model = "Bailey2004",
##D            plot = FALSE
##D            )
##D 
##D  return(get_RLum(data, recordType = "OSL$", drop = FALSE))
##D })
##D 
##D ##combine output curves
##D model.output.merged <- merge_RLum(model.output)
##D 
##D ##plot
##D plot_RLum(
##D  object = model.output.merged,
##D  xlab = "Illumination time [s]",
##D  ylab = "OSL signal [a.u.]",
##D  main = "OSL signal dependency on optical power of stimulation light",
##D  legend.text = paste("Optical power density", 20*optical_power/100, "mW/cm^2"),
##D  combine = TRUE)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("model_LuminescenceSignals", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_SEQ2R")
### * read_SEQ2R

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_SEQ2R
### Title: Parse a Risoe SEQ-file to a sequence neccessary for simulating
###   quartz luminescence
### Aliases: read_SEQ2R

### ** Examples

##search "example_SAR_cycle.SEQ" in "extdata" in package "RLumModel"
path <- system.file("extdata", "example_SAR_cycle.SEQ", package="RLumModel")

sequence <- read_SEQ2R(file = path)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_SEQ2R", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

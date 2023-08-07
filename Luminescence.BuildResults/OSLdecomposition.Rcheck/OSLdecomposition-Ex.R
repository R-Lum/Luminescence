pkgname <- "OSLdecomposition"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "OSLdecomposition-Ex.timings", pos = 'CheckExEnv')
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
library('OSLdecomposition')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("RLum.OSL_correction")
### * RLum.OSL_correction

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RLum.OSL_correction
### Title: Check and correct CW-OSL curves in RLum.Analysis data sets
### Aliases: RLum.OSL_correction

### ** Examples


# 'FB_10Gy' is a dose recovery test with the Fontainebleau quartz
# measured with a lexsyg research with green LED stimulation
data_path <- system.file("examples", "FB_10Gy_SAR.bin", package = "OSLdecomposition")
data_set <- Luminescence::read_BIN2R(data_path, fastForward = TRUE)

# To correct for the background signal, subtracted the average curve from the
# OSL curves of an empty aliquot (list item 11) from all other OSL records:
data_set_corrected <- RLum.OSL_correction(data_set, background = 11, remove_light_off = FALSE)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RLum.OSL_correction", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RLum.OSL_decomposition")
### * RLum.OSL_decomposition

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RLum.OSL_decomposition
### Title: Separate CW-OSL components in RLum.Analysis data sets
### Aliases: RLum.OSL_decomposition

### ** Examples


#'FB_10Gy' is a dose recovery test with the Fontainebleau quartz
# measured in a lexsyg research with green LED stimulation
data_path <- system.file("examples", "FB_10Gy_SAR.bin", package = "OSLdecomposition")
data_set <- Luminescence::read_BIN2R(data_path, fastForward = TRUE)

# Separate components
data_set_decomposed <- RLum.OSL_decomposition(
data_set, decay_rates = c(0.8, 0.05))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RLum.OSL_decomposition", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RLum.OSL_global_fitting")
### * RLum.OSL_global_fitting

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RLum.OSL_global_fitting
### Title: Identify CW-OSL signal components in RLum.Analysis data sets
### Aliases: RLum.OSL_global_fitting

### ** Examples


# 'FB_10Gy' is a dose recovery test with the Fontainebleau quartz
# measured in a lexsyg research with green LED stimulation
data_path <- system.file("examples", "FB_10Gy_SAR.bin", package = "OSLdecomposition")
data_set <- Luminescence::read_BIN2R(data_path, fastForward = TRUE)

# Check data set and perform background correction
data_set_corrected <- RLum.OSL_correction(data_set,
 background = 11,
 remove_light_off = FALSE)

# Identify components
data_set_fitted <- RLum.OSL_global_fitting(
 data_set_corrected,
 K_maximum = 2,
 stimulation_intensity = 50,
 stimulation_wavelength = 530)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RLum.OSL_global_fitting", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("decompose_OSLcurve")
### * decompose_OSLcurve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: decompose_OSLcurve
### Title: Multi-exponential CW-OSL decomposition
### Aliases: decompose_OSLcurve

### ** Examples


# Set some arbitrary decay parameter for a dim CW-OSL measurement of quartz
components <- data.frame(name = c("fast", "medium", "slow"),
                         lambda = c(2, 0.5, 0.02),
                         n = c(1000, 1000, 10000))

# Simulate the CW-OSL curve and add some signal noise and some detection background
curve <- simulate_OSLcomponents(components, simulate.curve = TRUE,
                                add.poisson.noise = TRUE, add.background = 40)

# Decompose the simulated curve
components <- decompose_OSLcurve(curve, components)

# Display the component separation results
plot_OSLcurve(curve, components)

### Decomposition including signal background fitting:

# Define optimized integration intervals, including an interval for the background
components <- optimise_OSLintervals(components, curve, background.component = TRUE)

# Decompose again and view results
components <- decompose_OSLcurve(curve, components, background.fitting = TRUE)
plot_OSLcurve(curve, components)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("decompose_OSLcurve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit_OSLcurve")
### * fit_OSLcurve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_OSLcurve
### Title: Multi-exponential CW-OSL curve fitting
### Aliases: fit_OSLcurve

### ** Examples


# Create a simple curve with just one component
curve <- data.frame(
  X = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  Y = c(377, 244, 163, 93, 59, 28, 17, 13, 10, 8, 9, 5))
# Perform fitting
components <- fit_OSLcurve(curve, F.threshold = 3)

# Display results
plot_OSLcurve(curve, components)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_OSLcurve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("optimise_OSLintervals")
### * optimise_OSLintervals

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: optimise_OSLintervals
### Title: Find adequate integration intervals for CW-OSL decomposition
### Aliases: optimise_OSLintervals

### ** Examples


A <- optimise_OSLintervals(c(2, 0.5, 0.02), channel.width = 0.1, channel.number = 200)
print(A, row.names = FALSE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("optimise_OSLintervals", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_OSLcurve")
### * plot_OSLcurve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_OSLcurve
### Title: Advanced plot function for component resolved CW-OSL curves
### Aliases: plot_OSLcurve

### ** Examples


# Set some arbitrary decay parameter for a dim CW-OSL measurement of quartz
components <- data.frame(name = c("fast", "medium", "slow"),
                         lambda = c(2, 0.5, 0.02),
                         n = c(1000, 1000, 10000))

# Simulate a CW-OSL curve including some signal noise
curve <- simulate_OSLcomponents(components, simulate.curve = TRUE, add.poisson.noise = TRUE)

# Display the simulated curve
plot_OSLcurve(curve, components)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_OSLcurve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_PhotoCrosssections")
### * plot_PhotoCrosssections

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_PhotoCrosssections
### Title: Plot comparison of CW-OSL component photoionisation cross
###   sections of different models
### Aliases: plot_PhotoCrosssections

### ** Examples


# Set some arbitrary decay parameter for a dim CW-OSL measurement of quartz
name <- c("fast", "slow")
lambda <- c(2, 0.02)
n <- c(1e6, 5e7)

# Build a component table
components <- data.frame(name, lambda, n)

# Simulate the CW-OSL curve and add some signal noise
curve <- simulate_OSLcomponents(components, simulate.curve = TRUE, add.poisson.noise = TRUE)

# Perform nonlinear regression at the simulated curve
fit_results <- fit_OSLcurve(curve, K.max = 2, output.complex = TRUE)

# Plot the fitting iterations and set them into context
plot_PhotoCrosssections(fit_results)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_PhotoCrosssections", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simulate_OSLcomponents")
### * simulate_OSLcomponents

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simulate_OSLcomponents
### Title: Simulates signal component decay curves and whole CW-OSL curves
### Aliases: simulate_OSLcomponents

### ** Examples


# Set some arbitrary decay parameter for a dim CW-OSL measurement of quartz
components <- data.frame(name = c("fast", "medium", "slow"),
                         lambda = c(2, 0.5, 0.02),
                         n = c(1000, 1000, 10000))

# Simulate the CW-OSL curve and add some signal noise
curve <- simulate_OSLcomponents(components, simulate.curve = TRUE, add.poisson.noise = TRUE)

# Display the simulated curve
plot_OSLcurve(curve, components)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simulate_OSLcomponents", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sum_OSLcurves")
### * sum_OSLcurves

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sum_OSLcurves
### Title: Combine RLum OSL records to one global average curve
### Aliases: sum_OSLcurves

### ** Examples


# 'FB_10Gy' is a dose recovery test with the Fontainebleau quartz
# measured in a lexsyg research with green LED stimulation
data_path <- system.file("examples", "FB_10Gy_SAR.bin", package = "OSLdecomposition")
data_set <- Luminescence::read_BIN2R(data_path, fastForward = TRUE)

# Give average CW-OSL curve back
average_curve <- sum_OSLcurves(data_set)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sum_OSLcurves", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

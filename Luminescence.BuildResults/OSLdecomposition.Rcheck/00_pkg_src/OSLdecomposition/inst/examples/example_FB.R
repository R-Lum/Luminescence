# Settings ----------------------------------------------------------------
library(Luminescence)
library(OSLdecomposition)
report <- TRUE #  disable auto-reporting to save a lot of computing time

# Load data ---------------------------------------------------------------
# FB_10 Gy is a dose recovery test with Fontainebleau quartz
# measured in the Bayreuth lexsyg system with green stimulation
data_path <- system.file("examples", "FB_10Gy_SAR.bin", package = "OSLdecomposition")
FB <- read_BIN2R(data_path, fastForward = TRUE)

# Component processing ----------------------------------------------------
# Background correction is recommended to prevent slow component overestimation.
# Here, the average curve from the OSL curves of an empty aliquot
# (data set entry 11) is subtracted from all other OSL records
FB_corrected <- RLum.OSL_correction(
  object = FB,
  background = 11)

# Interestingly, one very fast component with negative intensity is found
# But the 'fast' component is always the lambda = ~0.8 s^-1 component,
# no matter if the model K = 2, K = 3 or K = 4 is chosen
FB_fitted <- RLum.OSL_global_fitting(
  FB_corrected,
  stimulation_intensity = 50,
  stimulation_wavelength = 530,
  report = report
)

FB_decomposed <- RLum.OSL_decomposition(
  object = FB_fitted,
  K = 2,
  report = report)


# De analysis and plotting ------------------------------------------------
# Here, component 2 is the 'fast' component
FB_fast_De <- analyse_SAR.CWOSL(
  FB_decomposed[1:2],
  signal.integral.min = NA,
  signal.integral.max = NA,
  background.integral.min = NA,
  background.integral.max = NA,
  OSL.component = c(1,2),
  plot = TRUE
)

# the outlier comes from the background measurements
FB_late_background <-
  analyse_SAR.CWOSL(FB, 1, 10, 800, 999, plot = FALSE)

# black: late background, red: fast component
plot_AbanicoPlot(
  data = list(FB_late_background,
              FB_fast_De))

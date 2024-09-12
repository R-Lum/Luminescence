# the following three lines load necessary functions which will be integrated in Luminescence in the future
library(Luminescence)
library(OSLdecomposition)

# BIN files measured with Risoe TL/OSL Reader or Freiberg Instruments lexsygs can be used as usual.
# Here, we use the example data set provided by the Luminescence package
data(ExampleData.BINfileData, envir = environment())
OSLdata <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data)

# Own data sets can be easily selected for example by the following code snippet:
#
# OSLdata <- read_BIN2R(file.choose(), fastForward = TRUE)

# Consistent detection settings are essential. These are checked by RLum.OSL_correction() which also
# provides various tools for data correction and data improvement, like background subtraction.
# But all these tools are optional. Most data sets can be adequately decomposed without running RLum.OSL_correction()
OSLdata_corrected <- RLum.OSL_correction(OSLdata)

# In the first step, the OSL components and their decay rates are identified
# by multi-exponential fitting at a global CW-OSL curve.
# The obtained information can be found at: OSLdata_fitted$OSL_COMPONENTS
OSLdata_fitted <- RLum.OSL_global_fitting(OSLdata_corrected,
                                          stimulation_intensity = 75)

# In the second step, a very robust algorithm uses the decay constants to decompose
# every CW-OSL record into its signal components, see OSLdata_decomposed[[...]]@records[[...]]@info[["COMPONENTS"]]
OSLdata_decomposed <- RLum.OSL_decomposition(OSLdata_fitted,
                                             K = 3)

# The De's for specific signal components can be calculated without the need of
# defining integration intervals
De_fast_component <- analyse_SAR.CWOSL(OSLdata_decomposed, OSL.component = 1)
De_medium_component <- analyse_SAR.CWOSL(OSLdata_decomposed, OSL.component = 2)

# We can compare the component-based De's with the classical calculated late background subtraction De's
De_late_background <- analyse_SAR.CWOSL(OSLdata, 1, 14, 800, 1000, plot = FALSE)
plot_KDE(list(De_late_background,   # black
              De_fast_component,    # red
              De_medium_component)) # green

plot_RadialPlot(list(De_late_background,    # black
                     De_fast_component,     # red
                     De_medium_component))  # green

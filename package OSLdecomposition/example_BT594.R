# the following three lines load necessary functions which will be integrated in Luminescence in the future 
.LuminescenceEnv <- new.env(parent = emptyenv())
source(system.file("beta", "analyse_SAR.CWOSL_beta.R", package = "OSLdecomposition"))
source(system.file("beta", "calc_OSLLxTxDecomposed.R", package = "OSLdecomposition"))

library(Luminescence)
library(OSLdecomposition)

data_path <- system.file("examples", "BT594_607_608_612_619.BIN", package = "OSLdecomposition")
BT594 <- read_BIN2R(data_path, fastForward = TRUE)

BT594 <- BT594[1:24] # just the first 24 ailquots belong to BT594

BT594 <- RLum.OSL_correction(BT594)

BT594 <- RLum.OSL_global_fitting(BT594, stimulation_intensity = 50, report = TRUE)

BT594 <- RLum.OSL_decomposition(BT594, report = TRUE)

# Components can also be selected by their name
results_decomposed <- analyse_SAR.CWOSL_beta(BT594, OSL.component = "fast")

results_late_background <- analyse_SAR.CWOSL_beta(BT594, 1, 3, 80, 100, plot = FALSE)

plot_RadialPlot(list(results_late_background, results_decomposed))
plot_KDE(list(results_late_background, results_decomposed))

library(Luminescence)
library(OSLdecomposition)

data_path <- system.file("examples", "BT594_607_608_612_619.BIN", package = "OSLdecomposition")
BT594 <- read_BIN2R(data_path, fastForward = TRUE)

BT594 <- BT594[1:24]

BT594 <- RLum.OSL_correction(BT594)

BT594 <- RLum.OSL_global_fitting(BT594, stimulation_intensity = 50)

BT594 <- RLum.OSL_decomposition(BT594)

results_decomposed <- analyse_SAR.CWOSL(BT594, OSL.component = 1)

results_late_background <- analyse_SAR.CWOSL(BT594, 1, 3, 80, 100, plot = FALSE)

plot_RadialPlot(list(results_late_background, results_decomposed))
plot_KDE(list(results_late_background, results_decomposed))

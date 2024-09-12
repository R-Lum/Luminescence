pkgname <- "Luminescence"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "Luminescence-Ex.timings", pos = 'CheckExEnv')
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
library('Luminescence')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Analyse_SAR.OSLdata")
### * Analyse_SAR.OSLdata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Analyse_SAR.OSLdata
### Title: Analyse SAR CW-OSL measurements.
### Aliases: Analyse_SAR.OSLdata
### Keywords: datagen dplot

### ** Examples

##load data
data(ExampleData.BINfileData, envir = environment())

##analyse data
output <- Analyse_SAR.OSLdata(input.data = CWOSL.SAR.Data,
                              signal.integral = c(1:5),
                              background.integral = c(900:1000),
                              position = c(1:1),
                              output.plot = TRUE)

##combine results relevant for further analysis
output.SAR <- data.frame(Dose = output$LnLxTnTx[[1]]$Dose,
                         LxTx = output$LnLxTnTx[[1]]$LxTx,
                         LxTx.Error = output$LnLxTnTx[[1]]$LxTx.Error)
output.SAR




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Analyse_SAR.OSLdata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BaseDataSet.ConversionFactors")
### * BaseDataSet.ConversionFactors

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BaseDataSet.ConversionFactors
### Title: Base data set of dose-rate conversion factors
### Aliases: BaseDataSet.ConversionFactors
### Keywords: datasets

### ** Examples


## Load data
data("BaseDataSet.ConversionFactors", envir = environment())




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BaseDataSet.ConversionFactors", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BaseDataSet.CosmicDoseRate")
### * BaseDataSet.CosmicDoseRate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BaseDataSet.CosmicDoseRate
### Title: Base data set for cosmic dose rate calculation
### Aliases: BaseDataSet.CosmicDoseRate values.cosmic.Softcomp
###   values.factor.Altitude values.par.FJH
### Keywords: datasets

### ** Examples


##load data
data(BaseDataSet.CosmicDoseRate)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BaseDataSet.CosmicDoseRate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BaseDataSet.FractionalGammaDose")
### * BaseDataSet.FractionalGammaDose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BaseDataSet.FractionalGammaDose
### Title: Base data set of fractional gamma-dose values
### Aliases: BaseDataSet.FractionalGammaDose
### Keywords: datasets

### ** Examples


## Load data
data("BaseDataSet.FractionalGammaDose", envir = environment())




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BaseDataSet.FractionalGammaDose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("BaseDataSet.GrainSizeAttenuation")
### * BaseDataSet.GrainSizeAttenuation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BaseDataSet.GrainSizeAttenuation
### Title: Base dataset for grain size attenuation data by Guérin et al.
###   (2012)
### Aliases: BaseDataSet.GrainSizeAttenuation
### Keywords: datasets

### ** Examples


## load data
data("BaseDataSet.GrainSizeAttenuation", envir = environment())




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BaseDataSet.GrainSizeAttenuation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CW2pHMi")
### * CW2pHMi

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CW2pHMi
### Title: Transform a CW-OSL curve into a pHM-OSL curve via interpolation
###   under hyperbolic modulation conditions
### Aliases: CW2pHMi
### Keywords: manip

### ** Examples


##(1) - simple transformation

##load CW-OSL curve data
data(ExampleData.CW_OSL_Curve, envir = environment())

##transform values
values.transformed<-CW2pHMi(ExampleData.CW_OSL_Curve)

##plot
plot(values.transformed$x, values.transformed$y.t, log = "x")

##(2) - load CW-OSL curve from BIN-file and plot transformed values

##load BINfile
#BINfileData<-readBIN2R("[path to BIN-file]")
data(ExampleData.BINfileData, envir = environment())

##grep first CW-OSL curve from ALQ 1
curve.ID<-CWOSL.SAR.Data@METADATA[CWOSL.SAR.Data@METADATA[,"LTYPE"]=="OSL" &
                                    CWOSL.SAR.Data@METADATA[,"POSITION"]==1
                                  ,"ID"]

curve.HIGH<-CWOSL.SAR.Data@METADATA[CWOSL.SAR.Data@METADATA[,"ID"]==curve.ID[1]
                                    ,"HIGH"]

curve.NPOINTS<-CWOSL.SAR.Data@METADATA[CWOSL.SAR.Data@METADATA[,"ID"]==curve.ID[1]
                                       ,"NPOINTS"]

##combine curve to data set

curve<-data.frame(x = seq(curve.HIGH/curve.NPOINTS,curve.HIGH,
                          by = curve.HIGH/curve.NPOINTS),
                  y=unlist(CWOSL.SAR.Data@DATA[curve.ID[1]]))


##transform values

curve.transformed <- CW2pHMi(curve)

##plot curve
plot(curve.transformed$x, curve.transformed$y.t, log = "x")


##(3) - produce Fig. 4 from Bos & Wallinga (2012)

##load data
data(ExampleData.CW_OSL_Curve, envir = environment())
values <- CW_Curve.BosWallinga2012

##open plot area
plot(NA, NA,
     xlim=c(0.001,10),
     ylim=c(0,8000),
     ylab="pseudo OSL (cts/0.01 s)",
     xlab="t [s]",
     log="x",
     main="Fig. 4 - Bos & Wallinga (2012)")

values.t<-CW2pLMi(values, P=1/20)
lines(values[1:length(values.t[,1]),1],CW2pLMi(values, P=1/20)[,2],
      col="red" ,lwd=1.3)
text(0.03,4500,"LM", col="red" ,cex=.8)

values.t<-CW2pHMi(values, delta=40)
lines(values[1:length(values.t[,1]),1],CW2pHMi(values, delta=40)[,2],
      col="black", lwd=1.3)
text(0.005,3000,"HM", cex=.8)

values.t<-CW2pPMi(values, P=1/10)
lines(values[1:length(values.t[,1]),1],CW2pPMi(values, P=1/10)[,2],
      col="blue", lwd=1.3)
text(0.5,6500,"PM", col="blue" ,cex=.8)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CW2pHMi", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CW2pLM")
### * CW2pLM

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CW2pLM
### Title: Transform a CW-OSL curve into a pLM-OSL curve
### Aliases: CW2pLM
### Keywords: manip

### ** Examples


##read curve from CWOSL.SAR.Data transform curve and plot values
data(ExampleData.BINfileData, envir = environment())

##read id for the 1st OSL curve
id.OSL <- CWOSL.SAR.Data@METADATA[CWOSL.SAR.Data@METADATA[,"LTYPE"] == "OSL","ID"]

##produce x and y (time and count data for the data set)
x<-seq(CWOSL.SAR.Data@METADATA[id.OSL[1],"HIGH"]/CWOSL.SAR.Data@METADATA[id.OSL[1],"NPOINTS"],
       CWOSL.SAR.Data@METADATA[id.OSL[1],"HIGH"],
       by = CWOSL.SAR.Data@METADATA[id.OSL[1],"HIGH"]/CWOSL.SAR.Data@METADATA[id.OSL[1],"NPOINTS"])
y <- unlist(CWOSL.SAR.Data@DATA[id.OSL[1]])
values <- data.frame(x,y)

##transform values
values.transformed <- CW2pLM(values)

##plot
plot(values.transformed)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CW2pLM", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CW2pLMi")
### * CW2pLMi

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CW2pLMi
### Title: Transform a CW-OSL curve into a pLM-OSL curve via interpolation
###   under linear modulation conditions
### Aliases: CW2pLMi
### Keywords: manip

### ** Examples


##(1)
##load CW-OSL curve data
data(ExampleData.CW_OSL_Curve, envir = environment())

##transform values
values.transformed <- CW2pLMi(ExampleData.CW_OSL_Curve)

##plot
plot(values.transformed$x, values.transformed$y.t, log = "x")

##(2) - produce Fig. 4 from Bos & Wallinga (2012)
##load data
data(ExampleData.CW_OSL_Curve, envir = environment())
values <- CW_Curve.BosWallinga2012

##open plot area
plot(NA, NA,
     xlim = c(0.001,10),
     ylim = c(0,8000),
     ylab = "pseudo OSL (cts/0.01 s)",
     xlab = "t [s]",
     log = "x",
     main = "Fig. 4 - Bos & Wallinga (2012)")


values.t <- CW2pLMi(values, P = 1/20)
lines(values[1:length(values.t[,1]),1],CW2pLMi(values, P = 1/20)[,2],
      col = "red", lwd = 1.3)
text(0.03,4500,"LM", col = "red", cex = .8)

values.t <- CW2pHMi(values, delta = 40)
lines(values[1:length(values.t[,1]),1],CW2pHMi(values, delta = 40)[,2],
      col = "black", lwd = 1.3)
text(0.005,3000,"HM", cex =.8)

values.t <- CW2pPMi(values, P = 1/10)
lines(values[1:length(values.t[,1]),1], CW2pPMi(values, P = 1/10)[,2],
      col = "blue", lwd = 1.3)
text(0.5,6500,"PM", col = "blue", cex = .8)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CW2pLMi", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("CW2pPMi")
### * CW2pPMi

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CW2pPMi
### Title: Transform a CW-OSL curve into a pPM-OSL curve via interpolation
###   under parabolic modulation conditions
### Aliases: CW2pPMi
### Keywords: manip

### ** Examples



##(1)
##load CW-OSL curve data
data(ExampleData.CW_OSL_Curve, envir = environment())

##transform values
values.transformed <- CW2pPMi(ExampleData.CW_OSL_Curve)

##plot
plot(values.transformed$x,values.transformed$y.t, log = "x")

##(2) - produce Fig. 4 from Bos & Wallinga (2012)

##load data
data(ExampleData.CW_OSL_Curve, envir = environment())
values <- CW_Curve.BosWallinga2012

##open plot area
plot(NA, NA,
     xlim = c(0.001,10),
     ylim = c(0,8000),
     ylab = "pseudo OSL (cts/0.01 s)",
     xlab = "t [s]",
     log = "x",
     main = "Fig. 4 - Bos & Wallinga (2012)")

values.t <- CW2pLMi(values, P = 1/20)
lines(values[1:length(values.t[,1]),1],CW2pLMi(values, P = 1/20)[,2],
      col = "red",lwd = 1.3)
text(0.03,4500,"LM", col = "red", cex = .8)

values.t <- CW2pHMi(values, delta = 40)
lines(values[1:length(values.t[,1]),1], CW2pHMi(values, delta = 40)[,2],
      col = "black", lwd = 1.3)
text(0.005,3000,"HM", cex = .8)

values.t <- CW2pPMi(values, P = 1/10)
lines(values[1:length(values.t[,1]),1], CW2pPMi(values, P = 1/10)[,2],
      col = "blue", lwd = 1.3)
text(0.5,6500,"PM", col = "blue", cex = .8)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CW2pPMi", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.Al2O3C")
### * ExampleData.Al2O3C

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.Al2O3C
### Title: Example Al2O3:C Measurement Data
### Aliases: ExampleData.Al2O3C data_CrossTalk data_ITC
### Keywords: datasets

### ** Examples


##(1) curves
data(ExampleData.Al2O3C, envir = environment())
plot_RLum(data_ITC[1:2])




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.Al2O3C", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.BINfileData")
### * ExampleData.BINfileData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.BINfileData
### Title: Example data from a SAR OSL and SAR TL measurement for the
###   package Luminescence
### Aliases: ExampleData.BINfileData CWOSL.SAR.Data TL.SAR.Data
### Keywords: datasets

### ** Examples


## show first 5 elements of the METADATA and DATA elements in the terminal
data(ExampleData.BINfileData, envir = environment())
CWOSL.SAR.Data@METADATA[1:5,]
CWOSL.SAR.Data@DATA[1:5]




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.BINfileData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.CW_OSL_Curve")
### * ExampleData.CW_OSL_Curve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.CW_OSL_Curve
### Title: Example CW-OSL curve data for the package Luminescence
### Aliases: ExampleData.CW_OSL_Curve CW_Curve.BosWallinga2012
### Keywords: datasets

### ** Examples


data(ExampleData.CW_OSL_Curve, envir = environment())
plot(ExampleData.CW_OSL_Curve)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.CW_OSL_Curve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.CobbleData")
### * ExampleData.CobbleData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.CobbleData
### Title: Example data for calc_CobbleDoseRate()
### Aliases: ExampleData.CobbleData
### Keywords: datasets

### ** Examples


## Load data
data("ExampleData.CobbleData", envir = environment())




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.CobbleData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.DeValues")
### * ExampleData.DeValues

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.DeValues
### Title: Example De data sets for the package Luminescence
### Aliases: ExampleData.DeValues
### Keywords: datasets

### ** Examples


##(1) plot values as histogram
data(ExampleData.DeValues, envir = environment())
plot_Histogram(ExampleData.DeValues$BT998, xlab = "De [s]")

##(2) plot values as histogram (with second to gray conversion)
data(ExampleData.DeValues, envir = environment())

De.values <- Second2Gray(ExampleData.DeValues$BT998,
                         dose.rate = c(0.0438, 0.0019))


plot_Histogram(De.values, xlab = "De [Gy]")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.DeValues", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.Fading")
### * ExampleData.Fading

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.Fading
### Title: Example data for feldspar fading measurements
### Aliases: ExampleData.Fading
### Keywords: datasets

### ** Examples


## Load example data
data("ExampleData.Fading", envir = environment())

## Get fading measurement data of the IR50 signal
IR50_fading <- ExampleData.Fading$fading.data$IR50
head(IR50_fading)

## Determine g-value and rho' for the IR50 signal
IR50_fading.res <- analyse_FadingMeasurement(IR50_fading)

## Show g-value and rho' results
gval <- get_RLum(IR50_fading.res)
rhop <- get_RLum(IR50_fading.res, "rho_prime")

gval
rhop

## Get LxTx values of the IR50 DE measurement
IR50_De.LxTx <- ExampleData.Fading$equivalentDose.data$IR50

## Calculate the De of the IR50 signal
IR50_De <- plot_GrowthCurve(IR50_De.LxTx,
                                mode = "interpolation",
                                fit.method = "EXP")

## Extract the calculated De and its error
IR50_De.res <- get_RLum(IR50_De)
De <- c(IR50_De.res$De, IR50_De.res$De.Error)

## Apply fading correction (age conversion greatly simplified)
IR50_Age <- De / 7.00
IR50_Age.corr <- calc_FadingCorr(IR50_Age, g_value = IR50_fading.res)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.Fading", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.FittingLM")
### * ExampleData.FittingLM

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.FittingLM
### Title: Example data for fit_LMCurve() in the package Luminescence
### Aliases: ExampleData.FittingLM values.curve values.curveBG

### ** Examples


##show LM data
data(ExampleData.FittingLM, envir = environment())
plot(values.curve,log="x")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.FittingLM", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.LxTxData")
### * ExampleData.LxTxData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.LxTxData
### Title: Example Lx/Tx data from CW-OSL SAR measurement
### Aliases: ExampleData.LxTxData LxTxData

### ** Examples


## plot Lx/Tx data vs dose [s]
data(ExampleData.LxTxData, envir = environment())
plot(LxTxData$Dose,LxTxData$LxTx)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.LxTxData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.LxTxOSLData")
### * ExampleData.LxTxOSLData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.LxTxOSLData
### Title: Example Lx and Tx curve data from an artificial OSL measurement
### Aliases: ExampleData.LxTxOSLData Lx.data Tx.data

### ** Examples


##load data
data(ExampleData.LxTxOSLData, envir = environment())

##plot data
plot(Lx.data)
plot(Tx.data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.LxTxOSLData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.MortarData")
### * ExampleData.MortarData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.MortarData
### Title: Example equivalent dose data from mortar samples
### Aliases: ExampleData.MortarData MortarData

### ** Examples


##load data
data(ExampleData.MortarData, envir = environment())

##plot data
plot(MortarData)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.MortarData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.RLum.Analysis")
### * ExampleData.RLum.Analysis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.RLum.Analysis
### Title: Example data as RLum.Analysis objects
### Aliases: ExampleData.RLum.Analysis IRSAR.RF.Data
### Keywords: datasets

### ** Examples


##load data
data(ExampleData.RLum.Analysis, envir = environment())

##plot data
plot_RLum(IRSAR.RF.Data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.RLum.Analysis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.RLum.Data.Image")
### * ExampleData.RLum.Data.Image

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.RLum.Data.Image
### Title: Example data as RLum.Data.Image objects
### Aliases: ExampleData.RLum.Data.Image
### Keywords: datasets

### ** Examples


##load data
data(ExampleData.RLum.Data.Image, envir = environment())

##plot data
plot_RLum(ExampleData.RLum.Data.Image)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.RLum.Data.Image", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.ScaleGammaDose")
### * ExampleData.ScaleGammaDose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.ScaleGammaDose
### Title: Example data for scale_GammaDose()
### Aliases: ExampleData.ScaleGammaDose
### Keywords: datasets

### ** Examples


## Load data
data("ExampleData.ScaleGammaDose", envir = environment())




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.ScaleGammaDose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.SurfaceExposure")
### * ExampleData.SurfaceExposure

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.SurfaceExposure
### Title: Example OSL surface exposure dating data
### Aliases: ExampleData.SurfaceExposure
### Keywords: datasets

### ** Examples


## ExampleData.SurfaceExposure$sample_1
sigmaphi <- 5e-10
age <- 10000
mu <- 0.9
x <- seq(0, 10, 0.1)
fun <- exp(-sigmaphi * age * 365.25*24*3600 * exp(-mu * x))

set.seed(666)
synth_1 <- data.frame(depth = x,
                      intensity = jitter(fun, 1, 0.1),
                      error = runif(length(x), 0.01, 0.2))

## VALIDATE sample_1
fit_SurfaceExposure(synth_1, mu = mu, sigmaphi = sigmaphi)




## ExampleData.SurfaceExposure$sample_2
sigmaphi <- 5e-10
age <- 10000
mu <- 0.9
x <- seq(0, 10, 0.1)
Ddot <- 2.5   / 1000 / 365.25 / 24 / 60 / 60 # 2.5 Gy/ka in Seconds
D0 <- 40
fun <- (sigmaphi * exp(-mu * x) *
          exp(-(age * 365.25*24*3600) *
                (sigmaphi * exp(-mu * x) + Ddot/D0)) + Ddot/D0) /
  (sigmaphi * exp(-mu * x) + Ddot/D0)

set.seed(666)
synth_2 <- data.frame(depth = x,
                      intensity = jitter(fun, 1, 0.1),
                      error = runif(length(x), 0.01, 0.2))

## VALIDATE sample_2
fit_SurfaceExposure(synth_2, mu = mu, sigmaphi = sigmaphi, Ddot = 2.5, D0 = D0)



## ExampleData.SurfaceExposure$set_1
sigmaphi <- 5e-10
mu <- 0.9
x <- seq(0, 15, 0.2)
age <- c(1e3, 1e4, 1e5, 1e6)
set.seed(666)

synth_3 <- vector("list", length = length(age))

for (i in 1:length(age)) {
  fun <- exp(-sigmaphi * age[i] * 365.25*24*3600 * exp(-mu * x))
  synth_3[[i]] <- data.frame(depth = x,
                             intensity = jitter(fun, 1, 0.05))
}


## VALIDATE set_1
fit_SurfaceExposure(synth_3, age = age, sigmaphi = sigmaphi)



## ExampleData.SurfaceExposure$set_2
sigmaphi <- 5e-10
mu <- 0.9
x <- seq(0, 15, 0.2)
age <- c(1e2, 1e3, 1e4, 1e5, 1e6)
Ddot <- 1.0 / 1000 / 365.25 / 24 / 60 / 60 # 2.0 Gy/ka in Seconds
D0 <- 40
set.seed(666)

synth_4 <- vector("list", length = length(age))

for (i in 1:length(age)) {
  fun <- (sigmaphi * exp(-mu * x) *
            exp(-(age[i] * 365.25*24*3600) *
                  (sigmaphi * exp(-mu * x) + Ddot/D0)) + Ddot/D0) /
    (sigmaphi * exp(-mu * x) + Ddot/D0)

  synth_4[[i]] <- data.frame(depth = x,
                             intensity = jitter(fun, 1, 0.05))
}


## VALIDATE set_2
fit_SurfaceExposure(synth_4, age = age, sigmaphi = sigmaphi, D0 = D0, Ddot = 1.0)

## Not run: 
##D ExampleData.SurfaceExposure <- list(
##D   sample_1 = synth_1,
##D   sample_2 = synth_2,
##D   set_1 = synth_3,
##D   set_2 = synth_4
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.SurfaceExposure", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.TR_OSL")
### * ExampleData.TR_OSL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.TR_OSL
### Title: Example TR-OSL data
### Aliases: ExampleData.TR_OSL
### Keywords: datasets

### ** Examples


##(1) curves
data(ExampleData.TR_OSL, envir = environment())
plot_RLum(ExampleData.TR_OSL)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.TR_OSL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.XSYG")
### * ExampleData.XSYG

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.XSYG
### Title: Example data for a SAR OSL measurement and a TL spectrum using a
###   lexsyg reader
### Aliases: ExampleData.XSYG OSL.SARMeasurement TL.Spectrum
### Keywords: datasets

### ** Examples

##show data
data(ExampleData.XSYG, envir = environment())

## =========================================
##(1) OSL.SARMeasurement
OSL.SARMeasurement

##show $Sequence.Object
OSL.SARMeasurement$Sequence.Object

##grep OSL curves and plot the first curve
OSLcurve <- get_RLum(OSL.SARMeasurement$Sequence.Object,
recordType="OSL")[[1]]
plot_RLum(OSLcurve)

## =========================================
##(2) TL.Spectrum
TL.Spectrum

##plot simple spectrum (2D)
plot_RLum.Data.Spectrum(TL.Spectrum,
                        plot.type="contour",
                        xlim = c(310,750),
                        ylim = c(0,300),
                        bin.rows=10,
                        bin.cols = 1)

##plot 3d spectrum (uncomment for usage)
# plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="persp",
# xlim = c(310,750), ylim = c(0,300), bin.rows=10,
# bin.cols = 1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.XSYG", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ExampleData.portableOSL")
### * ExampleData.portableOSL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ExampleData.portableOSL
### Title: Example portable OSL curve data for the package Luminescence
### Aliases: ExampleData.portableOSL
### Keywords: datasets

### ** Examples


data(ExampleData.portableOSL, envir = environment())
plot_RLum(ExampleData.portableOSL)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ExampleData.portableOSL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GitHub-API")
### * GitHub-API

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GitHub-API
### Title: GitHub API
### Aliases: GitHub-API github_commits github_branches github_issues

### ** Examples


## Not run: 
##D github_branches(user = "r-lum", repo = "luminescence")
##D github_issues(user = "r-lum", repo = "luminescence")
##D github_commits(user = "r-lum", repo = "luminescence", branch = "master", n = 10)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GitHub-API", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PSL2Risoe.BINfileData")
### * PSL2Risoe.BINfileData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PSL2Risoe.BINfileData
### Title: Convert portable OSL data to a Risoe.BINfileData object
### Aliases: PSL2Risoe.BINfileData
### Keywords: IO

### ** Examples


# (1) load and plot example data set
data("ExampleData.portableOSL", envir = environment())
plot_RLum(ExampleData.portableOSL)

# (2) merge all RLum.Analysis objects into one
merged <- merge_RLum(ExampleData.portableOSL)
merged

# (3) convert to RisoeBINfile object
bin <- PSL2Risoe.BINfileData(merged)
bin

# (4) write Risoe BIN file
## Not run: 
##D write_R2BIN(bin, "~/portableOSL.binx")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PSL2Risoe.BINfileData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RLum-class")
### * RLum-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RLum-class
### Title: Class '"RLum"'
### Aliases: RLum-class replicate_RLum,RLum-method
### Keywords: classes

### ** Examples


showClass("RLum")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RLum-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RLum.Analysis-class")
### * RLum.Analysis-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RLum.Analysis-class
### Title: Class '"RLum.Analysis"'
### Aliases: RLum.Analysis-class show,RLum.Analysis-method
###   set_RLum,RLum.Analysis-method get_RLum,RLum.Analysis-method
###   structure_RLum,RLum.Analysis-method length_RLum,RLum.Analysis-method
###   names_RLum,RLum.Analysis-method smooth_RLum,RLum.Analysis-method
### Keywords: classes internal methods

### ** Examples


showClass("RLum.Analysis")

##set empty object
set_RLum(class = "RLum.Analysis")

###use example data
##load data
data(ExampleData.RLum.Analysis, envir = environment())

##show curves in object
get_RLum(IRSAR.RF.Data)

##show only the first object, but by keeping the object
get_RLum(IRSAR.RF.Data, record.id = 1, drop = FALSE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RLum.Analysis-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RLum.Data-class")
### * RLum.Data-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RLum.Data-class
### Title: Class '"RLum.Data"'
### Aliases: RLum.Data-class
### Keywords: classes internal

### ** Examples


showClass("RLum.Data")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RLum.Data-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RLum.Data.Curve-class")
### * RLum.Data.Curve-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RLum.Data.Curve-class
### Title: Class '"RLum.Data.Curve"'
### Aliases: RLum.Data.Curve-class show,RLum.Data.Curve-method
###   set_RLum,RLum.Data.Curve-method get_RLum,RLum.Data.Curve-method
###   length_RLum,RLum.Data.Curve-method names_RLum,RLum.Data.Curve-method
###   bin_RLum.Data,RLum.Data.Curve-method
###   smooth_RLum,RLum.Data.Curve-method
### Keywords: classes internal

### ** Examples


showClass("RLum.Data.Curve")

##set empty curve object
set_RLum(class = "RLum.Data.Curve")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RLum.Data.Curve-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RLum.Data.Image-class")
### * RLum.Data.Image-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RLum.Data.Image-class
### Title: Class '"RLum.Data.Image"'
### Aliases: RLum.Data.Image-class show,RLum.Data.Image-method
###   set_RLum,RLum.Data.Image-method get_RLum,RLum.Data.Image-method
###   names_RLum,RLum.Data.Image-method
### Keywords: classes internal

### ** Examples


showClass("RLum.Data.Image")

##create empty RLum.Data.Image object
set_RLum(class = "RLum.Data.Image")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RLum.Data.Image-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RLum.Data.Spectrum-class")
### * RLum.Data.Spectrum-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RLum.Data.Spectrum-class
### Title: Class '"RLum.Data.Spectrum"'
### Aliases: RLum.Data.Spectrum-class show,RLum.Data.Spectrum-method
###   set_RLum,RLum.Data.Spectrum-method get_RLum,RLum.Data.Spectrum-method
###   names_RLum,RLum.Data.Spectrum-method
###   bin_RLum.Data,RLum.Data.Spectrum-method
### Keywords: classes internal

### ** Examples


showClass("RLum.Data.Spectrum")

##show example data
data(ExampleData.XSYG, envir = environment())
TL.Spectrum

##show data matrix
get_RLum(TL.Spectrum)

##plot spectrum
## Not run: 
##D plot_RLum(TL.Spectrum)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RLum.Data.Spectrum-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("RLum.Results-class")
### * RLum.Results-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: RLum.Results-class
### Title: Class '"RLum.Results"'
### Aliases: RLum.Results-class show,RLum.Results-method
###   set_RLum,RLum.Results-method get_RLum,RLum.Results-method
###   length_RLum,RLum.Results-method names_RLum,RLum.Results-method
### Keywords: classes internal methods

### ** Examples


showClass("RLum.Results")

##create an empty object from this class
set_RLum(class = "RLum.Results")

##use another function to show how it works

##Basic calculation of the dose rate for a specific date
 dose.rate <-  calc_SourceDoseRate(
   measurement.date = "2012-01-27",
   calib.date = "2014-12-19",
   calib.dose.rate = 0.0438,
   calib.error = 0.0019)

##show object
dose.rate

##get results
get_RLum(dose.rate)

##get parameters used for the calcualtion from the same object
get_RLum(dose.rate, data.object = "parameters")

##alternatively objects can be accessed using S3 generics, such as
dose.rate$parameters




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("RLum.Results-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Risoe.BINfileData-class")
### * Risoe.BINfileData-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Risoe.BINfileData-class
### Title: Class '"Risoe.BINfileData"'
### Aliases: Risoe.BINfileData-class show,Risoe.BINfileData-method
###   set_Risoe.BINfileData,ANY-method
###   get_Risoe.BINfileData,Risoe.BINfileData-method
### Keywords: classes internal

### ** Examples


showClass("Risoe.BINfileData")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Risoe.BINfileData-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Risoe.BINfileData2RLum.Analysis")
### * Risoe.BINfileData2RLum.Analysis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Risoe.BINfileData2RLum.Analysis
### Title: Convert Risoe.BINfileData object to an RLum.Analysis object
### Aliases: Risoe.BINfileData2RLum.Analysis
### Keywords: manip

### ** Examples


##load data
data(ExampleData.BINfileData, envir = environment())

##convert values for position 1
Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Risoe.BINfileData2RLum.Analysis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Second2Gray")
### * Second2Gray

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Second2Gray
### Title: Converting equivalent dose values from seconds (s) to Gray (Gy)
### Aliases: Second2Gray
### Keywords: manip

### ** Examples


##(A) for known source dose rate at date of measurement
## - load De data from the example data help file
data(ExampleData.DeValues, envir = environment())
## - convert De(s) to De(Gy)
Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))





##(B) for source dose rate calibration data
## - calculate source dose rate first
dose.rate <-  calc_SourceDoseRate(measurement.date = "2012-01-27",
                                  calib.date = "2014-12-19",
                                  calib.dose.rate = 0.0438,
                                  calib.error = 0.0019)
# read example data
data(ExampleData.DeValues, envir = environment())

# apply dose.rate to convert De(s) to De(Gy)
Second2Gray(ExampleData.DeValues$BT998, dose.rate)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Second2Gray", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_Al2O3C_CrossTalk")
### * analyse_Al2O3C_CrossTalk

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_Al2O3C_CrossTalk
### Title: Al2O3:C Reader Cross Talk Analysis
### Aliases: analyse_Al2O3C_CrossTalk
### Keywords: datagen

### ** Examples


##load data
data(ExampleData.Al2O3C, envir = environment())

##run analysis
analyse_Al2O3C_CrossTalk(data_CrossTalk)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_Al2O3C_CrossTalk", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_Al2O3C_ITC")
### * analyse_Al2O3C_ITC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_Al2O3C_ITC
### Title: Al2O3 Irradiation Time Correction Analysis
### Aliases: analyse_Al2O3C_ITC
### Keywords: datagen

### ** Examples


##load data
data(ExampleData.Al2O3C, envir = environment())

##run analysis
analyse_Al2O3C_ITC(data_ITC)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_Al2O3C_ITC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_Al2O3C_Measurement")
### * analyse_Al2O3C_Measurement

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_Al2O3C_Measurement
### Title: Al2O3:C Passive Dosimeter Measurement Analysis
### Aliases: analyse_Al2O3C_Measurement
### Keywords: datagen

### ** Examples

##load data
data(ExampleData.Al2O3C, envir = environment())

##run analysis
analyse_Al2O3C_Measurement(data_CrossTalk)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_Al2O3C_Measurement", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_FadingMeasurement")
### * analyse_FadingMeasurement

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_FadingMeasurement
### Title: Analyse fading measurements and returns the fading rate per
###   decade (g-value)
### Aliases: analyse_FadingMeasurement
### Keywords: datagen

### ** Examples


## load example data (sample UNIL/NB123, see ?ExampleData.Fading)
data("ExampleData.Fading", envir = environment())

##(1) get fading measurement data (here a three column data.frame)
fading_data <- ExampleData.Fading$fading.data$IR50

##(2) run analysis
g_value <- analyse_FadingMeasurement(
fading_data,
plot = TRUE,
verbose = TRUE,
n.MC = 10)

##(3) this can be further used in the function
## to correct the age according to Huntley & Lamothe, 2001
results <- calc_FadingCorr(
age.faded = c(100,2),
g_value = g_value,
n.MC = 10)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_FadingMeasurement", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_IRSAR.RF")
### * analyse_IRSAR.RF

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_IRSAR.RF
### Title: Analyse IRSAR RF measurements
### Aliases: analyse_IRSAR.RF
### Keywords: datagen

### ** Examples


##load data
data(ExampleData.RLum.Analysis, envir = environment())

##(1) perform analysis using the method 'FIT'
results <- analyse_IRSAR.RF(object = IRSAR.RF.Data)

##show De results and test paramter results
get_RLum(results, data.object = "data")
get_RLum(results, data.object = "test_parameters")

##(2) perform analysis using the method 'SLIDE'
results <- analyse_IRSAR.RF(object = IRSAR.RF.Data, method = "SLIDE", n.MC = 1)

## Not run: 
##D ##(3) perform analysis using the method 'SLIDE' and method control option
##D ## 'trace
##D results <- analyse_IRSAR.RF(
##D  object = IRSAR.RF.Data,
##D  method = "SLIDE",
##D  method.control = list(trace = TRUE))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_IRSAR.RF", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_SAR.CWOSL")
### * analyse_SAR.CWOSL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_SAR.CWOSL
### Title: Analyse SAR CW-OSL measurements
### Aliases: analyse_SAR.CWOSL
### Keywords: datagen plot

### ** Examples


##load data
##ExampleData.BINfileData contains two BINfileData objects
##CWOSL.SAR.Data and TL.SAR.Data
data(ExampleData.BINfileData, envir = environment())

##transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##perform SAR analysis and set rejection criteria
results <- analyse_SAR.CWOSL(
object = object,
signal.integral.min = 1,
signal.integral.max = 2,
background.integral.min = 900,
background.integral.max = 1000,
log = "x",
fit.method = "EXP",
rejection.criteria = list(
  recycling.ratio = 10,
  recuperation.rate = 10,
  testdose.error = 10,
  palaeodose.error = 10,
  recuperation_reference = "Natural",
  exceed.max.regpoint = TRUE)
)

##show De results
get_RLum(results)

##show LnTnLxTx table
get_RLum(results, data.object = "LnLxTnTx.table")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_SAR.CWOSL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_SAR.TL")
### * analyse_SAR.TL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_SAR.TL
### Title: Analyse SAR TL measurements
### Aliases: analyse_SAR.TL
### Keywords: datagen plot

### ** Examples


##load data
data(ExampleData.BINfileData, envir = environment())

##transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos=3)

##perform analysis
analyse_SAR.TL(
 object = object,
 signal.integral.min = 210,
 signal.integral.max = 220,
 fit.method = "EXP OR LIN",
 sequence.structure = c("SIGNAL", "BACKGROUND"))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_SAR.TL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_baSAR")
### * analyse_baSAR

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_baSAR
### Title: Bayesian models (baSAR) applied on luminescence data
### Aliases: analyse_baSAR
### Keywords: datagen

### ** Examples


##(1) load package test data set
data(ExampleData.BINfileData, envir = environment())

##(2) selecting relevant curves, and limit dataset
CWOSL.SAR.Data <- subset(
  CWOSL.SAR.Data,
  subset = POSITION%in%c(1:3) & LTYPE == "OSL")

## Not run: 
##D ##(3) run analysis
##D ##please not that the here selected parameters are
##D ##choosen for performance, not for reliability
##D results <- analyse_baSAR(
##D   object = CWOSL.SAR.Data,
##D   source_doserate = c(0.04, 0.001),
##D   signal.integral = c(1:2),
##D   background.integral = c(80:100),
##D   fit.method = "LIN",
##D   plot = FALSE,
##D   n.MCMC = 200
##D 
##D )
##D 
##D print(results)
##D 
##D 
##D ##XLS_file template
##D ##copy and paste this the code below in the terminal
##D ##you can further use the function write.csv() to export the example
##D 
##D XLS_file <-
##D structure(
##D list(
##D  BIN_FILE = NA_character_,
##D  DISC = NA_real_,
##D  GRAIN = NA_real_),
##D    .Names = c("BIN_FILE", "DISC", "GRAIN"),
##D    class = "data.frame",
##D    row.names = 1L
##D )
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_baSAR", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_pIRIRSequence")
### * analyse_pIRIRSequence

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_pIRIRSequence
### Title: Analyse post-IR IRSL measurement sequences
### Aliases: analyse_pIRIRSequence
### Keywords: datagen plot

### ** Examples



### NOTE: For this example existing example data are used. These data are non pIRIR data.
###
##(1) Compile example data set based on existing example data (SAR quartz measurement)
##(a) Load example data
data(ExampleData.BINfileData, envir = environment())

##(b) Transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##(c) Grep curves and exclude the last two (one TL and one IRSL)
object <- get_RLum(object, record.id = c(-29,-30))

##(d) Define new sequence structure and set new RLum.Analysis object
sequence.structure  <- c(1,2,2,3,4,4)
sequence.structure <- as.vector(sapply(seq(0,length(object)-1,by = 4),
                                       function(x){sequence.structure + x}))

object <-  sapply(1:length(sequence.structure), function(x){

  object[[sequence.structure[x]]]

})

object <- set_RLum(class = "RLum.Analysis", records = object, protocol = "pIRIR")

##(2) Perform pIRIR analysis (for this example with quartz OSL data!)
## Note: output as single plots to avoid problems with this example
results <- analyse_pIRIRSequence(object,
     signal.integral.min = 1,
     signal.integral.max = 2,
     background.integral.min = 900,
     background.integral.max = 1000,
     fit.method = "EXP",
     sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
     main = "Pseudo pIRIR data set based on quartz OSL",
     plot.single = TRUE)


##(3) Perform pIRIR analysis (for this example with quartz OSL data!)
## Alternative for PDF output, uncomment and complete for usage
## Not run: 
##D tempfile <- tempfile(fileext = ".pdf")
##D pdf(file = tempfile, height = 15, width = 15)
##D   results <- analyse_pIRIRSequence(object,
##D          signal.integral.min = 1,
##D          signal.integral.max = 2,
##D          background.integral.min = 900,
##D          background.integral.max = 1000,
##D          fit.method = "EXP",
##D          main = "Pseudo pIRIR data set based on quartz OSL")
##D 
##D   dev.off()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_pIRIRSequence", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("analyse_portableOSL")
### * analyse_portableOSL

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: analyse_portableOSL
### Title: Analyse portable CW-OSL measurements
### Aliases: analyse_portableOSL
### Keywords: datagen plot

### ** Examples


## example profile plot
# (1) load example data set
data("ExampleData.portableOSL", envir = environment())

# (2) merge and plot all RLum.Analysis objects
merged <- merge_RLum(ExampleData.portableOSL)
plot_RLum(
 object = merged,
 combine = TRUE,
 records_max = 5,
 legend.pos = "outside")
merged

# (3) analyse and plot
results <- analyse_portableOSL(
  merged,
  signal.integral = 1:5,
  invert = FALSE,
  normalise = TRUE)
get_RLum(results)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("analyse_portableOSL", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("apply_CosmicRayRemoval")
### * apply_CosmicRayRemoval

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apply_CosmicRayRemoval
### Title: Function to remove cosmic rays from an RLum.Data.Spectrum S4
###   class object
### Aliases: apply_CosmicRayRemoval
### Keywords: manip

### ** Examples


##(1) - use with your own data and combine (uncomment for usage)
## run two times the default method and smooth with another method
## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "Pych")
## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "Pych")
## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "smooth")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apply_CosmicRayRemoval", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("apply_EfficiencyCorrection")
### * apply_EfficiencyCorrection

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apply_EfficiencyCorrection
### Title: Function to apply spectral efficiency correction to
###   RLum.Data.Spectrum S4 class objects
### Aliases: apply_EfficiencyCorrection
### Keywords: manip

### ** Examples


##(1) - use with your own data (uncomment for usage)
## spectral.efficiency <- read.csv("your data")
##
## your.spectrum <- apply_EfficiencyCorrection(your.spectrum, )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apply_EfficiencyCorrection", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bin_RLum.Data")
### * bin_RLum.Data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bin_RLum.Data
### Title: Channel binning - method dispatcher
### Aliases: bin_RLum.Data
### Keywords: utilities

### ** Examples


##load example data
data(ExampleData.CW_OSL_Curve, envir = environment())

##create RLum.Data.Curve object from this example
curve <-
  set_RLum(
      class = "RLum.Data.Curve",
      recordType = "OSL",
      data = as.matrix(ExampleData.CW_OSL_Curve)
  )

##plot data without and with 2 and 4 channel binning
plot_RLum(curve)
plot_RLum(bin_RLum.Data(curve, bin_size = 2))
plot_RLum(bin_RLum.Data(curve, bin_size = 4))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bin_RLum.Data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_AliquotSize")
### * calc_AliquotSize

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_AliquotSize
### Title: Estimate the amount of grains on an aliquot
### Aliases: calc_AliquotSize

### ** Examples


## Estimate the amount of grains on a small aliquot
calc_AliquotSize(grain.size = c(100,150), sample.diameter = 1, MC.iter = 100)

## Calculate the mean packing density of large aliquots
calc_AliquotSize(grain.size = c(100,200), sample.diameter = 8,
                 grains.counted = c(2525,2312,2880), MC.iter = 100)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_AliquotSize", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_AverageDose")
### * calc_AverageDose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_AverageDose
### Title: Calculate the Average Dose and the dose rate dispersion
### Aliases: calc_AverageDose
### Keywords: datagen

### ** Examples


##Example 01 using package example data
##load example data
data(ExampleData.DeValues, envir = environment())

##calculate Average dose
##(use only the first 56 values here)
AD <- calc_AverageDose(ExampleData.DeValues$CA1[1:56,], sigma_m = 0.1)

##plot De and set Average dose as central value
plot_AbanicoPlot(
 data = ExampleData.DeValues$CA1[1:56,],
 z.0 = AD$summary$AVERAGE_DOSE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_AverageDose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_CentralDose")
### * calc_CentralDose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_CentralDose
### Title: Apply the central age model (CAM) after Galbraith et al. (1999)
###   to a given De distribution
### Aliases: calc_CentralDose

### ** Examples


##load example data
data(ExampleData.DeValues, envir = environment())

##apply the central dose model
calc_CentralDose(ExampleData.DeValues$CA1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_CentralDose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_CobbleDoseRate")
### * calc_CobbleDoseRate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_CobbleDoseRate
### Title: Calculate dose rate of slices in a spherical cobble
### Aliases: calc_CobbleDoseRate
### Keywords: datagen

### ** Examples

## load example data
data("ExampleData.CobbleData", envir = environment())

## run function
calc_CobbleDoseRate(ExampleData.CobbleData)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_CobbleDoseRate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_CommonDose")
### * calc_CommonDose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_CommonDose
### Title: Apply the (un-)logged common age model after Galbraith et al.
###   (1999) to a given De distribution
### Aliases: calc_CommonDose

### ** Examples


## load example data
data(ExampleData.DeValues, envir = environment())

## apply the common dose model
calc_CommonDose(ExampleData.DeValues$CA1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_CommonDose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_CosmicDoseRate")
### * calc_CosmicDoseRate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_CosmicDoseRate
### Title: Calculate the cosmic dose rate
### Aliases: calc_CosmicDoseRate

### ** Examples


##(1) calculate cosmic dose rate (one absorber)
calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                    latitude = 38.06451, longitude = 1.49646,
                    altitude = 364, error = 10)

##(2a) calculate cosmic dose rate (two absorber)
calc_CosmicDoseRate(depth = c(5.0, 2.78), density = c(2.65, 1.7),
                    latitude = 38.06451, longitude = 1.49646,
                    altitude = 364, error = 10)

##(2b) calculate cosmic dose rate (two absorber) and
##correct for geomagnetic field changes
calc_CosmicDoseRate(depth = c(5.0, 2.78), density = c(2.65, 1.7),
                    latitude = 12.04332, longitude = 4.43243,
                    altitude = 364, corr.fieldChanges = TRUE,
                    est.age = 67, error = 15)


##(3) calculate cosmic dose rate and export results to .csv file
#calculate cosmic dose rate and save to variable
results<- calc_CosmicDoseRate(depth = 2.78, density = 1.7,
                              latitude = 38.06451, longitude = 1.49646,
                              altitude = 364, error = 10)

# the results can be accessed by
get_RLum(results, "summary")

#export results to .csv file - uncomment for usage
#write.csv(results, file = "c:/users/public/results.csv")

##(4) calculate cosmic dose rate for 6 samples from the same profile
##    and save to .csv file
#calculate cosmic dose rate and save to variable
results<- calc_CosmicDoseRate(depth = c(0.1, 0.5 , 2.1, 2.7, 4.2, 6.3),
                              density = 1.7, latitude = 38.06451,
                              longitude = 1.49646, altitude = 364,
                              error = 10)

#export results to .csv file - uncomment for usage
#write.csv(results, file = "c:/users/public/results_profile.csv")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_CosmicDoseRate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_FadingCorr")
### * calc_FadingCorr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_FadingCorr
### Title: Fading Correction after Huntley & Lamothe (2001)
### Aliases: calc_FadingCorr
### Keywords: datagen

### ** Examples


##run the examples given in the appendix of Huntley and Lamothe, 2001

##(1) faded age: 100 a
results <- calc_FadingCorr(
   age.faded = c(0.1,0),
   g_value = c(5.0, 1.0),
   tc = 2592000,
   tc.g_value = 172800,
   n.MC = 100)

##(2) faded age: 1 ka
results <- calc_FadingCorr(
   age.faded = c(1,0),
   g_value = c(5.0, 1.0),
   tc = 2592000,
   tc.g_value = 172800,
   n.MC = 100)

##(3) faded age: 10.0 ka
results <- calc_FadingCorr(
   age.faded = c(10,0),
   g_value = c(5.0, 1.0),
   tc = 2592000,
   tc.g_value = 172800,
   n.MC = 100)

##access the last output
get_RLum(results)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_FadingCorr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_FastRatio")
### * calc_FastRatio

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_FastRatio
### Title: Calculate the Fast Ratio for CW-OSL curves
### Aliases: calc_FastRatio

### ** Examples

# load example CW-OSL curve
data("ExampleData.CW_OSL_Curve")

# calculate the fast ratio w/o further adjustments
res <- calc_FastRatio(ExampleData.CW_OSL_Curve)

# show the summary table
get_RLum(res)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_FastRatio", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_FiniteMixture")
### * calc_FiniteMixture

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_FiniteMixture
### Title: Apply the finite mixture model (FMM) after Galbraith (2005) to a
###   given De distribution
### Aliases: calc_FiniteMixture

### ** Examples


## load example data
data(ExampleData.DeValues, envir = environment())

## (1) apply the finite mixture model
## NOTE: the data set is not suitable for the finite mixture model,
## which is why a very small sigmab is necessary
calc_FiniteMixture(ExampleData.DeValues$CA1,
                   sigmab = 0.2, n.components = 2,
                   grain.probability = TRUE)

## (2) repeat the finite mixture model for 2, 3 and 4 maximum number of fitted
## components and save results
## NOTE: The following example is computationally intensive. Please un-comment
## the following lines to make the example work.
FMM<- calc_FiniteMixture(ExampleData.DeValues$CA1,
                         sigmab = 0.2, n.components = c(2:4),
                         pdf.weight = TRUE, dose.scale = c(0, 100))

## show structure of the results
FMM

## show the results on equivalent dose, standard error and proportion of
## fitted components
get_RLum(object = FMM, data.object = "components")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_FiniteMixture", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_FuchsLang2001")
### * calc_FuchsLang2001

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_FuchsLang2001
### Title: Apply the model after Fuchs & Lang (2001) to a given De
###   distribution.
### Aliases: calc_FuchsLang2001
### Keywords: dplot

### ** Examples

## load example data
data(ExampleData.DeValues, envir = environment())

## calculate De according to Fuchs & Lang (2001)
temp<- calc_FuchsLang2001(ExampleData.DeValues$BT998, cvThreshold = 5)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_FuchsLang2001", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_HomogeneityTest")
### * calc_HomogeneityTest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_HomogeneityTest
### Title: Apply a simple homogeneity test after Galbraith (2003)
### Aliases: calc_HomogeneityTest

### ** Examples


## load example data
data(ExampleData.DeValues, envir = environment())

## apply the homogeneity test
calc_HomogeneityTest(ExampleData.DeValues$BT998)

## using the data presented by Galbraith (2003)
df <-
 data.frame(
   x = c(30.1, 53.8, 54.3, 29.0, 47.6, 44.2, 43.1),
   y = c(4.8, 7.1, 6.8, 4.3, 5.2, 5.9, 3.0))

calc_HomogeneityTest(df)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_HomogeneityTest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_Huntley2006")
### * calc_Huntley2006

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_Huntley2006
### Title: Apply the Huntley (2006) model
### Aliases: calc_Huntley2006
### Keywords: datagen

### ** Examples


## Load example data (sample UNIL/NB123, see ?ExampleData.Fading)
data("ExampleData.Fading", envir = environment())

## (1) Set all relevant parameters
# a. fading measurement data (IR50)
fading_data <- ExampleData.Fading$fading.data$IR50

# b. Dose response curve data
data <- ExampleData.Fading$equivalentDose.data$IR50

## (2) Define required function parameters
ddot <- c(7.00, 0.004)
readerDdot <- c(0.134, 0.0067)

# Analyse fading measurement and get an estimate of rho'.
# Note that the RLum.Results object can be directly used for further processing.
# The number of MC runs is reduced for this example
rhop <- analyse_FadingMeasurement(fading_data, plot = TRUE, verbose = FALSE, n.MC = 10)

## (3) Apply the Kars et al. (2008) model to the data
kars <- calc_Huntley2006(
 data = data,
 rhop = rhop,
 ddot = ddot,
 readerDdot = readerDdot,
 n.MC = 25)


## Not run: 
##D # You can also provide LnTn values separately via the 'LnTn' argument.
##D # Note, however, that the data frame for 'data' must then NOT contain
##D # a LnTn value. See argument descriptions!
##D LnTn <- data.frame(
##D  LnTn = c(1.84833, 2.24833),
##D  nTn.error = c(0.17, 0.22))
##D 
##D LxTx <- data[2:nrow(data), ]
##D 
##D kars <- calc_Huntley2006(
##D  data = LxTx,
##D  LnTn = LnTn,
##D  rhop = rhop,
##D  ddot = ddot,
##D  readerDdot = readerDdot,
##D  n.MC = 25)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_Huntley2006", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_IEU")
### * calc_IEU

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_IEU
### Title: Apply the internal-external-uncertainty (IEU) model after
###   Thomsen et al. (2007) to a given De distribution
### Aliases: calc_IEU

### ** Examples


## load data
data(ExampleData.DeValues, envir = environment())

## apply the IEU model
ieu <- calc_IEU(ExampleData.DeValues$CA1, a = 0.2, b = 1.9, interval = 1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_IEU", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_Kars2008")
### * calc_Kars2008

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_Kars2008
### Title: Apply the Kars et al. (2008) model (deprecated)
### Aliases: calc_Kars2008
### Keywords: datagen

### ** Examples


## Load example data (sample UNIL/NB123, see ?ExampleData.Fading)
data("ExampleData.Fading", envir = environment())

## (1) Set all relevant parameters
# a. fading measurement data (IR50)
fading_data <- ExampleData.Fading$fading.data$IR50

# b. Dose response curve data
data <- ExampleData.Fading$equivalentDose.data$IR50

## (2) Define required function parameters
ddot <- c(7.00, 0.004)
readerDdot <- c(0.134, 0.0067)

# Analyse fading measurement and get an estimate of rho'.
# Note that the RLum.Results object can be directly used for further processing.
# The number of MC runs is reduced for this example
rhop <- analyse_FadingMeasurement(fading_data, plot = TRUE, verbose = FALSE, n.MC = 10)

## (3) Apply the Kars et al. (2008) model to the data
kars <- suppressWarnings(
  calc_Kars2008(data = data,
                rhop = rhop,
                ddot = ddot,
                readerDdot = readerDdot,
                n.MC = 25)
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_Kars2008", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_Lamothe2003")
### * calc_Lamothe2003

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_Lamothe2003
### Title: Apply fading correction after Lamothe et al., 2003
### Aliases: calc_Lamothe2003
### Keywords: datagen

### ** Examples


##load data
##ExampleData.BINfileData contains two BINfileData objects
##CWOSL.SAR.Data and TL.SAR.Data
data(ExampleData.BINfileData, envir = environment())

##transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##perform SAR analysis and set rejection criteria
results <- analyse_SAR.CWOSL(
object = object,
signal.integral.min = 1,
signal.integral.max = 2,
background.integral.min = 900,
background.integral.max = 1000,
verbose = FALSE,
plot = FALSE,
onlyLxTxTable = TRUE
)

##run fading correction
results_corr <- calc_Lamothe2003(
  object = results,
  dose_rate.envir =  c(1.676 , 0.180),
  dose_rate.source = c(0.184, 0.003),
  g_value =  c(2.36, 0.6),
  plot = TRUE,
  fit.method = "EXP")





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_Lamothe2003", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_MaxDose")
### * calc_MaxDose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_MaxDose
### Title: Apply the maximum age model to a given De distribution
### Aliases: calc_MaxDose

### ** Examples


## load example data
data(ExampleData.DeValues, envir = environment())

# apply the maximum dose model
calc_MaxDose(ExampleData.DeValues$CA1, sigmab = 0.2, par = 3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_MaxDose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_MinDose")
### * calc_MinDose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_MinDose
### Title: Apply the (un-)logged minimum age model (MAM) after Galbraith et
###   al. (1999) to a given De distribution
### Aliases: calc_MinDose

### ** Examples


## Load example data
data(ExampleData.DeValues, envir = environment())

# (1) Apply the minimum age model with minimum required parameters.
# By default, this will apply the un-logged 3-parameter MAM.
calc_MinDose(data = ExampleData.DeValues$CA1, sigmab = 0.1)

## Not run: 
##D # (2) Re-run the model, but save results to a variable and turn
##D # plotting of the log-likelihood profiles off.
##D mam <- calc_MinDose(
##D  data = ExampleData.DeValues$CA1,
##D  sigmab = 0.1,
##D  plot = FALSE)
##D 
##D # Show structure of the RLum.Results object
##D mam
##D 
##D # Show summary table that contains the most relevant results
##D res <- get_RLum(mam, "summary")
##D res
##D 
##D # Plot the log likelihood profiles retroactively, because before
##D # we set plot = FALSE
##D plot_RLum(mam)
##D 
##D # Plot the dose distribution in an abanico plot and draw a line
##D # at the minimum dose estimate
##D plot_AbanicoPlot(data = ExampleData.DeValues$CA1,
##D                  main = "3-parameter Minimum Age Model",
##D                  line = mam,polygon.col = "none",
##D                  hist = TRUE,
##D                  rug = TRUE,
##D                  summary = c("n", "mean", "mean.weighted", "median", "in.ci"),
##D                  centrality = res$de,
##D                  line.col = "red",
##D                  grid.col = "none",
##D                  line.label = paste0(round(res$de, 1), "\U00B1",
##D                                      round(res$de_err, 1), " Gy"),
##D                  bw = 0.1,
##D                  ylim = c(-25, 18),
##D                  summary.pos = "topleft",
##D                  mtext = bquote("Parameters: " ~
##D                                   sigma[b] == .(get_RLum(mam, "args")$sigmab) ~ ", " ~
##D                                   gamma == .(round(log(res$de), 1)) ~ ", " ~
##D                                   sigma == .(round(res$sig, 1)) ~ ", " ~
##D                                   rho == .(round(res$p0, 2))))
##D 
##D 
##D 
##D # (3) Run the minimum age model with bootstrap
##D # NOTE: Bootstrapping is computationally intensive
##D # (3.1) run the minimum age model with default values for bootstrapping
##D calc_MinDose(data = ExampleData.DeValues$CA1,
##D              sigmab = 0.15,
##D              bootstrap = TRUE)
##D 
##D # (3.2) Bootstrap control parameters
##D mam <- calc_MinDose(data = ExampleData.DeValues$CA1,
##D                     sigmab = 0.15,
##D                     bootstrap = TRUE,
##D                     bs.M = 300,
##D                     bs.N = 500,
##D                     bs.h = 4,
##D                     sigmab.sd = 0.06,
##D                     plot = FALSE)
##D 
##D # Plot the results
##D plot_RLum(mam)
##D 
##D # save bootstrap results in a separate variable
##D bs <- get_RLum(mam, "bootstrap")
##D 
##D # show structure of the bootstrap results
##D str(bs, max.level = 2, give.attr = FALSE)
##D 
##D # print summary of minimum dose and likelihood pairs
##D summary(bs$pairs$gamma)
##D 
##D # Show polynomial fits of the bootstrap pairs
##D bs$poly.fits$poly.three
##D 
##D # Plot various statistics of the fit using the generic plot() function
##D par(mfcol=c(2,2))
##D plot(bs$poly.fits$poly.three, ask = FALSE)
##D 
##D # Show the fitted values of the polynomials
##D summary(bs$poly.fits$poly.three$fitted.values)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_MinDose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_OSLLxTxRatio")
### * calc_OSLLxTxRatio

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_OSLLxTxRatio
### Title: Calculate 'Lx/Tx' ratio for CW-OSL curves
### Aliases: calc_OSLLxTxRatio
### Keywords: datagen

### ** Examples


##load data
data(ExampleData.LxTxOSLData, envir = environment())

##calculate Lx/Tx ratio
results <- calc_OSLLxTxRatio(
 Lx.data = Lx.data,
 Tx.data = Tx.data,
 signal.integral = c(1:2),
 background.integral = c(85:100))

##get results object
get_RLum(results)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_OSLLxTxRatio", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_SourceDoseRate")
### * calc_SourceDoseRate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_SourceDoseRate
### Title: Calculation of the source dose rate via the date of measurement
### Aliases: calc_SourceDoseRate
### Keywords: manip

### ** Examples



##(1) Simple function usage
##Basic calculation of the dose rate for a specific date
dose.rate <-  calc_SourceDoseRate(measurement.date = "2012-01-27",
                                  calib.date = "2014-12-19",
                                  calib.dose.rate = 0.0438,
                                  calib.error = 0.0019)

##show results
get_RLum(dose.rate)

##(2) Usage in combination with another function (e.g., Second2Gray() )
## load example data
data(ExampleData.DeValues, envir = environment())

## use the calculated variable dose.rate as input argument
## to convert De(s) to De(Gy)
Second2Gray(ExampleData.DeValues$BT998, dose.rate)

##(3) source rate prediction and plotting
dose.rate <-  calc_SourceDoseRate(measurement.date = "2012-01-27",
                                  calib.date = "2014-12-19",
                                  calib.dose.rate = 0.0438,
                                  calib.error = 0.0019,
                                  predict = 1000)
plot_RLum(dose.rate)


##(4) export output to a LaTeX table (example using the package 'xtable')
## Not run: 
##D xtable::xtable(get_RLum(dose.rate))
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_SourceDoseRate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_Statistics")
### * calc_Statistics

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_Statistics
### Title: Function to calculate statistic measures
### Aliases: calc_Statistics
### Keywords: datagen

### ** Examples


## load example data
data(ExampleData.DeValues, envir = environment())

## show a rough plot of the data to illustrate the non-normal distribution
plot_KDE(ExampleData.DeValues$BT998)

## calculate statistics and show output
str(calc_Statistics(ExampleData.DeValues$BT998))

## Not run: 
##D ## now the same for 10000 normal distributed random numbers with equal errors
##D x <- as.data.frame(cbind(rnorm(n = 10^5, mean = 0, sd = 1),
##D                          rep(0.001, 10^5)))
##D 
##D ## note the congruent results for weighted and unweighted measures
##D str(calc_Statistics(x))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_Statistics", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_TLLxTxRatio")
### * calc_TLLxTxRatio

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_TLLxTxRatio
### Title: Calculate the Lx/Tx ratio for a given set of TL curves -beta
###   version-
### Aliases: calc_TLLxTxRatio
### Keywords: datagen

### ** Examples


##load package example data
data(ExampleData.BINfileData, envir = environment())

##convert Risoe.BINfileData into a curve object
temp <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)


Lx.data.signal <- get_RLum(temp, record.id=1)
Lx.data.background <- get_RLum(temp, record.id=2)
Tx.data.signal <- get_RLum(temp, record.id=3)
Tx.data.background <- get_RLum(temp, record.id=4)
signal.integral.min <- 210
signal.integral.max <- 230

output <- calc_TLLxTxRatio(
 Lx.data.signal,
 Lx.data.background,
 Tx.data.signal,
 Tx.data.background,
 signal.integral.min,
 signal.integral.max)
get_RLum(output)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_TLLxTxRatio", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_ThermalLifetime")
### * calc_ThermalLifetime

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_ThermalLifetime
### Title: Calculates the Thermal Lifetime using the Arrhenius equation
### Aliases: calc_ThermalLifetime
### Keywords: datagen

### ** Examples


##EXAMPLE 1
##calculation for two trap-depths with similar frequency factor for different temperatures
E <- c(1.66, 1.70)
s <- 1e+13
T <- 10:20
temp <- calc_ThermalLifetime(
  E = E,
  s = s,
  T = T,
  output_unit = "Ma"
)
contour(x = E, y = T, z = temp$lifetimes[1,,],
        ylab = "Temperature [\u00B0C]",
        xlab = "Trap depth [eV]",
        main = "Thermal Lifetime Contour Plot"
)
mtext(side = 3, "(values quoted in Ma)")

##EXAMPLE 2
##profiling of thermal life time for E and s and their standard error
E <- c(1.600, 0.003)
s <- c(1e+13,1e+011)
T <- 20
calc_ThermalLifetime(
  E = E,
  s = s,
  T = T,
  profiling = TRUE,
  output_unit = "Ma"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_ThermalLifetime", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_WodaFuchs2008")
### * calc_WodaFuchs2008

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_WodaFuchs2008
### Title: Obtain the equivalent dose using the approach by Woda and Fuchs
###   2008
### Aliases: calc_WodaFuchs2008

### ** Examples


## read example data set
data(ExampleData.DeValues, envir = environment())

results <- calc_WodaFuchs2008(
  data = ExampleData.DeValues$CA1,
   xlab = expression(paste(D[e], " [Gy]"))
 )




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_WodaFuchs2008", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_gSGC")
### * calc_gSGC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_gSGC
### Title: Calculate De value based on the gSGC by Li et al., 2015
### Aliases: calc_gSGC
### Keywords: datagen

### ** Examples


results <- calc_gSGC(data = data.frame(
LnTn =  2.361, LnTn.error = 0.087,
Lr1Tr1 = 2.744, Lr1Tr1.error = 0.091,
Dr1 = 34.4))

get_RLum(results, data.object = "De")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_gSGC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_gSGC_feldspar")
### * calc_gSGC_feldspar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_gSGC_feldspar
### Title: Calculate Global Standardised Growth Curve (gSGC) for Feldspar
###   MET-pIRIR
### Aliases: calc_gSGC_feldspar
### Keywords: datagen

### ** Examples


##test on a generated random sample
n_samples <- 10
data <- data.frame(
  LnTn = rnorm(n=n_samples, mean=1.0, sd=0.02),
  LnTn.error = rnorm(n=n_samples, mean=0.05, sd=0.002),
  Lr1Tr1 = rnorm(n=n_samples, mean=1.0, sd=0.02),
  Lr1Tr1.error = rnorm(n=n_samples, mean=0.05, sd=0.002),
  Dr1 = rep(100,n_samples))

results <- calc_gSGC_feldspar(
  data = data, gSGC.type = "50LxTx",
  plot = FALSE)

plot_AbanicoPlot(results)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_gSGC_feldspar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("combine_De_Dr")
### * combine_De_Dr

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: combine_De_Dr
### Title: Combine Dose Rate and Equivalent Dose Distribution
### Aliases: combine_De_Dr
### Keywords: datagen distribution dplot

### ** Examples

## set parameters
Dr <- stats::rlnorm (1000, 0, 0.3)
De <- 50*sample(Dr, 50, replace = TRUE)
s <- stats::rnorm(50, 10, 2)

## run modelling
## note: modify parameters for more realistic results
## Not run: 
##D results <- combine_De_Dr(
##D  Dr = Dr,
##D  int_OD = 0.1,
##D  De,
##D  s,
##D  Age_range = c(0,100),
##D  method_control = list(
##D   n.iter = 100,
##D   n.chains = 1))
##D 
##D ## show models used
##D writeLines(results@info$model_IAM)
##D writeLines(results@info$model_BCAM)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("combine_De_Dr", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_Activity2Concentration")
### * convert_Activity2Concentration

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_Activity2Concentration
### Title: Convert Nuclide Activities to Abundance and Vice Versa
### Aliases: convert_Activity2Concentration
### Keywords: IO

### ** Examples


##construct data.frame
data <- data.frame(
 NUCLIDES = c("U-238", "Th-232", "K-40"),
 VALUE = c(40,80,100),
 VALUE_ERROR = c(4,8,10),
 stringsAsFactors = FALSE)

##perform analysis
convert_Activity2Concentration(data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_Activity2Concentration", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_BIN2CSV")
### * convert_BIN2CSV

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_BIN2CSV
### Title: Export Risoe BIN-file(s) to CSV-files
### Aliases: convert_BIN2CSV
### Keywords: IO

### ** Examples


##transform Risoe.BINfileData values to a list
data(ExampleData.BINfileData, envir = environment())
convert_BIN2CSV(subset(CWOSL.SAR.Data, POSITION == 1), export = FALSE)

## Not run: 
##D ##select your BIN-file
##D file <- file.choose()
##D 
##D ##convert
##D convert_BIN2CSV(file)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_BIN2CSV", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_Concentration2DoseRate")
### * convert_Concentration2DoseRate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_Concentration2DoseRate
### Title: Dose-rate conversion function
### Aliases: convert_Concentration2DoseRate
### Keywords: datagen

### ** Examples


## create input template
input <- convert_Concentration2DoseRate()

## fill input
input$Mineral <- "FS"
input$K <- 2.13
input$K_SE <- 0.07
input$Th <- 9.76
input$Th_SE <- 0.32
input$U <- 2.24
input$U_SE <- 0.12
input$GrainSize <- 200
input$WaterContent <- 30
input$WaterContent_SE <- 5

## convert
convert_Concentration2DoseRate(input)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_Concentration2DoseRate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_Daybreak2CSV")
### * convert_Daybreak2CSV

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_Daybreak2CSV
### Title: Export measurement data produced by a Daybreak luminescence
###   reader to CSV-files
### Aliases: convert_Daybreak2CSV
### Keywords: IO

### ** Examples


## Not run: 
##D ##select your BIN-file
##D file <- file.choose()
##D 
##D ##convert
##D convert_Daybreak2CSV(file)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_Daybreak2CSV", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_PSL2CSV")
### * convert_PSL2CSV

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_PSL2CSV
### Title: Export PSL-file(s) to CSV-files
### Aliases: convert_PSL2CSV
### Keywords: IO

### ** Examples


## export into single data.frame
file <- system.file("extdata/DorNie_0016.psl", package="Luminescence")
convert_PSL2CSV(file, export = FALSE, single_table = TRUE)


## Not run: 
##D ##select your BIN-file
##D file <- file.choose()
##D 
##D ##convert
##D convert_PSL2CSV(file)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_PSL2CSV", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_RLum2Risoe.BINfileData")
### * convert_RLum2Risoe.BINfileData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_RLum2Risoe.BINfileData
### Title: Converts RLum.Analysis-objects and RLum.Data.Curve-objects to
###   RLum2Risoe.BINfileData-objects
### Aliases: convert_RLum2Risoe.BINfileData
### Keywords: IO

### ** Examples


##simple conversion using the example dataset
data(ExampleData.RLum.Analysis, envir = environment())
convert_RLum2Risoe.BINfileData(IRSAR.RF.Data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_RLum2Risoe.BINfileData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_SG2MG")
### * convert_SG2MG

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_SG2MG
### Title: Converts Single-Grain Data to Multiple-Grain Data
### Aliases: convert_SG2MG
### Keywords: IO

### ** Examples

## simple run
## (please not that the example is not using SG data)
data(ExampleData.BINfileData, envir = environment())
convert_SG2MG(CWOSL.SAR.Data)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_SG2MG", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_Wavelength2Energy")
### * convert_Wavelength2Energy

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_Wavelength2Energy
### Title: Emission Spectra Conversion from Wavelength to Energy Scales
###   (Jacobian Conversion)
### Aliases: convert_Wavelength2Energy
### Keywords: IO

### ** Examples


##=====================##
##(1) Literature example after Mooney et al. (2013)
##(1.1) create matrix
m <- matrix(
  data = c(seq(400, 800, 50), rep(1, 9)), ncol = 2)

##(1.2) set plot function to reproduce the
##literature figure
p <- function(m) {
 plot(x = m[, 1], y = m[, 2])
 polygon(
 x = c(m[, 1], rev(m[, 1])),
 y = c(m[, 2], rep(0, nrow(m))))
 for (i in 1:nrow(m)) {
  lines(x = rep(m[i, 1], 2), y = c(0, m[i, 2]))
 }
}

##(1.3) plot curves
par(mfrow = c(1,2))
p(m)
p(convert_Wavelength2Energy(m))

##=====================##
##(2) Another example using density curves
##create dataset
xy <- density(
 c(rnorm(n = 100, mean = 500, sd = 20),
 rnorm(n = 100, mean = 800, sd = 20)))
xy <- data.frame(xy$x, xy$y)

##plot
par(mfrow = c(1,2))
plot(
 xy,
 type = "l",
 xlim = c(150, 1000),
 xlab = "Wavelength [nm]",
 ylab = "Luminescence [a.u.]"
)
plot(
 convert_Wavelength2Energy(xy),
 xy$y,
 type = "l",
 xlim = c(1.23, 8.3),
 xlab = "Energy [eV]",
 ylab = "Luminescence [a.u.]"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_Wavelength2Energy", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("convert_XSYG2CSV")
### * convert_XSYG2CSV

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_XSYG2CSV
### Title: Export XSYG-file(s) to CSV-files
### Aliases: convert_XSYG2CSV
### Keywords: IO

### ** Examples


##transform XSYG-file values to a list
data(ExampleData.XSYG, envir = environment())
convert_XSYG2CSV(OSL.SARMeasurement$Sequence.Object[1:10], export = FALSE)

## Not run: 
##D ##select your BIN-file
##D file <- file.choose()
##D 
##D ##convert
##D convert_XSYG2CSV(file)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_XSYG2CSV", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_IrradiationTimes")
### * extract_IrradiationTimes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_IrradiationTimes
### Title: Extract Irradiation Times from an XSYG-file
### Aliases: extract_IrradiationTimes
### Keywords: IO manip

### ** Examples

## (1) - example for your own data
##
## set files and run function
#
#   file.XSYG <- file.choose()
#   file.BINX <- file.choose()
#
#     output <- extract_IrradiationTimes(file.XSYG = file.XSYG, file.BINX = file.BINX)
#     get_RLum(output)
#
## export results additionally to a CSV.file in the same directory as the XSYG-file
#       write.table(x = get_RLum(output),
#                   file = paste0(file.BINX,"_extract_IrradiationTimes.csv"),
#                   sep = ";",
#                   row.names = FALSE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_IrradiationTimes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("extract_ROI")
### * extract_ROI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: extract_ROI
### Title: Extract Pixel Values through Circular Region-of-Interests (ROI)
###   from an Image
### Aliases: extract_ROI
### Keywords: manip

### ** Examples


m <- matrix(runif(100,0,255), ncol = 10, nrow = 10)
roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
extract_ROI(object = m, roi = roi, plot = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("extract_ROI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit_CWCurve")
### * fit_CWCurve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_CWCurve
### Title: Nonlinear Least Squares Fit for CW-OSL curves -beta version-
### Aliases: fit_CWCurve
### Keywords: dplot models

### ** Examples


##load data
data(ExampleData.CW_OSL_Curve, envir = environment())

##fit data
fit <- fit_CWCurve(values = ExampleData.CW_OSL_Curve,
                   main = "CW Curve Fit",
                   n.components.max = 4,
                   log = "x")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_CWCurve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit_EmissionSpectra")
### * fit_EmissionSpectra

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_EmissionSpectra
### Title: Luminescence Emission Spectra Deconvolution
### Aliases: fit_EmissionSpectra
### Keywords: datagen

### ** Examples


##load example data
data(ExampleData.XSYG, envir = environment())

##subtract background
TL.Spectrum@data <- TL.Spectrum@data[] - TL.Spectrum@data[,15]

results <- fit_EmissionSpectra(
 object = TL.Spectrum,
 frame = 5,
 method_control = list(max.runs = 10)
)

##deconvolution of a TL spectrum
## Not run: 
##D 
##D ##load example data
##D 
##D ##replace 0 values
##D results <- fit_EmissionSpectra(
##D  object = TL.Spectrum,
##D  frame = 5, main = "TL spectrum"
##D )
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_EmissionSpectra", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit_LMCurve")
### * fit_LMCurve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_LMCurve
### Title: Nonlinear Least Squares Fit for LM-OSL curves
### Aliases: fit_LMCurve
### Keywords: dplot models

### ** Examples


##(1) fit LM data without background subtraction
data(ExampleData.FittingLM, envir = environment())
fit_LMCurve(values = values.curve, n.components = 3, log = "x")

##(2) fit LM data with background subtraction and export as JPEG
## -alter file path for your preferred system
##jpeg(file = "~/Desktop/Fit_Output%03d.jpg", quality = 100,
## height = 3000, width = 3000, res = 300)
data(ExampleData.FittingLM, envir = environment())
fit_LMCurve(values = values.curve, values.bg = values.curveBG,
            n.components = 2, log = "x", plot.BG = TRUE)
##dev.off()

##(3) fit LM data with manual start parameters
data(ExampleData.FittingLM, envir = environment())
fit_LMCurve(values = values.curve,
            values.bg = values.curveBG,
            n.components = 3,
            log = "x",
            start_values = data.frame(Im = c(170,25,400), xm = c(56,200,1500)))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_LMCurve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit_OSLLifeTimes")
### * fit_OSLLifeTimes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_OSLLifeTimes
### Title: Fitting and Deconvolution of OSL Lifetime Components
### Aliases: fit_OSLLifeTimes

### ** Examples


##load example data
data(ExampleData.TR_OSL, envir = environment())

##fit lifetimes (short run)
fit_OSLLifeTimes(
 object = ExampleData.TR_OSL,
 n.components = 1)

##long example
## Not run: 
##D fit_OSLLifeTimes(
##D object = ExampleData.TR_OSL)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_OSLLifeTimes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit_SurfaceExposure")
### * fit_SurfaceExposure

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_SurfaceExposure
### Title: Nonlinear Least Squares Fit for OSL surface exposure data
### Aliases: fit_SurfaceExposure
### Keywords: datagen

### ** Examples


## Load example data
data("ExampleData.SurfaceExposure")

## Example 1 - Single sample
# Known parameters: 10000 a, mu = 0.9, sigmaphi = 5e-10
sample_1 <- ExampleData.SurfaceExposure$sample_1
head(sample_1)
results <- fit_SurfaceExposure(
 data = sample_1,
 mu = 0.9,
 sigmaphi = 5e-10)
get_RLum(results)


## Example 2 - Single sample and considering dose rate
# Known parameters: 10000 a, mu = 0.9, sigmaphi = 5e-10,
# dose rate = 2.5 Gy/ka, D0 = 40 Gy
sample_2 <- ExampleData.SurfaceExposure$sample_2
head(sample_2)
results <- fit_SurfaceExposure(
 data = sample_2,
 mu = 0.9,
 sigmaphi = 5e-10,
 Ddot = 2.5,
 D0 = 40)
get_RLum(results)

## Example 3 - Multiple samples (global fit) to better constrain 'mu'
# Known parameters: ages = 1e3, 1e4, 1e5, 1e6 a, mu = 0.9, sigmaphi = 5e-10
set_1 <- ExampleData.SurfaceExposure$set_1
str(set_1, max.level = 2)
results <- fit_SurfaceExposure(
  data = set_1,
  age = c(1e3, 1e4, 1e5, 1e6),
  sigmaphi = 5e-10)
get_RLum(results)


## Example 4 - Multiple samples (global fit) and considering dose rate
# Known parameters: ages = 1e2, 1e3, 1e4, 1e5, 1e6 a, mu = 0.9, sigmaphi = 5e-10,
# dose rate = 1.0 Ga/ka, D0 = 40 Gy
set_2 <- ExampleData.SurfaceExposure$set_2
str(set_2, max.level = 2)
results <- fit_SurfaceExposure(
 data = set_2,
 age = c(1e2, 1e3, 1e4, 1e5, 1e6),
 sigmaphi = 5e-10,
 Ddot = 1,
 D0 = 40)
get_RLum(results)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_SurfaceExposure", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit_ThermalQuenching")
### * fit_ThermalQuenching

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_ThermalQuenching
### Title: Fitting Thermal Quenching Data
### Aliases: fit_ThermalQuenching

### ** Examples


##create short example dataset
data <- data.frame(
  T = c(25, 40, 50, 60, 70, 80, 90, 100, 110),
  V = c(0.06, 0.058, 0.052, 0.051, 0.041, 0.034, 0.035, 0.033, 0.032),
  V_X = c(0.012, 0.009, 0.008, 0.008, 0.007, 0.006, 0.005, 0.005, 0.004))

##fit
fit_ThermalQuenching(
 data = data,
 n.MC = NULL)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_ThermalQuenching", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_Layout")
### * get_Layout

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_Layout
### Title: Collection of layout definitions
### Aliases: get_Layout

### ** Examples


## read example data set
data(ExampleData.DeValues, envir = environment())

## show structure of the default layout definition
layout.default <- get_Layout(layout = "default")
str(layout.default)

## show colour definitions for Abanico plot, only
layout.default$abanico$colour

## set Abanico plot title colour to orange
layout.default$abanico$colour$main <- "orange"

## create Abanico plot with modofied layout definition
plot_AbanicoPlot(data = ExampleData.DeValues,
                 layout = layout.default)

## create Abanico plot with predefined layout "journal"
plot_AbanicoPlot(data = ExampleData.DeValues,
                 layout = "journal")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_Layout", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_Quote")
### * get_Quote

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_Quote
### Title: Function to return essential quotes
### Aliases: get_Quote

### ** Examples


## ask for an arbitrary quote
get_Quote()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_Quote", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_RLum")
### * get_RLum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_RLum
### Title: General accessors function for RLum S4 class objects
### Aliases: get_RLum get_RLum,list-method get_RLum,NULL-method
### Keywords: utilities

### ** Examples


##Example based using data and from the calc_CentralDose() function

##load example data
data(ExampleData.DeValues, envir = environment())

##apply the central dose model 1st time
temp1 <- calc_CentralDose(ExampleData.DeValues$CA1)

##get results and store them in a new object
temp.get <- get_RLum(object = temp1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_RLum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_rightAnswer")
### * get_rightAnswer

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_rightAnswer
### Title: Function to get the right answer
### Aliases: get_rightAnswer

### ** Examples


## you really want to know?
get_rightAnswer()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_rightAnswer", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("import_Data")
### * import_Data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: import_Data
### Title: Import Luminescence Data into R
### Aliases: import_Data
### Keywords: datagen

### ** Examples


## import BINX/BIN
file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
temp <- import_Data(file)

## RF data
file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
temp <- import_Data(file)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("import_Data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("install_DevelopmentVersion")
### * install_DevelopmentVersion

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: install_DevelopmentVersion
### Title: Attempts to install the development version of the
###   'Luminescence' package
### Aliases: install_DevelopmentVersion

### ** Examples


## Not run: 
##D install_DevelopmentVersion()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("install_DevelopmentVersion", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("merge_RLum.Analysis")
### * merge_RLum.Analysis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: merge_RLum.Analysis
### Title: Merge function for RLum.Analysis S4 class objects
### Aliases: merge_RLum.Analysis
### Keywords: internal utilities

### ** Examples



##merge different RLum objects from the example data
data(ExampleData.RLum.Analysis, envir = environment())
data(ExampleData.BINfileData, envir = environment())

object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
curve <- get_RLum(object)[[2]]

temp.merged <- merge_RLum.Analysis(list(curve, IRSAR.RF.Data, IRSAR.RF.Data))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("merge_RLum.Analysis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("merge_RLum.Data.Curve")
### * merge_RLum.Data.Curve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: merge_RLum.Data.Curve
### Title: Merge function for RLum.Data.Curve S4 class objects
### Aliases: merge_RLum.Data.Curve
### Keywords: internal utilities

### ** Examples



##load example data
data(ExampleData.XSYG, envir = environment())

##grep first and 3d TL curves
TL.curves  <- get_RLum(OSL.SARMeasurement$Sequence.Object, recordType = "TL (UVVIS)")
TL.curve.1 <- TL.curves[[1]]
TL.curve.3 <- TL.curves[[3]]

##plot single curves
plot_RLum(TL.curve.1)
plot_RLum(TL.curve.3)

##subtract the 1st curve from the 2nd and plot
TL.curve.merged <- merge_RLum.Data.Curve(list(TL.curve.3, TL.curve.1), merge.method = "/")
plot_RLum(TL.curve.merged)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("merge_RLum.Data.Curve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("merge_RLum")
### * merge_RLum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: merge_RLum
### Title: General merge function for RLum S4 class objects
### Aliases: merge_RLum
### Keywords: utilities

### ** Examples



##Example based using data and from the calc_CentralDose() function

##load example data
data(ExampleData.DeValues, envir = environment())

##apply the central dose model 1st time
temp1 <- calc_CentralDose(ExampleData.DeValues$CA1)

##apply the central dose model 2nd time
temp2 <- calc_CentralDose(ExampleData.DeValues$CA1)

##merge the results and store them in a new object
temp.merged <- get_RLum(merge_RLum(objects = list(temp1, temp2)))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("merge_RLum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("merge_Risoe.BINfileData")
### * merge_Risoe.BINfileData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: merge_Risoe.BINfileData
### Title: Merge Risoe.BINfileData objects or Risoe BIN-files
### Aliases: merge_Risoe.BINfileData
### Keywords: IO manip

### ** Examples


##merge two objects
data(ExampleData.BINfileData, envir = environment())

object1 <- CWOSL.SAR.Data
object2 <- CWOSL.SAR.Data

object.new <- merge_Risoe.BINfileData(c(object1, object2))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("merge_Risoe.BINfileData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("methods_RLum")
### * methods_RLum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: methods_RLum
### Title: methods_RLum
### Aliases: methods_RLum plot.list plot.RLum.Results plot.RLum.Analysis
###   plot.RLum.Data.Curve plot.RLum.Data.Spectrum plot.RLum.Data.Image
###   plot.Risoe.BINfileData hist.RLum.Results hist.RLum.Data.Image
###   hist.RLum.Data.Curve hist.RLum.Analysis summary.RLum.Results
###   summary.RLum.Analysis summary.RLum.Data.Image summary.RLum.Data.Curve
###   subset.Risoe.BINfileData subset.RLum.Analysis bin bin.RLum.Data.Curve
###   bin.RLum.Data.Spectrum length.RLum.Results length.RLum.Analysis
###   length.RLum.Data.Curve length.Risoe.BINfileData dim.RLum.Data.Curve
###   dim.RLum.Data.Spectrum rep.RLum names.RLum.Data.Curve
###   names.RLum.Data.Spectrum names.RLum.Data.Image names.RLum.Analysis
###   names.RLum.Results names.Risoe.BINfileData
###   row.names.RLum.Data.Spectrum as.data.frame.RLum.Data.Curve
###   as.data.frame.RLum.Data.Spectrum as.data.frame.Risoe.BINfileData
###   as.list.RLum.Results as.list.RLum.Data.Curve as.list.RLum.Data.Image
###   as.list.RLum.Analysis as.matrix.RLum.Data.Curve
###   as.matrix.RLum.Data.Spectrum as.matrix.RLum.Data.Image is.RLum
###   is.RLum.Data is.RLum.Data.Curve is.RLum.Data.Spectrum
###   is.RLum.Data.Image is.RLum.Analysis is.RLum.Results merge.RLum
###   unlist.RLum.Analysis +.RLum.Data.Curve -.RLum.Data.Curve
###   *.RLum.Data.Curve /.RLum.Data.Curve [.RLum.Data.Curve
###   [.RLum.Data.Spectrum [.RLum.Data.Image [.RLum.Analysis [.RLum.Results
###   [<-.RLum.Data.Curve [[.RLum.Analysis [[.RLum.Results
###   $.RLum.Data.Curve $.RLum.Analysis $.RLum.Results
### Keywords: internal

### ** Examples


##load example data
data(ExampleData.RLum.Analysis, envir = environment())


##combine curve is various ways
curve1 <- IRSAR.RF.Data[[1]]
curve2 <-  IRSAR.RF.Data[[1]]
curve1 + curve2
curve1 - curve2
curve1 / curve2
curve1 * curve2


##`$` access curves
IRSAR.RF.Data$RF




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("methods_RLum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_AbanicoPlot")
### * plot_AbanicoPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_AbanicoPlot
### Title: Function to create an Abanico Plot.
### Aliases: plot_AbanicoPlot

### ** Examples


## load example data and recalculate to Gray
data(ExampleData.DeValues, envir = environment())
ExampleData.DeValues <- ExampleData.DeValues$CA1

## plot the example data straightforward
plot_AbanicoPlot(data = ExampleData.DeValues)

## now with linear z-scale
plot_AbanicoPlot(data = ExampleData.DeValues,
                 log.z = FALSE)

## now with output of the plot parameters
plot1 <- plot_AbanicoPlot(data = ExampleData.DeValues,
                          output = TRUE)
str(plot1)
plot1$zlim

## now with adjusted z-scale limits
plot_AbanicoPlot(data = ExampleData.DeValues,
                 zlim = c(10, 200))

## now with adjusted x-scale limits
plot_AbanicoPlot(data = ExampleData.DeValues,
                 xlim = c(0, 20))

## now with rug to indicate individual values in KDE part
plot_AbanicoPlot(data = ExampleData.DeValues,
                 rug = TRUE)

## now with a smaller bandwidth for the KDE plot
plot_AbanicoPlot(data = ExampleData.DeValues,
                 bw = 0.04)

## now with a histogram instead of the KDE plot
plot_AbanicoPlot(data = ExampleData.DeValues,
                 hist = TRUE,
                 kde = FALSE)

## now with a KDE plot and histogram with manual number of bins
plot_AbanicoPlot(data = ExampleData.DeValues,
                 hist = TRUE,
                 breaks = 20)

## now with a KDE plot and a dot plot
plot_AbanicoPlot(data = ExampleData.DeValues,
                 dots = TRUE)

## now with user-defined plot ratio
plot_AbanicoPlot(data = ExampleData.DeValues,
                 plot.ratio = 0.5)
## now with user-defined central value
plot_AbanicoPlot(data = ExampleData.DeValues,
                 z.0 = 70)

## now with median as central value
plot_AbanicoPlot(data = ExampleData.DeValues,
                 z.0 = "median")

## now with the 17-83 percentile range as definition of scatter
plot_AbanicoPlot(data = ExampleData.DeValues,
                 z.0 = "median",
                 dispersion = "p17")

## now with user-defined green line for minimum age model
CAM <- calc_CentralDose(ExampleData.DeValues,
                        plot = FALSE)

plot_AbanicoPlot(data = ExampleData.DeValues,
                 line = CAM,
                 line.col = "darkgreen",
                 line.label = "CAM")

## now create plot with legend, colour, different points and smaller scale
plot_AbanicoPlot(data = ExampleData.DeValues,
                 legend = "Sample 1",
                 col = "tomato4",
                 bar.col = "peachpuff",
                 pch = "R",
                 cex = 0.8)

## now without 2-sigma bar, polygon, grid lines and central value line
plot_AbanicoPlot(data = ExampleData.DeValues,
                 bar.col = FALSE,
                 polygon.col = FALSE,
                 grid.col = FALSE,
                 y.axis = FALSE,
                 lwd = 0)

## now with direct display of De errors, without 2-sigma bar
plot_AbanicoPlot(data = ExampleData.DeValues,
                 bar.col = FALSE,
                 ylab = "",
                 y.axis = FALSE,
                 error.bars = TRUE)

## now with user-defined axes labels
plot_AbanicoPlot(data = ExampleData.DeValues,
                 xlab = c("Data error (%)",
                          "Data precision"),
                 ylab = "Scatter",
                 zlab = "Equivalent dose [Gy]")

## now with minimum, maximum and median value indicated
plot_AbanicoPlot(data = ExampleData.DeValues,
                 stats = c("min", "max", "median"))

## now with a brief statistical summary as subheader
plot_AbanicoPlot(data = ExampleData.DeValues,
                 summary = c("n", "in.2s"))

## now with another statistical summary
plot_AbanicoPlot(data = ExampleData.DeValues,
                 summary = c("mean.weighted", "median"),
                 summary.pos = "topleft")

## now a plot with two 2-sigma bars for one data set
plot_AbanicoPlot(data = ExampleData.DeValues,
                 bar = c(30, 100))

## now the data set is split into sub-groups, one is manipulated
data.1 <- ExampleData.DeValues[1:30,]
data.2 <- ExampleData.DeValues[31:62,] * 1.3

## now a common dataset is created from the two subgroups
data.3 <- list(data.1, data.2)

## now the two data sets are plotted in one plot
plot_AbanicoPlot(data = data.3)

## now with some graphical modification
plot_AbanicoPlot(data = data.3,
                 z.0 = "median",
                 col = c("steelblue4", "orange4"),
                 bar.col = c("steelblue3", "orange3"),
                 polygon.col = c("steelblue1", "orange1"),
                 pch = c(2, 6),
                 angle = c(30, 50),
                 summary = c("n", "in.2s", "median"))

## create Abanico plot with predefined layout definition
plot_AbanicoPlot(data = ExampleData.DeValues,
                 layout = "journal")

## now with predefined layout definition and further modifications
plot_AbanicoPlot(
 data = data.3,
 z.0 = "median",
 layout = "journal",
 col = c("steelblue4", "orange4"),
 bar.col = adjustcolor(c("steelblue3", "orange3"),
                         alpha.f = 0.5),
 polygon.col = c("steelblue3", "orange3"))

## for further information on layout definitions see documentation
## of function get_Layout()

## now with manually added plot content
## create empty plot with numeric output
AP <- plot_AbanicoPlot(data = ExampleData.DeValues,
                       pch = NA,
                       output = TRUE)

## identify data in 2 sigma range
in_2sigma <- AP$data[[1]]$data.in.2s

## restore function-internal plot parameters
par(AP$par)

## add points inside 2-sigma range
points(x = AP$data[[1]]$precision[in_2sigma],
       y = AP$data[[1]]$std.estimate.plot[in_2sigma],
       pch = 16)

## add points outside 2-sigma range
points(x = AP$data[[1]]$precision[!in_2sigma],
       y = AP$data[[1]]$std.estimate.plot[!in_2sigma],
       pch = 1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_AbanicoPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("plot_DRCSummary")
### * plot_DRCSummary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_DRCSummary
### Title: Create a Dose-Response Curve Summary Plot
### Aliases: plot_DRCSummary

### ** Examples


#load data example data
data(ExampleData.BINfileData, envir = environment())

#transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

results <- analyse_SAR.CWOSL(
  object = object,
  signal.integral.min = 1,
  signal.integral.max = 2,
   background.integral.min = 900,
   background.integral.max = 1000,
   plot = FALSE
 )

##plot only DRC
plot_DRCSummary(results)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_DRCSummary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_DRTResults")
### * plot_DRTResults

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_DRTResults
### Title: Visualise dose recovery test results
### Aliases: plot_DRTResults
### Keywords: dplot

### ** Examples


## read example data set and misapply them for this plot type
data(ExampleData.DeValues, envir = environment())

## plot values
plot_DRTResults(
  values = ExampleData.DeValues$BT998[7:11,],
  given.dose = 2800,
  mtext = "Example data")

## plot values with legend
plot_DRTResults(
  values = ExampleData.DeValues$BT998[7:11,],
  given.dose = 2800,
  legend = "Test data set")

## create and plot two subsets with randomised values
x.1 <- ExampleData.DeValues$BT998[7:11,]
x.2 <- ExampleData.DeValues$BT998[7:11,] * c(runif(5, 0.9, 1.1), 1)

plot_DRTResults(
  values = list(x.1, x.2),
  given.dose = 2800)

## some more user-defined plot parameters
plot_DRTResults(
  values = list(x.1, x.2),
  given.dose = 2800,
  pch = c(2, 5),
  col = c("orange", "blue"),
  xlim = c(0, 8),
  ylim = c(0.85, 1.15),
  xlab = "Sample aliquot")

## plot the data with user-defined statistical measures as legend
plot_DRTResults(
  values = list(x.1, x.2),
  given.dose = 2800,
  summary = c("n", "weighted$mean", "sd.abs"))

## plot the data with user-defined statistical measures as sub-header
plot_DRTResults(
  values = list(x.1, x.2),
  given.dose = 2800,
  summary = c("n", "weighted$mean", "sd.abs"),
  summary.pos = "sub")

## plot the data grouped by preheat temperatures
plot_DRTResults(
  values = ExampleData.DeValues$BT998[7:11,],
  given.dose = 2800,
  preheat = c(200, 200, 200, 240, 240))

## read example data set and misapply them for this plot type
data(ExampleData.DeValues, envir = environment())

## plot values
plot_DRTResults(
  values = ExampleData.DeValues$BT998[7:11,],
  given.dose = 2800,
  mtext = "Example data")

## plot two data sets grouped by preheat temperatures
plot_DRTResults(
  values = list(x.1, x.2),
  given.dose = 2800,
  preheat = c(200, 200, 200, 240, 240))

## plot the data grouped by preheat temperatures as boxplots
plot_DRTResults(
  values = ExampleData.DeValues$BT998[7:11,],
  given.dose = 2800,
  preheat = c(200, 200, 200, 240, 240),
  boxplot = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_DRTResults", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_DetPlot")
### * plot_DetPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_DetPlot
### Title: Create De(t) plot
### Aliases: plot_DetPlot

### ** Examples


## Not run: 
##D ##load data
##D ##ExampleData.BINfileData contains two BINfileData objects
##D ##CWOSL.SAR.Data and TL.SAR.Data
##D data(ExampleData.BINfileData, envir = environment())
##D 
##D ##transform the values from the first position in a RLum.Analysis object
##D object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)
##D 
##D plot_DetPlot(
##D   object,
##D   signal.integral.min = 1,
##D   signal.integral.max = 3,
##D   background.integral.min = 900,
##D   background.integral.max = 1000,
##D   n.channels = 5)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_DetPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_FilterCombinations")
### * plot_FilterCombinations

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_FilterCombinations
### Title: Plot filter combinations along with the (optional) net
###   transmission window
### Aliases: plot_FilterCombinations
### Keywords: aplot datagen

### ** Examples


## (For legal reasons no real filter data are provided)

## Create filter sets
filter1 <- density(rnorm(100, mean = 450, sd = 20))
filter1 <- matrix(c(filter1$x, filter1$y/max(filter1$y)), ncol = 2)
filter2 <- matrix(c(200:799,rep(c(0,0.8,0),each = 200)), ncol = 2)

## Example 1 (standard)
plot_FilterCombinations(filters = list(filter1, filter2))

## Example 2 (with d and P value and name for filter 2)
results <- plot_FilterCombinations(
filters = list(filter_1 = filter1, Rectangle = list(filter2, d = 2, P = 0.6)))
results

## Example 3 show optical density
plot(results$OD_total)

## Not run: 
##D ##Example 4
##D ##show the filters using the interactive mode
##D plot_FilterCombinations(filters = list(filter1, filter2), interactive = TRUE)
##D 
## End(Not run)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_FilterCombinations", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_GrowthCurve")
### * plot_GrowthCurve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_GrowthCurve
### Title: Fit and plot a dose-response curve for luminescence data (Lx/Tx
###   against dose)
### Aliases: plot_GrowthCurve

### ** Examples


##(1) plot growth curve for a dummy data.set and show De value
data(ExampleData.LxTxData, envir = environment())
temp <- plot_GrowthCurve(LxTxData)
get_RLum(temp)

##(1b) horizontal plot arrangement
layout(mat = matrix(c(1,1,2,3), ncol = 2))
plot_GrowthCurve(LxTxData, output.plotExtended.single = TRUE)

##(1c) to access the fitting value try
get_RLum(temp, data.object = "Fit")

##(2) plot the growth curve only - uncomment to use
##pdf(file = "~/Desktop/Growth_Curve_Dummy.pdf", paper = "special")
plot_GrowthCurve(LxTxData)
##dev.off()

##(3) plot growth curve with pdf output - uncomment to use, single output
##pdf(file = "~/Desktop/Growth_Curve_Dummy.pdf", paper = "special")
plot_GrowthCurve(LxTxData, output.plotExtended.single = TRUE)
##dev.off()

##(4) plot resulting function for given interval x
x <- seq(1,10000, by = 100)
plot(
 x = x,
 y = eval(temp$Formula),
 type = "l"
)

##(5) plot using the 'extrapolation' mode
LxTxData[1,2:3] <- c(0.5, 0.001)
print(plot_GrowthCurve(LxTxData,mode = "extrapolation"))

##(6) plot using the 'alternate' mode
LxTxData[1,2:3] <- c(0.5, 0.001)
print(plot_GrowthCurve(LxTxData,mode = "alternate"))

##(7) import and fit test data set by Berger & Huntley 1989
QNL84_2_unbleached <-
read.table(system.file("extdata/QNL84_2_unbleached.txt", package = "Luminescence"))

results <- plot_GrowthCurve(
 QNL84_2_unbleached,
 mode = "extrapolation",
 plot = FALSE,
 verbose = FALSE)

#calculate confidence interval for the parameters
#as alternative error estimation
confint(results$Fit, level = 0.68)


## Not run: 
##D QNL84_2_bleached <-
##D read.table(system.file("extdata/QNL84_2_bleached.txt", package = "Luminescence"))
##D STRB87_1_unbleached <-
##D read.table(system.file("extdata/STRB87_1_unbleached.txt", package = "Luminescence"))
##D STRB87_1_bleached <-
##D read.table(system.file("extdata/STRB87_1_bleached.txt", package = "Luminescence"))
##D 
##D print(
##D  plot_GrowthCurve(
##D  QNL84_2_bleached,
##D  mode = "alternate",
##D  plot = FALSE,
##D  verbose = FALSE)$Fit)
##D 
##D print(
##D  plot_GrowthCurve(
##D  STRB87_1_unbleached,
##D  mode = "alternate",
##D  plot = FALSE,
##D  verbose = FALSE)$Fit)
##D 
##D print(
##D  plot_GrowthCurve(
##D  STRB87_1_bleached,
##D  mode = "alternate",
##D  plot = FALSE,
##D  verbose = FALSE)$Fit)
##D  
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_GrowthCurve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_Histogram")
### * plot_Histogram

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_Histogram
### Title: Plot a histogram with separate error plot
### Aliases: plot_Histogram

### ** Examples


## load data
data(ExampleData.DeValues, envir = environment())
ExampleData.DeValues <-
  Second2Gray(ExampleData.DeValues$BT998, dose.rate = c(0.0438,0.0019))

## plot histogram the easiest way
plot_Histogram(ExampleData.DeValues)

## plot histogram with some more modifications
plot_Histogram(ExampleData.DeValues,
               rug = TRUE,
               normal_curve = TRUE,
               cex.global = 0.9,
               pch = 2,
               colour = c("grey", "black", "blue", "green"),
               summary = c("n", "mean", "sdrel"),
               summary.pos = "topleft",
               main = "Histogram of De-values",
               mtext = "Example data set",
               ylab = c(expression(paste(D[e], " distribution")),
                        "Standard error"),
               xlim = c(100, 250),
               ylim = c(0, 0.1, 5, 20))





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_Histogram", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_KDE")
### * plot_KDE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_KDE
### Title: Plot kernel density estimate with statistics
### Aliases: plot_KDE

### ** Examples


## read example data set
data(ExampleData.DeValues, envir = environment())
ExampleData.DeValues <-
  Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))

## create plot straightforward
plot_KDE(data = ExampleData.DeValues)

## create plot with logarithmic x-axis
plot_KDE(data = ExampleData.DeValues,
         log = "x")

## create plot with user-defined labels and axes limits
plot_KDE(data = ExampleData.DeValues,
         main = "Dose distribution",
         xlab = "Dose (s)",
         ylab = c("KDE estimate", "Cumulative dose value"),
         xlim = c(100, 250),
         ylim = c(0, 0.08, 0, 30))

## create plot with boxplot option
plot_KDE(data = ExampleData.DeValues,
         boxplot = TRUE)

## create plot with statistical summary below header
plot_KDE(data = ExampleData.DeValues,
         summary = c("n", "median", "skewness", "in.2s"))

## create plot with statistical summary as legend
plot_KDE(data = ExampleData.DeValues,
         summary = c("n", "mean", "sd.rel", "se.abs"),
         summary.pos = "topleft")

## split data set into sub-groups, one is manipulated, and merge again
data.1 <- ExampleData.DeValues[1:15,]
data.2 <- ExampleData.DeValues[16:25,] * 1.3
data.3 <- list(data.1, data.2)

## create plot with two subsets straightforward
plot_KDE(data = data.3)

## create plot with two subsets and summary legend at user coordinates
plot_KDE(data = data.3,
         summary = c("n", "median", "skewness"),
         summary.pos = c(110, 0.07),
         col = c("blue", "orange"))

## example of how to use the numerical output of the function
## return plot output to draw a thicker KDE line
KDE_out <- plot_KDE(data = ExampleData.DeValues,
output = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_KDE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_NRt")
### * plot_NRt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_NRt
### Title: Visualise natural/regenerated signal ratios
### Aliases: plot_NRt

### ** Examples


## load example data
data("ExampleData.BINfileData", envir = environment())

## EXAMPLE 1

## convert Risoe.BINfileData object to RLum.Analysis object
data <- Risoe.BINfileData2RLum.Analysis(object = CWOSL.SAR.Data, pos = 8, ltype = "OSL")

## extract all OSL curves
allCurves <- get_RLum(data)

## keep only the natural and regenerated signal curves
pos <- seq(1, 9, 2)
curves <- allCurves[pos]

## plot a standard NR(t) plot
plot_NRt(curves)

## re-plot with rolling mean data smoothing
plot_NRt(curves, smooth = "rmean", k = 10)

## re-plot with a logarithmic x-axis
plot_NRt(curves, log = "x", smooth = "rmean", k = 5)

## re-plot with custom axes ranges
plot_NRt(curves, smooth = "rmean", k = 5,
         xlim = c(0.1, 5), ylim = c(0.4, 1.6),
         legend.pos = "bottomleft")

## re-plot with smoothing spline on log scale
plot_NRt(curves, smooth = "spline", log = "x",
         legend.pos = "top")

## EXAMPLE 2

# you may also use this function to check whether all
# TD curves follow the same shape (making it a TnTx(t) plot).
posTD <- seq(2, 14, 2)
curves <- allCurves[posTD]

plot_NRt(curves, main = "TnTx(t) Plot",
         smooth = "rmean", k = 20,
         ylab = "TD natural / TD regenerated",
         xlim = c(0, 20), legend = FALSE)

## EXAMPLE 3

# extract data from all positions
data <- lapply(1:24, FUN = function(pos) {
   Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = pos, ltype = "OSL")
})

# get individual curve data from each aliquot
aliquot <- lapply(data, get_RLum)

# set graphical parameters
par(mfrow = c(2, 2))

# create NR(t) plots for all aliquots
for (i in 1:length(aliquot)) {
   plot_NRt(aliquot[[i]][pos],
            main = paste0("Aliquot #", i),
            smooth = "rmean", k = 20,
            xlim = c(0, 10),
            cex = 0.6, legend.pos = "bottomleft")
}

# reset graphical parameters
par(mfrow = c(1, 1))





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_NRt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("plot_OSLAgeSummary")
### * plot_OSLAgeSummary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_OSLAgeSummary
### Title: Plot Posterior OSL-Age Summary
### Aliases: plot_OSLAgeSummary
### Keywords: dplot hplot

### ** Examples

##generate random data
set.seed(1234)
object <- rnorm(1000, 100, 10)
plot_OSLAgeSummary(object)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_OSLAgeSummary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_RLum.Analysis")
### * plot_RLum.Analysis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_RLum.Analysis
### Title: Plot function for an RLum.Analysis S4 class object
### Aliases: plot_RLum.Analysis
### Keywords: aplot

### ** Examples


##load data
data(ExampleData.BINfileData, envir = environment())

##convert values for position 1
temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##(1) plot (combine) TL curves in one plot
plot_RLum.Analysis(
temp,
subset = list(recordType = "TL"),
combine = TRUE,
norm = TRUE,
abline = list(v = c(110))
)

##(2) same as example (1) but using
## the argument smooth = TRUE
plot_RLum.Analysis(
temp,
subset = list(recordType = "TL"),
combine = TRUE,
norm = TRUE,
smooth = TRUE,
abline = list(v = c(110))
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_RLum.Analysis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_RLum.Data.Curve")
### * plot_RLum.Data.Curve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_RLum.Data.Curve
### Title: Plot function for an RLum.Data.Curve S4 class object
### Aliases: plot_RLum.Data.Curve
### Keywords: aplot

### ** Examples


##plot curve data

#load Example data
data(ExampleData.CW_OSL_Curve, envir = environment())

#transform data.frame to RLum.Data.Curve object
temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

#plot RLum.Data.Curve object
plot_RLum.Data.Curve(temp)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_RLum.Data.Curve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_RLum.Data.Image")
### * plot_RLum.Data.Image

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_RLum.Data.Image
### Title: Plot function for an 'RLum.Data.Image' S4 class object
### Aliases: plot_RLum.Data.Image
### Keywords: aplot

### ** Examples


##load data
data(ExampleData.RLum.Data.Image, envir = environment())

##plot data
plot_RLum.Data.Image(ExampleData.RLum.Data.Image)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_RLum.Data.Image", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_RLum.Data.Spectrum")
### * plot_RLum.Data.Spectrum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_RLum.Data.Spectrum
### Title: Plot function for an RLum.Data.Spectrum S4 class object
### Aliases: plot_RLum.Data.Spectrum
### Keywords: aplot

### ** Examples


##load example data
data(ExampleData.XSYG, envir = environment())

##(1)plot simple spectrum (2D) - image
plot_RLum.Data.Spectrum(
 TL.Spectrum,
 plot.type="image",
 xlim = c(310,750),
 ylim = c(0,300),
 bin.rows=10,
 bin.cols = 1)

##(2) plot spectrum (3D)
plot_RLum.Data.Spectrum(
  TL.Spectrum,
  plot.type="persp",
  xlim = c(310,750),
  ylim = c(0,100),
  bin.rows=10,
  bin.cols = 1)

##(3) plot spectrum on energy axis
##please note the background subtraction
plot_RLum.Data.Spectrum(TL.Spectrum,
plot.type="persp",
ylim = c(0,200),
bin.rows=10,
bg.channels = 10,
bin.cols = 1,
xaxis.energy = TRUE)

##(4) plot multiple lines (2D) - multiple.lines (with ylim)
plot_RLum.Data.Spectrum(
 TL.Spectrum,
 plot.type="multiple.lines",
 xlim = c(310,750),
 ylim = c(0,100),
 bin.rows=10,
 bin.cols = 1)

## Not run: 
##D  ##(4) interactive plot using the package plotly ("surface")
##D  plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="interactive",
##D  xlim = c(310,750), ylim = c(0,300), bin.rows=10,
##D  bin.cols = 1)
##D 
##D  ##(5) interactive plot using the package plotly ("contour")
##D  plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="interactive",
##D  xlim = c(310,750), ylim = c(0,300), bin.rows=10,
##D  bin.cols = 1,
##D  type = "contour",
##D  showscale = TRUE)
##D 
##D  ##(6) interactive plot using the package plotly ("heatmap")
##D  plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="interactive",
##D  xlim = c(310,750), ylim = c(0,300), bin.rows=10,
##D  bin.cols = 1,
##D  type = "heatmap",
##D  showscale = TRUE)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_RLum.Data.Spectrum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_RLum")
### * plot_RLum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_RLum
### Title: General plot function for RLum S4 class objects
### Aliases: plot_RLum
### Keywords: dplot

### ** Examples

#load Example data
data(ExampleData.CW_OSL_Curve, envir = environment())

#transform data.frame to RLum.Data.Curve object
temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

#plot RLum object
plot_RLum(temp)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_RLum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_RLum.Results")
### * plot_RLum.Results

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_RLum.Results
### Title: Plot function for an RLum.Results S4 class object
### Aliases: plot_RLum.Results
### Keywords: aplot

### ** Examples



###load data
data(ExampleData.DeValues, envir = environment())

# apply the un-logged minimum age model
mam <- calc_MinDose(data = ExampleData.DeValues$CA1, sigmab = 0.2, log = TRUE, plot = FALSE)

##plot
plot_RLum.Results(mam)

# estimate the number of grains on an aliquot
grains<- calc_AliquotSize(grain.size = c(100,150), sample.diameter = 1, plot = FALSE, MC.iter = 100)

##plot
plot_RLum.Results(grains)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_RLum.Results", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_ROI")
### * plot_ROI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_ROI
### Title: Create Regions of Interest (ROI) Graphic
### Aliases: plot_ROI
### Keywords: datagen plot

### ** Examples


## simple example
file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
temp <- read_RF2R(file)
plot_ROI(temp)

## in combination with extract_ROI()
m <- matrix(runif(100,0,255), ncol = 10, nrow = 10)
roi <- matrix(c(2.,4,2,5,6,7,3,1,1), ncol = 3)
t <- extract_ROI(object = m, roi = roi)
plot_ROI(t, bg_image = m)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_ROI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_RadialPlot")
### * plot_RadialPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_RadialPlot
### Title: Function to create a Radial Plot
### Aliases: plot_RadialPlot

### ** Examples


## load example data
data(ExampleData.DeValues, envir = environment())
ExampleData.DeValues <- Second2Gray(
  ExampleData.DeValues$BT998, c(0.0438,0.0019))

## plot the example data straightforward
plot_RadialPlot(data = ExampleData.DeValues)

## now with linear z-scale
plot_RadialPlot(
  data = ExampleData.DeValues,
  log.z = FALSE)

## now with output of the plot parameters
plot1 <- plot_RadialPlot(
  data = ExampleData.DeValues,
  log.z = FALSE,
  output = TRUE)
plot1
plot1$zlim

## now with adjusted z-scale limits
plot_RadialPlot(
  data = ExampleData.DeValues,
  log.z = FALSE,
  xlim = c(0, 5),
  zlim = c(100, 200))

## now the two plots with serious but seasonally changing fun
#plot_RadialPlot(data = data.3, fun = TRUE)

## now with user-defined central value, in log-scale again
plot_RadialPlot(
  data = ExampleData.DeValues,
  central.value = 150)

## now with a rug, indicating individual De values at the z-scale
plot_RadialPlot(
  data = ExampleData.DeValues,
  rug = TRUE)

## now with legend, colour, different points and smaller scale
plot_RadialPlot(
  data = ExampleData.DeValues,
  legend.text = "Sample 1",
  col = "tomato4",
  bar.col = "peachpuff",
  pch = "R",
  cex = 0.8)

## now without 2-sigma bar, y-axis, grid lines and central value line
plot_RadialPlot(
  data = ExampleData.DeValues,
  bar.col = "none",
  grid.col = "none",
  y.ticks = FALSE,
  lwd = 0)

## now with user-defined axes labels
plot_RadialPlot(
  data = ExampleData.DeValues,
  xlab = c("Data error (%)", "Data precision"),
  ylab = "Scatter",
  zlab = "Equivalent dose [Gy]")

## now with minimum, maximum and median value indicated
plot_RadialPlot(
  data = ExampleData.DeValues,
  central.value = 150,
  stats = c("min", "max", "median"))

## now with a brief statistical summary
plot_RadialPlot(
  data = ExampleData.DeValues,
  summary = c("n", "in.2s"))

## now with another statistical summary as subheader
plot_RadialPlot(
  data = ExampleData.DeValues,
  summary = c("mean.weighted", "median"),
  summary.pos = "sub")

## now the data set is split into sub-groups, one is manipulated
data.1 <- ExampleData.DeValues[1:15,]
data.2 <- ExampleData.DeValues[16:25,] * 1.3

## now a common dataset is created from the two subgroups
data.3 <- list(data.1, data.2)

## now the two data sets are plotted in one plot
plot_RadialPlot(data = data.3)

## now with some graphical modification
plot_RadialPlot(
  data = data.3,
  col = c("darkblue", "darkgreen"),
  bar.col = c("lightblue", "lightgreen"),
  pch = c(2, 6),
  summary = c("n", "in.2s"),
  summary.pos = "sub",
  legend = c("Sample 1", "Sample 2"))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_RadialPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_Risoe.BINfileData")
### * plot_Risoe.BINfileData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_Risoe.BINfileData
### Title: Plot single luminescence curves from a BIN file object
### Aliases: plot_Risoe.BINfileData
### Keywords: dplot

### ** Examples


##load data
data(ExampleData.BINfileData, envir = environment())

##plot all curves from the first position to the desktop
#pdf(file = "~/Desktop/CurveOutput.pdf", paper = "a4", height = 11, onefile = TRUE)

##example - load from *.bin file
#BINfile<- file.choose()
#BINfileData<-read_BIN2R(BINfile)

#par(mfrow = c(4,3), oma = c(0.5,1,0.5,1))
#plot_Risoe.BINfileData(CWOSL.SAR.Data,position = 1)
#mtext(side = 4, BINfile, outer = TRUE, col = "blue", cex = .7)
#dev.off()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_Risoe.BINfileData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot_ViolinPlot")
### * plot_ViolinPlot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot_ViolinPlot
### Title: Create a violin plot
### Aliases: plot_ViolinPlot

### ** Examples


## read example data set
data(ExampleData.DeValues, envir = environment())
ExampleData.DeValues <- Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))

## create plot straightforward
plot_ViolinPlot(data = ExampleData.DeValues)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot_ViolinPlot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_BIN2R")
### * read_BIN2R

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_BIN2R
### Title: Import Risø BIN/BINX-files into R
### Aliases: read_BIN2R
### Keywords: IO

### ** Examples


file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
temp <- read_BIN2R(file)
temp




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_BIN2R", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_Daybreak2R")
### * read_Daybreak2R

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_Daybreak2R
### Title: Import measurement data produced by a Daybreak TL/OSL reader
###   into R
### Aliases: read_Daybreak2R
### Keywords: IO

### ** Examples


## Not run: 
##D file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
##D temp <- read_Daybreak2R(file)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_Daybreak2R", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_HeliosOSL2R")
### * read_HeliosOSL2R

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_HeliosOSL2R
### Title: Import Luminescence Data from Helios Luminescence Reader
### Aliases: read_HeliosOSL2R
### Keywords: IO

### ** Examples

file <- system.file("extdata/HeliosOSL_Example.osl", package = "Luminescence")
read_HeliosOSL2R(file)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_HeliosOSL2R", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_PSL2R")
### * read_PSL2R

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_PSL2R
### Title: Import PSL files to R
### Aliases: read_PSL2R
### Keywords: IO

### ** Examples


# (1) Import PSL file to R

file <- system.file("extdata", "DorNie_0016.psl", package = "Luminescence")
psl <- read_PSL2R(file, drop_bg = FALSE, as_decay_curve = TRUE, smooth = TRUE, merge = FALSE)
print(str(psl, max.level = 3))
plot(psl, combine = TRUE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_PSL2R", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_RF2R")
### * read_RF2R

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_RF2R
### Title: Import RF-files to R
### Aliases: read_RF2R
### Keywords: IO

### ** Examples


##Import
file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
temp <- read_RF2R(file)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_RF2R", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_SPE2R")
### * read_SPE2R

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_SPE2R
### Title: Import Princeton Instruments (TM) SPE-file into R
### Aliases: read_SPE2R
### Keywords: IO

### ** Examples


## to run examples uncomment lines and run the code

##(1) Import data as RLum.Data.Spectrum object
#file <- file.choose()
#temp <- read_SPE2R(file)
#temp

##(2) Import data as RLum.Data.Image object
#file <- file.choose()
#temp <- read_SPE2R(file, output.object = "RLum.Data.Image")
#temp

##(3) Import data as matrix object
#file <- file.choose()
#temp <- read_SPE2R(file, output.object = "matrix")
#temp

##(4) Export raw data to csv, if temp is a RLum.Data.Spectrum object
# write.table(x = get_RLum(temp),
#             file = "[your path and filename]",
#             sep = ";", row.names = FALSE)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_SPE2R", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_TIFF2R")
### * read_TIFF2R

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_TIFF2R
### Title: Import TIFF Image Data into R
### Aliases: read_TIFF2R
### Keywords: IO

### ** Examples


## Not run: 
##D file <- file.choose()
##D image <- read_TIFF2R(file)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_TIFF2R", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_XSYG2R")
### * read_XSYG2R

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_XSYG2R
### Title: Import XSYG files to R
### Aliases: read_XSYG2R
### Keywords: IO

### ** Examples


##(1) import XSYG file to R (uncomment for usage)

#FILE <- file.choose()
#temp <- read_XSYG2R(FILE)

##(2) additional examples for pure XML import using the package XML
##    (uncomment for usage)

  ##import entire XML file
  #FILE <- file.choose()
  #temp <- XML::xmlRoot(XML::xmlTreeParse(FILE))

  ##search for specific subnodes with curves containing 'OSL'
  #getNodeSet(temp, "//Sample/Sequence/Record[@recordType = 'OSL']/Curve")

##(2) How to extract single curves ... after import
data(ExampleData.XSYG, envir = environment())

##grep one OSL curves and plot the first curve
OSLcurve <- get_RLum(OSL.SARMeasurement$Sequence.Object, recordType="OSL")[[1]]

##(3) How to see the structure of an object?
structure_RLum(OSL.SARMeasurement$Sequence.Object)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_XSYG2R", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("report_RLum")
### * report_RLum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: report_RLum
### Title: Create a HTML-report for (RLum) objects
### Aliases: report_RLum

### ** Examples


## Not run: 
##D ## Example: RLum.Results ----
##D 
##D # load example data
##D data("ExampleData.DeValues")
##D 
##D # apply the MAM-3 age model and save results
##D mam <- calc_MinDose(ExampleData.DeValues$CA1, sigmab = 0.2)
##D 
##D # create the HTML report
##D report_RLum(object = mam, file = "~/CA1_MAM.Rmd",
##D             timestamp = FALSE,
##D             title = "MAM-3 for sample CA1")
##D 
##D # when creating a report the input file is automatically saved to a
##D # .Rds file (see saveRDS()).
##D mam_report <- readRDS("~/CA1_MAM.Rds")
##D all.equal(mam, mam_report)
##D 
##D 
##D ## Example: Temporary file & Viewer/Browser ----
##D 
##D # (a)
##D # Specifying a filename is not necessarily required. If no filename is provided,
##D # the report is rendered in a temporary file. If you use the RStudio IDE, the
##D # temporary report is shown in the interactive Viewer pane.
##D report_RLum(object = mam)
##D 
##D # (b)
##D # Additionally, you can view the HTML report in your system's default web browser.
##D report_RLum(object = mam, launch.browser = TRUE)
##D 
##D 
##D ## Example: RLum.Analysis ----
##D 
##D data("ExampleData.RLum.Analysis")
##D 
##D # create the HTML report (note that specifying a file
##D # extension is not necessary)
##D report_RLum(object = IRSAR.RF.Data, file = "~/IRSAR_RF")
##D 
##D 
##D ## Example: RLum.Data.Curve ----
##D 
##D data.curve <- get_RLum(IRSAR.RF.Data)[[1]]
##D 
##D # create the HTML report
##D report_RLum(object = data.curve, file = "~/Data_Curve")
##D 
##D ## Example: Any other object ----
##D x <- list(x = 1:10,
##D           y = runif(10, -5, 5),
##D           z = data.frame(a = LETTERS[1:20], b = dnorm(0:9)),
##D           NA)
##D 
##D report_RLum(object = x, file = "~/arbitray_list")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("report_RLum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sTeve")
### * sTeve

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sTeve
### Title: sTeve - sophisticated tool for efficient data validation and
###   evaluation
### Aliases: sTeve
### Keywords: manip

### ** Examples


##no example available




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sTeve", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("scale_GammaDose")
### * scale_GammaDose

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: scale_GammaDose
### Title: Calculate the gamma dose deposited within a sample taking
###   layer-to-layer variations in radioactivity into account (according to
###   Aitken, 1985)
### Aliases: scale_GammaDose
### Keywords: datagen

### ** Examples


# Load example data
data("ExampleData.ScaleGammaDose", envir = environment())
x <- ExampleData.ScaleGammaDose

# Scale gamma dose rate
results <- scale_GammaDose(data = x,
                           conversion_factors = "Cresswelletal2018",
                           fractional_gamma_dose = "Aitken1985",
                           verbose = TRUE,
                           plot = TRUE)

get_RLum(results)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("scale_GammaDose", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("set_RLum")
### * set_RLum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: set_RLum
### Title: General set function for RLum S4 class objects
### Aliases: set_RLum
### Keywords: utilities

### ** Examples


##produce empty objects from each class
set_RLum(class = "RLum.Data.Curve")
set_RLum(class = "RLum.Data.Spectrum")
set_RLum(class = "RLum.Data.Spectrum")
set_RLum(class = "RLum.Analysis")
set_RLum(class = "RLum.Results")

##produce a curve object with arbitrary curve values
object <- set_RLum(
class = "RLum.Data.Curve",
curveType = "arbitrary",
recordType = "OSL",
data = matrix(c(1:100,exp(-c(1:100))),ncol = 2))

##plot this curve object
plot_RLum(object)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("set_RLum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("smooth_RLum")
### * smooth_RLum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: smooth_RLum
### Title: Smoothing of data
### Aliases: smooth_RLum smooth_RLum,list-method
### Keywords: utilities

### ** Examples


##load example data
data(ExampleData.CW_OSL_Curve, envir = environment())

##create RLum.Data.Curve object from this example
curve <-
  set_RLum(
      class = "RLum.Data.Curve",
      recordType = "OSL",
      data = as.matrix(ExampleData.CW_OSL_Curve)
  )

##plot data without and with smoothing
plot_RLum(curve)
plot_RLum(smooth_RLum(curve))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("smooth_RLum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("structure_RLum")
### * structure_RLum

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: structure_RLum
### Title: General structure function for RLum S4 class objects
### Aliases: structure_RLum structure_RLum,list-method
### Keywords: utilities

### ** Examples


##load example data
data(ExampleData.XSYG, envir = environment())

##show structure
structure_RLum(OSL.SARMeasurement$Sequence.Object)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("structure_RLum", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("subset_SingleGrainData")
### * subset_SingleGrainData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: subset_SingleGrainData
### Title: Simple Subsetting of Single Grain Data from Risø BIN/BINX files
### Aliases: subset_SingleGrainData
### Keywords: datagen manip

### ** Examples


## load example data
data(ExampleData.BINfileData, envir = environment())

## set POSITION/GRAIN pair dataset
selection <- data.frame(POSITION = c(1,5,7), GRAIN = c(0,0,0))

##subset
subset_SingleGrainData(object = CWOSL.SAR.Data, selection = selection)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("subset_SingleGrainData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("template_DRAC")
### * template_DRAC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: template_DRAC
### Title: Create a DRAC input data template (v1.2)
### Aliases: template_DRAC

### ** Examples


# create a new DRAC input input
input <- template_DRAC(preset = "DRAC-example_quartz")

# show content of the input
print(input)
print(input$`Project ID`)
print(input[[4]])


## Example: DRAC Quartz example
# note that you only have to assign new values where they
# are different to the default values
input$`Project ID` <- "DRAC-Example"
input$`Sample ID` <- "Quartz"
input$`Conversion factors` <- "AdamiecAitken1998"
input$`External U (ppm)` <- 3.4
input$`errExternal U (ppm)` <- 0.51
input$`External Th (ppm)` <- 14.47
input$`errExternal Th (ppm)` <- 1.69
input$`External K (%)` <- 1.2
input$`errExternal K (%)` <- 0.14
input$`Calculate external Rb from K conc?` <- "N"
input$`Calculate internal Rb from K conc?` <- "N"
input$`Scale gammadoserate at shallow depths?` <- "N"
input$`Grain size min (microns)` <- 90
input$`Grain size max (microns)` <- 125
input$`Water content ((wet weight - dry weight)/dry weight) %` <- 5
input$`errWater content %` <- 2
input$`Depth (m)` <- 2.2
input$`errDepth (m)` <- 0.22
input$`Overburden density (g cm-3)` <- 1.8
input$`errOverburden density (g cm-3)` <- 0.1
input$`Latitude (decimal degrees)` <- 30.0000
input$`Longitude (decimal degrees)` <- 70.0000
input$`Altitude (m)` <- 150
input$`De (Gy)` <- 20
input$`errDe (Gy)` <- 0.2

# use DRAC
## Not run: 
##D output <- use_DRAC(input)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("template_DRAC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("trim_RLum.Data")
### * trim_RLum.Data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: trim_RLum.Data
### Title: Trim Channels of RLum.Data-class Objects
### Aliases: trim_RLum.Data
### Keywords: manip

### ** Examples

## trim all TL curves in the object to channels 10 to 20
data(ExampleData.BINfileData, envir = environment())
temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)

c <- trim_RLum.Data(
object = temp,
recordType = "TL",
trim_range = c(10,20))

plot_RLum.Analysis(
object = c,
combine = TRUE,
subset = list(recordType = "TL"))

## simulate a situation where one OSL curve
## in the dataset has only 999 channels instead of 1000
## all curves should be limited to 999
temp@records[[2]]@data <- temp@records[[2]]@data[-nrow(temp[[2]]@data),]

c <- trim_RLum.Data(object = temp)
nrow(c@records[[4]]@data)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("trim_RLum.Data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tune_Data")
### * tune_Data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tune_Data
### Title: Tune data for experimental purpose
### Aliases: tune_Data
### Keywords: manip

### ** Examples


## load example data set
data(ExampleData.DeValues, envir = environment())
x <- ExampleData.DeValues$CA1

## plot original data
plot_AbanicoPlot(data = x,
                 summary = c("n", "mean"))

## decrease error by 10 %
plot_AbanicoPlot(data = tune_Data(x, decrease.error = 0.1),
                 summary = c("n", "mean"))

## increase sample size by 200 %
#plot_AbanicoPlot(data = tune_Data(x, increase.data = 2) ,
#                summary = c("n", "mean"))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tune_Data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("use_DRAC")
### * use_DRAC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: use_DRAC
### Title: Use DRAC to calculate dose rate data
### Aliases: use_DRAC

### ** Examples


## (1) Method using the DRAC spreadsheet

file <-  "/PATH/TO/DRAC_Input_Template.csv"

# send the actual IO template spreadsheet to DRAC
## Not run: 
##D use_DRAC(file = file)
## End(Not run)



## (2) Method using an R template object

# Create a template
input <- template_DRAC(preset = "DRAC-example_quartz")

# Fill the template with values
input$`Project ID` <- "DRAC-Example"
input$`Sample ID` <- "Quartz"
input$`Conversion factors` <- "AdamiecAitken1998"
input$`External U (ppm)` <- 3.4
input$`errExternal U (ppm)` <- 0.51
input$`External Th (ppm)` <- 14.47
input$`errExternal Th (ppm)` <- 1.69
input$`External K (%)` <- 1.2
input$`errExternal K (%)` <- 0.14
input$`Calculate external Rb from K conc?` <- "N"
input$`Calculate internal Rb from K conc?` <- "N"
input$`Scale gammadoserate at shallow depths?` <- "N"
input$`Grain size min (microns)` <- 90
input$`Grain size max (microns)` <- 125
input$`Water content ((wet weight - dry weight)/dry weight) %` <- 5
input$`errWater content %` <- 2
input$`Depth (m)` <- 2.2
input$`errDepth (m)` <- 0.22
input$`Overburden density (g cm-3)` <- 1.8
input$`errOverburden density (g cm-3)` <- 0.1
input$`Latitude (decimal degrees)` <- 30.0000
input$`Longitude (decimal degrees)` <- 70.0000
input$`Altitude (m)` <- 150
input$`De (Gy)` <- 20
input$`errDe (Gy)` <- 0.2

# use DRAC
## Not run: 
##D output <- use_DRAC(input)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("use_DRAC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("verify_SingleGrainData")
### * verify_SingleGrainData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: verify_SingleGrainData
### Title: Verify single grain data sets and check for invalid grains, i.e.
###   zero-light level grains
### Aliases: verify_SingleGrainData
### Keywords: datagen manip

### ** Examples


##01 - basic example I
##just show how to apply the function
data(ExampleData.XSYG, envir = environment())

##verify and get data.frame out of it
verify_SingleGrainData(OSL.SARMeasurement$Sequence.Object)$selection_full

##02 - basic example II
data(ExampleData.BINfileData, envir = environment())
id <- verify_SingleGrainData(object = CWOSL.SAR.Data,
cleanup_level = "aliquot")$selection_id

## Not run: 
##D ##03 - advanced example I
##D ##importing and exporting a BIN-file
##D 
##D ##select and import file
##D file <- file.choose()
##D object <- read_BIN2R(file)
##D 
##D ##remove invalid aliquots(!)
##D object <- verify_SingleGrainData(object, cleanup = TRUE)
##D 
##D ##export to new BIN-file
##D write_R2BIN(object, paste0(dirname(file),"/", basename(file), "_CLEANED.BIN"))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("verify_SingleGrainData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_R2BIN")
### * write_R2BIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_R2BIN
### Title: Export Risoe.BINfileData into Risø BIN/BINX-file
### Aliases: write_R2BIN
### Keywords: IO

### ** Examples

##load example dataset
file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
temp <- read_BIN2R(file)

##create temporary file path
##(for usage replace by own path)
temp_file <- tempfile(pattern = "output", fileext = ".binx")

##export to temporary file path
write_R2BIN(temp, file = temp_file)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_R2BIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_R2TIFF")
### * write_R2TIFF

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_R2TIFF
### Title: Export RLum.Data.Image and RLum.Data.Spectrum objects to TIFF
###   Images
### Aliases: write_R2TIFF
### Keywords: IO

### ** Examples

data(ExampleData.RLum.Data.Image, envir = environment())
write_R2TIFF(ExampleData.RLum.Data.Image, file = tempfile())




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_R2TIFF", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_RLum2CSV")
### * write_RLum2CSV

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_RLum2CSV
### Title: Export RLum-objects to CSV
### Aliases: write_RLum2CSV
### Keywords: IO

### ** Examples


##transform values to a list (and do not write)
data(ExampleData.BINfileData, envir = environment())
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data)[[1]]
write_RLum2CSV(object, export = FALSE)

## Not run: 
##D 
##D ##create temporary filepath
##D ##(for usage replace by own path)
##D temp_file <- tempfile(pattern = "output", fileext = ".csv")
##D 
##D ##write CSV-file to working directory
##D write_RLum2CSV(temp_file)
##D 
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_RLum2CSV", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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

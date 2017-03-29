#' Comprehensive Luminescence Dating Data Analysis
#'
#' A collection of various R functions for the purpose of Luminescence dating
#' data analysis. This includes, amongst others, data import, export,
#' application of age models, curve deconvolution, sequence analysis and
#' plotting of equivalent dose distributions.
#'
#' \tabular{ll}{ Package: \tab Luminescence\cr Type: \tab Package\cr Version:
#' \tab 0.7.4 \cr Date: \tab 2017-03-31 \cr License: \tab GPL-3\cr }
#'
#' @name Luminescence-package
#' @aliases Luminescence-package Luminescence
#' @docType package
#' @author \bold{Full list of authors and contributors} (alphabetic order)
#'
#' \tabular{ll}{
#' Christoph Burow \tab University of Cologne, Germany \cr
#' Claire Christophe \tab IRAMAT-CRP2A, Universite Bordeaux Montaigne, France \cr
#' Michael Dietze \tab GFZ Helmholtz Centre Potsdam, Germany \cr
#' Julie Durcan \tab University of Oxford, United Kingdom \cr
#' Manfred Fischer\tab University of Bayreuth, Germany \cr
#' Margret C. Fuchs \tab Helmholtz-Zentrum Dresden-Rossendorf, Helmholtz-Institute Freiberg for Resource Technology,
#' Freiberg, Germany \cr
#' Johannes Friedrich \tab University of Bayreuth, Germany \cr
#' Guillaume Guerin \tab IRAMAT-CRP2A, Universite Bordeaux Montaigne, France \cr
#' Georgina King \tab Institute of Geological Sciences, University of Bern, Switzerland \cr
#' Sebastian Kreutzer \tab IRAMAT-CRP2A, Universite Bordeaux Montaigne, France \cr
#' Norbert Mercier \tab IRAMAT-CRP2A, Universite Bordeaux Montaigne, France \cr
#' Anne Philippe \tab  Universite de Nantes and ANJA INRIA, Rennes, France \cr
#' Christoph Schmidt \tab University of Bayreuth, Germany \cr
#' Rachel K. Smedley \tab Aberystwyth University, United Kingdom \cr
#' Antoine Zink \tab C2RMF, Palais du Louvre, Paris, France
#' }
#'
#' \bold{Supervisor of the initial version in 2012}
#'
#' Markus Fuchs, Justus-Liebig-University Giessen, Germany\cr
#'
#' \bold{Support contact}
#'
#' \email{developers@@r-luminescence.org}\cr
#'
#' We may further encourage the usage of our support forum. For this please
#' visit our project website (link below).
#'
#' \bold{Bug reporting}
#'
#' \email{developers@@r-luminescence.org} or \cr
#' \url{https://github.com/R-Lum/Luminescence/issues} \cr
#'
#' \bold{Project website}
#'
#' \url{http://www.r-luminescence.org}\cr
#'
#' \bold{Project source code repository}\cr
#' \url{https://github.com/R-Lum/Luminescence}\cr
#'
#' \bold{Related package projects}\cr
#' \url{https://cran.r-project.org/package=RLumShiny}\cr
#' \url{http://shiny.r-luminescence.org}\cr
#' \url{https://cran.r-project.org/package=RLumModel}\cr
#' \url{http://model.r-luminescence.org}\cr
#'
#' \bold{Package maintainer}
#'
#' Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne, Pessac,
#' France, \cr \email{sebastian.kreutzer@@u-bordeaux-montaigne.fr}
#'
#' \bold{Acknowledgement}
#'
#' Cooperation and personal exchange between the developers is gratefully
#' funded by the DFG (SCHM 3051/3-1) in the framework of the program
#' "Scientific Networks". Project title: "RLum.Network: Ein
#' Wissenschaftsnetzwerk zur Analyse von Lumineszenzdaten mit R" (2014-2017)
#'
#' @references Dietze, M., Kreutzer, S., Fuchs, M.C., Burow, C., Fischer, M.,
#' Schmidt, C., 2013. A practical guide to the R package Luminescence.
#' Ancient TL, 31, 11-18.
#'
#' Dietze, M., Kreutzer, S., Burow, C., Fuchs, M.C., Fischer, M., Schmidt, C., 2016. The abanico plot:
#' visualising chronometric data with individual standard errors. Quaternary Geochronology 31, 1-7.
#' http://dx.doi.org/10.1016/j.quageo.2015.09.003
#'
#' Fuchs, M.C., Kreutzer, S., Burow, C., Dietze, M., Fischer, M., Schmidt, C.,
#' Fuchs, M., 2015. Data processing in luminescence dating analysis: An
#' exemplary workflow using the R package 'Luminescence'. Quaternary
#' International, 362,8-13. http://dx.doi.org/10.1016/j.quaint.2014.06.034
#'
#' Kreutzer, S., Schmidt, C., Fuchs, M.C., Dietze, M., Fischer, M., Fuchs, M.,
#' 2012. Introducing an R package for luminescence dating analysis. Ancient TL,
#' 30, 1-8.
#'
#' Smedley, R.K., 2015. A new R function for the Internal External Uncertainty (IEU) model.
#' Ancient TL 33, 16-21.
#'
#' @keywords package
#' @import utils methods data.table magrittr
#' @importFrom raster nlayers raster contour plot plotRGB brick
#' @importFrom graphics plot plot.default frame abline mtext text lines par layout lines arrows axTicks axis barplot box boxplot contour curve grconvertX grconvertY hist legend persp points polygon rug segments title grid
#' @importFrom grDevices adjustcolor axisTicks colorRampPalette gray.colors rgb topo.colors xy.coords dev.off
#' @importFrom stats approx as.formula complete.cases density dnorm glm lm median na.exclude na.omit nls nls.control pchisq pnorm quantile rnorm runif sd smooth smooth.spline spline t.test uniroot var weighted.mean setNames coef confint predict update residuals
#' @importFrom parallel parLapply makeCluster stopCluster
#' @importFrom httr GET accept_json status_code content
#' @useDynLib Luminescence, .registration = TRUE
NULL


#' Base data set for cosmic dose rate calculation
#'
#' Collection of data from various sources needed for cosmic dose rate
#' calculation
#'
#'
#' @format
#'
#' \tabular{ll}{
#'
#' \code{values.cosmic.Softcomp}: \tab data frame containing cosmic dose rates
#' for shallow depths (< 167 g cm^-2) obtained using the "AGE" program by
#' Rainer Gruen (cf. Gruen 2009). These data essentially reproduce the graph
#' shown in Fig. 1 of Prescott & Hutton (1988). \cr
#'
#' \code{values.factor.Altitude}: \tab data frame containing altitude factors
#' for adjusting geomagnetic field-change factors. Values were read from Fig. 1
#' in Prescott & Hutton (1994). \cr
#'
#' \code{values.par.FJH}: \tab data frame containing values for parameters F, J
#' and H (read from Fig. 2 in Prescott & Hutton 1994) used in the expression }
#'
#' \deqn{Dc = D0*(F+J*exp((altitude/1000)/H))}
#' @section Version: 0.1
#' @references
#' Gruen, R., 2009. The "AGE" program for the calculation of luminescence age estimates.
#' Ancient TL, 27, pp. 45-46.
#'
#' Prescott, J.R., Hutton, J.T., 1988. Cosmic ray and gamma ray dosimetry for
#' TL and ESR. Nuclear Tracks and Radiation Measurements, 14, pp. 223-227.
#'
#' Prescott, J.R., Hutton, J.T., 1994. Cosmic ray contributions to dose rates
#' for luminescence and ESR dating: large depths and long-term time variations.
#' Radiation Measurements, 23, pp. 497-500.
#' @source The following data were carefully read from figures in mentioned
#' sources and used for fitting procedures. The derived expressions are used in
#' the function \code{calc_CosmicDoseRate}.
#'
#' \bold{values.cosmic.Softcomp}
#'
#' \tabular{ll}{
#'
#' Program: \tab "AGE"\cr Reference: \tab Gruen (2009) \cr Fit: \tab
#' Polynomials in the form of
#'
#' }
#'
#' For depths between 40-167 g cm^-2:
#'
#' \deqn{y = 2*10^-6*x^2-0.0008*x+0.2535}
#'
#' (For depths <40 g cm^-2)
#'
#' \deqn{y = -6*10^-8*x^3+2*10^-5*x^2-0.0025*x+0.2969}
#'
#' \bold{values.factor.Altitude}
#'
#' \tabular{ll}{
#'
#' Reference: \tab Prescott & Hutton (1994) \cr Page: \tab 499 \cr Figure: \tab
#' 1 \cr Fit: \tab 2-degree polynomial in the form of
#'
#' }
#'
#' \deqn{y = -0.026*x^2 + 0.6628*x + 1.0435}
#'
#' \bold{values.par.FJH}
#'
#' \tabular{ll}{
#'
#' Reference: \tab Prescott & Hutton (1994) \cr Page: \tab 500 \cr Figure: \tab
#' 2 \cr Fits: \tab 3-degree polynomials and linear fits
#'
#' }
#'
#' F (non-linear part, \eqn{\lambda} < 36.5 deg.):
#'
#' \deqn{y = -7*10^-7*x^3-8*10^-5*x^2-0.0009*x+0.3988}
#'
#' F (linear part, \eqn{\lambda} > 36.5 deg.):
#'
#' \deqn{y = -0.0001*x + 0.2347}
#'
#' J (non-linear part, \eqn{\lambda} < 34 deg.):
#'
#' \deqn{y = 5*10^-6*x^3-5*10^-5*x^2+0.0026*x+0.5177}
#'
#' J (linear part, \eqn{\lambda} > 34 deg.):
#'
#' \deqn{y = 0.0005*x + 0.7388}
#'
#' H (non-linear part, \eqn{\lambda} < 36 deg.):
#'
#' \deqn{y = -3*10^-6*x^3-5*10^-5*x^2-0.0031*x+4.398}
#'
#' H (linear part, \eqn{\lambda} > 36 deg.):
#'
#' \deqn{y = 0.0002*x + 4.0914}
#' @keywords datasets
#' @examples
#'
#' ##load data
#' data(BaseDataSet.CosmicDoseRate)
#' @name BaseDataSet.CosmicDoseRate
NULL


#' Example data from a SAR OSL and SAR TL measurement for the package
#' Luminescence
#'
#' Example data from a SAR OSL and TL measurement for package Luminescence
#' directly extracted from a Risoe BIN-file and provided in an object of type
#' \link{Risoe.BINfileData-class}
#'
#'
#' @format
#'
#' \code{CWOSL.SAR.Data}: SAR OSL measurement data
#'
#' \code{TL.SAR.Data}: SAR TL measurement data
#'
#' Each class object contains two slots: (a) \code{METADATA} is a
#' \link{data.frame} with all metadata stored in the BIN file of the
#' measurements and (b) \code{DATA} contains a list of vectors of the measured
#' data (usually count values).
#' @section Version: 0.1
#' @references
#' \bold{CWOSL.SAR.Data}: unpublished data \cr
#'
#' \bold{TL.SAR.Data}: unpublished data
#' @source \bold{CWOSL.SAR.Data}
#'
#' \tabular{ll}{
#'
#' Lab: \tab Luminescence Laboratory Bayreuth\cr Lab-Code: \tab BT607\cr
#' Location: \tab Saxony/Germany\cr Material: \tab Middle grain quartz measured
#' \cr \tab on aluminum cups on a Risoe TL/OSL DA-15 reader\cr Reference: \tab
#' unpublished }
#'
#' \bold{TL.SAR.Data}
#'
#' \tabular{ll}{
#'
#' Lab: \tab Luminescence Laboratory of Cologne\cr Lab-Code: \tab LP1_5\cr
#' Location: \tab Spain\cr Material: \tab Flint \cr Setup: \tab Risoe TL/OSL
#' DA-20 reader \cr \tab (Filter: Semrock Brightline, \cr \tab HC475/50, N2,
#' unpolished steel discs) \cr Reference: \tab unpublished \cr Remarks: \tab
#' dataset limited to one position\cr }
#'
#' @note Please note that this example data cannot be exported to a BIN-file using the function
#' \code{writeR2BIN} as it was generated and implemented in the package long time ago. In the meantime
#' the BIN-file format changed.
#'
#' @keywords datasets
#' @examples
#'
#' ##show first 5 elements of the METADATA and DATA elements in the terminal
#' data(ExampleData.BINfileData, envir = environment())
#' CWOSL.SAR.Data@@METADATA[1:5,]
#' CWOSL.SAR.Data@@DATA[1:5]
#'
#' @name ExampleData.BINfileData
NULL


#' Example CW-OSL curve data for the package Luminescence
#'
#' \code{data.frame} containing CW-OSL curve data (time, counts)
#'
#' @name ExampleData.CW_OSL_Curve
#' @docType data
#' @format Data frame with 1000 observations on the following 2 variables:
#' \describe{ \item{list("x")}{a numeric vector, time} \item{list("y")}{a
#' numeric vector, counts} }
#' @references Baartman, J.E.M., Veldkamp, A., Schoorl, J.M., Wallinga, J.,
#' Cammeraat, L.H., 2011. Unravelling Late Pleistocene and Holocene landscape
#' dynamics: The Upper Guadalentin Basin, SE Spain. Geomorphology, 125,
#' 172-185.
#'
#' Bos, A.J.J. & Wallinga, J., 2012. How to visualize quartz OSL signal
#' components. Radiation Measurements, 47, 752-758.
#'
#' @source \bold{ExampleData.CW_OSL_Curve}
#'
#' \tabular{ll}{ Lab: \tab Luminescence Laboratory Bayreuth\cr Lab-Code: \tab
#' BT607\cr Location: \tab Saxony/Germany\cr Material: \tab Middle grain quartz
#' measured on aluminum cups on a Risoe TL/OSL DA-15 reader.\cr Reference: \tab
#' unpublished data }
#'
#' \bold{CW_Curve.BosWallinga2012}
#'
#' \tabular{ll}{ Lab: \tab Netherlands Centre for Luminescence Dating (NCL)\cr
#' Lab-Code: \tab NCL-2108077\cr Location: \tab Guadalentin Basin, Spain\cr
#' Material: \tab Coarse grain quartz\cr Reference: \tab Bos & Wallinga (2012)
#' and Baartman et al. (2011) }
#'
#' @keywords datasets
#' @examples
#'
#' data(ExampleData.CW_OSL_Curve, envir = environment())
#' plot(ExampleData.CW_OSL_Curve)
#'
NULL



#' Example portable OSL curve data for the package Luminescence
#'
#' A \code{list} of \code{\linkS4class{RLum.Analysis}} objects, each containing
#' the same number of \code{\linkS4class{RLum.Data.Curve}} objects representing
#' individual OSL, IRSL and dark count measurements of a sample.
#'
#' @name ExampleData.portableOSL
#' @docType data
#'
#' @source \bold{ExampleData.portableOSL}
#'
#' \tabular{ll}{ Lab: \tab Cologne Luminescence Laboratory\cr Lab-Code: \tab
#' - \cr Location: \tab Nievenheim/Germany\cr Material: \tab Fine grain quartz
#' \cr Reference: \tab unpublished data }
#'
#' @keywords datasets
#' @examples
#'
#' data(ExampleData.portableOSL, envir = environment())
#' plot_RLum(ExampleData.portableOSL)
#'
NULL



#' Example data for fit_LMCurve() in the package Luminescence
#'
#' Lineraly modulated (LM) measurement data from a quartz sample from Norway
#' including background measurement. Measurements carried out in the
#' luminescence laboratory at the University of Bayreuth.
#'
#'
#' @format Two objects (data.frames) with two columns (time and counts).
#' @references
#' Fuchs, M., Kreutzer, S., Fischer, M., Sauer, D., Soerensen, R., 2012. OSL and IRSL
#' dating of raised beach sand deposits along the southeastern coast of Norway.
#' Quaternary Geochronology, 10, 195-200.
#' @source
#' \tabular{ll}{ Lab: \tab Luminescence Laboratory Bayreuth\cr Lab-Code: \tab
#' BT900\cr Location: \tab Norway\cr Material: \tab Beach deposit, coarse grain
#' quartz measured on aluminum discs on a Risoe TL/OSL DA-15 reader\cr }
#' @examples
#'
#' ##show LM data
#' data(ExampleData.FittingLM, envir = environment())
#' plot(values.curve,log="x")
#'
#' @name ExampleData.FittingLM
NULL


#' Example Lx/Tx data from CW-OSL SAR measurement
#'
#' LxTx data from a SAR measurement for the package Luminescence.
#'
#'
#' @format A \code{data.frame} with 4 columns (Dose, LxTx, LxTx.Error, TnTx).
#' @references unpublished data
#' @source
#' \tabular{ll}{ Lab: \tab Luminescence Laboratory Bayreuth\cr Lab-Code: \tab
#' BT607\cr Location: \tab Ostrau (Saxony-Anhalt/Germany)\cr Material: \tab
#' Middle grain (38-63 \eqn{\mu}m) quartz measured on a Risoe TL/OSL DA-15
#' reader.\cr }
#' @examples
#'
#' ##plot Lx/Tx data vs dose [s]
#' data(ExampleData.LxTxData, envir = environment())
#' plot(LxTxData$Dose,LxTxData$LxTx)
#'
#' @name ExampleData.LxTxData
NULL


#' Example Lx and Tx curve data from an artificial OSL measurement
#'
#' Lx and Tx data of continous wave (CW-) OSL signal curves.
#'
#'
#' @format Two \code{data.frames} containing time and count values.
#' @references unpublished data
#' @source
#' Arbitrary OSL measurement.
#' @examples
#'
#' ##load data
#' data(ExampleData.LxTxOSLData, envir = environment())
#'
#' ##plot data
#' plot(Lx.data)
#' plot(Tx.data)
#'
#' @name ExampleData.LxTxOSLData
NULL


#' Example data as \code{\linkS4class{RLum.Analysis}} objects
#'
#' Collection of different \code{\linkS4class{RLum.Analysis}} objects for
#' protocol analysis.
#'
#'
#' @format
#'
#' \code{IRSAR.RF.Data}: IRSAR.RF.Data on coarse grain feldspar
#'
#' Each object contains data needed for the given protocol analysis.
#' @section Version: 0.1
#' @references
#' \bold{IRSAR.RF.Data}
#'
#' Kreutzer, S., Lauer, T., Meszner, S., Krbetschek, M.R., Faust, D., Fuchs,
#' M., 2014. Chronology of the Quaternary profile Zeuchfeld in Saxony-Anhalt /
#' Germany - a preliminary luminescence dating study. Zeitschrift fuer
#' Geomorphologie 58, 5-26. doi: 10.1127/0372-8854/2012/S-00112
#' @source \bold{IRSAR.RF.Data}
#'
#' These data were kindly provided by Tobias Lauer and Matthias Krbetschek.
#'
#' \tabular{ll}{
#'
#' Lab: \tab Luminescence Laboratory TU Bergakademie Freiberg\cr Lab-Code: \tab
#' ZEU/SA1\cr Location: \tab Zeuchfeld (Zeuchfeld Sandur;
#' Saxony-Anhalt/Germany)\cr Material: \tab K-feldspar (130-200 \eqn{\mu}m)\cr
#' Reference: \tab Kreutzer et al. (2014)\cr
#'
#' }
#' @keywords datasets
#' @examples
#'
#' ##load data
#' data(ExampleData.RLum.Analysis, envir = environment())
#'
#' ##plot data
#' plot_RLum(IRSAR.RF.Data)
#'
#' @name ExampleData.RLum.Analysis
NULL


#' Example data as \code{\linkS4class{RLum.Data.Image}} objects
#'
#' Measurement of Princton Instruments camera imported with the function
#' \code{\link{read_SPE2R}} to R to produce an
#' \code{\linkS4class{RLum.Data.Image}} object.
#'
#'
#' @format Object of class \code{\linkS4class{RLum.Data.Image}}
#' @section Version: 0.1
#' @source \bold{ExampleData.RLum.Data.Image}
#'
#' These data were kindly provided by Regina DeWitt.
#'
#' \tabular{ll}{
#'
#' Lab.: \tab Department of Physics, East-Carolina University, NC, USA\cr
#' Lab-Code: \tab -\cr Location: \tab - \cr Material: \tab - \cr Reference:
#' \tab - \cr
#'
#' }
#'
#' Image data is a measurement of fluorescent ceiling lights with a cooled
#' Princeton Instruments (TM) camera fitted on Risoe DA-20 TL/OSL reader.
#' @keywords datasets
#' @examples
#'
#' ##load data
#' data(ExampleData.RLum.Data.Image, envir = environment())
#'
#' ##plot data
#' plot_RLum(ExampleData.RLum.Data.Image)
#'
#' @name ExampleData.RLum.Data.Image
NULL


#' Example data for a SAR OSL measurement and a TL spectrum using a lexsyg
#' reader
#'
#' Example data from a SAR OSL measurement and a TL spectrum for package
#' Luminescence imported from a Freiberg Instruments XSYG file using the
#' function \code{\link{read_XSYG2R}}.
#'
#'
#' @format
#'
#' \code{OSL.SARMeasurement}: SAR OSL measurement data
#'
#' The data contain two elements: (a) \code{$Sequence.Header} is a
#' \link{data.frame} with metadata from the measurement,(b)
#' \code{Sequence.Object} contains an \code{\linkS4class{RLum.Analysis}} object
#' for further analysis.\cr
#'
#' \code{TL.Spectrum}: TL spectrum data
#'
#' \code{\linkS4class{RLum.Data.Spectrum}} object for further analysis. The
#' spectrum was cleaned from cosmic-rays using the function
#' \code{apply_CosmicRayRemoval}. Note that no quantum efficiency calibration
#' was performed.
#' @section Version: 0.1
#' @seealso \code{\link{read_XSYG2R}}, \code{\linkS4class{RLum.Analysis}},\cr
#' \code{\linkS4class{RLum.Data.Spectrum}}, \code{\link{plot_RLum}},\cr
#' \code{\link{plot_RLum.Analysis}}, \code{\link{plot_RLum.Data.Spectrum}}
#' @references Unpublished data measured to serve as example data for that
#' package. Location origin of sample BT753 is given here:
#'
#' Fuchs, M., Kreutzer, S., Rousseau, D.D., Antoine, P., Hatte, C., Lagroix,
#' F., Moine, O., Gauthier, C., Svoboda, J., Lisa, L., 2013. The loess sequence
#' of Dolni Vestonice, Czech Republic: A new OSL-based chronology of the Last
#' Climatic Cycle. Boreas, 42, 664--677.
#' @source \bold{OSL.SARMeasurement}
#'
#' \tabular{ll}{
#'
#' Lab: \tab Luminescence Laboratory Giessen\cr Lab-Code: \tab no code\cr
#' Location: \tab not specified\cr Material: \tab Coarse grain quartz \cr \tab
#' on steel cups on lexsyg research reader\cr Reference: \tab unpublished }
#'
#' \bold{TL.Spectrum}
#'
#' \tabular{ll}{
#'
#' Lab: \tab Luminescence Laboratory Giessen\cr Lab-Code: \tab BT753\cr
#' Location: \tab Dolni Vestonice/Czech Republic\cr Material: \tab Fine grain
#' polymineral \cr \tab on steel cups on lexsyg rearch reader\cr Reference:
#' \tab Fuchs et al., 2013 \cr Spectrum: \tab Integration time 19 s, channel
#' time 20 s\cr Heating: \tab 1 K/s, up to 500 deg. C }
#' @keywords datasets
#' @examples
#'
#' ##show data
#' data(ExampleData.XSYG, envir = environment())
#'
#' ## =========================================
#' ##(1) OSL.SARMeasurement
#' OSL.SARMeasurement
#'
#' ##show $Sequence.Object
#' OSL.SARMeasurement$Sequence.Object
#'
#' ##grep OSL curves and plot the first curve
#' OSLcurve <- get_RLum(OSL.SARMeasurement$Sequence.Object,
#' recordType="OSL")[[1]]
#' plot_RLum(OSLcurve)
#'
#' ## =========================================
#' ##(2) TL.Spectrum
#' TL.Spectrum
#'
#' ##plot simple spectrum (2D)
#' plot_RLum.Data.Spectrum(TL.Spectrum,
#'                         plot.type="contour",
#'                         xlim = c(310,750),
#'                         ylim = c(0,300),
#'                         bin.rows=10,
#'                         bin.cols = 1)
#'
#' ##plot 3d spectrum (uncomment for usage)
#' # plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="persp",
#' # xlim = c(310,750), ylim = c(0,300), bin.rows=10,
#' # bin.cols = 1)
#'
#' @name ExampleData.XSYG
NULL


#' Example De data sets for the package Luminescence
#'
#' Equivalent dose (De) values measured for a fine grain quartz sample from a
#' loess section in Rottewitz (Saxony/Germany) and for a coarse grain quartz
#' sample from a fluvial deposit in the rock shelter of Cueva Anton
#' (Murcia/Spain).
#'
#'
#' @format A \code{\link{list}} with two elements, each containing a two column
#' \code{\link{data.frame}}:
#'
#' \describe{ \code{$BT998}: De and De error values for a fine grain quartz
#' sample from a loess section in Rottewitz.\cr\cr \code{$CA1}: Single grain De
#' and De error values for a coarse grain quartz sample from a fluvial deposit
#' in the rock shelter of Cueva Anton }
#' @references \bold{BT998} \cr\cr Unpublished data \cr\cr
#' \bold{CA1} \cr\cr
#' Burow, C., Kehl, M., Hilgers, A., Weniger, G.-C., Angelucci, D., Villaverde,
#' V., Zapata, J. and Zilhao, J.  (2015). Luminescence dating of fluvial
#' deposits in the rock shelter of Cueva Anton, Spain. Geochronometria 52, 107-125.
#'
#' \bold{BT998} \cr
#' \tabular{ll}{ Lab: \tab Luminescence Laboratory Bayreuth\cr
#' Lab-Code: \tab BT998\cr Location: \tab Rottewitz (Saxony/Germany)\cr
#' Material: \tab Fine grain quartz measured on aluminum discs on a Risoe
#' TL/OSL DA-15 reader\cr Units: \tab Values are given in seconds \cr Dose
#' Rate: \tab Dose rate of the beta-source at measurement ca. 0.0438 Gy/s +/-
#' 0.0019 Gy/s\cr Measurement Date: \tab 2012-01-27 }
#' \bold{CA1} \cr
#' \tabular{ll}{ Lab: \tab Cologne Luminescence Laboratory (CLL)\cr Lab-Code:
#' \tab C-L2941\cr Location: \tab Cueva Anton (Murcia/Spain)\cr Material: \tab
#' Coarse grain quartz (200-250 microns) measured on single grain discs on a
#' Risoe TL/OSL DA-20 reader\cr Units: \tab Values are given in Gray \cr
#' Measurement Date: \tab 2012 }
#' @keywords datasets
#' @examples
#'
#' ##(1) plot values as histogram
#' data(ExampleData.DeValues, envir = environment())
#' plot_Histogram(ExampleData.DeValues$BT998, xlab = "De [s]")
#'
#' ##(2) plot values as histogram (with second to gray conversion)
#' data(ExampleData.DeValues, envir = environment())
#'
#' De.values <- Second2Gray(ExampleData.DeValues$BT998,
#'                          dose.rate = c(0.0438, 0.0019))
#'
#'
#' plot_Histogram(De.values, xlab = "De [Gy]")
#'
#' @name ExampleData.DeValues
NULL


#' Example data for feldspar fading measurements
#'
#' Example data set for fading measurements of the IR50, IR100, IR150 and
#' IR225 feldspar signals of sample UNIL/NB123. It further contains regular equivalent dose
#' measurement data of the same sample, which can be used to apply a
#' fading correction to.
#'
#'
#' @format A \code{\link{list}} with two elements, each containing a further
#' \code{\link{list}} of \code{\link{data.frame}}s containing the data
#' on the fading and equivalent dose measurements:
#'
#' \describe{
#'
#' \code{$fading.data}: A named \code{\link{list}} of \code{\link{data.frame}}s,
#' each having three named columns (\code{LxTx, LxTx.error, timeSinceIrradiation}).\cr
#' \code{..$IR50}: Fading data of the IR50 signal.\cr
#' \code{..$IR100}: Fading data of the IR100 signal.\cr
#' \code{..$IR150}: Fading data of the IR150 signal.\cr
#' \code{..$IR225}: Fading data of the IR225 signal.\cr
#' \cr\cr
#'
#' \code{$equivalentDose.data}: A named of \code{\link{data.frame}}s,
#' each having three named columns (\code{dose, LxTx, LxTx.error}).\cr
#' \code{..$IR50}: Equivalent dose measurement data of the IR50 signal.\cr
#' \code{..$IR100}: Equivalent dose measurement data of the IR100 signal.\cr
#' \code{..$IR150}: Equivalent dose measurement data of the IR150 signal.\cr
#' \code{..$IR225}: Equivalent dose measurement data of the IR225 signal.\cr
#' \cr\cr
#'
#' }
#'
#' @source
#'
#' These data were kindly provided by Georgina King. Detailed information
#' on the sample UNIL/NB123 can be found in the reference given below. The raw
#' data can be found in the accompanying supplementary information.
#'
#' @references
#'
#' King, G.E., Herman, F., Lambert, R., Valla, P.G., Guralnik, B., 2016.
#' Multi-OSL-thermochronometry of feldspar. Quaternary Geochronology 33, 76-87. doi:10.1016/j.quageo.2016.01.004
#'
#' \bold{Details} \cr
#' \tabular{ll}{
#' Lab: \tab University of Lausanne \cr
#' Lab-Code: \tab UNIL/NB123 \cr
#' Location: \tab Namche Barwa (eastern Himalaya)\cr
#' Material: \tab Coarse grained (180-212 microns) potassium feldspar \cr
#' Units: \tab Values are given in seconds \cr
#' Lab Dose Rate: \tab Dose rate of the beta-source at measurement ca. 0.1335 +/-
#' 0.004 Gy/s \cr
#' Environmental Dose Rate: \tab 7.00 +/- 0.92 Gy/ka (includes internal dose rate)
#' }
#'
#'
#' @keywords datasets
#'
#' @examples
#'
#' ## Load example data
#' data("ExampleData.Fading", envir = environment())
#'
#' ## Get fading measurement data of the IR50 signal
#' IR50_fading <- ExampleData.Fading$fading.data$IR50
#' head(IR50_fading)
#'
#' ## Determine g-value and rho' for the IR50 signal
#' IR50_fading.res <- analyse_FadingMeasurement(IR50_fading)
#'
#' ## Show g-value and rho' results
#' gval <- get_RLum(IR50_fading.res)
#' rhop <- get_RLum(IR50_fading.res, "rho_prime")
#'
#' gval
#' rhop
#'
#' ## Get LxTx values of the IR50 DE measurement
#' IR50_De.LxTx <- ExampleData.Fading$equivalentDose.data$IR50
#'
#' ## Calculate the De of the IR50 signal
#' IR50_De <- plot_GrowthCurve(IR50_De.LxTx,
#'                                 mode = "interpolation",
#'                                 fit.method = "EXP")
#'
#' ## Extract the calculated De and its error
#' IR50_De.res <- get_RLum(IR50_De)
#' De <- c(IR50_De.res$De, IR50_De.res$De.Error)
#'
#' ## Apply fading correction (age conversion greatly simplified)
#' IR50_Age <- De / 7.00
#' IR50_Age.corr <- calc_FadingCorr(IR50_Age, g_value = IR50_fading.res)
#'
#'
#' @name ExampleData.Fading
NULL

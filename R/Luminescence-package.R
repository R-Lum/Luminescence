#' Comprehensive Luminescence Dating Data Analysis
#'
#' A collection of various R functions for the purpose of Luminescence dating
#' data analysis. This includes, amongst others, data import, export,
#' application of age models, curve deconvolution, sequence analysis and
#' plotting of equivalent dose distributions.
#'
#' \tabular{ll}{ Package: \tab Luminescence\cr Type: \tab Package\cr Version:
#' \tab 0.4.4\cr Date: \tab 2015-05-19\cr License: \tab GPL-3\cr }
#'
#' @name Luminescence-package
#' @aliases Luminescence-package Luminescence
#' @docType package
#' @author \bold{Authors}
#'
#' \tabular{ll}{ Christoph Burow \tab University of Cologne, Germany \cr
#' Michael Dietze \tab GFZ Helmholtz Centre Potsdam, Germany \cr Manfred
#' Fischer\tab University of Bayreuth, Germany \cr Margret C. Fuchs \tab Alfred
#' Wegener Insitute for Polar and Marine Research, Potsdam, Germany\cr
#' Sebastian Kreutzer \tab IRAMAT-CRP2A, Universite Bordeaux Montaigne, Pessac,
#' France\cr Christoph Schmidt \tab University of Bayreuth, Germany\cr Rachel
#' K. Smedley\tab Aberystwyth University, United Kingdom }
#'
#' \bold{Beta-Tester}
#'
#' Thomas Kolb, University of Bayreuth, Germany\cr
#'
#' \bold{Supervisor}
#'
#' Markus Fuchs, Justus-Liebig-University Giessen, Germany\cr
#'
#' \bold{Support contact}
#'
#' \email{developer@@r-luminescence.de}\cr
#'
#' We may further encourage the usage of our support forum. For this please
#' visit our project website (link below).
#'
#' \bold{Bug reporting}
#'
#' \email{bugtracker@@r-luminescence.de} \cr
#'
#' \bold{Project website}
#'
#' \url{http://www.r-luminescence.de}\cr
#'
#' \bold{Project source code repository}\cr
#' \url{https://github.com/R-Lum/Luminescence}\cr
#'
#' \bold{Related package projects}\cr
#' \url{http://cran.r-project.org/web/packages/RLumShiny}\cr
#' \url{http://shiny.r-luminescence.de}\cr
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
#' "Scientific Networks". Project title: "Lum.Network: Ein
#' Wissenschaftsnetzwerk zur Analyse von Lumineszenzdaten mit R" (2014-2017)
#' @references Dietze, M., Kreutzer, S., Fuchs, M.C., Burow, C., Fischer, M.,
#' Schmidt, C., 2013. A practical guide to the R package Luminescence.  Ancient
#' TL, 31, pp. 11-18.
#'
#' Fuchs, M.C., Kreutzer, S., Burow, C., Dietze, M., Fischer, M., Schmidt, C.,
#' Fuchs, M., 2015. Data processing in luminescence dating analysis: An
#' exemplary workflow using the R package 'Luminescence'. Quaternary
#' International, 362, pp. 8-13. http://dx.doi.org/10.1016/j.quaint.2014.06.034
#'
#' Kreutzer, S., Schmidt, C., Fuchs, M.C., Dietze, M., Fischer, M., Fuchs, M.,
#' 2012. Introducing an R package for luminescence dating analysis. Ancient TL,
#' 30, pp. 1-8.
#' @keywords package
#' @import methods shape data.table bbmle
#' @importFrom parallel parLapply makeCluster stopCluster
#' @importFrom raster brick raster contour plotRGB nlayers
#' @importFrom zoo as.Date as.Date.numeric
#' @importFrom matrixStats rowDiffs
#' @importFrom XML xmlSize xmlValue xmlAttrs xmlRoot xmlTreeParse getEncoding xmlErrorCumulator
#' @importFrom Rcpp evalCpp
#' @importFrom rgl persp3d
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @exportPattern ^[[:alpha:]]+
#' @useDynLib Luminescence
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
#' \code{\link{readSPE2R}} to R to produce an
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
#' function \code{\link{readXSYG2R}}.
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
#' @seealso \code{\link{readXSYG2R}}, \code{\linkS4class{RLum.Analysis}},\cr
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
#' OSLcurve <- get_RLum.Analysis(OSL.SARMeasurement$Sequence.Object,
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



#' Class \code{"Risoe.BINfileData"}
#'
#' S4 class object for luminescence data in R. The object is produced as output
#' of the function \code{\link{readBIN2R}}.
#'
#'
#' @name Risoe.BINfileData-class
#' @aliases Risoe.BINfileData-class show,Risoe.BINfileData-method
#' set_Risoe.BINfileData set_Risoe.BINfileData,Risoe.BINfileData-method
#' set_Risoe.BINfileData,data.frame,list-method
#' set_Risoe.BINfileData,ANY-method get_Risoe.BINfileData
#' get_Risoe.BINfileData-methods get_Risoe.BINfileData,Risoe.BINfileData-method
#' @docType class
#' @note
#'
#' \bold{Internal METADATA - object structure}
#'
#' \tabular{rllll}{
#' \bold{#} \tab \bold{Name} \tab \bold{Data Type} \tab \bold{V} \tab \bold{Description} \cr
#' [,1]  \tab ID  \tab \code{numeric} \tab RLum \tab Unique record ID (same ID as in slot \code{DATA})\cr
#' [,2]  \tab SEL \tab \code{logic} \tab RLum \tab Record selection, not part official BIN-format, triggered by TAG\cr
#' [,3]  \tab VERSION \tab \code{raw} \tab 03-07 \tab BIN-file version number \cr
#' [,4]  \tab LENGTH \tab \code{integer} \tab 03-07 \tab Length of this record\cr
#' [,5]  \tab PREVIOUS \tab \code{integer} \tab 03-07 \tab Length of previous record\cr
#' [,6]  \tab NPOINTS \tab \code{integer} \tab 03-07 \tab Number of data points in the record\cr
#' [,7]  \tab RUN \tab \code{integer} \tab 03-07 \tab Run number\cr
#' [,8]  \tab SET \tab \code{integer} \tab 03-07 \tab Set number\cr
#' [,9]  \tab POSITION \tab  \code{integer} \tab 03-07 \tab Position number\cr
#' [,10] \tab GRAIN \tab \code{integer} \tab 03-04 \tab Grain number\cr
#' [,11] \tab GRAINNUMBER \tab \code{integer} \tab 06-07 \tab Grain number\cr
#' [,12] \tab CURVENO \tab \code{integer} \tab 06-07 \tab Curve number\cr
#' [,13] \tab XCOORD \tab \code{integer} \tab 03-07 \tab X position of a single grain\cr
#' [,14] \tab YCOORD \tab \code{integer} \tab 03-07 \tab Y position of a single grain\cr
#' [,15] \tab SAMPLE \tab \code{factor} \tab 03-07 \tab Sample name\cr
#' [,16] \tab COMMENT \tab \code{factor} \tab 03-07 \tab Comment name\cr
#' [,17] \tab SYSTEMID \tab \code{integer} \tab 03-07 \tab Risoe system id\cr
#' [,18] \tab FNAME \tab \code{factor} \tab 06-07 \tab File name (*.bin/*.binx)\cr
#' [,19] \tab USER \tab \code{facotr} \tab 03-07 \tab User name\cr
#' [,20] \tab TIME \tab \code{character} \tab 03-07 \tab Data collection time (hh-mm-ss)\cr
#' [,21] \tab DATE \tab \code{factor} \tab 03-07 \tab Data collection date (ddmmyy)\cr
#' [,22] \tab DTYPE \tab \code{character} \tab 03-07 \tab Data type\cr
#' [,23] \tab BL_TIME \tab \code{numeric} \tab 03-07 \tab Bleaching time\cr
#' [,24] \tab BL_UNIT \tab \code{integer} \tab 03-07 \tab Bleaching unit (mJ, J, secs, mins, hrs)\cr
#' [,25] \tab NORM1 \tab \code{numeric} \tab 03-07 \tab Normalisation factor (1)\cr
#' [,26] \tab NORM2 \tab \code{numeric} \tab 03-07 \tab Normalisation factor (2)\cr
#' [,27] \tab NORM3 \tab \code{numeric} \tab 03-07 \tab Normalisation factor (3)\cr
#' [,28] \tab BG \tab \code{numeric} \tab 03-07 \tab Background level\cr
#' [,29] \tab SHIFT \tab \code{integer} \tab 03-07 \tab Number of channels to shift data\cr
#' [,30] \tab TAG \tab \code{integer} \tab 03-07 \tab Tag, triggers SEL\cr
#' [,31] \tab LTYPE \tab \code{character} \tab 03-07 \tab Luminescence type\cr
#' [,32] \tab LIGHTSOURCE \tab \code{character} \tab 03-07 \tab Light source\cr
#' [,33] \tab LPOWER \tab \code{numeric} \tab 03-07 \tab Optical stimulation power\cr
#' [,34] \tab LIGHTPOWER \tab \code{numeric} \tab 06-07 \tab Optical stimulation power\cr
#' [,35] \tab LOW \tab \code{numeric} \tab 03-07 \tab Low (temperature, time, wavelength)\cr
#' [,36] \tab HIGH \tab \code{numeric} \tab 03-07 \tab High (temperature, time, wavelength)\cr
#' [,37] \tab RATE \tab \code{numeric} \tab 03-07 \tab Rate (heating rate, scan rate)\cr
#' [,38] \tab TEMPERATURE \tab \code{integer} \tab 03-07 \tab Sample temperature\cr
#' [,39] \tab MEASTEMP \tab \code{integer} \tab 06-07 \tab Measured temperature\cr
#' [,40] \tab AN_TEMP \tab \code{numeric} \tab 03-07 \tab Annealing temperature\cr
#' [,41] \tab AN_TIME \tab \code{numeric} \tab 03-07 \tab Annealing time\cr
#' [,42] \tab TOLDELAY \tab \code{integer} \tab 03-07 \tab TOL 'delay' channels\cr
#' [,43] \tab TOLON \tab \code{integer} \tab 03-07 \tab TOL 'on' channels\cr
#' [,44] \tab TOLOFF \tab \code{integer} \tab 03-07 \tab TOL 'off' channels\cr
#' [,45] \tab IRR_TIME \tab \code{numeric} \tab 03-07 \tab Irradiation time\cr
#' [,46] \tab IRR_TYPE \tab \code{integer} \tab 03-07 \tab Irradiation type (alpha, beta or gamma)\cr
#' [,47] \tab IRR_UNIT \tab \code{integer} \tab 03-04 \tab Irradiation unit (Gy, Rads, secs, mins, hrs)\cr
#' [,48] \tab IRR_DOSERATE \tab \code{numeric} \tab 06-07 \tab Irradiation dose rate (Gy/s)\cr
#' [,49] \tab IRR_DOSERATEERR \tab \code{numeric} \tab 06-07 \tab Irradiation dose rate error (Gy/s)\cr
#' [,50] \tab TIMESINCEIRR \tab \code{integer} \tab 06-07 \tab Time since irradiation (s)\cr
#' [,51] \tab TIMETICK \tab \code{numeric} \tab 06-07 \tab Time tick for pulsing (s)\cr
#' [,52] \tab ONTIME \tab \code{integer} \tab 06-07 \tab On-time for pulsing (in time ticks)\cr
#' [,53] \tab STIMPERIOD \tab \code{integer} \tab 06-07 \tab Stimulation period (on+off in time ticks)\cr
#' [,54] \tab GATE_ENABLED \tab \code{raw} \tab 06-07 \tab PMT signal gating enabled\cr
#' [,55] \tab ENABLE_FLAGS \tab \code{raw} \tab 06-07 \tab PMT signal gating  enabled\cr
#' [,56] \tab GATE_START \tab \code{integer} \tab 06-07 \tab Start gating (in time ticks)\cr
#' [,57] \tab GATE_STOP \tab \code{ingeter} \tab 06-07 \tab Stop gating (in time ticks), 'Gateend' for version 04, here only GATE_STOP is used\cr
#' [,58] \tab PTENABLED \tab \code{raw} \tab 06-07 \tab Photon time enabled\cr
#' [,59] \tab DTENABLED \tab \code{raw} \tab 06-07 \tab PMT dead time correction enabled\cr
#' [,60] \tab DEADTIME \tab \code{numeric} \tab 06-07 \tab PMT dead time (s)\cr
#' [,61] \tab MAXLPOWER \tab \code{numeric} \tab 06-07 \tab Stimulation power to 100 percent (mW/cm^2)\cr
#' [,62] \tab XRF_ACQTIME \tab \code{numeric} \tab 06-07 \tab XRF acquisition time (s)\cr
#' [,63] \tab XRF_HV \tab \code{numeric} \tab 06-07 \tab XRF X-ray high voltage (V)\cr
#' [,64] \tab XRF_CURR \tab \code{integer} \tab 06-07 \tab XRF X-ray current (uA)\cr
#' [,65] \tab XRF_DEADTIMEF \tab \code{numeric} \tab 06-07 \tab XRF dead time fraction\cr
#' [,66] \tab SEQUENCE \tab \code{character} \tab 03-04 \tab Sequence name\cr
#' [,67] \tab DETECTOR_ID \tab \code{raw} \tab 07 \tab Detector ID\cr
#' [,68] \tab LOWERFILTER_ID \tab \code{integer} \tab 07 \tab Lower filter ID in reader\cr
#' [,69] \tab UPPERFILTER_ID \tab \code{integer} \tab 07 \tab Uper filter ID in reader\cr
#' [,70] \tab ENOISEFACTOR \tab \code{numeric} \tab 07 \tab Excess noise filter, usage unknown
#'
#' } V = BIN-file version (RLum means that it does not depend on a specific BIN
#' version)\cr
#'
#' Note that the \code{Risoe.BINfileData} object combines all values from
#' different versions from the BIN-file, reserved bits are skipped, however,
#' the function \code{\link{writeR2BIN}} reset arbitrary reserved bits. Invalid
#' values for a specific version are set to \code{NA}. Furthermore, the
#' internal R data types do not necessarily match the required data types for
#' the BIN-file data import! Data types are converted during data import.\cr
#'
#' \bold{LTYPE} values
#'
#' \tabular{rll}{ [,0] \tab TL \tab: Thermoluminescence \cr [,1] \tab OSL \tab:
#' Optically stimulated luminescence \cr [,2] \tab IRSL \tab: Infrared
#' stimulated luminescence \cr [,3] \tab M-IR \tab: Infrared monochromator
#' scan\cr [,4] \tab M-VIS \tab: Visible monochromator scan\cr [,5] \tab TOL
#' \tab: Thermo-optical luminescence \cr [,6] \tab TRPOSL \tab: Time Resolved
#' Pulsed OSL\cr [,7] \tab RIR \tab: Ramped IRSL\cr [,8] \tab RBR \tab: Ramped
#' (Blue) LEDs\cr [,9] \tab USER \tab: User defined\cr [,10] \tab POSL \tab:
#' Pulsed OSL \cr [,11] \tab SGOSL \tab: Single Grain OSL\cr [,12] \tab RL
#' \tab: Radio Luminescence \cr [,13] \tab XRF \tab: X-ray Fluorescence }
#'
#' \bold{DTYPE} values \tabular{rll}{ [,0] \tab 0 \tab Natural \cr [,1] \tab 1
#' \tab N+dose \cr [,2] \tab 2 \tab Bleach \cr [,3] \tab 3 \tab Bleach+dose \cr
#' [,4] \tab 4 \tab Natural (Bleach) \cr [,5] \tab 5 \tab N+dose (Bleach) \cr
#' [,6] \tab 6 \tab Dose \cr [,7] \tab 7 \tab Background }
#'
#' \bold{LIGHTSOURCE} values \tabular{rll}{ [,0] \tab 0 \tab Non \cr [,1] \tab
#' 1 \tab Lamp \cr [,2] \tab 2 \tab IR diodes/IR Laser \cr [,3] \tab 3 \tab
#' Calibration LED \cr [,4] \tab 4 \tab Blue Diodes \cr [,5] \tab 5 \tab White
#' lite \cr [,6] \tab 6 \tab Green laser (single grain) \cr [,7] \tab 7 \tab IR
#' laser (single grain) }
#'
#' (information on the BIN/BINX file format are kindly provided by Risoe, DTU
#' Nutech)
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("Risoe.BINfileData", ...)}.
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#' @seealso
#' \code{\link{plot_Risoe.BINfileData}}, \code{\link{readBIN2R}},
#' \code{\link{writeR2BIN}},\code{\link{merge_Risoe.BINfileData}},
#' \code{\link{Risoe.BINfileData2RLum.Analysis}},
#' \code{\link{Risoe.BINfileData2RLum.Data.Curve}}
#' @references Risoe DTU, 2013. The Sequence Editor User Manual - Feb 2013 and Risoe DTU, 2015. The
#' Sequence Editor User Manual - March 2015
#'
#' \code{http://www.nutech.dtu.dk/}
#' @keywords classes
#' @examples
#'
#' showClass("Risoe.BINfileData")
#'
NULL



#' Class \code{"RLum"}
#'
#' Abstract class for data in the package Luminescence
#'
#'
#' @name RLum-class
#' @docType class
#' @note \code{RLum} is a virtual class.
#' @section Objects from the Class: A virtual Class: No objects can be created
#' from it.
#' @author Sebastian Kreutzer, 2013 (Freiberg Instruments/JLU Giessen, Germany)
#' @seealso \code{\linkS4class{RLum.Data}}, \code{\linkS4class{RLum.Analysis}}
#' @references #
#' @keywords classes
#' @examples
#'
#' showClass("RLum")
#'
NULL





#' Class \code{"RLum.Analysis"}
#'
#' Object class containing analysis data for protocol analysis.
#'
#'
#' @name RLum.Analysis-class
#' @aliases RLum.Analysis-class show,RLum.Analysis-method set_RLum.Analysis
#' set_RLum.Analysis,RLum.Analysis-method set_RLum.Analysis,list-method
#' get_RLum.Analysis get_RLum.Analysis-methods
#' get_RLum.Analysis,RLum.Analysis-method get_structure.RLum.Analysis
#' get_structure.RLum.Analysis,RLum.Analysis-method length_RLum.Analysis
#' length_RLum.Analysis-methods length_RLum.Analysis,RLum.Analysis-method
#' @docType class
#' @note The method \code{get_structure.RLum.Analysis} is currently just
#' avaiblable for objects containing \code{\linkS4class{RLum.Data.Curve}}.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RLum.Analysis", ...)}.
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#' @seealso \code{\link{Risoe.BINfileData2RLum.Analysis}},
#' \code{\linkS4class{Risoe.BINfileData}}, \code{\linkS4class{RLum}}
#' @references #
#' @keywords classes
#' @examples
#'
#' showClass("RLum.Analysis")
#'
#' ## usage of get_RLum.Analysis() with returining an RLum.Analysis object
#' #  get_RLum.Analysis(object, keep.object = TRUE)
#'
#'
NULL





#' Class \code{"RLum.Data"}
#'
#' Generalized virtual data class for luminescence data.
#'
#'
#' @name RLum.Data-class
#' @docType class
#' @note Just a virtual class.
#' @section Objects from the Class: A virtual Class: No objects can be created
#' from it.
#' @author Sebastian Kreutzer, 2013 (Freiberg Instruments/JLU Giessen, Germany)
#' @seealso \code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data.Curve}},
#' \code{\linkS4class{RLum.Data.Spectrum}}
#' @references #
#' @keywords classes
#' @examples
#'
#' showClass("RLum.Data")
#'
NULL





#' Class \code{"RLum.Data.Curve"}
#'
#' Class for luminescence curve data.
#'
#'
#' @name RLum.Data.Curve-class
#' @aliases RLum.Data.Curve-class coerce,RLum.Analysis-method
#' show,RLum.Data.Curve-method set_RLum.Data.Curve set_RLum.Data.Curve-methods
#' set_RLum.Data.Curve,RLum.Data.Curve-method set_RLum.Data.Curve,ANY-method
#' set_RLum.Data.Curve,character,matrix-method get_RLum.Data.Curve
#' get_RLum.Data.Curve-methods get_RLum.Data.Curve,ANY-method
#' @docType class
#' @note The class should only contain data for a single curve. For additional
#' elements the slot \code{info} can be used (e.g. providing additional heating
#' ramp curve).
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RLum.Data.Curve", ...)}.
#' @author Sebastian Kreutzer Freiberg Instruments/JLU Giessen (Germany)
#' @seealso \code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data}},
#' \code{\link{plot_RLum}}
#' @references #
#' @keywords classes
#' @examples
#'
#' showClass("RLum.Data.Curve")
#'
NULL





#' Class \code{"RLum.Data.Image"}
#'
#' Class for luminescence image data (TL/OSL/RF).
#'
#'
#' @name RLum.Data.Image-class
#' @aliases RLum.Data.Image-class coerce,RLum.Data.Image-method
#' show,RLum.Data.Image-method set_RLum.Data.Image set_RLum.Data.Image-methods
#' set_RLum.Data.Image,RLum.Data.Image-method set_RLum.Data.Image,ANY-method
#' set_RLum.Data.Image,character,matrix-method get_RLum.Data.Image
#' get_RLum.Data.Image-methods get_RLum.Data.Image,ANY-method
#' @docType class
#' @note The class should only contain data for a set of images. For additional
#' elements the slot \code{info} can be used.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RLum.Data.Image", ...)}.
#' @author Sebastian Kreutzer, Universite Bordeaux Montaigne (France)
#' @seealso \code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data}},
#' \code{\link{plot_RLum}}
#' @references #
#' @keywords classes
#' @examples
#'
#' showClass("RLum.Data.Image")
#'
#' ##so far no further example available
#'
#'
#'
NULL





#' Class \code{"RLum.Data.Spectrum"}
#'
#' Class for luminescence spectra data (TL/OSL/RF).
#'
#'
#' @name RLum.Data.Spectrum-class
#' @aliases RLum.Data.Spectrum-class coerce,RLum.Data.Spectrum-method
#' show,RLum.Data.Spectrum-method set_RLum.Data.Spectrum
#' set_RLum.Data.Spectrum-methods
#' set_RLum.Data.Spectrum,RLum.Data.Spectrum-method
#' set_RLum.Data.Spectrum,ANY-method
#' set_RLum.Data.Spectrum,character,matrix-method get_RLum.Data.Spectrum
#' get_RLum.Data.Spectrum-methods get_RLum.Data.Spectrum,ANY-method
#' @docType class
#' @note The class should only contain data for a single spectra data set. For
#' additional elements the slot \code{info} can be used.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RLum.Data.Spectrum", ...)}.
#' @author Sebastian Kreutzer, JLU Giessen (Germany)
#' @seealso \code{\linkS4class{RLum}}, \code{\linkS4class{RLum.Data}},
#' \code{\link{plot_RLum}}
#' @references #
#' @keywords classes
#' @examples
#'
#' showClass("RLum.Data.Spectrum")
#'
#' ##show example data (uncomment for usage)
#' # data(ExampleData.XSYG, envir = environment())
#' # TL.Spectrum
#'
#'
#'
NULL





#' Class \code{"RLum.Results"}
#'
#' Object class contains results data from functions.
#'
#'
#' @name RLum.Results-class
#' @aliases RLum.Results-class show,RLum.Results-method set_RLum.Results
#' set_RLum.Results,RLum.Results-method set_RLum.Results,ANY,list-method
#' get_RLum.Results get_RLum.Results,RLum.Results-method merge_RLum.Results
#' merge_RLum.Results-methods merge_RLum.Results,list-method
#' validObject,RLum.Results-method
#' @docType class
#' @note The class is intended to store results from functions to be used by
#' other functions. The data in the object should always be accessed by the
#' method \code{get_RLum.Results}.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RLum.Results", ...)}.
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
#' (France)
#' @seealso \code{\linkS4class{RLum}}
#' @references #
#' @keywords classes methods
#' @examples
#'
#' showClass("RLum.Results")
#'
NULL

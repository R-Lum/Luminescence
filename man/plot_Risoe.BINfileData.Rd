\name{plot_Risoe.BINfileData}
\alias{plot_Risoe.BINfileData}
\title{Plot single luminescence curves from a BIN file object}
\description{Plots single luminescence curves from an object returned by the \link{readBIN2R} function.}
\usage{plot_Risoe.BINfileData(BINfileData, position, run, set, sorter = "POSITION", 
    ltype = c("IRSL", "OSL", "TL", "RIR", "RBR", "RL"), curve.transformation, 
    dose_rate, temp.lab, cex.global = 1, ...)}
\arguments{
  \item{BINfileData}{\link{Risoe.BINfileData-class} (\bold{required}): 
requires an S4 object returned by the \link{readBIN2R} function.}
  \item{position}{\link{vector} (optional): option to limit the plotted curves by 
position (e.g. \code{position = 1}, \code{position = c(1,3,5)}).}
  \item{run}{\link{vector} (optional): option to limit the plotted curves by run 
(e.g., \code{run = 1}, \code{run = c(1,3,5)}).}
  \item{set}{\link{vector} (optional): option to limit the plotted curves by set 
(e.g., \code{set = 1}, \code{set = c(1,3,5)}).}
  \item{sorter}{\link{character} (with default): the plot output can be ordered by 
"POSITION","SET" or "RUN". POSITION, SET and RUN are options defined in 
the Risoe Sequence Editor.}
  \item{ltype}{\link{character} (with default): option to limit the plotted curves by 
the type of luminescence stimulation. 
Allowed values: \code{"IRSL"}, \code{"OSL"},\code{"TL"}, \code{"RIR"},
\code{"RBR"} (corresponds to LM-OSL), \code{"RL"}. 
All type of curves are plotted by default.}
  \item{curve.transformation}{\link{character} (optional): allows transforming CW-OSL and CW-IRSL curves
to pseudo-LM curves via transformation functions. 
Allowed values are: \code{CW2pLM}, \code{CW2pLMi}, \code{CW2pHMi} and 
\code{CW2pPMi}. See details.}
  \item{dose_rate}{\link{numeric} (optional): dose rate of the irradition source at the 
measurement date. If set, the given irradiation dose will be shown in Gy. 
See details.}
  \item{temp.lab}{\link{character} (optional): option to allow for different temperature units.
If no value is set deg. C is chosen.}
  \item{cex.global}{\link{numeric} (with default): global scaling factor.}
  \item{\dots}{further undocumented plot arguments.}
}
\details{\bold{Nomenclature}\cr

The nomenclature used for the function (e.g., ltype, position) are taken from 
the Analyst manual (Duller, 2007, p. 42): \cr

\tabular{rlll}{
[,1] \tab ID \tab: Unique record ID (same ID as in slot \code{DATA}) \tab \code{numeric} \cr
[,2] \tab SEL \tab: Record selection \tab \code{logical} \cr
[,3] \tab VERSION \tab: Data format version number \tab \code{raw} \cr
[,4] \tab LENGTH  \tab: Length of this record \tab \code{integer} \cr
[,5] \tab PREVIOUS \tab: Length of previous record \tab \code{integer} \cr
[,6] \tab NPOINTS \tab: Number of data points in the record \tab \code{integer} \cr
[,7] \tab LTYPE \tab: Luminescence type \tab \code{factor} \cr
[,8] \tab LOW \tab: Low (temperature, time, wavelength) \tab \code{numeric} \cr
[,10] \tab HIGH \tab: High (temperature, time, wavelength) \tab \code{numeric} \cr
[,11] \tab RATE \tab: Rate (heating rate, scan rate) \tab \code{numeric} \cr
[,12] \tab TEMPERATURE \tab: Sample temperature \tab \code{integer} \cr
[,13] \tab XCOORD \tab: X position of a single grain \tab \code{integer} \cr
[,14] \tab YCOORD \tab: Y position of a single grain \tab \code{integer} \cr
[,15] \tab TOLDELAY \tab: TOL 'delay' channels \tab \code{integer} \cr
[,16] \tab TOLON  \tab: TOL 'on' channels \tab \code{integer} \cr
[,17] \tab TOLOFF  \tab: TOL 'off' channels \tab \code{integer} \cr
[,18] \tab POSITION \tab: Carousel position \tab \code{integer} \cr
[,19] \tab RUN \tab: Run number \tab \code{integer} \cr
[,20] \tab TIME \tab: Data collection time (hh-mm-ss) \tab \code{factor} \cr
[,21] \tab DATA \tab: Data collection date (dd-mm-yy) \tab \code{factor} \cr
[,22] \tab SEQUENCE  \tab: Sequence name \tab \code{factor} \cr
[,23] \tab USER  \tab: User name \tab \code{factor} \cr
[,24] \tab DTYPE  \tab: Data type \tab \code{factor} \cr
[,25] \tab IRR_TIME \tab: Irradiation time \tab \code{numeric} \cr
[,26] \tab IRR_TYPE  \tab: Irradiation type (alpha, beta or gamma) \tab \code{integer} \cr
[,27] \tab IRR_UNIT  \tab: Irradiation unit (Gy, Rads, secs, mins, hrs) \tab \code{integer} \cr
[,28] \tab BL_TIME  \tab: Bleaching time \tab \code{numeric} \cr
[,29] \tab BL_UNIT  \tab: Bleaching unit (mJ, J, secs, mins, hrs) \tab \code{integer} \cr
[,30] \tab AN_TEMP  \tab: Annealing temperature \tab \code{numeric} \cr
[,31] \tab AN_TIME  \tab: Annealing time \tab \code{numeric} \cr
[,32] \tab NORM1  \tab: Normalisation factor (1) \tab \code{numeric} \cr
[,33] \tab NORM2  \tab: Normalisation factor (2) \tab \code{numeric} \cr
[,34] \tab NORM3  \tab: Normalisation factor (3) \tab \code{numeric} \cr
[,35] \tab BG \tab: Background level \tab \code{numeric} \cr
[,36] \tab SHIFT \tab: Number of channels to shift data \tab \code{integer} \cr
[,37] \tab SAMPLE \tab: Sample name \tab \code{factor} \cr
[,38] \tab COMMENT \tab: Comment \tab \code{factor} \cr
[,39] \tab LIGHTSOURCE \tab: Light source \tab \code{factor} \cr
[,40] \tab SET \tab: Set Number \tab \code{integer} \cr
[,41] \tab TAG \tab: Tag \tab \code{integer} \cr
[,42] \tab GRAIN \tab: Grain number \tab \code{integer} \cr
[,43] \tab LPOWER \tab: Optical Stimulation Power \tab \code{numeric} \cr
[,44] \tab SYSTEMID \tab: System ID \tab \code{integer} 
}

\bold{curve.transformation}\cr

This argument allows transforming continuous wave (CW) curves to pseudo (linear)
modulated curves. For the transformation, the functions of the package are used. 
Currently, it is not possible to pass further arguments to the transformation functions.
The argument works only for \code{ltype} \code{OSL} and \code{IRSL}.\cr

\bold{Irradiation time}\cr

Plotting the irradiation time (s) or the given dose (Gy) requires that the 
variable \code{IRR_TIME} has been set within the BIN-file. 
This is normally done by using the 'Run Info' option within the Sequence Editor or by 
editing in R.}
\value{Returns a plot.  }
\references{Duller, G., 2007. Analyst. pp. 1-45. }
\author{Sebastian Kreutzer, JLU Giessen (Germany), 
Michael Dietze, GFZ Potsdam (Germany)\cr
R Luminescence Package Team}
\note{The function has been successfully tested for the Sequence Editor file output 
version 3 and 4.   }


\seealso{\code{\link{readBIN2R}}, \code{\link{CW2pLM}}, \code{\link{CW2pLMi}}, 
\code{\link{CW2pPMi}}, \code{\link{CW2pHMi}}}
\examples{

##load data
data(ExampleData.BINfileData, envir = environment())

##plot all curves from the first position to the desktop
#pdf(file = "~/Desktop/CurveOutput.pdf", paper = "a4", height = 11, onefile = TRUE)

##example - load from *.bin file
#BINfile<-"[your path]"
#BINfileData<-readBIN2R(BINfile)

#par(mfrow = c(4,3), oma = c(0.5,1,0.5,1))
#plot_Risoe.BINfileData(CWOSL.SAR.Data,position = 1)
#mtext(side = 4, BINfile, outer = TRUE, col = "blue", cex = .7)
#dev.off()

}

\keyword{dplot}
\section{Function version}{0.4.1 (2014-09-15 02:28:00)}
\section{Function version}{0.4.1 (2014-09-15 02:28:00)}

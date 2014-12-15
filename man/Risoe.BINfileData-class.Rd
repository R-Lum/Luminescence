\name{Risoe.BINfileData-class}
\Rdversion{1.1}
\docType{class}
\alias{Risoe.BINfileData-class}
\alias{show,Risoe.BINfileData-method}
\alias{set_Risoe.BINfileData}
\alias{set_Risoe.BINfileData,Risoe.BINfileData-method}
\alias{set_Risoe.BINfileData,data.frame,list-method}
\alias{get_Risoe.BINfileData}
\alias{get_Risoe.BINfileData-methods}
\alias{get_Risoe.BINfileData,Risoe.BINfileData-method}

\title{Class \code{"Risoe.BINfileData"}}
\description{
S4 class object for luminescence data in R. The object is produced as output of the 
function \code{\link{readBIN2R}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Risoe.BINfileData", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{METADATA}:}{Object of class \code{"data.frame"} containing the meta information for each curve.}
    \item{\code{DATA}:}{Object of class \code{"list"} containing numeric vector with count data }
    \item{\code{.S3Class}:}{Object of class \code{"character"}}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Risoe.BINfileData")}: ... }
	 
    \item{set_Risoe.BINfileData}{\code{signature(METADATA = "data.frame", DATA = "list")}: 
    
    The \code{\linkS4class{Risoe.BINfileData}} is normally produced as output of 
    the function \code{\link{readBIN2R}}. This construction method is intended for internal usage
    only.
   } 
   
   \item{get_Risoe.BINfileData}{\code{signature(object = "Risoe.BINfileData")}: 
    
    Formal get-method for \code{\linkS4class{Risoe.BINfileData}} object. It does not
    allow accessing the object directly, it is just showing a terminal message.  
   } 
   

  }
}
  

\references{
%%  ~~put references to the literature/web site here~~
Risoe DTU, 2013. The Sequence Editor User Manual - Feb 2013

\code{http://www.nutech.dtu.dk/}

}
\author{Sebastian Kreutzer, JLU Giessen (Germany)
}

\section{Version}{0.3 (2013-06-02)}

\note{

\bold{Internal METADATA - object structure}

\tabular{rllll}{
\bold{#} \tab \bold{Name} \tab \bold{Data Type} \tab \bold{V} \tab \bold{Description} \cr
[,1]     \tab  ID             \tab  \code{numeric}  \tab RLum \tab Unique record ID (same ID as in slot \code{DATA})\cr
[,2]     \tab SEL             \tab  \code{logic}    \tab RLum \tab Record selection, not part  official BIN-format, triggered by TAG\cr
[,3]     \tab VERSION         \tab  \code{raw}      \tab 03-06 \tab BIN-file version number \cr
[,4]     \tab LENGTH          \tab  \code{integer}  \tab 03-06 \tab Length of this record\cr
[,5]     \tab PREVIOUS        \tab  \code{integer}  \tab 03-06 \tab Length of previous record\cr
[,6]     \tab NPOINTS         \tab  \code{integer}  \tab 03-06 \tab Number of data points in the record\cr
[,7]     \tab RUN             \tab  \code{integer}  \tab 03-06 \tab Run number\cr
[,8]     \tab SET             \tab  \code{integer}  \tab 03-06 \tab Set number\cr
[,9]     \tab POSITION        \tab  \code{integer}  \tab 03-06 \tab Position number\cr
[,10]    \tab GRAIN           \tab  \code{integer}  \tab 03-04 \tab Grain number\cr
[,11]    \tab GRAINNUMBER     \tab  \code{integer}  \tab 06 \tab Grain number\cr
[,12]    \tab CURVENO         \tab  \code{integer}  \tab 06 \tab Curve number\cr
[,13]    \tab XCOORD          \tab  \code{integer}  \tab 03-06 \tab X position of a single grain\cr 
[,14]    \tab YCOORD          \tab  \code{integer}  \tab 03-06 \tab Y position of a single grain\cr 
[,15]    \tab SAMPLE          \tab  \code{factor}   \tab 03-06 \tab Sample name\cr
[,16]    \tab COMMENT         \tab  \code{factor}   \tab 03-06 \tab Comment name\cr 
[,17]    \tab SYSTEMID        \tab  \code{integer}  \tab 03-06 \tab Risoe system id\cr
[,18]    \tab FNAME           \tab  \code{factor}   \tab 06    \tab File name (*.bin/*.binx)\cr
[,19]    \tab USER            \tab  \code{facotr}   \tab 03-06 \tab User name\cr
[,20]    \tab TIME            \tab  \code{character} \tab 03-06 \tab Data collection time (hh-mm-ss)\cr
[,21]    \tab DATE            \tab  \code{factor}    \tab 03-06 \tab Data collection date (ddmmyy)\cr
[,22]    \tab DTYPE           \tab  \code{character} \tab 03-06 \tab Data type\cr
[,23]    \tab BL_TIME         \tab  \code{numeric}  \tab 03-06 \tab Bleaching time\cr
[,24]    \tab BL_UNIT         \tab  \code{integer}  \tab 03-06 \tab Bleaching unit (mJ, J, secs, mins, hrs)\cr
[,25]    \tab NORM1           \tab  \code{numeric}  \tab 03-06 \tab Normalisation factor (1)\cr 
[,26]    \tab NORM2           \tab  \code{numeric}  \tab 03-06 \tab Normalisation factor (2)\cr 
[,27]    \tab NORM3           \tab  \code{numeric}  \tab 03-06 \tab Normalisation factor (3)\cr 
[,28]    \tab BG              \tab  \code{numeric}  \tab 03-06 \tab Background level\cr 
[,29]    \tab SHIFT           \tab  \code{integer}  \tab 03-06 \tab Number of channels to shift data\cr
[,30]    \tab TAG             \tab  \code{integer}  \tab 03-06 \tab Tag, triggers SEL\cr
[,31]    \tab LTYPE           \tab  \code{character} \tab 03-06 \tab Luminescence type\cr
[,32]    \tab LIGHTSOURCE     \tab  \code{character} \tab 03-06 \tab Light source\cr
[,33]    \tab LPOWER          \tab  \code{numeric}  \tab 03-06 \tab Optical stimulation power\cr
[,34]    \tab LIGHTPOWER      \tab  \code{numeric}  \tab 06    \tab Optical stimulation power\cr
[,35]    \tab LOW             \tab  \code{numeric}  \tab 03-06 \tab Low (temperature, time, wavelength)\cr
[,36]    \tab HIGH            \tab  \code{numeric}  \tab 03-06 \tab High (temperature, time, wavelength)\cr
[,37]    \tab RATE            \tab  \code{numeric}  \tab 03-06 \tab Rate (heating rate, scan rate)\cr
[,38]    \tab TEMPERATURE     \tab  \code{integer}  \tab 03-06 \tab Sample temperature\cr 
[,39]    \tab MEASTEMP        \tab  \code{integer} \tab 06    \tab Measured temperature\cr
[,40]    \tab AN_TEMP         \tab  \code{numeric}  \tab 03-06 \tab Annealing temperature\cr
[,41]    \tab AN_TIME         \tab  \code{numeric}  \tab 03-06 \tab Annealing time\cr
[,42]    \tab TOLDELAY        \tab  \code{integer}  \tab 03-06 \tab TOL 'delay' channels\cr
[,43]    \tab TOLON           \tab  \code{integer}  \tab 03-06 \tab TOL 'on' channels\cr
[,44]    \tab TOLOFF          \tab  \code{integer}  \tab 03-06 \tab TOL 'off' channels\cr
[,45]    \tab IRR_TIME        \tab  \code{numeric}  \tab 03-06 \tab Irradiation time\cr
[,46]    \tab IRR_TYPE        \tab  \code{integer}  \tab 03-06 \tab Irradiation type (alpha, beta or gamma)\cr
[,47]    \tab IRR_UNIT        \tab  \code{integer}  \tab 03-04 \tab Irradiation unit (Gy, Rads, secs, mins, hrs)\cr
[,48]    \tab IRR_DOSERATE    \tab  \code{numeric}  \tab 06   \tab Irradiation dose rate (Gy/s)\cr
[,49]    \tab IRR_DOSERATEERR \tab  \code{numeric}  \tab 06   \tab Irradiation dose rate error (Gy/s)\cr 
[,50]    \tab TIMESINCEIRR    \tab  \code{integer}  \tab 06   \tab Time since irradiation (s)\cr
[,51]    \tab TIMETICK        \tab  \code{numeric}  \tab 06   \tab Time tick for pulsing (s)\cr
[,52]    \tab ONTIME          \tab  \code{integer}  \tab 06   \tab On-time for pulsing (in time ticks)\cr
[,53]    \tab STIMPERIOD      \tab  \code{integer}  \tab 06   \tab Stimulation period (on+off in time ticks)\cr
[,54]    \tab GATE_ENABLED    \tab  \code{raw}      \tab 06   \tab PMT signal gating enabled\cr
[,55]    \tab ENABLE_FLAGS    \tab  \code{raw}      \tab 03   \tab PMT signal gating enabled\cr
[,56]    \tab GATE_START      \tab  \code{integer}  \tab 06   \tab Start gating (in time ticks)\cr
[,57]    \tab GATE_STOP       \tab  \code{ingeter}  \tab 06   \tab Stop gating (in time ticks), 'Gateend' for version 04, here only GATE_STOP is used\cr
[,58]    \tab PTENABLED       \tab  \code{raw}      \tab 06   \tab Photon time enabled\cr
[,59]    \tab DTENABLED       \tab  \code{raw}      \tab 06   \tab PMT dead time correction enabled\cr
[,60]    \tab DEADTIME        \tab  \code{numeric}  \tab 06   \tab PMT dead time (s)\cr
[,61]    \tab MAXLPOWER       \tab  \code{numeric}  \tab 06   \tab Stimulation power to 100 percent (mW/cm^2)\cr
[,62]    \tab XRF_ACQTIME     \tab  \code{numeric}  \tab 06   \tab XRF acquisition time (s)\cr
[,63]    \tab XRF_HV          \tab  \code{numeric}  \tab 06   \tab XRF X-ray high voltage (V)\cr
[,64]    \tab XRF_CURR        \tab  \code{integer}  \tab 06    \tab XRF X-ray current (uA)\cr
[,65]    \tab XRF_DEADTIMEF   \tab  \code{numeric}  \tab 06   \tab XRF dead time fraction\cr
[,66]    \tab SEQUENCE        \tab  \code{character}  \tab 03-04 \tab Sequence name\cr
}
V = BIN-file version (RLum means that it does not depend on a specific 
BIN version)\cr

Note that the \code{Risoe.BINfileData} object combines all values from 
different versions from the BIN-file, reserved bits are skipped, however, the function \code{\link{writeR2BIN}} reset arbitrary reserved bits. Invalid values for a specific version 
are set to \code{NA}. Furthermore, the internal R data types do not necessarily 
match the required data types for the BIN-file data import! Data types are
converted during data import.\cr

\bold{LTYPES}

\tabular{rll}{
[,0] \tab TL \tab: Thermoluminescence \cr
[,1] \tab OSL \tab: Optically stimulated luminescence \cr
[,2] \tab IRSL \tab: Infrared stimulated luminescence \cr
[,3] \tab M-IR \tab: Infrared monochromator scan\cr
[,4] \tab M-VIS \tab: Visible monochromator scan\cr
[,5] \tab TOL \tab: Thermo-optical luminescence \cr
[,6] \tab TRPOSL \tab: Time Resolved Pulsed OSL\cr
[,7] \tab RIR \tab: Ramped IRSL\cr
[,8] \tab RBR \tab: Ramped (Blue) LEDs\cr
[,9] \tab USER \tab: User defined\cr
[,10] \tab POSL \tab: Pulsed OSL \cr
[,11] \tab SGOSL \tab: Single Grain OSL\cr
[,12] \tab RL \tab: Radio Luminescence \cr
[,13] \tab XRF \tab: X-ray Fluorescence
}

\bold{LTYPES}
\tabular{rll}{
[,0] \tab 0 \tab Natural \cr
[,1] \tab 1 \tab N+dose \cr
[,2] \tab 2 \tab Bleach \cr
[,3] \tab 3 \tab Bleach+dose \cr
[,4] \tab 4 \tab Natural (Bleach) \cr
[,5] \tab 5 \tab N+dose (Bleach) \cr
[,6] \tab 6 \tab Dose \cr
[,7] \tab 7 \tab Background
}


(information on the \code{LTYPE} and \code{DTYPE} kindly provided by Risoe, DTU Nutech)


}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
\code{\link{plot_Risoe.BINfileData}}, \code{\link{readBIN2R}}, \code{\link{writeR2BIN}},\code{\link{merge_Risoe.BINfileData}}, \code{\link{Risoe.BINfileData2RLum.Analysis}}, \code{\link{Risoe.BINfileData2RLum.Data.Curve}}
}
\examples{
showClass("Risoe.BINfileData")
}
\keyword{classes}

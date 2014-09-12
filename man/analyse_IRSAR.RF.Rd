\name{analyse_IRSAR.RF}
\alias{analyse_IRSAR.RF}
\title{Analyse IRSAR RF measurements}
\description{Function to analyse IRSAR RF measurements on K-feldspar samples, performed 
using the protocol according to Erfurt et al. (2003)}
\usage{analyse_IRSAR.RF(object, sequence.structure = c("NATURAL", "REGENERATED"), 
    fit.range.min, fit.range.max, fit.trace = FALSE, fit.MC.runs = 10, 
    output.plot = TRUE, xlab.unit = "s", legend.pos = "bottom", 
    ...)}
\arguments{
  \item{object}{\code{\linkS4class{RLum.Analysis}} (\bold{required}): 
input object containing data for protocol analysis}
  \item{sequence.structure}{\link{vector} \link{character} (with default): specifies the general 
sequence structure. Allowed steps are \code{NATURAL}, \code{REGENERATED}
In addition any other character is allowed in the sequence structure; 
such curves will be ignored. }
  \item{fit.range.min}{\link{integer} (optional): set the minimum channel range for signal fitting.   
Usually the entire data set is used for curve fitting, but there might be 
reasons to limit the channels used for fitting.
Note: This option also limits the values used for natural signal calculation.}
  \item{fit.range.max}{\link{integer} (optional): set maximum channel range for signal fitting. 
Usually the entire data set is used for curve fitting, but there might be 
reasons to limit the channels used for fitting.}
  \item{fit.trace}{\link{logical} (with default): trace fitting (for debugging use)}
  \item{fit.MC.runs}{\link{numeric} (with default): set number of Monte Carlo runs for start 
parameter estimation. Note: Higher values will significantly increase 
the calculation time   }
  \item{output.plot}{\link{logical} (with default): plot output (\code{TRUE} or \code{FALSE})}
  \item{xlab.unit}{\link{character} (with default): set unit for x-axis}
  \item{legend.pos}{\link{character} (with default): useful keywords are \code{bottomright}, 
\code{bottom}, \code{bottomleft}, \code{left}, \code{topleft}, 
\code{top}, \code{topright}, \code{right} and \code{center}. 
For further details see \code{\link{legend}.}}
  \item{\dots}{further arguments that will be passed to the plot output. 
Currently supported arguments are \code{main}, \code{xlab}, \code{ylab}}
}
\details{The function performs an IRSAR analysis described for feldspar samples by 
Erfurt et al. (2003) assuming a negligible sensitivity change of the RF signal.\cr
\bold{General Sequence Structure} (according to Erfurt et al. (2003))
\enumerate{
\item Measuring IR-RF intensity of the natural dose for a few seconds (\eqn{D_{natural}})     
\item Bleach the samples under solar conditions for at least 30 min without changing 
the geometry 
\item Waiting for at least one hour 
\item Regeneration of the IR-RF signal to at least the natural level
\item Fitting data with a stretched exponential function
\item Calculate the the palaedose \eqn{D} using the parameters from the fitting
}
\bold{Function Used For The Fitting} (according to Erfurt et al. (2003))\cr
\deqn{\phi(D) = \phi_{0}-\Delta\phi(1-exp(-\lambda*D))^\beta}
with \eqn{\phi(D)} the dose dependent IR-RF flux, \eqn{\phi_{0}} the inital IR-RF flux, 
\eqn{\Delta\phi} the dose dependent change of the IR-RF flux, \eqn{\lambda} 
the exponential parameter, \eqn{D} the dose and \eqn{\beta} the dispersive 
factor.\cr\cr
To obtain the palaedose the function is changed to:\cr
\deqn{D = ln(-(\phi(D) - \phi_{0})/(-\lambda*\phi)^{1/\beta}+1)/-\lambda}\cr
The fitting is done using the \code{port} algorithm of the \code{\link{nls}} function.}
\value{A plot (optional) and an \code{\linkS4class{RLum.Results}} object is returned 
containing the following elements: \cr
\item{De.values}{\code{\link{data.frame}} containing De-values with error 
(gray dashed lines in the plot) and further parameters}
\item{fit}{\link{nls} \code{nlsModel} object}\cr
\bold{Note:} The output (\code{De.values}) should be accessed using the 
function \code{\link{get_RLum.Results}}}
\references{Buylaert, J.P., Jain, M., Murray, A.S., Thomsen, K.J., Lapp, T., 2012. 
IR-RF dating of sand-sized K-feldspar extracts: A test of accuracy. 
Radiation Measurements 1-7. doi: 10.1016/j.radmeas.2012.06.021

Erfurt, G., Krbetschek, M.R., 2003. IRSAR - A single-aliquot regenerative-dose 
dating protocol applied to the infrared radiofluorescence (IR-RF) of coarse- grain 
K-feldspar. Ancient TL 21, 35-42.

Erfurt, G., 2003. Infrared luminescence of Pb+ centres in potassium-rich feldspars. 
physica status solidi (a) 200, 429-438.

Erfurt, G., Krbetschek, M.R., 2003. Studies on the physics of the infrared 
radioluminescence of potassium feldspar and on the methodology of its application 
to sediment dating. Radiation Measurements 37, 505-510.

Erfurt, G., Krbetschek, M.R., Bortolot, V.J., Preusser, F., 2003. 
A fully automated multi-spectral radioluminescence reading system for 
geochronometry and dosimetry. Nuclear Instruments and Methods in Physics Research 
Section B: Beam Interactions with Materials and Atoms 207, 487-499.

Trautmann, T., 2000. A study of radioluminescence kinetics of natural feldspar 
dosimeters: experiments and simulations. Journal of Physics D: Applied Physics 33, 2304-2310.

Trautmann, T., Krbetschek, M.R., Dietrich, A., Stolz, W., 1998. 
Investigations of feldspar radioluminescence: potential for a new dating technique. 
Radiation Measurements 29, 421-425.

Trautmann, T., Krbetschek, M.R., Dietrich, A., Stolz, W., 1999. Feldspar 
radioluminescence: a new dating method and its physical background. 
Journal of Luminescence 85, 45-58.

Trautmann, T., Krbetschek, M.R., Stolz, W., 2000. A systematic study of the 
radioluminescence properties of single feldspar grains. 
Radiation Measurements 32, 685-690.}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France) \cr
R Luminescence Package Team}
\note{This function assumes that there is no sensitivity change during the measurements (natural
vs. regenerated signal), which is in contrast to the findings from Buylaert et al. (2012).}


\seealso{\code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Results}},
\code{\link{get_RLum.Results}}, \code{\link{nls}}}
\examples{
##load data
data(ExampleData.RLum.Analysis, envir = environment())

##perform analysis
temp <- analyse_IRSAR.RF(object = IRSAR.RF.Data) 
}

\keyword{datagen}

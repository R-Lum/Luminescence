\name{analyse_pIRIRSequence}
\alias{analyse_pIRIRSequence}
\title{Analyse post-IR IRSL sequences}
\description{The function performs an analysis of post-IR IRSL sequences including 
curve fitting on \code{\linkS4class{RLum.Analysis}}
objects.}
\usage{analyse_pIRIRSequence(object, signal.integral.min, signal.integral.max, 
    background.integral.min, background.integral.max, dose.points, 
    sequence.structure = c("TL", "IR50", "pIRIR225"), output.plot = TRUE, 
    output.plot.single = FALSE, ...)}
\arguments{
  \item{object}{\code{\linkS4class{RLum.Analysis}}(\bold{required}): 
input object containing data for analysis}
  \item{signal.integral.min}{\code{\link{integer}} (\bold{required}): lower bound of the signal integral.
Provide this value as vector for different integration limits for the different
IRSL curves.}
  \item{signal.integral.max}{\code{\link{integer}} (\bold{required}): upper bound of the signal integral.
Provide this value as vector for different integration limits for the different
IRSL curves.}
  \item{background.integral.min}{\code{\link{integer}} (\bold{required}): lower bound of the background integral.
Provide this value as vector for different integration limits for the different
IRSL curves.}
  \item{background.integral.max}{\code{\link{integer}} (\bold{required}): upper bound of the background integral.
Provide this value as vector for different integration limits for the different
IRSL curves.}
  \item{dose.points}{\code{\link{numeric}} (optional): a numeric vector containg the dose points values
Using this argument overwrites dose point values in the signal curves. }
  \item{sequence.structure}{\link{vector} \link{character} (with default): specifies the general 
sequence structure. Allowed values are \code{"TL"} 
and any \code{"IR"} combination (e.g., \code{"IR50"},\code{"pIRIR225"}). 
Addiationally a parameter \code{"EXCLUDE"} is allowed to exclude curves 
from the analysis  
(Note: If a preheat without PMT measurement is used, remove the TL step.)}
  \item{output.plot}{\code{\link{logical}} (with default): enables or disables plot output.}
  \item{output.plot.single}{\code{\link{logical}} (with default): 
single plot output (\code{TRUE/FALSE}) to allow for plotting the results 
in single plot windows. 
Requires \code{output.plot = TRUE}.}
  \item{\dots}{further arguments that will be passed to the function 
\code{\link{analyse_SAR.CWOSL}}}
}
\details{To enable post-IR IRSL protocol (Thomsen et al., 2008) measurement analysis 
this function has been written as wrapper function for the function 
\code{\link{analyse_SAR.CWOSL}} facilitating an entire sequence analysis 
in one run. On the other hand, its functionality is stritctly limited 
by the functionality of the function \code{\link{analyse_SAR.CWOSL}}
}
\value{Plots (optional) and an \code{\linkS4class{RLum.Results}} object is 
returned containing the following elements: 
\item{De.values}{\link{data.frame} containing De-values, 
De-error and further parameters}
\item{LnLxTnTx.values}{\link{data.frame} of all calculated Lx/Tx values 
including signal, background counts and the dose points.}
\item{rejection.criteria}{\link{data.frame} with values that might by 
used as rejection criteria. NA is produced if no R0 dose point 
exists.}\cr

The output should be accessed using the function 
\code{\link{get_RLum.Results}}}
\references{Thomsen, K.J., Murray, A.S., Jain, M., Boetter-Jensen, L., 2008. 
Laboratory fading rates of various luminescence signals from feldspar-rich 
sediment extracts. Radiation Measurements 43, 1474-1486. 
doi:10.1016/j.radmeas.2008.06.002}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)\cr
R Luminescence Package Team}
\note{-}


\seealso{\code{\link{analyse_SAR.CWOSL}}, \code{\link{calc_OSLLxTxRatio}}, 
\code{\link{plot_GrowthCurve}}, 
\code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Results}}
\code{\link{get_RLum.Results}}}
\examples{

## TODO provide pIRIR measurements? 


}

\keyword{datagen}
\keyword{plot}
\section{Function version}{0.1 (2014-09-15 02:28:00)}

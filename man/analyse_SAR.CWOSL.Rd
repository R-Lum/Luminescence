\name{analyse_SAR.CWOSL}
\alias{analyse_SAR.CWOSL}
\title{Analyse SAR CW-OSL measurements}
\description{The function performs a SAR CW-OSL analysis on a \code{\linkS4class{RLum.Analysis}}
object including growth curve fitting.}
\usage{analyse_SAR.CWOSL(object, signal.integral.min, signal.integral.max, 
    background.integral.min, background.integral.max, rejection.criteria = list(recycling.ratio = 10, 
        recuperation.rate = 10, palaeodose.error = 10), dose.points, 
    log = "", output.plot = TRUE, output.plot.single = FALSE, 
    ...)}
\arguments{
  \item{object}{\code{\linkS4class{RLum.Analysis}}(\bold{required}): 
input object containing data for analysis}
  \item{signal.integral.min}{\code{\link{integer}} (\bold{required}): lower bound of the signal integral}
  \item{signal.integral.max}{code{\link{integer}} (\bold{required}): upper bound of the signal integral}
  \item{background.integral.min}{\code{\link{integer}} (\bold{required}): lower bound of the background integral}
  \item{background.integral.max}{\code{\link{integer}} (\bold{required}): upper bound of the background integral}
  \item{rejection.criteria}{
}
  \item{dose.points}{\link{numeric} (optional): a numeric vector containg the dose points values
Using this argument overwrites dose point values in the signal curves. }
  \item{log}{\link{character} (with default): a character string which contains "x" 
if the x axis is to be logarithmic, "y" if the y-axis is to be logarithmic 
and "xy" or "yx" if both axes are to be logarithmic. See \link{plot.default}.}
  \item{output.plot}{\link{logical} (with default): enables or disables plot output.}
  \item{output.plot.single}{\link{logical} (with default): single plot output (\code{TRUE/FALSE}) to 
allow for plotting the results in single plot windows. 
Requires \code{output.plot = TRUE}.}
  \item{\dots}{further arguments that will be passed to the function 
\code{\link{plot_GrowthCurve}}}
}
\details{The function performs an analysis for a standard SAR protocol measurements 
introduced by Murray and Wintle (2000) with CW-OSL curves. 
For the calculation of the Lx/Tx value the function \link{calc_OSLLxTxRatio} 
is used. \cr\cr

\bold{Working with IRSL data}\cr\cr

The function was originally designed to work just for 'OSL' curves, 
following the principles of the SAR protocol. An IRSL measurement protocol
may follow this procedure, e.g., post-IR IRSL protocol (Thomsen et al., 2008).
Therefore this functions has been enhanced to work with IRSL data, however, 
the function is only capable to analyse curves that follow the SAR protocol structure, i.e., 
to analyse a post-IR IRSL protocol curve data have to be pre-selected by the user to 
fit the standards of the SAR protocol, i.e., Lx,Tx,Lx,Tx and so on. \cr

Example: Imagine the measurement contains pIRIR50 and pIRIR225 IRSL curves. Only
one curve type can be analysed at the same time: 
The pIRIR50 curves or the pIRIR225 curves.\cr\cr

\bold{Provided rejection criteria}\cr\cr
\sQuote{recyling.ratio}: calculated for every repeated regeneration dose point.\cr
\sQuote{recuperation.rate}: recuperation rate calculated by comparing the 
Lx/Tx values of the zero regeneration point with the Ln/Tn value 
(the Lx/Tx ratio of the natural signal). 
For methodological background see Aitken and Smith (1988)}
\value{A plot (optional) and an \code{\linkS4class{RLum.Results}} object is 
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
\references{Aitken, M.J. & Smith, B.W., 1988. Optical dating: recuperation after bleaching. 
Quaternary Science Reviews, 7, pp. 387-393.

Duller, G., 2003. Distinguishing quartz and feldspar in single grain luminescence
measurements. Radiation Measurements, 37 (2), pp. 161-165. 

Murray, A.S. & Wintle, A.G.,  2000. Luminescence dating of quartz using an 
improved single-aliquot regenerative-dose protocol. Radiation Measurements, 
32, pp. 57-73. 

Thomsen, K.J., Murray, A.S., Jain, M., Boetter-Jensen, L., 2008. 
Laboratory fading rates of various luminescence signals from feldspar-rich 
sediment extracts. Radiation Measurements 43, 1474-1486. 
doi:10.1016/j.radmeas.2008.06.002}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)\cr
R Luminescence Package Team}
\note{This function must not be mixed up with the function 
\code{\link{Analyse_SAR.OSLdata}}, which works with \link{Risoe.BINfileData-class}
objects.\cr

\bold{The function currently does only support 'OSL' or 'IRSL' data!}}


\seealso{\code{\link{calc_OSLLxTxRatio}}, \code{\link{plot_GrowthCurve}}, 
\code{\linkS4class{RLum.Analysis}}, \code{\linkS4class{RLum.Results}}
\code{\link{get_RLum.Results}}}
\examples{

##load data
##ExampleData.BINfileData contains two BINfileData objects 
##CWOSL.SAR.Data and TL.SAR.Data
data(ExampleData.BINfileData, envir = environment())

##transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##perform SAR analysis
results <- analyse_SAR.CWOSL(object, 
                  signal.integral.min = 1,
                  signal.integral.max = 2,
                  background.integral.min = 900,
                  background.integral.max = 1000,
                  log = "x",
                  fit.method = "EXP")

##show results 
get_RLum.Results(results)

}

\keyword{datagen}
\keyword{plot}

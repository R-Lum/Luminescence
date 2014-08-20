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
32, pp. 57-73. }
\author{Sebastian Kreutzer, Universite Bordeaux Montaigne (France)\cr
R Luminescence Package Team}
\note{This function must not be mixed up with the function 
\code{\link{Analyse_SAR.OSLdata}}, which works with \link{Risoe.BINfileData-class}
objects.\cr

\bold{The function currently does not work for IRSL data!}}


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
\section{Function version}{0.3.5 (2014-08-13 17:03:37)}

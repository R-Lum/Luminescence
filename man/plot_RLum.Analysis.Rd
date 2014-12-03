\name{plot_RLum.Analysis}
\alias{plot_RLum.Analysis}
\title{Plot function for an RLum.Analysis S4 class object}
\description{The function provides a standardised plot output for curve data of an 
RLum.Analysis S4 class object}
\usage{plot_RLum.Analysis(object, nrows = 3, ncols = 2, abline, combine = FALSE, 
    ...)}
\arguments{
  \item{object}{\code{\linkS4class{RLum.Analysis}} (\bold{required}): S4 object of class 
\code{RLum.Analysis}}
  \item{nrows}{\code{\link{integer}} (with default): sets number of rows for plot output}
  \item{ncols}{\code{\link{integer}} (with default): sets number of columns for plot output}
  \item{abline}{\code{\link{list}} (optional): allows to set similar ablines in each plot. This option 
uses the function \code{\link{do.call}}, meaning that every argument in the \code{list} has
to be provided as \code{list}, e.g. \code{abline = list(list(v = 120), list(v = 350))} produces
two vertical ablines: One at 150 and another one at 350. Within the call all arguments 
supported by \code{\link{abline}} are fully supported,}
  \item{combine}{\code{\link{logical}} (with default): allows to combine all 
code{\linkS4class{RLum.Data.Curve}} objects in one single plot.
Works only for \code{\linkS4class{RLum.Analysis}} that comprises a single 
curve object (option is currently only roughly implemented)}
  \item{\dots}{further arguments and graphical parameters will be passed to the \code{plot} function.
Supported arguments: \code{main}, \code{mtext}, \code{log}, \code{lwd}, \code{lty}
\code{type}, \code{pch}, \code{col} ... and for \code{combine = TRUE} also: \code{xlim},
\code{ylim}, \code{xlab}, \code{ylab}, \code{sub}, \code{legend.text}, \code{legend.pos}}
}
\details{The function produces a multiple plot output. 
A file output is recommended (e.g., \code{\link{pdf}}).}
\value{Returns multiple plots.}
\references{#}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
R Luminescence Package Team}
\note{Not all arguments available for \code{\link{plot}} will be passed!
Only plotting of \code{RLum.Data.Curve} and \code{RLum.Data.Spectrum}
objects are currently supported. }


\seealso{\code{\link{plot}}, \code{\link{plot_RLum}}, 
\code{\link{plot_RLum.Data.Curve}}}
\examples{

###load data
data(ExampleData.BINfileData, envir = environment())

##convert values for position 1
temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##plot all values
plot_RLum.Analysis(temp)

##plot (combine) TL curves in one plot
temp.sel <- get_RLum.Analysis(temp, recordType = "TL", keep.object = TRUE)
plot_RLum.Analysis(temp.sel, combine = TRUE, norm = TRUE, main = "TL combined")

}

\keyword{aplot}
\section{Function version}{0.1.5 (2014-12-03 13:31:41)}

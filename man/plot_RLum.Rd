\name{plot_RLum}
\alias{plot_RLum}
\title{General plot function for RLum S4 class objects}
\description{Function calls object specific plot functions for RLum S4 class objects. }
\usage{plot_RLum(object, ...)}
\arguments{
  \item{object}{\code{\linkS4class{RLum}} (\bold{required}): S4 object of class \code{RLum}}
  \item{\dots}{further arguments and graphical parameters that will be passed to the specific 
plot functions}
}
\details{The function provides a generalised access point for plotting specific 
\code{\linkS4class{RLum}} objects.\cr
Depending on the input object, the corresponding plot function will be selected. 
Allowed arguments can be found in the documentations of each plot function. 
\tabular{lll}{
\bold{object} \tab \tab \bold{corresponding plot function} \cr

\code{\linkS4class{RLum.Data.Curve}} \tab : \tab \code{\link{plot_RLum.Data.Curve}} \cr
\code{\linkS4class{RLum.Data.Spectrum}} \tab : \tab \code{\link{plot_RLum.Data.Spectrum}}\cr
\code{\linkS4class{RLum.Data.Image}} \tab : \tab \code{\link{plot_RLum.Data.Image}}\cr
\code{\linkS4class{RLum.Analysis}} \tab : \tab \code{\link{plot_RLum.Analysis}}
}}
\value{Returns a plot.}
\references{#}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
R Luminescence Package Team}
\note{The provided plot output depends on the input object.}


\seealso{\code{\link{plot_RLum.Data.Curve}}, \code{\linkS4class{RLum.Data.Curve}},
\code{\link{plot_RLum.Data.Spectrum}}, \code{\linkS4class{RLum.Data.Spectrum}},
\code{\link{plot_RLum.Data.Image}}, \code{\linkS4class{RLum.Data.Image}},
\code{\link{plot_RLum.Analysis}}, \code{\linkS4class{RLum.Analysis}}}
\examples{

#load Example data
data(ExampleData.CW_OSL_Curve, envir = environment())

#transform data.frame to RLum.Data.Curve object
temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

#plot RLum object 
plot_RLum(temp)

}

\keyword{dplot}
\section{Function version}{0.2 (2014-09-15 02:28:00)}
\section{Function version}{0.2 (2014-09-15 02:28:00)}

\name{plot_RLum.Data.Image}
\alias{plot_RLum.Data.Image}
\title{Plot function for an RLum.Data.Image S4 class object}
\description{The function provides a standardised plot output for image data of an 
RLum.Data.Image S4 class object mainly using the plot functions provided by the
\code{\link{raster}} package [beta version]. }
\usage{plot_RLum.Data.Image(object, par.local = TRUE, plot.type = "plot.raster", 
    ...)}
\arguments{
  \item{object}{\code{\linkS4class{RLum.Data.Image}} (\bold{required}): 
S4 object of class \code{RLum.Data.Image}}
  \item{par.local}{\code{\link{logical}} (with default): use local graphical parameters for plotting, e.g.
the plot is shown in one column and one row. If \code{par.local = FALSE} 
global parameters are inherited.}
  \item{plot.type}{\code{\link{character}} (with default): plot types. Supported types are  
\code{plot.raster}, \code{plotRGB} or \code{contour}}
  \item{\dots}{further arguments and graphical parameters that will be passed to the 
specific plot functions.}
}
\details{\bold{Details on the plot functions} \cr

Image is visualised as 2D plot usinng generic plot types provided by other 
package.
R plot functions. \cr

\bold{\code{plot.type = "plot.raster"}}\cr

Uses the standard plot function for raster data from the package \code{\link{raster}}:
\code{\link[raster]{plot}}

\bold{\code{plot.type = "plotRGB"}}\cr

Uses the function \code{\link[raster]{plotRGB}} from the \code{\link{raster}} package.

\bold{\code{plot.type = "contour"}}\cr

Uses the function contour plot function from the \code{\link{raster}}
function (\code{\link[raster]{contour}}).}
\value{Returns a plot.}
\references{-}
\author{Sebastian Kreutzer, Universite Bordeaux Montaigne (France), \cr
R Luminescence Package Team}
\note{This function has been created to faciliate the plotting of image data imported
by the function \code{\link{readSPE2R}}. However, so far the function 
is not optimized to handle image data > ca. 200 MByte and thus the plot
my or extremely slow. }


\seealso{\code{\linkS4class{RLum.Data.Image}}, 
\code{\link{plot}}, \code{\link{plot_RLum}}, \code{\link{raster}}, }
\examples{
 
##load example data
##TODO
 
}

\keyword{aplot}
\section{Function version}{0.1 (2014-08-19 11:20:13)}

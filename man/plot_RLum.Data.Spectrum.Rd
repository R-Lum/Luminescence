\name{plot_RLum.Data.Spectrum}
\alias{plot_RLum.Data.Spectrum}
\title{Plot function for an RLum.Data.Spectrum S4 class object}
\description{The function provides a standardised plot output for spectrum data of an 
RLum.Data.Spectrum S4 class object }
\usage{plot_RLum.Data.Spectrum(object, par.local = TRUE, plot.type = "contour", 
    optical.wavelength.colours = TRUE, bg.channels, bin.rows = 1, 
    bin.cols = 1, rug = TRUE, xaxis.energy = FALSE, ...)}
\arguments{
  \item{object}{\code{\linkS4class{RLum.Data.Spectrum}} (\bold{required}): 
S4 object of class \code{RLum.Data.Spectrum}}
  \item{par.local}{\code{\link{logical}} (with default): use local graphical parameters for plotting, e.g.
the plot is shown in one column and one row. If \code{par.local = FALSE} 
global parameters are inherited.}
  \item{plot.type}{\code{\link{character}} (with default): plot type, for 3D-plot use 
\code{persp}, or \code{persp3d}, for a 2D-plot \code{contour}, 
\code{single} or \code{multiple.lines}
(along the time or temperature axis) or \code{transect} (along the wavelength
axis) \cr

Note: The use of \code{persp3d} will produce a dynamic 3D surface plot 
on the screen.}
  \item{optical.wavelength.colours}{\code{\link{logical}} (with default): use optical wavelength colour palette.
Note: For this, the spectrum range is limited: \code{c(350,750)}. 
Own colours can be set with the argument \code{col}.}
  \item{bg.channels}{\code{\link{vector}} (optional): defines channel for background subtraction
If a vector is provided the mean of the channels is used for subtraction.
Note: Background subtraction is applied prior to channel binning}
  \item{bin.rows}{\code{\link{integer}} (with defaul): allow summing-up wavelength channels 
(horizontal binning), e.g. \code{bin.rows = 2} two channels are summed up}
  \item{bin.cols}{\code{\link{integer}} (with default): allow summing-up channel counts 
(vertical binning) for plotting, 
e.g. \code{bin.cols = 2} two channels are summed up}
  \item{rug}{\code{\link{logical}} (with default): enables or disables colour rug. 
Currently only implemented for plot type \code{multiple.lines}.}
  \item{xaxis.energy}{\code{\link{logical}} (with default): enables or disables energy instead
of wavelength axis. Axis labelling are changed accordingly, so far no 
manual axis labelling is choosen. }
  \item{\dots}{further arguments and graphical parameters that will be passed to the 
\code{plot} function.}
}
\details{\bold{Matrix structure} \cr
(cf. \code{\linkS4class{RLum.Data.Spectrum}}) \cr

\itemize{
\item \code{rows} (x-values): wavelengths/channels (xlim, xlab)
\item \code{columns} (y-values): time/temperature  (ylim, ylab)
\item \code{cells} (z-values): count values (zlim, zlab)
}

\emph{Note: This nomenclature is valid for all plot types of this function!}


\bold{Nomenclature for value limiting}\cr

\code{xlim}: Limits values along the wavelength axis\cr
\code{ylim}: Limits values along the time/temperature axis\cr
\code{zlim}: Limits values along the count value axis

\bold{Details on the plot functions} \cr

Spectrum is visualised as 3D or 2D plot. Both plot types are based on internal
R plot functions. \cr

\bold{\code{plot.type = "persp"}}

Arguments that will be passed to \code{\link{persp}}:
\itemize{ 
\item \code{shade}: default is \code{0.4}  
\item \code{phi}: default is \code{30}
\item \code{theta}: default is \code{30}
\item \code{expand}: default is \code{1}
\item \code{ticktype}: default is \code{detailed}
} 

\emph{Note: Further parameters can be adjusted via \code{par}. For example
to set the background transparent and reduce the thickness of the lines use:
\code{par(bg = NA, lwd = 0.7)} previous the function call.}

\bold{\code{plot.type = "single"}}\cr
Per frame a single curve is returned. Frames are time or temperature steps.\cr

\bold{\code{plot.type = "multiple.lines"}}\cr
All frames drawn in one frame.\cr

\bold{\code{plot.type = "transect"}}\cr
Depending on the selected wavelength/channel range a transect over the 
time/temperature (y-axis) will be plotted along the wavelength/channels (x-axis).
If the range contains more than one channel, values (z-values) are summed up.
To select a transect use the \code{xlim} argument, e.g. \code{xlim = c(300,310)}
plot along the summed up count values of channel 300 to 310.


\bold{Further arguments that will be passed (depending on the plot type)}\cr
\code{xlab}, \code{ylab}, \code{zlab}, \code{xlim}, \code{ylim}, \code{zlim}, 
\code{main}, \code{mtext}, \code{pch}, \code{type}, \code{border}, \code{box} 
\code{lwd} \cr}
\value{Returns a plot.}
\references{#}
\author{Sebastian Kreutzer, IRMAT-CRP2A, Universite Bordeaux Montaigne (France), \cr
R Luminescence Package Team}
\note{Not all additional arguments (\code{...}) will be passed similarly!}


\seealso{\code{\linkS4class{RLum.Data.Spectrum}}, 
\code{\link{plot}}, \code{\link{plot_RLum}}, \code{\link{persp}}, 
\code{\link{persp3d}}, \code{\link{contour}}}
\examples{
 
##load example data
data(ExampleData.XSYG, envir = environment())

##(1)plot simple spectrum (2D) - contour
plot_RLum.Data.Spectrum(TL.Spectrum, 
                        plot.type="contour", 
                        xlim = c(310,750), 
                        ylim = c(0,300),
                        bin.rows=10, 
                        bin.cols = 1)

##(2) plot simple spectrum (2D) - multiple.lines (with ylim)
plot_RLum.Data.Spectrum(TL.Spectrum, 
                        plot.type="multiple.lines", 
                        xlim = c(310,750), 
                        ylim = c(0,100),
                        bin.rows=10, 
                        bin.cols = 1)

##(3) plot 3d spectrum (uncomment for usage)
# plot_RLum.Data.Spectrum(TL.Spectrum, plot.type="persp", 
# xlim = c(310,750), ylim = c(0,300), bin.rows=10, 
# bin.cols = 1)

 
}

\keyword{aplot}
\section{Function version}{0.3.4 (2014-09-15 02:28:00)}
\section{Function version}{0.3.4 (2014-09-15 02:28:00)}

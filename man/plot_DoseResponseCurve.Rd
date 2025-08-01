% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_DoseResponseCurve.R
\name{plot_DoseResponseCurve}
\alias{plot_DoseResponseCurve}
\title{Plot a dose-response curve for luminescence data (Lx/Tx against dose)}
\usage{
plot_DoseResponseCurve(
  object,
  plot_extended = TRUE,
  plot_singlePanels = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{object}{\linkS4class{RLum.Results} (\strong{required}):
An object produced by \link{fit_DoseResponseCurve}.}

\item{plot_extended}{\link{logical} (\emph{with default}):
If \code{TRUE}, 3 plots on one plot area are provided:
\enumerate{
\item growth curve,
\item histogram from Monte Carlo error simulation and
\item a test dose response plot.
}

If \code{FALSE}, just the growth curve will be plotted.}

\item{plot_singlePanels}{\link{logical} (\emph{with default}):
single plot output (\code{TRUE/FALSE}) to allow for plotting the results in
single plot windows. Ignored if \code{plot_extended = FALSE}.}

\item{verbose}{\link{logical} (\emph{with default}):
enable/disable output to the terminal.}

\item{...}{Further graphical parameters to be passed (supported:
\code{main}, \code{mtext}, \code{xlim}, \code{ylim}, \code{xlab}, \code{ylab}, \code{log} (not valid for objects
fitted with \code{mode = 'extrapolation'}), \code{legend}, \code{box} (\code{TRUE}/\code{FALSE}),
\code{reg_points_pch}, \code{density_polygon} (\code{TRUE/FALSE}), \code{density_polygon_col},
\code{density_rug} (\code{TRUE}/\code{FALSE})).}
}
\value{
A plot (or a series of plots) is produced.
}
\description{
A dose-response curve is produced for luminescence measurements using a
regenerative or additive protocol as implemented in \link{fit_DoseResponseCurve}.
}
\section{Function version}{
 1.0.6
}

\examples{

##(1) plot dose-response curve for a dummy dataset
data(ExampleData.LxTxData, envir = environment())
fit <- fit_DoseResponseCurve(LxTxData)
plot_DoseResponseCurve(fit)

##(1b) horizontal plot arrangement
layout(mat = matrix(c(1,1,2,3), ncol = 2))
plot_DoseResponseCurve(fit, plot_singlePanels = TRUE)

##(2) plot the dose-response curve with pdf output - uncomment to use
##pdf(file = "~/Dose_Response_Curve_Dummy.pdf", paper = "special")
plot_DoseResponseCurve(fit)
##dev.off()

##(3) plot the growth curve with pdf output - uncomment to use, single output
##pdf(file = "~/Dose_Response_Curve_Dummy.pdf", paper = "special")
plot_DoseResponseCurve(fit, plot_singlePanels = TRUE)
##dev.off()

##(4) plot resulting function for given interval x
x <- seq(1,10000, by = 100)
plot(
 x = x,
 y = eval(fit$Formula),
 type = "l"
)

}
\references{
Berger, G.W., Huntley, D.J., 1989. Test data for exponential fits. Ancient TL 7, 43-46.

Guralnik, B., Li, B., Jain, M., Chen, R., Paris, R.B., Murray, A.S., Li, S.-H., Pagonis, P.,
Herman, F., 2015. Radiation-induced growth and isothermal decay of infrared-stimulated luminescence
from feldspar. Radiation Measurements 81, 224-231.

Pagonis, V., Kitis, G., Chen, R., 2020. A new analytical equation for the dose response of dosimetric materials,
based on the Lambert W function. Journal of Luminescence 225, 117333. \doi{10.1016/j.jlumin.2020.117333}
}
\seealso{
\link{fit_DoseResponseCurve}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
Michael Dietze, GFZ Potsdam (Germany) \cr
Marco Colombo, Institute of Geography, Heidelberg University (Germany)
}

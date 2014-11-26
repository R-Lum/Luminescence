\name{calc_FadingCorr}
\alias{calc_FadingCorr}
\title{Apply a fading correction according to Huntley & Lamothe (2001) for a given g-value.}
\description{This function runs the iterations that are needed to calculate the corrected 
age including the error for a given g-value according to Huntley & Lamothe (2001).}
\usage{calc_FadingCorr(g_value, tc, age.faded, n.MCruns = 500)}
\arguments{
  \item{g_value}{\link{vector} (\bold{required}): g-value and error obtained from separate 
fading measurements (see example)}
  \item{tc}{\link{numeric} (\bold{required}): time in seconds (time between irradiation and
the prompt measurement, cf. Huntely & Lamothe 2001)}
  \item{age.faded}{\link{numeric} \link{vector} (\bold{required}): uncorrected age with error 
in ka (see example)}
  \item{n.MCruns}{\link{integer} (with default): number of Monte Carlo simulation runs for 
error estimation}
}
\details{The error of the fading-corrected age is determined using a Monte 
Carlo simulation approach. 
Large values for \code{n.MCruns} will significantly increase the computation time.}
\value{A \link{data.frame} containing the fading-corrected age is returned.}
\references{Huntley, D.J., Lamothe, M., 2001. Ubiquity of anomalous fading in K-feldspars 
and the measurement and correction for it in optical dating. 
Canadian Journal of Earth Sciences, 38, 1093-1106.}
\author{Sebastian Kreutzer, JLU Giessen (Germany)
R Luminescence Package Team}
\note{The upper age limit is set to 500 ka!}


\seealso{#}
\examples{

calc_FadingCorr(g_value = c(3.3,0.03), tc = 752, 
                age.faded = c(100,10), 
                n.MCruns=50)

}

\keyword{datagen}
\section{Function version}{0.1.2 (2014-09-15 02:28:00)}

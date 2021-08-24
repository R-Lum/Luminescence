\name{Marine20}
\alias{Marine20}
\docType{data}
\title{
Marine data for calibration of 14C age }
\description{
As 14C years is not equal to calendar years because atmospheric 14C concentration varies through time. Hence, data in marine_CalC14 allows a calibration for hypothetical "global" marine reservoir.
}
\usage{data("Marine20")}
\format{
  A data frame with 3 variables.
  \describe{
    \item{\code{CAL.BP}}{a numeric vector correpondig to calendar years befor present}
    \item{\code{X14C.age}}{a numeric vector correponding to 14C age}
    \item{\code{Error}}{a numeric vector correponding to error arround 14C age measurement}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Heaton, T., Köhler, P., Butzin, M., Bard, E., Reimer, R., Austin, W., . . . Skinner, L. (2020). Marine20—The Marine Radiocarbon Age Calibration Curve (0–55,000 cal BP). Radiocarbon, 62(4), 779-820. doi:10.1017/RDC.2020.68
}
\examples{
data(Marine20)
## maybe str(Marine20) ; head(Marine20) ...
}
\keyword{datasets}

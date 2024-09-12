\name{IntCal20}
\alias{IntCal20}
\docType{data}
\title{
Atmospheric North data for calibration of 14C age
}
\description{
As 14C years is not equal to calendar years because atmospheric 14C concentration varies through time. Hence, data in AtmosphericNorth_CalC14 allows a calibration for mid-latitude Northern Hemisphere atmospher reservoir.
}
\usage{data("IntCal20")}
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
Reimer, P., Austin, W., Bard, E., Bayliss, A., Blackwell, P., Bronk Ramsey, C., . . . Talamo, S. (2020). The IntCal20 Northern Hemisphere Radiocarbon Age Calibration Curve (0–55 cal kBP). Radiocarbon, 62(4), 725-757. doi:10.1017/RDC.2020.41
}
\examples{
data(IntCal20)
## maybe str(IntCal20) ; head(IntCal20) ...
}
\keyword{datasets}

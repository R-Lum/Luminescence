% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_Risoe.BINfileData.R
\name{set_Risoe.BINfileData}
\alias{set_Risoe.BINfileData}
\title{General accessor function for RLum S4 class objects}
\usage{
set_Risoe.BINfileData(
  METADATA = data.frame(),
  DATA = list(),
  .RESERVED = list()
)
}
\arguments{
\item{METADATA}{x}

\item{DATA}{x}

\item{.RESERVED}{x}
}
\value{
Return is the same as input objects as provided in the list.
}
\description{
Function calls object-specific get functions for RisoeBINfileData S4 class objects.
}
\details{
The function provides a generalised access point for specific
\linkS4class{Risoe.BINfileData} objects.\cr
Depending on the input object, the corresponding get function will be selected.
Allowed arguments can be found in the documentations of the corresponding
\linkS4class{Risoe.BINfileData} class.
}
\section{Function version}{
 0.1
}

\seealso{
\linkS4class{Risoe.BINfileData}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
, RLum Developer Team} 

\section{How to cite}{
Kreutzer, S., 2024. set_Risoe.BINfileData(): General accessor function for RLum S4 class objects. Function version 0.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., 2024. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 0.9.25. https://r-lum.github.io/Luminescence/
}

\keyword{utilities}

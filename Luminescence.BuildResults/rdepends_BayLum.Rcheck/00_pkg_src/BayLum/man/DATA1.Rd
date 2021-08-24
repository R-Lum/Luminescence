\name{DATA1}
\alias{DATA1}
\docType{data}
\title{DATA of sample named GDB3
%%   ~~ data name/kind ... ~~
}
\description{
list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J,K,Nb_measurement obtained using \code{\link{Generate_DataFile}} function with single-grain OSL measurementsl of the sample GDB3.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("DATA1")}
\format{
  A list containing:
  \describe{
   \item{\code{LT:}}{(one list per sample): each list contains all L/T values for the corresponding sample;}
   \item{\code{sLT:}}{(one list per sample): each list contains all uncertainties on L/T values for the corresponding sample;}
   \item{\code{ITimes:}}{(one list per sample): each list contains irradiation time values for the corresponding sample;}
   \item{\code{dLab=}}{a matrix containing in line \code{i}, the laboratory dose rate and its variance for sample \code{i};}
   \item{\code{ddot_env:}}{a matrix containing in line \code{i}, the environmental dose rate and its variance (excluding the common error terms) for sample \code{i};}
   \item{\code{regDose:}}{(one list per sample): each list contains all regenerated doses;}
   \item{\code{J:}}{a vector giving, for each BIN file, the number of aliquots selected for the analysis;}
   \item{\code{K:}}{a vector giving, for each BIN file, the number of regenerative doses in the SAR protocol;}
   \item{\code{Nb_measurement:}}{a vector giving, for each BIN file, the number of measurements;}
  }
}
%\source{ A FAIRE
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
For more informations on this sample we refer to the following publication:

Tribolo, C., Asrat, A., Bahain, J. J., Chapon, C., Douville, E., Fragnol, C., Hernandez, M., Hovers, E., Leplongeon, A.,  Martin, L,  Pleurdeau, D,  Pearson, O ,  Puaud, S,  Assefa, Z. (2017). Across the Gap: Geochronological and Sedimentological Analyses from the Late Pleistocene-Holocene Sequence of Goda Buticha, Southeastern Ethiopia. PloS one, 12(1), e0169418.
}
\examples{
data(DATA1)
str(DATA1)
}
\keyword{datasets}

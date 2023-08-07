\name{DATA3}
\alias{DATA3}
\docType{data}
\title{
DATA of sample named FER1}
\description{
list of objects: LT, sLT, ITimes, dLab, ddot_env, regDose, J,K,Nb_measurement obtained using \code{\link{Generate_DataFile}} function with multi-grain OSL measurementsl of the sample FER1.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("DATA3")}
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
\references{
For more informations on this sample we refer to the following publication:

Guerin, G., Frouin, M., Talamo, S., Aldeias, V., Bruxelles, L., Chiotti, L., Goldberg, P., Hublin, J.J., Jain, M., Lahaye, C., Madelaine, S., Maureille, B., McPherron, S., Mercier, N., Murray, A., Sandgathe, D., Steele, T., Thomsen, K., Turq, A. (2015). A multi-method luminescence dating of the Palaeolithic sequence of La Ferrassie based on new excavations adjacent to the La Ferrassie 1 and 2 skeletons. Journal of Archaeological Science, 58, 147-166.}
\examples{
data(DATA3)
str(DATA3)
}
\keyword{datasets}

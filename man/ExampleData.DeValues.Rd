\name{ExampleData.DeValues}
\alias{ExampleData.DeValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example De data sets for the package Luminescence}
\description{Equivalent dose (De) values measured for a fine grain quartz sample from a loess section 
in Rottewitz (Saxony/Germany) and for a coarse grain quartz sample from a fluvial deposit in the rock
shelter of Cueva Anton (Murcia/Spain).}
\usage{
ExampleData.DeValues
}
%- maybe also 'usage' for other objects documented here.
\format{
A \code{\link{list}} with two elements, each containing a two column \code{\link{data.frame}}:

\describe{
    \code{$BT998}: De and De error values for a 
    fine grain quartz sample from a loess section in Rottewitz.\cr\cr
    \code{$CA1}: Single grain De and De error values for a 
    coarse grain quartz sample from a fluvial deposit in the rock shelter of Cueva Anton
  }


} 

\source{
%%  ~~ If necessary, more details than the description above ~~

\bold{BT998} \cr
\tabular{ll}{
Lab: \tab Luminescence Laboratory Bayreuth\cr
Lab-Code: \tab BT998\cr
Location: \tab Rottewitz (Saxony/Germany)\cr
Material: \tab Fine grain quartz measured on aluminum discs on a Risoe TL/OSL DA-15 reader\cr
Units:    \tab Values are given in seconds \cr
Dose Rate: \tab Dose rate of the beta-source at measurement ca. 0.0438 Gy/s +/- 0.0019 Gy/s\cr
Measurement Date: \tab 2012-01-27
}
\bold{CA1} \cr
\tabular{ll}{
Lab: \tab Cologne Luminescence Laboratory (CLL)\cr
Lab-Code: \tab C-L2941\cr
Location: \tab Cueva Anton (Murcia/Spain)\cr
Material: \tab Coarse grain quartz (200-250 microns) measured on single grain discs on a Risoe TL/OSL DA-20 reader\cr
Units:    \tab Values are given in Gray \cr
Measurement Date: \tab 2012
}

}

\references{
\bold{BT998} \cr\cr
Unpublished data \cr\cr
\bold{CA1} \cr\cr
Burow, C., Kehl, M., Hilgers, A., Weniger, G.-C., Angelucci, D., Villaverde, V., Zapata, J. and Zilhao, J. 
(accepted). Luminescence dating of fluvial deposits in the rock shelter of Cueva Anton, Spain. Geochronometria.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##(1) plot values as histogram
data(ExampleData.DeValues, envir = environment())
plot_Histogram(ExampleData.DeValues$BT998, xlab = "De [s]")

##(2) plot value as histogram (with Second to Gray convertion)
data(ExampleData.DeValues, envir = environment())

De.values <- Second2Gray(ExampleData.DeValues$BT998, 
                         dose.rate = c(0.0438, 0.0019), 
                         method = "gaussian")

plot_Histogram(De.values, xlab = "De [Gy]")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{IO}

\name{ExampleData.DeValues}
\alias{ExampleData.DeValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example De data for the package Luminescence}
\description{25 equivalent dose (De) values measured for a fine grain quartz sample from a loess section 
in Rottewitz (Saxony/Germany).}
\usage{
ExampleData.DeValues
}
%- maybe also 'usage' for other objects documented here.
\format{
A \code{\link{data.frame}} with two columns:

\describe{
    \item{\code{x}}{a numeric vector, De}
    \item{\code{y}}{a numeric vector, De error}
  }


} 

\source{
%%  ~~ If necessary, more details than the description above ~~
\tabular{ll}{
Lab: \tab Luminescence Laboratory Bayreuth\cr
Lab-Code: \tab BT998\cr
Location: \tab Rottewitz (Saxony/Germany)\cr
Material: \tab Fine grain quartz measured on aluminum discs on a Risoe TL/OSL DA-15 reader\cr
Units:    \tab Values are given in seconds \cr
Dose Rate: \tab Dose rate of the beta-source at measurement ca. 0.0438 Gy/s +/- 0.0019 Gy/s
}
}

\references{
%% ~put references to the literature/web site here ~
unpublished data
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##(1) plot values as histogram
data(ExampleData.DeValues, envir = environment())
plot_Histogram(ExampleData.DeValues, xlab = "De [s]")

##(2) plot value as histogram (with Second to Gray convertion)
data(ExampleData.DeValues, envir = environment())

De.values <- Second2Gray(ExampleData.DeValues, 
                         dose_rate = c(0.0438, 0.0019), 
                         method = "gaussian")

plot_Histogram(De.values, xlab = "De [Gy]")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{IO}

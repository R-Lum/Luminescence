% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Luminescence-package.R
\docType{data}
\name{ExampleData.portableOSL}
\alias{ExampleData.portableOSL}
\title{Example portable OSL curve data for the package Luminescence}
\source{
\strong{ExampleData.portableOSL}

\tabular{ll}{
Lab: \tab Cologne Luminescence Laboratory\cr
Lab-Code: \tab \code{none} \cr
Location: \tab Nievenheim/Germany\cr
Material: \tab Fine grain quartz \cr
Reference: \tab unpublished data
}
}
\description{
A \code{list} of \linkS4class{RLum.Analysis} objects, each containing
the same number of \linkS4class{RLum.Data.Curve} objects representing
individual OSL, IRSL and dark count measurements of a sample.
}
\examples{

data(ExampleData.portableOSL, envir = environment())
plot_RLum(ExampleData.portableOSL)

}
\keyword{datasets}

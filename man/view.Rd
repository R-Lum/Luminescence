% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view.R
\name{view}
\alias{view}
\title{Convenient View Function}
\usage{
view(object, ...)
}
\arguments{
\item{object}{(\strong{required}) object to view}

\item{...}{further arguments passed to the function}
}
\value{
\code{NULL} and opens the data viewer
}
\description{
Invokes the \link[utils:View]{utils::View} function tailored to objects
in the package. If started from RStudio, it uses the RStudio view.
}
\seealso{
\code{\link[utils:View]{utils::View()}}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
}
\keyword{utilities}

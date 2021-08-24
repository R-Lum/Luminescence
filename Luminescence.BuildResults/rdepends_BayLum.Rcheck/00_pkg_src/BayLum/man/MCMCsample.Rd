\name{MCMCsample}
\alias{MCMCsample}
\docType{data}
\title{MCMC sample from the posterior distribution of the dataset GDB5
%%   ~~ data name/kind ... ~~
}
\description{MCMC samples from the posterior distribution of "A" for age, "D" for palaeodose and "sD" for dispersion of equivalent doses around "D", of the data set GDB5.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("MCMCsample")}
\format{
  It is a matric with 6000 row and tree column.
  \describe{
    \item{\code{A}}{The first column of the matrice are sampled from the posterior distribution of the paramete \code{A}  }
    \item{\code{D}}{The first column of the matrice are sampled from the posterior distribution of the paramete \code{D}  }
    \item{\code{sD}}{The first column of the matrice are sampled from the posterior distribution of the paramete \code{sD}  }
  }
}
%\source{ A FAIRE
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Tribolo, C., Asrat, A., Bahain, J. J., Chapon, C., Douville, E., Fragnol, C., Hernandez, M., Hovers, E., Leplongeon, A.,  Martin, L,  Pleurdeau, D,  Pearson, O ,  Puaud, S,  Assefa, Z. (2017). Across the Gap: Geochronological and Sedimentological Analyses from the Late Pleistocene-Holocene Sequence of Goda Buticha, Southeastern Ethiopia. PloS one, 12(1), e0169418.
}
\examples{
data(MCMCsample)
## maybe str(MCMCsample) ; plot(MCMCsample[,1],type="l") ...
}
\keyword{datasets}

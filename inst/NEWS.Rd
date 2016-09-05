\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.7.0 ( ,2016)}{

  \subsection{New functions}{
    \itemize{
        \item \code{analyse_FadingMeasurement()}: Analysing fading measurements
        and calculating g-values

        \item \code{analyse_portableOSL()}:
        The function analyses CW-OSL curve data produced by a SUERC portable
        OSL reader and produces a combined plot of OSL/IRSL signal intensities,
        OSL/IRSL depletion ratios and the IRSL/OSL ratio.


        \item \code{calc_WodaFuchs2008()}:
        Function to obtain the equivalent dose using
        the approach proposed by Woda and Fuchs, 2008.

        \item \code{PSL2Risoe.BINfileData()}:
        Converts an \code{RLum.Analysis} object produced by the function
        \code{read_PSL2R()} to an \code{Risoe.BINfileData} object (BETA).


        \item \code{read_PSL2R()}:
        Imports PSL files produced by a SUERC portable OSL reader into R (BETA).


    }
  }

    \subsection{Changes in S4-classes and methods}{
      \itemize{
      \item Method \code{get_RLum} for \code{RLum.Analysis}-objects did not respect \code{.pid}, fixed.
      \item \code{plot_RLum} was not passing the argument \code{sub}, as it was fetched by the partial
      argument matching; fixed.


      }



    }

  \subsection{Bugfixes and changes}{

   \itemize{
      \item \code{plot_GrowthCurve()}{
        \itemize{
          dummu

        }


      }

    }

    }

  \subsection{Internals}{

   \itemize{
      \item The internal function converting BIN-file curves to RLum.Data.Curve() objects had been
      optimised and, amongst others, now uses a function written using Rcpp to create the curve matrix.
      The conversion now works ca. 2 times faster,

      \item add \code{`[<-]`} method for \code{RLum.Data.Curve} objects.

    }

    }


  }

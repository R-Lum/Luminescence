\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.6.1 ( ,2016)}{



  \subsection{Bugfixes and changes}{
   \itemize{
    \item \code{analyse_baSAR()}
        \itemize{
          \item Add \code{n.chains} option to \code{method_control}.

      }


      \item \code{merge_RisoeBINfileData()}
        \itemize{
          \item Account for 'implicit list embedding of S4 objects is deprecated' warning by changing
          the code (no visible user effect, apart from no visible warning anymore).
      }

       \item \code{plot_GrowthCurve()}
        \itemize{
          \item Move \code{call} output in the \code{RLum.Results} output element \code{list}.

      }


    }



    }

 \subsection{S3-generics}{
   \itemize{
      \item The method \code{subset} for \code{RisoeBINfileData} objects got a new
      argument \code{records.rm}. If set to \code{FALSE} curves are only selected/deselected,
      but not removed all.

    }



    }
  }

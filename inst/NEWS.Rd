\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.6.1 ( ,2016)}{



  \subsection{Bugfixes and changes}{
   \itemize{
    \item \code{analyse_baSAR()}
        \itemize{
          \item Add \code{n.chains} option to \code{method_control},
          \item add \code{inits} to \code{method_control} to set the RNG,
          \item add \code{thin} to \code{method_control} to control the monitoring,
          \item account for probable odd use input if the \code{XLS_file} is a \code{data.frame}
          (column 1 is of type \code{factor} instead \code{character}),
          \item a lots of minor improvements.

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

      \item \code{read_BIN2R()}
      \itemize{
        \item Add suppurt for BIN-file version 8; thanks for the support by Kristina Thomsen from Risoe.

      }

      \item \code{Risoe.BINfileData-class}{
          \itemize{
            \item Add support for BIN-file version 8.


          }

      }

      \item \code{write_R2BIN()}{
          \itemize{
            \item Add support for BIN-file version 8.


          }

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

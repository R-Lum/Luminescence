\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.5.2 (, 2016)}{

  \subsection{Bugfixes and changes}{
   \itemize{


    \item \code{analyse_pIRIRSequence()}{

      \itemize{

          \item Fix graphical output for MET protocol.

      }

      }

      \item \code{plot_RLum.Analysis()}{

      \itemize{

          \item Argument 'norm' did not work for single curves, corrected (#261).

      }


      }

      \item \code{read_BIN2R()}

        \itemize{

          \item The record index was not re-calulated when the argument \code{duplicated.rm} was used.
          This caused some unwanted errors in rare cases (reported by Norbert Mercier).

        }

      }
  }
}


\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.6.4 (9th September 2016)}{



  \subsection{Bugfixes and changes}{
   \itemize{


      \item \code{analyse_baSAR()}

       \itemize{
          \item Fix problem that causes a function crash if an XLS-file was provided as input
          for the grain selection.
       }

      \item \code{analyse_pIRIRSequence()}

       \itemize{
          \item Account for a minor layout problem while plotting the combined growth curve (y-axis
          scaling was not sufficient)
       }


      \item \code{plot_AbanicoPlot()}

       \itemize{
          \item The relative and absolute standard deviation were mixed up in in the summary; fixed.
       }


      }

    }


}

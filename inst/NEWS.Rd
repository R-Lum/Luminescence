\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.9.0 (, 2018)}{

  \subsection{New functions}{
    \itemize{

        \item \code{plot_DRCSummary()} Summarise all dose-response curves from your SAR OSL
         analysis in one single plot.

    }
  }



  \subsection{Bugfixes and changes}{

  \itemize{

       \item \code{analyse_baSAR()}
        \itemize{
          \item Add support for \code{RLum.Analysis}-objects as input,
          \item correct odd message informing about removed aliquots that had been never removed,
          \item add new argument \code{distribution_plot} to allow a manual selection of the frequentist
          distribution plot output.

        }

       }
     }


}

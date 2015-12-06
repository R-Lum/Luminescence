\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.6.0 (,2016)}{


  \subsection{New functions}{
   \itemize{
     \item \code{plot_DetPlot()}: Plots the equivalent dose (De) in dependency of the chosen
       signal integral (cf. Bailey et al., 2003; #23).
   }
  }

 \subsection{Removed functions}{
   \itemize{

      \item \code{readBIN2R()}, replacement \code{read_BIN2R()},
      \item \code{readSPE2R()}, replacement \code{read_SPE2R()},
      \item \code{readXSYG2R()}, replacement \code{read_XSYG2R()},
      \item \code{writeR2BIN()}, replacement \code{write_R2BIN()},
      \item \code{get_RLum.Data.Curve()}, replacement \code{get_RLum()},
      \item \code{get_RLum.Data.Spectrum()}, replacement \code{get_RLum()},
      \item \code{get_RLum.Data.Image()}, replacement \code{get_RLum()},
      \item \code{get_RLum.Analysis()}, replacement \code{get_RLum()},
      \item \code{get_RLum.Results()}, replacement \code{get_RLum()},
      \item \code{set_RLum.Data.Curve()}, replacement \code{set_RLum()},
      \item \code{set_RLum.Data.Spectrum()}, replacement \code{set_RLum()},
      \item \code{set_RLum.Data.Image()}, replacement \code{set_RLum()},
      \item \code{set_RLum.Analysis()}, replacement \code{set_RLum()},
      \item \code{set_RLum.Results()}, replacement \code{set_RLum()},


  }
  }
  \subsection{Bugfixes and changes}{
   \itemize{


      \item \code{calc_FadingCorr()}
      \itemize{

        \item Table column headers now have all capital letters,
        \item minor performance improvements.
      }
   }
  }
 }




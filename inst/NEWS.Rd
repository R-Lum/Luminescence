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
      \item \code{Risoe.BINfileData2RLum.Data.Curve}, replacement \code{.Risoe.BINfileData2RLum.Data.Curve}
      The function was made an internal function, as this function was used only rarely by users,
      but often needed internally. The function and its functionally is still available under its new
      name but a lot of integrity checks had been removed from the code, which doubles the
      speed when transforming objects, but makes the function failing fast.


  }
  }
  \subsection{Bugfixes and changes}{
   \itemize{


      \item \code{calc_FadingCorr()}
      \itemize{

        \item Table column headers now have all capital letters,
        \item minor performance improvements.

      }

     \item \code{calc_OSLLxTxRatio()}
      \itemize{

        \item New argument \code{digits} introduced to allow direct rounding of the numbers.
      }

       \item \code{plot_RLum.Analysis()}
      \itemize{

        \item All \code{...} arguments can now be provided as \code{vector} or \code{list} to
        gain more control of the plot appearance,
        \item argument \code{abline} now works also in combination with \code{combine = TRUE} and
        allows to set ablines independently over different plots (#260),
        \item automatic axes labelling improved for \code{combine = TRUE},
        \item support further \code{...} argument \code{legend} (TRUE/FALSE).
      }

    \item \code{merge_RLum.Data.Curve()}
      \itemize{

        \item Support further merge operations: \code{median}, \code{var}, \code{sd}, \code{min}, \code{max}.
      }
   }
  }
 }




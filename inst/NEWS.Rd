\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.9.0 (, 2019)}{

  \subsection{New functions}{
    \itemize{

       \item \code{fit_OSLLifeTimes()} Fit and deconvolution of OSL lifetimes using off-time measurements, i.e.
       after the stimulation was switched off.

       \item \code{plot_DRCSummary()} Summarise all dose-response curves from your SAR OSL
       analysis in one single plot.

       \item \code{scale_GammaDose()} Scale the gamma dose rate considering layer-to-layer variations
       in soil radioactivity. Contributed by S. Riedesel and M. Autzen.

       \item \code{fit_ThermalQuenching()} Provide an easy option to fit thermal quenching data and
       determine the activation energy.

       \item \code{read_RF2R()} Add support for importing spatially resolved RF measurement results.
    }
  }


  \subsection{Bugfixes and changes}{

  \itemize{

       \item \code{analyse_FadingMeasurements()}

         \itemize{

           \item Enable support for data imported from a BIN/BINX-file.


        }

        \item \code{read_BIN2R()}{
          \item In the autofile recognition (only a path instead of a file is provided),
          sometimes the found BIN/BINX-files were imported twice. The corresponding code was simplified
          to avoid such errors in future.


       }

       }
     }


}

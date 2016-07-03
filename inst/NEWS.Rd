\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.6.1 (July 5th ,2016)}{



  \subsection{Bugfixes and changes}{
   \itemize{
    \item \code{analyse_baSAR()}
        \itemize{
          \item Add \code{n.chains} option to \code{method_control},
          \item add \code{inits} to \code{method_control} to set the RNG,
          \item add \code{thin} to \code{method_control} to control the monitoring,
          \item account for probable odd use input if the \code{XLS_file} is a \code{data.frame}
          (column 1 is of type \code{factor} instead \code{character}),
          \item function will not stop anymore if grain and position number do not match,
          \item adjust graphical ouptut,
          \item add new argument \code{model_baSAR} for user defined models,
          \item a lots of minor improvements.

      }

      \item \code{analyse_SAR.CWOSL()}
        \itemize{
          \item The argument 'sig0' was not passed to \code{calc_OSLLxTxRatio()} as stated
          in the description of the \code{...} argument. Fixed.
          \item Now also works for dose recovery measurement data where the dose of the
          first OSL signal is not zero. Previously the function crashed with an arbitrary error.
          Now it checks if the natural dose is 0, and if not, overwrites it whilst warning
          the user about this.
      }



      \item \code{calc_OSLLxTxRatio()}
        \itemize{
          \item \code{NaN} values now are automatically changed to 0. With regard to the underlying equations
          this is mathematically not correct, as the function returns \code{NaN} in cases where 0 is divided by 0,
          but \code{NaN} makes no sense for the further data processing and is therefore not meaningful,
          \item object \code{call} is moved into the info slot.

      }

      \item \code{merge_RisoeBINfileData()}
        \itemize{
          \item Account for 'implicit list embedding of S4 objects is deprecated' warning by changing
          the code (no visible user effect, apart from no visible warning anymore).
      }


     \item \code{plot_AbanicoPlot()}
        \itemize{
          \item Function got a new argument \code{line.lty} to controll the appearance of additional
          lines.

      }

       \item \code{plot_GrowthCurve()}
        \itemize{
          \item Move \code{call} output in the \code{RLum.Results} output element \code{list},
          \item if less than 3 regeneration points are provided the function will not stop and
          return \code{NULL}, but set \code{fit.method = "LIN"}, return a message and continue.
          With this the user gets at least a plot,
          \item allow linear fitting with only 1 dose point if the curve is forced through the origin,
          \item function now returns values even the error 'figure margins too large' shows up.

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

        \item \code{verify_SingleGrainData()}{
          \itemize{
            \item Add argument \code{plot} to function and with this a graphical feedback
            (default is \code{FALSE}),
            \item enhance manual to better describe the applied selection method.



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

  \subsection{Internals}{
   \itemize{
      \item Add package \code{coda} as suggested package.

    }

    }


  }

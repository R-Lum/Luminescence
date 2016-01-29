\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.6.0 (,2016)}{


  \subsection{New functions}{
   \itemize{
     \item \code{plot_DetPlot()}: Plots the equivalent dose (De) in dependency of the chosen
       signal integral (cf. Bailey et al., 2003; #23).
   }
  }

 \subsection{Changes in S4-objects and related methods}{
   \itemize{

   \item{\code{set_RLum}}
    \itemize{
        \item The function \code{set_RLum()} gained the new argument \code{.uid} which is by default
        presetting the slot with the same name using the internal function \code{.create_UID()}.
        In the previous package version this slot of the RLum-object was filled automatically as well,
        but the way it was done was a little bit semitransparent and a different function was used.

    }

    \item{\code{RLum}}
      \itemize{
        \item The slot \code{.uid} from objects of this class has now the prototype \code{NA_character_}.
        The unique id is only set if the corresponding \code{set_RLum} function is called, which should be,
        however, the usual case,
        \item all inhertied object classes of the class \code{RLum} gained a new slot called \code{info},
        with this a more consistent workflow with the objects should be possible, before only objects
        of the class \code{RLum.Data} had such slot.

      }


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


      \item \code{analyse_SAR.CWOSL()}
      \itemize{

        \item The function will not stop with an error if no 'OSL' or 'IRSL' record is detected,
        but return \code{NULL} and print a warning,
        \item under really rare circumstances, many recycling points and recuperation points the
        function could crash (fixed).

      }

      \item \code{apply_CosmicRayRemoval()}
      \itemize{

          \item Argument \code{silent} renamed to \code{verbose},
          \item new argument \code{method.Pych.threshold_factor} added to have a better control
          for the threshold value used for the removal,
          \item new argument \code{MARGIN} added to allow the application of the methods along
          the wavelength or the time.


      }


      \item \code{calc_FadingCorr()}
      \itemize{

        \item Table column headers now have all capital letters,
        \item minor performance improvements.

      }

     \item \code{calc_OSLLxTxRatio()}
      \itemize{

        \item New argument \code{digits} introduced to allow direct rounding of the numbers,
        \item add argument \code{sig0} to allow for an extra component of error.
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

    \item \code{plot_GrowthCurve()}
      \itemize{

        \item If the input data contain a least one infinite value the function will not crash
        any longer, but return \code{NULL} (#263, Norbert Mercier),
        \item check for insufficient values in the data set and return \code{NULL} (before the function crashed).

      }

    \item \code{merge_RLum.Data.Curve()}
      \itemize{

        \item Support further merge operations: \code{median}, \code{var}, \code{sd}, \code{min}, \code{max}.
      }


    \item \code{read_BIN2R()}
      \itemize{

        \item Handling of corrupt BIN-files improved, the functions tries to import whatever possible,
        \item the argument \code{n.records} was not respected under rare circumstances.
      }

      \item \code{Risoe.BINfileData2RLum.Analysis()}
      \itemize{

        \item Single grain data had not been converted correctly to \code{RLum.Analysis} objects.
        Each grain within a position should represent one \code{RLum.Analysis} object with a set
        of curve. This was so far not the case; corrected.
      }

   }
  }

  \subsection{Internals}{
     \itemize{

        \item New internal function \code{.create_UID()} added to create the unique identifier for
        RLum-objects. The new function reduces significantly the overhead while producing the objects.

        \item Package 'digits' removed from the dependency list and the functionality (creating a
        unique identifier for RLum-objects) was implemented using a self-written function.


     }
   }


}




\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.6.0 (,2016)}{


  \subsection{New functions}{
   \itemize{
    \item \code{analyse_baSAR()}: Bayesian models (baSAR) applied on luminescence data
    \item \code{model_LuminescenceSignals()}: Wrapper for the function \code{model_LuminescenceSignals()}
    from the package \code{RLumModel} by Johannes Friedrich, Sebastian Kreutzer and Christoph Schmidt
     \item \code{plot_DetPlot()}: Plots the equivalent dose (De) in dependency of the chosen
       signal integral (cf. Bailey et al., 2003; #23).
     \item \code{verify_SingleGrainData()}: Verify single grain data sets and check for invalid grains, i.e. zero light level grains
   }
  }

 \subsection{Changes in S4-objects and related methods}{
   \itemize{

   \item{\code{set_RLum}}
    \itemize{
        \item The function \code{set_RLum()} gained the new argument \code{.uid} which is by default
        presetting the slot with the same name using the internal function \code{.create_UID()}.
        In the previous package version this slot of the RLum-object was filled automatically as well,
        but the way it was done was a little bit semitransparent and a different function was used,
        \item the performance of the function was improved significantly.

    }

     \item{\code{get_RLum}}

       \itemize{
        \item Methods for \code{RLum.Analysis} and \code{RLum.Results} objects updated, so that
        they can handle the new info slot,
        \item method for \code{RLum.Data.Curve} objects new reacts more relaxed for wrong info
        object names. Instead of a function error, \code{NULL} is returned. This makes handling
        in loops easier.


       }

    \item{\code{structure_RLum}}

    \itemize{
      \item The method gained a \code{\dots} argument for further passing information to the specific
      class methods,
      \item The method for the class \code{RLum.Analysis} gained the new argument \code{fullExtent}
      to extent the returned data frame to all info elements. This makes it possible to investigate
      \code{RLum.Analysis} objects in great detail and subset them efficiently.


    }



    \item{\code{RLum}}
      \itemize{
        \item The slot \code{.uid} from objects of this class has now the prototype \code{NA_character_}.
        The unique id is only set if the corresponding \code{set_RLum} function is called, which should be,
        however, the usual case,
        \item all inherited object classes gained a new slot called \code{.pid}. This slot is reserved
        for storing parent ids so that objects can be nested at free will. Corresponding import function
        had been adapted in the way the pid will become an inherited uid from the \code{RLum.Analysis} object,
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
      \item \code{get_structure.RLum.Analysis()}, replacement \code{structure_RLum},
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

    \item \code{merge_RLum.Data.Curve()}
      \itemize{

        \item Support further merge operations: \code{median}, \code{var}, \code{sd}, \code{min}, \code{max}.
      }

    \item \code{plot_GrowthCurve()}
      \itemize{

        \item If the input data contain a least one infinite value the function will not crash
        any longer, but return \code{NULL} (#263, Norbert Mercier),
        \item check for insufficient values in the data set and return \code{NULL} (before the function crashed).

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

         \item \code{plot_RLum.Data.Curve()}
      \itemize{
        \item Fix seldom bug that causes unexpected function behaviour if \code{xlab} or \code{ylab}
        exist as objects on the global environment (#265).

      }


    \item \code{read_BIN2R()}
      \itemize{

        \item Handling of corrupt BIN-files improved, the functions tries to import whatever possible,
        \item the argument \code{n.records} was not respected under rare circumstances,
        \item in course of this (corrupt BIN-files, not respected \code{n.records}) the output data frame
        was longer than the list of count values; corrected.
      }

      \item \code{Risoe.BINfileData2RLum.Analysis()}
      \itemize{

        \item Single grain data had not been converted correctly to \code{RLum.Analysis} objects.
        Each grain within a position should represent one \code{RLum.Analysis} object with a set
        of curve. This was so far not the case; corrected,
        \item significant performance improvements, especially for big Risoe.BINfileData2RLum.Analysis()
        objects.
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




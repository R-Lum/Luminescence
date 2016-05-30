\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.6.0 (May 30th ,2016)}{


  \subsection{New functions}{
   \itemize{
    \item \code{analyse_baSAR()}: Function to allow the application of Bayesian models (baSAR) on luminescence data
    based on the contribution by Combes et al. (2015)
    \item \code{app_RLum()}: Wrapper for the function \code{app_RLum()} from the package
    \code{RLumShiny} by Christoph Burow
    \item \code{bin_RLum.Data}: Function for binning in \code{RLum.Data} objects, e.g., binning of
    \code{RLum.Data.Curve} objects
    \item \code{calc_FastRatio(): Function to calculate the fast ratio of CW-OSL
    curves after Durcan & Duller (2011)}
    \item \code{calc_ThermalLifetime(): Function to calculate thermal lifetimes and their errors of a charge
    using the Arrhenius equation}
    \item \code{model_LuminescenceSignals()}: Wrapper for the function \code{model_LuminescenceSignals()}
    from the package \code{RLumModel} by Johannes Friedrich, Sebastian Kreutzer and Christoph Schmidt
    \item \code{plot_DetPlot()}: Plots the equivalent dose (De) in dependency of the chosen
    signal integral (cf. Bailey et al., 2003; #23).
    \item \code{report_RLum()}: Function to create a HTML report of RLum objects (or any other), containing
    the complete structure and content of the provided object. Requires the packages 'rmarkdown' and 'pander'.
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
        in loops easier,
        \item the function crashed under very rare circumstances if a provided list with RLum-objects
        contained \code{RLum.Analysis} objects with zero records; corrected.


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
      \item \code{length_RLum.Analysis()}, replacement \code{length_RLum()},
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

       \item \code{analyse_IRSAR.RF()}
      \itemize{
        \item Add self call to the function, i.e. as input alternatively now a list of \code{RLum.Analysis}
        objects can be provided for a higher efficiency,
        \item the test parameter \code{intersection_ratio} was wrongly calculated; corrected,
        \item correct wrong shift if the option \code{RF_nat.lim} was used,
        \item internal parameter added that allows to show the applied fit, while using \code{method = "SLIDE"},
        \item add argument \code{plot_reduced} for a better plot control,
        \item argument \code{sequence.structure} renamed to \code{sequence_structure},
        \item support further plot arguments via \code{...},
        \item \code{method.control = list(trace = TRUE)} now shows the control graph as expected for the method
        \code{SLIDE},
        \item argument \code{test_parameter} renamed to \code{test_parameters}.

      }


      \item \code{analyse_SAR.CWOSL()}
      \itemize{

        \item The function will not stop with an error if no 'OSL' or 'IRSL' record is detected,
        but return \code{NULL} and print a warning,
        \item under really rare circumstances, many recycling points and recuperation points the
        function could crash (fixed).
        \item Fixed a bug in the evaluation of the rejection criteria where the status of the palaedose
        error criterion was returned as 'OK' even though it was not (Thanks to Annette Kadereit)
        function could crash (fixed),
        \item add support for testdose error as rejection criteria,
        \item recuperation rate was not shown properely in the rejection criteria plot,
        \item instead of showing failed, the polygon borders are now plotted in red in the rejection criteria plot,
        \item if a list of RLum-objects was provided the rejection criteria could not be change any more
        fixed (thanks to Andrea Junge),
        \item every rejection criterium now allows the input value \code{NA}, in this case values are calculated,
        but not considered as rejection criterium, i.e. the RC.Status will be always \code{'OK'},
        \item minor graphical adjustments.


      }

      \item \code{apply_CosmicRayRemoval()}
      \itemize{

          \item Argument \code{silent} renamed to \code{verbose},
          \item new argument \code{method.Pych.threshold_factor} added to have a better control
          for the threshold value used for the removal,
          \item new argument \code{MARGIN} added to allow the application of the methods along
          the wavelength or the time,
          \item set default method to \code{smooth}.


      }

      \item \code{calc_AliquotSize()}
      \itemize{

        \item Fixed a bug in the Monte Carlo Simulation that occured when estimating the number of grains for aliquots
        with diameters larger than 9.8 mm. The user is, however, now warned that the function assumes common aliquot
        discs not larger than 9.8 mm when providing larger sample diameters.
      }

      \item \code{calc_FadingCorr()}
      \itemize{

        \item Table column headers now have all capital letters,
        \item minor performance improvements,
        \item for very large standard errors uniroot failed, causing a function break; corrected (#270),
        \item if uniroot is not able to solve the equation during the MC runs the function will not stop
        anymore with an error.

      }


      \item \code{calc_MinDose()}
      \itemize{

        \item The user is now warned if the final estimate of \code{mu} is larger than \code{gamma},
        \item New \code{...} arguments \code{gamma.lower, gamma.upper, sigma.lower, sigma.upper, p0.lower, p0.upper, mu.lower, mu.upper}
        that can be used to override the default boundary values,
        \item New argument \code{level} to allow specifying the required confidence level (defaults to 0.95).

      }

     \item \code{calc_OSLLxTxRatio()}
      \itemize{

        \item New argument \code{digits} introduced to allow direct rounding of the numbers,
        \item add argument \code{sig0} to allow for an extra component of error.
      }

        \item \code{calc_Statistics()}
      \itemize{

        \item MCM-option added.
      }


      \item \code{extrac_IrradiationTimes()}
      \itemize{

        \item Function now supports the self call mode, i.e. a \code{list} of \code{RLum.Analysis}
        objects can be provided as input and the function iterates of this list.

      }



      \item \code{fit_LMCurve()}
       \itemize{

          \item Fix bug that caused a function break if data set contained 0 time values but a
          log-scale was wanted; now the \code{xlim} is adjusted and a warning is given.



     }

         \item \code{merge_RLum.Data.Curve()}
      \itemize{

        \item Support further merge operations: \code{median}, \code{var}, \code{sd}, \code{min}, \code{max}.
      }


      \item \code{plot_AbanicoPlot()}
      \itemize{

        \item Fixed a bug where the function failed to get input data from
        \code{RLum.Results} objects,
        \item new argument \code{interactive} to produce an interactive plot
        using the JavaScript graphing library 'plotly.js' (requires the
        'plotly' package),
        \item notes about consequences of \code{log.z = TRUE} updated,
        \item boxplot option added,
        \item summary keywords homogenised,
        \item if data contain 0 dose values, the values are now positively shifted by 0.01, but only
        if \code{log.z == TRUE}.

      }

     \item \code{plot_DRTResults()}
       \itemize{

       \item If no given dose is provided the horizontal 'ab-lines' (1.0 and +/- 0.1) are no
       longer plotted,
       \item default value for the argument \code{given.dose} is now \code{NULL}.

    }

    \item \code{plot_GrowthCurve()}
      \itemize{

        \item If the input data contain a least one infinite value the function will not crash
        any longer, but return \code{NULL} (#263, Norbert Mercier),
        \item check for insufficient values in the data set and return \code{NULL} (before the function crashed),
        \item data frame also exports now error of the D0-values based on the MC runs for the De error,
        \item the option \code{fit.includingRepeatedRegPoints = FALSE} was not respected properly
        under all circumstances; corrected,
        \item under rather rare circumstances the function became trapped in an endless mode as the
        function \code{nls} is trapped in such mode if only two points are provided. The minimum
        of required data points for \code{EXP} was set to three,
        \item homogenise terminal output.

      }

    \item \code{plot_Histogram()}
      \itemize{

        \item New argument \code{interactive} to produce an interactive plot
        using the JavaScript graphing library 'plotly.js' (requires the
        'plotly' package).

      }

       \item \code{plot_KDE()}
      \itemize{

        \item Boxplot option added,
        \item Depreciated arguments removed (centrality, dispersion, weights, polygon.col),
        \item Colour specification simplified,
        \item Summary keywords homogenised,
        \item Layout functionality implemented.


      }


             \item \code{plot_RLum.Analysis()}
      \itemize{

        \item All \code{...} arguments can now be provided as \code{vector} or \code{list} to
        gain more control of the plot appearance,
        \item argument \code{abline} now works also in combination with \code{combine = TRUE} and
        allows to set ablines independently over different plots (#260),
        \item automatic axes labelling improved for \code{combine = TRUE},
        \item support further \code{...} argument \code{legend} (TRUE/FALSE),
        \item add support for argument \code{smooth} for \code{RLum.Data.Curve} objects (see
        entry for \code{plot_RLum.Data.Curve},
        \item fix minor bug introduced for the acceptence of the argument \code{lty}.
      }

         \item \code{plot_RLum.Data.Curve()}
      \itemize{
        \item Fix seldom bug that causes unexpected function behaviour if \code{xlab} or \code{ylab}
        exist as objects on the global environment (#265),
        \item add argument \code{smooth} to provide an automatic curve smoothing for plotting based
        on a running mean.

      }

            \item \code{plot_RLum.Data.Spectrum()}
      \itemize{
        \item Added \code{matrix} as allowed input,
        \item add new argument \code{limit_counts} to further the counts to a maximum value (the value
        provided), this might be especially helpful in case of TL spectra,
        \item the argument \code{optical.wavelength.colours} was not working in all cases sufficiently; corrected,
        \item minor graphical adjustments,
        \item the 3D plot is now produced using the package \code{plotly} which allows an interactive
        visualisation. The new plot type added is called \code{interactive}, the suggestion of the
        package \code{rgl} has been removed with this change.


      }

            \item \code{plot_ViolinPlot()}
      \itemize{

        \item Fixed a bug where the function failed to get input data from
        \code{RLum.Results} objects,
        \item argument \code{na.rm} got default value \code{TRUE},
        \item function is now a little bit more robust against odd input values.

      }

    \item \code{read_BIN2R()}
      \itemize{

        \item Handling of corrupt BIN-files improved, the functions tries to import whatever possible,
        \item the argument \code{n.records} was not respected under rare circumstances,
        \item in course of this (corrupt BIN-files, not respected \code{n.records}) the output data frame
        was longer than the list of count values; corrected,
        \item in some cases the grain number was not imported correctly; fixed,
        \item new argument \code{verbose} added,
        \item minor performance improvements.
      }


     \item \code{read_BIN2R()}
      \itemize{
          \item Behaviour while using the option \code{import = TRUE} and \code{fastForward = TRUE}
          have been improved. If both arguments are combined now only a single data frame with all information
          is returned,
          \item support more arguments in the function self-call mode (argument \code{file} is of type \code{list}),
          \item function gained the new argument \code{pattern}, which can be used to filter imported BIN-files by name,
          \item if the field \code{FNAME} is totally empty in the BIN-file the function files this field with the
          name of the BIN-file itself. This can be rather useful in particular cases,
          \item function gained a new argument \code{zero_data.rm}, which is preset to \code{TRUE}
          and removes data with zero data (no count values) errorneously produced during the measurement.

      }

      \item \code{Risoe.BINfileData2RLum.Analysis()}
      \itemize{

        \item Single grain data had not been converted correctly to \code{RLum.Analysis} objects.
        Each grain within a position should represent one \code{RLum.Analysis} object with a set
        of curve. This was so far not the case; corrected,
        \item significant performance improvements, especially for big Risoe.BINfileData2RLum.Analysis()
        objects,
        \item if the argument \code{pos} or \code{grain} contains invalid entries the function will
        not longer return an error. The invalid values are discarded and the function
        tries to import values for valid positions or grains only,
        \item the function gained the new argument \code{dtype} to further limit the converted
        data types during the conversion (#269)
      }

      \item \code{write_R2BIN()}

      \itemize{
        \item Output for \code{version = "06"} was a corrupt BINX-file ... fixed.


      }

   }
  }

  \subsection{Internals}{
     \itemize{

        \item New internal function \code{.create_UID()} added to create the unique identifier for
        RLum-objects. The new function reduces significantly the overhead while producing the objects.

        \item Package 'digits' removed from the dependency list and the functionality (creating a
        unique identifier for RLum-objects) was implemented using a self-written function.

        \item Package 'rgl' was removed from suggests and replaced by the package 'plotly'.

        \item Package 'RLumModel' added as suggested package.

        \item Package 'RLumShiny' added as suggested package.

        \item Package 'rmarkdown' added as suggested package.

        \item Package 'pander' added as suggested package.

        \item Package 'rstudioapi' added as suggested package.

        \item Package 'rjags' added as suggested package.

        \item New S3 generic \code{bin} implemented, this method dispatches to the S4 method
        \code{bin_RLum.Data} for the \code{RLum.Data} objects.

        \item Add S3 generics to support all combinations for the function \code{is}, e.g.,
        \code{is.RLum.Data.Curve}.

        \item Add S3 generic for method \code{subset} of objects of type \code{Risoe.BINfileData}.
        This is a little bit inconsistent, however, still a lot of users are using this object created
        by the function \code{read_BIN2R(..., fastForward = FALSE)} and this new method (subset)
        tremendously simplifies the work with this object.


     }
   }

}

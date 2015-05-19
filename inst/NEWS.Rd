\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.4.4 (19th, May 2015)}{


  \subsection{Changes S4-objects, methods, classes}{
   \itemize{

      \item \code{merge_RLum.Results()} clear break implemented when objects cannot be merged.

  }
  }

 \subsection{Bugfixes and changes}{


  \itemize{

  \item \code{analyse_SAR.CWOSL()}

  \itemize{
    \item Add support for \code{...} argument for \code{calc_OSLLxTxRatio()} arguments
    \code{background.count.distribution} and \code{sigmab}.

   }

  \item \code{Analyse_SAR.OSLdata()}

  \itemize{
    \item Add support for \code{...} argument for \code{calc_OSLLxTxRatio()} arguments
    \code{background.count.distribution} and \code{sigmab} similar as done for the function \code{analyse_SAR.CWOSL()}.

   }

  \item \code{calc_OSLLxTxRatio()}

  \itemize{
    \item 'LxTx.Error' could become negative, which is not meaningful by definition (corrected),
    \item minor internal changes to allow for \code{sigmab = NULL}, which has the same
    effect as a missing \code{sigmab} argument.

   }

  \item \code{calc_FadingCorr()}

  \itemize{
    \item Argument \code{seed} added to allow to recreate previously determined results by
    using a similar seed of the R random number generator,
    \item argument\code{n.MCruns} no allows an option \code{'auto'}. Using this mode the error
    estimation is continued until a stable error is reached,
    \item added argument \code{txtProgressBar},
    \item output table enhanced by input options and values resulting from the new arguments.

   }


    \item \code{calc_Statistics()}

    \itemize{
    \item Wrongly assigned output values corrected.

    }

  \item \code{fit_CWCurve()}

  \itemize{
    \item 'component.contribution.matrix' is now returned as list, as otherwise the merging
    of the objects will break.

   }

  \item \code{fit_LMCurve()}

  \itemize{
    \item 'component.contribution.matrix' is now returned as list, as otherwise the merging
    of the objects will break.

   }

   \item \code{plot_GrowthCurve()}

  \itemize{
    \item Function crashed with unclear error if the number of regenerations points was <=3), fixed,
    \item fitting is now allowed with at leat two regenerations points.

   }

   \item \code{plot_AbanicoPlot()}

  \itemize{
      \item Argument \code{na.exclude} changed to \code{na.rm},
      \item add sample group specific colours for rugs,
      \item summary and legend placement bugs for rotated abanico plot fixed,
      \item labelling errors fixed,
      \item argument \code{frame} added to abanico plot,
      \item adjustment of colours for polygon and bar in abanico plot,
      \item summary keyword list extended and corrected.


   }

  \item \code{plot_Histogram()}

  \itemize{
      \item Argument \code{na.exclude} changed to \code{na.rm},
      \item summary keyword list extended and corrected.

   }

    \item \code{plot_KDE()}

  \itemize{
      \item Argument \code{na.exclude} changed to \code{na.rm},
      \item argument weights removed,
      \item summary keyword list extended and corrected.

   }

       \item \code{plot_RadialPlot()}

  \itemize{
      \item Argument \code{na.exclude} changed to \code{na.rm},
      \item summary keyword list extended and corrected.

   }



  \item \code{plot_RLum.Analysis()}

  \itemize{
    \item Add \code{...} argument support \code{legend.pos = "ouside"} and \code{legend.col} for
    argument \code{combine = TRUE}

   }

  \item \code{writeR2BIN()}

  \itemize{
    \item Function broke if information in SEQUENCE, COMMENT or USER was always NA. Corrected.

  }

   }

}

\subsection{Internals}{
   \itemize{
    \item Switch from inlinedocs to roxygen2 to build the manuals (#193),
    \item remove dependency to package 'sp'.

   }
  }

}

\section{Changes in version 0.4.3 (17th, April 2015)}{

\subsection{Note on the update}{

  \itemize{
  \item This is an unplanned update but it was necessary as with the last package version (0.4.2)
  unfortunately we introduced (again) a bug in the error calculation routine of the function plot_GrowthCurve().
  Due to this the error of the resulting De became unrealistically small.


}}

  \subsection{Bugfixes and changes}{
   \itemize{


      \item \code{calc_OSLLxTxRatio()}
       \itemize{
          \item Support for \code{RLum.Data.Curve} objects implemented.
       }

      \item \code{calc_MinDose()}
       \itemize{
          \item Fixed a false argument in \code{parallel::makeCluster()} that unintentionally required the 'snow' package.
       }


      \item \code{analyse_SAR.CWOSL()}
       \itemize{
          \item If the threshold for the rejection criteria was reached due an error this information
          was not properly stored in the output object (i.e. every time 'RC.Status' was set to 'OK' instead of 'FAILED');
          corrected (#180).
       }


      \item \code{extract_IrradiationTimes()}
       \itemize{
          \item Argument \code{file.XSYG} replaced by \code{object} for the sake of argument consistency on the package,
          \item allow input for \code{RLum.Analysis} object. In such cases the arguments \code{file.BINX}
          and \code{recordType} are ignored. This mode allows for more flexibility in applying this function,
          \item new argument \code{compatibility.mode} to trigger the binary write output of the BIN/BINX-file,
          \item additionally now the time between the steps is exported (\code{TIMESINCELAST.STEP}).

          }


      \item \code{fit_CWVCurve()}
       \itemize{
          \item The function broke with an error under rare circumstances when no fit was made (reported by Alexandra Rowell); corrected,
          \item change start parameter estimation to better account for the input data,
              this hopefully further stabilise the fitting (thanks to Alexandra Rowell),
          \item more proper re-scaling of the graphical output if no fit could be produced,
          \item new argument add \code{fit.method} to allow using the function \code{nlsLM} from the
          \code{minpack.lm package} and with this the Levenberg-Marquardt algorithm,
          \item argument \code{cex.global} replaced by \code{cex} and moved to the \code{...} argument,
          \item argument \code{output.plot} changed to \code{plot},
          \item changing the way how warnings are surpressed.

        }


      \item \code{plot_Histogram()}
       \itemize{
          \item Argument 'breaks' was without function due to a coding error. Corrected (#217).,
          \item argument 'breaks' removed from the argument list, however, it remains supported via the \code{'...'}
          argument.
       }

      \item \code{plot_GrowthCurve()}
       \itemize{
          \item Error estimation using the Monte Carlo runs for the
          fitting function \code{fit.method = "EXP+LIN"} and \code{fit.method = "EXP+EXP"} results in unrealistic values, as the Monte Carlo simulation
          was fed by mistake with wrong input variables (introduced in version 0.4.2 with the uniroot function) ... fixed,
          \item the function broke if the uniroot algorithm was not be able to find a solution within the given ranges, fixed. Now the De will become
          NA insteadly and the function will not stop,
          \item error messages coming from the fitting process itself are now surpressed.
       }


    \item \code{plot_RLum.Data.Spectrum()}
       \itemize{
          \item Colour handing changed towards a more logical behaviour for \code{plot.type = 'single'}.
          The colour now changes only the colour of the points and the argument \code{rug} is now recognised also
          by this plot type. Automatic wavelength colouring for points for in this plot type has been removed.
          \item same changes are made for  \code{plot.type = 'multiple.lines'},
          \item instead of just plotting a new x-axis when the argument \code{xaxis.energy = TRUE} is used,
          the spectrum (intensity) is now proper re-calculated (#192),
          \item minor code polishing.

       }

      \item \code{plot_RLum.Anaysis()}
       \itemize{
          \item Setting the argument \code{combine = TRUE} for 'OSL' and 'IRSL' caused an error related
          to the missing argument 'curve.transformation'; corrected.,
          \item colour handling for setting \code{combine = TRUE} improved. It now works as for \code{plot.default},
          \item support for \code{xlim} and \code{ylim} in the non-combined mode,
          \item combine mode (\code{combine = TRUE}) is no more limited to an input object of similar curves. Now
          the plot function tries to combine curves of similar type automatically.

       }


      \item \code{readBIN2R()}
       \itemize{
          \item Support for BIN/BINX-file version 07 added thanks to the support by Torben Lapp (#215).
       }

       \item \code{readXSYG2R()}
       \itemize{

          \item Info objects from the sequence header like 'name' and 'position' are now be additionally
          transferred to each single output object. With this the information is kept within the curve object.


       }

      \item \code{writeR2BIN()}
       \itemize{
          \item Support for BIN/BINX-file version 07 added thanks to the support by Torben Lapp (#215),
          \item new argument \code{compatibility.mode} to avoid position numbers >48 as this caused
          errors using external software like the Analyst. The old version number is kept and added
          in the comment field.
       }


    }
  }

  \subsection{Internals}{
   \itemize{

      \item Description on the class \code{Risoe.BINfileData} updated in accordance with the implemented
      changes on the import (no change in the S4-object class chart),

      \item added dependency to package \code{minpack.lm} to support the Levenberg-Marquardt algorithm
      for fitting in the function \code{fit_LMCurve()}


    }
  }

}


\section{Changes in version 0.4.2 (March 23th, 2015)}{

  \subsection{New functions}{

  \itemize{

  \item \code{calc_IEU()}: Apply the internal-external-uncertainty (IEU) model after Thomsen et al. (2007)
  to a given De distribution

  \item \code{extract_IrradiationTimes()}: Extract irradiation times from an XSYG file

  }}

    \subsection{Changes S4-objects, methods, classes}{
   \itemize{


    \item \code{RLum.Analysis-class}:


     \itemize{

    \item Fix: Argument \code{keep.object == TRUE} was not respected for every case. After using
    the function \code{get_RLum.Analysis()} the object just contained a single object. In such
    cases only the object itself, but no \code{RLum.Analysis} object was returned (#200).

    }

   \item \code{RisoeBINfileData-class}:


     \itemize{

    \item Slot \code{.RESERVED} added to allow transferring undocumented raw binary values to a
    new BIN/BINX-file. For this all reserved bytes are just copied and pasted into the new slot
    \code{.RESERVED} using the function \code{readBIN2R()}. Using the function \code{writeR2BIN()} transfers
    the values unchanged to the new BIN/BINX-file. Allc corresponding functions have been updated in accordance
    (\code{readBIN2R()}, \code{writeR2BIN()} and \code{merge_Risoe.BINfileData()}. Other functions are not
    affected as they can simply ignore the new slot.

    }


  }}


  \subsection{Bugfixes and changes}{
   \itemize{

   \item \code{analyse_IRSAR.RF()}\cr

  \itemize{

  \item Function now checks the number of channels in each curve. If the number of the channels in the
  first curve is > than in the 2nd curve the execution of the function is stopped,
  \item add error estimation using bootstraping and MC loops for \code{method = "SLIDE"},
  \item add scaled KDE for distribution of De,
  \item the functions no longer stops with an error if a undefined \code{method} was chosen. Instead
  a warning message is shown and the available curves are shown in a plot anyway,
  \item the labelling for the residuals is replaced by 'E' to avoid overplotting,
  \item new argument \code{rejection.criteria} provided to allow for setting rejection criteria. The
  argument works similar as for other \code{analyse_...} functions. Details are provided in the manual,
  \item speed up caculation time for \code{method = "SLIDE"} by re-writing the sliding process
  as C++ function. It does not the affect readability of the R-code at all, as just the sum of the residual
  squares is calculated in this way, i.e. the calculations are simple, but now much faster.

  }


  \item \code{analyse_SAR.CWOSL()}

  \itemize{

  \item For the case that no recycling ratio and no recuperation rate could be calculated the
  functions chrashed. This has bee corrected.


  }

   \item \code{analyse_pIRIRSequence()}

  \itemize{

  \item For the case that no recycling ratio and no recuperation rate could be calculated the
  functions chrashed. This has bee corrected; in accordance with the similar behaviour in the function
  analyse_SAR.CWOSL().


  }

  \item \code{apply_CosmicRayRemoval()}

  \itemize{

  \item Argument \code{method.Pych.histogram.plot} replaced by the more general and consistent
  argument \code{plot},
  \item minor code polishing.


  }



    \item \code{calc_FadingCorr()}

  \itemize{

  \item Change the way the equation is solved using the R function \code{uniroot}. This markedly
  speeds up the computation process,
  \item now additionally an \code{RLum.Results} object is returned,
  \item minor code polishing.
  }

  \item \code{calc_MinDose()}
  \itemize{

  \item Major changes in terms of reliablity and perfomance to the optimisation of the log
  likelihood function. Amongst minor fixes these include:
  \item Optimiser of the log likelihood function changed from 'optim' to 'nlminb' and
  switch from 'L-BFGS-B' method to default PORT routine,
  \item Removed parameter: \code{boundaries}
  \item Boundaries for the model parameters are now hard-coded and suffice all input data,
  \item Warning messages are now suppressed unless \code{debug=TRUE},
  \item Improved error handling and fallback for profiling the log likelihood function,
  \item Start value for gamma and mu are changed to lower quartile of the dose values,
  \item fixed an error in the documentation that prevented the 'value' section from being displayed
  \item additional information on bootstrapping added in 'details' section,
  \item new example added,
  \item add multicore support for bootstrapping,
  \item new arguments: \code{multicore} (logical), \code{cores} (numeric).


  }

  \item \code{calc_Statistics()}
  \itemize{

  \item argument \code{na.rm} added (#198).

  }

   \item \code{CW2pHMi()}
  \itemize{

  \item Functions was rather inflexible selecting the allowed curves, e.g. \code{"OSL"} worked,
  \code{"OSL (UVVIS)"} not. Corrected.

  }

     \item \code{CW2pLMi()}
  \itemize{

  \item Functions was rather inflexible selecting the allowed curves, e.g. \code{"OSL"} worked,
  \code{"OSL (UVVIS)"} not. Corrected.

  }

       \item \code{CW2pLM()}
  \itemize{

  \item Functions was rather inflexible selecting the allowed curves, e.g. \code{"OSL"} worked,
  \code{"OSL (UVVIS)"} not. Corrected.

  }

  \item \code{CW2pPMi()}
  \itemize{

  \item Functions was rather inflexible selecting the allowed curves, e.g. \code{"OSL"} worked,
  \code{"OSL (UVVIS)"} not. Corrected.

  }



 \item \code{merge_Risoe.BINfileData()}

  \itemize{

  \item Support for the changes in the corresponding S4-class are implemented.

  }

   \item \code{plot_AbanicoPlot()}

   \itemize{

   \item Missing summary keywords \code{"kdemax"}, \code{"skewness"} and \code{"kurtosis"} added
   (#205)

   }


 \item \code{plot_Histogram()}

   \itemize{

   \item Missing summary keywords \code{"kdemax"}, \code{"skewness"} and \code{"kurtosis"} added
   (#205)

   }


   \item \code{plot_KDE()}

   \itemize{

   \item Missing summary keywords \code{"kdemax"}, \code{"skewness"} and \code{"kurtosis"} added
   (#205)

   }

   \item \code{plot_GrowthCurve()}


   \itemize{

    \item Fix an error that causes a function chrash if the input data.frame (argument \code{sample}) comprises
    row names. Now all pre-set row names are removed automatically,

    \item using the option \code{fit.weights = FALSE} in combination with \code{fit.method = "LIN"} caused an error. Corrected,

    \item avoid confusing warning messages in case \code{fit.weights} was set to \code{FALSE},

    \item fix an error that causes a chrash for the combination \code{fit.method = "LIN"} and error column just comprises 0,

    \item argument \code{main} was not working as expected, corrected. Furthermore the argument is removed from the
    argument list, but is still available via \code{...},

    \item the slow self-written approach to calculate the De for \code{fit.method = "EXP+LIN"} and
    \code{fit.method = "EXP+EXP"} has been replaced by the function \code{uniroot}. This markedly reduces
    the computation times for the MC simulation (partly addresses issue #100)

   }


  \item \code{plot_RadialPlot()}

   \itemize{

   \item Missing summary keywords \code{"kdemax"}, \code{"skewness"} and \code{"kurtosis"} added
   (#205)

   }

   \item \code{plot_RLum.Analysis()}


   \itemize{

    \item Fix an error appeared for \code{combine = TRUE} if only one single object was in the \code{RLum.Analysis} object (#199),
    \item added logical argument \code{plot.single}. In fact this just resets \code{ncols} and \code{nrows} to \code{1} (#201),
    \item added argument \code{curve.transformation} to allow a quick data overview using the pseudo-LM curve transformation methods.


   }

   \item \code{plot_RLum.Results()}

   \itemize{

   \item Improved error handling for \code{RLum.Results} objects originating from \code{calc_MinDose()}

   }

    \item \code{plot_RLum.Data.Spectrum()}

   \itemize{

   \item For \code{plot.type="multiple.lines"} the legend was placed odd, corrected using \code{par()$usr} parameters,

   \item argument \code{legend.text} added for plot types which support a legend (e.g., \code{plot.type="multiple.lines"}).

   }

  \item \code{readBIN2R()}

  \itemize{

  \item Improved handling of odd time formats,
  \item support for the changes in the corresponding S4-class implemented.

  }

    \item \code{readXSYG2R()}

  \itemize{

  \item For using the argument \code{recalculate.TL.curves = FALSE} the import of spectrometer
  data was broken. Fixed.
  \item during TL-curve recalculation for spectra in rare cases the produced object was not
  valid for further plotting due to duplicated x-values in the matrix. This has been fixed by
  adding artificially 1 K to such values. In addition a warnings is produced.

  }

   \item \code{writeR2BIN()}

  \itemize{

  \item Part of the manual was missing due to an error in the inlinedoc documentation. Fixed.,
  \item the argument \code{version} no expects a \code{character} as import instead of a \code{raw} object,
  \item further suppressing of confusing warning messages,
  \item date has not been written properly in accordance with the format description (fixed),
  \item time values have been not properly exported in same cases, corrected.
  \item added support for changes in corresponding S4-class, means that reserved values are transfered,
  \item the version conversion was partly broken due to the new routines implemented with last update (fixed),
  \item if empty comments or sample names are provided these are replaced by \code{"  "} (double empty space),
  \item if the BIN-file is stored in a different version, the version number and the record lengths are now changed accordingly.


   }


}}

\subsection{New example data sets}{
\itemize{


  \item ExampleData.DeValues is now a list with two elements containing an additional single grain quartz data set (#203)

}}

\subsection{Internals}{
\itemize{


  \item Adding dependency for package 'Rcpp'. This is used in particular to export some time consuming
  calculations to compiled C++ code to speed up the functions. Currently it is used e.g. in the function
  \code{analyse_IRSAR.RF()},

  \item adding dependency (import) for the packages \code{parallel}. With this
  the multi-core support is realised within in the package. First implemented in the
  function \code{calc_MinDose()}.

}}



}

\section{Changes in version 0.4.1 (January 29th, 2015)}{

  \subsection{New functions}{

  \itemize{

  \item \code{merge_RLum.Data.Curve()}: Function allows merging of \code{RLum.Data.Curve}
  objects in different ways (#128).

  }}


   \subsection{Changes S4-objects, methods, classes}{
   \itemize{



    \item \code{RLum.Analysis-class}:


     \itemize{

    \item Show warnings messages when empty object are produced by a \code{get_RLum.Analysis()}
    request (implemented in get method and show method).



    }


}}



  \subsection{Bugfixes and changes}{
   \itemize{


   \item \code{analyse_SAR.CWOSL()}

     \itemize{

    \item rename argument \code{output.plot} to \code{plot},
    \item rename argument \code{output.plot.single} to \code{plot.single},
    \item rejection criteria added 'De > max. dose point'. These avoids that
    values are accepted even the De from the fitting should be considered as invalid.


    }


    \item \code{analyse_IRSAR.RF()}

     \itemize{

    \item Add enhanced support for \code{...}, e.g. \code{log}, \code{xlim}, \code{ylim},
    \item rename argument \code{output.plot} to \code{plot}.

    }

    \item \code{analyse_pIRIRSequence()}

     \itemize{

    \item Fix an error that causes problems during plotting (Error in plot.new(): figure margins too large),
     \item rename argument \code{output.plot} to \code{plot},
    \item rename argument \code{output.plot.single} to \code{plot.single}.

    }

    \item \code{merge_RLum()}: Function extended accordingly with the newly implemented function
    \code{merge_RLum.Data.Curve()} (#128).



   \item \code{plot_AbanicoPlot()}:


     \itemize{

    \item ticks removed from error bars,
    \item line value output removed,
    \item placement of y- and z-labels set to center of plot,
    \item weights-option for KDE integrated,
    \item warning for linear z-scale removed,
    \item fixed an error that occured in Abanico Plot for a large data range (#188),
    \item function output extended by data points in/out of confidence interval,
    \item histogram option added,
    \item dot plot option added,
    \item KDE bandwidth added to plot output,
    \item KDE bandwidth default changed to "SJ",
    \item rugs coloured according to subsets,
    \item examples section extended.


    }

    \item \code{plot_GrowthCurve()}:

      \itemize{

    \item Thanks to a querry from Rachel Smedly (#194) the De error calculation method has been updated.
    So far only the Lx/Tx values have been re-sampled using a Monte Carlo simlation approach to estimate
    the error. In contrast to the Analyst the Ln/Tn value was just considered without any error. This
    has been changed for all fitting methods and the final De error is calculated now also using the Ln/Tn error.
    Note: Usually this change results in a larger De error, however this approach is much more realistic,

    \item consequently to the changes made in the error calculation the corresponding De resulting from
    the MC simulation is now added to the output object (\code{$De$De.MC}),

    \item Ln/Tn point including the error is now added on the plot output as red circle,

    \item fix a bug occasionally occurred for \code{fit.method = "EXP+EXP"} and the function broke
    subsequently (#159).


    }

   \item \code{readBIN2R()}:


     \itemize{

    \item critical bug fixed: \code{GRAIN} and \code{GRAINNUMBER} were not imported properly
    (thanks to Norber Mercier),

    \item import speed has been improved by at least an order of magnitude, due to a different
    internal data handling (using of \code{data.table} instead of \code{data.frame}). This may become
    relevant for huge BIN-files like they are produced by single grain measurements (#191).
    Note: The \code{Risoe.BINfileData-class} remained untouched.


    }


     \item \code{writeR2BIN()}:


     \itemize{

    \item minor changes due to the changes in the function \code{readBIN2R()},
    \item confusing warnings during writing are suppressed.


    }

}}

 \subsection{Internals}{
   \itemize{

    \item Add new dependency for package \code{data.table} (used in the function \code{readBIN2R()}).

  }}


}

\section{Changes in version 0.4.0 (December 19th, 2014)}{

 \subsection{Preface}{
 \itemize{
  \item Amongst a few minor changes with this release we continued the homogenisation process to further unify our function arguments.
  So far meaningful similar function arguments can be used over all functions in the package to produce similar effects
  (e.g., na.rm = TRUE/FALSE). For you as package user it means that self-written R scripts using package functions
  are likely to break. Please check the list of changes below.

  We apologise for any inconvenience!

  }}


   \subsection{New S4-classes, methods}{
   \itemize{

    \item \code{RLum.Data.Image}: S4-class for handling image data including. Image
    data in slot \code{data} are stored as \code{RasterBrick}

    \item \code{RLum.Results}: homogenised structure for containing the following base items in the
    slot data:  'summary', 'args' and 'call'. Where applicable the 'data' slot contains the original input data.
    Depending on the function various further slots are added containing additional (intermediate) data and objects,

    \item \code{set_RLum.Data.Image()}: Set method for class \code{RLum.Data.Image}

    \item \code{get_RLum.Data.Image()}: get method for class \code{RLum.Data.Image}

    \item further: corresponding show and coercing methods added for class \code{RLum.Data.Image}.
  }
  }


   \subsection{New functions}{
   \itemize{

  \item \code{analyse_pIRIRSequence()}: Function to analyse post-IR IRSL sequence
   measurements (#133),

   \item \code{calc_MinDose()}: the previous \code{calc_MinDose3()} and \code{calc_MinDose4()}
   functions were merged to the new \code{calc_MinDose()} function, which was rewritten from
   scratch and now contains both the 3- and 4-parametric
   Minimum Age Models (introducing the new argument \code{par}).
   Detailed changes and bugfixes are listed further below,

   \item \code{calc_MaxDose()}: a wrapper function that calls \code{calc_MinDose()},
    but transforms the data set similar to the method described in Olley et al. (2006).
    Rather than estimating the minimum dose of a De distribution, this function estimates the maximum dose.

    \item \code{calc_SourceDoseRate()}: calculating the dose rate of the irradiation source at the
    date of measurement based on: source calibration date, source dose rate, dose rate error.

  \item \code{get_Layout()}: Helper function to return a list with layout definitions for
  more homogeneous plotting,

   \item \code{get_RLum()}: Wrapper function for various get methods for \code{RLum}-objects (#148),

   \item \code{merge_RLum.Analysis()}: Function allows merging of RLum.Analysis objects and adding
   of objects an RLum.Analysis object consists of (#138),

   \item \code{merge_RLum()}: Wrapper function for various merge methods for \code{RLum}-objects (#139),

   \item \code{plot_RLum.Data.Image()}: Plot function for \code{RLum.Data.Image} object.
   Function also added to wrapper function \code{plot_RLum},

   \item \code{plot_RLum.Results()}: Plot function for \code{RLum.Results} objects.
    RLum.Results objects created, e.g. by the age model functions may now be passed to \code{plot_RLum.Results()}
    to create specific plots that previously were generated by the functions themselves.
    Function also added to wrapper function \code{plot_RLum},

  \item \code{readSPE2R()}: Add support Princenton Instruments *.SPE files
    (camera and spectrometer measurements). Function output is a \code{RLum.Data.Spectrum}
    or \code{RLum.Data.Image} object.




  }


  }


   \subsection{New example data sets}{
   \itemize{

  \item \code{ExampleData.RLum.Data.Image}: Example data for \code{\linkS4class{RLum.Data.Image}} object
  (part of #135).

  }


  }



  \subsection{Changes S4-objects, methods, classes}{
   \itemize{

     \item \code{get_RLum.Analysis()}: Add support for inverse \code{record.id} selection, e.g. \code{record.id = -1} selects
     all objects except the first.


    \item \code{get_RLum.Results()}:


     \itemize{

    \item Add support for data objects for objects
    produced by the function \code{calc_OSLLxTxRatio()},

    \item match access behavoir of objects produced by the function
    \code{calc_TLLxTxRatio()}. Now it works similar to the function \code{calc_OSLLxTxRatio()}.


    }


   \item \code{Risoe.BINfileData-class}: Description updated due to established changes
   made for a better file format support.

    \item \code{Risoe.BINfileData2RLum.Analysis()}: If the \code{pos} value
    is invalid now the function does not return an error any longer. Instead
    \code{NA} is returned.



  }
  }

   \subsection{Bugfixes and changes}{
   \itemize{

     \item \code{analyse_IRSAR.RF()}:
      \itemize{

        \item Add \code{...} support for \code{mtext},

        \item argument \code{method} added to allow selection between the method \code{"FIT"} and
        \code{"SLIDE"}. The latter method has been newly implemented (#143).

      }


    \item \code{analyse_SAR.CWOSL()}:
      \itemize{

        \item Allow 'IRSL' curves as input to support rough data analysis of the
        post-IR IRSL protocol (#132, Alice Versendaal, #140, Georgina King),

        \item support multiple recuperation measurements,

        \item correct wrong TL curve figure scaling,

        \item argument \code{log} moved to the \code{...} argument,

        \item correct wrong regeneration point numbering (#144),

        \item arguments \code{cex}, \code{main} now supported via the \code{...} argument,

        \item argument \code{mtext.outer} added,

        \item output object \code{Formula} added to return the formula of the
         growth curve,

         \item output object \code{De.values} extended for D01, D02 and information
         on the applied fitting,

        \item numerical output extended for information on the choosen signal and
        background integration limits (#122),

        \item enhance argument \code{output.plot.single} for numerical values.
        This allows to select each single plot output individually,

        \item details from the argument \code{rejection.criteria} were removed
        from the argument list to allow a more flexible handling (e.g. changing only
        one parameter at time) and to clean up the argument list. However, the argument
        work still as expected (#145),

        \item reformat of rejection criteria output to allow easier further treatment,

        \item support graphical output of rejection criteria and IRSL curve (if exists),

        \item minor bug fixes (e.g., not properly caluculated TL curve resolution) and graphical adjustements.

      }

       \item \code{Analyse_SAR.OSLdata()}:

       \itemize{

        \item Error message implemented for non OSL data sets (#130, Georgina King),

        \item add argument \code{dtype} to provide further curve limitation options,

        \item add argument \code{keep.SEL} to allow a manual curve selection (advanced users),

        \item minor code polishing.

       }


  \item \code{calc_AliquotSize()}:

  \itemize{

    \item new argument \code{plot} (logical): option to (dis-)allow plotting of the Monte Carlo estimation,
    \item argument \code{MC.estimate} was renamed to \code{MC},
    \item new structure of the returned \code{RLum.Results} object, containing the elements
    'summary', 'args', 'call' and 'MC'.
    \item plotting now done by \code{plot_RLum.Results()}.

  }

  \item \code{calc_CentralDose()}:

  \itemize{

  \item removed argument \code{sample.id},
  \item removed argument \code{print.iterations},
  \item renamed argument \code{input.data} to \code{data},
  \item new argument \code{'...'}, which is checked for arguments \code{verbose} (logical) and \code{trace}
  (logical) to (dis-)allow (extended) console output,
  \item new structure of the returned \code{RLum.Results} object, containing the elements 'summary', 'data', 'args', 'call' and 'profile',
  \item fixed a bug that occured for non-dispersed data sets and resulted in a virtually infinite loop using up all available memory,
  \item plotting now done by \item{plot_RLum.Results()}.

  }

  \item \code{calc_CommonDose()}:

  \itemize{

  \item removed argument \code{sample.id},
  \item renamed argument \code{input.data} to \code{data},
  \item new argument \code{'...'} (currently not used),
  \item new structure of the returned \code{RLum.Results} object, containing the elements 'summary', 'data', 'args' and 'call'.


  }

  \item \code{calc_CosmicDoseRate()}:
  \itemize{

  \item new structure of the returned RLum.Results object, containing the elements 'summary', 'args' and 'call'

  }

  \item \code{calc_FiniteMixture()}:

  \itemize{

  \item removed argument \code{sample.id},
  \item removed argument \code{n.iterations},
  \item removed argument \code{main},
  \item renamed argument \code{input.data} to \code{data},
  \item new argument \code{plot} option to (dis-)allow plotting when a vector of length > 1 is provided for \code{n.components},
  \item new argument \code{'...'}, which is checked for arguments \code{verbose} (logical) and \code{trace}
    (logical) to (dis-)allow (extended) console output. \code{main} may be used to set the title of the plot,
  \item new structure of the returned \code{RLum.Results} object, containing the elements
  'summary', 'data', 'args', 'call', 'mle', 'BIC', 'llik', 'grain.probability', 'components' and 'single.comp',
  \item plotting now done by \code{plot_RLum.Results()}.

  }

   \item \code{calc_FuchsLang2001()}:

   \itemize{

   \item removed argument \code{sample.mtext},
   \item removed argument \code{sample.id},
   \item removed argument \code{output.terminal},
   \item renamed argument \code{sample} to \code{data},
   \item renamed argument \code{output.plot} to \code{plot},
   \item the argument \code{'...'} is checked for \code{verbose} (logical) to (dis-)allow console output,
   \item new structure of the returned \code{RLum.Results} object, containing the elements 'summary', 'data', 'args', 'call' and 'usedDeValues',
   \item plotting now done by \code{plot_RLum.Results()}.

   }

   \item \code{calc_HomogeneityTest()}:

   \itemize{

   \item removed argument \code{sample.id},
   \item renamed argument \code{input.data} to \code{data},
   \item the argument \code{'...'} is checked for \code{verbose} (logical) to (dis-)allow console output,
   \item new structure of the returned \code{RLum.Results} object, containing the elements 'summary', 'data', 'args' and 'call'.

   }

   \item \code{calc_MaxDose3()}:

   \itemize{

   \item Function renamed to \code{calc_MaxDose()},
   \item this is now just a wrapper function that calls the new \code{calc_MinDose()} function,
   but transforms the data set similar to the method described in Olley et al. (2006).
   For further details please see the changes/bugfixes for the \code{calc_MinDose3()} / \code{calc_MinDose4()} functions,
  \item plotting now done by \code{plot_RLum.Results()}.


   }

   \item \code{calc_MinDose3()} and \code{calc_MinDose4()}

   \itemize{

   \item the functions \code{calc_MinDose3()} and \code{calc_MinDose4()} were merged in the new function \code{calc_MinDose()},
   \item renamed argument \code{input.data} to \code{data},
   \item renamed argument \code{output.plot} to \code{plot},
   \item removed arguments \code{sample.id}, \code{gamma.xlb}, \code{gamma.xub}, \code{sigma.xlb}, \code{sigma.xub},
   \code{mu.xlb}, \code{mu.xub}, \code{init.gamma}, \code{init.sigma}, \code{init.p0}, \code{init.mu}, \code{ignore.NA},
   \code{calc.ProfileLikelihoods}, \code{console.ProfileLikelihoods}, \code{console.extendedOutput}, \code{output.indices},
   \item new argument \code{par} that accepts a numeric value (either 3 or 4) to set whether the 3- or 4-parametric Minimum Age Model should be applied,
   \item new argument \code{bootstrap} (logical) to apply the recycled bootstrap approach of Cunningham & Wallinga (2012),
   \item new argument \code{boundaries} that accepts a named list containing values for the lower and upper boundaries for all parameters,
   \item new argument \code{init.values} that accepts a named list containing the starting values for all  parameters,
   \item new argument \code{'...'} that, amongst others, checks for \code{debug} (logical) and \code{verbose} (logical) to (dis-)allow (extended) console
    output. Further allowed arguments are \code{bs.M}, \code{bs.N}, \code{bs.h} and \code{sigmab.sd} to set various bootstrap parameters,
   \item implemented (recycled) bootstrap method as described in Cunningham & Wallinga (2012),
   \item function now iteratively adjusts boundaries for sigma when the estimator reaches the upper boundary,
   \item function now tries to estimate reasonable starting values for all parameters,
   \item calculation and plotting of the maximum log likelihood functions/profiles are now done using the mle2() function of the 'bbmle' package
   \item the optimisation routine was changed from 'nlminb' to 'optim' using the 'L-BFGS-B' method,
   \item confidence intervals for all parameters are now calculated by the 'confint()' method and exclusively on the 95\% confidence level
   \item the asymmetric error on gamma (i.e., the minimum dose) is now exclusively determined on the 95\% confidence interval of the
   profile log likelihood function for gamma
   \item a symmetric standard error for gamma is automatically derived from the 95\% confidence interval of the
   profile log likelihood function by dividing the length of this interval by 3.92 (see Galbraith & Roberts, 2012).
   \item new structure of the returned \code{RLum.Results} object, containing the elements 'summary', 'data',
   'args', 'call', 'mle', 'BIC', 'confint', 'profile', 'bootstrap',
   \item plotting now done by \code{plot_RLum.Results()}.

   }


    \item \code{calc_OSLLxTxRatio()}:
      \itemize{

      \item Correct an error that causes \code{NA} values
      within the curve errors for rare combinations of signal and background
      integration limits,
      \item provide a name for the output data frame \code{LxTx.table} for the
      \code{RLum.Results} object,

      \item error fixed for the marginal case that k = 2 and exceed the maximal curve length. In that
      case accidentially no error was calculated,

      \item argument \code{sigmab} was listed but was so far not implement. Fixed (#166)

      }

        \item \code{calc_OSLLxTxRatio()}:
      \itemize{

      \item support of data without given uncertainties.

      }

     \item \code{calc_Statistics()}:
      \itemize{

      \item support of data with \code{NA} or zero errors

      }



    \item \code{calc_TLLxTxRatio()}:

          \itemize{

          \item Lx/Tx ratio wrongly calculated as due to a coding error just the first value of
          the signal integral was taken for summing up the values and not the range of the values,

          \item Output produced by the function now is a \code{list} (data slot in the RLum object), to
          equal behaviour of the functions.


          }

    \item \code{fit_CWCurve()}:
      \itemize{

      \item Implement \code{...} argument to replace explicit arguments \code{ylab},
      \code{xlab} and \code{main},

      \item Fix an issue that causes an error when input values were <= 0
      (#123 reported by Johanna Lomax),

      \item Correct wrong y-axis labelling (wrong channel resolution value).
      }



    \item \code{plot_AbanicoPlot()}:
      \itemize{

        \item numeric values allowed for parameter centrality,
        \item KDE label value fixed,
        \item layout functionality added,
        \item not supported summary keywords fixed,
        \item dispersion polygon bug fixed,
        \item dispersion type enriched by userdefined confidence interval,
        \item rug option added,
        \item multiple 2-sigma-bars allowed,
        \item rotate-Option added to Abanico Plot,
        \item minor modifications of layout definitions (#124),
        \item \code{grid.col} bug fixed (#149),
        \item error message for data with zero or without errors added (#154),
        \item age models as input for lines supported (#107),
        \item support of \code{par(mfcol())}l,
        \item bug in lower z-axis limit eliminated,
        \item example list updated (i.e. fast CAM instead of slow MAM)
        \item bug for small values fixed, rounding to 0 decimals replaced by 3 significant digits,
        \item parameter \code{at} to allow user-defined z-tick positions,
        \item individual statistical summary order supported (#184).

      }


      \item \code{plot_DRTResults()}:
      \itemize{

        \item Argument \code{na.exclude} renamed to \code{na.rm},

        \item by default ylim scaling now is set to the minimum and  maximum extend of the
        data set,

        \item argument \code{given.dose} now allows a vector as input, e.g., if more then one data set
        is provided (#156),

        \item argument \code{given.dose} are not required any longer (became an optional argument).
        With this the plot function can be easily re-used, e.g. for preheat plateau tests,

        \item change behaviour of the argument \code{na.rm}. If a data set is provided containing
        \code{NA} values also the corresponding values in provided by the argument \code{preheat} are
        cleaned up. This allows a proper usage of the function in combination with grouping option
        (e.g., \code{boxplot = TRUE},

        \item add argument \code{par.local} to allow to combine output plots in broader plot
        frameworks,

        \item minor corrections in error handling and graphical appearance.

      }


      \item \code{plot_Histogram()}:
      \itemize{

        \item Change default y-axis labelling from 'Counts' to 'Frequency',
        \item support of \code{par(mfcol())} (#146),
        \item support of data without and \code{NA}-values for error values (#152, #162),
        \item implementation of \code{calc_Stats()}-call for extended statistic summary,
        \item support of summary keywords "kurtosis" and "skewness",
        \item example y-lim bug removed (#177),
        \item individual statistical summary order supported (#184).


      }

      \item \code{plot_KDE()}:
      \itemize{

        \item wrong x-axis and y-axis limit calculation fixed (#158),
        \item renaming and extension of summary and \code{summary.pos} parameter,
        \item plot now longer shows a density error when no error exists (#160),
        \item De.stats error fixed (#163),
        \item support of \code{par(mfcol())},
        \item support of error-free data extended (now also for calculation of kde max) (#178),
        \item individual statistical summary order supported (#184).

      }


      \item \code{plot_GrowthCurve()}:
      \itemize{

        \item Minor graphical and code polishing,

        \item fix wrong regneration point plotting, if sequence contains more then
        one zero dose point,

        \item argument \code{na.exclude} for the sake consistency replaced by \code{na.rm},

        \item proper error message provided if nothing is left after \code{NA} value removal,

        \item overshoot of normal distribution curve in MC simulation plot corrected
        (#111),

        \item argument \code{output.plotExtended.single} added allowing a single
        window plot output of all plots,

        \item formula of the fitted function is now returend as R expression
        instead of a \code{formula} object. The return object remains, however,
        \code{Formula},

        \item correct wrong error message and graphical output for cases where no De can be
        calculated for Ln/Tn values much higher than the highest Lx/Tx value,

        \item fix \code{'EXP+EXP'} fit as is was broken, due to a coding error,

        \item correct for wrong \code{par()} resetting,

        \item minor code polishing and adjustment.

      }

   \item \code{plot_RadialPlot()}:
      \itemize{

     \item support of user-defined centrality value,
     \item rug option added,
     \item bug for small values fixed, rounding to 0 decimals replaced by 3 significant digits,
     \item individual statistical summary order supported (#184),
     \item minor fixes for the graphical output.
    }

    \item \code{plot_RLum.Analysis()}:
      \itemize{

      \item Add argument \code{abline} to allow inserting similar vertical or horizontal lines
      in every plot. The decision to implement this argument in this function instead the function
      \code{plot_RLum.Data.Curve()} was made because in a single plot modus \code{abline} is more
      sufficiently used as separate function call,

      \item now reset \code{par} parameters sufficiently,

      \item add argument \code{combine} to allow fast combining of curves in single plot window.

      }


      \item \code{plot_RLum.Data.Curve()}:
      \itemize{

        \item Add argument \code{norm} to allow for curve normalisation to
        the highest value (#129),

        \item Correct for insufficient information on the channel resolution when data was imported
        from an XSYG file.

      }



      \item \code{plot_RLum.Data.Spectrum()}:
      \itemize{

        \item Correct an error that caused a chrash when \code{xlim} was set
        in a way that only one wavelength (colour value) was remained,

        \item correct zebra-colour-pattern for rare cases of \code{xlim} settings
        using \code{plot.type = "persp"} or \code{plot.type = "persp3d"},

        \item add argument new plot type \code{"transect"} for plotting a transect
        along the time/temperature axis for a specific wavelength/channel interval
        (#120, requested by Regina DeWitt),

        \item correct function break for a single column matrix (single frame spectrum)
        by automatically resetting \code{plot.type} to \code{single}
        for single frame spectra,

        \item colour rug (\code{rug = TRUE}) implemented for plot type \code{"multiple.lines"}
        (#130),

        \item argument \code{xaxis.energy} for wavelength energy conversion for
        2D plots added (#141).

      }


   \item \code{readBIN2R()}:
      \itemize{

        \item Integrity checks added to avoid unspecific errors for wrong input
        formats,

        \item complete format support as now a more throughtful distinction is made between
        version 03 and 04 (also made for function \code{writeR2BIN()}) (#182),

        \item \code{TAG} bit is now used to trigger the \code{SEL} element during import.
        With this, previous curve selections made with other software are kept
        (#181, Norbert Mercier).


      }

   \item \code{readXSYG2R()}:
      \itemize{

        \item If the stimulator is infrared light the \code{recordType} will be set to \code{IRSL}. This
        makes the curve types more consistent with the commonly applied nomenclature,

        \item now the function automatically converts time values to temperature values also
        for spectra data (#121); behavious is triggered with the argument \code{recalculate.TL.curves}.

      }


    \item \code{Second2Gray()}:
      \itemize{

       \item Minor code and example adjustments in along with creating a new function
       \code{calc_SourceDoseRate()} (#56).

      }


   \item \code{writeBIN2R()}:
      \itemize{

       \item Complete format support as now a more throughtful distinction is made between
        version 03 and 04 (also made for function \code{readBIN2R()}) (#182),

        \item \code{SEL} will now be kept using the \code{TAG} bit, related to changes made
        in function \code{readBIN2R} (#181, Norbert Mercier).

      }

   }

 }

  \subsection{Internals}{
   \itemize{

    \item Add new dependency for package \code{raster} and \code{sp}, which is used
    for the \code{RLum.Data.Image-class},

    \item dependency to package 'Rserve' removed,

    \item dependcy to package 'zoo' added (used for running means),

    \item dependency to package 'bbmle' added (used for maximum likelihood estimations).

  }
  }

}

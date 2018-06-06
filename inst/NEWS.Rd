\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.8.5 (June 8th, 2018)}{

 \subsection{Bugfixes and changes}{
  \itemize{

     \item \code{analyse_Al2O3C_Measurement()}
      \itemize{
        \item The cross-talk value was erroneously substracted the dose point values instead from the final De,
        the effect is small, but still, it was not correct; fixed,
        \item add new argument \code{calculate_TL_dose} which allows to experimentally calculate
        the apparent dose based on the TL curves. This feature may help in certain cases to cross-check
        the results obtained with the GSL,
        \item the manual was not complete and the reference wrong, fixed.


      }

  \item \code{analyse_baSAR()}
   \itemize{
          \item Add support for \code{RLum.Analysis}-objects as input,
          \item correct odd message informing about removed aliquots that had been never removed,
          \item add new argument \code{distribution_plot} to allow a manual selection of the frequentist
          distribution plot output.

        }


    \item \code{analyse_SAR.CWOSL()}
      \itemize{
        \item A newly introduced typo caused a function crash; fixed.


      }

   \item \code{bin.RLum.Data.Curve()}
      \itemize{
        \item The method was not passing the argument \code{bin_size} properly; fixed.


      }



  }

 }

}

\section{Changes in version 0.8.4 (April 20th, 2018)}{

 \subsection{Bugfixes and changes}{
  \itemize{

    \item The previous package versions did not pass the CRAN tests on the Solaris platform; fixed.

  }

 }

}


\section{Changes in version 0.8.3 (April 20th, 2018)}{

 \subsection{Bugfixes and changes}{
  \itemize{
      \item \code{analyse_SAR.CWOSL()}
    \itemize{
      \item The function crashed if the \code{plot_GrowthCurve()} returned \code{NULL}; fixed,
      \item ff the dose-response curve was not fitted, but \code{plot} was set to \code{TRUE} no
      plot was returned (even no empty plot); fixed,
      \item if all curves were IRSL curves, the function crashed while trying to plot the IRSL curve
      for the IRSL test. Now only the last IRSL curve is plotted. This does not make really sense
      since probably the user did not measure quartz, however, this solution fixes the problem and
      does not break other users code.


    }

    \item \code{analyse_FiniteMixture()}
    \itemize{
      \item For wrong user input and not well understood assumptions, the function crashed uncontrolled; fixed.

    }

    \item \code{analyse_pIRIRSequence()}
    \itemize{
      \item Fixed unwanted functions crashes for very odd datasets (e.g., \code{Inf} values).

    }


   \item \code{convert_RLum2Risoe.BINfileData()}
    \itemize{
      \item The record type was not correclty extracted, fixed,
      \item the conversion now keeps the original position numbers instead of assigning new.

    }


    \item \code{plot_GrowthCurve()}
    \itemize{
      \item If number of regeneration points was reduced for plotting and fitting, an unwanted
      warning message appeared due to a coding error,
      \item if the D0 values is precisely 0.0000, the fit probably went wrong, in such cases now another
      attempt is made,
      \item if a value for Lx/Tx is 0 now the value is set to \code{.Machine$double.eps}, instead of 0.001,
      \item the formula output for \code{EXP+EXP} was not correct, fixed.

    }

    \item \code{src_create_RLumDataCurve_matrix()}
    \itemize{

      \item Until today, you probably did not know that this function exists at all. This function
      is written in C++, working in the back and helps to create \code{RLum.Data.Curve}-objects out of BIN/BINX-files.
      Recently we became aware of non-conform BIN/BINX-files, circulating in the wild. For such files the TL curves
      were not created correctly, more important, the user did not know why. Now the function throws out a message, at least.

    }
  }

 }

}


\section{Changes in version 0.8.2 (March 10th, 2018)}{

 \subsection{Internals}{
  \itemize{
    \item Package size significantly reduced by excluding the XSYG-file used for buildung
    the vignette.

  }

 }

}


\section{Changes in version 0.8.1 (March 9th, 2018)}{


  \subsection{New functions}{

  \itemize{
  \item \code{calc_Huntley2006()}{
    \itemize{
      \item A function to calculate the expected sample specific fraction of saturation
      based on the model of Huntley (2006) using the approach as implemented in
      Kars et al. (2008) or Guralnik et al. (2015). This function supersedes the formerly
      released function 'calc_Kars2008()'. The new function is identical to 'calc_Kars2008()'
      when 'fit.method = "EXP"' (the default) is used, but also allows fitting
      a general-order kinetic function after Guralnik et al. (2015) ('fit.method = "GOK"').
     }
    }

   }
  }

  \subsection{Bugfixes and changes}{

  \itemize{

       \item \code{calc_Kars2008()}{
          \itemize{
             \item This function is now deprecated and will eventually be removed
             in an upcoming version of the 'Luminescence' package. Older scripts using
             this function will still work, but it is highly recommended to use the
             newly introduced function 'calc_Huntley2006()'.
          }
      }

      \item \code{calc_MinDose()}{
          \itemize{
             \item Previously, the function globally disabled warning messages via
             'options(warn = -1)' due to the overly verbose output of functions in
             the 'bbmle' package. Globally disabling warning messages may prevent
             the display of unexpected warnings, however, and in rmarkdown files
             it wouldn't work anyway. This behaviour was replaced with explicit
             calls to 'suppressWarnings()' for particular functions. (#66)
             \item Fixed a bug where the function tried to calculate the log of
             infinite (Inf, -Inf) values, which produced 'NaN' warnings.
          }
      }

     \item \code{get_RLum()}{
          \itemize{
             \item The argument \code{subset} was broken and not working anymore, similar to the
             S3-method \code{subset} dispatching to \code{get_RLum} for \code{RLum.Analysis}-objects.
             Both issues have been fixed.

          }
      }

      \item \code{plot_DRTResults()}{
          \itemize{
             \item If \code{boxplot = TRUE} and \code{give.dose = NULL} the plot still showed the
             abline at 1 including the +/- 10 \% error range. Now, the abline is only showed
             if a numeric value is provided for \code{give.dose}, \code{NULL} plots no abline.

          }
      }

      \item \code{plot_GrowthCurve()}{
          \itemize{
             \item Now allows fitting of the general-order kinetics function after Guralnik
             et al. (2015). Use \code{"GOK"} as input for argument \code{fit.method}.

          }
      }

     \item \code{template_DRAC()}{
          \itemize{
             \item New argument \code{preset} to allow the selection of pre-defined templates
             for various configurations (e.g., \code{'quartz_coarse'}) for quartz coarse grain
             data. The templates bring pre-filled values to speed up the workflow, however, they
             do not free the researcher from cross-checking these values.

          }
      }
    }
  }

    \subsection{Internals}{

    \itemize{
      \item Lower required dependent package versions to get the package passed on CRAN.
      \item Further compress example data ... it was not yet the optimum.
      \item Further compress 'vignettes'.

    }

  }

}

\section{Changes in version 0.8.0 (Feburary 9th, 2018)}{

  \subsection{New functions}{
    \itemize{

         \item \code{analyse_Al2O3C_ITC()}:
         Function to analyse data determined to correct irradiation times as suggested by
         Kreutzer et al., 2018

       \item \code{analyse_Al2O3C_CrossTalk()}:
         Function to analyse data to analyse crosstalk measurements as shown in
         Kreutzer et al., 2018

       \item \code{analyse_Al2O3C_Measurement()}:
         Function to analyse Al2O3:C data as shown by Kreutzer et al., 2018

        \item \code{convert_Activity2Concentration()}: converts nuclide activity data (Bq/kg) to
        concentrations (ppm or \%) and vice versa

        \item \code{convert_RLum2Risoe.BINfileData()}: Function to convert \code{RLum.Analysis} and \code{RLum.Data.Curve}
        objects to \code{Risoe.BINfileData}, which can be exported to BIN/BINX-files using the function \code{write_R2BIN()}.

        \item \code{calc_WodaFuchs2008()}:
        Function to obtain the equivalent dose using
        the approach proposed by Woda and Fuchs, 2008.

        \item \code{fit_SurfaceExposure()}:
        Function to fit eq. 1 in Sohbati et al. (2012a) to OSL surface exposure data
        and derive estimates of its parameters (i.a., the age). Alternatively,
        eq. 12 in Sohbati et al. (2012b) can be fitted, which also considers the
        dose rate.

        \item \code{calc_Lamothe2003()}:
        Function to support the fading correction suggested by Lamothe et al., 2003. As input the function accepts
        \code{data.frame} output from the functions \code{analyse_SAR.CWOSL()} and
        \code{analyse_pIRIRSequence)}.

    }
  }

    \subsection{New example data}{
    \itemize{

        \item \code{ExampleData.SurfaceExposure}:
        A set of synthetic OSL surface exposure dating data to demonstrate
        the fit_SurfaceExposure() functionality. See ?ExampleData.SurfaceExposure
        for details.

        \item \code{ExampleData.Al2O3C}:
        Al2O3:C chips measurement data to supplement the newly implemented functions to analyse
        Al2O3:C data.


    }
  }

  \subsection{S4-classes and methods}{
    \itemize{

        \item The \code{RLum.Analysis} show method failed for \code{RLum.Data.Spectrum} objects; fixed.
        \item Method for \code{get_RLum} with signature \code{NULL} added. Now it returns \code{NULL}.
        \item The coercion from \code{list} to \code{RLum.Results} using \code{as()} did not work; fixed.
        \item Method \code{get_RLum} for \code{RLum.Analysis} returned a confusing error if what was provided via 'subset' was not a logical expression; fixed.
        \item \code{structure_RLum()} and \code{names_RLum()} each got a method to deal with a list of \code{RLum} objects.


    }
  }


  \subsection{Bugfixes and changes}{

  \itemize{

       \item \code{analyse_SAR.CWOSL()}{
          \itemize{
             \item The function gained a new argument \code{'onlyLxTxTable'}. If set to \code{TRUE} only
             the Lx/Tx table is returned (including regeneration point sorting).
       }
     }

     \item \code{analyse_IRSAR.RF()}{
        \itemize{
           \item Multicore processing now also supports Windows (#47)
           \item Fixed a bug where manually specifying the number of CPU cores
           via 'method.control = list(cores = x)' was not respected,
           \item mixing of curves (e.g., regenerated first, natural second) led to a crash
           of the function, even the structure was correctly specified; fixed (#61),
           \item the function will not break anymore uncontrolled of both RF curves are of similar
           extension on the x-axis.
     }
   }


    \item \code{analyse_pIRIRSequence()}{
        \itemize{
           \item If the number of records did not match the sequence structure the function crashed
           with a confusing error message. Now it shows an error and returns \code{NULL},
           \item make the argument \code{main} more powerful if the input is a list,
           \item add missing items to the manual.
     }
   }

     \item \code{calc_HomogeneityTest()}{
        \itemize{
          \item The calculation was wrong for the option \code{log = TRUE} (the default); fixed (#63).

        }
    }

      \item \code{calc_Kars2008()}{
        \itemize{
         \item In some cases the calculated LxTx values of the simulated natural
         dose-response curve were slightly underestimated and its maximum value
         differed from the individually calculated nN(ss) value. The approach
         to calculate the natural DRC was changed and now yields a maximum LxTx
         value indistinguishable from nN(ss).

         \item Numeric output in the plot should now always be inside the plotting
         frame.

         \item The number of digits for the numeric output in the plot was slightly
         adjusted.


    }
  }

   \item \code{plot_FilterCombinations()}{
        \itemize{
         \item Add interactive mode (\code{interactive = TRUE}),
         \item correct wrong visualisation of transmission windows for long or short pass filters,
         \item fix legend error if only one filter was provided (due to a recent matrix behaviour change).


    }
  }

  \item \code{plot_GrowthCurve()}{
        \itemize{
         \item The function now turns \code{Inf} into \code{NA},
         \item the argument \code{na.rm} is defunct and should not be used anymore,
         \item minor code modifications to handle special cases such as input values with a
         \code{data.frame} with no error column,
         \item extent examples by for test data set by Berger & Huntley (1989),
         \item remove one more confusion warning if the fit did not work.

    }
  }

  \item \code{plot_Historgram()}{
        \itemize{
         \item The forwarded argument \code{breaks} was not treated correctly (#54); fixed.

    }
  }

    \item \code{plot_RLum()}{
        \itemize{
         \item The function now supports plotting even of highly nested lists of `RLum-objects`.

    }
  }

  \item \code{plot_RLum.Analysis()}{
        \itemize{
         \item The option `col` was not working as expected if `black` was provided; fixed,
         \item setting \code{ylim} caused a function break (#65); fixed.

    }
  }



  \item \code{read_BIN2R()}{
        \itemize{
         \item The function unexpectedly dropped are warning for an unused connection; fixed,
         \item if a file link was provided and the argument \code{pattern} was set and error was shown; fixed.


    }
  }


  \item \code{read_Daybreak2R()}{
        \itemize{
         \item The functions did not work anymore on ASCII files; fixed!


    }
  }


   \item \code{read_XSYG2R()}{
        \itemize{
         \item Argument \code{verbose} added for more consistency with other functions,
         \item the functions now supports the import form files via the internet,
         \item minor modifications to support minimal examples without 'recordType',
         \item give the XSYG-file import a 50\% performance boost,
         \item set option `XML_PARSE_HUGE` to allow parsing of larger nodes within the XSYG-file,
         \item the metadata entries \code{curveType} and \code{parentID} appeared duplicated, fixed.


    }
  }

    \item \code{read_SPE2R()}{
        \itemize{
         \item Add support for internet connections,
         \item fix unclear file extension recognition.


    }
  }

  \item \code{write_R2BIN()}{
        \itemize{
         \item The BIN-file export to version 04 failed in very rare cases for \code{NA} values
         in the byte position \code{SEQUENCE}; fixed,
         \item the compatibility mode was extended, now limiting the maxmium number of points for one
         curve to 9,999 (if the option is chosen). This improves the compatibility with older versions
         of the Analyst.


    }
  }


 }

}

  \subsection{Internals}{
    \itemize{
      \item Add new vignette 'Analyse Al2O3:C Measurements'
      \item Add new vignette 'Analyse post-IR IRSL Measurements'

    }
  }

  \subsection{Internals}{

      \itemize{
        \item Start supporting RStudio add-ins for more advanced users.
        \item Add fitting test data set by Berger & Huntley (1989) as external data.
        \item The internal documentation now uses 'markdown'.
        \item Internal function `.unlist_RLum()` allows to get rid of nested lists.
        \item Required R version increased to 3.4.0 (patchlevel 0).
        \item Cleanup help index and hide internal functions and methods.


    }
  }

}

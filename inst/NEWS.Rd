\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.7.3 (8th Feburary, 2017)}{

  \subsection{Bugfixes and changes}{
  \itemize{


   \item \code{Risoe.BINfileData()}{
    \itemize{
       \item Correct for mistakes in the manual.
     }
    }

   \item \code{write_R2BIN()}{
    \itemize{
       \item Correct for broken function (introduced with v0.7.0).
     }
    }
   }
  }

  \subsection{Miscellaneous}{
  \itemize{

  \item Correct wrong package date format.
  \item Add NEWS again to the package.
 }

}

}


\section{Changes in version 0.7.2 (7th Feburary (evening), 2017)}{

  \itemize{
  \item The CRAN check on the Solaris machines gave an error while performing
the (on all other platform sucessful) unit tests. Consequently, and to
reduce the load for the CRAN resources all tests are skipped on CRAN.
  \item This version never made it on CRAN!

  }



}


\section{Changes in version 0.7.1 (6th February (evening), 2017)}{

 \itemize{

\item This release accounts for the CRAN check errors on the Solaris machines by
preventing the unfortunate overload of the C++ function pow() with integer
values.

}

}

\section{Changes in version 0.7.0 (6th February (morning), 2017)}{

  \subsection{New functions}{
    \itemize{

        \item \code{analyse_FadingMeasurement()}:
        Analyse fading measurements to calculate g-values and to estimate
        the density of recombination centres.

        \item \code{analyse_portableOSL()}:
        The function analyses CW-OSL curve data produced by a SUERC portable
        OSL reader and produces a combined plot of OSL/IRSL signal intensities,
        OSL/IRSL depletion ratios and the IRSL/OSL ratio.

        \item \code{calc_Kars2008()}:
        A function to calculate the expected sample specific fraction of saturation
        following Kars et al. (2008) and Huntley (2006).

        \item \code{calc_AverageDose()}: Function to calculate the average dose and their extrinsic
        dispersion.

        \item \code{convert_BIN2R()}: wrapper function around the functions \code{read_BIN2R()} and
        \code{write_RLum2CSV()} to convert a BIN-file to CSV-files; so far possible.

         \item \code{convert_Daybreak2R()}: wrapper function around the functions \code{read_Daybreak2R()} and
        \code{write_RLum2CSV()} to convert Daybreak measurement data (TXT-file, DATE-file) to CSV-files; so far possible.

        \item \code{convert_PSL2R()}: wrapper function around the functions \code{read_PSL2R()} and
        \code{write_RLum2CSV()} to convert a PSL-file (SUERC portable OSL reader file format) to CSV-files; so far possible.

        \item \code{convert_XSYG2R()}: wrapper function around the functions \code{read_XSYG2R()} and
        \code{write_RLum2CSV()} to convert XSYG-file to CSV-files; so far possible.

        \item \code{github_branches(), github_commits(), github_issues()}:
        R Interface to the GitHub API v3. These functions can be used to query a specific repository hosted on GitHub.

        \item \code{install_DevelopmentVersion()}:
        This function is a convenient method for installing the development version of the R package
        'Luminescence' directly from GitHub.

        \item \code{PSL2Risoe.BINfileData()}:
        Converts an \code{RLum.Analysis} object produced by the function
        \code{read_PSL2R()} to an \code{Risoe.BINfileData} object.

        \item \code{read_PSL2R()}:
        Imports PSL files produced by a SUERC portable OSL reader into R.

        \item \code{smooth_RLum()}: wrapper function to call the corresponding methods to smooth
        data based on the function \code{zoo:rollmean}.

        \item \code{write_RLum2CSV()}:
        Exports \code{RLum}-objects to CSV-files to improve the compatibility to other software.
        Supported are only numerical values, i.e., \code{data.frame}, \code{matrix} and \code{numeric}.

    }
  }

    \subsection{New example data}{
    \itemize{

      \item \code{ExampleData.fading}:
        Example data set for fading measurements of the IR50, IR100, IR150 and IR225
        feldspar signals of sample UNIL/NB123. It further contains regular
        equivalent dose measurement data of the same sample, which can be used
        to apply a fading correction to. These data were kindly provided by Georgina King.
    }
  }

    \subsection{Changes in S4-classes and methods}{
      \itemize{

      \item Method \code{get_RLum} for \code{RLum.Analysis}-objects did not respect \code{.pid}, fixed.

      \item Method \code{get_RLum} for \code{list}-objects now accepts lists with all kinds of
      \code{RLum}-objects. Previously, only lists of \code{RLum.Analysis}-objects were allowed.

      \item \code{plot_RLum} was not passing the argument \code{sub}, as it was fetched by the partial
      argument matching; fixed.

      \item \code{set_RLum} produced \code{NA} as originator, if the function calling the function
      \code{set_RLum()} was called from outside of the package using the double colons
      (e.g., \code{Luminescence::function()}); fixed.

      \item \code{smooth_RLum} add method support for \code{RLum.Data.Curve}, \code{RLum.Analysis} and
      \code{list} of this objects implemented.


      }

    }

  \subsection{Bugfixes and changes}{

  \itemize{

   \item \code{analyse_baSARL()}{
    \itemize{
       \item Due to a typo in the manual the \code{method_control} parameter \code{variable.names} was
       not working if correctly typed as written in the manual (in the manual: 'variables.names', but
       correct is 'variable.names'); typo corrected fixed,
       \item minor improvements and error corrections.

    }
  }


  \itemize{

   \item \code{analyse_IRSAR.RF()}{

  \itemize{
    \item Add option for a vertical sliding of the RF_nat curve (\code{method_control = list(vslide_range = 'auto')}). This
    feature has beta status and usage for publication work is not recommended yet. By default no vertical
    sliding is applied,
   \item allow a parallel processing of MC runs by using the argument \code{method_control = list(cores = 'auto')}.
   }
  }

  \item \code{analyse_SAR.CWOSL()}{
    \itemize{
       \item Fix wrongly set threshold value for recuperation rate (#26),
       \item fix a rare bug for the combination 'recyling.ratio = NA' and more than
       one provided recyling point,
       \item a check has been implemented to refrain from using wrong rejection criteria
       keywords.
    }
  }

  \item \code{calc_AliquotSize()}{
    \itemize{
       \item Console output can now be suppressed via 'verbose = TRUE' (#24).
    }
  }


  \item \code{calc_CosmicDoseRate()}{
    \itemize{
       \item Console output can now be suppressed via 'verbose = TRUE' (#24).
    }
  }

    \item \code{calc_FastRatio()}{
    \itemize{
       \item New arguments 'Ch_L2' and 'Ch_L3' to allow the user to specify
       custom values for channels L2 and L3. Feature requested by A. Versendaal (#29).
    }
  }

  \item \code{calc_FadingCorr()}{
    \itemize{
       \item Fixed a bug where the function would crash when providing an
       \code{RLum.Results} object for \code{g_value},
       \item new argument \code{interval} to control the age interval for solving the equation
       via \code{uniroot}.

    }

  }

  \item \code{calc_FiniteMixture()}{
    \itemize{
       \item Fixed a bug where certain arguments where not passed to `plot_RLum.Results`
       so that the plot was not as customisable as intended.
       Thanks to Daniele Questiaux for reporting the bug.
    }
  }

  \item \code{calc_MaxDose()}{
    \itemize{
       \item Fixed a bug in the console output, which provided wrong values for the
       asymmetric error on gamma (Note that the values in the output object
       were correct!). Thankfully reported by Xue Rui.
    }
  }

    \item \code{calc_Statistics()}{
    \itemize{
       \item The argument \code{n.MC} got a new value \code{NULL} which is now
       used as default. With this the basic statistical measures are in accordance
       with the expectations (e.g., the standard deviation is returned by default in
       the conventional way and not calculated using an MC simulation).
    }
  }


    \item \code{calc_OSLLxTxRatio()}{
    \itemize{
       \item Add argument \code{use_previousBG} to use the background of the Lx-curve to
       get the net signal of the Tx-curve (request #15).
    }
  }

      \item \code{fit_CWCurve()}{
    \itemize{
       \item Change order of \code{RLum.Results} output list elements,
       \item rename first element to \code{data},
       \item add element slot 'info'.
    }
  }

   \item \code{fit_LWCurve()}{
    \itemize{
       \item Change order of \code{RLum.Results} output list elements,
       \item rename first element to \code{data},
       \item add element slot 'info'.
    }
  }

  \item \code{model_LuminescenceSignals()}{
    \itemize{
       \item Update function arguments to account for changes in RLumModel version 0.2.0.
      }
    }


    \item \code{plot_DetPlot()}{
      \itemize{
        \item Correct negative y-axis, the minimum is now the real minimum.
      }

    }

      \item \code{plot_GrowthCurve()}{
        \itemize{
           \item Reduce number of confusing warning,
           \item add new argument \code{mode} to select the calculation mode of the function.
           This allows in particular to only fit data without calculating a De or calculating
           a De assuming an additive dose response curve,
           \item account for the very specifc case that all dose points are similar. The function
           stops with an error and returns NULL,
           \item under weird circumstances points on the growth curve were not plotted correctly; fixed.

        }

      }

    \item \code{plot_RadialPlot()}{
        \itemize{
           \item Sometimes the function crashed with an out of bonds error if more than one data set
           was provided; fixed,
           \item argument \code{negatives} caused an error if not set to \code{'remove'} and fix
           some errors around this option,
           \item De-adjustment for negative values optimised for large scatter.

        }

      }

    \item \code{plot_RLum.Analysis()}{
        \itemize{
           \item The usage of the argument \code{smooth} led to a crash; fixed.

        }

    }

   \item \code{plot_RLum.Data.Curve()}{
        \itemize{
           \item Function will not stop anymore if the curve contains \code{NA} values, but if
           the curve consists of only \code{NA} values.

        }

    }

     \item \code{plot_RLum.Data.Spectrum()}{
        \itemize{
           \item The interactive plot option was broken with the last update of the
           package 'plotly'; fixed.

        }

    }

      \item \code{plot_ViolinPlot()}{
        \itemize{
           \item The function erroneously produced a NA value warning; fixed.

        }

    }


    \item \code{read_BIN2R()}{
        \itemize{

        \item If BIN-files are automatically imported the function skipped non BIN-files
        without crashing if it is used in combination with the argument \code{pattern},
        \item add new argument \code{irgnore.RECTYPE} to provide a solution for broken BIN-files
        are BIN-files with non documented entries. Furthermore the general behaviour for such
        cases had been optimised.
      }
     }

     \item \code{read_Daybreak2R()}{
        \itemize{

        \item Add support for DAT-files produced by at 1100 reader using the software (TLAPLLIC v.3.2).
        Thanks to Antoine Zink,
        \item minor error corrections and adding example code.

      }
     }


    \item \code{template_DRAC()}{
        \itemize{

        \item Fixed a typo in the column names (#28).

      }

     }


   \item \code{use_DRAC()}{
    \itemize{
       \item Now supports DRAC v1.2 and the newly introduced CSV input template.
       Older v1.1 excel sheet input templates are still supported, but users
       are highly encouraged to use the new CSV file.

       \item Columns in the output tables are now assigned proper classes (#27).
    }
  }

    }
  }

}

  \subsection{Internals}{

   \itemize{
      \item The internal function converting BIN-file curves to RLum.Data.Curve() objects had been
      optimised and, amongst others, now uses a function written using Rcpp to create the curve matrix.
      The conversion now works ca. two times faster,

      \item add \code{`[<-]`} method for \code{RLum.Data.Curve} objects,

      \item a hint on how to cite a function is now added automatically to every major function manual page,

      \item add 'magrittr' to the package dependencies (imports) to further support the usage of this amazing
      pipe operator,

      \item thanks to Johannes Friedrich this release introduces regular unit tests using
      the package 'testthat' to improve the code quality and stability,

      \item add internal helper function \code{.smoothing}; no Rd entry.

    }

  }

}

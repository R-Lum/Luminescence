\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.7.0 ( ,2016)}{

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

        \item \code{calc_WodaFuchs2008()}:
        Function to obtain the equivalent dose using
        the approach proposed by Woda and Fuchs, 2008.

        \item \code{PSL2Risoe.BINfileData()}:
        Converts an \code{RLum.Analysis} object produced by the function
        \code{read_PSL2R()} to an \code{Risoe.BINfileData} object.


        \item \code{read_PSL2R()}:
        Imports PSL files produced by a SUERC portable OSL reader into R.


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


      }



    }

  \subsection{Bugfixes and changes}{

  \itemize{

  \item \code{analyse_SAR.CWOSL()}{
    \itemize{
       \item Fix wrongly set threshold value for recuperation rate (#26).
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
           a De assuming an additive dose reponse curve.

        }

      }

    \item \code{plot_RadialPlot()}{
        \itemize{
           \item Sometimes the function crashed with an out of bonds error if more than one data set
           was provided; fixed,
           \item argument \code{negatives} caused an error if not set to \code{'remove'} and fix
           some errors around this option,
           \item argument \code{negatives} renamed to \code{negatives.rm} and changed to type
           \code{logical}.

        }

      }

     \item \code{plot_RLum.Data.Spectrum()}{
        \itemize{
           \item The interactive plot option was broken with the last update of the
           package 'plotly'; fixed.

        }

    }


    \item \code{read_BIN2R()}{
        \itemize{

        \item If BIN-files are automatically imported the function skipped non BIN-files
        without crashing if it is used in combination with the argument \code{pattern}.
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

  \subsection{Internals}{

   \itemize{
      \item The internal function converting BIN-file curves to RLum.Data.Curve() objects had been
      optimised and, amongst others, now uses a function written using Rcpp to create the curve matrix.
      The conversion now works ca. two times faster,

      \item add \code{`[<-]`} method for \code{RLum.Data.Curve} objects,

      \item a hint on how to cite a function is now added automatically to every major function manual page,

      \item add 'magrittr' to the package dependencies (imports) to further support the usage of this amazing
      pipe operator.

    }

    }


  }

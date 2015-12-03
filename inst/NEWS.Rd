\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.5.0 (4th, December, 2015)}{

  \subsection{General remarks}{
  \itemize{
  \item This major update provides, apart from few new functions, many bugfixes and non-visible
  improvements in the underlying code to keep the growing package still usable and efficient.
  The most important change is provided with the support of S3-generics.
  For example: The well known R function \code{plot()} can now deal with \code{RLum}-objects.
  However, this improvement also went along with a more standardised way how RLum objects can be treated.
  In particular, to access RLum-objects only one function is necessary: \code{get_RLum()}.
  The function will subsequently dispatch the right method on the basis of the provided object.
  Due to this a couple of functions had bee deprecated. See below for details.
  }

  }

  \subsection{Changes S4-objects, methods, classes}{
   \itemize{
      \item Extend class definition for \code{RLum}-class by slots \code{originator} and \code{.uid}
      (and with that for all \code{RLum}classes),


      \item \code{get_RLum}
      \itemize{
        \item the method for the \code{RLum.Results} object returned \code{NULL} if nothing was
        defined for a specific function, corrected. Now, if nothing is defined the first  object
        in the slot \code{data} is returned (#223),
        \item the method was enhanced to deal with objects of type \code{list} containing only
        \code{RLum} objects. Depending on the \code{RLum} objects in the list the corresponding
         \code{get_RLum} methods
        are called,
        \item argument \code{keep.object} (for \code{RLum.Analysis} objects) has been replace by
        \code{drop}. The default is \code{drop = TRUE}, this mean common behaviour changes
        as former it was \code{keep.object = TRUE}. However, \code{drop} is behaviour consistent
        with standard R code,
        \item argument \code{data.object} (\code{get_RLum} for 'RLum.Results' objects can now handle
        numeric values) and numeric, as well as character vectors,
        \item argument \code{drop} also added for \code{get_RLum} method for 'RLum.Results',
        \item add argument \code{null.rm} for input of type \code{list} of \code{RLum} objects, with
        this option the resulting \code{list} can be cleaned up,
        \item argument \code{record.id} (\code{RLum.Analysis} objects) now accepts as input \code{logical}
        values. This was necessary to be consistent with standard R behaviour.

      }

      \item \code{merge_RLum}
      \itemize{
        \item Method for \code{RLum.Results} objects now combines the data.frame using the function
        \code{data.table::rbindList} this should give a little bit more speed,
        \item the method was not working properely for objects of types \code{RLum.Results} if they
        tried to combine matrices and vectors found in the 'data.object' list.

      }

      \item \code{length_RLum}
       \itemize{
        \item Method added for \code{RLum.Results} objects.

       }

      \item \code{names_RLum}
       \itemize{
        \item Method added for \code{RLum.Data.Curve}, \code{RLum.Data.Spectrum},
        \code{RLum.Data.Image}, \code{RLum.Analysis} and \code{RLum.Results} objects.

       }

       \item \code{replicate_RLum}
       \itemize{
        \item New method to replicate RLum objects (valid for all objects)

       }


     \item \code{Risoe.BINfileData-class}
      \itemize{
        \item Show method now returns the valid grain range,
        \item the method \code{get_Risoe.BINfileData} was never active, but the corresponding message
        on using the function \code{Risoe.BINfileData2RLum.Analysis()} disappeared
        if a user tried to pass additional (unused) arguments. Corrected (thanks to Alice Versendaal).
      }

      \item {RLum-class}
      \itemize{
        \item Two new slots have been added: \code{originator} and \code{.uid}. Due to inheritance
        they are now available for all \code{RLum} objects and both are set automatically
        if an \code{RLum} object is created.

      }

      \item {RLum.Data.Curve}
      \itemize{
        \item The corresponding \code{set_RLum} method of this class now comes with default values,
        corresponding to the prototype of this class and the method itself does not check any longer
        for missing values, means, empty objects can be produced,
        \item furthermore the  \code{set_RLum} method now accepts an \code{RLum.Data.Curve} object
        as input for the argument \code{data}. With this the object can be changed (e.g. recordType)
        without de-constructing the object,
        \item add coercing method from and to \code{list}.

      }

        \item {RLum.Data.Spectrum}
      \itemize{
        \item orrect coercion method for \code{RLum.Data.Spectrum} to a \code{data.frame},
        before only the first two columns had been returned.

      }

       \item {RLum.Analysis}
      \itemize{
        \item Add method for coercing to a \code{list} and the way back.

      }

      \item {RLum.Results}
      \itemize{
        \item Add method for coercing to a \code{list} and the way back.
      }

   }
  }

  \subsection{New functions}{
   \itemize{
      \item \code{calc_gSGC()} Calculate De value based on the gSGC by Li et al., 2015,
      \item \code{plot_NRt()} Creates a Natural/Regenerated signal vs. time (NR(t)) plot as shown in
      Steffen et al. 2009 (#234),
       \item \code{read_Daybreak2R()} Imports ASCII data produced by a Daybreak TL/OSL reader,
       \item \code{use_DRAC()} Provides an R interface to the 'Dose Rate Calculator' (DRAC) (#219),
       \item \code{template_DRAC()} Creates a DRAC input template (v1.1) to be used for \code{use_DRAC()},
       \item \code{plot_ViolinPlot()} Creates a combination of a kernel density plot with a boxplot.
       This plot was especially written for visualisation of data where standard errors are zero or
       to small to be visualised at all.
   }
  }

  \subsection{Bugfixes and changes}{
   \itemize{


      \item \code{Analyse_SAR.OSLdata()}
      \itemize{

        \item Argument \code{log} removed, but option still supported via \code{...} (means
        log can be used as usual but it is not listed as explicit argument),
        \item enhanced support for graphical parameters such as: \code{mtext}


      }

      \item \code{analyse_IRSAR.RF()}
      \itemize{

        \item Argument \code{fit.trace} removed. It can be controlled by the new argument \code{method.control}
        instead,
        \item argument \code{slide.show_density} removed and can now be controlled by within the new argument
        \code{method.control},
        \item argument \code{method.control} added allowing to control parameters of the chosen method,
        \item now also the sliding matrix is returned with the RLum.Results 'data.object' \code{slide},
        \item the way the error is calculated was changed again to account for drawbacks if always
        just the global minimum is chosen (details see manual),
        \item adjust the threshold finding for the criterium 'curve_bounds' to ensure that a warning message
        is shown, if results should not be used,
        \item the 'curve_bounds' message will no longer be shown, if the natural curve was limited
        by the argument \code{RF_nat.lim} and within the curve limits the bounds have not been reached,
        \item fix a bug that forces the function to crash if a limited atural curve reached the end
        of the regenerated curve after sliding and remains longer (#254),
        \item if no IRSL curve is detected it is now stated so as plot,
        \item some graphical adjustments.


      }


  \item \code{analyse_pIRIRSequence()}
      \itemize{

        \item Now acts a little bit more relaxed if something goes wrong, function does not break,
        but returns \code{NULL} instead,
        \item argument \code{dose.points} now pre-set with \code{NULL},
        \item function now supports a list of RLum-objects as input,
        \item add data.object \code{call} to the output,
        \item the plot device is no longer changed if \code{plot = FALSE}.

      }



      \item \code{analyse_SAR.CWOSL()}
      \itemize{

        \item Internal curve selection now works using the \code{get_RLum}. This should give
        a little bit more flexibility and prevent errors with odd curve names,
        \item in the rare case where no recuperation rate can be calculated due to the missing
        R0 point the function stopped with an error. This has been corrected.
        \item function now how to deal with a \code{list} of \code{RLum.Analysis} objects and will
        re-call itself as elements are in the \code{list} (argument \code{object}). In particular
        cases this may lead to a tremendous efficient data analysis process,
        \item add 'call' to data output object,
        \item add 'UID' column to data object 'De.values' and 'rejection.criteria',
        \item if the function is not able to proceed to due to unequal number of OSL curves the
        analysis will no longer break with an error. Instead \code{NULL} is returned. This allows
        to use the function more efficiently in a loop,
        \item improve behaviour if wrong background integrals are provided, even the values make
        no sense the functions tries to do the best without breaking,
        \item plot argumet \code{log = "x"} results in non-plotting of the Lx/Tx curves, fixed,
        \item support for independent Lx and Tx integral limites added,
        \item argument \code{dose.points} pre-set with \code{NULL},
        \item correct self call behaviour.

      }

       \item \code{analyse_SAR.TL()}
      \itemize{

        \item New argument \code{dose.points} and its functionality added.

      }


      \item \code{calc_CentralDose()}
       \itemize{

        \item Fixed a bug in the \code{RLum.Results} object, which returned false profile
        log-likelihood values for \code{sigma}. Report was gratefully provided by Eike F. Rades.

        \item Fixed a bug in the calculation of the standard error of sigma ('overdispersion'),
        which was carried over from RGs original S script

        \item new argument \code{log} added to apply the un-logged central age model

      }

       \item \code{calc_OSLLxTxRatio()}
       \itemize{

        \item The function produced nonsense if only Lx data are provided as RLum.Data.Curve object;
        corrected (raised by Andrea Junge),

        \item new arguments added \code{signal.integral.Tx} and \code{background.integral.Tx} to
        allow for independent signal and background integrals for the Lx and Tx curves (suggested
        by Norbert Mercier). To avoid compatibly problems by default integrals are similar for
        Lx and Tx,

        \item new output data object \code{call} added.

      }

      \item \code{calc_SourceDoseRate()}
       \itemize{

        \item Function now returns the original function call as data.object,
        \item argument \code{predict} added to allow for source dose rate prediction in days,
        \item adding plotting method for object output, available via (\code{plot_RLum}),
        \item extend examples.



      }

       \item \code{calc_Statistics()}
       \itemize{

        \item Weighted median is now completly removed from the calculation, values is replaced
        by the unweighted median,
        \item parameter \code{digits} added to allow rounding to the number of specified digits.


      }


     \item \code{fit_LMCurve()}
      \itemize{

        \item Argument \code{output.plot} renamed to \code{plot},
        \item argument \code{output.plotBG} renamed to \code{plot.BG},
        \item arguemtn \code{output.terminal} renamed to \code{verbose},
        \item argument \code{output.path} removed without replacement, corresponding code has been
        removed from the function,
        \item argument \code{cex.global} removed, scaling can be still used via \code{cex}
        \item argument \code{output.terminaladvanced} has been removed without replacement, terminal
        output can just be triggered via \code{verbose},
        \item new argument added \code{fit.method}. Argument works similar as in \code{fit_CWCurve()}
        and allows to use the Levenberg-Marquardt algorithm for fitting (addressing #55)
        \item new return data object added \code{call} containing the original function call.



      }

      \item \code{merge_RLum()}
      \itemize{

        \item The function now removes empty list objects before proceeding.

      }

     \item \code{merge_RisoeBINfileData()}
      \itemize{

        \item The position number of the merged BIN-files was not appropriately recalculated due
        to a coding error (#251). Report and fix was gratefully provided by Alice Versandaal.

      }


      \item \code{plot_AbanicoPlot()}
       \itemize{

        \item Further adjustment to deal with zero error values (will now be removed automatically),
        \item if the De was zero or negative the function stopped with an error, changed to a
        warning message and a return of \code{NULL} (this is a much better behaviour when
        the function is used within a loop, #253),
        \item make sure that the graphical parameters are restored properly after the function was run,
        \item function now properly accepts \code{par()} settings to allow multiple plots on one
        page, e.g., \code{par(mfrow = c(3,3))} (#247),
        \item function now accepts direct input from \code{RLum.Results} objects produced
        by the function \code{analyse_SAR.CWOSL()} without error (#245),
        \item function no longer breaks if an empty data set is provided (#244).


      }

     \item \code{plot_DRTResults()}
       \itemize{

        \item Function struggled with RLum.Results objects as input; corrected.
        \item improve graphical representation using groups of boxplots.


      }

      \item \code{plot_GrowthCurve()}
       \itemize{

        \item Final fitting and fitting during the MC runs is now done using the Levenberg-Marquardt
        from the package 'minpack.lm'. This
        increases the fitting stability and the computation step markedly (#5),
        \item output object is enhanced by the \code{call} containing the function call,
        \item for the option \code{fit.method = "LIN"} and \code{fit.method = "EXP OR LIN"} weights
        had been not considered; corrected,
        \item add argument \code{fit.force_through_origin}, it does exactly what you may expect from such
        argument (#225, requested by Alice Versendaal),
        \item get rid of some not very informative warning messages if some fitting failed and if start
        parameters are not set sufficiently,
        \item add argument \code{txtProgressBar} and \code{verbose} to provide more output options
        (this is helpful when writing tutorials),
        \item new fit options added \code{QDR} (polynomial function) to account for super-linear
        signal grow,
        \item improve fitting behaviour for modelled data, especially causing errors due to singular
        convergence failures,
        \item add \code{De.MC}, the De values from the MC runs, to the output.


      }


     \item \code{plot_RLum()}
      \itemize{

        \item The \code{...} argument \code{main} and \code{mtext} can be now be of type \code{list} if a \code{list} of
        objects is provided. This allows to set different plot titles for this specific plot option,
        \item on the top of this argument \code{mtext} will be set automatically if the object is of
        type \code{RLum.Analysis} to indicate the different records.

      }


      \item \code{plot_RLum.Analysis()}
      \itemize{

        \item The \code{...} argument \code{main} can now be provided as vector if \code{combine = TRUE},
        \item this methods now supports a vector for the argument \code{main},
        \item arguments \code{ncols} and \code{nrows} are no optional, the functions tries to set
        a good value.


      }

      \item \code{plot_RLum.Results()}{

        \itemize{

          \item Argument \code{data} renamed to \code{object} to be consistent within. the package.

        }

      }

    \item \code{readBIN2R()}
      \itemize{

        \item Set function on deprecated, new name: \code{read_BIN2R()},
        \item new argument \code{position} implemented that allows to limit the import to specific positions
        (this will have, however, no positive effect on the import performance),
        \item add argument \code{...} to let arguments pass to the function \code{Risoe.BINfileData2RLum.Analysis},
        this case the argument \code{fastForward} will forced to \code{TRUE},
        \item argument \code{file} now supports a \code{list} of file names or just a path, in this
        case the functions tries to detect and import all possible BIN/BINX files found in this path,
        \item in really seldom cases, if an old BIN-file (03) version was used, the 'LTYPE' become OSL, while
        the used 'LIGHTSOURCE' was "IR diodes/IR Laser". Sometimes this led to unexpected behaviour in the
        subsequent analysis functions. Now the import function checks imported BIN-files (only version 03) for
        such discrepancy and correct the 'LTYPE' accordingly,
        \item import speed improvements by using lookup talbes instead of 'sapply'-translation matrices,
        \item add argument \code{duplicated.rm} to account for duplicated entries that might have been
        produced during single grain measurements (thanks for Norbert Mercier).


      }


    \item \code{readSPE2R()}
      \itemize{

        \item Set function on deprecated, new name: \code{read_SPE2R()}.

      }


      \item \code{readXSYG2R()}
      \itemize{

        \item Set function to deprecated, new name: \code{read_XSYG2R()},
        \item the argument \code{file} can now handle a \code{list} of files, along with this
        just a path to directory is allowed instead a path to a file.
        In this case the function tries to import all XSYG-files that can be found in the directory,
        \item the curves were not 'named' IRSL if laser diods instead of LEDs are facilitated in the
        lexsyg reader; fixed\
        \item the element 'stimulator' and 'detecteur' appeared twice in the info element list after
        import, corrected.,
        \item functioned gained a new argument \code{pattern} to be used if more than one XSYG-file is
        imported at one time,
        \item all hard function stops had been removed (e.g., wrong file name etc.), instead a warning
        is shown and \code{NULL} is returned. This ensures that the function can be safely run within
        a loop,
        \item pressing the turbo key by more efficient coding.

      }

      \item \code{Risoe.BINfileData2RLum.Analysis()}
      \itemize{

        \item If the lower temperature for the TL curve calculation was not at 0, wrong shifted
        TL curves had been produced (this happens normally for BIN-files produced by FI lexsyg readers),
        \item along with this the x-values for the curves are now consequently calculated using
        the LOW and the HIGH values given in the BIN-files. This may cause a shift of the surves
        towards 0 for CW-curves (#235),
        \item TL curves can furthermore consists of different parts, including preheat and
        a second TL rise. This was not respected during calculting the x-axis values, leading
        to wrong scaled temperature axis, corrected (#255).


      }

     \item \code{Risoe.BINfileData2RLum.Analysis()}
      \itemize{

        \item A new argument \code{grain} can be used to further limit the transformation to
        specific grains in the data set,
        \item the arguement \code{pos} is no longer required. If nothing is provided every position
        will be transformed and the function returns a list of \code{RLum.Analysis}-objects,
        \item along with this the argument \code{pos} now accepts a vector as input (i.e., more than
        on position can be provided for the conversion),
        \item new argument \code{txtProgressBar} added. The default is set to \code{FALSE} as the
        computation time just become relevant for single grain data,
        \item an error was fixed that occurred if the curves used for the transformation had been
        of zero length,
        \item function nows uses \code{Risoe.BINfileData2RLum.Data.Curve} for curve conversion.

      }

   \item \code{Seconds2Gray()}
      \itemize{

        \item Add new method for the error calculation \code{omit} and set this method
        to 'default' to avoid biasing the De-distributions,
        \item argument \code{method} renamed to \code{error.propagation}.

      }



      \item \code{writeR2BIN()}
      \itemize{

        \item Set function to deprecated, new name: \code{write_R2BIN()}

      }

     }
  }

   \subsection{Internals}{
   \itemize{
      \item Re-adding the package 'zoo' in imports,
      \item Adding package 'readxl' to imports (used in \code{use_DRAC} to import the XLS file)
      \item Adding package 'httr' to imports (used in \code{use_DRAC} to establish the connection)
      \item This package is now linked to 'Rcpp' and 'RcppArmadillo' (used within the function
      \code{.analyse_IRSARRF_SRS)},
      \item Package 'rgl' is no longer requested, but suggested. This frees most of the users from
      installing this package as it just be used by one specific plot type for \code{RLum.Data.Spectrum} objects.
      \item Remove dependency to package 'assertive',
      \item The slot 'originator' in \code{RLum.Results} is no longer explicit, but inherited from
      the class \code{RLum},
      \item Added methods for S3 generics \code{`$`, `[<-`, `[[<-`, print, as.data.frame}
      for objects of class \code{DRAC.list},
      \item Added methods for S3 generics \code{print} for objects of class \code{DRAC.highlights}
      \item Roxygen2: replaced \code{@exportPattern ^[[:alpha:]]+} with with explicit \code{@export}
      statements to allow better NAMESPACE housekeeping,
      \item Roxygen2: The documentation for ExampleData.DeValues was not produced automatically. Corrected.
      \item \code{merge_RLum.Results} now became an explicit function, instead of a S4-method for the sake
      of consistency.
      \item Add the following methods for S3 generics for RLum-objects. Please note that so far all
      these generics are not properely documented within the package as the implementation is not
      finished yet.
      \itemize{

        \item \code{`[`} : \code{RLum.Data.Curve}, \code{RLum.Data.Spectrum}, \code{RLum.Data.Image}, \code{RLum.Analysis}, \code{RLum.Results}
        \item \code{`[[`} : \code{RLum.Analysis}, \code{RLum.Results}
        \item \code{`$`} :  \code{RLum.Analysis}, \code{RLum.Results}
        \item \code{`+`} : \code{RLum.Data.Curve}
        \item \code{`-`} : \code{RLum.Data.Curve}
        \item \code{`*`} : \code{RLum.Data.Curve}
        \item \code{`"\"`} : \code{RLum.Data.Curve}
        \item \code{plot()} : \code{RLum.Data.Curve}, \code{RLum.Data.Image}, \code{RLum.Data.Image},
        \code{RLum.Data.Spectrum}, \code{RLum.Analysis}, \code{RLum.Results}, \code{Risoe.BINfileData}
        \item \code{hist()}: \code{RLum.Results}, \code{RLum.Data.Image}, \code{RLum.Data.Curve},
        \code{RLum.Analysis}
        \item \code{summary()}: \code{RLum.Results}, \code{RLum.Data.Image}, \code{RLum.Data.Curve},
        \code{RLum.Analysis}
        \item \code{length()}: \code{RLum.Data.Curve}, \code{RLum.Analysis}, \code{Risoe.BINfileData},
        \code{RLum.Results}
        \item \code{nrow()}: \code{RLum.Data.Curve}, \code{RLum.Data.Spectrum}
        \item \code{unlist()}: \code{RLum.Analysis}
        \item \code{dim()}: \code{RLum.Data.Curve}, \code{RLum.Data.Spectrum}
        \item \code{row.names()}: \code{RLum.Data.Spectrum}
        \item \code{names()}: \code{RLum.Data.Curve}, \code{RLum.Data.Spectrum}, \code{RLum.Data.Image},
        \code{RLum.Analysis}, \code{RLum.Results}, \code{Risoe.BINfileData}
        \item \code{as.data.frame()}: \code{RLum.Data.Curve}
        \item \code{as.list()}: \code{RLum.Data.Curve},\code{RLum.Analysis} ,\code{RLum.Results}
        \item \code{as.matrix()}:  \code{RLum.Data.Curve}
        \item \code{merge()}: \code{RLum}
        \item \code{rep()}: \code{RLum}


      }


   }
  }



}

\name{NEWS}
\title{NEWS for the R Package Luminescence}
\section{Changes in version 0.5.0 (, 2015)}{

  \subsection{Changes S4-objects, methods, classes}{
   \itemize{
      \item Extend class definition for \code{RLum}-class by slots \code{originator} and \code{.uid}
      (and with that for all \code{RLum}classes)
   }
  }

  \subsection{New functions}{
   \itemize{
      \item \code{calc_gSGC()} Calculate De value based on the gSGC by Li et al., 2015
      \item \code{plot_NRt()} Creates a Natural/Regenerated signal vs. time (NR(t)) plot as shown in Steffen et al. 2009
       \item \code{read_Daybreak2R()} Imports data prodcued by a Daybreak TL/OSL reader
       \item \code{use_DRAC()} Provides an R interface to
   }
  }

  \subsection{Bugfixes and changes}{
   \itemize{
      \item \code{analyse_SAR.CWOSL()}
      \itemize{

        \item Internal curve selection now works using the \code{get_RLum}. This should give
        a little bit more flexibility and prevent errors with odd curve names,
        \item in the rare case where no recuperation rate can be calculated due to the missing
        R0 point the function stopped with an error. Corrected.

      }

     \item \code{plot_RLum()}
      \itemize{

        \item The \code{...} argument \code{main} can be now be of type \code{list} if a \code{list} of
        objects is provided. This allows to set different plot titles for this specific plot option.

      }


      \item \code{plot_RLum.Analysis()}
      \itemize{

        \item The \code{...} argument \code{main} can now be provided as vector if \code{combine = TRUE}.

      }

     }
  }

   \subsection{Internals}{
   \itemize{
      \item Re-adding the package 'zoo' in imports,
      \item adding package 'readxl' to imports (used in \code{use_DRAC} to import the XLS file)
      \item adding package 'httr' to imports (used in \code{use_DRAC} to establish the connection)


   }
  }



}



\name{NEWS}
\title{NEWS for the R Package Luminescence}

\section{Changes in version 0.4.0 (XX XXXX, 2014)}{

   \subsection{New S4-classes, methods}{
   \itemize{   
  
    \item \code{RLum.Data.Image}: S4-class for handling image data including. Image
    data in slot \code{data} are stored as \code{RasterBrick}

    \item \code{set_RLum.Data.Image()}: Set method for class \code{RLum.Data.Image}
    
    \item \code{get_RLum.Data.Image()}: get method for class \code{RLum.Data.Image}
    
    \item further: corresponding show and coercing methods added for class. 
  }
  }


   \subsection{New functions}{
   \itemize{   
  
    \item \code{readSPE2R()}: Add support Princenton Instruments *.SPE files
    (camera and spectrometer measurements). Function output is a \code{RLum.Data.Spectrum}
    or \code{RLum.Data.Image} object.   
    
   \item \code{plot_RLum.Data.Image()}: Plot function for \code{RLum.Data.Image} object.
   Function also added to wrapper function \code{plot_RLum}
  
    \item \code{get_Layout()}: Helper function to return a list with layout definitions for
      more homogenous plotting.
      
   \item \code{analyse_pIRIRSequence()}: Function to analyse post-IR IRSL sequence
   measurements.
   
   \item \code{merge_RLum()}: Wrapper functions for various merge for \code{RLum}-objects (#139).
   
   
  }
  
  
  }


  \subsection{Changes S4-objects, methods, classes}{
   \itemize{   
    
     \item \code{get_RLum.Analysis()}: Add support for inverse \code{record.id} selection, e.g. \code{record.id = -1} selects
     all objects except the first. 
    
    \item \code{get_RLum.Results()}: Add support for data objects for objects
    produced by the function \code{calc_OSLLxTxRatio()}
    
    \item \code{Risoe.BINfileData2RLum.Analysis()}: If the \code{pos} value
    is invalid now the function does not return an error any longer. Instead 
    \code{NA} is returned.
    
  
  }
  }
  
   \subsection{Bugfixes and changes}{
   \itemize{   
   
      \item \code{plot_RLum.Data.Spectrum()}:
      \itemize{
      
        \item Correct an error that caused a chrash when \code{xlim} was set 
        in a way that only one wavelength (colour value) was remained,
        
        \item correct zebra-colour-pattern for rare cases of \code{xlim} settings
        using \code{plot.type = "persp"} or \code{plot.type = "persp3d"},
        
        \item add argument new plot type \code{"transect"} for plotting a transect
        along the time/temperature axis for a specific wavelength/channel intervall
        (#120, requested by Regina DeWitt),
        
        \item correct function break for a single column matrix (single frame spectrum)
        by automatically resetting \code{plot.type} to \code{single} 
        for single frame spectra,
        
        \item colour rug (\code{rug = TRUE}) implemented for plot type \code{"multiple.lines"}
        (#130), 
        
        \item argument \code{xaxis.energy} for wavelength energy conversion for
        2D plots added (#141).
         
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
        \code{Formula}.
      
      }
      
      \item \code{calc_OSLLxTxRatio()}: 
      \itemize{
      
      \item Correct an error that causes \code{NA} values
      within the curve errors for rare combinations of signal and background 
      integreation limits, 
      \item provide a name for the output data frame \code{LxTx.table} for the
      \code{RLum.Results} object,
      
      \item error fixed for the marginal case that k = 2 and exceed the maximal curve length. In that 
      case accidentially no error was calculated. 
      
      }
      
      \item \code{fit_CWCurve()}: 
      \itemize{
      
      \item Implement \code{...} argument to replace explicit arguments \code{ylab}, 
      \code{xlab} and \code{main},
      
      \item Fix an issue that causes an error when input values were <= 0
      (#123 reported by Johanna Lomax),
      
      \item Correct wrong y-axis labelling (wrong channel resolution value).        
      }
      
      \item \code{analyse_IRSAR.RF()}:
      \itemize{
      
        \item Add \code{...} support for \code{mtext}
      
      }
      
      \item \code{plot_Histogram()}:
      \itemize{
      
        \item Change default y-axis labelling from 'Counts' to 'Frequency'
      
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
        \item minor modifications of layout definitions.
        
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
        
        \item output object \code{Formula} added to return the fromula of the 
         growth curve, 
         
         \item output object \code{De.values} extended for D01, D02 and information
         on the applied fittin, 
        
        \item numerical output extended for information on the choosen signal and
        background integration limits (#122), 
        
        \item enhance argument \code{output.plot.single} for numerical values. 
        This allows to select each single plot output individually, 
        
        \item details from the argument \code{rejection.criteria} were removed 
        from the argument list to allow a more flexible handling (e.g. changing only
        one parameter at time) and to clean up the argument list. However, the argument
        work still as excpected (#145).
        
        \item minor bug fixes (e.g., not properly caluculated TL curve resolution) and graphical adjustements.
      
      }
      
       \item \code{Analyse_SAR.OSLdata()}:
       
       \itemize{
       
        \item Error message implemented for non OSL data sets (#130, Geogrina King)
       
       }
       
       
       
      
        \item \code{plot_RLum.Data.Curve()}:
      \itemize{
      
        \item Add argument \code{norm} to allow for curve normalisation to 
        the highest value (#129).
      
      }
      
      
      \item \code{calc_MaxDose3()} & \code{calc_MaxDose4()}:
      
        \itemize{
        
        \item bootstrap method (recycled) as described in Cunningham & Wallinga (2012) implemented,
        \item upper and lower plot margins of profile log likelihood plots decreased,
        
        \item function now iteratively adjusts boundaries for gamma, sigma and p0 when one of the estimators reaches the lower or upper boundary,
        
        \item new arguments: \code{boundary.sigma}, \code{boundary.gamma}, \code{init.values}, \code{...} (\code{bs.M}, \code{bs.N}, \code{bs.h}, \code{sigmab.sd}),
        
        \item removed/merged arguments: \code{sigma.xlb}, \code{sigma.xub}, \code{gamma.xlb}, \code{gamma.xub}, \code{init.gamma}, \code{init.sigma}, \code{init.p0}, \code{output.indices}
        
        }
   
   
   \item \code{readBIN2R()}:
      \itemize{
      
        \item Integrity checks added to avoid unspecific errors for wrong input 
        formats. 
      
      }
   
   
   
   }
   
 }
 
  \subsection{Internals}{
   \itemize{   
  
    \item Add new dependency for package \code{raster}, which is used 
    for the \code{RLum.Data.Image-class},

    \item dependency to package Rserve removed.  
  }
  }

}

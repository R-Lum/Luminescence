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
  }
  
  
  
  
  }


  \subsection{Changes S4-objects, methods, classes}{
   \itemize{   
    
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
        for single frame spectra.
         
      }
      
      \item \code{plot_GrowthCurve()}:
      \itemize{
      
        \item Minor graphical and code polishing,
      
      }
      
      \item \code{calc_OSLLxTxRatio()}: 
      \itemize{
      
      \item Correct an error that causes \code{NA} values
      within the curve errors for rare combinations of signal and background 
      integreation limits, 
      \item provide a name for the output data frame \code{LxTx.table} for the
      \code{RLum.Results} object
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
        \item multiple 2-sigma-bars allowed.
      
      }
      
      
      
       \item \code{analyse_SAR.CWOSL()}:
      \itemize{
      
        \item Clarify behaviour when no OSL curve is provided; function
        now stops with an error (#132, reported by Alice Versendaal)
      
      }
      
        \item \code{plot_RLum.Data.Curve()}:
      \itemize{
      
        \item Add argument \code{norm} to allow for curve normalisation to 
        the highest value (#129)
      
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
    for the \code{RLum.Data.Image-class}
  }
  }

}

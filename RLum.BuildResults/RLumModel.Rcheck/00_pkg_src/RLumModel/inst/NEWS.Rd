\name{NEWS}
\title{NEWS for the R Package RLumModel}
\section{Changes in version 0.1.2 (2nd September, 2016)}{

\subsection{Bugfixes and changes:}{
\itemize{

    \item{added vignette():}

	  \itemize{
	    \item Added vignette 'Getting started with RLumModel'.}

    \item{set_ODE():}

	  \itemize{
	    \item Remove 'b', 'temp', 'R' and 'P' from unpacking, because these parameters are already stored in parameters.step.
	    \item Remove distinction of cases 'model = Pagonis' or 'model = Bailey' due to a misstake in Bailey 2001, 2002 and 2004. The rate equation for the valence band is wrong. Thanks to an unknown reviewer of Friedrich et al. 2016.}
	    
	 \item{set_ODE_LM_OSL():}

	   \itemize{
	   \item Remove 'a', 'b', 'temp', 'R' and 'P' from unpacking, because these parameters are already stored in parameters.step.
	   \item Remove distinction of cases 'model = Pagonis' or 'model = Bailey' due to a misstake in Bailey 2001, 2002 and 2004. The rate equation for the valence band is wrong. Thanks to an unknown reviewer of Friedrich et al. 2016.}
	    
	    
	 \item{simulate_CW_OSL():}

	  \itemize{
	    \item Changed ode solver to 'ode' with method 'bdf' for faster solving long illumination times.
	    }
	    
	 \item{simulate_illumination():}

	  \itemize{
	    \item Changed ode solver to 'ode' with method 'bdf' for faster solving long illumination times.
	    }
    
    \item{simulate_irradiation():}

	  \itemize{
	    \item Changed choice of pair-production-rate 'R', so that Bailey 2004 has the right value of 2.5e10.
	    }
	    
	   \item{simulate_LM_OSL():}

	  \itemize{
	    \item Changed ode solver to 'ode' with method 'bdf' for faster solving long illumination times.
	    }
	    
	 \item{simulate_RF():}

	  \itemize{
	    \item Changed choice of pair-production-rate 'R', so that Bailey 2004 has the right value of 2.5e10.
	    }

    }
  }
}

\section{Changes in version 0.1.1 (2nd May, 2016)}{

 \itemize{
  \item Changed tests because of new package structure of 'testthat'.


 }

}

\section{Changes in version 0.1.0 (3rd February, 2016)}{

 \itemize{
  \item Initial version.


 }

}



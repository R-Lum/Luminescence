\name{calc_OSLLxTxRatio}
\alias{calc_OSLLxTxRatio}
\title{Calculate Lx/Tx ratio for CW-OSL curves}
\description{Calculate Lx/Tx ratios from a given set of CW-OSL curves. }
\usage{calc_OSLLxTxRatio(Lx.data, Tx.data, signal.integral, background.integral, 
    background.count.distribution = "non-poisson", sigmab)}
\arguments{
  \item{Lx.data}{\link{data.frame} (\bold{required}): requires a CW-OSL shine down curve 
(x = time, y = counts)}
  \item{Tx.data}{\link{data.frame} (optional): requires a CW-OSL shine down curve 
(x = time, y = counts). If no input is given the Tx.data will 
be treated as \code{NA} and no Lx/Tx ratio is calculated.}
  \item{signal.integral}{\link{vector} (\bold{required}): vector with the limits 
for the signal integral.}
  \item{background.integral}{\link{vector} (\bold{required}): vector with the bounds for the 
background integral.}
  \item{background.count.distribution}{\link{character} (with default): Sets the count distribution assumed for the 
error calculation. Possible arguments \code{poisson} or \code{non-poisson}.
See details for further information}
  \item{sigmab}{\link{numeric} (optional): Option to set a manual value for the overdispersion (for LnTx and 
TnTx), used for the Lx/Tx error calculation. The value should be provided as absolute
squared count values, e.g. \code{sigmab = c(300,300)}. 
Note: If only one value is provided
this value is taken for both (LnTx and TnTx) signals.}
}
\details{The integrity of the chosen values for the signal and background integral 
is checked by the function; 
the signal integral limits have to be lower than the background integral limits. 
If a \link{vector} is given as input instead of a \link{data.frame}, 
an artificial \code{data.frame}
is produced. The error calculation is done according to Galbraith (2002).\cr

\bold{background.count.distribution}\cr

This argument allows selecting the distribution assumption that is used for
the error calculation. According to Galbraith (2002, 2014) the background 
counts may be overdispersed (i.e. do not follow a poisson distribution, which 
is assumed for the photomultiplier counts). 
In that case (might be the normal case) it has to be accounted for the 
overdispersion by estimating \eqn{\sigma^2} (i.e. the overdispersion value).
Therefore the relative standard error is calculated as:\cr\cr
(a) \code{poisson}\cr
\deqn{rse(\mu_{S}) \approx \sqrt(Y_{0} + Y_{1}/k^2)/Y_{0} - Y_{1}/k}
(b) \code{non-poisson}\cr
\deqn{rse(\mu_{S}) \approx \sqrt(Y_{0} + Y_{1}/k^2 + \sigma^2(1+1/k))/Y_{0} - Y_{1}/k}}
\value{Returns an S4 object of type \code{\linkS4class{RLum.Results}}. 
Slot \code{data} contains a \code{\link{list}} with the following structure:\cr 
$ LxTx.table (data.frame)
.. $ LnLx  \cr        
.. $ LnLx.BG   \cr     
.. $ TnTx    \cr       
.. $ TnTx.BG    \cr   
.. $ Net_LnLx   \cr   
.. $ Net_LnLx.Error\cr 
.. $ Net_TnTx.Error\cr
.. $ LxTx\cr
.. $ LxTx.Error \cr
$ calc.parameters (list)\cr
.. $ sigmab.LnTx\cr
.. $ sigmab.TnTx\cr
.. $ k}
\references{Duller, G., 2007. Analyst. \url{http://www.nutech.dtu.dk/english/~/media/Andre_Universitetsenheder/Nutech/Produkter\%20og\%20services/Dosimetri/radiation_measurement_instruments/tl_osl_reader/Manuals/analyst_manual_v3_22b.ashx}\cr

Galbraith, R.F., 2002. A note on the variance of a background-corrected 
OSL count. Ancient TL, 20 (2), 49-51. 

Galbraith, R.F., 2014. A further note on the variance of a background-corrected OSL count.
Ancient TL, 31 (2), 1-3.}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France) \cr
R Luminescence Package Team}
\note{The results of this function have been cross-checked with the Analyst (vers. 3.24b).   
Access to the results object via \code{\link{get_RLum.Results}}.}


\seealso{\code{\link{Analyse_SAR.OSLdata}}, \code{\link{plot_GrowthCurve}}, 
\code{\link{analyse_SAR.CWOSL}}}
\examples{

##load data
data(ExampleData.LxTxOSLData, envir = environment())

##calculate Lx/Tx ratio
results <- calc_OSLLxTxRatio(Lx.data, Tx.data, signal.integral = c(1:2), 
                             background.integral = c(85:100))

##get results object
get_RLum.Results(results)

}

\keyword{datagen}
\section{Function version}{0.4.5 (2014-12-20 10:59:21)}

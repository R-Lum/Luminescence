\name{calc_TLLxTxRatio}
\alias{calc_TLLxTxRatio}
\title{Calculate the Lx/Tx ratio for a given set of TL curves [beta version]}
\description{Calculate Lx/Tx ratio for a given set of TL curves. }
\usage{calc_TLLxTxRatio(Lx.data.signal, Lx.data.background, Tx.data.signal, 
    Tx.data.background, signal.integral.min, signal.integral.max)}
\arguments{
  \item{Lx.data.signal}{\link{data.frame} (\bold{required}): TL data 
(x = temperature, y = counts) (TL signal)}
  \item{Lx.data.background}{\link{data.frame} (optional): TL data (x = temperature, 
y = counts). If no data are provided no background subtraction is performed.}
  \item{Tx.data.signal}{\link{data.frame} (\bold{required}): TL data 
(x = temperature, y = counts) (TL test signal)}
  \item{Tx.data.background}{\link{data.frame} (optional): TL data (x = temperature, 
y = counts). If no data are provided no background subtraction is performed. }
  \item{signal.integral.min}{\link{integer} (\bold{required}): channel number for the 
upper signal integral bound (e.g. \code{signal.integral.min = 100})}
  \item{signal.integral.max}{\link{integer} (\bold{required}): channel number for the 
upper signal integral bound (e.g. \code{signal.integral.max = 200})}
}
\details{-}
\value{Returns an S4 object of type \code{\linkS4class{RLum.Results}}. 
Slot \code{data} contains a \link{data.frame} with the following 
structure:\cr\cr 
$ LnLx  \cr        
$ LnLx.BG   \cr     
$ TnTx    \cr       
$ TnTx.BG    \cr   
$ Net_LnLx   \cr   
$ Net_LnLx.Error\cr }
\references{-}
\author{Sebastian Kreutzer, JLU Giessen/Freiberg Instruments (Germany),
Christoph Schmidt, University of Bayreuth (Germany),\cr
R Luminescence Package Team}
\note{\bold{This function is a beta version!}}


\seealso{\code{\linkS4class{RLum.Results}}, \code{\link{analyse_SAR.TL}}}
\examples{
         
##load package example data
data(ExampleData.BINfileData, envir = environment())

##convert Risoe.BINfileData into a curve object
temp <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)


Lx.data.signal <- get_RLum.Analysis(temp, record.id=1)
Lx.data.background <- get_RLum.Analysis(temp, record.id=2)
Tx.data.signal <- get_RLum.Analysis(temp, record.id=3)
Tx.data.background <- get_RLum.Analysis(temp, record.id=4)
signal.integral.min <- 210
signal.integral.max <- 230

output <- calc_TLLxTxRatio(Lx.data.signal, 
                           Lx.data.background, 
                           Tx.data.signal, Tx.data.background, 
                           signal.integral.min, signal.integral.max)
get_RLum.Results(output)
                        
}

\keyword{datagen}
\section{Function version}{0.2.1 (2014-06-13 11:04:18)}

\name{calc_MinDose}
\alias{calc_MinDose}
\title{Apply the (un-)logged minimum age model (MAM) after Galbraith et al. (1999) to a given De distribution}
\description{Function to fit the (un-)logged three or four parameter minimum dose model 
(MAM-3/4) to De data.}
\usage{calc_MinDose(data, sigmab, log = TRUE, par = 3, bootstrap = FALSE, 
    init.values, plot = TRUE, ...)}
\arguments{
  \item{data}{\code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
for \code{data.frame}: two columns with De \code{(data[ ,1])} and
De error \code{(values[ ,2])}}
  \item{sigmab}{\code{\link{numeric}}  (\bold{required}): spread in De values given as a 
fraction (e.g. 0.2). This value represents the expected overdispersion in
the data should the sample be well-bleached (Cunningham & Walling 2012, p. 100).}
  \item{log}{\code{\link{logical}} (with default): fit the (un-)logged 
minimum dose model to De data}
  \item{par}{\code{\link{numeric}} (with default): apply the 3- or 4-parametric minimum age
model (\code{par=3} or \code{par=4}). The MAM-3 is used by default.}
  \item{bootstrap}{\code{\link{logical}} (with default): apply the recycled bootstrap approach of Cunningham & Wallinga (2012).}
  \item{init.values}{\code{\link{numeric}} (optional): a named list with starting values for gamma, sigma, p0 and mu
(e.g. \code{list(gamma=100 sigma=1.5, p0=0.1, mu=100)}).
If no values are provided reasonable values are tried to be estimated from the data.}
  \item{plot}{\code{\link{logical}} (with default): plot output
(\code{TRUE}/\code{FALSE})}
  \item{\dots}{(optional) further arguments for bootstrapping (\code{bs.M, bs.N, bs.h, sigmab.sd}). 
See details for their usage.}
}
\details{\bold{Parameters} \cr\cr
This model has four parameters: \cr\cr
\tabular{rl}{
\code{gamma}: \tab minimum dose on the log scale \cr
\code{mu}: \tab mean of the non-truncated normal distribution \cr
\code{sigma}: \tab spread in ages above the minimum \cr
\code{p0}: \tab proportion of grains at gamma \cr }
If \code{par=3} (default) the 3-parametric minimum age model is applied,
where \code{gamma=mu}. For \code{par=4} the 4-parametric model is applied
instead.\cr\cr
\bold{(Un-)logged model} \cr\cr
In the original version of the three-parameter minimum dose model, the 
basic data are the natural logarithms of the De estimates and relative 
standard errors of the De estimates. This model will be applied if 
\code{log=TRUE}. \cr\cr
If \code{log=FALSE}, the modified un-logged model will be applied 
instead. This has essentially the same form as the original version. 
\code{gamma} and \code{sigma} are in Gy and \code{gamma} becomes the
minimum true dose in the population. \cr\cr
While the original (logged) version of the mimimum dose model may be
appropriate for most samples (i.e. De distributions), the modified 
(un-logged) version is specially designed for modern-age and young
samples containing negative, zero or near-zero De estimates (Arnold 
et al. 2009, p. 323). \cr\cr
\bold{Initial values & boundaries} \cr\cr
The log likelihood calculations use the \link{nlminb} function for
box-constrained optimisation using PORT routines. 
Accordingly, initial values for the four parameters can be specified via
\code{init.values}. If no values are provided for \code{init.values} reasonable starting 
values are estimated from the input data. 
If the final estimates of \emph{gamma}, \emph{mu}, 
\emph{sigma} and \emph{p0} are totally off target, consider providing custom
starting values via \code{init.values}. \cr
In contrast to previous versions of this function the boundaries for the
individual model parameters can no longer be specified. Appropriate boundary
are now hard-coded and are valid for all input data sets. \cr\cr
\bold{Bootstrap} \cr\cr
When \code{bootstrap=TRUE} the function applies the bootstrapping method as
described in Wallinga & Cunningham (2012). By default, the minimum age model 
produces 1000 first level and 3000 second level bootstrap replicates
(actually, the number of second level bootstrap replicates is three times 
the number of first level replicates unless specified otherwise). 
The uncertainty on sigmab is 0.04 by default. These values can be changed by
using the arguments \code{bs.M} (first level replicates), \code{bs.N}
(second level replicates) and \code{sigmab.sd} (error on sigmab). With \code{bs.h}
the bandwidth of the kernel density estimate can be specified. By default, \code{h} 
is calculated as \cr
\deqn{h = (2*\sigma_{DE})/\sqrt{n}}}
\value{Returns a plot (optional) and terminal output. In addition an 
\code{\linkS4class{RLum.Results}} object is 
returned containing the following elements:

\item{summary}{\link{data.frame} summary of all relevant model results.}
\item{data}{\link{data.frame} original input data}
\item{args}{\link{list} used arguments}
\item{call}{\link{call} the function call}
\item{mle}{\link{mle2} \code{mle2} object containing the maximum log likelhood functions for all parameters}
\item{BIC}{\link{numeric} BIC score}
\item{confint}{\link{data.frame} confidence intervals for all parameters}
\item{profile}{\link{profile.mle2} the log likelihood profiles}
\item{bootstrap}{\link{list} bootstrap results}

The output should be accessed using the function 
\code{\link{get_RLum.Results}}  }
\references{Arnold, L.J., Roberts, R.G., Galbraith, R.F. & DeLong, S.B., 2009. A revised
burial dose estimation procedure for optical dating of young and modern-age 
sediments. Quaternary Geochronology 4, 306-325. \cr\cr
Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission 
track ages. Nuclear Tracks Radiation Measurements 4, 459-470. \cr\cr
Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley, J.M., 
1999. Optical dating of single grains of quartz from Jinmium rock shelter, 
northern Australia. Part I: experimental design and statistical models. 
Archaeometry 41, 339-364. \cr\cr
Galbraith, R.F., 2005. Statistics for Fission Track Analysis, Chapman & 
Hall/CRC, Boca Raton. \cr\cr
Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose
and error calculation and display in OSL dating: An overview and some
recommendations. Quaternary Geochronology 11, 1-27. \cr\cr
\bold{Further reading} \cr\cr
Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain 
equivalent dose (De) distributions: Implications for OSL dating of sediment 
mixtures. Quaternary Geochronology 4, 204-230. \cr\cr
Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain 
quartz De distributions and an assessment of procedures for estimating burial
dose. Quaternary Science Reviews 25, 2475-2502. \cr\cr
Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial
archives using robust OSL chronologies. Quaternary Geochronology 12, 
98-106. \cr\cr
Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the
reproducibility and accuracy of optical dating of fluvial deposits. 
Quaternary Geochronology 1, 109-120. \cr\cr
Rodnight, H., 2008. How many equivalent dose values are needed to obtain a
reproducible distribution?. Ancient TL 26, 3-10. \cr\cr}
\author{Christoph Burow, University of Cologne (Germany) \cr
Based on a rewritten S script of Rex Galbraith, 2010 \cr
The bootstrap approach is based on a rewritten MATLAB script of Alastair Cunningham. \cr
Alastair Cunningham is thanked for his help in implementing and cross-checking the code.\cr
R Luminescence Package Team}
\note{The default starting values for \emph{gamma}, \emph{mu}, 
\emph{sigma} and \emph{p0} may only be appropriate for some De data sets 
and may need to be changed for other data. This is especially true when
the un-logged version is applied. \cr 
Also note that all R warning messages are suppressed when running this
function. If the results seem odd consider re-running the model with
\code{debug=TRUE} which provides extended console output and forwards
all internal warning messages.}


\seealso{\code{\link{calc_CentralDose}},
\code{\link{calc_CommonDose}}, \code{\link{calc_FiniteMixture}},
\code{\link{calc_FuchsLang2001}}, \code{\link{calc_MaxDose}}}
\examples{

## load example data
data(ExampleData.DeValues, envir = environment())

# apply the un-logged, 3-parametric minimum age model
calc_MinDose(data = ExampleData.DeValues$CA1, 
             par = 3, 
             sigmab = 0.2, 
             log = TRUE)

# re-run the model, but save results to a variable
mam <- calc_MinDose(data = ExampleData.DeValues$CA1, 
                   par = 3, 
                   sigmab = 0.2, 
                   log = FALSE, 
                   plot = FALSE)

# show summary table
get_RLum.Results(mam, "summary")

# plot the log likelihood profiles retroactively
plot_RLum.Results(mam)
}
\section{Function version}{0.4.1 (2015-03-04 13:29:41)}

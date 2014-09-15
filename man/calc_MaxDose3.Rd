\name{calc_MaxDose3}
\alias{calc_MaxDose3}
\title{Apply the maximum age model to a given De distribution }
\description{Function to fit the maximum age model to De data. This function is a
modified version of the three parameter minimum age model after Galbraith
et al. (1999) using a similiar approach described in Olley et al. (2006).}
\usage{calc_MaxDose3(input.data, sigmab, log = TRUE, sample.id = "unknown sample", 
    gamma.xlb = 0.1, gamma.xub = 100, sigma.xlb = 0.001, sigma.xub = 5, 
    init.gamma = 10, init.sigma = 1.2, init.p0 = 0.01, ignore.NA = FALSE, 
    calc.ProfileLikelihoods = TRUE, console.ProfileLikelihoods = FALSE, 
    console.extendedOutput = FALSE, output.plot = FALSE, output.indices = 3)}
\arguments{
  \item{input.data}{\code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
for \code{data.frame}: two columns with De \code{(input.data[,1])} and
De error \code{(values[,2])}}
  \item{sigmab}{\code{\link{numeric}}  (\bold{required}): spread in De values given as a 
fraction (e.g. 0.2). This value represents the expected overdispersion in
the data should the sample be well-bleached (Cunningham & Walling 2012, p. 100).}
  \item{log}{\code{\link{logical}} (with default): fit the (un-)logged three parameter 
maximum dose model to De data. An un-logged version is currently not
supported}
  \item{sample.id}{\code{\link{character}} (with default): sample id}
  \item{gamma.xlb}{\code{\link{numeric}} (with default): lower boundary of gamma}
  \item{gamma.xub}{\code{\link{numeric}} (with default): upper boundary of gamma}
  \item{sigma.xlb}{\code{\link{numeric}} (with default): lower boundary of sigma }
  \item{sigma.xub}{\code{\link{numeric}} (with default): upper boundary of sigma }
  \item{init.gamma}{\code{\link{numeric}} (with default): starting value of gamma }
  \item{init.sigma}{\code{\link{numeric}} (with default): starting value of sigma}
  \item{init.p0}{\code{\link{numeric}} (with default): starting value of p0 }
  \item{ignore.NA}{\code{\link{logical}} (with default): ignore NA values during log
likelihood calculations. See details.}
  \item{calc.ProfileLikelihoods}{\code{\link{logical}} (with default): calculate profile log likelihood
functions for gamma, sigma, p0. See \code{output.indices}.}
  \item{console.ProfileLikelihoods}{\code{\link{logical}} (with default): print profile log likelihood
functions for gamma, sigma, p0 to console.}
  \item{console.extendedOutput}{\code{\link{logical}} (with default): extended terminal output }
  \item{output.plot}{\code{\link{logical}} (with default): plot output
(\code{TRUE}/\code{FALSE})}
  \item{output.indices}{\code{\link{numeric}} (with default): requires 
\code{calc.ProfileLikelihoods} = \code{TRUE}. Indices: 1 = gamma, 
2 = gamma/sigma, 3 = gamma/sigma/p0.}
}
\details{\bold{Parameters} \cr\cr
This model has three parameters: \cr
\tabular{rl}{
\code{gamma}: \tab maximum dose on the log scale \cr
\code{sigma}: \tab spread in ages above the maximum \cr
\code{p0}: \tab proportion of grains at gamma \cr }
\bold{Data transformation} \cr\cr
To estimate the maximum dose population and its standard error, the three 
parameter minimum age model of Galbraith et al. (1999) is adapted. The
measured De values are transformed as follows: \cr\cr
1. convert De values to natural logs \cr
2. multiply the logged data to creat a mirror image of the De distribution\cr
3. shift De values along x-axis by the smallest x-value found to obtain 
only positive values \cr
4. combine in quadrature the measurement error associated with each De value
with a relative error specified by sigmab
5. apply the MAM to these data \cr\cr
When all calculations are done the results are then converted
as follows\cr\cr
1. subtract the x-offset \cr
2. multiply the natural logs by -1 \cr
3. take the exponent to obtain the maximum dose estimate in Gy \cr\cr
\bold{(Un-)logged model} \cr\cr
In the original version of the three-parameter maximum dose model, the basic
data are the natural logarithms of the De estimates and relative standard 
errors of the De estimates. This model will be applied if \code{log = TRUE}.
\cr\cr
If \code{log = FALSE}, the modified un-logged model will be applied instead.
This has essentially the same form as the original version. \code{gamma} and 
\code{sigma} are in Gy and \code{gamma} becomes the maximum true dose in 
the population. NOTE: This option is disabled for the maximum dose 
model, as the data transformation requires logged De values! \cr\cr
While the original (logged) version of the mimimum dose model may be 
appropriate for most samples (i.e. De distributions), the modified 
(un-logged) version is specially designed for modern-age and young samples 
containing negative, zero or near-zero De estimates (Arnold et al. 2009,
p. 323). \cr\cr
\bold{Boundaries} \cr\cr
Depending on the data, the upper and lower bounds for gamma (\code{gamma.xlb}
and \code{gamma.xub}) need to be specified. If the final estimate of gamma is
on the boundary, \code{gamma.xlb} and \code{gamma.xub} need to be adjusted 
appropriately, so that gamma lies within the bounds. The same applies for 
sigma boundaries (\code{sigma.xlb} and \code{sigma.xub}). \cr\cr
\bold{Initial values} \cr\cr
The log likelihood calculations use the \link{nlminb} function. Accordingly,
initial values for the three parameters \code{init.gamma}, \code{init.sigma} 
and \code{init.p0} need to be specified. \cr\cr
\bold{Ignore NA values} \cr\cr
In some cases during the calculation of the log likelihoods NA values 
are produced instantly terminating the minimum age model. It is advised to 
adjust some of the values provided for any argument. If the model still
produces NA values it is possible to omit these values by setting 
\code{ignore.NA = TRUE}. While the model is then usually able to finish
all calculations the integrity of the final estimates cannot be ensured.
Use this argument at own risk.}
\value{Returns a plot (optional) and terminal output. In addition an 
\code{\linkS4class{RLum.Results}} object is 
returned containing the following element:

\item{results}{\link{data.frame} with statistical parameters.}

The output should be accessed using the function 
\code{\link{get_RLum.Results}}  }
\references{Arnold, L.J., Roberts, R.G., Galbraith, R.F. & DeLong, S.B., 2009. A revised
burial dose estimation procedure for optical dating of young and modern-age 
sediments. Quaternary Geochronology, 4, pp. 306-325. \cr\cr
Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission 
track ages. Nuclear Tracks Radiation Measurements, 4, pp. 459-470. \cr\cr
Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley, J.M., 
1999. Optical dating of single grains of quartz from Jinmium rock shelter, 
northern Australia. Part I: experimental design and statistical models. 
Archaeometry, 41, pp. 339-364. \cr\cr
Galbraith, R.F., 2005. Statistics for Fission Track Analysis, Chapman & 
Hall/CRC, Boca Raton. \cr\cr
Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose
and error calculation and display in OSL dating: An overview and some
recommendations. Quaternary Geochronology, 11, pp. 1-27. \cr\cr
Olley, J.M., Roberts, R.G., Yoshida, H., Bowler, J.M., 2006. Single-grain
optical dating of grave-infill associated with human burials at Lake Mungo,
Australia. Quaternary Science Reviews, 25, pp. 2469-2474.\cr\cr
\bold{Further reading} \cr\cr
Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain 
equivalent dose (De) distributions: Implications for OSL dating of sediment 
mixtures. Quaternary Geochronology, 4, pp. 204-230. \cr\cr
Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain 
quartz De distributions and an assessment of procedures for estimating burial
dose. Quaternary Science Reviews, 25, pp. 2475-2502. \cr\cr
Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial
archives using robust OSL chronologies. Quaternary Geochronology, 12, 
pp. 98-106. \cr\cr
Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the
reproducibility and accuracy of optical dating of fluvial deposits. 
Quaternary Geochronology, 1, pp. 109-120. \cr\cr
Rodnight, H., 2008. How many equivalent dose values are needed to obtain a
reproducible distribution?. Ancient TL, 26, pp. 3-10. \cr\cr}
\author{Christoph Burow, University of Cologne (Germany) \cr
Based on a rewritten S script of Rex Galbraith, 2010 \cr\cr
R Luminescence Package Team}
\note{The default boundary and starting values for \emph{gamma}, \emph{sigma} and 
\emph{p0} may only be appropriate for some De data sets and may need to be 
changed for other data. An un-logged version is currently not supported.}


\seealso{\code{\link{nlminb}}, 
\code{\link{calc_CentralDose}},
\code{\link{calc_CommonDose}}, \code{\link{calc_FiniteMixture}},
\code{\link{calc_FuchsLang2001}}, \code{\link{calc_MinDose4}}}
\examples{
## load example data
data(ExampleData.DeValues, envir = environment())

## apply the logged maximum dose model
## NOTE THAT THE EXAMPLE DATA SET IS NOT SUITABLE FOR THE
## MAXIMUM DOSE MODEL.
calc_MaxDose3(ExampleData.DeValues,
              sigmab = 0.3, gamma.xub = 4000)
}
\section{Function version}{0.22 (2014-09-15 02:28:00)}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_DoseResponseCurve.R
\name{fit_DoseResponseCurve}
\alias{fit_DoseResponseCurve}
\title{Fit a dose-response curve for luminescence data (Lx/Tx against dose)}
\usage{
fit_DoseResponseCurve(
  object,
  mode = "interpolation",
  fit.method = "EXP",
  fit.force_through_origin = FALSE,
  fit.weights = TRUE,
  fit.includingRepeatedRegPoints = TRUE,
  fit.NumberRegPoints = NULL,
  fit.NumberRegPointsReal = NULL,
  fit.bounds = TRUE,
  n.MC = 100,
  txtProgressBar = TRUE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{object}{\link{data.frame} or a \link{list} of such objects (\strong{required}):
data frame with columns for \code{Dose}, \code{LxTx}, \code{LxTx.Error} and \code{TnTx}.
The column for the test dose response is optional, but requires \code{'TnTx'} as
column name if used. For exponential fits at least three dose points
(including the natural) should be provided. If \code{object} is a list,
the function is called on each of its elements.
If \code{fit.method = "OTORX"} you have  to provide the test dose in the same unit
as the dose in a column called \code{Test_Dose}. The function searches explicitly
for this column name. Only the first value will be used assuming a constant
test dose over the measurement cycle.}

\item{mode}{\link{character} (\emph{with default}):
selects calculation mode of the function.
\itemize{
\item \code{"interpolation"} (default) calculates the De by interpolation,
\item \code{"extrapolation"} calculates the equivalent dose by extrapolation
(useful for MAAD measurements) and
\item \code{"alternate"} calculates no equivalent dose and just fits the data points.
}

Please note that for option \code{"interpolation"} the first point is considered
as natural dose}

\item{fit.method}{\link{character} (\emph{with default}):
function used for fitting. Possible options are:
\itemize{
\item \code{LIN},
\item \code{QDR},
\item \code{EXP},
\item \verb{EXP OR LIN},
\item \code{EXP+LIN},
\item \code{EXP+EXP} (not defined for extrapolation),
\item \code{GOK},
\item \code{OTOR},
\item \code{OTORX}
}

See details.}

\item{fit.force_through_origin}{\link{logical} (\emph{with default})
allow to force the fitted function through the origin.
For \code{method = "EXP+EXP"} the function will be fixed through
the origin in either case, so this option will have no effect.}

\item{fit.weights}{\link{logical} (\emph{with default}):
option whether the fitting is done with or without weights. See details.}

\item{fit.includingRepeatedRegPoints}{\link{logical} (\emph{with default}):
includes repeated points for fitting (\code{TRUE}/\code{FALSE}).}

\item{fit.NumberRegPoints}{\link{integer} (\emph{optional}):
set number of regeneration points manually. By default the number of all (!)
regeneration points is used automatically.}

\item{fit.NumberRegPointsReal}{\link{integer} (\emph{optional}):
if the number of regeneration points is provided manually, the value of the
real, regeneration points = all points (repeated points) including reg 0,
has to be inserted.}

\item{fit.bounds}{\link{logical} (\emph{with default}):
set lower fit bounds for all fitting parameters to 0. Limited for the use
with the fit methods \code{EXP}, \code{EXP+LIN}, \verb{EXP OR LIN}, \code{GOK}, \code{OTOR}, \code{OTORX}
Argument to be inserted for experimental application only!}

\item{n.MC}{\link{integer} (\emph{with default}):  number of Monte Carlo simulations
for error estimation, see details.}

\item{txtProgressBar}{\link{logical} (\emph{with default}):
enable/disable the progress bar. If \code{verbose = FALSE} also no
\code{txtProgressBar} is shown.}

\item{verbose}{\link{logical} (\emph{with default}):
enable/disable output to the terminal.}

\item{...}{Further arguments to be passed (currently ignored).}
}
\value{
An \code{RLum.Results} object is returned containing the slot \code{data} with the
following elements:

\strong{Overview elements}
\tabular{lll}{
\strong{DATA.OBJECT} \tab \strong{TYPE} \tab \strong{DESCRIPTION} \cr
\code{..$De} : \tab  \code{data.frame} \tab Table with De values \cr
\code{..$De.MC} : \tab \code{numeric} \tab Table with De values from MC runs \cr
\code{..$Fit} : \tab \link{nls} or \link{lm} \tab object from the fitting for \code{EXP}, \code{EXP+LIN} and \code{EXP+EXP}.
In case of a resulting  linear fit when using \code{LIN}, \code{QDR} or \verb{EXP OR LIN} \cr
\code{..Fit.Args} : \tab \code{list} \tab Arguments to the function \cr
\code{..$Formula} : \tab \link{expression} \tab Fitting formula as R expression \cr
\code{..$call} : \tab \code{call} \tab The original function call\cr
}

If \code{object} is a list, then the function returns a list of \code{RLum.Results}
objects as defined above.

\strong{Details - \code{DATA.OBJECT$De}}
This object is a \link{data.frame} with the following columns
\tabular{lll}{
\code{De} \tab \link{numeric} \tab equivalent dose \cr
\code{De.Error} \tab \link{numeric} \tab standard error the equivalent dose \cr
\code{D01} \tab \link{numeric} \tab D-naught value, curvature parameter of the exponential \cr
\code{D01.ERROR} \tab \link{numeric} \tab standard error of the D-naught value\cr
\code{D02} \tab \link{numeric} \tab 2nd D-naught value, only for \code{EXP+EXP}\cr
\code{D02.ERROR} \tab \link{numeric} \tab standard error for 2nd D-naught; only for \code{EXP+EXP}\cr
\code{Dc} \tab \link{numeric} \tab value indicating saturation level; only for \code{OTOR} \cr
\code{D63} \tab \link{numeric} \tab the specific saturation level; only for \code{OTORX} \cr
\code{n_N} \tab \link{numeric} \tab saturation level of dose-response curve derived via integration from the used function; it compares the full integral of the curves (\code{N}) to the integral until \code{De} (\code{n}) (e.g.,  Guralnik et al., 2015)\cr
\code{De.MC} \tab \link{numeric} \tab equivalent dose derived by Monte-Carlo simulation; ideally identical to \code{De}\cr
\code{De.plot} \tab \link{numeric} \tab equivalent dose use for plotting \cr
\code{Fig} \tab \link{character} \tab applied fit function \cr
\code{HPDI68_L} \tab \link{numeric} \tab highest probability density of approximated equivalent dose probability curve representing the lower boundary of 68\% probability \cr
\code{HPDI68_U} \tab \link{numeric} \tab same as \code{HPDI68_L} for the upper bound \cr
\code{HPDI95_L} \tab \link{numeric} \tab same as \code{HPDI68_L} but for 95\% probability \cr
\code{HPDI95_U} \tab \link{numeric} \tab same as \code{HPDI95_L} but for the upper bound \cr

}
}
\description{
A dose-response curve is produced for luminescence measurements using a
regenerative or additive protocol. The function supports interpolation and
extrapolation to calculate the equivalent dose.
}
\details{
\subsection{Implemented fitting methods}{

For all options (except for the \code{LIN}, \code{QDR} and the \verb{EXP OR LIN}),
the \link[minpack.lm:nlsLM]{minpack.lm::nlsLM} function with the \code{LM} (Levenberg-Marquardt algorithm)
algorithm is used. Note: For historical reasons for the Monte Carlo
simulations partly the function \link{nls} using the \code{port} algorithm.

The solution is found by transforming the function or using \link[stats:uniroot]{stats::uniroot}.

\strong{Keyword: \code{LIN}}

Fits a linear function to the data using \link{lm}:
\deqn{y = mx + n}

\strong{Keyword: \code{QDR}}

Fits a linear function with a quadratic term to the data using  \link{lm}:
\deqn{y = a + bx + cx^2}

\strong{Keyword: \code{EXP}}

Adapts a function of the form
\deqn{y = a(1 - exp(-\frac{(x+c)}{b}))}

Parameters b and c are approximated by a linear fit using \link{lm}. Note: \eqn{b = D0}

\strong{Keyword: \verb{EXP OR LIN}}

Works for some cases where an \code{EXP} fit fails. If the \code{EXP} fit fails,
a \code{LIN} fit is done instead, which always works.

\strong{Keyword: \code{EXP+LIN}}

Tries to fit an exponential plus linear function of the form:

\deqn{y = a(1-exp(-\frac{x+c}{b}) + (gx))}
The \eqn{D_e} is calculated by iteration.

\strong{Note:} In the context of luminescence dating, this function has no physical meaning.
Therefore, no \eqn{D_0} value is returned.

\strong{Keyword: \code{EXP+EXP}}

Tries to fit a double exponential function of the form

\deqn{y = (a_1 (1-exp(-\frac{x}{b_1}))) + (a_2 (1 - exp(-\frac{x}{b_2})))}

\emph{This fitting procedure is not really robust against wrong start parameters.}

\strong{Keyword: \code{GOK}}

Tries to fit the general-order kinetics function following Guralnik et al. (2015)
of the form

\deqn{y = a (d - (1 + (\frac{1}{b}) x c)^{(-1/c)})}

where \eqn{c > 0} is a kinetic order modifier
(not to be confused with \strong{c} in \code{EXP} or \code{EXP+LIN}!).

\strong{Keyword: \code{OTOR}} (former \code{LambertW})

This tries to fit a dose-response curve based on the Lambert W function
and the one trap one recombination centre (OTOR)
model according to Pagonis et al. (2020). The function has the form

\deqn{y = (1 + (\mathcal{W}((R - 1) * exp(R - 1 - ((x + D_{int}) / D_{c}))) / (1 - R))) * N}

with \eqn{W} the Lambert W function, calculated using the package \link[lamW:lamW]{lamW::lambertW0},
\eqn{R} the dimensionless retrapping ratio, \eqn{N} the total concentration
of trappings states in cm\eqn{^{-3}} and \eqn{D_{c} = N/R} a constant. \eqn{D_{int}} is
the offset on the x-axis. Please note that finding the root in \code{mode = "extrapolation"}
is a non-easy task due to the shape of the function and the results might be
unexpected.

\strong{Keyword: \code{OTORX}}

This adapts extended OTOR (therefore: OTORX) model proposed by Lawless and Timar-Gabor (2024)
accounting for retrapping. Mathematically, the implementation reads (the equation here
as implemented, it is slightly differently written than in the original manuscript):

\deqn{F_{OTORX} = 1 + \left[\mathcal{W}(-Q * exp(-Q-(1-Q*(1-\frac{1}{exp(1)})) * \frac{(D + a)}{D_{63}}))\right] / Q}

with

\deqn{Q = \frac{A_m - A_n}{A_m}\frac{N}{N+N_D}}

where \eqn{A_m} and \eqn{A_n} are rate constants for the recombination and
the trapping of electrons (\eqn{N}), respectively. \eqn{D_{63}} corresponds to
the value at which the trap occupation corresponds to the 63\% of the saturation value.
\eqn{a} is in an offset. If set to zero, the curve will be forced through the origin
as in the original publication.

For the implementation the calculation reads further

\deqn{y = \frac{F_{OTORX}(((D + a)/D_{63}), Q)}{F_{OTORX}((D_{test} + a)/D_{63}, Q)}}

with \eqn{D_{test}} being the test dose in the same unit (usually s or Gy) as
the regeneration dose points. This value is essential and needs to provided
along with the usual dose and \eqn{\frac{L_x}{T_x}} values (see \code{object} parameter input
and the example section). For more details see Lawless and Timar-Gabor (2024).

\emph{Note: The offset adder \eqn{a} is not part of the formula in Timar-Gabor (2024) and can
be set to zero with the option \code{fit.force_through_origin = TRUE}}

\strong{Fit weighting}

If the option \code{fit.weights =  TRUE} is chosen, weights are calculated using
provided signal errors (\eqn{\frac{L_x}{T_x}} error):
\deqn{fit.weights = \frac{\frac{1}{error}}{\Sigma{\frac{1}{error}}}}

\strong{Error estimation using Monte Carlo simulation}

Error estimation is done using a parametric bootstrapping approach. A set of
\eqn{\frac{L_x}{T_x}} values is constructed by randomly drawing curve data sampled from normal
distributions. The normal distribution is defined by the input values (\code{mean = value}, \code{sd = value.error}). Then, a dose-response curve fit is attempted for each
dataset resulting in a new distribution of single \code{De} values. The standard
deviation of this distribution becomes then the error of the \code{De}. With increasing
iterations, the error value becomes more stable. However, naturally the error
will not decrease with more MC runs.

Alternatively, the function returns highest probability density interval
estimates as output, users may find more useful under certain circumstances.

\strong{Note:} It may take some calculation time with increasing MC runs,
especially for the composed functions (\code{EXP+LIN} and \code{EXP+EXP}).\cr
Each error estimation is done with the function of the chosen fitting method.
}
}
\section{Function version}{
 1.4.0
}

\examples{

##(1) fit growth curve for a dummy data.set and show De value
data(ExampleData.LxTxData, envir = environment())
temp <- fit_DoseResponseCurve(LxTxData)
get_RLum(temp)

##(1b) to access the fitting value try
get_RLum(temp, data.object = "Fit")

##(2) fit using the 'extrapolation' mode
LxTxData[1,2:3] <- c(0.5, 0.001)
print(fit_DoseResponseCurve(LxTxData, mode = "extrapolation"))

##(3) fit using the 'alternate' mode
LxTxData[1,2:3] <- c(0.5, 0.001)
print(fit_DoseResponseCurve(LxTxData, mode = "alternate"))

##(4) import and fit test data set by Berger & Huntley 1989
QNL84_2_unbleached <-
read.table(system.file("extdata/QNL84_2_unbleached.txt", package = "Luminescence"))

results <- fit_DoseResponseCurve(
 QNL84_2_unbleached,
 mode = "extrapolation",
 verbose = FALSE)

#calculate confidence interval for the parameters
#as alternative error estimation
confint(results$Fit, level = 0.68)

\dontrun{
##(5) special case the OTORX model with test dose column
df <- cbind(LxTxData, Test_Dose = 15)
fit_DoseResponseCurve(object = df, fit.method = "OTORX", n.MC = 10) |>
 plot_DoseResponseCurve()

QNL84_2_bleached <-
read.table(system.file("extdata/QNL84_2_bleached.txt", package = "Luminescence"))
STRB87_1_unbleached <-
read.table(system.file("extdata/STRB87_1_unbleached.txt", package = "Luminescence"))
STRB87_1_bleached <-
read.table(system.file("extdata/STRB87_1_bleached.txt", package = "Luminescence"))

print(
 fit_DoseResponseCurve(
 QNL84_2_bleached,
 mode = "alternate",
 verbose = FALSE)$Fit)

print(
 fit_DoseResponseCurve(
 STRB87_1_unbleached,
 mode = "alternate",
 verbose = FALSE)$Fit)

print(
 fit_DoseResponseCurve(
 STRB87_1_bleached,
 mode = "alternate",
 verbose = FALSE)$Fit)
 }

}
\references{
Berger, G.W., Huntley, D.J., 1989. Test data for exponential fits. Ancient TL 7, 43-46. \doi{10.26034/la.atl.1989.150}

Guralnik, B., Li, B., Jain, M., Chen, R., Paris, R.B., Murray, A.S., Li, S.-H., Pagonis, P.,
Herman, F., 2015. Radiation-induced growth and isothermal decay of infrared-stimulated luminescence
from feldspar. Radiation Measurements 81, 224-231. \doi{10.1016/j.radmeas.2015.02.011}

Lawless, J.L., Timar-Gabor, A., 2024. A new analytical model to fit both fine and coarse grained quartz luminescence dose response curves. Radiation Measurements 170, 107045. \doi{10.1016/j.radmeas.2023.107045}

Pagonis, V., Kitis, G., Chen, R., 2020. A new analytical equation for the dose response of dosimetric materials,
based on the Lambert W function. Journal of Luminescence 225, 117333. \doi{10.1016/j.jlumin.2020.117333}
}
\seealso{
\link{plot_GrowthCurve}, \link{nls}, \linkS4class{RLum.Results}, \link{get_RLum},
\link[minpack.lm:nlsLM]{minpack.lm::nlsLM}, \link{lm}, \link{uniroot}, \link[lamW:lamW]{lamW::lambertW0}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
Michael Dietze, GFZ Potsdam (Germany) \cr
Marco Colombo, Institute of Geography, Heidelberg University (Germany)
}

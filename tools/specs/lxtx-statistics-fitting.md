# Port-Spec: calc_OSLLxTxRatio / calc_Statistics / fit_DoseResponseCurve

Extracted 2026-08-12 from the R sources (calc_OSLLxTxRatio.R v0.9.8, calc_Statistics.R
v0.1.8, fit_DoseResponseCurve.R v1.7, src_fit_functions.cpp, internals_RLum.R).
plot_GrowthCurve.R is a pure wrapper around fit_DoseResponseCurve — no numerics.

## 1. calc_OSLLxTxRatio

Signature: `calc_OSLLxTxRatio(Lx.data, Tx.data=None, signal_integral, background_integral,
signal_integral_Tx=None, background_integral_Tx=None, integral_input="channel",
background.count.distribution="non-poisson", use_previousBG=False, sigmab=None,
od_rates=None, sig0=0, digits=None)`.

Validation:
- Lx/Tx must have identical row counts.
- `integral_input="measurement"`: convert times to channels: `imin=argmin(|min(int)-x|)`,
  `imax=argmin(|max(int)-x|)`, result `imin:imax` contiguous — the **Lx** time axis is used
  for all four integrals.
- Integrals: drop out-of-bounds elements (warning), sort/unique; background must start
  strictly after `max(signal_integral)+1`.
- Tx integrals must be supplied together or not at all. `use_previousBG=True` with
  independent Tx limits → warning, Tx limits reset to Lx ones.
- `Tx.data=None` → TnTx and LxTx are NaN.

Alternate mode (`signal_integral is NA`): `LnLx=sum(Lx.y)`, `TnTx=sum(Tx.y)`,
`LxTx=LnLx/TnTx`, all BG/error fields 0, no calc parameters.

k factors: `n=len(sig)`, `m=len(bg)`, `k=m/n`; `m.Tx = m if use_previousBG else len(bg_Tx)`,
`k.Tx=m.Tx/n.Tx`.

Signals (Galbraith symbols): `Y0=sum(Lx.y[sig])`, `Y1=sum(Lx.y[bg])`,
`Lx.background=Y1/k` (0 if bg is NA), `LnLx=Y0-Lx.background`; analogously Tx with `k.Tx`
(`Tx.BG.counts=Lx.BG.counts` and `Tx.background=Lx.background` when use_previousBG).

sigmab (Galbraith 2002 eq.4), computed per curve when neither sigmab nor od_rates given:
```
if round(k,1)>=2 and min(bg)+n_sig*3 <= len(curve):
    Y_i = blockwise sums: floor(k) blocks of n_sig channels starting at min(bg); scale=1
else:
    (warn if m<25) Y_i = per-channel bg values; scale=n_sig
sigmab = max(var(Y_i, ddof=1) - mean(Y_i), 0) * scale
```
Supplied sigmab: `[Lx, Tx]` or scalar for both. sigmab+od_rates → warning, od_rates dropped.

Errors, Galbraith path (default): `rse(Y0,Y1,k,sig)=sqrt(Y0+Y1/k^2+sig*(1+1/k))/(Y0-Y1/k)`;
sig=0 if poisson or bg-NA else sigmab. **QUIRK: Tx rse uses k, NOT k.Tx.**
`Net_*.Error = abs(net * rse)`; NaN → 0 (both paths, Inf not sanitised).

Bluszcz path (od_rates=[B_DC,k_DC,k_ph]): `se(N,t)=sqrt(k_ph^2*N+(k_DC^2-k_ph^2)*B_DC*t)`;
`t` = inclusive integration time from the curve's x column
(`times[max]-(0 if min==1 else times[min-1])`); bg errors scaled by `t_sig/t_bg`;
`Net.Error=sqrt(e_sig^2+e_bg^2)`. Replaces Galbraith entirely; sigmab=NA.

LxTx (via .calculate_LxTx_error): `LxTx=Net_LnLx/Net_TnTx` (NaN→0);
`relErr=sqrt((eL/L)^2+(eT/T)^2)`; `LxTx.Error=abs(LxTx*relErr)` (NaN→0);
then `LxTx.Error=sqrt(LxTx.Error^2+(sig0*LxTx)^2)`. `digits` rounds every column.

Output row (fixed order): LnLx (raw), LnLx.BG, TnTx, TnTx.BG, Net_LnLx, Net_LnLx.Error,
Net_TnTx, Net_TnTx.Error, SN_RATIO_LnLx (NaN if bg NA else Y0/BG), SN_RATIO_TnTx, LxTx,
LxTx.Error. Plus calc.parameters {sigmab.LnLx, sigmab.TnTx, k, od_rates}.

Edge cases: all-zero/negative curves legal; measurement-mode range expansion uses only
min/max; list input maps element-wise.

## 2. calc_Statistics

Signature: `calc_Statistics(data (n,2)=[De, De.Error], weight.calc="inverse_var"|"inverse_std",
digits=None, n.MCM=None, na.rm=True)`.

Preprocessing: drop NA rows (error if empty); missing error column → NA; NA errors → 0;
**if any error == 0 → ENTIRE error column := 1e-9** (warning only when sum==0).

Weights: `w_raw = 1/err` (inverse_std) or `1/err^2` (inverse_var); `w = w_raw/sum(w_raw)`.

MC matrix: `data.MCM[i,j] ~ Normal(De_i, err_i)`, shape (n, n.MCM); without n.MCM the
raw De column.

Statistics (S.n = n rows; used as `n` in ALL three lists):
- unweighted: mean, median, sd (ddof=1), `skew=(1/S.n)*sum(((x-m)/sd)^3)`,
  `kurt=(1/S.n)*sum(((x-m)/sd)^4)` (non-excess).
- weighted: `w.mean=sum(w*x)/sum(w)`; `w.median=.weighted.median` (sort by x, p=cumsum(w)/sum(w),
  n=count(p<0.5); if p[n]>0.5 → x[n] else (x[n]+x[n+1])/2, 0-based);
  `w.sd=sqrt(sum(w*(x-w.mean)^2)/(((S.n-1)*sum(w))/S.n))`.
  **QUIRK: weighted skew/kurt = the UNWEIGHTED values.**
- MCM: mean/median/sd over flattened matrix; skew/kurt normalised by m.n=S.n*n.MCM.

Each list: {n, mean, median, sd.abs, sd.rel=sd/mean*100, se.abs=sd/sqrt(S.n),
se.rel=se.abs/mean*100, skewness, kurtosis}; digits rounds everything.

## 3. fit_DoseResponseCurve

Signature/defaults: `mode="interpolation"|"extrapolation"|"alternate"; fit.method="SSE"
("SSE","LIN","QDR","SSE OR LIN","SSE+LIN","DSE","GOK","OTOR","OTORX"); legacy EXP→SSE,
EXP OR LIN→SSE OR LIN, EXP+LIN→SSE+LIN, EXP+EXP→DSE, LambertW→OTOR;
fit.force_through_origin=False; fit.weights="inverse_var"|"inverse_std"|"norm_inverse_std"
|vector|None; fit.includingRepeatedRegPoints=True; fit.bounds=True; n.MC=100`.
DSE+extrapolation → error.

Input normalisation (order): coerce; <2 cols error; 2 cols → zero error col appended;
Inf→NaN+warning; column detection by lowercase name match of
["dose","lxtx","lxtx.error","tntx","test_dose"] if ≥3 hit, else positional; all-same-dose
→ message+None; all-NA 4th col dropped; incomplete rows dropped (warning), empty → None;
LxTx==0 → 2.220446049250313e-16 (warning); NumberRegPoints=nrow-1.

Row order: **row 0 = natural**; `first.idx=1 (0-based) if interpolation else 0`;
fit data = rows first..NumberRegPoints. includingRepeatedRegPoints=False → drop
duplicated(x) rows (keep first), filtering weights/MC/yError consistently.

Weights: invalid error col (NaN/Inf/0 present) → weights=None+warning. None→1s;
"inverse_std"→1/|e|; "norm_inverse_std"→normalised; "inverse_var"→1/e^2 (default,
unnormalised). Objective: sum(w*(y-f(x))^2), i.e. residual sqrt(w)*(y-f(x)).

MC inputs: per fit row j: pool=rnorm(10000, LxTx_j, |err_j|), column i = sample(pool,
n.MC, replace=True). data.MC.De: interpolation → same scheme with row 0; extrapolation →
zeros. x.natural = NaN vector (n.MC).

Start heuristics: a=max(y); b=1/slope(wlm(log(y)~x)) if any y>0 else 1;
c=|intercept/slope| of wlm(y~x); g=max(y/max(x)). 50 jitters: a.MC~N(a,a/100),
b.MC~N(b,b/100), c.MC=0 if through-origin else N(c,c/100), g.MC~N(g,g) (sd=g!).

Min-data check: num.params = 3 (QDR/SSE/"SSE OR LIN"), 5 (DSE), 4 (others); if
fit.method!="LIN" and nrows < num.params → fall back to LIN (warning).

Models: LIN `y=m*x+b`; QDR `y=a+b*x+c*x^2`; SSE `y=N*(1-exp(-(x+Di)/D0))`;
SSE+LIN `y=N*(1-exp(-(x+Di)/D0)+g*x)` (g INSIDE N); DSE
`y=N1*(1-exp(-(x+Di)/D01))+N2*(1-exp(-(x+Di)/D02))` (5 params incl. Di);
GOK `y=a*(d-(1+(1/D0)*x*c)^(-1/c))`; OTOR `y=(1+W0((R-1)*exp(R-1-(x+Di)/Dc))/(1-R))*N`;
OTORX via `D2nN(D,Q,D63)=1-exp(-D/D63)` if |Q|<1e-6 else
`1+W0(-Q*exp(-Q-(1-Q*(1-1/e))*D/D63))/Q`, y=D2nN(x+Di)/D2nN(Dtest+Di)*c.
All nonlinear fits: Levenberg–Marquardt (nlsLM, maxiter 500) with box bounds → scipy
least_squares(method="trf") on sqrt(w)*(y-f(x)).

SSE procedure: (1) 50 unweighted pre-fits from jittered starts, lower=(0,1e-6,0);
(2) final starts N=median(N.fits), **D0=mean(b.MC)** (not fitted D0s!), **Di=0 hard-coded**;
(3) bounds lower=(0,0,0) if fit.bounds else -Inf; upper=(Inf,Inf,0) if through-origin;
(4) final weighted fit; on failure use last pre-fit. D80=1.609*D0; D01=D0.
De closed form: interpolation `De=-Di-D0*log(1-LnTn/N)` (LnTn=LxTx[0]); extrapolation
`De=-Di`. MC: refit per column, `x.natural[i]=-Di-D0*log(1-data.MC.De[i]/N)`;
D01.ERROR=sd over zero-prefilled length-n.MC vector (failures contribute 0.0).

LIN / SSE-OR-LIN fallback: wlm y~x (or y~x-1); De=(y_target-intercept)/slope
(y_target=LxTx[0] interp, 0 extrap); MC analogous; Fit reported as "LIN".

QDR: weighted lm y~x+x^2; De via uniroot of predict-y_target on [lower, 1.5*max(Dose)],
lower decremented by 10 from 0 (> -1000) while endpoint positive.

SSE+LIN: 50-start loop (SSE prefit seeds params), final starts=medians; lower=(0,**10**,0,0);
De via uniroot(tol=0.001, extendInt="yes", maxiter=3000) on [0 (interp) / -1e6 (extrap),
1.5*max(x)]; **no D01 reported**; MC root-solve without extendInt.

DSE: lower=zeros(5); starts N1=a.MC, N2=N1/2, D01=b.MC, D02=D01/2, Di=c.MC, medians;
De interp-only via uniroot as SSE+LIN; D01/D02 rounded to 2 digits; errors=sd(var).

GOK: single weighted fit, start (a,b,1,1), lower=zeros(4), upper d<=1 if through-origin.
De closed form: `u=(a*d-y)/a; De=-D0*(1-u^(-c))/c`. D01=D0, D01.ERROR=sd.

OTOR/OTORX: deferred (quantile-based errors, fragile extrapolation root finding).

Post-processing: interpolation → De.MC=max(x.natural,0), De.MC.NA=x.natural with neg→NaN;
extrapolation → all = |x.natural|. Scalar De.MC=nanmean(De.MC); **De.Error=nansd(De.MC.NA,
ddof=1)**. HPDI68/95 via Gaussian KDE (nrd0, 512 pts) if >=5 non-NaN. n_N = integral of
fitted curve 0..De divided by 0..max(x). Negative interp De: De=NA (abs elsewhere),
.De.raw keeps sign. Fit failure → valid Results with NA values + fit_message; SSE with NA
coefficients → None.

Output data: De row (De=abs, De.Error, D01, D01.ERROR, D02, D02.ERROR, R/Dc/D63/D80 with
LOWER/UPPER, n_N, De.MC, Fit, Mode, HPDI68_L/U, HPDI95_L/U, .De.plot, .De.raw); De.MC
vector; Fit object; Fit.Args; Formula. No RC.Status here (that's analyse_SAR.CWOSL).

Regression anchors (set.seed(1), n.MC=10, LxTxData): SSE 1737.71, LIN 1673.0, SSE+LIN 1793,
DSE 1786.98, QDR 1646.8, GOK 1786, OTOR 1784.4. fit(results$LnLxTnTx.table) must equal
analyse_SAR.CWOSL's De exactly (extra columns must not perturb).

## 4. Chain conventions (analyse_SAR.CWOSL → fit)

LnLxTnTx.table: Dose prepended (from info IRR_TIME or dose.points); natural row Dose forced
to 0 (warning if !=0, unless alternate). Labels: Name="R0","R1",... by index-1; all
Dose==0 rows named "R0", the first → "Natural"; Repeated=duplicated(Dose) with
Dose==0 → False. Recycling points = Repeated rows; recuperation = R0 rows.
se_ratio(a,sa,b,sb)=a/b*sqrt((sa/a)^2+(sb/b)^2). Fit input frame:
{Dose, LxTx, LxTx.Error, TnTx=Net_TnTx, Test_Dose}.

## 5. Must-reproduce quirks

1. Tx rse uses k, not k.Tx. 2. NaN→0 for net errors/LxTx/LxTx.Error; Inf untouched.
3. calc_Statistics: one zero error → whole column 1e-9. 4. weighted skew/kurt = unweighted.
5. SSE final start D0=mean(b.MC), Di=0. 6. SSE+LIN D0 lower bound 10; no D01 output.
7. D01.ERROR sd over zero-prefilled vectors. 8. R sd/var ddof=1; quantile type 7.
9. De reported abs; negative interp De → NA (raw keeps sign).

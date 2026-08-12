# Port-Spec: analyse_SAR.CWOSL

Extracted 2026-08-12 from analyse_SAR.CWOSL.R (1,497 lines) + RLum.Analysis-class.R.

## Input handling
- Accepts Analysis or list[Analysis] (sequential self-call; merge results; ALQ renumbered
  1..nrow — misaligned if aliquots fail). Risoe.BINfileData NOT accepted (convert first).
- Pre-processing order: validate; trim_channels (optional, truncate OSL|IRSL groups to
  shortest); remove records with recordType starting "_" (auto_curve_removal); determine
  CW curve type = most frequent regex match `(P?OSL[a-zA-Z]*|IRSL[a-zA-Z]*)` over record
  types (none → message+None); strip " .*" from ALL recordTypes (mutates); if
  "irradiation" records: inject info IRR_TIME per record (forward-fill of preceding
  irradiation duration) then drop them; structural checks: CW-curve count must be even
  ("Input OSL/IRSL curves are not a multiple of two"), all CW curves same length.
- Integral validation: measurement→channel conversion uses the FIRST CW curve's x-axis
  (argmin |t-x|, contiguous range); background must start > max(signal); out-of-bounds
  dropped w/ warning; single-channel background → expanded to [v-25, v] (26 ch) w/ warning;
  Tx integrals: sig_Tx defaults to signal_integral (warning) when only bg_Tx given, bg_Tx
  defaults to background_integral when only sig_Tx given; signal NA/None → alternate mode
  (all integrals NA, warning unless OSL.component).
- Rejection-criteria defaults: recycling.ratio=10 (%), recuperation.rate=10 (%),
  palaeodose.error=10 (%), testdose.error=10 (%), sn.ratio=NA (absolute),
  exceed.max.regpoint=True, consider.uncertainties=False, sn_reference="Natural",
  recuperation_reference="Natural". User dict merged over defaults (None survives).

## SAR loop
- CW curve indices split alternating: Lx=idx[0::2], Tx=idx[1::2]. TL curves (recordType
  "TL$") used for plots only: TL.Lx = TL indices that are (OSL.Lx index - 1).
- One call calc_OSLLxTxRatio(Lx list, Tx list, integrals, background.count.distribution,
  sigmab, sig0, od_rates) → list of 1-row tables (integral_input/use_previousBG NOT
  forwarded — integrals already channels).
- Dose column: per Lx record info["IRR_TIME"] else NA; dose.points overrides whole column
  (length must match); error if None and any NA. dose.points.test → Test_Dose col (-1 if
  unset). dose_rate_source: Dose *= rate; Test_Dose likewise (where >= 1).
- Natural reset: if Dose[0]!=0 and mode!="alternate": warn (dose recovery test), Dose[0]=0.
- Labels: Name = "R{i}" by index (0-based → R0,R1,...); all Dose==0 rows → "R0"; first
  zero-dose row → "Natural"; Repeated = duplicated(Dose), False for Dose==0.
- LnLxTnTx.table column order: Name, Repeated, Dose, LnLx, LnLx.BG, TnTx, TnTx.BG,
  Net_LnLx, Net_LnLx.Error, Net_TnTx, Net_TnTx.Error, SN_RATIO_LnLx, SN_RATIO_TnTx, LxTx,
  LxTx.Error, Test_Dose, UID.

## Rejection criteria
- se_ratio(a,sa,b,sb) = a/b*sqrt((sa/a)^2+(sb/b)^2). status: NA threshold → "OK";
  comparator true → "OK" else "FAILED" (NA/NaN value comparison → FAILED, except
  recuperation NA → OK).
- Recycling: repeated rows vs FIRST non-repeated row with same Dose; ratio=rep/prev;
  consider.uncertainties → ratio moved toward 1 by se_ratio; round 4; label
  "Recycling ratio (R5/R1)"; displayed threshold 1±crit/100; status on
  abs(1-ratio) <= recycling.ratio/100.
- Recuperation: rows named R0 / rows named recuperation_reference (invalid ref → error
  listing valid names); minus se_ratio if consider.uncertainties; label
  "Recuperation rate (<ref>) <i>"; threshold rate/100; status <=; NA value → OK.
- Testdose error: (Net_TnTx.Error/Net_TnTx)[0]; threshold /100; <=.
- SN ratio: SN_RATIO_LnLx[index of sn_reference] (invalid → error); threshold ABSOLUTE
  (sn.ratio, default NA); comparator >=.
- Criteria frame: Criteria/Value/Threshold/Status; order recycling*, recuperation*,
  "Testdose error", "Signal-to-noise ratio"; NA-valued rows REMOVED.
- After fit (if run & non-None): append "Palaeodose error" (round(De.Error/De,5) vs /100,
  <=) and "De > max. dose point" (threshold NA if crit NA, Inf if False, max(Dose) if
  True; Value = De - (De.Error if consider.uncertainties); status uses RAW De <=).
  These two are NOT NA-filtered.
- RC.Status = "FAILED" if any Status FAILED else "OK".

## De determination
- Skipped when onlyLxTxTable. fit_DoseResponseCurve(DataFrame(Dose, LxTx, LxTx.Error,
  TnTx=Net_TnTx, Test_Dose), verbose=False, **forwarded kwargs). Failed/None fit → all-NA
  De template (28 cols, in sync with fit output), no palaeodose/exceed rows.

## Output (Results, originator "analyse_SAR.CWOSL")
- data: 1-row frame = fit De frame (28 cols) + RC.Status + signal.range,
  background.range, signal.range.Tx, background.range.Tx (strings "min:max", "NA:NA") +
  ALQ (1; renumbered in list path) + POS + GRAIN + UID.
- POSITION/GRAIN: first unique case-insensitive info["position"]/["grain"] over records,
  numeric, NA if absent.
- LnLxTnTx.table (+UID), rejection.criteria (UID, Criteria, Value, Threshold, Status),
  Formula, .plot.data (skip in port).

## get_RLum(Analysis) contract (Python get_records must support)
- record.id numeric (1-based, negatives exclude) or bool mask, applied FIRST;
- recordType: glob2rx with ^$ stripped → substring/regex grepl match;
- curveType exact; RLum.type class filter; get.index → indices; drop=False → Analysis;
- recursive=True: single match + drop → the record itself; info.object → object info;
- subset expression over curveType/recordType/info columns.
- Empty selection → warning + None.

## Skip in first port
plot* args, deprecated signal.integral.min/max, OSL.component/calc_OSLLxTxDecomposed,
dose.points.test/OTORX, od_rates branch (keep arg), trim_channels (require pre-trimmed),
method_control (hard-code auto_curve_removal=True), .NCF_mode/.aliquot_number.
Keep: integrals, integral_input, rejection.criteria, dose.points, dose_rate_source,
onlyLxTxTable, verbose, background.count.distribution, sigmab, sig0, and fit
pass-throughs (mode, fit.method, fit.weights, fit.force_through_origin,
fit.includingRepeatedRegPoints, n.MC).

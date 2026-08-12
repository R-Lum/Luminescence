# Port-Spec: Risø BIN/BINX Reader (read_BIN2R)

Extracted 2026-08-12; offsets independently verified against inst/extdata/BINfile_V8.binx
and tests/testthat/_data/BINfile_V3.bin. All values little-endian. Versions 3-8.

## Record framing
`[VERSION:1][pad:1][LENGTH:int][PREVIOUS:int][NPOINTS:int][header...][DATA]`
int = i16 for V3/4, i32 for V5-8. LENGTH = total record size incl. VERSION byte; next
record at start+LENGTH. PREVIOUS never used for navigation (may be garbage).
Header sizes: V3/V4 272, V5 423, V6 447, V7 447, V8 507. LENGTH = header + 4*NPOINTS
(RECTYPE 0/1) or header + 504*NPOINTS (V8 RECTYPE 128).
Signedness: R readBin int defaults SIGNED; unsigned only: V8 RECTYPE byte, V3/4
POSITION and RUN bytes.

## V3/V4 header (272 B, offsets from record start)
0 u8 VERSION | 1 pad | 2 i16 LENGTH | 4 i16 PREVIOUS | 6 i16 NPOINTS | 8 i8 LTYPE |
9 f32 LOW | 13 f32 HIGH | 17 f32 RATE | 21 i16 TEMPERATURE | 23 i16 XCOORD |
25 i16 YCOORD | 27 i16 TOLDELAY | 29 i16 TOLON | 31 i16 TOLOFF | 33 u8 POSITION |
34 u8 RUN | 35 str(7) TIME (force6) | 42 str(7) DATE (force6) | 49 str(9) SEQUENCE |
58 str(9) USER | 67 i8 DTYPE | 68 f32 IRR_TIME | 72 i8 IRR_TYPE | 73 i8 IRR_UNIT |
74 f32 BL_TIME | 78 i8 BL_UNIT | 79 f32 AN_TEMP | 83 f32 AN_TIME | 87 f32 NORM1 |
91 f32 NORM2 | 95 f32 NORM3 | 99 f32 BG | 103 i16 SHIFT | 105 str(21) SAMPLE |
126 str(81) COMMENT | 207 i8 LIGHTSOURCE | 208 i8 SET | 209 i8 TAG |
210 i16 GRAINNUMBER | 212 f32 LIGHTPOWER | 216 i16 SYSTEMID |
V3 tail: 218 raw(36) RES1 | 254 f32 ONTIME | 258 f32 OFFTIME | 262 u8 GATE_ENABLED |
263 f32 GATE_START | 267 f32 GATE_STOP | 271 raw(1) RES2.
V4 tail: 218 raw(20) RES1 | 238 i16 CURVENO | 240 f32 TIMETICK | 244 i32 ONTIME |
248 i32 STIMPERIOD | 252 u8 GATE_ENABLED | 253 f32 GATE_START | 257 f32 GATE_STOP |
261 u8 PTENABLED | 262 raw(10) RES2.

## V5-8 header (offsets for V8; V5-7 lack RECTYPE byte at 14 → shift -1 from 15 on;
## V5 additionally lacks IRR_DOSERATEERR → further -4 from 369 on)
0 u8 VERSION | 1 pad | 2 i32 LENGTH | 6 i32 PREVIOUS | 10 i32 NPOINTS |
14 u8 RECTYPE (V8 only) | 15 i16 RUN | 17 i16 SET | 19 i16 POSITION |
21 i16 GRAINNUMBER | 23 i16 CURVENO | 25 i16 XCOORD | 27 i16 YCOORD |
29 str(21) SAMPLE | 50 str(81) COMMENT | 131 i16 SYSTEMID | 133 str(101) FNAME |
234 str(31) USER | 265 str(7) TIME (no force) | 272 str(7) DATE (force6) |
279 i8 DTYPE | 280 f32 BL_TIME | 284 i8 BL_UNIT | 285 f32 NORM1 | 289 f32 NORM2 |
293 f32 NORM3 | 297 f32 BG | 301 i16 SHIFT | 303 i8 TAG | 304 raw(20) RES1 |
324 i8 LTYPE | 325 i8 LIGHTSOURCE | 326 f32 LIGHTPOWER | 330 f32 LOW | 334 f32 HIGH |
338 f32 RATE | 342 i16 TEMPERATURE | 344 i16 MEASTEMP | 346 f32 AN_TEMP |
350 f32 AN_TIME | 354 i16 TOLDELAY | 356 i16 TOLON | 358 i16 TOLOFF |
360 f32 IRR_TIME | 364 i8 IRR_TYPE | 365 f32 IRR_DOSERATE |
369 f32 IRR_DOSERATEERR (V6-8 only) | 373 i32 TIMESINCEIRR | 377 f32 TIMETICK |
381 i32 ONTIME | 385 i32 STIMPERIOD | 389 u8 GATE_ENABLED | 390 i32 GATE_START |
394 i32 GATE_STOP | 398 u8 PTENABLED | 399 u8 DTENABLED | 400 f32 DEADTIME |
404 f32 MAXLPOWER | 408 f32 XRF_ACQTIME | 412 f32 XRF_HV | 416 i32 XRF_CURR |
420 f32 XRF_DEADTIMEF.
Tails: V5 raw(4) → 423. V6 raw(24) → 447. V7/V8: 424 i8 DETECTOR_ID |
425 i16 LOWERFILTER_ID | 427 i16 UPPERFILTER_ID | 429 f32 ENOISEFACTOR; V7 then
raw(15) → 447; V8 then 8×f32 MARKPOS_X1,Y1,X2,Y2,X3,Y3, EXTR_START, EXTR_END at
433-464, raw(42) → 507.

## Strings
Pascal-style in fixed field: read field_length bytes; strlen = buf[0] (unsigned, clamp
to field_length-1); force_size overrides strlen (TIME/DATE V3/4, DATE V5-8: force 6);
strlen 0 → ""; decode latin-1; TRUNCATE AT FIRST NUL byte.

## RECTYPE (V8 only; default column value 0 for V3-7)
Read u8 at offset 14. Order: (1) numeric ignore_rectype == RECTYPE → seek(LENGTH-15),
message "skipped due to ignore.RECTYPE", row dropped (R quirk: temp_ID NOT incremented).
(2) not in {0,1,128}: seek(LENGTH-15); ignore_rectype falsy → error "Byte RECTYPE = r
is not supported in record #n, set 'ignore.RECTYPE = TRUE'"; else message + skip.
(3) 128 → ROI record: only first 15 header bytes valid, seek(+492), parse ROI payload;
all other metadata stays at defaults; reserved → None. (4) 0/1 → normal.
R BUG (do not port): temp.RECTYPE persists across records so V3-7 records after a V8
RECTYPE-128 record misbehave; Python resets rectype=0 per record for version != 8.

## DATA payload
Normal: NPOINTS × i32 LE. SHORT READS MUST NOT RAISE: clamp to bytes remaining
(zero-data-record.binx depends on it). NPOINTS=0 legal → empty vector.
ROI (RECTYPE 128): NPOINTS defs × 504 B each: 0 i32 NOFPOINTS | 4 48×u8 USEDFOR (bool)
| 52 48×u8 SHOWFOR | 100 i32 ROICOLOR | 104 50×f32 X | 304 50×f32 Y.

## Curve matrix (src_create_RLumDataCurve_matrix)
seq_rlum(from,to,n): by=(to-from)/n; values from+by*arange(1,n+1) — excludes `from`,
includes `to`. NPOINTS<=0 → [[nan,nan]].
TL branch (LTYPE=="TL" and VERSION>=4): if TOLON==TOLOFF==TOLDELAY==0 → print
"[src_create_RLumDataCurve_matrix()] BIN/BINX-file non-conform. TL curve may be wrong!"
and TOLOFF=NPOINTS. X = [seq_rlum(LOW,AN_TEMP,TOLDELAY) | AN_TEMP*TOLON |
seq_rlum(AN_TEMP,HIGH,TOLOFF)[:NPOINTS-TOLDELAY-TOLON]]; clamp all slices to
[0,NPOINTS]; pad tail with last value/NaN, never throw. Else X=seq_rlum(LOW,HIGH,NPOINTS).
Note: BINfile_V8.binx hits the non-conform fallback (TL, all TOL 0) → X starts at
AN_TEMP (220), not LOW.

## METADATA (80 columns, exact order)
ID, SEL(=bool(TAG)), VERSION, LENGTH, PREVIOUS, NPOINTS, RECTYPE, RUN, SET, POSITION,
GRAIN(=GRAINNUMBER), GRAINNUMBER, CURVENO, XCOORD, YCOORD, SAMPLE, COMMENT, SYSTEMID,
FNAME, USER, TIME, DATE, DTYPE, BL_TIME, BL_UNIT, NORM1, NORM2, NORM3, BG, SHIFT, TAG,
LTYPE, LIGHTSOURCE, LPOWER(=LIGHTPOWER), LIGHTPOWER, LOW, HIGH, RATE, TEMPERATURE,
MEASTEMP, AN_TEMP, AN_TIME, TOLDELAY, TOLON, TOLOFF, IRR_TIME, IRR_TYPE, IRR_UNIT(V3/4),
IRR_DOSERATE, IRR_DOSERATEERR(V6+), TIMESINCEIRR, TIMETICK, ONTIME, OFFTIME(V3),
STIMPERIOD, GATE_ENABLED, ENABLE_FLAGS(=GATE_ENABLED), GATE_START, GATE_STOP, PTENABLED,
DTENABLED, DEADTIME, MAXLPOWER, XRF_ACQTIME, XRF_HV, XRF_CURR, XRF_DEADTIMEF,
DETECTOR_ID(V7+), LOWERFILTER_ID, UPPERFILTER_ID, ENOISEFACTOR, MARKPOS_X1..Y3,
EXTR_START, EXTR_END (V8), SEQUENCE(V3/4).
Defaults: 0 for LENGTH/PREVIOUS/NPOINTS/RECTYPE/POSITION/GRAIN/GRAINNUMBER; NA/NaN/""
for everything else. Int-read-but-float-stored: TEMPERATURE, MEASTEMP, TIMESINCEIRR,
ONTIME, OFFTIME, GATE_START/STOP, XRF_CURR, GATE_ENABLED, PTENABLED, DTENABLED.
LTYPE/DTYPE/LIGHTSOURCE stored as str (raw int stringified before translation).

Lookups: LTYPE {0 TL, 1 OSL, 2 IRSL, 3 M-IR, 4 M-VIS, 5 TOL, 6 TRPOSL, 7 RIR, 8 RBR,
9 USER, 10 POSL, 11 SGOSL, 12 RL, 13 XRF}; DTYPE {0 Natural, 1 N+dose, 2 Bleach,
3 Bleach+dose, 4 Natural (Bleach), 5 N+dose (Bleach), 6 Dose, 7 Background};
LIGHTSOURCE {0 None, 1 Lamp, 2 IR diodes/IR Laser, 3 Calibration LED, 4 Blue Diodes,
5 White light, 6 Green laser (single grain), 7 IR laser (single grain)}.
Out-of-range → NA, never raise.

## Post-loop pipeline (order observable)
1 assemble (SEL/GRAIN/LPOWER/ENABLE_FLAGS aliases); 2 drop VERSION-NA rows; 3 verbose
count; 4 position filter (all-or-nothing: any invalid → warn
"At least one position number is not valid", change nothing); 5 zero_data_rm (default
True): drop empty-DATA rows, warn "Zero-data records detected and removed: ..."
(regardless of verbose); 6 empty → message "Empty object returned", empty object;
7 duplicate check only if >=2 rows and all RECTYPE != 128: adjacent-only comparison
(DATA[i-1]==DATA[i]); duplicated_rm True → remove + message; False → warn;
8 ID recalc to 1..nrow if needed; 9 translations unless show_raw_values: lookups; V3
fix (if FIRST row VERSION==3: rows with LTYPE OSL + LIGHTSOURCE "IR diodes/IR Laser" →
IRSL, applies file-wide); TIME: len 5 → prepend "0", len 6 → "%H%M%S"→"%H:%M:%S";
10 FNAME fallback: if all empty → basename without extension; 11 build object.

## Pre-scan (pass 1) corruption semantics
Unsupported version byte: if records already read → warn "BIN-file appears to be
corrupt, import limited to the first k records" (or "'n.records' reset to k"),
n_records := 1..k; if FIRST record → error "BIN/BINX format version (vv) is not
supported or file is broken. Supported version numbers are: 03, 04, 05, 06, 07, 08".
Record with unreadable/too-small LENGTH: message "Record #n skipped due to wrong
record length", continue (temp_ID NOT incremented). Seek past EOF allowed. 0 records →
warn "0 records read, NULL returned" → None.

## read_bin arguments to port
file (path/list), show_raw_values=False, position=None (post-filter), n_records=None
(1-based file record indices; 0 → empty), zero_data_rm=True, duplicated_rm=False,
fast_forward=False (→ list[Analysis] via bridge, always a list), force_version=None,
ignore_rectype=False (bool or number), verbose=True. Errors: nonexistent file,
extension not bin/binx, zero-byte file.

## Bridge
Risoe→Curve(id): recordType = f"{LTYPE} (PMT)" (space sep; NOT UVVIS — that's XSYG);
curve_type NA; originator ".Risoe.BINfileData2RLum.Data.Curve"; data = curve matrix;
info = ALL 80 metadata columns for that row.
Risoe→Analysis(pos, grain, run, set, ltype, dtype, protocol="unknown",
keep_empty=True): required cols ID/POSITION/GRAIN/RUN/SET/LTYPE/DTYPE; invalid pos/
grain → warn+intersect, invalid run/set/ltype/dtype → error listing valid. Grouping:
nested loops pos-major then grain; one Analysis per (pos, grain); selection: POSITION==
pos AND (GRAIN is NA OR GRAIN==grain) AND RUN/LTYPE/SET/DTYPE in filters; ordered by ID.
RECTYPE 128 rows → empty placeholder Curve. Children's pid = analysis uid. Single
(pos,grain) → bare Analysis, else flat list pos-major. fastForward wraps in list.

## Edge-case corpus (tests/testthat/_data/bin-tests/)
corrupted.bin: V3 ok + version byte 1 → 1 row + corrupt warning (n.records=2 → reset
warning). two-versions.binx: V3,V3,V8,V8 → 4 rows, V3 IRSL fix applies file-wide.
rectype-128.binx: 2×RECTYPE1 + 1×RECTYPE128 (100 ROIs) → 3 rows, duplicate check
skipped, fastForward → ROI becomes empty Curve, POSITION 0 vs 1 → separate Analysis.
duplicated-records.binx: record 2 == record 1 → warn (default) / remove (rm=True).
zero-data-record.binx: truncated → record 2 has 0 points → removed w/ warning.
zero-data-all.binx: NPOINTS=0 both → "Empty object returned".

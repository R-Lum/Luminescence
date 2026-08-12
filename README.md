# Luminescence (Python)

A Python port of the R package
[`Luminescence`](https://github.com/R-Lum/Luminescence) for luminescence dating data
analysis.

[![Python ≥ 3.12](https://img.shields.io/badge/python-%E2%89%A5%203.12-blue)](pyproject.toml)
[![License: GPL-3.0](https://img.shields.io/badge/license-GPL--3.0-green)](LICENSE)

This repository is being migrated from R to Python. The Python package grows under
[`src/luminescence/`](src/luminescence/) while the original R sources remain in place
([`R/`](R/), [`man/`](man/), [`tests/testthat/`](tests/testthat/)) as the reference
implementation and numerical oracle until the migration is complete.

## Status

The port is pre-alpha and proceeds in phases:

| Phase | Scope | Status |
|---|---|---|
| 0 | Scaffolding, tooling, CI, physical lookup tables | done |
| 1 | Core object model, BIN/BINX reader, SAR CW-OSL analysis chain | done |
| 2 | Remaining instrument readers (XSYG, SPE, PSL, Daybreak, TIFF, RF, Helios), writers | next |
| 3 | Equivalent-dose / age models, dosimetry, DRAC client | open |
| 4 | Remaining fitting routines and analysis protocols | open |
| 5 | Plotting layer (matplotlib) | open |
| 6 | Bayesian analyses (PyMC, optional extra) | open |
| 7 | Documentation and PyPI release | open |

Reading Risø BIN/BINX files (format versions 3 to 8) and the complete SAR CW-OSL
workflow already work end to end, including Lx/Tx tables, rejection criteria,
dose-response fitting (LIN/EXP/GOK), and equivalent-dose determination with
Monte-Carlo errors.

## Installation

Not yet on PyPI. Install from source (Python ≥ 3.12):

```bash
git clone https://github.com/Cologne-Geomorphological-Software-Lab/Luminescence.git
cd Luminescence
uv sync            # or: pip install -e .
```

## Quickstart

```python
import luminescence as lum

# read a Risø BIN/BINX file and group records per aliquot
data = lum.read_bin("measurement.binx")
aliquot = data.to_analysis(pos=1)

# SAR CW-OSL analysis: LxTx table, rejection criteria, De
results = lum.analyse_sar_cwosl(
    aliquot,
    signal_integral=range(1, 3),        # channels, 1-based inclusive (as in R)
    background_integral=range(900, 1001),
)
print(results["data"][["De", "De.Error", "D01", "RC.Status"]])
```

API naming follows the R package with snake_case, so R users can translate calls
one-to-one: `analyse_SAR.CWOSL()` becomes `analyse_sar_cwosl()`, `read_BIN2R()` becomes
`read_bin()`, `calc_Statistics()` becomes `calc_statistics()`.

## Numerical validation

Every ported function is verified against the R implementation:

- Reference fixtures in [`tests/python/fixtures/`](tests/python/fixtures/) are generated
  by [`tools/generate_fixtures.R`](tools/generate_fixtures.R) (provenance in
  `MANIFEST.json`). Deterministic results must match within documented tolerances
  (arithmetic 1e-9, fitted parameters 1e-4).
- The R test suite's snapshots (`tests/testthat/_snaps/`) serve as an additional
  cross-language oracle, parsed by [`tests/python/oracle.py`](tests/python/oracle.py).
- Monte-Carlo error estimates are compared statistically. All stochastic functions take
  an explicit `rng` argument; there is no global seeding.

Porting specifications extracted from the R sources live in [`tools/specs/`](tools/specs/).

## Development

```bash
uv sync                    # environment incl. dev dependencies
uv run pytest              # test suite
uv run ruff check .        # lint
uv run ruff format .       # format
uv run basedpyright        # type check (strict)
```

The CI workflow [`python-check.yml`](.github/workflows/python-check.yml) runs all of the
above on Linux, Windows, and macOS. It is not triggered by R-only changes, and the
original R workflows remain untouched.

## License

This is a derivative work of the R package Luminescence by the R-Luminescence Group and,
like the original, licensed under [GPL-3.0](LICENSE).

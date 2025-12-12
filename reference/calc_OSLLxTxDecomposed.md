# Calculate Lx/Tx ratio for decomposed CW-OSL signal components

Calculate `Lx/Tx` ratios from a given set of decomposed CW-OSL curves
decomposed by `OSLdecomposition::RLum.OSL_decomposition`.

## Usage

``` r
calc_OSLLxTxDecomposed(
  Lx.data,
  Tx.data = NULL,
  OSL.component = 1L,
  sig0 = 0,
  digits = NULL
)
```

## Arguments

- Lx.data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (**required**):
  Component table created by `OSLdecomposition::RLum.OSL_decomposition`
  and per default located at `object@records[[...]]@info$COMPONENTS`.The
  value of `$n[OSL.component]` is set as `LnLx`. The value of
  `$n.error[OSL.component]` is set as `LnLx.error`

- Tx.data:

  [data.frame](https://rdrr.io/r/base/data.frame.html) (*optional*):
  Component table created by `OSLdecomposition::RLum.OSL_decomposition`
  and per default located at `object@records[[...]]@info$COMPONENTS`.
  The value of `$n[OSL.component]` is set as `TnTx`. The value of
  `$n.error[OSL.component]` is set as `TnTx.error`

- OSL.component:

  [integer](https://rdrr.io/r/base/integer.html) or
  [character](https://rdrr.io/r/base/character.html) (*optional*): a
  single index or a name describing which OSL signal component shall be
  evaluated. This argument can either be the name of the OSL component
  assigned by `OSLdecomposition::RLum.OSL_global_fitting` or the index
  of component. Then `'1'` selects the fastest decaying component, `'2'`
  the second fastest and so on. If not defined, the fastest decaying
  component is selected.

- sig0:

  [numeric](https://rdrr.io/r/base/numeric.html) (*with default*):
  allows adding an extra error component to the final `Lx/Tx` error
  value (e.g., instrumental error).

- digits:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*): round
  numbers to the specified digits. If digits is set to `NULL` nothing is
  rounded.

## Value

Returns an S4 object of type
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md).

Slot `data` contains a [list](https://rdrr.io/r/base/list.html) with the
following structure:

**@data**

    $LxTx.table (data.frame)
    .. $ LnLx
    .. $ TnTx
    .. $ Net_LnLx
    .. $ Net_LnLx.Error
    .. $ Net_TnTx
    .. $ Net_TnTx.Error
    .. $ LxTx
    .. $ LxTx.relError
    .. $ LxTx.Error

## Function version

0.1.0

## How to cite

Mittelstrass, D., 2025. calc_OSLLxTxDecomposed(): Calculate Lx/Tx ratio
for decomposed CW-OSL signal components. Function version 0.1.0. In:
Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer,
M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M.,
Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L.,
Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data
Analysis. R package version 1.1.2. https://r-lum.github.io/Luminescence/

## References

Mittelstrass D., Schmidt C., Beyer J., Straessner A., 2019. Automated
identification and separation of quartz CW-OSL signal components with R.
talk presented at DLED 2019, Bingen, Germany
<http://luminescence.de/OSLdecomp_talk.pdf>  

## See also

[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md),
[fit_DoseResponseCurve](https://r-lum.github.io/Luminescence/reference/fit_DoseResponseCurve.md),
[analyse_SAR.CWOSL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.CWOSL.md)

## Author

Dirk Mittelstrass , RLum Developer Team

# Calculate the Lx/Tx ratio for a given set of TL curves -beta version-

Calculate Lx/Tx ratio for a given set of TL curves.

## Usage

``` r
calc_TLLxTxRatio(
  Lx.data.signal,
  Lx.data.background = NULL,
  Tx.data.signal = NULL,
  Tx.data.background = NULL,
  signal.integral.min = NULL,
  signal.integral.max = NULL
)
```

## Arguments

- Lx.data.signal:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html)
  (**required**): TL data (x = temperature, y = counts) (TL signal).

- Lx.data.background:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html) (*optional*):
  TL data (x = temperature, y = counts). If no data are provided no
  background subtraction is performed.

- Tx.data.signal:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html)
  (**required**): TL data (x = temperature, y = counts) (TL test
  signal).

- Tx.data.background:

  [RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)
  or [data.frame](https://rdrr.io/r/base/data.frame.html) (*optional*):
  TL data (x = temperature, y = counts). If no data are provided no
  background subtraction is performed.

- signal.integral.min:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): channel
  number for the lower signal integral bound (e.g.
  `signal.integral.min = 100`).

- signal.integral.max:

  [integer](https://rdrr.io/r/base/integer.html) (**required**): channel
  number for the upper signal integral bound (e.g.
  `signal.integral.max = 200`).

## Value

Returns an S4 object of type
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md).
Slot `data` contains a [list](https://rdrr.io/r/base/list.html) with the
following structure:

    $ LxTx.table
    .. $ LnLx
    .. $ LnLx.BG
    .. $ TnTx
    .. $ TnTx.BG
    .. $ Net_LnLx
    .. $ Net_LnLx.Error

## Details

**Uncertainty estimation**

The standard errors are calculated using the following generalised
equation:

\$\$SE\_{signal} = abs(Signal\_{net} \* BG_f /BG\_{signal})\$\$

where \\BG_f\\ is a term estimated by calculating the standard deviation
of the sum of the \\L_x\\ background counts and the sum of the \\T_x\\
background counts. However, if both signals are similar the error
becomes zero.

## Note

**This function has still BETA status!** Please further note that a
similar background for both curves results in a zero error and is
therefore set to `NA`.

## Function version

0.3.4

## See also

[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md),
[analyse_SAR.TL](https://r-lum.github.io/Luminescence/reference/analyse_SAR.TL.md)

## Author

Sebastian Kreutzer, Institute of Geography, Heidelberg University
(Germany)  
Christoph Schmidt, University of Bayreuth (Germany) , RLum Developer
Team

## How to cite

Kreutzer, S., Schmidt, C., 2025. calc_TLLxTxRatio(): Calculate the Lx/Tx
ratio for a given set of TL curves -beta version-. Function version
0.3.4. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt,
C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

## Examples

``` r
##load package example data
data(ExampleData.BINfileData, envir = environment())

##convert Risoe.BINfileData into a curve object
temp <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)

Lx.data.signal <- get_RLum(temp, record.id=1)
Lx.data.background <- get_RLum(temp, record.id=2)
Tx.data.signal <- get_RLum(temp, record.id=3)
Tx.data.background <- get_RLum(temp, record.id=4)
signal.integral.min <- 210
signal.integral.max <- 230

output <- calc_TLLxTxRatio(
 Lx.data.signal,
 Lx.data.background,
 Tx.data.signal,
 Tx.data.background,
 signal.integral.min,
 signal.integral.max)
get_RLum(output)
#>     LnLx LnLx.BG  TnTx TnTx.BG net_LnLx net_LnLx.Error net_TnTx net_TnTx.Error
#> 1 257042    4068 82298    2943   252974       49468.92    79355       21449.72
#>       LxTx LxTx.Error
#> 1 3.187877   1.485073
```

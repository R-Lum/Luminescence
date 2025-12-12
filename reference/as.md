# as() - RLum-object coercion

for `[RLum.Analysis-class]`

for `[RLum.Data.Curve-class]`

for `[RLum.Data.Image-class]`

for `[RLum.Data.Spectrum-class]`

for `[RLum.Results-class]`

## Arguments

- from:

  [RLum](https://r-lum.github.io/Luminescence/reference/RLum-class.md),
  [list](https://rdrr.io/r/base/list.html),
  [data.frame](https://rdrr.io/r/base/data.frame.html),
  [matrix](https://rdrr.io/r/base/matrix.html) (**required**): object to
  be coerced from

- to:

  [character](https://rdrr.io/r/base/character.html) (**required**):
  class name to be coerced to

## Details

**[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)**

|          |        |
|----------|--------|
| **from** | **to** |
| `list`   | `list` |

Given that the [list](https://rdrr.io/r/base/list.html) consists of
[RLum.Analysis](https://r-lum.github.io/Luminescence/reference/RLum.Analysis-class.md)
objects.

**[RLum.Data.Curve](https://r-lum.github.io/Luminescence/reference/RLum.Data.Curve-class.md)**

|              |              |
|--------------|--------------|
| **from**     | **to**       |
| `list`       | `list`       |
| `data.frame` | `data.frame` |
| `matrix`     | `matrix`     |

**[RLum.Data.Image](https://r-lum.github.io/Luminescence/reference/RLum.Data.Image-class.md)**

|              |              |
|--------------|--------------|
| **from**     | **to**       |
| `data.frame` | `data.frame` |
| `matrix`     | `matrix`     |

**[RLum.Data.Spectrum](https://r-lum.github.io/Luminescence/reference/RLum.Data.Spectrum-class.md)**

|              |              |
|--------------|--------------|
| **from**     | **to**       |
| `data.frame` | `data.frame` |
| `matrix`     | `matrix`     |
| `list`       | `list`       |

**[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)**

|          |        |
|----------|--------|
| **from** | **to** |
| `list`   | `list` |

Given that the [list](https://rdrr.io/r/base/list.html) consists of
[RLum.Results](https://r-lum.github.io/Luminescence/reference/RLum.Results-class.md)
objects.

## Note

Due to the complex structure of the `RLum` objects itself a coercing to
standard R data structures will be always loosely!

## See also

[methods::as](https://rdrr.io/r/methods/as.html)

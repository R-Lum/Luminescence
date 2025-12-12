# GitHub API - Deprecated

R Interface to the GitHub API v3.

## Usage

``` r
github_commits(user = "r-lum", repo = "luminescence", branch = "master", n = 5)

github_branches(user = "r-lum", repo = "luminescence")

github_issues(user = "r-lum", repo = "luminescence", verbose = TRUE)
```

## Arguments

- user:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  GitHub user name (defaults to `'r-lum'`).

- repo:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  name of a GitHub repository (defaults to `'luminescence'`).

- branch:

  [character](https://rdrr.io/r/base/character.html) (*with default*):
  branch of a GitHub repository (defaults to `'master'`).

- n:

  [integer](https://rdrr.io/r/base/integer.html) (*with default*):
  number of commits returned (defaults to 5).

- verbose:

  [logical](https://rdrr.io/r/base/logical.html) (*with default*):
  enable/disable output to the terminal.

## Value

`github_commits`: [data.frame](https://rdrr.io/r/base/data.frame.html)
with columns:

|         |         |
|---------|---------|
| `[ ,1]` | SHA     |
| `[ ,2]` | AUTHOR  |
| `[ ,3]` | DATE    |
| `[ ,4]` | MESSAGE |

`github_branches`: [data.frame](https://rdrr.io/r/base/data.frame.html)
with columns:

|         |         |
|---------|---------|
| `[ ,1]` | BRANCH  |
| `[ ,2]` | SHA     |
| `[ ,3]` | INSTALL |

`github_commits`: Nested [list](https://rdrr.io/r/base/list.html) with
`n` elements. Each commit element is a list with elements:

|         |         |
|---------|---------|
| `[[1]]` | NUMBER  |
| `[[2]]` | TITLE   |
| `[[3]]` | BODY    |
| `[[4]]` | CREATED |
| `[[5]]` | UPDATED |
| `[[6]]` | CREATOR |
| `[[7]]` | URL     |
| `[[8]]` | STATUS  |

## Details

These functions can be used to query a specific repository hosted on
GitHub.  

`github_commits` lists the most recent `n` commits of a specific branch
of a repository.

`github_branches` can be used to list all current branches of a
repository and returns the corresponding SHA hash as well as an
installation command to install the branch in R via the 'devtools'
package.

`github_issues` lists all open issues for a repository in valid YAML.

## Function version

0.1.0

## How to cite

Burow, C., 2025. GitHub-API(): GitHub API - Deprecated. Function version
0.1.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt,
C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S.,
Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M.,
Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive
Luminescence Dating Data Analysis. R package version 1.1.2.
https://r-lum.github.io/Luminescence/

## References

GitHub Developer API v3. <https://docs.github.com/v3/>, last accessed:
10/01/2017.

## Author

Christoph Burow, University of Cologne (Germany) , RLum Developer Team

## Examples

``` r
if (FALSE) { # \dontrun{
github_branches(user = "r-lum", repo = "luminescence")
github_issues(user = "r-lum", repo = "luminescence")
github_commits(user = "r-lum", repo = "luminescence", branch = "master", n = 10)
} # }
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyklips

<!-- badges: start -->
<!-- badges: end -->

tidyklips is an R package that allows users to interface with the Korea
Labor Institute’s yearly KLIPS(Korea Labor & Income Panel Study) and
return tidyverse-ready data frames, optionally with time-series features
included.

## Installation

You can install the development version of tidyklips from
[GitHub](https://github.com/cheongchoi/tidyklips) with:

``` r
# install from CRAN
install.packages("tidyklips")
#> Installing package into 'C:/Users/choi/AppData/Local/Temp/RtmpKCpcNg/temp_libpath2bc854e73edb'
#> (as 'lib' is unspecified)
#> Warning: package 'tidyklips' is not available for this version of R
#> 
#> A version of this package for your version of R might be available elsewhere,
#> see the ideas at
#> https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

# install dev version
devtools::install_github("cheongchoi/tidyklips")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo cheongchoi/tidyklips@HEAD
#> 
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\choi\AppData\Local\Temp\RtmpEvWqBU\remotes7064321c45ab\cheongchoi-tidyklips-2b0647c/DESCRIPTION' ...  ✔  checking for file 'C:\Users\choi\AppData\Local\Temp\RtmpEvWqBU\remotes7064321c45ab\cheongchoi-tidyklips-2b0647c/DESCRIPTION' (433ms)
#>       ─  preparing 'tidyklips':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts (492ms)
#>       ─  checking for empty or unneeded directories
#>       ─  building 'tidyklips_0.0.0.9000.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/choi/AppData/Local/Temp/RtmpKCpcNg/temp_libpath2bc854e73edb'
#> (as 'lib' is unspecified)
```

## Example

This is a basic example which shows you how to obain a dataframe file
from KLIPS:

``` r
# first, you need to download the .zip file from KLIPS homepage <https://www.kli.re.kr/klips>.
# second, you need to unzip that file.
# third, you need to remember the folder path.

library(tidyklips)
library(dplyr)
library(magrittr)
path <- system.file("extdata", package = "tidyklips")
df <- getpklips(path = mypath, year = 2023, datatype = "stata") 

# you need to adjust the path. 
# This code is equivalent to the following code

df <- klips26p
df2 <- df 
    dplyr::group_by(gender) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(proportion = count / sum(count))
```

``` r
summary(df)
```

``` r
summary(df2)
```

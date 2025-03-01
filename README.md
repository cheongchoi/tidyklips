
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

# install dev version
devtools::install_github("cheongchoi/tidyklips")
```

## Example

This is a basic example which shows you how to obain a dataframe file
from KLIPS:

- tidyklips::getpklips(): retrieve the KLIPS household member survey

``` r
# first, you need to download the .zip file from KLIPS homepage <https://www.kli.re.kr/klips>.
# second, you need to unzip that file.
# third, you need to remember the folder path.

library(tidyklips)
library(dplyr)
library(magrittr)
path <- system.file("extdata", package = "tidyklips")
df <- getpklips(path = path, year = 2023, datatype = "stata") 

# you need to adjust the path. 
# This code is equivalent to the following code

df <- klips26p
df2 <- df 
    dplyr::group_by(gender) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(proportion = count / sum(count))

summary(df)

summary(df2)

barplot(df2$proportion~as.factor(df2$gender))
```

- tidyklips::gethklips() : retrieve the KLIPS heald of household survey

``` r
# first, you need to download the .zip file from KLIPS homepage <https://www.kli.re.kr/klips>.
# second, you need to unzip that file.
# third, you need to remember the folder path.

library(tidyklips)
library(dplyr)
library(magrittr)
path <- system.file("extdata", package = "tidyklips")
df <- gethklips(path = path, year = 2022:2023, datatype = "stata") 

# you need to adjust the path. 
# This code is equivalent to the following code

df2 <- df 
    dplyr::group_by(year) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(proportion = count / sum(count))

summary(df)

summary(df2)
```

- tidyklips::getaklips() : retrieve the KLIPS additional survey

``` r
# first, you need to download the .zip file from KLIPS homepage <https://www.kli.re.kr/klips>.
# second, you need to unzip that file.
# third, you need to remember the folder path.

library(tidyklips)
library(dplyr)
library(magrittr)

path <- system.file("extdata", package = "tidyklips")
df <- gethklips(path = path, year = 2022:2023, datatype = "stata") 

df2 <- df 
    dplyr::group_by(year, activity) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(proportion = count / sum(count))

summary(df)

summary(df2)
```

- tidyklips::getwklips() : retrieve the KLIPS career data

``` r
# first, you need to download the .zip file from KLIPS homepage <https://www.kli.re.kr/klips>.
# second, you need to unzip that file.
# third, you need to remember the folder path.

library(tidyklips)
library(dplyr)
library(magrittr)

path <- system.file("extdata", package = "tidyklips")
df <- gethklips(path = path, datatype = "stata") 

df2 <- df 
    dplyr::group_by(jobseq) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(proportion = count / sum(count))

summary(df)

summary(df2)
```

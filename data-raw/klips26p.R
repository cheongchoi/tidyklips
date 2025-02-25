library(dplyr)
library(readr)
library(haven)

mypath <- system.file("data-raw", package = "tidyklips")
klips26p <- read_dta(paste0(mypath,"/klips26p.dta"), encoding='utf-8', col_select=c("pid", "p260101", "p260107"))

klips26p <- klips26p  %>%
  dplyr::rename(gender = p260101,
                age = p260107) %>%
  dplyr::select(c(pid, gender, age)) %>%
  dplyr::mutate(year = 2023)


save(klips26p, file = "data/klips26p.rda")

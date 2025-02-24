library(dplyr)
library(readr)

klips26p <- read_dta(file = "klips26p.dta", encoding='utf-8', col_select=c("pid", "p260101", "p260107"))

save(klips26p, file = "../data/klips26p.rda")

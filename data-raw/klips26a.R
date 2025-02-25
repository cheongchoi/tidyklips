library(dplyr)
library(haven)

df <- read_dta("klips26a.dta", encoding='utf-8')
colnames(df)
clnm<- colnames(df)[c(1:10)]
clnm

df2<- df  %>%
  select(clnm)
df2<- df2[1:1000,]


write_dta(df2, "../inst/extdata/klips26a.dta")

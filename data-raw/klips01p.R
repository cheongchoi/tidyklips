library(dplyr)
library(haven)

mypath <- system.file("extdata", package = "tidyklips")
df <- read_dta(paste0(mypath,"/klips01p.dta"), encoding='utf-8')
colnames(df)
clnm<- colnames(df)[c(1, 7:13)]
clnm

df2<- df  %>%
  select(clnm)
df2<- df2[1:1000,]


write_dta(df2, "inst/extdata/klips01p.dta")

mypath <- system.file("extdata", package = "tidyklips")
df <- read_dta(paste0(mypath,"/klips01p.dta"), encoding='utf-8')
colnames(df)
clnm<- colnames(df)[c(1, 7:13)]
clnm

df2<- df  %>%
  select(clnm)
df2<- df2[1:1000,]


write_dta(df2, "inst/extdata/klips01p.dta")


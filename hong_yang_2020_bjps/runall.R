rm(list = ls())
# invisibly load packages together 
invisible(lapply(c("haven", # import data
                   "plyr", # data manipulation
                   "tidyverse", # data manipulation
                   "magrittr", # data manipulation 
                   "broom", # model statistics
                   "MASS", # negative binomial reg
                   "plm", # panel reg
                   "sandwich", # clustered standard errors
                   "lmtest", # hypothesis test according to adjusted standard errors
                   "xtable", # export table
                   "texreg"), # export reg table
                 library, character.only = T, warn.conflict = F)
)
setwd("~/Box/myBox/Github/replication/hong_yang_2020_bjps/")

source("r/analysis.R")
source("r/report.R")
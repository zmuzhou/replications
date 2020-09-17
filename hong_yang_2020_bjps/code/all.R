rm(list = ls())
pkgs <- c(
  "haven", "plyr", "tidyverse", "magrittr", "broom",
  "MASS", "plm", "sandwich", "lmtest",
  "xtable", "texreg"
)
invisible(lapply(pkgs, library, character.only = T))
setwd("~/Box/myBox/GitHub/replications/hong_yang_2020_bjps/")

source("code/analysis.R")
source("code/tabs.R")

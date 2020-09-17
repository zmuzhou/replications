rm(list = ls())
pkgs <- c(
  "haven", "tidyverse", "magrittr",
  "estimatr", "MASS", "brant", "survey",
  "sjPlot", "cowplot",
  "texreg", "xtable"
)
invisible(sapply(pkgs, library, character.only = T))
setwd("~/Box/myBox/GitHub/replications/rogowski_tucker_2019_psrm/")

source("code/analysis.R")
source("code/tabs_figs.R")

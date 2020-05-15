invisible(sapply(c("haven", "tidyverse", "magrittr",
                   "estimatr", "MASS", "brant", "survey",
                   "sjPlot", "cowplot",
                   "texreg", "xtable"),
                 library, character.only = T))
setwd("~/Box/myBox/GitHub/replication/rogowski_tucker_2019_psrm/")

source("r/sandyhook.R")
source("r/sandyhook_report.R")
source("r/orlando.R")
source("r/orlando_report.R")

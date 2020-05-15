# settings ----
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
setwd("~/Box/repository/replication/hong_yang_2020_bjps/")

# load and process data ----
download.file("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/SIREAP/HLWLV6",
              "data.dta") # download data for replication from Harvard Dataverse
dat <- read_dta("data.dta") %>% # import dta file 
  zap_label() %>% # delete labels
  dplyr::rename(year = Year, id = ID, p_uighur = p_Uighur)
dat_main <- dat %>% group_by(id) %>% # group obs by county id for lag operation
  mutate_at(c("logoilsales", "loggassales", "logoilgassales", "logmosdens",
              "p_uighur", "density2", "logrevenue", "loggrant", "logpgdp", "bingtuan"), 
            list(dplyr::lag)) %>% # lag all time-varying vars.
  ungroup() %>%
  # select vars for analysis
  dplyr::select(year, id, # id
                conflict, # outcome
                logoilsales, loggassales, logoilgassales, logmosdens, # explanatory
                p_uighur, density2, logrevenue, loggrant, logpgdp, bingtuan, distance, slope) # controls
dat_mechanism <- dat %>%
  mutate_at(c("p_uighur", "density2", "bingtuan"),
            list(dplyr::lag)) %>%
  dplyr::select(year, id,
                employ_rate, # outcome
                logoilsales, loggassales, logoilgassales,
                p_uighur, density2, bingtuan)
rm(dat)

# summary statistics ----
obs <- NULL
for (i in 1:13) { 
  obs[i] <- dat_main[, i + 2] %>%
    is.na() %>% {nrow(dat_main) - sum(.)}
  rm(i)
}
sumstats <- dat_main %>%
  dplyr::select(3:15) %>%
  sapply(each(min, median, max, mean, sd), na.rm = T) %>% t() %>%
  cbind(matrix(obs, nrow = 13, ncol = 1))
rm(obs)

# column 7, 8, 9 in tab 1 in HY ----
# model specification
eqs_tab1 <- NULL
for (i in 1:3) {
  eqs_tab1[[i]] <- reformulate(c(names(dat_main[i + 3]), names(dat_main[8:15]), "as.factor(year)"), "conflict")
  rm(i)
}
# estimation
fits_tab1 <- map(eqs_tab1, glm.nb, data = dat_main)
# model statistics
modstats_tab1 <- map(fits_tab1, broom::glance)
# clustered standard errors
for (i in 1:3) {
  fits_tab1[[i]] <- coeftest(fits_tab1[[i]], vcov. = vcovCL(fits_tab1[[i]], cluster = dat_main$id))
}

# column 7, 8, 9 in tab 3 in HY ----
# model specification
eqs_tab3 <- NULL
for (i in 1:3) {
  eqs_tab3[[i]] <- reformulate(c(names(dat_main[i + 3]), "logmosdens", paste0(names(dat_main[i + 3]), "*", "logmosdens"),
                            names(dat_main[8:15]), "as.factor(year)"), 
                          "conflict")
  rm(i)
}
# estimation
fits_tab3 <- map(eqs_tab3, glm.nb, data = dat_main)
# model statistics
modstats_tab3 <- map(fits_tab3, broom::glance)
# clustered standard errors
for (i in 1:3) {
  fits_tab3[[i]] <- coeftest(fits_tab3[[i]], vcov. = vcovCL(fits_tab3[[i]], cluster = dat_main$id))
}

# column 4, 5, 6 in tab 7 in HY ----
# model specification
eqs_tab7 <- NULL
for (i in 1:3) {
  eqs_tab7[[i]] <- reformulate(c(names(dat_mechanism[i + 3]), names(dat_mechanism[7:9])), "employ_rate")
  rm(i)
}
# estimation
fits_tab7 <- map(eqs_tab7, plm, data = dat_mechanism, index = c("id", "year"), model = "within", effect = "twoways")
# model statistics
modstats_tab7 <- map(fits_tab7, broom::glance)
obs <- NULL
for (i in 1:3) {
  obs[i] <- length(fits_tab7[[i]]$residual)
  rm(i)
}
# clustered standard errors
for (i in 1:3) {
  fits_tab7[[i]] <- coeftest(fits_tab7[[i]], vcov. = vcovHC.plm(fits_tab7[[i]], method = "arellano", type = "sss", cluster = "group"))
  rm(i)
}
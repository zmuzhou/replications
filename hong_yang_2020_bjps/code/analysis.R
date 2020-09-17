# load and process data ----
download.file(
  "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/SIREAP/HLWLV6",
  "dat/hong_yang_2020_bjps.dta"
)
dat <- read_dta("dat/hong_yang_2020_bjps.dta") %>%
  zap_label() %>% # delete labels
  dplyr::rename(year = Year, id = ID, p_uighur = p_Uighur)
dat_main <- dat %>%
  group_by(id) %>%
  mutate_at(
    c(
      "logoilsales", "loggassales", "logoilgassales", "logmosdens",
      "p_uighur", "density2", "logrevenue", "loggrant", "logpgdp", "bingtuan"
    ),
    list(dplyr::lag)
  ) %>%
  ungroup() %>%
  dplyr::select(
    year, id,
    conflict,
    logoilsales, loggassales, logoilgassales, logmosdens,
    p_uighur, density2, logrevenue, loggrant, logpgdp, bingtuan, distance, slope
  )
dat_mechanism <- dat %>%
  mutate_at(
    c("p_uighur", "density2", "bingtuan"),
    list(dplyr::lag)
  ) %>%
  dplyr::select(
    year, id,
    employ_rate,
    logoilsales, loggassales, logoilgassales,
    p_uighur, density2, bingtuan
  )
rm(dat)

# summary statistics ----
obs <- NULL
for (i in 1:13) {
  obs[i] <- dat_main[, i + 2] %>%
    is.na() %>%
    {
      nrow(dat_main) - sum(.)
    }
  rm(i)
}
sumstats <- dat_main %>%
  dplyr::select(3:15) %>%
  sapply(each(min, median, max, mean, sd), na.rm = T) %>%
  t() %>%
  cbind(matrix(obs, nrow = 13, ncol = 1))
rm(obs)

# column 7, 8, 9 in tab 1 ----
eqs_tab1 <- NULL
for (i in 1:3) {
  eqs_tab1[[i]] <- reformulate(c(names(dat_main[i + 3]), names(dat_main[8:15]), "as.factor(year)"), "conflict")
  rm(i)
}
fits_tab1 <- map(eqs_tab1, glm.nb, data = dat_main)
modstats_tab1 <- map(fits_tab1, broom::glance)
for (i in 1:3) {
  fits_tab1[[i]] <- coeftest(fits_tab1[[i]], vcov. = vcovCL(fits_tab1[[i]], cluster = dat_main$id))
}

# column 7, 8, 9 in tab 3 ----
eqs_tab3 <- NULL
for (i in 1:3) {
  eqs_tab3[[i]] <- reformulate(
    c(
      names(dat_main[i + 3]), "logmosdens", paste0(names(dat_main[i + 3]), "*", "logmosdens"),
      names(dat_main[8:15]), "as.factor(year)"
    ),
    "conflict"
  )
  rm(i)
}
fits_tab3 <- map(eqs_tab3, glm.nb, data = dat_main)
modstats_tab3 <- map(fits_tab3, broom::glance)
for (i in 1:3) {
  fits_tab3[[i]] <- coeftest(fits_tab3[[i]], vcov. = vcovCL(fits_tab3[[i]], cluster = dat_main$id))
}

# column 4, 5, 6 in tab 7 ----
eqs_tab7 <- NULL
for (i in 1:3) {
  eqs_tab7[[i]] <- reformulate(c(names(dat_mechanism[i + 3]), names(dat_mechanism[7:9])), "employ_rate")
  rm(i)
}
fits_tab7 <- map(eqs_tab7, plm, data = dat_mechanism, index = c("id", "year"), model = "within", effect = "twoways")
modstats_tab7 <- map(fits_tab7, broom::glance)
obs <- NULL
for (i in 1:3) {
  obs[i] <- length(fits_tab7[[i]]$residual)
  rm(i)
}
for (i in 1:3) {
  fits_tab7[[i]] <- coeftest(fits_tab7[[i]], vcov. = vcovHC.plm(fits_tab7[[i]], method = "arellano", type = "sss", cluster = "group"))
  rm(i)
}

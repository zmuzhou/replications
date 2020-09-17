# variable names ----
varnames <- c(
  "Ethnic Violence",
  "Employment Rate",
  "Oil Sales, log",
  "Gas Sales, log",
  "Oil and Gas Sales, log",
  "Mosque Density, log",
  "Oil Sales \\(\\times\\) Mosque Density",
  "Gas Sales \\(\\times\\) Mosque Density",
  "Oil and Gas Sales \\(\\times\\) Mosque Density",
  "Percentage of Uighur Minority",
  "Population Density",
  "Fiscal Revenue, log",
  "Central Fiscal Transfer, log",
  "GDP per capita, log",
  "\\textit{Bingtuan} Population",
  "Distance to Border",
  "Terrain Slope"
)

# summary statistics table ----
sumstats %>%
  set_colnames(c("Min", "Median", "Max", "Mean", "SD", "\\(N\\)")) %>%
  set_rownames(varnames[1:13]) %>%
  xtable(
    digits = c(0, rep(2, 5), 0), align = c("l", rep("r", 6)),
    caption = "Summary Statistics of Variables for the Main Results",
    label = "sumstats"
  ) %>%
  print.xtable(
    sanitize.text.function = function(x) {
      x
    },
    math.style.negative = T,
    caption.placement = "top", booktabs = T, floating = T, table.placement = "htbp!",
    file = "tabs/sumstats.tex"
  )

# column 7, 8, 9 in tab 1 ----
fits_tab1 %<>% map(texreg::extract)
for (i in 1:3) {
  fits_tab1[[i]]@gof <- c(modstats_tab1[[i]]$BIC, modstats_tab1[[i]]$df.null + 1)
  fits_tab1[[i]]@gof.names <- c("BIC", "\\(N\\)")
  fits_tab1[[i]]@gof.decimal <- c(T, F)
  rm(i)
}
texreg(
  fits_tab1,
  file = "tabs/tab1.tex", label = "tab1",
  omit.coef = "(year)|(Intercept)",
  custom.coef.names = varnames[c(3, 10:17, 4:5)],
  reorder.coef = c(1, 10:11, 2:9),
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("(1)", "(2)", "(3)"),
  custom.note = "\n\\item Note: Year fixed-effects included. County-clustered standard errors in parentheses. Time-varying variables are one-year lagged. %stars.\n",
  caption = "Natural Resources and Ethnic Violence in Xinjiang, Negative Binomial Regression Estimation Results",
  caption.above = T, dcolumn = T, booktabs = T, threeparttable = T, use.packages = F
)

# column 7, 8, 9 in tab 3 ----
fits_tab3 %<>% map(texreg::extract)
for (i in 1:3) {
  fits_tab3[[i]]@gof <- c(modstats_tab3[[i]]$BIC, modstats_tab3[[i]]$df.null + 1)
  fits_tab3[[i]]@gof.names <- c("BIC", "\\(N\\)")
  fits_tab3[[i]]@gof.decimal <- c(T, F)
  rm(i)
}
texreg(
  fits_tab3,
  file = "tabs/tab3.tex", label = "tab3",
  omit.coef = "(year)|(Intercept)",
  custom.coef.names = varnames[c(3, 6, 10:17, 7, 4, 8, 5, 9)],
  reorder.coef = c(1, 12, 14, 2, 11, 13, 15, 3:10),
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("(1)", "(2)", "(3)"),
  custom.note = "\n\\item Note: Year fixed-effects included. County-clustered standard errors in parentheses. Time-varying variables are one-year lagged. %stars.\n",
  caption = "Natural Resources and Ethnic Violence in Xinjiang, Negative Binomial Regression with Interaction Term Estimation Results",
  caption.above = T, dcolumn = T, booktabs = T, threeparttable = T, use.packages = F
)

# column 4, 5, 6 in tab 7 ----
fits_tab7 %<>% map(texreg::extract)
for (i in 1:3) {
  fits_tab7[[i]]@gof <- c(modstats_tab7[[i]]$r.squared, obs[i])
  fits_tab7[[i]]@gof.names <- c("R-squared", "\\(N\\)")
  fits_tab7[[i]]@gof.decimal <- c(T, F)
  rm(i)
}
texreg(
  fits_tab7,
  file = "tabs/tab7.tex", label = "tab7",
  omit.coef = "(year)|(Intercept)",
  custom.coef.names = varnames[c(3, 10, 11, 15, 4, 5)],
  reorder.coef = c(1, 5, 6, 2:4),
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("(1)", "(2)", "(3)"),
  custom.note = "\n\\item Note: Two-way fixed-effects included. County-clustered standard errors in parentheses. Control Variables are one-year lagged. %stars.\n",
  caption = "Natural Resources and Employment in Xinjiang",
  caption.above = T, dcolumn = T, booktabs = T, threeparttable = T, use.packages = F
)

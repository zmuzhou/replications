# sample size ----
obs_cs <- rep(0, 15)
for (i in 1:15) {
  obs_cs[i] <- dat_cs %>% filter_(group[i]) %>%
    dplyr::select(postevent) %>% nrow
  rm(i)
}

# group name ----
name <- c(
  "Full Sample",
  "Democrats",
  "Republicans",
  "Independents",
  "Liberal",
  "Moderate",
  "Conservative",
  "Female",
  "Male",
  "Parents",
  "Non-parents",
  "NRA Membership: Yes",
  "NRA Membership: No",
  "Proximity: Yes",
  "Proximity: No"
)

# atab14 ----
atab14 <- function(input, row, caption, label, file, foonote) {
  input %>% map(broom::tidy) %>% map(dplyr::select, estimate, std.error, statistic, p.value, df) %>%
    map(mutate, obs = df + row) %>% map(dplyr::select,-df) %>% map(slice, row) %>% unlist %>%
    matrix(
      nrow = 15,
      ncol = 5,
      byrow = T,
      dimnames = list(
        name,
        c(
          "Estimate",
          "Standard Error",
          "\\(t\\)-statistic",
          "\\(p\\)-value",
          "\\(N\\)"
        )
      )
    ) %>%
    xtable(
      caption = caption,
      label = label,
      align = "lrcrrr",
      digits = c(0, rep(3, 4), 0)
    ) %>%
    print.xtable(
      type = "latex",
      file = file,
      table.placement = "!htbp",
      add.to.row = list(pos = list(15),
                        command = foonote),
      hline.after = c(-1, 0),
      sanitize.text.function = function(x) {
        x
      },
      math.style.negative = T,
      booktabs = T,
      caption.placement = "top"
    )
}
atab14(
  atab1,
  2,
  paste(
    "Effect of the Sandy Hook Shooting on Gun Control Support (Binary),",
    "Cross-sectional Analysis"
  ),
  "atab1",
  "tabs/atab1.tex",
  paste(
    "\\bottomrule\n",
    "\\multicolumn{5}{l}",
    "{\\footnotesize It replicates the numerical results underlying the left panel of Figure 2 in RT.}\\\\\n",
    "\\multicolumn{5}{l}",
    "{\\footnotesize Standard errors are heteroskedasticity-robust.}"
  )
)
atab14(
  atab2,
  1,
  paste(
    "Effect of the Sandy Hook Shooting on Gun Control Support (Binary),",
    "Panel (First-difference) Analysis"
  ),
  "atab2",
  "tabs/atab2.tex",
  paste(
    "\\bottomrule\n",
    "\\multicolumn{5}{l}\n",
    "{\\footnotesize It replicates the numerical results underlying the right panel of Figure 2 in RT.}\\\\\n",
    "\\multicolumn{5}{l}",
    "{\\footnotesize Standard errors are heteroskedasticity-robust.}"
  )
)
atab14(
  atab3,
  2,
  paste(
    "Effect of the Sandy Hook Shooting on Gun Control Support (5-points Likert Scale),",
    "Cross-sectional Analysis"
  ),
  "atab3",
  "tabs/atab3.tex",
  paste(
    "\\bottomrule\n",
    "\\multicolumn{5}{l}\n",
    "{\\footnotesize It replicates the numerical results underlying the left panel of Figure A.1 in RT.}\\\\\n",
    "\\multicolumn{5}{l}",
    "{\\footnotesize Standard errors are heteroskedasticity-robust.}"
  )
)

atab14(
  atab4,
  1,
  paste(
    "Effect of the Sandy Hook Shooting on Gun Control Support (5-points Likert Scale),",
    "Panel (First-difference) Analysis"
  ),
  "atab4",
  "tabs/atab4.tex",
  paste(
    "\\bottomrule\n",
    "\\multicolumn{5}{l}\n",
    "{\\footnotesize It replicates the numerical results underlying the right panel of Figure A.1 in RT.}\\\\\n",
    "\\multicolumn{5}{l}",
    "{\\footnotesize Standard errors are heteroskedasticity-robust.}"
  )
)

# atab5 ----
atab5 <- list(atab5_1, atab5_2, atab5_3) %>% map(broom::tidy) %>%
  map(dplyr::select, estimate, std.error, statistic, p.value, df)
for (i in 1:3) {
  atab5[[i]] %<>% {
    .[i + 2, ]
  } %>%
    mutate(obs = df + 2) %>%
    dplyr::select(-df)
  rm(i)
}
atab5 %>% unlist %>%
  matrix(
    nrow = 3,
    ncol = 5,
    byrow = T,
    dimname = list(
      c(
        "Supporters vs Opponents",
        "Democrats vs Republicans",
        "Liberal vs Conservative"
      ),
      c(
        "Estimate",
        "Standard Error",
        "\\(t\\)-statistic",
        "\\(p\\)-value",
        "\\(N\\)"
      )
    )
  ) %>%
  xtable(
    digits = c(0, rep(3, 4), 0),
    align = "lrcrrr",
    caption = "Effect of the Sandy Hook shooting on Gun Control Support Polarization",
    label = "atab5"
  ) %>%
  print.xtable(
    type = "latex",
    "tabs/atab5.tex",
    table.placement = "!htbp",
    add.to.row = list(
      pos = list(3),
      command =
        paste(
          "\\bottomrule\n",
          "\\multicolumn{5}{l}\n",
          "{\\footnotesize It replicates the numerical results underlying Figure 3 in RT.}\\\\\n",
          "\\multicolumn{5}{l}\n",
          "{\\footnotesize We do not discuss this table in the main text. See RT for more details.}"
        )
    ),
    hline.after = c(-1, 0),
    sanitize.text.function = function(x) {
      x
    },
    math.style.negative = T,
    caption.placement = "top",
    booktabs = T
  )

# fig1 ----
prob <- function(input, obs) {
  gglist <-
    input %>% map(plot_model, type = "pred", term = "postevent")
  for (i in 1:15) {
    gglist[[i]] <- gglist[[i]] + labs(
      title = name[i],
      subtitle = paste("N =",
                       obs[i]),
      x = "",
      y = ""
    ) +
      theme_half_open() + background_grid(major = "y") +
      theme(
        plot.title = element_text(family = "Times", face = "plain"),
        plot.subtitle = element_text(family = "Times"),
        axis.text = element_text(family = "Times"),
      )
  }
  gglist %>% map(ggplotGrob) %>%
    cowplot::plot_grid(plotlist = .)
}
prob(logit, obs_cs)
ggsave("figs/fig1.pdf", width = 12, height = 10)

# tab1 ----
ologit %<>% map(broom::tidy, exponentiate = T) %>% map(slice, 1) %>%
  map(dplyr::select, estimate, std.error, statistic) %>%
  map(mutate, pvalue = pnorm(abs(statistic), lower.tail = F) * 2)
for (i in 1:15) {
  ologit[[i]] %<>% mutate(obs = obs_cs[i])
  rm(i)
}
ologit %>% unlist %>%
  matrix(
    nrow = 15,
    ncol = 5,
    byrow = T,
    dimname = list(
      name,
      c(
        "Estimate",
        "Standard Error",
        "\\(z\\)-statistic",
        "\\(p\\)-value",
        "\\(N\\)"
      )
    )
  ) %>%
  xtable(
    digits = c(0, rep(3, 4), 0),
    align = "lccrrr",
    caption = paste(
      "Effect of the Sandy Hook Shooting on Gun Control Support (5-points Likert Scale),",
      "Ordered Logit Results"
    ),
    label = "tab1"
  ) %>%
  print.xtable(
    file = "tabs/tab1.tex",
    type = "latex",
    add.to.row = list(
      pos = list(15),
      command =
        paste(
          "\\bottomrule\n",
          "\\multicolumn{5}{l}",
          "{\\footnotesize Estimates (but not standard errors) exponentiated. Cut point estimates omitted.}\n"
        )
    ),
    hline.after = c(-1, 0),
    sanitize.text.function = function(x) {
      x
    },
    math.style.negative = T,
    caption.placement = "top",
    booktabs = T
  )

# tab2 ----
texreg(
  list(edu_logit, edu_ologit),
  file = "tabs/tab2.tex",
  label = "tab2",
  custom.coef.name = c(
    "Constant",
    "Post-shooting (Yes = 1)",
    "Education (No = 1, Doctorate = 15)",
    "Post-shooting \\(\\times\\) Education",
    "\\(\\tau,\\) 1\\textbar 2",
    "\\(\\tau,\\) 2\\textbar 3",
    "\\(\\tau,\\) 3\\textbar 4",
    "\\(\\tau,\\) 4\\textbar 5"
  ),
  reorder.coef = c(2:4, 1, 5:8),
  custom.model.name = c("Logit", "Ordered Logit"),
  custom.note = "\\(\\tau\\) denotes cut point. Standard errors in parentheses. %stars.",
  include.aic = F,
  include.deviance = F,
  custom.gof.name = c("BIC", "\\(\\ln\\mathcal{L}\\)", "\\(N\\)"),
  caption = "Effect of the Sandy Hook Shooting on Gun Control Support Conditional on Education",
  caption.above = T,
  dcolumn = T,
  booktabs = T,
  use.packages = F
)
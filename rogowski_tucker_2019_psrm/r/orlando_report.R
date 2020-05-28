name <- c("Full Sample",
          "Female",
          "Male",
          "Parents",
          "Non-parents",
          "Political Interest: Yes",
          "Political Interest: No",
          "News Everyday: Yes",
          "News Everyday: No",
          "Liberal",
          "Conservative",
          "Support Obama: Yes",
          "Support Obama: No",
          "Political Knowledge: Yes",
          "Political Knowledge: No")
obs_full <- rep(0, 15)
obs_narrow <- rep(0, 15)
for (i in 1:15) {
  obs_full[i] <- dat_full %>% filter_(group[i]) %>% 
    dplyr::select(postevent) %>% nrow
  obs_narrow[i] <- dat_narrow %>% filter_(group[i]) %>% 
    dplyr::select(postevent) %>% nrow
  rm(i)
}
tab3 %>% unlist %>% matrix(nrow = 15, ncol = 2, byrow = T,
                            dimnames = list(name,
                                            c("Pre-shooting", "Post-shooting"))) %>%
  {100 * .} %>%
  xtable(align = "lcc",
         digits = c(0, 3, 3),
         caption = "Percentage of Gun Control Supporters, before and after the 2016 Orlando Shooting",
         label = "tab3"
         ) %>%
  print.xtable(
    type = "latex",
    "tabs/tab3.tex",
    table.placement = "!htbp",
    caption.placement = "top",
    booktabs = T
  )
prob(fig2, obs_full)
ggsave("figs/fig2.pdf", width = 11, height = 9)
prob(fig3, obs_narrow)
ggsave("figs/fig3.pdf", width = 11, height = 9)
texreg(atab6,
       file = "tabs/atab6.tex", label = "atab6",
       custom.coef.name = c(
         "Constant",
         "Post-shooting (Yes = 1)"
       ),
       reorder.coef = c(2, 1),
       custom.model.name = "(Logit)",
       custom.note = "Standard errors in parentheses. %stars.",
       include.aic = F,
       include.deviance = F,
       custom.gof.name = c("BIC", "\\(\\ln\\mathcal{L}\\)", "\\(N\\)"),
       caption = "Survey Completion Date and Gun Control Question No Opinion Rate",
       caption.above = T,
       dcolumn = T,
       booktabs = T,
       use.packages = F
       )
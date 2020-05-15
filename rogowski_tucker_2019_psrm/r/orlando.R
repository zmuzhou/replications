rm(list = setdiff(ls(), "prob"))

# load data ----
dat_full <- read_spss("taps2016.sav") %>%
  mutate(
    finishdate = as.Date(.$tm_finishS55 / 86400, origin = "1582-10-14"),
    postevent = case_when(
      finishdate > as.Date("2016-06-12") ~ 1,
      finishdate < as.Date("2016-06-12") ~ 0
    ),
    postevent = factor(postevent),
    guncontrol = case_when(ISSUESA4GS55 == 3 ~ 0,
                           ISSUESA4GS55 == 2 ~ 1),
    hasopinion = case_when(ISSUESA4GS55 == 4 ~ 0,
                           ISSUESA4GS55 %in% c(2, 3) ~ 1),
    female = case_when(SEXS55 == 3 ~ 1,
                       SEXS55 == 2 ~ 0),
    parent = case_when(CHILD1S55 == 2 ~ 1,
                       CHILD1S55 == 3 ~ 0),
    polinterest = case_when(INTERESTPOLS55 %in% c(2, 3) ~ 1, # very and somewhat
                            INTERESTPOLS55 %in% c(4, 5) ~ 0),
    # slightly and not at all
    newseveryday = case_when(NEWSFREQS55 == 2 ~ 1,
                             NEWSFREQS55 %in% c(3:8) ~ 0),
    liberal = case_when(LIBCON0S55 %in% c(1:3) ~ 1,
                        LIBCON0S55 %in% c(4:6) ~ 0),
    obamaapprove = case_when(APPRPRESS55 %in% c(2, 3) ~ 1, # strongly and somewhat approve
                             APPRPRESS55 %in% c(4, 5, 7) ~ 0),
    # disapprove and not sure
    knowledge = case_when(POLKNOW3S55 == 4 ~ 1, # senator term
                          POLKNOW3S55 %in% c(1:3, 5:6) ~ 0)
  ) %>% # including refused and dk
  drop_na(jun2016wt1S55)

group <- c(
  "postevent %in% c(0, 1)",
  "female == 1",
  "female == 0",
  "parent == 1",
  "parent == 0",
  "polinterest == 1",
  "polinterest == 0",
  "newseveryday == 1",
  "newseveryday == 0",
  "liberal == 1",
  "liberal == 0",
  "obamaapprove == 1",
  "obamaapprove == 0",
  "knowledge == 1",
  "knowledge == 0"
)

# atab6 ----
atab6 <-
  glm(hasopinion ~ postevent,
      data = dat_full,
      family = binomial (link = "logit"))

# fig2 ----
est_fig2 <- function(group) {
  dat_full %>% filter_(group) %>%
    svyglm(
      guncontrol ~ postevent,
      design = svydesign(
        ids = ~ 1,
        data = .,
        weights = .$jun2016wt1S55
      ),
      data = .,
      family = binomial (link = "logit")
    )
}
fig2 <- map(group, est_fig2)

# fig3 ----
dat_narrow <- dat_full %>%
  mutate(
    postevent = case_when(
      finishdate > as.Date("2016-06-09") &
        finishdate < as.Date("2016-06-12") ~ 0,
      finishdate > as.Date("2016-06-13") &
        finishdate < as.Date("2016-06-20") ~ 1
    ),
    postevent = factor(postevent)
  ) %>%
  filter(postevent %in% c(0, 1))
est_fig3 <- function(group) {
  dat_narrow %>% filter_(group) %>%
    svyglm(
      guncontrol ~ postevent,
      design = svydesign(
        ids = ~ 1,
        data = .,
        weights = .$jun2016wt1S55
      ),
      data = .,
      family = binomial (link = "logit")
    )
}
fig3 <- map(group, est_fig3)

rm(list = ls())

# load data ----
# cross-sectional
dat_cs <- read_dta("dataverse_files/PSRM_Replication_Data.dta") %>% 
  mutate(IGUNS13_ordinal = case_when(IGUNS13 == 5 ~ 1,
                                     IGUNS13 == 4 ~ 2,
                                     IGUNS13 == 3 ~ 3,
                                     IGUNS13 == 2 ~ 4,
                                     IGUNS13 == 1 ~ 5),
         IGUNS13_binary = case_when(IGUNS13_ordinal %in% c(4, 5) ~ 1,
                                    IGUNS13_ordinal %in% c(1:3) ~ 0),
         postevent = case_when(before_newtown == 0 ~ 1,
                               before_newtown == 1 ~ 0),
         postevent = factor(postevent),
         proximity = case_when(newregion %in% c(2, 3, 4) ~ 0,
                               newregion == 1 ~ 1),
         democrat = case_when(PID3_MAXN == 1 ~ 1,
                              TRUE ~ 0),
         republican = case_when(PID3_MAXN == 2 ~ 1,
                                TRUE ~ 0),
         indep = case_when(PID3_MAXN == 3 ~ 1,
                           TRUE ~ 0),
         liberal = case_when(IDEOL4SP == 1 ~ 1,
                             TRUE ~ 0),
         moderate = case_when(IDEOL4SP == 2 ~ 1,
                              TRUE ~ 0),
         conservative = case_when(IDEOL4SP == 3 ~ 1,
                                  TRUE ~ 0),
         female = case_when(ppgender == 2 ~ 1,
                            ppgender == 1 ~ 0),
         parent = case_when(CHLDUNDR18SP == 1 ~ 1,
                            CHLDUNDR18SP == 2 ~ 0),
         nra = MEM1C_2_3) %>%
  filter(IGUNS13_ordinal %in% c(1:5)) # drop missing values

# first difference
dat_fd <- dat_cs %>%
  mutate(IGUNS14_ordinal = case_when(IGUNS14 == 5 ~ 1,
                                     IGUNS14 == 4 ~ 2,
                                     IGUNS14 == 3 ~ 3,
                                     IGUNS14 == 2 ~ 4,
                                     IGUNS14 == 1 ~ 5),
         IGUNS14_binary = case_when(IGUNS14_ordinal %in% c(4, 5) ~ 1,
                                    IGUNS14_ordinal %in% c(1:3) ~ 0)) %>%
  filter(postevent == 0, # only keep Dec respondents who finished before the shooting
         IGUNS14_ordinal %in% c(1:5)) # drop missing values

# group conditions and variable names
group <- c("postevent %in% c(0, 1)", # trivial condition for full sample
                     "democrat == 1",
                     "republican == 1",
                     "indep == 1",
                     "liberal == 1",
                     "moderate == 1",
                     "conservative == 1",
                     "female == 1",
                     "female == 0",
                     "parent == 1",
                     "parent == 0",
                     "nra == 1",
                     "nra == 0",
                     "proximity == 1",
                     "proximity == 0")

# rep_fig2 ----
est_atab1 <- function(group) {
  dat_cs %>% filter_(group) %>%
    lm_robust(IGUNS13_binary ~ postevent,
              data = ., se_type = "stata",
              weights = .$dec2012wt1)
}
est_atab2 <- function(group) {
  dat_fd %>% filter_(group) %>%
    lm_robust((IGUNS14_binary - IGUNS13_binary) ~ 1,
              data = ., se_type = "stata",
              weights = .$dec2012wt1)
}
atab1 <- map(group, est_atab1)
atab2 <- map(group, est_atab2)

# rep_figa1 ----
est_atab3 <- function(group) {
  dat_cs %>% filter_(group) %>%
    lm_robust(IGUNS13_ordinal ~ postevent,
              data = ., se_type = "stata",
              weights = .$dec2012wt1)
}
est_atab4 <- function(group) {
  dat_fd %>% filter_(group) %>%
    lm_robust((IGUNS14_ordinal - IGUNS13_ordinal) ~ 1,
              data = ., se_type = "stata",
              weights = .$dec2012wt1)
}
atab3 <- map(group, est_atab3)
atab4 <- map(group, est_atab4)

# rep_fig3 ----
# two new variables
dat_fd %<>% mutate(
  ideodk = case_when(IDEOL4SP == 4 ~ 1,
                     TRUE ~ 0),
  support = case_when(IGUNS13 %in% c(1, 2) ~ 1,
                      IGUNS13 %in% c(4, 5) ~ 0))
# linear combination test
est_atab5 <- function(formula, lincom) {
  lh_robust(formula, linear_hypothesis = lincom,
            data = dat_fd, weights = dat_fd$dec2012wt1, se_type = "stata")
}
atab5_1 <- est_atab5((IGUNS14_ordinal - IGUNS13_ordinal) ~ support, 
                      "support = 0")
atab5_2 <- est_atab5((IGUNS14_ordinal - IGUNS13_ordinal) ~ -1 + democrat + republican + indep,
                      "democrat = republican")
atab5_3 <- est_atab5((IGUNS14_ordinal - IGUNS13_ordinal) ~ -1 + liberal + moderate + conservative + ideodk,
                      "liberal = conservative")

# robustness_logit ----
est_logit<- function(group) {
  dat_cs %>% filter_(group) %>%
    glm(IGUNS13_binary ~ postevent,
        data = ., family = binomial (link = "logit"))
}
logit <- map(group, est_logit)

# robustness_ordered_logit ----
dat_cs %<>% 
  mutate(IGUNS13_ordinal = factor(IGUNS13_ordinal, levels = c(1:5)))
est_ologit <- function(group) {
  dat_cs %>% filter_(group) %>%
    polr(IGUNS13_ordinal ~ postevent, data = ., Hess = T, method = "logistic")
}
ologit <- map(group, est_ologit)

# edu ----
edu_logit <- dat_cs %>% filter(educsp != -1) %>%
glm(IGUNS13_binary ~ postevent + educsp + postevent * educsp,
             data = ., family = binomial (link = "logit"))
edu_ologit <- dat_cs %>% filter(educsp != -1) %>%
  polr(IGUNS13_ordinal ~ postevent + educsp + postevent * educsp, 
              data = ., Hess = T, method = "logistic")

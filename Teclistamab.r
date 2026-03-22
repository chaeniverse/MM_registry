library(tidyverse)
library(readxl)
library(gtsummary)
library(flextable)
library(MatchIt)
library(flextable)
library(officer)
library(cobalt)
library(survival)
library(ggsurvfit)
library(ggsci)
library(gt)
library(data.table)
set.seed(0619)
library(anytime)


read_excel('data/teclistamab_data_weight_height_2025-11-12.xlsx', sheet = 1) %>% colnames()


read_excel('data/Control cohort_250628_new.xlsx', skip = 1) %>% colnames()

data_soc <- read_excel('C:/Users/chaehyun/Downloads/SOC cohort_n87 data_0111.xlsx', skip = 1) %>% filter(Group == 2) %>% select(ISS_DX, SEX, CTX_S_DATE, BIRTH, CTX_LINE, HB_CTX, PLT_CTX, GFR_CTX, LDH_HIGH1_CTX, VS_PFS_CTX, DATE_PFS_CTX, PRE_CTX_R, VS, DATE_LAST, RESP_CTX, PID)%>% 
                    mutate(CTX_S_DATE = as.Date(CTX_S_DATE),
                           BIRTH = as.Date(BIRTH),
                           DATE_PFS_CTX = anytime::anydate(DATE_PFS_CTX),
                           DATE_LAST = as.Date(DATE_LAST)) %>%
                    mutate(AGE = CTX_S_DATE - BIRTH) %>%
                    mutate(AGE = as.numeric(AGE) / 365.25) %>%
                    mutate(
                      HB_CTX = as.numeric(HB_CTX),
                      PLT_CTX = as.numeric(PLT_CTX),
                      GFR_CTX = as.numeric(GFR_CTX),
                      VS_PFS_CTX = as.numeric(VS_PFS_CTX),
                      PRE_CTX_R = as.numeric(PRE_CTX_R)) %>%
                      mutate(Group = 2)

data_tecli <-read_excel('C:/Users/chaehyun/Downloads/teclistamab_case cohort_n29_250111.xlsx', sheet = 1) %>%  select(ISS_DX, SEX, CTX_S_DATE, BIRTH, CTX_LINE, HB_CTX, PLT_CTX, GFR_CTX, LDH_HIGH1_CTX, VS_PFS_CTX, DATE_PFS_CTX, PRE_CTX_R, VS, DATE_LAST, RESP_CTX, PID)%>% 
                    mutate(CTX_S_DATE = as.Date(CTX_S_DATE),
                           BIRTH = as.Date(BIRTH),
                           DATE_PFS_CTX = as.Date(DATE_PFS_CTX),
                           DATE_LAST = as.Date(DATE_LAST)) %>%
                    mutate(AGE = CTX_S_DATE - BIRTH) %>%
                    mutate(AGE = as.numeric(AGE) / 365.25) %>%
                    mutate(
                      HB_CTX = as.numeric(HB_CTX),
                      PLT_CTX = as.numeric(PLT_CTX),
                      GFR_CTX = as.numeric(GFR_CTX),
                      VS_PFS_CTX = as.numeric(VS_PFS_CTX),
                      PRE_CTX_R = as.numeric(PRE_CTX_R)) %>%
                      mutate(Group = 1)

data_soc <- read_excel('data/Control cohort_250628_new.xlsx', skip = 1) %>% select(ISS_DX, SEX, CTX_S_DATE, BIRTH, CTX_LINE, HB_CTX, PLT_CTX, GFR_CTX, LDH_HIGH1_CTX, VS_PFS_CTX, DATE_PFS_CTX, PRE_CTX_R, VS, DATE_LAST, RESP_CTX, PID) %>% 
            mutate(CTX_S_DATE = as.Date(CTX_S_DATE),
                           BIRTH = as.Date(BIRTH),
                           DATE_PFS_CTX = as.Date(DATE_PFS_CTX),
                           DATE_LAST = as.Date(DATE_LAST)) %>%
                    mutate(AGE = CTX_S_DATE - BIRTH) %>%
                    mutate(AGE = as.numeric(AGE) / 365.25) %>%
                    mutate(
                      HB_CTX = as.numeric(HB_CTX),
                      PLT_CTX = as.numeric(PLT_CTX),
                      GFR_CTX = as.numeric(GFR_CTX),
                      VS_PFS_CTX = as.numeric(VS_PFS_CTX),
                      PRE_CTX_R = as.numeric(PRE_CTX_R)) %>%
                    mutate(Group = 2)

data_tecli_all <- rbind(data_tecli, data_soc)

data_tidy <- data_tecli_all %>% 
              mutate(CTX_LINE = case_when(
                CTX_LINE == 1 ~ "3",
                CTX_LINE == 2 ~ "3",
                CTX_LINE == 3 ~ "3",
                CTX_LINE == 4 ~ "4",
                CTX_LINE == 5 ~ "5",
                CTX_LINE == 6 ~ "6",
                CTX_LINE == 7 ~ "6",
                CTX_LINE == 8 ~ "6",
                CTX_LINE == 9 ~ "6",
                CTX_LINE >= 10 ~ "6",
                TRUE ~ NA_character_))  %>% 
                # mutate(Group = as.factor(Group)) %>%
                mutate(
                  ISS_DX = case_when(
                    ISS_DX == "1" ~ "ISS I/II",
                    ISS_DX == "2" ~ "ISS I/II",
                    ISS_DX == "3" ~ "ISS III",
                    is.na(ISS_DX) ~ "Unknown",
                    TRUE ~ ISS_DX
                  )) %>%
                mutate(
                  GFR_CTX = case_when(
                    GFR_CTX < 30 ~ "<30",
                    GFR_CTX >= 30 & GFR_CTX < 60 ~ "<60",
                    GFR_CTX >= 60 & GFR_CTX < 90 ~ "<60",
                    GFR_CTX >= 90 ~"<60",
                    TRUE ~ "NA"
                  )) %>%
                mutate(
                  PLT_CTX = case_when(
                    PLT_CTX < 50 ~ "<100",
                    PLT_CTX >= 50 & PLT_CTX < 100 ~ "<100",
                    PLT_CTX >= 100 & PLT_CTX < 150 ~ "50-99",
                    PLT_CTX >= 150 & PLT_CTX < 200 ~ "50-99",
                    PLT_CTX >= 200 ~ "50-99",
                    TRUE ~ "NA"
                  ))  %>%
                mutate(
                  AGE = case_when(
                    AGE < 65 ~ "<70",
                    AGE >= 65 & AGE < 70 ~ "<70",
                    AGE >= 70 & AGE < 75 ~ "70-79",
                    AGE >= 75 & AGE < 80 ~ "70-79",
                    AGE >= 80 ~ ">=80",
                    TRUE ~ "NA"
                  )) %>% 
                  mutate(HB_CTX = case_when(
                    HB_CTX < 8 ~ "<8",
                    HB_CTX >= 8 & HB_CTX < 10 ~ "8-12",
                    HB_CTX >= 10 & HB_CTX < 12 ~ "8-12",
                    HB_CTX >= 12 ~ "8-12",
                    TRUE ~ "NA"
                  )) %>%
                mutate(
                    TIME = DATE_PFS_CTX - CTX_S_DATE,
                    TIME2 = DATE_LAST - CTX_S_DATE, 
                    TIME = as.numeric(TIME),
                    TIME2 = as.numeric(TIME2)) %>% 
                filter(TIME >= 0 & TIME2 >= 0) 

data_tidy$Group <- factor(data_tidy$Group, levels = c("2", "1"), labels = c("SOC", "Teclistamab"))

####! Survival analysis ------------------------------------------------------
# Fit survival model (PFS)

fit <- survfit2(Surv(TIME/365, VS_PFS_CTX) ~ Group, data = data_tidy)

four_digit <- function(p) {
  sprintf("%.4f", p)
}

p <- fit %>%
  ggsurvfit(linewidth = 1.2,) +
  add_risktable() + 
  add_pvalue(
    location     = "caption",   # or "plot" / "annotation"
    pvalue_fun   = four_digit,
    prepend_p    = TRUE         # adds “p=” before the number
  )+  
  add_quantile(linewidth = 1, linetype = "dashed") +
    scale_color_manual(
    name   = "Group",                 # 범례 제목
    values = pal_lancet()(2)) +
  scale_ggsurvfit() +
  # scale_x_continuous(breaks = seq(0, 1, by = 100)) +
  theme_classic(base_size = 14) +
  labs(color = "Group") +
  labs(title = "Kaplan-Meier Curve by Group", x = "Time", y = "Survival Probability")

p

# Fit survival model (OS)

fit <- survfit2(Surv(TIME2/365, VS) ~ Group, data = data_tidy)
fit


four_digit <- function(p) {
  sprintf("%.4f", p)
}


p <- fit %>%
  ggsurvfit(linewidth = 1.2,) +
  add_risktable() + 
  add_pvalue(
    location     = "caption",   # or "plot" / "annotation"
    pvalue_fun   = four_digit,
    prepend_p    = TRUE         # adds “p=” before the number
  )+  
  add_quantile(linewidth = 1, linetype = "dashed") +
    scale_color_manual(
    name   = "Group",                 # 범례 제목
    values = pal_lancet()(2)) +
  scale_ggsurvfit() +
  # scale_x_continuous(breaks = seq(0, 1, by = 100)) +
  theme_classic(base_size = 14) +
  labs(color = "Group") +
  labs(title = "Kaplan-Meier Curve by Group", x = "Time", y = "Survival Probability")
p


#* OS, PFS Rate Table -----------------------------------


# --- helper ---------------------------------------------------------------
format_rate <- function(est, lcl, ucl, digits = 1) {
  sprintf("%.*f%% (95%% CI %.*f–%.*f)", digits, est*100, digits, lcl*100, digits, ucl*100)
}

format_median <- function(med, lcl, ucl, digits = 2) {
  sprintf("%.*f (95%% CI %.*f–%.*f)", digits, med, digits, lcl, digits, ucl)
}

make_surv_table <- function(fit, times = c(1, 2, 3)) {
  ## 1-, 2-, 3-year survival ----------------------------------------------
  s <- summary(fit, times = times)
  rate_tbl <- 
    tibble::tibble(
      Group   = s$strata,
      Time    = s$time,
      Stat    = format_rate(s$surv, s$lower, s$upper)
    ) |>
    tidyr::pivot_wider(names_from = Time,
                       values_from = Stat,
                       names_glue = "{Time}-year")

  ## median survival --------------------------------------------------------
  med <- summary(fit)$table
  med_tbl <- tibble::tibble(
      Group  = rownames(med),
      Median = format_median(med[, "median"],
                             med[, "0.95LCL"],
                             med[, "0.95UCL"])
    )

  ## merge & order ----------------------------------------------------------
  dplyr::left_join(rate_tbl, med_tbl, by = "Group") |>
    dplyr::rename(`Median (yr)` = Median)
}


surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME2/365, VS) ~ Group, data = data_tidy)
)
surv_table

surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )


surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME/365, VS_PFS_CTX) ~ Group, data = data_tidy)
)

surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )



####! Response analysis ------------------------------------------------------
# Response rate table

data_tidy %>% select(Group, RESP_CTX) %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    CR = sum(RESP_CTX == "CR", na.rm = TRUE),
    PR = sum(RESP_CTX == "PR", na.rm = TRUE),
    SD = sum(RESP_CTX == "SD", na.rm = TRUE),
    PD = sum(RESP_CTX == "PD", na.rm = TRUE),
    ORR = (CR + PR) / n * 100,
    DCR = (CR + PR + SD) / n * 100
  ) %>%
  mutate(
    ORR = sprintf("%.1f%%", ORR),
    DCR = sprintf("%.1f%%", DCR)
  ) %>%
  select(Group, n, CR, PR, SD, PD, ORR, DCR)

data_tidy %>% select(Group, RESP_CTX) %>%
    tbl_summary(Group, 
                statistic = list(RESP_CTX ~ "{n} ({p}%)"),
                label = list(RESP_CTX ~ "Response")) %>%
    add_overall() %>%
    modify_header(label = "**Response**") 



####! Matching ---------------------------------------------------------------

head(data_tidy)
library(tableone)
psm = matchit(Group~ SEX+CTX_LINE + AGE + PLT_CTX,
                data=data_tidy, method = "nearest", distance = "logit", replace = FALSE, caliper = 0.2, ratio =1)

bal.tab(psm, m.threshold = 0.1, v.threshold = 2, un = TRUE)
bal.tab(psm,
        un = TRUE,
        m.threshold = 0.1, v.threshold = 2,
        binary = "std")

matched_data = match.data(psm)
tab <- CreateTableOne(vars = c("SEX","AGE", "PLT_CTX", "CTX_LINE"),strata="Group", data=matched_data, test=F)
ExtractSmd(tab)


theme_gtsummary_compact()
matched_data %>% filter(Group == "SOC") %>% pull(PID)
data_tecli %>% filter(PID %in% matched_data$PID) %>% select(Group, AGE) %>%
  mutate(AGE = ifelse(AGE < 65, "65", "over 65")) %>%
  tbl_summary(Group)

matched_data %>%
  select(Group, ISS_DX, SEX, CTX_LINE, HB_CTX, PLT_CTX, GFR_CTX, AGE, LDH_HIGH1_CTX) %>%
  tbl_summary(Group, 
            statistic = list(all_continuous() ~ "{mean} ({sd})",
                             all_categorical() ~ "{n} ({p}%)"),
            label = list(CTX_LINE ~ "CTX Line",
                         AGE ~ "Age",
                         PLT_CTX ~ "Platelet Count")) 
matched_data

matched_data2 <- matched_data %>% select(Group, CTX_LINE, PID,  SEX, AGE, PLT_CTX)
psm = matchit(Group~ CTX_LINE,
                data=matched_data2, method = "nearest", distance = "logit", replace = FALSE, caliper = 0.2, ratio =1)

matched_data2 = match.data(psm)
tab <- CreateTableOne(vars = c("SEX","AGE", "PLT_CTX", "CTX_LINE"),strata="Group", data=matched_data2, test=F)
ExtractSmd(tab)


library(dplyr)

pair_id <- matched_data2 %>%
  group_by(subclass) %>%
  summarise(
    PID_SOC   = PID[Group == "SOC"][1],
    PID_Tecli = PID[Group == "Teclistamab"][1],
    .groups   = "drop"
  ) %>%
  arrange(PID_Tecli)   # 보기 좋게 정렬 (원하는 대로 변경 가능)

pair_id


pair_id_ctx <- pair_id %>%
  # Teclistamab 쪽 CTX_LINE 붙이기
  left_join(
    data_tecli %>%
      select(PID_Tecli = PID, CTX_LINE_Tecli = CTX_LINE),
    by = "PID_Tecli"
  ) %>%
  # SOC 쪽 CTX_LINE 붙이기
  left_join(
    data_soc %>%
      select(PID_SOC = PID, CTX_LINE_SOC = CTX_LINE),
    by = "PID_SOC"
  )
getwd()
pair_id_ctx %>% write.csv("tecli_paired_SEX추가.csv")
view(pair_id_ctx)
#! Survival analysis ------------------------------------------------------
# Fit survival model (PFS)

fit <- survfit2(Surv(TIME/365, VS_PFS_CTX) ~ Group, data = matched_data)

cox_pfs <- coxph(Surv(TIME / 365, VS_PFS_CTX) ~ Group + cluster(subclass),
                 data = matched_data)
summary(cox_pfs)

four_digit <- function(p) {
  sprintf("%.4f", p)
}

p <- fit %>%
  ggsurvfit(linewidth = 1.2,) +
  add_risktable() + 
  add_pvalue(
    location     = "caption",   # or "plot" / "annotation"
    pvalue_fun   = four_digit,
    prepend_p    = TRUE         # adds “p=” before the number
  )+  
  add_quantile(linewidth = 1, linetype = "dashed") +
    scale_color_manual(
    name   = "Group",                 # 범례 제목
    values = pal_lancet()(2)) +
  scale_ggsurvfit() +
  # scale_x_continuous(breaks = seq(0, 1, by = 100)) +
  theme_classic(base_size = 14) +
  labs(color = "Group") +
  labs(title = "Kaplan-Meier Curve by Group", x = "Time", y = "Survival Probability")

p

# Fit survival model (OS)


fit <- survfit2(Surv(TIME2/365, VS) ~ Group, data = matched_data)
fit

cox_os <- coxph(Surv(TIME2 / 365, VS) ~ Group + cluster(subclass),
                data = matched_data)
summary(cox_os)

four_digit <- function(p) {
  sprintf("%.4f", p)
}


p <- fit %>%
  ggsurvfit(linewidth = 1.2,) +
  add_risktable() + 
  add_pvalue(
    location     = "caption",   # or "plot" / "annotation"
    pvalue_fun   = four_digit,
    prepend_p    = TRUE         # adds “p=” before the number
  )+  
  add_quantile(linewidth = 1, linetype = "dashed") +
    scale_color_manual(
    name   = "Group",                 # 범례 제목
    values = pal_lancet()(2)) +
  scale_ggsurvfit() +
  # scale_x_continuous(breaks = seq(0, 1, by = 100)) +
  theme_classic(base_size = 14) +
  labs(color = "Group") +
  labs(title = "Kaplan-Meier Curve by Group", x = "Time", y = "Survival Probability")
p


#* OS, PFS Rate Table -----------------------------------


# --- helper ---------------------------------------------------------------
format_rate <- function(est, lcl, ucl, digits = 1) {
  sprintf("%.*f%% (95%% CI %.*f–%.*f)", digits, est*100, digits, lcl*100, digits, ucl*100)
}


format_median <- function(med, lcl, ucl, digits = 2) {
  sprintf("%.*f (95%% CI %.*f–%.*f)", digits, med, digits, lcl, digits, ucl)
}

make_surv_table <- function(fit, times = c(1, 2, 3)) {
  ## 1-, 2-, 3-year survival ----------------------------------------------
  s <- summary(fit, times = times)
  rate_tbl <- 
    tibble::tibble(
      Group   = s$strata,
      Time    = s$time,
      Stat    = format_rate(s$surv, s$lower, s$upper)
    ) |>
    tidyr::pivot_wider(names_from = Time,
                       values_from = Stat,
                       names_glue = "{Time}-year")

  ## median survival --------------------------------------------------------
  med <- summary(fit)$table
  med_tbl <- tibble::tibble(
      Group  = rownames(med),
      Median = format_median(med[, "median"],
                             med[, "0.95LCL"],
                             med[, "0.95UCL"])
    )

  ## merge & order ----------------------------------------------------------
  dplyr::left_join(rate_tbl, med_tbl, by = "Group") |>
    dplyr::rename(`Median (yr)` = Median)
}


surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME2/365, VS) ~ Group, data = matched_data)
)
surv_table

surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )


surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME/365, VS_PFS_CTX) ~ Group, data = matched_data)
)

summary(fit, times =1)

surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    # `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )



#! Response analysis ------------------------------------------------------
# Response rate table

matched_data %>% select(Group, RESP_CTX) %>% 
  group_by(Group) %>%
  summarise(
    n = n(),
    CR = sum(RESP_CTX == "CR", na.rm = TRUE),
    sCR = sum(RESP_CTX == "sCR", na.rm = TRUE),
    PR = sum(RESP_CTX == "PR", na.rm = TRUE),
    SD = sum(RESP_CTX == "SD", na.rm = TRUE),
    PD = sum(RESP_CTX == "PD", na.rm = TRUE),
    VGPR = sum(RESP_CTX == "VGPR", na.rm = TRUE),
    ORR = (CR + sCR + VGPR + PR) / n * 100,
    DCR = (CR + sCR + VGPR + PR + SD) / n * 100
  ) %>%
  mutate(
    ORR = sprintf("%.1f%%", ORR),
    DCR = sprintf("%.1f%%", DCR)
  ) %>%
  select(Group, n, CR, sCR, PR, SD, PD, ORR, DCR)

matched_data %>% select(Group, RESP_CTX) %>%
    tbl_summary(Group, 
                statistic = list(RESP_CTX ~ "{n} ({p}%)"),
                label = list(RESP_CTX ~ "Response")) %>%
    add_overall() %>%
    modify_header(label = "**Response**") 




#! Subgroup aanalysis (PR 이상)----------------------------------------------------


matched_data_pr <- matched_data %>% 
  filter(RESP_CTX %in% c("CR", "PR", "VGPR", "sCR")) 




#! Survival analysis ------------------------------------------------------
# Fit survival model (PFS)

fit <- survfit2(Surv(TIME/365, VS_PFS_CTX) ~ Group, data = matched_data_pr)

four_digit <- function(p) {
  sprintf("%.4f", p)
}

p <- fit %>%
  ggsurvfit(linewidth = 1.2,) +
  add_risktable() + 
  add_pvalue(
    location     = "caption",   # or "plot" / "annotation"
    pvalue_fun   = four_digit,
    prepend_p    = TRUE         # adds “p=” before the number
  )+  
  add_quantile(linewidth = 1, linetype = "dashed") +
    scale_color_manual(
    name   = "Group",                 # 범례 제목
    values = pal_lancet()(2)) +
  scale_ggsurvfit() +
  # scale_x_continuous(breaks = seq(0, 1, by = 100)) +
  theme_classic(base_size = 14) +
  labs(color = "Group") +
  labs(title = "Kaplan-Meier Curve by Group", x = "Time", y = "Survival Probability")

p

# Fit survival model (OS)


fit <- survfit2(Surv(TIME2/365, VS) ~ Group, data = matched_data_pr)
fit


four_digit <- function(p) {
  sprintf("%.4f", p)
}


p <- fit %>%
  ggsurvfit(linewidth = 1.2,) +
  add_risktable() + 
  add_pvalue(
    location     = "caption",   # or "plot" / "annotation"
    pvalue_fun   = four_digit,
    prepend_p    = TRUE         # adds “p=” before the number
  )+  
  add_quantile(linewidth = 1, linetype = "dashed") +
    scale_color_manual(
    name   = "Group",                 # 범례 제목
    values = pal_lancet()(2)) +
  scale_ggsurvfit() +
  # scale_x_continuous(breaks = seq(0, 1, by = 100)) +
  theme_classic(base_size = 14) +
  labs(color = "Group") +
  labs(title = "Kaplan-Meier Curve by Group", x = "Time", y = "Survival Probability")
p


#* OS, PFS Rate Table -----------------------------------


# --- helper ---------------------------------------------------------------
format_rate <- function(est, lcl, ucl, digits = 1) {
  sprintf("%.*f%% (95%% CI %.*f–%.*f)", digits, est*100, digits, lcl*100, digits, ucl*100)
}


format_median <- function(med, lcl, ucl, digits = 2) {
  sprintf("%.*f (95%% CI %.*f–%.*f)", digits, med, digits, lcl, digits, ucl)
}

make_surv_table <- function(fit, times = c(1, 2, 3)) {
  ## 1-, 2-, 3-year survival ----------------------------------------------
  s <- summary(fit, times = times)
  rate_tbl <- 
    tibble::tibble(
      Group   = s$strata,
      Time    = s$time,
      Stat    = format_rate(s$surv, s$lower, s$upper)
    ) |>
    tidyr::pivot_wider(names_from = Time,
                       values_from = Stat,
                       names_glue = "{Time}-year")

  ## median survival --------------------------------------------------------
  med <- summary(fit)$table
  med_tbl <- tibble::tibble(
      Group  = rownames(med),
      Median = format_median(med[, "median"],
                             med[, "0.95LCL"],
                             med[, "0.95UCL"])
    )

  ## merge & order ----------------------------------------------------------
  dplyr::left_join(rate_tbl, med_tbl, by = "Group") |>
    dplyr::rename(`Median (yr)` = Median)
}


surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME2/365, VS) ~ Group, data = matched_data_pr)
)
surv_table

surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )


surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME/365, VS_PFS_CTX) ~ Group, data = matched_data_pr)
)


surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    # `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )



#! Response analysis ------------------------------------------------------
# Response rate table

matched_data %>% select(Group, RESP_CTX) %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    CR = sum(RESP_CTX == "CR", na.rm = TRUE),
    sCR = sum(RESP_CTX == "sCR", na.rm = TRUE),
    PR = sum(RESP_CTX == "PR", na.rm = TRUE),
    SD = sum(RESP_CTX == "SD", na.rm = TRUE),
    PD = sum(RESP_CTX == "PD", na.rm = TRUE),
    VGPR = sum(RESP_CTX == "VGPR", na.rm = TRUE),
    ORR = (CR + sCR + VGPR + PR) / n * 100,
    DCR = (CR + sCR + VGPR + PR + SD) / n * 100
  ) %>%
  mutate(
    ORR = sprintf("%.1f%%", ORR),
    DCR = sprintf("%.1f%%", DCR)
  ) %>%
  select(Group, n, CR, PR, SD, PD, ORR, DCR)

matched_data %>% select(Group, RESP_CTX) %>%
    tbl_summary(Group, 
                statistic = list(RESP_CTX ~ "{n} ({p}%)"),
                label = list(RESP_CTX ~ "Response")) %>%
    add_overall() %>%
    modify_header(label = "**Response**") 



#! Exposure analysis

exposure <- read_excel('data/3~10차_chemo_2025-07-18_투여일_AE완료_simulation결과.xlsx', sheet = 1, skip = 1) %>% select(Group, starts_with('AUC'), starts_with('CMAX'), starts_with('AMT'), PID, TEC_NO) %>% filter(Group == 1, TEC_NO >= 10) %>% select(-Group)
exposure
colnames(exposure)
exposure_join <- matched_data %>% 
  left_join(exposure, by = "PID") %>%
  mutate(
    # AUCmean_AMT = AUCLST_mean/AMT_mean,
    AUCall_AMT = AUCALL_mean/AMT_mean,
  )


##–– check whether any exposure metric (AUC / CMAX) is associated with OS or PFS –––––##
library(tidyverse)
library(survival)
library(broom)

## (1) prepare analysis data --------------------------------------------------------
expo_dat <- exposure_join |>
  mutate(
    os_time  = TIME2 / 365,          # overall-survival time (yr)
    pfs_time = TIME  / 365,          # PFS time (yr)
    ORR = if_else(RESP_CTX %in% c("CR",  "VGPR", "sCR"), 1, 0, missing = NA_real_)
  )


## list of exposure variables (all AUC* or CMAX* columns)  
expo_vars <- names(expo_dat) |> str_subset("^(AUC|CMAX)")

## (2) helper to run univariable Cox models ----------------------------------------
cox_screen <- function(time, event) {
  map_dfr(
    expo_vars,
    \(v) {
      fit <- coxph(as.formula(sprintf("Surv(%s, %s) ~ %s + ISS_DX", time, event, v)),
                   data = expo_dat)
      tidy(fit, exponentiate = TRUE, conf.int = TRUE) |>      # HR, CI, p-value
        slice(1) |>
        transmute(
          variable = v,
          HR       = estimate,
          `95%CI low`  = conf.low,
          `95%CI high` = conf.high,
          p.value  = p.value
        )
    }
  )
}

## (3) run the screen for OS and PFS ------------------------------------------------
os_assoc  <- cox_screen("os_time",  "VS")           # Overall survival
pfs_assoc <- cox_screen("pfs_time", "VS_PFS_CTX")   # Progression-free survival

## (4) inspect the results ----------------------------------------------------------

os_assoc
pfs_assoc

## (5) Kaplan–Meier plots by exposure magnitude -----------------------------------
## helper: KM plot for an exposure variable ---------------------------------------
km_expo_plot <- function(var,
                         time  = "os_time",
                         event = "VS",
                         n_groups = 2) {
  ## build grouped dataset --------------------------------------------------------
  dat <- expo_dat |>
    filter(!is.na(.data[[var]])) |>
    mutate(expo_grp = ntile(.data[[var]], n_groups) |>
                       factor(labels = paste0("G", seq_len(n_groups))))
  
  ## fit & draw survival ----------------------------------------------------------
  survfit2(as.formula(sprintf("Surv(%s, %s) ~ expo_grp", time, event)), data = dat) |>
    ggsurvfit(linewidth = 1.2) +
    add_risktable() +
    labs(title  = sprintf("Survival by %s (quantiles)", var),
         x      = "Years",
         y      = "Survival probability",
         color  = var) +
    theme_classic(base_size = 14)
}

## example: overall‑survival curves by the first AUC & CMAX variables -------------
# adapt `expo_vars[ ]` index to the specific metric you want to inspect
km_expo_plot(expo_vars[grep("^AUC", expo_vars)][1])      # OS by AUC
km_expo_plot(expo_vars[grep("^CMAX", expo_vars)][1])     # OS by CMAX

## example: progression‑free survival curves (change `time` / `event`) ------------
km_expo_plot(expo_vars[grep("^AUC", expo_vars)][1],
             time  = "pfs_time",
             event = "VS_PFS_CTX")

km_expo_plot(expo_vars[grep("^CMAX", expo_vars)][1],
             time  = "pfs_time",
             event = "VS_PFS_CTX")

## (6) Response (ORR) association with exposure metrics --------------------------------
## logistic screen (one model per exposure metric) --------------------------------------
resp_screen <- function(outcome = "ORR") {
  map_dfr(
    expo_vars,
    \(v) {
      fit <- glm(as.formula(sprintf("%s ~ %s + AGE + SEX", outcome, v)),
                 data = expo_dat,
                 family = binomial)
      tidy(fit, exponentiate = TRUE, conf.int = TRUE) |>
        dplyr::filter(term == v) |>
        transmute(
          variable     = v,
          OR           = estimate,
          `95%CI low`  = conf.low,
          `95%CI high` = conf.high,
          p.value      = p.value
        )
    }
  )
}

orr_assoc <- resp_screen()
orr_assoc

## (7) Quick table of ORR rates by exposure quantile (example AUC, CMAX) --------------
orr_tbl_expo <- function(var, n_groups = 2) {
  expo_dat |>
    filter(!is.na(.data[[var]])) |>
    mutate(expo_grp = ntile(.data[[var]], n_groups) |>
                       factor(labels = paste0("Q", seq_len(n_groups)))) |>
    mutate(ORR = if_else(ORR == 1, "Responders (CR/sCR/VGPR)", "Non‑responders")) |>
    tbl_summary(by = expo_grp,
                statistic = list(ORR ~ "{n} ({p}%)"),
                include = c(ISS_DX, AGE, CTX_LINE, HB_CTX, PLT_CTX, GFR_CTX, SEX, LDH_HIGH1_CTX, ORR, RESP_CTX),
                label = list(ORR ~ "Overall Response Rate")) |>
    modify_spanning_header(all_stat_cols() ~ sprintf("**%s quantile**", var)) |>
    bold_labels()
}

# Example tables
orr_tbl_expo(expo_vars[grep("^AUC", expo_vars)][1])
orr_tbl_expo(expo_vars[grep("^CMAX", expo_vars)][1])


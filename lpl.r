# -- library -- #
library(tidyverse)
library(readxl)
library(gtsummary)
library(flextable)
library(officer)
library(lubridate)
library(devtools)
library(dplyr)
library(survminer)
library(stringr)
library(haven)
library(mstate)
library(nnet)
library(splines)
library(ggsci)
library(tableone)
library(tidyr)
library(MatchIt)
library(cobalt)
library(eha)
library(caTools)
library(data.table)
library(magrittr)
library(moonBook)
library(styler)
library(ggplot2); library(survival); library(survminer)
library(tibble)
library(cmprsk)
library(gmodels)
library(numDeriv)
library(MASS)
library(htmltools)
library(pROC)
library(car)
select <- dplyr::select

# -- paths -- #
DATA_PATH <- '/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Data/WM final_260401.xlsx'
OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Results'

# -- gtsummary theme -- #
my_theme <- list(
  "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
  "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
  "tbl_summary-arg:statistic" = list(
    all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})"),  
    all_categorical() ~ "{n} ({p}%)"
  )
)
set_gtsummary_theme(my_theme)
theme_gtsummary_compact()

# 원데이터값이 NA인 연속형 변수 -> 범주형으로 변환했을 때 NA로 활용


# =============================================================================
# 1. CRF Sheet - Main Baseline Characteristics
# =============================================================================
crf <- read_excel(DATA_PATH, sheet = 'CRF') %>%
  rename(
    `1L_1` = `1L...9`,
    IgM_1 = `IgM...24`,
    IgM_2 = `IgM...25`,
    B2MG_cont = `B2MG...40`,
    B2MG_cat = `B2MG...41`,
    `1L_2` = `1L...65`
  )
# colnames(crf)
# str(crf)

# -- 전처리 -- #
# 제외 변수: "image","WBC"
# IgM_2: 연속형 변환
# ANC re-categorization: 1000초과/1000이하
# first-line treatment에서 2,3 묶기
# BM_date에서 Not done -> 일단 NA로 표기
# IgM_1 -> >3240이면 3240으로 처리. 일단 이렇게 해서 baseline에 넣고 빼도 될 거 같으면 빼기.
# 1L_2는 1L_1과 중복. 변수 하나 제거.
# 연속형 NA -> 범주형 NA로 변환 (예: Hb, PLT, ALB, LDH, IgM_2, B2MG_cont, sPEP, WBC, ANC)
crf <- crf %>%
  select(c("TLT12", "1L_1", "age", "age65", "sex", "ECOG", "PS", "B_Sx", "LNE", 
  "HS", "IgM4", "ANC", "Hb", "Hb10", "Hb11", "PLT", "PLT100", "LDH", "LDH2", 
  "ALB", "ALB3.5", "B2MG_cont", "B2MG_cat", "IPSS", "RIPSS", "MSS", "MYD88", "CXCR4",
  "진단일","last_fu", "death")) %>%
  mutate(
    age = as.numeric(age),
    ANC = as.numeric(ANC),
    Hb = as.numeric(Hb),
    PLT = as.numeric(PLT),
    LDH = as.numeric(LDH),
    ALB = as.numeric(ALB),
    B2MG_cont = as.numeric(B2MG_cont),
    진단일 = as.Date(진단일, "%Y-%m-%d"),
    last_fu = as.Date(last_fu, "%Y-%m-%d")) %>%
  mutate(`ANC < 1000` = case_when(
    ANC < 1000 ~ 1,
    ANC >= 1000 ~ 0,
    TRUE ~ NA_integer_
  ),
  `1L_1` = case_when(
    `1L_1` == '1_BR' ~ "BR",
    `1L_1` == '2_R_Cy' | `1L_1` == '3_R_borte' ~ "R_Cy or R_borte",
    `1L_1` == '4_others' ~ "Others",
    TRUE ~ NA_character_
  ),
  `ECOG performance status < 2` = case_when(
    PS == "low" ~ 1,
    PS == "high" ~ 0,
    TRUE ~ NA_integer_
  )) %>%
  mutate(`1L_1` = factor(`1L_1`, levels = c("BR", "R_Cy or R_borte", "Others"))) %>%
  mutate(
    Hb10 = ifelse(is.na(Hb), NA, Hb10),
    Hb11 = ifelse(is.na(Hb), NA, Hb11),
    PLT100 = ifelse(is.na(PLT), NA, PLT100),
    LDH2 = ifelse(is.na(LDH), NA, LDH2),
    `ALB3.5` = ifelse(is.na(ALB), NA, `ALB3.5`),
    B2MG_cat = ifelse(is.na(B2MG_cont), NA, B2MG_cat)
  ) %>%
  mutate(
    death   = as.numeric(death),
    death_day = as.numeric(last_fu - 진단일),
    death_yr = death_day/365.25) %>%
  # IPSS, rIPSS, MSS에서 UK인 경우 NA로 처리
  mutate(
    IPSS  = case_when(IPSS  == "4_UK" ~ NA_character_, TRUE ~ as.character(IPSS)),
    RIPSS = case_when(RIPSS == "6_UK" ~ NA_character_, TRUE ~ as.character(RIPSS)),
    MSS   = case_when(MSS   == "5_UK" ~ NA_character_, TRUE ~ as.character(MSS))
  ) %>%
  # factor 변환
  mutate(
    IPSS  = factor(IPSS,  levels = c("1_Low", "2_Int", "3_High")),
    RIPSS = factor(RIPSS, levels = c("1_VL", "2_Low", "3_Int", "4_High", "5_VH")),
    MSS   = factor(MSS,   levels = c("1_Low", "2_low_Int", "3_Int", "4_High")),
    TLT12 = factor(ifelse(TLT12 == "NA", NA, TLT12)),
    TLT12 = factor(TLT12, levels=c(0,1)),
    MYD88 = factor(ifelse(MYD88 == "NA", NA, MYD88)),
    CXCR4 = factor(ifelse(CXCR4 == "NA", NA, CXCR4))
  )

# View(crf)

# --- 2-1. 분석용 데이터 준비 ---
# sPEP 제외
dat <- crf %>%
  select(
    c("TLT12", "age65", "sex", `ECOG performance status < 2`, "B_Sx", 
    "LNE", "HS", "IgM4", "ANC < 1000", "Hb10", "PLT100", "LDH2", 
    "ALB3.5", "B2MG_cat", "IPSS", "RIPSS", 
    "MSS", "MYD88", "CXCR4", "진단일","last_fu", 
    "death","death_day","death_yr")) %>%
  mutate(
    sex=factor(sex,levels=c("M","F")),
    age65=factor(age65,levels=c(0,1)),
    `ECOG performance status < 2`=factor(`ECOG performance status < 2`,levels=c(0,1)),
    LNE=factor(LNE,levels=c(0,1)),
    HS=factor(HS,levels=c(0,1)),
    Hb10=factor(Hb10,levels=c(0,1)),
    PLT100=factor(PLT100,levels=c(0,1)),
    ALB3.5=factor(ALB3.5,levels=c(0,1)),
    LDH2=factor(LDH2,levels=c(0,1)),
    IgM4=factor(IgM4,levels=c(0,1)),
    B2MG_cat=factor(B2MG_cat,levels=c(0,1)),
    B_Sx=factor(B_Sx,levels=c(0,1)),
    `ANC < 1000` = factor(`ANC < 1000`, levels = c(0, 1))
  )     
# View(dat)



# -- Uni results (Cox PH) -- #
library(gt)

covariates <- dat %>%
  select(-c("진단일","last_fu", "death", "death_day", "death_yr")) %>%
  names() 

uni_list <- lapply(covariates, function(var){
  model <- coxph(as.formula(paste("Surv(death_yr, death) ~", paste0("`", var, "`"))),
                 data = dat)
  s <- summary(model)
  coefs <- as.data.frame(s$coefficients)
  ci <- as.data.frame(s$conf.int)
  
  data.frame(
    term = rownames(coefs),
    uni_HR  = ci[["exp(coef)"]],
    uni_LCL = ci[["lower .95"]],
    uni_UCL = ci[["upper .95"]],
    uni_p   = coefs[["Pr(>|z|)"]],
    stringsAsFactors = F
  )
})

uni_list %>%
  bind_rows() %>%
  mutate(
    `HR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", uni_HR, uni_LCL, uni_UCL),
    `P-value` = ifelse(uni_p < 0.001, "<.001", sprintf("%.3f", uni_p))
  ) %>%
  select(Variable = term, `HR (95% CI)`, `P-value`) %>%
  gt() %>%
  tab_header(title = "Univariable Cox PH results (outcome = death)")



# -- IPSS/RIPSS/MSS multi results -- #
# Base covariates: missing ≤10%, excluding IPSS/RIPSS/MSS
base_vars <- dat %>%
  select(-c("진단일","last_fu","death","death_day","death_yr",
            "B2MG_cat","MYD88","CXCR4",
            "IPSS","RIPSS","MSS")) %>%
  names()

for (nm in c("None", "IPSS", "RIPSS", "MSS")) {
  if (nm == "None") {
    vars <- base_vars
    title_nm <- "Base only (no prognostic index)"
  } else {
    vars <- c(base_vars, nm)
    title_nm <- sprintf("%s model", nm)
  }
  
  fml <- as.formula(paste("Surv(death_yr, death) ~", paste0("`", vars, "`", collapse = " + ")))
  model <- coxph(fml, data = dat)
  s <- summary(model)
  coefs <- as.data.frame(s$coefficients)
  ci <- as.data.frame(s$conf.int)
  
  tbl <- data.frame(
    term = rownames(coefs),
    multi_HR  = ci[["exp(coef)"]],
    multi_LCL = ci[["lower .95"]],
    multi_UCL = ci[["upper .95"]],
    multi_p   = coefs[["Pr(>|z|)"]],
    stringsAsFactors = F
  ) %>%
    mutate(
      `HR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", multi_HR, multi_LCL, multi_UCL),
      `P-value` = ifelse(multi_p < 0.001, "<.001", sprintf("%.3f", multi_p))
    ) %>%
    select(Variable = term, `HR (95% CI)`, `P-value`) %>%
    gt() %>%
    tab_header(
      title = sprintf("Multivariable Cox PH - %s", title_nm),
      subtitle = sprintf("AIC = %.1f, N = %d", AIC(model), model$n)
    )
  
  print(tbl)
}


# -- Cox PH -- #
base_vars <- dat %>%
  select(-c("진단일","last_fu","death","death_day","death_yr",
            "B2MG_cat","MYD88","CXCR4",
            "IPSS","RIPSS","MSS")) %>%
  names()

for (nm in c("None", "IPSS", "RIPSS", "MSS")) {
  if (nm == "None") {
    vars <- base_vars
    title_nm <- "None"
  } else {
    vars <- c(base_vars, nm)
    title_nm <- sprintf("%s", nm)
  }
  
  fml <- as.formula(paste("Surv(death_yr, death) ~", paste0("`", vars, "`", collapse = " + ")))
  dat_complete <- dat[, c("death", "death_yr", vars)] %>% na.omit()
  
  # Full model
  full.model <- coxph(fml, data = dat_complete)

  # ---- VIF 확인 ---- #
  vif_vals <- car::vif(full.model)
  
  # 범주형(GVIF) 처리: vif()가 matrix를 반환하면 GVIF^(1/(2*Df)) 사용
  if (is.matrix(vif_vals)) {
    vif_df <- data.frame(
      Variable = rownames(vif_vals),
      VIF = (vif_vals[, "GVIF^(1/(2*Df))"])^2
    )
  } else {
    vif_df <- data.frame(
      Variable = names(vif_vals),
      VIF = vif_vals
    )
  }
  
  vif_tbl <- vif_df %>%
    gt() %>%
    tab_header(
      title = sprintf("VIF - %s", ifelse(nm == "None", "Base only", nm))
    )
  print(vif_tbl)


  s <- summary(full.model)
  coefs <- as.data.frame(s$coefficients)
  ci <- as.data.frame(s$conf.int)
  
  multi_df <- data.frame(
    term = rownames(coefs),
    multi_HR  = ci[["exp(coef)"]],
    multi_LCL = ci[["lower .95"]],
    multi_UCL = ci[["upper .95"]],
    multi_p   = coefs[["Pr(>|z|)"]],
    stringsAsFactors = F
  )
  
  # Stepwise
  step.model <- full.model %>% stepAIC(direction = "both", trace = F)
  s2 <- summary(step.model)
  coefs2 <- as.data.frame(s2$coefficients)
  ci2 <- as.data.frame(s2$conf.int)
  
  step_df <- data.frame(
    term = rownames(coefs2),
    step_HR  = ci2[["exp(coef)"]],
    step_LCL = ci2[["lower .95"]],
    step_UCL = ci2[["upper .95"]],
    step_p   = coefs2[["Pr(>|z|)"]],
    step_beta = coefs2[["coef"]],
    step_Score = round(coefs2[["coef"]] * 5),
    stringsAsFactors = F
  )
  
  # Merge + gt
  tbl <- full_join(multi_df, step_df, by = "term") %>%
    mutate(
      multi_ci = sprintf("%.2f (%.2f-%.2f)", multi_HR, multi_LCL, multi_UCL),
      multi_pv = ifelse(multi_p < 0.001, "<.001", sprintf("%.3f", multi_p)),
      step_ci = ifelse(is.na(step_HR), "-",
        sprintf("%.2f (%.2f-%.2f)", step_HR, step_LCL, step_UCL)),
      step_pv = ifelse(is.na(step_p), "-",
        ifelse(step_p < 0.001, "<.001", sprintf("%.3f", step_p))),
      Beta = ifelse(is.na(step_beta), "-", sprintf("%.2f", step_beta)),
      Score = ifelse(is.na(step_Score), "-", as.character(step_Score))
    ) %>%
    select(Variable = term, multi_ci, multi_pv, step_ci, step_pv) %>%
    gt() %>%
    cols_label(
      multi_ci = "HR (95% CI)", multi_pv = "P-value",
      step_ci = "HR (95% CI)", step_pv = "P-value"
    ) %>%
    tab_header(
      title = sprintf("Multivariable Cox Regression - %s", title_nm),
      subtitle = sprintf("Full AIC = %.1f / Step AIC = %.1f, N = %d / %d", AIC(full.model), AIC(step.model), full.model$n, nrow(dat))
    ) %>%
    tab_spanner(label = "Full Model", columns = c(multi_ci, multi_pv)) %>%
    tab_spanner(label = "Stepwise", columns = c(step_ci, step_pv))
  
  print(tbl)
}


# -- Survival -- #
kwci_score$risk <- ifelse(kwci_score$Score <= 5, "Low", "High")
kwci_score$death_yr <- dat_complete$death_yr
kwci_score$death <- dat_complete$death
kwci_score$TLT12 <- dat_complete$TLT12

# landmark
kwci_land <- kwci_score[kwci_score$death_yr >= 1, ]

# -- Low risk group -- #
low_dat <- kwci_land[kwci_land$risk == "Low", ]
fit_low <- survfit(Surv(death_yr, death) ~ TLT12, data = low_dat)
cox_low <- coxph(Surv(death_yr, death) ~ TLT12, data = low_dat)
sum_low <- summary(cox_low)

# median survival
sprintf("Low TLT12=0: %.1f (%.1f, %.1f)", surv_median(fit_low)[1,2], surv_median(fit_low)[1,3], surv_median(fit_low)[1,4])
sprintf("Low TLT12=1: %.1f (%.1f, %.1f)", surv_median(fit_low)[2,2], surv_median(fit_low)[2,3], surv_median(fit_low)[2,4])

# median follow-up (reverse KM)
rev_low <- low_dat
rev_low$death <- ifelse(rev_low$death == 1, 0, 1)
sfit_low <- survfit(Surv(death_yr, death) ~ TLT12, data = rev_low)
sprintf("Low FU TLT12=0: %.1f (%.1f, %.1f)", surv_median(sfit_low)[1,2], surv_median(sfit_low)[1,3], surv_median(sfit_low)[1,4])
sprintf("Low FU TLT12=1: %.1f (%.1f, %.1f)", surv_median(sfit_low)[2,2], surv_median(sfit_low)[2,3], surv_median(sfit_low)[2,4])

# HR
sprintf("Low risk - HR: %.2f (%.2f, %.2f)",
        sum_low$conf.int[1,1], sum_low$conf.int[1,3], sum_low$conf.int[1,4])
sprintf("Low risk - p: %s",
        ifelse(sum_low$coefficients[1,5] < 0.001, "<.001",
               sprintf("%.3f", sum_low$coefficients[1,5])))

# survival probability
format_surv <- function(fit, time_point) {
  s <- summary(fit, times = time_point)
  data.frame(
    group = s$strata,
    result = sprintf("%.1f%% (%.1f-%.1f)", s$surv * 100, s$lower * 100, s$upper * 100)
  )
}
format_surv(fit_low, 5) %>% gt() %>% tab_header(title = "Low risk - 5yr survival")
format_surv(fit_low, 10) %>% gt() %>% tab_header(title = "Low risk - 10yr survival")


plot
png("survival_low_risk.png", height = 10, width = 10, units = "in", res = 300)
font_size <- 18
p_low <- ggsurvplot(fit_low,
                    data = low_dat,
                    surv.median.line = "hv",
                    risk.table = TRUE,
                    tables.col = "strata",
                    tables.y.text = FALSE,
                    conf.int = FALSE,
                    xlim = c(0, 12),
                    xlab = "Time (years)",
                    ylab = "Survival Probability (%)",
                    legend = "none",
                    tables.height = 0.2,
                    break.time.by = 1,
                    risk.table.fontsize = 5,
                    palette = pal_lancet()(2),
                    # title = sprintf("Low risk (Score <= 5)\nHR: %s, %s", HR_low, p_low),
                    tables.theme = theme_cleantable() +
                      theme(plot.title = element_text(size = font_size))
)
p_low$plot <- p_low$plot +
  scale_y_continuous(labels = function(x) x * 100) +
  theme(
    axis.title = element_text(size = font_size),
    axis.text = element_text(size = font_size),
    legend.text = element_text(size = font_size - 2)
  )
print(p_low)
dev.off()

# -- High risk group -- #
high_dat <- kwci_land[kwci_land$risk == "High", ]
fit_high <- survfit(Surv(death_yr, death) ~ TLT12, data = high_dat)
cox_high <- coxph(Surv(death_yr, death) ~ TLT12, data = high_dat)
sum_high <- summary(cox_high)

# median survival
sprintf("High TLT12=0: %.1f (%.1f, %.1f)", surv_median(fit_high)[1,2], surv_median(fit_high)[1,3], surv_median(fit_high)[1,4])
sprintf("High TLT12=1: %.1f (%.1f, %.1f)", surv_median(fit_high)[2,2], surv_median(fit_high)[2,3], surv_median(fit_high)[2,4])

# median follow-up (reverse KM)
rev_high <- high_dat
rev_high$death <- ifelse(rev_high$death == 1, 0, 1)
sfit_high <- survfit(Surv(death_yr, death) ~ TLT12, data = rev_high)
sprintf("High FU TLT12=0: %.1f (%.1f, %.1f)", surv_median(sfit_high)[1,2], surv_median(sfit_high)[1,3], surv_median(sfit_high)[1,4])
sprintf("High FU TLT12=1: %.1f (%.1f, %.1f)", surv_median(sfit_high)[2,2], surv_median(sfit_high)[2,3], surv_median(sfit_high)[2,4])

# HR
sprintf("High risk - HR: %.2f (%.2f, %.2f)",
        sum_high$conf.int[1,1], sum_high$conf.int[1,3], sum_high$conf.int[1,4])
sprintf("High risk - p: %s",
        ifelse(sum_high$coefficients[1,5] < 0.001, "<.001",
               sprintf("%.3f", sum_high$coefficients[1,5])))

# survival probability
format_surv(fit_high, 5) %>% gt() %>% tab_header(title = "High risk - 5yr survival")
format_surv(fit_high, 10) %>% gt() %>% tab_header(title = "High risk - 10yr survival")

# plot
png("survival_high_risk.png", height = 10, width = 10, units = "in", res = 300)
font_size <- 18
p_high <- ggsurvplot(fit_high,
                     data = high_dat,
                     surv.median.line = "hv",
                     risk.table = TRUE,
                     tables.col = "strata",
                     tables.y.text = FALSE,
                     conf.int = FALSE,
                     xlim = c(0, 12),
                     xlab = "Time (years)",
                     ylab = "Survival Probability (%)",
                     legend = "none",
                     tables.height = 0.2,
                     break.time.by = 1,
                     risk.table.fontsize = 5,
                     palette = pal_lancet()(2),
                     tables.theme = theme_cleantable() +
                       theme(plot.title = element_text(size = font_size))
)
p_high$plot <- p_high$plot +
  scale_y_continuous(labels = function(x) x * 100) +
  theme(
    axis.title = element_text(size = font_size),
    axis.text = element_text(size = font_size),
    legend.text = element_text(size = font_size - 2)
  )
print(p_high)
dev.off()



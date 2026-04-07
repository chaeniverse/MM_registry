# =============================================================================
# LPL/WM Baseline Characteristics Table
# =============================================================================

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
  select(-c("No","센터","DOB","image","WBC", "1L_2", "BM_date", "MYD 비고", "CXCR 비고","Tx 이유","Tx course","cause1","cause2","cause3","cause4","cause5", "기타 이유", "비고")) %>%
  mutate(
    age = as.numeric(age),
    IgM_2 = as.numeric(IgM_2),
    sPEP = as.numeric(sPEP),
    ANC = as.numeric(ANC),
    Hb = as.numeric(Hb),
    PLT = as.numeric(PLT),
    LDH = as.numeric(LDH),
    ALB = as.numeric(ALB),
    TLT = as.numeric(TLT),
    B2MG_cont = as.numeric(B2MG_cont),
    진단일 = as.Date(진단일, "%Y-%m-%d"),
    last_fu = as.Date(last_fu, "%Y-%m-%d")) %>%
  mutate(`ANC > 1000` = case_when(
    ANC > 1000 ~ 1,
    ANC <= 1000 ~ 0,
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

# # -- Baseline Table -- #
# colnames(crf)
# crf %>%
#   select(c("TLT12", "age", "age65", "sex", "ECOG performance status < 2", "B_Sx",     
#   "LNE", "HS", "spleen", "liver", "IgM_2", "IgM4", "sPEP", "ANC > 1000", "Hb", "Hb10", "PLT", "PLT100", "LDH",
#   "LDH2", "ALB", "ALB3.5", "B2MG_cont", "B2MG_cat", "IPSS", "RIPSS", "MSS", "MYD88", "CXCR4")) %>% 
#   tbl_summary(
#     label = list(
#       age ~ "Age (years)",
#       age65 ~ "Age > 65",
#       sex ~ "Sex",
#       `ECOG performance status < 2` ~ "ECOG performance status < 2",
#       LNE ~ "Lymphadenopathy",
#       HS ~ "Hepatosplenomegaly",
#       Hb ~ "Hemoglobin (g/dL)",
#       Hb10 ~ "Hemoglobin < 10 g/dL",
#       `ANC > 1000` ~ "ANC > 1000",
#       PLT ~ "Platelet (x10^9/uL)",
#       PLT100 ~ "Platelet < 100 x10^9/uL",
#       ALB ~ "Albumin (g/dL)",
#       ALB3.5 ~ "Albumin < 3.5 g/dL",
#       LDH ~ "LDH (IU/L)",
#       LDH2 ~ "LDH > upper limit of normal",
#       IgM_2 ~ "IgM (mg/dL)",
#       IgM4 ~ "IgM > 4000 mg/dL",
#       B2MG_cont ~ "Beta-2 microglobulin (mg/L)",
#       B2MG_cat ~ "Beta-2 microglobulin > 3 mg/L",
#       IPSS ~ "IPSS-WM",
#       RIPSS ~ "rIPSS-WM",
#       MSS ~ "MSS-WM",
#       MYD88 ~ "MYD88 mutation",
#       CXCR4 ~ "CXCR4 mutation",
#       sPEP ~ "Serum protein electrophoresis (g/dL)"
#     ),
#     type = list(
#       all_continuous() ~ "continuous"
#     ),
#     statistic = list(                   
#       all_continuous() ~ "{median} ({p25}-{p75})",
#       all_categorical() ~ "{n} ({p})%"
#     ),
#     digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
#     missing = "ifany",
#     missing_text = "Missing",
#     missing_stat = "{N_miss} ({p_miss}%)"
#   ) %>%
#   bold_labels() %>%
#   modify_header(label = "**Characteristic**") %>%
#   modify_caption("**Table 1. Baseline Characteristics (CRF, N = {N})**") %>%
#   as_flex_table() %>%
#   flextable::save_as_docx(path = file.path(OUTPUT_DIR, "[26-04-03] Baseline_CRF.docx"))


crf %>%
  select(c("TLT12", "age", "age65", "sex", "ECOG performance status < 2", "B_Sx",     
  "LNE", "HS", "spleen", "liver", "IgM_2", "IgM4", "sPEP", "ANC > 1000", "Hb", "Hb10", "PLT", "PLT100", "LDH",
  "LDH2", "ALB", "ALB3.5", "B2MG_cont", "B2MG_cat", "IPSS", "RIPSS", "MSS", "MYD88", "CXCR4")) %>% 
  tbl_summary(
    by = TLT12,
    label = list(
      age ~ "Age (years)",
      age65 ~ "Age > 65",
      sex ~ "Sex",
      `ECOG performance status < 2` ~ "ECOG performance status < 2",
      LNE ~ "Lymphadenopathy",
      HS ~ "Hepatosplenomegaly",
      Hb ~ "Hemoglobin (g/dL)",
      Hb10 ~ "Hemoglobin < 10 g/dL",
      `ANC > 1000` ~ "ANC > 1000",
      PLT ~ "Platelet (x10^9/uL)",
      PLT100 ~ "Platelet < 100 x10^9/uL",
      ALB ~ "Albumin (g/dL)",
      ALB3.5 ~ "Albumin < 3.5 g/dL",
      LDH ~ "LDH (IU/L)",
      LDH2 ~ "LDH > upper limit of normal",
      IgM_2 ~ "IgM (mg/dL)",
      IgM4 ~ "IgM > 4000 mg/dL",
      B2MG_cont ~ "Beta-2 microglobulin (mg/L)",
      B2MG_cat ~ "Beta-2 microglobulin > 3 mg/L",
      IPSS ~ "IPSS-WM",
      RIPSS ~ "rIPSS-WM",
      MSS ~ "MSS-WM",
      MYD88 ~ "MYD88 mutation",
      CXCR4 ~ "CXCR4 mutation",
      sPEP ~ "Serum protein electrophoresis (g/dL)"
    ),
    type = list(
      all_continuous() ~ "continuous"
    ),
    statistic = list(                   
      all_continuous() ~ "{median} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p})"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing",
    missing_stat = "{N_miss} ({p_miss})"
  ) %>%
  add_p() %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 2. Baseline Characteristics by TLT12 (N = {N})**") %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = file.path(OUTPUT_DIR, "[26-04-04] Baseline_by_TLT12.docx"))


# --- 2-1. 분석용 데이터 준비 ---

dat <- crf %>%
  select(
    c("TLT12","진단일","last_fu", "death","death_day","death_yr",
    "age65","sex","ECOG performance status < 2","B_Sx", "LNE",
     "HS","spleen","liver","IgM4","sPEP", "ANC > 1000",
     "Hb10","PLT100","LDH2","ALB3.5","B2MG_cat",
     "IPSS","RIPSS","MSS","MYD88","CXCR4")) %>%
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
    spleen=factor(spleen,levels=c(0,1)),
    liver=factor(liver,levels=c(0,1)),
    B_Sx=factor(B_Sx,levels=c(0,1)),
    `ANC > 1000` = factor(`ANC > 1000`, levels = c(0, 1))
  )     
# View(dat)




# -- Uni results -- #
library(gt)

covariates <- dat %>%
  select(-c("진단일","last_fu", "death", "death_day", "death_yr")) %>%
  names() %>%
  setdiff("TLT12")

uni_list <- lapply(covariates, function(var){
  model <- glm(as.formula(paste("TLT12 ~", paste0("`", var, "`"))),
               data = dat, family="binomial")
  s <- summary(model)
  coefs <- as.data.frame(s$coefficients)[-1, , drop=F]
  
  data.frame(
    term=rownames(coefs),
    uni_OR=exp(coefs[["Estimate"]]),
    uni_LCL = exp(coefs[["Estimate"]] - 1.96*coefs[["Std. Error"]]),
    uni_UCL = exp(coefs[["Estimate"]] + 1.96*coefs[["Std. Error"]]),
    uni_p = coefs[["Pr(>|z|)"]],
    stringsAsFactors = F
  )
})

uni_list %>%
  bind_rows() %>%
  mutate(
    `OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", uni_OR, uni_LCL, uni_UCL),
    `P-value` = ifelse(uni_p < 0.001, "<.001", sprintf("%.3f", uni_p))
  ) %>%
  select(Variable = term, `OR (95% CI)`, `P-value`) %>%
  gt() %>%
  tab_header(title = "Table 3. Univariable results")


# -- IPSS/RIPSS/MSS multi results -- #
# Base covariates: missing ≤10%, excluding IPSS/RIPSS/MSS
base_vars <- dat %>%
  select(-c("진단일","last_fu","death","death_day","death_yr","TLT12",
            "B2MG_cat","MYD88","CXCR4",
            "IPSS","RIPSS","MSS")) %>%
  names()

# Three models: base + each scoring system
model_sets <- list(
  IPSS  = c(base_vars, "IPSS"),
  RIPSS = c(base_vars, "RIPSS"),
  MSS   = c(base_vars, "MSS")
)

multi_results <- lapply(names(model_sets), function(nm){
  vars <- model_sets[[nm]]
  fml <- as.formula(paste("TLT12 ~", paste0("`", vars, "`", collapse = " + ")))
  model <- glm(fml, data = dat, family = "binomial")
  s <- summary(model)
  coefs <- as.data.frame(s$coefficients)[-1, , drop = F]
  
  df <- data.frame(
    model = nm,
    term = rownames(coefs),
    multi_OR  = exp(coefs[["Estimate"]]),
    multi_LCL = exp(coefs[["Estimate"]] - 1.96 * coefs[["Std. Error"]]),
    multi_UCL = exp(coefs[["Estimate"]] + 1.96 * coefs[["Std. Error"]]),
    multi_p   = coefs[["Pr(>|z|)"]],
    AIC = AIC(model),
    stringsAsFactors = F
  )
  df
})

multi_results %>%
  bind_rows() %>%
  mutate(
    `OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", multi_OR, multi_LCL, multi_UCL),
    `P-value` = ifelse(multi_p < 0.001, "<.001", sprintf("%.3f", multi_p)),
    AIC = sprintf("%.1f", AIC)
  ) %>%
  select(Model = model, Variable = term, `OR (95% CI)`, `P-value`, AIC) %>%
  gt(groupname_col = "Model") %>%
  tab_header(title = "Multivariable results")

# -- VIF -- #
library(car)

# MSS 모델: 모든 base_vars + MSS
fml <- as.formula(paste("TLT12 ~", paste0("`", c(base_vars, "MSS"), "`", collapse = " + ")))
model_mss <- glm(fml, data = dat, family = "binomial")

# VIF 확인
vif_raw <- vif(model_mss)

vif_df <- data.frame(
  Variable = rownames(vif_raw),
  VIF = round(vif_raw[, "GVIF^(1/(2*Df))"]^2, 2)
)

vif_df %>%
  arrange(desc(VIF)) %>%
  gt() %>%
  tab_header(title = "VIF - MSS Model")

# HS 제거
vars_no_hs <- setdiff(c(base_vars, "MSS"), "HS")
model_no_hs <- glm(as.formula(paste("TLT12 ~", paste0("`", vars_no_hs, "`", collapse=" + "))), data=dat, family="binomial")
vif_no_hs <- vif(model_no_hs)
df_no_hs <- data.frame(Variable=rownames(vif_no_hs), VIF=round(vif_no_hs[,"GVIF^(1/(2*Df))"]^2,2), Model="HS 제거")

# spleen 제거
vars_no_sp <- setdiff(c(base_vars, "MSS"), "spleen")
model_no_sp <- glm(as.formula(paste("TLT12 ~", paste0("`", vars_no_sp, "`", collapse=" + "))), data=dat, family="binomial")
vif_no_sp <- vif(model_no_sp)
df_no_sp <- data.frame(Variable=rownames(vif_no_sp), VIF=round(vif_no_sp[,"GVIF^(1/(2*Df))"]^2,2), Model="spleen 제거")

bind_rows(df_no_hs, df_no_sp) %>%
  arrange(Model, desc(VIF)) %>%
  gt(groupname_col = "Model") %>%
  tab_header(title = "VIF 비교")

df_no_hs$AIC <- round(AIC(model_no_hs), 1)
df_no_sp$AIC <- round(AIC(model_no_sp), 1)

bind_rows(df_no_hs, df_no_sp) %>%
  arrange(Model, desc(VIF)) %>%
  gt(groupname_col = "Model") %>%
  tab_header(title = "VIF 및 AIC 비교")


# -- Score model -- #
base_vars <- dat %>%
  select(-c("진단일","last_fu","death","death_day","death_yr","TLT12",
            "B2MG_cat","MYD88","CXCR4",
            "IPSS","RIPSS","MSS",
            "spleen", "ANC > 1000")) %>%
  names()

vars_mss <- c(base_vars, "MSS")
fml <- as.formula(paste("TLT12 ~", paste0("`", vars_mss, "`", collapse = " + ")))

# 결측치 제거
dat_complete <- dat[, c("TLT12", vars_mss, "death", "death_yr")] %>% na.omit()

# Full model
full.model <- glm(fml, data = dat_complete, family = "binomial")
s <- summary(full.model)
coefs <- as.data.frame(s$coefficients)[-1, , drop = F]
multi_df <- data.frame(
  term = rownames(coefs),
  multi_OR  = exp(coefs[["Estimate"]]),
  multi_LCL = exp(coefs[["Estimate"]] - 1.96 * coefs[["Std. Error"]]),
  multi_UCL = exp(coefs[["Estimate"]] + 1.96 * coefs[["Std. Error"]]),
  multi_p   = coefs[["Pr(>|z|)"]],
  multi_beta = coefs[["Estimate"]],
  multi_Score = round(coefs[["Estimate"]] * 5),
  stringsAsFactors = F
)

# Stepwise
step.model <- full.model %>% stepAIC(direction = "both", trace = F)
s <- summary(step.model)
coefs <- as.data.frame(s$coefficients)[-1, , drop = F]
step_df <- data.frame(
  term = rownames(coefs),
  step_OR  = exp(coefs[["Estimate"]]),
  step_LCL = exp(coefs[["Estimate"]] - 1.96 * coefs[["Std. Error"]]),
  step_UCL = exp(coefs[["Estimate"]] + 1.96 * coefs[["Std. Error"]]),
  step_p   = coefs[["Pr(>|z|)"]],
  stringsAsFactors = F
)

# Merge + gt
full_join(multi_df, step_df, by = "term") %>%
  mutate(
    multi_ci = sprintf("%.2f (%.2f-%.2f)", multi_OR, multi_LCL, multi_UCL),
    multi_pv = ifelse(multi_p < 0.001, "<.001", sprintf("%.3f", multi_p)),
    Beta = sprintf("%.2f", multi_beta),
    Score = as.character(multi_Score),
    step_ci = ifelse(is.na(step_OR), "-",
      sprintf("%.2f (%.2f-%.2f)", step_OR, step_LCL, step_UCL)),
    step_pv = ifelse(is.na(step_p), "-",
      ifelse(step_p < 0.001, "<.001", sprintf("%.3f", step_p)))
  ) %>%
  select(Variable = term, multi_ci, multi_pv, Beta, Score, step_ci, step_pv) %>%
  gt() %>%
  cols_label(
    multi_ci = "OR (95% CI)", multi_pv = "P-value",
    Beta = "Beta", Score = "Score",
    step_ci = "OR (95% CI)", step_pv = "P-value"
  ) %>%
  tab_header(
    title = "Score Model",
    subtitle = sprintf("Full AIC = %.1f / Step AIC = %.1f", AIC(full.model), AIC(step.model))
  ) %>%
  tab_spanner(label = "Multivariable", columns = c(multi_ci, multi_pv, Beta, Score)) %>%
  tab_spanner(label = "Stepwise", columns = c(step_ci, step_pv))

library(pROC)
library(ggplot2)
library(ggsci)
library(plyr)

# -- Score 계산 -- #
kwci_score <- dat_complete %>%
  mutate(Score =
           if_else(age65 == 1, 1, 0) +
           if_else(sex == "F", -3, 0) +
           if_else(`ECOG performance status < 2` == 1, 2, 0) +
           if_else(B_Sx == 1, 2, 0) +
           if_else(LNE == 1, 1, 0) +
           if_else(HS == 1, -3, 0) +
           if_else(liver == 1, 10, 0) +
           if_else(IgM4 == 1, 2, 0) +
           if_else(Hb10 == 1, 10, 0) +
           if_else(LDH2 == 1, 9, 0) +
           if_else(ALB3.5 == 1, 8, 0) +
           case_when(
             MSS == "2_low_Int" ~ -4,
             MSS == "3_Int" ~ -9,
             MSS == "4_High" ~ -9,
             TRUE ~ 0
           )
  )
kwci_score$y <- full.model$y
lancet_cols <- pal_lancet()(2)



rlibrary(pROC)
library(ggplot2)
library(ggsci)
library(plyr)

# -- Score 계산 -- #
kwci_score <- dat_complete %>%
  mutate(Score =
           if_else(age65 == 1, 1, 0) +
           if_else(sex == "F", -3, 0) +
           if_else(`ECOG performance status < 2` == 1, 2, 0) +
           if_else(B_Sx == 1, 2, 0) +
           if_else(LNE == 1, 1, 0) +
           if_else(HS == 1, -3, 0) +
           if_else(liver == 1, 10, 0) +
           if_else(IgM4 == 1, 2, 0) +
           if_else(Hb10 == 1, 10, 0) +
           if_else(LDH2 == 1, 9, 0) +
           if_else(ALB3.5 == 1, 8, 0) +
           case_when(
             MSS == "2_low_Int" ~ -4,
             MSS == "3_Int" ~ -9,
             MSS == "4_High" ~ -9,
             TRUE ~ 0
           )
  )
kwci_score$y <- full.model$y
lancet_cols <- pal_lancet()(2)


# -- Optimal cutoff (Youden index) -- #
roc2 <- roc(kwci_score$y, kwci_score$Score)
coords_best <- coords(roc2, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))
print(coords_best)

# -- Histogram -- #
kwci_score$outcome <- factor(kwci_score$y, levels = c(0, 1),
                              labels = c("TLT12-", "TLT12+"))

# mu <- kwci_score %>%
#   group_by(outcome) %>%
#   summarise(grp.mean = mean(Score))

# p <- ggplot(kwci_score, aes(x = Score, fill = outcome, color = outcome)) +
#   geom_histogram(aes(y = after_stat(density)), binwidth = 1, alpha = 0.2, position = "identity") +
#   geom_vline(data = mu, aes(xintercept = grp.mean, color = outcome), linetype = "dashed") +
#   geom_vline(xintercept = coords_best$threshold, linetype = "dotted", color = "black", linewidth = 1) +
#   annotate("text", x = coords_best$threshold + 1, y = Inf, vjust = 2,
#            label = sprintf("Cutoff = %.1f", coords_best$threshold), size = 4) +
#   scale_fill_manual(values = lancet_cols, labels = c("TLT12-", "TLT12+")) +
#   scale_color_manual(values = lancet_cols, labels = c("TLT12-", "TLT12+")) +
#   theme(
#     panel.background = element_rect(fill = "white"),
#     panel.border = element_rect(fill = NA, colour = "black"),
#     axis.text = element_text(size = 12, colour = "black"),
#     axis.title = element_text(size = 14, colour = "black"),
#     legend.position = "bottom",
#     legend.title = element_text(face = "bold", size = 12),
#     legend.text = element_text(size = 11)
#   )

# ggsave("KWCI_histogram.png", plot = p, height = 10, width = 10, dpi = 300)


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


# plot
# png("survival_low_risk.png", height = 10, width = 10, units = "in", res = 300)
# font_size <- 18
# p_low <- ggsurvplot(fit_low,
#                     data = low_dat,
#                     surv.median.line = "hv",
#                     risk.table = TRUE,
#                     tables.col = "strata",
#                     tables.y.text = FALSE,
#                     conf.int = TRUE,
#                     xlim = c(0, 12),
#                     xlab = "Time (years)",
#                     ylab = "Survival Probability (%)",
#                     legend = "none",
#                     tables.height = 0.2,
#                     break.time.by = 1,
#                     risk.table.fontsize = 5,
#                     palette = pal_lancet()(2),
#                     # title = sprintf("Low risk (Score <= 5)\nHR: %s, %s", HR_low, p_low),
#                     tables.theme = theme_cleantable() +
#                       theme(plot.title = element_text(size = font_size))
# )
# p_low$plot <- p_low$plot +
#   scale_y_continuous(labels = function(x) x * 100) +
#   theme(
#     axis.title = element_text(size = font_size),
#     axis.text = element_text(size = font_size),
#     legend.text = element_text(size = font_size - 2)
#   )
# print(p_low)
# dev.off()

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
                     conf.int = TRUE,
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



# =============================================================================
# Chapter 2. Score model (without prognostic indices)
# =============================================================================

base_vars <- dat %>%
  select(-c("진단일","last_fu","death","death_day","death_yr","TLT12",
            "B2MG_cat","MYD88","CXCR4",
            "IPSS","RIPSS","MSS",
            "spleen", "ANC > 1000")) %>%
  names()

fml <- as.formula(paste("TLT12 ~", paste0("`", base_vars, "`", collapse = " + ")))

# 결측치 제거
dat_complete <- dat[, c("TLT12", base_vars, "death", "death_yr")] %>% na.omit()

# Full model
full.model <- glm(fml, data = dat_complete, family = "binomial")
s <- summary(full.model)
coefs <- as.data.frame(s$coefficients)[-1, , drop = F]
multi_df <- data.frame(
  term = rownames(coefs),
  multi_OR  = exp(coefs[["Estimate"]]),
  multi_LCL = exp(coefs[["Estimate"]] - 1.96 * coefs[["Std. Error"]]),
  multi_UCL = exp(coefs[["Estimate"]] + 1.96 * coefs[["Std. Error"]]),
  multi_p   = coefs[["Pr(>|z|)"]],
  multi_beta = coefs[["Estimate"]],
  multi_Score = round(coefs[["Estimate"]] * 5),
  stringsAsFactors = F
)

# Stepwise
step.model <- full.model %>% stepAIC(direction = "both", trace = F)
s <- summary(step.model)
coefs <- as.data.frame(s$coefficients)[-1, , drop = F]
step_df <- data.frame(
  term = rownames(coefs),
  step_OR  = exp(coefs[["Estimate"]]),
  step_LCL = exp(coefs[["Estimate"]] - 1.96 * coefs[["Std. Error"]]),
  step_UCL = exp(coefs[["Estimate"]] + 1.96 * coefs[["Std. Error"]]),
  step_p   = coefs[["Pr(>|z|)"]],
  stringsAsFactors = F
)

# Merge + gt
full_join(multi_df, step_df, by = "term") %>%
  mutate(
    multi_ci = sprintf("%.2f (%.2f-%.2f)", multi_OR, multi_LCL, multi_UCL),
    multi_pv = ifelse(multi_p < 0.001, "<.001", sprintf("%.3f", multi_p)),
    Beta = sprintf("%.2f", multi_beta),
    Score = as.character(multi_Score),
    step_ci = ifelse(is.na(step_OR), "-",
      sprintf("%.2f (%.2f-%.2f)", step_OR, step_LCL, step_UCL)),
    step_pv = ifelse(is.na(step_p), "-",
      ifelse(step_p < 0.001, "<.001", sprintf("%.3f", step_p)))
  ) %>%
  select(Variable = term, multi_ci, multi_pv, Beta, Score, step_ci, step_pv) %>%
  gt() %>%
  cols_label(
    multi_ci = "OR (95% CI)", multi_pv = "P-value",
    Beta = "Beta", Score = "Score",
    step_ci = "OR (95% CI)", step_pv = "P-value"
  ) %>%
  tab_header(
    title = "Score Model",
    subtitle = sprintf("Full AIC = %.1f / Step AIC = %.1f", AIC(full.model), AIC(step.model))
  ) %>%
  tab_spanner(label = "Multivariable", columns = c(multi_ci, multi_pv, Beta, Score)) %>%
  tab_spanner(label = "Stepwise", columns = c(step_ci, step_pv))

# -- ROC 비교 -- #
mod_mss <- glm(TLT12 ~ MSS, data = dat, family = "binomial")
mod_ipss <- glm(TLT12 ~ IPSS, data = dat, family = "binomial")
mod_ripss <- glm(TLT12 ~ RIPSS, data = dat, family = "binomial")

# -- ROC from each model -- #
roc_mss <- roc(mod_mss$y, mod_mss$fitted.values)
roc_ipss <- roc(mod_ipss$y, mod_ipss$fitted.values)
roc_ripss <- roc(mod_ripss$y, mod_ripss$fitted.values)

ci_mss <- ci.auc(roc_mss)
ci_ipss <- ci.auc(roc_ipss)
ci_ripss <- ci.auc(roc_ripss)

sprintf("MSS AUC: %.3f (%.3f-%.3f)", auc(roc_mss), ci_mss[1], ci_mss[3])
sprintf("IPSS AUC: %.3f (%.3f-%.3f)", auc(roc_ipss), ci_ipss[1], ci_ipss[3])
sprintf("RIPSS AUC: %.3f (%.3f-%.3f)", auc(roc_ripss), ci_ripss[1], ci_ripss[3])

# -- Full model ROC -- #
roc_full <- roc(full.model$y, full.model$fitted.values)
ci_full <- ci.auc(roc_full)
sprintf("Full model AUC: %.3f (%.3f-%.3f)", auc(roc_full), ci_full[1], ci_full[3])

# -- ROC curve 겹쳐 그리기 -- #
lancet_cols <- pal_lancet()(4)

# png("ROC_comparison.png", height = 10, width = 10, units = "in", res = 300)
# par(pty = "s")
# plot(roc_full, legacy.axes = T, col = lancet_cols[1], lwd = 3,
#      xlab = "1 - Specificity", ylab = "Sensitivity",
#      cex.lab = 1.3, cex.axis = 1.2)
# plot(roc_mss, legacy.axes = T, col = lancet_cols[2], lwd = 3, add = TRUE)
# plot(roc_ipss, legacy.axes = T, col = lancet_cols[3], lwd = 3, add = TRUE)
# plot(roc_ripss, legacy.axes = T, col = lancet_cols[4], lwd = 3, add = TRUE)

# legend("bottomright",
#        legend = c(
#          sprintf("Full model: %.3f (%.3f-%.3f)", auc(roc_full), ci_full[1], ci_full[3]),
#          sprintf("MSS: %.3f (%.3f-%.3f)", auc(roc_mss), ci_mss[1], ci_mss[3]),
#          sprintf("IPSS: %.3f (%.3f-%.3f)", auc(roc_ipss), ci_ipss[1], ci_ipss[3]),
#          sprintf("RIPSS: %.3f (%.3f-%.3f)", auc(roc_ripss), ci_ripss[1], ci_ripss[3])
#        ),
#        col = lancet_cols[1:4],
#        lwd = 3, cex = 0.9, bty = "n")
# dev.off()



# -- Score 계산 (without MSS) -- #
kwci_score <- dat_complete %>%
  mutate(Score =
           if_else(age65 == 1, -3, 0) +
           if_else(sex == "F", -3, 0) +
           if_else(`ECOG performance status < 2` == 1, 3, 0) +
           if_else(B_Sx == 1, 2, 0) +
           if_else(LNE == 1, 1, 0) +
           if_else(HS == 1, -3, 0) +
           if_else(liver == 1, 11, 0) +
           if_else(IgM4 == 1, 2, 0) +
           if_else(Hb10 == 1, 9, 0) +
           if_else(LDH2 == 1, 5, 0) +
           if_else(ALB3.5 == 1, 5, 0)
  )
kwci_score$y <- full.model$y
lancet_cols <- pal_lancet()(2)

# -- Optimal cutoff (Youden index) -- #
roc2 <- roc(kwci_score$y, kwci_score$Score)
coords_best <- coords(roc2, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))
print(coords_best)

# -- Histogram -- #
kwci_score$outcome <- factor(kwci_score$y, levels = c(0, 1),
                              labels = c("TLT12-", "TLT12+"))

# mu <- kwci_score %>%
#   group_by(outcome) %>%
#   summarise(grp.mean = mean(Score))

# p <- ggplot(kwci_score, aes(x = Score, fill = outcome, color = outcome)) +
#   geom_histogram(aes(y = after_stat(density)), binwidth = 1, alpha = 0.2, position = "identity") +
#   geom_vline(data = mu, aes(xintercept = grp.mean, color = outcome), linetype = "dashed") +
#   geom_vline(xintercept = coords_best$threshold, linetype = "dotted", color = "black", linewidth = 1) +
#   annotate("text", x = coords_best$threshold + 1, y = Inf, vjust = 2,
#            label = sprintf("Cutoff = %.1f", coords_best$threshold), size = 4) +
#   scale_fill_manual(values = lancet_cols, labels = c("TLT12-", "TLT12+")) +
#   scale_color_manual(values = lancet_cols, labels = c("TLT12-", "TLT12+")) +
#   theme(
#     panel.background = element_rect(fill = "white"),
#     panel.border = element_rect(fill = NA, colour = "black"),
#     axis.text = element_text(size = 12, colour = "black"),
#     axis.title = element_text(size = 14, colour = "black"),
#     legend.position = "bottom",
#     legend.title = element_text(face = "bold", size = 12),
#     legend.text = element_text(size = 11)
#   )

# ggsave("KWCI_histogram_noMSS.png", plot = p, height = 10, width = 10, dpi = 300)

# -- Survival -- #
kwci_score$risk <- ifelse(kwci_score$Score <= coords_best$threshold, "Low", "High")
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

# plot
# png("survival_low_risk_noMSS.png", height = 10, width = 10, units = "in", res = 300)
# font_size <- 18
# p_low <- ggsurvplot(fit_low,
#                     data = low_dat,
#                     surv.median.line = "hv",
#                     risk.table = TRUE,
#                     tables.col = "strata",
#                     tables.y.text = FALSE,
#                     conf.int = TRUE,
#                     xlim = c(0, 12),
#                     xlab = "Time (years)",
#                     ylab = "Survival Probability (%)",
#                     legend = "none",
#                     tables.height = 0.2,
#                     break.time.by = 1,
#                     risk.table.fontsize = 5,
#                     palette = pal_lancet()(2),
#                     tables.theme = theme_cleantable() +
#                       theme(plot.title = element_text(size = font_size))
# )
# p_low$plot <- p_low$plot +
#   scale_y_continuous(labels = function(x) x * 100) +
#   theme(
#     axis.title = element_text(size = font_size),
#     axis.text = element_text(size = font_size),
#     legend.text = element_text(size = font_size - 2)
#   )
# print(p_low)
# dev.off()

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
png("survival_high_risk_noMSS.png", height = 10, width = 10, units = "in", res = 300)
font_size <- 18
p_high <- ggsurvplot(fit_high,
                     data = high_dat,
                     surv.median.line = "hv",
                     risk.table = TRUE,
                     tables.col = "strata",
                     tables.y.text = FALSE,
                     conf.int = TRUE,
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

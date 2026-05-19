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
library(ggplot2) 
library(survival)
library(tibble)
library(cmprsk)
library(gmodels)
library(numDeriv)
library(MASS)
library(htmltools)
library(pROC)
library(car)
library(gt)
library(ggsci)
library(dplyr)
library(survAUC)
library(timeROC)

select <- dplyr::select

# -- paths -- #
DATA_PATH <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Data/WM final_260517.xlsx'
OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Results'

# -- functions -- #
source('/Users/chaehyun/Documents/GitHub/MM_registry/Function_Lpl.r')

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
    IgM_1 = `IgM...26`,
    IgM_2 = `IgM...27`,
    B2MG_cont = `B2MG...42`,
    B2MG_cat = `B2MG...43`,
    `1L_2` = `1L...67`
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
  select(c("TLT12", "1L_1", "1L_start", "age", "age65", "sex", "ECOG", "PS", "B_Sx", "LNE", 
  "HS", "spleen", "liver", "IgM7", "ANC", "Hb", "Hb10", "Hb11", "PLT", "PLT100", "LDH", "LDH2", 
  "ALB", "ALB3.5", "B2MG_cont", "B2MG_cat", "IPSS", "RIPSS", "MSS", "MYD88", "CXCR4", "sPEP",
  "진단일","last_fu", "death", "SUBG_BR_OTHER", "ASCT")) %>%
  mutate(
    age = as.numeric(age),
    ANC = as.numeric(ANC),
    Hb = as.numeric(Hb),
    PLT = as.numeric(PLT),
    LDH = as.numeric(LDH),
    ALB = as.numeric(ALB),
    B2MG_cont = as.numeric(B2MG_cont),
    sPEP = as.numeric(sPEP),
    진단일 = as.Date(진단일, "%Y-%m-%d"),
    `1L_start` = as.Date(suppressWarnings(as.numeric(`1L_start`)), origin = "1899-12-30"),
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
  `age75` = case_when(
    age <= 65 ~ "<=65",
    age >= 66 & age <= 75 ~ "66-75",
    age > 75 ~ ">75",
    TRUE ~ NA_character_
  ),
  `B2MG4` = case_when(
    B2MG_cont >= 4 ~ 1,
    B2MG_cont < 4 ~ 0,
    TRUE ~ NA_integer_
  ),
  `Hb11.5` = case_when(
    Hb <= 11.5 ~ 1,
    Hb > 11.5 ~ 0,
    TRUE ~ NA_integer_
  ),
  `LDH250` = case_when(
    LDH > 250 ~ 1,
    LDH <= 250 ~ 0,
    TRUE ~ NA_integer_
  ),
  `ECOG performance status < 2` = case_when(
    PS == "low" ~ 1,
    PS == "high" ~ 0,
    TRUE ~ NA_integer_
  )) %>%
  mutate(`1L_1` = factor(`1L_1`, levels = c("BR", "R_Cy or R_borte", "Others"))) %>%
  mutate(`age75` = factor(`age75`, levels = c("<=65", "66-75", ">75"))) %>%
  mutate(
    Hb10 = ifelse(is.na(Hb), NA, Hb10),
    Hb11 = ifelse(is.na(Hb), NA, Hb11),
    `Hb11.5` = ifelse(is.na(Hb), NA, `Hb11.5`),
    PLT100 = ifelse(is.na(PLT), NA, PLT100),
    LDH2 = ifelse(is.na(LDH), NA, LDH2),
    LDH250 = ifelse(is.na(LDH), NA, LDH250),
    `ALB3.5` = ifelse(is.na(ALB), NA, `ALB3.5`),
    B2MG_cat = ifelse(is.na(B2MG_cont), NA, B2MG_cat),
    B2MG4 = ifelse(is.na(B2MG_cont), NA, B2MG4),
  ) %>%
  mutate(
    death   = as.numeric(death),
    death_day = as.numeric(last_fu - `1L_start`),
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


OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Baseline'
# -- Baseline characteristics table -- #
crf %>%
  select(c("age", "age65", "age75", "sex", "ECOG performance status < 2", "B_Sx",     
  "LNE", "HS", "spleen", "liver", "IgM7", "sPEP", "ANC < 1000", "Hb", "Hb11.5", "PLT", "PLT100", "LDH",
  "LDH250", "ALB", "ALB3.5", "B2MG_cont", "B2MG_cat", "B2MG4", "IPSS", "RIPSS", "MSS", "MYD88", "CXCR4")) %>% 
  tbl_summary(
    label = list(
      age ~ "Age (years)",
      age65 ~ "Age > 65",
      age75 ~ "Age > 75",
      sex ~ "Sex",
      `ECOG performance status < 2` ~ "ECOG performance status < 2",
      LNE ~ "Lymphadenopathy",
      HS ~ "Hepatosplenomegaly",
      Hb ~ "Hemoglobin (g/dL)",
      Hb11.5 ~ "Hemoglobin <= 11.5 g/dL",
      `ANC < 1000` ~ "ANC < 1000",
      PLT ~ "Platelet",
      PLT100 ~ "Platelet <= 100 x10^9/L",
      ALB ~ "Albumin (g/dL)",
      ALB3.5 ~ "Albumin < 3.5 g/dL",
      LDH ~ "LDH (IU/L)",
      LDH250 ~ "LDH > 250 IU/L",
      IgM7 ~ "IgM > 7.0 g/dL",
      B2MG_cont ~ "Beta-2 microglobulin (mg/L)",
      B2MG_cat ~ "Beta-2 microglobulin > 3 mg/L",
      B2MG4 ~ "Beta-2 microglobulin >= 4 mg/L",
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
  # add_overall() %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table. Baseline Characteristics (N = {N})**") %>%
  as_flex_table() 
  # %>%
  # flextable::save_as_docx(path = file.path(OUTPUT_DIR, "[26-05-12] Baseline.docx"))


# --- 2-1. 분석용 데이터 준비 ---
dat <- crf %>%
  select(
    c("1L_1", "TLT12", "age65", "age75", "sex", `ECOG performance status < 2`, "B_Sx", 
    "LNE", "HS", "IgM7", "ANC < 1000", "PLT100", "LDH250",
    "ALB3.5", "B2MG_cat", "B2MG4", "Hb11.5", "MYD88", "CXCR4", 
    "IPSS", "RIPSS", "MSS", 
    "진단일","last_fu", "death","death_day","death_yr", "SUBG_BR_OTHER", "ASCT")) %>%
  mutate(
    sex=factor(sex,levels=c("M","F")),
    `ECOG performance status < 2`=factor(`ECOG performance status < 2`,levels=c(0,1)),
    LNE=factor(LNE,levels=c(0,1)),
    HS=factor(HS,levels=c(0,1)),
    Hb11.5=factor(Hb11.5,levels=c(0,1)),
    PLT100=factor(PLT100,levels=c(0,1)),
    ALB3.5=factor(ALB3.5,levels=c(0,1)),
    LDH250=factor(LDH250,levels=c(0,1)),
    IgM7=factor(IgM7,levels=c(0,1)),
    B2MG_cat=factor(B2MG_cat,levels=c(0,1)),
    B2MG4=factor(B2MG4,levels=c(0,1)),
    B_Sx=factor(B_Sx,levels=c(0,1)),
    `ANC < 1000` = factor(`ANC < 1000`, levels = c(0, 1))
  )     
table(dat$SUBG_BR_OTHER)
table(dat$ASCT)

# -- Score 계산 -- # 
dat <- dat %>% rename(first_regimen = `1L_1`) %>%
  mutate(
    Score_IPSS = 
      case_when(age65 == 1 ~ 1, 
                age65 == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(`Hb11.5` == 1 ~ 1, 
                `Hb11.5` == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(PLT100 == 1 ~ 1, 
                PLT100 == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(B2MG_cat == 1 ~ 1, 
                B2MG_cat == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(IgM7 == 1 ~ 1, 
                IgM7 == 0 ~ 0, 
                TRUE ~ NA_real_),
    Score_RIPSS = 
      case_when(age75 == '<=65' ~ 0, 
                age75 == '66-75' ~ 1, 
                age75 == '>75' ~ 2, 
                TRUE ~ NA_real_) +
      case_when(B2MG4 == 1 ~ 1, 
                B2MG4 == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(LDH250 == 1 ~ 1, 
                LDH250 == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(`ALB3.5` == 1 ~ 1, 
                `ALB3.5` == 0 ~ 0, 
                TRUE ~ NA_real_)
)
score_list <- list(
  IPSS  = "Score_IPSS",
  RIPSS = "Score_RIPSS"
)

# median FU에서 time-dependent ROC
# roc_res <- plot_timeROC_medianFU(score_list, list_name = "Scores")

# Integrated AUC (1-8년)
compute_iAUC(score_list, list_name = "Scores", iauc_times = 1:8)

base_vars <- c("Score_RIPSS", "sex", "ECOG performance status < 2", "B_Sx", "LNE", "HS", "ANC < 1000", "Hb11.5", "PLT100", "IgM7")
multi_step("None", dat, base_vars) 


# -- Uni results (Cox PH) -- #
uni_list <- lapply(base_vars, function(var){
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




dat <- dat %>% 
  mutate(
    `Score_RIPSS_Others` = Score_RIPSS +
      case_when(B_Sx == 1 ~ 2, 
                B_Sx == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(`ANC < 1000` == 1 ~ 5, 
                `ANC < 1000` == 0 ~ 0, 
                TRUE ~ NA_real_)
)

# 3) IPSS 인자 + RIPSS 인자 모델
model_list3 <- list(
  "IPSS score"        = c("Score_IPSS"),
  "RIPSS score"        = c("Score_RIPSS"),
  "RIPSS-augmented model"        = c("Score_RIPSS", "B_Sx", "ANC < 1000"),
  "RIPSS-augmented score"        = c("Score_RIPSS_Others")
)

# plot_timeROC_medianFU(model_list3, list_name = "Scores with RIPSS-augmented")

compute_iAUC(model_list3, list_name = "Scores with RIPSS-augmented", iauc_times = 1:8)

# -- Score별 event rate table -- #
score_summary_table(dat, "Score_IPSS", "death")
score_summary_table(dat, "Score_RIPSS", "death")
score_summary_table(dat, "Score_RIPSS_Others", "death")

# -- Supplementary Table: model list1 ~ model list3 model results -- #
model_list1 <- list(
  "age65 + B2MG3"        = c("age65", "Hb11.5", "PLT100", "B2MG_cat", "IgM7", "LDH250", "ALB3.5"),
  "age65 + B2MG4"           = c("age65", "Hb11.5", "PLT100", "B2MG4",    "IgM7", "LDH250", "ALB3.5"),
  "age75 + B2MG3"        = c("age75", "Hb11.5", "PLT100", "B2MG_cat", "IgM7", "LDH250", "ALB3.5"),
  "age75 + B2MG4"           = c("age75", "Hb11.5", "PLT100", "B2MG4",    "IgM7", "LDH250", "ALB3.5"))

model_list2 <- list(
  "age65 + B2MG3 + B_Sx" = c("age65", "Hb11.5", "PLT100", "B2MG_cat", "IgM7", "LDH250", "ALB3.5", "B_Sx"),
  "age65 + B2MG4 + B_Sx"    = c("age65", "Hb11.5", "PLT100", "B2MG4",    "IgM7", "LDH250", "ALB3.5", "B_Sx"),
  "age75 + B2MG3 + B_Sx" = c("age75", "Hb11.5", "PLT100", "B2MG_cat", "IgM7", "LDH250", "ALB3.5", "B_Sx"),
  "age75 + B2MG4 + B_Sx"    = c("age75", "Hb11.5", "PLT100", "B2MG4",    "IgM7", "LDH250", "ALB3.5", "B_Sx")
)

model_list3 <- list(
  "age65 + B2MG3 + others"        = c("sex", "ECOG performance status < 2", "B_Sx", "LNE", "HS", "ANC < 1000", "age65", "Hb11.5", "PLT100", "B2MG_cat", "IgM7", "LDH250", "ALB3.5"),
  "age65 + B2MG4 + others"           = c("sex", "ECOG performance status < 2", "B_Sx", "LNE", "HS", "ANC < 1000", "age65", "Hb11.5", "PLT100", "B2MG4",    "IgM7", "LDH250", "ALB3.5"),
  "age75 + B2MG3 + others"        = c("sex", "ECOG performance status < 2", "B_Sx", "LNE", "HS", "ANC < 1000", "age75", "Hb11.5", "PLT100", "B2MG_cat", "IgM7", "LDH250", "ALB3.5"),
  "age75 + B2MG4 + others"           = c("sex", "ECOG performance status < 2", "B_Sx", "LNE", "HS", "ANC < 1000", "age75", "Hb11.5", "PLT100", "B2MG4",    "IgM7", "LDH250", "ALB3.5")
)




for (i in seq_along(model_list1)) {
   multi_step("None", dat, model_list1[[i]])
}
for (i in seq_along(model_list2)) {
  multi_step("None", dat, model_list2[[i]])
}
for (i in seq_along(model_list3)) {
  multi_step("None", dat, model_list3[[i]])
}


# -- Supplementary: model list1 ~ model list3 roc curve (step-wise selection 된 걸로) -- #

model_list1 <- list(
  IPSS  = "Score_IPSS",
  RIPSS = "Score_RIPSS",
  "age65 + B2MG3"        = c("ALB3.5"),
  "age65 + B2MG4"           = c("ALB3.5"),
  "age75 + B2MG3"        = c("ALB3.5"),
  "age75 + B2MG4"           = c("ALB3.5"))

model_list2 <- list(
  IPSS  = "Score_IPSS",
  RIPSS = "Score_RIPSS",
  "age65 + B2MG3 + B_Sx" = c("ALB3.5", "B_Sx"),
  "age65 + B2MG4 + B_Sx"    = c("ALB3.5", "B_Sx"),
  "age75 + B2MG3 + B_Sx" = c("ALB3.5", "B_Sx"),
  "age75 + B2MG4 + B_Sx"    = c("ALB3.5", "B_Sx")
)


model_list3 <- list(
  IPSS  = "Score_IPSS",
  RIPSS = "Score_RIPSS",
  "age65 + B2MG3 + others"        = c("B_Sx", "ANC < 1000", "LDH250"),
  "age65 + B2MG4 + others"           = c("B_Sx", "ANC < 1000", "LDH250"),
  "age75 + B2MG3 + others"        = c("B_Sx", "ANC < 1000", "age75", "LDH250"),
  "age75 + B2MG4 + others"           = c("B_Sx", "ANC < 1000", "age75", "LDH250")
)

# plot_timeROC_medianFU(model_list1)
# plot_timeROC_medianFU(model_list2)
# plot_timeROC_medianFU(model_list3)

compute_iAUC(model_list1)
compute_iAUC(model_list2)
compute_iAUC(model_list3)



# -- survival curve 그리기 by score group -- #
survival <- dat  %>% 
  mutate(
    group_IPSS = 
      case_when(
        Score_IPSS >= 3                ~ "High",
        Score_IPSS == 2                ~ "Intermediate",
        Score_IPSS <= 1 & age65 == 1   ~ "Intermediate",
        Score_IPSS <= 1 & age65 == 0   ~ "Low",
        T ~ "High"),
    group_RIPSS = 
      case_when(
        Score_RIPSS<=1 ~"Low",
        Score_RIPSS<=2 ~ "Intermediate",
        T ~ "High"),
    group_RIPSS_Others = 
      case_when(
        Score_RIPSS_Others<=1 ~"Low",
        Score_RIPSS_Others<=5 ~ "Intermediate",
        T ~ "High")
  ) %>% 
  mutate(group_IPSS = factor(group_IPSS, levels=c("Low","Intermediate","High")),
         group_RIPSS = factor(group_RIPSS, levels=c("Low","Intermediate","High")),
         group_RIPSS_Others = factor(group_RIPSS_Others, levels=c("Low","Intermediate","High")) )

fit1 <- survfit(Surv(death_yr, death) ~ group_IPSS, data = survival)
fit2 <- survfit(Surv(death_yr, death) ~ group_RIPSS, data = survival)
fit3 <- survfit(Surv(death_yr, death) ~ group_RIPSS_Others, data = survival)

OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure'

# plot_survival_by_group(fit1, survival, OUTPUT_DIR, file_name = "/survival_by_group_IPSS.png", group_col = "group_IPSS")
# plot_survival_by_group(fit2, survival, OUTPUT_DIR, file_name = "/survival_by_group_RIPSS.png", group_col = "group_RIPSS")
# plot_survival_by_group(fit3, survival, OUTPUT_DIR, file_name = "/survival_by_group_RIPSS_Others.png", group_col = "group_RIPSS_Others")

km_summary_by_group(survival, "group_IPSS", "death_yr", "death")
km_summary_by_group(survival, "group_RIPSS", "death_yr", "death")
km_summary_by_group(survival, "group_RIPSS_Others", "death_yr", "death")
cox_hr_by_group(survival, "group_IPSS", "death_yr", "death")
cox_hr_by_group(survival, "group_RIPSS", "death_yr", "death")
cox_hr_by_group(survival, "group_RIPSS_Others", "death_yr", "death")

# -- time-dependent ROC -- #
model_sets <- list(
  "IPSS score"        = c("Score_IPSS"),
  "RIPSS score"        = c("Score_RIPSS"),
  "RIPSS-augmented score"        = c("Score_RIPSS_Others")
)

OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure/RIPSS-augmented ROC'
# plot_timeROC_overlay(model_sets, AUC_TIMES = c(2,4,6,8), PLOT_TIMES = c(2,4,6,8), OUTPUT_DIR = OUTPUT_DIR)



# -- Optimal cutoff (Youden index) -- #
lancet_cols <- pal_lancet()(2)
OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure/'
plot_score_histogram_with_cutoff(survival, "Score_IPSS", OUTPUT_DIR, lancet_cols)
plot_score_histogram_with_cutoff(survival, "Score_RIPSS", OUTPUT_DIR, lancet_cols)
plot_score_histogram_with_cutoff(survival, "Score_RIPSS_Others", OUTPUT_DIR, lancet_cols)

# -- first-regimen 분포 -- #

fit <- survfit(Surv(death_yr, death) ~ first_regimen, data = survival)
OUTPUT_DIR="/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure/"
plot_survival_by_group(fit, survival, OUTPUT_DIR, file_name = "/survival_by_1L_1.png", group_col = "first_regimen")

km_summary_by_group(survival, "first_regimen", "death_yr", "death")
cox_hr_by_group(survival, "first_regimen", "death_yr", "death")

regimen_dist <- survival %>%
  count(first_regimen, name = "N", .drop = FALSE) %>%
  mutate(
    Percent = sprintf("%.1f%%", N / sum(N) * 100)
  )

regimen_dist %>%
  gt() 
  
# Bar chart
# png(file.path(OUTPUT_DIR, "regimen_distribution.png"),
#     height = 6, width = 8, units = "in", res = 300)
# ggplot(survival, aes(x = first_regimen, fill = first_regimen)) +
#   geom_bar(width = 0.7) +
#   geom_text(stat = "count", aes(label = after_stat(count)),
#             vjust = -0.4, size = 5) +
#   scale_fill_lancet() +
#   labs(title = "1st-line regimen distribution",
#        x = NULL, y = "N") +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none",
#         plot.title = element_text(face = "bold"))
# dev.off()



regimens <- survival %>% filter(!is.na(first_regimen)) %>%
  pull(first_regimen) %>% unique() %>% sort()

group_name <- c("IPSS group", "RIPSS group", "RIPSS-augmented group")
for (group_var in c("group_IPSS", "group_RIPSS", "group_RIPSS_Others")) {
   cat(sprintf("\n=== Grouping by %s ===\n", group_var))
   
  # 그림 (2x2 합쳐서 저장)
  plot_list <- lapply(regimens, function(r){
    plot_regimen(survival %>% filter(first_regimen == r), as.character(r), group_var = group_var)
  })

  OUTPUT_DIR="/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure/"
  dev.new()
  png(paste0(OUTPUT_DIR, "survival_by_regimen_2x1_", group_name[which(c("group_IPSS", "group_RIPSS", "group_RIPSS_Others") == group_var)], ".png"),
      width = 16, height = 16, units = "in", res = 300)
  arrange_ggsurvplots(plot_list, ncol = 2, nrow = 2, print = TRUE)
  dev.off()
}

# 통계 테이블 (regimen별로 gt 두 개씩)
for (group_var in c("group_IPSS", "group_RIPSS", "group_RIPSS_Others")) {
   cat(sprintf("\n=== Grouping by %s ===\n", group_var))
   
   for (r in regimens){
     res <- summary_regimen(survival %>% filter(first_regimen == r), as.character(r), group_var = group_var)
     cat(sprintf("\n--- Regimen: %s ---\n", r))
     print(res$surv)
     print(res$hr)
   }
} 

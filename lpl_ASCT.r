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


survival <- dat %>% filter(death_yr >= 0.5)
table(survival$ASCT)
fit1 <- survfit(Surv(death_yr, death) ~ ASCT, data = survival)

OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure'

# plot_survival_by_group(fit1, survival, OUTPUT_DIR, file_name = "/survival_by_group_ASCT.png", group_col = "ASCT")

km_summary_by_group(survival, "ASCT", "death_yr", "death")
cox_hr_by_group(survival, "ASCT", "death_yr", "death")



# 혹시 모르니 아래 코드 확인하면서 진행하기
####! Matching ---------------------------------------------------------------
dat_complete <- dat %>% filter(!is.na(ASCT), !is.na(RIPSS))

theme_gtsummary_compact()
dat_complete %>% 
  select(ASCT, RIPSS) %>%
  tbl_summary(ASCT,
            statistic = list(all_continuous() ~ "{mean} ({sd})",
                             all_categorical() ~ "{n} ({p}%)")) %>%
  add_p() |>
  gtsummary::as_gt() |>
  tab_header(
    title = sprintf("Unmatched cohort (N = %d/%d)", nrow(dat_complete), nrow(dat))
  )


psm = matchit(ASCT~ RIPSS,
                data=dat_complete, method = "nearest", distance = "logit", replace = FALSE, caliper = 0.2, ratio =3)

bal.tab(psm, m.threshold = 0.1, v.threshold = 2, un = TRUE) |> gt()

matched_data = match.data(psm)

matched_data %>% 
  select(ASCT, RIPSS) %>%
  tbl_summary(ASCT,
            statistic = list(all_continuous() ~ "{mean} ({sd})",
                             all_categorical() ~ "{n} ({p}%)")) %>%
  add_p() |>
  gtsummary::as_gt() |>
  tab_header(
    title = sprintf("Matched cohort")
  )

survival <- matched_data %>% filter(death_yr >= 0.5)
table(survival$ASCT)
fit1 <- survfit(Surv(death_yr, death) ~ ASCT, data = survival)

OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure'

plot_survival_by_group(fit1, survival, OUTPUT_DIR, file_name = "/matched_survival_by_group_ASCT.png", group_col = "ASCT")

km_summary_by_group(survival, "ASCT", "death_yr", "death")
cox_hr_by_group(survival, "ASCT", "death_yr", "death")

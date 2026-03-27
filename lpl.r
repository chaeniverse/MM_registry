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
DATA_PATH <- '/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Data/WM final.xlsx'
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
crf <- crf %>%
  select(-c("No","센터","image","WBC", "1L_2")) %>%
  mutate(
    age = as.numeric(age),
    ECOG = factor(ECOG),
    PS = factor(PS, levels = c("high","low")),
    IgM_2 = as.numeric(IgM_2),
    sPEP = as.numeric(sPEP),
    ANC = as.numeric(ANC),
    Hb = as.numeric(Hb),
    PLT = as.numeric(PLT),
    LDH = as.numeric(LDH),
    ALB = as.numeric(ALB),
    TLT = as.numeric(TLT),
    B2MG_cont = as.numeric(B2MG_cont),
    DOB = as.Date(DOB, "%Y-%m-%d"),
    진단일 = as.Date(진단일, "%Y-%m-%d"),
    BM_date = as.Date(ifelse(BM_date == "Not done", NA, BM_date), "%Y-%m-%d"),
    last_fu = as.Date(last_fu, "%Y-%m-%d")
  ) %>%
  mutate(ANC = case_when(
    ANC > 1000 ~ "> 1000",
    ANC <= 1000 ~ "<= 1000",
    TRUE ~ NA_character_
  ),
  `1L_1` = case_when(
    `1L_1` == '1_BR' ~ "BR",
    `1L_1` == '2_R_Cy' | `1L_1` == '3_R_borte' ~ "R_Cy or R_borte",
    `1L_1` == '4_others' ~ "Others",
    TRUE ~ NA_character_
  )) %>%
  mutate(ANC = factor(ANC, levels = c("<= 1000", "> 1000")),
  `1L_1` = factor(`1L_1`, levels = c("BR", "R_Cy or R_borte", "Others"))) %>%
  mutate(IgM_1 = as.numeric(gsub(">|\\s", "", IgM_1))) %>%
  mutate(
    IgM4 = ifelse(is.na(IgM_2), NA, IgM4),
    IgM7 = ifelse(is.na(IgM_2), NA, IgM7),
    Hb10 = ifelse(is.na(Hb), NA, Hb10),
    Hb11 = ifelse(is.na(Hb), NA, Hb11),
    PLT100 = ifelse(is.na(PLT), NA, PLT100),
    LDH2 = ifelse(is.na(LDH), NA, LDH2),
    `ALB3.5` = ifelse(is.na(ALB), NA, `ALB3.5`),
    B2MG_cat = ifelse(is.na(B2MG_cont), NA, B2MG_cat)
  )
# View(crf)
crf$TLT12
# -- 전처리 -- #
# 연속형 NA -> 범주형 NA로 변환 (예: Hb, PLT, ALB, LDH, IgM_2, B2MG_cont, sPEP, WBC, ANC)

whole <- crf %>% 
  select(-c("TLT24","TNT24","TLT18","TNT18","DOB","진단일","BM_date", "last_fu", "death"))

priority_cols <- c(
  "age", "age65", "sex", "ECOG", "PS", "LNE", "HS", 
  "Hb", "Hb10", "Hb11", "PLT", "PLT100", "ALB", "ALB3.5", "LDH", "LDH2",
  "IgM_1", "IgM_2", "IgM4", "IgM7", "B2MG_cont", "B2MG_cat", "IPSS", "RIPSS", "MSS", "MYD88", "CXCR4",
  "spleen", "liver", "B_Sx", "sPEP", "ANC"
)


whole %>%
  select(-c("TLT12", "TNT12")) %>% 
  select(all_of(priority_cols), everything()) %>% 
  # select(all_of(priority_cols)) %>% # 필요한 것만 선택
  tbl_summary(
    label = list(
      age ~ "Age (years)",
      age65 ~ "Age > 65",
      sex ~ "Sex",
      PS ~ "ECOG performance status < 2",
      LNE ~ "Lymphadenopathy",
      HS ~ "Hepatosplenomegaly",
      Hb ~ "Hemoglobin (g/dL)",
      Hb10 ~ "Hemoglobin < 10 g/dL",
      Hb11 ~ "Hemoglobin < 11 g/dL",
      PLT ~ "Platelet (x10^9/uL)",
      PLT100 ~ "Platelet < 100 x10^9/uL",
      ALB ~ "Albumin (g/dL)",
      ALB3.5 ~ "Albumin < 3.5 g/dL",
      LDH ~ "LDH (IU/L)",
      LDH2 ~ "LDH > upper limit of normal",
      IgM_2 ~ "IgM (mg/dL)",
      IgM4 ~ "IgM > 4000 mg/dL",
      IgM7 ~ "IgM > 7000 mg/dL",
      B2MG_cont ~ "Beta-2 microglobulin (mg/L)",
      B2MG_cat ~ "Beta-2 microglobulin > 3 mg/L",
      IPSS ~ "IPSS-WM",
      RIPSS ~ "rIPSS-WM",
      MSS ~ "MSS-WM",
      MYD88 ~ "MYD88 mutation",
      CXCR4 ~ "CXCR4 mutation",
      B_Sx ~ "B symptoms",
      sPEP ~ "Serum protein electrophoresis (g/dL)",
      ANC ~ "ANC (/uL)"
    ),
    type = list(
      all_continuous() ~ "continuous2"  # 새로 추가: mean+median 두 줄 표시
    ),
    statistic = list(                   
      all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})"),
      all_categorical() ~ "{n} ({p})%"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing",
    missing_stat = "{N_miss} ({p_miss}%)"
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 1. Baseline Characteristics (CRF, N = {N})**") %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = file.path(OUTPUT_DIR, "[26-03-23] Baseline_CRF.docx"))



make_baseline_table <- function(data, group, exclude_cols, name) {
  data %>%
    filter(!is.na(.data[[group]]) & .data[[group]] != "NA") %>%
    select(c('TLT12','TNT12','TLT','1L_1'),priority_cols) %>%
    select(-all_of(exclude_cols)) %>%
  tbl_summary(
    by = all_of(group),
    label = list(
      age ~ "Age (years)",
      age65 ~ "Age > 65",
      sex ~ "Sex",
      PS ~ "ECOG performance status < 2",
      LNE ~ "Lymphadenopathy",
      HS ~ "Hepatosplenomegaly",
      Hb ~ "Hemoglobin (g/dL)",
      Hb10 ~ "Hemoglobin < 10 g/dL",
      Hb11 ~ "Hemoglobin < 11 g/dL",
      PLT ~ "Platelet (x10^9/uL)",
      PLT100 ~ "Platelet < 100 x10^9/uL",
      ALB ~ "Albumin (g/dL)",
      ALB3.5 ~ "Albumin < 3.5 g/dL",
      LDH ~ "LDH (IU/L)",
      LDH2 ~ "LDH > upper limit of normal",
      IgM_2 ~ "IgM (mg/dL)",
      IgM4 ~ "IgM > 4000 mg/dL",
      IgM7 ~ "IgM > 7000 mg/dL",
      B2MG_cont ~ "Beta-2 microglobulin (mg/L)",
      B2MG_cat ~ "Beta-2 microglobulin > 3 mg/L",
      IPSS ~ "IPSS-WM",
      RIPSS ~ "rIPSS-WM",
      MSS ~ "MSS-WM",
      MYD88 ~ "MYD88 mutation",
      CXCR4 ~ "CXCR4 mutation",
      B_Sx ~ "B symptoms",
      sPEP ~ "Serum protein electrophoresis (g/dL)",
      ANC ~ "ANC (/uL)"
    ),
      type = list(
        all_continuous() ~ "continuous2"
      ),
      statistic = list(
        all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})"),
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
      missing = "ifany",
      missing_text = "Missing",
      missing_stat = "{N_miss} ({p_miss}%)"
    ) %>%
    add_p() %>%
    bold_labels() %>%
    modify_header(label = "**Characteristic**") %>%
    modify_caption(paste0("**Table 1. Baseline Characteristics (", name, ", N = {N})**")) %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = file.path(OUTPUT_DIR, paste0("[26-03-23] Baseline (", name, ").docx")))
}
# TLT12
make_baseline_table(whole, group="TLT12", c("TLT","TNT12",'1L_1'), "TLT12")
# TNT12
make_baseline_table(whole, group="TNT12", c("TLT12"), "TNT12") 

View(dat)
# -- 전처리 -- #
dat <- crf %>%
  select(c("TLT12","진단일","last_fu", "death",
  "age65","sex","PS","LNE","HS","Hb10","PLT100","ALB3.5","LDH2",
           "IgM4","B2MG_cat","IPSS","RIPSS","MSS","MYD88","CXCR4",
           "spleen", "liver", "B_Sx", "sPEP", "ANC")) %>%
  mutate(
        TLT12 = factor(TLT12, levels=c(0,1)),
        death=as.numeric(death),
        age65=factor(age65,levels=c(0,1)),
        sex=factor(sex,levels=c("M","F")),
        PS=factor(PS,levels=c("high","low")),
        LNE=factor(LNE,levels=c(0,1)),
        HS=factor(HS,levels=c(0,1)),
        Hb10=factor(Hb10,levels=c(0,1)),
        PLT100=factor(PLT100,levels=c(0,1)),
        ALB3.5=factor(ALB3.5,levels=c(0,1)),
        LDH2=factor(LDH2,levels=c(0,1)),
        IgM4=factor(IgM4,levels=c(0,1)),
        B2MG_cat=factor(B2MG_cat,levels=c(0,1)),
        IPSS=factor(IPSS,levels=c("1_Low","2_Int","3_High","4_UK")), #
        RIPSS=factor(RIPSS,levels=c("1_VL","2_Low","3_Int","4_High","5_VH","6_UK")), #
        MSS=factor(MSS,levels=c("1_Low","2_low_Int","3_Int","4_High","5_UK")), #
        MYD88 = factor(ifelse(MYD88 == "NA", NA, MYD88)),
        CXCR4 = factor(ifelse(CXCR4 == "NA", NA, CXCR4)),
        spleen=factor(spleen,levels=c(0,1)),
        liver=factor(liver,levels=c(0,1)),
        B_Sx=factor(B_Sx,levels=c(0,1)),
        sPEP=as.numeric(sPEP),
        ANC=factor(ANC,levels=c("<= 1000","> 1000"))) %>% 
  mutate(death_day = 
  as.numeric(last_fu - 진단일)
  ) %>%
  mutate(death_yr=death_day/365.25)
table(dat$TLT12)


# landmark
dat_land <- dat[dat$death_yr >= 1,]
fit <- survfit(Surv(death_yr, death) ~ TLT12, data=dat_land)


# -- Uni and multi results -- #
# Uni #
covariates <- setdiff(names(dat_land), c("death_yr", "death","last_fu", "진단일","death_day"))

uni_list <- lapply(covariates, function(var){
  model <- coxph(as.formula(paste("Surv(death_yr, death) ~", var)), data = dat_land)
  s <- summary(model)
  coefs <- as.data.frame(s$coefficients)
  
  data.frame(
    term = rownames(coefs),
    uni_HR = coefs[["exp(coef)"]],
    uni_LCL = as.data.frame(s$conf.int)[["lower .95"]],
    uni_UCL = as.data.frame(s$conf.int)[["upper .95"]],
    uni_p = coefs[["Pr(>|z|)"]],
    stringsAsFactors = FALSE
  )
})
uni_df <- bind_rows(uni_list)
uni_df %>%
  mutate(
    `Univariable HR (95% CI)` = sprintf("%.2f (%.2f\u2013%.2f)", uni_HR, uni_LCL, uni_UCL),
    `Uni P` = ifelse(uni_p < 0.001, "<.001", sprintf("%.3f", uni_p))
  ) %>%
  select(term, `Univariable HR (95% CI)`, `Uni P`) %>%
  gt()


dat_0.1_IP <- dat_land %>% 
  select(c(death_yr, death, 
  TLT12, age65, ALB3.5, LDH2, 
  IPSS, MSS, 
  liver, B_Sx, sPEP, ANC))
dat_0.1_RIP <- dat_land %>% 
  select(c(death_yr, death, 
  TLT12, age65, ALB3.5, LDH2, 
  RIPSS, MSS, 
  liver, B_Sx, sPEP, ANC))


# -- multi results 0.1 cutoff -- #
# IPSS
full.model <- coxph(Surv(death_yr, death) ~ ., data = dat_0.1_IP)
s <- summary(full.model)
coefs <- as.data.frame(s$coefficients)

multi_0.1_IP <- data.frame(
  term = rownames(coefs),
  multi_HR = coefs[["exp(coef)"]],
  multi_LCL = as.data.frame(s$conf.int)[["lower .95"]],
  multi_UCL = as.data.frame(s$conf.int)[["upper .95"]],
  multi_p = coefs[["Pr(>|z|)"]],
  stringsAsFactors = FALSE
)
multi_0.1_IP %>%
  mutate(
    `Multivariable HR (95% CI)` = sprintf("%.2f (%.2f\u2013%.2f)", multi_HR, multi_LCL, multi_UCL),
    `Multi P` = ifelse(multi_p < 0.001, "<.001", sprintf("%.3f", multi_p))
  ) %>%
  select(term, `Multivariable HR (95% CI)`, `Multi P`) %>%
  gt()


# RIPSS
full.model <- coxph(Surv(death_yr, death) ~ ., data = dat_0.1_RIP)
s <- summary(full.model)
coefs <- as.data.frame(s$coefficients)

multi_0.1_RIP <- data.frame(
  term = rownames(coefs),
  multi_HR = coefs[["exp(coef)"]],
  multi_LCL = as.data.frame(s$conf.int)[["lower .95"]],
  multi_UCL = as.data.frame(s$conf.int)[["upper .95"]],
  multi_p = coefs[["Pr(>|z|)"]],
  stringsAsFactors = FALSE
)
multi_0.1_RIP %>%
  mutate(
    `Multivariable HR (95% CI)` = sprintf("%.2f (%.2f\u2013%.2f)", multi_HR, multi_LCL, multi_UCL),
    `Multi P` = ifelse(multi_p < 0.001, "<.001", sprintf("%.3f", multi_p))
  ) %>%
  select(term, `Multivariable HR (95% CI)`, `Multi P`) %>%
  gt()

# -- multi uni end -- #



# reverse kaplan-meier curve
reverse_dat <- copy(dat_land)
reverse_dat$death = ifelse(reverse_dat$death == 1, 0, 1)

sfit <- survfit(Surv(death_yr, death) ~ TLT12, data=reverse_dat)

sprintf("%.1f (%.1f, %.1f)", surv_median(sfit)[1,2], surv_median(sfit)[1,3], surv_median(sfit)[1,4])
sprintf("%.1f (%.1f, %.1f)", surv_median(sfit)[2,2], surv_median(sfit)[2,3], surv_median(sfit)[2,4])

# median
sprintf("%.1f (%.1f, %.1f)", surv_median(fit)[1,2], surv_median(fit)[1,3], surv_median(fit)[1,4])
sprintf("%.1f (%.1f, %.1f)", surv_median(fit)[2,2], surv_median(fit)[2,3], surv_median(fit)[2,4])


# cox model
# 매칭 안 했으니까 아직은 안 써도 되지 않나.
cox_fit <- coxph(Surv(time=death_yr, event=death)~TLT12, data=dat_land)
table(dat_land$TLT12)
sum_cox_fit <- summary(cox_fit)
sum_cox_fit

# Survival probability
format_surv <- function(fit, time_point) {
  s <- summary(fit, times = time_point)
  data.frame(
    group = s$strata,
    result = sprintf("%.1f%% (%.1f–%.1f)", s$surv * 100, s$lower * 100, s$upper * 100)
  )
}

surv5 <- format_surv(fit, 5)
surv10 <- format_surv(fit, 10)
library(gt)
surv5 |> gt()
surv10 |> gt()


HR <- sprintf("%.2f (%.2f, %.2f)", 
              sum_cox_fit$conf.int[1,1], 
              sum_cox_fit$conf.int[1,3], 
              sum_cox_fit$conf.int[1,4])

p_val <- ifelse(sum_cox_fit$coefficients[1,5] < 0.001, 
                "p <.001", 
                paste0("p = ", sprintf("%.3f", sum_cox_fit$coefficients[1,5])))

print(HR)
print(p_val)

# -- plot -- #
dev.new()
pdf('C:/Users/chaehyun/Downloads/survival_plot.pdf',height=10,width=10)
font_size = 18

p <- ggsurvplot(fit,
                data = dat_land,
                surv.median.line = "hv",
                risk.table = TRUE,
                tables.col = "strata",
                tables.y.text = FALSE,
                conf.int = TRUE,
                xlim = c(0, 12),
                xlab = "Time (years)",
                ylab = "Survival Probability (%)",
                legend="none",
                tables.height = 0.2,
                break.time.by = 1,                          # 0.5 → 1 (x축 너무 빽빽함)
                risk.table.fontsize = 5,
                palette = pal_lancet()(2),                  # 그룹 2개니까 2색
                tables.theme = theme_cleantable() +
                  theme(plot.title = element_text(size = font_size))
)

p$plot <- p$plot +
  scale_y_continuous(labels = function(x) x * 100) +
  theme(
    axis.title.y = element_text(size = font_size),
    axis.text.y = element_text(size = font_size),
    axis.title.x = element_text(size = font_size),
    axis.text.x = element_text(size = font_size),
    legend.text = element_text(size = font_size - 2)        # 범례 폰트
  )

p
dev.off()



# -- iptw -- #
dat_land <- dat[dat$death_yr >= 1,]

my_weights = get_sw(TLT12 ~ age65 + ALB3.5 + LDH2 + RIPSS + MSS + liver + B_Sx + sPEP + ANC, data = dat_land)

cox_fit <- coxph(Surv(death_yr, death) ~ TLT12, data=dat_land, weights= my_weights$weight, robust=TRUE)
sum_cox_fit <- summary(cox_fit)
sum_cox_fit


# survival curve
fit <- survfit(Surv(time=death_yr, event=death)~TLT12, data=dat_land, weights = my_weights$weight)


# reverse kaplan-meier curve
reverse_dat <- copy(dat_land)
reverse_dat$death = ifelse(reverse_dat$death == 1, 0, 1)

sfit <- survfit(Surv(death_yr, death) ~ TLT12, data=reverse_dat, weights = my_weights$weight)
surv_median(sfit)

sprintf("%.1f (%.1f, %.1f)", surv_median(sfit)[1,2], surv_median(sfit)[1,3], surv_median(sfit)[1,4])
sprintf("%.1f (%.1f, %.1f)", surv_median(sfit)[2,2], surv_median(sfit)[2,3], surv_median(sfit)[2,4])

# median
sprintf("%.1f (%.1f, %.1f)", surv_median(fit)[1,2], surv_median(fit)[1,3], surv_median(fit)[1,4])
sprintf("%.1f (%.1f, %.1f)", surv_median(fit)[2,2], surv_median(fit)[2,3], surv_median(fit)[2,4])


format_surv <- function(fit, time_point) {
  s <- summary(fit, times = time_point)
  data.frame(
    group = s$strata,
    result = sprintf("%.1f%% (%.1f–%.1f)", s$surv * 100, s$lower * 100, s$upper * 100)
  )
}

surv5 <- format_surv(fit, 5)
surv10 <- format_surv(fit, 10)

surv5 |> gt()
surv10 |> gt()


HR <- sprintf("%.2f (%.2f, %.2f)", 
              sum_cox_fit$conf.int[1,1], 
              sum_cox_fit$conf.int[1,3], 
              sum_cox_fit$conf.int[1,4])

p_val <- ifelse(sum_cox_fit$coefficients[1,6] < 0.001, 
                "p <.001", 
                paste0("p = ", sprintf("%.3f", sum_cox_fit$coefficients[1,6])))

print(HR)
print(p_val)

# -- plot -- #
dev.new()
pdf('C:/Users/chaehyun/Downloads/survival_plot2.pdf',height=10,width=13)
font_size = 18

p <- ggsurvplot(fit,
                data = dat_land,
                surv.median.line = "hv",
                risk.table = FALSE,
                conf.int = FALSE,
                xlim = c(0, 12),
                xlab = "Time (years)",
                ylab = "Survival Probability (%)",
                legend="none",
                tables.height = 0.2,
                break.time.by = 1,                          # 0.5 → 1 (x축 너무 빽빽함)
                palette = pal_lancet()(2),                  # 그룹 2개니까 2색
                tables.theme = theme_cleantable() +
                  theme(plot.title = element_text(size = font_size))
)

p$plot <- p$plot +
  scale_y_continuous(labels = function(x) x * 100) +
  theme(
    axis.title.y = element_text(size = font_size),
    axis.text.y = element_text(size = font_size),
    axis.title.x = element_text(size = font_size),
    axis.text.x = element_text(size = font_size),
    legend.text = element_text(size = font_size - 2)        # 범례 폰트
  )

p
dev.off()




###### balance check ------

exp_form = group ~ ageg4 + SEX_TP_CD + CCI_score + Doublet + Low_intensity_triplet + High_intensity_triplet
exp_var = all.vars(exp_form)[1]

summary(my_weights$weight)
tab_smd_adj = svyCreateTableOne(vars = all.vars(exp_form)[-1], strata = exp_var,
                                data=svydesign(ids = ~1, data=scoring_MM_6mths_filtered, weights=my_weights$weight))

tab_smd_un = svyCreateTableOne(vars = all.vars(exp_form)[-1], strata = exp_var,
                               data=svydesign(ids = ~1, data=scoring_MM_6mths_filtered))
tab_smd_adj1 = ExtractSmd(tab_smd_adj) %>% as.data.frame
tab_smd_un1 = ExtractSmd(tab_smd_un) %>% as.data.frame

tab_smd_adj1$variable = rownames(tab_smd_adj1); rownames(tab_smd_adj1) = NULL
tab_smd_un1$variable = rownames(tab_smd_un1); rownames(tab_smd_un1) = NULL
tab_smd_adj1$type = "Adjusted"
tab_smd_un1$type = "Unadjusted"


library(ggsci)



col_val = NULL
col_lab = NULL
var_lab = c("High-intensity triplet", "Low-intensity triplet", "Doublet", "CCI score", "Sex", "Age group")

if(is.null(col_val)){col_val = pal_lancet()(3)[c(1,3,2)]}
if(is.null(col_lab)){col_lab = seq_len(3)-1}

colnames(tab_smd_adj1)[2:4] <- colnames(tab_smd_un1)[2:4] <-
  names(col_val) <- names(col_lab) <- c("SMD12", "SMD13", "SMD23")

tab_smd_res = rbind.data.frame(tab_smd_adj1[,-1], tab_smd_un1[,-1])
tab_smd_res$variable = factor(tab_smd_res$variable, levels = rev(unique(tab_smd_res$variable)),
                              labels = var_lab)

dev.new()
pdf("250630_smdplot.pdf",height=7,width=12)
tab_smd_res %>%
  gather(key = "key", value = "value", -variable, -type) %>%
  ggplot() +
  geom_point(aes(x = value, y = variable, shape = type, col = key),size=3) +
  
  scale_shape_manual(values = c("Adjusted" = 19, "Unadjusted" = 4), name = "") +
  scale_color_manual(values = col_val, labels = c("MGUS to MM",'SMM to MM', "De novo MM"), name = "") +
  geom_vline(aes(xintercept = 0.1), linetype = "dashed") +
  
  labs(x = "Standardized mean difference", y = "Covariates") +
  ggtitle("") +
  
  theme_classic() +
  
  theme(axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))
dev.off()





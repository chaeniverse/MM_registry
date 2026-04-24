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
library(gt)
library(ggsci)
library(dplyr)
library(survAUC)

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
  "HS", "IgM7", "ANC", "Hb", "Hb10", "Hb11", "PLT", "PLT100", "LDH", "LDH2", 
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
  # `1L_1` = case_when(
  #   `1L_1` == '1_BR' ~ "BR",
  #   `1L_1` == '2_R_Cy' | `1L_1` == '3_R_borte' ~ "R_Cy or R_borte",
  #   `1L_1` == '4_others' ~ "Others",
  #   TRUE ~ NA_character_
  # ),
  `ECOG performance status < 2` = case_when(
    PS == "low" ~ 1,
    PS == "high" ~ 0,
    TRUE ~ NA_integer_
  )) %>%
  # mutate(`1L_1` = factor(`1L_1`, levels = c("BR", "R_Cy or R_borte", "Others"))) %>%
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
    "LNE", "HS", "IgM7", "ANC < 1000", "PLT100", "LDH2", 
    "ALB3.5", "B2MG_cat", "IPSS", "RIPSS", 
    "MSS", "MYD88", "CXCR4", "진단일","last_fu", 
    "death","death_day","death_yr")) %>%
  mutate(
    sex=factor(sex,levels=c("M","F")),
    age65=factor(age65,levels=c(0,1)),
    `ECOG performance status < 2`=factor(`ECOG performance status < 2`,levels=c(0,1)),
    LNE=factor(LNE,levels=c(0,1)),
    HS=factor(HS,levels=c(0,1)),
    # Hb10=factor(Hb10,levels=c(0,1)),
    PLT100=factor(PLT100,levels=c(0,1)),
    ALB3.5=factor(ALB3.5,levels=c(0,1)),
    LDH2=factor(LDH2,levels=c(0,1)),
    IgM7=factor(IgM7,levels=c(0,1)),
    B2MG_cat=factor(B2MG_cat,levels=c(0,1)),
    B_Sx=factor(B_Sx,levels=c(0,1)),
    `ANC < 1000` = factor(`ANC < 1000`, levels = c(0, 1))
  )     
# View(dat)

# -- Cox PH and time-dependent ROC curve -- #
base_vars <- dat %>%
  select(-c("진단일","last_fu","death","death_day","death_yr",
            "B2MG_cat","MYD88","CXCR4",
            "IPSS","RIPSS","MSS",
            "age65", "PLT100", "IgM7", "TLT12")) %>%
  names()
base_vars <- dat %>%
  select(-c("진단일","last_fu","death","death_day","death_yr",
            "B2MG_cat","MYD88","CXCR4",
            "IPSS","RIPSS","MSS","TLT12")) %>%
  names()
base_vars <- c("TLT12")
base_vars <- c()

var_list <- c("None", "IPSS", "RIPSS", "MSS")
var_list <- c("IPSS", "RIPSS", "MSS")
var_list <- "IPSS"  # 임시로 IPSS 모델만 돌려보기
var_list <- "None"  # 임시로 None 모델만 돌려보기

for (nm in var_list) {
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



# -- ROC curve 비교 -- #
library(timeROC)

AUC_TIMES <- 1:8
PLOT_TIMES <- 1:8

base_vars <- dat %>%
  select(-c("진단일", "last_fu", "death", "death_day", "death_yr",
            "B2MG_cat", "MYD88", "CXCR4",
            "IPSS", "RIPSS", "MSS",
            "TLT12")) %>%
  names()

ipss_components <- c("age65", "IgM7", "PLT100")
extra_vars <- setdiff(base_vars, ipss_components)

model_sets <- list(
  "Base"         = base_vars,
  "IPSS+extras"  = c("IPSS", extra_vars),
  "IPSS"         = "IPSS",
  "RIPSS"        = "RIPSS",
  "MSS"          = "MSS"
)

all_vars <- unique(unlist(model_sets))
dat_cc   <- dat[, c("death", "death_yr", all_vars)] %>% na.omit()
cat(sprintf("Common complete case N = %d\n", nrow(dat_cc)))

results <- list()
sapply(model_sets, function(v) "TLT12" %in% v)
# 다 FALSE여야 정상

for (nm in names(model_sets)) {
  vars <- model_sets[[nm]]
  fml  <- as.formula(paste("Surv(death_yr, death) ~",
                           paste0("`", vars, "`", collapse = " + ")))
  mod  <- coxph(fml, data = dat_cc)
  lp   <- predict(mod, type = "lp")
  
  tr <- timeROC(T         = dat_cc$death_yr,
                delta     = dat_cc$death,
                marker    = lp,
                cause     = 1,
                weighting = "marginal",
                times     = AUC_TIMES,
                iid       = TRUE)
  
  results[[nm]] <- list(model = mod, timeROC = tr, n = nrow(dat_cc))
  cat(sprintf("  [%s] fitted (N=%d, p=%d)\n", nm, nrow(dat_cc), length(vars)))
}

colors <- pal_lancet()(length(results))

for (tp in PLOT_TIMES) {
  png(sprintf("/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/Stepwise/ROC_overlay_%dyr.png", tp),
      height = 8, width = 8, units = "in", res = 300)
  par(pty = "s", mar = c(5, 5, 4, 2))

  for (i in seq_along(results)) {
    plot(results[[i]]$timeROC, time = tp,
        col = colors[i], lwd = 3,
        add = (i != 1), title = FALSE)
  }
  title(main = sprintf("Time-dependent ROC (%d-year OS)", tp),
        cex.main = 1.3)

  legend_labels <- sapply(seq_along(results), function(i) {
    tr  <- results[[i]]$timeROC
    idx <- which(tr$times == tp)
    a   <- tr$AUC[idx]
    se  <- tr$inference$vect_sd_1[idx]
    sprintf("%s = %.3f (%.3f–%.3f)",
            names(results)[i], a, a - 1.96*se, a + 1.96*se)
  })
  legend("bottomright", legend = legend_labels,
        col = colors, lwd = 3, cex = 0.9, bty = "n")
  dev.off()

  cat(sprintf("  [%d-year] ROC plotted\n", tp))
}



# -- ROC curve 비교: Stepwise -- #
AUC_TIMES <- 1:8
PLOT_TIMES <- 1:8


base_vars     <- c("age65", "ECOG performance status < 2", "B_Sx", "ANC < 1000", "LDH2", "ALB3.5") # Base 모델 변수
ipss_extras   <- c("B_Sx", "ANC < 1000", "LDH2", "ALB3.5") # IPSS 모델에 추가할 변수들 


model_sets <- list(
  "Base"        = base_vars,
  "IPSS+extras" = c("IPSS", ipss_extras),   # IPSS + b_sx, anc, ldh, alb
  "IPSS"        = "IPSS",
  "RIPSS"       = "RIPSS",
  "MSS"         = "MSS"
)

all_vars <- unique(unlist(model_sets))
dat_cc   <- dat[, c("death", "death_yr", all_vars)] %>% na.omit()
cat(sprintf("Common complete case N = %d\n", nrow(dat_cc)))

results <- list()
sapply(model_sets, function(v) "TLT12" %in% v)
# 다 FALSE여야 정상

for (nm in names(model_sets)) {
  vars <- model_sets[[nm]]
  fml  <- as.formula(paste("Surv(death_yr, death) ~",
                           paste0("`", vars, "`", collapse = " + ")))
  mod  <- coxph(fml, data = dat_cc)
  lp   <- predict(mod, type = "lp")
  
  tr <- timeROC(T         = dat_cc$death_yr,
                delta     = dat_cc$death,
                marker    = lp,
                cause     = 1,
                weighting = "marginal",
                times     = AUC_TIMES,
                iid       = TRUE)
  
  results[[nm]] <- list(model = mod, timeROC = tr, n = nrow(dat_cc))
  cat(sprintf("  [%s] fitted (N=%d, p=%d)\n", nm, nrow(dat_cc), length(vars)))
}

colors <- pal_lancet()(length(results))

for (tp in PLOT_TIMES) {
  png(sprintf("/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/Stepwise/ROC_overlay_%dyr.png", tp),
      height = 8, width = 8, units = "in", res = 300)
  par(pty = "s", mar = c(5, 5, 4, 2))

  for (i in seq_along(results)) {
    plot(results[[i]]$timeROC, time = tp,
        col = colors[i], lwd = 3,
        add = (i != 1), title = FALSE)
  }
  title(main = sprintf("Time-dependent ROC (%d-year OS)", tp),
        cex.main = 1.3)

  legend_labels <- sapply(seq_along(results), function(i) {
    tr  <- results[[i]]$timeROC
    idx <- which(tr$times == tp)
    a   <- tr$AUC[idx]
    se  <- tr$inference$vect_sd_1[idx]
    sprintf("%s = %.3f (%.3f–%.3f)",
            names(results)[i], a, a - 1.96*se, a + 1.96*se)
  })
  legend("bottomright", legend = legend_labels,
        col = colors, lwd = 3, cex = 0.9, bty = "n")
  dev.off()

  cat(sprintf("  [%d-year] ROC plotted\n", tp))
}


# -- Integrated AUC 계산 -- #
AUC_TIMES <- 1:8
PLOT_TIMES <- 1:8


base_vars     <- c("age65", "ECOG performance status < 2", "B_Sx", "ANC < 1000", "LDH2", "ALB3.5") # Base 모델 변수
ipss_extras   <- c("B_Sx", "ANC < 1000", "LDH2", "ALB3.5") # IPSS 모델에 추가할 변수들 


model_sets <- list(
  "Base"        = base_vars,
  "IPSS+extras" = c("IPSS", ipss_extras),   # IPSS + b_sx, anc, ldh, alb
  "IPSS"        = "IPSS",
  "RIPSS"       = "RIPSS",
  "MSS"         = "MSS"
)

all_vars <- unique(unlist(model_sets))
dat_cc   <- dat[, c("death", "death_yr", all_vars)] %>% na.omit()
cat(sprintf("Common complete case N = %d\n", nrow(dat_cc)))

results <- list()

for (nm in names(model_sets)) {
  vars <- model_sets[[nm]]
  fml  <- as.formula(paste("Surv(death_yr, death) ~",
                           paste0("`", vars, "`", collapse = " + ")))
  mod  <- coxph(fml, data = dat_cc)
  lp   <- predict(mod, type = "lp")
  
  tr <- timeROC(T         = dat_cc$death_yr,
                delta     = dat_cc$death,
                marker    = lp,
                cause     = 1,
                weighting = "marginal",
                times     = AUC_TIMES,
                iid       = TRUE)
  
  # 각 AUC_TIMES 시점의 marginal OS 확률 (IntAUC 가중치용)
  temp_surv_prop <- summary(
    survfit(Surv(death_yr, death) ~ 1, data = dat_cc),
    times  = AUC_TIMES,
    extend = TRUE
  )$surv
  
  # Integrated AUC
  iauc <- IntAUC(tr$AUC, AUC_TIMES, temp_surv_prop,
                 tmax = max(AUC_TIMES))
  
  results[[nm]] <- list(model   = mod,
                        timeROC = tr,
                        n       = nrow(dat_cc),
                        iauc    = iauc)
  
  cat(sprintf("  [%s] fitted (N=%d, p=%d) | iAUC=%.3f\n",
              nm, nrow(dat_cc), length(vars), iauc))
}


# -- Survival curve by 1L_1 -- #

dat <- crf %>%
  select(c("1L_1", "TLT12", "age65", "sex", `ECOG performance status < 2`,
           "B_Sx", "LNE", "HS", "IgM7", "ANC < 1000", "PLT100", "LDH2",
           "ALB3.5", "B2MG_cat", "IPSS", "RIPSS", "MSS", "MYD88", "CXCR4",
           "진단일", "last_fu", "death", "death_day", "death_yr")) %>%
  mutate(
    sex   = factor(sex,   levels = c("M", "F")),
    age65 = factor(age65, levels = c(0, 1)),
    `ECOG performance status < 2` =
      factor(`ECOG performance status < 2`, levels = c(0, 1)),
    LNE      = factor(LNE,      levels = c(0, 1)),
    HS       = factor(HS,       levels = c(0, 1)),
    PLT100   = factor(PLT100,   levels = c(0, 1)),
    ALB3.5   = factor(ALB3.5,   levels = c(0, 1)),
    LDH2     = factor(LDH2,     levels = c(0, 1)),
    IgM7     = factor(IgM7,     levels = c(0, 1)),
    B2MG_cat = factor(B2MG_cat, levels = c(0, 1)),
    B_Sx     = factor(B_Sx,     levels = c(0, 1)),
    `ANC < 1000` = factor(`ANC < 1000`, levels = c(0, 1))
  )

# Landmark: 1년 이상 생존 환자만
dat_land <- dat[dat$death_yr >= 1, ]

nrow(dat_land)

# first_regimen factor (reference = 1_BR)
dat_land$first_regimen <- factor(dat_land$`1L_1`,
  levels = c("1_BR", "2_R_Cy", "3_R_borte", "4_others"))


model_sets <- list(
  "Base"        = c("age65", "`ECOG performance status < 2`", "B_Sx",
                    "`ANC < 1000`", "LDH2", "ALB3.5"),
  "IPSS+extras" = c("IPSS", "B_Sx", "`ANC < 1000`", "LDH2", "ALB3.5"),
  "RIPSS"       = "RIPSS"
)


fit <- survfit(Surv(death_yr, death) ~ first_regimen, data = dat_land)

png("survival_by_1L_1.png", height = 10, width = 10, units = "in", res = 300)
font_size <- 16
p <- ggsurvplot(
  fit, data           = dat_land,
  surv.median.line    = "hv",
  risk.table          = TRUE,
  tables.col          = "strata",
  tables.y.text       = FALSE,
  conf.int            = FALSE,
  xlim                = c(0, 12),
  xlab                = "Time (years)",
  ylab                = "Survival Probability (%)",
  legend.title        = "1st-line regimen",
  legend.labs         = levels(dat_land$first_regimen),
  tables.height       = 0.25,
  break.time.by       = 1,
  risk.table.fontsize = 5,
  palette             = pal_lancet()(nlevels(dat_land$first_regimen)),
  tables.theme        = theme_cleantable()
)
p$plot <- p$plot +
  scale_y_continuous(labels = function(x) x * 100) +
  theme(axis.title  = element_text(size = font_size),
        axis.text   = element_text(size = font_size),
        legend.text = element_text(size = font_size - 2))
print(p)
dev.off()


# -- Median OS --
med_surv <- as.data.frame(surv_median(fit)) %>%
  mutate(`Median OS (95% CI)` =
           sprintf("%.1f (%.1f–%.1f)", median, lower, upper)) %>%
  select(strata, `Median OS (95% CI)`)

# -- Median follow-up (reverse KM) --
rev_dat <- dat_land %>% mutate(death_rev = 1 - death)
fit_rev <- survfit(Surv(death_yr, death_rev) ~ first_regimen, data = rev_dat)
med_fu  <- as.data.frame(surv_median(fit_rev)) %>%
  mutate(`Median FU (95% CI)` =
           sprintf("%.1f (%.1f–%.1f)", median, lower, upper)) %>%
  select(strata, `Median FU (95% CI)`)

# -- Survival probability at 5yr / 10yr --
fmt_surv <- function(fit, tp) {
  s <- summary(fit, times = tp, extend = TRUE)
  data.frame(
    strata = as.character(s$strata),
    value  = sprintf("%.1f%% (%.1f–%.1f)",
                     s$surv * 100, s$lower * 100, s$upper * 100),
    stringsAsFactors = FALSE
  )
}
surv_5yr  <- fmt_surv(fit, 5);  names(surv_5yr)[2]  <- "5yr OS (95% CI)"
surv_10yr <- fmt_surv(fit, 10); names(surv_10yr)[2] <- "10yr OS (95% CI)"

km_summary <- med_surv %>%
  full_join(med_fu,    by = "strata") %>%
  full_join(surv_5yr,  by = "strata") %>%
  full_join(surv_10yr, by = "strata") %>%
  mutate(Group = gsub("first_regimen=", "", strata)) %>%
  select(Group, `Median OS (95% CI)`, `Median FU (95% CI)`,
         `5yr OS (95% CI)`, `10yr OS (95% CI)`)

km_summary %>%
  gt() %>%
  tab_header(title    = "Survival summary by 1st-line regimen (unadjusted)",
             subtitle = "Landmark: 1 year") 


mod_unadj <- coxph(Surv(death_yr, death) ~ first_regimen, data = dat_land)
s_unadj   <- summary(mod_unadj)

unadj_tbl <- data.frame(
  Group = gsub("^`?first_regimen`?", "", rownames(s_unadj$conf.int)),
  HR_CI = sprintf("%.2f (%.2f–%.2f)",
                  s_unadj$conf.int[, "exp(coef)"],
                  s_unadj$conf.int[, "lower .95"],
                  s_unadj$conf.int[, "upper .95"]),
  P     = ifelse(s_unadj$coefficients[, "Pr(>|z|)"] < 0.001, "<.001",
                 sprintf("%.3f", s_unadj$coefficients[, "Pr(>|z|)"])),
  stringsAsFactors = FALSE
)

unadj_tbl %>%
  gt() %>%
  cols_label(
    HR_CI = "Unadjusted HR (95% CI)",
    P     = "P-value"
  ) %>%
  tab_header(
    title    = "Unadjusted HR for 1st-line regimen",
    subtitle = sprintf("N = %d", mod_unadj$n)
  )

extract_1L_HR <- function(model_name, covars) {
  fml <- as.formula(paste("Surv(death_yr, death) ~ first_regimen +",
                          paste(covars, collapse = " + ")))
  mod <- coxph(fml, data = dat_land)
  s   <- summary(mod)
  
  rows_1L <- grep("^`?first_regimen`?", rownames(s$conf.int))
  
  data.frame(
    Model = model_name,
    Group = rownames(s$conf.int)[rows_1L],
    N     = mod$n,
    HR_CI = sprintf("%.2f (%.2f–%.2f)",
                    s$conf.int[rows_1L, "exp(coef)"],
                    s$conf.int[rows_1L, "lower .95"],
                    s$conf.int[rows_1L, "upper .95"]),
    P     = ifelse(s$coefficients[rows_1L, "Pr(>|z|)"] < 0.001, "<.001",
                   sprintf("%.3f", s$coefficients[rows_1L, "Pr(>|z|)"])),
    stringsAsFactors = FALSE
  )
}

hr_tbl <- do.call(rbind,
                  mapply(extract_1L_HR,
                         names(model_sets), model_sets,
                         SIMPLIFY = FALSE)) %>%
  mutate(Group = gsub("^`?first_regimen`?", "", Group))


for (nm in names(model_sets)) {
  sub_tbl <- hr_tbl %>% filter(Model == nm)
  n_used  <- unique(sub_tbl$N)
  
  tbl <- sub_tbl %>%
    select(-Model, -N) %>%
    gt() %>%
    cols_label(
      HR_CI = "Adjusted HR (95% CI)",
      P     = "P-value"
    ) %>%
    tab_header(
      title    = sprintf("Adjusted HR for 1st-line regimen — %s model", nm),
      subtitle = sprintf("N = %d / %d",
                         n_used, nrow(dat_land))
    )  
  print(tbl)
}

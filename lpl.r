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
  select(c("TLT12", "1L_1", "1L_start", "age", "age65", "sex", "ECOG", "PS", "B_Sx", "LNE", 
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
    "진단일","last_fu", "death","death_day","death_yr")) %>%
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
# View(dat)

# -- Cox PH and time-dependent ROC curve -- #
multi_step <- function (nm, dat, base_vars) {
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
    multi_beta = coefs[["coef"]],
    multi_Score = round(coefs[["coef"]] * 5),
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
      multi_Beta = ifelse(is.na(multi_beta), "-", sprintf("%.2f", multi_beta)),
      multi_Score = ifelse(is.na(multi_Score), "-", as.character(multi_Score)),

      step_ci = ifelse(is.na(step_HR), "-",
        sprintf("%.2f (%.2f-%.2f)", step_HR, step_LCL, step_UCL)),
      step_pv = ifelse(is.na(step_p), "-",
        ifelse(step_p < 0.001, "<.001", sprintf("%.3f", step_p))),
      step_Beta = ifelse(is.na(step_beta), "-", sprintf("%.2f", step_beta)),
      step_Score = ifelse(is.na(step_Score), "-", as.character(step_Score))
    ) %>%
    select(Variable = term, multi_ci, multi_pv, step_ci, step_pv, step_Beta, step_Score) %>%
    gt() %>%
    cols_label(
      multi_ci = "HR (95% CI)", multi_pv = "P-value",
      step_ci = "HR (95% CI)", step_pv = "P-value", step_Beta = "Beta", step_Score = "Score"
    ) %>%
    tab_header(
      title = sprintf("Multivariable Cox Regression - %s", title_nm),
      subtitle = sprintf("Full AIC = %.1f / Step AIC = %.1f, N = %d / %d", AIC(full.model), AIC(step.model), full.model$n, nrow(dat))
    ) %>%
    tab_spanner(label = "Full Model", columns = c(multi_ci, multi_pv)) %>%
    tab_spanner(label = "Stepwise", columns = c(step_ci, step_pv, step_Beta, step_Score))
  
  print(tbl)
  invisible(tbl)   # ← 이 한 줄만 추가
}

# -- Cox PH multivariable only -- #
multi <- function (nm, dat, base_vars) {
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
  
  # gt
  tbl <- multi_df %>%
    mutate(
      multi_ci = sprintf("%.2f (%.2f-%.2f)", multi_HR, multi_LCL, multi_UCL),
      multi_pv = ifelse(multi_p < 0.001, "<.001", sprintf("%.3f", multi_p))
    ) %>%
    select(Variable = term, multi_ci, multi_pv) %>%
    gt() %>%
    cols_label(
      multi_ci = "HR (95% CI)", multi_pv = "P-value"
    ) %>%
    tab_header(
      title = sprintf("Multivariable Cox Regression - %s", title_nm),
      subtitle = sprintf("AIC = %.1f, N = %d / %d", AIC(full.model), full.model$n, nrow(dat))
    ) %>%
    tab_spanner(label = "Full Model", columns = c(multi_ci, multi_pv))
  
  print(tbl)
  
}

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


# 3) IPSS 인자 + RIPSS 인자 모델
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

plot_timeROC_medianFU <- function(model_list,
                                  list_name = deparse(substitute(model_list))) {

  all_vars <- unique(unlist(model_list))
  dat_cc   <- dat[, c("death", "death_yr", all_vars)] %>% na.omit()

  fit_rev   <- survfit(Surv(death_yr, 1 - death) ~ 1, data = dat_cc)
  median_fu <- as.numeric(summary(fit_rev)$table["median"])

  cat(sprintf("\n========== [%s] Time-dependent ROC @ median FU ==========\n",
              list_name))
  cat(sprintf("N = %d | median FU = %.2f yr\n\n", nrow(dat_cc), median_fu))

  results <- list()
  for (nm in names(model_list)) {
    vars <- model_list[[nm]]
    fml  <- as.formula(paste("Surv(death_yr, death) ~",
                             paste0("`", vars, "`", collapse = " + ")))
    mod  <- coxph(fml, data = dat_cc)
    lp   <- predict(mod, type = "lp")

    tr <- timeROC(T = dat_cc$death_yr, delta = dat_cc$death,
                  marker = lp, cause = 1, weighting = "marginal",
                  times = median_fu, iid = TRUE)

    idx <- which.min(abs(tr$times - median_fu))
    auc <- tr$AUC[idx]
    se  <- tr$inference$vect_sd_1[idx]


    results[[nm]] <- list(timeROC = tr, auc = auc, se = se)
    cat(sprintf("  [%-30s] AUC = %.3f (%.3f-%.3f)\n",
                nm, auc, auc - 1.96 * se, auc + 1.96 * se))
  }

  # ---- Plot ----
  out_dir <- "/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/[26-05-11] time-dependent ROC"
  png_path <- file.path(out_dir, sprintf("ROC_medianFU_%s.png", list_name))

  colors <- pal_lancet()(length(results))
  png(png_path, height = 8, width = 8, units = "in", res = 300)
  par(pty = "s", mar = c(5, 5, 4, 2))
  for (i in seq_along(results)) {
    plot(results[[i]]$timeROC, time = median_fu,
         col = colors[i], lwd = 3, add = (i != 1), title = FALSE)
  }
  title(main = sprintf("Time-dependent ROC (median FU = %.2f yr) — %s",
                       median_fu, list_name), cex.main = 1.1)
  legend_labels <- sapply(seq_along(results), function(i) {
    r <- results[[i]]
    sprintf("%s = %.3f (%.3f-%.3f)", names(results)[i],
            r$auc, r$auc - 1.96 * r$se, r$auc + 1.96 * r$se)
  })
  legend("bottomright", legend = legend_labels,
         col = colors, lwd = 3, cex = 0.8, bty = "n")
  dev.off()
  cat(sprintf("\nPlot saved: %s\n", png_path))

  invisible(results)
}


compute_iAUC <- function(model_list,
                         list_name  = deparse(substitute(model_list)),
                         iauc_times = 1:8) {

  all_vars <- unique(unlist(model_list))
  dat_cc   <- dat[, c("death", "death_yr", all_vars)] %>% na.omit()

  cat(sprintf("\n========== [%s] Integrated AUC (times %d-%d yr) ==========\n",
              list_name, min(iauc_times), max(iauc_times)))
  cat(sprintf("N = %d\n\n", nrow(dat_cc)))

  # marginal OS 확률 (모델 무관, 한 번만 계산)
  temp_surv_prop <- summary(
    survfit(Surv(death_yr, death) ~ 1, data = dat_cc),
    times = iauc_times, extend = TRUE
  )$surv

  for (nm in names(model_list)) {
    vars <- model_list[[nm]]
    fml  <- as.formula(paste("Surv(death_yr, death) ~",
                             paste0("`", vars, "`", collapse = " + ")))
    mod  <- coxph(fml, data = dat_cc)
    lp   <- predict(mod, type = "lp")

    tr <- timeROC(T = dat_cc$death_yr, delta = dat_cc$death,
                  marker = lp, cause = 1, weighting = "marginal",
                  times = iauc_times, iid = TRUE)

    iauc <- IntAUC(tr$AUC, iauc_times, temp_surv_prop,
                   tmax = max(iauc_times))
    cat(sprintf("  [%-30s] iAUC = %.3f\n", nm, iauc))
  }
}


# ---- 호출 ----
plot_timeROC_medianFU(model_list1)
plot_timeROC_medianFU(model_list2)
plot_timeROC_medianFU(model_list3)

compute_iAUC(model_list1)
compute_iAUC(model_list2)
compute_iAUC(model_list3)



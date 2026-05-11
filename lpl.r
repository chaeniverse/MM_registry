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


# 3) IPSS 인자 + RIPSS 인자 모델
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

for (i in seq_along(model_list1)) {
  cat(sprintf("=== Model: %s ===\n", names(model_list1)[i]))
  multi_step("None", dat, model_list1[[i]])
}

model_list3 <- list(
  "age65 + B2MG3 + others"        = c("sex", "ECOG performance status < 2", "B_Sx", "LNE", "HS", "ANC < 1000", "age65", "Hb11.5", "PLT100", "B2MG_cat", "IgM7", "LDH250", "ALB3.5"),
  "age65 + B2MG4 + others"           = c("sex", "ECOG performance status < 2", "B_Sx", "LNE", "HS", "ANC < 1000", "age65", "Hb11.5", "PLT100", "B2MG4",    "IgM7", "LDH250", "ALB3.5"),
  "age75 + B2MG3 + others"        = c("sex", "ECOG performance status < 2", "B_Sx", "LNE", "HS", "ANC < 1000", "age75", "Hb11.5", "PLT100", "B2MG_cat", "IgM7", "LDH250", "ALB3.5"),
  "age75 + B2MG4 + others"           = c("sex", "ECOG performance status < 2", "B_Sx", "LNE", "HS", "ANC < 1000", "age75", "Hb11.5", "PLT100", "B2MG4",    "IgM7", "LDH250", "ALB3.5")
)

for (i in seq_along(model_list)) {
  cat(sprintf("=== Model: %s ===\n", names(model_list)[i]))
  multi_step("None", dat, model_list[[i]])
}


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
                TRUE ~ NA_real_),

    Score_Our_B2MG3 = 
      case_when(`ALB3.5` == 1 ~ 3, 
                `ALB3.5` == 0 ~ 0, 
                TRUE ~ NA_real_),

    Score_Our_B2MG4 = 
      case_when(`ALB3.5` == 1 ~ 3, 
                `ALB3.5` == 0 ~ 0, 
                TRUE ~ NA_real_),
)

base_vars <- c("Score_Our_B2MG3", "B_Sx")
multi_step("None", dat, base_vars)

base_vars <- c("Score_Our_B2MG4", "B_Sx")
multi_step("None", dat, base_vars)

dat <- dat %>% mutate(
    Score_Our_B2MG3 = Score_Our_B2MG3 +
      case_when(B_Sx == 1 ~ 4, 
                B_Sx == 0 ~ 0, 
                TRUE ~ NA_real_),
    Score_Our_B2MG4 = Score_Our_B2MG4 +
      case_when(B_Sx == 1 ~ 4, 
                B_Sx == 0 ~ 0, 
                TRUE ~ NA_real_)
  )
  
# 분포 확인
summary(dat[, c("Score_IPSS", "Score_RIPSS", "Score_Our_B2MG3", "Score_Our_B2MG4")])


# -- Score별 요약 테이블 -- #
score_summary_table <- function(data, score_var, outcome_var) {

  d <- data
  d$.score <- d[[score_var]]
  d$.y     <- d[[outcome_var]]

  # Score별 N / Event / Event rate
  per_score <- d %>% group_by(Score = .score) %>%
    summarise(N = n(),
              Event = sum(.y == 1, na.rm = TRUE),
              Event_Rate = round(Event / N * 100, 1),
              .groups = "drop") %>%
    arrange(Score) %>%
    mutate(Score = ifelse(is.na(Score), "NA", as.character(Score)))
    

  # Total 행 추가
  total <- d %>% summarise(
    Score = "Total",
    N = n(),
    Event = sum(.y == 1, na.rm = TRUE),
    Event_Rate = round(Event / N * 100, 1)
  )

  bind_rows(
    per_score,
    total
  ) |> gt()
}

# -- Score별 event rate table -- #
score_summary_table(dat, "Score_IPSS", "death")
score_summary_table(dat, "Score_RIPSS", "death")
score_summary_table(dat, "Score_Our_B2MG3", "death")
score_summary_table(dat, "Score_Our_B2MG4", "death")




# ---- Survival curve by 1L_1 ---- #
format_median <- function(fit){
  m <- surv_median(fit)
  data.frame(
    strata = as.character(m[[1]]),
    value  = sprintf("%.1f (%.1f-%.1f)", m[, 2], m[, 3], m[, 4]),
    stringsAsFactors = FALSE
  )
}

format_surv <- function(fit, time_point){
  s <- summary(fit, times = time_point, extend = TRUE)
  data.frame(
    strata = as.character(s$strata),
    value  = sprintf("%.1f%% (%.1f-%.1f)", s$surv*100, s$lower*100, s$upper*100),
    stringsAsFactors = FALSE
  )
}


fit <- survfit(Surv(death_yr, death) ~ first_regimen, data = dat)

OUTPUT_DIR="/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/"
png(paste0(OUTPUT_DIR, "survival_by_1L_1.png"), height = 10, width = 10, units = "in", res = 300)
font_size <- 16
p <- ggsurvplot(
  fit, data           = dat,
  surv.median.line    = "hv",
  risk.table          = TRUE,
  tables.col          = "strata",
  tables.y.text       = FALSE,
  conf.int            = FALSE,
  xlim                = c(0, 12),
  xlab                = "Time (years)",
  ylab                = "Survival Probability (%)",
  legend.title        = "1st-line regimen",
  legend.labs         = levels(dat$first_regimen),
  tables.height       = 0.25,
  break.time.by       = 1,
  risk.table.fontsize = 5,
  palette             = pal_lancet()(nlevels(dat$first_regimen)),
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
rev_dat <- dat %>% mutate(death_rev = 1 - death)
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
  tab_header(title    = "Survival summary by 1st-line regimen") 

nrow(dat)
mod_unadj <- coxph(Surv(death_yr, death) ~ first_regimen, data = dat)
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
    HR_CI = "HR (95% CI)",
    P     = "P-value"
  ) %>%
  tab_header(
    title    = "HR for 1st-line regimen",
    subtitle = sprintf("N = %d", mod_unadj$n)
  )


# -- 1. Regimen 분포 -- #
regimen_dist <- dat %>%
  count(first_regimen, name = "N", .drop = FALSE) %>%
  mutate(
    Percent = sprintf("%.1f%%", N / sum(N) * 100)
  )

regimen_dist %>%
  gt() %>%
  tab_header(
    title    = "1st-line regimen distribution"
  )
OUTPUT_DIR = "/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure"
# Bar chart
png(file.path(OUTPUT_DIR, "regimen_distribution.png"),
    height = 6, width = 8, units = "in", res = 300)
ggplot(dat, aes(x = first_regimen, fill = first_regimen)) +
  geom_bar(width = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.4, size = 5) +
  scale_fill_lancet() +
  labs(title = "1st-line regimen distribution",
       x = NULL, y = "N") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))
dev.off()




# -- 아래 것은 사용할 때 코드 확인하며 하기 -- #

# -- Survival analysis -- #
survival <- dat  %>% 
  mutate(
    groupA65 = 
      case_when(
        ScoreA65<=1 ~"Low",
        ScoreA65<=5 ~ "Intermediate",
        T ~ "High"),
    groupA75 = 
      case_when(
        ScoreA75<=0 ~"Low",
        ScoreA75<=5 ~ "Intermediate",
        T ~ "High"),
    groupD = 
      case_when(
        ScoreD<=1 ~"Low",
        ScoreD<=4 ~ "Intermediate",
        T ~ "High"),
    groupE = 
      case_when(
        ScoreE<=0 ~"Low",
        ScoreE<=5 ~ "Intermediate",
        T ~ "High")
  ) %>% 
  mutate(groupA65 = factor(groupA65, levels=c("Low","Intermediate","High")),
         groupA75 = factor(groupA75, levels=c("Low","Intermediate","High")),
         groupD = factor(groupD, levels=c("Low","Intermediate","High")),
         groupE = factor(groupE, levels=c("Low","Intermediate","High")))

table(survival$first_regimen)


# 변경 전: sapply로 character vector 반환 → sub()로 파싱 필요
# 변경 후: data.frame으로 strata + value 반환 → join 가능


plot_regimen <- function(dat, regimen_name, group_var = "groupA65"){
  fmla <- as.formula(paste0("Surv(as.numeric(death_yr), death) ~ ", group_var))
  fit  <- surv_fit(fmla, data = dat)

  font_size <- 14
  p <- ggsurvplot(fit, data = dat,
                  surv.median.line = "hv",
                  risk.table = TRUE,
                  tables.col = "strata",
                  tables.y.text = FALSE,
                  conf.int = TRUE,
                  xlim = c(0, 12),
                  xlab = "Time (years)",
                  ylab = "Survival Probability (%)",
                  title = regimen_name,
                  legend = "right",
                  legend.title = group_var,
                  tables.height = 0.25,
                  break.time.by = 2,
                  risk.table.fontsize = 4,
                  palette = pal_lancet()(3)[c(1, 3, 2)],
                  tables.theme = theme_cleantable())

  p$plot <- p$plot +
    scale_y_continuous(labels = function(x) x * 100) +
    theme(
      axis.title  = element_text(size = font_size),
      axis.text   = element_text(size = font_size),
      plot.title  = element_text(size = font_size + 2, face = "bold"),
      legend.text = element_text(size = font_size - 2)
    )
  p
}


summary_regimen <- function(dat, regimen_name, group_var = "groupA65"){
  fmla <- as.formula(paste0("Surv(as.numeric(death_yr), death) ~ ", group_var))
  fit  <- surv_fit(fmla, data = dat)
  cox  <- coxph(fmla, data = dat)
  sc   <- summary(cox)

  # reverse KM (median FU)
  rev_dat <- dat
  rev_dat$death <- 1 - rev_dat$death
  sfit <- surv_fit(fmla, data = rev_dat)

  # 통계 join — sub() 정규식 없이 strata 컬럼으로 깔끔하게 join
  med <- format_median(fit)   |> dplyr::rename(median_os = value)
  s5  <- format_surv(fit, 5)  |> dplyr::rename(y5        = value)
  s10 <- format_surv(fit, 10) |> dplyr::rename(y10       = value)
  fu  <- format_median(sfit)  |> dplyr::rename(median_fu = value)

  surv_df <- med |>
    dplyr::full_join(s5,  by = "strata") |>
    dplyr::full_join(s10, by = "strata") |>
    dplyr::full_join(fu,  by = "strata") |>
    dplyr::mutate(Group = sub(paste0("^", group_var, "="), "", strata)) |>
    dplyr::select(Group, median_os, y5, y10, median_fu)

  # gt 1: 생존 통계
  surv_gt <- surv_df |>
    gt::gt() |>
    gt::tab_header(title = regimen_name) |>
    gt::cols_label(
      median_os = "Median OS, yr (95% CI)",
      y5        = "5-yr survival (95% CI)",
      y10       = "10-yr survival (95% CI)",
      median_fu = "Median FU, yr (95% CI)"
    ) |>
    gt::sub_missing(missing_text = "-")

  # gt 2: Cox HR (reference 행 없이 깔끔하게)
  hr_df <- data.frame(
    group = rownames(sc$coefficients),
    HR  = sprintf("%.2f (%.2f-%.2f)",
                  sc$conf.int[,1], sc$conf.int[,3], sc$conf.int[,4]),
    p   = ifelse(sc$coefficients[,5] < 0.001, "<.001",
                 sprintf("%.3f", sc$coefficients[,5]))
  )

  hr_gt <- hr_df |>
    gt::gt() |>
    gt::tab_header(title = regimen_name)

  list(surv = surv_gt, hr = hr_gt)
}

regimens <- survival %>% filter(!is.na(first_regimen)) %>%
  pull(first_regimen) %>% unique() %>% sort()

group_name <- c("Our_model_age65", "Our_model_age75", "Our_model_score_BSx_age65", "Our_model_score_BSx_age75")
for (group_var in c("groupA65", "groupA75", "groupD", "groupE")) {
  # 그림 (2x2 합쳐서 저장)
  plot_list <- lapply(regimens, function(r){
    plot_regimen(survival %>% filter(first_regimen == r), as.character(r), group_var = group_var)
  })

  OUTPUT_DIR="/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/"
  dev.new()
  png(paste0(OUTPUT_DIR, "survival_by_regimen_2x1_", group_name[which(c("groupA65", "groupA75", "groupD", "groupE") == group_var)], ".png"),
      width = 16, height = 16, units = "in", res = 300)
  arrange_ggsurvplots(plot_list, ncol = 2, nrow = 2, print = TRUE)
  dev.off()
}

# 통계 테이블 (regimen별로 gt 두 개씩)
for (group_var in c("groupA65", "groupA75", "groupD", "groupE")) {
   cat(sprintf("\n=== Grouping by %s ===\n", group_var))
   
   for (r in regimens){
     res <- summary_regimen(survival %>% filter(first_regimen == r), as.character(r), group_var = group_var)
     cat(sprintf("\n--- Regimen: %s ---\n", r))
     print(res$surv)
     print(res$hr)
   }
} 


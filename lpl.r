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
    age > 65 & age <= 75 ~ "65-75",
    age > 75 ~ ">75",
    TRUE ~ NA_character_
  ),
  `B2MG4` = case_when(
    B2MG_cont > 4 ~ 1,
    B2MG_cont <= 4 ~ 0,
    TRUE ~ NA_integer_
  ),
  `ECOG performance status < 2` = case_when(
    PS == "low" ~ 1,
    PS == "high" ~ 0,
    TRUE ~ NA_integer_
  )) %>%
  mutate(`1L_1` = factor(`1L_1`, levels = c("BR", "R_Cy or R_borte", "Others"))) %>%
  mutate(`age75` = factor(`age75`, levels = c("<=65", "65-75", ">75"))) %>%
  mutate(
    Hb10 = ifelse(is.na(Hb), NA, Hb10),
    Hb11 = ifelse(is.na(Hb), NA, Hb11),
    PLT100 = ifelse(is.na(PLT), NA, PLT100),
    LDH2 = ifelse(is.na(LDH), NA, LDH2),
    `ALB3.5` = ifelse(is.na(ALB), NA, `ALB3.5`),
    B2MG_cat = ifelse(is.na(B2MG_cont), NA, B2MG_cat),
    B2MG4 = ifelse(is.na(B2MG_cont), NA, B2MG4)    
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
# View(crf)
crf$`1L_start`
# --- 2-1. 분석용 데이터 준비 ---
# sPEP 제외
dat <- crf %>%
  select(
    c("1L_1", "TLT12", "age65", "sex", `ECOG performance status < 2`, "B_Sx", 
    "LNE", "HS", "IgM7", "ANC < 1000", "PLT100", "LDH2", 
    "ALB3.5", "B2MG_cat", "IPSS", "RIPSS", 
    "MSS", "MYD88", "CXCR4", "진단일","last_fu", 
    "death","death_day","death_yr", "age75", "B2MG4", "Hb11")) %>%
  mutate(
    sex=factor(sex,levels=c("M","F")),
    age65=factor(age65,levels=c(0,1)),
    `ECOG performance status < 2`=factor(`ECOG performance status < 2`,levels=c(0,1)),
    LNE=factor(LNE,levels=c(0,1)),
    HS=factor(HS,levels=c(0,1)),
    # Hb10=factor(Hb10,levels=c(0,1)),
    Hb11=factor(Hb11,levels=c(0,1)),
    PLT100=factor(PLT100,levels=c(0,1)),
    ALB3.5=factor(ALB3.5,levels=c(0,1)),
    LDH2=factor(LDH2,levels=c(0,1)),
    IgM7=factor(IgM7,levels=c(0,1)),
    B2MG_cat=factor(B2MG_cat,levels=c(0,1)),
    B_Sx=factor(B_Sx,levels=c(0,1)),
    `ANC < 1000` = factor(`ANC < 1000`, levels = c(0, 1)),
    `B2MG4` = factor(`B2MG4`, levels = c(0, 1))
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
    select(Variable = term, multi_ci, multi_pv, multi_Beta, multi_Score, step_ci, step_pv, step_Beta, step_Score) %>%
    gt() %>%
    cols_label(
      multi_ci = "HR (95% CI)", multi_pv = "P-value", multi_Beta = "Beta", multi_Score = "Score",
      step_ci = "HR (95% CI)", step_pv = "P-value", step_Beta = "Beta", step_Score = "Score"
    ) %>%
    tab_header(
      title = sprintf("Multivariable Cox Regression - %s", title_nm),
      subtitle = sprintf("Full AIC = %.1f / Step AIC = %.1f, N = %d / %d", AIC(full.model), AIC(step.model), full.model$n, nrow(dat))
    ) %>%
    tab_spanner(label = "Full Model", columns = c(multi_ci, multi_pv, multi_Beta, multi_Score)) %>%
    tab_spanner(label = "Stepwise", columns = c(step_ci, step_pv, step_Beta, step_Score))
  
  print(tbl)
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

# 1) IPSS 변수만 들어간 모델
multi("IPSS", dat, base_vars = c())
multi_step("IPSS", dat, base_vars = c())

# 2) RIPSS 변수만 들어간 모델
multi("RIPSS", dat, base_vars = c())
multi_step("RIPSS", dat, base_vars = c())

# 3) IPSS 인자 + RIPSS 인자 모델
base_vars <- c("age65", "PLT100", "IgM7", "LDH2", "ALB3.5")
multi("None", dat, base_vars)
multi_step("None", dat, base_vars)

base_vars <- c("age65", "PLT100", "IgM7", "LDH2", "ALB3.5", "B_Sx")
multi("None", dat, base_vars)




# -- Score 계산 -- # 
dat <- dat %>% rename(first_regimen = `1L_1`) %>%
  mutate(
    ScoreA = 
      case_when(age65 == 1 ~ 1, 
                age65 == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(PLT100 == 1 ~ 1, 
                PLT100 == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(IgM7 == 1 ~ 1, 
                IgM7 == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(LDH2 == 1 ~ 2, 
                LDH2 == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(ALB3.5 == 1 ~ 3, 
                ALB3.5 == 0 ~ 0, 
                TRUE ~ NA_real_),

    ScoreB = 
      case_when(IPSS == "1_Low"  ~ 0,
                IPSS == "2_Int"  ~ 10,
                IPSS == "3_High" ~ 11,
                TRUE ~ NA_real_),

    ScoreC = 
      case_when(RIPSS == "1_VL"   ~ 0,
                RIPSS == "2_Low"  ~ 1,
                RIPSS == "3_Int"  ~ 7,
                RIPSS == "4_High" ~ 7,
                RIPSS == "5_VH"   ~ 4,
                TRUE ~ NA_real_),
)

base_vars <- c("ScoreA", "B_Sx")
multi("None", dat, base_vars)
multi_step("None", dat, base_vars)

dat <- dat %>% mutate(
    ScoreD = 
      case_when(B_Sx == 1 ~ 4, 
                B_Sx == 0 ~ 0, 
                TRUE ~ NA_real_)
  )
  
# 분포 확인
summary(dat[, c("ScoreA", "ScoreB", "ScoreC", "ScoreD")])

# -- score model의 ROC 계산 -- #
rocA <- roc(dat$death, dat$ScoreA, quiet = TRUE)
rocB <- roc(dat$death, dat$ScoreB, quiet = TRUE)
rocC <- roc(dat$death, dat$ScoreC, quiet = TRUE)
rocD <- roc(dat$death, dat$ScoreD, quiet = TRUE)

ciA <- ci.auc(rocA)
ciB <- ci.auc(rocB)
ciC <- ci.auc(rocC)
ciD <- ci.auc(rocD)

colors <- pal_lancet()(4)[c(2, 3, 1, 4)]   # A, B, C, D 순서대로

# -- ROC plot -- #
while (!is.null(dev.list())) dev.off()

png("/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/ROC_score.png", width = 8, height = 8, units = "in", res = 300)
par(pty = "s")

plot(rocA, legacy.axes = TRUE,
     col = colors[1], lwd = 3,
     xlab = "1 - Specificity", ylab = "Sensitivity",
     cex.lab = 1.3, cex.axis = 1.2)

plot(rocB, col = colors[2], lwd = 3, add = TRUE)
plot(rocC, col = colors[3], lwd = 3, add = TRUE)
plot(rocD, col = colors[4], lwd = 3, add = TRUE)
legend("bottomright",
       legend = c(
         sprintf("Score A (Our Model):  AUC %.3f (%.3f-%.3f)", auc(rocA), ciA[1], ciA[3]),
         sprintf("Score B (IPSS):  AUC %.3f (%.3f-%.3f)", auc(rocB), ciB[1], ciB[3]),
         sprintf("Score C (RIPSS): AUC %.3f (%.3f-%.3f)", auc(rocC), ciC[1], ciC[3]),
         sprintf("Score D (Our Model + B_Sx): AUC %.3f (%.3f-%.3f)", auc(rocD), ciD[1], ciD[3])
       ),
       col = colors,
       lwd = 3, cex = 0.9, bty = "n")

dev.off()

get_youden <- function(roc_obj) {
  coords(roc_obj, "best",
         best.method = "youden",
         ret = c("threshold", "sensitivity", "specificity", "youden"))
}
youdenA <- get_youden(rocA); print(youdenA) #3.5
youdenB <- get_youden(rocB); print(youdenB) #10.5
youdenC <- get_youden(rocC); print(youdenC) #2.5
youdenD <- get_youden(rocD); print(youdenD) #2

plot_score_hist <- function(data, score_var, outcome_var,
                            outcome_labels = c("Event-", "Event+"),
                            cutoff = NULL, binwidth = 1,
                            file = NULL, width = 8, height = 6) {

  data$.score <- data[[score_var]]
  data$.outcome <- factor(data[[outcome_var]],
                          levels = c(0, 1), labels = outcome_labels)

  mu <- data %>% group_by(.outcome) %>%
    summarise(grp.mean = mean(.score, na.rm = TRUE), .groups = "drop")

  p <- ggplot(data, aes(x = .score, fill = .outcome, color = .outcome)) +
    geom_histogram(aes(y = ..density..),
                   binwidth = binwidth, alpha = 0.2, position = "identity") +
    geom_vline(data = mu, aes(xintercept = grp.mean, color = .outcome),
               linetype = "dashed") +
    scale_fill_manual(values = pal_lancet()(2), labels = outcome_labels) +
    scale_color_manual(values = pal_lancet()(2), labels = outcome_labels) +
    labs(x = score_var, y = "Density", fill = NULL, color = NULL) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(fill = NA, colour = "black"),
      axis.text = element_text(size = 12, colour = "black"),
      axis.title = element_text(size = 14, colour = "black"),
      legend.position = "bottom",
      legend.text = element_text(size = 11)
    )

  if (!is.null(cutoff)) {
    p <- p + geom_vline(xintercept = cutoff,
                        linetype = "dotted", linewidth = 1) +
      annotate("text", x = cutoff, y = Inf, vjust = 2,
               label = sprintf("cutoff = %.1f", cutoff))
  }

  if (!is.null(file)) {
    ggsave(file, plot = p, width = width, height = height, dpi = 300)
  }
  p
}
plot_score_hist(dat, "ScoreA", "death", cutoff = youdenA$threshold, file = "/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/ScoreA_hist.png")
plot_score_hist(dat, "ScoreB", "death", cutoff = youdenB$threshold, file = "/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/ScoreB_hist.png")
plot_score_hist(dat, "ScoreC", "death", cutoff = youdenC$threshold, file = "/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/ScoreC_hist.png")
plot_score_hist(dat, "ScoreD", "death", cutoff = youdenD$threshold, file = "/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/ScoreD_hist.png")


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
    arrange(Score)

  # Total 행 추가
  total <- d %>% summarise(
    Score = NA_real_,
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
score_summary_table(dat, "ScoreA", "death")
score_summary_table(dat, "ScoreB", "death")
score_summary_table(dat, "ScoreC", "death")
score_summary_table(dat, "ScoreD", "death")



# -- time-dependent ROC -- #
AUC_TIMES <- 1:8
PLOT_TIMES <- 1:8

base_vars <- c("age65", "PLT100", "IgM7", "LDH2", "ALB3.5")
base_vars2 <- c("age65", "PLT100", "IgM7", "LDH2", "ALB3.5", "B_Sx")

model_sets <- list(
  "Our model"        = base_vars,
  "IPSS"        = "IPSS",
  "RIPSS"       = "RIPSS",
  "Our score + B_Sx"         = base_vars2
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


# Integrated AUC 계산

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
    groupA = 
      case_when(
        ScoreA<=2 ~"Low",
        ScoreA<=6 ~ "Intermediate",
        T ~ "High"),
    groupB = 
      case_when(
        ScoreB<=10 ~"Low",
        ScoreB<=15 ~ "Intermediate",
        T ~ "High")
  ) %>% 
  mutate(groupA = factor(groupA, levels=c("Low","Intermediate","High")),
         groupB = factor(groupB, levels=c("Low","Intermediate","High")))

table(survival$first_regimen)


# 변경 전: sapply로 character vector 반환 → sub()로 파싱 필요
# 변경 후: data.frame으로 strata + value 반환 → join 가능


plot_regimen <- function(dat, regimen_name, group_var = "groupA"){
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


summary_regimen <- function(dat, regimen_name, group_var = "groupA"){
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

# 그림 (2x2 합쳐서 저장)
plot_list <- lapply(regimens, function(r){
  plot_regimen(survival %>% filter(first_regimen == r), as.character(r), group_var = "groupB")
})

OUTPUT_DIR="/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Lpl/Figure/"
dev.new()
png(paste0(OUTPUT_DIR, "survival_by_regimen_2x1_IPSSextras.png"),
    width = 16, height = 16, units = "in", res = 300)
arrange_ggsurvplots(plot_list, ncol = 2, nrow = 2, print = TRUE)
dev.off()

# 통계 테이블 (regimen별로 gt 두 개씩)
for (r in regimens){
  res <- summary_regimen(survival %>% filter(first_regimen == r), as.character(r), group_var = "groupB")
  print(res$surv)
  print(res$hr)
}


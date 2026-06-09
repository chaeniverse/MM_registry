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
library(prodlim)
library(tidycmprsk)
library(ggsurvfit)
library(boot)
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
  "진단일","last_fu", "death", "SUBG_BR_OTHER", "ASCT","No")) %>%
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
    death_yr = death_day/365.25,
    death_mo = death_day/30.4375) %>%
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

OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Baseline'
# -- Baseline characteristics table -- #
# crf %>% filter(!is.na(death_day)) %>%
#   select(c("age", "age65", "age75", "sex", "ECOG performance status < 2", "B_Sx",     
#   "LNE", "HS", "spleen", "liver", "IgM7", "sPEP", "ANC < 1000", "Hb", "Hb11.5", "PLT", "PLT100", "LDH",
#   "LDH250", "ALB", "ALB3.5", "B2MG_cont", "B2MG_cat", "B2MG4", "IPSS", "RIPSS", "MYD88", "CXCR4","death"))  %>% 
#   tbl_summary(death,
#     label = list(
#       age ~ "Age (years)",
#       age65 ~ "Age > 65",
#       age75 ~ "Age > 75",
#       sex ~ "Sex",
#       `ECOG performance status < 2` ~ "ECOG performance status < 2",
#       LNE ~ "Lymphadenopathy",
#       HS ~ "Hepatosplenomegaly",
#       Hb ~ "Hemoglobin (g/dL)",
#       Hb11.5 ~ "Hemoglobin <= 11.5 g/dL",
#       `ANC < 1000` ~ "ANC < 1000",
#       PLT ~ "Platelet",
#       PLT100 ~ "Platelet <= 100 x10^9/L",
#       ALB ~ "Albumin (g/dL)",
#       ALB3.5 ~ "Albumin < 3.5 g/dL",
#       LDH ~ "LDH (IU/L)",
#       LDH250 ~ "LDH > 250 IU/L",
#       IgM7 ~ "IgM > 7.0 g/dL",
#       B2MG_cont ~ "Beta-2 microglobulin (mg/L)",
#       B2MG_cat ~ "Beta-2 microglobulin > 3 mg/L",
#       B2MG4 ~ "Beta-2 microglobulin >= 4 mg/L",
#       IPSS ~ "IPSS-WM",
#       RIPSS ~ "rIPSS-WM",
#       MYD88 ~ "MYD88 mutation",
#       CXCR4 ~ "CXCR4 mutation",
#       sPEP ~ "Serum protein electrophoresis (g/dL)"
#     ),
#     type = list(
#       all_continuous() ~ "continuous"
#     ),
#     statistic = list(                   
#       all_continuous() ~ "{median} ({p25}-{p75})",
#       all_categorical() ~ "{n} ({p})"
#     ),
#     digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
#     missing = "ifany",
#     missing_text = "Missing",
#     missing_stat = "{N_miss} ({p_miss})"
#   ) %>%
#   add_overall() %>%
#   bold_labels() %>%
#   modify_header(label = "**Characteristic**") %>%
#   modify_caption("**Table. Baseline Characteristics (N = {N})**") %>%
#   as_flex_table() %>%
#   flextable::save_as_docx(path = file.path(OUTPUT_DIR, "[26-05-21] Baseline by death.docx"))


# --- 분석용 데이터 준비 ---
dat <- crf %>%
  select(
    c("1L_1", "TLT12", "age65", "age75", "sex", `ECOG performance status < 2`, "B_Sx", 
    "LNE", "HS", "IgM7", "ANC < 1000", "PLT100", "LDH250",
    "ALB3.5", "B2MG_cat", "B2MG4", "Hb11.5", "MYD88", "CXCR4", 
    "IPSS", "RIPSS",
    "진단일","last_fu", "death","death_day","death_yr", "SUBG_BR_OTHER", "ASCT", "1L_start","No", "death_mo")) %>%
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
  ) %>%
  mutate(outcome_1L =
    case_when(is.na(SUBG_BR_OTHER) ~ 0,
              TRUE ~ 1)
  )


# -- Figure. Survival curve (Whole) -- #
survival <- dat %>% filter(!is.na(death_yr))

OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure/'
file_name <- "survival_whole.png"

fit <- survfit(Surv(death_yr, death) ~ 1, data = survival)

# png(paste0(OUTPUT_DIR, file_name), width = 12, height = 10, units = "in", res = 300)
# font_size <- 12
# p <- ggsurvplot(
#   fit, data           = survival,
#   surv.median.line    = "hv",
#   risk.table          = TRUE,
#   # tables.col          = "strata",
#   tables.y.text       = FALSE,
#   conf.int            = FALSE,
#   xlim                = c(0, 10),
#   xlab                = "Time (years)",
#   ylab                = "Survival Probability (%)",
#   legend.title        = "",
#   # legend.labs         = levels(survival[[group_col]]),
#   tables.height       = 0.25,
#   break.time.by       = 1,
#   risk.table.fontsize = 5,
#   palette             = pal_lancet()(3)[c(2)],
#   tables.theme        = theme_cleantable()
# )
# p$plot <- p$plot +
#   scale_y_continuous(labels = function(x) x * 100) +
#   theme(axis.title.x  = element_text(size = font_size),
#         axis.text.x = element_text(size = font_size),
#         axis.title.y  = element_text(size = font_size),
#         axis.text.y = element_text(size = font_size))
# print(p)
# dev.off()



# -- Cumulative incidence (Whole) -- #
data_now <- dat %>%
    mutate(
      status = case_when(
        outcome_1L == 1 ~ 1, 
        death == 1 & outcome_1L == 0 ~ 2, 
        death == 0 & outcome_1L == 0 ~ 0, 
        TRUE ~ NA_real_)) %>%
    mutate(
      time_yr = case_when(
        status == 1 ~ (`1L_start` - 진단일)/365.25,
        status == 0 ~ (last_fu - 진단일)/365.25,
        status == 2 ~ (last_fu - 진단일)/365.25
      )
    ) |> as.data.table()

data_now$status %>% as.factor %>% summary
sum(is.na(data_now$time_yr))
sum(data_now$time_yr<0)

data_now <- data_now %>% 
  mutate(time_yr = as.numeric(time_yr),
         status = factor(status))

ci.model <- cmprsk::cuminc(ftime = data_now$time_yr, 
                    fstatus = data_now$status, 
                    cencode = "0")


# 6-year CIF
est_6  <- timepoints(ci.model, times = 6)$est[1, 1]
se_6   <- sqrt(timepoints(ci.model, times = 6)$var[1, 1])
lb_6   <- est_6^(exp(-1.96 * se_6 / (est_6 * log(est_6))))
ub_6   <- est_6^(exp( 1.96 * se_6 / (est_6 * log(est_6))))

# 3-year CIF
est_3  <- timepoints(ci.model, times = 3)$est[1, 1]
se_3   <- sqrt(timepoints(ci.model, times = 3)$var[1, 1])
lb_3   <- est_3^(exp(-1.96 * se_3 / (est_3 * log(est_3))))
ub_3   <- est_3^(exp( 1.96 * se_3 / (est_3 * log(est_3))))

# 하나의 data.frame으로
ci_tbl <- data.frame(
  Timepoint = c("6-year", "3-year"),
  Time_yr   = c(6, 3),
  CIF       = c(round(est_6  * 100, 1), round(est_3  * 100, 1)),
  Lower     = c(round(lb_6   * 100, 1), round(lb_3   * 100, 1)),
  Upper     = c(round(ub_6   * 100, 1), round(ub_3   * 100, 1))
)

ci_tbl |>
  mutate(`CIF (%) (95% CI)` = sprintf("%.1f (%.1f–%.1f)", CIF, Lower, Upper)) |>
  select(Timepoint, Time_yr, `CIF (%) (95% CI)`) |>
  gt() |>
  cols_label(
    Timepoint = "Time point",
    Time_yr   = "Time (yr)"
  ) 

# plot
font_size <- 12

common_theme <- theme(
  axis.title = element_text(size = font_size),
  axis.text  = element_text(size = font_size),
  axis.line  = element_line(linewidth = 0.5),
  axis.ticks = element_line(linewidth = 0.5),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  plot.margin = margin(5, 5, 5, 5),
  legend.position = "none"
)

default_gray <- theme_gray()$line$colour

# p <- tidycmprsk::cuminc(Surv(time_yr, status) ~ 1, data = data_now) |>
#   ggcuminc(outcome = "1", linewidth = 1) +
#   add_confidence_interval() +
#   add_risktable(
#     risktable_stats = "n.risk",                  # at risk 수만
#     stats_label     = list(n.risk = "Number at risk"),
#     risktable_group = "strata",
#     times           = 0:6,  
#     size            = 5,
#     theme = theme_risktable_default(
#       axis.text.y.size = 16,
#       plot.title.size  = font_size
#     )
#   ) +  
#   scale_y_continuous(breaks = seq(0, 1, 0.2), labels = function(x) x * 100, limits = c(0, 1)) +
#   scale_x_continuous(breaks = 0:6, limits = c(0, 6), oob = scales::oob_keep) +
#   labs(x = "Time (years)", y = "Cumulative Incidence (%)") +
#   geom_vline(xintercept = 6,            linetype = "dashed",
#              color = default_gray, linewidth = 0.5) +
#   geom_vline(xintercept = 3, linetype = "dashed",
#              color = default_gray, linewidth = 0.5) +
#   theme_classic(base_size = font_size) +     
#   common_theme                                

# # --- layer 직접 수정으로 색 적용 ---
# p$layers[[1]]$aes_params$colour <- pal_lancet()(3)[2]
# p$layers[[2]]$aes_params$fill   <- pal_lancet()(3)[2]

# # --- risk table 포함해 하나의 객체로 합친 뒤 저장 ---
# p_built <- ggsurvfit_build(p)
# ggsave(
#   filename = paste0(OUTPUT_DIR, "cuminc_whole.png"),
#   plot = p_built, width = 12, height = 10, units = "in", dpi = 300
# )

# -- Score 계산 -- # 
survival <- survival %>% rename(first_regimen = `1L_1`) %>%
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
) %>%
  mutate(Score_RIPSS_cat = case_when(
    Score_RIPSS == 0 ~ '1_VL',
    Score_RIPSS == 1 ~ '2_Low',
    Score_RIPSS == 2 ~ '3_Int',
    Score_RIPSS >= 3 ~ '4_High | 5_VH',
    TRUE ~ NA_character_
  )) %>%
  mutate(Score_RIPSS_cat = factor(Score_RIPSS_cat, levels = c('1_VL', '2_Low', '3_Int', '4_High | 5_VH')))

# -- Multivariable results (Cox PH) -- #
base_vars <- c("Score_RIPSS_cat", "sex", "ECOG performance status < 2", "B_Sx", "LNE", "HS", "ANC < 1000", "Hb11.5", "PLT100", "IgM7")
# multi_step("None", survival, base_vars) 


# -- Uni results (Cox PH) -- #
uni_list <- lapply(base_vars, function(var){
  model <- coxph(as.formula(paste("Surv(death_yr, death) ~", paste0("`", var, "`"))),
                 data = survival)
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

# uni_list %>%
#   bind_rows() %>%
#   mutate(
#     `HR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", uni_HR, uni_LCL, uni_UCL),
#     `P-value` = ifelse(uni_p < 0.001, "<.001", sprintf("%.3f", uni_p))
#   ) %>%
#   select(Variable = term, `HR (95% CI)`, `P-value`) %>%
#   gt() %>%
#   tab_header(title = "Univariable Cox PH results (outcome = death)")

# -- RIPSS-Augmented score 계산 -- #
survival <- survival %>% 
  mutate(
    `Score_RIPSS_augmented` = 
      case_when(Score_RIPSS_cat == "1_VL" ~ 0, 
                Score_RIPSS_cat == "2_Low" ~ 1, 
                Score_RIPSS_cat == "3_Int" ~ 1.5, 
                Score_RIPSS_cat == "4_High | 5_VH" ~ 2, 
                TRUE ~ NA_real_) +
      case_when(B_Sx == 1 ~ 1, 
                B_Sx == 0 ~ 0, 
                TRUE ~ NA_real_) +
      case_when(`ANC < 1000` == 1 ~ 1, 
                `ANC < 1000` == 0 ~ 0, 
                TRUE ~ NA_real_)
)

# -- Score별 event rate table -- #
# score_summary_table(survival, "Score_IPSS", "death")
# score_summary_table(survival, "Score_RIPSS", "death")
# score_summary_table(survival, "Score_RIPSS_augmented", "death")

# -- score groups -- #
survival <- survival %>% 
  mutate(
    group_IPSS = 
      case_when(
        Score_IPSS >= 3                ~ "High",
        Score_IPSS == 2                ~ "Intermediate",
        Score_IPSS <= 1 & age65 == 1   ~ "Intermediate",
        Score_IPSS <= 1 & age65 == 0   ~ "Low",
        T ~ NA_character_),
    group_RIPSS = 
      case_when(
        Score_RIPSS >= 3 ~ "High",
        Score_RIPSS == 2 ~ "Intermediate",
        Score_RIPSS <= 1 ~"Low",
        T ~ NA_character_),
    group_RIPSS_seg = 
      case_when(
        Score_RIPSS == 0 ~"VL",
        Score_RIPSS == 1 ~"Low",
        Score_RIPSS == 2 ~"Intermediate",
        Score_RIPSS == 3 ~"High",
        Score_RIPSS >= 4 ~ "VH",
        T ~ NA_character_),
    group_RIPSS_augmented = 
      case_when(
        Score_RIPSS_augmented<=1 ~"Low",
        Score_RIPSS_augmented<=2 ~ "Intermediate",
        Score_RIPSS_augmented>=2.5 ~ "High",
        T ~ NA_character_)
  ) %>% 
  mutate(group_IPSS = factor(group_IPSS, levels=c("Low","Intermediate","High")),
         group_RIPSS = factor(group_RIPSS, levels=c("Low","Intermediate","High")),
         group_RIPSS_seg = factor(group_RIPSS_seg, levels=c("VL","Low","Intermediate","High","VH")),
         group_RIPSS_augmented = factor(group_RIPSS_augmented, levels=c("Low","Intermediate","High")) )


# IPSS score group별 event rate table
event_tab <- function(group){
  tab <- table(survival[[group]], survival$death)

  report <- data.frame(
    Group = rownames(tab),
    N     = rowSums(tab),
    Event = tab[, "1"],
    Rate  = sprintf("%.1f%%", tab[, "1"] / rowSums(tab) * 100)
  )
  report %>%
    gt()}
# event_tab("group_IPSS");event_tab("group_RIPSS");event_tab("group_RIPSS_augmented")

# -- Score histogram with cutoff -- #
lancet_cols <- pal_lancet()(2)
OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure/'
# plot_score_histogram_with_cutoff(survival, "Score_RIPSS_augmented", OUTPUT_DIR, lancet_cols)


# -- Figure. time-dependent AUC 그리기 -- #

model_sets <- list(
  "IPSS score"            = c("Score_IPSS"),
  "RIPSS score"           = c("Score_RIPSS"),
  "RIPSS-augmented score" = c("Score_RIPSS_augmented")
)

dat_cc <- na.omit(survival[, c("death_yr", "death", unlist(model_sets))])

score_cols <- c(IPSS            = "Score_IPSS",
                RIPSS           = "Score_RIPSS",
                RIPSS_augmented = "Score_RIPSS_augmented")

time_points <- c(2, 4, 6, 8, 10)
# time_points <- 2:10
# time_points <- 3:14
tmax_val <- max(time_points)


calc_iauc <- function(d, marker_col, times = time_points, tmax = tmax_val) {
  out <- tryCatch({
    roc.dat <- timeROC(T = d$death_yr, delta = d$death,
                       marker = d[[marker_col]], cause = 1,
                       weighting = "marginal", times = times)
    auc <- roc.dat$AUC

    stopifnot(length(auc) == length(times))   # ← 여기

    sf <- summary(survfit(Surv(death_yr, death) ~ 1, data = d), times = times)
    temp <- rep(NA_real_, length(times))
    temp[match(sf$time, times)] <- sf$surv

    ok <- is.finite(auc) & is.finite(temp)
    if (sum(ok) < 2) return(NA_real_)
    IntAUC(auc[ok], times[ok], temp[ok], tmax)
  }, error = function(e) NA_real_)
  out
}

iauc_boot <- function(data, indices,
                      times = time_points, tmax = tmax_val,
                      cols = score_cols) {
  d <- data[indices, ]
  ia <- sapply(cols, function(cc) calc_iauc(d, cc, times, tmax))
  c(IPSS = ia[["IPSS"]],
    RIPSS = ia[["RIPSS"]],
    RIPSS_augmented = ia[["RIPSS_augmented"]]
  )
}

set.seed(1234)
boot_res <- boot(dat_cc, statistic = iauc_boot, R = 1000)

boot_dat <- as.data.frame(boot_res$t)
ok <- complete.cases(boot_dat[, c(1,2,3)])
n  <- sum(ok)
sum(boot_dat[[3]][ok] < 0.5) / n
sum(boot_dat[[3]][ok] < boot_dat[[1]][ok]) / n
sum(boot_dat[[3]][ok] < boot_dat[[2]][ok]) / n


tbl <- lapply(1:3, function(i) {
  ci <- boot.ci(boot_res, type = "perc", index = i)$percent[4:5]
  tibble(
    Score = names(model_sets)[i],
    iAUC  = boot_res$t0[i],   # 원본 데이터에서의 iAUC = 점추정치
    lower = ci[1],
    upper = ci[2]
  )
}) %>% bind_rows() 

OUTPUT_DIR <- "/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure"

plot_timeAUC_overlay(model_sets, AUC_TIMES = 2:10,
                     OUTPUT_DIR = OUTPUT_DIR, file_name = "timeAUC_overlay.png", data = survival, ci_tbl = tbl)


# -- time-dependent AUC 그리기 (BR vs Other) -- #
BR_group <- survival %>% filter(SUBG_BR_OTHER == 1)
Other_group <- survival %>% filter(SUBG_BR_OTHER == 2)

BR_cc <- na.omit(BR_group[, c("death_yr", "death", unlist(model_sets))])
Other_cc <- na.omit(Other_group[, c("death_yr", "death", unlist(model_sets))])

set.seed(1234)
boot_BR <- boot(BR_cc, statistic = iauc_boot, R = 1000)
set.seed(1234)
boot_Other <- boot(Other_cc, statistic = iauc_boot, R = 1000)
boot_dat <- as.data.frame(boot_BR$t)
ok <- complete.cases(boot_dat[, c(1,2,3)])
n  <- sum(ok)
sum(boot_dat[[3]][ok] < 0.5) / n # 0.001
sum(boot_dat[[3]][ok] < boot_dat[[1]][ok]) / n # 0.029
sum(boot_dat[[3]][ok] < boot_dat[[2]][ok]) / n # 0.184


boot_dat <- as.data.frame(boot_Other$t)
ok <- complete.cases(boot_dat[, c(1,2,3)])
n  <- sum(ok)
sum(boot_dat[[3]][ok] < 0.5) / n # 0.069
sum(boot_dat[[3]][ok] < boot_dat[[1]][ok]) / n # 0.224
sum(boot_dat[[3]][ok] < boot_dat[[2]][ok]) / n # 0.112

tbl_BR <- lapply(1:3, function(i) {
  ci <- boot.ci(boot_BR, type = "perc", index = i)$percent[4:5]
  tibble(
    Score = names(model_sets)[i],
    iAUC  = boot_BR$t0[i],   # 원본 데이터에서의 iAUC = 점추정치
    lower = ci[1],
    upper = ci[2]
  )
}) %>% bind_rows() 

tbl_Other <- lapply(1:3, function(i) {
  ci <- boot.ci(boot_Other, type = "perc", index = i)$percent[4:5]
  tibble(
    Score = names(model_sets)[i],
    iAUC  = boot_Other$t0[i],   # 원본 데이터에서의 iAUC = 점추정치
    lower = ci[1],
    upper = ci[2]
  )
}) %>% bind_rows() 

plot_timeAUC_overlay(model_sets, AUC_TIMES = 2:10,
                     OUTPUT_DIR = OUTPUT_DIR, file_name = "timeAUC_overlay_BR.png", data = BR_group, ci_tbl = tbl_BR)
plot_timeAUC_overlay(model_sets, AUC_TIMES = 2:10,
                     OUTPUT_DIR = OUTPUT_DIR, file_name = "timeAUC_overlay_Other.png", data = Other_group, ci_tbl = tbl_Other)

# -- survival curve 그리기 by score group -- #
fit1 <- survfit(Surv(death_yr, death) ~ group_IPSS, data = survival)
fit2 <- survfit(Surv(death_yr, death) ~ group_RIPSS, data = survival)
fit3 <- survfit(Surv(death_yr, death) ~ group_RIPSS_augmented, data = survival)
fit4 <- survfit(Surv(death_yr, death) ~ group_RIPSS_seg, data = survival)

sd1 <- survdiff(Surv(death_yr, death) ~ group_IPSS,            data = survival)
sd2 <- survdiff(Surv(death_yr, death) ~ group_RIPSS,           data = survival)
sd3 <- survdiff(Surv(death_yr, death) ~ group_RIPSS_augmented, data = survival)
sd4 <- survdiff(Surv(death_yr, death) ~ group_RIPSS_seg,       data = survival)

sd_list <- list(IPSS = sd1, RIPSS = sd2, RIPSS_augmented = sd3, RIPSS_seg = sd4)

logrank_p <- sapply(sd_list, function(s) {
  ifelse(s$pvalue < .001, "<.001", sprintf("%.3f", s$pvalue))
})
logrank_p %>% as.data.frame() %>% gt

OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure'

plot_survival_by_group(fit1, survival, OUTPUT_DIR, file_name = "/survival_by_group_IPSS.png", group_col = "group_IPSS")
plot_survival_by_group(fit2, survival, OUTPUT_DIR, file_name = "/survival_by_group_RIPSS.png", group_col = "group_RIPSS")
plot_survival_by_group(fit3, survival, OUTPUT_DIR, file_name = "/survival_by_group_RIPSS_augmented.png", group_col = "group_RIPSS_augmented")
plot_survival_by_group(fit4, survival, OUTPUT_DIR, file_name = "/survival_by_group_RIPSS_seg.png", group_col = "group_RIPSS_seg", palette = pal_lancet()(5)[c(1, 3, 2, 4, 5)])

# Survival results
km_summary_by_group(survival, "group_IPSS", "death_yr", "death")
cox_hr_by_group(survival, "group_IPSS", "death_yr", "death")
km_summary_by_group(survival, "group_RIPSS", "death_yr", "death")
cox_hr_by_group(survival, "group_RIPSS", "death_yr", "death")
km_summary_by_group(survival, "group_RIPSS_augmented", "death_yr", "death")
cox_hr_by_group(survival, "group_RIPSS_augmented", "death_yr", "death")
km_summary_by_group(survival, "group_RIPSS_seg", "death_yr", "death")
cox_hr_by_group(survival, "group_RIPSS_seg", "death_yr", "death")

vars <- c("IPSS"            = "group_IPSS",
          "RIPSS"           = "group_RIPSS",
          "RIPSS-augmented" = "group_RIPSS_augmented",
          "RIPSS seg" = "group_RIPSS_seg")

tbl <- do.call(rbind, lapply(names(vars), function(sys) {
  t <- table(survival[[ vars[sys] ]])
  data.frame(System = sys,
             Group  = names(t),
             N      = as.integer(t),
             row.names = NULL)
}))

tbl |>
  gt(groupname_col = "System") |>
  cols_label(Group = "Risk group", N = "N") |>
  summary_rows(
    groups = everything(),
    columns = N,
    fns = list(Total = ~sum(.)),
    fmt = ~fmt_number(., decimals = 0)
  )



# -- survival curve 그리기 by score group in BR group -- #
fit1 <- survfit(Surv(death_yr, death) ~ group_IPSS, data = BR_group)
fit2 <- survfit(Surv(death_yr, death) ~ group_RIPSS, data = BR_group)
fit3 <- survfit(Surv(death_yr, death) ~ group_RIPSS_augmented, data = BR_group)

sd1 <- survdiff(Surv(death_yr, death) ~ group_IPSS,            data = BR_group)
sd2 <- survdiff(Surv(death_yr, death) ~ group_RIPSS,           data = BR_group)
sd3 <- survdiff(Surv(death_yr, death) ~ group_RIPSS_augmented, data = BR_group)

sd_list <- list(IPSS = sd1, RIPSS = sd2, RIPSS_augmented = sd3)

logrank_p <- sapply(sd_list, function(s) {
  ifelse(s$pvalue < .001, "<.001", sprintf("%.3f", s$pvalue))
})
logrank_p %>% as.data.frame() %>% gt

plot_survival_by_group(fit1, BR_group, OUTPUT_DIR, file_name = "/survival_by_group_IPSS_BR.png", group_col = "group_IPSS")
plot_survival_by_group(fit2, BR_group, OUTPUT_DIR, file_name = "/survival_by_group_RIPSS_BR.png", group_col = "group_RIPSS")
plot_survival_by_group(fit3, BR_group, OUTPUT_DIR, file_name = "/survival_by_group_RIPSS_augmented_BR.png", group_col = "group_RIPSS_augmented")


km_summary_by_group(BR_group, "group_IPSS", "death_yr", "death")
cox_hr_by_group(BR_group, "group_IPSS", "death_yr", "death")
km_summary_by_group(BR_group, "group_RIPSS", "death_yr", "death")
cox_hr_by_group(BR_group, "group_RIPSS", "death_yr", "death")
km_summary_by_group(BR_group, "group_RIPSS_augmented", "death_yr", "death")
cox_hr_by_group(BR_group, "group_RIPSS_augmented", "death_yr", "death")




tbl <- do.call(rbind, lapply(names(vars), function(sys) {
  t <- table(BR_group[[ vars[sys] ]])
  data.frame(System = sys,
             Group  = names(t),
             N      = as.integer(t),
             row.names = NULL)
}))

tbl |>
  gt(groupname_col = "System") |>
  cols_label(Group = "Risk group", N = "N")


# -- survival curve 그리기 by score group in Other group -- #
fit1 <- survfit(Surv(death_yr, death) ~ group_IPSS, data = Other_group)
fit2 <- survfit(Surv(death_yr, death) ~ group_RIPSS, data = Other_group)
fit3 <- survfit(Surv(death_yr, death) ~ group_RIPSS_augmented, data = Other_group)

sd1 <- survdiff(Surv(death_yr, death) ~ group_IPSS,            data = Other_group)
sd2 <- survdiff(Surv(death_yr, death) ~ group_RIPSS,           data = Other_group)
sd3 <- survdiff(Surv(death_yr, death) ~ group_RIPSS_augmented, data = Other_group)

sd_list <- list(IPSS = sd1, RIPSS = sd2, RIPSS_augmented = sd3)

logrank_p <- sapply(sd_list, function(s) {
  ifelse(s$pvalue < .001, "<.001", sprintf("%.3f", s$pvalue))
})
logrank_p %>% as.data.frame() %>% gt

plot_survival_by_group(fit1, Other_group, OUTPUT_DIR, file_name = "/survival_by_group_IPSS_Other.png", group_col = "group_IPSS")
plot_survival_by_group(fit2, Other_group, OUTPUT_DIR, file_name = "/survival_by_group_RIPSS_Other.png", group_col = "group_RIPSS")
plot_survival_by_group(fit3, Other_group, OUTPUT_DIR, file_name = "/survival_by_group_RIPSS_augmented_Other.png", group_col = "group_RIPSS_augmented")

km_summary_by_group(Other_group, "group_IPSS", "death_yr", "death")
cox_hr_by_group(Other_group, "group_IPSS", "death_yr", "death")
km_summary_by_group(Other_group, "group_RIPSS", "death_yr", "death")
cox_hr_by_group(Other_group, "group_RIPSS", "death_yr", "death")
km_summary_by_group(Other_group, "group_RIPSS_augmented", "death_yr", "death")
cox_hr_by_group(Other_group, "group_RIPSS_augmented", "death_yr", "death")


tbl <- do.call(rbind, lapply(names(vars), function(sys) {
  t <- table(Other_group[[ vars[sys] ]])
  data.frame(System = sys,
             Group  = names(t),
             N      = as.integer(t),
             row.names = NULL)
}))

tbl |>
  gt(groupname_col = "System") |>
  cols_label(Group = "Risk group", N = "N")



# -- survival curve 그리기 by BR/Other group -- #
OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure'
fit <- survfit(Surv(death_yr, death) ~ SUBG_BR_OTHER, data = survival)
plot_survival_by_group(fit, survival, OUTPUT_DIR, file_name = "/survival_by_group_BR_OTHER.png", group_col = "SUBG_BR_OTHER", palette = pal_lancet()(3)[c(1,2)])

sd <- survdiff(Surv(death_yr, death) ~ SUBG_BR_OTHER, data = survival)
print(ifelse(sd$pvalue < .001, "<.001", sprintf("%.3f", sd$pvalue)))

# Survival results
km_summary_by_group(survival, "SUBG_BR_OTHER", "death_yr", "death")
cox_hr_by_group(survival, "SUBG_BR_OTHER", "death_yr", "death")



# -- time-dependent ROC (2,4,6,8 years) -- #
OUTPUT_DIR <- '/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure'
plot_timeROC_overlay(data = survival, model_sets, AUC_TIMES = c(2,4,6,8,10), PLOT_TIMES = c(2,4,6,8,10), OUTPUT_DIR = OUTPUT_DIR)


# 공통 complete-case 데이터
all_vars <- unique(unlist(model_sets))
dat_cc   <- survival[, c("death", "death_yr", all_vars)] %>% na.omit()

AUC_TIMES <- c(2, 4, 6, 8, 10)

auc_boot <- function(data, indices, times = AUC_TIMES) {
  d <- data[indices, ]
  tryCatch({
    tr_ripss <- timeROC(T = d$death_yr, delta = d$death,
                        marker = d$Score_RIPSS, cause = 1,
                        weighting = "marginal", times = times)
    tr_aug   <- timeROC(T = d$death_yr, delta = d$death,
                        marker = d$Score_RIPSS_augmented, cause = 1,
                        weighting = "marginal", times = times)
    c(tr_ripss$AUC, tr_aug$AUC)
  }, error = function(e) rep(NA_real_, 2 * length(times)))
}

set.seed(1234)
boot_auc <- boot(dat_cc, statistic = auc_boot, R = 1000)
boot_dat <- as.data.frame(boot_auc$t)
head(boot_dat)

nt <- length(AUC_TIMES)
colnames(boot_dat) <- c(paste0("RIPSS_t", AUC_TIMES),
                        paste0("AUG_t",   AUC_TIMES))

# 시점별 단측검정
#   H0: AUC_aug <= AUC_ripss   vs   H1: AUC_aug > AUC_ripss
#   p = (augmented < RIPSS 인 복제 비율)
res <- lapply(seq_len(nt), function(j) {
  ripss <- boot_dat[[j]]          # RIPSS AUC  (j번째 시점)
  aug   <- boot_dat[[j + nt]]     # augmented AUC
  ok <- is.finite(ripss) & is.finite(aug)
  pval <- sum(aug[ok] < ripss[ok]) / sum(ok)
  data.frame(
    time      = AUC_TIMES[j],
    AUC_RIPSS = boot_auc$t0[j],          # 원본 데이터 점추정치
    AUC_AUG   = boot_auc$t0[j + nt],
    n_valid   = sum(ok),
    p_value   = ifelse(pval < 0.001, "<.001", sprintf("%.3f", pval))
  )
})
res <- do.call(rbind, res)
res


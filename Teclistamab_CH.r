# -- library -- #
library(gtsummary)
library(kableExtra); library(MatchIt); 
# install.packages("optmatch")
library(optmatch); 
library(MatchThem)
library(cobalt); library(survival); library(survminer); library(ggfortify)
library(ggsci); library(flextable); library(officer); library(mice)
library(forestplot); library(forestploter); library(ipw); library(survey)
library(ggplotify); library(patchwork); library(EValue); library(tableone)
library(adjustedCurves)
library(tidyverse)
library(readxl)
library(plotly)
library(lubridate)
library(ggsurvfit)
library(purrr)
library(broom)
library(dplyr)
library(stringr)
library(data.table)
library(gt)
library(gtsummary)
library(dplyr)

# -- setting -- #
setwd("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/Data")
select = dplyr::select

# -- source -- #
source("C:/Users/chaehyun/Dropbox/Work/PIPET/데이터팀_표준화/Survival_plot/function.R")
source("C:/Users/chaehyun/Projects/MM_registry/love_plot_code.r")

set.seed(0619)


# option
my_theme <-
  list(
    "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
    "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
    # "tbl_summary-arg:statistic" = list(all_continuous() ~ "{mean} ± {sd} ({median} [{p0}-{p100}])",
    #                                    all_categorical() ~ "{n} ({p}%)")
    "tbl_summary-arg:statistic" = list(all_continuous() ~ "{median} ({p0}-{p100})",
                                       all_categorical() ~ "{n} ({p}%)")
  )
set_gtsummary_theme(my_theme)
theme_gtsummary_compact()

# date_pfs에서 refactory 한명 DATE 업데이트 함. (2024-04-09)
data_soc <- read_excel('SOC cohort_n87 data_0111.xlsx', sheet=1, skip = 1) %>%
 filter(Group == 2) %>% 
   dplyr::select(ISS_DX, SEX, CTX_S_DATE, BIRTH, CTX_LINE, HB_CTX, PLT_CTX, GFR_CTX, LDH_HIGH1_CTX, VS_PFS_CTX, DATE_PFS_CTX, PRE_CTX_R, VS, DATE_LAST, RESP_CTX, PID, DATE_DX, LDH_CTX, CREAT_CTX, ANC_CTX, ALC_CTX, Heavychain, Lightchain, FGFR3_IGH_DX_VALUE, MAF_IGH_DX_VALUE, CCND1_IGH_DX_VALUE, RB1_DX_VALUE, TP53_DX_VALUE, `1Q_DX`) %>%
                    mutate(CTX_S_DATE = as.Date(CTX_S_DATE),
                           BIRTH = as.Date(BIRTH),
                           DATE_PFS_CTX = as.Date(DATE_PFS_CTX),
                           DATE_LAST = as.Date(DATE_LAST)) %>%
                    mutate(AGE = CTX_S_DATE - BIRTH) %>%
                    mutate(AGE = as.numeric(AGE) / 365.25) %>%
                    mutate(AGE_num = AGE) %>%
                    mutate(
                      `Time from diagnosis to index date` = as.numeric(interval(DATE_DX, CTX_S_DATE) %/% months(1)), 
                      HB_CTX = as.numeric(HB_CTX),
                      PLT_CTX = as.numeric(PLT_CTX),
                      GFR_CTX = as.numeric(GFR_CTX),
                      VS_PFS_CTX = as.numeric(VS_PFS_CTX),
                      PRE_CTX_R = as.numeric(PRE_CTX_R),
                      LDH_CTX = as.numeric(LDH_CTX),
                      CREAT_CTX = as.numeric(CREAT_CTX),
                      ANC_CTX = as.numeric(ANC_CTX),
                      ALC_CTX = as.numeric(ALC_CTX)) %>% 
                    mutate(LDH_CTX = if_else(LDH_CTX > 225, 'high', 'low'),
                          ANC_CTX = if_else(ANC_CTX < 1.0, 1,0)) %>% 
                    mutate(FGFR3_IGH_DX_VALUE = case_when(is.na(FGFR3_IGH_DX_VALUE) ~ NA_real_,    
                                                        FGFR3_IGH_DX_VALUE > 0 ~ 1,            
                                                        FGFR3_IGH_DX_VALUE == 0 ~ 0 ),
                          TP53_DX_VALUE = case_when(is.na(TP53_DX_VALUE) ~ NA_real_,    
                                                          TP53_DX_VALUE > 0 ~ 1,            
                                                          TP53_DX_VALUE == 0 ~ 0 ),
                          MAF_IGH_DX_VALUE = case_when(is.na(MAF_IGH_DX_VALUE) ~ NA_real_,    
                                                          MAF_IGH_DX_VALUE > 0 ~ 1,            
                                                          MAF_IGH_DX_VALUE == 0 ~ 0 ),
                          CCND1_IGH_DX_VALUE = case_when(is.na(CCND1_IGH_DX_VALUE) ~ NA_real_,    
                                                          CCND1_IGH_DX_VALUE > 0 ~ 1,            
                                                          CCND1_IGH_DX_VALUE == 0 ~ 0 ),
                          RB1_DX_VALUE = case_when(is.na(RB1_DX_VALUE) ~ NA_real_,    
                                                          RB1_DX_VALUE > 0 ~ 1,            
                                                          RB1_DX_VALUE == 0 ~ 0 ),
                          `1Q_DX` = case_when(is.na(`1Q_DX`) ~ NA_real_,    
                                                          `1Q_DX` > 0 ~ 1,            
                                                          `1Q_DX` == 0 ~ 0 )) %>% 
                      mutate(Group = 2)

data_tecli <-read_excel('teclistamab_case cohort_n29_250111.xlsx', sheet = 1) %>%   
   dplyr::select(ISS_DX, SEX, CTX_S_DATE, BIRTH, CTX_LINE, HB_CTX, PLT_CTX, GFR_CTX, LDH_HIGH1_CTX, VS_PFS_CTX, DATE_PFS_CTX, PRE_CTX_R, VS, DATE_LAST, RESP_CTX, PID, DATE_DX, LDH_CTX, CREAT_CTX, ANC_CTX, ALC_CTX, Heavychain, Lightchain, FGFR3_IGH_DX_VALUE, MAF_IGH_DX_VALUE, CCND1_IGH_DX_VALUE, RB1_DX_VALUE, TP53_DX_VALUE, `1Q_DX`) %>%
                    mutate(CTX_S_DATE = as.Date(CTX_S_DATE),
                           BIRTH = as.Date(BIRTH),
                           DATE_PFS_CTX = as.Date(DATE_PFS_CTX),
                           DATE_LAST = as.Date(DATE_LAST)) %>%
                    mutate(AGE = CTX_S_DATE - BIRTH) %>%
                    mutate(AGE = as.numeric(AGE) / 365.25) %>%
                    mutate(AGE_num = AGE) %>%
                    mutate(
                      `Time from diagnosis to index date` = as.numeric(interval(DATE_DX, CTX_S_DATE) %/% months(1)), 
                      HB_CTX = as.numeric(HB_CTX),
                      PLT_CTX = as.numeric(PLT_CTX),
                      GFR_CTX = as.numeric(GFR_CTX),
                      VS_PFS_CTX = as.numeric(VS_PFS_CTX),
                      PRE_CTX_R = as.numeric(PRE_CTX_R),
                      LDH_CTX = as.numeric(LDH_CTX),
                      CREAT_CTX = as.numeric(CREAT_CTX),
                      ANC_CTX = as.numeric(ANC_CTX),
                      ALC_CTX = as.numeric(ALC_CTX)) %>% 
                    mutate(LDH_CTX = if_else(LDH_CTX > 225, 'high', 'low'),
                          ANC_CTX = if_else(ANC_CTX < 1.0, 1,0)) %>% 
                    mutate(FGFR3_IGH_DX_VALUE = case_when(is.na(FGFR3_IGH_DX_VALUE) ~ NA_real_,    
                                                        FGFR3_IGH_DX_VALUE > 0 ~ 1,            
                                                        FGFR3_IGH_DX_VALUE == 0 ~ 0 ),
                          TP53_DX_VALUE = case_when(is.na(TP53_DX_VALUE) ~ NA_real_,    
                                                          TP53_DX_VALUE > 0 ~ 1,            
                                                          TP53_DX_VALUE == 0 ~ 0 ),
                          MAF_IGH_DX_VALUE = case_when(is.na(MAF_IGH_DX_VALUE) ~ NA_real_,    
                                                          MAF_IGH_DX_VALUE > 0 ~ 1,            
                                                          MAF_IGH_DX_VALUE == 0 ~ 0 ),
                          CCND1_IGH_DX_VALUE = case_when(is.na(CCND1_IGH_DX_VALUE) ~ NA_real_,    
                                                          CCND1_IGH_DX_VALUE > 0 ~ 1,            
                                                          CCND1_IGH_DX_VALUE == 0 ~ 0 ),
                          RB1_DX_VALUE = case_when(is.na(RB1_DX_VALUE) ~ NA_real_,    
                                                          RB1_DX_VALUE > 0 ~ 1,            
                                                          RB1_DX_VALUE == 0 ~ 0 ),
                          `1Q_DX` = case_when(is.na(`1Q_DX`) ~ NA_real_,    
                                                          `1Q_DX` > 0 ~ 1,            
                                                          `1Q_DX` == 0 ~ 0 )) %>% 
                      mutate(Group = 1)



data_tecli_all <- rbind(data_tecli, data_soc)

data_tidy <- data_tecli_all %>% 
              mutate(CTX_LINE = case_when(
                CTX_LINE == 1 ~ "3",
                CTX_LINE == 2 ~ "3",
                CTX_LINE == 3 ~ "3",
                CTX_LINE == 4 ~ "4",
                CTX_LINE == 5 ~ "5",
                CTX_LINE == 6 ~ "6",
                CTX_LINE == 7 ~ "6",
                CTX_LINE == 8 ~ "6",
                CTX_LINE == 9 ~ "6",
                CTX_LINE >= 10 ~ "6",
                TRUE ~ NA_character_))  %>% 
                # mutate(Group = as.factor(Group)) %>%
                mutate(
                  ISS_DX = case_when(
                    ISS_DX == "1" ~ "ISS I/II",
                    ISS_DX == "2" ~ "ISS I/II",
                    ISS_DX == "3" ~ "ISS III",
                    is.na(ISS_DX) ~ "Unknown",
                    TRUE ~ ISS_DX
                  )) %>%
                mutate(
                  GFR_CTX = case_when(
                    GFR_CTX < 30 ~ "<30",
                    GFR_CTX >= 30 & GFR_CTX < 60 ~ "<60",
                    GFR_CTX >= 60 & GFR_CTX < 90 ~ "<60",
                    GFR_CTX >= 90 ~"<60",
                    TRUE ~ "NA"
                  )) %>%
                mutate(
                  PLT_CTX = case_when(
                    PLT_CTX < 50 ~ "<100",
                    PLT_CTX >= 50 & PLT_CTX < 100 ~ "<100",
                    PLT_CTX >= 100 & PLT_CTX < 150 ~ "50-99",
                    PLT_CTX >= 150 & PLT_CTX < 200 ~ "50-99",
                    PLT_CTX >= 200 ~ "50-99",
                    TRUE ~ "NA"
                  ))  %>%
                mutate(
                  AGE = case_when(
                    AGE < 65 ~ "<70",
                    AGE >= 65 & AGE < 70 ~ "<70",
                    AGE >= 70 & AGE < 75 ~ "70-79",
                    AGE >= 75 & AGE < 80 ~ "70-79",
                    AGE >= 80 ~ ">=80",
                    TRUE ~ "NA"
                  )) %>% 
                  mutate(HB_CTX = case_when(
                    HB_CTX < 8 ~ "<8",
                    HB_CTX >= 8 & HB_CTX < 10 ~ "8-12",
                    HB_CTX >= 10 & HB_CTX < 12 ~ "8-12",
                    HB_CTX >= 12 ~ "8-12",
                    TRUE ~ "NA"
                  )) %>%
                mutate(
                    TIME = DATE_PFS_CTX - CTX_S_DATE,
                    TIME2 = DATE_LAST - CTX_S_DATE, 
                    TIME = as.numeric(TIME),
                    TIME2 = as.numeric(TIME2)) %>% 
                filter(TIME >= 0 & TIME2 >= 0) 

data_tidy$Group <- factor(data_tidy$Group, levels = c("2", "1"), labels = c("SOC", "Teclistamab"))

#! Baseline ---------------------------------------------------------------
data_tidy$Group <- factor(data_tidy$Group, levels = c("Teclistamab", "SOC")) #soc에 비해

colnames(data_tidy)
bsline_cols <- c("SEX", 
                 "AGE_num", 
                 "AGE", 
                 "Heavychain",
                 "Lightchain",
                 "ISS_DX",
                 "Time from diagnosis to index date",
                 "CTX_LINE",
                 
                 "TP53_DX_VALUE",
                 "1Q_DX",
                 "FGFR3_IGH_DX_VALUE",
                 "MAF_IGH_DX_VALUE",
                 "RB1_DX_VALUE",
                 "CCND1_IGH_DX_VALUE",
                 
                 "LDH_CTX",
                 "CREAT_CTX",
                 "HB_CTX", 
                 "ANC_CTX",
                 "PLT_CTX", 
                 "ALC_CTX",
                 
                 "Group")

# dir.create('C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/4th/Baseline', recursive=T, showWarnings=F)

data_tidy %>%
  dplyr::select(bsline_cols) %>%
  tbl_summary(by = Group,
  statistic = list(all_categorical() ~ "{n} ({p}%)"),
  digits = list(all_categorical() ~ c(0, 1)),
              label = list(SEX ~ "SEX",
                           AGE_num ~ "AGE",
                           AGE ~ "AGEG",
                           Heavychain ~ "Type of Myeloma",
                           Lightchain ~ "Light chain type",
                           ISS_DX ~ "ISS stage at diagnosis",
                           `Time from diagnosis to index date` ~ "Time from diagnosis to index date (months)",
                           CTX_LINE ~ "Line of therapy",
                           
                           TP53_DX_VALUE ~ "del(17p)",
                           `1Q_DX` ~ "amp(1q)",
                           FGFR3_IGH_DX_VALUE ~ "t(4:14)",
                           MAF_IGH_DX_VALUE ~ "t(14:16)",
                           RB1_DX_VALUE ~ "del(13q)",
                           CCND1_IGH_DX_VALUE ~ "t(11:14)",
                           
                           LDH_CTX ~ "Lactate dehydrogenase ≥ ULN",
                           CREAT_CTX ~ "Creatinine (mg/dL)",
                           HB_CTX ~ "Hemoglobin <7.5 g/dL",
                           ANC_CTX ~ "Absoulte neutrophil count <1.0 x 10^9/L",
                           PLT_CTX ~ "Platelet count <50 x 10^9/L",
                           ALC_CTX ~ "Absoulte lymphocyte count, /uL")) %>%
  add_p(pvalue_fun = ~ style_pvalue(., digits=3)) %>% add_overall() %>%
  modify_caption("**Patients' clinical charateristics.** (N = {N})") %>%
  bold_labels() %>% bold_p() %>% italicize_levels() %>% 
  gtsummary::as_flex_table() %>%
  flextable::save_as_docx(
    "Unmatched covariates" = .,
    path = "C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/4th/Baseline/baseline_Unmatched.docx"
  )

data_tidy$Group <- factor(data_tidy$Group, levels = c("SOC", "Teclistamab")) 

####! Survival analysis ------------------------------------------------------


# Fit survival model (OS)


four_digit <- function(p) {
  sprintf("%.4f", p)
}


fit <- survfit2(Surv(TIME2/30.42, VS) ~ Group, data = data_tidy)
cox_fit <- coxph(Surv(TIME2/365, VS) ~ Group, data = data_tidy)

# dir.create('C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/4th/[26-01-15] Figure', recursive=T, showWarnings=F)
setwd("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/4th/[26-01-15] Figure")
# opt_plot(fit, name="image_OS_b4", deadline=2, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")
summary(cox_fit)

# Fit survival model (PFS)
fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ Group, data = data_tidy)
cox_fit <- coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group, data = data_tidy)
summary(cox_fit)
# opt_plot(fit, name="image_PFS_b4", deadline=1.5, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")

# Median follow-up survival ---------------------------------------------------------------
median_fw_surv_dat <- copy(data_tidy)
median_fw_surv_dat$VS <- ifelse(median_fw_surv_dat$VS==0, 1, 0)
median_fw_surv_dat$VS_PFS_CTX <- ifelse(median_fw_surv_dat$VS_PFS_CTX==0, 1, 0)

# OS
fit <- survfit2(Surv(TIME2/30.42, VS) ~ Group, data = median_fw_surv_dat)
med_res <- summary(fit)

Tecli_med_fw <- as.data.frame(round(med_res$table[, c("median", "0.95LCL", "0.95UCL")],1))

Tecli_med_fw %>%
  cbind(" "=row.names(med_res$table),.)|>
  gt()

# PFS
fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ Group, data = median_fw_surv_dat)
med_res <- summary(fit)

Tecli_med_fw <- as.data.frame(round(med_res$table[, c("median", "0.95LCL", "0.95UCL")],1))

Tecli_med_fw %>%
  cbind(" "=row.names(med_res$table),.)|>
  gt()


#* OS, PFS Rate Table -----------------------------------


# --- helper ---------------------------------------------------------------
format_rate <- function(est, lcl, ucl, digits = 1) {
  sprintf("%.*f%% (95%% CI %.*f–%.*f)", digits, est*100, digits, lcl*100, digits, ucl*100)
}

format_median <- function(med, lcl, ucl, digits = 2) {
  sprintf("%.*f (95%% CI %.*f–%.*f)", digits, med, digits, lcl, digits, ucl)
}

make_surv_table <- function(fit, times = c(1, 2, 3)) {
  ## 1-, 2-, 3-year survival ----------------------------------------------
  s <- summary(fit, times = times)
  rate_tbl <- 
    tibble::tibble(
      Group   = s$strata,
      Time    = s$time,
      Stat    = format_rate(s$surv, s$lower, s$upper)
    ) |>
    tidyr::pivot_wider(names_from = Time,
                       values_from = Stat,
                       names_glue = "{Time}-year")
  
  ## median survival --------------------------------------------------------
  med <- summary(fit)$table
  med_tbl <- tibble::tibble(
    Group  = rownames(med),
    Median = format_median(med[, "median"],
                           med[, "0.95LCL"],
                           med[, "0.95UCL"])
  )
  
  ## merge & order ----------------------------------------------------------
  dplyr::left_join(rate_tbl, med_tbl, by = "Group") |>
    dplyr::rename(`Median (yr)` = Median)
}


# OS
surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME2/365, VS) ~ Group, data = data_tidy)
)

summary(survfit2(Surv(TIME2/365, VS) ~ Group, data = data_tidy), times = 1)

surv_table

surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    `2-year`     = "2-year",
#    `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )


# PFS
surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME/365, VS_PFS_CTX) ~ Group, data = data_tidy)
)

surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )



####! Response analysis ------------------------------------------------------
# Response rate table

data_tidy %>% dplyr::select(Group, RESP_CTX) %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    CR = sum(RESP_CTX == "CR", na.rm = TRUE),
    PR = sum(RESP_CTX == "PR", na.rm = TRUE),
    SD = sum(RESP_CTX == "SD", na.rm = TRUE),
    PD = sum(RESP_CTX == "PD", na.rm = TRUE),
    ORR = (CR + PR) / n * 100,
    DCR = (CR + PR + SD) / n * 100
  ) %>%
  mutate(
    ORR = sprintf("%.1f%%", ORR),
    DCR = sprintf("%.1f%%", DCR)
  ) %>%
  dplyr::select(Group, n, CR, PR, SD, PD, ORR, DCR) |> gt()

data_tidy %>% dplyr::select(Group, RESP_CTX) %>%
  tbl_summary(Group, 
              statistic = list(RESP_CTX ~ "{n} ({p}%)"),
              label = list(RESP_CTX ~ "Response")) %>%
  add_overall() %>%
  modify_header(label = "**Response**") 


####! Matching ---------------------------------------------------------------

psm = matchit(Group~ SEX+AGE + PLT_CTX+ CTX_LINE , data=data_tidy, method = "nearest", distance = "logit", replace = FALSE, caliper = 0.2, ratio =1)
matched_data = match.data(psm)

tmp <- matched_data %>% select(-c(distance,weights,subclass))
psm2 = matchit(Group~ CTX_LINE,
                data=tmp, method = "nearest", distance = "logit", replace = FALSE, caliper = 0.2, ratio =1)

matched_data2 = match.data(psm2)
tab <- CreateTableOne(vars = c("SEX","AGE", "PLT_CTX", "CTX_LINE"), strata="Group", data=matched_data2, test=F)
tab <- ExtractSmd(tab) %>% as.data.frame
names(tab) <- "SMD"


pair_id <- matched_data2 %>%
  group_by(subclass) %>%
  summarise(
    PID_SOC   = PID[Group == "SOC"][1],
    PID_Tecli = PID[Group == "Teclistamab"][1],
    .groups   = "drop"
  ) %>%
  arrange(PID_Tecli)   # 보기 좋게 정렬 (원하는 대로 변경 가능)

pair_id


pair_id_ctx <- pair_id %>%
  # Teclistamab 쪽 CTX_LINE 붙이기
  left_join(
    data_tecli %>%
      select(PID_Tecli = PID, CTX_LINE_Tecli = CTX_LINE),
    by = "PID_Tecli"
  ) %>%
  # SOC 쪽 CTX_LINE 붙이기
  left_join(
    data_soc %>%
      select(PID_SOC = PID, CTX_LINE_SOC = CTX_LINE),
    by = "PID_SOC"
  )
matched_tecli <- matched_data2 %>% select(-c(distance,weights))
matched_tecli <- matched_tecli[matched_tecli$Group=='Teclistamab',]

# pair_id_ctx %>% write.csv("tecli_paired.csv")
# view(pair_id_ctx)
#! Balance check ---------------------------------------------------------------

exp_form = Group~ SEX+AGE + PLT_CTX + CTX_LINE
exp_var = all.vars(exp_form)[1]

# summary(my_weights$weight)
tab_smd_adj = svyCreateTableOne(vars = all.vars(exp_form)[-1], strata = exp_var,
                                data=svydesign(ids = ~1, data=data_tidy, weights=psm$weights))

tab_smd_un = svyCreateTableOne(vars = all.vars(exp_form)[-1], strata = exp_var,
                               data=svydesign(ids = ~1, data=data_tidy,weights=~1))
tab_smd_adj1 = ExtractSmd(tab_smd_adj) %>% as.data.frame
tab_smd_un1 = ExtractSmd(tab_smd_un) %>% as.data.frame

tab_smd_adj1$variable = rownames(tab_smd_adj1); rownames(tab_smd_adj1) = NULL
tab_smd_un1$variable = rownames(tab_smd_un1); rownames(tab_smd_un1) = NULL
tab_smd_adj1$type = "Adjusted"
tab_smd_un1$type = "Unadjusted"

col_val = NULL
col_lab = NULL
var_lab = c("Line of therapy","Platelet count <50 x 10^9/L", "Age","Sex")

if(is.null(col_val)){col_val = pal_lancet()(2)[2]}
if(is.null(col_lab)){col_lab = 0}

colnames(tab_smd_adj1)[1] <- colnames(tab_smd_un1)[1] <-
  names(col_val) <- names(col_lab) <- c("SMD")

tab_smd_adj1$SMD <- tab$SMD

tab_smd_res = rbind.data.frame(tab_smd_adj1, tab_smd_un1)
tab_smd_res$variable = factor(tab_smd_res$variable, levels = rev(unique(tab_smd_res$variable)),
                              labels = var_lab)


tab_smd_res |>
  gt()


# dev.new()
# pdf("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/4th/[26-01-15] Figure/260115_smdplot.pdf",height=7,width=12)
tab_smd_res %>%
  gather(key = "key", value = "value", -variable, -type) %>%
  ggplot() +
  geom_point(aes(x = value, y = variable, shape = type, col = key),size=3) +
  
  scale_shape_manual(values = c("Adjusted" = 19, "Unadjusted" = 4), name = "") +
  scale_color_manual(values = col_val, labels = c("Teclistamab Vs. SOC"), name = "") +
  geom_vline(aes(xintercept = 0.2), linetype = "dashed") +
  
  labs(x = "Standardized mean difference", y = "Covariates") +
  ggtitle("") +
  
  theme_classic() +
  
  theme(axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))
# dev.off()


#! Load SOC dataset after PSM ---------------------------------------------------------------
data_soc_new <- read_excel('C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/Data/tecli_paired_n27_SOC데이터_0114.xlsx') %>%
   dplyr::select(ISS_DX, SEX, CTX_S_DATE, BIRTH, CTX_LINE, HB_CTX, PLT_CTX, GFR_CTX, LDH_HIGH1_CTX, VS_PFS_CTX, DATE_PFS_CTX, PRE_CTX_R, 
   VS, DATE_LAST, RESP_CTX, PID, DATE_DX, LDH_CTX, CREAT_CTX, ANC_CTX, ALC_CTX, Heavychain, Lightchain, 
   FGFR3_IGH_DX_VALUE, MAF_IGH_DX_VALUE, CCND1_IGH_DX_VALUE, RB1_DX_VALUE, TP53_DX_VALUE, `1Q_DX`, subclass) %>%
                    mutate(CTX_S_DATE = as.Date(CTX_S_DATE),
                           BIRTH = as.Date(BIRTH),
                           DATE_PFS_CTX = as.Date(DATE_PFS_CTX),
                           DATE_LAST = as.Date(DATE_LAST)) %>%
                    mutate(AGE = CTX_S_DATE - BIRTH) %>%
                    mutate(AGE = as.numeric(AGE) / 365.25) %>%
                    mutate(AGE_num = AGE) %>%
                    mutate(
                      `Time from diagnosis to index date` = as.numeric(interval(DATE_DX, CTX_S_DATE) %/% months(1)), 
                      HB_CTX = as.numeric(HB_CTX),
                      PLT_CTX = as.numeric(PLT_CTX),
                      GFR_CTX = as.numeric(GFR_CTX),
                      VS_PFS_CTX = as.numeric(VS_PFS_CTX),
                      PRE_CTX_R = as.numeric(PRE_CTX_R),
                      LDH_CTX = as.numeric(LDH_CTX),
                      CREAT_CTX = as.numeric(CREAT_CTX),
                      ANC_CTX = as.numeric(ANC_CTX),
                      ALC_CTX = as.numeric(ALC_CTX)) %>% 
                    mutate(LDH_CTX = if_else(LDH_CTX > 225, 'high', 'low'),
                          ANC_CTX = if_else(ANC_CTX < 1.0, 1,0)) %>% 
                    mutate(FGFR3_IGH_DX_VALUE = case_when(is.na(FGFR3_IGH_DX_VALUE) ~ NA_real_,    
                                                        FGFR3_IGH_DX_VALUE > 0 ~ 1,            
                                                        FGFR3_IGH_DX_VALUE == 0 ~ 0 ),
                          TP53_DX_VALUE = case_when(is.na(TP53_DX_VALUE) ~ NA_real_,    
                                                          TP53_DX_VALUE > 0 ~ 1,            
                                                          TP53_DX_VALUE == 0 ~ 0 ),
                          MAF_IGH_DX_VALUE = case_when(is.na(MAF_IGH_DX_VALUE) ~ NA_real_,    
                                                          MAF_IGH_DX_VALUE > 0 ~ 1,            
                                                          MAF_IGH_DX_VALUE == 0 ~ 0 ),
                          CCND1_IGH_DX_VALUE = case_when(is.na(CCND1_IGH_DX_VALUE) ~ NA_real_,    
                                                          CCND1_IGH_DX_VALUE > 0 ~ 1,            
                                                          CCND1_IGH_DX_VALUE == 0 ~ 0 ),
                          RB1_DX_VALUE = case_when(is.na(RB1_DX_VALUE) ~ NA_real_,    
                                                          RB1_DX_VALUE > 0 ~ 1,            
                                                          RB1_DX_VALUE == 0 ~ 0 ),
                          `1Q_DX` = case_when(is.na(`1Q_DX`) ~ NA_real_,    
                                                          `1Q_DX` > 0 ~ 1,            
                                                          `1Q_DX` == 0 ~ 0 )) %>% 
                      mutate(Group = "SOC")

data_soc_new <- data_soc_new %>% 
              mutate(CTX_LINE = case_when(
                CTX_LINE == 1 ~ "3",
                CTX_LINE == 2 ~ "3",
                CTX_LINE == 3 ~ "3",
                CTX_LINE == 4 ~ "4",
                CTX_LINE == 5 ~ "5",
                CTX_LINE == 6 ~ "6",
                CTX_LINE == 7 ~ "6",
                CTX_LINE == 8 ~ "6",
                CTX_LINE == 9 ~ "6",
                CTX_LINE >= 10 ~ "6",
                TRUE ~ NA_character_))  %>% 
                # mutate(Group = as.factor(Group)) %>%
                mutate(
                  ISS_DX = case_when(
                    ISS_DX == "1" ~ "ISS I/II",
                    ISS_DX == "2" ~ "ISS I/II",
                    ISS_DX == "3" ~ "ISS III",
                    is.na(ISS_DX) ~ "Unknown",
                    TRUE ~ ISS_DX
                  )) %>%
                mutate(
                  GFR_CTX = case_when(
                    GFR_CTX < 30 ~ "<30",
                    GFR_CTX >= 30 & GFR_CTX < 60 ~ "<60",
                    GFR_CTX >= 60 & GFR_CTX < 90 ~ "<60",
                    GFR_CTX >= 90 ~"<60",
                    TRUE ~ "NA"
                  )) %>%
                mutate(
                  PLT_CTX = case_when(
                    PLT_CTX < 50 ~ "<100",
                    PLT_CTX >= 50 & PLT_CTX < 100 ~ "<100",
                    PLT_CTX >= 100 & PLT_CTX < 150 ~ "50-99",
                    PLT_CTX >= 150 & PLT_CTX < 200 ~ "50-99",
                    PLT_CTX >= 200 ~ "50-99",
                    TRUE ~ "NA"
                  ))  %>%
                mutate(
                  AGE = case_when(
                    AGE < 65 ~ "<70",
                    AGE >= 65 & AGE < 70 ~ "<70",
                    AGE >= 70 & AGE < 75 ~ "70-79",
                    AGE >= 75 & AGE < 80 ~ "70-79",
                    AGE >= 80 ~ ">=80",
                    TRUE ~ "NA"
                  )) %>% 
                  mutate(HB_CTX = case_when(
                    HB_CTX < 8 ~ "<8",
                    HB_CTX >= 8 & HB_CTX < 10 ~ "8-12",
                    HB_CTX >= 10 & HB_CTX < 12 ~ "8-12",
                    HB_CTX >= 12 ~ "8-12",
                    TRUE ~ "NA"
                  )) %>%
                mutate(
                    TIME = DATE_PFS_CTX - CTX_S_DATE,
                    TIME2 = DATE_LAST - CTX_S_DATE, 
                    TIME = as.numeric(TIME),
                    TIME2 = as.numeric(TIME2)) %>% 
                filter(TIME >= 0 & TIME2 >= 0) 

matched_tecli2 <- matched_tecli %>% 
  as_tibble() %>% 
  mutate(across(where(is.factor), as.character))

data_soc_new2 <- data_soc_new %>% 
  as_tibble() %>% 
  mutate(across(where(is.factor), as.character)) %>% select(colnames(matched_tecli2))

matched_tidy <- rbind(matched_tecli2, data_soc_new2)
matched_tidy$Group <- factor(matched_tidy$Group, levels = c("SOC","Teclistamab"))

#! Baseline ---------------------------------------------------------------
matched_tidy$Group <- factor(matched_tidy$Group, levels = c("Teclistamab", "SOC"))

bsline_cols <- c("SEX", "AGE_num", "AGE", "Heavychain","Lightchain","ISS_DX",
                 "Time from diagnosis to index date","CTX_LINE","TP53_DX_VALUE",
                 "1Q_DX","FGFR3_IGH_DX_VALUE","MAF_IGH_DX_VALUE","RB1_DX_VALUE","CCND1_IGH_DX_VALUE",
                 "LDH_CTX","CREAT_CTX","HB_CTX", "ANC_CTX","PLT_CTX", "ALC_CTX","Group")

matched_tidy %>%
  dplyr::select(bsline_cols) %>%
  tbl_summary(by = Group,
  statistic = list(all_categorical() ~ "{n} ({p}%)"),
  digits = list(all_categorical() ~ c(0, 1)),
              label = list(SEX ~ "SEX",
                           AGE_num ~ "AGE",
                           AGE ~ "AGEG",
                           Heavychain ~ "Type of Myeloma",
                           Lightchain ~ "Light chain type",
                           ISS_DX ~ "ISS stage at diagnosis",
                           `Time from diagnosis to index date` ~ "Time from diagnosis to index date (months)",
                           CTX_LINE ~ "Line of therapy",
                           
                           TP53_DX_VALUE ~ "del(17p)",
                           `1Q_DX` ~ "amp(1q)",
                           FGFR3_IGH_DX_VALUE ~ "t(4:14)",
                           MAF_IGH_DX_VALUE ~ "t(14:16)",
                           RB1_DX_VALUE ~ "del(13q)",
                           CCND1_IGH_DX_VALUE ~ "t(11:14)",
                           
                           LDH_CTX ~ "Lactate dehydrogenase ≥ ULN",
                           CREAT_CTX ~ "Creatinine (mg/dL)",
                           HB_CTX ~ "Hemoglobin <7.5 g/dL",
                           ANC_CTX ~ "Absoulte neutrophil count <1.0 x 10^9/L",
                           PLT_CTX ~ "Platelet count <50 x 10^9/L",
                           ALC_CTX ~ "Absoulte lymphocyte count, /uL")) %>%
  add_p(pvalue_fun = ~ style_pvalue(., digits=3)) %>% add_overall() %>%
  modify_caption("**Patients' clinical charateristics.** (N = {N})") %>%
  bold_labels() %>% bold_p() %>% italicize_levels() %>% 
  gtsummary::as_flex_table() %>%
  flextable::save_as_docx(
    "Matched covariates" = .,
    path = "C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/4th/Baseline/baseline_Matched.docx"
  )

matched_tidy$Group <- factor(matched_tidy$Group, levels = c("SOC", "Teclistamab")) #soc에 비해

#! Survival analysis ------------------------------------------------------

# Fit survival model (OS)

setwd("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/4th/[26-01-15] Figure")
fit <- survfit2(Surv(TIME2/30.42, VS) ~ Group, data = matched_tidy)
cox_fit <- coxph(Surv(TIME2 / 365, VS) ~ Group + cluster(subclass),data = matched_tidy)
summary(cox_fit)
# opt_plot(fit, name="image_OS_psm", deadline=2, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")

# Fit survival model (PFS)
fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ Group, data = matched_tidy)
cox_fit <- coxph(Surv(TIME / 365, VS_PFS_CTX) ~ Group + cluster(subclass), data = matched_tidy)
summary(cox_fit)
# opt_plot(fit, name="image_PFS_psm", deadline=1.5, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")


# Median survival ---------------------------------------------------------------
# OS

fit <- survfit2(Surv(TIME2/30.42, VS) ~ Group, data = matched_tidy)
med_res <- summary(fit)
Tecli_med <- as.data.frame(round(med_res$table[, c("median", "0.95LCL", "0.95UCL")],1))

Tecli_med %>%
  cbind(" "=row.names(med_res$table),.)|>
  gt()

# PFS
fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ Group, data = matched_tidy)
med_res <- summary(fit)
Tecli_med <- as.data.frame(round(med_res$table[, c("median", "0.95LCL", "0.95UCL")],1))

Tecli_med %>%
  cbind(" "=row.names(med_res$table),.)|>
  gt()


# Median follow-up survival ---------------------------------------------------------------
# OS

fit <- survfit2(Surv(TIME2/30.42, VS==0) ~ Group, data = matched_tidy)
med_res <- summary(fit)
Tecli_med_fw <- as.data.frame(round(med_res$table[, c("median", "0.95LCL", "0.95UCL")],1))

Tecli_med_fw %>%
  cbind(" "=row.names(med_res$table),.)|>
  gt()

# PFS
fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX==0) ~ Group, data = matched_tidy)
med_res <- summary(fit)
Tecli_med_fw <- as.data.frame(round(med_res$table[, c("median", "0.95LCL", "0.95UCL")],1))

Tecli_med_fw %>%
  cbind(" "=row.names(med_res$table),.)|>
  gt()

#* OS, PFS Rate Table -----------------------------------


# --- helper ---------------------------------------------------------------
format_rate <- function(est, lcl, ucl, digits = 1) {
  sprintf("%.*f%% (95%% CI %.*f–%.*f)", digits, est*100, digits, lcl*100, digits, ucl*100)
}


format_median <- function(med, lcl, ucl, digits = 2) {
  sprintf("%.*f (95%% CI %.*f–%.*f)", digits, med, digits, lcl, digits, ucl)
}

make_surv_table <- function(fit, times = c(1, 2, 3)) {
  ## 1-, 2-, 3-year survival ----------------------------------------------
  s <- summary(fit, times = times)
  rate_tbl <- 
    tibble::tibble(
      Group   = s$strata,
      Time    = s$time,
      Stat    = format_rate(s$surv, s$lower, s$upper)
    ) |>
    tidyr::pivot_wider(names_from = Time,
                       values_from = Stat,
                       names_glue = "{Time}-year")
  
  ## median survival --------------------------------------------------------
  med <- summary(fit)$table
  med_tbl <- tibble::tibble(
    Group  = rownames(med),
    Median = format_median(med[, "median"],
                           med[, "0.95LCL"],
                           med[, "0.95UCL"])
  )
  
  ## merge & order ----------------------------------------------------------
  dplyr::left_join(rate_tbl, med_tbl, by = "Group") |>
    dplyr::rename(`Median (yr)` = Median)
}

# OS
surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME2/365, VS) ~ Group, data = matched_tidy)
)
surv_table

surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )


# PFS
surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME/365, VS_PFS_CTX) ~ Group, data = matched_tidy)
)

summary(fit, times =1)

surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )



#! Response analysis ------------------------------------------------------
# Response rate table
matched_tidy %>% dplyr::select(Group, RESP_CTX) %>% 
  group_by(Group) %>%
  summarise(
    n = n(),
    CR = sum(RESP_CTX == "CR", na.rm = TRUE),
    sCR = sum(RESP_CTX == "sCR", na.rm = TRUE),
    PR = sum(RESP_CTX == "PR", na.rm = TRUE),
    SD = sum(RESP_CTX == "SD", na.rm = TRUE),
    PD = sum(RESP_CTX == "PD", na.rm = TRUE),
    VGPR = sum(RESP_CTX == "VGPR", na.rm = TRUE),
    ORR = (CR + sCR + VGPR + PR) / n * 100,
    DCR = (CR + sCR + VGPR + PR + SD) / n * 100
  ) %>%
  mutate(
    ORR = sprintf("%.1f%%", ORR),
    DCR = sprintf("%.1f%%", DCR)
  ) %>%
  dplyr::select(Group, n, CR, sCR, PR, SD, PD, ORR, DCR) |> gt()


matched_tidy %>% 
  mutate(RESP_CTX = factor(RESP_CTX, levels=c("sCR", "CR", "VGPR", "PR", "MR", "SD", "Refactory", "Unknown"))) %>% 
  mutate(
    ORR = 
    case_when(RESP_CTX %in% c("sCR","CR","VGPR","PR") ~ 1,
              TRUE ~ 0),
    overVGPR = 
    case_when(RESP_CTX %in% c("sCR","CR","VGPR") ~ 1,
              TRUE ~ 0),
    overCR = 
    case_when(RESP_CTX %in% c("sCR","CR") ~ 1,
              TRUE ~ 0)) %>%
  dplyr::select(Group, RESP_CTX,ORR,overVGPR,overCR) %>%
  tbl_summary(Group, 
              statistic = list(all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_categorical() ~ c(0,1)),
              label = list(
                RESP_CTX ~ "Best response",
                ORR ~ "ORR",
                overVGPR ~ "≥VGPR",
                overCR ~ "≥CR")) %>%
  add_p(pvalue_fun = ~ style_pvalue(., digits=3)) %>%
  add_overall() %>%
  modify_header(label = "**Response**") %>%
  modify_spanning_header(everything() ~ NA) %>%
  bold_labels()
  # italicize_levels()




#! Landmark analysis ------------------------------------------------------
# OS
matched_data_land7 <- matched_tidy[matched_tidy$TIME2 >= 7,]
matched_data_land30 <- matched_tidy[matched_tidy$TIME2 >= 30,]

fit <- survfit2(Surv(TIME2/30.42, VS) ~ Group, data = matched_data_land7)
cox_fit <- coxph(Surv(TIME2 / 365, VS) ~ Group + cluster(subclass), data = matched_data_land7)
summary(cox_fit)
# opt_plot(fit, name="image_OS_land7_psm", deadline=2, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")

fit <- survfit2(Surv(TIME2/30.42, VS) ~ Group, data = matched_data_land30)
cox_fit <- coxph(Surv(TIME2 / 365, VS) ~ Group + cluster(subclass),data = matched_data_land30)
summary(cox_fit)
# opt_plot(fit, name="image_OS_land30_psm", deadline=2, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")


# PFS
matched_data_land7 <- matched_tidy[matched_tidy$TIME >= 7,]
matched_data_land30 <- matched_tidy[matched_tidy$TIME >= 30,]

fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ Group, data = matched_data_land7)
cox_fit <- coxph(Surv(TIME / 365, VS_PFS_CTX) ~ Group + cluster(subclass), data = matched_data_land7)
summary(cox_fit)
# opt_plot(fit, name="image_PFS_land7_psm", deadline=1.5, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")

fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ Group, data = matched_data_land30)
cox_fit <- coxph(Surv(TIME / 365, VS_PFS_CTX) ~ Group + cluster(subclass), data = matched_data_land30)
summary(cox_fit)
# opt_plot(fit, name="image_PFS_land30_psm", deadline=1.5, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")


#! Forest plot----------------------------------------------------

# 0) 이진 반응 변수 만들기 (전체 데이터에서)
match_sbt <- copy(matched_tidy)


# 1) Group 참조 레벨 고정 (첫 번째가 ref가 됨)
#   예: "SOC"가 참조, "Teclistamab"가 비교라면 아래처럼.
match_sbt <- match_sbt %>% mutate(Group = fct_relevel(Group, "SOC"))

match_sbt$AGEG <- ifelse(match_sbt$AGE_num>=65, ">=65", "<65")
match_sbt$eGFR <- ifelse(match_sbt$GFR_CTX=="<60", ">=30", match_sbt$GFR_CTX)
match_sbt$CTX_LINE <- ifelse(match_sbt$CTX_LINE=="6",">5","<=5")
match_sbt$ISS_DX <- ifelse(match_sbt$ISS_DX=="NA", NA, match_sbt$ISS_DX)

match_sbt$AGEG <- factor(match_sbt$AGEG, levels = c(">=65", "<65"))
match_sbt$SEX <- factor(match_sbt$SEX, levels = c("F", "M"))
match_sbt$eGFR <- factor(match_sbt$eGFR, levels = c(">=30", "<30"))
match_sbt$CTX_LINE <- factor(match_sbt$CTX_LINE, levels = c("<=5",">5"))
match_sbt$ISS_DX <- factor(match_sbt$ISS_DX, levels = c("ISS I/II","ISS III"))

# 3) 각 행(서브그룹 레벨)에 대응하는 모델 만들기
# OS subgroup analysis------
fit_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt)

fitAge1_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt %>% filter(AGEG == ">=65"))
fitAge2_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt %>% filter(AGEG == "<65"))

fitSex1_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt %>% filter(SEX == "F"))
fitSex2_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt %>% filter(SEX == "M"))

fiteGFR1_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt %>% filter(eGFR == ">=30"))
fiteGFR2_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt %>% filter(eGFR == "<30"))

fitLine1_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt %>% filter(CTX_LINE == "<=5"))
fitLine2_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt %>% filter(CTX_LINE == ">5"))

fitISS1_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt %>% filter(ISS_DX == "ISS I/II"))
fitISS2_OS = coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = match_sbt %>% filter(ISS_DX == "ISS III"))


# PFS subgroup analysis------
fit_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt)

fitAge1_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt %>% filter(AGEG == ">=65"))
fitAge2_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt %>% filter(AGEG == "<65"))

fitSex1_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt %>% filter(SEX == "F"))
fitSex2_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt %>% filter(SEX == "M"))

fiteGFR1_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt %>% filter(eGFR == ">=30"))
fiteGFR2_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt %>% filter(eGFR == "<30"))

fitLine1_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt %>% filter(CTX_LINE == "<=5"))
fitLine2_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt %>% filter(CTX_LINE == ">5"))

fitISS1_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt %>% filter(ISS_DX == "ISS I/II"))
fitISS2_PFS = coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = match_sbt %>% filter(ISS_DX == "ISS III"))


rowlist <- c(
  "All patients",    # 전체
  "AGE",               # 서브그룹 이름
  "    >=65",
  "    <65",

  "SEX",
  "    F", 
  "    M",
  
  "eGFR",
  "    >=30",
  "    <30", 

  "Line of therapy",
  "    <=5", 
  "    >5", 

  "ISS stage at diagnosis",
  "    ISS I/II", 
  "    ISS III"
)

# 4) model 리스트 (rowlist와 1:1로 맞추되, 섹션 헤더 자리는 NA)
model <- list(
  fit_OS,
  NA,  fitAge1_OS, fitAge2_OS, 
  NA,  fitSex1_OS, fitSex2_OS,
  NA,  fiteGFR1_OS, fiteGFR2_OS, 
  NA,  fitLine1_OS, fitLine2_OS, 
  NA,  fitISS1_OS, fitISS2_OS
)

# 5) level 벡터 = 모델의 설명변수인 Group의 레벨 순서(참조가 첫 번째)
level <- levels(match_sbt$Group)  # 예: c("SOC","Teclistamab")
source('C:/Users/chaehyun/Dropbox/Work/PIPET/데이터팀_표준화/forest plot/function.R')
# 6) 호출
dev.new()
# pdf("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/4th/[26-01-15] Figure/OS_forest_plot.pdf",height=7,width=12)
png("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/4th/[26-01-15] Figure/OS_forest_plot.png", width = 1000, height = 900, res = 220)
p <- opt_plot2(rowlist, model, level)   # 필요하면 col="rev"
dev.off()

print("## P-value ##")
round(summary(fit_OS)$coefficients[6],3)
round(summary(fitAge1_OS)$coefficients[6],3);round(summary(fitAge2_OS)$coefficients[6],3)
round(summary(fitSex1_OS)$coefficients[6],3);round(summary(fitSex2_OS)$coefficients[6],3)
round(summary(fiteGFR1_OS)$coefficients[6],3);round(summary(fiteGFR2_OS)$coefficients[6],3)
round(summary(fitLine1_OS)$coefficients[6],3);round(summary(fitLine2_OS)$coefficients[6],3)
round(summary(fitISS1_OS)$coefficients[6],3);round(summary(fitISS2_OS)$coefficients[6],3)



# 4) model 리스트 (rowlist와 1:1로 맞추되, 섹션 헤더 자리는 NA)
model <- list(
  fit_PFS,
  NA,  fitAge1_PFS, fitAge2_PFS,
  NA,  fitSex1_PFS, fitSex2_PFS,
  NA,  fiteGFR1_PFS, fiteGFR2_PFS,
  NA,  fitLine1_PFS, fitLine2_PFS,
  NA,  fitISS1_PFS, fitISS2_PFS
)
level <- levels(match_sbt$Group)  # 예: c("SOC","Teclistamab")

# 6) 호출
dev.new()
# pdf("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/3rd/[25-12-08] Figure/PFS_forest_plot.pdf",height=7,width=12)
png("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/4th/[26-01-15] Figure/PFS_forest_plot.png", width = 1000, height = 900, res = 220)
p <- opt_plot2(rowlist, model, level)   # 필요하면 col="rev"
dev.off()

print("## P-value ##")
round(summary(fit_PFS)$coefficients[6],3)
round(summary(fitAge1_PFS)$coefficients[6],3);round(summary(fitAge2_PFS)$coefficients[6],3)
round(summary(fitSex1_PFS)$coefficients[6],3);round(summary(fitSex2_PFS)$coefficients[6],3)
round(summary(fiteGFR1_PFS)$coefficients[6],3);round(summary(fiteGFR2_PFS)$coefficients[6],3)
round(summary(fitLine1_PFS)$coefficients[6],3);round(summary(fitLine2_PFS)$coefficients[6],3)
round(summary(fitISS1_PFS)$coefficients[6],3);round(summary(fitISS2_PFS)$coefficients[6],3)



#! Subgroup analysis (PR 이상)----------------------------------------------------


matched_data_pr <- matched_tidy %>% 
  filter(RESP_CTX %in% c("CR", "PR", "VGPR", "sCR")) 




#! Survival analysis ------------------------------------------------------

# Fit survival model (OS)
fit <- survfit2(Surv(TIME2/30.42, VS) ~ Group, data = matched_data_pr)
cox_fit <- coxph(Surv(TIME2/365, VS) ~ Group + cluster(subclass), data = matched_data_pr)
# opt_plot(fit, name="image_OS_psm_sub", deadline=2, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")
summary(cox_fit)

# Fit survival model (PFS)
fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ Group, data = matched_data_pr)
cox_fit <- coxph(Surv(TIME/365, VS_PFS_CTX) ~ Group + cluster(subclass), data = matched_data_pr)
# opt_plot(fit, name="image_PFS_psm_sub", deadline=1.5, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")
summary(cox_fit)



#* OS, PFS Rate Table -----------------------------------


# --- helper ---------------------------------------------------------------
format_rate <- function(est, lcl, ucl, digits = 1) {
  sprintf("%.*f%% (95%% CI %.*f–%.*f)", digits, est*100, digits, lcl*100, digits, ucl*100)
}


format_median <- function(med, lcl, ucl, digits = 2) {
  sprintf("%.*f (95%% CI %.*f–%.*f)", digits, med, digits, lcl, digits, ucl)
}

make_surv_table <- function(fit, times = c(1, 2, 3)) {
  ## 1-, 2-, 3-year survival ----------------------------------------------
  s <- summary(fit, times = times)
  rate_tbl <- 
    tibble::tibble(
      Group   = s$strata,
      Time    = s$time,
      Stat    = format_rate(s$surv, s$lower, s$upper)
    ) |>
    tidyr::pivot_wider(names_from = Time,
                       values_from = Stat,
                       names_glue = "{Time}-year")
  
  ## median survival --------------------------------------------------------
  med <- summary(fit)$table
  med_tbl <- tibble::tibble(
    Group  = rownames(med),
    Median = format_median(med[, "median"],
                           med[, "0.95LCL"],
                           med[, "0.95UCL"])
  )
  
  ## merge & order ----------------------------------------------------------
  dplyr::left_join(rate_tbl, med_tbl, by = "Group") |>
    dplyr::rename(`Median (yr)` = Median)
}


surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME2/365, VS) ~ Group, data = matched_data_pr)
)
surv_table

surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )


surv_table <- make_surv_table(
  fit = survfit2(Surv(TIME/365, VS_PFS_CTX) ~ Group, data = matched_data_pr)
)


surv_table |>
  gt() |>
  cols_label(
    Group        = "Treatment arm",
    `1-year`     = "1-year",
    # `2-year`     = "2-year",
    # `3-year`     = "3-year",
    `Median (yr)`= "Median (yr)"
  )



colnames(exposure)
#! Exposure analysis
exposure <- read_excel('C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/Data/Teclistamab_SOC_2025-12-03.xlsx', sheet = 1)
 %>% select(Group, starts_with('AUC'), starts_with('CMAX'), starts_with('AMT'), `Ctrough(last obs-one wk)`, PID, TEC_NO)
  %>% filter(Group == 1, TEC_NO >= 5) %>% select(-Group)
exposure
colnames(exposure)
exposure_join <- matched_tidy %>% 
  left_join(exposure, by = "PID") %>%
  mutate(
#    AUCmean_AMT = AUCLST_mean/AMT_mean,
    AUCall_AMT = `AUCLST (cumulative)`/AMT_mean,
  )
colnames(exposure_join)
exposure_join <- exposure_join %>% 
  rename(AUCLST_cumulative = `AUCLST (cumulative)`,
         Ctrough_last_obs_one_wk = `Ctrough(last obs-one wk)`)
# print(exposure_join["AUCLST (cumulative)"], n=100)


##–– check whether any exposure metric (AUC / CMAX) is associated with OS or PFS –––––##
library(tidyverse)
library(survival)
library(broom)

## (1) prepare analysis data --------------------------------------------------------
expo_dat <- exposure_join |>
  mutate(
    ORR = if_else(RESP_CTX %in% c("CR",  "VGPR", "sCR"), 1, 0, missing = NA_real_)
  )


## list of exposure variables (all AUC* or CMAX* columns)  
expo_vars <- names(expo_dat) |> str_subset("^(AUCLST_mean|CMAX_max)")



# changed------
# 필요 패키지
library(broom)
library(rlang)


# Make DB for Resp-expo analysis------ 
Teclistamab <- expo_dat[expo_dat$Group=='Teclistamab',]
nrow(Teclistamab) # 19
length(Teclistamab$CMAX_max[!is.na(Teclistamab$CMAX_max)]) #11

ORR_expo <- function(Exposure_var){
  # Expo
  Tecli_Expo <- Teclistamab[!is.na(Teclistamab[[Exposure_var]]),]
  Expo_medi <- median(Tecli_Expo[[Exposure_var]])
  Tecli_Expo$G <- ifelse(Tecli_Expo[[Exposure_var]]>=Expo_medi,"Upper","Lower")
  
  Expo_sum <-
    Tecli_Expo |>
    mutate(G = factor(G, levels = c("Lower", "Upper"))) |>
    group_by(G) |>
    summarise(
      N = n(),
      ORR_num = sum(ORR==1, na.rm=T),
      ORR_pct = round(ORR_num / N * 100,2),
      .groups= "drop"
    )
  
  fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ G, data = Tecli_Expo)
  med_res <- summary(fit)
  Expo_med_pfs <- tibble(median=round(med_res$table[, c("median")],1))
  
  Expo_glm <- glm(ORR ~ G, data = Tecli_Expo, family = binomial()) |>
    tidy(exponentiate = T, conf.int=T) |>
    transmute(
      Variable = term,
      OR = estimate,
      OR_low = conf.low,
      OR_high = conf.high,
      p.value = p.value
    ) %>% 
    filter(Variable != "(Intercept)") %>% 
    transmute(
      G = case_when(grepl("Upper", Variable) ~ "Upper", TRUE ~ NA_character_),
      `OR (CI)` = sprintf("%.2f (%.3f, %.1f)", OR, OR_low, OR_high),
      p_value = sprintf("%.3f", p.value)  )

  Expo_final <- 
    Expo_sum %>% 
    bind_cols(Expo_med_pfs) %>% 
    left_join(Expo_glm, by = "G") %>% 
    mutate(
      ExposureGroup=sprintf("%s (μg/mL)",Exposure_var),
      Level = if_else(G=="Lower", sprintf("Lower(<%.2f)", Expo_medi),sprintf("Upper (≥%.2f)",Expo_medi)),
      ORR = sprintf('%d (%.0f%%)', ORR_num, ORR_pct),
      `Median PFS` = sprintf("%.1f", median),
      `OR (95% CI)` = if_else(G == "Lower", "Reference", `OR (CI)`),
      `P-value` = if_else(G=="Lower", "", p_value)
    ) %>% 
    select(
      ExposureGroup,
      Level,
      N,
      ORR,
      `Median PFS`,
      `OR (95% CI)`,
      `P-value`
    )
  
  Expo_final %>% 
    gt(groupname_col = "ExposureGroup") %>% 
    cols_label(
      Level = "Exposure Group",
      N = html("N"),
      ORR = "ORR",
      `Median PFS` = "Median PFS",
      `OR (95% CI)` = html("OR<br> (95% CI)"),
      `P-value` = "P-value"
    )
}
ORR_expo("CMAX_max")
ORR_expo("AUCLST_mean")



Tecli_Cmax <- Teclistamab[!is.na(Teclistamab[["CMAX_max"]]),]
Expo_medi <- median(Tecli_Cmax[["CMAX_max"]])
Tecli_Cmax$G <- ifelse(Tecli_Cmax[["CMAX_max"]]>=Expo_medi,"Upper","Lower")

setwd("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/2nd/[25-10-17] Figure")
fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ G, data = Tecli_Cmax)
cox_fit <- coxph(Surv(TIME / 365, VS_PFS_CTX) ~ G, data = Tecli_Cmax)
summary(cox_fit)
opt_plot(fit, name="image_PFS_Cmax", deadline=1.5, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")


Tecli_AUCLST <- Teclistamab[!is.na(Teclistamab[["AUCLST_mean"]]),]
Expo_medi <- median(Tecli_AUCLST[["AUCLST_mean"]])
Tecli_AUCLST$G <- ifelse(Tecli_AUCLST[["AUCLST_mean"]]>=Expo_medi,"Upper","Lower")

# AUClst
setwd("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/2nd/[25-10-17] Figure")
fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ G, data = Tecli_AUCLST)
cox_fit <- coxph(Surv(TIME / 365, VS_PFS_CTX) ~ G, data = Tecli_AUCLST)
summary(cox_fit)
opt_plot(fit, name="image_PFS_AUCLST", deadline=1.5, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")




library(dplyr)
library(ggplot2)
library(ggsci)

tmp <- copy(Teclistamab)
tmp <- tmp %>% 
  rename(`Cmax (μg/mL)`=CMAX_max,
         `AUClast  (μg/mL)`=AUCLST_mean)


# 예측 곡선용 데이터 생성 함수
mk_pred <- function(df, xvar) {
  d <- df[!is.na(df[[xvar]]), c("ORR", xvar)]
  names(d) <- c("ORR", "x")
  fit <- glm(ORR ~ x, data = d, family = binomial)
  newx <- seq(min(d$x), max(d$x), length.out = 200)
  tibble(
    Variable = xvar,
    x = newx,
    p = predict(fit, newdata = data.frame(x = newx), type = "response")
  )
}

# 원 점들
pts_df <- bind_rows(
  tibble(Variable = "CMAX_max",      x = Tecli_Cmax$CMAX_max,      ORR = Tecli_Cmax$ORR),
  tibble(Variable = "AUCLST_mean",   x = Tecli_AUCLST$AUCLST_mean, ORR = Tecli_AUCLST$ORR)
)

# 예측 곡선들
pred_df <- bind_rows(
  mk_pred(tmp, "Cmax (μg/mL)"),
  mk_pred(tmp, "AUClast  (μg/mL)")
)

pred_df$Variable <- factor(pred_df$Variable, levels=c("Cmax (μg/mL)","AUClast  (μg/mL)"))


p <- ggplot() +
#  geom_point(data = pts_df, aes(x = x, y = ORR), size = 1, alpha = 0.9) +
  geom_line(data = pred_df, aes(x = x, y = p, colour = Variable), linewidth = 1) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = -Inf, linewidth = 0.6, colour = "black") +
  geom_vline(xintercept = -Inf, linewidth = 0.6, colour = "black") +
  scale_colour_manual(values = pal_lancet()(2), labels = \(z) gsub("_"," ", z)) +
  scale_fill_manual(values = pal_lancet()(2), guide = "none") +
  facet_wrap(~ Variable, ncol = 2, scales = "free_x",
             labeller = labeller(Variable = \(x) gsub("_"," ", x))) +
  labs(title = NULL, x = "Exposure", y = "Response (ORR)", color = "Variable") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 14) +
  theme(
    legend.position    = "right",
    axis.line.x        = element_blank(),
    #    axis.text.x        = element_blank(),
    #    axis.ticks.x       = element_blank(),
    strip.background.x = element_blank()
  )

p

ggsave(filename = "C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/2nd/[25-10-17] Figure/Regplot_2panel.tiff",
       plot = p, device = "tiff", width = 8, height = 4.5, units = "in", dpi = 300)

# (8) Box plot (example AUC CMAX) --------------
library(reshape2)


library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)

# 1) 길게(pivot_longer) 변환: 두 변수만 뽑아 이름/순서 확정
expo_sbt <-
  Teclistamab %>%
  transmute(`Cmax (μg/mL)` = CMAX_max,
            `AUClast  (μg/mL)` = AUCLST_mean) %>%   # (주의) AUClast 뒤 공백 2칸은 의도적
  pivot_longer(cols = everything(),
               names_to = "Exposure", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(
    # 패싯 순서 고정: Cmax -> AUClast
    Exposure = factor(Exposure, levels = c("Cmax (μg/mL)", "AUClast  (μg/mL)"))
  )

# 2) 2분할 박스플롯
p <- expo_sbt %>% 
  ggplot(aes(x = factor(1), y = value, fill = Exposure)) +
  scale_fill_manual(values = pal_lancet()(2),
                    labels = function(x) gsub("_"," ", x)) +
  geom_boxplot(width = 0.35, outlier.shape = 21, outlier.stroke = 0.4) +
  geom_hline(yintercept = -Inf, linewidth = 0.6, colour = "black") +  # 바닥선
  facet_wrap(~ Exposure, ncol = 2, scales = "free_y",
             labeller = labeller(Exposure = function(x) gsub("_"," ", x))) +
  labs(title = NULL, x = NULL, y = "Value") +
  theme_classic(base_size = 14) +
  theme(
    legend.position    = "none",
    axis.line.x        = element_blank(),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    strip.background.x = element_blank()
  )

p

ggsave(filename = "C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/2nd/[25-10-17] Figure/Boxplot_2panel.tiff",
       plot = p, device = "tiff", width = 8, height = 4.5, units = "in", dpi = 300)


# ?????????????/Fit survival model (OS)
# 
expo_dat$ORR <- as.factor(expo_dat$ORR)
expo_dat$ORR <- factor(expo_dat$ORR, levels = c(0,1), labels = c("No", "Yes"))
# 
setwd("C:/Users/chaehyun/Dropbox/Work/PIPET/과제/혈액내과/Teclistamab/논문 작업/1st/[25-10-17] Figure")
fit <- survfit2(Surv(TIME2/30.42, VS) ~ ORR, data = expo_dat)
cox_fit <- coxph(Surv(TIME2 / 365, VS) ~ ORR,data = expo_dat)
summary(cox_fit)
opt_plot(fit, name="image_OS_ORR", deadline=2, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")

# Fit survival model (PFS)
fit <- survfit2(Surv(TIME/30.42, VS_PFS_CTX) ~ ORR, data = expo_dat)
cox_fit <- coxph(Surv(TIME / 365, VS_PFS_CTX) ~ ORR,data = expo_dat)
summary(cox_fit)
opt_plot(fit, name="image_PFS_ORR", deadline=1.5, conf = FALSE, ptitle = "up", ms = TRUE, cox = cox_fit, col = "")


# E-value---------
library(EValue)

# 낮을수록 좋다. 우연에 의해서 이 값이 나올 확률이 적다?
evalues.HR(est=0.529, lo=0.286, hi=0.979, rare=T) # 지난 E-value
round(evalues.HR(est=0.545, lo=0.302, hi=0.983, rare=T),2)

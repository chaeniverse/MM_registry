# =============================================================================
# LPL/WM Baseline Characteristics Table
# =============================================================================

# -- library -- #
# install.packages(c("tidyverse", "readxl", "gtsummary", "flextable", "officer"))
library(tidyverse)
library(readxl)
library(gtsummary)
library(flextable)
library(officer)

select <- dplyr::select

# -- paths -- #
DATA_PATH <- 'C:/Users/chaehyun/Dropbox/PIPET_Hematology/MM/Lpl/WM final.xlsx'
OUTPUT_DIR <- 'C:/Users/chaehyun/Dropbox/PIPET_Hematology/MM/Lpl/분석 결과'

# -- gtsummary theme (ref: 251208_Teclistamab_CH.r) -- #
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

# ---- 수정 2: 결측치 비율 + 10% 플래그 함수 추가 (새로 추가) ----
missing_rate_fn <- function(data, variable, ...) {
  n_miss <- sum(is.na(data[[variable]]))
  n_total <- nrow(data)
  p_miss <- n_miss / n_total * 100
  flag <- ifelse(p_miss >= 10, "\u2265 10%", "< 10%")
  
  ifelse(n_miss == 0,
         "0 (0.0%)",
         sprintf("%d (%.1f%%)", n_miss, p_miss))
}

# =============================================================================
# 1. CRF Sheet - Main Baseline Characteristics
# =============================================================================
crf <- read_excel(DATA_PATH, sheet = 'CRF') %>%
  rename(
    `1L` = `1L...9`,
    IgM_1 = `IgM...24`,
    IgM_2 = `IgM...25`,
    B2MG_cont = `B2MG...40`,
    B2MG_cat = `B2MG...41`
  )
colnames(crf)

tbl_crf <- crf %>%
  select(
    age, age65, sex, ECOG, PS, LNE, HS, spleen, liver,
         Hb, Hb10, Hb11, PLT, PLT100, ALB, ALB3.5, LDH, LDH2,
         IgM_2, IgM4, IgM7, B2MG_cont, B2MG_cat, IPSS, RIPSS, MSS, MYD88, CXCR4,
         B_Sx, image, sPEP, WBC, ANC # 추가
  ) %>%
  mutate(
    Hb = as.numeric(Hb),
    PLT = as.numeric(PLT),
    ALB = as.numeric(ALB),
    LDH = as.numeric(LDH),
    IgM_2 = as.numeric(IgM_2),
    B2MG_cont = as.numeric(B2MG_cont),
    sPEP = as.numeric(sPEP),
    WBC = as.numeric(WBC),
    ANC = as.numeric(ANC),
    age65 = factor(age65, levels = c(0, 1)),
    sex = factor(sex),
    PS = factor(PS, levels = c("high","low"), labels = c(0,1)),
    LNE = factor(LNE, levels = c(0, 1)),
    HS = factor(HS, levels = c(0, 1)),
    spleen = factor(spleen, levels = c(0, 1)),
    liver = factor(liver, levels = c(0, 1)),
    Hb10 = factor(Hb10, levels = c(0, 1)),
    Hb11 = factor(Hb11, levels = c(0, 1)),
    PLT100 = factor(PLT100, levels = c(0, 1)),
    ALB3.5 = factor(ALB3.5, levels = c(0, 1)),
    LDH2 = factor(LDH2, levels = c(0, 1)),
    IgM4 = factor(IgM4, levels = c(0, 1)),
    IgM7 = factor(IgM7, levels = c(0, 1)),
    B2MG_cat = factor(B2MG_cat, levels = c(0, 1)),
    IPSS = factor(IPSS, levels = c("1_Low", "2_Int", "3_High", "4_UK")),
    RIPSS = factor(RIPSS, levels = c("1_VL", "2_Low", "3_Int", "4_High", "5_VH", "6_UK")),
    MSS = factor(MSS, levels = c("1_Low", "2_low_Int", "3_Int", "4_High", "5_UK")),
    MYD88 = factor(MYD88, levels = c("MT", "WT")),
    CXCR4 = factor(CXCR4, levels = c("MT", "WT")),
    B_Sx = factor(B_Sx, levels = c(0, 1)),
    image = factor(image, levels = c(0, 1))
  ) %>%
  tbl_summary(
    label = list(
      age ~ "Age (years)",
      age65 ~ "Age > 65",
      sex ~ "Sex",
      ECOG ~ "ECOG",
      PS ~ "ECOG performance status < 2",
      LNE ~ "Lymphadenopathy",
      HS ~ "Hepatosplenomegaly",
      spleen ~ "Splenomegaly",
      liver ~ "Liver enlargement",
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
      image ~ "Imaging abnormalities",
      sPEP ~ "Serum protein electrophoresis (g/dL)",
      WBC ~ "WBC (/uL)",
      ANC ~ "ANC (/uL)"
    ),
    type = list(
      all_continuous() ~ "continuous2"  # 새로 추가: mean+median 두 줄 표시
    ),
    statistic = list(                   # 새로 추가: theme과 동일하지만 명시적으로
      all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})"),
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 1. Baseline Characteristics (CRF, N = {N})**") %>%
  add_stat(
    fns = everything() ~ missing_rate_fn    # header 제거
  ) %>%
  modify_header(add_stat_1 = "**Missing, n (%)**")  # 여기서 컬럼명 지정



tbl_tlt12 <- crf %>%
  select(
    TLT12,    # ---- 추가: 그룹 변수 ----
    age, age65, sex, ECOG, PS, LNE, HS, spleen, liver,
    Hb, Hb10, Hb11, PLT, PLT100, ALB, ALB3.5, LDH, LDH2,
    IgM_2, IgM4, IgM7, B2MG_cont, B2MG_cat, IPSS, RIPSS, MSS, MYD88, CXCR4,
    B_Sx, image, sPEP, WBC, ANC
  ) %>%
  mutate(
    TLT12 = factor(TLT12, levels = c(0, 1),              # ---- 추가 ----
                    labels = c("TLT12-negative", "TLT12-positive")),
    Hb = as.numeric(Hb),
    PLT = as.numeric(PLT),
    ALB = as.numeric(ALB),
    LDH = as.numeric(LDH),
    IgM_2 = as.numeric(IgM_2),
    B2MG_cont = as.numeric(B2MG_cont),
    sPEP = as.numeric(sPEP),
    WBC = as.numeric(WBC),
    ANC = as.numeric(ANC),
    age65 = factor(age65, levels = c(0, 1)),
    sex = factor(sex),
    PS = factor(PS, levels = c("high","low"), labels = c(0,1)),
    LNE = factor(LNE, levels = c(0, 1)),
    HS = factor(HS, levels = c(0, 1)),
    spleen = factor(spleen, levels = c(0, 1)),
    liver = factor(liver, levels = c(0, 1)),
    Hb10 = factor(Hb10, levels = c(0, 1)),
    Hb11 = factor(Hb11, levels = c(0, 1)),
    PLT100 = factor(PLT100, levels = c(0, 1)),
    ALB3.5 = factor(ALB3.5, levels = c(0, 1)),
    LDH2 = factor(LDH2, levels = c(0, 1)),
    IgM4 = factor(IgM4, levels = c(0, 1)),
    IgM7 = factor(IgM7, levels = c(0, 1)),
    B2MG_cat = factor(B2MG_cat, levels = c(0, 1)),
    IPSS = factor(IPSS, levels = c("1_Low", "2_Int", "3_High", "4_UK")),
    RIPSS = factor(RIPSS, levels = c("1_VL", "2_Low", "3_Int", "4_High", "5_VH", "6_UK")),
    MSS = factor(MSS, levels = c("1_Low", "2_low_Int", "3_Int", "4_High", "5_UK")),
    MYD88 = factor(MYD88, levels = c("MT", "WT")),
    CXCR4 = factor(CXCR4, levels = c("MT", "WT")),
    B_Sx = factor(B_Sx, levels = c(0, 1)),
    image = factor(image, levels = c(0, 1))
  ) %>%
  tbl_summary(
    by = TLT12,    # ---- 추가: 그룹 비교 ----
    label = list(
      age ~ "Age (years)",
      age65 ~ "Age > 65",
      sex ~ "Sex",
      ECOG ~ "ECOG",
      PS ~ "ECOG performance status < 2",
      LNE ~ "Lymphadenopathy",
      HS ~ "Hepatosplenomegaly",
      spleen ~ "Splenomegaly",
      liver ~ "Liver enlargement",
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
      image ~ "Imaging abnormalities",
      sPEP ~ "Serum protein electrophoresis (g/dL)",
      WBC ~ "WBC (/uL)",
      ANC ~ "ANC (/uL)"
    ),
    type = list(
      all_continuous() ~ "continuous2"  # 새로 추가: mean+median 두 줄 표시
    ),
    statistic = list(                   # 새로 추가: theme과 동일하지만 명시적으로
      all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})"),
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  add_p() %>%                    # ---- 추가: p-value ----
  add_overall() %>%              # ---- 추가: 전체 열 ----
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 1. Baseline Characteristics (CRF, N = {N})**") %>%
  add_stat(
    fns = everything() ~ missing_rate_fn    # header 제거
  ) %>%
  modify_header(add_stat_1 = "**Missing, n (%)**")  # 여기서 컬럼명 지정


tbl_tnt12 <- crf %>%
  select(
    TNT12,    # ---- 추가: 그룹 변수 ----
    age, age65, sex, ECOG, PS, LNE, HS, spleen, liver,
    Hb, Hb10, Hb11, PLT, PLT100, ALB, ALB3.5, LDH, LDH2,
    IgM_2, IgM4, IgM7, B2MG_cont, B2MG_cat, IPSS, RIPSS, MSS, MYD88, CXCR4,
    B_Sx, image, sPEP, WBC, ANC,TLT,
        TLT12,    # ---- 추가: Table 4 추가 변수 ----
    `1L`     # ---- 추가: Table 4 추가 변수 ----
  ) %>%
  mutate(
    TNT12 = factor(TNT12, levels = c(0, 1),              # ---- 추가 ----
                    labels = c("TNT12-negative", "TNT12-positive")),
    TLT12 = factor(TLT12, levels = c(0, 1)),
    `1L` = factor(`1L`),                                   # ---- 추가 ----
    TLT = as.numeric(TLT),                                   # ---- 추가 ----
    Hb = as.numeric(Hb),
    PLT = as.numeric(PLT),
    ALB = as.numeric(ALB),
    LDH = as.numeric(LDH),
    IgM_2 = as.numeric(IgM_2),
    B2MG_cont = as.numeric(B2MG_cont),
    sPEP = as.numeric(sPEP),
    WBC = as.numeric(WBC),
    ANC = as.numeric(ANC),
    age65 = factor(age65, levels = c(0, 1)),
    sex = factor(sex),
    PS = factor(PS, levels = c("high","low"), labels = c(0,1)),
    LNE = factor(LNE, levels = c(0, 1)),
    HS = factor(HS, levels = c(0, 1)),
    spleen = factor(spleen, levels = c(0, 1)),
    liver = factor(liver, levels = c(0, 1)),
    Hb10 = factor(Hb10, levels = c(0, 1)),
    Hb11 = factor(Hb11, levels = c(0, 1)),
    PLT100 = factor(PLT100, levels = c(0, 1)),
    ALB3.5 = factor(ALB3.5, levels = c(0, 1)),
    LDH2 = factor(LDH2, levels = c(0, 1)),
    IgM4 = factor(IgM4, levels = c(0, 1)),
    IgM7 = factor(IgM7, levels = c(0, 1)),
    B2MG_cat = factor(B2MG_cat, levels = c(0, 1)),
    IPSS = factor(IPSS, levels = c("1_Low", "2_Int", "3_High", "4_UK")),
    RIPSS = factor(RIPSS, levels = c("1_VL", "2_Low", "3_Int", "4_High", "5_VH", "6_UK")),
    MSS = factor(MSS, levels = c("1_Low", "2_low_Int", "3_Int", "4_High", "5_UK")),
    MYD88 = factor(MYD88, levels = c("MT", "WT")),
    CXCR4 = factor(CXCR4, levels = c("MT", "WT")),
    B_Sx = factor(B_Sx, levels = c(0, 1)),
    image = factor(image, levels = c(0, 1))
  ) %>%
  tbl_summary(
    by = TNT12,    # ---- 추가: 그룹 비교 ----
    label = list(
      age ~ "Age (years)",
      age65 ~ "Age > 65",
      sex ~ "Sex",
      ECOG ~ "ECOG",
      PS ~ "ECOG performance status < 2",
      LNE ~ "Lymphadenopathy",
      HS ~ "Hepatosplenomegaly",
      spleen ~ "Splenomegaly",
      liver ~ "Liver enlargement",
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
      image ~ "Imaging abnormalities",
      sPEP ~ "Serum protein electrophoresis (g/dL)",
      WBC ~ "WBC (/uL)",
      ANC ~ "ANC (/uL)",
      TLT ~ "Time to first-line Tx, months",   # ---- 추가 ----
            TLT12 ~ "TLT < 12 months",            # ---- 추가 ----
      `1L` ~ "First-line treatment"          # ---- 추가 ----
    ),
    type = list(
      all_continuous() ~ "continuous2"  # 새로 추가: mean+median 두 줄 표시
    ),
    statistic = list(                   # 새로 추가: theme과 동일하지만 명시적으로
      all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}-{p75})"),
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  add_p() %>%                    # ---- 추가: p-value ----
  add_overall() %>%              # ---- 추가: 전체 열 ----
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 1. Baseline Characteristics (CRF, N = {N})**") %>%
  add_stat(
    fns = everything() ~ missing_rate_fn    # header 제거
  ) %>%
  modify_header(add_stat_1 = "**Missing, n (%)**")  # 여기서 컬럼명 지정


doc <- read_docx() %>%
  # ---- Table S1: 전체 baseline (tbl_crf) ----
  body_add_par("Table S1. Baseline Characteristics of All Patients", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_crf), align = "left") %>%
  body_add_break() %>%
  # ---- Table 1: by TLT12 (tbl_tlt12) ---- 기존 tbl_1l → tbl_tlt12로 변경
  body_add_par("Table 1. Baseline Characteristics by TLT12 Status", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_tlt12), align = "left") %>%
  body_add_break() %>%
  # ---- Table 4: by TNT12 (tbl_tnt12) ---- 새로 추가
  body_add_par("Table 4. Baseline Characteristics by TNT12 Status", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_tnt12), align = "left")

print(doc, target = file.path(OUTPUT_DIR, "Baseline.docx"))
cat("Saved to:", file.path(OUTPUT_DIR, "Baseline.docx"), "\n")

# 아래부터 다시 보면서 하기 # =============================================================================

# =============================================================================
# 2. 1L Tx Sheet - First-line Treatment Characteristics
# =============================================================================
tx1l <- read_excel(DATA_PATH, sheet = '1L Tx')

tbl_1l <- tx1l %>%
  mutate(
    `1L` = factor(`1L`,
                  levels = c("1_BR", "2_R_Cy", "3_R_borte", "4_others"),
                  labels = c("BR", "R-Cyclophosphamide", "R-Bortezomib", "Others")),
    subgroup = factor(subgroup),
    BR = factor(BR,
                levels = c("1_CR", "2_VGPR", "3_PR", "4_MR", "5_SD", "6_PD", "7_NE"),
                labels = c("CR", "VGPR", "PR", "MR", "SD", "PD", "NE")),
    ORR = factor(ORR, levels = c(0, 1), labels = c("No", "Yes")),
    MRR = factor(MRR, levels = c(0, 1), labels = c("No", "Yes")),
    VGCR = factor(VGCR, levels = c(0, 1), labels = c("No", "Yes")),
    PD = factor(PD, levels = c(0, 1), labels = c("No", "Yes")),
    death = factor(death, levels = c(0, 1), labels = c("Alive", "Dead"))
  ) %>%
  select(`1L`, subgroup, cycle, Tx_Dr, BR, ORR, MRR, VGCR, PD, PFS, death) %>%
  tbl_summary(
    label = list(
      `1L` ~ "1L Regimen",
      subgroup ~ "Treatment subgroup",
      cycle ~ "Number of cycles",
      Tx_Dr ~ "Treatment duration (months)",
      BR ~ "Best response",
      ORR ~ "Overall response rate (>= PR)",
      MRR ~ "Major response rate (>= MR)",
      VGCR ~ ">= VGPR",
      PD ~ "Progression",
      PFS ~ "PFS (months)",
      death ~ "Vital status"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 2. First-line Treatment Characteristics (N = {N})**")

# =============================================================================
# 3. Tx Sheet - Treatment Overview (all lines)
# =============================================================================
# Treatment line summary from CRF
tx_summary <- crf %>%
  mutate(
    `치료 line` = as.numeric(`치료 line`),
    Tx_line_cat = case_when(
      `치료 line` == 0 ~ "No treatment",
      `치료 line` == 1 ~ "1 line",
      `치료 line` == 2 ~ "2 lines",
      `치료 line` >= 3 ~ ">= 3 lines",
      TRUE ~ "Unknown"
    ),
    Tx_line_cat = factor(Tx_line_cat,
                         levels = c("No treatment", "1 line", "2 lines", ">= 3 lines", "Unknown"))
  )

tbl_tx <- tx_summary %>%
  select(`치료 line`, Tx_line_cat) %>%
  tbl_summary(
    label = list(
      `치료 line` ~ "Number of treatment lines",
      Tx_line_cat ~ "Treatment lines (categorized)"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 3. Treatment Overview (N = {N})**")

# =============================================================================
# 4. BTK Sheet - BTK Inhibitor Treatment Characteristics
# =============================================================================
btk <- read_excel(DATA_PATH, sheet = 'BTK')

tbl_btk <- btk %>%
  mutate(
    agent = factor(agent),
    BTK_line = as.numeric(BTK_line),
    BR = factor(BR,
                levels = c("1_CR", "2_VGPR", "3_PR", "4_MR", "5_SD", "6_PD", "7_NE"),
                labels = c("CR", "VGPR", "PR", "MR", "SD", "PD", "NE")),
    ORR = factor(ORR, levels = c(0, 1), labels = c("No", "Yes")),
    MRR = factor(MRR, levels = c(0, 1), labels = c("No", "Yes")),
    VGCR = factor(VGCR, levels = c(0, 1), labels = c("No", "Yes")),
    PD = factor(PD, levels = c(0, 1), labels = c("No", "Yes")),
    death = factor(death, levels = c(0, 1), labels = c("Alive", "Dead")),
    `N-fever` = factor(`N-fever`, levels = c(0, 1, 2, 3, 4),
                       labels = c("None", "Grade 1", "Grade 2", "Grade 3", "Grade 4")),
    `T-penia` = factor(`T-penia`, levels = c(0, 1, 2, 3, 4),
                       labels = c("None", "Grade 1", "Grade 2", "Grade 3", "Grade 4")),
    anemia = factor(anemia, levels = c(0, 1, 2, 3, 4),
                    labels = c("None", "Grade 1", "Grade 2", "Grade 3", "Grade 4")),
    infection = factor(infection, levels = c(0, 1, 2, 3, 4),
                       labels = c("None", "Grade 1", "Grade 2", "Grade 3", "Grade 4")),
    neuropathy = factor(neuropathy, levels = c(0, 1, 2, 3, 4),
                        labels = c("None", "Grade 1", "Grade 2", "Grade 3", "Grade 4")),
    diarrhea = factor(diarrhea, levels = c(0, 1, 2, 3, 4),
                      labels = c("None", "Grade 1", "Grade 2", "Grade 3", "Grade 4")),
    bleeding = factor(bleeding, levels = c(0, 1, 2, 3, 4),
                      labels = c("None", "Grade 1", "Grade 2", "Grade 3", "Grade 4")),
    cardiac = factor(cardiac, levels = c(0, 1, 2, 3, 4),
                     labels = c("None", "Grade 1", "Grade 2", "Grade 3", "Grade 4"))
  ) %>%
  select(agent, BTK_line, cycle, Tx_Dr, BR, ORR, MRR, VGCR, PD, death,
         `N-fever`, `T-penia`, anemia, infection, neuropathy, diarrhea, bleeding, cardiac) %>%
  tbl_summary(
    label = list(
      agent ~ "BTK inhibitor agent",
      BTK_line ~ "BTK treatment line",
      cycle ~ "Number of cycles",
      Tx_Dr ~ "Treatment duration (months)",
      BR ~ "Best response",
      ORR ~ "Overall response rate (>= PR)",
      MRR ~ "Major response rate (>= MR)",
      VGCR ~ ">= VGPR",
      PD ~ "Progression",
      death ~ "Vital status",
      `N-fever` ~ "Neutropenic fever",
      `T-penia` ~ "Thrombocytopenia",
      anemia ~ "Anemia",
      infection ~ "Infection",
      neuropathy ~ "Neuropathy",
      diarrhea ~ "Diarrhea",
      bleeding ~ "Bleeding",
      cardiac ~ "Cardiac toxicity"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 4. BTK Inhibitor Treatment Characteristics (N = {N})**")

# =============================================================================
# 5. TLT TNT 분류 Sheet
# =============================================================================
tlt_tnt_cls <- read_excel(DATA_PATH, sheet = 'TLT TNT 분류')

tbl_tlt_tnt_cls <- tlt_tnt_cls %>%
  mutate(
    TLT24 = factor(TLT24, levels = c(0, 1), labels = c("No", "Yes")),
    TNT24 = factor(TNT24, levels = c(0, 1), labels = c("No", "Yes")),
    TLT18 = factor(TLT18, levels = c(0, 1), labels = c("No", "Yes")),
    TNT18 = factor(TNT18, levels = c(0, 1), labels = c("No", "Yes")),
    TLT12 = factor(TLT12, levels = c(0, 1), labels = c("No", "Yes")),
    TNT12 = factor(TNT12, levels = c(0, 1), labels = c("No", "Yes")),
    death = factor(death, levels = c(0, 1), labels = c("Alive", "Dead"))
  ) %>%
  select(`Tx line`, TLT24, TNT24, TLT18, TNT18, TLT12, TNT12, death) %>%
  tbl_summary(
    label = list(
      `Tx line` ~ "Number of treatment lines",
      TLT24 ~ "TLT event at 24 months",
      TNT24 ~ "TNT event at 24 months",
      TLT18 ~ "TLT event at 18 months",
      TNT18 ~ "TNT event at 18 months",
      TLT12 ~ "TLT event at 12 months",
      TNT12 ~ "TNT event at 12 months",
      death ~ "Vital status"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 5. TLT/TNT Classification (N = {N})**")

# =============================================================================
# 6. TLT Sheet
# =============================================================================
tlt <- read_excel(DATA_PATH, sheet = 'TLT') %>%
  rename(OS_TLT24 = `OS...13`, OS_TLT18 = `OS...16`, OS_TLT12 = `OS...19`)

tbl_tlt <- tlt %>%
  mutate(
    TLT24 = factor(TLT24, levels = c(0, 1), labels = c("No", "Yes")),
    death = factor(death, levels = c(0, 1), labels = c("Alive", "Dead")),
    COD_WM = factor(COD_WM, levels = c(0, 1), labels = c("No", "Yes"))
  ) %>%
  select(`Tx line`, `1L`, TLT, TLT24, death, COD_WM) %>%
  tbl_summary(
    label = list(
      `Tx line` ~ "Number of treatment lines",
      `1L` ~ "First-line regimen",
      TLT ~ "Time to next line of treatment (months)",
      TLT24 ~ "TLT event at 24 months",
      death ~ "Vital status",
      COD_WM ~ "WM-related death"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 6. Time to Next Line of Treatment (TLT, N = {N})**")

# =============================================================================
# 7. TNT Sheet
# =============================================================================
tnt <- read_excel(DATA_PATH, sheet = 'TNT') %>%
  rename(OS_TNT24 = `OS...16`, OS_TNT18 = `OS...19`, OS_TNT12 = `OS...22`)

tbl_tnt <- tnt %>%
  mutate(
    TNT24 = factor(TNT24, levels = c(0, 1), labels = c("No", "Yes")),
    RTX = factor(RTX, levels = c(0, 1), labels = c("No", "Yes")),
    death = factor(death, levels = c(0, 1), labels = c("Alive", "Dead")),
    COD_WM = factor(COD_WM, levels = c(0, 1), labels = c("No", "Yes"))
  ) %>%
  select(`Tx line`, `1L`, RTX, TNT, TNT24, death, COD_WM) %>%
  tbl_summary(
    label = list(
      `Tx line` ~ "Number of treatment lines",
      `1L` ~ "First-line regimen",
      RTX ~ "Rituximab-based",
      TNT ~ "Time to next treatment (months)",
      TNT24 ~ "TNT event at 24 months",
      death ~ "Vital status",
      COD_WM ~ "WM-related death"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 7. Time to Next Treatment (TNT, N = {N})**")

# =============================================================================
# 8. Cox Sheet
# =============================================================================
cox <- read_excel(DATA_PATH, sheet = 'Cox')

tbl_cox <- cox %>%
  mutate(
    age65 = factor(age65, levels = c(0, 1), labels = c("< 65", ">= 65")),
    sex = factor(sex),
    PS = factor(PS, levels = c("low", "high"), labels = c("Low", "High")),
    LNE = factor(LNE, levels = c(0, 1), labels = c("No", "Yes")),
    HS = factor(HS, levels = c(0, 1), labels = c("No", "Yes")),
    IgM4 = factor(IgM4, levels = c(0, 1), labels = c("< 4000", ">= 4000")),
    IgM7 = factor(IgM7, levels = c(0, 1), labels = c("< 7000", ">= 7000")),
    Hb10 = factor(Hb10, levels = c(0, 1), labels = c(">= 10", "< 10")),
    Hb11 = factor(Hb11, levels = c(0, 1), labels = c(">= 11.5", "< 11.5")),
    PLT = factor(PLT, levels = c(0, 1), labels = c(">= 100", "< 100")),
    LDH = factor(LDH, levels = c(0, 1), labels = c("Normal", "Elevated")),
    ALB = factor(ALB, levels = c(0, 1), labels = c(">= 3.5", "< 3.5")),
    B2MG = factor(B2MG, levels = c(0, 1), labels = c("Normal", "Elevated")),
    RIPSS = factor(RIPSS, levels = c("1_VL", "2_Low", "3_Int", "4_High", "5_VH", "6_UK"),
                   labels = c("Very Low", "Low", "Intermediate", "High", "Very High", "Unknown")),
    MSS = factor(MSS, levels = c("1_Low", "2_low_Int", "3_Int", "4_High", "5_UK"),
                 labels = c("Low", "Low-Intermediate", "Intermediate", "High", "Unknown")),
    MYD88 = factor(MYD88, levels = c("1_WT", "2_MT", "3_UK"),
                   labels = c("Wild-type", "Mutant", "Unknown")),
    CXCR4 = factor(CXCR4, levels = c("1_WT", "2_MT", "3_UK"),
                   labels = c("Wild-type", "Mutant", "Unknown")),
    RTX = factor(RTX, levels = c(0, 1), labels = c("No", "Yes")),
    BR = factor(BR, levels = c(0, 1), labels = c("No", "Yes")),
    BTK = factor(BTK, levels = c(0, 1), labels = c("No", "Yes")),
    death = factor(death, levels = c(0, 1), labels = c("Alive", "Dead")),
    COD_WM = factor(COD_WM, levels = c(0, 1), labels = c("No", "Yes"))
  ) %>%
  select(age, age65, sex, PS, LNE, HS, IgM, IgM4, IgM7, Hb10, Hb11,
         PLT, LDH, ALB, B2MG, RIPSS, MSS, MYD88, CXCR4,
         `Tx line`, RTX, BR, BTK, death, COD_WM) %>%
  tbl_summary(
    label = list(
      age ~ "Age (years)",
      age65 ~ "Age >= 65",
      sex ~ "Sex",
      PS ~ "Performance status",
      LNE ~ "Lymphadenopathy",
      HS ~ "Hepatosplenomegaly",
      IgM ~ "IgM (mg/dL)",
      IgM4 ~ "IgM >= 4000",
      IgM7 ~ "IgM >= 7000",
      Hb10 ~ "Hb < 10 g/dL",
      Hb11 ~ "Hb < 11.5 g/dL",
      PLT ~ "Platelet < 100K",
      LDH ~ "LDH elevation",
      ALB ~ "Albumin < 3.5 g/dL",
      B2MG ~ "B2MG elevation",
      RIPSS ~ "rIPSS-WM",
      MSS ~ "MSS",
      MYD88 ~ "MYD88",
      CXCR4 ~ "CXCR4",
      `Tx line` ~ "Treatment lines",
      RTX ~ "Rituximab-based 1L",
      BR ~ "BR 1L",
      BTK ~ "BTK inhibitor used",
      death ~ "Vital status",
      COD_WM ~ "WM-related death"
    ),
    digits = list(all_continuous() ~ 1, all_categorical() ~ c(0, 1)),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_caption("**Table 8. Cox Model Variables Summary (N = {N})**")

# =============================================================================
# Save all tables to a single Word document
# =============================================================================
doc <- read_docx() %>%
  body_add_par("WM/LPL Baseline Characteristics Tables", style = "heading 1") %>%
  body_add_par("") %>%
  body_add_par("Table 1. Baseline Characteristics (CRF)", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_crf), align = "left") %>%
  body_add_break() %>%
  body_add_par("Table 2. First-line Treatment Characteristics", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_1l), align = "left") %>%
  body_add_break() %>%
  body_add_par("Table 3. Treatment Overview", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_tx), align = "left") %>%
  body_add_break() %>%
  body_add_par("Table 4. BTK Inhibitor Treatment Characteristics", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_btk), align = "left") %>%
  body_add_break() %>%
  body_add_par("Table 5. TLT/TNT Classification", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_tlt_tnt_cls), align = "left") %>%
  body_add_break() %>%
  body_add_par("Table 6. Time to Next Line of Treatment (TLT)", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_tlt), align = "left") %>%
  body_add_break() %>%
  body_add_par("Table 7. Time to Next Treatment (TNT)", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_tnt), align = "left") %>%
  body_add_break() %>%
  body_add_par("Table 8. Cox Model Variables Summary", style = "heading 2") %>%
  body_add_flextable(as_flex_table(tbl_cox), align = "left")

print(doc, target = file.path(OUTPUT_DIR, "Baseline.docx"))
cat("Saved to:", file.path(OUTPUT_DIR, "Baseline.docx"), "\n")

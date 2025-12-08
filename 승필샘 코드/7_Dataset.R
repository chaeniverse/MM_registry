# -- library -- #
if (!require(tidyverse)){ install.packages('tidyverse'); library(tidyverse)}
if (!require(readxl)){ install.packages('readxl'); library(readxl)}
if (!require(plotly)){ install.packages('plotly'); library(plotly)}
if (!require(gtsummary)){ install.packages('gtsummary'); library(gtsummary)}
if (!require(kableExtra)){ install.packages('kableExtra'); library(kableExtra)}
if (!require(MatchIt)){ install.packages('MatchIt'); library(MatchIt)}
# if (!require(optmatch)){ install.packages('optmatch'); library(optmatch)}
if (!require(MatchThem)){ install.packages('MatchThem'); library(MatchThem)}
if (!require(cobalt)){ install.packages('cobalt'); library(cobalt)}
if (!require(survival)){ install.packages('survival'); library(survival)}
if (!require(survminer)){ install.packages('survminer'); library(survminer)}
if (!require(ggfortify)){ install.packages('ggfortify'); library(ggfortify)}
if (!require(ggsci)){ install.packages('ggsci'); library(ggsci)}
if (!require(flextable)){ install.packages('flextable'); library(flextable)}
if (!require(officer)){ install.packages('officer'); library(officer)}
if (!require(mice)){ install.packages('mice'); library(mice)}
if (!require(forestplot)){ install.packages('forestplot'); library(forestplot)}
if (!require(forestploter)){ install.packages('forestploter'); library(forestploter)}
if (!require(ipw)){ install.packages('ipw'); library(ipw)}
if (!require(survey)){ install.packages('ipw'); library(survey)}
if (!require(ggplotify)){ install.packages('ggplotify'); library(ggplotify)}
if (!require(patchwork)){ install.packages('patchwork'); library(patchwork)}
if (!require(EValue)){ install.packages('EValue'); library(EValue)}
if (!require(tableone)){ install.packages('tableone'); library(tableone)}
if (!require(adjustedCurves)){ install.packages('adjustedCurves'); library(adjustedCurves)}


# -- data -- #
my_theme <-
  list(
    "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 2),
    "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 2, prepend_p = TRUE),
    "tbl_summary-arg:statistic" = list(all_continuous() ~ "{mean} ± {sd} ({median} [{p0}-{p100}])",
                                       all_categorical() ~ "{n} ({p}%)")
  )
set_gtsummary_theme(my_theme)
theme_gtsummary_compact()

case <- read_excel('data/서울대merge_TCE_vs_SOC_MM_240326_PFS2_01coding.xlsx', skip=8, sheet = 'new(서울대merge)n=96')
control <- read_excel('data/서울대merge_TCE_vs_SOC_MM_240326_PFS2_01coding.xlsx', skip=4, sheet = 'old(n=255)')

case$cohort <- 1
control$cohort <- 0

case.s <- case %>% select(PID, NAME, SEX, CTX_S_DATE, BIRTH, Heavychain, Lightchain, ISS_DX, 
                          FGFR3_IGH_DX_VALUE,TP53_DX_VALUE, MAF_IGH_DX_VALUE, CCND1_IGH_DX_VALUE, RB1_DX_VALUE, `1Q_DX`, 
                          B2MG_CTX, ALB_CTX, LDH_CTX, DATE_DX, CTX_LINE, HB_CTX, ANC_CTX, ALC_CTX, PLT_CTX, CREAT_CTX, RESP_CTX, 
                          DAYS_VS_PFS_CTX, VS_PFS_CTX, DATE_LAST, VS, cohort, CTX_NAME, CD138_CTX)

case.s <- case.s %>%
  mutate(DATE_DX = as.character(DATE_DX),
         DATE_DX = case_when(
           str_detect(DATE_DX, "UK") ~ as.Date(NA),
           as.numeric(DATE_DX) > 60000 ~ as.Date(as.POSIXct(as.numeric(DATE_DX), origin = "1970-01-01", tz = "UTC")), # Handle Unix timestamps
           as.numeric(DATE_DX) <= 60000 ~ as.Date(as.numeric(DATE_DX), origin = "1899-12-30"), # Handle Excel serial numbers
           TRUE ~ as.Date(NA)
         ))

case.s.line_cleaned <- case.s %>% 
  filter(CTX_LINE > 2) %>%
  mutate(CTX_LINE = if_else(CTX_LINE > 6, 6, CTX_LINE))


control.s <- control %>% select(PID, NAME, SEX, CTX_S_DATE, BIRTH, Heavychain, Lightchain, ISS_DX, 
                                FGFR3_IGH_DX_VALUE,TP53_DX_VALUE, MAF_IGH_DX_VALUE, CCND1_IGH_DX_VALUE, RB1_DX_VALUE, `1Q_DX`, 
                                B2MG_CTX, ALB_CTX, LDH_CTX, DATE_DX = DATE_DX, CTX_LINE, HB_CTX, ANC_CTX, ALC_CTX, PLT_CTX, CREAT_CTX, RESP_CTX, 
                                DAYS_VS_PFS_CTX, VS_PFS_CTX, DATE_LAST, VS, cohort, CTX_NAME, CD138_CTX)

data0 <- rbind(case.s.line_cleaned, control.s)
data0$DATE_DIF <- as.numeric(data0$DATE_LAST - data0$CTX_S_DATE) / 30.42
data0$DAYS_VS_PFS_CTX = data0$DAYS_VS_PFS_CTX / 30.42

data0$DATE_DIF[data0$VS == 0 & data0$VS_PFS_CTX == 0] = 
  data0$DAYS_VS_PFS_CTX[data0$VS == 0 & data0$VS_PFS_CTX == 0]

data <- data0  %>% 
  mutate_at(c(8:16, 20:24), as.numeric) %>%
  mutate(cohort = factor(cohort, levels = c(0, 1), labels = c("Old", "New")),
         AGE = year(data0$CTX_S_DATE) - year(data0$BIRTH),
         `Type of Myeloma` = Heavychain,
         `Light chain type` = Lightchain,
         `ISS stage at diagnosis` = case_when(is.na(ISS_DX) ~ NA_character_,    
                                              ISS_DX == 1 ~ 'I',            
                                              ISS_DX == 2 ~ 'II',
                                              ISS_DX == 3 ~ 'III',
                                              is.na(ISS_DX) ~ "Unknown"),
         `ISS stage 3_ISS0` = case_when(is.na(ISS_DX) ~ 0, # simple imputation
                                           ISS_DX == 3 ~ 1,
                                           TRUE ~ 0),    
         `ISS stage 3_factored` = case_when(is.na(ISS_DX) ~ "unknown", # factored unknown
                                            ISS_DX == 3 ~ "1",
                                            TRUE ~ "0"),
         `ISS stage 3_mice` = case_when(is.na(ISS_DX) ~ NA_real_, # multiple imputation
                                        ISS_DX == 3 ~ 1,
                                        TRUE ~ 0),
         `t(4:14), cf. UN` = case_when(is.na(FGFR3_IGH_DX_VALUE) ~ NA_real_,    
                                       FGFR3_IGH_DX_VALUE > 0 ~ 1,            
                                       FGFR3_IGH_DX_VALUE == 0 ~ 0 ),
         `del(17p), cf. UN` = case_when(is.na(TP53_DX_VALUE) ~ NA_real_,    
                                        TP53_DX_VALUE > 0 ~ 1,            
                                        TP53_DX_VALUE == 0 ~ 0 ),
         `t(14;16), cf. UN` = case_when(is.na(MAF_IGH_DX_VALUE) ~ NA_real_,    
                                        MAF_IGH_DX_VALUE > 0 ~ 1,            
                                        MAF_IGH_DX_VALUE == 0 ~ 0 ),
         `t(11:14), cf. UN` = case_when(is.na(CCND1_IGH_DX_VALUE) ~ NA_real_,    
                                        CCND1_IGH_DX_VALUE > 0 ~ 1,            
                                        CCND1_IGH_DX_VALUE == 0 ~ 0 ),
         `del(13q), cf. UN` = case_when(is.na(RB1_DX_VALUE) ~ NA_real_,    
                                        RB1_DX_VALUE > 0 ~ 1,            
                                        RB1_DX_VALUE == 0 ~ 0 ),
         `amp(1q21), cf. UN` = case_when(is.na(`1Q_DX`) ~ NA_real_,    
                                         `1Q_DX` > 0 ~ 1,            
                                         `1Q_DX` == 0 ~ 0 ),
         `β2-microglobuline` = B2MG_CTX,
         `β2-microglobuline >=5.5 µg/mL` = if_else(B2MG_CTX >= 5.5, 1, 0),
         `Albumin` = ALB_CTX,
         `Albumin >=3.5 mg/dL` = if_else(ALB_CTX >= 3.5, 1, 0),
         # `Lactate dehydrogenase ≥ ULN` = if_else(LDH_HIGH1_CTX == 'Y', 'high', 'low'), #upper limit of normal (ULN) (225 U/L)
         `Lactate dehydrogenase ≥ ULN` = if_else(LDH_CTX > 225, 'high', 'low'), #upper limit of normal (ULN) (225 U/L)
         `Time to treatment with chemotherapy` = as.numeric(interval(DATE_DX, CTX_S_DATE) %/% months(1)), 
         `Line of chemotherapy` = as.numeric(CTX_LINE),
         `Hemoglobin` = HB_CTX,
         `Hemoglobin <7.5 g/dL` = if_else(HB_CTX < 7.5, 1, 0),
         `Absoulte neutrophil count` = ANC_CTX,
         `Absoulte neutrophil count <1.0 x 10^9/L` = if_else(ANC_CTX < 1.0, 1,0),
         `Absoulte lymphocyte count` = ALC_CTX,
         `Platelet count` = PLT_CTX,
         `Platelet count <50 x 10^9/L` = if_else(PLT_CTX < 50, 1, 0),
         `Creatinine` = CREAT_CTX,
         `Creatinine >2.0 mg/dL` = if_else(CREAT_CTX > 2.0, 1, 0))

data <- data %>%
  mutate(CTX_NAME = str_to_title(CTX_NAME)) %>%
  mutate(CTX_NAME_TIDY = case_when(
    CTX_NAME == "Teclistamab + Talquetamab" ~ "Teclistamab + Talquetamab",
    CTX_NAME == "Elranatamab + Cevostamab" ~ "Elranatamab + Cevostamab",
    str_detect(CTX_NAME, "Elrana") | str_detect(CTX_NAME, "Erlana") ~ "Elranatamab",
    str_detect(CTX_NAME, "Teclista") ~ "Teclistamab",
    str_detect(CTX_NAME, "Talquetamab") ~ "Talquetamab",
    str_detect(CTX_NAME, "Regeneron") ~ "Linvoseltamab",
    TRUE ~ CTX_NAME
  )) %>%
  mutate(
    CTX_GROUP = case_when(
      CTX_NAME_TIDY %in% c("Teclistamab + Talquetamab", "Elranatamab + Cevostamab") ~ "Combi",
      CTX_NAME_TIDY %in% c("Elranatamab", "Linvoseltamab", "Teclistamab", "Car-T Cell Therapy") ~ "BCMAxCD3 bispecific",
      CTX_NAME_TIDY %in% c("Forimtamig", "Talquetamab") ~ "GPRC5DxCD3 bispecific",
      CTX_NAME_TIDY == "Cevostamab" ~ "FcRH5xCD5 bispecific",
      TRUE ~ CTX_NAME
    ),
    CTX_GROUP2 = case_when(
      CTX_GROUP == "Combi" ~ "Combi",
      CTX_GROUP %in% c("GPRC5DxCD3 bispecific", "FcRH5xCD5 bispecific") ~ "Non-BCMAxCD3 bispecific",
      cohort == "Old" ~ "SOC",
      TRUE ~ CTX_GROUP
    ),
    CTX_GROUP3 = factor(case_when(
      CTX_GROUP2 %in% c("Combi", "SOC") ~ NA,
      TRUE ~ CTX_GROUP2
    ), levels = c("BCMAxCD3 bispecific", "Non-BCMAxCD3 bispecific")),
    CTX_GROUP4 = factor(case_when(
      str_detect(CTX_GROUP, "BCMA") ~ "B",
      str_detect(CTX_GROUP, "FcRH") ~ "F",
      str_detect(CTX_GROUP, "GPRC") ~ "G",
      TRUE ~ "SOC"
    ), levels = c("B", "F", "G", "SOC")),
    
    AGE1 = cut(AGE, breaks = c(-Inf, 50, 60, 70, Inf), labels = c("<50", "<60", "<70", "70+"), right = F)
  )
cut(data$AGE, breaks = c(-Inf, 50, 60, 70, Inf), right = F)

# idx_high = which(data$`amp(1q21), cf. UN` == 1 | data$`t(4:14), cf. UN` == 1 | data$`del(17p), cf. UN` == 1 | data$`t(14;16), cf. UN` == 1)
# idx_standard = which(data$`amp(1q21), cf. UN` == 0 & data$`t(4:14), cf. UN` == 0 & data$`del(17p), cf. UN` == 0)

# data$risk_group = "Unknown"
# data$risk_group[idx_high] = "High"

idx_high = (data$`del(17p), cf. UN` == 1 | data$`amp(1q21), cf. UN` == 1 | data$`t(4:14), cf. UN` == 1 | data$`t(14;16), cf. UN` == 1)
idx_unknown = is.na(data$`del(17p), cf. UN`) & is.na(data$`amp(1q21), cf. UN`) & is.na(data$`t(4:14), cf. UN`) & is.na(data$`t(14;16), cf. UN`) &
  is.na(data$`del(13q), cf. UN`) & is.na(data$`t(11:14), cf. UN`)
# idx_low = (data$`del(13q), cf. UN` == 1 | data$`t(11:14), cf. UN` == 1) + 0
data$gen_risk = 0
data$gen_risk[which(idx_high)] = "high"
data$gen_risk[which(idx_unknown)] = "unknown"
data$gen_risk[data$gen_risk == 0] = "standard"

data$DATE_DIF = as.numeric(data$DATE_DIF)
data$cohort = factor(data$cohort, labels = c("SOC", "BiTE"))

data$CD138_CTX = as.numeric(data$CD138_CTX)
data$LDH_CTX = as.numeric(data$LDH_CTX)



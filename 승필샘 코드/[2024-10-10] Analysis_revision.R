# -- source -- #
source("7_Dataset.R")
source("7_Function.R")
source("7_adjustedCurve_function.R")

# -- option -- #
set.seed(123)

# -- analysis -- #
data = data[data$CTX_NAME_TIDY != "Car-T Cell Therapy",]

data.ISS0 <- data %>% 
  filter(if_all(c("Hemoglobin <7.5 g/dL", "Absoulte neutrophil count <1.0 x 10^9/L", "Lactate dehydrogenase ≥ ULN"), ~ !is.na(.))) %>%
  select(-all_of(paste0("ISS stage 3_", c("factored", "mice")))) %>%
  rename(`ISS stage 3` = `ISS stage 3_ISS0`)

data.factored <- data %>% 
  filter(if_all(c("Hemoglobin <7.5 g/dL", "Absoulte neutrophil count <1.0 x 10^9/L", "Lactate dehydrogenase ≥ ULN"), ~ !is.na(.))) %>%
  select(-all_of(paste0("ISS stage 3_", c("ISS0", "mice")))) %>%
  rename(`ISS stage 3` = `ISS stage 3_factored`) %>%
  mutate_at(vars(`ISS stage 3`), as.factor)

# Make MICE object
data_imp <- data %>% select(PID, gen_risk, CTX_NAME, CD138_CTX, CTX_GROUP4, CTX_GROUP3, 
                            cohort, DATE_DIF, VS, DAYS_VS_PFS_CTX, VS_PFS_CTX, SEX, `AGE`, `Line of chemotherapy`,
                            `ISS stage 3_mice`, `Hemoglobin <7.5 g/dL`, `Absoulte neutrophil count <1.0 x 10^9/L`,
                            `Absoulte lymphocyte count`, `Platelet count <50 x 10^9/L`, `Creatinine >2.0 mg/dL`,
                            `Light chain type`, `β2-microglobuline >=5.5 µg/mL`, `Lactate dehydrogenase ≥ ULN`, `Albumin >=3.5 mg/dL`, AGE1)

colnames(data_imp)[-(1:11)] <- c("SEX", "AGE", "Line", "ISS3", "Hb", "Neutro", "Lympho", "Platelet", "Creatinine", "Light_chain", "beta2",
                                 "Lactate", "Albumin", "AGE1")

data_imp <- data_imp %>% mutate_at(vars(ISS3, Line, Hb, Neutro, Light_chain, Lactate, beta2, Albumin), as.factor)

imp2 <- suppressWarnings(mice(data_imp, maxit = 0, m = 1, printFlag = F)) 

pred_mat = imp2$predictorMatrix
pred_mat[,c(1:11)] = 0 # Ignore treat and response variable
pred_mat[c(1:11),] = 0 # Ignore treat and response variable

where_imp = is.na(data_imp)
where_imp[,1:11] = F
imp2 <- mice(data_imp, where = where_imp, maxit = 10, m = 10, seed = 2023, printFlag = F, predictorMatrix = pred_mat)

## Matching

# match_form = cohort ~ SEX + AGE1 + 
#   `Line of chemotherapy` * `ISS stage 3` +
#   `ISS stage 3` *  `Absoulte neutrophil count <1.0 x 10^9/L` +
#   `Platelet count <50 x 10^9/L` +
#   `Hemoglobin <7.5 g/dL` + 
#   `Lactate dehydrogenase ≥ ULN` +
#   `Creatinine >2.0 mg/dL`

match_form = cohort ~ SEX + AGE1 + 
  `ISS stage 3` + `Line of chemotherapy` + 
  `Absoulte neutrophil count <1.0 x 10^9/L` +
  `Hemoglobin <7.5 g/dL` + 
  `Platelet count <50 x 10^9/L`


match_form_mice = as.formula(cohort~SEX+AGE1+ISS3+Line+Neutro+Hb+Platelet+
                               Line:Platelet+Line:ISS3+ISS3:Neutro)
# match_form_mice = as.formula(cohort~SEX+AGE1+ISS3+Line+Neutro+Hb_count+Platelet_count)

match_data_ISS0 <- matchit(match_form, data = data.ISS0, method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)
match_data_factored <- matchit(match_form, data = data.factored, method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)
match_data_mice <- suppressMessages(matchthem(match_form_mice, data = imp2, method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25))

matched_data_ISS0 <- match.data(match_data_ISS0)
matched_data_factored <- match.data(match_data_factored)

# match_data_mice <- suppressMessages(matchthem(match_form_mice, data = imp2, method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25))
# bal.tab(match_data_mice$models[[8]], thresholds = c(m = 0.2, v = 2), un =T, binary = "std")
# bal.tab(match_data_mice, thresholds = c(m = 0.2, v = 2), un =T, binary = "std")

# is.na(data_imp)
# naniar::mcar_test(
#   data_imp %>% 
#     select(all.vars(match_form_mice)[-1]) %>%
#     filter(!is.na(Hb), !is.na(Neutro), !is.na(Platelet), !is.na(Creatinine)))


table_match_idx = 8

matched_data_mice1 =
  match.data(match_data_mice$models[[table_match_idx]]) %>%
  left_join(data %>% select("PID", "cohort", "Type of Myeloma","Light chain type",
                            "del(17p), cf. UN", "amp(1q21), cf. UN", "t(4:14), cf. UN", "t(14;16), cf. UN",
                            "del(13q), cf. UN", "t(11:14), cf. UN",
                            "Time to treatment with chemotherapy", 
                            Creatinine_cont = "Creatinine", Hemoglobin_cont = "Hemoglobin",
                            Platelet_cont = "Platelet count", Lactate_cont = "LDH_CTX"
  ), by = c("PID", "cohort"), multiple = "first")


data_m <- matched_data_mice1 %>% select("cohort", "SEX", "AGE", "AGE1","Type of Myeloma","Light chain type", "ISS3",
                                        "Time to treatment with chemotherapy", "Line",
                                        "gen_risk", "del(17p), cf. UN", "amp(1q21), cf. UN", "t(4:14), cf. UN", "t(14;16), cf. UN",
                                        "del(13q), cf. UN", "t(11:14), cf. UN",
                                        "Lactate_cont", "Lactate", "Creatinine_cont", "Hemoglobin_cont", "Hb",
                                        "Neutro", "Platelet_cont", "Platelet", "CD138_CTX")

# data_m %>%
#   tbl_summary(
#     by='cohort',
#     label = list(
#       SEX ~ 'Sex', AGE1 ~ 'Age',
#       'ISS3' ~ 'ISS stage >= 3 at diagnosis',
#       't(4:14), cf. UN' ~ 't(4:14)',
#       'del(17p), cf. UN' ~ 'deletion of 17p',
#       't(14;16), cf. UN' ~ 't(14:16)',
#       't(11:14), cf. UN' ~ 't(11:14)',
#       'del(13q), cf. UN' ~ 'Deletion of 13q',
#       'amp(1q21), cf. UN' ~ 'Amplication of 1q21',
#       'Lactate' ~ 'Lactate dehydrogenase'
#       )) %>%
#   add_p(simulate.p.value = T) %>% add_overall() %>%
#   modify_caption("**Patients' clinical charateristics.** (N = {N})") %>%
#   bold_labels() %>% bold_p() %>% italicize_levels() %>% as_gt() %>%
#   gt::tab_header(title = gt::md("**Matched covariates**")) %>%
#   gt::gtsave("Table2_1.docx")


# data_imp %>%
#   left_join(data %>% select("PID", "cohort", "Type of Myeloma","Light chain type",
#                             "del(17p), cf. UN", "amp(1q21), cf. UN", "t(4:14), cf. UN", "t(14;16), cf. UN",
#                             "del(13q), cf. UN", "t(11:14), cf. UN",
#                             "Time to treatment with chemotherapy",
#                             Creatinine_cont = "Creatinine", Hemoglobin_cont = "Hemoglobin",
#                             Platelet_cont = "Platelet count", Lactate_cont = "LDH_CTX",
#                             "CTX_NAME"
#   ), by = c("PID", "cohort"), multiple = "first") %>%
#   select("cohort", "SEX", "AGE", "AGE1","Type of Myeloma","Light chain type", "ISS3",
#          "Time to treatment with chemotherapy", "Line",
#          "gen_risk", "del(17p), cf. UN", "amp(1q21), cf. UN", "t(4:14), cf. UN", "t(14;16), cf. UN",
#          "del(13q), cf. UN", "t(11:14), cf. UN",
#          "Lactate_cont", "Lactate", "Creatinine_cont", "Hemoglobin_cont", "Hb",
#          "Neutro", "Platelet_cont", "Platelet", "CD138_CTX") %>%
#   tbl_summary(
#     by='cohort',
#     label = list(
#       SEX ~ 'Sex', AGE1 ~ 'Age',
#       'ISS3' ~ 'ISS stage >= 3 at diagnosis',
#       't(4:14), cf. UN' ~ 't(4:14)',
#       'del(17p), cf. UN' ~ 'deletion of 17p',
#       't(14;16), cf. UN' ~ 't(14:16)',
#       't(11:14), cf. UN' ~ 't(11:14)',
#       'del(13q), cf. UN' ~ 'Deletion of 13q',
#       'amp(1q21), cf. UN' ~ 'Amplication of 1q21',
#       'Lactate' ~ 'Lactate dehydrogenase'
#     )) %>%
#   add_p(simulate.p.value = T) %>% add_overall() %>%
#   modify_caption("**Patients' clinical charateristics.** (N = {N})") %>%
#   bold_labels() %>% bold_p() %>% italicize_levels() %>% as_gt() %>%
#   gt::tab_header(title = gt::md("**Matched covariates**")) %>%
#   gt::gtsave("Table S3.docx")


# survfit(Surv(DATE_DIF, VS == 0) ~ cohort, data = data) %>% print(digit = 4)
# survfit(Surv(DATE_DIF, VS == 0) ~ 1, data = data) %>% print(digit = 4)

# survfit(Surv(DATE_DIF, VS == 0) ~ cohort, data = matched_data_mice1) %>% print(digit = 4)
# survfit(Surv(DATE_DIF, VS == 0) ~ 1, data = matched_data_mice1) %>% print(digit = 4)


# summary(match_data_mice$models[[4]])

# kable(bal.tab(match_data_completed, thresholds = c(m = 0.2, v = 2))$Balance[,c(4:7)])
# kable(bal.tab(match_data_ISS0, thresholds = c(m = 0.2, v = 2), un =T)$Balance[,c(4:7)])
# kable(bal.tab(match_data_factored, thresholds = c(m = 0.2, v = 2), un =T)$Balance[,c(4:7)])
# kable(bal.tab(match_data_mice, thresholds = c(m = 0.2, v = 2), un =T, imp.fun = 'max')$Balance.Across.Imputations[,c(4:7)])
# bal.tab(match_data_mice$models[[4]], thresholds = c(m = 0.2, v = 2), un =T)

# bal.tab(match_data_mice$models[[7]], thresholds = c(m = 0.2, v = 2), un =T, binary = "std")


# -- analysis -- #
psm_ISS01 = coxph(Surv(DATE_DIF, VS) ~ cohort, data = matched_data_ISS0, cluster = subclass)
psm_ISS02 = coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, data = matched_data_ISS0, cluster = subclass)

psm_factored1 = coxph(Surv(DATE_DIF, VS) ~ cohort, data = matched_data_factored, cluster = subclass)
psm_factored2 = coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, data = matched_data_factored, cluster = subclass)

psm_mice1 = with(match_data_mice, coxph(Surv(DATE_DIF, VS) ~ cohort, cluster = subclass)) %>% pool
psm_mice2 = with(match_data_mice, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, cluster = subclass)) %>% pool

mat_HR_res_table = data.frame(
  HR_VS = c(get_est_CI(psm_mice1, type = "exp", r = 2),
            get_est_CI(psm_ISS01, r = 2),
            get_est_CI(psm_factored1, r = 2)),
  
  HR_PFS_CTX = c(get_est_CI(psm_mice2, type = "exp", r = 2),
                 get_est_CI(psm_ISS02, r = 2),
                 get_est_CI(psm_factored2, r = 2))
)

mat_HR_res = rbind(
  get_est_CI(psm_mice1, paper = F, type = "exp"),
  get_est_CI(psm_ISS01, paper = F),
  get_est_CI(psm_factored1, paper = F),
  
  get_est_CI(psm_mice2, paper = F, type = "exp"),
  get_est_CI(psm_ISS02, paper = F),
  get_est_CI(psm_factored2, paper = F)
) %>% as.data.frame %>%
  mutate(model = rep(c("MICE", "Mode", "Missing category"), 2),
         response = rep(c("OS", "PFS"), each = 3),
         summary = c(mat_HR_res_table[,1], mat_HR_res_table[,2]))


mat_forest = 
  mat_HR_res %>%
  forestplot(labeltext = c(response, model, summary),
             mean = Estimate, lower = Lower, upper = Upper,
             xlab = "Hazard Ratio (HR)",
             xlog = T, title = "Matching",
             xticks = log(c(0.35, 0.5, 0.7, 1, 1.4, 2)),
             arrow_lab = c("a", "b")) %>%
  fp_add_lines(h_2 = gpar(lty = "solid"), 
               h_5 = gpar(lty = "dashed"), 
               h_8 = gpar(lty = "solid")) %>%
  fp_set_style(box = pal_lancet()(1),
               line = pal_lancet()(1),
               txt_gp = fpTxtGp(label = gpar(cex = 0.7),
                                ticks = gpar(cex = 0.7),
                                xlab = gpar(cex = 0.7))) %>%
  fp_add_header(response = c("Outcome"),
                model = c("Imputation"),
                summary = c("HR (95% CI)")) %>%
  fp_set_zebra_style("#EFEFEF") %>%
  print %>% grid2grob %>% wrap_elements

mat_forest
grid.text("favor to BiTE", x=unit(0.65, "npc"), y=unit(0.025, "npc"), just="center")
grid.text("favor to SOC", x=unit(0.80, "npc"), y=unit(0.025, "npc"), just="center")
grid.lines(x=unit(c(0.55, 0.70), "npc"), y=unit(0.04, "npc"),
           arrow=arrow(angle=30, length=unit(0.1,"inches"), ends="first"))
grid.lines(x=unit(c(0.90, 0.75), "npc"), y=unit(0.04, "npc"),
           arrow=arrow(angle=30, length=unit(0.1,"inches"), ends="first"))

# -- IPW analysis -- #
sw_ISS0 <- my_ipw_weights(match_form, data = data.ISS0, stabilize = F, trim = T)
sw_factored <- my_ipw_weights(match_form, data = data.factored, stabilize = F, trim = T)

des_ISS0 = svydesign(ids = ~1, weights = ~sw_ISS0, data = data.ISS0)
des_factored = svydesign(ids = ~1, weights = ~sw_factored, data = data.factored)

ipw_ISS01 = svycoxph(Surv(DATE_DIF, VS) ~ cohort, design = des_ISS0)
ipw_ISS02 = svycoxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, design = des_ISS0)

ipw_factored1 = svycoxph(Surv(DATE_DIF, VS) ~ cohort, design = des_factored)
ipw_factored2 = svycoxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, design = des_factored)

ipw_mice1 <- my_ipw_pool(match_form_mice, Surv(DATE_DIF, VS) ~ cohort, imp_set = imp2)
ipw_mice2 <- my_ipw_pool(match_form_mice, Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, imp_set = imp2)

ipw_HR_res_table = data.frame(
  HR_VS = c(get_est_CI(ipw_mice1, type = "exp", r = 2),
            get_est_CI(ipw_ISS01, r = 2),
            get_est_CI(ipw_factored1, r = 2)),
  
  HR_PFS_CTX = c(get_est_CI(ipw_mice2, type = "exp", r = 2),
                 get_est_CI(ipw_ISS02, r = 2),
                 get_est_CI(ipw_factored2, r = 2))
)

ipw_HR_res = rbind(
  get_est_CI(ipw_mice1, paper = F, type = "exp"),
  get_est_CI(ipw_ISS01, paper = F),
  get_est_CI(ipw_factored1, paper = F),
  
  get_est_CI(ipw_mice2, paper = F, type = "exp"),
  get_est_CI(ipw_ISS02, paper = F),
  get_est_CI(ipw_factored2, paper = F)
) %>% as.data.frame %>%
  mutate(model = rep(c("MICE", "Mode", "Missing category"), 2),
         response = rep(c("OS", "PFS"), each = 3),
         summary = c(ipw_HR_res_table[,1], ipw_HR_res_table[,2]))


ipw_forest = 
  ipw_HR_res %>%
  forestplot(labeltext = c(response, model, summary),
             mean = Estimate, lower = Lower, upper = Upper,
             xlab = "Hazard Ratio (HR)",
             xlog = T, title = "IPW",
             xticks = log(c(0.35, 0.5, 0.7, 1, 1.4, 2))) %>%
  fp_add_lines(h_2 = gpar(lty = "solid"), 
               h_5 = gpar(lty = "dashed"), 
               h_8 = gpar(lty = "solid")) %>%
  fp_set_style(box = pal_lancet()(1),
               line = pal_lancet()(1),
               txt_gp = fpTxtGp(label = gpar(cex = 0.7),
                                ticks = gpar(cex = 0.7),
                                xlab = gpar(cex = 0.7))) %>%
  fp_add_header(response = c("Outcome"),
                model = c("Imputation"),
                summary = c("HR (95% CI)")) %>%
  fp_set_zebra_style("#EFEFEF") %>%
  print %>% grid2grob %>% wrap_elements


# PS balancing plot
res4 = 
  bal.plot(match_data_mice$models[[table_match_idx]],  
           var.name = "distance", 
           which = "both", 
           # type = "density",  
           type = "histogram",
           mirror = TRUE,
           treat = c("00", "11"),
           # bins = 12,
           sample.names = c("Unadjusted Sample", "Matched"),
           legend.labs = c("00", "11")) +
  scale_fill_manual(values = c("0" = pal_lancet()(2)[1], "1" = pal_lancet()(2)[2]),
                    labels = c("0" = "SOC", "1" = "BiTE"),
                    name = "Treatment") +
  labs(x = "Propensity score", title = "") ; res4


# ggsave("Figure S1.png", res4, device = "png", height = 4, width = 7, dpi = 3000)


# Survival curve
matched_BiTE_ID = matched_data_mice1 %>% filter(cohort == "BiTE") %>% .$PID

imp_dat1 = matched_data_mice1 %>% mutate(cohort = factor(cohort, levels = c("BiTE", "SOC")))
imp_dat2 = 
  complete(imp2, table_match_idx) %>% 
  filter((CTX_NAME %in% c("Erlanatamab", "Linvoseltamab", "Regeneron", "Teclistamab") & PID %in% matched_BiTE_ID) | 
           cohort == "SOC") %>% 
  matchit(match_form_mice, data = ., method = "nearest", distance ="glm", ratio = 1, replace = FALSE) %>%
  match.data() %>%
  mutate(cohort = factor(cohort, levels = c("BiTE", "SOC")))


imp_dat3 = 
  complete(imp2, table_match_idx) %>% 
  filter((CTX_NAME %in% c("Cevostamab", "Forimtamig", "Talquetamab") & PID %in% matched_BiTE_ID )| 
           cohort == "SOC") %>% 
  matchit(match_form_mice, data = ., method = "nearest", distance ="glm", ratio = 1, replace = FALSE) %>%
  match.data() %>%
  mutate(cohort = factor(cohort, levels = c("BiTE", "SOC")))

imp_dat_mice2 = 
  imp2 %>% 
  filter((CTX_NAME %in% c("Erlanatamab", "Linvoseltamab", "Regeneron", "Teclistamab") & PID %in% matched_BiTE_ID) | 
           cohort == "SOC") %>% 
  matchthem(match_form_mice, data = ., method = "nearest", distance ="glm", ratio = 1, replace = FALSE)

imp_dat_mice3 = 
  imp2 %>% 
  filter((CTX_NAME %in% c("Cevostamab", "Forimtamig", "Talquetamab") & PID %in% matched_BiTE_ID )| 
           cohort == "SOC") %>% 
  matchthem(match_form_mice, data = ., method = "nearest", distance ="glm", ratio = 1, replace = FALSE)

OS_survfit1 = survfit(Surv(DATE_DIF, VS) ~ cohort, data = imp_dat1) 
OS_survfit2 = survfit(Surv(DATE_DIF, VS) ~ cohort, data = imp_dat2)
OS_survfit3 = survfit(Surv(DATE_DIF, VS) ~ cohort, data = imp_dat3)

PFS_survfit1 = survfit(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, data = imp_dat1)
PFS_survfit2 = survfit(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, data = imp_dat2)
PFS_survfit3 = survfit(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, data = imp_dat3)

survfit(Surv(DATE_DIF, VS == 0) ~ cohort, data = imp_dat1) 
survfit(Surv(DATE_DIF, VS == 0) ~ cohort, data = imp_dat2)
survfit(Surv(DATE_DIF, VS == 0) ~ cohort, data = imp_dat3)


OS_coxph1 = psm_mice1
OS_coxph2 = with(imp_dat_mice2, coxph(Surv(DATE_DIF, VS) ~ cohort, cluster = subclass)) %>% pool
OS_coxph3 = with(imp_dat_mice3, coxph(Surv(DATE_DIF, VS) ~ cohort, cluster = subclass)) %>% pool

PFS_coxph1 = psm_mice2
PFS_coxph2 = with(imp_dat_mice2, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, cluster = subclass)) %>% pool
PFS_coxph3 = with(imp_dat_mice3, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, cluster = subclass)) %>% pool


OS_survplot1 = OS_survfit1 %>% my_survplot(text_vec = c(16, 0.8, round(c(exp(summary(OS_coxph1)[[2]]), summary(OS_coxph1)[[6]]), 2)), 
                                           legend.labs = c("BiTE", "SOC"), legend = c(0.2, 0.2), title = "(A)")

OS_survplot2 = OS_survfit2 %>% my_survplot(text_vec = c(18, 0.7, round(c(exp(summary(OS_coxph2)[[2]]), summary(OS_coxph2)[[6]]), 2)), 
                                           legend.labs = c("BCMA-targeted BiTEs", "SOC"), legend = c(0.3, 0.2),
                                           risk.table.label = c("SOC", "BCMA"), title = "(C)", vjust = -3)

OS_survplot3 = OS_survfit3 %>% my_survplot(text_vec = c(16, 0.5, round(c(exp(summary(OS_coxph3)[[2]]), summary(OS_coxph3)[[6]]), 2)), 
                                           legend.labs = c("Non-BCMA-targeted BiTEs", "SOC"), legend = c(0.35, 0.2),
                                           risk.table.label = c("SOC", "Non-BCMA"), title = "(E)", vjust = -11)

PFS_survplot1 = PFS_survfit1 %>% my_survplot(text_vec = c(16, 0.7, round(c(exp(summary(PFS_coxph1)[[2]]), summary(PFS_coxph1)[[6]]), 2)), 
                                             legend.labs = c("BiTE", "SOC"), legend = c(0.2, 0.2), title = "(B)")

PFS_survplot2 = PFS_survfit2 %>% my_survplot(text_vec = c(18, 0.25, round(c(exp(summary(PFS_coxph2)[[2]]), summary(PFS_coxph2)[[6]]), 2)), 
                                             legend.labs = c("BCMA-targeted BiTEs", "SOC"), legend = c(0.5, 0.8),
                                             risk.table.label = c("SOC", "BCMA"), title = "(D)", vjust = -3)

PFS_survplot3 = PFS_survfit3 %>% my_survplot(text_vec = c(6, 0.1, round(c(exp(summary(PFS_coxph3)[[2]]), summary(PFS_coxph3)[[6]]), 2)), 
                                             legend.labs = c("Non-BCMA-targeted BiTEs", "SOC"), legend = c(0.73, 0.8),
                                             risk.table.label = c("SOC", "Non-BCMA"), title = "(F)", vjust = -11)

ggsave("Figure 2A.png", OS_survplot1$plot / OS_survplot1$table + plot_layout(heights = c(0.83, 0.17)), device = "png", width = 5, height = 5, dpi = 600)
ggsave("Figure 2B.png", PFS_survplot1$plot / PFS_survplot1$table + plot_layout(heights = c(0.83, 0.17)), device = "png", width = 5, height = 5, dpi = 600)
ggsave("Figure 2C.png", OS_survplot2$plot / OS_survplot2$table + plot_layout(heights = c(0.83, 0.17)) +
         plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -7), "pt"))), device = "png", width = 5, height = 5, dpi = 600)
ggsave("Figure 2D.png", PFS_survplot2$plot / PFS_survplot2$table + plot_layout(heights = c(0.83, 0.17)) +
         plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -7), "pt"))), device = "png", width = 5, height = 5, dpi = 600)
ggsave("Figure 2E.png", OS_survplot3$plot / OS_survplot3$table + plot_layout(heights = c(0.83, 0.17)) +
         plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))), device = "png", width = 5, height = 5, dpi = 600)
ggsave("Figure 2F.png", PFS_survplot3$plot / PFS_survplot3$table + plot_layout(heights = c(0.83, 0.17)) +
         plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))), device = "png", width = 5, height = 5, dpi = 600)

Figure_2_tot = 
  ggarrange(
    ggarrange(OS_survplot1$plot / OS_survplot1$table + plot_layout(heights = c(0.83, 0.17)), 
              PFS_survplot1$plot / PFS_survplot1$table + plot_layout(heights = c(0.83, 0.17))),
    ggarrange(OS_survplot2$plot / OS_survplot2$table + plot_layout(heights = c(0.83, 0.17)) +
                plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -7), "pt"))),
              PFS_survplot2$plot / PFS_survplot2$table + plot_layout(heights = c(0.83, 0.17)) +
                plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -7), "pt")))),
    ggarrange(OS_survplot3$plot / OS_survplot3$table + plot_layout(heights = c(0.83, 0.17)) +
                plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))),
              PFS_survplot3$plot / PFS_survplot3$table + plot_layout(heights = c(0.83, 0.17)) +
                plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt")))),
    nrow = 3
  )

ggsave("Figure 2.png", device = "png", width = 10, height = 15, dpi = 600)

# OS_survplot = adsurv(imp2, variable = "cohort", ev_time = "DATE_DIF", event = "VS", method = "matching", 
#                      treatment_model = match_form_mice,
#                      matching_method = "nearest", bootstrap = T, n_boot = 1000)

# PFS_survplot = adsurv(imp2, variable = "cohort", ev_time = "DAYS_VS_PFS_CTX", event = "VS_PFS_CTX", method = "matching", 
#                      treatment_model = match_form_mice,
#                      matching_method = "nearest", bootstrap = T, n_boot = 1000)

survminer::surv_summary(OS_survfit1) %>% filter(time >= 10, time <= 12) %>% arrange(strata, -time)
survminer::surv_summary(OS_survfit1) %>% filter(time >= 22, time <= 24) %>% arrange(strata, -time)

logrank_time(Surv(DATE_DIF, VS) ~ cohort, imp_dat1, max_time = 12)
logrank_time(Surv(DATE_DIF, VS) ~ cohort, imp_dat1, max_time = 24)

survminer::surv_summary(PFS_survfit1) %>% filter(time >= 10, time <= 12) %>% arrange(strata, -time)
survminer::surv_summary(PFS_survfit1) %>% filter(time >= 22, time <= 24) %>% arrange(strata, -time)

logrank_time(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, imp_dat1, max_time = 12)
logrank_time(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, imp_dat1, max_time = 24)


mat_HR_res

# OS_survplot$adj %>% filter(time <= 24) %>% group_by(group) %>% summarize(rate = min(surv))
# PFS_survplot$adj %>% filter(time <= 12) %>% group_by(group) %>% summarize(rate = min(surv))
# PFS_survplot$adj %>% filter(time <= 24) %>% group_by(group) %>% summarize(rate = min(surv))

# PFS_survplot$adj %>% filter(surv >= 0.45, surv <= 0.55)
# PFS_survplot$mids_analyses[[8]]$survfit_object

# logrank_mice(Surv(DATE_DIF/30.42, VS) ~ cohort, match_data_mice, max_time = 12)
# logrank_mice(Surv(DATE_DIF/30.42, VS) ~ cohort, match_data_mice, max_time = 24)

survfit(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort, matched_data_mice1)

# logrank_mice(Surv(DAYS_VS_PFS_CTX/30.42, VS_PFS_CTX) ~ cohort, match_data_mice, max_time = 12)
# logrank_mice(Surv(DAYS_VS_PFS_CTX/30.42, VS_PFS_CTX) ~ cohort, match_data_mice, max_time = 24)



# Forest plot
res1 = mat_forest / ipw_forest ; res1
res_new = res1 + plot_annotation(tag_levels = 'A', tag_prefix = "(", tag_suffix = ")") & theme(plot.tag = element_text(size = 10))

ggsave("Figure 3.png", res_new, device = "png", width = 6, height = 5, dpi = 2000)


c(exp(summary(psm_mice1)[2]), summary(psm_mice1)[6]) # 0.511
c(exp(summary(psm_mice2)[2]), summary(psm_mice2)[6]) # 0.511
summary(psm_mice2) # 0.511

# subgroup analysis
match_sub_mice1 <- matchthem(del_var(match_form_mice, 2), data = filter(imp2, AGE1 == "<60"), method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)
match_sub_mice2 <- matchthem(del_var(match_form_mice, 2), data = filter(imp2, AGE1 %in% c("<70", "70+")), method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)
# match_sub_mice3 <- matchthem(del_var(match_form_mice, 2), data = filter(imp2, AGE1 == "70+"), method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)
match_sub_mice3 <- matchthem(del_var(match_form_mice, 1), data = filter(imp2, SEX == "F"), method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)
match_sub_mice4 <- matchthem(del_var(match_form_mice, 1), data = filter(imp2, SEX == "M"), method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)
match_sub_mice5 <- matchthem(match_form_mice, data = filter(imp2, high_risk == 1), method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)
match_sub_mice6 <- matchthem(match_form_mice, data = filter(imp2, low_risk == 1), method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)


psm_sub_mice1_1 = with(match_sub_mice1, coxph(Surv(DATE_DIF, VS) ~ cohort + strata(subclass))) %>% pool
psm_sub_mice2_1 = with(match_sub_mice2, coxph(Surv(DATE_DIF, VS) ~ cohort + strata(subclass))) %>% pool
psm_sub_mice3_1 = with(match_sub_mice3, coxph(Surv(DATE_DIF, VS) ~ cohort + strata(subclass))) %>% pool
psm_sub_mice4_1 = with(match_sub_mice4, coxph(Surv(DATE_DIF, VS) ~ cohort + strata(subclass))) %>% pool
psm_sub_mice5_1 = with(match_sub_mice5, coxph(Surv(DATE_DIF, VS) ~ cohort + strata(subclass))) %>% pool
psm_sub_mice6_1 = with(match_sub_mice6, coxph(Surv(DATE_DIF, VS) ~ cohort + strata(subclass))) %>% pool
# psm_sub_mice7_1 = with(match_sub_mice7, coxph(Surv(DATE_DIF, VS) ~ cohort + strata(subclass))) %>% pool
psm_sub_miceB_1 = with(match_data_mice, coxph(Surv(DATE_DIF, VS) ~ cohort + strata(subclass), subset = CTX_GROUP4 %in% c("B", "SOC"))) %>% pool
psm_sub_miceF_1 = with(match_data_mice, coxph(Surv(DATE_DIF, VS) ~ cohort + strata(subclass), subset = CTX_GROUP4 %in% c("F", "SOC"))) %>% pool
psm_sub_miceG_1 = with(match_data_mice, coxph(Surv(DATE_DIF, VS) ~ cohort + strata(subclass), subset = CTX_GROUP4 %in% c("G", "SOC"))) %>% pool

psm_sub_mice1_2 = with(match_sub_mice1, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort + strata(subclass))) %>% pool
psm_sub_mice2_2 = with(match_sub_mice2, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort + strata(subclass))) %>% pool
psm_sub_mice3_2 = with(match_sub_mice3, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort + strata(subclass))) %>% pool
psm_sub_mice4_2 = with(match_sub_mice4, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort + strata(subclass))) %>% pool
psm_sub_mice5_2 = with(match_sub_mice5, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort + strata(subclass))) %>% pool
psm_sub_mice6_2 = with(match_sub_mice6, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort + strata(subclass))) %>% pool
# psm_sub_mice7_2 = with(match_sub_mice7, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort + strata(subclass))) %>% pool
psm_sub_miceB_2 = with(match_data_mice, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort + strata(subclass), subset = CTX_GROUP4 %in% c("B", "SOC"))) %>% pool
psm_sub_miceF_2 = with(match_data_mice, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort + strata(subclass), subset = CTX_GROUP4 %in% c("F", "SOC"))) %>% pool
psm_sub_miceG_2 = with(match_data_mice, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohort + strata(subclass), subset = CTX_GROUP4 %in% c("G", "SOC"))) %>% pool



sub_name = c("Age: <60", "Age: ≥60", "Sex: female", "Sex: male", "Cytogenetic risk: High", "Cytogenetic risk: Standard", 
             "BCMA-targeted BiTEs", "FcRH5-targeted BiTEs", "GPRC5d-targeted BiTEs")
sub_res_table = 
  cbind.data.frame(
    OS = c(get_est_CI(psm_sub_mice1_1, type = "exp", r = 2),
           get_est_CI(psm_sub_mice2_1, type = "exp", r = 2),
           get_est_CI(psm_sub_mice3_1, type = "exp", r = 2),
           get_est_CI(psm_sub_mice4_1, type = "exp", r = 2),
           get_est_CI(psm_sub_mice5_1, type = "exp", r = 2),
           get_est_CI(psm_sub_mice6_1, type = "exp", r = 2),
           # get_est_CI(psm_sub_mice7_1, type = "exp", r = 2),
           get_est_CI(psm_sub_miceB_1, type = "exp", r = 2),
           get_est_CI(psm_sub_miceF_1, type = "exp", r = 2),
           get_est_CI(psm_sub_miceG_1, type = "exp", r = 2)),
    
    PFS = c(get_est_CI(psm_sub_mice1_2, type = "exp", r = 2),
            get_est_CI(psm_sub_mice2_2, type = "exp", r = 2),
            get_est_CI(psm_sub_mice3_2, type = "exp", r = 2),
            get_est_CI(psm_sub_mice4_2, type = "exp", r = 2),
            get_est_CI(psm_sub_mice5_2, type = "exp", r = 2),
            get_est_CI(psm_sub_mice6_2, type = "exp", r = 2),
            # get_est_CI(psm_sub_mice7_2, type = "exp", r = 2),
            get_est_CI(psm_sub_miceB_2, type = "exp", r = 2),
            get_est_CI(psm_sub_miceF_2, type = "exp", r = 2),
            get_est_CI(psm_sub_miceG_2, type = "exp", r = 2))
  )

sub_res = 
  rbind(get_est_CI(psm_sub_mice1_1, type = "exp", paper = F),
        get_est_CI(psm_sub_mice2_1, type = "exp", paper = F),
        get_est_CI(psm_sub_mice3_1, type = "exp", paper = F),
        get_est_CI(psm_sub_mice4_1, type = "exp", paper = F),
        get_est_CI(psm_sub_mice5_1, type = "exp", paper = F),
        get_est_CI(psm_sub_mice6_1, type = "exp", paper = F),
        # get_est_CI(psm_sub_mice7_1, type = "exp", paper = F),
        get_est_CI(psm_sub_miceB_1, type = "exp", paper = F),
        get_est_CI(psm_sub_miceF_1, type = "exp", paper = F),
        get_est_CI(psm_sub_miceG_1, type = "exp", paper = F),
        
        get_est_CI(psm_sub_mice1_2, type = "exp", paper = F),
        get_est_CI(psm_sub_mice2_2, type = "exp", paper = F),
        get_est_CI(psm_sub_mice3_2, type = "exp", paper = F),
        get_est_CI(psm_sub_mice4_2, type = "exp", paper = F),
        get_est_CI(psm_sub_mice5_2, type = "exp", paper = F),
        get_est_CI(psm_sub_mice6_2, type = "exp", paper = F),
        # get_est_CI(psm_sub_mice7_2, type = "exp", paper = F),
        get_est_CI(psm_sub_miceB_2, type = "exp", paper = F),
        get_est_CI(psm_sub_miceF_2, type = "exp", paper = F),
        get_est_CI(psm_sub_miceG_2, type = "exp", paper = F)) %>%
  as.data.frame %>%
  mutate(model = rep(sub_name, 2),
         response = rep(c('OS', 'PFS'), each = 9),
         summary = c(sub_res_table[,1], sub_res_table[,2]))


sub_forest = 
  sub_res %>%
  forestplot(labeltext = c(response, model, summary),
             mean = Estimate, lower = Lower, upper = Upper,
             xlab = "Hazard Ratio (HR)",
             xlog = T, title = "Subgroup analysis") %>%
  fp_add_lines(h_2 = gpar(lty = "solid"), 
               h_11 = gpar(lty = "dashed"), 
               h_20 = gpar(lty = "solid")) %>%
  fp_set_style(box = pal_lancet()(1),
               line = pal_lancet()(1),
               txt_gp = fpTxtGp(label = gpar(cex = 0.7),
                                ticks = gpar(cex = 0.7),
                                xlab = gpar(cex = 0.7))) %>%
  fp_add_header(response = c("Outcome"),
                model = c("Subgroup"),
                summary = c("HR (95% CI)")) %>%
  fp_set_zebra_style("#EFEFEF") %>%
  print %>% grid2grob %>% wrap_elements

ggsave("Figure S2.png", sub_forest, device = "png", width = 6, height = 5, dpi = 600)


match_sub_mice7 <- matchthem(match_form_mice, data = filter(imp2, CTX_GROUP4 %in% c("B", "SOC")), method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)
match_sub_mice8 <- matchthem(match_form_mice, data = filter(imp2, CTX_GROUP4 %in% c("F", "SOC")), method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)
match_sub_mice9 <- matchthem(match_form_mice, data = filter(imp2, CTX_GROUP4 %in% c("G", "SOC")), method = "nearest", distance ="glm", ratio = 1, replace = FALSE, caliper = 0.25)

dat_set = paste0("match_sub_mice", 1:9)
p_sub_list = list()
for(i in 1:9){
  # if(i <= 7){
  dd = get(dat_set[i])$models[[table_match_idx]]
  # } else{
  # Cdd = match_data_mice$models[[7]]
  # }c
  
  p_sub_list[[i]] = 
    bal.plot(dd,  
             var.name = "distance", 
             which = "adjusted", 
             type = "histogram",
             mirror = TRUE,
             treat = c("00", "11"),
             sample.names = sub_name[i],
             legend.labs = c("00", "11")) +
    scale_fill_manual(values = c("0" = pal_lancet()(2)[1], "1" = pal_lancet()(2)[2]),
                      labels = c("0" = "SOC", "1" = "BiTE"),
                      name = "Treatment") +
    labs(x = "Propensity score", title = "")
  
  
}

sub_plot_bal = ggarrange(plotlist = p_sub_list, nrow = 5, ncol = 2, common.legend = T)
ggsave(filename = "Figure S3.png", sub_plot_bal, device = "png", width = 9, height = 12, dpi = 600)


## E-values (Factored)
toRR(HR(mat_HR_res[5,1], rare = F))
evalues.HR(HR(mat_HR_res[5,1], rare = F), mat_HR_res[5,2], mat_HR_res[5,3])[2,]
# evalues.HR(HR(ipw_HR_res[5,1], rare = F), ipw_HR_res[5,2], ipw_HR_res[5,3])[2,]


new <- read_excel("data/서울대merge_TCE_vs_SOC_MM_AE_240402.xlsx", sheet = 2, skip = 8) %>% mutate(cohort = "T-cell engager")
old <- read_excel("data/서울대merge_TCE_vs_SOC_MM_AE_240402.xlsx", sheet = 3, skip = 4) %>% mutate(cohort = "SOC")

# select(cohort, CRS, Neurotoxicity, AE_INFECTION_GR, AE_ANEMIA_GR, AE_NEUTROPENIA_GR, AE_TROMBOCYTOPENIA_GR,
#          AE_NEUROPATHY_LOAD_GR, AE_SECONDARY_MALIGNANCY_GR, AE_BILIRUBINEMIA_GR, AE_LIVER_ENZYME_ELEVATION_GR, AE_CREATININE_ELEVATION_GR,
#          AE_PSYCHIATRIC_DYSFUNCTION_GR, AE_LYMPHOPENIA, AE_NEUTROPENIA_GR_3, AE_TROMBOCYTOPENIA_GR_3, AE_LYMPHOPENIA_GR_3) %>%

new1 =
  new %>%
  select(PID, AE_ANEMIA_GR, AE_TROMBOCYTOPENIA_GR, AE_LYMPHOPENIA, AE_NEUTROPENIA_GR,
         AE_INFECTION_GR, CTX_NAME, cohort, CRS, Neurotoxicity = `Neurologic toxicity`,
         AE_NEUROPATHY_LOAD_GR, AE_SECONDARY_MALIGNANCY_GR, AE_BILIRUBINEMIA_GR, AE_LIVER_ENZYME_ELEVATION_GR, AE_CREATININE_ELEVATION_GR,
         AE_PSYCHIATRIC_DYSFUNCTION_GR, AE_LYMPHOPENIA)

old1 =
  old %>%
  select(PID, AE_ANEMIA_GR, AE_TROMBOCYTOPENIA_GR, AE_LYMPHOPENIA, AE_NEUTROPENIA_GR,
         AE_INFECTION_GR, CTX_NAME, cohort, AE_NEUROPATHY_LOAD_GR,
         AE_SECONDARY_MALIGNANCY_GR, AE_BILIRUBINEMIA_GR, AE_LIVER_ENZYME_ELEVATION_GR, AE_CREATININE_ELEVATION_GR,
         AE_PSYCHIATRIC_DYSFUNCTION_GR, AE_LYMPHOPENIA) %>%
  mutate(AE_ANEMIA_GR = as.numeric(AE_ANEMIA_GR),
         AE_TROMBOCYTOPENIA_GR = as.numeric(AE_TROMBOCYTOPENIA_GR),
         AE_LYMPHOPENIA = as.numeric(AE_LYMPHOPENIA),
         AE_NEUTROPENIA_GR = as.numeric(AE_NEUTROPENIA_GR),
         AE_INFECTION_GR = as.numeric(AE_INFECTION_GR),
         AE_BILIRUBINEMIA_GR = as.numeric(AE_BILIRUBINEMIA_GR),
         AE_LIVER_ENZYME_ELEVATION_GR = as.numeric(AE_LIVER_ENZYME_ELEVATION_GR),
         AE_CREATININE_ELEVATION_GR = as.numeric(AE_CREATININE_ELEVATION_GR),
         CRS = NA, Neurotoxicity = NA) %>%
  suppressWarnings()


safe_dat =
  new1 %>%
  bind_rows(old1) %>%
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
    )
  )

matched_data_mice1 =
  match.data(match_data_mice$models[[table_match_idx]]) %>%
  mutate(cohort = ifelse(cohort == "BiTE", "T-cell engager", "SOC")) %>%
  left_join(safe_dat, by = c("PID", "cohort"), multiple = "first")

data.safety.new1 <-
  matched_data_mice1 %>%
  mutate(
    CRS = ifelse(is.na(CRS), "No event", CRS),
    Neurotoxicity = ifelse(is.na(Neurotoxicity), "No event", Neurotoxicity),
    AE_ANEMIA_GR_3 = ifelse(AE_ANEMIA_GR %in% c("3", "4"), ">= Grade 3", "<= Grade 2"),
    AE_NEUTROPENIA_GR = ifelse(is.na(AE_NEUTROPENIA_GR), "2", AE_NEUTROPENIA_GR),
    AE_NEUTROPENIA_GR_3 = ifelse(AE_NEUTROPENIA_GR %in% c("3", "4"), ">= Grade 3", "<= Grade 2"),
    AE_TROMBOCYTOPENIA_GR_3 = case_when(
      AE_TROMBOCYTOPENIA_GR %in% c("3", "4") ~ ">= Grade 3",
      AE_TROMBOCYTOPENIA_GR %in% c("0", "1", "2") ~ "<= Grade 2",
      TRUE ~ NA_character_),
    AE_LYMPHOPENIA_GR_3 = case_when(
      AE_LYMPHOPENIA %in% c("3", "4") ~ ">= Grade 3",
      AE_LYMPHOPENIA %in% c("0", "1", "2") ~ "<= Grade 2",
      TRUE ~ NA_character_),
    AE_INFECTION_GR = factor(AE_INFECTION_GR),
    AE_NEUROPATHY_LOAD_GR = factor(AE_NEUROPATHY_LOAD_GR),
    AE_LIVER_ENZYME_ELEVATION_GR = factor(AE_LIVER_ENZYME_ELEVATION_GR),
    AE_CREATININE_ELEVATION_GR = factor(AE_CREATININE_ELEVATION_GR),
    AE_BILIRUBINEMIA_GR = factor(AE_BILIRUBINEMIA_GR)
  )

# filter(cohort == "T-cell engager" & CTX_GROUP != "Combi") %>%

# data.safety.new1 %>%
#   select(cohort, CRS, Neurotoxicity, AE_ANEMIA_GR_3, AE_NEUTROPENIA_GR_3, AE_TROMBOCYTOPENIA_GR_3, AE_LYMPHOPENIA_GR_3,
#          AE_INFECTION_GR, AE_NEUROPATHY_LOAD_GR, AE_BILIRUBINEMIA_GR, , AE_SECONDARY_MALIGNANCY_GR, AE_PSYCHIATRIC_DYSFUNCTION_GR) %>%
#   tbl_summary(
#     by = cohort
#   ) %>%
#   add_p() %>%
#   add_overall() %>%
#   bold_labels() %>% bold_p() %>% italicize_levels() %>% as_gt() %>%
#   gt::gtsave("Table_safety1.docx")

# data.safety.new1 %>%
#   filter(cohort == "T-cell engager" & CTX_GROUP != "Combi") %>%
#   select(CTX_GROUP2, CRS, Neurotoxicity, AE_ANEMIA_GR_3, AE_NEUTROPENIA_GR_3, AE_TROMBOCYTOPENIA_GR_3, AE_LYMPHOPENIA_GR_3,
#          AE_INFECTION_GR, AE_NEUROPATHY_LOAD_GR, AE_BILIRUBINEMIA_GR, , AE_SECONDARY_MALIGNANCY_GR, AE_PSYCHIATRIC_DYSFUNCTION_GR) %>%
#   tbl_summary(
#     by = CTX_GROUP2
#   ) %>%
#   add_p() %>%
#   add_overall() %>%
#   bold_labels() %>% bold_p() %>% italicize_levels() %>% as_gt() %>%
#   gt::gtsave("Table_safety2.docx")



### T-cell subgroup analysis
bcma.mice <- filter(imp2, !is.na(CTX_GROUP3))

# -- matching -- #
match_form = CTX_GROUP3 ~ AGE * `ISS stage 3` # remove Line and Platelet
match_form_mice = CTX_GROUP3 ~ AGE * ISS3


# bal.tab(match_bcma_mice$models[[1]], thresholds = c(m = 0.2, v = 2), un =T, binary = "std")

# Survival curve
# table(complete(imp2, 1)$CTX_NAME, complete(imp2, 1)$CTX_GROUP3)
# table(data$CTX_NAME, data$CTX_GROUP3)
# table(imp_dat1$CTX_NAME, imp_dat1$CTX_GROUP3)

imp_dat1 = 
  complete(bcma.mice, 1) %>% 
  filter(PID %in% matched_BiTE_ID) %>%
  mutate(cohortBD = ifelse(CTX_NAME %in% c("Erlanatamab", "Linvoseltamab", "Regeneron", "Teclistamab"), "B", 
                           ifelse(CTX_NAME %in% c("Cevostamab", "Forimtamig", "Talquetamab"), "D", NA)),
         cohortBD = factor(cohortBD, levels = c("D", "B"))) %>%
  filter(!is.na(cohortBD)) %>% 
  matchit(cohortBD ~ AGE * ISS3, data = ., method = "full", distance ="glm", ratio = 1, replace = FALSE) %>%
  match.data()

imp_dat2 = 
  complete(bcma.mice, 1) %>% 
  filter(PID %in% matched_BiTE_ID) %>%
  mutate(cohortBD = ifelse(CTX_NAME %in% c("Erlanatamab + Drd", "Erlanatamab +Daratumumab", "Teclistamab+Daratumumab"), "C", 
                           ifelse(CTX_NAME %in% c("Talquetamab + Daratumumab", "Talquetamab + Dpd"), "E", NA)),
         cohortBD = factor(cohortBD, levels = c("E", "C"))) %>%
  filter(!is.na(cohortBD)) %>% 
  matchit(cohortBD ~ AGE * ISS3, data = ., method = "full", distance ="glm", ratio = 1, replace = FALSE) %>%
  match.data()

imp_dat_mice1 = 
  bcma.mice %>% 
  filter(PID %in% matched_BiTE_ID) %>%
  filter(CTX_NAME %in% c("Erlanatamab", "Linvoseltamab", "Regeneron", "Teclistamab", "Cevostamab", "Forimtamig", "Talquetamab")) %>% 
  matchthem(CTX_GROUP3 ~ AGE * ISS3, data = ., method = "full", distance ="glm", ratio = 1, replace = FALSE)

imp_dat_mice2 = 
  bcma.mice %>% 
  filter(PID %in% matched_BiTE_ID) %>%
  filter(CTX_NAME %in% c("Erlanatamab + Drd", "Erlanatamab +Daratumumab", "Teclistamab+Daratumumab",
                         "Talquetamab + Daratumumab", "Talquetamab + Dpd")) %>% 
  matchthem(CTX_GROUP3 ~ AGE * ISS3, data = ., method = "full", distance ="glm", ratio = 1, replace = FALSE)


OS_coxph1 = with(imp_dat_mice1, coxph(Surv(DATE_DIF, VS) ~ CTX_GROUP3, cluster = subclass)) %>% pool
OS_coxph2 = with(imp_dat_mice2, coxph(Surv(DATE_DIF, VS) ~ CTX_GROUP3, cluster = subclass)) %>% pool

PFS_coxph1 = with(imp_dat_mice1, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ CTX_GROUP3, cluster = subclass)) %>% pool
PFS_coxph2 = with(imp_dat_mice2, coxph(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ CTX_GROUP3, cluster = subclass)) %>% pool


OS_survfit1 = survfit(Surv(DATE_DIF, VS) ~ cohortBD, data = imp_dat1) 
OS_survfit2 = survfit(Surv(DATE_DIF, VS) ~ cohortBD, data = imp_dat2)

survfit(Surv(DATE_DIF, VS == 0) ~ cohortBD, data = imp_dat1)
survfit(Surv(DATE_DIF, VS == 0) ~ cohortBD, data = imp_dat2)

PFS_survfit1 = survfit(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohortBD, data = imp_dat1)
PFS_survfit2 = survfit(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohortBD, data = imp_dat2)


survminer::surv_summary(OS_survfit1, data = imp_dat1) %>% filter(time >= 4, time <= 6) %>% arrange(strata, -time)
survminer::surv_summary(OS_survfit2, data = imp_dat2) %>% filter(time >= 4, time <= 6) %>% arrange(strata, -time)

logrank_time(Surv(DATE_DIF, VS) ~ cohortBD, imp_dat1, max_time = 6)
logrank_time(Surv(DATE_DIF, VS) ~ cohortBD, imp_dat2, max_time = 6)

survminer::surv_summary(PFS_survfit1, data = imp_dat1) %>% filter(time >= 4, time <= 6) %>% arrange(strata, -time)
survminer::surv_summary(PFS_survfit2, data = imp_dat2) %>% filter(time >= 4, time <= 6) %>% arrange(strata, -time)

logrank_time(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohortBD, imp_dat1, max_time = 6)
logrank_time(Surv(DAYS_VS_PFS_CTX, VS_PFS_CTX) ~ cohortBD, imp_dat2, max_time = 6)



OS_survplot1 = OS_survfit1 %>% my_survplot(text_vec = c(16, 0.7, round(c(exp(summary(OS_coxph1)[[2]]), summary(OS_coxph1)[[6]]), 2)), 
                                           legend.labs = c("Non-BCMA monotherapy", "BCMA monotherapy"), legend = c(0.32, 0.2),
                                           risk.table.label = c("BCMA<br>monotherapy", "Non-BCMA<br>monotherapy"), title = "(A)", vjust = -8)

OS_survplot2 = OS_survfit2 %>% my_survplot(text_vec = c(18, 0.7, round(c(exp(summary(OS_coxph2)[[2]]), summary(OS_coxph2)[[6]]), 2)), 
                                           legend.labs = c("Non-BCMA combination with daratumumab", "BCMA combination with daratumumab"), legend = c(0.42, 0.2),
                                           risk.table.label = c("BCMA<br>combination", "Non-BCMA<br>combination"), title = "(C)", vjust = -8)

PFS_survplot1 = PFS_survfit1 %>% my_survplot(text_vec = c(16, 0.7, round(c(exp(summary(PFS_coxph1)[[2]]), summary(PFS_coxph1)[[6]]), 2)), 
                                             legend.labs = c("Non-BCMA monotherapy", "BCMA monotherapy"), legend = c(0.32, 0.2),
                                             risk.table.label = c("BCMA<br>monotherapy", "Non-BCMA<br>monotherapy"), title = "(B)", vjust = -8)

PFS_survplot2 = PFS_survfit2 %>% my_survplot(text_vec = c(18, 0.7, round(c(exp(summary(PFS_coxph2)[[2]]), summary(PFS_coxph2)[[6]]), 2)), 
                                             legend.labs = c("Non-BCMA combination with daratumumab", "BCMA combination with daratumumab"), legend = c(0.42, 0.2),
                                             risk.table.label = c("BCMA<br>combination", "Non-BCMA<br>combination"), title = "(D)", vjust = -8)


ggsave("Figure 3A.png", OS_survplot1$plot / OS_survplot1$table + plot_layout(heights = c(0.83, 0.17)) +
         plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))), device = "png", width = 5, height = 5, dpi = 600)
ggsave("Figure 3B.png", PFS_survplot1$plot / PFS_survplot1$table + plot_layout(heights = c(0.83, 0.17)) +
         plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))), device = "png", width = 5, height = 5, dpi = 600)
ggsave("Figure 3C.png", OS_survplot2$plot / OS_survplot2$table + plot_layout(heights = c(0.83, 0.17)) +
         plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))), device = "png", width = 5, height = 5, dpi = 600)
ggsave("Figure 3D.png", PFS_survplot2$plot / PFS_survplot2$table + plot_layout(heights = c(0.83, 0.17)) +
         plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))), device = "png", width = 5, height = 5, dpi = 600)


ggarrange(
  ggarrange(OS_survplot1$plot / OS_survplot1$table + plot_layout(heights = c(0.83, 0.17)) +
              plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))),
            PFS_survplot1$plot / PFS_survplot1$table + plot_layout(heights = c(0.83, 0.17)) +
              plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))), ncol = 2),
  ggarrange(OS_survplot2$plot / OS_survplot2$table + plot_layout(heights = c(0.83, 0.17)) +
              plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))),
            PFS_survplot2$plot / PFS_survplot2$table + plot_layout(heights = c(0.83, 0.17)) +
              plot_annotation(theme = theme(plot.margin = unit(c(5.5, 5.5, 5.5, -20), "pt"))), ncol = 2),
  nrow = 2
)
ggsave("Figure 4.png", device = "png", dpi = 600, width = 10, height = 10)



OS_survplot1 = 
  OS_survfit1 %>% ggsurvplot(risk.table = T, xlim = c(0, 24), break.x.by = 4, xlab = "Time (Month)", 
                             legend.title = "Treatment", legend.labs = c("Non-BCMA monotherapy", "BCMA monotherapy"),
                             legend = c(0.4, 0.3), palette = rev(pal_lancet()(2)),
                             risk.table.col = "strata", font.family = "serif",
                             risk.table.legend.labs = c("A", "B"), risk.table.ylab = "")
OS_survplot1$plot = OS_survplot1$plot + theme(text = element_text(family = "serif"))
OS_survplot1$table = OS_survplot1$table + theme(text = element_text(family = "serif"), 
                                                axis.title.y = element_blank(), legend.position = "none") +
  scale_y_discrete(labels = c("Non-BCMA monotherapy" = "Non-BCMA", "BCMA monotherapy" = "BCMA"))
OS_survplot2 = 
  OS_survfit2 %>% ggsurvplot(risk.table = T, xlim = c(0, 24), break.x.by = 4, xlab = "Time (Month)", 
                             legend.title = "Treatment", legend.labs = c("Non-BCMA combination", "BCMA combination"),
                             legend = c(0.4, 0.3), palette = rev(pal_lancet()(2)),
                             risk.table.col = "strata", font.family = "serif")
OS_survplot2$plot = OS_survplot2$plot + theme(text = element_text(family = "serif"))
OS_survplot2$table = OS_survplot2$table + theme(text = element_text(family = "serif"), 
                                                axis.title.y = element_blank(), legend.position = "none") +
  scale_y_discrete(labels = c("Non-BCMA combination" = "Non-BCMA", "BCMA combination" = "BCMA"))

PFS_survplot1 = 
  PFS_survfit1 %>% ggsurvplot(risk.table = T, xlim = c(0, 24), break.x.by = 4, xlab = "Time (Month)", 
                              legend.title = "Treatment", legend.labs = c("Non-BCMA monotherapy", "BCMA monotherapy"), 
                              legend = c(0.75, 0.83), palette = rev(pal_lancet()(2)),
                              risk.table.col = "strata", font.family = "serif", surv.median.line = "hv")
PFS_survplot1$plot = PFS_survplot1$plot + theme(text = element_text(family = "serif"))
PFS_survplot1$table = PFS_survplot1$table + theme(text = element_text(family = "serif"), 
                                                  axis.title.y = element_blank(), legend.position = "none") +
  scale_y_discrete(labels = c("Non-BCMA monotherapy" = "Non-BCMA", "BCMA monotherapy" = "BCMA"))

PFS_survplot2 = 
  PFS_survfit2 %>% ggsurvplot(risk.table = T, xlim = c(0, 24), break.x.by = 4, xlab = "Time (Month)", 
                              legend.title = "Treatment", legend.labs = c("Non-BCMA combination", "BCMA combination"),
                              legend = c(0.4, 0.3), palette = rev(pal_lancet()(2)),
                              risk.table.col = "strata", font.family = "serif", surv.median.line = "hv")
PFS_survplot2$plot = PFS_survplot2$plot + theme(text = element_text(family = "serif"))
PFS_survplot2$table = PFS_survplot2$table + theme(text = element_text(family = "serif"), 
                                                  axis.title.y = element_blank(), legend.position = "none") +
  scale_y_discrete(labels = c("Non-BCMA combination" = "Non-BCMA", "BCMA combination" = "BCMA"))





OS_survplot = adsurv(bcma.mice, variable = "CTX_GROUP3", ev_time = "DATE_DIF", event = "VS", method = "matching", 
                     treatment_model = match_form_mice,
                     matching_method = "full")
OS_survplot$data$data$CTX_GROUP3 = 
  factor(ifelse(OS_survplot$data$data$CTX_GROUP3 == 0, levels(data$CTX_GROUP3)[1],levels(data$CTX_GROUP3)[2]),
         levels = c(levels(data$CTX_GROUP3)[2], levels(data$CTX_GROUP3)[1]))

PFS_survplot = adsurv(bcma.mice, variable = "CTX_GROUP3", ev_time = "DAYS_VS_PFS_CTX", event = "VS_PFS_CTX", method = "matching", 
                      treatment_model = match_form_mice,
                      matching_method = "full")
PFS_survplot$data$data$CTX_GROUP3 = 
  factor(ifelse(PFS_survplot$data$data$CTX_GROUP3 == 0, levels(data$CTX_GROUP3)[1],levels(data$CTX_GROUP3)[2]),
         levels = c(levels(data$CTX_GROUP3)[2], levels(data$CTX_GROUP3)[1]))

OS_curv = 
  plot(OS_survplot, 
       risk_table = T, risk_table_stratify = T, risk_table_warn = F, risk_table_ylab = "",
       censoring_ind = "lines", median_surv_lines = T,
       legend = c(0.28, 0.2), legend.title= "Treatment", 
       ylab = "Survival probability", xlab = "Time (Month)", x_breaks = seq(0,12,4),
       ylim = c(0, 1),
       custom_colors = rev(pal_lancet()(2)),
       font.x = c(13), font.y = c(13), font.legend = c(12),
       title = "A", max_t = 12, do_rev = T, risk_table_family = "serif") +
  annotate("text", x = 0.80, y = 0.5, label = paste0("HR=", round_pad(mat_HR_res[1,1], 2), "; P=", round_pad(summary(psm_mice1)[6], 2)), family = "serif")

# Treatment 이름 바꾸기
PFS_curv = 
  plot(PFS_survplot, 
       risk_table = T, risk_table_stratify = T, risk_table_warn = F, risk_table_ylab = "",
       censoring_ind = "lines", median_surv_lines = T, 
       legend = c(0.28, 0.2), legend.title= "Treatment",
       ylab = "Survival probability", xlab = "Time (Month)", x_breaks = seq(0,12,4), ylim = c(0,1),
       custom_colors = rev(pal_lancet()(2)),
       font.x = c(13), font.y = c(13), font.legend = c(12),
       title = "B", max_t = 12, do_rev = T, risk_table_family = "serif") +
  annotate("text", x = 0.75, y = 0.5, label = paste0("HR=", round_pad(mat_HR_res[4,1], 2), "; P=", round_pad(summary(psm_mice2)[6], 2)), family = "serif")

res = ggarrange(OS_curv, PFS_curv) ; res
res_new = annotate_figure(res, top = text_grob("", size = 14, family = "serif", hjust = 7))

ggsave("Figure4.png", res_new, device = "png", width = 10, height = 5, dpi = 2000, bg = "white")

# ggsave("FigureS2_1.png", res, device = "png", width = 10, height = 5, dpi = 300)


OS_survplot$adj %>% filter(time <= 6) %>% group_by(group) %>% summarize(rate = min(surv))
OS_survplot$adj %>% filter(time <= 12) %>% group_by(group) %>% summarize(rate = min(surv))
PFS_survplot$adj %>% filter(time <= 6) %>% group_by(group) %>% summarize(rate = min(surv))
PFS_survplot$adj %>% filter(time <= 12) %>% group_by(group) %>% summarize(rate = min(surv))


# logrank_mice(Surv(DATE_DIF/30.42, VS) ~ CTX_GROUP3, match_bcma_mice, max_time = 6)
# logrank_mice(Surv(DATE_DIF/30.42, VS) ~ CTX_GROUP3, match_bcma_mice, max_time = 12)

# logrank_mice(Surv(DAYS_VS_PFS_CTX/30.42, VS_PFS_CTX) ~ CTX_GROUP3, match_bcma_mice, max_time = 6)
# logrank_mice(Surv(DAYS_VS_PFS_CTX/30.42, VS_PFS_CTX) ~ CTX_GROUP3, match_bcma_mice, max_time = 12)



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
# plot_timeROC_medianFU(model_list1)
# plot_timeROC_medianFU(model_list2)
# plot_timeROC_medianFU(model_list3)

# compute_iAUC(model_list1)
# compute_iAUC(model_list2)
# compute_iAUC(model_list3)

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


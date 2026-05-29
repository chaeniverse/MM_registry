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
library(boot)

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
  out_dir <- "/Users/chaehyun/Library/CloudStorage/Dropbox/연구_PIPET/PIPET_Hematology/MM/Lpl/Figure/[26-05-11] time-dependent ROC"
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


plot_timeROC_overlay <- function(data, model_sets, AUC_TIMES, PLOT_TIMES, OUTPUT_DIR) {

  all_vars <- unique(unlist(model_sets))
  dat_cc   <- data[, c("death", "death_yr", all_vars)] %>% na.omit()

  results <- list()

  for (nm in names(model_sets)) {
    vars   <- model_sets[[nm]]
    marker <- dat_cc[[vars]]          # 스코어 값 자체를 marker로 사용

    tr <- timeROC(T         = dat_cc$death_yr,
                  delta     = dat_cc$death,
                  marker    = marker,
                  cause     = 1,
                  weighting = "marginal",
                  times     = AUC_TIMES,
                  iid       = TRUE)

    results[[nm]] <- list(timeROC = tr, n = nrow(dat_cc))
  }

  colors <- pal_lancet()(length(results))

  for (tp in PLOT_TIMES) {
    png(file.path(OUTPUT_DIR, sprintf("ROC_overlay_%dyr.png", tp)),
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
}

plot_survival_by_group <- function(fit, survival, OUTPUT_DIR, file_name, group_col, palette = pal_lancet()(3)[c(1, 3, 2)]) {

  png(paste0(OUTPUT_DIR, file_name), height = 10, width = 10, units = "in", res = 300)
  font_size <- 16
  p <- ggsurvplot(
    fit, data           = survival,
    surv.median.line    = "hv",
    risk.table          = TRUE,
    tables.col          = "strata",
    tables.y.text       = FALSE,
    conf.int            = FALSE,
    xlim                = c(0, 12),
    xlab                = "Time (years)",
    ylab                = "Survival Probability (%)",
    legend.title        = "",
    legend.labs         = levels(survival[[group_col]]),
    tables.height       = 0.25,
    break.time.by       = 1,
    risk.table.fontsize = 5,
    palette             = palette,
    tables.theme        = theme_cleantable()
  )
  p$plot <- p$plot +
    scale_y_continuous(labels = function(x) x * 100) +
    theme(axis.title  = element_text(size = font_size),
          axis.text   = element_text(size = font_size),
          legend.text = element_text(size = font_size - 2))
  print(p)
  dev.off()
}

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

km_summary_by_group <- function(data, group_col, time_col, event_col) {

  fml <- as.formula(sprintf("Surv(%s, %s) ~ %s", time_col, event_col, group_col))
  fit <- survfit(fml, data = data)

  # -- Median OS --
  med_surv <- as.data.frame(surv_median(fit)) %>%
    mutate(`Median OS (95% CI)` =
            sprintf("%.1f (%.1f–%.1f)", median, lower, upper)) %>%
    select(strata, `Median OS (95% CI)`)

  # -- Median follow-up (reverse KM) --
  rev_dat <- data %>% mutate(death_rev = 1 - .data[[event_col]])
  fml_rev <- as.formula(sprintf("Surv(%s, death_rev) ~ %s", time_col, group_col))
  fit_rev <- survfit(fml_rev, data = rev_dat)
  med_fu  <- as.data.frame(surv_median(fit_rev)) %>%
    mutate(`Median FU (95% CI)` =
            sprintf("%.1f (%.1f–%.1f)", median, lower, upper)) %>%
    select(strata, `Median FU (95% CI)`)

  surv_5yr  <- fmt_surv(fit, 5);  names(surv_5yr)[2]  <- "5yr OS (95% CI)"
  surv_10yr <- fmt_surv(fit, 10); names(surv_10yr)[2] <- "10yr OS (95% CI)"

  km_summary <- med_surv %>%
    full_join(med_fu,    by = "strata") %>%
    full_join(surv_5yr,  by = "strata") %>%
    full_join(surv_10yr, by = "strata") %>%
    mutate(Group = gsub(paste0(group_col, "="), "", strata, fixed = TRUE)) %>%
    select(Group, `Median OS (95% CI)`, `Median FU (95% CI)`,
          `5yr OS (95% CI)`, `10yr OS (95% CI)`)

  km_summary %>%
    gt() 
}


cox_hr_by_group <- function(data, group_col, time_col, event_col) {
  fml <- as.formula(sprintf("Surv(%s, %s) ~ %s", time_col, event_col, group_col))
  mod_unadj <- coxph(fml, data = data)
  s_unadj   <- summary(mod_unadj)

  unadj_tbl <- data.frame(
    Group = gsub(paste0("^`?", group_col, "`?"), "", rownames(s_unadj$conf.int)),
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
    tab_header(
      title    = "",
      subtitle = sprintf("N = %d/%d", mod_unadj$n, nrow(data))
    )


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


plot_regimen <- function(dat, regimen_name, group_var = "group_RIPSS_Others") {
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


summary_regimen <- function(dat, regimen_name, group_var = "group_RIPSS_Others"){
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
  s5  <- fmt_surv(fit, 5)  |> dplyr::rename(y5        = value)
  s10 <- fmt_surv(fit, 10) |> dplyr::rename(y10       = value)
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



plot_score_histogram_with_cutoff <- function(survival, score_var, OUTPUT_DIR, lancet_cols) {

  roc2 <- roc(survival[["death"]], survival[[score_var]])
  coords_best <- coords(roc2, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))
  print(coords_best)

  # -- Histogram -- #
  survival$outcome <- factor(survival$death, levels = c(0, 1),
                                labels = c("Alive", "Dead"))

  mu <- survival %>%
    group_by(outcome) %>%
    summarise(grp.mean = mean(.data[[score_var]], na.rm = TRUE))

  p <- ggplot(survival, aes(x = .data[[score_var]], fill = outcome, color = outcome)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, center = 0, alpha = 0.2, position = "identity") +
    # geom_vline(data = mu, aes(xintercept = grp.mean, color = outcome), linetype = "dashed") +
    geom_vline(xintercept = coords_best$threshold, linetype = "dotted", color = "black", linewidth = 1) +
    annotate("text", x = coords_best$threshold + 1, y = Inf, vjust = 2,
            label = sprintf("Cutoff = %.1f", coords_best$threshold), size = 4) +
    scale_fill_manual(values = lancet_cols, labels = c("Alive", "Dead")) +
    scale_color_manual(values = lancet_cols, labels = c("Alive", "Dead")) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(fill = NA, colour = "black"),
      axis.text = element_text(size = 12, colour = "black"),
      axis.title = element_text(size = 14, colour = "black"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11)
    )

  ggsave(paste0(OUTPUT_DIR, score_var, "_histogram.png"), plot = p, height = 10, width = 10, dpi = 300)

}

plot_timeAUC_overlay <- function(model_sets, AUC_TIMES, OUTPUT_DIR,
                                 xlab = "Time (years)",
                                 file_name = "timeAUC_overlay.png",
                                 data = dat,
                                 ci_tbl = NULL) {

  all_vars <- unique(unlist(model_sets))
  dat_cc   <- data[, c("death", "death_yr", all_vars)] %>% na.omit()

  results <- list()
  for (nm in names(model_sets)) {
    vars <- model_sets[[nm]]

    tr <- timeROC(T         = dat_cc$death_yr,
                  delta     = dat_cc$death,
                  marker    = dat_cc[[ vars[1] ]],     # 첫 변수(점수)로 AUC(t) 계산 — 의도대로 유지
                  cause     = 1,
                  weighting = "marginal",
                  times     = AUC_TIMES)

    # iAUC / CI 는 앞서 구한 ci_tbl 에서 가져옴
    iauc <- NA_real_                                   # NULL -> NA_real_ (범례 안전)
    ci   <- c(NA_real_, NA_real_)
    if (!is.null(ci_tbl)) {
      row <- ci_tbl[ci_tbl$Score == nm, ]
      if (nrow(row) == 1) {
        ci   <- c(row$lower, row$upper)
        iauc <- row$iAUC
      }
    }

    results[[nm]] <- list(timeROC = tr, vars = vars,
                          n = nrow(dat_cc), iauc = iauc, ci = ci)
  }

  colors <- pal_lancet()(length(results))

  # ---- 단일 overlay 그림: AUC(t) vs time ----
  png(file.path(OUTPUT_DIR, file_name), height = 8, width = 8, units = "in", res = 300)
  par(pty = "s", mar = c(5, 5, 4, 2))

  plot(NA, xlim = range(AUC_TIMES), ylim = c(0.1, 1),
       xlab = xlab, ylab = "Time-dependent AUC",
       cex.lab = 1.3, cex.axis = 1.1, las = 1)
  abline(h = 0.5, lty = 2, col = "grey60")

  for (i in seq_along(results)) {
    tr <- results[[i]]$timeROC
    lines(tr$times, tr$AUC, col = colors[i], lwd = 3, type = "o", pch = 16)
  }
  title(main = "Time-dependent AUC", cex.main = 1.3)

  # 범례 = 모델명 + iAUC(있으면) (+CI 있으면)
  legend_labels <- sapply(seq_along(results), function(i) {
    r  <- results[[i]]
    nm <- names(results)[i]
    if (is.null(r$iauc) || is.na(r$iauc))              # iAUC 없으면 이름만 (NULL 에러 방지)
      nm
    else if (anyNA(r$ci))
      sprintf("%s: %.3f", nm, r$iauc)
    else
      sprintf("%s: %.3f (%.3f-%.3f)", nm, r$iauc, r$ci[1], r$ci[2])
  })
  legend("bottomright", legend = legend_labels,
         col = colors, lwd = 3, cex = 0.9, bty = "n")
  dev.off()

  cat(sprintf("Plot saved: %s\n", file.path(OUTPUT_DIR, file_name)))
  invisible(results)
}

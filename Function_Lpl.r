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
library(patchwork)

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
  names(colors) <- names(results)

  font_size <- 24

  for (tp in PLOT_TIMES) {
    # 모델별 ROC 좌표(FP, TP) (plot_timeAUC_overlay 스타일 ggplot)
    df <- do.call(rbind, lapply(names(results), function(nm) {
      tr  <- results[[nm]]$timeROC
      idx <- which(tr$times == tp)
      data.frame(model = nm,
                 FP = as.numeric(tr$FP[, idx]),
                 TP = as.numeric(tr$TP[, idx]))
    }))
    df$model <- factor(df$model, levels = names(results))

    # 범례 = 모델명 + AUC (95% CI)
    legend_labels <- sapply(seq_along(results), function(i) {
      tr  <- results[[i]]$timeROC
      idx <- which(tr$times == tp)
      a   <- tr$AUC[idx]
      se  <- tr$inference$vect_sd_1[idx]
      sprintf("%s = %.3f (%.3f-%.3f)",
              names(results)[i], a, a - 1.96*se, a + 1.96*se)
    })

    p <- ggplot(df, aes(x = FP, y = TP, color = model)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
      geom_line(linewidth = 1) +
      scale_color_manual(values = colors, labels = legend_labels) +
      scale_x_continuous(limits = c(0, 1)) +   # expand 생략 = 기본 5% 여백
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "1-Specificity", y = "Sensitivity", color = NULL) +   # ylab(Sensitivity) 유지
      theme_classic(base_size = font_size) +
      theme(axis.title           = element_text(size = font_size),
            axis.text            = element_text(size = font_size),
            legend.text          = element_text(size = font_size - 2),
            legend.position      = c(0.98, 0.02),
            legend.justification = c(1, 0),
            legend.background    = element_blank(),   # 범례 흰색 박스 제거 (투명)
            legend.key           = element_blank(),   # 범례 키 흰색 박스 제거 (투명)
            aspect.ratio         = 1)

    ggsave(file.path(OUTPUT_DIR, sprintf("ROC_overlay_%dyr.png", tp)), p,
           width = 10, height = 10, units = "in", dpi = 300)   # timeAUC와 동일 비율
    cat(sprintf("  [%d-year] ROC plotted\n", tp))
  }
}

# -- Cumulative incidence curve (opt_plot 스타일) -- #
opt_cuminc <- function(data, OUTPUT_DIR, file_name,
                       time_col = "time_yr", status_col = "status",
                       outcome = "1",
                       risk_times = 0:6, marker_times = c(3, 6),
                       palette = pal_lancet()(2)[2], font_size = 24) {

  suppressWarnings(suppressMessages({

  default_gray <- theme_gray()$line$colour
  xmax <- max(risk_times)

  brk <- if (length(risk_times) > 1) risk_times[2] - risk_times[1] else 1
  # 두 패널 x축 정렬용 공통 스케일
  sx  <- scale_x_continuous(breaks = risk_times, limits = c(0, xmax),
                            expand = expansion(mult = c(0.06, 0.02)), oob = scales::oob_keep)  # 왼쪽 여백 ↑ (첫 숫자 안 잘리게)

  # -- CIF curve (ggcuminc) --
  fml <- as.formula(sprintf("Surv(%s, %s) ~ 1", time_col, status_col))
  cif <- tidycmprsk::cuminc(fml, data = data) |>
    ggcuminc(outcome = outcome, linewidth = 1) +
    add_confidence_interval() +
    scale_y_continuous(breaks = seq(0, 1, 0.25), labels = function(x) x * 100, limits = c(0, 1)) +
    sx +
    labs(x = "Time (Year)", y = "Cumulative Incidence (%)") +
    theme_classic(base_size = font_size) +
    theme(axis.title      = element_text(size = font_size),
          axis.text       = element_text(size = font_size),
          legend.position = "none")

  # marker time 점선 (예: 3yr / 6yr)
  for (mt in marker_times) {
    cif <- cif + geom_vline(xintercept = mt, linetype = "dashed",
                            color = default_gray, linewidth = 0.5)
  }
  cif$layers[[1]]$aes_params$colour <- palette[1]
  cif$layers[[2]]$aes_params$fill   <- palette[1]

  # -- Number at risk table : survival curve(ggsurvplot)의 $table 을 그대로 사용 --
  ev   <- as.integer(as.character(data[[status_col]]) != levels(factor(data[[status_col]]))[1])
  df   <- data.frame(.t = data[[time_col]], .e = ev)
  sfit <- survfit(Surv(.t, .e) ~ 1, data = df)

  g <- ggsurvplot(
    sfit, data       = df,
    risk.table       = TRUE,
    risk.table.fontsize         = 9,
    xlab             = "Time (Year)",                                #!! 위험표 x축 라벨
    tables.theme     = theme(axis.text.y  = element_text(size = 24),
                             axis.text.x  = element_text(size = 24),
                             axis.title.x = element_text(size = 24),
                             axis.title.y = element_blank(),          #!! "Strata" 제목 제거 ("All"은 유지)
                             plot.title   = element_text(size = 24)),  # "Number at risk" 제목
    xlim             = c(0, xmax),
    break.time.by    = brk,
    palette          = palette[1]
  )
  risktab <- g$table + sx +
    theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))

  # -- curve + risk table 결합 (opt_plot 스타일, heights 5:1) --
  image <- cif / risktab + plot_layout(heights = c(5, 1))

  ggsave(paste0(OUTPUT_DIR, file_name), image, width = 10, height = 10, units = "in", dpi = 300)
  print(image)
  }))
}

# -- Survival probability at 5yr / 10yr --
# extend = TRUE 로 표 구조(모든 그룹 × 모든 시점)는 유지하되,
# 요청 시점(tp)이 그 그룹의 최대 추적시간을 넘으면(외삽) 그 칸만 "NR"로 마스킹
fmt_surv <- function(fit, tp) {
  s          <- summary(fit, times = tp, extend = TRUE)
  strata_chr <- as.character(s$strata)

  # 그룹별 최대 추적시간(마지막 관찰시점)
  if (is.null(fit$strata)) {                         # 단일 그룹
    max_fu <- setNames(max(fit$time), strata_chr[1])
  } else {                                           # 다중 그룹
    grp    <- rep(names(fit$strata), fit$strata)
    max_fu <- tapply(fit$time, grp, max)
  }

  value <- sprintf("%.1f%% (%.1f–%.1f)", s$surv * 100, s$lower * 100, s$upper * 100)
  value[tp > max_fu[strata_chr]] <- "NR"             # 추적 못 미친 시점 = 외삽 → NR (원하면 "NE"/"—"/NA로 변경)

  data.frame(strata = strata_chr, value = value, stringsAsFactors = FALSE)
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


plot_score_histogram_with_cutoff <- function(survival, score_var, OUTPUT_DIR, lancet_cols, xlab = score_var, xticks = waiver()) {

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
    scale_fill_manual(values = lancet_cols, labels = c("Alive", "Dead")) +
    scale_color_manual(values = lancet_cols, labels = c("Alive", "Dead")) +
    labs(x = xlab, y = "Density") +
    scale_x_continuous(breaks = xticks) +
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
                                 xlab = "Time (Year)",
                                 file_name = "timeAUC_overlay.png",
                                 data = dat,
                                 ci_tbl = NULL,
                                 font_size = 24) {

  suppressWarnings(suppressMessages({

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
  names(colors) <- names(results)

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

  # ---- 단일 overlay 그림: AUC(t) vs time (opt_plot 스타일 ggplot) ----
  df <- do.call(rbind, lapply(names(results), function(nm) {
    tr <- results[[nm]]$timeROC
    data.frame(model = nm, time = tr$times, AUC = as.numeric(tr$AUC))
  }))
  df$model <- factor(df$model, levels = names(results))

  p <- ggplot(df, aes(x = time, y = AUC, color = model)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey60") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_color_manual(values = colors, labels = legend_labels) +
    scale_x_continuous(breaks = AUC_TIMES,
                       expand = expansion(mult = c(0.02, 0.02))) +
    scale_y_continuous(limits = c(0.1, 1)) +
    labs(x = xlab, y = "AUC (t)", color = NULL) +
    theme_classic(base_size = font_size) +
    theme(axis.title           = element_text(size = font_size),
          axis.text            = element_text(size = font_size),
          legend.text          = element_text(size = font_size),
          legend.position      = c(0.98, 0.02),
          legend.justification = c(1, 0),
          aspect.ratio         = 1)

  ggsave(file.path(OUTPUT_DIR, file_name), p,
         width = 10, height = 10, units = "in", dpi = 300)   # opt_plot/opt_cuminc과 동일 비율
  cat(sprintf("Plot saved: %s\n", file.path(OUTPUT_DIR, file_name)))
  print(p)
  invisible(results)
  }))
}


opt_plot = function(a, OUTPUT_DIR, file_name, deadline = 8, conf = TRUE, ptitle = "up", col="", palette = NULL, pval = TRUE,
                    risk_fontsize = 9, table_height = 1) {
# Option ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  suppressWarnings(suppressMessages({
  color <- if (col == "rev") {color <- list(pal_lancet()(2)[2], rev(pal_lancet()(2)), rev(pal_lancet()(3)[c(3, 1, 2)])) # 컬러 조합 방법
  } else {color <- list(pal_lancet()(2)[2], pal_lancet()(2),pal_lancet()(3)[c(1, 3, 2)])}
  
  group <- length(a$n)                                                            # group 개수

  if (is.null(palette)) palette <- color[[group]]                                 # palette 미지정 시 기본 lancet(그룹 1~3); 지정 시 그대로(예: 5그룹)

  l <- c("none")                                                                  # legend 사용 여부

  deadline <- c(-0.1, deadline)                                                   # x축 범위 (연 단위)

#  itv <- ifelse(deadline[2] >= 10, 2, 1)                                          # 눈금(조건에 따라, 1년 또는 2년)
  itv <- 1
  
  pv <- FALSE                                                                     # p-value 출력 여부(뒤에 조건에 따라 변경)
  
  if (sum(ptitle %in% "up")>0) {ptitle = c(deadline[2]*0.75,0.90)                 #!! p-value 위치
  } else if (sum(ptitle %in% "down")>0) {ptitle = c(0,0.25)
  } else {ptitle = c(deadline[2]*ptitle[1],ptitle[2])}

# Survival Plot ------------------------------------------------------------------------------------------------------------------------------------------------------------------  
  if (group > 1){
    names(a$strata) <- substr(names(a$strata), str_locate(names(a$strata), "=")[1,1] + 1, 100)      # 라벨링
    ytitle <- -max(nchar(names(a$strata)))                                                          # 텍스트 위치 이동(Survival probability (%))
    l <- "top"                                                                                      # legend 위치
    pv <- pval                                                                                      # p-value 출력(그룹 2개 이상 & pval=TRUE일 때만)
  }
  
  p =
    a %>% ggsurvplot(legend.title = "",                                                             
                     legend = l,
                     xlab = "Time (Year)",
                     xlim = deadline,
                     break.x.by = itv,
                     ylab = "Survival probability (%)",
                     conf.int = conf,
                     conf.int.fill = "strata",                                                      # 신뢰구간 그룹 색상 지정
                     risk.table = T,
                     risk.table.fontsize = risk_fontsize,
                     tables.theme = theme(
                       axis.text.y  = element_text(size = 24),   # 그룹명 (Low/Intermediate/High)
                       axis.text.x  = element_text(size = 24),   # 0~8 숫자
                       axis.title.x = element_text(size = 24),
                       axis.title.y = element_text(size = 24),
                       plot.title   = element_text(size = 24)
                     ),
                     palette = palette,
                     surv.median.line = "hv",
                     pval = pv,
                     pval.coord = ptitle,
                     pval.size = 5,
                     font.x = 24,
                     font.y = 24,
                     font.legend = 24,
                     font.tickslab = 24
                    ) %>% suppressMessages() %>% suppressWarnings()                                 # 경고메시지 미출력

# Plot Option -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  p$plot =                                                                                                                                
    p$plot +
    scale_y_continuous(breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25)*100)                                                            # y 축 라벨링 변경
  
  if (group !=1){                                                                                                                         
    p$plot =
      p$plot + theme(axis.title.y = element_text(vjust = ytitle))                                                                         # x 축 라벨링 위치 조정

    if (pv) {                                                                                                                              # p-value 표시할 때만 라벨 포맷 변경
      p_val_geomloc = which(sapply(p$plot$layers, function(x) any(class(x$geom) == "GeomText")))                                          # p-value 출력 값 변경

      if (surv_pvalue(a)[[2]] < 0.001) {p$plot$layers[[p_val_geomloc[1]]]$aes_params$label = "p < 0.001"
        } else {p$plot$layers[[p_val_geomloc[1]]]$aes_params$label = paste0("p = ", sprintf("%.3f", surv_pvalue(a)[2]))}
    }

    p$table = 
      p$table + 
      theme(
        legend.position = "none", 
        plot.margin = margin(0, 0, 0, 0)
      )
  }

  image = p$plot / p$table + plot_layout(heights = c(5, table_height)) # 이미지 사이즈 (table_height로 위험표 높이 조절)

  ggsave(paste0(OUTPUT_DIR, file_name), image, width = 10, height = 10, units = "in", dpi = 300) # PNG 저장 (opt_cuminc과 동일 비율)

  # return(image)
  print(image)
  }))
}

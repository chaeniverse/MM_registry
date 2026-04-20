library(tidyverse)
library(forestploter)
library(grid)

tm <- forest_theme(
  base_size      = 10,
  refline_gp     = gpar(lty = "dashed", col = "grey50"),
  ci_col         = "black",
  ci_fill        = "black",
  ci_Theight     = 0.15,
  core           = list(padding = unit(c(4, 3), "mm"))
)

# --- Helper functions ---

# Subgroup 헤더 행 (e.g., "Age", "Region") + P for interaction
make_header <- function(lab, p_int = "") {
  data.frame(
    Subgroup                      = lab,
    `No. of\nPatients`            = "",
    `Weak`                        = "",
    `Strong`                      = "",
    `Weak and\nStrong`            = "",
    Outcome                       = "",
    `OR (95% CI)`                 = "",
    `P value for\nInteraction`    = p_int,
    est = NA_real_, low = NA_real_, hi = NA_real_,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# 데이터 행 (OR 있는 행)
make_row <- function(lab, n, weak, strong, weak_strong, or, lower, upper) {
  data.frame(
    Subgroup                      = lab,
    `No. of\nPatients`            = as.character(n),
    `Weak`                        = weak,
    `Strong`                      = strong,
    `Weak and\nStrong`            = weak_strong,
    Outcome                       = "",
    `OR (95% CI)`                 = sprintf("%.2f (%.2f\u2013%.2f)", or, lower, upper),
    `P value for\nInteraction`    = "",
    est = or, low = lower, hi = upper,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# --- 데이터 입력 ---
sub_data <- bind_rows(
  # Overall
  make_row("Overall",          3621, "1535 (42.4%)",  "1049 (29.0%)", "1037 (28.6%)", 1.95, 1.78, 2.13),

  # Age
  make_header("Age", "0.403"),
  make_row("  < 50",            230, "86 (37.4%)",    "75 (32.6%)",   "69 (30.0%)",   1.79, 1.29, 2.52),
  make_row("  50\u201365",     1362, "538 (39.5%)",   "445 (32.7%)",  "379 (27.8%)",  2.14, 1.84, 2.49),
  make_row("  ≥ 65",           2029, "911 (44.9%)",   "529 (26.1%)",  "589 (29.0%)",  1.90, 1.68, 2.14),

  # Region
  make_header("Region", "0.522"),
  make_row("  Metropolitan",   2481, "1081 (43.6%)",  "680 (27.4%)",  "720 (29.0%)",  1.91, 1.72, 2.13),
  make_row("  Province",       1140, "454 (39.8%)",   "369 (32.4%)",  "317 (27.8%)",  2.04, 1.73, 2.43),

  # Fracture history
  make_header("History of fracture", "0.637"),
  make_row("  Yes",             580, "175 (30.2%)",   "199 (34.3%)",  "206 (35.5%)",  1.84, 1.47, 2.33),
  make_row("  No",             3041, "1360 (44.7%)",  "850 (28.0%)",  "831 (27.3%)",  1.96, 1.78, 2.16),

  # Mood or sleep disorder
  make_header("Mood or sleep disorder", "0.670"),
  make_row("  Yes",             756, "293 (38.8%)",   "194 (25.7%)",  "269 (35.6%)",  2.01, 1.65, 2.47),
  make_row("  No",             2865, "1242 (43.4%)",  "855 (29.8%)",  "768 (26.8%)",  1.92, 1.74, 2.12),

  # Spinal pathology or pain procedure
  make_header("Spinal pathology or\npain procedure", "0.678"),
  make_row("  Yes",            1942, "789 (40.6%)",   "545 (28.1%)",  "608 (31.3%)",  1.91, 1.69, 2.16),
  make_row("  No",             1679, "746 (44.4%)",   "504 (30.0%)",  "429 (25.6%)",  1.98, 1.74, 2.27),

  # Line of therapy
  make_header("Bortezomib-based therapy", "0.210"),
  make_row("  Bortezomib",     2762, "1165 (42.2%)",  "801 (29.0%)",  "796 (28.8%)",  1.89, 1.70, 2.10),
  make_row("  Other",           859, "370 (43.1%)",   "248 (28.9%)",  "241 (28.1%)",  2.16, 1.81, 2.59)
)

# --- forest plot 용 데이터 ---
sub_vmodel <- sub_data %>%
  select(Subgroup, `No. of\nPatients`, `Weak`, `Strong`, `Weak and\nStrong`,
         Outcome, `OR (95% CI)`, `P value for\nInteraction`)

# --- forest plot 생성 ---
p_sub <- forest(
  sub_vmodel,
  est       = sub_data$est,
  lower     = sub_data$low,
  upper     = sub_data$hi,
  ci_column = 6,            # "Outcome" 컬럼에 forest plot 그림
  ref_line  = 1,
  xlim      = c(1, 3),
  ticks_at  = c(1, 1.5, 2, 3),
  x_trans   = "log",
  theme     = tm
)

# --- 저장 ---
png("/Users/chaehyun/Library/CloudStorage/Dropbox/PIPET_Hematology/MM/Opioid/Figure/Forest_plot.png", height = 7, width = 10, units = "in", res = 300)
plot(p_sub)
dev.off()

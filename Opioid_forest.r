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
    `Short-term\ngroup`           = "",
    `Long-term\ngroup`            = "",
    Outcome                       = "",
    `OR (95% CI)`                 = "",
    `P value for\nInteraction`    = p_int,
    est = NA_real_, low = NA_real_, hi = NA_real_,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# 데이터 행 (OR 있는 행)
make_row <- function(lab, n, short, long, or, lower, upper) {
  data.frame(
    Subgroup                      = lab,
    `No. of\nPatients`            = as.character(n),
    `Short-term\ngroup`           = short,
    `Long-term\ngroup`            = long,
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
  make_row("Overall",          4085, "1467 (35.9%)", "2618 (64.1%)", 2.05, 1.89, 2.22),

  # Age
  make_header("Age", "0.766"),
  make_row("  < 50",        293, "138 (47.1%)",  "155 (52.9%)",  2.08, 1.56, 2.81),
  make_row("  50\u201365",     1561, "576 (36.9%)",  "985 (63.1%)",  2.16, 1.89, 2.47),
  make_row("  65\u201370",      714, "236 (33.1%)",  "478 (66.9%)",  2.12, 1.74, 2.61),
  make_row("  \u226570",       1517, "517 (34.1%)",  "1000 (65.9%)", 1.95, 1.71, 2.24),

  # Region
  make_header("Region", "0.911"),
  make_row("  Metropolitan",   2717, "1004 (37%)",   "1713 (63%)",   2.04, 1.85, 2.26),
  make_row("  Rural",          1368, "463 (33.8%)",  "905 (66.2%)",  2.06, 1.78, 2.39),

  # Opioid history
  make_header("Prior opioid use", "0.600"),
  make_row("  Yes",            1606, "521 (32.4%)",  "1085 (67.6%)", 2.00, 1.75, 2.29),
  make_row("  No",             2479, "946 (38.2%)",  "1533 (61.8%)", 2.09, 1.89, 2.33),

  # Fracture history
  make_header("History of fracture", "0.327"),
  make_row("  Yes",             677, "204 (30.1%)",  "473 (69.9%)",  1.85, 1.51, 2.28),
  make_row("  No",             3408, "1263 (37.1%)", "2145 (62.9%)", 2.07, 1.89, 2.27),

  # Mood or sleep disorder
  make_header("Mood or sleep disorder", "0.769"),
  make_row("  Yes",             820, "235 (28.7%)",  "585 (71.3%)",  2.09, 1.73, 2.53),
  make_row("  No",             3265, "1232 (37.7%)", "2033 (62.3%)", 2.02, 1.85, 2.22),

  # Spinal pathology or pain procedure
  make_header("Spinal pathology or\npain procedure", "0.630"),
  make_row("  Yes",            2205, "736 (33.4%)",  "1469 (66.6%)", 2.00, 1.79, 2.24),
  make_row("  No",             1880, "731 (38.9%)",  "1149 (61.1%)", 2.08, 1.85, 2.36),

  # Line of therapy
  make_header("Bortezomib-based therapy", "0.431"),
  make_row("  Bortezomib",     3040, "1038 (34.1%)", "2002 (65.9%)", 2.01, 1.83, 2.22),
  make_row("  Other",          1045, "429 (41.1%)",  "616 (58.9%)",  2.17, 1.85, 2.54)
)

# --- forest plot 용 데이터 ---
sub_vmodel <- sub_data %>%
  select(Subgroup, `No. of\nPatients`, `Short-term\ngroup`, `Long-term\ngroup`,
         Outcome, `OR (95% CI)`, `P value for\nInteraction`)

# --- forest plot 생성 ---
p_sub <- forest(
  sub_vmodel,
  est       = sub_data$est,
  lower     = sub_data$low,
  upper     = sub_data$hi,
  ci_column = 5,            # "Outcome" 컬럼에 forest plot 그림
  ref_line  = 1,
  xlim      = c(1, 3.5),
  ticks_at  = c(1, 1.5, 2, 3),
  x_trans   = "log",
  theme     = tm
)

# --- 저장 ---
png("Figure_forest_subgroup_v2.png", height = 7, width = 9, units = "in", res = 300)
plot(p_sub)
dev.off()
# Clear environment
rm(list=ls())

## The R script for Žužić et al. manuscript

# Set path to the working directory containing prepared data set

library(tidyverse)
getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = value, into = c("key", "value"), sep = "=", fill = 'right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file) == 0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())

# Load required libraries
library(tidyverse)
library(broom)

getCurrentFileLocation <- function() {
  this_file <- commandArgs() %>%
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = value, into = c("key", "value"), sep = "=", fill = "right") %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file) == 0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

setwd(getCurrentFileLocation())

num_days <- 120

# -------------------------------------------------------------------------
# Helper functions for LaTeX formatting
# -------------------------------------------------------------------------
fmt_num <- function(x, digits = 4) {
  sapply(x, function(v) {
    if (is.na(v) || is.nan(v)) return("-")
    if (v == 0) return("$0$")
    if (abs(v) < 1e-4) {
      s <- formatC(v, format = "e", digits = digits)
      parts <- strsplit(s, "e")[[1]]
      mant <- sub("0+$", "", parts[1])
      mant <- sub("\\.$", "", mant)
      exp_val <- as.integer(parts[2])
      return(paste0("$", mant, " \\times 10^{", exp_val, "}$"))
    } else {
      val <- round(v, digits)
      s <- format(val, scientific = FALSE, trim = TRUE)
      return(paste0("$", s, "$"))
    }
  })
}

fmt_metric <- function(m) {
  paste0("$", m, "$")
}

fmt_model <- function(mod) {
  dplyr::case_match(
    as.character(mod),
    "sin"             ~ "Sine",
    "normal"          ~ "Normal",
    "ar"              ~ "AR",
    "brownian"        ~ "Brownian",
    "logistic"        ~ "Logistic",
    "logistic_linear" ~ "Logistic (Linear)",
    .default = as.character(mod)
  )
}

write_tex_table <- function(lines, filename) {
  writeLines(lines, filename)
  cat("Generated:", filename, "\n")
}

# -------------------------------------------------------------------------
# 1. Table 3: Characteristic classes of recurrence plots (examples_indicators)
# -------------------------------------------------------------------------
models_df <- read_csv(paste0("examples_indicators_", num_days, ".csv"), show_col_types = FALSE)
rqa_metrics <- c("RR", "DET", "NRLINE", "L", "ENTR", "rENTR", "LAM", "TT")

tab3_rows <- map_chr(rqa_metrics, function(m) {
  vals <- models_df[[m]]
  formatted_vals <- if (m == "NRLINE") {
    paste0("$", round(vals, 0), "$")
  } else {
    fmt_num(vals, 4)
  }
  paste(c(fmt_metric(m), formatted_vals), collapse = " & ") %>% paste0(" \\\\")
})

tab3_tex <- c(
  "\\vspace{-3pt}",
  "\\begin{table}[H]",
  "\\caption{RPA results for characteristic classes of recurrence plots.\\label{tab:examples}}",
  "\\begin{tabularx}{\\textwidth}{CCCCCCC}",
  "\\toprule",
  "\\textbf{Function} & \\textbf{Sine} & \\textbf{Normal} & \\textbf{AR} & \\textbf{Brownian} & \\textbf{Logistic} & \\textbf{Logistic} \\\\",
  " & & & & & & \\textbf{(Linear)} \\\\",
  "\\midrule",
  tab3_rows,
  "\\bottomrule",
  "\\end{tabularx}",
  "\\end{table}",
  "\\vspace{-3pt}"
)
write_tex_table(tab3_tex, paste0("table_examples_",num_days,".tex"))

# -------------------------------------------------------------------------
# 2. Table 5: Omnibus Station Comparison (ANOVA & Kruskal-Wallis)
# -------------------------------------------------------------------------
station_comp <- read_csv(paste0("station_comparison_", num_days, ".csv"), show_col_types = FALSE)

tab5_rows <- pmap_chr(station_comp, function(Metric, ANOVA_p_value, Kruskal_stat, Kruskal_p_value) {
  paste(
    fmt_metric(Metric),
    fmt_num(ANOVA_p_value, 4),
    fmt_num(Kruskal_stat, 4),
    fmt_num(Kruskal_p_value, 4),
    sep = " & "
  ) %>% paste0(" \\\\")
})

tab5_tex <- c(
  "\\vspace{-3pt}",
  "\\begin{table}[H]",
  paste0("\\caption{Omnibus unpaired comparison of RQA metrics across the three IGS stations using parametric One-Way ANOVA and non-parametric Kruskal-Wallis tests ($N = ",num_days,"$ days per station).\\label{tab:station_comparison}}"),
  "\\begin{tabularx}{\\textwidth}{CCCC}",
  "\\toprule",
  "\\textbf{Metric} & \\textbf{ANOVA} $\\boldsymbol{p}$\\textbf{-value} & \\textbf{Kruskal stat.} & \\textbf{Kruskal} $\\boldsymbol{p}$\\textbf{-value} \\\\",
  "\\midrule",
  tab5_rows,
  "\\bottomrule",
  "\\end{tabularx}",
  "\\end{table}",
  "\\vspace{-3pt}"
)
write_tex_table(tab5_tex, paste0("table_station_comparison_",num_days,".tex"))

# -------------------------------------------------------------------------
# 3. Table 6: Pairwise Post-Hoc Wilcoxon Tests (BH adjusted)
# -------------------------------------------------------------------------
posthoc_df <- read_csv(paste0("posthoc_results_", num_days, ".csv"), show_col_types = FALSE)

tab6_rows <- pmap_chr(posthoc_df, function(Metric, group1, group2, p.value) {
  paste(
    fmt_metric(Metric),
    group1,
    group2,
    fmt_num(p.value, 4),
    sep = " & "
  ) %>% paste0(" \\\\")
})

tab6_tex <- c(
  "\\vspace{-3pt}",
  "\\begin{table}[H]",
  "\\caption{Pairwise post-hoc comparisons between stations for each RQA metric using Wilcoxon rank-sum tests with Benjamini--Hochberg (BH) false discovery rate adjustment.\\label{tab:posthoc_results}}",
  "\\begin{tabularx}{\\textwidth}{CCCC}",
  "\\toprule",
  "\\textbf{Metric} & \\textbf{Group 1} & \\textbf{Group 2} & \\textbf{Adjusted} $\\boldsymbol{p}$\\textbf{-value} \\\\",
  "\\midrule",
  tab6_rows,
  "\\bottomrule",
  "\\end{tabularx}",
  "\\end{table}",
  "\\vspace{-3pt}"
)
write_tex_table(tab6_tex, paste0("table_posthoc_results_",num_days,".tex"))

# -------------------------------------------------------------------------
# 4. Table 7: Paired Friedman Rank Sum Test Results
# -------------------------------------------------------------------------
friedman_df <- read_csv(paste0("friedman_results_", num_days, ".csv"), show_col_types = FALSE)

tab7_rows <- pmap_chr(friedman_df, function(Metric, Friedman_Chi, p_value) {
  paste(
    fmt_metric(Metric),
    fmt_num(Friedman_Chi, 4),
    fmt_num(p_value, 4),
    sep = " & "
  ) %>% paste0(" \\\\")
})

tab7_tex <- c(
  "\\vspace{-3pt}",
  "\\begin{table}[H]",
  paste0("\\caption{Non-parametric Friedman rank-sum test results comparing the three stations blocked by Day of Year (DOY, $N = ",num_days,"$ complete blocks, $\\text{df} = 2$).\\label{tab:friedman_results}}"),
  "\\begin{tabularx}{\\textwidth}{CCC}",
  "\\toprule",
  "\\textbf{Metric} & \\textbf{Friedman} $\\boldsymbol{\\chi^2}$ & $\\boldsymbol{p}$\\textbf{-value} \\\\",
  "\\midrule",
  tab7_rows,
  "\\bottomrule",
  "\\end{tabularx}",
  "\\end{table}",
  "\\vspace{-3pt}"
)
write_tex_table(tab7_tex, paste0("table_friedman_results_",num_days,".tex"))

# -------------------------------------------------------------------------
# 5. Table 8: Multivariate MANOVA (Pillai's Trace)
# -------------------------------------------------------------------------
matera  <- read_csv(paste0("afullframe_Matera_all_new_", num_days, ".csv"), na = c("NaN", "NA"), show_col_types = FALSE)
iqaluit <- read_csv(paste0("afullframe_Iqaluit_all_new_", num_days, ".csv"), na = c("NaN", "NA"), show_col_types = FALSE)
darwin  <- read_csv(paste0("afullframe_Darwin_all_new_", num_days, ".csv"), na = c("NaN", "NA"), show_col_types = FALSE)

stations_df <- bind_rows(matera, iqaluit, darwin) %>%
  mutate(name = as.factor(name))

manova_fit <- manova(cbind(RR, DET, NRLINE, L, ENTR, rENTR, LAM, TT) ~ name, data = stations_df)
manova_sum <- summary(manova_fit, test = "Pillai")$stats

df_name   <- manova_sum["name", "Df"]
pillai_v  <- round(manova_sum["name", "Pillai"], 4)
approx_f  <- round(manova_sum["name", "approx F"], 2)
num_df    <- manova_sum["name", "num Df"]
den_df    <- manova_sum["name", "den Df"]
df_resid  <- manova_sum["Residuals", "Df"]

tab8_tex <- c(
  "\\vspace{-3pt}",
  "\\begin{table}[H]",
  "\\caption{Multivariate Analysis of Variance (MANOVA) using Pillai's trace across all eight RQA metrics simultaneously across the three stations.\\label{tab:manova_fit}}",
  "\\begin{tabularx}{\\textwidth}{CCCCCCCC}",
  "\\toprule",
  "\\textbf{Effect} & \\textbf{Df} & \\textbf{Pillai's Trace} & \\textbf{Approx.} $\\boldsymbol{F}$ & \\textbf{Num. Df} & \\textbf{Den. Df} & $\\boldsymbol{p}$\\textbf{-value} & \\textbf{Residuals} \\\\",
  "\\midrule",
  paste0("Station (name) & $", df_name, "$ & $", pillai_v, "$ & $", approx_f, "$ & $", num_df, "$ & $", den_df, "$ & $< 2.2 \\times 10^{-16}$ & $", df_resid, "$ \\\\"),
  "\\bottomrule",
  "\\end{tabularx}",
  "\\end{table}",
  "\\vspace{-3pt}"
)
write_tex_table(tab8_tex, paste0("table_manova_fit_",num_days,".tex"))

# -------------------------------------------------------------------------
# 6. Table 9: One-Sample Station vs. Benchmark Models (Wilcoxon p_adj > 0.05)
# -------------------------------------------------------------------------
vs_models_df1 <- read_csv(paste0("station_vs_models_", num_days, ".csv"), show_col_types = FALSE)
vs_models_df <- vs_models_df1[vs_models_df1$`Wilcoxon_p_adj` > 0.05,]

tab9_rows <- pmap_chr(vs_models_df, function(Station, Model, Metric, Station_Median, Model_Value, Wilcoxon_p, T_test_p, Wilcoxon_p_adj) {
  paste(
    Station,
    fmt_model(Model),
    fmt_metric(Metric),
    fmt_num(Station_Median, 4),
    fmt_num(Model_Value, 4),
    fmt_num(Wilcoxon_p, 4),
    fmt_num(T_test_p, 4),
    fmt_num(Wilcoxon_p_adj, 4),
    sep = " & "
  ) %>% paste0(" \\\\")
})

tab9_tex <- c(
  "\\vspace{-3pt}",
  "\\begin{table}[H]",
  "\\caption{Univariate one-sample comparisons between station RQA distributions and theoretical benchmark models where the BH-adjusted Wilcoxon signed-rank $p$-value exceeds $0.05$ (indicating no statistically significant difference).\\label{tab:station_vs_models}}",
  "\\begin{tabularx}{\\textwidth}{CCCCCCCC}",
  "\\toprule",
  "\\textbf{Station} & \\textbf{Model} & \\textbf{Metric} & \\textbf{Station median} & \\textbf{Model value} & \\textbf{Wilcoxon} $\\boldsymbol{p}$\\textbf{-value} & $\\boldsymbol{t}$\\textbf{-test} $\\boldsymbol{p}$\\textbf{-value} & \\textbf{Wilcoxon} $\\boldsymbol{p}$\\textbf{-value} \\textbf{adj.} \\\\",
  "\\midrule",
  tab9_rows,
  "\\bottomrule",
  "\\end{tabularx}",
  "\\end{table}",
  "\\vspace{-3pt}"
)
write_tex_table(tab9_tex, paste0("table_station_vs_models_",num_days,".tex"))

# -------------------------------------------------------------------------
# 7. Table 10: Multivariate Mahalanobis Distance to Models
# -------------------------------------------------------------------------
mahal_df <- read_csv(paste0("distance_to_models_", num_days, ".csv"), show_col_types = FALSE)

tab10_rows <- pmap_chr(mahal_df, function(Station, Model, Mahalanobis_Dist, ChiSq_p_value) {
  paste(
    Station,
    fmt_model(Model),
    fmt_num(Mahalanobis_Dist, 4),
    fmt_num(ChiSq_p_value, 4),
    sep = " & "
  ) %>% paste0(" \\\\")
})

tab10_tex <- c(
  "\\vspace{-3pt}",
  "\\begin{table}[H]",
  "\\caption{Multivariate Mahalanobis distances ($D$) and corresponding $\\chi^2$ $p$-values ($\\text{df} = 6$) between each station's RQA distribution ($RR$, $DET$, $L$, $ENTR$, $rENTR$, $LAM$) and the six canonical benchmark models, ordered from closest to furthest.\\label{tab:distance_to_models}}",
  "\\begin{tabularx}{\\textwidth}{CCCC}",
  "\\toprule",
  "\\textbf{Station} & \\textbf{Model} & \\textbf{Mahalanobis dist.} & $\\boldsymbol{\\chi^2}$ $\\boldsymbol{p}$\\textbf{-value} \\\\",
  "\\midrule",
  tab10_rows,
  "\\bottomrule",
  "\\end{tabularx}",
  "\\end{table}",
  "\\vspace{-3pt}"
)
write_tex_table(tab10_tex, paste0("table_distance_to_models_",num_days,".tex"))

# -------------------------------------------------------------------------
# 8. Table 11 & Table 12: Effect Sizes and Station Medians (IQR)
# -------------------------------------------------------------------------
pub_df <- read_csv(paste0("publication_summary_", num_days, ".csv"), show_col_types = FALSE)

tab11_rows <- pmap_chr(pub_df, function(Metric, Darwin, Iqaluit, Matera, Kruskal_stat, Kruskal_Epsilon2, Kruskal_p_value, Friedman_Chi, Friedman_Kendall_W) {
  paste(
    fmt_metric(Metric),
    fmt_num(Kruskal_stat, 4),
    fmt_num(Kruskal_Epsilon2, 4),
    fmt_num(Kruskal_p_value, 4),
    fmt_num(Friedman_Chi, 4),
    fmt_num(Friedman_Kendall_W, 4),
    sep = " & "
  ) %>% paste0(" \\\\")
})

tab11_tex <- c(
  "\\vspace{-3pt}",
  "\\begin{table}[H]",
  "\\caption{Non-parametric omnibus test statistics and bounded effect sizes: Kruskal-Wallis epsilon-squared ($\\varepsilon^2$) and Friedman Kendall's coefficient of concordance ($W$) across all eight RQA metrics.\\label{tab:publication_summary}}",
  "\\begin{tabularx}{\\textwidth}{CCCCCC}",
  "\\toprule",
  "\\textbf{Metric} & \\textbf{Kruskal stat.} & \\textbf{Kruskal} $\\boldsymbol{\\varepsilon^2}$ & \\textbf{Kruskal} $\\boldsymbol{p}$\\textbf{-value} & \\textbf{Friedman} $\\boldsymbol{\\chi^2}$ & \\textbf{Friedman Kendall} $\\boldsymbol{W}$ \\\\",
  "\\midrule",
  tab11_rows,
  "\\bottomrule",
  "\\end{tabularx}",
  "\\end{table}",
  "\\vspace{-3pt}"
)
write_tex_table(tab11_tex, paste0("table_publication_summary_",num_days,".tex"))

format_med_iqr <- function(s) {
  parts <- strsplit(s, " \\(")[[1]]
  med <- parts[1]
  iqr <- sub("\\)", "", parts[2])
  paste0("$", med, "$ ($", iqr, "$)")
}

tab12_rows <- pmap_chr(pub_df, function(Metric, Darwin, Iqaluit, Matera, ...) {
  paste(
    fmt_metric(Metric),
    format_med_iqr(Darwin),
    format_med_iqr(Iqaluit),
    format_med_iqr(Matera),
    sep = " & "
  ) %>% paste0(" \\\\")
})

tab12_tex <- c(
  "\\vspace{-3pt}",
  "\\begin{table}[H]",
  paste0("\\caption{Station median and Interquartile Range (IQR, in parentheses) for each RQA metric across the $",num_days,"$ analysed days in 2014.\\label{tab:IQR}}"),
  "\\begin{tabularx}{\\textwidth}{CCCC}",
  "\\toprule",
  "\\textbf{Metric} & \\textbf{Darwin} & \\textbf{Iqaluit} & \\textbf{Matera} \\\\",
  "\\midrule",
  tab12_rows,
  "\\bottomrule",
  "\\end{tabularx}",
  "\\end{table}",
  "\\vspace{-3pt}"
)
write_tex_table(tab12_tex, paste0("table_IQR_",num_days,".tex"))

# -------------------------------------------------------------------------
# 9. Table 13: Standardised Euclidean (z-score) Distance to Models
# -------------------------------------------------------------------------
z_df <- read_csv(paste0("z_distance_to_models_", num_days, ".csv"), show_col_types = FALSE)

tab13_rows <- pmap_chr(z_df, function(Station, Model, Z_Euclidean_Dist) {
  paste(
    Station,
    fmt_model(Model),
    fmt_num(Z_Euclidean_Dist, 4),
    sep = " & "
  ) %>% paste0(" \\\\")
})

tab13_tex <- c(
  "\\vspace{-3pt}",
  "\\begin{table}[H]",
  "\\caption{Collinearity-robust standardised Euclidean ($z$-score) distances between each station's median RQA profile and the six canonical benchmark models.\\label{tab:z_distance_to_models}}",
  "\\begin{tabularx}{\\textwidth}{CCC}",
  "\\toprule",
  "\\textbf{Station} & \\textbf{Model} & $\\boldsymbol{z}$\\textbf{-score Euclidean dist.} \\\\",
  "\\midrule",
  tab13_rows,
  "\\bottomrule",
  "\\end{tabularx}",
  "\\end{table}",
  "\\vspace{-3pt}"
)
write_tex_table(tab13_tex, paste0("table_z_distance_to_models_",num_days,".tex"))

# -------------------------------------------------------------------------
# 10. Optional Table: Hourly-Mean RQA Summary (from RPA_plots_total.R)
# -------------------------------------------------------------------------
mean_file <- paste0("afullframe_mean_new_", num_days, ".csv")
if (file.exists(mean_file)) {
  mean_df <- read_csv(mean_file, show_col_types = FALSE)
  tab_mean_rows <- map_chr(rqa_metrics, function(m) {
    vals <- mean_df[[m]]
    formatted_vals <- if (m == "NRLINE") {
      paste0("$", round(vals, 0), "$")
    } else {
      fmt_num(vals, 4)
    }
    paste(c(fmt_metric(m), formatted_vals), collapse = " & ") %>% paste0(" \\\\")
  })
  
  tab_mean_tex <- c(
    "\\vspace{-3pt}",
    "\\begin{table}[H]",
    "\\caption{RQA metrics computed on the aggregated hourly-mean horizontal positioning error time series across the 100-day observation period.\\label{tab:hourly_mean_rqa}}",
    paste0("\\begin{tabularx}{\\textwidth}{C", paste(rep("C", nrow(mean_df)), collapse = ""), "}"),
    "\\toprule",
    paste0("\\textbf{Metric} & ", paste0("\\textbf{", mean_df$name, "}", collapse = " & "), " \\\\"),
    "\\midrule",
    tab_mean_rows,
    "\\bottomrule",
    "\\end{tabularx}",
    "\\end{table}",
    "\\vspace{-3pt}"
  )
  write_tex_table(tab_mean_tex, paste0("table_hourly_mean_rqa_",num_days,".tex"))
}

# Combine all generated tables into a single master file for easy copy-pasting
all_tables <- c(
  "% --- Master file of auto-generated LaTeX tables ---",
  tab3_tex, "",
  tab5_tex, "",
  tab6_tex, "",
  tab7_tex, "",
  tab8_tex, "",
  tab9_tex, "",
  tab10_tex, "",
  tab11_tex, "",
  tab12_tex, "",
  tab13_tex
)
write_tex_table(all_tables, paste0("all_manuscript_tables_",num_days,".tex"))
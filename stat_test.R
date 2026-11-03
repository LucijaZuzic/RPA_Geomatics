# Clear environment
rm(list=ls())

## The R script for Žužić et al. manuscript

# Set path to the working directory containing prepared data set

library(tidyverse)
filter <- dplyr::filter
select <- dplyr::select
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
library(broom)
library(ggplot2)

num_days <- 100
# 1. Load and combine the three station CSVs
matera  <- read.csv(paste0("afullframe_Matera_all_new_",num_days,".csv"), na.strings = c("NaN", "NA"))
iqaluit <- read.csv(paste0("afullframe_Iqaluit_all_new_",num_days,".csv"), na.strings = c("NaN", "NA"))
darwin  <- read.csv(paste0("afullframe_Darwin_all_new_",num_days,".csv"), na.strings = c("NaN", "NA"))

matera <- matera[matera$`doy` != 39,]
matera <- matera[matera$`doy` != 47,]
iqaluit <- iqaluit[iqaluit$`doy` != 39,]
iqaluit <- iqaluit[iqaluit$`doy` != 47,]
darwin <- darwin[darwin$`doy` != 39,]
darwin <- darwin[darwin$`doy` != 47,]

stations_df <- bind_rows(matera, iqaluit, darwin) %>%
  mutate(name = as.factor(name))

# 2. Load the synthetic reference models CSV
models_df <- read.csv(paste0("examples_indicators_",num_days,".csv"), na.strings = c("NaN", "NA"))

# Define the RQA metric columns
rqa_metrics <- c("RR", "DET", "NRLINE", "L", "ENTR", "rENTR", "LAM", "TT")

# --- A. Omnibus Tests (ANOVA & Kruskal-Wallis across all RQA metrics) ---
station_comparison <- map_dfr(rqa_metrics, function(metric) {
  formula_obj <- as.formula(paste(metric, "~ name"))
  
  # Parametric ANOVA
  aov_res <- aov(formula_obj, data = stations_df)
  aov_p   <- summary(aov_res)[[1]][["Pr(>F)"]][1]
  
  # Non-parametric Kruskal-Wallis
  kw_res  <- kruskal.test(formula_obj, data = stations_df)
  
  tibble(
    Metric          = metric,
    ANOVA_p_value   = aov_p,
    Kruskal_stat    = unname(kw_res$statistic),
    Kruskal_p_value = kw_res$p.value
  )
})

write_csv(data.frame(station_comparison), paste0("station_comparison_",num_days,".csv"))

# --- B. Pairwise Post-Hoc Tests (Wilcoxon with BH adjustment) ---
posthoc_results <- map_dfr(rqa_metrics, function(metric) {
  pw <- pairwise.wilcox.test(
    x = stations_df[[metric]],
    g = stations_df$name,
    p.adjust.method = "BH",
    exact = FALSE
  )
  tidy(pw) %>% mutate(Metric = metric, .before = 1)
})

write_csv(data.frame(posthoc_results), paste0("posthoc_results_",num_days,".csv"))

friedman_results <- map_dfr(rqa_metrics, function(metric) {
  # Keep only complete doy blocks across all 3 stations
  clean_block <- stations_df %>%
    select(doy, name, all_of(metric)) %>%
    pivot_wider(names_from = name, values_from = all_of(metric)) %>%
    drop_na() %>%
    pivot_longer(-doy, names_to = "name", values_to = "value")
  
  fr <- friedman.test(value ~ name | doy, data = clean_block)
  
  tibble(
    Metric       = metric,
    Friedman_Chi = unname(fr$statistic),
    p_value      = fr$p.value
  )
})

write_csv(data.frame(friedman_results), paste0("friedman_results_",num_days,".csv"))

manova_fit <- manova(
  cbind(RR, DET, NRLINE, L, ENTR, rENTR, LAM, TT) ~ name, 
  data = stations_df
)

# Pillai's trace is most robust to deviations from multivariate normality
print(summary(manova_fit, test = "Pillai"))

# Exclude NRLINE due to time-series length scaling differences
scale_invariant_metrics <- c("RR", "DET", "L", "ENTR", "rENTR", "LAM", "TT")

# Compare every station against every synthetic model for each metric
station_vs_models <- expand_grid(
  station = unique(stations_df$name),
  model   = models_df$name,
  metric  = scale_invariant_metrics
) %>%
  pmap_dfr(function(station, model, metric) {
    # Extract station sample and model reference value
    station_vals <- stations_df %>% filter(name == station) %>% pull(all_of(metric)) %>% na.omit()
    model_val    <- models_df %>% filter(name == model) %>% pull(all_of(metric))
    
    # Skip if the model value is NaN/NA (e.g., TT for 'logistic')
    if (is.na(model_val) || length(station_vals) < 2) {
      return(tibble(
        Station = station, Model = model, Metric = metric,
        Station_Median = median(station_vals, na.rm = TRUE),
        Model_Value = NA_real_, Wilcoxon_p = NA_real_, T_test_p = NA_real_
      ))
    }
    
    # One-sample Wilcoxon and One-sample t-test against mu = model_val
    w_test <- suppressWarnings(wilcox.test(station_vals, mu = model_val, exact = FALSE))
    t_test <- t.test(station_vals, mu = model_val)
    
    tibble(
      Station        = as.character(station),
      Model          = as.character(model),
      Metric         = metric,
      Station_Median = median(station_vals),
      Model_Value    = model_val,
      Wilcoxon_p     = w_test$p.value,
      T_test_p       = t_test$p.value
    )
  }) %>%
  mutate(Wilcoxon_p_adj = p.adjust(Wilcoxon_p, method = "BH"))

# View cases where a station is NOT significantly different from a benchmark model (p greater than 0.05)
station_vs_models %>%
  filter(Wilcoxon_p_adj > 0.05) %>%
  print()
write_csv(data.frame(station_vs_models), paste0("station_vs_models_",num_days,".csv"))

# Use metrics without NaN across all models (exclude NRLINE for scale and TT due to logistic NaN)
common_metrics <- c("RR", "DET", "L", "ENTR", "rENTR", "LAM")

distance_to_models <- map_dfr(unique(stations_df$name), function(st) {
  st_mat <- stations_df %>%
    filter(name == st) %>%
    select(all_of(common_metrics)) %>%
    drop_na() %>%
    as.matrix()
  
  st_mean <- colMeans(st_mat)
  st_cov  <- cov(st_mat)
  
  # Calculate Mahalanobis distance and Chi-square p-value for each synthetic model
  map_dfr(1:nrow(models_df), function(i) {
    mod_name <- models_df$name[i]
    mod_vec  <- as.numeric(models_df[i, common_metrics])
    
    # Mahalanobis squared distance D^2
    d2 <- mahalanobis(x = mod_vec, center = st_mean, cov = st_cov)
    # p-value under Chi-square with df = number of metrics
    p_val <- pchisq(d2, df = length(common_metrics), lower.tail = FALSE)
    
    tibble(
      Station            = as.character(st),
      Model              = mod_name,
      Mahalanobis_Dist   = sqrt(d2),
      ChiSq_p_value      = p_val
    )
  })
}) %>%
  arrange(Station, Mahalanobis_Dist)

# Closest synthetic model for each station appears first
write_csv(data.frame(distance_to_models), paste0("distance_to_models_",num_days,".csv"))

# Build a publication-ready table: Median (IQR) per station + Effect Sizes
n_total  <- nrow(stations_df)
n_blocks <- length(unique(stations_df$doy))

publication_summary <- stations_df %>%
  tidyr::pivot_longer(cols = dplyr::all_of(rqa_metrics), names_to = "Metric", values_to = "val") %>%
  dplyr::group_by(Metric, name) %>%
  dplyr::summarise(
    Median_IQR = sprintf("%.2f (%.2f)", median(val, na.rm = TRUE), IQR(val, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = name, values_from = Median_IQR) %>%
  dplyr::left_join(station_comparison, by = "Metric") %>%
  dplyr::left_join(friedman_results, by = "Metric") %>%
  dplyr::mutate(
    # Epsilon-squared effect size for Kruskal-Wallis (0 to 1)
    Kruskal_Epsilon2 = Kruskal_stat / ((n_total^2 - 1) / (n_total + 1)),
    # Kendall's W effect size for Friedman test (0 to 1, where k = 3 stations)
    Friedman_Kendall_W = Friedman_Chi / (n_blocks * (3 - 1))
  ) %>%
  dplyr::select(
    Metric, Darwin, Iqaluit, Matera, 
    Kruskal_stat, Kruskal_Epsilon2, Kruskal_p_value,
    Friedman_Chi, Friedman_Kendall_W
  )

write_csv(data.frame(publication_summary), paste0("publication_summary_",num_days,".csv"))

# Combine stations and models to standardize each metric on a common scale
common_metrics <- c("RR", "DET", "L", "ENTR", "rENTR", "LAM")

# Compute global mean and SD across stations for scaling
global_means <- colMeans(stations_df[, common_metrics], na.rm = TRUE)
global_sds   <- apply(stations_df[, common_metrics], 2, sd, na.rm = TRUE)

z_distance_to_models <- purrr::map_dfr(unique(stations_df$name), function(st) {
  st_med <- stations_df %>%
    dplyr::filter(name == st) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(common_metrics), ~ median(.x, na.rm = TRUE))) %>%
    as.numeric()
  
  st_z <- (st_med - global_means) / global_sds
  
  purrr::map_dfr(1:nrow(models_df), function(i) {
    mod_name <- models_df$name[i]
    mod_vec  <- as.numeric(models_df[i, common_metrics])
    mod_z    <- (mod_vec - global_means) / global_sds
    
    tibble::tibble(
      Station        = as.character(st),
      Model          = mod_name,
      Z_Euclidean_Dist = sqrt(sum((st_z - mod_z)^2))
    )
  })
}) %>%
  dplyr::arrange(Station, Z_Euclidean_Dist)

write_csv(data.frame(z_distance_to_models), paste0("z_distance_to_models_",num_days,".csv"))

# 1. Fit PCA on the 3 stations using scale-invariant metrics without NaN
pca_metrics <- c("RR", "DET", "L", "ENTR", "rENTR", "LAM")
stations_clean <- stations_df %>% tidyr::drop_na(dplyr::all_of(pca_metrics))

pca_fit <- prcomp(stations_clean[, pca_metrics], center = TRUE, scale. = TRUE)

translate_name <- function(names_list) {
  names_list_new <- c()
  for (name in names_list) {
    if (name == "sin") {
      names_list_new <- c(names_list_new, "Sine")
    }
    if (name == "normal") {
      names_list_new <- c(names_list_new, "White\n noise")
    }
    if (name == "ar") {
      names_list_new <- c(names_list_new, "Auto-\n regressive")
    }
    if (name == "brownian") {
      names_list_new <- c(names_list_new, "Brownian")
    }
    if (name == "logistic") {
      names_list_new <- c(names_list_new, "Logistic\n map")
    }
    if (name == "logistic_linear") {
      names_list_new <- c(names_list_new, "Logistic map with\n a linear trend")
    }
  }
  return(names_list_new)
}
                       
# 2. Extract station PCA coordinates
station_pca_coords <- as.data.frame(pca_fit$x[, 1:2]) %>%
  dplyr::mutate(name = stations_clean$name, Type = "Station")

# 3. Project the 6 synthetic models into the exact same PCA space
model_pca_coords <- predict(pca_fit, newdata = models_df[, pca_metrics])[, 1:2] %>%
  as.data.frame() %>%
  dplyr::mutate(name = models_df$name, Type = "Benchmark Model")

# 4. Plot Stations (clouds + ellipses) and Benchmark Models (labeled diamonds)
p <- ggplot(station_pca_coords, aes(x = PC1, y = PC2, color = name)) +
  geom_point(alpha = 0.5, size = 2) +
  stat_ellipse(level = 0.95, linewidth = 1) +
  geom_point(
    data = model_pca_coords,
    aes(x = PC1, y = PC2),
    color = "black", fill = "red", shape = 23, size = 4
  ) +
  geom_text(
    data = model_pca_coords,
    aes(x = PC1, y = PC2, label = translate_name(name)),
    color = "black", fontface = "bold", vjust = -1, size = 3.5
  ) +
  labs(
    title = "RQA Dynamical Space: Stations vs. Theoretical Models",
    subtitle = "95% confidence ellipses for stations; diamonds mark theoretical benchmarks",
    x = paste0("PC1 (", round(summary(pca_fit)$importance[2, 1] * 100, 1), "% variance)"),
    y = paste0("PC2 (", round(summary(pca_fit)$importance[2, 2] * 100, 1), "% variance)"),
    color = "Station"
  ) +
  theme_minimal()
ggsave(paste0("final_plot_",num_days,".png"), plot = p, width = 7.25, height = 7.25, units = "in", dpi = 300)
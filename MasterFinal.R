# ============================================================
# MASTERFINAL.R — Full Replication Package
# ============================================================
# Runs in sequence:
#   1. Synthetic Control Method (SCM)       [Master.R]
#   2. DID & Cross-Sectional Regressions    [RegressionTests.r]
#   3. Dube & Zipperer (2015) Extension     [Extension.r]
#   4. Tables & Figures                     [Figures.r]
#
# Before running: set Directory to your "Created Data" folder path.
# ============================================================


# ============================================================
# LIBRARIES
# ============================================================

 install.packages(c("readxl","dplyr","tidyr","stringr","janitor","openxlsx",
                    "ggplot2","Synth","knitr","kableExtra","modelsummary","zoo"))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(openxlsx)
library(ggplot2)
library(Synth)
library(knitr)
library(kableExtra)
library(modelsummary)
library(zoo)


# ============================================================
# DIRECTORY & SHARED CONSTANTS
# ============================================================

Directory <- "C:\\Users\\LAEG\\OneDrive - Trinity College Dublin\\Senior Year\\Capstone\\Replication Package"

data_path <- paste0(Directory, "\\FinalData.xlsx")

Bailout_Regions <- c(
  "Ireland","Greece","Portugal","Spain","Hungary","France",
  "Austria","Latvia","Sweden","Germany","Iceland","Luxembourg",
  "Netherlands","Belgium","United Kingdom", "Denmark"
)


# ============================================================
# INITIAL DATA LOAD
# ============================================================

df5      <- read.xlsx(data_path, sheet = "Data5")
controls <- read.xlsx(data_path, sheet = "Controls") %>%
  mutate(bailout_expenditure_pct_gdp = as.numeric(bailout_expenditure_pct_gdp))


# ============================================================
# SECTION 1: SYNTHETIC CONTROL METHOD (SCM)
# ============================================================

df5 <- df5 %>%
  group_by(country_name) %>%
  mutate(
    pre_mean  = mean(fdi_inflows_bop_usd[year >= 1995 & year <= 2010], na.rm = TRUE),
    pre_sd    = sd(fdi_inflows_bop_usd[year >= 1995 & year <= 2010],   na.rm = TRUE),
    fdi_usd_z = (fdi_inflows_bop_usd - pre_mean) / pre_sd
  ) %>%
  select(-pre_mean, -pre_sd) %>%
  ungroup() %>%
  arrange(country_name, year) %>%
  group_by(country_name) %>%
  mutate(
    fdi_usd_ma3   = zoo::rollmean(fdi_usd_z, k = 3, fill = NA, align = "right"),
    Lag_fdi_usd_z = lag(fdi_usd_z),
    Lag_fdi_pct   = lag(fdi_inflows_pct_gdp)
  ) %>%
  ungroup() %>%
  arrange(country_name, year) %>%
  group_by(country_name) %>%
  mutate(Lag_fdi_ma3  = lag(fdi_usd_ma3)) %>%
  mutate(Lag2_fdi_ma3 = lag(fdi_usd_ma3, 2)) %>%
  mutate(Lag3_fdi_ma3 = lag(fdi_usd_ma3, 3)) %>%
  ungroup()

normal_predictors       <- c("gross_national_expenditure_pct_gdp", "gdp_growth_annual_pct", "trade_pct_gdp")
special_predictors_vars <- c("fdi_usd_ma3")
dep_var                 <- "fdi_usd_ma3"
predictors              <- normal_predictors

df5 <- df5 %>%
  mutate(
    unit_id = as.numeric(factor(country_name)),
    time_id = year
  )

final_results_df <- data.frame()
all_weights_mc   <- data.frame()
region_store     <- list()

# Export df5_sub with only Bailout_Regions to Excel as "Country_Data"
df5_sub <- df5 %>% filter(country_name %in% Bailout_Regions)
df5_sub <- df5_sub %>%
  left_join(
    controls %>% filter(year == 2009) %>%
      select(region, bailout_expenditure_pct_gdp, bailout_net_cost_pct_gdp),
    by = c("country_name" = "region")
  ) %>%
  rename(bailout_expenditure_pct_gdp_2009 = bailout_expenditure_pct_gdp,
         bailout_net_cost_pct_gdp_2009    = bailout_net_cost_pct_gdp)

write.xlsx(df5_sub, paste0(Directory, "\\Country_Data.xlsx"))


# ---- Loop over treated countries ----

for (region in Bailout_Regions) {

  cat("========", region, "========\n")

  region_subset <- df5 %>%
    filter(country_name == region | !(country_name %in% Bailout_Regions)) %>%
    filter(!is.na(country_name))

  region_subset <- region_subset %>%
    filter(!is.na(.data[[dep_var]])) %>%
    filter(if_all(all_of(c(predictors, special_predictors_vars)), ~ !is.na(.)))

  treated_years        <- region_subset %>% filter(country_name == region) %>% pull(year) %>% sort()
  pre_treatment_period <- sort(treated_years[treated_years <= 2010])

  if (length(pre_treatment_period) == 0) {
    cat("  -> Skipping", region, ": no pre-treatment years available\n"); next
  }

  optimize_years <- pre_treatment_period[pre_treatment_period >= (max(pre_treatment_period) - 15)]
  plot_years     <- treated_years[treated_years >= 1995 & treated_years <= 2020]

  if (length(optimize_years) == 0 || length(plot_years) == 0) {
    cat("  -> Skipping", region, ": optimization/plot window has no available years\n"); next
  }

  required_years <- sort(unique(c(optimize_years, plot_years)))

  complete_controls <- region_subset %>%
    filter(country_name != region) %>%
    group_by(country_name) %>%
    summarise(has_all = all(required_years %in% year), .groups = "drop") %>%
    filter(has_all) %>%
    pull(country_name)

  region_subset <- region_subset %>%
    filter(country_name == region | country_name %in% complete_controls) %>%
    filter(year %in% required_years)

  treated_id  <- region_subset %>% filter(country_name == region) %>% pull(unit_id) %>% unique()
  control_ids <- region_subset %>% filter(country_name != region) %>% pull(unit_id) %>% unique()

  year_blocks        <- split(optimize_years, ceiling(seq_along(optimize_years) / 3))
  special_preds_list <- do.call(c, lapply(special_predictors_vars, function(v) {
    lapply(year_blocks, function(blk) list(v, blk, "mean"))
  }))

  synth_data <- dataprep(
    foo                   = as.data.frame(region_subset),
    predictors            = predictors,
    special.predictors    = special_preds_list,
    dependent             = dep_var,
    unit.variable         = "unit_id",
    time.variable         = "time_id",
    treatment.identifier  = treated_id,
    controls.identifier   = control_ids,
    time.predictors.prior = optimize_years,
    time.optimize.ssr     = optimize_years,
    time.plot             = plot_years
  )

  synth_model <- synth(synth_data)

  weights <- data.frame(
    region     = region,
    country    = region_subset %>%
      distinct(unit_id, country_name) %>%
      arrange(unit_id) %>%
      filter(unit_id %in% control_ids) %>%
      pull(country_name),
    weight_pct = round(as.numeric(synth_model$solution.w) * 100, 2)
  )
  print(weights)
  all_weights_mc <- bind_rows(all_weights_mc, weights)

  results_df <- data.frame(
    region    = region,
    year      = plot_years,
    actual    = as.vector(synth_data$Y1plot),
    synthetic = as.vector(synth_data$Y0plot %*% synth_model$solution.w)
  ) %>%
    mutate(gap = actual - synthetic)

  final_results_df <- bind_rows(final_results_df, results_df)

  region_store[[region]] <- list(
    region_subset  = region_subset,
    optimize_years = optimize_years,
    plot_years     = plot_years,
    solution_w     = synth_model$solution.w,
    control_ids    = control_ids,
    results_df     = results_df
  )
}

final_results_df %>%
  pivot_longer(cols = c("actual", "synthetic"), names_to = "series", values_to = "value") %>%
  ggplot(aes(x = year, y = value, color = series)) +
  annotate("rect", xmin = 2010, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.4) +
  geom_line(size = 1) +
  facet_wrap(~ region, scales = "free_y") +
  labs(title = paste("Actual vs Synthetic FDI (", dep_var, ")"),
       x = "Year", y = paste(dep_var, "(Z-score)")) +
  theme_minimal() +
  theme(legend.title = element_blank())
ggsave(paste0(Directory, "\\Country_Actual_vs_Synthetic_Main.png"), width = 12, height = 8)

final_results_df %>%
  ggplot(aes(x = year, y = gap)) +
  annotate("rect", xmin = 2010, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.4) +
  geom_line(size = 1, color = "blue") +
  facet_wrap(~ region, scales = "free_y") +
  labs(title = paste("Gap Between Actual and Synthetic FDI (", dep_var, ")"),
       x = "Year", y = paste("Gap (Actual - Synthetic) -", dep_var)) +
  theme_minimal()
ggsave(paste0(Directory, "\\Country_Gap_Plot.png"), width = 12, height = 8)

write.xlsx(final_results_df, paste0(Directory, "\\Main_Country_results.xlsx"))
write.xlsx(all_weights_mc,   paste0(Directory, "\\Country_Weights.xlsx"))


# ---- Placebo Tests (In-Space) ----

placebo_results_df <- data.frame()

for (region in names(region_store)) {

  cat("======== PLACEBO:", region, "========\n")

  rs <- region_store[[region]]

  ctrl_country_map <- rs$region_subset %>%
    distinct(unit_id, country_name) %>%
    arrange(unit_id) %>%
    filter(unit_id %in% rs$control_ids)

  all_donors <- ctrl_country_map$country_name
  cat("  All donors:", paste(all_donors, collapse = ", "), "\n")

  placebo_pool <- rs$region_subset %>% filter(country_name != region)

  for (placebo_country in all_donors) {

    cat("  -> Placebo unit:", placebo_country, "\n")

    placebo_id       <- placebo_pool %>% filter(country_name == placebo_country) %>% pull(unit_id) %>% unique()
    placebo_ctrl_ids <- placebo_pool %>% filter(country_name != placebo_country) %>% pull(unit_id) %>% unique()

    if (length(placebo_ctrl_ids) < 2) { cat("     Skipping: fewer than 2 controls available\n"); next }

    year_blocks_p        <- split(rs$optimize_years, ceiling(seq_along(rs$optimize_years) / 3))
    special_preds_list_p <- do.call(c, lapply(special_predictors_vars, function(v) {
      lapply(year_blocks_p, function(blk) list(v, blk, "mean"))
    }))

    tryCatch({
      placebo_data <- dataprep(
        foo                   = as.data.frame(placebo_pool),
        predictors            = normal_predictors,
        special.predictors    = special_preds_list_p,
        dependent             = dep_var,
        unit.variable         = "unit_id",
        time.variable         = "time_id",
        treatment.identifier  = placebo_id,
        controls.identifier   = placebo_ctrl_ids,
        time.predictors.prior = rs$optimize_years,
        time.optimize.ssr     = rs$optimize_years,
        time.plot             = rs$plot_years
      )

      placebo_model <- synth(placebo_data)

      placebo_results_df <- bind_rows(
        placebo_results_df,
        data.frame(
          region       = region,
          placebo_unit = placebo_country,
          year         = rs$plot_years,
          gap          = as.vector(placebo_data$Y1plot) -
                         as.vector(placebo_data$Y0plot %*% placebo_model$solution.w)
        )
      )
    }, error = function(e) cat("     ERROR:", conditionMessage(e), "\n"))
  }
}

combined_gaps <- bind_rows(
  final_results_df %>% select(region, year, gap) %>% mutate(placebo_unit = "Treated", is_treated = TRUE),
  placebo_results_df %>% mutate(is_treated = FALSE)
)

ggplot(combined_gaps,
       aes(x = year, y = gap, group = placebo_unit,
           color = is_treated, size = is_treated, alpha = is_treated)) +
  annotate("rect", xmin = 2010, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.4) +
  geom_line() +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "tomato"),
                     labels = c("TRUE" = "Treated", "FALSE" = "Placebo donor")) +
  scale_size_manual(values  = c("TRUE" = 1.2, "FALSE" = 0.6)) +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.6)) +
  facet_wrap(~ region, scales = "free_y") +
  labs(title = "Placebo-in-Space Tests: Treated Country vs. All Donor Gaps",
       x = "Year", y = paste0("Gap (Actual - Synthetic) — ", dep_var), color = NULL) +
  guides(size = "none", alpha = "none") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(paste0(Directory, "\\Country_Placebo_Space.png"), width = 14, height = 10)

write.xlsx(placebo_results_df, paste0(Directory, "\\Country_Placebo_Space_results.xlsx"))


# ---- Placebo Tests (In-Time) ----

placebo_time_year <- 2005
intime_placebo_df <- data.frame()

for (region in names(region_store)) {

  cat("======== IN-TIME PLACEBO:", region, "========\n")

  rs <- region_store[[region]]

  pit_pre_years  <- rs$optimize_years[rs$optimize_years < placebo_time_year]
  pit_plot_years <- sort(rs$region_subset %>%
    filter(country_name == region) %>%
    pull(year) %>%
    unique())
  pit_plot_years <- pit_plot_years[
    pit_plot_years >= min(rs$optimize_years) & pit_plot_years <= 2010
  ]

  if (length(pit_pre_years) < 3) {
    cat("  -> Skipping", region, ": not enough pre-placebo years\n"); next
  }

  required_years_pit <- sort(unique(c(pit_pre_years, pit_plot_years)))

  pit_subset <- rs$region_subset %>% filter(year %in% required_years_pit)

  complete_ctrl_pit <- pit_subset %>%
    filter(country_name != region) %>%
    group_by(country_name) %>%
    summarise(has_all = all(required_years_pit %in% year), .groups = "drop") %>%
    filter(has_all) %>%
    pull(country_name)

  pit_subset <- pit_subset %>%
    filter(country_name == region | country_name %in% complete_ctrl_pit)

  pit_treated_id  <- pit_subset %>% filter(country_name == region) %>% pull(unit_id) %>% unique()
  pit_control_ids <- pit_subset %>% filter(country_name != region) %>% pull(unit_id) %>% unique()

  if (length(pit_control_ids) < 2) {
    cat("  -> Skipping", region, ": fewer than 2 controls available\n"); next
  }

  year_blocks_pit   <- split(pit_pre_years, ceiling(seq_along(pit_pre_years) / 3))
  special_preds_pit <- do.call(c, lapply(special_predictors_vars, function(v) {
    lapply(year_blocks_pit, function(blk) list(v, blk, "mean"))
  }))

  tryCatch({
    pit_data <- dataprep(
      foo                   = as.data.frame(pit_subset),
      predictors            = normal_predictors,
      special.predictors    = special_preds_pit,
      dependent             = dep_var,
      unit.variable         = "unit_id",
      time.variable         = "time_id",
      treatment.identifier  = pit_treated_id,
      controls.identifier   = pit_control_ids,
      time.predictors.prior = pit_pre_years,
      time.optimize.ssr     = pit_pre_years,
      time.plot             = pit_plot_years
    )

    pit_model <- synth(pit_data)

    intime_placebo_df <- bind_rows(
      intime_placebo_df,
      data.frame(
        region    = region,
        year      = pit_plot_years,
        actual    = as.vector(pit_data$Y1plot),
        synthetic = as.vector(pit_data$Y0plot %*% pit_model$solution.w)
      ) %>%
        mutate(gap = actual - synthetic)
    )
    cat("  -> Done\n")
  }, error = function(e) cat("  -> ERROR:", conditionMessage(e), "\n"))
}

intime_placebo_df %>%
  pivot_longer(cols = c("actual", "synthetic"), names_to = "series", values_to = "value") %>%
  ggplot(aes(x = year, y = value, color = series)) +
  annotate("rect", xmin = placebo_time_year, xmax = 2010,
           ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.1) +
  geom_line(size = 1) +
  geom_vline(xintercept = placebo_time_year, linetype = "dashed", color = "steelblue") +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title    = paste0("In-Time Placebo: Fake Treatment at ", placebo_time_year),
    subtitle = paste0("Fitted on pre-", placebo_time_year, " data; shaded = placebo post-period (",
                      placebo_time_year, "–2010)"),
    x = "Year", y = paste(dep_var, "(Z-score)")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())
ggsave(paste0(Directory, "\\Country_Placebo_InTime_ActualVsSynthetic.png"), width = 12, height = 8)

ggplot(intime_placebo_df, aes(x = year, y = gap)) +
  annotate("rect", xmin = placebo_time_year, xmax = 2010,
           ymin = -Inf, ymax = Inf, fill = "steelblue", alpha = 0.1) +
  geom_line(size = 1, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = placebo_time_year, linetype = "dashed", color = "steelblue") +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title    = paste0("In-Time Placebo Gap: Fake Treatment at ", placebo_time_year),
    subtitle = paste0("Shaded = placebo post-period (", placebo_time_year, "–2010)"),
    x = "Year", y = paste0("Gap (Actual - Synthetic) — ", dep_var)
  ) +
  theme_minimal()
ggsave(paste0(Directory, "\\Country_Placebo_InTime_Gap.png"), width = 12, height = 8)

write.xlsx(intime_placebo_df, paste0(Directory, "\\Country_Placebo_InTime_results.xlsx"))


# ---- Jackknife Analysis (Leave-One-Donor-Out) ----

jackknife_results_df <- data.frame()
treatment_year       <- 2010

for (region in names(region_store)) {

  cat("======== JACKKNIFE:", region, "========\n")

  rs <- region_store[[region]]

  region_weights <- all_weights_mc %>% filter(region == !!region, weight_pct >= 1)
  has_dominant   <- any(region_weights$weight_pct >= 90)

  ctrl_country_map <- rs$region_subset %>%
    distinct(unit_id, country_name) %>%
    arrange(unit_id) %>%
    filter(unit_id %in% rs$control_ids,
           country_name %in% region_weights$country)

  if (nrow(ctrl_country_map) == 0) {
    cat("  No donors with weight >= 1% — using all donors in control pool\n")
    ctrl_country_map <- rs$region_subset %>%
      distinct(unit_id, country_name) %>%
      arrange(unit_id) %>%
      filter(unit_id %in% rs$control_ids)
  }

  dominant_idx <- which(region_weights$weight_pct[match(ctrl_country_map$country_name, region_weights$country)] >= 90)
  drop_sets    <- as.list(seq_len(nrow(ctrl_country_map)))

  if (has_dominant && length(dominant_idx) > 0 && nrow(ctrl_country_map) >= 2) {
    cat("  Dominant donor detected (weight >= 90%) — running leave-one-out + leave-two-out\n")
    other_idx     <- setdiff(seq_len(nrow(ctrl_country_map)), dominant_idx)
    two_drop_sets <- lapply(other_idx, function(j) c(dominant_idx[1], j))
    drop_sets     <- c(drop_sets, two_drop_sets)
  } else {
    cat("  No dominant donor — using leave-one-out\n")
  }

  for (drop_idx in drop_sets) {

    dropped_countries <- ctrl_country_map$country_name[drop_idx]
    dropped_label     <- paste(dropped_countries, collapse = " & ")
    cat("  Drop donor(s):", dropped_label, "\n")

    jk_subset      <- rs$region_subset %>% filter(!country_name %in% dropped_countries)
    jk_treated_id  <- jk_subset %>% filter(country_name == region) %>% pull(unit_id) %>% unique()
    jk_control_ids <- jk_subset %>% filter(country_name != region) %>% pull(unit_id) %>% unique()

    if (length(jk_control_ids) < 2) { cat("     Skipping: fewer than 2 controls remain\n"); next }

    year_blocks_j        <- split(rs$optimize_years, ceiling(seq_along(rs$optimize_years) / 3))
    special_preds_list_j <- do.call(c, lapply(special_predictors_vars, function(v) {
      lapply(year_blocks_j, function(blk) list(v, blk, "mean"))
    }))

    tryCatch({
      jk_data <- dataprep(
        foo                   = as.data.frame(jk_subset),
        predictors            = normal_predictors,
        special.predictors    = special_preds_list_j,
        dependent             = dep_var,
        unit.variable         = "unit_id",
        time.variable         = "time_id",
        treatment.identifier  = jk_treated_id,
        controls.identifier   = jk_control_ids,
        time.predictors.prior = rs$optimize_years,
        time.optimize.ssr     = rs$optimize_years,
        time.plot             = rs$plot_years
      )

      jk_model <- synth(jk_data)

      jackknife_results_df <- bind_rows(
        jackknife_results_df,
        data.frame(
          region          = region,
          dropped_country = dropped_label,
          year            = rs$plot_years,
          gap             = as.vector(jk_data$Y1plot) -
                            as.vector(jk_data$Y0plot %*% jk_model$solution.w)
        )
      )
    }, error = function(e) cat("     ERROR:", conditionMessage(e), "\n"))
  }
}

baseline_gaps_mc <- final_results_df %>%
  select(region, year, gap) %>%
  mutate(dropped_country = "Baseline (all donors)", is_baseline = TRUE)

combined_jk_mc <- bind_rows(baseline_gaps_mc, jackknife_results_df %>% mutate(is_baseline = FALSE)) %>%
  filter(year <= 2020)

ggplot(combined_jk_mc,
       aes(x = year, y = gap, group = dropped_country,
           color = is_baseline, size = is_baseline, alpha = is_baseline)) +
  annotate("rect", xmin = 2010, xmax = 2020, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.4) +
  geom_line() +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "steelblue"),
                     labels = c("TRUE" = "Baseline (all donors)", "FALSE" = "Jackknife")) +
  scale_size_manual(values  = c("TRUE" = 1.2, "FALSE" = 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.5)) +
  facet_wrap(~ region, scales = "free_y") +
  labs(title = "Jackknife Sensitivity: Leave-One-Out & Leave-Two-Out (Dominant Donor)",
       x = "Year", y = paste0("Gap (Actual - Synthetic) — ", dep_var), color = NULL) +
  guides(size = "none", alpha = "none") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(paste0(Directory, "\\Country_Jackknife.png"), width = 14, height = 10)

write.xlsx(jackknife_results_df, paste0(Directory, "\\Country_Jackknife_results.xlsx"))


# ---- Jackknife Diagnostic ----

baseline_post_sign <- final_results_df %>%
  filter(year > 2010) %>%
  group_by(region) %>%
  summarise(baseline_avg_gap = mean(gap, na.rm = TRUE), .groups = "drop") %>%
  mutate(baseline_sign = sign(baseline_avg_gap))

jk_post_sign <- jackknife_results_df %>%
  filter(year > 2010) %>%
  group_by(region, dropped_country) %>%
  summarise(jk_avg_gap = mean(gap, na.rm = TRUE), .groups = "drop") %>%
  mutate(jk_sign = sign(jk_avg_gap))

sign_flippers <- jk_post_sign %>%
  left_join(baseline_post_sign, by = "region") %>%
  filter(jk_sign != baseline_sign) %>%
  select(region, dropped_country, baseline_avg_gap, jk_avg_gap) %>%
  arrange(region, dropped_country)

cat("\n===== SIGN-FLIPPING JACKKNIFE RESULTS =====\n")
if (nrow(sign_flippers) == 0) {
  cat("No donor removals flip the sign of the average post-treatment gap.\n")
} else {
  print(sign_flippers)
}

write.xlsx(sign_flippers, paste0(Directory, "\\Country_Jackknife_SignFlippers.xlsx"))


# ---- High-Bailout Subsample ----

high_bailout_controls <- controls %>%
  filter(!is.na(bailout_expenditure_pct_gdp), year == 2009, bailout_expenditure_pct_gdp > 8) %>%
  distinct(region) %>%
  pull(region)

high_bailout_treated <- intersect(high_bailout_controls, names(region_store))

cat("high_bailout_controls:", paste(high_bailout_controls, collapse = ", "), "\n")
cat("region_store keys:    ", paste(names(region_store),    collapse = ", "), "\n")
cat("high_bailout_treated: ", paste(high_bailout_treated,   collapse = ", "), "\n")

hb_results_df <- data.frame()

for (region in high_bailout_treated) {

  cat("======== HIGH-BAILOUT SUBSAMPLE:", region, "========\n")

  rs <- region_store[[region]]

  low_bailout_regions <- controls %>%
    filter(!is.na(bailout_expenditure_pct_gdp), year == 2009) %>%
    filter(!(region %in% high_bailout_controls)) %>%
    filter(!(region %in% c(!!region))) %>%
    pull(region) %>%
    unique()

  hb_required_years <- sort(unique(c(rs$optimize_years, rs$plot_years)))

  hb_subset <- rs$region_subset %>%
    bind_rows(
      df5 %>% filter(country_name %in% low_bailout_regions, year %in% hb_required_years)
    ) %>%
    filter(year %in% hb_required_years) %>%
    distinct(country_name, year, .keep_all = TRUE)

  hb_treated_id  <- hb_subset %>% filter(country_name == region) %>% pull(unit_id) %>% unique()
  hb_control_ids <- hb_subset %>% filter(country_name != region) %>% pull(unit_id) %>% unique()

  if (length(hb_control_ids) < 2) {
    cat("  -> Skipping", region, ": fewer than 2 controls available\n"); next
  }

  tryCatch({
    hb_data <- dataprep(
      foo                   = as.data.frame(hb_subset),
      predictors            = normal_predictors,
      special.predictors    = lapply(special_predictors_vars, function(v) list(v, rs$optimize_years, "mean")),
      dependent             = dep_var,
      unit.variable         = "unit_id",
      time.variable         = "time_id",
      treatment.identifier  = hb_treated_id,
      controls.identifier   = hb_control_ids,
      time.predictors.prior = rs$optimize_years,
      time.optimize.ssr     = rs$optimize_years,
      time.plot             = rs$plot_years
    )

    hb_model     <- synth(hb_data)
    hb_actual    <- as.vector(hb_data$Y1plot)
    hb_synthetic <- as.vector(hb_data$Y0plot %*% hb_model$solution.w)
    hb_baseline  <- final_results_df %>% filter(region == !!region) %>% arrange(year) %>% pull(synthetic)

    hb_results_df <- bind_rows(
      hb_results_df,
      data.frame(
        region             = region,
        year               = rs$plot_years,
        actual             = hb_actual,
        synthetic_hb       = hb_synthetic,
        synthetic_baseline = hb_baseline,
        gap                = hb_actual - hb_synthetic,
        baseline_gap       = hb_actual - hb_baseline
      )
    )
    cat("  -> Done\n")
  }, error = function(e) cat("  -> ERROR:", conditionMessage(e), "\n"))
}

write.xlsx(hb_results_df, paste0(Directory, "\\HighBailout_Subsample_Results.xlsx"))


# ============================================================
# SECTION 2: DID & CROSS-SECTIONAL REGRESSIONS
# ============================================================

sensitivity <- read.xlsx(paste0(Directory, "\\Country_Jackknife_SignFlippers.xlsx"))
country     <- read.xlsx(paste0(Directory, "\\Main_Country_results.xlsx"))

country <- country %>%
  left_join(controls, by = c("year", "region"))

hb <- read.xlsx(paste0(Directory, "\\HighBailout_Subsample_Results.xlsx"))
hb <- hb %>%
  left_join(controls, by = c("year", "region")) %>%
  mutate(region = paste0(region, "_HB"))

country <- bind_rows(country, hb)

country <- country %>%
  mutate(bailout_expenditure_pct_gdp = as.numeric(bailout_expenditure_pct_gdp)) %>%
  mutate(bailout_net_cost_pct_gdp    = as.numeric(bailout_net_cost_pct_gdp))


# ---- Country DID Regression ----

unique_regions <- unique(country$region)

post_treat_windows <- list(
  `2010_2015` = c(2010, 2015),
  `2015_2020` = c(2015, 2020),
  `2010_2020` = c(2010, 2020)
)

Country_did_results_by_window <- lapply(names(post_treat_windows), function(window_name) {
  years <- post_treat_windows[[window_name]]

  did_results <- lapply(unique_regions, function(p) {
    did_data <- country %>%
      filter(region == p) %>%
      select(year, gap) %>%
      mutate(post_treat = ifelse(year >= years[1] & year <= years[2], 1, 0))

    did_model   <- lm(gap ~ post_treat, data = did_data)
    did_summary <- summary(did_model)
    coef_table  <- did_summary$coefficients

    data.frame(
      region    = p,
      estimate  = coef_table["post_treat", "Estimate"],
      std_error = coef_table["post_treat", "Std. Error"],
      t_value   = coef_table["post_treat", "t value"],
      p_value   = coef_table["post_treat", "Pr(>|t|)"],
      r_squared = did_summary$r.squared,
      n_obs     = nobs(did_model),
      row.names = NULL
    )
  })

  did_results_df <- do.call(rbind, did_results)

  did_results_df %>%
    mutate(significance = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1  ~ "*",
      TRUE           ~ ""
    )) %>%
    mutate(
      base_region = sub("_HB$", "", region),
      is_hb       = grepl("_HB$", region),
      estimate    = round(estimate, 4)
    ) %>%
    arrange(base_region, is_hb) %>%
    select(region, estimate, std_error, t_value, p_value, significance, r_squared, n_obs)
})

names(Country_did_results_by_window) <- names(post_treat_windows)

write.xlsx(
  Country_did_results_by_window,
  file      = paste0(Directory, "\\Country_DID_PostTreat_Windows.xlsx"),
  asTable   = TRUE,
  overwrite = TRUE
)

invisible(lapply(names(Country_did_results_by_window), function(window_name) {
  cat("\nCountry DID Results for post_treat window:", window_name, "\n")
  print(Country_did_results_by_window[[window_name]])
}))


# ---- Country Cross-Sectional Regressions ----

country <- country %>%
  mutate(bailout_expenditure_pct_gdp = as.numeric(bailout_expenditure_pct_gdp)) %>%
  mutate(bailout_net_cost_pct_gdp    = as.numeric(bailout_net_cost_pct_gdp))

cross_sectional_results_by_window <- lapply(names(post_treat_windows), function(window_name) {
  years <- post_treat_windows[[window_name]]

  average_gap <- country %>%
    filter(year >= years[1] & year <= years[2]) %>%
    group_by(region) %>%
    summarise(avg_gap = mean(gap, na.rm = TRUE))

  bailout_data <- country %>%
    filter(year == 2009) %>%
    select(region, bailout_expenditure_pct_gdp, bailout_net_cost_pct_gdp) %>%
    mutate(net_to_expenditure_ratio = ifelse(bailout_expenditure_pct_gdp == 0, NA_real_,
                                             bailout_net_cost_pct_gdp / bailout_expenditure_pct_gdp))

  regression_data <- average_gap %>% left_join(bailout_data, by = "region")

  non_HB_base <- regression_data %>% filter(!grepl("_HB$", region))
  HB_base     <- regression_data %>% filter(grepl("_HB$", region))

  Robust_base <- non_HB_base %>%
    filter(!region %in% sensitivity$region) %>%
    filter(!region %in% c("Hungary", "Belgium", "Austria"))

  sample_data <- list(non_HB = non_HB_base, HB = HB_base, Robust = Robust_base)

  model_specs <- list(
    Model_1 = avg_gap ~ bailout_expenditure_pct_gdp,
    Model_2 = avg_gap ~ bailout_expenditure_pct_gdp + bailout_net_cost_pct_gdp,
    Model_3 = avg_gap ~ bailout_expenditure_pct_gdp + bailout_net_cost_pct_gdp +
                        bailout_expenditure_pct_gdp * bailout_net_cost_pct_gdp
  )

  results_by_sample <- lapply(names(sample_data), function(sample_name) {
    sample_df <- sample_data[[sample_name]]

    lapply(names(model_specs), function(model_name) {
      model_vars <- all.vars(model_specs[[model_name]])
      model_df   <- sample_df %>%
        select(all_of(model_vars)) %>%
        filter(if_all(everything(), ~ !is.na(.)))

      if (nrow(model_df) == 0) {
        return(data.frame(
          sample = sample_name, model = model_name, term = "NO_DATA",
          estimate = NA_real_, std_error = NA_real_, t_value = NA_real_,
          p_value = NA_real_, r_squared = NA_real_, n_obs = 0,
          significance = "", row.names = NULL
        ))
      }

      model         <- lm(model_specs[[model_name]], data = sample_df, na.action = na.omit)
      summary_model <- summary(model)

      data.frame(
        sample    = sample_name,
        model     = model_name,
        term      = rownames(summary_model$coefficients),
        estimate  = summary_model$coefficients[, "Estimate"],
        std_error = summary_model$coefficients[, "Std. Error"],
        t_value   = summary_model$coefficients[, "t value"],
        p_value   = summary_model$coefficients[, "Pr(>|t|)"],
        r_squared = summary_model$r.squared,
        n_obs     = nobs(model),
        row.names = NULL
      ) %>%
        mutate(significance = case_when(
          p_value < 0.01 ~ "***",
          p_value < 0.05 ~ "**",
          p_value < 0.1  ~ "*",
          TRUE           ~ ""
        ))
    }) %>% bind_rows()
  })

  names(results_by_sample) <- paste0(window_name, "_", names(sample_data))
  return(results_by_sample)
})

cross_sectional_results_by_window <- do.call(c, cross_sectional_results_by_window)

write.xlsx(
  cross_sectional_results_by_window,
  file      = paste0(Directory, "\\Country_CrossSectional_PostTreat_Windows.xlsx"),
  asTable   = TRUE,
  overwrite = TRUE
)

invisible(lapply(names(cross_sectional_results_by_window), function(window_name) {
  cat("\nCountry Cross-Sectional Regression Results:", window_name, "\n")
  print(cross_sectional_results_by_window[[window_name]])
}))


# ============================================================
# SECTION 3: DUBE & ZIPPERER (2015) EXTENSION
# ============================================================

placebo <- read.xlsx(paste0(Directory, "\\Country_Placebo_Space_results.xlsx"))
actual  <- read.xlsx(paste0(Directory, "\\Main_Country_results.xlsx"))

treatment_year <- 2009

# Step 1: Average Post-Treatment Gap per Treated Country
actual_gaps <- actual %>%
  filter(year > treatment_year) %>%
  group_by(region) %>%
  summarise(
    avg_gap_short = mean(gap[year <= 2015], na.rm = TRUE),
    avg_gap_long  = mean(gap[year <= 2020], na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Average Post-Treatment Gap per Placebo Unit
placebo_gaps <- placebo %>%
  filter(year > treatment_year) %>%
  group_by(region, placebo_unit) %>%
  summarise(
    avg_gap_short = mean(gap[year <= 2015], na.rm = TRUE),
    avg_gap_long  = mean(gap[year <= 2020], na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Rank Each Treated Country Within Its Own Placebo Distribution
rank_results <- actual_gaps %>%
  left_join(placebo_gaps, by = "region", suffix = c("_actual", "_placebo")) %>%
  group_by(region) %>%
  summarise(
    n_placebos        = n(),
    actual_gap_short  = first(avg_gap_short_actual),
    actual_gap_long   = first(avg_gap_long_actual),
    rank_short        = mean(avg_gap_short_placebo < actual_gap_short, na.rm = TRUE),
    rank_long         = mean(avg_gap_long_placebo  < actual_gap_long,  na.rm = TRUE),
    pval_upper_short  = mean(avg_gap_short_placebo >= actual_gap_short, na.rm = TRUE),
    pval_upper_long   = mean(avg_gap_long_placebo  >= actual_gap_long,  na.rm = TRUE),
    pval_lower_short  = mean(avg_gap_short_placebo <= actual_gap_short, na.rm = TRUE),
    pval_lower_long   = mean(avg_gap_long_placebo  <= actual_gap_long,  na.rm = TRUE),
    pval_2sided_short = pmin(1, 2 * pmin(pval_upper_short, pval_lower_short)),
    pval_2sided_long  = pmin(1, 2 * pmin(pval_upper_long,  pval_lower_long)),
    .groups = "drop"
  )

cat("\n--- Individual Country Percentile Ranks ---\n")
print(rank_results %>%
        select(region, n_placebos,
               actual_gap_short, rank_short, pval_2sided_short,
               actual_gap_long,  rank_long,  pval_2sided_long) %>%
        arrange(desc(rank_short)))

# Step 4: Mean Percentile Rank (Dube & Zipperer Aggregate Test)
n_treated       <- nrow(rank_results)
mean_rank_short <- mean(rank_results$rank_short)
mean_rank_long  <- mean(rank_results$rank_long)

ttest_short <- t.test(rank_results$rank_short, mu = 0.5)
ttest_long  <- t.test(rank_results$rank_long,  mu = 0.5)

cat("\n--- Dube & Zipperer Mean Percentile Rank Test ---\n")
cat(sprintf("\nShort Run (2010-2015):\n"))
cat(sprintf("  Mean Rank:  %.4f  (H0: 0.500)\n", mean_rank_short))
cat(sprintf("  t-stat:     %.3f\n", ttest_short$statistic))
cat(sprintf("  p-value:    %.4f\n", ttest_short$p.value))
cat(sprintf("\nLong Run (2010-2020):\n"))
cat(sprintf("  Mean Rank:  %.4f  (H0: 0.500)\n", mean_rank_long))
cat(sprintf("  t-stat:     %.3f\n", ttest_long$statistic))
cat(sprintf("  p-value:    %.4f\n", ttest_long$p.value))

# Step 5: Heterogeneity Test (Permutation)
obs_var_short <- var(rank_results$rank_short)
obs_var_long  <- var(rank_results$rank_long)

set.seed(42)
n_perm_ks <- 10000
n_r       <- nrow(rank_results)

perm_var_short <- replicate(n_perm_ks, var(runif(n_r)))
perm_var_long  <- replicate(n_perm_ks, var(runif(n_r)))

perm_pval_het_short <- mean(abs(perm_var_short - 1/12) >= abs(obs_var_short - 1/12))
perm_pval_het_long  <- mean(abs(perm_var_long  - 1/12) >= abs(obs_var_long  - 1/12))

cat("\n--- Heterogeneity Test (Permutation, handles ties) ---\n")
cat(sprintf("Short Run: Var(ranks) = %.4f, perm p = %.4f\n", obs_var_short, perm_pval_het_short))
cat(sprintf("Long Run:  Var(ranks) = %.4f, perm p = %.4f\n", obs_var_long,  perm_pval_het_long))
cat(sprintf("Expected variance under H0 (uniform): %.4f\n", 1/12))

# Step 6: Permutation-Based Inference for Second-Stage OLS
second_stage <- rank_results %>%
  filter(region != "Iceland") %>%
  select(region, actual_gap_short, actual_gap_long) %>%
  left_join(controls %>% select(region,
                                bailout_expenditure_pct_gdp,
                                bailout_net_cost_pct_gdp),
            by = "region")

ols_short <- lm(actual_gap_short ~ bailout_expenditure_pct_gdp +
                  bailout_net_cost_pct_gdp, data = second_stage)
ols_long  <- lm(actual_gap_long  ~ bailout_expenditure_pct_gdp +
                  bailout_net_cost_pct_gdp, data = second_stage)

actual_coef_gross_short <- coef(ols_short)["bailout_expenditure_pct_gdp"]
actual_coef_net_short   <- coef(ols_short)["bailout_net_cost_pct_gdp"]
actual_coef_gross_long  <- coef(ols_long)["bailout_expenditure_pct_gdp"]
actual_coef_net_long    <- coef(ols_long)["bailout_net_cost_pct_gdp"]

set.seed(42)
n_perms <- 10000

perm_coef_gross_short <- numeric(n_perms)
perm_coef_net_short   <- numeric(n_perms)
perm_coef_gross_long  <- numeric(n_perms)
perm_coef_net_long    <- numeric(n_perms)

for (i in 1:n_perms) {
  perm_data <- second_stage %>%
    mutate(
      bailout_expenditure_pct_gdp = sample(bailout_expenditure_pct_gdp),
      bailout_net_cost_pct_gdp    = sample(bailout_net_cost_pct_gdp)
    )

  fit_s <- lm(actual_gap_short ~ bailout_expenditure_pct_gdp +
                bailout_net_cost_pct_gdp, data = perm_data)
  fit_l <- lm(actual_gap_long  ~ bailout_expenditure_pct_gdp +
                bailout_net_cost_pct_gdp, data = perm_data)

  perm_coef_gross_short[i] <- coef(fit_s)["bailout_expenditure_pct_gdp"]
  perm_coef_net_short[i]   <- coef(fit_s)["bailout_net_cost_pct_gdp"]
  perm_coef_gross_long[i]  <- coef(fit_l)["bailout_expenditure_pct_gdp"]
  perm_coef_net_long[i]    <- coef(fit_l)["bailout_net_cost_pct_gdp"]
}

perm_pval <- function(actual, perm_dist) mean(abs(perm_dist) >= abs(actual))

cat("\n--- Permutation Inference: Second-Stage OLS ---\n")
cat(sprintf("\nShort Run (2010-2015):\n"))
cat(sprintf("  Gross Expenditure: coef = %+.4f, perm p = %.4f\n",
            actual_coef_gross_short,
            perm_pval(actual_coef_gross_short, perm_coef_gross_short)))
cat(sprintf("  Net Cost:          coef = %+.4f, perm p = %.4f\n",
            actual_coef_net_short,
            perm_pval(actual_coef_net_short, perm_coef_net_short)))
cat(sprintf("\nLong Run (2010-2020):\n"))
cat(sprintf("  Gross Expenditure: coef = %+.4f, perm p = %.4f\n",
            actual_coef_gross_long,
            perm_pval(actual_coef_gross_long, perm_coef_gross_long)))
cat(sprintf("  Net Cost:          coef = %+.4f, perm p = %.4f\n",
            actual_coef_net_long,
            perm_pval(actual_coef_net_long, perm_coef_net_long)))

# Step 7: Spearman Rank Correlations (permutation p-values)
spearman_perm_pval <- function(x, y, n_perm = 10000, seed = 42) {
  set.seed(seed)
  obs_rho  <- cor(x, y, method = "spearman")
  perm_rho <- replicate(n_perm, cor(sample(x), y, method = "spearman"))
  list(rho = obs_rho, p = mean(abs(perm_rho) >= abs(obs_rho)))
}

sr_gross <- spearman_perm_pval(second_stage$actual_gap_short, second_stage$bailout_expenditure_pct_gdp)
sr_net   <- spearman_perm_pval(second_stage$actual_gap_short, second_stage$bailout_net_cost_pct_gdp)
lr_gross <- spearman_perm_pval(second_stage$actual_gap_long,  second_stage$bailout_expenditure_pct_gdp)
lr_net   <- spearman_perm_pval(second_stage$actual_gap_long,  second_stage$bailout_net_cost_pct_gdp)

cat("\n--- Spearman Rank Correlations (permutation p-values) ---\n")
cat(sprintf("Short Run vs Gross Expenditure: rho = %.4f, perm p = %.4f\n", sr_gross$rho, sr_gross$p))
cat(sprintf("Short Run vs Net Cost:          rho = %.4f, perm p = %.4f\n", sr_net$rho,   sr_net$p))
cat(sprintf("Long Run  vs Gross Expenditure: rho = %.4f, perm p = %.4f\n", lr_gross$rho, lr_gross$p))
cat(sprintf("Long Run  vs Net Cost:          rho = %.4f, perm p = %.4f\n", lr_net$rho,   lr_net$p))

# Step 8: Export Results
wb_ext <- createWorkbook()

addWorksheet(wb_ext, "Rank Results")
writeData(wb_ext, "Rank Results", rank_results)

addWorksheet(wb_ext, "Aggregate Test")
agg <- data.frame(
  Window           = c("Short (2010-2015)", "Long (2010-2020)"),
  Mean_Rank        = c(mean_rank_short,      mean_rank_long),
  T_Stat           = c(ttest_short$statistic, ttest_long$statistic),
  P_Value          = c(ttest_short$p.value,   ttest_long$p.value),
  Var_Ranks        = c(obs_var_short,         obs_var_long),
  Het_Perm_P_Value = c(perm_pval_het_short,   perm_pval_het_long)
)
writeData(wb_ext, "Aggregate Test", agg)

addWorksheet(wb_ext, "Permutation Inference")
perm_out <- data.frame(
  Window    = c("Short", "Short", "Long", "Long"),
  Variable  = c("Gross Expenditure", "Net Cost", "Gross Expenditure", "Net Cost"),
  Coef      = c(actual_coef_gross_short, actual_coef_net_short,
                actual_coef_gross_long,  actual_coef_net_long),
  Perm_Pval = c(perm_pval(actual_coef_gross_short, perm_coef_gross_short),
                perm_pval(actual_coef_net_short,   perm_coef_net_short),
                perm_pval(actual_coef_gross_long,  perm_coef_gross_long),
                perm_pval(actual_coef_net_long,    perm_coef_net_long))
)
writeData(wb_ext, "Permutation Inference", perm_out)

addWorksheet(wb_ext, "Spearman")
spearman_out <- data.frame(
  Window    = c("Short", "Short", "Long", "Long"),
  Variable  = c("Gross Expenditure", "Net Cost", "Gross Expenditure", "Net Cost"),
  Rho       = c(sr_gross$rho, sr_net$rho, lr_gross$rho, lr_net$rho),
  Perm_Pval = c(sr_gross$p,   sr_net$p,   lr_gross$p,   lr_net$p)
)
writeData(wb_ext, "Spearman", spearman_out)

saveWorkbook(wb_ext, file.path(Directory, "Table_4_5&6_Results.xlsx"), overwrite = TRUE)
cat("\nResults saved to", file.path(Directory, "Table_4_5&6_Results.xlsx"), "\n")


# ============================================================
# SECTION 4: TABLES & FIGURES
# ============================================================

# ---- Table 1 ----

Table_1  <- read.xlsx(data_path, sheet = "Data5")
Table_1c <- read.xlsx(data_path, sheet = "Controls")

Table_1 <- Table_1 %>%
  select(country_name, year, fdi_inflows_bop_usd) %>%
  group_by(country_name) %>%
  mutate(
    pre_mean = mean(fdi_inflows_bop_usd[year >= 1995 & year <= 2020], na.rm = TRUE),
    pre_sd   = sd(fdi_inflows_bop_usd[year >= 1995 & year <= 2020],   na.rm = TRUE),
    pre_n    = sum(!is.na(fdi_inflows_bop_usd[year >= 1995 & year <= 2020]))
  ) %>%
  ungroup() %>%
  mutate(pre_mean = pre_mean / 1e9, pre_sd = pre_sd / 1e9) %>%
  arrange(country_name, year) %>%
  filter(country_name %in% Bailout_Regions)

Table_1c_summary <- Table_1c %>%
  rename(country_name = region) %>%
  filter(country_name %in% Bailout_Regions) %>%
  group_by(country_name) %>%
  summarise(
    bailout_expenditure_pct_gdp = first(bailout_expenditure_pct_gdp),
    bailout_net_cost_pct_gdp    = first(bailout_net_cost_pct_gdp),
    .groups = "drop"
  )

Table_1_summary <- Table_1 %>%
  group_by(country_name) %>%
  summarise(
    pre_mean_usd_bn = first(pre_mean),
    pre_sd_usd_bn   = first(pre_sd),
    n_obs           = first(pre_n)
  ) %>%
  ungroup() %>%
  left_join(Table_1c_summary, by = "country_name") %>%
  arrange(country_name)
print(Table_1_summary)

Table_1_export <- Table_1_summary %>%
  transmute(
    Country                       = country_name,
    Mean                          = formatC(pre_mean_usd_bn, format = "f", digits = 3),
    SD                            = formatC(pre_sd_usd_bn,   format = "f", digits = 3),
    `Bailout Expenditure (% GDP)` = ifelse(is.na(bailout_expenditure_pct_gdp), "",
                                      formatC(bailout_expenditure_pct_gdp, format = "f", digits = 3)),
    `Bailout Net Cost (% GDP)`    = ifelse(is.na(bailout_net_cost_pct_gdp), "",
                                      formatC(bailout_net_cost_pct_gdp,    format = "f", digits = 3)),
    Observations                  = as.character(n_obs)
  )

write.xlsx(Table_1_export, paste0(Directory, "\\Table_1.xlsx"), rowNames = FALSE)


# ---- Table 2 ----

EXCEL_PATH <- paste0(Directory, "\\Country_DID_PostTreat_Windows.xlsx")

read_sheet <- function(sheet) {
  read_excel(EXCEL_PATH, sheet = sheet) |>
    mutate(
      window       = sheet,
      across(c(estimate, std_error, r_squared, n_obs), as.numeric),
      significance = replace_na(significance, "")
    )
}

raw <- bind_rows(
  read_sheet("2010_2015"),
  read_sheet("2010_2020")
)

df <- raw |> filter(!str_detect(region, "_HB$"), region != "Iceland")

zero_band <- 0.05

windows <- list(
  "2010_2015" = "2010-2015",
  "2010_2020" = "2010-2020 (full period)"
)

country_order <- df |>
  filter(window == "2010_2015") |>
  mutate(
    coef_group = case_when(
      estimate > zero_band  ~ "positive",
      estimate < -zero_band ~ "negative",
      TRUE                  ~ "close_to_zero"
    ),
    coef_group_rank = match(coef_group, c("positive", "close_to_zero", "negative"))
  ) |>
  arrange(coef_group_rank, desc(estimate)) |>
  pull(region)

wb <- createWorkbook()

header_style <- createStyle(
  fontName = "Times New Roman", fontSize = 11, textDecoration = "bold",
  halign = "right", border = c("top", "bottom"), borderStyle = "medium"
)
header_left_style <- createStyle(
  fontName = "Times New Roman", fontSize = 11, textDecoration = "bold",
  halign = "left", border = c("top", "bottom"), borderStyle = "medium"
)
data_right_style <- createStyle(fontName = "Times New Roman", fontSize = 11, halign = "right")
data_left_style  <- createStyle(fontName = "Times New Roman", fontSize = 11, halign = "left")
sep_style        <- createStyle(border = "bottom", borderStyle = "thin")
bottom_style     <- createStyle(border = "bottom", borderStyle = "medium")

fmt_est <- function(est, sig) paste0(formatC(est, format = "f", digits = 3), sig)
fmt_se  <- function(se)       paste0("(", formatC(se,  format = "f", digits = 3), ")")
fmt_r2  <- function(r2)       formatC(r2, format = "f", digits = 3)
fmt_n   <- function(n)        as.character(as.integer(n))

for (wlabel in names(windows)) {
  sub <- df |>
    filter(window == wlabel) |>
    mutate(
      coef_group = case_when(
        estimate > zero_band  ~ "positive",
        estimate < -zero_band ~ "negative",
        TRUE                  ~ "close_to_zero"
      ),
      coef_group_rank = match(coef_group, c("positive", "close_to_zero", "negative")),
      region = factor(region, levels = country_order)
    ) |>
    arrange(region)

  tbl <- bind_rows(lapply(seq_len(nrow(sub)), function(i) {
    r        <- sub[i, ]
    est_cell <- fmt_est(r$estimate, r$significance)
    se_cell  <- fmt_se(r$std_error)
    r2_cell  <- fmt_r2(r$r_squared)
    n_cell   <- fmt_n(r$n_obs)
    data.frame(
      Country        = c(as.character(r$region), ""),
      `DID Estimate` = c(est_cell, se_cell),
      `R² / N`       = c(r2_cell,  n_cell),
      stringsAsFactors = FALSE,
      check.names      = FALSE
    )
  }))

  addWorksheet(wb, wlabel)
  writeData(wb, wlabel,
            paste0("Real GDP per capita - ", windows[[wlabel]]),
            startRow = 1, startCol = 1)
  writeData(wb, wlabel, tbl, startRow = 2, startCol = 1)

  n_rows <- nrow(tbl)
  addStyle(wb, wlabel, header_left_style,  rows = 2,            cols = 1)
  addStyle(wb, wlabel, header_style,       rows = 2,            cols = 2:3, gridExpand = TRUE)
  addStyle(wb, wlabel, data_left_style,    rows = 3:(2+n_rows), cols = 1)
  addStyle(wb, wlabel, data_right_style,   rows = 3:(2+n_rows), cols = 2:3, gridExpand = TRUE)
  addStyle(wb, wlabel, bottom_style,       rows = 2 + n_rows,   cols = 1:3, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, wlabel, cols = 1:3, widths = c(22, 20, 18))

  sep_positions <- which(diff(sub$coef_group_rank) != 0) * 2
  for (pos in sep_positions) {
    addStyle(wb, wlabel, sep_style, rows = pos + 2, cols = 1:3, gridExpand = TRUE, stack = TRUE)
  }
}

out_path <- paste0(Directory, "\\Table_2.xlsx")
saveWorkbook(wb, file = out_path, overwrite = TRUE)
cat("Saved:", out_path, "\n")


# ---- Table 3 ----

EXCEL_PATH <- paste0(Directory, "\\Country_CrossSectional_PostTreat_Windows.xlsx")

read_sheet <- function(sheet) {
  read_excel(EXCEL_PATH, sheet = sheet) |>
    mutate(
      window       = sheet,
      across(any_of(c("estimate", "std_error", "r_squared", "n_obs")), as.numeric),
      significance = replace_na(as.character(significance), "")
    )
}

raw <- bind_rows(
  read_sheet("2010_2015_non_HB"),
  read_sheet("2010_2020_non_HB"),
  read_sheet("2010_2015_HB"),
  read_sheet("2010_2020_HB"),
  read_sheet("2010_2015_Robust"),
  read_sheet("2010_2020_Robust")
)

windows3 <- list(
  "2010_2015_non_HB"  = "Full Sample 2010-2015",
  "2010_2020_non_HB"  = "Full Sample 2010-2020 (full period)",
  "2010_2015_HB"      = "HB 2010-2015",
  "2010_2020_HB"      = "HB 2010-2020 (full period)",
  "2010_2015_Robust"  = "Robust 2010-2015",
  "2010_2020_Robust"  = "Robust 2010-2020 (full period)"
)

intercept_labels <- c("(Intercept)", "Intercept", "intercept")

pretty_term <- function(trm) {
  dplyr::case_when(
    trm == "bailout_expenditure_pct_gdp" ~ "Bailout Expenditure",
    trm == "bailout_net_cost_pct_gdp"    ~ "Bailout Net Cost",
    trm %in% c(
      "bailout_expenditure_pct_gdp:bailout_net_cost_pct_gdp",
      "bailout_net_cost_pct_gdp:bailout_expenditure_pct_gdp"
    ) ~ "Net Cost * Expenditure",
    TRUE ~ trm
  )
}

footer_top_style <- createStyle(border = "top", borderStyle = "thin")

wb3 <- createWorkbook()

panel_specs <- list(
  list(window = "2010_2015_non_HB", group = "Main Finding: Non-HB Countries", period = "2010-2015"),
  list(window = "2010_2020_non_HB", group = "Main Finding: Non-HB Countries", period = "2010-2020"),
  list(window = "2010_2015_HB",     group = "Robustness: HB Countries",       period = "2010-2015"),
  list(window = "2010_2020_HB",     group = "Robustness: HB Countries",       period = "2010-2020"),
  list(window = "2010_2015_Robust", group = "Robustness: Restricted Sample",  period = "2010-2015"),
  list(window = "2010_2020_Robust", group = "Robustness: Restricted Sample",  period = "2010-2020")
)

model_labels       <- c("(1)", "(2)", "(3)")
model_ids          <- c("Model_1", "Model_2", "Model_3")
term_labels        <- c("Bailout Expenditure", "Bailout Net Cost", "Net Cost * Expenditure")
n_models_per_panel <- length(model_ids)
n_panels           <- length(panel_specs)
n_data_cols        <- n_models_per_panel * n_panels
n_cols             <- 1L + n_data_cols
col_nms            <- paste0("V", seq_len(n_cols))

make_row <- function(...) setNames(as.data.frame(t(c(...)), stringsAsFactors = FALSE), col_nms)

get_term_candidates <- function(lbl) {
  if (lbl == "Bailout Expenditure") return(c("bailout_expenditure_pct_gdp"))
  if (lbl == "Bailout Net Cost")    return(c("bailout_net_cost_pct_gdp"))
  c("bailout_expenditure_pct_gdp:bailout_net_cost_pct_gdp",
    "bailout_net_cost_pct_gdp:bailout_expenditure_pct_gdp")
}

get_cell <- function(window, model, term_label, field) {
  term_candidates <- get_term_candidates(term_label)
  r <- raw |>
    filter(window == !!window, model == !!model, term %in% term_candidates) |>
    slice(1)
  if (nrow(r) == 0) return("")
  if (field == "estimate") return(fmt_est(r$estimate[1], r$significance[1]))
  if (field == "se")       return(fmt_se(r$std_error[1]))
  ""
}

get_footer <- function(window, model, field) {
  r <- raw |>
    filter(window == !!window, model == !!model) |>
    slice(1)
  if (nrow(r) == 0) return("")
  if (field == "n")  return(fmt_n(r$n_obs[1]))
  if (field == "r2") return(fmt_r2(r$r_squared[1]))
  ""
}

group_row_cells  <- c("")
period_row_cells <- c("")
model_row_cells  <- c("Variable")

for (sp in panel_specs) {
  group_row_cells  <- c(group_row_cells,  sp$group,  rep("", n_models_per_panel - 1L))
  period_row_cells <- c(period_row_cells, sp$period, rep("", n_models_per_panel - 1L))
  model_row_cells  <- c(model_row_cells,  model_labels)
}

header_rows <- bind_rows(
  make_row(group_row_cells),
  make_row(period_row_cells),
  make_row(model_row_cells)
)

body_rows <- bind_rows(lapply(term_labels, function(lbl) {
  est_vals <- c()
  se_vals  <- c()
  for (sp in panel_specs) {
    for (m in model_ids) {
      est_vals <- c(est_vals, get_cell(sp$window, m, lbl, "estimate"))
      se_vals  <- c(se_vals,  get_cell(sp$window, m, lbl, "se"))
    }
  }
  bind_rows(make_row(lbl, est_vals), make_row("", se_vals))
}))

n_vals  <- c()
r2_vals <- c()
for (sp in panel_specs) {
  for (m in model_ids) {
    n_vals  <- c(n_vals,  get_footer(sp$window, m, "n"))
    r2_vals <- c(r2_vals, get_footer(sp$window, m, "r2"))
  }
}

footer_rows <- bind_rows(
  make_row("Observations", n_vals),
  make_row("R-squared",    r2_vals)
)

tbl <- bind_rows(header_rows, body_rows, footer_rows)

n_header <- nrow(header_rows)
n_body   <- nrow(body_rows)
n_footer <- nrow(footer_rows)

r_header_start <- 2L
r_header_end   <- r_header_start + n_header - 1L
r_body_start   <- r_header_end + 1L
r_body_end     <- r_body_start + n_body - 1L
r_footer_start <- r_body_end + 1L
r_footer_end   <- r_footer_start + n_footer - 1L

sheet_name <- "Table_3"
addWorksheet(wb3, sheet_name)
writeData(wb3, sheet_name, "Cross-Sectional Models of Bailout Impact", startRow = 1, startCol = 1)
writeData(wb3, sheet_name, tbl, startRow = r_header_start, startCol = 1, colNames = FALSE)

for (i in seq_along(panel_specs)) {
  c1 <- 2L + (i - 1L) * n_models_per_panel
  c2 <- c1 + n_models_per_panel - 1L
  mergeCells(wb3, sheet = sheet_name, cols = c1:c2, rows = r_header_start)
  mergeCells(wb3, sheet = sheet_name, cols = c1:c2, rows = r_header_start + 1L)
}

header_center_style <- createStyle(
  fontName = "Times New Roman", fontSize = 11, textDecoration = "bold",
  halign = "center", border = c("top", "bottom"), borderStyle = "medium"
)
header_period_style <- createStyle(
  fontName = "Times New Roman", fontSize = 11, textDecoration = "italic",
  halign = "center"
)

addStyle(wb3, sheet_name, header_center_style, rows = r_header_start,      cols = 2:n_cols, gridExpand = TRUE)
addStyle(wb3, sheet_name, header_period_style, rows = r_header_start + 1L, cols = 2:n_cols, gridExpand = TRUE)
addStyle(wb3, sheet_name, header_left_style,   rows = r_header_end,        cols = 1)
addStyle(wb3, sheet_name, header_style,        rows = r_header_end,        cols = 2:n_cols, gridExpand = TRUE)
addStyle(wb3, sheet_name, data_left_style,     rows = r_body_start:r_body_end,     cols = 1,        gridExpand = TRUE)
addStyle(wb3, sheet_name, data_right_style,    rows = r_body_start:r_body_end,     cols = 2:n_cols, gridExpand = TRUE)
addStyle(wb3, sheet_name, data_left_style,     rows = r_footer_start:r_footer_end, cols = 1,        gridExpand = TRUE, stack = TRUE)
addStyle(wb3, sheet_name, data_right_style,    rows = r_footer_start:r_footer_end, cols = 2:n_cols, gridExpand = TRUE, stack = TRUE)
addStyle(wb3, sheet_name, footer_top_style,    rows = r_footer_start,              cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
addStyle(wb3, sheet_name, bottom_style,        rows = r_footer_end,                cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
setColWidths(wb3, sheet_name, cols = 1,        widths = 28)
setColWidths(wb3, sheet_name, cols = 2:n_cols, widths = rep(11, n_data_cols))

out_path3 <- paste0(Directory, "\\Table_3.xlsx")
saveWorkbook(wb3, file = out_path3, overwrite = TRUE)
cat("Saved:", out_path3, "\n")


# ---- Country Weights Table ----

Country_Weights <- read.xlsx(paste0(Directory, "\\Country_Weights.xlsx"))

cw_sig <- Country_Weights %>%
  filter(weight_pct > 0) %>%
  arrange(region, desc(weight_pct)) %>%
  transmute(
    Country      = region,
    Donor        = country,
    `Weight (%)` = formatC(weight_pct, format = "f", digits = 2)
  )

wb_cw <- createWorkbook()
addWorksheet(wb_cw, "Donor_Weights")
writeData(wb_cw, "Donor_Weights", "Synthetic Control Donor Weights (Non-Zero)", startRow = 1, startCol = 1)
writeData(wb_cw, "Donor_Weights", cw_sig, startRow = 2, startCol = 1)

n_cw <- nrow(cw_sig)
addStyle(wb_cw, "Donor_Weights", header_left_style, rows = 2,            cols = 1)
addStyle(wb_cw, "Donor_Weights", header_style,      rows = 2,            cols = 2:3, gridExpand = TRUE)
addStyle(wb_cw, "Donor_Weights", data_left_style,   rows = 3:(2+n_cw),   cols = 1)
addStyle(wb_cw, "Donor_Weights", data_left_style,   rows = 3:(2+n_cw),   cols = 2)
addStyle(wb_cw, "Donor_Weights", data_right_style,  rows = 3:(2+n_cw),   cols = 3)
addStyle(wb_cw, "Donor_Weights", bottom_style,      rows = 2 + n_cw,     cols = 1:3, gridExpand = TRUE, stack = TRUE)
setColWidths(wb_cw, "Donor_Weights", cols = 1:3, widths = c(22, 22, 14))

group_breaks <- which(diff(as.integer(factor(cw_sig$Country, levels = unique(cw_sig$Country)))) != 0)
for (pos in group_breaks) {
  addStyle(wb_cw, "Donor_Weights", sep_style, rows = pos + 2, cols = 1:3, gridExpand = TRUE, stack = TRUE)
}

out_path_cw <- paste0(Directory, "\\Table_Donor_Weights.xlsx")
saveWorkbook(wb_cw, file = out_path_cw, overwrite = TRUE)
cat("Saved:", out_path_cw, "\n")


# ---- Donor List Table ----

all_donors <- Country_Weights %>%
  distinct(country) %>%
  arrange(country)

luxembourg_donors  <- Country_Weights %>% filter(region == "Luxembourg") %>% distinct(country) %>% pull(country)
other_region_donors <- Country_Weights %>% filter(region != "Luxembourg") %>% distinct(country) %>% pull(country)
luxembourg_only    <- setdiff(luxembourg_donors, other_region_donors)

donor_list <- all_donors %>%
  mutate(
    is_lux_only = country %in% luxembourg_only,
    Donor       = ifelse(is_lux_only, paste0(country, "*"), country)
  ) %>%
  select(Donor)

wb_donor <- createWorkbook()
addWorksheet(wb_donor, "Table_Donor_List")
writeData(wb_donor, "Table_Donor_List",
          "Synthetic Control Donor Countries\n* Donors unique to Luxembourg",
          startRow = 1, startCol = 1)
writeData(wb_donor, "Table_Donor_List", donor_list, startRow = 3, startCol = 1)

n_donor <- nrow(donor_list)
addStyle(wb_donor, "Table_Donor_List", header_left_style, rows = 3,              cols = 1)
addStyle(wb_donor, "Table_Donor_List", data_left_style,   rows = 4:(3+n_donor),  cols = 1)
addStyle(wb_donor, "Table_Donor_List", bottom_style,      rows = 3 + n_donor,    cols = 1, stack = TRUE)
setColWidths(wb_donor, "Table_Donor_List", cols = 1, widths = 26)

out_path_donor <- paste0(Directory, "\\Table_Donor_List.xlsx")
saveWorkbook(wb_donor, file = out_path_donor, overwrite = TRUE)
cat("Saved:", out_path_donor, "\n")


# ---- Figures 1 & 2 ----

Figure_1 <- read.xlsx(paste0(Directory, "\\Main_Country_results.xlsx")) %>%
  filter(region %in% Bailout_Regions)

panel_order <- c(sort(setdiff(Bailout_Regions, "Iceland")), "Iceland")
regions_a   <- panel_order[1:8]
regions_b   <- panel_order[9:16]

Figure_1 <- Figure_1 %>% mutate(region = factor(region, levels = panel_order))

Figure_1_a <- Figure_1 %>% filter(region %in% regions_a)
Figure_1_b <- Figure_1 %>% filter(region %in% regions_b)

common_nrow <- max(
  ceiling(length(regions_a) / 3),
  ceiling(length(regions_b) / 3)
)

build_combined_plot <- function(df, plot_title, nrow_value) {
  ggplot(df, aes(x = .data$year)) +
    geom_line(aes(y = .data$actual,    color = "Actual"),    linewidth = 1.2) +
    geom_line(aes(y = .data$synthetic, color = "Synthetic"), linewidth = 0.9, linetype = "dashed") +
    geom_vline(xintercept = 2010, linetype = "dotted", color = "black", size = 0.8) +
    facet_wrap(~ region, scales = "free_y", nrow = nrow_value, ncol = 3,
               axes = "all_x", axis.labels = "all_x") +
    scale_color_manual(values = c("Actual" = "black", "Synthetic" = "grey50")) +
    labs(x = "Year", y = "FDI Z-Score (3 Year Moving Average)", color = "Series:") +
    theme_bw(base_family = "Times New Roman") +
    theme(
      plot.background  = element_rect(fill = "grey93", color = "black", linewidth = 0.8),
      strip.background = element_rect(fill = "grey93", color = NA),
      panel.background = element_rect(fill = "white",  color = "grey80"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text   = element_text(size = 13, face = "bold"),
      axis.text    = element_text(size = 12, face = "bold"),
      axis.title   = element_text(size = 13, face = "bold"),
      panel.spacing = grid::unit(0.8, "lines"),
      plot.margin   = margin(t = 8, r = 12, b = 10, l = 10),
      legend.position    = c(0.99, 0.05),
      legend.direction   = "horizontal",
      legend.justification = c(1, 0),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 11, face = "bold"),
      legend.key.width  = grid::unit(0.7, "lines"),
      legend.key.height = grid::unit(0.5, "lines"),
      legend.spacing.x  = grid::unit(0.2, "lines"),
      legend.background = element_rect(fill = "grey93", color = NA)
    ) +
    guides(color = guide_legend(title.position = "left", nrow = 1, byrow = TRUE))
}

fig_width  <- 14
fig_height <- 10

ggsave(paste0(Directory, "\\Figure_1_A.png"),
       plot = build_combined_plot(Figure_1_a, "", common_nrow),
       width = fig_width, height = fig_height, dpi = 300)
ggsave(paste0(Directory, "\\Figure_1_B.png"),
       plot = build_combined_plot(Figure_1_b, "", common_nrow),
       width = fig_width, height = fig_height, dpi = 300)

Figure_2 <- read.xlsx(paste0(Directory, "\\Main_Country_results.xlsx")) %>%
  filter(region %in% Bailout_Regions) %>%
  mutate(region = factor(region, levels = panel_order))

Figure_2_a <- Figure_2 %>% filter(region %in% regions_a)
Figure_2_b <- Figure_2 %>% filter(region %in% regions_b)

ggsave(paste0(Directory, "\\Figure_2_A.png"),
       plot = build_combined_plot(
         Figure_2_a %>% transmute(year, region, actual = gap, synthetic = 0), "", common_nrow),
       width = fig_width, height = fig_height, dpi = 300)
ggsave(paste0(Directory, "\\Figure_2_B.png"),
       plot = build_combined_plot(
         Figure_2_b %>% transmute(year, region, actual = gap, synthetic = 0), "", common_nrow),
       width = fig_width, height = fig_height, dpi = 300)


# ---- Figure 3 ----

Figure_3 <- read.xlsx(paste0(Directory, "\\Country_Placebo_Space_results.xlsx")) %>%
  filter(region %in% Bailout_Regions) %>%
  left_join(Figure_2 %>% select(year, region, original_gap = gap), by = c("year", "region")) %>%
  mutate(region = factor(region, levels = panel_order))

Figure_3_a <- Figure_3 %>% filter(region %in% regions_a)
Figure_3_b <- Figure_3 %>% filter(region %in% regions_b)

build_placebo_plot <- function(df, plot_title, nrow_value) {
  ggplot(df, aes(x = .data$year)) +
    geom_line(aes(y = .data$gap, group = .data$placebo_unit, color = "Placebo"),
              linewidth = 0.5, alpha = 0.8) +
    geom_line(aes(y = .data$original_gap, color = "Actual"), linewidth = 1.2) +
    geom_vline(xintercept = 2010, linetype = "dotted", color = "black", size = 0.8) +
    facet_wrap(~ region, scales = "free_y", nrow = nrow_value, ncol = 3,
               axes = "all_x", axis.labels = "all_x") +
    scale_color_manual(values = c("Actual" = "black", "Placebo" = "grey70")) +
    labs(x = "Year", y = "FDI Z-Score Gap (Actual - Synthetic)", color = "Series:") +
    theme_bw(base_family = "Times New Roman") +
    theme(
      plot.background  = element_rect(fill = "grey93", color = "black", linewidth = 0.8),
      strip.background = element_rect(fill = "grey93", color = NA),
      panel.background = element_rect(fill = "white",  color = "grey80"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text   = element_text(size = 13, face = "bold"),
      axis.text    = element_text(size = 12, face = "bold"),
      axis.title   = element_text(size = 13, face = "bold"),
      panel.spacing = grid::unit(0.8, "lines"),
      plot.margin   = margin(t = 8, r = 12, b = 10, l = 10),
      legend.position    = c(0.99, 0.05),
      legend.direction   = "horizontal",
      legend.justification = c(1, 0),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 11, face = "bold"),
      legend.key.width  = grid::unit(0.7, "lines"),
      legend.key.height = grid::unit(0.5, "lines"),
      legend.spacing.x  = grid::unit(0.2, "lines"),
      legend.background = element_rect(fill = "grey93", color = NA)
    ) +
    guides(color = guide_legend(title.position = "left", nrow = 1, byrow = TRUE))
}

ggsave(paste0(Directory, "\\Figure_3_A.png"),
       plot = build_placebo_plot(Figure_3_a, "", common_nrow),
       width = fig_width, height = fig_height, dpi = 300)
ggsave(paste0(Directory, "\\Figure_3_B.png"),
       plot = build_placebo_plot(Figure_3_b, "", common_nrow),
       width = fig_width, height = fig_height, dpi = 300)

Figure_3_ireland <- Figure_3 %>% filter(region == "Ireland")
ggsave(paste0(Directory, "\\Figure_3_Ireland.png"),
       plot = build_placebo_plot(Figure_3_ireland, "", 1) +
         theme(
           legend.position      = "top",
           legend.justification = "right",
           legend.direction     = "horizontal",
           legend.title         = element_text(size = 9,  face = "bold"),
           legend.text          = element_text(size = 8,  face = "bold"),
           legend.key.width     = grid::unit(0.4, "lines"),
           legend.key.height    = grid::unit(0.3, "lines")
         ),
       width = fig_width / 3, height = fig_height / common_nrow, dpi = 300)


# ---- Figure 4 ----

Figure_4 <- read.xlsx(paste0(Directory, "\\HighBailout_Subsample_Results.xlsx"))

figure_4_regions <- sort(unique(Figure_4$region))
Figure_4 <- Figure_4 %>% filter(region %in% figure_4_regions)

figure_4_limits <- Figure_4 %>%
  group_by(region) %>%
  summarise(max_abs = max(c(abs(gap), abs(baseline_gap)), na.rm = TRUE), .groups = "drop")

Figure_4 <- Figure_4 %>% left_join(figure_4_limits, by = "region")

combined_plot_4 <- ggplot(Figure_4, aes(x = .data$year)) +
  geom_blank(aes(y =  .data$max_abs)) +
  geom_blank(aes(y = -.data$max_abs)) +
  geom_line(aes(y = .data$gap,          color = "New gap"), linewidth = 1.1, linetype = "dotdash") +
  geom_line(aes(y = .data$baseline_gap, color = "Old gap"), linewidth = 1.1) +
  geom_vline(xintercept = 2010, linetype = "dotted", color = "black", size = 0.8) +
  facet_wrap(~ region, scales = "free_y", nrow = 3, ncol = 3,
             axes = "all_x", axis.labels = "all_x") +
  scale_color_manual(values = c("New gap" = "red3", "Old gap" = "black")) +
  labs(x = "Year", y = "FDI Z-Score Gap", color = "Series:") +
  theme_bw(base_family = "Times New Roman") +
  theme(
    plot.background  = element_rect(fill = "grey93", color = "black", linewidth = 0.8),
    strip.background = element_rect(fill = "grey93", color = NA),
    panel.background = element_rect(fill = "white",  color = "grey80"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text   = element_text(size = 12),
    axis.text    = element_text(size = 11),
    axis.title   = element_text(size = 12),
    panel.spacing = grid::unit(0.8, "lines"),
    plot.margin   = margin(t = 8, r = 12, b = 10, l = 10),
    legend.position = "none"
  )

ggsave(paste0(Directory, "\\Figure_4.png"),
       plot = combined_plot_4, width = fig_width, height = fig_height, dpi = 300)


# ---- Figure 5 ----

jackknife <- read.xlsx(paste0(Directory, "\\Country_Jackknife_results.xlsx")) %>%
  filter(region %in% Bailout_Regions) %>%
  left_join(Figure_2 %>% select(year, region, original_gap = gap), by = c("year", "region")) %>%
  mutate(region = factor(region, levels = panel_order))

Figure_5_a <- jackknife %>% filter(region %in% regions_a)
Figure_5_b <- jackknife %>% filter(region %in% regions_b)

build_jackknife_plot <- function(df, plot_title, nrow_value) {
  ggplot(df, aes(x = .data$year)) +
    geom_line(aes(y = .data$gap, group = .data$dropped_country, color = "Jackknife"),
              linewidth = 0.5, alpha = 0.8) +
    geom_line(aes(y = .data$original_gap, color = "Actual"), linewidth = 1.2) +
    geom_vline(xintercept = 2010, linetype = "dotted", color = "black", size = 0.8) +
    facet_wrap(~ region, scales = "free_y", nrow = nrow_value, ncol = 3,
               axes = "all_x", axis.labels = "all_x") +
    scale_color_manual(values = c("Actual" = "black", "Jackknife" = "steelblue")) +
    labs(x = "Year", y = "FDI Z-Score Gap (Actual - Synthetic)", color = "Series:") +
    theme_bw(base_family = "Times New Roman") +
    theme(
      plot.background  = element_rect(fill = "grey93", color = "black", linewidth = 0.8),
      strip.background = element_rect(fill = "grey93", color = NA),
      panel.background = element_rect(fill = "white",  color = "grey80"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text   = element_text(size = 13, face = "bold"),
      axis.text    = element_text(size = 12, face = "bold"),
      axis.title   = element_text(size = 13, face = "bold"),
      panel.spacing = grid::unit(0.8, "lines"),
      plot.margin   = margin(t = 8, r = 12, b = 10, l = 10),
      legend.position    = c(0.99, 0.05),
      legend.direction   = "horizontal",
      legend.justification = c(1, 0),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 11, face = "bold"),
      legend.key.width  = grid::unit(0.7, "lines"),
      legend.key.height = grid::unit(0.5, "lines"),
      legend.spacing.x  = grid::unit(0.2, "lines"),
      legend.background = element_rect(fill = "grey93", color = NA)
    ) +
    guides(color = guide_legend(title.position = "left", nrow = 1, byrow = TRUE))
}

ggsave(paste0(Directory, "\\Figure_5_A.png"),
       plot = build_jackknife_plot(Figure_5_a, "", common_nrow),
       width = fig_width, height = fig_height, dpi = 300)
ggsave(paste0(Directory, "\\Figure_5_B.png"),
       plot = build_jackknife_plot(Figure_5_b, "", common_nrow),
       width = fig_width, height = fig_height, dpi = 300)

Figure_5_Iceland <- jackknife %>% filter(region == "Iceland")
ggsave(paste0(Directory, "\\Figure_5_Iceland.png"),
       plot = build_jackknife_plot(Figure_5_Iceland, "", 1) +
         theme(
           legend.position      = "top",
           legend.justification = "right",
           legend.direction     = "horizontal",
           legend.title         = element_text(size = 9,  face = "bold"),
           legend.text          = element_text(size = 8,  face = "bold"),
           legend.key.width     = grid::unit(0.4, "lines"),
           legend.key.height    = grid::unit(0.3, "lines")
         ),
       width = fig_width / 3, height = fig_height / common_nrow, dpi = 300)


# ---- Figure 6 ----

Figure_6 <- read.xlsx(paste0(Directory, "\\Country_Placebo_InTime_results.xlsx")) %>%
  filter(region %in% Bailout_Regions) %>%
  mutate(region = factor(region, levels = panel_order))

actual_col_candidates    <- c("actual", "actual_fdi", "actual_series")
synthetic_col_candidates <- c("synthetic", "synthetic_placebo", "synthetic_2005", "synthetic_series")

actual_col    <- actual_col_candidates[actual_col_candidates %in% names(Figure_6)][1]
synthetic_col <- synthetic_col_candidates[synthetic_col_candidates %in% names(Figure_6)][1]

if (is.na(actual_col) || is.na(synthetic_col)) {
  stop("Figure 6 requires actual and synthetic columns in Country_Placebo_InTime_results.xlsx")
}

Figure_6 <- Figure_6 %>%
  transmute(
    year, region,
    actual    = as.numeric(.data[[actual_col]]),
    synthetic = as.numeric(.data[[synthetic_col]])
  )

Figure_6_a <- Figure_6 %>% filter(region %in% regions_a)
Figure_6_b <- Figure_6 %>% filter(region %in% regions_b)

build_intime_actual_synth_plot <- function(df, plot_title, nrow_value) {
  ggplot(df, aes(x = .data$year)) +
    geom_line(aes(y = .data$actual,    color = "Actual"),    linewidth = 1.2) +
    geom_line(aes(y = .data$synthetic, color = "Synthetic"), linewidth = 0.9, linetype = "dashed") +
    geom_vline(xintercept = 2005, linetype = "dotted", color = "black", size = 0.8) +
    facet_wrap(~ region, scales = "free_y", nrow = nrow_value, ncol = 3,
               axes = "all_x", axis.labels = "all_x") +
    scale_color_manual(values = c("Actual" = "black", "Synthetic" = "grey50")) +
    labs(x = "Year", y = "FDI Z-Score (3 Year Moving Average)", color = "Series:") +
    theme_bw(base_family = "Times New Roman") +
    theme(
      plot.background  = element_rect(fill = "grey93", color = "black", linewidth = 0.8),
      strip.background = element_rect(fill = "grey93", color = NA),
      panel.background = element_rect(fill = "white",  color = "grey80"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text   = element_text(size = 13, face = "bold"),
      axis.text    = element_text(size = 12, face = "bold"),
      axis.title   = element_text(size = 13, face = "bold"),
      panel.spacing = grid::unit(0.8, "lines"),
      plot.margin   = margin(t = 8, r = 12, b = 10, l = 10),
      legend.position    = c(0.99, 0.05),
      legend.direction   = "horizontal",
      legend.justification = c(1, 0),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 11, face = "bold"),
      legend.key.width  = grid::unit(0.7, "lines"),
      legend.key.height = grid::unit(0.5, "lines"),
      legend.spacing.x  = grid::unit(0.2, "lines"),
      legend.background = element_rect(fill = "grey93", color = NA)
    ) +
    guides(color = guide_legend(title.position = "left", nrow = 1, byrow = TRUE))
}

ggsave(paste0(Directory, "\\Figure_6_A.png"),
       plot = build_intime_actual_synth_plot(Figure_6_a, "", common_nrow),
       width = fig_width, height = fig_height, dpi = 300)
ggsave(paste0(Directory, "\\Figure_6_B.png"),
       plot = build_intime_actual_synth_plot(Figure_6_b, "", common_nrow),
       width = fig_width, height = fig_height, dpi = 300)

Figure_6_Portugal <- Figure_6 %>% filter(region == "Portugal")
ggsave(paste0(Directory, "\\Figure_6_Portugal.png"),
       plot = build_intime_actual_synth_plot(Figure_6_Portugal, "", 1) +
         theme(
           legend.position      = "top",
           legend.justification = "right",
           legend.direction     = "horizontal",
           legend.title         = element_text(size = 9,  face = "bold"),
           legend.text          = element_text(size = 8,  face = "bold"),
           legend.key.width     = grid::unit(0.4, "lines"),
           legend.key.height    = grid::unit(0.3, "lines")
         ),
       width = fig_width / 3, height = fig_height / common_nrow, dpi = 300)

cat("\nMasterFinal.R complete. All outputs saved to:", Directory, "\n")
  

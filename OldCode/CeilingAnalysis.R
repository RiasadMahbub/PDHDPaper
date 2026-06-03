# =========================================================
# CEILING ANALYSIS
# What is the irreducible error given your data structure?
# =========================================================

# 1. Best possible RMSE by year size
cat("--- Year sizes and their contribution to error ---\n")
year_stats <- df_pd %>%
  group_by(Year) %>%
  summarise(
    n          = n(),
    PDDOY_mean = mean(PDDOY),
    PDDOY_sd   = sd(PDDOY),
    PDDOY_range = diff(range(PDDOY)),
    .groups = "drop"
  ) %>%
  left_join(
    bind_rows(
      results %>% dplyr::select(year = yr1, RF_Test_RMSE),
      results %>% dplyr::select(year = yr2, RF_Test_RMSE)
    ) %>%
      group_by(year) %>%
      summarise(mean_RMSE = mean(RF_Test_RMSE), .groups = "drop"),
    by = c("Year" = "year")
  )

print(as.data.frame(year_stats))

# 2. Correlation: does year size predict RMSE?
cat("\nCorrelation between year n and mean RMSE:",
    round(cor(year_stats$n, year_stats$mean_RMSE, use = "complete"), 3), "\n")
cat("Correlation between PDDOY_sd and mean RMSE:",
    round(cor(year_stats$PDDOY_sd, year_stats$mean_RMSE, use = "complete"), 3), "\n")
cat("Correlation between yr_tmin_anom and mean RMSE:",
    round(cor(
      year_climate$yr_tmin_anom,
      year_stats$mean_RMSE[match(year_climate$Year, year_stats$Year)],
      use = "complete"), 3), "\n")

# 3. What RMSE band is achievable?
cat("\n--- RMSE Distribution Across 45 Pairs ---\n")
cat("Min RMSE:    ", round(min(results$RF_Test_RMSE), 2), "\n")
cat("Median RMSE: ", round(median(results$RF_Test_RMSE), 2), "\n")
cat("Mean RMSE:   ", round(mean(results$RF_Test_RMSE), 2), "\n")
cat("Max RMSE:    ", round(max(results$RF_Test_RMSE), 2), "\n")
cat("Pairs < 12:  ", sum(results$RF_Test_RMSE < 12), "of 45\n")
cat("Pairs < 13:  ", sum(results$RF_Test_RMSE < 13), "of 45\n")
cat("Pairs < 14:  ", sum(results$RF_Test_RMSE < 14), "of 45\n")

# 4. Observed PDDOY variance per year
# If within-year PDDOY sd is already ~10-15 days,
# RMSE cannot get below that floor
cat("\n--- Within-Year PDDOY Spread (irreducible noise floor) ---\n")
df_pd %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    sd_PDDOY   = round(sd(PDDOY), 1),
    range_PDDOY = diff(range(PDDOY)),
    .groups = "drop"
  ) %>%
  as.data.frame() %>%
  print()

# 5. Observed vs predicted scatter for best and worst pairs
best_pair  <- results$test_years[which.min(results$RF_Test_RMSE)]
worst_pair <- results$test_years[which.max(results$RF_Test_RMSE)]

cat("\nBest pair:", best_pair,
    "RMSE:", round(min(results$RF_Test_RMSE), 2), "\n")
cat("Worst pair:", worst_pair,
    "RMSE:", round(max(results$RF_Test_RMSE), 2), "\n")

# Refit best pair for scatter plot
best_years  <- as.numeric(strsplit(best_pair,  "-")[[1]])
worst_years <- as.numeric(strsplit(worst_pair, "-")[[1]])

make_scatter <- function(test_yrs, label) {
  tr <- df_pd %>% filter(!Year %in% test_yrs)
  te <- df_pd %>% filter( Year %in% test_yrs)
  m  <- randomForest(PDDOY ~ . - Year, data = tr,
                     ntree = 300, mtry = best_mtry,
                     nodesize = best_nodesize, maxnodes = 40)
  pred <- predict(m, te)
  data.frame(
    obs   = te$PDDOY,
    pred  = pred,
    Year  = te$Year,
    label = label
  )
}

scatter_best  <- make_scatter(best_years,  paste("Best:",  best_pair))
scatter_worst <- make_scatter(worst_years, paste("Worst:", worst_pair))
scatter_all   <- bind_rows(scatter_best, scatter_worst)

ggplot(scatter_all, aes(x = obs, y = pred, color = factor(Year))) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "white") +
  facet_wrap(~label, scales = "free") +
  scale_color_viridis_d(option = "plasma") +
  labs(title = "Observed vs Predicted PDDOY",
       subtitle = "Best and worst year pairs",
       x = "Observed PDDOY", y = "Predicted PDDOY",
       color = "Year") +
  theme_minimal(base_size = 12)

# 6. Are predictions biased for hard years?
cat("\n--- Bias Check for Hardest Years ---\n")
bias_check <- data.frame()

for (yr in c(2017, 2021, 2022, 2024)) {
  # Use all other years to train, predict this year
  tr   <- df_pd %>% filter(Year != yr)
  te   <- df_pd %>% filter(Year == yr)
  m    <- randomForest(PDDOY ~ . - Year, data = tr,
                       ntree = 300, mtry = best_mtry,
                       nodesize = best_nodesize, maxnodes = 40)
  pred <- predict(m, te)
  bias_check <- rbind(bias_check, data.frame(
    Year      = yr,
    n         = nrow(te),
    obs_mean  = round(mean(te$PDDOY), 1),
    pred_mean = round(mean(pred), 1),
    bias      = round(mean(pred) - mean(te$PDDOY), 1),
    RMSE      = round(hydroGOF::rmse(pred, te$PDDOY), 2)
  ))
}

print(as.data.frame(bias_check))

# 7. Final summary plot — RMSE vs test year pair index
results_sorted <- results %>%
  arrange(RF_Test_RMSE) %>%
  mutate(pair_index = row_number())

ggplot(results_sorted,
       aes(x = pair_index, y = RF_Test_RMSE,
           color = RF_Test_RMSE)) +
  geom_point(size = 3) +
  geom_hline(yintercept = mean(results$RF_Test_RMSE),
             linetype = "dashed", color = "orange") +
  geom_hline(yintercept = 10,
             linetype = "dotted", color = "green") +
  scale_color_viridis_c(option = "plasma") +
  annotate("text", x = 5, y = mean(results$RF_Test_RMSE) + 0.3,
           label = paste("Mean =", round(mean(results$RF_Test_RMSE), 1)),
           color = "orange", size = 3.5) +
  annotate("text", x = 5, y = 10.3,
           label = "Target = 10", color = "green", size = 3.5) +
  labs(title = "RMSE Distribution Across All 45 Year Pairs",
       x = "Pair rank (best → worst)",
       y = "RF Test RMSE") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
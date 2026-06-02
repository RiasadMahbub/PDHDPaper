# =========================================================
# FIX: Ensure all summary objects have complete metrics
# Run this BEFORE building the tables
# =========================================================

# ---- Fix pd_random: ensure Val_Bias_mean exists ----
if (!"Val_Bias_mean" %in% names(pd_random$summary)) {
  pd_random$summary$Val_Bias_mean <- round(
    mean(pd_random$per_run$Val_Bias, na.rm=TRUE), 2)
  pd_random$summary$Val_Bias_sd <- round(
    sd(pd_random$per_run$Val_Bias, na.rm=TRUE), 2)
}
if (!"Train_Bias_mean" %in% names(pd_random$summary)) {
  pd_random$summary$Train_Bias_mean <- round(
    mean(pd_random$per_run$Train_Bias, na.rm=TRUE), 2)
}
if (!"Val_R2_mean" %in% names(pd_random$summary)) {
  pd_random$summary$Val_R2_mean <- round(
    mean(pd_random$per_run$Val_R2, na.rm=TRUE), 3)
}

# ---- Fix hd_random ----
if (!"Val_Bias_mean" %in% names(hd_random$summary)) {
  hd_random$summary$Val_Bias_mean <- round(
    mean(hd_random$per_run$Val_Bias, na.rm=TRUE), 2)
}
if (!"Train_Bias_mean" %in% names(hd_random$summary)) {
  hd_random$summary$Train_Bias_mean <- round(
    mean(hd_random$per_run$Train_Bias, na.rm=TRUE), 2)
}
if (!"Val_R2_mean" %in% names(hd_random$summary)) {
  hd_random$summary$Val_R2_mean <- round(
    mean(hd_random$per_run$Val_R2, na.rm=TRUE), 3)
}

# ---- Fix pd_final: ensure Bias exists ----
if (!"Train_Bias" %in% names(pd_final$summary)) {
  pd_final$summary$Train_Bias <- round(
    mean(pd_final$results$RF_Train_Bias, na.rm=TRUE), 2)
}
if (!"Test_Bias" %in% names(pd_final$summary)) {
  pd_final$summary$Test_Bias <- round(
    mean(pd_final$results$RF_Test_Bias, na.rm=TRUE), 2)
}
if (!"Test_MAE_sd" %in% names(pd_final$summary)) {
  pd_final$summary$Test_MAE_sd <- round(
    sd(pd_final$results$RF_Test_MAE, na.rm=TRUE), 2)
}

# ---- Fix hd_final ----
if (!"Train_Bias" %in% names(hd_final$summary)) {
  hd_final$summary$Train_Bias <- round(
    mean(hd_final$results$RF_Train_Bias, na.rm=TRUE), 2)
}
if (!"Test_Bias" %in% names(hd_final$summary)) {
  hd_final$summary$Test_Bias <- round(
    mean(hd_final$results$RF_Test_Bias, na.rm=TRUE), 2)
}
if (!"Test_MAE_sd" %in% names(hd_final$summary)) {
  hd_final$summary$Test_MAE_sd <- round(
    sd(hd_final$results$RF_Test_MAE, na.rm=TRUE), 2)
}

# ---- Fix sosder_summary: ensure all metrics ----
if (!"Train_Bias_mean" %in% names(sosder_summary)) {
  sosder_summary$Train_Bias_mean <- round(
    mean(results$Train_Bias, na.rm=TRUE), 2)
}
if (!"Test_Bias_mean" %in% names(sosder_summary)) {
  sosder_summary$Test_Bias_mean <- round(
    mean(results$Test_Bias, na.rm=TRUE), 2)
}
if (!"Test_MAE_sd" %in% names(sosder_summary)) {
  sosder_summary$Test_MAE_sd <- round(
    sd(results$Test_MAE, na.rm=TRUE), 2)
}
if (!"Train_R2_mean" %in% names(sosder_summary)) {
  sosder_summary$Train_R2_mean <- round(
    mean(results$Train_R2, na.rm=TRUE), 3)
}

# ---- Fix deines summaries ----
if (!"Train_Bias" %in% names(deines_pd_l2yo$summary)) {
  deines_pd_l2yo$summary$Train_Bias <- round(
    mean(deines_pd_l2yo$results$RF_Train_Bias, na.rm=TRUE), 2)
}
if (!"Test_Bias" %in% names(deines_pd_l2yo$summary)) {
  deines_pd_l2yo$summary$Test_Bias <- round(
    mean(deines_pd_l2yo$results$RF_Test_Bias, na.rm=TRUE), 2)
}
if (!"Train_Bias" %in% names(deines_hd_l2yo$summary)) {
  deines_hd_l2yo$summary$Train_Bias <- round(
    mean(deines_hd_l2yo$results$RF_Train_Bias, na.rm=TRUE), 2)
}
if (!"Test_Bias" %in% names(deines_hd_l2yo$summary)) {
  deines_hd_l2yo$summary$Test_Bias <- round(
    mean(deines_hd_l2yo$results$RF_Test_Bias, na.rm=TRUE), 2)
}

# ---- Fix PDSOSDER random split metrics ----
if (!"Test_Bias" %in% names(summary_metrics_df)) {
  summary_metrics_df$Test_Bias <- NA_real_
}
if (!"Val_Bias_Mean" %in% names(summary_metrics_df)) {
  summary_metrics_df$Val_Bias_Mean <- round(
    summary_metrics_df$Val_Bias_Mean %||% NA_real_, 2)
}

# ---- Fix Deines random planting ----
if (!"Bias" %in% names(test_results_planting)) {
  test_results_planting$Bias <- NA_real_
}
if (!"Val_Bias_mean" %in% names(summary_metrics_planting)) {
  summary_metrics_planting$Val_Bias_mean <-
    summary_metrics_planting$Val_Bias_mean %||% NA_real_
}

# ---- Fix Deines random harvest ----
if (!"Bias" %in% names(test_results_harvest)) {
  test_results_harvest$Bias <- NA_real_
}

cat("✓ All metric fixes applied\n")

# Helper for missing values
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 &&
                             !is.na(a[1])) a else b

# =========================================================
# HELPER FUNCTIONS
# =========================================================

fmt <- function(mean_val, sd_val=NA, digits=2) {
  if (length(mean_val)==0 || is.null(mean_val) ||
      all(is.na(mean_val))) return("—")
  mean_val <- mean_val[1]
  if (is.na(mean_val)) return("—")
  if (!missing(sd_val) && !is.null(sd_val) &&
      length(sd_val)>0 && !is.na(sd_val[1])) {
    return(sprintf("%.*f ± %.*f", digits, mean_val,
                   digits, sd_val[1]))
  }
  sprintf("%.*f", digits, mean_val)
}

fmt3 <- function(val) {
  if (length(val)==0 || is.null(val) ||
      all(is.na(val))) return("—")
  val <- val[1]
  if (is.na(val)) return("—")
  sprintf("%.3f", val)
}

# =========================================================
# BUILD PDDOY TABLE
# =========================================================

pddoy_rows <- list()

pddoy_rows[["LIMP RF (Random Split)"]] <- data.frame(
  Model="LIMP RF", Split="Random Split", Valid="❌",
  Train_RMSE = fmt(pd_random$summary$Train_RMSE_mean,
                   pd_random$summary$Train_RMSE_sd),
  Train_MAE  = fmt(pd_random$summary$Train_MAE_mean,
                   pd_random$summary$Train_MAE_sd),
  Train_R2   = fmt(pd_random$summary$Train_R2_mean,
                    pd_random$summary$Train_R2_sd),
  Train_MBD  = fmt(pd_random$summary$Train_MBE_mean,
                   pd_random$summary$Train_MBE_sd),
  Val_RMSE   = fmt(pd_random$summary$Val_RMSE_mean,
                   pd_random$summary$Val_RMSE_sd),
  Val_MAE    = fmt(pd_random$summary$Val_MAE_mean,
                   pd_random$summary$Val_MAE_sd),
  Val_R2     = fmt(pd_random$summary$Val_R2_mean,
                    pd_random$summary$Val_R2_sd),
  Val_MBD    = fmt(pd_random$summary$Val_MBE_mean,
                   pd_random$summary$Val_MBE_sd),
  Test_RMSE  = fmt(pd_random$test$Test_RMSE),
  Test_MAE   = fmt(pd_random$test$Test_MAE),
  Test_R2    = fmt3(pd_random$test$Test_R2),
  Test_MBD   = fmt(pd_random$test$Test_MBE),
  stringsAsFactors=FALSE)

pddoy_rows[["LIMP RF (L2YO)"]] <- data.frame(
  Model="LIMP RF", Split="L2YO", Valid="✅",
  Train_RMSE = fmt(pd_final$summary$Train_RMSE),
  Train_MAE  = fmt(pd_final$summary$Train_MAE),
  Train_R2   = fmt3(pd_final$summary$Train_R2),
  Train_MBD  = fmt(pd_final$summary$Train_MBE),
  Val_RMSE="—", Val_MAE="—", Val_R2="—", Val_MBD="—",
  Test_RMSE  = fmt(pd_final$summary$Test_RMSE,
                   pd_final$summary$Test_RMSE_sd),
  Test_MAE   = fmt(pd_final$summary$Test_MAE,
                   pd_final$summary$Test_MAE_sd),
  Test_R2    = fmt3(pd_final$summary$Test_R2),
  Test_MBD   = fmt(pd_final$summary$Test_MBE),
  stringsAsFactors=FALSE)

# pddoy_rows[["PDSOSDER (Random Split)"]] <- data.frame(
#   Model="PDSOSDER", Split="Random Split", Valid="❌",
#   Train_RMSE = fmt(summary_metrics_df$Train_RMSE_Mean,
#                    summary_metrics_df$Train_RMSE_SD),
#   Train_MAE  = fmt(summary_metrics_df$Train_MAE_Mean),
#   Train_R2   = fmt3(summary_metrics_df$Train_R2_Mean),
#   Train_MBD  = fmt(summary_metrics_df$Train_Bias_Mean),
#   Val_RMSE   = fmt(summary_metrics_df$Val_RMSE_Mean,
#                    summary_metrics_df$Val_RMSE_SD),
#   Val_MAE    = fmt(summary_metrics_df$Val_MAE_Mean),
#   Val_R2     = fmt3(summary_metrics_df$Val_R2_Mean),
#   Val_MBD    = fmt(summary_metrics_df$Val_Bias_Mean),
#   Test_RMSE  = fmt(summary_metrics_df$Test_RMSE),
#   Test_MAE   = fmt(summary_metrics_df$Test_MAE),
#   Test_R2    = fmt3(summary_metrics_df$Test_R2),
#   Test_MBD   = fmt(summary_metrics_df$Test_Bias),
#   stringsAsFactors=FALSE)

pddoy_rows[["PDSOSDER (L2YO)"]] <- data.frame(
  Model="PDSOSDER", Split="L2YO", Valid="✅",
  Train_RMSE = fmt(sosder_summary$Train_RMSE_mean),
  Train_MAE  = fmt(sosder_summary$Train_MAE_mean),
  Train_R2   = fmt3(sosder_summary$Train_R2_mean),
  Train_MBD  = fmt(sosder_summary$Train_Bias_mean),
  Val_RMSE="—", Val_MAE="—", Val_R2="—", Val_MBD="—",
  Test_RMSE  = fmt(sosder_summary$Test_RMSE_mean,
                   sosder_summary$Test_RMSE_sd),
  Test_MAE   = fmt(sosder_summary$Test_MAE_mean,
                   sosder_summary$Test_MAE_sd),
  Test_R2    = fmt3(sosder_summary$Test_R2_mean),
  Test_MBD   = fmt(sosder_summary$Test_Bias_mean),
  stringsAsFactors=FALSE)

# pddoy_rows[["Deines RF (Random Split)"]] <- data.frame(
#   Model="Deines RF", Split="Random Split", Valid="❌",
#   Train_RMSE = fmt(summary_metrics_planting$Train_RMSE_mean,
#                    summary_metrics_planting$Train_RMSE_sd),
#   Train_MAE  = fmt(summary_metrics_planting$Train_MAE_mean),
#   Train_R2   = fmt3(summary_metrics_planting$Train_R2_mean),
#   Train_MBD  = fmt(summary_metrics_planting$Train_Bias_mean),
#   Val_RMSE   = fmt(summary_metrics_planting$Val_RMSE_mean,
#                    summary_metrics_planting$Val_RMSE_sd),
#   Val_MAE    = fmt(summary_metrics_planting$Val_MAE_mean),
#   Val_R2     = fmt3(summary_metrics_planting$Val_R2_mean),
#   Val_MBD    = fmt(summary_metrics_planting$Val_Bias_mean),
#   Test_RMSE  = fmt(test_results_planting$RMSE),
#   Test_MAE   = fmt(test_results_planting$MAE),
#   Test_R2    = fmt3(test_results_planting$R2),
#   Test_MBD   = fmt(test_results_planting$Bias),
#   stringsAsFactors=FALSE)

pddoy_rows[["Deines RF (L2YO)"]] <- data.frame(
  Model="Deines RF", Split="L2YO", Valid="✅",
  Train_RMSE = fmt(deines_pd_l2yo$summary$Train_RMSE),
  Train_MAE  = fmt(deines_pd_l2yo$summary$Train_MAE),
  Train_R2   = fmt3(deines_pd_l2yo$summary$Train_R2),
  Train_MBD  = fmt(deines_pd_l2yo$summary$Train_Bias),
  Val_RMSE="—", Val_MAE="—", Val_R2="—", Val_MBD="—",
  Test_RMSE  = fmt(deines_pd_l2yo$summary$Test_RMSE,
                   deines_pd_l2yo$summary$Test_RMSE_sd),
  Test_MAE   = fmt(deines_pd_l2yo$summary$Test_MAE,
                   deines_pd_l2yo$summary$Test_MAE_sd),
  Test_R2    = fmt3(deines_pd_l2yo$summary$Test_R2),
  Test_MBD   = fmt(deines_pd_l2yo$summary$Test_Bias),
  stringsAsFactors=FALSE)

pddoy_table <- bind_rows(pddoy_rows)

cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║                  PDDOY MASTER TABLE                          ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
print(as.data.frame(pddoy_table))
pddoy_table
# =========================================================
# BUILD HDDOY TABLE
# =========================================================

hddoy_rows <- list()

hddoy_rows[["LIMP RF (Random Split)"]] <- data.frame(
  Model="LIMP RF", Split="Random Split", Valid="❌",
  Train_RMSE = fmt(hd_random$summary$Train_RMSE_mean,
                   hd_random$summary$Train_RMSE_sd),
  Train_MAE  = fmt(hd_random$summary$Train_MAE_mean),
  Train_R2   = fmt3(hd_random$summary$Train_R2_mean),
  Train_MBD  = fmt(hd_random$summary$Train_MBE_mean),
  Val_RMSE   = fmt(hd_random$summary$Val_RMSE_mean,
                   hd_random$summary$Val_RMSE_sd),
  Val_MAE    = fmt(hd_random$summary$Val_MAE_mean),
  Val_R2     = fmt3(hd_random$summary$Val_R2_mean),
  Val_MBD    = fmt(hd_random$summary$Val_Bias_mean),
  Test_RMSE  = fmt(hd_random$test$Test_RMSE),
  Test_MAE   = fmt(hd_random$test$Test_MAE),
  Test_R2    = fmt3(hd_random$test$Test_R2),
  Test_MBD   = fmt(hd_random$test$Test_MBE),
  stringsAsFactors=FALSE)

hddoy_rows[["LIMP RF (L2YO)"]] <- data.frame(
  Model="LIMP RF", Split="L2YO", Valid="✅",
  Train_RMSE = fmt(hd_final$summary$Train_RMSE),
  Train_MAE  = fmt(hd_final$summary$Train_MAE),
  Train_R2   = fmt3(hd_final$summary$Train_R2),
  Train_MBD  = fmt(hd_final$summary$Train_MBE),
  Val_RMSE="—", Val_MAE="—", Val_R2="—", Val_MBD="—",
  Test_RMSE  = fmt(hd_final$summary$Test_RMSE,
                   hd_final$summary$Test_RMSE_sd),
  Test_MAE   = fmt(hd_final$summary$Test_MAE,
                   hd_final$summary$Test_MAE_sd),
  Test_R2    = fmt3(hd_final$summary$Test_R2),
  Test_MBD  = fmt(hd_final$summary$Test_MBE),
  stringsAsFactors=FALSE)

# hddoy_rows[["Deines RF (Random Split)"]] <- data.frame(
#   Model="Deines RF", Split="Random Split", Valid="❌",
#   Train_RMSE = fmt(summary_metrics_harvest$Train_RMSE_mean,
#                    summary_metrics_harvest$Train_RMSE_sd),
#   Train_MAE  = fmt(summary_metrics_harvest$Train_MAE_mean),
#   Train_R2   = fmt3(summary_metrics_harvest$Train_R2_mean),
#   Train_MBD  = fmt(summary_metrics_harvest$Train_Bias_mean),
#   Val_RMSE   = fmt(summary_metrics_harvest$Val_RMSE_mean,
#                    summary_metrics_harvest$Val_RMSE_sd),
#   Val_MAE    = fmt(summary_metrics_harvest$Val_MAE_mean),
#   Val_R2     = fmt3(summary_metrics_harvest$Val_R2_mean),
#   Val_MBD    = fmt(summary_metrics_harvest$Val_Bias_mean),
#   Test_RMSE  = fmt(test_results_harvest$RMSE),
#   Test_MAE   = fmt(test_results_harvest$MAE),
#   Test_R2    = fmt3(test_results_harvest$R2),
#   Test_MBD   = fmt(test_results_harvest$Bias),
#   stringsAsFactors=FALSE)

# hddoy_rows[["Deines RF (L2YO)"]] <- data.frame(
#   Model="Deines RF", Split="L2YO", Valid="✅",
#   Train_RMSE = fmt(deines_hd_l2yo$summary$Train_RMSE),
#   Train_MAE  = fmt(deines_hd_l2yo$summary$Train_MAE),
#   Train_R2   = fmt3(deines_hd_l2yo$summary$Train_R2),
#   Train_MBD  = fmt(deines_hd_l2yo$summary$Train_Bias),
#   Val_RMSE="—", Val_MAE="—", Val_R2="—", Val_MBD="—",
#   Test_RMSE  = fmt(deines_hd_l2yo$summary$Test_RMSE,
#                    deines_hd_l2yo$summary$Test_RMSE_sd),
#   Test_MAE   = fmt(deines_hd_l2yo$summary$Test_MAE,
#                    deines_hd_l2yo$summary$Test_MAE_sd),
#   Test_R2    = fmt3(deines_hd_l2yo$summary$Test_R2),
#   Test_MBD   = fmt(deines_hd_l2yo$summary$Test_Bias),
#   stringsAsFactors=FALSE)

hddoy_table <- bind_rows(hddoy_rows)

cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║                  HDDOY MASTER TABLE                          ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
print(as.data.frame(hddoy_table))
hddoy_table
pddoy_table
# =========================================================
# SAVE CSVs
# =========================================================
library(readr)

out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"

# Use write_excel_csv instead of write.csv
write_excel_csv(pddoy_table, file.path(out_dir, "PDDOY_master_table.csv"))
write_excel_csv(hddoy_table, file.path(out_dir, "HDDOY_master_table.csv"))

cat("\n✓ Tables saved cleanly for Excel!\n")
out_dir <- "C:/Users/rbmahbub/Documents/RProjects/DOPDOHYIELD/Figure/ManuscriptFigure"
write.csv(
  pddoy_table,
  file.path(out_dir, "PDDOY_master_table.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  hddoy_table,
  file.path(out_dir, "HDDOY_master_table.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
cat("\n✓ Tables saved\n")
# Replace problematic Unicode characters

library(dplyr)
manuscript_table <- pddoy_clean %>%
  gt() %>%
  # 1. Add the spanning multi-column major headers
  tab_spanner(
    label = "Training",
    columns = c(Train_RMSE, Train_MAE, Train_R2, Train_MBE)
  ) %>%
  tab_spanner(
    label = "Validation",
    columns = c(Val_RMSE, Val_MAE, Val_R2, Val_MBE)
  ) %>%
  tab_spanner(
    label = "Testing",
    columns = c(Test_RMSE, Test_MAE, Test_R2, Test_MBE)
  ) %>%
  # 2. Clean up the sub-header names so they just show RMSE, MAE, etc.
  cols_label(
    Train_RMSE = "RMSE", Train_MAE = "MAE", Train_R2 = "R²", Train_MBE = "MBE",
    Val_RMSE   = "RMSE", Val_MAE   = "MAE", Val_R2   = "R²", Val_MBE   = "MBE",
    Test_RMSE  = "RMSE", Test_MAE  = "MAE", Test_R2  = "R²", Test_MBE  = "MBE"
  ) %>%
  # 3. Apply standard APA/Manuscript clean styling
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    table_font.size = "small",
    column_labels.border.bottom.color = "black",
    column_labels.border.top.color = "black"
  )

# Print the beautiful table
manuscript_table

# =========================================================
# NUMERIC VERSION FOR PLOTTING
# =========================================================

build_numeric_row <- function(model, split, valid,
                              train_rmse, train_rmse_sd=NA, train_mae, train_r2, train_mbd,
                              val_rmse=NA, val_rmse_sd=NA, val_mae=NA, val_r2=NA, val_mbd=NA,
                              test_rmse, test_rmse_sd=NA, test_mae, test_mae_sd=NA,
                              test_r2, test_mbd) {
  data.frame(
    Model=model, Split=split, Valid=valid,
    Train_RMSE=as.numeric(train_rmse[1]),
    Train_RMSE_sd=as.numeric(train_rmse_sd[1]),
    Train_MAE=as.numeric(train_mae[1]),
    Train_R2=as.numeric(train_r2[1]),
    Train_MBD=as.numeric(train_mbd[1]),
    Val_RMSE=as.numeric(val_rmse[1]),
    Val_RMSE_sd=as.numeric(val_rmse_sd[1]),
    Val_MAE=as.numeric(val_mae[1]),
    Val_R2=as.numeric(val_r2[1]),
    Val_MBD=as.numeric(val_mbd[1]),
    Test_RMSE=as.numeric(test_rmse[1]),
    Test_RMSE_sd=as.numeric(test_rmse_sd[1]),
    Test_MAE=as.numeric(test_mae[1]),
    Test_MAE_sd=as.numeric(test_mae_sd[1]),
    Test_R2=as.numeric(test_r2[1]),
    Test_MBD=as.numeric(test_mbd[1]),
    stringsAsFactors=FALSE)
}

pddoy_numeric <- bind_rows(
  build_numeric_row("LIMP RF","Random Split","❌",
                    pd_random$summary$Train_RMSE_mean,
                    pd_random$summary$Train_RMSE_sd,
                    pd_random$summary$Train_MAE_mean,
                    pd_random$summary$Train_R2_mean,
                    pd_random$summary$Train_Bias_mean,
                    pd_random$summary$Val_RMSE_mean,
                    pd_random$summary$Val_RMSE_sd,
                    pd_random$summary$Val_MAE_mean,
                    pd_random$summary$Val_R2_mean,
                    pd_random$summary$Val_Bias_mean,
                    pd_random$test$Test_RMSE, NA,
                    pd_random$test$Test_MAE,  NA,
                    pd_random$test$Test_R2,
                    pd_random$test$Test_Bias),
  
  build_numeric_row("LIMP RF","L2YO","✅",
                    pd_final$summary$Train_RMSE, NA,
                    pd_final$summary$Train_MAE,
                    pd_final$summary$Train_R2,
                    pd_final$summary$Train_Bias,
                    NA,NA,NA,NA,NA,
                    pd_final$summary$Test_RMSE,
                    pd_final$summary$Test_RMSE_sd,
                    pd_final$summary$Test_MAE,
                    pd_final$summary$Test_MAE_sd,
                    pd_final$summary$Test_R2,
                    pd_final$summary$Test_Bias),
  
  build_numeric_row("PDSOSDER","Random Split","❌",
                    summary_metrics_df$Train_RMSE_Mean,
                    summary_metrics_df$Train_RMSE_SD,
                    summary_metrics_df$Train_MAE_Mean,
                    summary_metrics_df$Train_R2_Mean,
                    summary_metrics_df$Train_Bias_Mean,
                    summary_metrics_df$Val_RMSE_Mean,
                    summary_metrics_df$Val_RMSE_SD,
                    summary_metrics_df$Val_MAE_Mean,
                    summary_metrics_df$Val_R2_Mean,
                    summary_metrics_df$Val_Bias_Mean,
                    summary_metrics_df$Test_RMSE, NA,
                    summary_metrics_df$Test_MAE,  NA,
                    summary_metrics_df$Test_R2,
                    summary_metrics_df$Test_Bias),
  
  build_numeric_row("PDSOSDER","L2YO","✅",
                    sosder_summary$Train_RMSE_mean, NA,
                    sosder_summary$Train_MAE_mean,
                    sosder_summary$Train_R2_mean,
                    sosder_summary$Train_Bias_mean,
                    NA,NA,NA,NA,NA,
                    sosder_summary$Test_RMSE_mean,
                    sosder_summary$Test_RMSE_sd,
                    sosder_summary$Test_MAE_mean,
                    sosder_summary$Test_MAE_sd,
                    sosder_summary$Test_R2_mean,
                    sosder_summary$Test_Bias_mean),
  
  build_numeric_row("Deines RF","Random Split","❌",
                    summary_metrics_planting$Train_RMSE_mean,
                    summary_metrics_planting$Train_RMSE_sd,
                    summary_metrics_planting$Train_MAE_mean,
                    summary_metrics_planting$Train_R2_mean,
                    summary_metrics_planting$Train_Bias_mean,
                    summary_metrics_planting$Val_RMSE_mean,
                    summary_metrics_planting$Val_RMSE_sd,
                    summary_metrics_planting$Val_MAE_mean,
                    summary_metrics_planting$Val_R2_mean,
                    summary_metrics_planting$Val_Bias_mean,
                    test_results_planting$RMSE, NA,
                    test_results_planting$MAE,  NA,
                    test_results_planting$R2,
                    test_results_planting$Bias),
  
  build_numeric_row("Deines RF","L2YO","✅",
                    deines_pd_l2yo$summary$Train_RMSE, NA,
                    deines_pd_l2yo$summary$Train_MAE,
                    deines_pd_l2yo$summary$Train_R2,
                    deines_pd_l2yo$summary$Train_Bias,
                    NA,NA,NA,NA,NA,
                    deines_pd_l2yo$summary$Test_RMSE,
                    deines_pd_l2yo$summary$Test_RMSE_sd,
                    deines_pd_l2yo$summary$Test_MAE,
                    deines_pd_l2yo$summary$Test_MAE_sd,
                    deines_pd_l2yo$summary$Test_R2,
                    deines_pd_l2yo$summary$Test_Bias)
)

hddoy_numeric <- bind_rows(
  build_numeric_row("LIMP RF","Random Split","❌",
                    hd_random$summary$Train_RMSE_mean,
                    hd_random$summary$Train_RMSE_sd,
                    hd_random$summary$Train_MAE_mean,
                    hd_random$summary$Train_R2_mean,
                    hd_random$summary$Train_Bias_mean,
                    hd_random$summary$Val_RMSE_mean,
                    hd_random$summary$Val_RMSE_sd,
                    hd_random$summary$Val_MAE_mean,
                    hd_random$summary$Val_R2_mean,
                    hd_random$summary$Val_Bias_mean,
                    hd_random$test$Test_RMSE, NA,
                    hd_random$test$Test_MAE,  NA,
                    hd_random$test$Test_R2,
                    hd_random$test$Test_Bias),
  
  build_numeric_row("LIMP RF","L2YO","✅",
                    hd_final$summary$Train_RMSE, NA,
                    hd_final$summary$Train_MAE,
                    hd_final$summary$Train_R2,
                    hd_final$summary$Train_Bias,
                    NA,NA,NA,NA,NA,
                    hd_final$summary$Test_RMSE,
                    hd_final$summary$Test_RMSE_sd,
                    hd_final$summary$Test_MAE,
                    hd_final$summary$Test_MAE_sd,
                    hd_final$summary$Test_R2,
                    hd_final$summary$Test_Bias),
  
  build_numeric_row("Deines RF","Random Split","❌",
                    summary_metrics_harvest$Train_RMSE_mean,
                    summary_metrics_harvest$Train_RMSE_sd,
                    summary_metrics_harvest$Train_MAE_mean,
                    summary_metrics_harvest$Train_R2_mean,
                    summary_metrics_harvest$Train_Bias_mean,
                    summary_metrics_harvest$Val_RMSE_mean,
                    summary_metrics_harvest$Val_RMSE_sd,
                    summary_metrics_harvest$Val_MAE_mean,
                    summary_metrics_harvest$Val_R2_mean,
                    summary_metrics_harvest$Val_Bias_mean,
                    test_results_harvest$RMSE, NA,
                    test_results_harvest$MAE,  NA,
                    test_results_harvest$R2,
                    test_results_harvest$Bias),
  
  build_numeric_row("Deines RF","L2YO","✅",
                    deines_hd_l2yo$summary$Train_RMSE, NA,
                    deines_hd_l2yo$summary$Train_MAE,
                    deines_hd_l2yo$summary$Train_R2,
                    deines_hd_l2yo$summary$Train_Bias,
                    NA,NA,NA,NA,NA,
                    deines_hd_l2yo$summary$Test_RMSE,
                    deines_hd_l2yo$summary$Test_RMSE_sd,
                    deines_hd_l2yo$summary$Test_MAE,
                    deines_hd_l2yo$summary$Test_MAE_sd,
                    deines_hd_l2yo$summary$Test_R2,
                    deines_hd_l2yo$summary$Test_Bias)
)

model_order <- c("LIMP RF","PDSOSDER","Deines RF")
split_order <- c("Random Split","L2YO")

pddoy_numeric <- pddoy_numeric %>%
  mutate(Model=factor(Model, levels=model_order),
         Split=factor(Split, levels=split_order),
         Label=paste0(Model,"\n(",Split,")"))

hddoy_numeric <- hddoy_numeric %>%
  mutate(Model=factor(Model, levels=model_order),
         Split=factor(Split, levels=split_order),
         Label=paste0(Model,"\n(",Split,")"))

# =========================================================
# PLOTTING — fixed make_bar_plot
# =========================================================

make_bar_plot <- function(df_num, metric, sd_col=NULL,
                          y_lab, title="") {
  df_num$y_val <- df_num[[metric]]
  
  # Handle sd_col — must be a column name string or NULL
  if (!is.null(sd_col) && sd_col %in% names(df_num)) {
    df_num$y_sd <- df_num[[sd_col]]
  } else {
    df_num$y_sd <- NA_real_
  }
  
  df_num$leaky <- df_num$Valid == "❌"
  
  ggplot(df_num,
         aes(x=Label, y=y_val,
             fill=Model,
             alpha=ifelse(leaky, 0.45, 1.0))) +
    geom_col(width=0.65) +
    geom_errorbar(
      aes(ymin=y_val - ifelse(is.na(y_sd),0,y_sd),
          ymax=y_val + ifelse(is.na(y_sd),0,y_sd)),
      width=0.2, na.rm=TRUE) +
    geom_text(
      aes(label=ifelse(
        is.na(y_sd) | y_sd==0,
        sprintf("%.2f", y_val),
        sprintf("%.2f±%.2f", y_val, y_sd))),
      vjust=-0.5, size=3, na.rm=TRUE) +
    geom_col(
      data=df_num %>% filter(leaky),
      fill=NA, color="red",
      linetype="dashed", linewidth=0.7,
      width=0.65, alpha=1) +
    scale_alpha_identity() +
    scale_fill_manual(values=c(
      "LIMP RF"   = "#2980b9",
      "PDSOSDER"  = "#27ae60",
      "Deines RF" = "#e67e22")) +
    labs(title=title, y=y_lab, x=NULL,
         caption="Red dashed = random split (leakage)") +
    theme_classic(base_size=12) +
    theme(axis.text.x=element_text(size=9, angle=15, hjust=0.7),
          legend.position="top",
          legend.title=element_blank(),
          plot.caption=element_text(color="red", size=8))
}

# PDDOY
p_pd_rmse <- make_bar_plot(pddoy_numeric, "Test_RMSE",
                           "Test_RMSE_sd", "Test RMSE (days)", "PDDOY — Test RMSE")
p_pd_mae  <- make_bar_plot(pddoy_numeric, "Test_MAE",
                           "Test_MAE_sd",  "Test MAE (days)",  "PDDOY — Test MAE")
p_pd_r2   <- make_bar_plot(pddoy_numeric, "Test_R2",
                           NULL, expression(italic(R)^2), "PDDOY — Test R²")
p_pd_mbd  <- make_bar_plot(pddoy_numeric, "Test_MBD",
                           NULL, "Test MBD (days)", "PDDOY — Test MBD")

# HDDOY
p_hd_rmse <- make_bar_plot(hddoy_numeric, "Test_RMSE",
                           "Test_RMSE_sd", "Test RMSE (days)", "HDDOY — Test RMSE")
p_hd_mae  <- make_bar_plot(hddoy_numeric, "Test_MAE",
                           "Test_MAE_sd",  "Test MAE (days)",  "HDDOY — Test MAE")
p_hd_r2   <- make_bar_plot(hddoy_numeric, "Test_R2",
                           NULL, expression(italic(R)^2), "HDDOY — Test R²")
p_hd_mbd  <- make_bar_plot(hddoy_numeric, "Test_MBD",
                           NULL, "Test MBD (days)", "HDDOY — Test MBD")

# Combined PDDOY (4 panels)
combined_pd <- (p_pd_rmse | p_pd_mae) /
  (p_pd_r2   | p_pd_mbd) +
  plot_annotation(
    title="Planting Date (PDDOY) — Model Comparison",
    tag_levels="A",
    theme=theme(plot.title=element_text(face="bold",size=14)))
print(combined_pd)

# Combined HDDOY (4 panels)
combined_hd <- (p_hd_rmse | p_hd_mae) /
  (p_hd_r2   | p_hd_mbd) +
  plot_annotation(
    title="Harvest Date (HDDOY) — Model Comparison",
    tag_levels="A",
    theme=theme(plot.title=element_text(face="bold",size=14)))
print(combined_hd)

# Side-by-side paper figure
combined_paper <- (
  (p_pd_rmse | p_hd_rmse) /
    (p_pd_mae  | p_hd_mae)  /
    (p_pd_r2   | p_hd_r2)   /
    (p_pd_mbd  | p_hd_mbd)
) + plot_annotation(
  tag_levels="A",
  theme=theme(plot.tag=element_text(face="bold",size=14)))
print(combined_paper)

ggsave(file.path(out_dir,"AllModels_PDDOY_comparison.png"),
       combined_pd, width=16, height=12, dpi=300)
ggsave(file.path(out_dir,"AllModels_HDDOY_comparison.png"),
       combined_hd, width=16, height=12, dpi=300)
ggsave(file.path(out_dir,"AllModels_Combined_paper.png"),
       combined_paper, width=22, height=20, dpi=300)

write.csv(pddoy_numeric,
          file.path(out_dir,"PDDOY_master_numeric.csv"), row.names=FALSE)
write.csv(hddoy_numeric,
          file.path(out_dir,"HDDOY_master_numeric.csv"), row.names=FALSE)

cat("\n✓ All done. Tables and plots saved.\n")

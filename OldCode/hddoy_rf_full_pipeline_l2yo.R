# =========================================================
# FULL HYPERPARAMETER TUNING
# 1. ntree (number of trees)
# 2. mtry (features per split)
# 3. n_features (feature count)
# All using year-based CV — no leakage
# =========================================================

# =========================================================
# STEP 1: FIND OPTIMAL ntree FIRST
# ntree does not need CV — use OOB error curve
# Run on full feature set, then apply to all sizes
# =========================================================

cat("=== STEP 1: OPTIMAL ntree ===\n")

find_optimal_ntree <- function(df_in, target, features,
                               ntree_options, label) {
  feat  <- intersect(features, names(df_in))
  df_use <- df_in %>%
    dplyr::select(all_of(feat), all_of(target), Year) %>%
    drop_na()
  
  oob_results <- data.frame()
  
  for (nt in ntree_options) {
    set.seed(123)
    m <- randomForest(
      as.formula(paste(target, "~ . - Year")),
      data     = df_use,
      ntree    = nt,
      mtry     = max(2, floor(sqrt(length(feat)))),
      nodesize = 5,
      maxnodes = 40
    )
    oob_results <- rbind(oob_results, data.frame(
      ntree    = nt,
      OOB_RMSE = round(sqrt(m$mse[nt]), 3),
      Var_expl = round(m$rsq[nt] * 100, 2)
    ))
    cat(sprintf("%s | ntree=%4d | OOB RMSE=%.3f | Var=%.1f%%\n",
                label, nt, sqrt(m$mse[nt]), m$rsq[nt]*100))
  }
  return(oob_results)
}

ntree_options <- c(50, 100, 150, 200, 300, 400, 500, 600, 800)

cat("\n--- PDDOY ntree search ---\n")
ntree_pd <- find_optimal_ntree(
  pd_v9_out$df_used, "PDDOY",
  pd_v9_out$features, ntree_options, "PDDOY"
)

cat("\n--- HDDOY ntree search ---\n")
ntree_hd <- find_optimal_ntree(
  hd_v2_out$df_used, "HDDOY",
  hd_v2_out$features, ntree_options, "HDDOY"
)

# Plot OOB RMSE vs ntree
ntree_plot <- bind_rows(
  ntree_pd %>% mutate(Target = "PDDOY"),
  ntree_hd %>% mutate(Target = "HDDOY")
)

ggplot(ntree_plot,
       aes(x=ntree, y=OOB_RMSE, color=Target, group=Target)) +
  geom_line(linewidth=1.2) +
  geom_point(size=3) +
  scale_color_manual(values=c("PDDOY"="#2980b9",
                              "HDDOY"="#e67e22")) +
  labs(title   = "OOB RMSE vs Number of Trees",
       subtitle = "Pick where curve flattens (elbow)",
       x = "ntree", y = "OOB RMSE (days)") +
  theme_minimal(base_size=13)

# Best ntree = where OOB stops improving (< 0.05 gain)
find_elbow_ntree <- function(oob_df) {
  oob_df <- oob_df %>% arrange(ntree)
  diffs  <- diff(oob_df$OOB_RMSE)
  # First point where improvement < 0.05 days
  elbow  <- which(abs(diffs) < 0.05)[1]
  if (is.na(elbow)) elbow <- nrow(oob_df)
  oob_df$ntree[elbow]
}

best_ntree_pd <- find_elbow_ntree(ntree_pd)
best_ntree_hd <- find_elbow_ntree(ntree_hd)

cat(sprintf("\nBest ntree PDDOY: %d\n", best_ntree_pd))
cat(sprintf("Best ntree HDDOY: %d\n", best_ntree_hd))

# =========================================================
# STEP 2: JOINT GRID SEARCH — ntree × mtry × n_features
# Year-based CV throughout
# =========================================================

cat("\n=== STEP 2: JOINT GRID SEARCH ===\n")

# Define search grids
ntree_candidates_pd <- c(best_ntree_pd,
                         max(50, best_ntree_pd - 50),
                         best_ntree_pd + 100)
ntree_candidates_hd <- c(best_ntree_hd,
                         max(50, best_ntree_hd - 50),
                         best_ntree_hd + 100)

size_options  <- c(10, 12, 15, 18)
mtry_options  <- c(3, 4, 5, 6, 8)

# =========================================================
# PDDOY GRID SEARCH
# =========================================================

cat("\n--- PDDOY Grid Search ---\n")
pd_grid_results <- data.frame()

# Get ranked features from previous RFE
pd_ranked <- rfe_pd_v9$variables %>%
  group_by(var) %>%
  summarise(Overall = mean(Overall), .groups="drop") %>%
  arrange(desc(Overall))

# Year fold list for PDDOY
pd_yr_col <- pd_v9_out$df_used$Year
pd_fl <- lapply(sort(unique(pd_yr_col)),
                function(yr) which(pd_yr_col != yr))
names(pd_fl) <- paste0("Year_", sort(unique(pd_yr_col)))

safe_r2 <- function(obs, pred) {
  tryCatch(summary(lm(obs~pred))$r.squared,
           error=function(e) NA_real_)
}

total_pd <- length(ntree_candidates_pd) *
  length(size_options) *
  length(mtry_options)
counter_pd <- 0

for (nt in ntree_candidates_pd) {
  for (ns in size_options) {
    feat_n  <- pd_ranked$var[1:ns]
    df_pd_n <- pd_v9_out$df_used %>%
      dplyr::select(all_of(feat_n), PDDOY, Year) %>%
      drop_na()
    
    for (mt in mtry_options) {
      # skip invalid mtry
      if (mt > ns) next
      counter_pd <- counter_pd + 1
      
      # L2YO with this combo
      pairs <- combn(sort(unique(df_pd_n$Year)),
                     2, simplify=FALSE)
      res_tmp <- data.frame()
      
      for (pair in pairs) {
        tr <- df_pd_n %>% filter(!Year %in% pair)
        te <- df_pd_n %>% filter( Year %in% pair)
        if (nrow(tr) < 10 || nrow(te) < 2) next
        
        m <- tryCatch(
          randomForest(PDDOY ~ . - Year, data=tr,
                       ntree=nt, mtry=mt,
                       nodesize=5, maxnodes=40),
          error=function(e) NULL)
        if (is.null(m)) next
        
        pred_te <- predict(m, te)
        pred_tr <- predict(m, tr)
        res_tmp <- rbind(res_tmp, data.frame(
          TR = hydroGOF::rmse(pred_tr, tr$PDDOY),
          TE = hydroGOF::rmse(pred_te, te$PDDOY),
          MA = hydroGOF::mae(pred_te,  te$PDDOY),
          R2 = safe_r2(te$PDDOY, pred_te)
        ))
      }
      
      if (nrow(res_tmp) == 0) next
      
      pd_grid_results <- rbind(pd_grid_results, data.frame(
        ntree      = nt,
        n_features = ns,
        mtry       = mt,
        Train_RMSE = round(mean(res_tmp$TR), 3),
        Test_RMSE  = round(mean(res_tmp$TE), 3),
        Test_MAE   = round(mean(res_tmp$MA), 3),
        Test_R2    = round(mean(res_tmp$R2), 4)
      ))
      
      cat(sprintf("[%d/%d] ntree=%d feat=%d mtry=%d → RMSE=%.3f\n",
                  counter_pd, total_pd, nt, ns, mt,
                  mean(res_tmp$TE)))
    }
  }
}

cat("\n--- PDDOY Best Configurations ---\n")
pd_grid_results %>%
  arrange(Test_RMSE) %>%
  head(10) %>%
  as.data.frame() %>%
  print()

# Best combo
best_pd <- pd_grid_results %>%
  arrange(Test_RMSE) %>%
  slice(1)

cat(sprintf("\nPDDOY WINNER: ntree=%d, feat=%d, mtry=%d → RMSE=%.3f\n",
            best_pd$ntree, best_pd$n_features,
            best_pd$mtry, best_pd$Test_RMSE))

# =========================================================
# HDDOY GRID SEARCH
# =========================================================

cat("\n--- HDDOY Grid Search ---\n")
hd_grid_results <- data.frame()

hd_ranked <- rfe_hd_v2$variables %>%
  group_by(var) %>%
  summarise(Overall = mean(Overall), .groups="drop") %>%
  arrange(desc(Overall))

hd_yr_col <- hd_v2_out$df_used$Year
hd_fl <- lapply(sort(unique(hd_yr_col)),
                function(yr) which(hd_yr_col != yr))
names(hd_fl) <- paste0("Year_", sort(unique(hd_yr_col)))

total_hd <- length(ntree_candidates_hd) *
  length(size_options) *
  length(mtry_options)
counter_hd <- 0

for (nt in ntree_candidates_hd) {
  for (ns in size_options) {
    feat_n  <- hd_ranked$var[1:ns]
    df_hd_n <- hd_v2_out$df_used %>%
      dplyr::select(all_of(feat_n), HDDOY, Year) %>%
      drop_na()
    
    for (mt in mtry_options) {
      if (mt > ns) next
      counter_hd <- counter_hd + 1
      
      pairs <- combn(sort(unique(df_hd_n$Year)),
                     2, simplify=FALSE)
      res_tmp <- data.frame()
      
      for (pair in pairs) {
        tr <- df_hd_n %>% filter(!Year %in% pair)
        te <- df_hd_n %>% filter( Year %in% pair)
        if (nrow(tr) < 10 || nrow(te) < 2) next
        
        m <- tryCatch(
          randomForest(HDDOY ~ . - Year, data=tr,
                       ntree=nt, mtry=mt,
                       nodesize=5, maxnodes=40),
          error=function(e) NULL)
        if (is.null(m)) next
        
        pred_te <- predict(m, te)
        pred_tr <- predict(m, tr)
        res_tmp <- rbind(res_tmp, data.frame(
          TR = hydroGOF::rmse(pred_tr, tr$HDDOY),
          TE = hydroGOF::rmse(pred_te, te$HDDOY),
          MA = hydroGOF::mae(pred_te,  te$HDDOY),
          R2 = safe_r2(te$HDDOY, pred_te)
        ))
      }
      
      if (nrow(res_tmp) == 0) next
      
      hd_grid_results <- rbind(hd_grid_results, data.frame(
        ntree      = nt,
        n_features = ns,
        mtry       = mt,
        Train_RMSE = round(mean(res_tmp$TR), 3),
        Test_RMSE  = round(mean(res_tmp$TE), 3),
        Test_MAE   = round(mean(res_tmp$MA), 3),
        Test_R2    = round(mean(res_tmp$R2), 4)
      ))
      
      cat(sprintf("[%d/%d] ntree=%d feat=%d mtry=%d → RMSE=%.3f\n",
                  counter_hd, total_hd, nt, ns, mt,
                  mean(res_tmp$TE)))
    }
  }
}

cat("\n--- HDDOY Best Configurations ---\n")
hd_grid_results %>%
  arrange(Test_RMSE) %>%
  head(10) %>%
  as.data.frame() %>%
  print()

best_hd <- hd_grid_results %>%
  arrange(Test_RMSE) %>%
  slice(1)

cat(sprintf("\nHDDOY WINNER: ntree=%d, feat=%d, mtry=%d → RMSE=%.3f\n",
            best_hd$ntree, best_hd$n_features,
            best_hd$mtry, best_hd$Test_RMSE))

# =========================================================
# STEP 3: VISUALISE GRID SEARCH RESULTS
# =========================================================

# PDDOY heatmap: features vs mtry (best ntree)
pd_best_ntree_grid <- pd_grid_results %>%
  filter(ntree == best_pd$ntree)

ggplot(pd_best_ntree_grid,
       aes(x=factor(mtry), y=factor(n_features),
           fill=Test_RMSE)) +
  geom_tile(color="white") +
  geom_text(aes(label=round(Test_RMSE,2)),
            size=3.5, color="white", fontface="bold") +
  scale_fill_viridis_c(option="plasma") +
  labs(title   = sprintf("PDDOY Grid Search (ntree=%d)",
                         best_pd$ntree),
       subtitle = "Test RMSE by feature count and mtry",
       x = "mtry", y = "N Features", fill = "RMSE") +
  theme_minimal(base_size=13)

# HDDOY heatmap
hd_best_ntree_grid <- hd_grid_results %>%
  filter(ntree == best_hd$ntree)

ggplot(hd_best_ntree_grid,
       aes(x=factor(mtry), y=factor(n_features),
           fill=Test_RMSE)) +
  geom_tile(color="white") +
  geom_text(aes(label=round(Test_RMSE,2)),
            size=3.5, color="white", fontface="bold") +
  scale_fill_viridis_c(option="plasma") +
  labs(title   = sprintf("HDDOY Grid Search (ntree=%d)",
                         best_hd$ntree),
       subtitle = "Test RMSE by feature count and mtry",
       x = "mtry", y = "N Features", fill = "RMSE") +
  theme_minimal(base_size=13)

# =========================================================
# STEP 4: FINAL MODELS WITH OPTIMAL HYPERPARAMETERS
# =========================================================

cat("\n=== STEP 4: FINAL MODELS ===\n")

# PDDOY final features
final_pd_feat <- pd_ranked$var[1:best_pd$n_features]
final_hd_feat <- hd_ranked$var[1:best_hd$n_features]

cat("\nFinal PDDOY features (", length(final_pd_feat), "):\n")
print(sort(final_pd_feat))

cat("\nFinal HDDOY features (", length(final_hd_feat), "):\n")
print(sort(final_hd_feat))

# Run final L2YO with optimal params
pd_optimal_out <- run_l2yo(
  pd_v9_out$df_used %>%
    dplyr::select(all_of(final_pd_feat), PDDOY, Year),
  "PDDOY", final_pd_feat,
  pd_fl,
  sprintf("PDDOY OPTIMAL (ntree=%d, feat=%d, mtry=%d)",
          best_pd$ntree, best_pd$n_features, best_pd$mtry)
)

hd_optimal_out <- run_l2yo(
  hd_v2_out$df_used %>%
    dplyr::select(all_of(final_hd_feat), HDDOY, Year),
  "HDDOY", final_hd_feat,
  hd_fl,
  sprintf("HDDOY OPTIMAL (ntree=%d, feat=%d, mtry=%d)",
          best_hd$ntree, best_hd$n_features, best_hd$mtry)
)

# =========================================================
# STEP 5: COMPLETE FINAL SUMMARY
# =========================================================

cat("\n╔══════════════════════════════════════════════════════════╗\n")
cat("║          FINAL OPTIMIZED MODEL SUMMARY                   ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n")

final_optimized <- data.frame(
  Target     = c("PDDOY", "HDDOY"),
  ntree      = c(best_pd$ntree, best_hd$ntree),
  mtry       = c(best_pd$mtry,  best_hd$mtry),
  N_features = c(best_pd$n_features, best_hd$n_features),
  Train_RMSE = c(pd_optimal_out$summary$Train_RMSE,
                 hd_optimal_out$summary$Train_RMSE),
  Train_MAE  = c(pd_optimal_out$summary$Train_MAE,
                 hd_optimal_out$summary$Train_MAE),
  Train_R2   = c(pd_optimal_out$summary$Train_R2,
                 hd_optimal_out$summary$Train_R2),
  Test_RMSE  = c(pd_optimal_out$summary$Test_RMSE,
                 hd_optimal_out$summary$Test_RMSE),
  Test_MAE   = c(pd_optimal_out$summary$Test_MAE,
                 hd_optimal_out$summary$Test_MAE),
  Test_R2    = c(pd_optimal_out$summary$Test_R2,
                 hd_optimal_out$summary$Test_R2),
  Rel_RMSE   = c(
    paste0(round(pd_optimal_out$summary$Test_RMSE/92*100,1),"%"),
    paste0(round(hd_optimal_out$summary$Test_RMSE/90*100,1),"%")
  )
)

print(as.data.frame(final_optimized))

# Final importance plots
plot_importance(
  pd_v9_out$df_used %>%
    dplyr::select(all_of(final_pd_feat), PDDOY, Year),
  "PDDOY", final_pd_feat, best_pd$mtry,
  sprintf("PDDOY Final (%d feat, ntree=%d, mtry=%d)",
          best_pd$n_features, best_pd$ntree, best_pd$mtry)
)

plot_importance(
  hd_v2_out$df_used %>%
    dplyr::select(all_of(final_hd_feat), HDDOY, Year),
  "HDDOY", final_hd_feat, best_hd$mtry,
  sprintf("HDDOY Final (%d feat, ntree=%d, mtry=%d)",
          best_hd$n_features, best_hd$ntree, best_hd$mtry)
)
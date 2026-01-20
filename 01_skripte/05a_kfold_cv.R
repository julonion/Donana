#!/usr/bin/env Rscript

# ===========================================
# Phase 2: k-fold Cross-Validation (Optional)
# Für alle Modelle - Stichproben-basiert
# ===========================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(caret)
  library(randomForest)
  library(xgboost)
})

# === Parameter ===
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"
work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"

# Eingabedateien
samples_csv_only <- file.path(work_dir, "02_daten", "training", "samples_corine_2018_variante_e.csv")
samples_csv_dgm <- file.path(work_dir, "02_daten", "training", "samples_corine_2018_with_dgm_variante_e.csv")

# Ausgabedateien
output_dir <- file.path(work_dir, "03_ergebnisse", "interne_validierung", "kfold_cv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Parameter
k_folds <- 5
cv_sample_size <- 2000  # Stichprobe für CV (um Laufzeit zu reduzieren)
set.seed(42)

cat("===========================================\n")
cat("k-fold Cross-Validation (Optional)\n")
cat("Räumlich (polygonbasiert) - verhindert Datenleckage\n")
cat("===========================================\n\n")

cat("Parameter:\n")
cat("  - k-folds:", k_folds, "\n")
cat("  - Stichprobe pro Klasse:", cv_sample_size, "\n")
cat("  - Gesamt-Stichprobe:", cv_sample_size * 5, "\n")
cat("  - Split-Methode: Räumlich (polygonbasiert)\n\n")

# === Helper-Funktion für k-fold CV (räumlich, polygonbasiert) ===
perform_kfold_cv <- function(samples_df, model_type = "rf", feature_cols, k = 5) {
  cat(">> k-fold CV für", model_type, "(räumlich, polygonbasiert)\n")
  
  # Prüfe ob polygon_id vorhanden ist
  if (!("polygon_id" %in% names(samples_df))) {
    warning("polygon_id nicht gefunden! Verwende Standard k-fold CV (nicht räumlich)")
    use_spatial_split <- FALSE
  } else {
    use_spatial_split <- TRUE
    cat("  Räumlicher Split aktiviert (polygonbasiert)\n")
  }
  
  # Stichprobe ziehen (um Laufzeit zu reduzieren)
  set.seed(42)
  sampled_data <- samples_df %>%
    group_by(class_name) %>%
    sample_n(min(cv_sample_size, n())) %>%
    ungroup()
  
  cat("  Stichprobe:", nrow(sampled_data), "Samples\n")
  
  # Daten vorbereiten
  train_df <- sampled_data %>%
    select(all_of(feature_cols)) %>%
    mutate(class_name = factor(sampled_data$class_name))
  
  vollstaendig <- complete.cases(train_df)
  train_df <- train_df[vollstaendig, ]
  
  if (use_spatial_split) {
    # Räumlicher k-fold CV: Polygone in k Folds aufteilen
    polygon_tbl <- sampled_data %>%
      filter(vollstaendig) %>%
      distinct(polygon_id, class_name)
    
    cat("  Eindeutige Polygone:", nrow(polygon_tbl), "\n")
    
    # Prüfe ob genug Polygone für räumlichen Split vorhanden sind
    # Mindestens k+1 Polygone pro Klasse, sonst Standard-Split
    min_polys_per_class <- polygon_tbl %>%
      group_by(class_name) %>%
      summarise(n = n(), .groups = "drop") %>%
      pull(n) %>%
      min()
    
    if (min_polys_per_class < k + 1) {
      cat("  ⚠️ Zu wenige Polygone für räumlichen Split (min:", min_polys_per_class, "pro Klasse)\n")
      cat("  → Verwende Standard k-fold CV (nicht räumlich)\n\n")
      use_spatial_split <- FALSE
      folds <- createFolds(train_df$class_name, k = k, list = TRUE, returnTrain = FALSE)
    } else {
      # Stratifizierte Aufteilung der Polygone in k Folds
      # WICHTIG: Stelle sicher, dass jede Klasse in jedem Fold mindestens 1 Polygon hat
      set.seed(42)
      polygon_folds <- polygon_tbl %>%
        group_by(class_name) %>%
        do({
          n_polys <- nrow(.)
          # Mindestens 1 Polygon pro Klasse pro Fold
          fold_assignments <- sample(rep(1:k, length.out = n_polys))
          mutate(., fold = fold_assignments)
        }) %>%
        ungroup()
      
      cat("  Polygone pro Fold:", round(nrow(polygon_tbl) / k), "\n\n")
    }
  } else {
    # Standard k-fold CV (nicht räumlich)
    folds <- createFolds(train_df$class_name, k = k, list = TRUE, returnTrain = FALSE)
  }
  
  cv_results <- list()
  
  for (fold in 1:k) {
    cat("  Fold", fold, "/", k, "...")
    
    if (use_spatial_split) {
      # Räumlicher Split: Polygone dieses Folds sind Test-Set
      test_poly_ids <- polygon_folds %>%
        filter(fold == fold) %>%
        pull(polygon_id)
      
      train_poly_ids <- polygon_folds %>%
        filter(fold != fold) %>%
        pull(polygon_id)
      
      # Split Samples basierend auf Polygon-Zugehörigkeit
      train_indices <- which(sampled_data$polygon_id[vollstaendig] %in% train_poly_ids)
      test_indices <- which(sampled_data$polygon_id[vollstaendig] %in% test_poly_ids)
      
      train_fold <- train_df[train_indices, ]
      test_fold <- train_df[test_indices, ]
      
      # Prüfe ob alle Klassen im Trainingsset vorhanden sind
      train_classes <- unique(train_fold$class_name)
      all_classes <- unique(train_df$class_name)
      
      if (length(train_classes) < 2) {
        cat(" ⚠️ Zu wenige Klassen im Trainingsset, überspringe Fold", fold, "\n")
        next
      }
      
      cat(" (Train-Polygone:", length(train_poly_ids), ", Test-Polygone:", length(test_poly_ids), 
          ", Train-Klassen:", length(train_classes), ")")
    } else {
      # Standard Split
      test_idx <- folds[[fold]]
      train_idx <- setdiff(1:nrow(train_df), test_idx)
      
      train_fold <- train_df[train_idx, ]
      test_fold <- train_df[test_idx, ]
    }
    
    if (model_type == "rf") {
      model <- randomForest(
        x = train_fold[, feature_cols],
        y = train_fold$class_name,
        ntree = 500,
        mtry = floor(sqrt(length(feature_cols))),
        importance = FALSE
      )
      pred <- predict(model, newdata = test_fold[, feature_cols])
    } else if (model_type == "xgb") {
      train_matrix <- as.matrix(train_fold[, feature_cols])
      test_matrix <- as.matrix(test_fold[, feature_cols])
      train_labels <- as.numeric(train_fold$class_name) - 1
      test_labels <- as.numeric(test_fold$class_name) - 1
      num_class <- length(unique(train_labels))
      
      # Erstelle DMatrix für Early Stopping (wie in Trainings-Skripten)
      dtrain_fold <- xgb.DMatrix(train_matrix, label = train_labels)
      dtest_fold <- xgb.DMatrix(test_matrix, label = test_labels)
      watchlist <- list(train = dtrain_fold, valid = dtest_fold)
      
      # Verbesserte Parameter (wie in Trainings-Skripten)
      model <- xgb.train(
        data = dtrain_fold,
        watchlist = watchlist,
        params = list(
          objective = "multi:softprob",
          num_class = num_class,
          eval_metric = "mlogloss",
          eta = 0.1,              # Konservativer
          max_depth = 4,          # Weniger tief
          subsample = 0.9,
          colsample_bytree = 0.9,
          min_child_weight = 2
        ),
        nrounds = 800,
        early_stopping_rounds = 30,
        verbose = 0
      )
      
      pred_probs <- predict(model, dtest_fold)
      pred_probs <- matrix(pred_probs, ncol = num_class, byrow = TRUE)
      pred_class <- max.col(pred_probs) - 1
      class_levels <- levels(train_fold$class_name)
      pred <- factor(pred_class, levels = 0:(num_class-1), labels = class_levels)
    }
    
    # Metriken
    cm <- confusionMatrix(pred, test_fold$class_name)
    cv_results[[fold]] <- list(
      accuracy = as.numeric(cm$overall["Accuracy"]),
      kappa = as.numeric(cm$overall["Kappa"]),
      f1_scores = cm$byClass[, "F1"]
    )
    
    cat(" Accuracy:", round(cv_results[[fold]]$accuracy, 3), "\n")
  }
  
  # Zusammenfassung
  accuracies <- sapply(cv_results, function(x) x$accuracy)
  kappas <- sapply(cv_results, function(x) x$kappa)
  
  return(list(
    mean_accuracy = mean(accuracies),
    sd_accuracy = sd(accuracies),
    mean_kappa = mean(kappas),
    sd_kappa = sd(kappas),
    fold_results = cv_results
  ))
}

# === 1. RF CORINE only ===
if (file.exists(samples_csv_only)) {
  cat("===========================================\n")
  cat("1. RF CORINE only\n")
  cat("===========================================\n\n")
  
  samples <- read_csv(samples_csv_only, show_col_types = FALSE)
  drop_cols <- c("polygon_id", "Code_18", "area_km2", "quell_jahr", "quelle", "class_id", "x", "y")
  feature_cols <- setdiff(names(samples), c(drop_cols, "class_name"))
  
  cv_rf_only <- perform_kfold_cv(samples, "rf", feature_cols, k_folds)
  
  cat("\nZusammenfassung:\n")
  cat("  Mean Accuracy:", round(cv_rf_only$mean_accuracy, 4), 
      "±", round(cv_rf_only$sd_accuracy, 4), "\n")
  cat("  Mean Kappa:", round(cv_rf_only$mean_kappa, 4), 
      "±", round(cv_rf_only$sd_kappa, 4), "\n\n")
  
  saveRDS(cv_rf_only, file.path(output_dir, "cv_rf_corine_only.rds"))
}

# === 2. XGBoost CORINE only ===
if (file.exists(samples_csv_only)) {
  cat("===========================================\n")
  cat("2. XGBoost CORINE only\n")
  cat("===========================================\n\n")
  
  samples <- read_csv(samples_csv_only, show_col_types = FALSE)
  drop_cols <- c("polygon_id", "Code_18", "area_km2", "quell_jahr", "quelle", "class_id", "x", "y")
  feature_cols <- setdiff(names(samples), c(drop_cols, "class_name"))
  
  cv_xgb_only <- perform_kfold_cv(samples, "xgb", feature_cols, k_folds)
  
  cat("\nZusammenfassung:\n")
  cat("  Mean Accuracy:", round(cv_xgb_only$mean_accuracy, 4), 
      "±", round(cv_xgb_only$sd_accuracy, 4), "\n")
  cat("  Mean Kappa:", round(cv_xgb_only$mean_kappa, 4), 
      "±", round(cv_xgb_only$sd_kappa, 4), "\n\n")
  
  saveRDS(cv_xgb_only, file.path(output_dir, "cv_xgb_corine_only.rds"))
}

# === 3. RF CORINE + DGM ===
if (file.exists(samples_csv_dgm)) {
  cat("===========================================\n")
  cat("3. RF CORINE + DGM\n")
  cat("===========================================\n\n")
  
  samples <- read_csv(samples_csv_dgm, show_col_types = FALSE)
  drop_cols <- c("polygon_id", "Code_18", "area_km2", "quell_jahr", "quelle", "class_id", "x", "y")
  feature_cols <- setdiff(names(samples), c(drop_cols, "class_name"))
  
  cv_rf_dgm <- perform_kfold_cv(samples, "rf", feature_cols, k_folds)
  
  cat("\nZusammenfassung:\n")
  cat("  Mean Accuracy:", round(cv_rf_dgm$mean_accuracy, 4), 
      "±", round(cv_rf_dgm$sd_accuracy, 4), "\n")
  cat("  Mean Kappa:", round(cv_rf_dgm$mean_kappa, 4), 
      "±", round(cv_rf_dgm$sd_kappa, 4), "\n\n")
  
  saveRDS(cv_rf_dgm, file.path(output_dir, "cv_rf_corine_dgm.rds"))
}

# === 4. XGBoost CORINE + DGM ===
if (file.exists(samples_csv_dgm)) {
  cat("===========================================\n")
  cat("4. XGBoost CORINE + DGM\n")
  cat("===========================================\n\n")
  
  samples <- read_csv(samples_csv_dgm, show_col_types = FALSE)
  drop_cols <- c("polygon_id", "Code_18", "area_km2", "quell_jahr", "quelle", "class_id", "x", "y")
  feature_cols <- setdiff(names(samples), c(drop_cols, "class_name"))
  
  cv_xgb_dgm <- perform_kfold_cv(samples, "xgb", feature_cols, k_folds)
  
  cat("\nZusammenfassung:\n")
  cat("  Mean Accuracy:", round(cv_xgb_dgm$mean_accuracy, 4), 
      "±", round(cv_xgb_dgm$sd_accuracy, 4), "\n")
  cat("  Mean Kappa:", round(cv_xgb_dgm$mean_kappa, 4), 
      "±", round(cv_xgb_dgm$sd_kappa, 4), "\n\n")
  
  saveRDS(cv_xgb_dgm, file.path(output_dir, "cv_xgb_corine_dgm.rds"))
}

# === Zusammenfassung ===
cat("===========================================\n")
cat("k-fold CV ABGESCHLOSSEN\n")
cat("===========================================\n")
cat("\nMethode: Räumlicher k-fold CV (polygonbasiert)\n")
cat("  - Verhindert Datenleckage durch räumliche Trennung\n")
cat("  - Polygone werden stratifiziert in k Folds aufgeteilt\n")
cat("  - XGBoost Parameter: Optimiert (eta=0.1, max_depth=4, early_stopping)\n\n")
cat("Ergebnisse gespeichert in:", output_dir, "\n")


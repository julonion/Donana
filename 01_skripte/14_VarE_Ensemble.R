#!/usr/bin/env Rscript

# ===========================================
# Phase 3: Majority Voting Ensemble
# Kombiniert alle Modelle für robustere Vorhersagen
# ===========================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(randomForest)
  library(xgboost)
  library(caret)
})

# === Parameter ===
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"
work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"

# Lade Pfade
paths_file <- file.path(work_dir, "02_daten", "input", "workflow_paths.rds")
if (!file.exists(paths_file)) {
  stop("Pfade nicht gefunden. Bitte zuerst '00_setup_workflow.R' ausführen!")
}
paths <- readRDS(paths_file)

# Eingabedateien
clcplus_raster <- paths$clcplus_raster
if (!file.exists(clcplus_raster)) {
  clcplus_raster <- paths$clcplus_raster_alt
}

feature_stack_spektral <- paths$feature_stack_2018
feature_stack_dgm <- paths$feature_stack_2018_dgm
aoi_path <- paths$aoi

# Modelle
model_rf_corine_only <- file.path(work_dir, "02_daten", "modelle", "rf_model_corine_only_variante_e.rds")
model_xgb_corine_only <- file.path(work_dir, "02_daten", "modelle", "xgb_model_corine_only_variante_e.rds")
model_rf_corine_dgm <- file.path(work_dir, "02_daten", "modelle", "rf_model_corine_dgm_variante_e.rds")
model_xgb_corine_dgm <- file.path(work_dir, "02_daten", "modelle", "xgb_model_corine_dgm_variante_e.rds")

# Feature-Listen
features_rf_corine_only <- file.path(work_dir, "02_daten", "modelle", "rf_corine_only_variante_e_features.rds")
features_xgb_corine_only <- file.path(work_dir, "02_daten", "modelle", "xgb_corine_only_variante_e_features.rds")
features_rf_corine_dgm <- file.path(work_dir, "02_daten", "modelle", "rf_corine_dgm_variante_e_features.rds")
features_xgb_corine_dgm <- file.path(work_dir, "02_daten", "modelle", "xgb_corine_dgm_variante_e_features.rds")

# Ausgabedateien
output_dir <- file.path(work_dir, "03_ergebnisse", "externe_validierung", "majority_voting")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

report_path <- file.path(output_dir, "majority_voting_report.txt")
metrics_csv <- file.path(output_dir, "majority_voting_metrics.csv")

# Parameter
set.seed(42)
max_samples_per_class <- 2000
sample_factor <- 5

cat("===========================================\n")
cat("Majority Voting Ensemble\n")
cat("Kombiniert alle Modelle für robustere Vorhersagen\n")
cat("===========================================\n\n")

# === Mapping CLCplus → Variante E Klassen ===
map_clcplus_to_variante_e <- function(code) {
  if (is.na(code)) return(NA)
  if (code == 2 || code == 3) return("Gehölzvegetation")
  if (code == 5) return("Offener_Boden")
  if (code == 6) return("Feuchtgebiet_salz")
  if (code == 7 || code == 10) return("Wasser")
  return(NA)
}

# === 1. Daten laden ===
cat(">> Lade Daten\n")

clc_ref <- rast(clcplus_raster)
feat_stack_spektral <- rast(feature_stack_spektral)
feat_stack_dgm <- rast(feature_stack_dgm)
aoi <- vect(aoi_path)

# Projektionen angleichen
if (!identical(crs(aoi), crs(feat_stack_spektral))) {
  aoi <- project(aoi, crs(feat_stack_spektral))
}
if (!identical(crs(clc_ref), crs(feat_stack_spektral))) {
  clc_ref <- project(clc_ref, crs(feat_stack_spektral), method = "near")
}

# Auf AOI beschränken
feat_stack_spektral <- mask(crop(feat_stack_spektral, aoi), aoi)
feat_stack_dgm <- mask(crop(feat_stack_dgm, aoi), aoi)
clc_ref <- mask(crop(clc_ref, feat_stack_spektral), aoi)

# === 2. Stichprobe ziehen ===
cat(">> Ziehe Stichprobe\n")
clc_sampled <- aggregate(clc_ref, fact = sample_factor, fun = "modal", na.rm = TRUE)
clc_sampled <- resample(clc_sampled, feat_stack_spektral, method = "near")

dat <- as.data.frame(c(clc_sampled), xy = TRUE, na.rm = TRUE)
names(dat)[3] <- "clc_code"
dat$reference <- sapply(dat$clc_code, map_clcplus_to_variante_e)
dat <- dat[!is.na(dat$reference), ]

# Balanciertes Sampling
dat_list <- list()
for (k in unique(dat$reference)) {
  sub <- dat[dat$reference == k, ]
  if (nrow(sub) > max_samples_per_class) {
    sub <- sub[sample(nrow(sub), max_samples_per_class), ]
  }
  dat_list[[k]] <- sub
}
dat <- bind_rows(dat_list)

cat("  Validierungspixel:", nrow(dat), "\n\n")

# === 3. Features extrahieren ===
cat(">> Extrahiere Features\n")
pts <- vect(dat[, c("x", "y")], geom = c("x", "y"), crs = crs(feat_stack_spektral))

feat_vals_spektral <- terra::extract(feat_stack_spektral, pts)
feat_vals_spektral <- feat_vals_spektral[, -1, drop = FALSE]

feat_vals_dgm <- terra::extract(feat_stack_dgm, pts)
feat_vals_dgm <- feat_vals_dgm[, -1, drop = FALSE]

# === 4. Modelle laden und Vorhersagen ===
cat(">> Lade Modelle und erstelle Vorhersagen\n\n")

all_classes <- c("Gehölzvegetation", "Offener_Boden", "Binnenmarschen", 
                 "Feuchtgebiet_salz", "Wasser")

predictions_list <- list()

# RF CORINE only
if (file.exists(model_rf_corine_only) && file.exists(features_rf_corine_only)) {
  cat("  RF CORINE only...\n")
  rf_only <- readRDS(model_rf_corine_only)
  features_rf_only <- readRDS(features_rf_corine_only)
  pred_data <- feat_vals_spektral[, features_rf_only, drop = FALSE]
  complete_idx <- which(complete.cases(pred_data))
  pred <- predict(rf_only, newdata = pred_data[complete_idx, , drop = FALSE])
  predictions_list[["RF_only"]] <- list(pred = pred, idx = complete_idx)
}

# XGBoost CORINE only
if (file.exists(model_xgb_corine_only) && file.exists(features_xgb_corine_only)) {
  cat("  XGBoost CORINE only...\n")
  xgb_only <- readRDS(model_xgb_corine_only)
  features_xgb_only <- readRDS(features_xgb_corine_only)
  pred_data <- feat_vals_spektral[, features_xgb_only, drop = FALSE]
  complete_idx <- which(complete.cases(pred_data))
  pred_matrix <- as.matrix(pred_data[complete_idx, , drop = FALSE])
  pred_probs <- predict(xgb_only, newdata = pred_matrix, reshape = TRUE)
  pred_class <- max.col(pred_probs) - 1
  if (!is.null(xgb_only$class_levels)) {
    class_levels <- xgb_only$class_levels
  } else {
    class_levels <- all_classes
  }
  pred <- factor(pred_class, levels = 0:(length(class_levels)-1), labels = class_levels)
  predictions_list[["XGB_only"]] <- list(pred = pred, idx = complete_idx)
}

# RF CORINE + DGM
if (file.exists(model_rf_corine_dgm) && file.exists(features_rf_corine_dgm)) {
  cat("  RF CORINE + DGM...\n")
  rf_dgm <- readRDS(model_rf_corine_dgm)
  features_rf_dgm <- readRDS(features_rf_corine_dgm)
  pred_data <- feat_vals_dgm[, features_rf_dgm, drop = FALSE]
  complete_idx <- which(complete.cases(pred_data))
  pred <- predict(rf_dgm, newdata = pred_data[complete_idx, , drop = FALSE])
  predictions_list[["RF_DGM"]] <- list(pred = pred, idx = complete_idx)
}

# XGBoost CORINE + DGM
if (file.exists(model_xgb_corine_dgm) && file.exists(features_xgb_corine_dgm)) {
  cat("  XGBoost CORINE + DGM...\n")
  xgb_dgm <- readRDS(model_xgb_corine_dgm)
  features_xgb_dgm <- readRDS(features_xgb_corine_dgm)
  pred_data <- feat_vals_dgm[, features_xgb_dgm, drop = FALSE]
  complete_idx <- which(complete.cases(pred_data))
  pred_matrix <- as.matrix(pred_data[complete_idx, , drop = FALSE])
  pred_probs <- predict(xgb_dgm, newdata = pred_matrix, reshape = TRUE)
  pred_class <- max.col(pred_probs) - 1
  if (!is.null(xgb_dgm$class_levels)) {
    class_levels <- xgb_dgm$class_levels
  } else {
    class_levels <- all_classes
  }
  pred <- factor(pred_class, levels = 0:(length(class_levels)-1), labels = class_levels)
  predictions_list[["XGB_DGM"]] <- list(pred = pred, idx = complete_idx)
}

cat("\n  Anzahl Modelle:", length(predictions_list), "\n\n")

# === 5. Majority Voting ===
cat(">> Majority Voting\n")

# Finde gemeinsame Indizes (wo alle Modelle Vorhersagen haben)
if (length(predictions_list) > 0) {
  # Debug: Zeige Anzahl vollständiger Fälle pro Modell
  for (i in 1:length(predictions_list)) {
    cat("  ", names(predictions_list)[i], ": ", length(predictions_list[[i]]$idx), " vollständige Fälle\n", sep = "")
  }
  
  common_idx <- predictions_list[[1]]$idx
  for (i in 2:length(predictions_list)) {
    common_idx <- intersect(common_idx, predictions_list[[i]]$idx)
  }
  
  cat("  Gemeinsame Pixel:", length(common_idx), "\n")
  cat("  Anzahl Zeilen in dat:", nrow(dat), "\n")
  
  # Extrahiere Vorhersagen für gemeinsame Pixel
  ensemble_preds <- matrix(NA, nrow = length(common_idx), ncol = length(predictions_list))
  colnames(ensemble_preds) <- names(predictions_list)
  
  for (i in 1:length(predictions_list)) {
    pred_vec <- as.character(predictions_list[[i]]$pred)
    # predictions_list[[i]]$idx enthält die ursprünglichen Zeilenindizes der vollständigen Fälle
    # common_idx enthält die gemeinsamen Indizes
    # Wir finden für jedes Element in common_idx seine Position in predictions_list[[i]]$idx
    # und extrahieren die entsprechende Vorhersage
    pos_in_pred <- match(common_idx, predictions_list[[i]]$idx)
    ensemble_preds[, i] <- pred_vec[pos_in_pred]
  }
  
  if (length(common_idx) == 0) {
    stop("Keine gemeinsamen Pixel gefunden! Möglicherweise haben die Modelle unterschiedliche complete_idx.")
  }
  
  # Entferne Zeilen mit NA-Werten in ensemble_preds
  na_rows <- apply(ensemble_preds, 1, function(x) any(is.na(x)))
  if (any(na_rows)) {
    cat("  Entferne", sum(na_rows), "Zeilen mit NA-Werten\n")
    ensemble_preds <- ensemble_preds[!na_rows, , drop = FALSE]
    common_idx <- common_idx[!na_rows]
  }
  
  if (nrow(ensemble_preds) == 0) {
    stop("Keine gültigen Vorhersagen nach NA-Entfernung!")
  }
  
  # Majority Voting
  majority_pred <- apply(ensemble_preds, 1, function(x) {
    # Entferne NA-Werte
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    # Zähle Häufigkeiten
    tab <- table(x)
    # Bei Gleichstand: nimm erste Klasse
    names(tab)[which.max(tab)]
  })
  
  # Entferne NA-Vorhersagen
  valid_pred <- !is.na(majority_pred)
  majority_pred <- majority_pred[valid_pred]
  common_idx_valid <- common_idx[valid_pred]
  
  if (length(majority_pred) == 0) {
    stop("Keine gültigen Majority-Voting-Vorhersagen!")
  }
  
  majority_pred <- factor(majority_pred, levels = all_classes)
  
  # Referenz: common_idx_valid sind Zeilenindizes in dat
  # Stelle sicher, dass die Indizes im gültigen Bereich sind
  if (any(common_idx_valid < 1 | common_idx_valid > nrow(dat))) {
    stop(sprintf("Ungültige Indizes: min = %d, max = %d, dat hat %d Zeilen", 
                 min(common_idx_valid), max(common_idx_valid), nrow(dat)))
  }
  
  ref_values <- dat$reference[common_idx_valid]
  ref <- factor(ref_values, levels = all_classes)
  
  # Prüfe Längen
  if (length(majority_pred) != length(ref)) {
    stop(sprintf("Längen stimmen nicht überein: majority_pred = %d, ref = %d", 
                 length(majority_pred), length(ref)))
  }
  
  # Confusion Matrix
  cm <- confusionMatrix(majority_pred, ref)
  
  accuracy <- cm$overall["Accuracy"]
  kappa <- cm$overall["Kappa"]
  f1_scores <- cm$byClass[, "F1"]
  f1_scores[is.nan(f1_scores)] <- 0
  
  cat("  Ensemble Accuracy:", round(accuracy, 4), "(", round(accuracy * 100, 2), "%)\n")
  cat("  Ensemble Kappa:", round(kappa, 4), "\n\n")
  
  # === Speichere Ergebnisse ===
  metrics_df <- data.frame(
    Modell = "Majority_Voting_Ensemble",
    Accuracy = as.numeric(accuracy),
    Kappa = as.numeric(kappa),
    stringsAsFactors = FALSE
  )
  
  write.csv(metrics_df, metrics_csv, row.names = FALSE)
  
  # Report
  sink(report_path)
  cat("Majority Voting Ensemble\n")
  cat("=======================\n\n")
  cat("Datum:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  cat("Modelle im Ensemble:\n")
  for (name in names(predictions_list)) {
    cat("  -", name, "\n")
  }
  cat("\n")
  
  cat("Validierungsparameter:\n")
  cat("  - Referenz: CLCplus 2018\n")
  cat("  - Validierungspixel:", length(common_idx), "\n")
  cat("  - Alle 5 Klassen validiert\n\n")
  
  cat("Ergebnisse:\n")
  cat("  - Accuracy:", round(accuracy, 4), "(", round(accuracy * 100, 2), "%)\n")
  cat("  - Kappa:", round(kappa, 4), "\n\n")
  
  cat("Klassen-spezifische F1-Scores:\n")
  for (i in 1:length(f1_scores)) {
    cat("  - ", names(f1_scores)[i], ": ", round(f1_scores[i], 4), "\n", sep = "")
  }
  cat("\n")
  
  cat("Confusion Matrix:\n")
  print(cm$table)
  cat("\n")
  
  sink()
  
  cat("  Ergebnisse gespeichert:\n")
  cat("    - Metriken:", basename(metrics_csv), "\n")
  cat("    - Report:", basename(report_path), "\n\n")
} else {
  cat("  ⚠️ Keine Modelle gefunden!\n")
}

cat("===========================================\n")
cat("MAJORITY VOTING ABGESCHLOSSEN\n")
cat("===========================================\n")


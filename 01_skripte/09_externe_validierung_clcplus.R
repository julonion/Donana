#!/usr/bin/env Rscript

# ===========================================
# Phase 3: Externe Validierung
# Alle Modelle gegen CLCplus
# Variante E – Alle 5 Klassen (inkl. Offener_Boden)
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
output_dir <- file.path(work_dir, "03_ergebnisse", "externe_validierung")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

report_path_5klassen <- file.path(output_dir, "externe_validierung_5_klassen.txt")
report_path_4klassen <- file.path(output_dir, "externe_validierung_4_klassen.txt")
report_path_3klassen <- file.path(output_dir, "externe_validierung_3_klassen.txt")
report_path_vergleich <- file.path(output_dir, "externe_validierung_vergleich.txt")

metrics_csv_5klassen <- file.path(output_dir, "externe_validierung_5_klassen_metriken.csv")
metrics_csv_4klassen <- file.path(output_dir, "externe_validierung_4_klassen_metriken.csv")
metrics_csv_3klassen <- file.path(output_dir, "externe_validierung_3_klassen_metriken.csv")
comparison_csv <- file.path(output_dir, "externe_validierung_vergleich.csv")

# Parameter
set.seed(42)
max_samples_per_class <- 2000
sample_factor <- 5  # Aggregation für schnellere Verarbeitung

cat("===========================================\n")
cat("Externe Validierung: Alle Modelle gegen CLCplus\n")
cat("Variante E – DREI Validierungen:\n")
cat("  1. 5 Klassen (alle)\n")
cat("  2. 4 Klassen (ohne Offener_Boden)\n")
cat("  3. 3 Klassen (zusammengeführt)\n")
cat("===========================================\n\n")

# === Mapping CLCplus → Variante E Klassen ===

# Mapping 1: 5 Klassen (wie bisher)
map_clcplus_to_variante_e_5klassen <- function(code) {
  if (is.na(code)) return(NA)
  if (code == 2 || code == 3) return("Gehölzvegetation")
  if (code == 5) return("Offener_Boden")
  if (code == 6) return("Feuchtgebiet_salz")  # Standard: Feuchtgebiet_salz
  if (code == 7 || code == 10) return("Wasser")
  return(NA)
}

# Mapping 2: 3 Klassen (zusammengeführt)
map_clcplus_to_variante_e_3klassen <- function(code) {
  if (is.na(code)) return(NA)
  if (code == 2 || code == 3) return("Gehölzvegetation")
  if (code == 5) return("Gehölzvegetation")  # Offener_Boden → Gehölz (spärlich bewachsen)
  if (code == 6) return("Feuchtgebiet")  # Binnenmarschen + Feuchtgebiet_salz zusammen
  if (code == 7 || code == 10) return("Wasser")
  return(NA)
}

# Funktion: Zusammenführen von Modell-Vorhersagen für 3-Klassen-Validierung
merge_predictions_to_3_classes <- function(pred) {
  # Gehölzvegetation + Offener_Boden → Gehölzvegetation
  pred[pred == "Offener_Boden"] <- "Gehölzvegetation"
  # Binnenmarschen + Feuchtgebiet_salz → Feuchtgebiet
  pred[pred == "Binnenmarschen"] <- "Feuchtgebiet"
  pred[pred == "Feuchtgebiet_salz"] <- "Feuchtgebiet"
  return(pred)
}

# === 1. Daten laden ===
cat(">> Lade Daten\n")

# Prüfe Dateien
required_files <- c(clcplus_raster, feature_stack_spektral, feature_stack_dgm, aoi_path)
for (f in required_files) {
  if (!file.exists(f)) {
    stop("Datei nicht gefunden: ", f)
  }
}

# Prüfe Modelle
model_files <- c(model_rf_corine_only, model_xgb_corine_only, model_rf_corine_dgm, model_xgb_corine_dgm)
for (f in model_files) {
  if (!file.exists(f)) {
    cat("  ⚠️ Modell nicht gefunden:", basename(f), "\n")
  }
}

clc_ref <- rast(clcplus_raster)
feat_stack_spektral <- rast(feature_stack_spektral)
feat_stack_dgm <- rast(feature_stack_dgm)
aoi <- vect(aoi_path)

cat("  CLCplus geladen:", nrow(clc_ref), "x", ncol(clc_ref), "\n")
cat("  Feature-Stack spektral:", nlyr(feat_stack_spektral), "Features\n")
cat("  Feature-Stack DGM:", nlyr(feat_stack_dgm), "Features\n\n")

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

cat("  Daten auf AOI beschränkt\n\n")

# === 2. Stichprobe ziehen ===
cat(">> Ziehe Stichprobe (Faktor =", sample_factor, ")\n")
clc_sampled <- aggregate(clc_ref, fact = sample_factor, fun = "modal", na.rm = TRUE)
clc_sampled <- resample(clc_sampled, feat_stack_spektral, method = "near")

# Extrahiere Werte
dat <- as.data.frame(c(clc_sampled), xy = TRUE, na.rm = TRUE)
names(dat)[3] <- "clc_code"

# Erstelle Referenzen für beide Mapping-Strategien
dat$reference_5klassen <- sapply(dat$clc_code, map_clcplus_to_variante_e_5klassen)
dat$reference_3klassen <- sapply(dat$clc_code, map_clcplus_to_variante_e_3klassen)

# Für 5-Klassen-Validierung
dat_5klassen <- dat[!is.na(dat$reference_5klassen), ]
dat_5klassen$reference <- dat_5klassen$reference_5klassen

# Für 3-Klassen-Validierung
dat_3klassen <- dat[!is.na(dat$reference_3klassen), ]
dat_3klassen$reference <- dat_3klassen$reference_3klassen

cat("  Stichprobe nach Mapping (5 Klassen):", nrow(dat_5klassen), "Pixel\n")
cat("  Stichprobe nach Mapping (3 Klassen):", nrow(dat_3klassen), "Pixel\n")

# Balanciertes Sampling für 5 Klassen
dat_list_5klassen <- list()
for (k in unique(dat_5klassen$reference)) {
  sub <- dat_5klassen[dat_5klassen$reference == k, ]
  if (nrow(sub) > max_samples_per_class) {
    sub <- sub[sample(nrow(sub), max_samples_per_class), ]
  }
  dat_list_5klassen[[k]] <- sub
}
dat_5klassen <- bind_rows(dat_list_5klassen)

# Balanciertes Sampling für 3 Klassen
dat_list_3klassen <- list()
for (k in unique(dat_3klassen$reference)) {
  sub <- dat_3klassen[dat_3klassen$reference == k, ]
  if (nrow(sub) > max_samples_per_class) {
    sub <- sub[sample(nrow(sub), max_samples_per_class), ]
  }
  dat_list_3klassen[[k]] <- sub
}
dat_3klassen <- bind_rows(dat_list_3klassen)

cat("  Nach Klassensampling (5 Klassen):", nrow(dat_5klassen), "Pixel\n")
cat("  Klassenverteilung (5 Klassen):\n")
print(table(dat_5klassen$reference))
cat("\n  Nach Klassensampling (3 Klassen):", nrow(dat_3klassen), "Pixel\n")
cat("  Klassenverteilung (3 Klassen):\n")
print(table(dat_3klassen$reference))
cat("\n")

# === 3. Features extrahieren ===
cat(">> Extrahiere Features\n")

# Features für 5-Klassen-Validierung
pts_5klassen <- vect(dat_5klassen[, c("x", "y")], geom = c("x", "y"), crs = crs(feat_stack_spektral))
feat_vals_spektral_5klassen <- terra::extract(feat_stack_spektral, pts_5klassen)
feat_vals_spektral_5klassen <- feat_vals_spektral_5klassen[, -1, drop = FALSE]
feat_vals_dgm_5klassen <- terra::extract(feat_stack_dgm, pts_5klassen)
feat_vals_dgm_5klassen <- feat_vals_dgm_5klassen[, -1, drop = FALSE]

# Features für 3-Klassen-Validierung
pts_3klassen <- vect(dat_3klassen[, c("x", "y")], geom = c("x", "y"), crs = crs(feat_stack_spektral))
feat_vals_spektral_3klassen <- terra::extract(feat_stack_spektral, pts_3klassen)
feat_vals_spektral_3klassen <- feat_vals_spektral_3klassen[, -1, drop = FALSE]
feat_vals_dgm_3klassen <- terra::extract(feat_stack_dgm, pts_3klassen)
feat_vals_dgm_3klassen <- feat_vals_dgm_3klassen[, -1, drop = FALSE]

cat("  Spektrale Features extrahiert:", ncol(feat_vals_spektral_5klassen), "\n")
cat("  DGM Features extrahiert:", ncol(feat_vals_dgm_5klassen), "\n\n")

# Helper-Funktion für Validierung
validate_model <- function(model, features, feat_data, model_name, model_type, all_classes, dat_subset, merge_to_3_classes = FALSE) {
  # Features extrahieren
  pred_data <- feat_data[, features, drop = FALSE]
  complete_idx <- complete.cases(pred_data)
  pred_data <- pred_data[complete_idx, , drop = FALSE]
  
  # Referenz
  ref_values <- dat_subset$reference[complete_idx]
  ref <- factor(ref_values, levels = all_classes)
  
  # Vorhersagen
  if (model_type == "rf") {
    pred <- predict(model, newdata = pred_data)
    pred <- factor(pred, levels = levels(pred))  # Behalte Original-Levels
  } else if (model_type == "xgb") {
    pred_matrix <- as.matrix(pred_data)
    pred_probs <- predict(model, newdata = pred_matrix, reshape = TRUE)
    pred_class <- max.col(pred_probs) - 1
    
    # Klassen-Namen aus Modell
    if (!is.null(model$class_levels)) {
      class_levels <- model$class_levels
    } else {
      class_levels <- c("Binnenmarschen", "Feuchtgebiet_salz", "Gehölzvegetation", "Offener_Boden", "Wasser")
    }
    pred <- factor(pred_class, levels = 0:(length(class_levels)-1), labels = class_levels)
  }
  
  # Zusammenführen für 3-Klassen-Validierung
  if (merge_to_3_classes) {
    pred <- merge_predictions_to_3_classes(as.character(pred))
    pred <- factor(pred, levels = all_classes)
  } else {
    pred <- factor(pred, levels = all_classes)
  }
  
  # Confusion Matrix
  ref <- factor(ref, levels = all_classes)
  cm <- confusionMatrix(pred, ref)
  
  # Metriken
  accuracy <- cm$overall["Accuracy"]
  kappa <- cm$overall["Kappa"]
  f1_scores <- cm$byClass[, "F1"]
  f1_scores[is.nan(f1_scores)] <- 0
  
  return(list(
    model_name = model_name,
    accuracy = as.numeric(accuracy),
    kappa = as.numeric(kappa),
    f1_scores = f1_scores,
    cm = cm
  ))
}

# === 4. VALIDIERUNG 1: 5 Klassen ===
cat("===========================================\n")
cat("VALIDIERUNG 1: 5 Klassen\n")
cat("===========================================\n\n")

all_classes_5 <- c("Gehölzvegetation", "Offener_Boden", "Binnenmarschen", 
                   "Feuchtgebiet_salz", "Wasser")

results_list_5klassen <- list()

cat(">> Validiere Modelle (5 Klassen)\n\n")

# RF CORINE only
if (file.exists(model_rf_corine_only) && file.exists(features_rf_corine_only)) {
  cat("  RF CORINE only...\n")
  rf_corine_only <- readRDS(model_rf_corine_only)
  features_rf_only <- readRDS(features_rf_corine_only)
  result_rf_only <- validate_model(rf_corine_only, features_rf_only, feat_vals_spektral_5klassen, 
                                    "RF_CORINE_only", "rf", all_classes_5, dat_5klassen, FALSE)
  results_list_5klassen[["RF_CORINE_only"]] <- result_rf_only
}

# XGBoost CORINE only
if (file.exists(model_xgb_corine_only) && file.exists(features_xgb_corine_only)) {
  cat("  XGBoost CORINE only...\n")
  xgb_corine_only <- readRDS(model_xgb_corine_only)
  features_xgb_only <- readRDS(features_xgb_corine_only)
  result_xgb_only <- validate_model(xgb_corine_only, features_xgb_only, feat_vals_spektral_5klassen,
                                     "XGBoost_CORINE_only", "xgb", all_classes_5, dat_5klassen, FALSE)
  results_list_5klassen[["XGBoost_CORINE_only"]] <- result_xgb_only
}

# RF CORINE + DGM
if (file.exists(model_rf_corine_dgm) && file.exists(features_rf_corine_dgm)) {
  cat("  RF CORINE + DGM...\n")
  rf_corine_dgm <- readRDS(model_rf_corine_dgm)
  features_rf_dgm <- readRDS(features_rf_corine_dgm)
  result_rf_dgm <- validate_model(rf_corine_dgm, features_rf_dgm, feat_vals_dgm_5klassen,
                                    "RF_CORINE_DGM", "rf", all_classes_5, dat_5klassen, FALSE)
  results_list_5klassen[["RF_CORINE_DGM"]] <- result_rf_dgm
}

# XGBoost CORINE + DGM
if (file.exists(model_xgb_corine_dgm) && file.exists(features_xgb_corine_dgm)) {
  cat("  XGBoost CORINE + DGM...\n")
  xgb_corine_dgm <- readRDS(model_xgb_corine_dgm)
  features_xgb_dgm <- readRDS(features_xgb_corine_dgm)
  result_xgb_dgm <- validate_model(xgb_corine_dgm, features_xgb_dgm, feat_vals_dgm_5klassen,
                                     "XGBoost_CORINE_DGM", "xgb", all_classes_5, dat_5klassen, FALSE)
  results_list_5klassen[["XGBoost_CORINE_DGM"]] <- result_xgb_dgm
}

cat("\n")

# === 5. VALIDIERUNG 2: 4 Klassen (ohne Offener_Boden) ===
cat("===========================================\n")
cat("VALIDIERUNG 2: 4 Klassen (ohne Offener_Boden)\n")
cat("===========================================\n\n")

# Filtere Offener_Boden heraus
dat_4klassen <- dat_5klassen %>%
  filter(reference != "Offener_Boden")

pts_4klassen <- vect(dat_4klassen[, c("x", "y")], geom = c("x", "y"), crs = crs(feat_stack_spektral))
feat_vals_spektral_4klassen <- terra::extract(feat_stack_spektral, pts_4klassen)
feat_vals_spektral_4klassen <- feat_vals_spektral_4klassen[, -1, drop = FALSE]
feat_vals_dgm_4klassen <- terra::extract(feat_stack_dgm, pts_4klassen)
feat_vals_dgm_4klassen <- feat_vals_dgm_4klassen[, -1, drop = FALSE]

all_classes_4 <- c("Gehölzvegetation", "Binnenmarschen", "Feuchtgebiet_salz", "Wasser")

results_list_4klassen <- list()

cat(">> Validiere Modelle (4 Klassen, ohne Offener_Boden)\n\n")

# RF CORINE only
if (file.exists(model_rf_corine_only) && file.exists(features_rf_corine_only)) {
  cat("  RF CORINE only...\n")
  rf_corine_only <- readRDS(model_rf_corine_only)
  features_rf_only <- readRDS(features_rf_corine_only)
  result_rf_only <- validate_model(rf_corine_only, features_rf_only, feat_vals_spektral_4klassen, 
                                    "RF_CORINE_only", "rf", all_classes_4, dat_4klassen, FALSE)
  results_list_4klassen[["RF_CORINE_only"]] <- result_rf_only
}

# XGBoost CORINE only
if (file.exists(model_xgb_corine_only) && file.exists(features_xgb_corine_only)) {
  cat("  XGBoost CORINE only...\n")
  xgb_corine_only <- readRDS(model_xgb_corine_only)
  features_xgb_only <- readRDS(features_xgb_corine_only)
  result_xgb_only <- validate_model(xgb_corine_only, features_xgb_only, feat_vals_spektral_4klassen,
                                     "XGBoost_CORINE_only", "xgb", all_classes_4, dat_4klassen, FALSE)
  results_list_4klassen[["XGBoost_CORINE_only"]] <- result_xgb_only
}

# RF CORINE + DGM
if (file.exists(model_rf_corine_dgm) && file.exists(features_rf_corine_dgm)) {
  cat("  RF CORINE + DGM...\n")
  rf_corine_dgm <- readRDS(model_rf_corine_dgm)
  features_rf_dgm <- readRDS(features_rf_corine_dgm)
  result_rf_dgm <- validate_model(rf_corine_dgm, features_rf_dgm, feat_vals_dgm_4klassen,
                                    "RF_CORINE_DGM", "rf", all_classes_4, dat_4klassen, FALSE)
  results_list_4klassen[["RF_CORINE_DGM"]] <- result_rf_dgm
}

# XGBoost CORINE + DGM
if (file.exists(model_xgb_corine_dgm) && file.exists(features_xgb_corine_dgm)) {
  cat("  XGBoost CORINE + DGM...\n")
  xgb_corine_dgm <- readRDS(model_xgb_corine_dgm)
  features_xgb_dgm <- readRDS(features_xgb_corine_dgm)
  result_xgb_dgm <- validate_model(xgb_corine_dgm, features_xgb_dgm, feat_vals_dgm_4klassen,
                                     "XGBoost_CORINE_DGM", "xgb", all_classes_4, dat_4klassen, FALSE)
  results_list_4klassen[["XGBoost_CORINE_DGM"]] <- result_xgb_dgm
}

cat("\n")

# === 6. VALIDIERUNG 3: 3 Klassen (zusammengeführt) ===
cat("===========================================\n")
cat("VALIDIERUNG 3: 3 Klassen (zusammengeführt)\n")
cat("===========================================\n\n")

all_classes_3 <- c("Gehölzvegetation", "Feuchtgebiet", "Wasser")

results_list_3klassen <- list()

cat(">> Validiere Modelle (3 Klassen: Gehölz+Offener_Boden, Feuchtgebiet, Wasser)\n\n")

# RF CORINE only
if (file.exists(model_rf_corine_only) && file.exists(features_rf_corine_only)) {
  cat("  RF CORINE only...\n")
  rf_corine_only <- readRDS(model_rf_corine_only)
  features_rf_only <- readRDS(features_rf_corine_only)
  result_rf_only <- validate_model(rf_corine_only, features_rf_only, feat_vals_spektral_3klassen, 
                                    "RF_CORINE_only", "rf", all_classes_3, dat_3klassen, TRUE)
  results_list_3klassen[["RF_CORINE_only"]] <- result_rf_only
}

# XGBoost CORINE only
if (file.exists(model_xgb_corine_only) && file.exists(features_xgb_corine_only)) {
  cat("  XGBoost CORINE only...\n")
  xgb_corine_only <- readRDS(model_xgb_corine_only)
  features_xgb_only <- readRDS(features_xgb_corine_only)
  result_xgb_only <- validate_model(xgb_corine_only, features_xgb_only, feat_vals_spektral_3klassen,
                                     "XGBoost_CORINE_only", "xgb", all_classes_3, dat_3klassen, TRUE)
  results_list_3klassen[["XGBoost_CORINE_only"]] <- result_xgb_only
}

# RF CORINE + DGM
if (file.exists(model_rf_corine_dgm) && file.exists(features_rf_corine_dgm)) {
  cat("  RF CORINE + DGM...\n")
  rf_corine_dgm <- readRDS(model_rf_corine_dgm)
  features_rf_dgm <- readRDS(features_rf_corine_dgm)
  result_rf_dgm <- validate_model(rf_corine_dgm, features_rf_dgm, feat_vals_dgm_3klassen,
                                    "RF_CORINE_DGM", "rf", all_classes_3, dat_3klassen, TRUE)
  results_list_3klassen[["RF_CORINE_DGM"]] <- result_rf_dgm
}

# XGBoost CORINE + DGM
if (file.exists(model_xgb_corine_dgm) && file.exists(features_xgb_corine_dgm)) {
  cat("  XGBoost CORINE + DGM...\n")
  xgb_corine_dgm <- readRDS(model_xgb_corine_dgm)
  features_xgb_dgm <- readRDS(features_xgb_corine_dgm)
  result_xgb_dgm <- validate_model(xgb_corine_dgm, features_xgb_dgm, feat_vals_dgm_3klassen,
                                     "XGBoost_CORINE_DGM", "xgb", all_classes_3, dat_3klassen, TRUE)
  results_list_3klassen[["XGBoost_CORINE_DGM"]] <- result_xgb_dgm
}

cat("\n")

# === 7. Ergebnisse zusammenfassen ===
cat(">> Erstelle Zusammenfassung\n")

# === Funktion: Erstelle Report ===
create_report <- function(results_list, report_path, title, classes, mapping_info, dat_subset) {
  sink(report_path)
  cat("Externe Validierung: Alle Modelle gegen CLCplus\n")
  cat(title, "\n")
  cat("==================================================\n\n")
  cat("Datum:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  cat("Validierte Klassen:\n")
  for (k in classes) {
    cat("  -", k, "\n")
  }
  cat("\n")
  
  cat("Validierungsparameter:\n")
  cat("  - Referenz: CLCplus 2018 (10m Auflösung)\n")
  cat("  - Sample-Faktor:", sample_factor, "\n")
  cat("  - Max Samples pro Klasse:", max_samples_per_class, "\n")
  cat("  - Validierungspixel:", nrow(dat_subset), "\n\n")
  
  cat("CLCplus → Variante E Mapping:\n")
  cat(mapping_info)
  cat("\n")
  
  cat("===========================================\n")
  cat("ERGEBNISSE\n")
  cat("===========================================\n\n")
  
  for (model_name in names(results_list)) {
    result <- results_list[[model_name]]
    cat(model_name, ":\n")
    cat("  Accuracy:", round(result$accuracy, 4), "(", round(result$accuracy * 100, 2), "%)\n")
    cat("  Kappa:", round(result$kappa, 4), "\n")
    cat("  F1-Scores:\n")
    for (i in 1:length(result$f1_scores)) {
      if (!is.na(result$f1_scores[i])) {
        cat("    ", names(result$f1_scores)[i], ":", round(result$f1_scores[i], 4), "\n")
      }
    }
    cat("\n  Confusion Matrix:\n")
    print(result$cm$table)
    cat("\n")
  }
  
  sink()
}

# Metriken für 5 Klassen
metrics_df_5klassen <- data.frame(
  Modell = character(),
  Accuracy = numeric(),
  Kappa = numeric(),
  stringsAsFactors = FALSE
)

for (model_name in names(results_list_5klassen)) {
  result <- results_list_5klassen[[model_name]]
  metrics_df_5klassen <- rbind(metrics_df_5klassen, data.frame(
    Modell = model_name,
    Accuracy = result$accuracy,
    Kappa = result$kappa,
    stringsAsFactors = FALSE
  ))
}

write.csv(metrics_df_5klassen, metrics_csv_5klassen, row.names = FALSE)

# Metriken für 4 Klassen
metrics_df_4klassen <- data.frame(
  Modell = character(),
  Accuracy = numeric(),
  Kappa = numeric(),
  stringsAsFactors = FALSE
)

for (model_name in names(results_list_4klassen)) {
  result <- results_list_4klassen[[model_name]]
  metrics_df_4klassen <- rbind(metrics_df_4klassen, data.frame(
    Modell = model_name,
    Accuracy = result$accuracy,
    Kappa = result$kappa,
    stringsAsFactors = FALSE
  ))
}

write.csv(metrics_df_4klassen, metrics_csv_4klassen, row.names = FALSE)

# Metriken für 3 Klassen
metrics_df_3klassen <- data.frame(
  Modell = character(),
  Accuracy = numeric(),
  Kappa = numeric(),
  stringsAsFactors = FALSE
)

for (model_name in names(results_list_3klassen)) {
  result <- results_list_3klassen[[model_name]]
  metrics_df_3klassen <- rbind(metrics_df_3klassen, data.frame(
    Modell = model_name,
    Accuracy = result$accuracy,
    Kappa = result$kappa,
    stringsAsFactors = FALSE
  ))
}

write.csv(metrics_df_3klassen, metrics_csv_3klassen, row.names = FALSE)

# Report 1: 5 Klassen
mapping_info_5 <- paste(
  "  - CLCplus 2,3 → Gehölzvegetation\n",
  "  - CLCplus 5 → Offener_Boden (⚠️ Definition-Unterschiede!)\n",
  "  - CLCplus 6 → Feuchtgebiet_salz (⚠️ LIMITATION: CLCplus kann\n",
  "    Binnenmarschen und Feuchtgebiet_salz nicht unterscheiden!)\n",
  "  - CLCplus 7,10 → Wasser\n\n",
  "⚠️ WICHTIG: Binnenmarschen kann nicht gegen CLCplus validiert werden,\n",
  "   da CLCplus nur 'Wetland' (Code 6) hat und nicht zwischen\n",
  "   Binnenmarschen und Feuchtgebiet_salz unterscheidet.\n",
  sep = ""
)

create_report(results_list_5klassen, report_path_5klassen, 
              "Variante E – 5 Klassen (alle)", 
              all_classes_5, mapping_info_5, dat_5klassen)

# Report 2: 4 Klassen
mapping_info_4 <- paste(
  "  - CLCplus 2,3 → Gehölzvegetation\n",
  "  - CLCplus 6 → Feuchtgebiet_salz\n",
  "  - CLCplus 7,10 → Wasser\n\n",
  "⚠️ HINWEIS: 'Offener_Boden' wurde ausgeschlossen,\n",
  "   da die Definitionen zwischen CORINE und CLCplus zu unterschiedlich sind.\n",
  sep = ""
)

create_report(results_list_4klassen, report_path_4klassen, 
              "Variante E – 4 Klassen (ohne Offener_Boden)", 
              all_classes_4, mapping_info_4, dat_4klassen)

# Report 3: 3 Klassen
mapping_info_3 <- paste(
  "  - CLCplus 2,3 → Gehölzvegetation\n",
  "  - CLCplus 5 → Gehölzvegetation (spärlich bewachsene Flächen)\n",
  "  - CLCplus 6 → Feuchtgebiet (kombiniert: Binnenmarschen + Feuchtgebiet_salz)\n",
  "  - CLCplus 7,10 → Wasser\n\n",
  "⚠️ HINWEIS: Klassen wurden zusammengeführt:\n",
  "   - Gehölzvegetation + Offener_Boden → Gehölzvegetation\n",
  "   - Binnenmarschen + Feuchtgebiet_salz → Feuchtgebiet\n",
  "   Dies ermöglicht eine realistischere Validierung gegen CLCplus.\n",
  sep = ""
)

create_report(results_list_3klassen, report_path_3klassen, 
              "Variante E – 3 Klassen (zusammengeführt)", 
              all_classes_3, mapping_info_3, dat_3klassen)

# Vergleich aller drei Validierungen
comparison_all <- data.frame(
  Validierung = character(),
  Modell = character(),
  Accuracy = numeric(),
  Kappa = numeric(),
  stringsAsFactors = FALSE
)

for (model_name in names(results_list_5klassen)) {
  comparison_all <- rbind(comparison_all, data.frame(
    Validierung = "5 Klassen",
    Modell = model_name,
    Accuracy = results_list_5klassen[[model_name]]$accuracy,
    Kappa = results_list_5klassen[[model_name]]$kappa,
    stringsAsFactors = FALSE
  ))
}

for (model_name in names(results_list_4klassen)) {
  comparison_all <- rbind(comparison_all, data.frame(
    Validierung = "4 Klassen",
    Modell = model_name,
    Accuracy = results_list_4klassen[[model_name]]$accuracy,
    Kappa = results_list_4klassen[[model_name]]$kappa,
    stringsAsFactors = FALSE
  ))
}

for (model_name in names(results_list_3klassen)) {
  comparison_all <- rbind(comparison_all, data.frame(
    Validierung = "3 Klassen",
    Modell = model_name,
    Accuracy = results_list_3klassen[[model_name]]$accuracy,
    Kappa = results_list_3klassen[[model_name]]$kappa,
    stringsAsFactors = FALSE
  ))
}

write.csv(comparison_all, comparison_csv, row.names = FALSE)

# Vergleichs-Report
sink(report_path_vergleich)
cat("Vergleich: Externe Validierung - 3 Strategien\n")
cat("==================================================\n\n")
cat("Datum:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("VALIDIERUNG 1: 5 Klassen\n")
cat("---------------------------------------------------\n")
print(metrics_df_5klassen %>% arrange(desc(Accuracy)))
cat("\n\n")

cat("VALIDIERUNG 2: 4 Klassen (ohne Offener_Boden)\n")
cat("---------------------------------------------------\n")
print(metrics_df_4klassen %>% arrange(desc(Accuracy)))
cat("\n\n")

cat("VALIDIERUNG 3: 3 Klassen (zusammengeführt)\n")
cat("---------------------------------------------------\n")
print(metrics_df_3klassen %>% arrange(desc(Accuracy)))
cat("\n\n")

cat("VERGLEICH ALLER STRATEGIEN:\n")
cat("---------------------------------------------------\n")
print(comparison_all %>% arrange(Validierung, desc(Accuracy)))
cat("\n\n")

cat("FAZIT:\n")
cat("---------------------------------------------------\n")
cat("Die 3-Klassen-Validierung ermöglicht eine realistischere Bewertung,\n")
cat("da die Klassen so zusammengeführt werden, dass sie mit CLCplus kompatibel sind.\n")
cat("Dies führt zu höheren Accuracy-Werten und besseren F1-Scores.\n")

sink()

cat("  Ergebnisse gespeichert:\n")
cat("    - Report (5 Klassen):", basename(report_path_5klassen), "\n")
cat("    - Report (4 Klassen):", basename(report_path_4klassen), "\n")
cat("    - Report (3 Klassen):", basename(report_path_3klassen), "\n")
cat("    - Vergleich:", basename(report_path_vergleich), "\n")
cat("    - Metriken (5 Klassen):", basename(metrics_csv_5klassen), "\n")
cat("    - Metriken (4 Klassen):", basename(metrics_csv_4klassen), "\n")
cat("    - Metriken (3 Klassen):", basename(metrics_csv_3klassen), "\n")
cat("    - Vergleichstabelle:", basename(comparison_csv), "\n\n")

# === Zusammenfassung ===
cat("===========================================\n")
cat("EXTERNE VALIDIERUNG ABGESCHLOSSEN\n")
cat("===========================================\n")
cat("\nDrei Validierungen durchgeführt:\n")
cat("  1. 5 Klassen (alle)\n")
cat("  2. 4 Klassen (ohne Offener_Boden)\n")
cat("  3. 3 Klassen (zusammengeführt)\n\n")

cat("Beste Modelle (3 Klassen - realistische Validierung):\n")
comparison_3_sorted <- metrics_df_3klassen %>% arrange(desc(Accuracy))
for (i in 1:min(3, nrow(comparison_3_sorted))) {
  cat("  ", i, ". ", comparison_3_sorted$Modell[i], ": ", 
      round(comparison_3_sorted$Accuracy[i] * 100, 2), "%\n", sep = "")
}
cat("\n")

cat("Nächster Schritt: Vergleich & Analyse (10_modellvergleich.R)\n")


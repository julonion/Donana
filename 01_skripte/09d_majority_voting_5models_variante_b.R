#!/usr/bin/env Rscript

# ===========================================
# Phase 3: Majority Voting Ensemble (5 Modelle)
# Ziel-Klassen (Variante B):
# 1. Wald
# 2. Buschland
# 3. Offener_Boden
# 4. Feuchtgebiet
# 5. Wasser
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

# Ausgabedateien
output_dir <- file.path(work_dir, "03_ergebnisse", "externe_validierung", "majority_voting_5models_var_b")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

report_path <- file.path(output_dir, "ensemble_5models_report.txt")
metrics_csv <- file.path(output_dir, "ensemble_5models_metrics.csv")

# === Pfade laden ===
paths_file <- file.path(work_dir, "02_daten", "input", "workflow_paths.rds")
if (!file.exists(paths_file)) stop("Pfade nicht gefunden.")
paths <- readRDS(paths_file)

# Eingabedaten
clcplus_raster <- paths$clcplus_raster
if (!file.exists(clcplus_raster)) clcplus_raster <- paths$clcplus_raster_alt
feature_stack_spektral <- paths$feature_stack_2018
feature_stack_dgm <- paths$feature_stack_2018_dgm
aoi_path <- paths$aoi

# === 1. MODELLE DEFINIEREN ===
cat(">> Definiere Modelle\n")

# Pfad zum Baseline Modell (Archiv)
baseline_model_path <- file.path(root_dir, "Archiv", "corine_new_class", "Modell_Baseline", "02_daten", "modelle", "rf_model_baseline.rds")
baseline_features_path <- file.path(root_dir, "Archiv", "corine_new_class", "Modell_Baseline", "02_daten", "modelle", "rf_baseline_features.rds")

# HINWEIS: Hier gehe ich davon aus, dass du die 4 neuen Modelle als "variante_b" gespeichert hast.
# Falls sie anders heißen, bitte hier anpassen!
models_config <- list(
  # 1. RF Sentinel (Variante B)
  rf_only = list(
    path = file.path(work_dir, "02_daten", "modelle", "rf_model_corine_only_variante_b.rds"),
    feat = file.path(work_dir, "02_daten", "modelle", "rf_corine_only_variante_b_features.rds"),
    type = "rf_var_b", stack = "spektral"
  ),
  # 2. XGB Sentinel (Variante B)
  xgb_only = list(
    path = file.path(work_dir, "02_daten", "modelle", "xgb_model_corine_only_variante_b.rds"),
    feat = file.path(work_dir, "02_daten", "modelle", "xgb_corine_only_variante_b_features.rds"),
    type = "xgb_var_b", stack = "spektral"
  ),
  # 3. RF Sentinel + DGM (Variante B)
  rf_dgm = list(
    path = file.path(work_dir, "02_daten", "modelle", "rf_model_corine_dgm_variante_b.rds"),
    feat = file.path(work_dir, "02_daten", "modelle", "rf_corine_dgm_variante_b_features.rds"),
    type = "rf_var_b", stack = "dgm"
  ),
  # 4. XGB Sentinel + DGM (Variante B)
  xgb_dgm = list(
    path = file.path(work_dir, "02_daten", "modelle", "xgb_model_corine_dgm_variante_b.rds"),
    feat = file.path(work_dir, "02_daten", "modelle", "xgb_corine_dgm_variante_b_features.rds"),
    type = "xgb_var_b", stack = "dgm"
  ),
  # 5. Baseline Modell (Original aus Hausarbeit)
  baseline = list(
    path = baseline_model_path,
    feat = baseline_features_path,
    type = "rf_baseline", stack = "spektral"
  )
)

# === MAPPING FUNKTIONEN ===

# Mapping Baseline -> Variante B (5 Klassen)
map_baseline_to_target <- function(pred) {
  pred <- as.character(pred)
  # Wald -> Wald
  pred[pred == "Wald"] <- "Wald"
  pred[pred == "Nadelwald"] <- "Wald" 
  # Hartlaub -> Buschland
  pred[pred == "Hartlaubvegetation"] <- "Buschland"
  # Salzwiesen -> Feuchtgebiet
  pred[pred == "Salzwiesen"] <- "Feuchtgebiet"
  # Sand -> Offener_Boden
  pred[pred == "Straende_Duenen_Sand"] <- "Offener_Boden"
  # Wasser -> Wasser
  pred[pred == "wasserflaechen"] <- "Wasser"
  pred[pred == "Wasserflächen"] <- "Wasser"
  
  return(pred)
}

# Mapping CLCplus Referenz -> Variante B (5 Klassen)
map_clc_to_target <- function(code) {
  if (is.na(code)) return(NA)
  
  # Wald (311, 312) -> CLCplus Code 2 (Forest)
  if (code == 2) return("Wald")
  
  # Buschland (323, 324, 231) -> CLCplus Code 3 (Scrub/Herbaceous)
  if (code == 3) return("Buschland")
  
  # Offener Boden (331, 333) -> CLCplus Code 5 (Bare/Sand)
  if (code == 5) return("Offener_Boden")
  
  # Feuchtgebiet (411, 421, 422) -> CLCplus Code 6 (Wetland)
  if (code == 6) return("Feuchtgebiet")
  
  # Wasser (511, 512) -> CLCplus Code 7 (Water) / 10 (Marine)
  if (code == 7 || code == 10) return("Wasser")
  
  return(NA)
}

# === 2. DATEN VORBEREITEN ===
cat(">> Lade Raster und AOI\n")
feat_stack_spektral <- rast(feature_stack_spektral)
feat_stack_dgm <- rast(feature_stack_dgm)
aoi <- vect(aoi_path)
clc_ref <- rast(clcplus_raster)

# Angleichen
if (!identical(crs(aoi), crs(feat_stack_spektral))) aoi <- project(aoi, crs(feat_stack_spektral))
feat_stack_spektral <- mask(crop(feat_stack_spektral, aoi), aoi)
feat_stack_dgm <- mask(crop(feat_stack_dgm, aoi), aoi)
clc_ref <- project(clc_ref, crs(feat_stack_spektral), method = "near")
clc_ref <- mask(crop(clc_ref, feat_stack_spektral), aoi)

# Sampling
cat(">> Ziehe Stichprobe für Validierung\n")
set.seed(42)
sample_factor <- 5
clc_sampled <- aggregate(clc_ref, fact = sample_factor, fun = "modal", na.rm = TRUE)
clc_sampled <- resample(clc_sampled, feat_stack_spektral, method = "near")

dat <- as.data.frame(c(clc_sampled), xy = TRUE, na.rm = TRUE)
names(dat)[3] <- "clc_code"
dat$reference <- sapply(dat$clc_code, map_clc_to_target)
dat <- dat[!is.na(dat$reference), ]

# Balanciertes Sampling (max 2000 pro Klasse)
dat_list <- list()
for (k in unique(dat$reference)) {
  sub <- dat[dat$reference == k, ]
  if (nrow(sub) > 2000) sub <- sub[sample(nrow(sub), 2000), ]
  dat_list[[k]] <- sub
}
dat <- bind_rows(dat_list)
cat("  Validierungspunkte:", nrow(dat), "\n")
print(table(dat$reference))
cat("\n")

# Features extrahieren
cat(">> Extrahiere Features\n")
pts <- vect(dat[, c("x", "y")], geom = c("x", "y"), crs = crs(feat_stack_spektral))
vals_spektral <- terra::extract(feat_stack_spektral, pts)[, -1, drop = FALSE]
vals_dgm <- terra::extract(feat_stack_dgm, pts)[, -1, drop = FALSE]

# === 3. VORHERSAGEN ERSTELLEN ===
cat(">> Erstelle Vorhersagen für 5 Modelle\n")
predictions_df <- data.frame(row_id = 1:nrow(dat))

for (name in names(models_config)) {
  cfg <- models_config[[name]]
  
  if (!file.exists(cfg$path) || !file.exists(cfg$feat)) {
    cat("  ⚠️ Modell", name, "nicht gefunden. Überspringe.\n")
    next
  }
  
  cat("  Modell:", name, "\n")
  model <- readRDS(cfg$path)
  feats <- readRDS(cfg$feat)
  
  # Wähle passenden Feature-Datensatz
  if (cfg$stack == "dgm") {
    pred_data <- vals_dgm
  } else {
    pred_data <- vals_spektral
  }
  
  # Features prüfen
  missing <- setdiff(feats, names(pred_data))
  if (length(missing) > 0) {
    cat("    ⚠️ Fehlende Features:", paste(missing, collapse=", "), "\n")
    next
  }
  
  # Vorhersage
  p_data <- pred_data[, feats, drop = FALSE]
  
  if (grepl("rf", cfg$type)) {
    # Random Forest
    pred_raw <- predict(model, newdata = p_data)
  } else {
    # XGBoost
    dmat <- as.matrix(p_data)
    probs <- predict(model, newdata = dmat, reshape = TRUE)
    idx <- max.col(probs) - 1
    
    # Label-Rekonstruktion
    # Variante B Labels (alphabetisch: Buschland, Feuchtgebiet, Offener_Boden, Wald, Wasser)
    labels <- sort(c("Wald", "Buschland", "Offener_Boden", "Feuchtgebiet", "Wasser"))
    if (!is.null(model$class_levels)) labels <- model$class_levels
    pred_raw <- factor(idx, levels = 0:(length(labels)-1), labels = labels)
  }
  
  # MAPPING: Nur für Baseline nötig!
  if (cfg$type == "rf_baseline") {
    pred_mapped <- map_baseline_to_target(pred_raw)
  } else {
    # Variante B Modelle haben schon die richtigen Klassen
    pred_mapped <- as.character(pred_raw)
  }
  
  predictions_df[[name]] <- pred_mapped
}

# === 4. MAJORITY VOTING ===
cat("\n>> Berechne Majority Vote\n")

if (ncol(predictions_df) <= 1) {
  stop("Keine Modelle erfolgreich geladen! Bitte prüfe, ob die Modelle (Variante B) trainiert wurden.")
}

vote_data <- predictions_df[, -1, drop = FALSE]

# Funktion für Majority Vote
get_majority <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  tab <- table(x)
  names(tab)[which.max(tab)]
}

ensemble_pred <- apply(vote_data, 1, get_majority)
target_levels <- c("Wald", "Buschland", "Offener_Boden", "Feuchtgebiet", "Wasser")
ensemble_pred <- factor(ensemble_pred, levels = target_levels)
ref <- factor(dat$reference, levels = target_levels)

# === 5. VALIDIERUNG ===
cm <- confusionMatrix(ensemble_pred, ref)

cat("\n===========================================\n")
cat("ERGEBNISSE ENSEMBLE (5 MODELLE - VARIANTE B)\n")
cat("===========================================\n")
cat("Klassen: Wald, Buschland, Offener_Boden, Feuchtgebiet, Wasser\n")
cat("Accuracy:", round(cm$overall["Accuracy"], 4), "\n")
cat("Kappa:   ", round(cm$overall["Kappa"], 4), "\n\n")
print(cm$table)

# Speichern
sink(report_path)
cat("Ensemble Report (5 Modelle - Variante B)\n")
cat("Datum:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
cat("Klassen:\n")
cat("  - Wald (Nadel/Laub)\n")
cat("  - Buschland (Hartlaub/Weideland)\n")
cat("  - Offener_Boden (Sand/Spärlich)\n")
cat("  - Feuchtgebiet (Salz/Binnen)\n")
cat("  - Wasser\n\n")
cat("Verwendete Modelle:\n")
print(names(vote_data))
cat("\n")
print(cm)
sink()

# Metriken CSV
metrics_df <- data.frame(
  Modell = "Ensemble_5Models_VarB",
  Accuracy = as.numeric(cm$overall["Accuracy"]),
  Kappa = as.numeric(cm$overall["Kappa"])
)
write.csv(metrics_df, metrics_csv, row.names = FALSE)

cat("\nFertig! Report gespeichert in:", report_path, "\n")
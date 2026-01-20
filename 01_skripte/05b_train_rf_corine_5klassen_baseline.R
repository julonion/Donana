#!/usr/bin/env Rscript

# ===========================================
# Phase 2 (Variante Baseline-kompatibel):
# Random Forest Training
# 5 Klassen: Hartlaubvegetation, Salzwiesen, Wasser, Wald, Sand
# Nur spektrale Features (ohne DGM)
# ===========================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(caret)
  library(randomForest)
})

# === Parameter ===
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"
work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"

# Eingabedatei: Trainingsdaten aus 03b_extract_training_corine_5klassen_baseline.R
samples_csv <- file.path(
  work_dir,
  "02_daten",
  "training",
  "samples_corine_2018_5klassen_baseline_kompatibel.csv"
)

# Ausgabedateien
model_output <- file.path(
  work_dir,
  "02_daten",
  "modelle",
  "rf_model_corine_5klassen_baseline_kompatibel.rds"
)
metrics_output <- file.path(
  work_dir,
  "03_ergebnisse",
  "interne_validierung",
  "rf_corine_5klassen_baseline_kompatibel_metrics.txt"
)
feature_list_path <- file.path(
  work_dir,
  "02_daten",
  "modelle",
  "rf_corine_5klassen_baseline_kompatibel_features.rds"
)

dir.create(dirname(metrics_output), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(model_output), showWarnings = FALSE, recursive = TRUE)

cat("===========================================\n")
cat("Random Forest Training\n")
cat("5 Klassen – Baseline-kompatibel (CORINE Trainingsdaten)\n")
cat("Klassen: Hartlaubvegetation, Salzwiesen, Wasser, Wald, Sand\n")
cat("===========================================\n\n")

# === Prüfungen ===
if (!file.exists(samples_csv)) {
  stop("Trainingsdaten nicht gefunden. Bitte zuerst '03b_extract_training_corine_5klassen_baseline.R' ausführen!")
}

# === Trainingsdaten laden ===
cat(">> Lade Trainingsdaten\n")
cat("  Datei:", basename(samples_csv), "\n")
samples <- read_csv(samples_csv, show_col_types = FALSE)
cat("  Samples geladen:", nrow(samples), "\n\n")

# === Features vorbereiten ===
cat(">> Identifiziere Features\n")
drop_cols <- c(
  "polygon_id", "Code_18", "area_km2", "quell_jahr",
  "quelle", "class_id", "x", "y"
)

fehlende_drop_cols <- setdiff(drop_cols, names(samples))
if (length(fehlende_drop_cols) > 0) {
  cat("  Hinweis: Folgende Drop-Spalten existieren nicht:\n")
  print(fehlende_drop_cols)
}

feature_cols <- setdiff(names(samples), c(drop_cols, "class_name"))

cat("  Anzahl Features:", length(feature_cols), "\n")
cat("  Features:", paste(feature_cols, collapse = ", "), "\n\n")

# Feature-Namen speichern
saveRDS(feature_cols, feature_list_path)
cat("  Feature-Namen gespeichert:", basename(feature_list_path), "\n\n")

# === Daten vorbereiten ===
cat(">> Bereite Daten vor\n")
train_df <- samples %>%
  select(all_of(feature_cols)) %>%
  mutate(class_name = factor(samples$class_name))

# Fehlende Werte entfernen
vollstaendig <- complete.cases(train_df)
if (!all(vollstaendig)) {
  cat("  Entferne", sum(!vollstaendig), "Samples mit fehlenden Feature-Werten\n")
  train_df <- train_df[vollstaendig, ]
}

cat("  Samples nach Bereinigung:", nrow(train_df), "\n")
cat("  Klassenverteilung:\n")
print(table(train_df$class_name))
cat("\n")

# === Standard Train/Test Split (stratifiziert) ===
cat(">> Erstelle Standard Train/Test Split (stratified)\n")
set.seed(42)
train_idx <- createDataPartition(train_df$class_name, p = 0.8, list = FALSE)
train_set <- train_df[train_idx, ]
test_set  <- train_df[-train_idx, ]

cat("  Trainingssamples:", nrow(train_set), "\n")
cat("  Validierungssamples:", nrow(test_set), "\n")
cat("  Klassen im Trainingsset:", length(unique(train_set$class_name)), "von",
    length(unique(train_df$class_name)), "\n")
cat("  Klassenverteilung (Training):\n")
train_dist <- table(train_set$class_name)
print(train_dist)
cat("\n")

# === Random Forest Training ===
cat(">> Trainiere Random Forest\n")
cat("  Parameter:\n")
cat("    - ntree: 500\n")
cat("    - mtry: sqrt(n_features)\n")
cat("    - Features:", length(feature_cols), "\n\n")

set.seed(42)
train_start <- Sys.time()

rf_model <- randomForest(
  x       = train_set[, feature_cols],
  y       = train_set$class_name,
  ntree   = 500,
  mtry    = floor(sqrt(length(feature_cols))),
  importance = TRUE,
  do.trace   = 50
)

train_end   <- Sys.time()
train_dauer <- as.numeric(difftime(train_end, train_start, units = "secs"))

cat("  Training abgeschlossen (Dauer:", round(train_dauer, 2), "Sekunden)\n\n")

# === Modell speichern ===
cat(">> Speichere Modell\n")
saveRDS(rf_model, model_output)
cat("  Modell gespeichert:", basename(model_output), "\n\n")

# === Vorhersagen auf Validierungs-Set ===
cat(">> Evaluierung auf Validierungs-Set\n")
test_pred    <- predict(rf_model, newdata = test_set[, feature_cols])
test_cm      <- confusionMatrix(test_pred, test_set$class_name)
test_acc     <- test_cm$overall["Accuracy"]
test_kappa   <- test_cm$overall["Kappa"]
f1_scores    <- test_cm$byClass[, "F1"]

cat("  Test Accuracy:", round(as.numeric(test_acc), 4),
    "(", round(as.numeric(test_acc) * 100, 2), "%)\n")
cat("  Test Kappa:", round(as.numeric(test_kappa), 4), "\n\n")

cat("  Klassen-spezifische F1-Scores:\n")
for (i in 1:length(f1_scores)) {
  cat("    ", names(f1_scores)[i], ":", round(f1_scores[i], 4), "\n")
}
cat("\n")

# === Feature Importance ===
cat(">> Top Features (Mean Decrease Gini):\n")
feature_importance <- importance(rf_model)[, "MeanDecreaseGini"]
top_features <- sort(feature_importance, decreasing = TRUE)[1:min(10, length(feature_importance))]
for (i in 1:length(top_features)) {
  cat("  ", i, ". ", names(top_features)[i], ": ",
      round(top_features[i], 2), "\n", sep = "")
}
cat("\n")

# === Metriken in Datei schreiben ===
cat(">> Speichere Metriken\n")
sink(metrics_output)
cat("Random Forest – 5 Klassen (Baseline-kompatibel)\n")
cat("==============================================\n\n")
cat("Datum:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
cat("Train/Test-Split: 80/20, stratifiziert\n\n")

cat("Accuracy:", round(as.numeric(test_acc), 4),
    "(", round(as.numeric(test_acc) * 100, 2), "%)\n")
cat("Kappa   :", round(as.numeric(test_kappa), 4), "\n\n")

cat("Klassen-spezifische F1-Scores:\n")
for (i in 1:length(f1_scores)) {
  cat("  ", names(f1_scores)[i], ": ", round(f1_scores[i], 4), "\n", sep = "")
}
cat("\n")

cat("Confusion Matrix:\n")
print(test_cm$table)
cat("\n")

sink()

cat("  Metriken gespeichert:", basename(metrics_output), "\n\n")
cat("===========================================\n")
cat("TRAINING ABGESCHLOSSEN (5 KLASSEN, BASELINE-KOMPATIBEL)\n")
cat("===========================================\n")



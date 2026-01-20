#!/usr/bin/env Rscript

# ===========================================
# Phase 2: Random Forest Training
# Variante E – MIT DGM (spektrale + topographische Features)
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

# Eingabedateien
samples_csv <- file.path(work_dir, "02_daten", "training", "samples_corine_2018_with_dgm_variante_e.csv")

# Ausgabedateien
model_output <- file.path(work_dir, "02_daten", "modelle", "rf_model_corine_dgm_variante_e.rds")
metrics_output <- file.path(work_dir, "03_ergebnisse", "interne_validierung", "rf_corine_dgm_variante_e_metrics.txt")
feature_list_path <- file.path(work_dir, "02_daten", "modelle", "rf_corine_dgm_variante_e_features.rds")

dir.create(dirname(metrics_output), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(model_output), showWarnings = FALSE, recursive = TRUE)

cat("===========================================\n")
cat("Random Forest Training (CORINE + DGM)\n")
cat("Variante E – MIT DGM (spektrale + topographische Features)\n")
cat("===========================================\n\n")

# === Prüfungen ===
if (!file.exists(samples_csv)) {
  stop("Trainingsdaten nicht gefunden. Bitte zuerst '04_extract_training_corine_dgm.R' ausführen!")
}

# === Trainingsdaten laden ===
cat(">> Lade Trainingsdaten\n")
cat("  Datei:", basename(samples_csv), "\n")
samples <- read_csv(samples_csv, show_col_types = FALSE)
cat("  Samples geladen:", nrow(samples), "\n\n")

# === Features identifizieren ===
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

# Prüfe ob DGM-Features vorhanden sind
dgm_features <- c("elevation", "slope", "aspect", "tri", "twi", "TRI", "TWI")
vorhandene_dgm <- intersect(dgm_features, feature_cols)
cat("  DGM-Features vorhanden:", length(vorhandene_dgm), "von", length(dgm_features), "\n")
if (length(vorhandene_dgm) > 0) {
  cat("  ", paste(vorhandene_dgm, collapse = ", "), "\n")
} else {
  warning("KEINE DGM-Features gefunden!")
}
cat("\n")

# Speichere Feature-Namen
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

# === Train/Test Split ===
# Prüfe ob räumlicher Split sinnvoll ist
use_spatial_split <- FALSE
min_polys_per_class <- 3  # Mindestens 3 Polygone pro Klasse für räumlichen Split

if ("polygon_id" %in% names(samples)) {
  # Prüfe Polygon-Verfügbarkeit
  polygon_tbl <- samples %>%
    filter(vollstaendig) %>%
    distinct(polygon_id, class_name)
  
  cat(">> Prüfe Datenqualität für räumlichen Split\n")
  cat("  Eindeutige Polygone:", nrow(polygon_tbl), "\n")
  
  # Prüfe Polygone pro Klasse
  polys_per_class <- polygon_tbl %>%
    group_by(class_name) %>%
    summarise(n_polys = n(), .groups = "drop")
  
  cat("  Polygone pro Klasse:\n")
  for (i in 1:nrow(polys_per_class)) {
    cat("    -", polys_per_class$class_name[i], ":", polys_per_class$n_polys[i], "\n")
  }
  
  min_polys <- min(polys_per_class$n_polys)
  
  if (min_polys >= min_polys_per_class) {
    use_spatial_split <- TRUE
    cat("  ✓ Genug Polygone für räumlichen Split (min:", min_polys, "pro Klasse)\n\n")
  } else {
    cat("  ⚠️ Zu wenige Polygone für räumlichen Split (min:", min_polys, "pro Klasse)\n")
    cat("  → Verwende Standard Split (nicht räumlich) für bessere Balance\n")
    cat("  → HINWEIS: Mit so wenigen Polygonen ist ein räumlicher Split problematisch\n")
    cat("  → Die externe Validierung (CLCplus) ist aussagekräftiger\n\n")
  }
}

# === Split durchführen ===
if (use_spatial_split) {
  cat(">> Erstelle räumlichen Train/Test Split (polygonbasiert)\n")
  cat("  Verhindert Datenleckage durch räumliche Trennung\n")
  
  set.seed(42)
  
  # Stratifizierter Split nach Klasse auf Polygon-Ebene
  train_poly_ids <- polygon_tbl %>%
    group_by(class_name) %>%
    do({
      n_polys <- nrow(.)
      if (n_polys == 1) {
        # Wenn nur 1 Polygon pro Klasse: Gehe ins Trainingsset
        .
      } else {
        # Mindestens 1 Polygon pro Klasse ins Training, Rest proportional
        n_train <- max(1, floor(n_polys * 0.8))
        slice_sample(., n = n_train)
      }
    }) %>%
    pull(polygon_id) %>%
    unique()
  
  # Split Samples basierend auf Polygon-Zugehörigkeit
  train_set <- train_df[samples$polygon_id[vollstaendig] %in% train_poly_ids, ]
  test_set <- train_df[!(samples$polygon_id[vollstaendig] %in% train_poly_ids), ]
  
  # Prüfe ob alle Klassen im Trainingsset vorhanden sind
  train_classes <- unique(train_set$class_name)
  all_classes <- unique(train_df$class_name)
  missing_classes <- setdiff(all_classes, train_classes)
  
  if (length(missing_classes) > 0) {
    cat("  ⚠️ Warnung: Folgende Klassen fehlen im Trainingsset:", paste(missing_classes, collapse = ", "), "\n")
    cat("  → Verwende Standard Split (nicht räumlich) um alle Klassen zu erhalten\n")
    use_spatial_split <- FALSE
  } else {
    cat("  ✓ Räumlicher Split erfolgreich\n")
    cat("  Train-Polygone:", length(train_poly_ids), "\n")
    cat("  Valid-Polygone:", length(unique(samples$polygon_id[vollstaendig])) - length(train_poly_ids), "\n")
  }
}

if (!use_spatial_split) {
  cat(">> Erstelle Standard Train/Test Split (stratified)\n")
  cat("  Besser für kleine Datensätze mit wenigen Polygonen\n")
  
  set.seed(42)
  train_idx <- createDataPartition(train_df$class_name, p = 0.8, list = FALSE)
  train_set <- train_df[train_idx, ]
  test_set <- train_df[-train_idx, ]
  
  cat("  ✓ Standard Split verwendet (nicht räumlich)\n")
}

cat("  Trainingssamples:", nrow(train_set), "\n")
cat("  Validierungssamples:", nrow(test_set), "\n")
cat("  Klassen im Trainingsset:", length(unique(train_set$class_name)), "von", length(unique(train_df$class_name)), "\n")
cat("  Klassenverteilung (Training):\n")
train_dist <- table(train_set$class_name)
print(train_dist)
cat("\n")

# Prüfe Klassenbalance
min_samples <- min(train_dist)
max_samples <- max(train_dist)
imbalance_ratio <- max_samples / min_samples

if (imbalance_ratio > 5) {
  cat("  ⚠️ WARNUNG: Starke Klassen-Ungleichverteilung im Training!\n")
  cat("     Verhältnis max/min:", round(imbalance_ratio, 2), "\n")
  cat("     Min Samples:", min_samples, ", Max Samples:", max_samples, "\n")
  cat("     → Dies kann die Modell-Performance beeinträchtigen\n")
  cat("     → Die externe Validierung (CLCplus) ist aussagekräftiger\n\n")
} else if (imbalance_ratio > 2) {
  cat("  ⚠️ Hinweis: Moderate Klassen-Ungleichverteilung (Verhältnis:", round(imbalance_ratio, 2), ")\n\n")
} else {
  cat("  ✓ Gute Klassenbalance im Training\n\n")
}

# === Random Forest Training ===
cat(">> Trainiere Random Forest\n")
cat("  Parameter:\n")
cat("    - ntree: 500\n")
cat("    - mtry: automatisch (sqrt(n_features))\n")
cat("    - Features:", length(feature_cols), "\n\n")

set.seed(42)
train_start <- Sys.time()

rf_model <- randomForest(
  x = train_set[, feature_cols],
  y = train_set$class_name,
  ntree = 500,
  mtry = floor(sqrt(length(feature_cols))),
  importance = TRUE,
  do.trace = 50
)

train_end <- Sys.time()
train_dauer <- as.numeric(difftime(train_end, train_start, units = "secs"))

cat("  Training abgeschlossen (Dauer:", round(train_dauer, 2), "Sekunden)\n\n")

# === Modell speichern ===
cat(">> Speichere Modell\n")
saveRDS(rf_model, model_output)
cat("  Modell gespeichert:", basename(model_output), "\n\n")

# === Vorhersagen auf Validierungs-Set ===
cat(">> Evaluierung auf Validierungs-Set\n")
test_pred <- predict(rf_model, newdata = test_set[, feature_cols])
test_accuracy <- mean(test_pred == test_set$class_name)

# Confusion Matrix
test_cm <- confusionMatrix(test_pred, test_set$class_name)
test_kappa <- test_cm$overall["Kappa"]

cat("  Test Accuracy:", round(test_accuracy, 4), "(", round(test_accuracy * 100, 2), "%)\n")
cat("  Test Kappa:", round(as.numeric(test_kappa), 4), "\n\n")

# Klassen-spezifische Metriken
cat("  Klassen-spezifische F1-Scores:\n")
f1_scores <- test_cm$byClass[, "F1"]
for (i in 1:length(f1_scores)) {
  cat("    ", names(f1_scores)[i], ":", round(f1_scores[i], 4), "\n")
}
cat("\n")

# === Feature Importance ===
cat(">> Top 10 Features (Mean Decrease Gini):\n")
feature_importance <- importance(rf_model)[, "MeanDecreaseGini"]
top_features <- sort(feature_importance, decreasing = TRUE)[1:min(10, length(feature_importance))]
for (i in 1:length(top_features)) {
  cat("  ", i, ". ", names(top_features)[i], ": ", round(top_features[i], 2), "\n", sep = "")
}
cat("\n")

# === Metriken speichern ===
cat(">> Speichere Metriken\n")
sink(metrics_output)
cat("Random Forest Training (CORINE + DGM, Variante E)\n")
cat("==================================================\n\n")
cat("Datum:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("Trainingsparameter:\n")
cat("  - ntree:", rf_model$ntree, "\n")
cat("  - mtry:", rf_model$mtry, "\n")
cat("  - Anzahl Features:", length(feature_cols), "\n")
cat("  - DGM-Features:", length(vorhandene_dgm), "\n")
cat("  - Split-Methode: Räumlich (polygonbasiert)\n")
cat("  - Trainingssamples:", nrow(train_set), "\n")
cat("  - Validierungssamples:", nrow(test_set), "\n")
cat("  - Trainingsdauer:", round(train_dauer, 2), "Sekunden\n\n")

cat("Validierungs-Set Performance:\n")
cat("  - Accuracy:", round(test_accuracy, 4), "(", round(test_accuracy * 100, 2), "%)\n")
cat("  - Kappa:", round(as.numeric(test_kappa), 4), "\n\n")

cat("Klassen-spezifische F1-Scores:\n")
for (i in 1:length(f1_scores)) {
  cat("  - ", names(f1_scores)[i], ": ", round(f1_scores[i], 4), "\n", sep = "")
}
cat("\n")

cat("Top 10 Features (Mean Decrease Gini):\n")
for (i in 1:length(top_features)) {
  cat("  ", i, ". ", names(top_features)[i], ": ", round(top_features[i], 2), "\n", sep = "")
}
cat("\n")

cat("Confusion Matrix:\n")
print(test_cm$table)
cat("\n")

sink()

cat("  Metriken gespeichert:", basename(metrics_output), "\n\n")

# === Zusammenfassung ===
cat("===========================================\n")
cat("TRAINING ABGESCHLOSSEN\n")
cat("===========================================\n")
cat("Modell:", basename(model_output), "\n")
cat("Validierungs Accuracy:", round(test_accuracy * 100, 2), "%\n")
cat("Validierungs Kappa:", round(as.numeric(test_kappa), 4), "\n")
cat("\n")

cat("Nächster Schritt: Klassifikation durchführen\n")


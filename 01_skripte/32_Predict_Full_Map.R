#!/usr/bin/env Rscript

# ============================================================================
# KLASSIFIKATION AUF GESAMTES RASTER ANWENDEN
# Wendet die trainierten Modelle auf das gesamte Feature-Stack-Raster an
# Erstellt vollständige Klassifikationskarten für alle Modelle
# ============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(randomForest)
  library(xgboost)
  library(dplyr)
})

cat("===========================================\n")
cat("Klassifikation auf gesamtes Raster anwenden\n")
cat("===========================================\n\n")

# === Parameter ===
# Versuche verschiedene mögliche Pfade
possible_work_dirs <- c(
  "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026",
  "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/dona_ess/Workflow_Variante_E_2026"
)

work_dir <- NULL
for (test_dir in possible_work_dirs) {
  if (dir.exists(test_dir)) {
    work_dir <- test_dir
    break
  }
}

if (is.null(work_dir)) {
  # Fallback
  work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"
}

# Root-Verzeichnis für Feature-Stacks
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"

cat("Verwende Workflow-Verzeichnis:", work_dir, "\n")
cat("Root-Verzeichnis:", root_dir, "\n\n")

# Lade Pfade
paths_file <- file.path(work_dir, "02_daten", "input", "workflow_paths.rds")
if (file.exists(paths_file)) {
  paths <- readRDS(paths_file)
  cat("Pfade geladen aus:", basename(paths_file), "\n")
} else {
  # Fallback: Definiere Pfade direkt
  cat("  ⚠ Pfade-Datei nicht gefunden, verwende Standard-Pfade\n")
  paths <- list(
    feature_stack_2018 = file.path(root_dir, "data", "processed", "Sentinel2", "2018", "FeatureStack_2018.tif"),
    feature_stack_2018_dgm = file.path(root_dir, "data", "processed", "Sentinel2", "2018", "FeatureStack_2018_with_dgm.tif")
  )
}

# Prüfe ob Feature-Stacks existieren, falls nicht, suche im Archiv
if (!file.exists(paths$feature_stack_2018)) {
  archiv_path <- file.path(root_dir, "Archiv", "dona_ess", "Archiv", "data", "processed", "Sentinel2", "2018", "FeatureStack_2018.tif")
  if (file.exists(archiv_path)) {
    cat("  → Feature-Stack im Archiv gefunden, verwende:", basename(archiv_path), "\n")
    paths$feature_stack_2018 <- archiv_path
  }
}

if (!file.exists(paths$feature_stack_2018_dgm)) {
  archiv_path_dgm <- file.path(root_dir, "Archiv", "dona_ess", "Archiv", "data", "processed", "Sentinel2", "2018", "FeatureStack_2018_with_dgm.tif")
  if (file.exists(archiv_path_dgm)) {
    cat("  → Feature-Stack mit DGM im Archiv gefunden, verwende:", basename(archiv_path_dgm), "\n")
    paths$feature_stack_2018_dgm <- archiv_path_dgm
  }
}

# Ausgabeverzeichnis
output_dir <- file.path(work_dir, "03_ergebnisse", "vorhersagen")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

modelle_dir <- file.path(work_dir, "02_daten", "modelle")

# Klassen-Definition
# WICHTIG: Reihenfolge muss mit der alphabetischen Sortierung übereinstimmen
# wie sie in den Modellen verwendet wird (factor levels sind alphabetisch sortiert)
klassen <- c(
  "Binnenmarschen",      # ID 1
  "Feuchtgebiet_salz",    # ID 2
  "Gehölzvegetation",    # ID 3
  "Offener_Boden",       # ID 4
  "Wasser"               # ID 5
)

# === Modelle und Feature-Stacks ===
model_configs <- list(
  list(
    name = "RF CORINE-only",
    model_file = file.path(modelle_dir, "rf_model_corine_only_variante_e.rds"),
    feature_file = file.path(modelle_dir, "rf_corine_only_variante_e_features.rds"),
    stack_file = paths$feature_stack_2018,
    output_file = file.path(output_dir, "rf_corine_only_prediction.tif")
  ),
  list(
    name = "XGBoost CORINE-only",
    model_file = file.path(modelle_dir, "xgb_model_corine_only_variante_e.rds"),
    feature_file = file.path(modelle_dir, "xgb_corine_only_variante_e_features.rds"),
    stack_file = paths$feature_stack_2018,
    output_file = file.path(output_dir, "xgb_corine_only_prediction.tif")
  ),
  list(
    name = "RF CORINE+DGM",
    model_file = file.path(modelle_dir, "rf_model_corine_dgm_variante_e.rds"),
    feature_file = file.path(modelle_dir, "rf_corine_dgm_variante_e_features.rds"),
    stack_file = paths$feature_stack_2018_dgm,
    output_file = file.path(output_dir, "rf_corine_dgm_prediction.tif")
  ),
  list(
    name = "XGBoost CORINE+DGM",
    model_file = file.path(modelle_dir, "xgb_model_corine_dgm_variante_e.rds"),
    feature_file = file.path(modelle_dir, "xgb_corine_dgm_variante_e_features.rds"),
    stack_file = paths$feature_stack_2018_dgm,
    output_file = file.path(output_dir, "xgb_corine_dgm_prediction.tif")
  )
)

# === Funktion: Klassifikation auf Raster anwenden ===
apply_classification <- function(config) {
  cat("\n===========================================\n")
  cat("Verarbeite:", config$name, "\n")
  cat("===========================================\n\n")
  
  # Prüfe ob alle Dateien existieren
  if (!file.exists(config$model_file)) {
    cat("  ⚠ Modell nicht gefunden:", basename(config$model_file), "\n")
    return(FALSE)
  }
  
  if (!file.exists(config$feature_file)) {
    cat("  ⚠ Feature-Liste nicht gefunden:", basename(config$feature_file), "\n")
    return(FALSE)
  }
  
  if (!file.exists(config$stack_file)) {
    cat("  ⚠ Feature-Stack nicht gefunden:", basename(config$stack_file), "\n")
    return(FALSE)
  }
  
  # Lade Modell und Features
  cat(">> Lade Modell und Features\n")
  model <- readRDS(config$model_file)
  feature_cols <- readRDS(config$feature_file)
  cat("  Modell geladen:", class(model)[1], "\n")
  cat("  Features:", length(feature_cols), "\n")
  cat("  Feature-Namen:", paste(feature_cols, collapse = ", "), "\n\n")
  
  # Lade Feature-Stack
  cat(">> Lade Feature-Stack\n")
  feature_stack <- rast(config$stack_file)
  cat("  Dimensionen:", nrow(feature_stack), "x", ncol(feature_stack), "\n")
  cat("  Anzahl Bänder:", nlyr(feature_stack), "\n")
  cat("  Bänder:", paste(names(feature_stack), collapse = ", "), "\n\n")
  
  # Prüfe ob alle Features im Stack vorhanden sind
  fehlende_features <- setdiff(feature_cols, names(feature_stack))
  if (length(fehlende_features) > 0) {
    cat("  ⚠ Fehlende Features im Stack:", paste(fehlende_features, collapse = ", "), "\n")
    return(FALSE)
  }
  
  # Extrahiere nur benötigte Features
  feature_stack_subset <- feature_stack[[feature_cols]]
  cat("  Feature-Stack reduziert auf", nlyr(feature_stack_subset), "Bänder\n\n")
  
  # === Klassifikation mit terra::predict ===
  cat(">> Wende Klassifikation an\n")
  cat("  Dies kann einige Minuten dauern...\n\n")
  
  start_time <- Sys.time()
  
  # terra::predict für Random Forest
  if (inherits(model, "randomForest")) {
    cat("  Verwende terra::predict für Random Forest\n")
    
    # Extrahiere Daten und mache Vorhersage manuell (terra::predict hat Probleme mit RF)
    cat("  Extrahiere Raster-Daten...\n")
    raster_data <- as.data.frame(feature_stack_subset, xy = FALSE, na.rm = FALSE)
    
    # Entferne NA-Zeilen
    complete_idx <- complete.cases(raster_data)
    raster_data_complete <- raster_data[complete_idx, , drop = FALSE]
    
    cat("  Vollständige Pixel:", nrow(raster_data_complete), "von", nrow(raster_data), "\n")
    
    if (nrow(raster_data_complete) == 0) {
      cat("  ⚠ Keine vollständigen Pixel gefunden\n")
      return(FALSE)
    }
    
    # Konvertiere zu Matrix
    pred_matrix <- as.matrix(raster_data_complete)
    
    # Vorhersagen
    cat("  Erstelle Vorhersagen...\n")
    pred_class <- predict(model, newdata = pred_matrix, type = "response")
    
    # Konvertiere Klassen-Namen zu numerischen IDs (1-5)
    class_levels <- levels(model$y)
    if (is.null(class_levels)) {
      class_levels <- model$classes
    }
    
    # WICHTIG: Erstelle Mapping basierend auf der tatsächlichen Reihenfolge der Levels
    # Die Levels sind alphabetisch sortiert: Binnenmarschen, Feuchtgebiet_salz, Gehölzvegetation, Offener_Boden, Wasser
    # Das entspricht den IDs 1-5
    class_levels_ordered <- sort(class_levels)
    pred_ids <- match(pred_class, class_levels_ordered)
    
    # Prüfe ob alle IDs gültig sind
    if (any(is.na(pred_ids))) {
      cat("  ⚠️ Warnung: Einige Vorhersagen konnten nicht gemappt werden\n")
      pred_ids[is.na(pred_ids)] <- 0  # Markiere als ungültig
    }
    
    # Erstelle Vorhersage-Raster
    pred_template <- feature_stack_subset[[1]]
    values(pred_template) <- NA
    
    # Setze Vorhersagen
    pred_values <- rep(NA, ncell(pred_template))
    pred_values[complete_idx] <- pred_ids
    values(pred_template) <- pred_values
    
    prediction_raster <- pred_template
    
  } else if (inherits(model, "xgb.Booster")) {
    cat("  Verwende terra::predict für XGBoost\n")
    
    # XGBoost benötigt spezielle Behandlung
    # Extrahiere Daten als Matrix
    cat("  Extrahiere Raster-Daten...\n")
    raster_data <- as.data.frame(feature_stack_subset, xy = FALSE, na.rm = FALSE)
    
    # Entferne NA-Zeilen
    complete_idx <- complete.cases(raster_data)
    raster_data_complete <- raster_data[complete_idx, , drop = FALSE]
    
    cat("  Vollständige Pixel:", nrow(raster_data_complete), "von", nrow(raster_data), "\n")
    
    if (nrow(raster_data_complete) == 0) {
      cat("  ⚠ Keine vollständigen Pixel gefunden\n")
      return(FALSE)
    }
    
    # Konvertiere zu Matrix
    pred_matrix <- as.matrix(raster_data_complete)
    
    # Vorhersagen
    cat("  Erstelle Vorhersagen...\n")
    pred_probs <- predict(model, newdata = pred_matrix, reshape = TRUE)
    
    # Bestimme Klasse (höchste Wahrscheinlichkeit)
    # which.max gibt 1-basierte Indizes zurück (1-5)
    pred_class_idx <- apply(pred_probs, 1, which.max)
    
    # Klassen-Levels aus Modell verwenden (falls gespeichert)
    if (!is.null(model$class_levels)) {
      class_levels_xgb <- model$class_levels
    } else {
      # Fallback: Alphabetische Reihenfolge (wie im Training)
      class_levels_xgb <- sort(klassen)
    }
    
    # WICHTIG: Die Spalten in pred_probs entsprechen der Reihenfolge der class_levels
    # which.max gibt 1-5 zurück, was den Indizes in class_levels_xgb entspricht
    # Diese müssen dann zu den IDs 1-5 gemappt werden basierend auf der alphabetischen Reihenfolge
    
    # Erstelle Mapping: XGBoost-Index (1-5) → Alphabetische ID (1-5)
    # class_levels_xgb ist bereits alphabetisch sortiert (aus dem Training)
    # Also: pred_class_idx ist bereits korrekt (1-5 für alphabetische Reihenfolge)
    
    # Erstelle Vorhersage-Raster
    pred_template <- feature_stack_subset[[1]]
    values(pred_template) <- NA
    
    # Setze Vorhersagen (bereits 1-5, entspricht alphabetischer Reihenfolge)
    pred_values <- rep(NA, ncell(pred_template))
    pred_values[complete_idx] <- pred_class_idx
    values(pred_template) <- pred_values
    
    prediction_raster <- pred_template
    
  } else {
    cat("  ⚠ Unbekannter Modelltyp:", class(model)[1], "\n")
    return(FALSE)
  }
  
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  cat("  Klassifikation abgeschlossen (Dauer:", round(duration, 2), "Sekunden)\n\n")
  
  # === Speichere Raster ===
  cat(">> Speichere Klassifikations-Raster\n")
  writeRaster(
    prediction_raster,
    filename = config$output_file,
    overwrite = TRUE,
    datatype = "INT1U",
    NAflag = 0
  )
  
  cat("  Gespeichert:", basename(config$output_file), "\n")
  cat("  Dimensionen:", nrow(prediction_raster), "x", ncol(prediction_raster), "\n")
  cat("  Wertebereich:", min(values(prediction_raster), na.rm = TRUE), "-", 
      max(values(prediction_raster), na.rm = TRUE), "\n\n")
  
  return(TRUE)
}

# === Wende Klassifikation auf alle Modelle an ===
cat("===========================================\n")
cat("Starte Klassifikation für alle Modelle\n")
cat("===========================================\n\n")

erfolgreich <- 0
for (config in model_configs) {
  if (apply_classification(config)) {
    erfolgreich <- erfolgreich + 1
  }
}

# === Zusammenfassung ===
cat("\n===========================================\n")
cat("ZUSAMMENFASSUNG\n")
cat("===========================================\n\n")
cat("Erfolgreich erstellt:", erfolgreich, "von", length(model_configs), "Klassifikationen\n\n")

if (erfolgreich > 0) {
  cat("Klassifikations-Raster gespeichert in:\n")
  cat("  ", output_dir, "\n\n")
  cat("Nächster Schritt: Visualisierung mit '03_vergleichskarten_klassifikationen.R'\n")
} else {
  cat("⚠ Keine Klassifikationen erstellt!\n")
  cat("Bitte prüfe:\n")
  cat("  - Sind alle Modelle trainiert?\n")
  cat("  - Existieren die Feature-Stacks?\n")
  cat("  - Sind die Pfade korrekt?\n")
}

cat("\n✓ Abgeschlossen!\n")


#!/usr/bin/env Rscript

# ============================================================================
# EXPORT KLASSIFIKATIONEN ALS GPKG
# Konvertiert Klassifikations-Raster zu GeoPackage (Polygone)
# ============================================================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
})

cat("===========================================\n")
cat("Export Klassifikationen als GPKG\n")
cat("===========================================\n\n")

# === Parameter ===
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"
work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"

# Eingabe: Klassifikations-Raster
predictions_dir <- file.path(work_dir, "03_ergebnisse", "vorhersagen")

# Ausgabe: GPKG-Dateien
gpkg_dir <- file.path(work_dir, "03_ergebnisse", "klassifikationen_gpkg")
dir.create(gpkg_dir, recursive = TRUE, showWarnings = FALSE)

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

cat("Eingabe-Verzeichnis:", predictions_dir, "\n")
cat("Ausgabe-Verzeichnis:", gpkg_dir, "\n\n")

# === Suche nach Klassifikations-Rastern ===
cat(">> Suche nach Klassifikations-Rastern\n")

prediction_files <- c(
  "RF CORINE-only" = file.path(predictions_dir, "rf_corine_only_prediction.tif"),
  "XGBoost CORINE-only" = file.path(predictions_dir, "xgb_corine_only_prediction.tif"),
  "RF CORINE+DGM" = file.path(predictions_dir, "rf_corine_dgm_prediction.tif"),
  "XGBoost CORINE+DGM" = file.path(predictions_dir, "xgb_corine_dgm_prediction.tif")
)

# Prüfe welche existieren
vorhandene_predictions <- c()
for (name in names(prediction_files)) {
  file <- prediction_files[[name]]
  if (file.exists(file)) {
    vorhandene_predictions <- c(vorhandene_predictions, name)
    cat("  ✓ Gefunden:", name, "\n")
  } else {
    cat("  ✗ Fehlt:", name, "\n")
  }
}

if (length(vorhandene_predictions) == 0) {
  cat("\n⚠ KEINE KLASSIFIKATIONS-RASTER GEFUNDEN!\n")
  cat("Bitte zuerst '12_klassifikation_auf_raster.R' ausführen.\n\n")
  stop("Keine Raster gefunden")
}

cat("\n")

# === Funktion: Raster zu GPKG konvertieren ===
convert_to_gpkg <- function(raster_file, model_name, output_dir) {
  cat(">> Konvertiere:", model_name, "\n")
  
  tryCatch({
    # Lade Raster
    r <- rast(raster_file)
    cat("  Raster geladen:", nrow(r), "x", ncol(r), "\n")
    
    # Prüfe ob Raster gültige Werte hat
    r_values <- values(r)
    if (all(is.na(r_values))) {
      cat("  ⚠️ Raster enthält nur NA-Werte, überspringe\n")
      return(FALSE)
    }
    
    # Konvertiere Raster zu Polygonen (dissolve = TRUE für zusammenhängende Flächen)
    cat("  Konvertiere zu Polygonen...\n")
    
    # Entferne NA-Werte vor Polygon-Konvertierung (setze temporär auf 0)
    r_clean <- r
    r_clean[is.na(r_clean)] <- 0
    
    r_poly <- as.polygons(r_clean, dissolve = TRUE, values = TRUE)
    
    # Setze CRS falls nicht vorhanden
    if (is.na(crs(r_poly)) || crs(r_poly) == "") {
      crs(r_poly) <- "EPSG:25829"  # ETRS89 / UTM zone 29N
      cat("  CRS gesetzt: EPSG:25829\n")
    }
    
    # Konvertiere zu sf
    r_sf <- st_as_sf(r_poly)
    cat("  Polygon-Features:", nrow(r_sf), "\n")
    
    if (nrow(r_sf) == 0) {
      cat("  ⚠️ Keine Polygon-Features erstellt, überspringe\n")
      return(FALSE)
    }
    
    # Finde die Klassifikations-Spalte
    data_cols <- names(r_sf)[names(r_sf) != attr(r_sf, "sf_column")]
    
    if (length(data_cols) == 0) {
      cat("  ⚠️ Keine Daten-Spalten gefunden, überspringe\n")
      return(FALSE)
    }
    
    # Suche nach numerischer Spalte (erste nicht-Geometrie Spalte)
    klasse_col <- NULL
    for (col in data_cols) {
      if (is.numeric(r_sf[[col]])) {
        klasse_col <- col
        break
      }
    }
    
    if (is.null(klasse_col)) {
      # Fallback: Erste Spalte
      klasse_col <- data_cols[1]
    }
    
    # Benenne Spalte um und füge Klassen-Info hinzu
    # FIX: Verwende direkte Spaltenzuweisung statt !!sym() für bessere Kompatibilität
    r_sf$klasse_id <- as.integer(r_sf[[klasse_col]])
    
    # Entferne temporäre 0-Werte (waren NA)
    r_sf <- r_sf[r_sf$klasse_id > 0, ]
    
    if (nrow(r_sf) == 0) {
      cat("  ⚠️ Keine gültigen Klassen-Werte gefunden, überspringe\n")
      return(FALSE)
    }
    
    # WICHTIG: Klassen-Mapping korrekt zuordnen
    # Die IDs 1-5 entsprechen der alphabetischen Reihenfolge der Klassen:
    # 1 = Binnenmarschen, 2 = Feuchtgebiet_salz, 3 = Gehölzvegetation, 4 = Offener_Boden, 5 = Wasser
    # Prüfe gültige IDs
    valid_ids <- r_sf$klasse_id >= 1 & r_sf$klasse_id <= length(klassen)
    if (any(!valid_ids)) {
      cat("  ⚠️ Warnung: Ungültige Klassen-IDs gefunden:", unique(r_sf$klasse_id[!valid_ids]), "\n")
      cat("    Erwartete IDs: 1-", length(klassen), "\n", sep = "")
      r_sf <- r_sf[valid_ids, ]
    }
    
    r_sf$klasse_name <- factor(
      r_sf$klasse_id,
      levels = 1:length(klassen),
      labels = klassen
    )
    
    # Debug: Zeige Klassenverteilung
    cat("  Klassenverteilung in GPKG:\n")
    print(table(r_sf$klasse_name, useNA = "ifany"))
    r_sf$modell <- model_name
    
    # Entferne temporäre Spalten und behalte nur benötigte
    keep_cols <- c("klasse_id", "klasse_name", "modell", attr(r_sf, "sf_column"))
    r_sf <- r_sf[, keep_cols]
    
    # Berechne Flächen
    r_sf$flaeche_ha <- as.numeric(st_area(r_sf)) / 10000  # m² zu ha
    cat("  Flächen berechnet\n")
    
    # Speichere als GPKG
    filename <- gsub(" ", "_", tolower(model_name))
    filename <- gsub("[+]", "plus", filename)
    filename <- paste0("klassifikation_", filename, "_variante_e.gpkg")
    output_file <- file.path(output_dir, filename)
    
    st_write(
      r_sf,
      output_file,
      layer = "klassifikation",
      delete_layer = TRUE,
      quiet = TRUE
    )
    
    cat("  ✓ Gespeichert:", filename, "\n")
    cat("    Features:", nrow(r_sf), "\n")
    cat("    Gesamtfläche:", round(sum(r_sf$flaeche_ha, na.rm = TRUE), 2), "ha\n\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("  ✗ Fehler:", e$message, "\n\n")
    return(FALSE)
  })
}

# === Konvertiere alle Raster ===
cat("===========================================\n")
cat("Konvertiere Raster zu GPKG\n")
cat("===========================================\n\n")

erfolgreich <- 0
for (name in vorhandene_predictions) {
  raster_file <- prediction_files[[name]]
  if (convert_to_gpkg(raster_file, name, gpkg_dir)) {
    erfolgreich <- erfolgreich + 1
  }
}

# === Zusammenfassung ===
cat("===========================================\n")
cat("EXPORT ABGESCHLOSSEN\n")
cat("===========================================\n\n")

cat("Erfolgreich konvertiert:", erfolgreich, "von", length(vorhandene_predictions), "\n\n")

if (erfolgreich > 0) {
  cat("GPKG-Dateien gespeichert in:\n")
  cat("  ", gpkg_dir, "\n\n")
  
  cat("Dateien:\n")
  gpkg_files <- list.files(gpkg_dir, pattern = "\\.gpkg$", full.names = FALSE)
  for (file in gpkg_files) {
    file_path <- file.path(gpkg_dir, file)
    file_size <- file.info(file_path)$size / (1024 * 1024)  # MB
    cat("  - ", file, " (", round(file_size, 2), " MB)\n", sep = "")
  }
  cat("\n")
  
cat("Die GPKG-Dateien enthalten:\n")
cat("  - klasse_id: Numerische Klassen-ID (1-5)\n")
cat("  - klasse_name: Klassen-Name\n")
cat("  - modell: Modell-Name\n")
cat("  - flaeche_ha: Fläche in Hektar\n")
cat("  - geometry: Polygon-Geometrie\n\n")
cat("Klassen-Mapping (ID → Name):\n")
for (i in 1:length(klassen)) {
  cat(sprintf("  %d = %s\n", i, klassen[i]))
}
cat("\n")
} else {
  cat("⚠ Keine GPKG-Dateien erstellt!\n\n")
}

cat("✓ Abgeschlossen!\n\n")


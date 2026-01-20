#!/usr/bin/env Rscript

# ===========================================
# Setup: Workflow Variante E
# Ordnerstruktur erstellen, Pfade definieren
# ===========================================

cat("===========================================\n")
cat("Setup: Workflow Variante E\n")
cat("===========================================\n\n")

# === Parameter ===
# Root-Verzeichnis: Eine Ebene über dem Workflow
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"
work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"
archiv_dir <- file.path(root_dir, "Archiv", "dona_ess", "Archiv")

# === Ordnerstruktur erstellen ===
cat(">> Erstelle Ordnerstruktur\n")

dirs <- list(
  skripte = file.path(work_dir, "01_skripte"),
  daten_input = file.path(work_dir, "02_daten", "input"),
  daten_training = file.path(work_dir, "02_daten", "training"),
  daten_modelle = file.path(work_dir, "02_daten", "modelle"),
  daten_klassifikationen = file.path(work_dir, "02_daten", "klassifikationen"),
  ergebnisse_interne = file.path(work_dir, "03_ergebnisse", "interne_validierung"),
  ergebnisse_externe = file.path(work_dir, "03_ergebnisse", "externe_validierung"),
  ergebnisse_vergleiche = file.path(work_dir, "03_ergebnisse", "vergleiche"),
  ergebnisse_visualisierungen = file.path(work_dir, "03_ergebnisse", "visualisierungen"),
  dokumentation = file.path(work_dir, "04_dokumentation")
)

for (dir_name in dirs) {
  dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  cat("  ✓", basename(dir_name), "\n")
}

cat("\n")

# === Pfade definieren ===
cat(">> Definiere Pfade\n\n")

# Eingabedaten - Zuerst im lokalen Input-Verzeichnis, dann im Archiv
input_dir <- file.path(work_dir, "02_daten", "input")

paths <- list(
  # Sentinel-2 Feature Stacks (zuerst lokal, dann Archiv)
  feature_stack_2018 = file.path(input_dir, "FeatureStack_2018.tif"),
  feature_stack_2018_dgm = file.path(input_dir, "FeatureStack_2018_with_dgm.tif"),
  
  # CORINE Polygone (zuerst lokal, dann Archiv)
  corine_polygone = file.path(input_dir, "corine_clipped_buffer.gpkg"),
  
  # CLCplus (externe Validierung) - zuerst lokal, dann Archiv
  clcplus_raster = file.path(input_dir, "CLCPlus_2018.tif"),
  clcplus_raster_alt = file.path(root_dir, "CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1", "Data", "CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1.tif"),
  
  # AOI (zuerst lokal, dann Archiv)
  aoi = file.path(input_dir, "aoi_donana_neu_noDEMhole.gpkg")
)

# Prüfe und korrigiere Pfade falls Dateien nicht existieren
# Feature Stack 2018
if (!file.exists(paths$feature_stack_2018)) {
  # Suche alternative Pfade (Archiv)
  alt_paths <- c(
    file.path(archiv_dir, "data", "processed", "Sentinel2", "2018", "FeatureStack_2018.tif"),
    file.path(root_dir, "data", "processed", "Sentinel2", "2018", "FeatureStack_2018.tif")
  )
  for (alt in alt_paths) {
    if (file.exists(alt)) {
      paths$feature_stack_2018 <- alt
      break
    }
  }
}

# Feature Stack 2018 mit DGM
if (!file.exists(paths$feature_stack_2018_dgm)) {
  alt_paths <- c(
    file.path(archiv_dir, "data", "processed", "Sentinel2", "2018", "FeatureStack_2018_with_dgm.tif"),
    file.path(root_dir, "data", "processed", "Sentinel2", "2018", "FeatureStack_2018_with_dgm.tif")
  )
  for (alt in alt_paths) {
    if (file.exists(alt)) {
      paths$feature_stack_2018_dgm <- alt
      break
    }
  }
}

# CORINE Polygone
if (!file.exists(paths$corine_polygone)) {
  alt_paths <- c(
    file.path(archiv_dir, "vergleiche", "Vergleich_CORINE_20260117", "02_daten", "input", "corine_clipped_buffer.gpkg"),
    file.path(root_dir, "Archiv", "corine_new_class", "Modell_Corine_only", "02_daten", "input", "corine_clipped_buffer.gpkg")
  )
  for (alt in alt_paths) {
    if (file.exists(alt)) {
      paths$corine_polygone <- alt
      break
    }
  }
}

# CLCplus
if (!file.exists(paths$clcplus_raster)) {
  alt_paths <- c(
    file.path(archiv_dir, "data", "processed", "Corine", "CLCPlus_2018.tif"),
    paths$clcplus_raster_alt
  )
  for (alt in alt_paths) {
    if (file.exists(alt)) {
      paths$clcplus_raster <- alt
      break
    }
  }
}

# AOI
if (!file.exists(paths$aoi)) {
  alt_paths <- c(
    file.path(archiv_dir, "data", "raw", "aoi", "aoi_donana_neu_noDEMhole.gpkg"),
    file.path(root_dir, "Archiv", "corine_new_class", "Modell_Baseline", "02_daten", "input", "aoi_donana_neu_noDEMhole.gpkg")
  )
  for (alt in alt_paths) {
    if (file.exists(alt)) {
      paths$aoi <- alt
      break
    }
  }
}

# Prüfe welche Dateien existieren
cat("Eingabedaten:\n")
for (name in names(paths)) {
  path <- paths[[name]]
  exists <- file.exists(path)
  status <- if (exists) "✓" else "✗"
  cat(sprintf("  %s %s: %s\n", status, name, basename(path)))
  if (!exists && name == "clcplus_raster") {
    # Prüfe alternative Pfade
    if (file.exists(paths$clcplus_raster_alt)) {
      cat(sprintf("    → Alternative gefunden: %s\n", basename(paths$clcplus_raster_alt)))
    }
  }
}

cat("\n")

# === Speichere Pfade als RDS ===
paths_file <- file.path(work_dir, "02_daten", "input", "workflow_paths.rds")
saveRDS(paths, paths_file)
cat(">> Pfade gespeichert:", basename(paths_file), "\n\n")

# === Variante E Klassen-Definition ===
cat(">> Variante E Klassen-Definition\n\n")

variante_e_classes <- list(
  Gehölzvegetation = c("311", "312", "323", "324", "231"),
  Offener_Boden = c("331", "333"),
  Binnenmarschen = c("411"),
  Feuchtgebiet_salz = c("421", "422"),
  Wasser = c("511", "512")
)

cat("5 Klassen (Variante E):\n")
for (class_name in names(variante_e_classes)) {
  codes <- variante_e_classes[[class_name]]
  cat(sprintf("  - %s: %s\n", class_name, paste(codes, collapse = ", ")))
}

# Speichere Klassen-Definition
classes_file <- file.path(work_dir, "02_daten", "input", "variante_e_classes.rds")
saveRDS(variante_e_classes, classes_file)
cat("\n>> Klassen-Definition gespeichert:", basename(classes_file), "\n\n")

# === Zusammenfassung ===
cat("===========================================\n")
cat("SETUP ABGESCHLOSSEN\n")
cat("===========================================\n")
cat("Workflow-Verzeichnis:", work_dir, "\n")
cat("Ordnerstruktur erstellt\n")
cat("Pfade definiert und gespeichert\n")
cat("Klassen-Definition gespeichert\n\n")
cat("Nächster Schritt: Phase 1 - Datenaufbereitung\n")


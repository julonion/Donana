#!/usr/bin/env Rscript

# ===========================================
# Phase 1: Datenprüfung
# Importieren & Prüfen aller benötigten Daten
# ===========================================

suppressPackageStartupMessages({
  library(terra)
  library(sf)
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

cat("===========================================\n")
cat("Phase 1: Datenprüfung\n")
cat("===========================================\n\n")

# === 1. Sentinel-2 Feature Stacks ===
cat(">> Prüfe Sentinel-2 Feature Stacks\n")

# Feature Stack ohne DGM
if (file.exists(paths$feature_stack_2018)) {
  feat_stack <- rast(paths$feature_stack_2018)
  cat("  ✓ FeatureStack_2018.tif\n")
  cat("    - Features:", nlyr(feat_stack), "\n")
  cat("    - Dimension:", nrow(feat_stack), "x", ncol(feat_stack), "\n")
  cat("    - Auflösung:", res(feat_stack)[1], "m\n")
  cat("    - CRS:", crs(feat_stack), "\n")
  cat("    - Feature-Namen:", paste(names(feat_stack)[1:min(5, nlyr(feat_stack))], collapse = ", "), "...\n")
} else {
  cat("  ✗ FeatureStack_2018.tif nicht gefunden!\n")
}

cat("\n")

# Feature Stack mit DGM
if (file.exists(paths$feature_stack_2018_dgm)) {
  feat_stack_dgm <- rast(paths$feature_stack_2018_dgm)
  cat("  ✓ FeatureStack_2018_with_dgm.tif\n")
  cat("    - Features:", nlyr(feat_stack_dgm), "\n")
  cat("    - Dimension:", nrow(feat_stack_dgm), "x", ncol(feat_stack_dgm), "\n")
  cat("    - Auflösung:", res(feat_stack_dgm)[1], "m\n")
  cat("    - CRS:", crs(feat_stack_dgm), "\n")
  cat("    - Feature-Namen:", paste(names(feat_stack_dgm)[1:min(5, nlyr(feat_stack_dgm))], collapse = ", "), "...\n")
  
  # Prüfe DGM-Features
  dgm_features <- c("elevation", "slope", "aspect", "TRI", "TWI", "tri", "twi")
  vorhandene_dgm <- intersect(dgm_features, names(feat_stack_dgm))
  cat("    - DGM-Features:", length(vorhandene_dgm), "von 5 erwartet\n")
  if (length(vorhandene_dgm) > 0) {
    cat("      ", paste(vorhandene_dgm, collapse = ", "), "\n")
  }
} else {
  cat("  ✗ FeatureStack_2018_with_dgm.tif nicht gefunden!\n")
}

cat("\n")

# === 2. CORINE Polygone ===
cat(">> Prüfe CORINE Polygone\n")

if (file.exists(paths$corine_polygone)) {
  corine <- st_read(paths$corine_polygone, quiet = TRUE)
  cat("  ✓ corine_clipped_buffer.gpkg\n")
  cat("    - Anzahl Polygone:", nrow(corine), "\n")
  cat("    - Spalten:", paste(names(corine)[1:min(5, ncol(corine))], collapse = ", "), "...\n")
  
  # Prüfe Code_18 Spalte
  if ("Code_18" %in% names(corine)) {
    codes <- unique(corine$Code_18)
    cat("    - Eindeutige CORINE Codes:", length(codes), "\n")
    cat("    - Codes:", paste(sort(codes)[1:min(10, length(codes))], collapse = ", "), "...\n")
  }
} else {
  cat("  ✗ corine_clipped_buffer.gpkg nicht gefunden!\n")
}

cat("\n")

# === 3. CLCplus (externe Validierung) ===
cat(">> Prüfe CLCplus Raster\n")

clcplus_path <- paths$clcplus_raster
if (!file.exists(clcplus_path)) {
  clcplus_path <- paths$clcplus_raster_alt
}

if (file.exists(clcplus_path)) {
  clcplus <- rast(clcplus_path)
  cat("  ✓ CLCplus Raster gefunden\n")
  cat("    - Dimension:", nrow(clcplus), "x", ncol(clcplus), "\n")
  cat("    - Auflösung:", res(clcplus)[1], "m\n")
  cat("    - CRS:", crs(clcplus), "\n")
  
  # Prüfe Werte
  clc_vals <- unique(values(clcplus, mat = FALSE))
  clc_vals <- clc_vals[!is.na(clc_vals)]
  cat("    - Eindeutige Werte:", length(clc_vals), "\n")
  cat("    - Werte:", paste(sort(clc_vals)[1:min(10, length(clc_vals))], collapse = ", "), "...\n")
} else {
  cat("  ✗ CLCplus Raster nicht gefunden!\n")
  cat("    Prüfe:", paths$clcplus_raster, "\n")
  cat("    Prüfe:", paths$clcplus_raster_alt, "\n")
}

cat("\n")

# === 4. AOI ===
cat(">> Prüfe AOI\n")

if (file.exists(paths$aoi)) {
  aoi <- st_read(paths$aoi, quiet = TRUE)
  cat("  ✓ aoi_donana_neu_noDEMhole.gpkg\n")
  cat("    - Anzahl Features:", nrow(aoi), "\n")
  cat("    - CRS:", st_crs(aoi)$input, "\n")
  
  # Berechne Fläche
  aoi_area <- st_area(aoi)
  aoi_area_km2 <- as.numeric(sum(aoi_area)) / 1e6
  cat("    - Fläche:", round(aoi_area_km2, 2), "km²\n")
} else {
  cat("  ✗ aoi_donana_neu_noDEMhole.gpkg nicht gefunden!\n")
}

cat("\n")

# === Zusammenfassung ===
cat("===========================================\n")
cat("DATENPRÜFUNG ABGESCHLOSSEN\n")
cat("===========================================\n")
cat("\nNächster Schritt: Trainingsdaten extrahieren\n")
cat("  - 03_extract_training_corine_only.R (ohne DGM)\n")
cat("  - 04_extract_training_corine_dgm.R (mit DGM)\n")


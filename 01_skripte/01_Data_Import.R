#!/usr/bin/env Rscript

# ===========================================
# Organisiere Daten: Kopiere/Verlinke benötigte Dateien
# ins Workflow-Verzeichnis
# ===========================================

cat("===========================================\n")
cat("Organisiere Daten für Workflow\n")
cat("===========================================\n\n")

# === Parameter ===
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"
work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"
archiv_dir <- file.path(root_dir, "Archiv", "dona_ess", "Archiv")

input_dir <- file.path(work_dir, "02_daten", "input")
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)

cat("Workflow-Verzeichnis:", work_dir, "\n")
cat("Input-Verzeichnis:", input_dir, "\n\n")

# === Funktion zum Kopieren/Verlinken ===
copy_or_link <- function(source, target, use_symlink = TRUE) {
  if (!file.exists(source)) {
    cat("  ✗ Nicht gefunden:", basename(source), "\n")
    return(FALSE)
  }
  
  # Erstelle Ziel-Verzeichnis falls nötig
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  
  # Entferne Ziel falls vorhanden
  if (file.exists(target)) {
    file.remove(target)
  }
  
  if (use_symlink) {
    # Erstelle Symlink (spart Platz)
    tryCatch({
      file.symlink(source, target)
      cat("  ✓ Symlink erstellt:", basename(target), "\n")
      return(TRUE)
    }, error = function(e) {
      # Fallback: Kopiere Datei
      cat("  ⚠ Symlink fehlgeschlagen, kopiere Datei:", basename(target), "\n")
      file.copy(source, target, overwrite = TRUE)
      return(TRUE)
    })
  } else {
    # Kopiere Datei
    file.copy(source, target, overwrite = TRUE)
    cat("  ✓ Kopiert:", basename(target), "\n")
    return(TRUE)
  }
}

# === 1. Feature Stacks ===
cat(">> Feature Stacks (Sentinel-2 2018)\n")

# Feature Stack ohne DGM
source_fs <- file.path(archiv_dir, "data", "processed", "Sentinel2", "2018", "FeatureStack_2018.tif")
target_fs <- file.path(input_dir, "FeatureStack_2018.tif")
copy_or_link(source_fs, target_fs, use_symlink = TRUE)

# Feature Stack mit DGM
source_fs_dgm <- file.path(archiv_dir, "data", "processed", "Sentinel2", "2018", "FeatureStack_2018_with_dgm.tif")
target_fs_dgm <- file.path(input_dir, "FeatureStack_2018_with_dgm.tif")
copy_or_link(source_fs_dgm, target_fs_dgm, use_symlink = TRUE)

cat("\n")

# === 2. CORINE Polygone ===
cat(">> CORINE Polygone\n")

source_corine <- file.path(archiv_dir, "vergleiche", "Vergleich_CORINE_20260117", "02_daten", "input", "corine_clipped_buffer.gpkg")
if (!file.exists(source_corine)) {
  # Alternative suchen
  source_corine <- file.path(root_dir, "Archiv", "corine_new_class", "Modell_Corine_only", "02_daten", "input", "corine_clipped_buffer.gpkg")
}
target_corine <- file.path(input_dir, "corine_clipped_buffer.gpkg")
copy_or_link(source_corine, target_corine, use_symlink = TRUE)

cat("\n")

# === 3. AOI ===
cat(">> AOI (Area of Interest)\n")

source_aoi <- file.path(archiv_dir, "data", "raw", "aoi", "aoi_donana_neu_noDEMhole.gpkg")
if (!file.exists(source_aoi)) {
  # Alternative suchen
  source_aoi <- file.path(root_dir, "Archiv", "corine_new_class", "Modell_Baseline", "02_daten", "input", "aoi_donana_neu_noDEMhole.gpkg")
}
target_aoi <- file.path(input_dir, "aoi_donana_neu_noDEMhole.gpkg")
copy_or_link(source_aoi, target_aoi, use_symlink = TRUE)

cat("\n")

# === 4. CLCplus (externe Validierung) ===
cat(">> CLCplus Raster (externe Validierung)\n")

source_clcplus <- file.path(archiv_dir, "data", "processed", "Corine", "CLCPlus_2018.tif")
target_clcplus <- file.path(input_dir, "CLCPlus_2018.tif")
if (file.exists(source_clcplus)) {
  copy_or_link(source_clcplus, target_clcplus, use_symlink = TRUE)
} else {
  # Prüfe alternative Pfade
  alt_clcplus <- file.path(root_dir, "CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1", "Data", "CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1.tif")
  if (file.exists(alt_clcplus)) {
    copy_or_link(alt_clcplus, target_clcplus, use_symlink = TRUE)
  } else {
    cat("  ✗ CLCplus Raster nicht gefunden\n")
  }
}

cat("\n")

# === Zusammenfassung ===
cat("===========================================\n")
cat("DATEN ORGANISATION ABGESCHLOSSEN\n")
cat("===========================================\n")
cat("\nAlle Dateien sind jetzt in:\n")
cat("  ", input_dir, "\n\n")

cat("Nächster Schritt: Führe '00_setup_workflow.R' aus\n")
cat("  (wird die Pfade aktualisieren)\n\n")


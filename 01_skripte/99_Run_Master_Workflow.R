#!/usr/bin/env Rscript

# ===========================================
# Master-Skript: Kompletter Workflow
# Führt alle Phasen nacheinander aus
# ===========================================

cat("===========================================\n")
cat("Master-Skript: Kompletter Workflow Variante E\n")
cat("===========================================\n\n")

# === Arbeitsverzeichnis setzen ===
# Setze Arbeitsverzeichnis auf Skript-Verzeichnis
script_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026/01_skripte"

# Versuche automatisch das Verzeichnis zu finden
if (exists("rstudioapi") && rstudioapi::isAvailable()) {
  tryCatch({
    script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  }, error = function(e) {
    # Fallback auf absoluten Pfad
  })
} else {
  # Versuche aus Datei-Pfad zu extrahieren
  tryCatch({
    script_dir <- dirname(sys.frame(1)$ofile)
    if (length(script_dir) == 0 || script_dir == "") {
      script_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026/01_skripte"
    }
  }, error = function(e) {
    # Fallback auf absoluten Pfad
    script_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026/01_skripte"
  })
}

# Setze Arbeitsverzeichnis
setwd(script_dir)
cat("Arbeitsverzeichnis:", getwd(), "\n\n")

# === Phase 0: Vorbereitung ===
cat(">>> Phase 0: Vorbereitung\n")
source("00_setup_workflow.R")
cat("\n")

# === Phase 1: Datenaufbereitung ===
cat(">>> Phase 1: Datenaufbereitung\n")
source("01_data_check.R")
cat("\n")
source("03_extract_training_corine_only.R")
cat("\n")
source("04_extract_training_corine_dgm.R")
cat("\n")

# === Phase 2: Modelltraining ===
cat(">>> Phase 2: Modelltraining\n")
source("05_train_rf_corine_only.R")
cat("\n")
source("06_train_xgb_corine_only.R")
cat("\n")
source("07_train_rf_corine_dgm.R")
cat("\n")
source("08_train_xgb_corine_dgm.R")
cat("\n")

# === Phase 3: Externe Validierung ===
cat(">>> Phase 3: Externe Validierung\n")
source("09_externe_validierung_clcplus.R")
cat("\n")

# === Phase 4: Vergleich & Analyse ===
cat(">>> Phase 4: Vergleich & Analyse\n")
source("10_modellvergleich.R")
cat("\n")
source("11_feature_anova.R")
cat("\n")

# === Optional: k-fold CV ===
cat(">>> Optional: k-fold Cross-Validation\n")
cat("Hinweis: Dies kann einige Zeit dauern...\n")
tryCatch({
  source("05a_kfold_cv.R")
  cat("\n")
}, error = function(e) {
  cat("  ⚠️ k-fold CV übersprungen:", conditionMessage(e), "\n\n")
})

# === Optional: Majority Voting ===
cat(">>> Optional: Majority Voting Ensemble\n")
tryCatch({
  source("09b_majority_voting.R")
  cat("\n")
}, error = function(e) {
  cat("  ⚠️ Majority Voting übersprungen:", conditionMessage(e), "\n\n")
})

# === Phase 5: Klassifikation auf Raster ===
cat(">>> Phase 5: Klassifikation auf gesamtes Raster\n")
cat("Hinweis: Dies kann einige Zeit dauern...\n")
tryCatch({
  source("12_klassifikation_auf_raster.R")
  cat("\n")
}, error = function(e) {
  cat("  ⚠️ Klassifikation übersprungen:", conditionMessage(e), "\n\n")
})

# === Phase 6: Export als GPKG ===
cat(">>> Phase 6: Export Klassifikationen als GPKG\n")
tryCatch({
  source("13_export_klassifikationen_gpkg.R")
  cat("\n")
}, error = function(e) {
  cat("  ⚠️ GPKG-Export übersprungen:", conditionMessage(e), "\n\n")
})

# === Zusammenfassung ===
cat("===========================================\n")
cat("WORKFLOW ABGESCHLOSSEN\n")
cat("===========================================\n")
cat("\nAlle Phasen erfolgreich ausgeführt!\n")
cat("\nErgebnisse finden Sie in:\n")
cat("  - 03_ergebnisse/interne_validierung/\n")
cat("  - 03_ergebnisse/externe_validierung/\n")
cat("  - 03_ergebnisse/vergleiche/\n")
cat("  - 03_ergebnisse/vorhersagen/ (Raster)\n")
cat("  - 03_ergebnisse/klassifikationen_gpkg/ (GPKG)\n")
cat("\nDokumentation:\n")
cat("  - 04_dokumentation/\n")
cat("\n")


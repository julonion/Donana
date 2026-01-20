#!/usr/bin/env Rscript

# =======================================================
# SKRIPT ZUR UMBENENNUNG UND STRUKTURIERUNG DES PROJEKTS
# =======================================================

# Pakete installieren falls nötig
if (!require("here")) install.packages("here")
if (!require("fs")) install.packages("fs")
if (!require("dplyr")) install.packages("dplyr")

library(here)
library(fs)
library(dplyr)

# 1. PFADE DEFINIEREN
# ===================
# Wir nutzen here(), um den Pfad zum Skript-Ordner relativ zum Projekt-Root zu finden
# Annahme: Deine Skripte liegen im Unterordner "01_skripte"
script_dir <- here("01_skripte")

if (!dir_exists(script_dir)) {
  stop(paste("Ordner nicht gefunden:", script_dir, 
             "\nBitte stelle sicher, dass du das Skript im Projekt startest."))
}

cat("Arbeite im Verzeichnis:", script_dir, "\n\n")

# 2. MAPPING DEFINIEREN (Alt -> Neu)
# ==================================
file_map <- tribble(
  ~old, ~new,
  
  # --- 00-09: Setup & Daten ---
  "00_setup_workflow.R",                  "00_Setup_Main.R",
  "00_organisiere_daten.R",               "01_Data_Import.R",
  "01_data_check.R",                      "02_Data_Quality_Check.R",
  
  # --- 10-19: Variante E (Hydrologisch/Alt) ---
  "03_extract_training_corine_only.R",    "10_VarE_Extract_Spectral.R",
  "04_extract_training_corine_dgm.R",     "10_VarE_Extract_DGM.R",
  "05_train_rf_corine_only.R",            "11_VarE_Train_RF_Spectral.R",
  "06_train_xgb_corine_only.R",           "11_VarE_Train_XGB_Spectral.R",
  "07_train_rf_corine_dgm.R",             "12_VarE_Train_RF_DGM.R",
  "08_train_xgb_corine_dgm.R",            "12_VarE_Train_XGB_DGM.R",
  "09_externe_validierung_clcplus.R",     "13_VarE_Validation_CLCplus.R",
  "09b_majority_voting.R",                "14_VarE_Ensemble.R",
  # Optional: Das alte k-fold CV Skript
  "05a_kfold_cv.R",                       "19_VarE_Internal_CV_Spatial.R",
  
  # --- 20-29: Variante B (Ökologisch/Neu) ---
  "03_extract_training_corine_variante_b.R", "20_VarB_Extract_Data.R",
  "05_train_rf_corine_only_variante_b.R",    "21_VarB_Train_RF_Spectral.R",
  "05b_train_xgb_corine_only_variante_b.R",  "21_VarB_Train_XGB_Spectral.R",
  "05c_train_rf_corine_dgm_variante_b.R",    "22_VarB_Train_RF_DGM.R",
  "05c_train_xgb_corine_dgm_variante_b.R",   "22_VarB_Train_XGB_DGM.R",
  "09d_majority_voting_5models_variante_b.R", "23_VarB_Ensemble_5Models.R",
  "09e_ensemble_detailed_analysis.R",         "24_VarB_Ensemble_Analysis.R",
  
  # --- 30-39: Analyse & Finale Produkte ---
  "10_modellvergleich.R",                 "30_Compare_All_Models.R",
  "11_feature_anova.R",                   "31_Feature_Analysis_ANOVA.R",
  "12_klassifikation_auf_raster.R",       "32_Predict_Full_Map.R",
  "13_export_klassifikationen_gpkg.R",    "33_Export_GPKG.R",
  "17_workflow_diagramm.R",               "34_Visualize_Workflow.R",
  "analyze_offener_boden.R",              "35_Special_Analysis_Sand.R"
)

# 3. DATEIEN UMBENENNEN
# =====================
cat(">> Starte Umbenennung...\n")

for (i in 1:nrow(file_map)) {
  old_path <- path(script_dir, file_map$old[i])
  new_path <- path(script_dir, file_map$new[i])
  
  if (file_exists(old_path)) {
    file_move(old_path, new_path)
    cat("  OK: ", file_map$old[i], " -> ", file_map$new[i], "\n")
  } else {
    # Prüfen, ob Datei vielleicht schon den neuen Namen hat
    if (file_exists(new_path)) {
      cat("  Info: ", file_map$new[i], " existiert bereits.\n")
    } else {
      cat("  Warnung: ", file_map$old[i], " nicht gefunden (übersprungen).\n")
    }
  }
}

# 4. ARCHIVIEREN (Optional)
# =========================
# Alte Skripte, die nicht mehr benötigt werden, in einen Archiv-Ordner verschieben
archiv_files <- c(
  "03b_extract_training_corine_5klassen_baseline.R",
  "05b_train_rf_corine_5klassen_baseline.R",
  "09c_ensemble_5klassen_baseline.R"
)

archiv_dir <- path(script_dir, "archiv_alt")
if (!dir_exists(archiv_dir)) dir_create(archiv_dir)

cat("\n>> Verschiebe alte Dateien ins Archiv...\n")
for (f in archiv_files) {
  old_path <- path(script_dir, f)
  new_path <- path(archiv_dir, f)
  
  if (file_exists(old_path)) {
    file_move(old_path, new_path)
    cat("  Archiviert:", f, "\n")
  }
}

cat("\nFertig! Dein Ordner ist jetzt sauber strukturiert.\n")
cat("HINWEIS: Falls du '00_run_complete_workflow.R' nutzt, musst du dort die Dateinamen anpassen!\n")
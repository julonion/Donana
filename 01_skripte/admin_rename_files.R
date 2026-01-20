#!/usr/bin/env Rscript

# =======================================================
# ADMIN-SKRIPT: Projekt aufräumen & Strukturieren
# =======================================================

# Pakete laden (installieren falls nötig)
if (!require("here")) install.packages("here")
if (!require("fs")) install.packages("fs")
if (!require("dplyr")) install.packages("dplyr")

library(here)
library(fs)
library(dplyr)

# 1. PFADE DEFINIEREN
# ===================
# here() findet den Projekt-Root. Wir gehen davon aus, 
# dass die Skripte im Unterordner "01_skripte" liegen.
script_dir <- here("01_skripte")

# Sicherheits-Check
if (!dir_exists(script_dir)) {
  stop(paste("Ordner nicht gefunden:", script_dir, 
             "\nBitte stelle sicher, dass du ein R-Projekt geöffnet hast."))
}

cat(">> Arbeite im Verzeichnis:", script_dir, "\n\n")

# 2. DEFINITION: WAS WIRD WIE UMBENANNT?
# ======================================
# Spalte 'old': Dein aktueller Dateiname
# Spalte 'new': Der neue, logische Name
rename_map <- tribble(
  ~old,                                         ~new,
  
  # --- 00-09: Setup & Daten ---
  "00_setup_workflow.R",                        "00_Setup_Main.R",
  "00_organisiere_daten.R",                     "01_Data_Import.R",
  "01_data_check.R",                            "02_Data_Quality_Check.R",
  
  # --- 10-19: Variante E (Hydrologisch / Alt) ---
  "03_extract_training_corine_only.R",          "10_VarE_Extract_Spectral.R",
  "04_extract_training_corine_dgm.R",           "10_VarE_Extract_DGM.R",
  "05_train_rf_corine_only.R",                  "11_VarE_Train_RF_Spectral.R",
  "06_train_xgb_corine_only.R",                 "11_VarE_Train_XGB_Spectral.R",
  "07_train_rf_corine_dgm.R",                   "12_VarE_Train_RF_DGM.R",
  "08_train_xgb_corine_dgm.R",                  "12_VarE_Train_XGB_DGM.R",
  "09_externe_validierung_clcplus.R",           "13_VarE_Validation_CLCplus.R",
  "09b_majority_voting.R",                      "14_VarE_Ensemble.R",
  "05a_kfold_cv.R",                             "19_VarE_Internal_CV_Spatial.R", # Optional behalten
  
  # --- 20-29: Variante B (Ökologisch / Neu) ---
  "03_extract_training_corine_variante_b.R",    "20_VarB_Extract_Data.R",
  "05_train_rf_corine_only_variante_b.R",       "21_VarB_Train_RF_Spectral.R",
  "05b_train_xgb_corine_only_variante_b.R",     "21_VarB_Train_XGB_Spectral.R",
  "05c_train_rf_corine_dgm_variante_b.R",       "22_VarB_Train_RF_DGM.R",
  "05c_train_xgb_corine_dgm_variante_b.R",      "22_VarB_Train_XGB_DGM.R",
  "09d_majority_voting_5models_variante_b.R",   "23_VarB_Ensemble_5Models.R",
  "09e_ensemble_detailed_analysis.R",           "24_VarB_Ensemble_Analysis.R",
  
  # --- 30-39: Analyse & Finale Produkte ---
  "10_modellvergleich.R",                       "30_Compare_All_Models.R",
  "11_feature_anova.R",                         "31_Feature_Analysis_ANOVA.R",
  "12_klassifikation_auf_raster.R",             "32_Predict_Full_Map.R",
  "13_export_klassifikationen_gpkg.R",          "33_Export_GPKG.R",
  "17_workflow_diagramm.R",                     "34_Visualize_Workflow.R",
  "analyze_offener_boden.R",                    "35_Special_Analysis_Sand.R",
  
  # --- Sonstiges ---
  "00_run_complete_workflow.R",                 "99_Run_Master_Workflow.R"
)

# 3. UMBENENNUNG DURCHFÜHREN
# ==========================
cat(">> Starte Umbenennung...\n")

count_renamed <- 0
for (i in 1:nrow(rename_map)) {
  old_f <- path(script_dir, rename_map$old[i])
  new_f <- path(script_dir, rename_map$new[i])
  
  if (file_exists(old_f)) {
    file_move(old_f, new_f)
    cat("  [OK] Umbenannt: ", rename_map$old[i], " -> ", rename_map$new[i], "\n")
    count_renamed <- count_renamed + 1
  } else if (file_exists(new_f)) {
    cat("  [Info] Datei existiert schon: ", rename_map$new[i], "\n")
  } else {
    cat("  [Warnung] Datei nicht gefunden: ", rename_map$old[i], "\n")
  }
}
cat("\n  -> ", count_renamed, " Dateien umbenannt.\n\n")


# 4. ARCHIVIEREN (Alte Dateien aufräumen)
# =======================================
# Diese Dateien werden nicht mehr aktiv genutzt
archive_files <- c(
  "03b_extract_training_corine_5klassen_baseline.R",
  "05b_train_rf_corine_5klassen_baseline.R",
  "09c_ensemble_5klassen_baseline.R"
)

archiv_dir <- path(script_dir, "archiv_alt")
if (!dir_exists(archiv_dir)) dir_create(archiv_dir)

cat(">> Verschiebe alte Dateien ins Archiv...\n")
for (f in archive_files) {
  src <- path(script_dir, f)
  dst <- path(archiv_dir, f)
  
  if (file_exists(src)) {
    file_move(src, dst)
    cat("  [Archiviert]:", f, "\n")
  }
}

cat("\n===========================================\n")
cat("FERTIG! Dein Ordner ist jetzt sauber.\n")
cat("WICHTIG: Bitte aktualisiere dein Master-Skript (99_Run...), \n")
cat("da die Dateinamen darin jetzt noch die alten sind!\n")
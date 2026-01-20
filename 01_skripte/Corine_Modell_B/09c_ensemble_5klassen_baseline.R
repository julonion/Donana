#!/usr/bin/env Rscript

# ===========================================
# Phase 3 (Variante Baseline-kompatibel):
# Ensemble-Modell (Soft Voting)
# Kombiniert:
# 1. Baseline (visuelle Expertendaten)
# 2. CORINE 5-Klassen (standardisierte Daten)
# ===========================================

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(randomForest)
  library(caret)
})

# === Parameter & Pfade ===
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"
work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"

# Workflow-Pfade (wie in den anderen Skripten)
paths_file <- file.path(work_dir, "02_daten", "input", "workflow_paths.rds")
if (!file.exists(paths_file)) {
  stop("Pfade nicht gefunden. Bitte zuerst '00_setup_workflow.R' ausführen!")
}
paths <- readRDS(paths_file)

# Modell-Pfade
model_corine_path <- file.path(
  work_dir, "02_daten", "modelle",
  "rf_model_corine_5klassen_baseline_kompatibel.rds"
)
model_baseline_path <- file.path(
  root_dir, "Archiv", "corine_new_class", "Modell_Baseline",
  "02_daten", "modelle", "rf_model_baseline.rds"
)

# Feature-Listen
features_corine_path <- file.path(
  work_dir, "02_daten", "modelle",
  "rf_corine_5klassen_baseline_kompatibel_features.rds"
)
features_baseline_path <- file.path(
  root_dir, "Archiv", "corine_new_class", "Modell_Baseline",
  "02_daten", "modelle", "rf_baseline_features.rds"
)

# Validierungs-Referenz & Feature-Stacks über workflow_paths.rds
clcplus_raster_path <- paths$clcplus_raster
if (!file.exists(clcplus_raster_path)) {
  clcplus_raster_path <- paths$clcplus_raster_alt
}
if (!file.exists(clcplus_raster_path)) {
  stop(
    "CLCplus-Raster nicht gefunden.\n",
    "  Erwartete Pfade:\n  - ", paths$clcplus_raster,
    "\n  - ", paths$clcplus_raster_alt, "\n",
    "Bitte prüfe, wo das CLCplus-TIF liegt, und passe workflow_paths.rds an."
  )
}

feature_stack_path <- paths$feature_stack_2018
aoi_path <- paths$aoi

# Ausgabedateien
output_dir <- file.path(work_dir, "03_ergebnisse", "ensemble_5klassen_baseline")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
report_txt <- file.path(output_dir, "ensemble_report.txt")
metrics_csv <- file.path(output_dir, "ensemble_metrics.csv")
agreement_csv <- file.path(output_dir, "ensemble_model_agreement.csv")

set.seed(42)

cat("===========================================\n")
cat("Ensemble-Modell: Baseline + CORINE (5 Klassen)\n")
cat("===========================================\n\n")

# === 1. Modelle und Daten laden ===
cat(">> Lade Modelle\n")
rf_corine <- readRDS(model_corine_path)
rf_baseline <- readRDS(model_baseline_path)
features_corine <- readRDS(features_corine_path)
features_baseline <- readRDS(features_baseline_path)

cat(">> Lade Referenz-Daten\n")
clc_ref <- rast(clcplus_raster_path)
feat_stack <- rast(feature_stack_path)
aoi <- vect(aoi_path)

# === 2. Mapping-Funktionen ===
# Harmonisierung auf gemeinsame 5 Zielklassen
target_levels <- c("Hartlaubvegetation", "Salzwiesen", "Sand", "Wald", "Wasser")

map_baseline_to_target <- function(prob_mat) {
  # Baseline Klassen: Hartlaubvegetation, Salzwiesen, Straende_Duenen_Sand, Wald, wasserflaechen
  df <- as.data.frame(prob_mat)
  data.frame(
    Hartlaubvegetation = df$Hartlaubvegetation,
    Salzwiesen = df$Salzwiesen,
    Sand = df$Straende_Duenen_Sand,
    Wald = df$Wald,
    Wasser = df$wasserflaechen
  )
}

map_clcplus_to_target <- function(code) {
  if (is.na(code)) return(NA)
  if (code == 2 || code == 3) return("Hartlaubvegetation/Wald") # Problem: CLCplus 2=Forest, 3=Scrub
  # Für Validierung müssen wir evtl. Klassen zusammenlegen oder Wald/Busch getrennt mappen:
  if (code == 2) return("Wald")
  if (code == 3) return("Hartlaubvegetation")
  if (code == 5) return("Sand")
  if (code == 6) return("Salzwiesen")
  if (code == 7 || code == 10) return("Wasser")
  return(NA)
}

# === 3. Validierungs-Punkte extrahieren ===
cat(">> Extrahiere Validierungs-Punkte (Stichprobe)\n")
sample_factor <- 10
clc_sampled <- aggregate(clc_ref, fact = sample_factor, fun = "modal", na.rm = TRUE)
clc_sampled <- project(clc_sampled, crs(feat_stack), method = "near")
clc_sampled <- mask(crop(clc_sampled, aoi), aoi)

dat <- as.data.frame(clc_sampled, xy = TRUE, na.rm = TRUE)
names(dat)[3] <- "clc_code"
dat$reference <- sapply(dat$clc_code, map_clcplus_to_target)
dat <- dat[!is.na(dat$reference), ]

cat("   Anzahl Validierungs-Punkte:", nrow(dat), "\n")

# Features extrahieren
pts <- vect(dat[, c("x", "y")], geom = c("x", "y"), crs = crs(feat_stack))
feat_vals <- terra::extract(feat_stack, pts)
feat_vals <- feat_vals[, -1] # ID entfernen

# === 4. Vorhersagen (Klassen) ===
cat(">> Berechne Vorhersagen der Modelle\n")

# CORINE-Modell: Direkte Klassen-Vorhersage
corine_class <- predict(rf_corine, newdata = feat_vals[, features_corine])
corine_class <- factor(as.character(corine_class), levels = target_levels)

# Baseline-Modell: Vorhersage + Mapping auf Zielklassen
baseline_class_raw <- predict(rf_baseline, newdata = feat_vals[, features_baseline])
baseline_class_raw <- as.character(baseline_class_raw)

# Mapping Baseline-Klassen → Zielklassen
baseline_class_mapped <- sapply(baseline_class_raw, function(x) {
  if (x == "Hartlaubvegetation") return("Hartlaubvegetation")
  if (x == "Salzwiesen") return("Salzwiesen")
  if (x == "Straende_Duenen_Sand") return("Sand")
  if (x == "Wald") return("Wald")
  if (x == "wasserflaechen") return("Wasser")
  return(NA)
})
baseline_class <- factor(baseline_class_mapped, levels = target_levels)

cat("   CORINE Vorhersagen:", length(corine_class), "Pixel\n")
cat("   Baseline Vorhersagen:", length(baseline_class), "Pixel\n\n")

# === 5. Ensemble bilden (Majority Voting) ===
cat(">> Bilde Ensemble (Majority Voting: Jedes Modell gibt eine Stimme ab)\n")
cat("   Bei Gleichstand gewinnt CORINE (als 'wissenschaftlicheres' Modell)\n\n")

# Majority Voting Funktion
majority_voting <- function(votes) {
  # votes ist ein Vektor mit Klassen (z.B. c("Wald", "Wasser", "Wald"))
  tab <- table(votes)
  max_votes <- max(tab)
  winners <- names(tab)[tab == max_votes]
  
  if (length(winners) == 1) {
    return(winners[1])
  } else {
    # Bei Gleichstand: Bevorzuge CORINE-Klasse
    # Finde welche Klasse CORINE gewählt hat
    corine_vote <- votes[1]  # Erste Stimme ist CORINE
    if (corine_vote %in% winners) {
      return(corine_vote)
    } else {
      # Falls CORINE nicht unter Gewinnern: nimm erste
      return(winners[1])
    }
  }
}

# Für jeden Pixel: Sammle Stimmen beider Modelle
ensemble_class <- character(length(corine_class))
for (i in 1:length(corine_class)) {
  votes <- c(as.character(corine_class[i]), as.character(baseline_class[i]))
  ensemble_class[i] <- majority_voting(votes)
}
ensemble_class <- factor(ensemble_class, levels = target_levels)

cat("   Ensemble-Vorhersagen:", length(ensemble_class), "Pixel\n\n")

# Referenz-Klasse (nur für CLCplus-Vergleich)
ref <- factor(dat$reference, levels = target_levels)

# === 6. Abstimmung der Modelle untereinander (ohne CLCplus) ===
cat(">> Analysiere Abstimmung der Modelle (Majority Voting)\n\n")

# 6.1 Übereinstimmung in der Klassenwahl (Pixelweise)
same_corine_baseline <- corine_class == baseline_class
same_ensemble_corine <- ensemble_class == corine_class
same_ensemble_baseline <- ensemble_class == baseline_class

# 6.2 Gleichstände zählen (wenn beide Modelle unterschiedlich votieren)
ties <- corine_class != baseline_class
n_ties <- sum(ties)
n_agreements <- sum(same_corine_baseline)

cat("  Abstimmungs-Statistik:\n")
cat("    - Beide Modelle stimmen überein:", n_agreements, "Pixel (", 
    round(100 * mean(same_corine_baseline), 1), "%)\n")
cat("    - Beide Modelle stimmen NICHT überein (Gleichstand):", n_ties, "Pixel (",
    round(100 * mean(ties), 1), "%)\n")
cat("    - Bei Gleichstand: CORINE gewinnt (als wissenschaftlicheres Modell)\n\n")

# 6.3 Wie oft gewinnt welches Modell bei Gleichstand?
corine_wins <- sum(ties & (ensemble_class == corine_class))
baseline_wins <- sum(ties & (ensemble_class == baseline_class))

cat("  Bei Gleichstand (", n_ties, " Pixel):\n", sep = "")
cat("    - CORINE gewinnt:", corine_wins, "Pixel (", 
    round(100 * corine_wins / n_ties, 1), "%)\n")
cat("    - Baseline gewinnt:", baseline_wins, "Pixel (",
    round(100 * baseline_wins / n_ties, 1), "%)\n\n")

agreement_rates <- data.frame(
  Paar = c("CORINE_vs_Baseline", "Ensemble_vs_CORINE", "Ensemble_vs_Baseline", 
           "Gleichstaende_gesamt", "Gleichstaende_CORINE_gewinnt", "Gleichstaende_Baseline_gewinnt"),
  Agreement = c(
    mean(same_corine_baseline),
    mean(same_ensemble_corine),
    mean(same_ensemble_baseline),
    mean(ties),
    ifelse(n_ties > 0, corine_wins / n_ties, 0),
    ifelse(n_ties > 0, baseline_wins / n_ties, 0)
  )
)

# 6.2 Klassenweise Konfusionsmatrizen (Modell vs. Modell)
conf_corine_baseline <- as.data.frame.matrix(table(CORINE = corine_class, Baseline = baseline_class))
conf_ensemble_corine <- as.data.frame.matrix(table(Ensemble = ensemble_class, CORINE = corine_class))
conf_ensemble_baseline <- as.data.frame.matrix(table(Ensemble = ensemble_class, Baseline = baseline_class))

# Für CSV speichern wir nur die Gesamt-Agreement-Raten und die Roh-Tabellen separat
write.csv(agreement_rates, agreement_csv, row.names = FALSE)

# === 7. Metriken gegen CLCplus berechnen ===
cat(">> Berechne Metriken gegen CLCplus\n")

cm_ensemble <- confusionMatrix(ensemble_class, ref)
cm_corine <- confusionMatrix(corine_class, ref)
cm_baseline <- confusionMatrix(baseline_class, ref)

# === 8. Ergebnisse speichern ===
cat(">> Speichere Ergebnisse in:", output_dir, "\n")

metrics_df <- data.frame(
  Modell = c("Ensemble", "CORINE_5Klassen", "Baseline"),
  Accuracy = c(cm_ensemble$overall["Accuracy"], cm_corine$overall["Accuracy"], cm_baseline$overall["Accuracy"]),
  Kappa = c(cm_ensemble$overall["Kappa"], cm_corine$overall["Kappa"], cm_baseline$overall["Kappa"])
)
write.csv(metrics_df, metrics_csv, row.names = FALSE)

sink(report_txt)
cat("Ensemble-Report: Baseline + CORINE (5 Klassen)\n")
cat("==============================================\n\n")
cat("Datum:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("ENSEMBLE-METHODE: Majority Voting (Hard Voting)\n")
cat("  - Jedes Modell gibt eine Klasse ab (1 Stimme pro Modell)\n")
cat("  - Die häufigste Klasse gewinnt\n")
cat("  - Bei Gleichstand (1:1): CORINE gewinnt (als wissenschaftlicheres Modell)\n\n")

cat("MODELL-ABSTIMMUNG (ohne Referenz):\n")
cat("  CORINE vs. Baseline (gleiche Klasse):",
    round(agreement_rates$Agreement[agreement_rates$Paar == "CORINE_vs_Baseline"], 4), "\n")
cat("  Ensemble vs. CORINE (gleiche Klasse):",
    round(agreement_rates$Agreement[agreement_rates$Paar == "Ensemble_vs_CORINE"], 4), "\n")
cat("  Ensemble vs. Baseline (gleiche Klasse):",
    round(agreement_rates$Agreement[agreement_rates$Paar == "Ensemble_vs_Baseline"], 4), "\n\n")

cat("GLEICHSTÄNDE (wenn Modelle unterschiedlich votieren):\n")
cat("  Anzahl Gleichstände:", n_ties, "Pixel (",
    round(100 * agreement_rates$Agreement[agreement_rates$Paar == "Gleichstaende_gesamt"], 1), "%)\n")
cat("  - CORINE gewinnt bei Gleichstand:",
    round(100 * agreement_rates$Agreement[agreement_rates$Paar == "Gleichstaende_CORINE_gewinnt"], 1), "%\n")
cat("  - Baseline gewinnt bei Gleichstand:",
    round(100 * agreement_rates$Agreement[agreement_rates$Paar == "Gleichstaende_Baseline_gewinnt"], 1), "%\n\n")

cat("VERGLEICH Accuracy & Kappa (gegen CLCplus):\n")
print(metrics_df)
cat("\n")

cat("CONFUSION MATRIX ENSEMBLE:\n")
print(cm_ensemble$table)
cat("\n")

cat("KLASSENWEISE METRIKEN ENSEMBLE:\n")
print(cm_ensemble$byClass)
sink()

cat("\nFertig! Ergebnisse liegen in:", output_dir, "\n")


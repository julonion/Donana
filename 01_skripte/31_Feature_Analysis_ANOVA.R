#!/usr/bin/env Rscript

# ===========================================
# Phase 4: Feature-Analyse mit ANOVA
# ANOVA für alle Features (Elevation, NDVI, NDWI, ...)
# ===========================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(broom)
})

# === Parameter ===
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"
work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"

# Eingabedateien
samples_csv_only <- file.path(work_dir, "02_daten", "training", "samples_corine_2018_variante_e.csv")
samples_csv_dgm <- file.path(work_dir, "02_daten", "training", "samples_corine_2018_with_dgm_variante_e.csv")

# Ausgabedateien
output_dir <- file.path(work_dir, "03_ergebnisse", "vergleiche", "feature_anova")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

anova_csv_only <- file.path(output_dir, "anova_corine_only.csv")
anova_csv_dgm <- file.path(output_dir, "anova_corine_dgm.csv")
anova_report <- file.path(output_dir, "anova_report.txt")

set.seed(42)

cat("===========================================\n")
cat("Feature-Analyse: ANOVA für alle Features\n")
cat("===========================================\n\n")

# === Helper-Funktion für ANOVA ===
perform_anova_analysis <- function(samples_df, output_file) {
  cat(">> ANOVA-Analyse\n")
  
  # Daten vorbereiten
  drop_cols <- c("polygon_id", "Code_18", "area_km2", "quell_jahr", "quelle", "class_id", "x", "y")
  feature_cols <- setdiff(names(samples_df), c(drop_cols, "class_name"))
  
  # Stichprobe für ANOVA (um Laufzeit zu reduzieren)
  sampled_data <- samples_df %>%
    group_by(class_name) %>%
    sample_n(min(1000, n())) %>%
    ungroup()
  
  cat("  Stichprobe:", nrow(sampled_data), "Samples\n")
  cat("  Features:", length(feature_cols), "\n\n")
  
  # ANOVA für jedes Feature
  anova_results <- data.frame(
    Feature = character(),
    F_statistic = numeric(),
    p_value = numeric(),
    df_between = numeric(),
    df_within = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (feature in feature_cols) {
    # Prüfe auf fehlende Werte
    complete_data <- sampled_data %>%
      select(all_of(c(feature, "class_name"))) %>%
      filter(!is.na(!!sym(feature)))
    
    if (nrow(complete_data) < 10) next
    
    # ANOVA
    formula_str <- paste(feature, "~ class_name")
    aov_result <- aov(as.formula(formula_str), data = complete_data)
    aov_summary <- summary(aov_result)
    
    # Extrahiere Ergebnisse
    f_stat <- aov_summary[[1]][["F value"]][1]
    p_val <- aov_summary[[1]][["Pr(>F)"]][1]
    df_between <- aov_summary[[1]][["Df"]][1]
    df_within <- aov_summary[[1]][["Df"]][2]
    
    anova_results <- rbind(anova_results, data.frame(
      Feature = feature,
      F_statistic = f_stat,
      p_value = p_val,
      df_between = df_between,
      df_within = df_within,
      stringsAsFactors = FALSE
    ))
    
    if (length(feature_cols) <= 20 || which(feature_cols == feature) %% 5 == 0) {
      cat("  ", feature, ": F =", round(f_stat, 2), ", p =", format.pval(p_val, digits = 3), "\n")
    }
  }
  
  # Sortiere nach p-Wert
  anova_results <- anova_results %>%
    arrange(p_value) %>%
    mutate(
      significant = p_value < 0.05,
      highly_significant = p_value < 0.01,
      very_highly_significant = p_value < 0.001
    )
  
  # Speichere Ergebnisse
  write.csv(anova_results, output_file, row.names = FALSE)
  
  cat("\n  Signifikante Features (p < 0.05):", sum(anova_results$significant), "von", nrow(anova_results), "\n")
  cat("  Hoch signifikant (p < 0.01):", sum(anova_results$highly_significant), "\n")
  cat("  Sehr hoch signifikant (p < 0.001):", sum(anova_results$very_highly_significant), "\n\n")
  
  return(anova_results)
}

# === 1. ANOVA für CORINE only (spektrale Features) ===
if (file.exists(samples_csv_only)) {
  cat("===========================================\n")
  cat("1. ANOVA: CORINE only (spektrale Features)\n")
  cat("===========================================\n\n")
  
  samples <- read_csv(samples_csv_only, show_col_types = FALSE)
  anova_only <- perform_anova_analysis(samples, anova_csv_only)
}

# === 2. ANOVA für CORINE + DGM (alle Features) ===
if (file.exists(samples_csv_dgm)) {
  cat("===========================================\n")
  cat("2. ANOVA: CORINE + DGM (alle Features)\n")
  cat("===========================================\n\n")
  
  samples <- read_csv(samples_csv_dgm, show_col_types = FALSE)
  anova_dgm <- perform_anova_analysis(samples, anova_csv_dgm)
}

# === Report ===
sink(anova_report)
cat("Feature-Analyse: ANOVA für alle Features\n")
cat("=========================================\n\n")
cat("Datum:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("Methode:\n")
cat("  - Einweg-ANOVA für jedes Feature\n")
cat("  - Nullhypothese: Keine Unterschiede zwischen Klassen\n")
cat("  - Signifikanzniveau: p < 0.05\n\n")

if (file.exists(samples_csv_only)) {
  cat("===========================================\n")
  cat("CORINE only (spektrale Features)\n")
  cat("===========================================\n\n")
  cat("Top 10 signifikanteste Features:\n")
  top_features <- head(anova_only[order(anova_only$p_value), ], 10)
  for (i in 1:nrow(top_features)) {
    cat(sprintf("  %d. %s: F = %.2f, p = %s\n", 
                i, top_features$Feature[i], 
                top_features$F_statistic[i],
                format.pval(top_features$p_value[i], digits = 3)))
  }
  cat("\n")
}

if (file.exists(samples_csv_dgm)) {
  cat("===========================================\n")
  cat("CORINE + DGM (alle Features)\n")
  cat("===========================================\n\n")
  cat("Top 10 signifikanteste Features:\n")
  top_features <- head(anova_dgm[order(anova_dgm$p_value), ], 10)
  for (i in 1:nrow(top_features)) {
    cat(sprintf("  %d. %s: F = %.2f, p = %s\n", 
                i, top_features$Feature[i], 
                top_features$F_statistic[i],
                format.pval(top_features$p_value[i], digits = 3)))
  }
  cat("\n")
}

cat("Interpretation:\n")
cat("  - Signifikante Features (p < 0.05) zeigen signifikante Unterschiede zwischen Klassen\n")
cat("  - Diese Features sind wichtig für die Klassifikation\n")
cat("  - DGM-Features (besonders elevation) sind typischerweise sehr signifikant\n\n")

sink()

cat("===========================================\n")
cat("ANOVA-ANALYSE ABGESCHLOSSEN\n")
cat("===========================================\n")
cat("\nErgebnisse gespeichert in:", output_dir, "\n")


#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(caret); library(randomForest)
})

work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"

# Input/Output für Variante B
samples_csv <- file.path(work_dir, "02_daten", "training", "samples_corine_2018_variante_b.csv")
model_output <- file.path(work_dir, "02_daten", "modelle", "rf_model_corine_only_variante_b.rds")
feature_list_path <- file.path(work_dir, "02_daten", "modelle", "rf_corine_only_variante_b_features.rds")

cat(">> Training RF Corine Only (Variante B)\n")
samples <- read_csv(samples_csv, show_col_types = FALSE)

# Features wählen (ohne DGM)
drop_cols <- c("polygon_id", "Code_18", "area_km2", "quell_jahr", "quelle", "class_id", "x", "y", "class_name")
all_features <- setdiff(names(samples), drop_cols)
# DGM Features ausschließen falls vorhanden
dgm_cols <- c("elevation", "slope", "aspect", "tri", "twi", "TRI", "TWI")
feature_cols <- setdiff(all_features, dgm_cols)

saveRDS(feature_cols, feature_list_path)

# Training
set.seed(42)
train_idx <- createDataPartition(samples$class_name, p = 0.8, list = FALSE)
train_set <- samples[train_idx, ]

rf_model <- randomForest(
  x = train_set[, feature_cols],
  y = factor(train_set$class_name),
  ntree = 500,
  importance = TRUE
)

saveRDS(rf_model, model_output)
cat("Fertig: ", basename(model_output), "\n")
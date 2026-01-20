#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(caret); library(xgboost)
})

work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"

samples_csv <- file.path(work_dir, "02_daten", "training", "samples_corine_2018_variante_b.csv")
model_output <- file.path(work_dir, "02_daten", "modelle", "xgb_model_corine_only_variante_b.rds")
feature_list_path <- file.path(work_dir, "02_daten", "modelle", "xgb_corine_only_variante_b_features.rds")

cat(">> Training XGBoost Corine Only (Variante B)\n")
samples <- read_csv(samples_csv, show_col_types = FALSE)

drop_cols <- c("polygon_id", "Code_18", "area_km2", "quell_jahr", "quelle", "class_id", "x", "y", "class_name")
all_features <- setdiff(names(samples), drop_cols)
dgm_cols <- c("elevation", "slope", "aspect", "tri", "twi", "TRI", "TWI")
feature_cols <- setdiff(all_features, dgm_cols)

saveRDS(feature_cols, feature_list_path)

set.seed(42)
train_idx <- createDataPartition(samples$class_name, p = 0.8, list = FALSE)
train_set <- samples[train_idx, ]
test_set <- samples[-train_idx, ]

train_matrix <- as.matrix(train_set[, feature_cols])
test_matrix <- as.matrix(test_set[, feature_cols])
train_labels <- as.numeric(factor(train_set$class_name)) - 1
test_labels <- as.numeric(factor(test_set$class_name)) - 1

dtrain <- xgb.DMatrix(train_matrix, label = train_labels)
dtest <- xgb.DMatrix(test_matrix, label = test_labels)

xgb_model <- xgb.train(
  data = dtrain,
  watchlist = list(train = dtrain, valid = dtest),
  params = list(objective = "multi:softprob", num_class = length(unique(train_labels)), eta = 0.1, max_depth = 4),
  nrounds = 500,
  early_stopping_rounds = 20,
  verbose = 0
)

# WICHTIG: Klassennamen speichern für Ensemble!
xgb_model$class_levels <- levels(factor(samples$class_name))
saveRDS(xgb_model, model_output)
cat("Fertig: ", basename(model_output), "\n")
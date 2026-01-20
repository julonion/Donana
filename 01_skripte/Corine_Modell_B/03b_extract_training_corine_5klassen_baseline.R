#!/usr/bin/env Rscript

# ===========================================
# Phase 1 (Variante Baseline-kompatibel):
# Extraktion von CORINE-Trainingsdaten
# 5 Klassen: Hartlaubvegetation, Salzwiesen, Wasser, Wald, Sand
# ===========================================

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tibble)
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

# Eingabedateien
corine_polygone <- paths$corine_polygone
feature_stack   <- paths$feature_stack_2018
aoi_path        <- paths$aoi

# Ausgabedatei
ausgabe_csv <- file.path(
  work_dir,
  "02_daten",
  "training",
  "samples_corine_2018_5klassen_baseline_kompatibel.csv"
)
dir.create(dirname(ausgabe_csv), showWarnings = FALSE, recursive = TRUE)

cat("===========================================\n")
cat("Extraktion von CORINE-Trainingsdaten\n")
cat("5 Klassen – Baseline-kompatibel\n")
cat("Klassen: Hartlaubvegetation, Salzwiesen, Wasser, Wald, Sand\n")
cat("===========================================\n\n")

# === Klasseneinteilung (Baseline-kompatibel) ===
# Mapping basiert auf Codes im AOI:
# 311, 312  → Wald
# 323, 324, 231 → Hartlaubvegetation
# 421, 422, 411 → Salzwiesen
# 511, 512 → Wasser
# 331, 333 → Sand

class_map <- tribble(
  ~Code_18, ~target_class,

  # Wald
  "311", "Wald",              # Laubwald
  "312", "Wald",              # Nadelwald

  # Hartlaubvegetation / Buschland / Weideland
  "323", "Hartlaubvegetation",   # Hartlaubvegetation
  "324", "Hartlaubvegetation",   # Übergangsvegetation (Wald–Busch)
  "231", "Hartlaubvegetation",   # Weideland (fachlich näher bei Buschland)

  # Salzwiesen / Feuchtgebiete
  "421", "Salzwiesen",        # Salzmarschen
  "422", "Salzwiesen",        # Salinen / Salzpfannen
  "411", "Salzwiesen",        # Binnenmarschen

  # Wasser
  "511", "Wasser",            # Fließgewässer
  "512", "Wasser",            # Binnenseen / -gewässer

  # Sand
  "331", "Sand",              # Strände, Dünen, Sandflächen
  "333", "Sand"               # Spärlich bewachsene Flächen
)

cat("Klasseneinteilung (5 Klassen, Baseline-kompatibel):\n")
print(class_map)
cat("\n")

# === Prüfungen ===
if (!file.exists(corine_polygone)) {
  stop("CORINE-Polygone nicht gefunden: ", corine_polygone)
}
if (!file.exists(feature_stack)) {
  stop("Feature-Stack nicht gefunden: ", feature_stack)
}

# === Daten laden ===
cat(">> Lade CORINE-Polygone\n")
corine_vect   <- vect(corine_polygone)
raster_stack  <- rast(feature_stack)
grenz_vektor  <- vect(aoi_path)

# Projektionen angleichen
if (!identical(crs(corine_vect), crs(grenz_vektor))) {
  grenz_vektor <- project(grenz_vektor, crs(corine_vect))
}

# Polygone auf AOI beschränken
beschnittene_polygone <- terra::intersect(corine_vect, grenz_vektor)

# Polygon-Metadaten vorbereiten
polygon_metadaten <- as.data.frame(beschnittene_polygone) %>%
  mutate(polygon_id = paste0("corine_", row_number()))

cat("  Anzahl Polygone (im AOI): ", nrow(polygon_metadaten), "\n", sep = "")
cat("  Originale Code_18 Werte:", paste(sort(unique(polygon_metadaten$Code_18)), collapse = ", "), "\n\n")

# === Klassen-Mapping anwenden ===
cat(">> Wende Klassen-Mapping (5 Klassen) an\n")

if (is.numeric(polygon_metadaten$Code_18)) {
  polygon_metadaten$Code_18 <- as.character(polygon_metadaten$Code_18)
}
if ("target_class" %in% names(polygon_metadaten)) {
  polygon_metadaten <- polygon_metadaten %>% select(-target_class)
}

polygon_metadaten <- polygon_metadaten %>%
  left_join(class_map, by = "Code_18") %>%
  filter(!is.na(target_class))

cat("  Polygone nach Mapping:", nrow(polygon_metadaten), "\n")
cat("  Klassenverteilung (Polygone):\n")
print(table(polygon_metadaten$target_class))
cat("\n")

# CRS-Abgleich Polygone ↔ Raster
if (!identical(crs(beschnittene_polygone), crs(raster_stack))) {
  cat(">> Projiziere Polygone auf Raster-CRS\n")
  beschnittene_polygone <- project(beschnittene_polygone, crs(raster_stack))
  polygon_metadaten <- polygon_metadaten %>%
    mutate(polygon_id = paste0("corine_", row_number()))
}

# Pixelwerte extrahieren
cat(">> Extrahiere Pixelwerte aus Feature-Stack\n")
extrahierte_werte <- terra::extract(raster_stack, beschnittene_polygone, xy = TRUE) %>%
  rename(temp_polygon_id = ID) %>%
  mutate(polygon_id = paste0("corine_", temp_polygon_id)) %>%
  select(-temp_polygon_id)

# Metadaten mit Pixelwerten verknüpfen
trainings_samples <- extrahierte_werte %>%
  left_join(polygon_metadaten %>% select(polygon_id, Code_18, target_class, area_km2),
            by = "polygon_id") %>%
  filter(!is.na(target_class)) %>%
  mutate(
    quell_jahr = "2018",
    quelle     = "CORINE"
  )

cat("  Extrahierte Samples:", nrow(trainings_samples), "\n\n")

# Klassenverteilung prüfen
cat(">> Klassenverteilung in extrahierten Samples:\n")
print(table(trainings_samples$target_class))
cat("\n")

# Balanciertes Sampling (max. 2000 Samples je Klasse)
cat(">> Balanciertes Sampling\n")
set.seed(42)
balancierte_samples <- list()

eindeutige_klassen      <- unique(trainings_samples$target_class)
max_samples_pro_klasse  <- 2000

for (klasse in eindeutige_klassen) {
  subset_klasse       <- trainings_samples[trainings_samples$target_class == klasse, ]
  verfuegbare_samples <- nrow(subset_klasse)
  samples_auswaehlen  <- min(max_samples_pro_klasse, verfuegbare_samples)

  if (samples_auswaehlen > 0) {
    ausgewaehlt <- subset_klasse[sample(nrow(subset_klasse), samples_auswaehlen), ]
    balancierte_samples[[klasse]] <- ausgewaehlt
    cat("  Klasse", klasse, ":", samples_auswaehlen, "von", verfuegbare_samples, "Samples\n")
  }
}

finaler_trainingsdatensatz <- do.call(rbind, balancierte_samples)

# Spalten für Kompatibilität anpassen
finaler_trainingsdatensatz <- finaler_trainingsdatensatz %>%
  rename(class_name = target_class) %>%
  mutate(class_id = as.numeric(factor(class_name)))

# Ausgabe speichern
cat("\n>> Speichere Trainingsdaten\n")
write.csv(finaler_trainingsdatensatz, ausgabe_csv, row.names = FALSE)
cat("  Gespeichert:", basename(ausgabe_csv), "\n\n")

cat("===========================================\n")
cat("Zusammenfassung\n")
cat("===========================================\n")
cat("Gesamtanzahl Trainingssamples:", nrow(finaler_trainingsdatensatz), "\n\n")
cat("Verteilung pro Klasse:\n")
print(table(finaler_trainingsdatensatz$class_name))
cat("\n")

cat("Nächster Schritt (Vorschlag):\n")
cat("  - 05b_train_rf_corine_5klassen_baseline.R ausführen\n")



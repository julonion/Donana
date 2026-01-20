#!/usr/bin/env Rscript

# ===========================================
# Phase 1: Extraktion von Trainingsdaten aus CORINE
# Variante B – 5 Klassen nach ökologischer Funktion
# (Wald, Buschland, Offener_Boden, Feuchtgebiet, Wasser)
# ===========================================

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tibble)
  library(sf) # Zur Sicherheit für Vektor-Operationen
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
feature_stack   <- paths$feature_stack_2018  # Standard Sentinel-2 Stack
aoi_path        <- paths$aoi

# Ausgabedatei
ausgabe_csv <- file.path(
  work_dir,
  "02_daten",
  "training",
  "samples_corine_2018_variante_b.csv"
)
dir.create(dirname(ausgabe_csv), showWarnings = FALSE, recursive = TRUE)

cat("===========================================\n")
cat("Extraktion von CORINE-Trainingsdaten\n")
cat("Variante B – Ökologische Funktionsgruppen\n")
cat("===========================================\n\n")

# === Klasseneinteilung (Variante B) ===
# Mapping basierend auf deiner Tabelle
class_map <- tribble(
  ~Code_18, ~target_class,
  
  # Wald (Laub + Nadel)
  "311", "Wald",
  "312", "Wald",
  
  # Buschland (Hartlaub + Übergang + Weideland)
  "323", "Buschland",
  "324", "Buschland",
  "231", "Buschland",
  
  # Offener Boden (Sand + Spärlich bewachsen)
  "331", "Offener_Boden",
  "333", "Offener_Boden",
  
  # Feuchtgebiet (Binnen + Salz + Salinen)
  "411", "Feuchtgebiet",
  "421", "Feuchtgebiet",
  "422", "Feuchtgebiet",
  
  # Wasser (Fließ + Binnen)
  "511", "Wasser",
  "512", "Wasser"
)

cat("Klasseneinteilung Variante B:\n")
print(class_map)
cat("\n")

# === Daten laden ===
cat(">> Lade Daten\n")
if (!file.exists(corine_polygone)) stop("CORINE Polygone fehlen.")
if (!file.exists(feature_stack)) stop("Feature Stack fehlt.")

corine_vect   <- vect(corine_polygone)
raster_stack  <- rast(feature_stack)
grenz_vektor  <- vect(aoi_path)

# Projektion angleichen
if (!identical(crs(corine_vect), crs(grenz_vektor))) {
  grenz_vektor <- project(grenz_vektor, crs(corine_vect))
}

# Polygone auf AOI beschränken
beschnittene_polygone <- terra::intersect(corine_vect, grenz_vektor)

# Metadaten vorbereiten
polygon_metadaten <- as.data.frame(beschnittene_polygone) %>%
  mutate(polygon_id = paste0("corine_", row_number()))

# === Klassen-Mapping anwenden ===
cat(">> Wende Mapping an\n")

if (is.numeric(polygon_metadaten$Code_18)) {
  polygon_metadaten$Code_18 <- as.character(polygon_metadaten$Code_18)
}

# Mapping durchführen
polygon_metadaten <- polygon_metadaten %>%
  # Alte target_class entfernen falls vorhanden
  select(-any_of("target_class")) %>%
  left_join(class_map, by = "Code_18") %>%
  filter(!is.na(target_class))

cat("  Polygone nach Filterung:", nrow(polygon_metadaten), "\n")
cat("  Klassenverteilung (Anzahl Polygone):\n")
print(table(polygon_metadaten$target_class))
cat("\n")

# Raster-Projektion prüfen
if (!identical(crs(beschnittene_polygone), crs(raster_stack))) {
  cat(">> Projiziere Polygone auf Raster-CRS\n")
  beschnittene_polygone <- project(beschnittene_polygone, crs(raster_stack))
}

# === Pixelwerte extrahieren ===
cat(">> Extrahiere Pixelwerte (das kann dauern)...\n")

# Nur Polygone verwenden, die im Mapping sind
relevante_ids <- polygon_metadaten$polygon_id
# Wir müssen sicherstellen, dass die Geometrien und Metadaten synchron bleiben.
# Da terra::intersect die Reihenfolge ändern kann oder Geometrien splittet,
# ist der sicherste Weg, die IDs vor dem Extrahieren an die Geometrie zu heften.

# ID an Geometrie heften (Trick: Wir nutzen die row-number der beschnittenen Polygone)
# Achtung: beschnittene_polygone und polygon_metadaten kommen aus demselben Objekt 'beschnittene_polygone'
# Wir müssen filtern:
beschnittene_polygone$polygon_id <- paste0("corine_", 1:nrow(beschnittene_polygone))
gefilterte_polygone <- beschnittene_polygone[beschnittene_polygone$polygon_id %in% relevante_ids, ]

# Extraktion
extrahierte_werte <- terra::extract(raster_stack, gefilterte_polygone, xy = TRUE) %>%
  rename(temp_id = ID)

# Die ID von extract entspricht dem Index in gefilterte_polygone (1 bis N)
# Wir mappen das zurück zur echten polygon_id
id_map <- data.frame(
  temp_id = 1:nrow(gefilterte_polygone),
  polygon_id = gefilterte_polygone$polygon_id
)

trainings_samples <- extrahierte_werte %>%
  left_join(id_map, by = "temp_id") %>%
  left_join(polygon_metadaten %>% select(polygon_id, Code_18, target_class), by = "polygon_id") %>%
  select(-temp_id) %>%
  filter(!is.na(target_class)) %>%
  mutate(
    quell_jahr = "2018",
    quelle     = "CORINE_VarB"
  )

cat("  Extrahierte Samples gesamt:", nrow(trainings_samples), "\n\n")

# === Balanciertes Sampling ===
cat(">> Balanciertes Sampling (max 2000 pro Klasse)\n")
set.seed(42)
balancierte_samples <- list()
max_samples <- 2000

for (klasse in unique(trainings_samples$target_class)) {
  sub <- trainings_samples[trainings_samples$target_class == klasse, ]
  n_avail <- nrow(sub)
  n_select <- min(max_samples, n_avail)
  
  if (n_select > 0) {
    balancierte_samples[[klasse]] <- sub[sample(n_avail, n_select), ]
    cat("  ", klasse, ":", n_select, "\n")
  }
}

finaler_ds <- do.call(rbind, balancierte_samples)

# Umbenennen für Kompatibilität mit Trainings-Skripten
finaler_ds <- finaler_ds %>%
  rename(class_name = target_class) %>%
  mutate(class_id = as.numeric(factor(class_name)))

# Speichern
cat("\n>> Speichere:", basename(ausgabe_csv), "\n")
write.csv(finaler_ds, ausgabe_csv, row.names = FALSE)

cat("===========================================\n")
cat("FERTIG. Nächster Schritt: Modell trainieren (mit diesem CSV)\n")
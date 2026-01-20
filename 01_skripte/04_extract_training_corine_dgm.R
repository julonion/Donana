#!/usr/bin/env Rscript

# ===========================================
# Phase 1: Extraktion von Trainingsdaten aus CORINE
# Variante E – MIT DGM (spektrale + topographische Features)
# ===========================================

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tibble)
})

# === Parameter ===
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"
work_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B/Workflow_Variante_E_2026"

# Lade Pfade und Klassen
paths_file <- file.path(work_dir, "02_daten", "input", "workflow_paths.rds")
classes_file <- file.path(work_dir, "02_daten", "input", "variante_e_classes.rds")

if (!file.exists(paths_file) || !file.exists(classes_file)) {
  stop("Pfade oder Klassen nicht gefunden. Bitte zuerst '00_setup_workflow.R' ausführen!")
}

paths <- readRDS(paths_file)
variante_e_classes <- readRDS(classes_file)

# Eingabedateien
corine_polygone <- paths$corine_polygone
feature_stack <- paths$feature_stack_2018_dgm  # MIT DGM!
aoi_path <- paths$aoi

# Ausgabedatei
ausgabe_csv <- file.path(work_dir, "02_daten", "training", "samples_corine_2018_with_dgm_variante_e.csv")

dir.create(dirname(ausgabe_csv), showWarnings = FALSE, recursive = TRUE)

cat("===========================================\n")
cat("Extraktion von CORINE-Trainingsdaten\n")
cat("Variante E – MIT DGM (spektrale + topographische Features)\n")
cat("===========================================\n\n")

# === Variante E: Klasseneinteilung ===
class_map <- tribble(
  ~Code_18, ~target_class,
  
  # Gehölzvegetation (Wald + Buschland + Weideland)
  "311", "Gehölzvegetation",   # Laubwald
  "312", "Gehölzvegetation",   # Nadelwald
  "323", "Gehölzvegetation",   # Hartlaubvegetation
  "324", "Gehölzvegetation",   # Übergang Wald–Busch
  "231", "Gehölzvegetation",   # Weideland
  
  # Offener Boden / Sand
  "331", "Offener_Boden",      # Strände, Dünen, Sandflächen
  "333", "Offener_Boden",      # Spärlich bewachsene Flächen
  
  # Binnenmarschen
  "411", "Binnenmarschen",     # Binnenmarschen
  
  # Feuchtgebiete (salzbeeinflusst)
  "421", "Feuchtgebiet_salz",  # Salzmarschen
  "422", "Feuchtgebiet_salz",  # Salinen / Salzpfannen
  
  # Wasser
  "511", "Wasser",             # Fließgewässer
  "512", "Wasser"              # Binnengewässer
)

# === Prüfungen ===
if (!file.exists(corine_polygone)) {
  stop("CORINE-Polygone nicht gefunden: ", corine_polygone)
}
if (!file.exists(feature_stack)) {
  stop("Feature-Stack mit DGM nicht gefunden: ", feature_stack)
}

# Prüfe Features im Stack
stack_check <- rast(feature_stack)
cat("Feature-Stack:\n")
cat("  Anzahl Features:", nlyr(stack_check), "\n")
cat("  Features:", paste(names(stack_check), collapse = ", "), "\n\n")

# === Daten laden ===
cat(">> Lade CORINE-Polygone\n")
corine_vect <- vect(corine_polygone)
raster_stack <- rast(feature_stack)
grenz_vektor <- vect(aoi_path)

# Projektionen angleichen
if (!identical(crs(corine_vect), crs(grenz_vektor))) {
  grenz_vektor <- project(grenz_vektor, crs(corine_vect))
}

# Polygone auf Gebiet beschränken
beschnittene_polygone <- terra::intersect(corine_vect, grenz_vektor)

# Polygon-Metadaten vorbereiten
polygon_metadaten <- as.data.frame(beschnittene_polygone) %>%
  mutate(polygon_id = paste0("corine_", row_number()))

# === Klassen-Mapping anwenden ===
cat(">> Wende Klassen-Mapping an (Variante E)\n")
if (is.numeric(polygon_metadaten$Code_18)) {
  polygon_metadaten$Code_18 <- as.character(polygon_metadaten$Code_18)
}
if ("target_class" %in% names(polygon_metadaten)) {
  polygon_metadaten <- polygon_metadaten %>%
    select(-target_class)
}
polygon_metadaten <- polygon_metadaten %>%
  left_join(class_map, by = "Code_18") %>%
  filter(!is.na(target_class))

cat("  Polygone nach Mapping:", nrow(polygon_metadaten), "\n")
cat("  Klassenverteilung:\n")
print(table(polygon_metadaten$target_class))
cat("\n")

# Projektion des Raster-Stacks anpassen falls nötig
if (!identical(crs(beschnittene_polygone), crs(raster_stack))) {
  cat(">> Projiziere Polygone auf Raster-CRS\n")
  beschnittene_polygone <- project(beschnittene_polygone, crs(raster_stack))
  polygon_metadaten <- polygon_metadaten %>%
    mutate(polygon_id = paste0("corine_", row_number()))
}

# Pixelwerte extrahieren
cat(">> Extrahiere Pixelwerte...\n")
extrahierte_werte <- terra::extract(raster_stack, beschnittene_polygone, xy = TRUE) %>%
  rename(temp_polygon_id = ID) %>%
  mutate(polygon_id = paste0("corine_", temp_polygon_id)) %>%
  select(-temp_polygon_id)

# Metadaten mit Pixelwerten verknüpfen
trainings_samples <- extrahierte_werte %>%
  left_join(polygon_metadaten %>% select(polygon_id, Code_18, target_class, area_km2), by = "polygon_id") %>%
  filter(!is.na(target_class)) %>%
  mutate(
    quell_jahr = "2018",
    quelle = "CORINE"
  )

cat("  Extrahierte Samples:", nrow(trainings_samples), "\n\n")

# Klassenverteilung prüfen
cat(">> Klassenverteilung in extrahierten Samples:\n")
klassen_verteilung <- table(trainings_samples$target_class)
print(klassen_verteilung)
cat("\n")

# Balanciertes Sampling
cat(">> Balanciertes Sampling\n")
set.seed(42)
balancierte_samples <- list()

eindeutige_klassen <- unique(trainings_samples$target_class)
max_samples_pro_klasse <- 2000

for(landbedeckungs_klasse in eindeutige_klassen) {
  klassen_subset <- trainings_samples[trainings_samples$target_class == landbedeckungs_klasse, ]
  verfuegbare_samples <- nrow(klassen_subset)
  samples_auswaehlen <- min(max_samples_pro_klasse, verfuegbare_samples)
  
  if(samples_auswaehlen > 0) {
    ausgewaehlte_samples <- klassen_subset[sample(nrow(klassen_subset), samples_auswaehlen), ]
    balancierte_samples[[landbedeckungs_klasse]] <- ausgewaehlte_samples
    cat("  Klasse", landbedeckungs_klasse, ":", samples_auswaehlen, "von", verfuegbare_samples, "Samples\n")
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

# Statistiken erstellen
cat("===========================================\n")
cat("Zusammenfassung\n")
cat("===========================================\n")
cat("Gesamtanzahl Trainingssamples:", nrow(finaler_trainingsdatensatz), "\n")
cat("\nVerteilung pro Klasse:\n")
klassen_summen <- table(finaler_trainingsdatensatz$class_name)
print(klassen_summen)
cat("\n")

cat("Nächster Schritt: RF/XGBoost-Modell mit DGM trainieren\n")


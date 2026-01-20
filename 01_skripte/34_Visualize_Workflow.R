#!/usr/bin/env Rscript

# ===========================================
# Phase 6: Workflow-Diagramm erstellen
# Mermaid-Diagramm für Präsentation
# ===========================================

# === Parameter ===
root_dir <- "/Users/julianhoen/BHT/WiSe_25_26/Umweltprj_B"
work_dir <- file.path(root_dir, "Workflow_Variante_E_2026")

output_dir <- file.path(work_dir, "03_ergebnisse", "visualisierungen")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

mermaid_file <- file.path(output_dir, "workflow_diagramm_variante_e.mmd")
mermaid_simple_file <- file.path(output_dir, "workflow_diagramm_simple.mmd")

cat("===========================================\n")
cat("Workflow-Diagramm erstellen\n")
cat("Variante E\n")
cat("===========================================\n\n")

# === Detailliertes Mermaid-Diagramm ===
mermaid_detailed <- "
graph TB
    subgraph Phase0[Phase 0: Vorbereitung]
        FQ[Forschungsfrage definieren]
        Setup[Ordnerstruktur & Pfade]
    end
    
    subgraph Phase1[Phase 1: Datenaufbereitung]
        Check[Daten prüfen<br/>Sentinel-2, CORINE, DGM, CLCplus]
        FE[Feature Engineering<br/>10 spektral + 5 DGM]
        Extract1[Trainingsdaten<br/>CORINE only<br/>10 Features]
        Extract2[Trainingsdaten<br/>CORINE + DGM<br/>15 Features]
        Split[Train/Test Split<br/>80/20]
    end
    
    subgraph Phase2[Phase 2: Modelltraining]
        RF1[RF CORINE only<br/>10 Features]
        XGB1[XGBoost CORINE only<br/>10 Features]
        RF2[RF CORINE + DGM<br/>15 Features]
        XGB2[XGBoost CORINE + DGM<br/>15 Features]
        CV[Optional: k-fold CV<br/>5-fold]
    end
    
    subgraph Phase3[Phase 3: Externe Validierung]
        ExtVal[Validierung gegen CLCplus<br/>Alle 5 Klassen]
        MV[Optional: Majority Voting<br/>Ensemble]
    end
    
    subgraph Phase4[Phase 4: Vergleich & Analyse]
        Comp[Modellvergleich<br/>RF vs. XGBoost<br/>mit vs. ohne DGM]
        ANOVA[ANOVA für Features<br/>Signifikanz-Tests]
        FI[Feature Importance<br/>RF/XGBoost]
    end
    
    subgraph Phase5[Phase 5-7: Interpretation]
        Interp[Interpretation<br/>Ergebnisse & Limitationen]
        Doc[Dokumentation]
    end
    
    FQ --> Setup
    Setup --> Check
    Check --> FE
    FE --> Extract1
    FE --> Extract2
    Extract1 --> Split
    Extract2 --> Split
    
    Split --> RF1
    Split --> XGB1
    Split --> RF2
    Split --> XGB2
    
    RF1 --> CV
    XGB1 --> CV
    RF2 --> CV
    XGB2 --> CV
    
    RF1 --> ExtVal
    XGB1 --> ExtVal
    RF2 --> ExtVal
    XGB2 --> ExtVal
    
    ExtVal --> MV
    MV --> Comp
    Comp --> ANOVA
    Comp --> FI
    ANOVA --> Interp
    FI --> Interp
    Interp --> Doc
    
    style Phase0 fill:#e3f2fd
    style Phase1 fill:#fff3e0
    style Phase2 fill:#f3e5f5
    style Phase3 fill:#e8f5e9
    style Phase4 fill:#fff9c4
    style Phase5 fill:#c8e6c9
"

# === Vereinfachtes Mermaid-Diagramm ===
mermaid_simple <- "
flowchart TD
    Start[Forschungsfrage:<br/>Robustheit & Generalisierbarkeit<br/>RF vs. XGBoost, mit/ohne DGM] --> Data[Datenaufbereitung<br/>Sentinel-2 + CORINE + DGM<br/>Feature Engineering]
    
    Data --> Models[4 Modelle trainieren<br/>RF/XGBoost × CORINE only/DGM<br/>Train/Test 80/20]
    
    Models --> IntVal[Interne Validierung<br/>Accuracy, Kappa, F1<br/>Optional: k-fold CV]
    
    Models --> ExtVal[Externe Validierung<br/>CLCplus<br/>Alle 5 Klassen]
    
    IntVal --> Compare[Vergleich & Analyse<br/>Modellvergleich<br/>Feature-Analyse ANOVA]
    ExtVal --> Compare
    
    Compare --> Ensemble[Optional: Majority Voting<br/>Ensemble-Methode]
    Ensemble --> Results[Ergebnisse & Interpretation<br/>Transparente Dokumentation]
    
    style Start fill:#e3f2fd
    style Data fill:#fff3e0
    style Models fill:#f3e5f5
    style IntVal fill:#e8f5e9
    style ExtVal fill:#e8f5e9
    style Compare fill:#fff9c4
    style Ensemble fill:#c8e6c9
    style Results fill:#c8e6c9
"

# === Speichere Diagramme ===
cat(">> Speichere Mermaid-Diagramme\n")
writeLines(mermaid_detailed, mermaid_file)
writeLines(mermaid_simple, mermaid_simple_file)

cat("  Detailliertes Diagramm:", basename(mermaid_file), "\n")
cat("  Vereinfachtes Diagramm:", basename(mermaid_simple_file), "\n\n")

cat("===========================================\n")
cat("WORKFLOW-DIAGRAMM ERSTELLT\n")
cat("===========================================\n")
cat("\nDie Diagramme können in Mermaid-Editoren visualisiert werden:\n")
cat("  - https://mermaid.live/\n")
cat("  - VS Code mit Mermaid Extension\n")
cat("  - R Markdown mit mermaid() Funktion\n\n")

cat("Dateien gespeichert in:", output_dir, "\n")


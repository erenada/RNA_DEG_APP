# RNA-seq DEG Analysis App - Documentation

Author: Eren Ada, PhD  
Version: 1.0  
Last Updated: October 21, 2025

---

## What’s Here

Concise guides for using the app, organized by workflow. Each guide includes essential UI, behavior, workflow, and quick troubleshooting. Use the Quick Start if you’re new.

---

## Start Here

- Quick Start Guide: [documentation/quick_start_guide.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/quick_start_guide.md)
- Data formats: see Count Matrix and Metadata examples in the Quick Start

---

## Tab Guides (in order)

1) Data Input: [documentation/01_data_input_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/01_data_input_tab.md)
2) Experimental Design: [documentation/02_experimental_design_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/02_experimental_design_tab.md)
3) DESeq2 Configuration: [documentation/03_analysis_configuration_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/03_analysis_configuration_tab.md)
4) Results: [documentation/04_results_display_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/04_results_display_tab.md)
5) Enrichment: [documentation/05_enrichment_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/05_enrichment_tab.md)
6) Volcano Plot: [documentation/06_volcano_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/06_volcano_tab.md)

---

## Common Tasks → Where to Look

| Task | Documentation |
|------|----------------|
| Upload count matrix and metadata | Data Input → Workflow |
| Fix validation errors | Data Input → Troubleshooting |
| Build contrasts (groups, covariates) | Experimental Design |
| Configure DESeq2 and run | DESeq2 Configuration |
| View and export results (per contrast) | Results |
| Run GO enrichment (BP/MF/CC) | Enrichment |
| Create and export volcano plots | Volcano Plot |

---

## File Map (for reference)

| Module | UI | Server | Utils |
|--------|----|--------|-------|
| Data Input | `ui/tab_input.R` | `server_modules/server_tab1_data_input.R` | `R/utils_validation.R` |
| Experimental Design | `ui/tab_design.R` | `server_modules/server_tab2_experimental_design.R` | - |
| DESeq2 Config | `ui/tab_config.R` | `server_modules/server_tab3_analysis_config.R` | `R/utils_deseq2.R` |
| Results | `ui/tab_results.R` | `server_modules/server_tab4_results_display.R` | `R/utils_deseq2.R` |
| Enrichment | `ui/tab_enrichment.R` | `server_modules/server_tab5_enrichment.R` | `R/utils_enrichment.R` |
| Volcano | `ui/tab_volcano.R` | `server_modules/server_tab6_volcano.R` | `R/utils_visualization.R` |

---

## Notes

- Use the “Next” buttons within the app to follow the workflow.
- Example datasets are in `example_data/`.
- Exports are written both to your browser and to `results/` where applicable.

---

End.


# RNA-seq Differential Gene Expression Analysis App (Shiny)

Author: Eren Ada, PhD  
Last Updated: October 21, 2025  
Version: 1.0

---

## Overview

A Shiny app that guides you through RNA-seq differential expression with DESeq2: upload data, build contrasts (with optional covariates), run analysis, review/export results, perform GO enrichment, and create publication-ready volcano plots. You can also use pre-computed DEG CSVs for Enrichment and Volcano without running DESeq2 in-app.

---

## Requirements

- R >= 4.1.0
- Use the installer to fetch CRAN/Bioconductor packages

```r
source("install_packages.R")
# optional
source("check_packages.R")
```

---

## Installation & Run

```bash
git clone https://github.com/erenada/RNA_DEG_APP.git
cd RNA_DEG_APP
```

```r
# Install dependencies once
source("install_packages.R")

# Launch the app
shiny::runApp()
# or
source("app.R")
```

---

## Quick Start (Essentials)

1) Data Input: upload count matrix and metadata; select organism and ID type; pass validation  
2) Experimental Design: drag samples to Numerator/Denominator; optionally add covariates; create contrasts  
3) DESeq2 Configuration: choose test (Wald/LRT), alpha, optional shrinkage; run analysis  
4) Results: browse per-contrast tables; export all/significant genes  
5) Enrichment (optional): run GO BP/MF/CC on significant genes; export tables and gene lists  
6) Volcano: customize thresholds/labels/colors; export PDF/PNG and parameters

- Example datasets: `example_data/` (toy and example CSVs)
- Outputs: written to browser and saved under `results/` where applicable

---

## Documentation

- Documentation index: [documentation/README.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/README.md)
- Quick Start Guide: [documentation/quick_start_guide.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/quick_start_guide.md)
- Tab Guides (in order):
  - Data Input: [01_data_input_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/01_data_input_tab.md)
  - Experimental Design: [02_experimental_design_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/02_experimental_design_tab.md)
  - DESeq2 Configuration: [03_analysis_configuration_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/03_analysis_configuration_tab.md)
  - Results: [04_results_display_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/04_results_display_tab.md)
  - Enrichment: [05_enrichment_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/05_enrichment_tab.md)
  - Volcano Plot: [06_volcano_tab.md](https://github.com/erenada/RNA_DEG_APP/blob/main/documentation/06_volcano_tab.md)

---

## Notes

- Organism databases (`org.Mm.eg.db`, `org.Hs.eg.db`) are loaded on demand from the Data Input tab.
- Pre-computed DEG CSVs can be used directly in Enrichment and Volcano tabs.

---

## License

Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (see `LICENSE`).

---

## Contact / Support

Author: Eren Ada, PhD  
Harvard Medical School, Department of Immunology  
GitHub: @erenada

For questions:
- Using the app: see the tab guides and Quick Start
- Bugs/requests: open an issue on GitHub
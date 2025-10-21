# Data Input Tab

Author: Eren Ada, PhD  
Last Updated: October 21, 2025  
Module Files: `ui/tab_input.R`, `server_modules/server_tab1_data_input.R`

---

## Purpose

Upload count matrix and metadata, configure organism and gene ID type, validate compatibility, and proceed to design.

---

## Inputs and Controls

- Count Matrix: `fileInput('count_file')`
  - Format: CSV/TSV/TXT; separator `count_sep`; `count_header`; `count_rownames`
  - Expected: genes as rows (row names), samples as columns (numeric)
- Metadata: `fileInput('meta_file')`
  - Format: CSV/TSV/TXT; separator `meta_sep`; `meta_header`; `meta_rownames`
  - Expected: samples as rows (row names), variables as columns
- Project Settings
  - Organism: `radioButtons('organism', choices = c('Mouse','Human'))`
    - Loads `org.Mm.eg.db` or `org.Hs.eg.db` via `get_orgdb(organism)` (in `global.R`)
  - Gene ID Type: `selectInput('id_type', choices = c('auto','SYMBOL','ENSEMBL'))`

---

## Behavior (Server)

- File Reads
  - Count: `read.csv` or `read.delim` based on extension and user separator
  - Metadata: same logic
  - After either file loads, `validate_data()` runs if both present
- Project Settings
  - `observeEvent(input$organism)`: loads OrgDb, stores in `values$orgdb`, clears enrichment cache
  - `observeEvent(input$id_type)`: stores ID type, clears enrichment cache
- UI Outputs
  - `count_preview`, `meta_preview`: DT tables (scrollable)
  - `data_summary`: dimensions and variable previews
  - `validation_status`: info/success/error box
  - `next_button`: rendered only when validation passes; navigates to `tab_design`

---

## Validation

Implemented in `R/utils_validation.R` and called via `validate_input_data(count, metadata)`.

- Count Matrix (errors)
  - No rows/columns; <2 samples; non-numeric columns; negative values
- Count Matrix (warnings)
  - Non-integer values; missing values; >50% genes max < 10
- Metadata (errors)
  - No rows/columns; missing/duplicate sample row names
- Metadata (warnings)
  - Missing values; single-level factors; all-unique factors
- Sample Matching (errors)
  - Missing names; 0 matches; <2 matches
- Sample Matching (warnings)
  - Samples only in counts; samples only in metadata (lists truncated)

Return shape:
```r
list(valid = TRUE/FALSE, errors = character(), warnings = character(), n_matched = integer)
```

---

## Workflow

1) Upload count matrix (set separator, header, row names if needed)
2) Upload metadata (match settings; sample IDs as row names)
3) Select organism and (optionally) gene ID type
4) Review validation status and preview tables
5) Click "Next: Experimental Design" when validation passes

---

## Troubleshooting (Quick)

- No matching samples found
  - Ensure count column names EXACTLY match metadata row names (case/whitespace)
- Non-numeric columns in count matrix
  - Remove non-count columns or set them as row names before upload
- Negative counts
  - Fix upstream data; negative values are invalid
- Non-integer counts warning
  - Expected for some preprocessing; proceed if intended
- File parsing issues
  - Check separator; ensure header and row-name options are correct; use UTF-8 CSV
- Organism database load error
  - Install required package:
    ```r
    BiocManager::install('org.Mm.eg.db')  # Mouse
    BiocManager::install('org.Hs.eg.db')  # Human
    ```

---

## Minimal References

- UI definitions: `ui/tab_input.R`
- Server logic: `server_modules/server_tab1_data_input.R`
- Validation utilities: `R/utils_validation.R`
- Organism loader: `global.R:get_orgdb()`

---

End.


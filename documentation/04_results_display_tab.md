# Results Display Tab

Author: Eren Ada, PhD  
Last Updated: October 21, 2025  
Module Files: `ui/tab_results.R`, `server_modules/server_tab4_results_display.R`

---

## Purpose

Present DESeq2 per-contrast results as searchable/sortable tables, enable quick filtering (significant vs all), downloads, and navigation to enrichment.

---

## Prerequisites

- `values$deseq_results` populated by the Analysis Configuration tab
- At least one contrast analyzed

---

## UI Overview

- Results availability gate: shows "No Analysis Results Yet" until results exist
- Dynamic contrast tabs: one tab per contrast name
- Each tab includes:
  - Summary panel (contrast groups, total genes, significant, up, down)
  - "Proceed to Enrichment" button (preselects the contrast on Enrichment tab)
  - Display toggle: significant-only vs all genes
  - Download buttons: CSV/Excel for all or significant genes
  - Results table (DT): sortable, filter row, buttons for copy/csv/excel

---

## Behavior (Server)

- Availability: `output$results_available` = TRUE if `values$deseq_results` exists and non-empty
- Dynamic UI: builds a tab for each contrast in `values$deseq_results`
- Summary: reads `values$analysis_summary[[contrast]]` to display counts
- Direction classification used across table and exports:
  - If `padj` exists: significant if `padj < alpha` (from `values$overall_summary`) and, if `lfc_threshold > 0`, also `|log2FC| > threshold`
  - Else (no `padj` column): classify by `log2FC` and `lfc_threshold` only
  - direction ∈ {Up, Down, NS}
- Display toggle:
  - Significant-only: rows where direction != NS
  - All genes: no filter
- Exports:
  - All genes and Significant-only, CSV and Excel (openxlsx)
  - Columns: gene, direction, baseMean, log2FoldChange, lfcSE, plus available {stat, pvalue, padj, contrast}
- Enrichment navigation:
  - Button updates main tab to `tab_enrichment` and preselects current contrast via `updateSelectizeInput('enrich_contrast', selected = contrast_name)`

---

## Workflow

1) Open Results tab after analysis completes
2) Switch contrast tabs to explore different comparisons
3) Use Display toggle to view significant genes only or all genes
4) Sort/filter/search in the table as needed
5) Export all or significant subsets (CSV/Excel)
6) Click "Proceed to Enrichment" to analyze GO terms for this contrast

---

## Troubleshooting (Quick)

- No Analysis Results Yet
  - Run analysis in the DESeq2 Configuration tab; ensure at least one contrast
- No padj column
  - Some shrinkage/paths may omit stat; padj can still be absent if upstream results lacked it. Direction falls back to threshold-based classification
- Zero significant genes shown
  - Increase alpha (e.g., 0.1) or reduce LFC threshold; verify design and sample sizes
- Exports missing columns
  - Only columns available in the underlying results are exported (stat/pvalue/padj may not exist)

---

End.



# Volcano Plot Tab

Author: Eren Ada, PhD  
Last Updated: October 21, 2025  
Module Files: `ui/tab_volcano.R`, `server_modules/server_tab6_volcano.R`, `R/utils_visualization.R`

---

## Purpose

Generate publication-ready volcano plots from DE results (computed or uploaded), with robust defaults, labeling, highlighting, caching, exports, and a synchronized gene table.

---

## Prerequisites

- Contrast selected OR uploaded volcano CSV present
- Computed source: `values$deseq_results[[contrast]]` (shrunk and/or unshrunken)
- Uploaded source: `values$uploaded_volcano_results[[contrast]]` with columns `gene, log2FoldChange, pvalue, padj, contrast`
- Availability gate: `output$volcano_available` requires contrast + data

---

## UI Overview (Key Controls)

- Contrast selector (computed and uploaded combined)
- Optional CSV upload (with clear button and status)
- Basic thresholds: adjusted p-value and |log2FC| cutoffs, top-N labels, use shrunk LFC
- Title and alignment
- Advanced: appearance (size/shape/alpha/colors), grid lines, labeling options (boxed/connectors, custom list, only custom), subtitle/caption (with %), threshold lines, fixed axes for multi-plot comparability, legend, count annotation, export dimensions (PDF/PNG/DPI)
- Outputs: data source banner, counts summary, plot, downloads (PDF/PNG/JSON parameters), and gene table with CSV export

---

## Data Source Resolution (`R/utils_visualization.R`)

`get_volcano_data_source(contrast, use_shrunk, values)`
- Priority 1: uploaded (`uploaded_volcano_results`)
- Priority 2: computed shrunk results if requested and available
- Priority 3: computed unshrunken results
- Returns data frame and `source_type` (uploaded/computed_shrunk/computed_unshrunk)

---

## Plot Generation (`generate_volcano_plot`)

- Package: EnhancedVolcano (required)
- Columns required: `gene, log2FoldChange, pvalue, padj`
- Y-axis selection: use `padj` by default; if all `padj` are NA, use `pvalue` (caption notes the fallback)
- Significance masks: consistent with chosen Y-axis (`padj_cutoff` with padj or pvalue)
- Label ranking: `-log10(Y) * |log2FC|` using the same Y as plotted
- Labeling: top-N per direction; optional custom labels; optional only-custom mode
- Colors: Up/Down/NS configurable; additionally highlight table-selected genes with a distinct color (legend unchanged)
- Threshold lines: shown/hidden with style control
- Axis limits: auto or fixed for comparing contrasts
- Title/subtitle/caption alignment; optional counts box on plot
- Returns `plot` and `stats` (up/down/total/sig/labeled, etc.)

---

## Server Behavior (`server_tab6_volcano.R`)

- Availability depends on selected contrast and data presence
- Upload handler: validates columns/types; stores by contrast; updates selector; success/empty-state notifications
- Selector population merges computed and uploaded contrast names
- Data source info banner reflects uploaded vs computed (shrunk/unshrunk per toggle)
- Persistent selected genes from table (`values$volcano_selected_genes`) keeps highlights across re-renders
- Plot reactive: builds params list, parses forced labels, merges with selected genes, caches plot by MD5 of contrast+params (`generate_cache_key_volcano`)
- Downloads:
  - Plot PDF/PNG (also saved under `results/volcano/<contrast>/`)
  - Parameters JSON (includes source type, thresholds, labeling, aesthetics, axes, export)

---

## Gene Table

- Source: same as plot (shrunk/unshrunk/uploaded)
- Adds `regulation` (Up/Down/NS) using current `padj_cutoff` and `lfc_cutoff`
- Display: significant-only or all; sortable/searchable; formatting for p/padj and LFC
- Selection persists and highlights selected genes on the plot
- CSV export reflects current filtering and sorting

---

## Troubleshooting (Quick)

- No plot available: select contrast; ensure results exist or upload CSV
- All padj are NA: plot falls back to p-values (caption notes it)
- No significant genes: relax cutoffs or verify analysis parameters/design
- Custom labels not shown: ensure names match `gene` column exactly (case-sensitive)
- Fixed axes look odd: set reasonable ranges or disable fix

---

End.

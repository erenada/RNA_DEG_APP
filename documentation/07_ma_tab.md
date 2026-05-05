# MA Plot Tab

Author: Eren Ada, PhD  
Last Updated: 05/05/2026  
Module Files: `ui/tab_ma.R`, `server_modules/server_tab7_ma.R`, `R/utils_visualization.R`

---

## Purpose

Generate publication-ready MA plots from DE results (computed or uploaded), with the same styling, labeling, caching, export, and gene table behavior as the Volcano tab, plus two MA-specific controls (x-axis transform of `baseMean` and a zero log2FC reference line).

---

## Prerequisites

- Contrast selected OR uploaded MA CSV present
- Computed source: `values$deseq_results[[contrast]]` (shrunk and/or unshrunken results)
- Uploaded source: `values$uploaded_ma_results[[contrast]]` with columns `gene, baseMean, log2FoldChange, pvalue, padj, contrast`
- Availability gate: `output$ma_available` requires contrast + data
- The CSV upload validator additionally requires a non-negative numeric `baseMean` column (this is the only schema difference from the Volcano upload format)

---

## Axes

- **Y-axis**: `log2FoldChange` (shrunk if available and the toggle is on; otherwise unshrunken)
- **X-axis**: a log-transformed baseMean. Selectable via `ma_x_transform`:
  - `log10(baseMean + 1)` (default)
  - `log2(baseMean + 1)`

---

## UI Overview (Key Controls)

- Contrast selector (computed and uploaded combined)
- Optional CSV upload (with clear button and status panel)
- Basic thresholds: adjusted p-value cutoff, |log2FC| cutoff, top-N labels per direction, "Use shrunk Log2FC (if available)"
- Title and alignment
- Advanced options:
  - Plot Appearance: point size/shape/alpha, colors (Up/Down/NS/Selected), x-axis transform, major/minor grid lines
  - Gene Labeling: label size, max overlaps, boxed labels, draw connectors (with width), custom force-labels list, "only custom labels" mode
  - Text Elements: subtitle (custom or auto, with optional percent significant), caption (custom or auto), alignment for both
  - Thresholds & Axes: zero log2FC reference line, log2FC threshold lines (with line style), fixed axis limits for cross-contrast comparability
  - Legend & Annotations: legend position, "Show gene counts on plot"
  - Export Settings: width / height / DPI for PDF and PNG
- Outputs: data source banner, counts summary card, plot, downloads (PDF / PNG / parameters JSON), and a gene table with CSV export

---

## Data Source Resolution (`R/utils_visualization.R`)

`get_ma_data_source(contrast, use_shrunk, values)`

- Priority 1: uploaded (`values$uploaded_ma_results`)
- Priority 2: computed shrunk results when `use_shrunk = TRUE` and `values$deseq_results[[contrast]]$results` is available
- Priority 3: computed unshrunken results (`values$deseq_results[[contrast]]$unshrunken_results`)
- Returns a list with `data`, `source_type` (`uploaded` / `computed_shrunk` / `computed_unshrunk`), and `shrinkage_applied`

---

## Plot Generation (`generate_ma_plot`)

- Backend: `ggplot2` + `ggrepel`
- Required columns on input: `gene, baseMean, log2FoldChange, pvalue, padj`
- Significance masks use `padj_cutoff` and `lfc_cutoff` against `padj`
- Label ranking: `-log10(padj) * |log2FC|` (per direction, top-N each)
- Custom labels: parsed from `ma_force_labels` via `parse_force_labels()`, optionally exclusive when `ma_custom_labels_only` is on
- Selected genes from the table are merged with custom labels and highlighted in `ma_color_selected`
- Reference lines: zero log2FC (`ma_show_zero_line`) and ±lfc_cutoff threshold lines with selectable style (`ma_threshold_style`)
- Axis limits: auto, or fixed via `ma_xlim_min/max` and `ma_ylim_min/max`
- Title / subtitle / caption alignment; optional on-plot count annotation (`ma_show_counts_on_plot`)
- Returns a list with `plot` and `stats` (n_up, n_down, n_total, n_sig, n_labeled_up, n_labeled_down, n_force_labeled, n_force_found, all_padj_na, etc.)

---

## Server Behavior (`server_tab7_ma.R`)

- Availability depends on selected contrast and data presence
- Upload handler: validates required columns, numeric types, and non-negative `baseMean`; splits the CSV by `contrast` into `values$uploaded_ma_results`; updates the contrast selector and auto-selects the first uploaded contrast
- Selector population merges computed and uploaded contrast names
- Data source banner reflects uploaded vs computed (shrunk / unshrunken per toggle)
- Persistent selected genes from the table (`values$ma_selected_genes`) keep highlights across re-renders; selections reset when the contrast changes
- Plot reactive: builds a params list, parses forced labels, merges with selected genes, caches the plot by MD5 of `contrast + params` via `generate_cache_key_ma()`
- Downloads:
  - PDF and PNG via `ggsave()` (also written under `results/ma/<contrast>/`)
  - Parameters JSON (data source, thresholds, labeling, aesthetics, x_transform, threshold lines, axes, export settings)

---

## Gene Table

- Source: same as plot (shrunk / unshrunken / uploaded)
- Adds a `regulation` column (Up / Down / NS) using current `padj_cutoff` and `lfc_cutoff`
- Display: significant only or all; sortable / searchable; formatting for `baseMean`, `log2FoldChange`, `lfcSE`, p / padj
- Selection persists and highlights the corresponding points on the plot
- CSV export reflects the current filtering and sorting

---

## Troubleshooting (Quick)

- No plot available: select a contrast, or upload a CSV containing `baseMean`
- Upload rejected: confirm required columns `gene, baseMean, log2FoldChange, pvalue, padj, contrast` are present and numeric where required, and that `baseMean` is non-negative
- All padj are NA: counts summary shows a warning; threshold-based significance becomes empty until you supply padj
- Custom labels not shown: ensure names match the `gene` column exactly (case-sensitive)
- Plot cluttered with labels: lower top-N labels per direction, or enable "Show only custom labels"
- Hard to compare across contrasts: enable Fix axis limits and pick consistent ranges

---

End.

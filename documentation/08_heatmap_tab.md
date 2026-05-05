# Heatmap Tab

Author: Eren Ada, PhD  
Last Updated: 05/05/2026  
Module Files: `ui/tab_heatmap.R`, `server_modules/server_tab8_heatmap.R`, `R/utils_heatmap.R`

---

## Purpose

Render publication-ready heatmaps in two complementary modes from a single tab, sharing one set of controls for gene selection, clustering, color, and export:

- **Expression (genes x samples)**: per-sample expression values for one contrast, drawn from the DESeq2 `dds` object via VST / rlog / log2(normalized + 1).
- **Cross-contrast LFC (genes x contrasts)**: per-contrast log2 fold changes for many contrasts side by side, drawn from pipeline results or uploaded CSVs.

Backend: `pheatmap` (with `RColorBrewer` palettes).

---

## Mode Comparison (at a glance)

| Aspect | Expression mode | LFC mode |
|--------|------------------|----------|
| Cell value | Transformed expression (VST / rlog / log2norm) | log2 fold change |
| Columns | Samples | Contrasts |
| Data source | Pipeline only (needs `dds`) | Pipeline AND/OR uploaded CSVs |
| Scale option | None / Z-score by row / Z-score by column | Forced to "none" (cells are log-ratios) |
| Color breaks | Auto from data range | Symmetric around 0 |
| Column annotations | Yes (from `colData(dds)`) | Not applicable |

---

## Prerequisites

- **Expression mode**: `values$deseq_results[[contrast]]$dds` and `$results` must be present (run the analysis first; uploaded CSVs do not provide a `dds`)
- **LFC mode**: at least one selected contrast available either in `values$deseq_results` (pipeline) or in `values$uploaded_heatmap_results` (uploaded)
- Uploaded CSV schema for LFC mode: columns `gene, log2FoldChange, padj, contrast` (numeric where required)
- Availability gate: `output$heatmap_available` checks the selected mode plus required data

---

## UI Overview (Key Controls)

- Mode selector (`heatmap_mode`): Expression vs Cross-contrast LFC
- Contrast selector:
  - Expression mode: single-select `heatmap_contrast` (pipeline contrasts only)
  - LFC mode: multi-select `heatmap_contrasts` (pipeline + uploaded)
- Optional CSV upload (LFC mode only) with clear button and status panel
- Gene selection:
  - Method (`heatmap_gene_method`): `Top N by padj`, `Top N by |Log2FC|`, `Threshold (padj/LFC)`, `Custom gene list`
  - Inputs (shown conditionally on method): top N, padj cutoff, |LFC| cutoff, threshold quantifier (LFC mode only: ANY vs ALL selected contrasts), paste box and file upload for custom lists
- Sample scope (Expression mode): "Samples in selected contrast" or "All samples in dds"
- Custom title and alignment
- Advanced options (toggle):
  - Layout & Transformation: transformation (VST / rlog / log2norm) for Expression, scaling (none / row / column) for Expression, cluster rows / cluster cols, distance (Euclidean / Correlation / Manhattan), linkage (Complete / Ward.D2 / Average / Single), tree heights
  - Cell Display & Labels: show row/col names, font sizes, cell width / height, NA color
  - Color Scale & Range: palette (RdBu, RdYlBu, Spectral, Viridis, Plasma), reverse palette, manual zlim min / max (NA = auto; LFC mode auto-defaults to a symmetric range around zero)
  - Legend & Annotations: show main legend; show annotation legend (Expression); annotation factors picker (`heatmap_anno_factors`, populated from eligible metadata columns)
  - Export Settings: width / height / DPI for PDF and PNG
- Outputs: data source banner, plot, summary panel (genes shown, dim, mode, warnings), downloads (PDF / PNG / parameters JSON), and a matrix preview table with CSV export

---

## Data Source Resolution (`R/utils_heatmap.R`)

### Expression mode

- `compute_transformed_matrix(dds, transform)` returns a genes x samples matrix using `DESeq2::vst()`, `DESeq2::rlog()`, or `log2(counts(dds, normalized = TRUE) + 1)`.
- `resolve_heatmap_samples(dds, contrast_info, sample_scope)` chooses the displayed columns: only the two contrast groups, or all samples in the `dds`.
- Server-side caches: `values$vst_cache` and `values$rlog_cache` (per contrast), so transformations are computed once.

### LFC mode

- `get_heatmap_lfc_matrices(contrasts, values)` builds two aligned matrices (genes x contrasts):
  - `lfc_matrix`: cells = `log2FoldChange`
  - `padj_matrix`: cells = `padj` (used only for gene selection)
  - Source priority per contrast: uploaded if present, otherwise pipeline; missing contrasts return NA columns.

---

## Gene Selection (`select_heatmap_genes`)

Four methods, applied identically across modes (with mode-specific inputs):

- **Top N by padj**:
  - Expression: smallest padj in the selected contrast's results
  - LFC: smallest minimum padj across the selected contrasts
- **Top N by |Log2FC|**:
  - Expression: largest |log2FC| in the selected contrast's results
  - LFC: largest maximum |log2FC| across the selected contrasts
- **Threshold (padj/LFC)**: keep genes that pass `padj < cutoff` and `|log2FC| > cutoff`
  - LFC adds the quantifier `heatmap_threshold_quantifier`: `any` (in at least one contrast) or `all` (in every selected contrast)
- **Custom gene list**: parsed by `parse_heatmap_gene_list()` from the paste box and/or an uploaded `.txt` or `.csv` (first column). Splits on commas, semicolons, whitespace, and newlines; intersected with available genes.

---

## Annotations (Expression mode)

- `eligible_annotation_columns(dds)` filters `colData(dds)` to columns whose unique-value count is between 2 and `n_samples - 1` (excludes single-value columns and per-sample IDs).
- `build_heatmap_annotations(dds, factor_cols)` returns a data frame with rownames matching `colnames(dds)`, ready for `pheatmap`'s `annotation_col`. Character / logical columns are coerced to factors.
- Annotation tracks render only when `heatmap_show_anno = TRUE` and at least one factor is selected.

---

## Plot Generation

- `generate_heatmap_expression(expr_matrix, gene_set, params, annotation_df = NULL)` and `generate_heatmap_lfc(lfc_matrix, gene_set, params)` both call `pheatmap::pheatmap(..., silent = TRUE)` and return a list with `plot` (the `pheatmap` object), `matrix` (the matrix actually plotted), `n_genes`, and `warnings`.
- Robustness:
  - All-NA rows are dropped via `.drop_problematic_rows()`. For row Z-scoring, rows with zero variance are also dropped (otherwise scaling would yield NaN).
  - Clustering is auto-disabled when fewer than 2 rows / cols remain after filtering.
  - LFC mode constructs symmetric `breaks` centered on 0, so red / blue are comparable across contrasts; expression mode uses the data's min / max unless `zlim_min` / `zlim_max` override.
  - When the user-supplied title is empty, the call falls back to a sensible default (the contrast name in Expression mode, "Cross-contrast LFC heatmap" in LFC mode).
- Color vectors come from `build_heatmap_palette(palette, n, reverse)`, which uses RColorBrewer for diverging palettes and `grDevices::hcl.colors()` for sequential ones.

---

## Server Behavior (`server_tab8_heatmap.R`)

- Availability gate `output$heatmap_available` enables the plot panel when the selected mode plus its required data are present
- Mode-aware contrast selectors are repopulated on every change (Expression: pipeline-only; LFC: pipeline plus uploaded)
- Annotation factor choices are pulled lazily from `eligible_annotation_columns()` and preserve the user's current selection on refresh
- Upload handler (LFC mode) validates required columns, ensures `log2FoldChange` and `padj` are numeric, and stores per-contrast frames into `values$uploaded_heatmap_results`
- Cache invalidation: when `values$deseq_results` changes, `values$heatmap_cache`, `values$vst_cache`, and `values$rlog_cache` are all cleared
- Plot reactive: assembles `base_params` from all controls (NA-safe via `isTruthy()`), dispatches to the mode-specific generator inside `tryCatch`, and stores results in `values$heatmap_cache` keyed by MD5 via `generate_cache_key_heatmap(mode, params, gene_set)`
- Render: `output$heatmap_plot` calls `grid::grid.newpage()` followed by `grid::grid.draw(pr$plot$gtable)` because `pheatmap` returns a `gtable`-based object
- Downloads:
  - PDF and PNG via `grDevices::pdf()` / `grDevices::png()` plus `grid.draw()`, also written under `results/heatmap/<contrast-or-lfc>/`
  - Parameters JSON via `jsonlite::toJSON()` (mode, contrast(s), gene method and inputs, sample scope, transformation, scaling, clustering, colors, display, annotations, title, export settings, gene set size)
  - Matrix CSV (one row per gene, one column per sample or contrast) via `write.csv()`

---

## Caching

| Cache | Keyed by | Cleared when |
|-------|----------|--------------|
| `values$heatmap_cache` | MD5 of `(mode, params, gene_set)` | `values$deseq_results` changes |
| `values$vst_cache` | contrast name | `values$deseq_results` changes |
| `values$rlog_cache` | contrast name | `values$deseq_results` changes |

`generate_cache_key_heatmap()` hashes `mode`, the full `params` list (including clustering and palette controls), and the gene set, so any meaningful change to the heatmap invalidates its cache slot and forces a fresh render.

---

## Troubleshooting (Quick)

- Expression mode shows "requires pipeline-computed results": uploaded CSVs do not contain a `dds`; run the DESeq2 analysis first, or switch to LFC mode
- LFC upload rejected: confirm required columns `gene, log2FoldChange, padj, contrast` are present and numeric where required
- Heatmap is empty: relax the gene-selection thresholds, or verify your custom list intersects the available genes
- Clustering tree missing: clustering is auto-disabled with fewer than 2 rows or 2 columns; with the Correlation distance, rows / columns must have non-zero variance (constant vectors are dropped before clustering)
- LFC heatmap looks washed out: set a manual `zlim_min` / `zlim_max` to clip the symmetric range
- Annotation legend overflows the page: deselect high-cardinality factors, or hide the annotation legend in Advanced Options

---

End.

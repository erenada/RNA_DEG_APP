# Enrichment Analysis Tab

Author: Eren Ada, PhD  
Last Updated: October 21, 2025  
Module Files: `ui/tab_enrichment.R`, `server_modules/server_tab5_enrichment.R`, `R/utils_enrichment.R`

---

## Purpose

Run GO enrichment on significant genes from DE results (computed in-app or uploaded CSV), with caching, per-ontology tables/plots, and per-term gene list downloads.

---

## Prerequisites

- Results available from either:
  - Computed DESeq2: `values$deseq_results`
  - Uploaded CSVs: `values$uploaded_deg_results` with required columns: `gene`, `log2FoldChange`, `padj`, `contrast`
- Project settings loaded: `values$organism`, `values$id_type`, `values$orgdb`

Availability gate: `output$enrichment_available` is TRUE if computed or uploaded results exist.

---

## UI Overview (Key Controls)

- Upload DEG Results (optional): multiple CSVs; clear uploaded results
- Select Contrast(s): single or multi-contrast run
- Ontologies: BP, MF, CC
- Gene Set: Up, Down, All
- Run buttons: Selected contrasts or All contrasts
- Advanced Options: padj and |log2FC| cutoffs, GO p/q cutoffs, gene set size, simplify cutoff, cache clear
- Results panel: per-contrast summary, per-ontology tabs with table and bar/dot plots, per-term gene-list export

---

## Behavior (Server)

- Contrast source priority: uploaded results (if present) else computed
- Contrast selector observes both uploaded and computed results and updates options
- When a contrast changes, defaults for padj and LFC cutoff are seeded from the contrast's analysis parameters when available
- Cache: results keyed by contrast + parameters (`generate_cache_key`) and stored under `values$enrichment_results[[contrast]][[cache_key]]`

---

## Gene List Construction

Function: `build_gene_lists(contrast_name, direction, alpha, lfc_threshold)`
- Source results: uploaded or computed, selected by contrast name
- Universe: genes with non-NA `padj`
- Significant: `padj < alpha` and (if `lfc_threshold > 0`) `|log2FC| > threshold`
- Direction:
  - Up: log2FC > threshold (or >0 if threshold == 0)
  - Down: log2FC < -threshold (or <0 if threshold == 0)
  - All: union of significant genes
- Returns genes/universe data frames, vectors of IDs, named log2FC, and counts

---

## ID Mapping and Utilities

- `perform_id_mapping(gene_lists, id_type, orgdb)`
  - Auto-detects ID type if `id_type == 'auto'` (prefix ENS ⇒ ENSEMBL, else SYMBOL)
  - Maps to ENTREZ using `clusterProfiler::bitr`
  - Computes mapping stats (`compute_mapping_stats`) and warns on too few/low mapping
- `safe_simplify(ego, cutoff)`
  - Wraps `clusterProfiler::simplify` with error handling
- `compute_mean_log2fc(gene_ids, fold_changes)`
  - Computes mean log2FC per GO term
- `generate_cache_key(contrast_name, params)`
  - MD5 over key parameters for robust caching
- `augment_enrichment_results(ego, fold_changes)`
  - Adds `mean_log2FC` and `enrichment_score` ((k/n)/(M/N)) to results

---

## Execution Flow

Single contrast: `run_enrichment_for_single_contrast(contrast, params, progress_callback)`
- Build gene lists → map IDs → run `enrichGO` per ontology → optional simplify → augment → return results/statistics

Batch:
- `observeEvent(input$run_enrichment)`: runs selected contrasts
- `observeEvent(input$run_all_contrasts)`: runs all available contrasts (uploaded ∪ computed), with optional confirmation if many
- Uses cache; skipped contrasts counted as cached

Parameters (common):
- `ontologies` (BP/MF/CC), `direction` (Up/Down/All)
- `alpha`, `lfc_threshold` (gene list filter)
- `pvalue_cutoff`, `qvalue_cutoff`, `min_gs_size`, `max_gs_size`
- `simplify_cutoff` (0–1) for term reduction

---

## Outputs and Exports

- Results panel: per-contrast summary, ontology tabs with table + plots
- Tables (DT): include per-term download button and export (copy/csv/excel)
- Exports:
  - Per-ontology table: CSV/XLSX (also saved under `results/enrichment/<contrast>/<organism>/<id_type>/<ont>/<direction>/`)
  - Per-ontology gene lists: CSV (term and gene list per row)
  - Per-term gene list: on-table button generates CSV and saves under `results/enrichment/gene_lists/<contrast>/<ont>/`

---

## Troubleshooting (Quick)

- No enrichment results available
  - Ensure DEG results exist (run DESeq2 or upload CSVs) and select at least one contrast
- Too few genes after filtering
  - Increase `alpha`, reduce `lfc_threshold`, or choose All direction
- Low mapping rate or mapping failed
  - Verify organism and ID type; try `id_type = 'SYMBOL'`/`'ENSEMBL'` explicitly
- No enriched terms found
  - Relax GO p/q cutoffs or simplify cutoff; check gene set size bounds
- Cache shows old results
  - Click Clear Cache and re-run with current parameters

---

End.

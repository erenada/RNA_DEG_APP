# DESeq2 Configuration Tab

Author: Eren Ada, PhD  
Last Updated: October 21, 2025  
Module Files: `ui/tab_config.R`, `server_modules/server_tab3_analysis_config.R`

---

## Purpose

Configure DESeq2 analysis parameters, run per-contrast DE analysis, and expose results for downstream tabs.

---

## Inputs and Controls

- Test Type: `radioButtons('test_type', c('Wald','LRT'))`
  - Wald: standard pairwise tests (supports shrinkage)
  - LRT: omnibus test using reduced model (no shrinkage)
- Significance
  - `alpha_level`: adjusted p-value cutoff (FDR)
  - `lfc_threshold`: minimum |log2FC| for significance filters (downstream)
- Shrinkage (Wald only)
  - `apply_lfc_shrinkage` (bool)
  - `shrinkage_type`: `apeglm` (default), `ashr`, `normal`
- Multiple Testing and Dispersion
  - `p_adjust_method`: BH (default), Bonferroni, BY, Holm
  - `dispersion_fit_type`: parametric (default), local, mean
- Pre-filtering (optional)
  - `apply_prefiltering` (bool)
  - `min_count_threshold`, `min_samples_threshold`

---

## Behavior (Server)

- Summary panel: `output$config_summary` shows current settings and contrasts count
- Validation for Run button (`run_analysis`): requires
  - ≥1 contrast; count data and metadata loaded
  - Valid `alpha_level` (0–1), non-negative `lfc_threshold`
  - For LRT: non-empty `lrt_reduced_formula`
  - If pre-filtering enabled: positive thresholds
- Execution: `observeEvent(input$run_analysis)`
  - Rounds counts to integers; iterates over contrasts
  - Subsets count/metadata per contrast; rebuilds factor with reference = denominator
  - Builds `dds` with contrast-specific design formula
  - Runs DESeq2 (Wald or LRT) with selected dispersion fit
  - Applies optional pre-filtering prior to DESeq2
  - Extracts `results()` with `alpha` and `pAdjustMethod`
  - Optionally applies LFC shrinkage (Wald only) using selected method
  - Aggregates per-contrast summaries: total, significant, up, down
  - Stores results in `values$deseq_results[[contrast_name]]`
  - Stores overall summary in `values$overall_summary`; flags `values$analysis_completed = TRUE`

---

## Outputs and Exports

- Completion message: `output$analysis_completion_message`
- "View Results" navigates to Results tab
- Download: `output$download_results` (combined CSV/XLSX filename)

Stored per contrast at `values$deseq_results[[name]]`:
```r
list(
  results = data.frame,         # (log2FC, lfcSE, stat, pvalue, padj, gene, contrast)
  unshrunken_results = data.frame,
  dds = DESeqDataSet,
  shrinkage_info = list(applied, method, coefficient, note, available_coefficients),
  contrast_info = <structure from design tab>,
  analysis_params = list(test_type, alpha, lfc_threshold, shrinkage_type, p_adjust_method, dispersion_fit_type, prefiltering, ...)
)
```

---

## Minimal Guidance

- Choose Wald unless you explicitly need LRT (time-series, multi-factor omnibus).
- Use `alpha = 0.05` and enable shrinkage (`apeglm`) for effect-size interpretation and plots.
- If you enforce an |log2FC| minimum, set it in the UI (results will be filtered accordingly downstream).
- Use pre-filtering for large datasets to improve speed; DESeq2 still performs independent filtering in results().

### Shrinkage implementation details (from `R/utils_deseq2.R`)

- Only `apeglm` is applied; other methods are disabled (shrinkage is skipped with a note).
- Coefficient resolution is robust for multi-factor designs:
  - Prefer exact patterns prefixed by the factor of interest (e.g., `factor_numerator_vs_denominator`).
  - Fallbacks progressively filter by factor, numerator, and denominator; if ambiguous, shrinkage is skipped to avoid applying to a covariate.
- Shrinkage is applied to unthresholded results (hypothesis testing remains on unshrunken stats). Use shrunken LFCs for effect size and plots, not p-values.
- Metadata recorded in `shrinkage_info`: `available_coefficients`, `coefficient_used` (if any), `pattern_used` (if any), `note`.

---

## Troubleshooting (Quick)

- Run button disabled
  - Create at least one contrast; ensure `alpha_level` in (0,1]; set LRT reduced model if using LRT; set prefilter thresholds if enabled
- DESeq2 analysis failed: count data contains missing/non-numeric values
  - Ensure counts are numeric with no NAs; raw counts recommended; app rounds to integers
- Shrinkage not applied
  - Shrinkage is disabled for LRT; only `apeglm` is used for Wald; skipped if coefficient is ambiguous/not found
- Too few significant genes
  - Consider alpha 0.1; check design (replicates, confounding); reduce LFC threshold

---

End.

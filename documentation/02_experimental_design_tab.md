# Experimental Design Tab

Author: Eren Ada, PhD  
Last Updated: October 21, 2025  
Module Files: `ui/tab_design.R`, `server_modules/server_tab2_experimental_design.R`

---

## Purpose

Assign samples to comparison groups (Numerator vs Denominator), optionally add covariates, validate the design, and create contrasts for DESeq2.

---

## UI and Controls (Essentials)

- Available Samples (left)
  - Dynamic filters from metadata
  - Count of filtered vs total
  - Draggable sample badges with metadata tooltips
- Comparison Groups (right)
  - Group 1 (Numerator): name input + drop zone
  - Group 2 (Denominator): name input + drop zone
  - Contrast preview (group sizes, formula, covariates, contrast)
  - Create Contrast / Clear All
- Advanced: Add Covariates (collapsible)
  - Multi-select covariates (excludes sample IDs/all-unique; excludes factor name)
  - Categorical reference-level selectors
  - Design formula preview + warnings
  - Reset to simple design
- Contrast Management
  - Import contrasts from CSV
  - Export created contrasts to CSV
  - List of created contrasts with details and remove buttons
- Navigation
  - Back: Data Input
  - Next: Analysis Configuration (enabled when at least one contrast exists)

---

## Behavior (Server)

- Sample list is limited to matched samples from Data Input validation
- Filters reduce the available list without altering groups
- Drag-and-drop moves samples between Available / Group1 / Group2
- Covariate picker auto-excludes:
  - Factor-of-interest name
  - Sample ID columns
  - All-unique columns (likely identifiers)
- Reference level controls shown only for categorical covariates (numeric are included as-is)
- Design formula is always: `~ covariates + factor_of_interest`
- Group/factor names must be valid identifiers (letter first; letters/digits/underscore)

---

## Workflow

1) Filter (optional) and review available samples  
2) Drag samples to Group 1 (Numerator) and Group 2 (Denominator)  
3) Name groups (e.g., Treatment vs Control)  
4) (Optional) Add covariates and set reference levels for categorical covariates  
5) Review design formula and warnings  
6) Create Contrast (appears in the list); repeat as needed  
7) Proceed to Analysis Configuration when ready

Best practice: ≥3 biological replicates per group; avoid confounding.

---

## Validation

- Names
  - Must start with a letter; only letters/digits/underscore
- Covariates
  - Categorical with ≤1 level across contrast samples → error/remove
  - Numeric with near-zero variance → warning
- Rank deficiency (not full rank)
  - Checks confounding between covariates and factor-of-interest
  - Drops empty levels after subsetting (including the "Unused" level)
  - Stops with a clear message if not full rank

---

## Contrast Import/Export

- Export CSV (one row per contrast):
  - `contrast_name, group1_samples, group2_samples, design_formula, covariates`
- Import CSV:
  - Validates required columns and sample names
  - Reconstructs contrasts, sets factor levels and design formula
  - Skips invalid rows with clear warnings

Use case: share designs between projects or team members.

---

## Troubleshooting (Quick)

- Group name invalid
  - Start with a letter; use letters/digits/underscore only
- Design not full rank
  - Remove/adjust confounded covariates; ensure levels exist in both groups
- Covariate has ≤1 level
  - Remove the covariate or add samples with different levels
- Samples missing from available list
  - Click "Show All" to reset filters
- Drag-and-drop not responding
  - Refresh the browser; ensure JS assets are loaded

---

## Notes

- DESeq2 log2FC sign: Numerator / Denominator
- Factor-of-interest is placed last in the design formula to ensure correct coefficient extraction
- Non-contrast samples are labeled "Unused" and excluded during analysis for that contrast

---

End.


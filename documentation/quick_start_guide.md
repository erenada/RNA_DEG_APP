# Quick Start Guide

RNA-seq Differential Expression Analysis App

---

## 5-Minute Quick Start

1) Prepare data
- Count matrix: genes × samples (raw counts; genes as row names; samples as column names)
- Metadata: samples × variables (sample IDs as row names; variables as columns)

2) Upload
- Data Input tab → upload count matrix and metadata
- Select organism (Mouse/Human) → wait for green validation message

3) Create contrast
- Experimental Design tab → drag samples to Numerator (focus) and Denominator (reference)
- Name groups; click Create Contrast

4) Run analysis
- DESeq2 Configuration tab → use defaults → Run DESeq2 Analysis

5) Review
- Results tab (tables) → Volcano Plot tab (plots) → Enrichment tab (optional)

---

## File Formats

### Count Matrix (CSV)
```csv
gene_id,Sample1,Sample2,Sample3
Actb,5421,6234,5892
Gapdh,8821,9234,8455
Tp53,421,512,398
```
Requirements:
- First column = gene IDs (row names after upload)
- Other columns = non-negative numeric counts
- Header row present; separator matches selection

### Metadata (CSV)
```csv
sample_id,condition,batch
Sample1,Control,Batch1
Sample2,Control,Batch1
Sample3,Treatment,Batch2
```
Requirements:
- First column = sample IDs (row names after upload)
- Other columns = experimental factors/covariates
- Sample IDs exactly match count matrix column names

---

## Common Issues & Fixes

- No matching samples found
  - Ensure exact matches (case/whitespace) between count column names and metadata row names
- Non-numeric columns in count matrix
  - Keep only count columns (set gene IDs as row names)
- Non-integer counts warning
  - Safe to proceed if expected from preprocessing
- File parsing problems
  - Use correct separator; enable Header and Row names; save as UTF-8 CSV
- Organism database error
  - Install required package:
    ```r
    install.packages('BiocManager')
    BiocManager::install('org.Mm.eg.db')  # Mouse
    BiocManager::install('org.Hs.eg.db')  # Human
    ```
- "Next" button not shown
  - Validation must be green; fix listed errors and re-upload

---

## Minimal Defaults

DESeq2 Configuration (recommended defaults):
- Test: Wald
- Alpha: 0.05
- LFC shrinkage: apeglm
- Pre-filter: enabled
Change only if you have a specific design (e.g., time series → LRT, higher LFC threshold).

Enrichment (typical):
- Ontology: BP
- Gene set: Up or Down (more specific than All)
- Cutoffs: p-value 0.05, q-value 0.2

---

## Exports

| Tab | Export |
|-----|--------|
| Results | All genes or significant genes (CSV/XLSX) |
| Enrichment | Enrichment tables and per-term gene lists (CSV/XLSX) |
| Volcano | Plots (PDF/PNG) and parameters (JSON) |

---

## Example Data

Located in `example_data/`:
- Toy: `toy_counts.csv`, `toy_metadata.csv` (quick checks)
- Example: `example_counts.csv`, `example_metadata.csv` (full workflow)

---

## Minimal Checklist

- Data Input: count matrix uploaded; metadata uploaded; organism selected; green validation
- Design: samples assigned; groups named; contrast created
- Analysis: defaults reviewed; run completed
- Results: tables reviewed; volcano plotted; (optional) enrichment run
- Export: results/plots saved; parameters recorded if needed

---

## Help

- Detailed docs: `documentation/01_data_input_tab.md`, `documentation/02_experimental_design_tab.md`
- Code references: `R/`, `ui/`, `server_modules/`
- Issues/requests: GitHub Issues (see main README)


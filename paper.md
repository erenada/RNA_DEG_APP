---
title: 'RNA-seq Differential Gene Expression Analysis App: An Interactive Shiny Application for DESeq2-Based Statistical Analysis and Visualization'
tags:
  - R
  - RNA-seq
  - differential expression
  - DESeq2
  - transcriptomics
  - bioinformatics
  - Shiny
authors:
  - name: Eren Ada
    orcid: 0000-0001-9141-8282
    affiliation: 1
affiliations:
  - name: Department of Immunology, Harvard Medical School
    index: 1
date: 2026-04-20
bibliography: paper.bib
---

# Summary

RNA_DEG_APP is an interactive R Shiny application that performs differential
gene expression (DGE) analysis of bulk RNA-seq data using the DESeq2
statistical framework [@love2014moderated]. The application guides users
through a seven-step workflow spanning data upload and validation,
drag-and-drop contrast construction with covariate support, DESeq2 model
fitting (Wald and likelihood-ratio tests) with optional log-fold-change
shrinkage, tabular results inspection, Gene Ontology (GO) overrepresentation
analysis through `clusterProfiler` [@yu2012clusterprofiler; @wu2021clusterprofiler],
and publication-ready volcano plot rendering with per-contrast parameter
control. No R programming is required: all statistical choices, factor-level
references, and visualisation options are exposed through the user interface.
RNA_DEG_APP is the downstream companion to `RNA_QC_APP`, completing a
modular, end-to-end no-code pipeline from raw-count quality control through
differentially expressed gene lists and enrichment plots. Example count
matrices and metadata tables are shipped with the repository so that users
can exercise the full workflow without preparing their own data.

# Statement of Need

Differential expression is a core step in RNA-seq analysis, but running
DESeq2 [@love2014moderated], edgeR [@robinson2010edger], or limma-voom
[@ritchie2015limma] directly requires writing R code, constructing design
matrices, specifying contrasts, choosing p-value adjustment methods, and
handling shrinkage estimators. Wet-lab biologists, immunologists, and
clinician-scientists without formal bioinformatics training are often unable
to run these steps themselves, which either stalls projects or pushes
routine re-analyses onto shared bioinformatics staff. Existing graphical
tools only partially bridge this gap. DEBrowser
[@kucukural2019debrowser] offers interactive analysis but integrates less
tightly with a surrounding no-code suite; iDEP [@ge2018idep] provides a
powerful web-hosted workflow but does not expose a flexible contrast builder
that supports covariates alongside the factor of interest.

RNA_DEG_APP addresses this gap by pairing DESeq2 with a drag-and-drop
contrast builder. Users assign samples to two groups, name the comparison,
optionally add covariates (for example batch or sex) into the design
formula, and the application constructs the corresponding DESeqDataSet,
runs the selected test, and extracts results for each stored contrast. The
same interface supports Wald and likelihood-ratio tests, adjustable alpha
and log-fold-change thresholds, and optional `apeglm` shrinkage
[@zhu2019apeglm]. The target audience is researchers at the wet-lab and
clinical interface who need reproducible, inspectable DESeq2 output without
writing code. RNA_DEG_APP was developed at the Harvard Medical School (HMS)
Department of Immunology in direct response to collaborator requests, is
used internally on several ongoing immunology projects, and is designed to
accept count and metadata files produced by `RNA_QC_APP` without additional
reformatting.

# State of the Field

Several Bioconductor-based graphical tools address pieces of the RNA-seq
exploration workflow. iSEE [@ruealbrecht2018isee] provides a flexible
`SummarizedExperiment` browser for pre-computed results. pcaExplorer
[@marini2019pcaexplorer] focuses on interactive PCA and sample-level
diagnostics. DEBrowser [@kucukural2019debrowser] offers end-to-end DGE with
DESeq2 and edgeR backends and multiple plot types. iDEP [@ge2018idep] is a
web-based tool that couples expression analysis with pathway enrichment.
Command-line frameworks such as DESeq2 [@love2014moderated], edgeR
[@robinson2010edger], and limma [@ritchie2015limma] remain the statistical
standard but require scripting. What distinguishes RNA_DEG_APP is its
explicit focus on a single, linear, no-code path from contrast design
through GO enrichment and volcano plot export, bundled with a companion
quality-control application (`RNA_QC_APP`) so that the same laboratory can
move from raw counts to publication figures without leaving the Shiny
environment.

# Software Design

RNA_DEG_APP is organised as a Shiny application whose entry point is
`app.R`, which sources `global.R`, `ui.R`, and `server.R`. `global.R` loads
CRAN and Bioconductor dependencies, increases the Shiny upload limit to
300 MB, and conditionally attaches the appropriate organism annotation
package (`org.Mm.eg.db` or `org.Hs.eg.db`) once the user selects mouse or
human in the interface. The UI and server layers are split into seven
self-contained tab modules under `ui/` and `server_modules/`, each owning
one step of the workflow: data input, experimental design, analysis
configuration, results display, enrichment, volcano plots, and
documentation. Shared state (count matrix, metadata, design factors,
contrasts, DESeq2 results, enrichment cache, volcano cache) is held in a
single `reactiveValues` object created in `server.R` and threaded through
the tab modules by sourcing them with `local = TRUE`.

Data input accepts CSV, TSV, text, or XLSX files for both counts and
metadata. Input validation is implemented in `R/utils_validation.R` and
checks matrix dimensions, numeric type, non-negative integer counts, sample
name matching between counts and metadata, and degenerate metadata columns.
These checks are covered by `testthat` unit tests in
`tests/testthat/test_validation.R`. The experimental design tab implements
a custom drag-and-drop assignment of samples into two groups (backed by
`www/js/design_drag_drop.js` and jQuery UI), collects optional covariates,
and stores each named contrast along with its factor name, reference level,
and design formula.

DESeq2 integration lives in `server_modules/server_tab3_analysis_config.R`.
For every stored contrast, the app subsets the counts and metadata to the
two groups, constructs a `DESeqDataSet` with the user-specified design
formula, applies optional pre-filtering on minimum counts per sample, and
runs `DESeq()` with either the Wald or LRT test. Results are extracted with
the user's alpha and multiple-testing method. When shrinkage is requested,
the helper `apply_lfc_shrinkage()` in `R/utils_deseq2.R` performs robust
coefficient-name resolution for apeglm in multi-factor designs
[@zhu2019apeglm] and falls back to unshrunken results on failure. Each
contrast's shrunken results, unshrunken results, shrinkage metadata, and
analysis parameters are stored in the reactive values object.

GO overrepresentation analysis (tab 5) is built on
`clusterProfiler::enrichGO()` [@yu2012clusterprofiler; @wu2021clusterprofiler]
and exposes the Biological Process, Molecular Function, and Cellular
Component ontologies. Gene symbols and Ensembl IDs are harmonised through a
`bitr`-based wrapper in `R/utils_enrichment.R`, and results are augmented
with fold enrichment and mean log-fold-change per term. Results are cached
per contrast using `digest`-based keys so that repeated queries are
instantaneous. Volcano plots (tab 6) are generated by
`R/utils_visualization.R` on top of `EnhancedVolcano`, supporting adjustable
thresholds, custom colours, gene-label forcing, highlighting of
table-selected genes, and export to PDF and PNG via `downloadHandler`s;
current parameter sets are exportable as JSON for reproducibility.

# Research Impact Statement

RNA_DEG_APP was developed at the HMS Department of Immunology to remove the
coding barrier for bench scientists performing differential expression on
bulk RNA-seq data, with built-in support for both mouse and human studies
through conditional loading of the `org.Mm.eg.db` and `org.Hs.eg.db`
annotation databases. As the downstream companion of `RNA_QC_APP`, it
enables a no-code pipeline from quality control through DESeq2 results, GO
enrichment, and publication-ready volcano plots within a single Shiny
environment. The application is in active internal use on several ongoing
immunology projects and accepts the outputs of `RNA_QC_APP` without further
reformatting, shortening the path from sequencing output to biological
interpretation. Example count and metadata tables are included so new users
can exercise the full workflow immediately. The tool is open source and
intended for broad adoption in transcriptomics, immunology, cancer biology,
and translational research, particularly in laboratories that lack
dedicated bioinformatics support but require reproducible, inspectable DGE
output.

# AI Usage Disclosure

Development of RNA_DEG_APP was assisted by the same AI tooling used for
`RNA_QC_APP`: GitHub Copilot, OpenAI ChatGPT, Anthropic Claude, and the
Cursor IDE. These tools were used for code completion, targeted
refactoring, documentation drafting, and prose editing of supporting
materials. All AI-generated suggestions were reviewed, edited, tested, and
validated by the author before being incorporated into the codebase or this
manuscript.

# Acknowledgements

The author thanks the Bioconductor community [@huber2015orchestrating] and
the developers of the core statistical and visualisation packages on which
RNA_DEG_APP depends, including DESeq2 [@love2014moderated], clusterProfiler
[@yu2012clusterprofiler; @wu2021clusterprofiler], edgeR
[@robinson2010edger], and limma [@ritchie2015limma], as well as the Shiny
[@chang2023shiny], ggplot2 [@wickham2016ggplot2], and plotly
[@sievert2020plotly] communities whose tools underpin the user interface
and graphics layer. The author also thanks colleagues in the HMS Department
of Immunology for feedback during internal deployment and for motivating
the development of this application.

# References

# RNA-seq Differential Gene Expression Analysis App

**Author:** Eren Ada, PhD  
**Date:** 06/24/2025  
**Version:** 0.1.0 (Development)

A Shiny web application for differential gene expression analysis of RNA-seq data using DESeq2. This application provides an intuitive interface for uploading count data, designing experiments, and performing statistical analysis of gene expression differences between conditions.

## Features

### Current Features (v0.1.0)
- **Data Upload & Validation**: Upload count matrices and metadata with comprehensive validation
- **Smart Experimental Design**: Drag-and-drop interface for creating comparison groups
- **Intelligent DESeq2 Integration**: Automatic design formula generation based on user-defined groups
- **Custom Factor Naming**: Allow users to specify meaningful factor names for DESeq2 analysis
- **Real-time Preview**: See experimental design and DESeq2 formulas before running analysis
- **Professional UI**: Clean, responsive interface with progress indicators and error handling

### Planned Features (Roadmap)
- **DESeq2 Analysis Execution**: Run differential expression analysis with customizable parameters
- **Interactive Results Tables**: Browse and filter DEG results with statistical summaries
- **Visualization Suite**: MA plots, volcano plots, PCA, and heatmaps
- **Export Functionality**: Download results as CSV/Excel, generate reproducible R scripts
- **Advanced Design Support**: Complex experimental designs with interaction terms
- **Integration Ready**: Designed for integration with RNA-seq QC preprocessing modules

## Requirements

### System Requirements
- **R Version**: >= 4.1.0
- **RAM**: Minimum 8GB (16GB recommended for large datasets)
- **Operating System**: Windows, macOS, or Linux

### R Package Dependencies

**Core Dependencies:**
```r
# Shiny Framework
shiny (>=1.7.0), shinythemes, shinyWidgets, shinyjs, DT

# Data Manipulation
dplyr, tidyr, readr, tools

# Bioconductor Packages
DESeq2 (>=1.40.0), SummarizedExperiment

# Visualization
ggplot2, plotly
```

## Installation

### Option 1: Automated Installation (Recommended)

1. **Clone the repository:**
```bash
git clone https://github.com/erenada/RNA_DEG_APP.git
cd RNA_DEG_APP
```

2. **Install dependencies automatically:**
```r
# Run the automated package installer
source("install_packages.R")

# Verify installation
source("check_packages.R")
```

3. **Launch the application:**
```r
# Option 1: Using shiny::runApp()
shiny::runApp()

# Option 2: Using the app launcher
source("app.R")
```

### Option 2: Manual Installation

1. **Install Bioconductor (if not already installed):**
```r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
```

2. **Install required packages:**
```r
# CRAN packages
install.packages(c("shiny", "shinythemes", "shinyWidgets", "shinyjs", 
                   "DT", "dplyr", "tidyr", "readr", "ggplot2", "plotly", "tools"))

# Bioconductor packages
BiocManager::install(c("DESeq2", "SummarizedExperiment"))
```

3. **Launch the application:**
```r
shiny::runApp()
```

## Usage

### Quick Start Guide

1. **Data Upload (Tab 1)**
   - Upload your count matrix (genes × samples) in CSV/TSV format
   - Upload corresponding metadata (samples × variables) in CSV/TSV format
   - The app will automatically validate data compatibility

2. **Experimental Design (Tab 2)**
   - Use filters to subset samples of interest
   - Drag samples into "Treatment" and "Control" groups
   - Specify a custom factor name (default: "contrast_group")
   - Preview the DESeq2 design formula before creating contrasts

3. **Create Contrasts**
   - Name your comparison groups meaningfully
   - Click "Create Contrast" to save the experimental design
   - View detailed DESeq2 formula and contrast specifications

### Input Data Format

**Count Matrix:**
- Rows: Genes/transcripts (gene IDs as row names)
- Columns: Samples (sample IDs as column names)
- Values: Raw count integers (non-negative)

```
           Sample_01  Sample_02  Sample_03  Sample_04
GENE_001       120       85       200       150
GENE_002         5        8        12        10
GENE_003       350      280       420       380
```

**Metadata:**
- Rows: Samples (sample IDs as row names, must match count matrix columns)
- Columns: Experimental variables (strain, treatment, tissue, etc.)

```
           Strain  Treatment  Tissue
Sample_01  StrainA   Control  Tissue1
Sample_02  StrainA   Control  Tissue1
Sample_03  StrainA  TreatmentX Tissue1
Sample_04  StrainA  TreatmentX Tissue1
```

## Experimental Design Philosophy

This application uses a **user-defined grouping approach** for maximum flexibility and reliability:

- **Simple & Reliable**: Creates contrast factors based on your sample selections
- **No Assumptions**: Doesn't try to guess your experimental design from metadata
- **Maximum Control**: You define exactly what to compare
- **DESeq2 Compatible**: Generates proper design formulas for statistical analysis

### Example Output
```r
# Your selections generate:
design = ~ treatment_condition
results(dds, contrast = c('treatment_condition', 'Treatment', 'Control'))
```

## Testing

The application includes comprehensive testing infrastructure:

```r
# Test app startup
source("test_app_startup.R")

# Run validation tests
testthat::test_dir("tests/testthat/")
```

## Development Status

**Current Phase:** Sprint 2 - Core Analysis Pipeline (In Progress)

### Completed
- Data upload and validation system
- Drag-and-drop experimental design interface  
- Smart DESeq2 formula generation
- Custom factor naming
- Real-time design preview
- Comprehensive error handling

### In Development
- DESeq2 analysis execution
- Results table display
- Basic visualization plots

### Planned
- Advanced DESeq2 parameter control
- Interactive results exploration
- Export functionality
- Complex experimental design support

## Project Structure

```
RNA_DEG_APP/
├── R/                          # Utility functions
│   ├── utils_deseq2.R         # DESeq2 wrapper functions (planned)
│   └── utils_validation.R     # Data validation functions
├── www/                       # Web assets
│   └── styles.css            # Application styling
├── tests/                     # Testing infrastructure
│   └── testthat/
│       └── test_validation.R  # Validation tests
├── ui.R                       # User interface definition
├── server.R                   # Server logic
├── app.R                      # Application launcher
├── global.R                   # Global configuration and packages
├── install_packages.R         # Automated package installation
├── check_packages.R           # Package verification
├── test_app_startup.R         # Startup testing
└── README.md                  # This file
```

## Contributing

This is an active development project. Contributions, suggestions, and feedback are welcome!

### Development Guidelines
- Follow existing code style and structure
- Add tests for new functionality
- Update documentation for changes
- Test with real RNA-seq datasets

### Reporting Issues
Please provide:
- R version and operating system
- Package versions (`sessionInfo()`)
- Sample data format (if applicable)
- Error messages and reproducible steps

## License

This project is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License - see the LICENSE file for details.

## Citation

If you use this tool in your research, please cite:

```
@software{ada2025rna_deg,
  author = {Ada, Eren},
  title = {RNA-seq Differential Gene Expression Analysis App},
  year = {2025},
  institution = {Harvard Medical School, Department of Immunology},
  url = {https://github.com/erenada/RNA_DEG_APP}
}
```

## Acknowledgments

This project builds upon the excellent work of the Bioconductor community and R developers:

- **DESeq2**: Love, M.I., Huber, W., Anders, S. (2014) Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2. Genome Biology, 15:550.
- **Shiny**: Chang, W., Cheng, J., Allaire, J., Xie, Y., & McPherson, J. (2021). shiny: Web Application Framework for R.
- **DT**: Xie, Y., Cheng, J., & Tan, X. (2021). DT: A Wrapper of the JavaScript Library 'DataTables'.
- **plotly**: Sievert, C. (2020). Interactive Web-Based Data Visualization with R, plotly, and shiny.

## Related Projects

This application is part of a modular RNA-seq analysis suite:

- **RNA_QC_APP**: Quality Control and Preprocessing Tool - [https://github.com/erenada/RNA_QC_APP](https://github.com/erenada/RNA_QC_APP)
- **RNA_DEG_APP**: Differential Gene Expression Analysis Tool (this repository)

## Support & Community

### Getting Help

1. Check the documentation and README
2. Review system requirements and dependencies
3. Try with example data to isolate issues
4. Create an issue with detailed information including:
   - R version and operating system
   - Browser and version
   - Error messages and steps to reproduce

### Reporting Issues

When reporting bugs or requesting features:
- Use descriptive titles
- Include system information (R version, OS, browser)
- Provide reproducible examples when possible
- Specify dataset characteristics if relevant

## Contact

**Eren Ada, PhD**  
Harvard Medical School, Department of Immunology  
GitHub: [@erenada](https://github.com/erenada)  
Repository: [RNA_DEG_APP](https://github.com/erenada/RNA_DEG_APP)

---

**Current Focus**: Sprint 2 - Core DESeq2 analysis pipeline development. The application foundation is complete and ready for analysis execution implementation. 
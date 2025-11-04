# Toxicant GWAS Manuscript

This repository contains all code and data needed to reproduce the analyses and figures from the manuscript.

## Reproducibility

### Quick Start

To reproduce all analyses and figures:

```bash
quarto render
```

This will:
1. Decompress pre-computed GWAS mapping results (~6 GB)
2. Process all intermediate data
3. Generate all manuscript figures

### Pre-computed GWAS Results

The GWAS mapping results (`data/processed/20231116_Analysis_NemaScan/`) were generated using the [NemaScan pipeline](https://github.com/andersenlab/nemascan) and are provided pre-computed due to computational requirements (HPC cluster with ~500 CPU hours).

**For detailed information about how these results were generated**, see the rendered documentation at `_products/analysis/nemascan_pipeline_documentation.html` after running `quarto render`, or view the source at [`analysis/nemascan_pipeline_documentation.qmd`](analysis/nemascan_pipeline_documentation.qmd).

**Note**: The compressed archive (`data/raw/nemascan_output/20231116_Analysis_NemaScan.tar.gz`, ~602 MB) is included in this repository and will be automatically decompressed on first render.

## Project Structure

- **`analysis/`** - Data processing and analysis scripts (Quarto documents)
- **`paper/`** - Figure and table generation scripts for the manuscript
- **`data/raw/`** - Raw input data
- **`data/processed/`** - Intermediate processed data (generated during rendering)
- **`bin/`** - Utility functions and helper scripts
- **`_products/`** - Rendered outputs (HTML reports, figures, tables)

## Requirements

- R (version 4.0 or higher recommended)
- Quarto
- Required R packages (will be loaded/installed during rendering)

## Citation

[Citation information to be added]

## License

[License information to be added]

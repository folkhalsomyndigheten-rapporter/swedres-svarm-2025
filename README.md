# Swedres-Svarm Report 2025

A [Quarto](https://quarto.org/) book containing the **Swedres-Svarm Report 2025** — a collaborative publication analyzing Swedish antibiotic sales and resistance in human medicine (Swedres) and veterinary monitoring (Svarm).

The report is published on GitHub Pages: [https://www.folkhalsomyndigheten-rapporter.github.io/swedres-svarm-2025](https://www.folkhalsomyndigheten-rapporter.github.io/swedres-svarm-2025)

## About

This report is a joint publication by:

- [Public Health Agency of Sweden (Folkhälsomyndigheten)](https://www.folkhalsomyndigheten.se/)
- [Swedish Veterinary Agency (Statens veterinärmedicinska anstalt)](https://www.sva.se/)

The report combines data from humans, animals, and food to provide a comprehensive overview of antibiotic sales and resistance trends in Sweden, and has been jointly published for over two decades.

## Repository Structure

This repository contains all source files to render the report as a Quarto book:

```
.
├── _quarto.yml          # Quarto configuration
├── index.qmd            # Landing page
├── chapters/            # Report chapters
│   ├── 0_Infocus/
│   │   └── data/
│   ├── 1_Sales_antibiotics_humans/
│   │   └── data/
│   ├── 2_Sales_antibiotics_animals/
│   │   └── data/
│   ├── 3_Antibiotic_resistance_humans/
│   │   └── data/
│   ├── 4_Antibiotic_resistance_animals/
│   │   └── data/
│   ├── 5_Comparative_analysis/
│   │   └── data/
│   └── 6_Background_data_material_methods/
│       └── data/
├── references/
│   └── references.bib   # Bibliography
└── README.md            # This file
```

## Rendering the Report

### Prerequisites

- [Quarto CLI](https://quarto.org/docs/get-started/)
- R (optional, for R code execution)
- Python (optional, for Python code execution)

### Build the Book

```bash
quarto render
```

This will generate the report in HTML format.

### Output

Rendered output will be in the `docs/` directory.

---

## Open Science & Transparency

This repository supports open science principles by providing:

- **Full reproducibility**: All source code, data processing scripts, and configuration files are included
- **Transparent methodology**: Complete documentation of report building
- **Open access**: The published report is freely available without paywalls
- **Version control**: All changes are tracked via Git, enabling full audit trails

### Data Sources

All data used in this report comes from official Swedish monitoring programs:

- **Swedres**: Human antibiotic consumption and resistance data
- **Svarm**: Veterinary antibiotic consumption and resistance data
- Additional sources are cited in the report and references.bib file

Raw data files are included in the `chapters/*/data/` directories where applicable.

### License

**Report Content**: [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/)

**Code**: [MIT License](https://opensource.org/licenses/MIT)

You are free to share, adapt, and build upon this work for any purpose, including commercially, as long as appropriate credit is given.

### Citation

To cite this report:

```
Folkhälsomyndigheten and Statens veterinärmedicinska anstalt. (2025). Swedres-Svarm Report 2025: Antibiotic sales and resistance in Sweden. https://www.folkhalsomyndigheten-rapporter.github.io/swedres-svarm-2025
```

BibTeX:

```bibtex
@report{swedres_svarm_2025,
  author = {{Folkhälsomyndigheten} and {Statens veterinärmedicinska anstalt}},
  title = {Swedres-Svarm Report 2025: Antibiotic sales and resistance in Sweden},
  year = {2025},
  url = {https://www.folkhalsomyndigheten-rapporter.github.io/swedres-svarm-2025}
}
```

### Contributing

We welcome contributions to improve the report. Please:

1. Fork the repository
2. Create a feature branch
3. Submit a pull request with a clear description of changes

For questions or to report issues, please use the GitHub issue tracker.

### Contact

For questions about the data or report content:

- Folkhälsomyndigheten: [info@folkhalsomyndigheten.se](mailto:info@folkhalsomyndigheten.se)
- SVA: [info@sva.se](mailto:info@sva.se)

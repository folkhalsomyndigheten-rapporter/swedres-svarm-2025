# Swedres-Svarm Report 2025

A [Quarto](https://quarto.org/) book containing the **Swedres-Svarm Report 2025** — a collaborative publication analyzing Swedish antibiotic sales and resistance in human medicine (Swedres) and veterinary monitoring (Svarm).

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
|       └── data/
│   ├── 1_Sales_antibiotics_humans/
|       └── data/
│   ├── 2_Sales_antibiotics_animals/
|       └── data/
│   ├── 3_Antibiotic_resistance_humans/
|       └── data/
│   ├── 4_Antibiotic_resisteance_animals/
|       └── data/
│   ├── 5_Comparative_analysis/
|       └── data/
│   ├── 6_Background_data_material_methods/
|       └── data/
├── references/
|       └── references.bib       # Bibliography
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

This will generate the report in html-format as `_quarto.yml`.

### Output

Rendered output will be in the `docs/` directory.
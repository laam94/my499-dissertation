# When grievances become formal claims

**Explaining variation in complaint filing before subnational human rights institutions in Mexico, 2019–2024**

MSc Social Research Methods · London School of Economics and Political Science

---

## Overview

This repository contains the data, code, and documentation used in an original panel study of complaint filing before Mexico's subnational human rights institutions (SHRIs / *organismos públicos de derechos humanos*). The analysis draws on a balanced panel covering all 32 Mexican states between 2019 and 2024 and estimates negative binomial fixed-effects models to examine how human rights NGO presence and the organisational presence of the receiving institutions are associated with complaint filing.

Rather than treating complaint filing as a proxy for the prevalence of human rights violations, this study conceptualises complaint filing as an observable indicator of formal claiming and of the conditions under which grievances become formal claims.

The principal empirical finding is that institutional presence—operationalised through SHRI staffing—displays a substantially stronger and more robust association with complaint filing than external NGO density.

---

## Repository structure

```text
├── data/
│   ├── raw/                 Original source files (as downloaded)
│   │   ├── cndhe/           INEGI census of SHRIs (complaints, staff, budget)
│   │   ├── sat/             SAT Donatarias Autorizadas (NGO density)
│   │   ├── sesnsp/          SESNSP intentional homicides
│   │   ├── conapo/          CONAPO population projections
│   │   ├── pibe/            INEGI state GDP
│   │   ├── cluni/           CLUNI civil society registry
│   │   ├── red_tdt/         Red TDT directory
│   │   └── gobernadores/    Governor party affiliation
│   ├── processed/           Analysis-ready panel dataset
│   └── geo/                 INEGI state boundaries (maps)
├── scripts/
│   ├── 01_data_construction.R
│   └── 02_analysis.R
└── docs/
    └── transparencia/       Freedom-of-information requests and official responses
```

---

## Data sources

| Source | Institution | Use |
|--------|-------------|-----|
| CNDHE | INEGI | Complaints (dependent variable); staff, budget, offices |
| Donatarias Autorizadas | SAT | Human rights NGO density (independent variable) |
| Incidencia delictiva | SESNSP | Intentional homicide rate (control) |
| Population projections | CONAPO | Population offset |
| PIBE | INEGI | State GDP per capita (control) |
| CLUNI | Secretaría de Bienestar | External validation of NGO measure |
| Red TDT | Red TDT | External validation of NGO measure |

Some raw files are distributed as `.zip` archives to comply with GitHub file-size limits. Decompress them before running the data construction script.

---

## Reproducing the analysis

The analysis requires **R (≥ 4.0)** with the following packages:

`fixest`, `dplyr`, `tidyr`, `readr`, `readxl`, `stringr`, `ggplot2`, `sf`, `patchwork`

```r
# From the repository root

source("scripts/01_data_construction.R")
# Builds data/processed/panel_mexico_shri_2019_2024.csv

source("scripts/02_analysis.R")
# Reproduces all models, tables, and figures
```

All results reported in the dissertation can be reproduced directly from the processed panel included in the repository. Rebuilding the dataset from raw sources is therefore optional.

---

## Method

The dependent variable is the annual number of complaints filed before each state SHRI. Models are estimated using negative binomial regression with two-way (state and year) fixed effects and a population offset. The preferred specification includes lagged NGO density, political alignment, SHRI staffing, and the intentional homicide rate. Robustness analyses include clustered standard errors, alternative measures of institutional presence, accepted complaints as an alternative outcome, endogeneity diagnostics, and alternative mechanism tests based on institutional educational outreach (see the dissertation appendices).

---

## Data availability and transparency

All data are derived from publicly available Mexican government sources.

The CLUNI registry was obtained through a freedom-of-information request submitted to the Secretaría de Bienestar (folio **340025800049526**). The request and the official response are included in `docs/transparencia/`.

---

## Author

Candidate 59923

MSc Social Research Methods

London School of Economics and Political Science

---

## Acknowledgements

This research was supported by the Secretaría de Ciencia, Humanidades, Tecnología e Innovación (SECIHTI) under the Graduate Scholarship Abroad Programme (2025 Call).

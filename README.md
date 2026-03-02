# Heritability of Twinning from Sibling Correlations

This repository contains analysis code and derived outputs for a project estimating the heritability of giving birth to twins using correlations between siblings (especially sisters).

## Data availability and privacy

The underlying registry data used in this project contain sensitive person-level information and were analyzed in a secure server environment.

For that reason, this repository **does not include raw person-level data**. Instead, it includes:

- analysis scripts,
- selected derived outputs,
- and an `on_server/` folder exported from the secure server **without sensitive source data**.

## Repository structure

- `on_server/01_data/` – data preparation scripts that were run in the secure environment.
- `on_server/02_descriptives/` – descriptive analyses (e.g., prevalence summaries).
- `on_server/03_regressions/` – regression analyses and related modeling scripts.
- `on_server/04_correlations/` – correlation analysis used for sibling-based twinning estimates.
- `on_server/08_result_files/` – derived output files exported from analysis (e.g., sibling correlations, contingency tables, figure file).
- `on_server/09_functions/` – reusable utility functions and project fonts.
- `on_server/00_misc_outside_project/` – auxiliary scripts used outside the main project workflow.

## Main derived outputs currently included

- `on_server/08_result_files/sibling_correlations.csv`
- `on_server/08_result_files/contingency_tables.rds`
- `on_server/08_result_files/fig1.svg`

## Reproducibility notes

Because sensitive registry inputs are not distributed here, the full end-to-end pipeline cannot be executed outside the secure environment without approved access to the original data sources.

However, this repository documents the analytic workflow and preserves key derived results needed for transparency and reporting.

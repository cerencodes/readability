# Readability Analysis

The main analysis scripts are:

- `readability_3.R`
- `readability_2.R`
- `readability.R`

Input datasets used by these scripts are stored in the [`data/`](./data) folder.

The scripts were updated to read from `data/` when that folder exists, with a
fallback to the repository root for compatibility with the older file layout.

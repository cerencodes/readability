# Readability Analysis

Repository layout:

- `script/`: analysis scripts
- `data/`: input datasets used by the scripts
- `results/`: generated `.docx` appendix outputs

The main analysis scripts are:

- `script/readability_3.R`
- `script/readability_2.R`
- `script/readability.R`

The scripts were updated so they can be run either from the repository root or
from inside `script/`. They read inputs from `data/` and write generated Word
outputs to `results/`.

# Readability Analysis

Repository layout:

- `script/`: analysis scripts
- `data/`: input datasets used by the scripts
- `results/`: generated `.docx` appendix outputs

The current analysis script is:

- `script/readability.R`

The older scripts, `script/readability_2.R` and `script/readability_3.R`, are
legacy versions kept only for reference.

The scripts were updated so they can be run either from the repository root or
from inside `script/`. They read inputs from `data/` and write generated Word
outputs to `results/`.

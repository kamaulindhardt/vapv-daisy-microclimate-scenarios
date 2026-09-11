# vapv-daisy-microclimate-scenarios

Reproducible analysis pipeline for a vertical agrivoltaics (VAPV) microclimate-scenario study:
Daisy/SPAWN crop-microclimate simulations under radiation, wind, and temperature perturbation
scenarios, comparing open-field and VAPV strip-position outcomes for crop productivity, water
balance, nitrogen cycling, and soil organic carbon (SOC) across a Danish five-course rotation.

This repository contains the **code** used to turn archived Daisy/SPAWN/NWAPS simulation output
into the manuscript's figures and tables. The simulation data itself — Daisy model setup files,
simulation inputs, and simulation output — is archived separately on Zenodo (see
[Data](#data) below); this split follows the manuscript's Data Availability Statement.

> This snapshot corresponds to a specific point in the project's development and reproduces the
> main-text and supplementary figures present in `outputs/` at that point. Check
> `outputs/figures/` directly for what this snapshot actually contains.

## Repository structure

- `R/` — pipeline functions, one file per scientific domain: `import.R`, `clean.R`,
  `productivity.R`, `water_balance.R`, `nitrogen.R`, `soc.R`, `tradeoffs.R`,
  `wind_diagnostics.R`, `figures.R`, `tables.R`, `manuscript_values.R`, `theme_manuscript.R`.
- `_targets.R` — declares the [`targets`](https://books.ropensci.org/targets/) pipeline built
  from the `R/` functions.
- `data/raw/` — Daisy/SPAWN/NWAPS setup files and simulation output this pipeline treats as
  fixed input. **Not included in this repository** — see [Data](#data).
- `data/interim/`, `data/processed/` — intermediate and analysis-ready derived datasets.
- `outputs/` — figures, tables, and figure-source data, split into `main/` and `supplementary/`.
- `validation/` — numerical checks on pipeline output (internal consistency, and comparison
  against a prior implementation's saved output where a local fixture is available — see
  `validation/README.md`).
- `docs/provenance.md` — data lineage and the rationale for the GitHub/Zenodo split.
- `renv.lock` — pinned R package versions (see [Requirements](#requirements)).

## Requirements

R (built against 4.6.0) and [`renv`](https://rstudio.github.io/renv/) for package management.
No system dependencies beyond a standard R/RStudio installation are required to run the
pipeline against already-downloaded data (running Daisy/SPAWN itself is out of scope here —
see [Data](#data)).

## Data

The Daisy/SPAWN/NWAPS setup files, simulation inputs, and simulation output this pipeline
consumes are archived on Zenodo under a CC BY 4.0 license:

> **[DOI: 10.5281/zenodo.TODO]** — TODO: add once minted.

Download and place the contents at `data/raw/` (matching the structure Zenodo documents) before
running the pipeline. See [`docs/provenance.md`](docs/provenance.md) for the exact folder
layout, lineage, and an integrity-check snippet against the included SHA-256 manifest.

## Reproduction

```r
install.packages("renv")
renv::restore()
```

Then, with the Zenodo data in place at `data/raw/` (see [Data](#data)):

```r
targets::tar_make()
```

Only outdated targets rebuild. Figures land in `outputs/figures/main/` and
`outputs/figures/supplementary/`; their source data lands alongside as CSVs in
`outputs/figure_data/`; tables land in `outputs/tables/`.

To inspect a built figure interactively (`targets::tar_read(fig_02_plot)` etc.) rather than
just opening the saved file in `outputs/figures/`, load the plotting packages the figure uses
*first* — `tar_make()` loads them automatically while building, but `tar_read()` only
deserializes the stored object, so printing it in a fresh session needs the same packages
loaded to render any custom theme/geom classes (e.g. `ggtext::element_markdown()`, `ggpattern`
fills):

```r
library(ggplot2)
library(ggtext)
library(ggpattern)
tar_read(fig_02_plot)
```

## Validation

`validation/` holds scripts that check pipeline output for internal numerical consistency and,
where a legacy-project fixture is available locally, against a prior implementation's saved
output (`validation/compare_fig04_numeric.R`; see `validation/README.md` for how that fixture
is sourced — it isn't committed here, as it's a large local-only comparison aid).

## Citation

See [`CITATION.cff`](CITATION.cff) for both the software citation (this repository) and the
preferred citation for the associated manuscript.

## License

Code in this repository is released under the [MIT License](LICENSE). The archived simulation
data on Zenodo is released separately under CC BY 4.0 (see [Data](#data)).

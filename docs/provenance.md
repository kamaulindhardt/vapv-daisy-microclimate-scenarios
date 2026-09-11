# Data provenance and the code/data split

This repository holds the **code** used to turn Daisy/SPAWN/NWAPS simulation
output into the manuscript's figures and tables. It does not hold the
simulation data itself. This note says where that data lives, how it relates
to what's here, and how to check a download is intact.

## Where the data is

The Daisy model setup files, simulation inputs, and simulation output are
archived on Zenodo:

> **[DOI: 10.5281/zenodo.TODO]** — TODO: add once minted, under CC BY 4.0.

That deposit contains what would otherwise live in `data/raw/` in this
repository (excluded here via `.gitignore` — see below):

| Folder | Contents |
|---|---|
| `spawn_common_setup_files/` | Daisy `.dai` crop, rotation, fertilisation and soil-column setup files, and the `WEATHER/` drivers |
| `nwaps_full_run/` | NWAPS-aggregated Daisy/SPAWN simulation output that this pipeline treats as fixed input |
| `gc_calibration/` | Grass-clover calibration results |
| `MANIFEST.sha256.csv` | SHA-256 checksum, size, and source timestamp for every file above |

## Where it came from

`data/raw/` was copied from a companion Daisy/SPAWN modelling project
(`DAISY_modelling_agrivoltaics`) — specifically
`SPAWN_NWAPS_DAISY/SPAWN_NWAPS_RUN_OUTPUT/NWAPS_FULL_RUN/` (85 files, 19.78 GB,
written 2026-06-30) plus `SPAWN_COMMON_SETUP_FILES/` (crop calibration `.dai`
files and weather drivers). Running Daisy/SPAWN itself is out of scope for
this pipeline (`targets::tar_make()`) — the raw output is treated as fixed
input, the way a measured dataset would be.

## Reproducing a figure from scratch

1. Download the Zenodo deposit above.
2. Verify integrity against the manifest it includes, e.g. in R:
   ```r
   manifest <- read.csv("data/raw/MANIFEST.sha256.csv")
   actual <- vapply(file.path("data/raw", manifest$relative_path),
                     \(f) toupper(digest::digest(f, algo = "sha256", file = TRUE)),
                     character(1))
   stopifnot(all(actual == manifest$sha256))
   ```
3. Place the extracted contents at `data/raw/` in a clone of this repository
   (matching the folder names in the table above).
4. `renv::restore()` then `targets::tar_make()` — see the main [README](../README.md).

## Why the split

Static, large (tens of GB) simulation data doesn't fit well in a Git
repository — GitHub has a 100 MB per-file limit without Git LFS, and no
mechanism for a permanent, citable dataset DOI on its own. Zenodo is built for
exactly that: large-file hosting, a permanent DOI, dataset-level metadata, and
a license (CC BY 4.0) independent of the code license (MIT, see
[LICENSE](../LICENSE)). Code, in turn, benefits from GitHub's version control
and issue tracking in a way Zenodo doesn't support. Hence: code here, data on
Zenodo, cross-referenced by DOI in both directions and in the manuscript's
Data Availability Statement.

## Data tiers excluded from this repository

Per `.gitignore`:

- `data/raw/` — see above.
- `data/interim/` — intermediate, regenerable by `tar_make()` from `data/raw/`.
- `_targets/` — the `targets` build cache, machine-specific and fully
  regenerable.
- `tests/fixtures/legacy_baseline/` — ground-truth fixtures used only to
  validate this pipeline's port against the companion project's last run;
  see `validation/README.md`.

`data/processed/` (analysis-ready, small, derived tables) is not excluded and
is tracked normally once populated.

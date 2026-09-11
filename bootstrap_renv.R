# One-time environment bootstrap for vapv-daisy-microclimate-scenarios.
# Installs renv + the package set the ported R/ functions will need (per the
# library() audit of the legacy scripts), snapshots the lockfile, then smoke-tests
# that targets::tar_make() runs cleanly on the (currently empty) _targets.R.

proj <- getwd()  # run this from the project root (e.g. via the .Rproj)

cat("=== renv bootstrap started", format(Sys.time()), "===\n")

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

renv::init(project = proj, bare = TRUE)

pkgs <- c(
  # "qs" dropped: no build available for this R version/platform (confirmed
  # twice). "qs2" (its actively-maintained successor) installs fine but the
  # installed targets (1.12.0) rejects it: "unsupported format: qs2" - format
  # string support wasn't added until a later targets release. _targets.R
  # uses format = "rds" instead, which works everywhere; revisit only if
  # target-cache size/speed becomes an actual bottleneck.
  "targets", "tarchetypes", "testthat",
  "tidyverse", "data.table", "janitor", "fs", "glue", "here", "cli",
  "gt", "scales", "patchwork", "ggtext", "slider", "ggpattern",
  "ggridges", "ggrepel", "SPEI", "lubridate", "progress", "processx",
  "progressr"
)

renv::install(pkgs, project = proj)
renv::snapshot(project = proj, prompt = FALSE)

cat("=== renv bootstrap complete", format(Sys.time()), "===\n")

targets::tar_make()

cat("=== tar_make() smoke test complete", format(Sys.time()), "===\n")

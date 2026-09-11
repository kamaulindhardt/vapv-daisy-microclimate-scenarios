# Phase 3.4 smoke test: confirm the copied NWAPS output is complete and
# readable before any pipeline logic gets built on top of it. Deliberately
# lighter than literally re-running the legacy script's CFG/path-resolution
# machinery (which is tied to the old repo layout and is exactly what R/import.R
# is going to replace) - the goal here is "is the copy intact", not "does the
# legacy Rmd run unmodified from a new location".

library(data.table)

raw_dir <- "data/raw/nwaps_full_run"  # run from the project root (e.g. via the .Rproj)

check_file <- function(filename) {
  path <- file.path(raw_dir, filename)
  dt <- fread(path)
  cat(sprintf(
    "\n%s\n  dims: %d rows x %d cols\n  columns: %s\n",
    filename, nrow(dt), ncol(dt), paste(names(dt), collapse = ", ")
  ))
  invisible(dt)
}

harvest <- check_file("out_Annual-Harvest.csv")
n_apr <- check_file("out_Annual-FN_100cm_Apr.csv")

id_col <- intersect(c("run_id", "scenario_id", "id"), names(harvest))
if (length(id_col) > 0) {
  cat(sprintf(
    "\nDistinct %s values in harvest: %d\n",
    id_col[1], length(unique(harvest[[id_col[1]]]))
  ))
}

cat("\n=== smoke test complete: copied data is readable ===\n")

# Interactive figure development loop - NOT part of the pipeline (_targets.R
# does not source this file). Run this file once per session, then the loop
# for any figure is exactly one call:
#
#   1. Edit R/figures.R (e.g. tweak plot_fig04()).
#   2. preview_figure("fig04")
#   3. Repeat.
#
# preview_figure() re-sources the whole R/ folder itself every time (the same
# way _targets.R's tar_source("R") does - plot_figNN() functions depend on
# theme_manuscript() and palette objects defined in other R/ files, so
# sourcing figures.R alone is not enough), so step 2 always reflects your
# latest edit with no separate "now re-source" step to remember.
# It reads its data via tar_load_raw(), a fast disk read of the already-built
# target (not a recompute), and opens it at the figure's real save
# proportions from figure_specs (R/figures.R), so text doesn't overlap the
# way it does in RStudio's default Plots pane.
#
# Works for any figure without editing this file: preview_figure("fig06")
# will work as soon as R/figures.R defines plot_fig06() and figure_specs$fig06,
# and fig06_data is a real target in _targets.R.
#
# Once happy, run targets::tar_make() to regenerate the pipeline's official
# cached target + saved PNG/PDF/CSV, and confirm only what you'd expect reran.

library(targets)
library(ggplot2)
library(ggtext)     # Figure 4
library(ggpattern)  # Figure 5

preview_figure <- function(fig_id) {
  tar_source("R")

  data_name <- paste0(fig_id, "_data")
  plot_fn <- get(paste0("plot_", fig_id))
  tar_load_raw(data_name, envir = environment())

  spec <- figure_specs[[fig_id]]
  if (is.null(spec)) stop("No figure_specs entry for '", fig_id, "' - add one in R/figures.R.")

  dev.new(width = spec$width, height = spec$height)
  print(plot_fn(get(data_name)))
}

preview_all_main_figures <- function() {
  invisible(lapply(names(figure_specs), preview_figure))
}

# --- Usage -----------------------------------------------------------------
preview_figure("fig04")
preview_figure("fig05")
# preview_all_main_figures()   # opens every main figure at once

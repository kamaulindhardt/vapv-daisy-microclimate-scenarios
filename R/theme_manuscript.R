# Shared manuscript design system: theme, factor ordering, and export.
#
# Extracted from reference/legacy_snapshot/SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd,
# chunks "fig-setup-output-dirs" and "fig-setup-step17a-helpers"
# (theme_pub_daisy_17, save_ms_fig/save_pub_plot_17, the shared factor-level
# constants). Logic unchanged; renamed and relocated per the reconstruction
# plan (Phase 7) - the baseline-reference-join logic that lived alongside
# these in the legacy 05_plotting.R has been moved to clean.R instead, since
# it's a data-transformation concern shared by every domain, not a plotting
# concern.

theme_manuscript <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey92", colour = NA),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.15)),
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      plot.caption = ggplot2::element_text(colour = "grey40", hjust = 0)
    )
}

weather_factor_levels <- c("Baseline", "Radiation", "Wind", "Temperature")

management_levels <- c(
  "Mineral fertiliser | Residue Removed",
  "Mineral fertiliser | Residue Retained",
  "Biogas digestate | Residue Removed",
  "Biogas digestate | Residue Retained"
)

strip_position_levels <- c("Open field", "West", "Center", "East")

driver_palette <- c("Radiation" = "#E69F00", "Wind" = "#0072B2", "Temperature" = "#D55E00")

fertiliser_palette <- c("Mineral fertiliser" = "#998EC3", "Biogas digestate" = "#F5A641")

# Crop palette, carried over unchanged from the legacy script so crop colours
# stay identical to every previously-circulated figure. Okabe-Ito-adjacent and
# colour-blind safe; greyscale separation is handled by redundant encoding
# (facets/shape), not by these hues alone.
crop_palette <- c(
  "Winter Wheat" = "#F1A983",
  "Spring Barley" = "#FFD966",
  "Grass-Clover" = "#4EA72E",
  "Ryegrass (undersown)" = "#4EA78E",
  "Soybean" = "#44B3E1"
)

# Compact axis labels: the internal scenario keys ("Tmp +0.5degC", "Rad -20%")
# are unambiguous but too long for an axis. Strips the driver prefix (shown in
# the facet strip instead) and renders degrees properly.
scen_axis_label <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "Reference" ~ "Ref",
    stringr::str_starts(x, "Rad ") ~ stringr::str_remove(x, "^Rad "),
    stringr::str_starts(x, "Wind ") ~ stringr::str_remove(x, "^Wind "),
    stringr::str_starts(x, "Tmp ") ~ stringr::str_remove(x, "^Tmp ") |> stringr::str_replace("degC", "°C"),
    TRUE ~ x
  )
}

prep_pub_factors <- function(df) {
  out <- df
  if ("weather_factor" %in% names(out)) out$weather_factor <- factor(out$weather_factor, levels = weather_factor_levels)
  if ("management_label" %in% names(out)) out$management_label <- factor(out$management_label, levels = management_levels)
  if ("rotation" %in% names(out)) out$rotation <- factor(out$rotation)
  if ("strip_position" %in% names(out)) out$strip_position <- factor(out$strip_position, levels = strip_position_levels)
  if ("weather_label" %in% names(out)) out$weather_label <- factor(out$weather_label)
  out
}

# Standardised manuscript figure export: PNG (quick preview) + PDF (vector,
# editable), both at the given width/height/dpi. Matches the legacy
# save_ms_fig()/save_pub_plot_17() pattern (500 DPI, PNG+PDF pair) - that
# part of the legacy design was already correct, not a source of the
# stalling/duplication problems, so it's carried forward unchanged.
save_manuscript_plot <- function(plot, filename, dir, width = 11, height = 7, units = "in", dpi = 500) {
  fs::dir_create(dir, recurse = TRUE)
  path_png <- file.path(dir, filename)
  ggplot2::ggsave(filename = path_png, plot = plot, width = width, height = height, units = units, dpi = dpi)
  path_pdf <- sub("\\.png$", ".pdf", path_png)
  ggplot2::ggsave(filename = path_pdf, plot = plot, width = width, height = height, units = units, device = "pdf")
  invisible(c(png = path_png, pdf = path_pdf))
}

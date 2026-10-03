# Manuscript figure plotting functions. Each figure consumes an explicit,
# already-prepared dataset (from productivity.R etc.) - no data transformation
# happens in this file, per the reconstruction plan's "no heavy processing in
# figure code" rule.
#
# figure_specs is the single source of truth for each figure's save
# dimensions - both _targets.R's save_manuscript_plot() calls and
# dev_figures.R's interactive preview helpers read from here, so the two
# can't silently drift apart. Add an entry here whenever a new fig_NN gets
# a plot_figNN() function.
figure_specs <- list(
  fig_02 = list(width = 11, height = 8),
  fig_03 = list(width = 12, height = 7.5),
  fig_04 = list(width = 11, height = 5),
  fig_05 = list(width = 10, height = 6.6),
  fig_06 = list(width = 10, height = 8.5),
  fig_07 = list(width = 11, height = 4.6),
  fig_08 = list(width = 11, height = 10.5),
  # Methods figure (site context + rotation design); see R/methods_figure.R
  fig_02_mm = list(width = 12, height = 12),
  # -------------------------------------------------------------------------
  # Supplementary, in manuscript citation order. S02 and S03 (microclimate
  # model validation against HOBOnet sensor observations) have no entry: they
  # belong to the weather-generation step upstream of this pipeline, which
  # consumes its .dwf output as fixed input. The gap is expected, not missing
  # work - see docs/workflow.md.
  # -------------------------------------------------------------------------
  # Methods
  fig_s01 = list(width = 11, height = 10),   # weather + crop calendar
  fig_s04 = list(width = 11, height = 13),   # soybean parameterisation
  fig_s05 = list(width = 11, height = 4.2),  # grass-clover calibration
  # Results 3.1 - productivity
  fig_s06 = list(width = 11, height = 9),    # rotation yield composition
  fig_s07 = list(width = 12, height = 4.2),  # wind: hydrological not agronomic
  fig_s08 = list(width = 11, height = 12),   # wind vs radiation/temperature
  # Results 3.2 - water
  fig_s09 = list(width = 11, height = 7),    # canopy + cumulative water use
  fig_s10 = list(width = 12, height = 7),    # annual water balance components
  # Results 3.2 - nitrogen
  fig_s11 = list(width = 11, height = 11),   # N fluxes, all managements
  fig_s12 = list(width = 11, height = 9),    # nitrogen budget
  fig_s13 = list(width = 12, height = 9),    # seasonal nitrate leaching
  # Results 3.2 - soil organic carbon
  fig_s14 = list(width = 11, height = 11),   # SOC dynamics + crop calendar
  fig_s15 = list(width = 11, height = 9),    # total SOC by depth
  fig_s16 = list(width = 10, height = 8),    # SOC change by rotation phase
  # Results 3.3 - trade-offs and cross-cutting
  fig_s17 = list(width = 11, height = 8),    # yield-SOC trade-off
  fig_s18 = list(width = 10, height = 7),    # substrip position effects
  # Discussion
  fig_s19 = list(width = 9, height = 5.5),   # permutation vs scenario effect
  fig_s20 = list(width = 12, height = 10),   # yield stability / downside risk
  fig_s21 = list(width = 11, height = 12),   # SOC distance from equilibrium
  # Added after the 2026-08-22 renumbering (which stopped at S21); appended
  # rather than slotted into Results 3.2 (N) to avoid re-running that
  # renumbering for one figure - see docs/workflow.md.
  fig_s24 = list(width = 11, height = 12.5), # seasonal N-leaching dynamics + crop calendar
  fig_s25 = list(width = 11, height = 13),   # N-supply mechanism: ladder vs substrip
  # Supplement v2 composite (plot_fig_s07_v2): a 9:16 page-shaped canvas saved at
  # 320 dpi. Both matter - the panels' text is drawn at native size, so changing
  # either changes how crowded the composite looks.
  fig_s07_v2 = list(width = 9, height = 16, dpi = 320),
  # ... and the same figure in two parts, panels (a)-(c) and (d)-(f), each on a
  # canvas shaped for the Word supplement's 6 x 9 in text block: the 9 in width
  # scales to 5.95 in on the page, so 12.5 in of height becomes 8.3 in, and 8.1 in
  # becomes 5.35 in, which leaves the caption room on the second page.
  fig_s07_v2_part1 = list(width = 9, height = 12.5, dpi = 320),
  fig_s07_v2_part2 = list(width = 9, height = 8.1, dpi = 320),
  # Retired legacy-numbered figures (not manuscript figures - see productivity.R)
  fig04 = list(width = 14, height = 4.5),
  fig05 = list(width = 10, height = 8)
)

# ===========================================================================
# MANUSCRIPT FIGURE 2 - Crop productivity response to radiation, temperature
# and wind. (Manuscript v12 numbering: this was Figure 3 before the
# restructure; see docs/workflow.md for the full old -> new figure mapping.)
#
# One message: radiation is the dominant productivity constraint, temperature
# is secondary at system level but larger and asymmetric per crop, wind is
# negligible.
#
# (a) absolute annualised harvested AGB per crop across all three drivers
# (b) grain-yield response (% of the open-field baseline) for WW + SY
#
# Caption discipline (co-author feedback): the caption states what is plotted,
# the aggregation, and what the error bars are - no mechanism, no
# interpretation. Those belong in the Results text.
# ===========================================================================

plot_fig_02_a_agb_by_crop <- function(fig_02_a_data, fig_02_a_system_data) {
  ggplot2::ggplot(
    fig_02_a_data,
    ggplot2::aes(x = scen_label, y = mean_resp, colour = crop_renamed, group = crop_renamed)
  ) +
    # Scenario-specific total ASY (all crops) as grey bars behind the per-crop
    # points. Drawn first so it sits underneath; mapped to fill so it gets its
    # own legend key next to the crop colours. No error bars on the bars: they
    # would sit on top of the crop whiskers at the same x. The SD across
    # rotations is in the source data (Fig_02_a_system_data.csv).
    ggplot2::geom_col(
      data = fig_02_a_system_data,
      ggplot2::aes(x = scen_label, y = mean_asy, fill = "Total ASY (all crops)"),
      width = 0.7, inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = c("Total ASY (all crops)" = "grey82"), name = "System") +
    # Mark the open-field anchor so "Ref" is visually distinct from the VAPV
    # 0-level immediately to its right - reviewers asked for these to be
    # unambiguous.
    ggplot2::geom_vline(xintercept = 1.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
    # No lines connecting scenario levels: each level is an independent
    # simulation run, not a repeated measure on a continuum, so a connecting
    # line would imply interpolation between points that were never simulated.
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_resp - sd_resp, ymax = mean_resp + sd_resp),
      width = 0.18, linewidth = 0.4, alpha = 0.8
    ) +
    ggplot2::geom_point(size = 1.9) +
    ggplot2::scale_colour_manual(values = crop_palette, name = "Crop") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(
      name = expression("Harvested AGB (t DM ha"^-1~"yr"^-1*")"),
      breaks = seq(0, 25, 2.5)
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1),
                    fill = ggplot2::guide_legend(order = 2)) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, tag = "a") +
    theme_manuscript() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.3, colour = "grey90"),
      legend.position = "right"
    )
}

plot_fig_02_b_grain_response <- function(fig_02_b_data) {
  ggplot2::ggplot(
    fig_02_b_data,
    ggplot2::aes(x = scen_label, y = mean_resp, fill = crop_renamed, pattern = residue_policy)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
    # Separate the open-field Ref from the VAPV 0-level, as in panel (a).
    ggplot2::geom_vline(xintercept = 1.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
    ggpattern::geom_col_pattern(
      position = ggplot2::position_dodge(width = 0.8), width = 0.78,
      colour = "grey25", linewidth = 0.15,
      pattern_fill = "white", pattern_colour = "white",
      pattern_density = 0.45, pattern_spacing = 0.03, pattern_angle = 45
    ) +
    ggpattern::scale_pattern_manual(
      values = c("Residue removed" = "none", "Residue retained" = "stripe"),
      name = "Residue"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_resp - sd_resp, ymax = mean_resp + sd_resp,
                   group = interaction(crop_renamed, residue_policy)),
      position = ggplot2::position_dodge(width = 0.8), width = 0.22,
      linewidth = 0.35, colour = "grey20"
    ) +
    ggplot2::scale_fill_manual(values = crop_palette, name = "Crop") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(
      breaks = seq(-100, 60, 10),
      labels = scales::label_number(suffix = "%", accuracy = 1)
    ) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, y = "Grain yield response\n(% of open field)", tag = "b") +
    theme_manuscript() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.3, colour = "grey90"),
      legend.position = "right"
    )
}

# ===========================================================================
# MANUSCRIPT FIGURE 3 - Seasonal biomass and grain accumulation under drought.
# (v12 draft Figure 4.) One panel per crop: (a) winter wheat, (b) soybean,
# (c) spring barley, (d) grass-clover.
#
# One message: the cost of shading depends on when drought falls relative to
# phenology, and that dependence differs between crops.
#
# Lines are correct here (unlike the scenario-level figures): the x-axis is a
# real continuum - one growing season traced day by day.
# ===========================================================================

# Per-crop panel configuration: how far the season runs, where the
# phenological stages sit, and what gets annotated at the end of each series.
#
# Stage windows for winter wheat and soybean are carried over unchanged from
# reference/legacy_snapshot/SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd (ww_rects_18H /
# sy_rects_18H). Grass-clover and spring barley had no stage definitions there
# and are set here from their simulated growth windows.
#
# `annotate`:
#   "grain" - label final grain yield (GY); the marketable product for WW/SY
#   "agb"   - label final AGB; spring barley is cut green, so AGB is the product
#   "cuts"  - label AGB at every cut peak; the multi-cut ley
fig_03_crop_config <- list(
  "Winter Wheat" = list(
    doy_range = c(1, 245), stage_fill = c("#74C476", "#FDAE6B"),
    stages = list(c(60, 130), c(150, 220)),
    stage_labels = c("Spring growth", "Grain filling"),
    annotate = "grain"
  ),
  "Soybean" = list(
    doy_range = c(121, 288), stage_fill = c("#74C476", "#FDAE6B"),
    stages = list(c(152, 212), c(212, 260)),
    stage_labels = c("Vegetative", "Reproductive"),
    annotate = "grain"
  ),
  "Grass-Clover" = list(
    doy_range = c(60, 305), stage_fill = c("#74C476", "#FDAE6B"),
    stages = list(c(60, 130), c(130, 275)),
    stage_labels = c("Spring growth", "Peak production"),
    annotate = "cuts"
  ),
  "Spring Barley" = list(
    doy_range = c(60, 240), stage_fill = c("#74C476", "#FDAE6B"),
    stages = list(c(90, 150), c(150, 215)),
    stage_labels = c("Establishment", "Stem ext. / grain fill"),
    annotate = "agb"
  )
)

fig_03_scenario_cols <- c("Open field" = "grey15", "VAPV (Rad 0%)" = "#E15A0C")

# End-of-series labels. For single-harvest crops this is one point per series;
# for the grass-clover ley it is the AGB at each cut peak, found the same way
# harvests are detected upstream (a >50% day-over-day drop).
fig_03_series_endpoints <- function(crop_data, mode) {
  if (mode == "cuts") {
    crop_data |>
      dplyr::group_by(series, scenario, year_class) |>
      dplyr::arrange(day_of_year, .by_group = TRUE) |>
      dplyr::mutate(.is_cut = !is.na(dplyr::lead(agb_MgDM_ha)) &
                      dplyr::lead(agb_MgDM_ha) < 0.5 * agb_MgDM_ha & agb_MgDM_ha > 1) |>
      dplyr::filter(.is_cut | dplyr::row_number() == dplyr::n()) |>
      dplyr::mutate(label = sprintf("%.1f", agb_MgDM_ha)) |>
      dplyr::ungroup()
  } else {
    value_col <- if (mode == "grain") "grain_MgDM_ha" else "agb_MgDM_ha"
    prefix <- if (mode == "grain") "GY = " else "AGB = "
    crop_data |>
      dplyr::group_by(series, scenario, year_class) |>
      dplyr::slice_max(day_of_year, n = 1, with_ties = FALSE) |>
      dplyr::mutate(label = paste0(prefix, sprintf("%.1f", .data[[value_col]]))) |>
      dplyr::ungroup()
  }
}

plot_fig_03_panel <- function(fig_03_data, crop, panel_tag, contrast_years,
                               show_legend = FALSE) {
  cfg <- fig_03_crop_config[[crop]]
  d <- fig_03_data |>
    dplyr::filter(crop_renamed == crop,
                  day_of_year >= cfg$doy_range[1], day_of_year <= cfg$doy_range[2])
  if (nrow(d) == 0) return(patchwork::plot_spacer())

  years <- contrast_years |> dplyr::filter(crop_renamed == crop)
  dry_year <- years$year[years$year_class == "Dry"][1]
  normal_year <- years$year[years$year_class == "Near-normal"][1]
  crop_display <- if (crop == "Spring Barley") "Spring Barley (green harvest)" else crop

  ends <- fig_03_series_endpoints(d, cfg$annotate)
  stage_df <- tibble::tibble(
    xmin = vapply(cfg$stages, \(s) s[1], numeric(1)),
    xmax = vapply(cfg$stages, \(s) s[2], numeric(1)),
    fill = cfg$stage_fill, label = cfg$stage_labels
  )

  month_breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  ggplot2::ggplot(d, ggplot2::aes(x = day_of_year, y = agb_MgDM_ha,
                                   colour = scenario, linetype = year_class, group = series)) +
    ggplot2::geom_rect(
      data = stage_df, inherit.aes = FALSE,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = I(fill)),
      alpha = 0.16
    ) +
    ggplot2::geom_text(
      data = stage_df, inherit.aes = FALSE,
      ggplot2::aes(x = (xmin + xmax) / 2, y = Inf, label = label),
      vjust = 1.4, size = 2.6, fontface = "italic", colour = "grey35"
    ) +
    ggplot2::geom_line(linewidth = 0.65) +
    # Filled marker = near-normal season, open marker = dry season, matching
    # the solid/dashed line coding so the two are redundant rather than
    # competing encodings.
    ggplot2::geom_point(
      data = ends, ggplot2::aes(shape = year_class), size = 1.9, stroke = 0.7, fill = "white"
    ) +
    ggrepel::geom_text_repel(
      data = ends, ggplot2::aes(label = label),
      size = 2.4, hjust = 0, direction = "y", nudge_x = 6,
      segment.size = 0.2, segment.alpha = 0.5, min.segment.length = 0.2,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = fig_03_scenario_cols, name = "Weather case") +
    ggplot2::scale_linetype_manual(values = c("Near-normal" = "solid", "Dry" = "22"),
                                    name = "Year type") +
    # 16 = filled, 21 = open (white-filled). Redundant with the solid/dashed
    # line coding so the near-normal/dry contrast survives in greyscale.
    ggplot2::scale_shape_manual(values = c("Near-normal" = 16, "Dry" = 21), guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = month_breaks, labels = month_labels,
      limits = cfg$doy_range, expand = ggplot2::expansion(mult = c(0.01, 0.10))
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.14))) +
    ggplot2::labs(
      x = NULL, y = expression("AGB (t DM ha"^-1*")"), tag = panel_tag,
      title = sprintf("%s — dry: %s | near-normal: %s", crop_display, dry_year, normal_year)
    ) +
    theme_manuscript() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 9),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7),
      axis.text.y = ggplot2::element_text(size = 7),
      axis.title.y = ggplot2::element_text(size = 8),
      panel.grid.major.x = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
      legend.position = if (show_legend) "bottom" else "none",
      legend.box = "horizontal",
      plot.tag = ggplot2::element_text(face = "bold", size = 11)
    )
}

plot_fig_03 <- function(fig_03_data, contrast_years) {
  ww <- plot_fig_03_panel(fig_03_data, "Winter Wheat", "a)", contrast_years, show_legend = TRUE)
  sy <- plot_fig_03_panel(fig_03_data, "Soybean", "b)", contrast_years)
  gc <- plot_fig_03_panel(fig_03_data, "Grass-Clover", "c)", contrast_years)
  sb <- plot_fig_03_panel(fig_03_data, "Spring Barley", "d)", contrast_years)

  # a/b stacked left, c/d stacked right - grain crops together, forage and
  # green-cut cereal together.
  ((ww / sy) | (gc / sb)) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom", legend.box = "horizontal")
}

# ===========================================================================
# MANUSCRIPT FIGURE 4 - Soil water during critical dry periods.
# (v12 draft Figure 5, panel a only; 5b and 5c move to Supplementary.)
#
# One message: VAPV shifts soil water availability in opposite directions for
# winter wheat and soybean, depending on when each crop's critical window
# falls.
#
# Single panel by design. The v12 draft combined this with AET/WUE-vs-wind and
# drainage-vs-radiation, which are different metrics answering different
# questions - the reviewer objection was specifically to that combination.
#
# No connecting lines: scenario levels are independent simulation runs.
# ===========================================================================

plot_fig_04 <- function(fig_04_data) {
  crop_cols <- c("Winter Wheat" = crop_palette[["Winter Wheat"]],
                 "Soybean" = crop_palette[["Soybean"]])

  ggplot2::ggplot(
    fig_04_data,
    ggplot2::aes(x = scen_label, y = mean_pct_change,
                 colour = crop_renamed, shape = crop_renamed)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_pct_change - sd_pct_change,
                   ymax = mean_pct_change + sd_pct_change),
      width = 0.2, linewidth = 0.4, alpha = 0.8,
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::scale_colour_manual(values = crop_cols, name = "Crop") +
    ggplot2::scale_shape_manual(values = c("Winter Wheat" = 16, "Soybean" = 17), name = "Crop") +
    ggplot2::scale_x_discrete(labels = \(x) stringr::str_remove(x, "^(Rad|Wind|Tmp) ") |>
                                 stringr::str_replace("degC", "°C")) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, y = "Change in soil matrix water\n(% of open field)") +
    theme_manuscript() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "right"
    )
}

# ===========================================================================
# MANUSCRIPT FIGURE 5 - Nitrogen-cycle fluxes and leaching.
# (v12 draft Figure 6; "Figure 6" in the submitted-manuscript numbering, which
# counts the Figure 1 study-design schematic.)
#
# One message: radiation and temperature restructure the whole nitrogen cycle,
# not leaching alone - fixation is the most radiation-sensitive flux, while
# temperature drives the leaching response.
#
# The supply/demand fluxes (biogas digestate, residue removed) and the leaching
# they feed share ONE panel per driver, so they read as cause and consequence.
# Fluxes use the left y axis; leaching, for all four fertilisation x residue
# regimes, uses a secondary (right) axis. Co-author feedback asked for the former
# panels (a) and (b) to be merged and for every management option to be shown;
# the fourth regime, Mineral fertiliser | Residue Retained, had been left out of
# the leaching panel.
#
# HOW THE SECONDARY AXIS WORKS. ggplot2 can only rescale an axis, not plot a
# second variable on its own, so leaching is PLOTTED at fig_05_leach_scale x its
# value on the left scale and the right axis divides it back. The exported source
# tables (Fig_05_a_data.csv, Fig_05_b_data.csv) stay in real kg N/ha/yr; only the
# drawing is rescaled. The factor is a free choice. 3 keeps leaching in a band of
# its own beneath the fluxes (shaded, so it is clear which axis applies); 5 or
# more puts the leaching markers on top of the flux markers. The price is that
# leaching gets about a third of the vertical resolution it had as a separate
# panel, and that vertical distances are not comparable between the two axes -
# the caption says so. Even at 3, fixation dips into the leaching band at four
# levels (Rad -20 / -30 %, Tmp -3 / -2 degC).
#
# All series are dodged as ONE group: the three fluxes sit left of each tick and
# the leaching regimes right of it, so the left cluster belongs to the left axis
# and the right cluster to the right axis. The two-column legend gets its
# headings from pseudo-levels ("hdr_*", "blank") that hold no data: drop = FALSE
# keeps them in the legend and their keys are transparent. (One colour scale
# cannot produce two separate legends, and ggnewscale is not in this project.)
#
# No connecting lines between scenario levels - independent simulation runs.
# ===========================================================================

# Regime encodings, shared with Fig. S11 panel (b) so a regime looks the same in
# both. The three regimes the leaching series always showed keep their colours and
# shapes; Mineral | Residue Retained takes the dark purple it has in the other
# four-regime figures, with a diamond because the other three shapes are taken.
fig_05_regime_cols <- c("Mineral fertiliser | Residue Removed" = "#998EC3",
                        "Mineral fertiliser | Residue Retained" = "#542788",
                        "Biogas digestate | Residue Removed" = "#F5A641",
                        "Biogas digestate | Residue Retained" = "#B35806")
fig_05_regime_shapes <- c("Mineral fertiliser | Residue Removed" = 16,
                          "Mineral fertiliser | Residue Retained" = 18,
                          "Biogas digestate | Residue Removed" = 17,
                          "Biogas digestate | Residue Retained" = 15)

fig_05_leach_scale <- 3      # leaching is drawn at 3 x its value; right axis = left axis / 3
fig_05_leach_axis_max <- 40  # top of the right axis, kg N/ha/yr (the data reach ~34 with +1 SD)
fig_05_flux_axis_max <- 350  # top of the left axis, kg N/ha/yr

plot_fig_05 <- function(fig_05_a_data, fig_05_b_data) {
  k <- fig_05_leach_scale
  band_top <- fig_05_leach_axis_max * k

  flux_cols <- c("Mineralisation" = "#8C6D31",
                 "Crop N uptake" = "#4EA72E",
                 "Biological N fixation" = "#0072B2")
  flux_shapes <- c("Mineralisation" = 16, "Crop N uptake" = 17, "Biological N fixation" = 15)
  regimes <- levels(fig_05_b_data$management_label)

  fluxes <- fig_05_a_data |>
    dplyr::transmute(series = as.character(flux), scen_label, driver, y = mean_kgN_ha,
                     lo = mean_kgN_ha - sd_kgN_ha, hi = mean_kgN_ha + sd_kgN_ha)
  leaching <- fig_05_b_data |>
    dplyr::transmute(series = as.character(management_label), scen_label, driver,
                     y = k * mean_kgN_ha,
                     lo = k * (mean_kgN_ha - sd_kgN_ha), hi = k * (mean_kgN_ha + sd_kgN_ha))
  if (any(leaching$hi > band_top)) {
    warning("plot_fig_05(): a leaching error bar reaches above the right axis maximum (",
            fig_05_leach_axis_max, " kg N/ha/yr); raise fig_05_leach_axis_max.")
  }

  # "blank" pads the flux column (3 fluxes + heading) to the height of the
  # leaching column (4 regimes + heading) so the two legend columns line up.
  lv <- c("hdr_flux", names(flux_cols), "blank", "hdr_leach", regimes)
  d <- dplyr::bind_rows(fluxes, leaching) |>
    dplyr::mutate(series = factor(series, levels = lv))
  cols <- c(hdr_flux = "transparent", flux_cols, blank = "transparent",
            hdr_leach = "transparent", fig_05_regime_cols[regimes])
  shapes <- c(hdr_flux = NA, flux_shapes, blank = NA, hdr_leach = NA, fig_05_regime_shapes[regimes])
  labels <- c(hdr_flux = "**N fluxes** (left axis; biogas digestate, residue removed)",
              stats::setNames(names(flux_cols), names(flux_cols)),
              blank = "",
              hdr_leach = "**N leaching** (right axis; by management)",
              stats::setNames(regimes, regimes))
  dodge <- ggplot2::position_dodge(width = 0.86)

  ggplot2::ggplot(d, ggplot2::aes(x = scen_label, y = y, colour = series, shape = series)) +
    # The band marks where the RIGHT axis applies and its white lines are that
    # axis' gridlines. It is opaque so the left-axis gridline at 100 does not run
    # through the leaching values.
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = band_top, fill = "grey95") +
    ggplot2::geom_hline(yintercept = k * seq(10, fig_05_leach_axis_max - 10, 10),
                        colour = "white", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = band_top, colour = "grey75", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 1.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi),
                           width = 0.14, linewidth = 0.3, alpha = 0.75, position = dodge) +
    ggplot2::geom_point(size = 1.9, position = dodge) +
    ggplot2::scale_colour_manual(name = NULL, values = cols, breaks = lv, labels = labels, drop = FALSE) +
    ggplot2::scale_shape_manual(name = NULL, values = shapes, breaks = lv, labels = labels, drop = FALSE) +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(
      name = expression("N flux (kg N ha"^-1~"yr"^-1*")"),
      breaks = seq(0, 300, 100), limits = c(0, max(fig_05_flux_axis_max, d$hi)),
      expand = ggplot2::expansion(mult = c(0, 0.02)),
      sec.axis = ggplot2::sec_axis(~ . / k, name = expression("N leaching (kg N ha"^-1~"yr"^-1*")"),
                                   breaks = seq(0, fig_05_leach_axis_max, 10))
    ) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL) +
    theme_manuscript() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      # The right title reads top to bottom, so hjust = 1 puts it at the bottom,
      # beside the leaching band.
      axis.title.y.right = ggplot2::element_text(hjust = 1),
      legend.position = "bottom",
      legend.text = ggtext::element_markdown(size = 9),
      legend.key.height = ggplot2::unit(0.9, "lines")
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 5, byrow = FALSE),
                    shape = ggplot2::guide_legend(nrow = 5, byrow = FALSE))
}

# ===========================================================================
# MANUSCRIPT FIGURE 6 - Total soil organic carbon response.
# (v12 draft Figure 7.)
#
# One message: temperature drives large, bidirectional SOC change; radiation
# causes a smaller, one-directional decline.
#
# Total SOC = SOM1-C + SOM2-C + SOM3-C throughout. The legacy figure plotted
# the slow pool only (SOM2-C + SOM3-C) - see R/soc.R for why that matters.
#
# Panel (a) carries the time dimension, including that the trajectories are
# still diverging in 2024, i.e. not at equilibrium. Panel (b) carries the full
# scenario response at end of simulation. Lines are correct in (a) - a
# trajectory through time - and absent in (b), where levels are independent
# runs.
# ===========================================================================

# Diverging on temperature: cooling blue, warming red. The legacy palette used
# two blues for -3 and +3 degC, which made the figure's central contrast - that
# temperature moves SOC in OPPOSITE directions - the hardest thing on the plot
# to see.
soc_scenario_palette <- c(
  "Rad 0%" = "#FDBE85", "Rad -30%" = "#A63603",
  "Tmp -3degC" = "#2166AC", "Tmp +3degC" = "#B2182B"
)

plot_fig_06_a_trajectories <- function(fig_06_a_data) {
  ggplot2::ggplot(
    fig_06_a_data,
    ggplot2::aes(x = year, y = mean_delta_soc_pct, colour = scen_label, fill = scen_label)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = mean_delta_soc_pct - sd_delta_soc_pct,
                   ymax = mean_delta_soc_pct + sd_delta_soc_pct),
      alpha = 0.18, colour = NA
    ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = soc_scenario_palette, name = "Scenario",
                                  labels = scen_axis_label) +
    ggplot2::scale_fill_manual(values = soc_scenario_palette, name = "Scenario",
                                labels = scen_axis_label) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::facet_grid(depth ~ driver, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "ΔSOC (% of open field)", tag = "a") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                    legend.position = "right")
}

plot_fig_06_b_endsim <- function(fig_06_b_data) {
  mgmt_cols <- c(
    "Mineral fertiliser | Residue Removed" = "#998EC3",
    "Mineral fertiliser | Residue Retained" = "#542788",
    "Biogas digestate | Residue Removed" = "#F5A641",
    "Biogas digestate | Residue Retained" = "#B35806"
  )

  ggplot2::ggplot(
    fig_06_b_data,
    ggplot2::aes(x = scen_label, y = mean_delta_soc_pct,
                 colour = management_label, shape = management_label)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_delta_soc_pct - sd_delta_soc_pct,
                   ymax = mean_delta_soc_pct + sd_delta_soc_pct),
      width = 0.18, linewidth = 0.35, alpha = 0.7,
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_point(size = 1.9, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::scale_colour_manual(values = mgmt_cols, name = "Management") +
    ggplot2::scale_shape_manual(values = c(16, 17, 15, 18), name = "Management") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::facet_grid(depth ~ driver, scales = "free", space = "free_x") +
    ggplot2::labs(x = NULL, y = "ΔSOC at 2024 (% of open field)", tag = "b") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
                    legend.position = "right")
}

plot_fig_06 <- function(fig_06_a_data, fig_06_b_data) {
  patchwork::wrap_plots(
    plot_fig_06_a_trajectories(fig_06_a_data),
    plot_fig_06_b_endsim(fig_06_b_data),
    ncol = 1, heights = c(1, 1.1)
  )
}

# ===========================================================================
# MANUSCRIPT FIGURE 7 - Yield-nitrogen-leaching trade-off (synthesis).
# (v12 draft Figure 8.)
#
# One message: temperature is the only driver producing a genuine bidirectional
# trade-off; radiation is a joint decline rather than a trade-off; wind is
# negligible.
#
# Each point is one weather scenario x management combination, already averaged
# across the four rotations. Crosshairs mark open-field parity in both axes, so
# the quadrant a point falls in reads directly: upper-left = worse on both,
# lower-right = better on both.
#
# A reviewer (Jeroen) asked whether the grain-only panel (b) was needed; it was
# dropped, so the figure now shows the all-crop ASY AGB signal only. The grain
# data (fig_07_b_data) is still built because Supplementary S12 uses it.
# ===========================================================================

tradeoff_driver_palette <- c(
  "Open field" = "grey30",
  "Radiation" = "#B2182B",
  "Temperature" = "#2166AC",
  "Wind" = "#1B7837"
)

plot_fig_07 <- function(fig_07_a_data) {
  reference_lines <- build_tradeoff_reference_lines(fig_07_a_data)

  # Shaded region = favourable quadrant: yield at or above openfield parity and
  # N leaching at or below the management-matched openfield value.
  shade <- reference_lines |>
    dplyr::transmute(management_label, xmin = reference_yield_pct, xmax = Inf,
                     ymin = -Inf, ymax = reference_leaching_kgN_ha)

  ggplot2::ggplot(
    fig_07_a_data,
    ggplot2::aes(x = relative_yield_pct, y = leaching_kgN_ha,
                 colour = driver, shape = driver)
  ) +
    ggplot2::geom_rect(
      data = shade, inherit.aes = FALSE,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "#1B7837", alpha = 0.07
    ) +
    ggplot2::geom_vline(
      data = reference_lines,
      ggplot2::aes(xintercept = reference_yield_pct),
      linetype = "dashed", colour = "grey50", linewidth = 0.4
    ) +
    ggplot2::geom_hline(
      data = reference_lines,
      ggplot2::aes(yintercept = reference_leaching_kgN_ha),
      linetype = "dashed", colour = "grey50", linewidth = 0.4
    ) +
    ggplot2::geom_point(size = 2.1, alpha = 0.9) +
    ggplot2::geom_text(
      data = dplyr::mutate(shade, label = "higher yield,
less leaching"),
      inherit.aes = FALSE,
      ggplot2::aes(x = Inf, y = -Inf, label = label),
      hjust = 1.05, vjust = -0.4, size = 2.4, fontface = "italic",
      colour = "#1B7837", lineheight = 0.9
    ) +
    ggplot2::scale_colour_manual(values = tradeoff_driver_palette, name = "Driver",
                                  labels = c("Open field" = "Openfield")) +
    ggplot2::scale_shape_manual(values = c("Open field" = 15, "Radiation" = 16,
                                            "Temperature" = 17, "Wind" = 3), name = "Driver",
                                 labels = c("Open field" = "Openfield")) +
    ggplot2::scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::facet_wrap(~management_label, nrow = 1) +
    ggplot2::labs(
      x = "Relative all-crop ASY AGB (% of openfield)",
      y = expression("N leaching (kg N ha"^-1~"yr"^-1*")"),
      caption = "Shaded region = yield at or above openfield parity with N leaching at or below the openfield value."
    ) +
    theme_manuscript() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "right",
      strip.text = ggplot2::element_text(size = 8)
    )
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S7 - wind: hydrological effect, no agronomic effect.
# (a) FAO-56 aerodynamic fraction vs wind speed (analytical)
# (b) simulated ET0 / PET / AET across the wind-shelter ladder
# (c) per-crop AGB across the same ladder
# ===========================================================================

plot_fig_s07_a <- function(fig_s07_a_data) {
  ggplot2::ggplot(fig_s07_a_data$curve, ggplot2::aes(x = wind_m_s, y = 100 * aero_fraction)) +
    ggplot2::geom_line(linewidth = 0.8, colour = "grey20") +
    ggplot2::geom_vline(
      data = fig_s07_a_data$reference_speeds,
      ggplot2::aes(xintercept = wind_m_s, colour = label),
      linetype = "dashed", linewidth = 0.5
    ) +
    ggplot2::geom_point(
      data = fig_s07_a_data$reference_speeds,
      ggplot2::aes(x = wind_m_s, y = 100 * aero_fraction, colour = label), size = 2.4
    ) +
    ggrepel::geom_text_repel(
      data = fig_s07_a_data$reference_speeds,
      ggplot2::aes(x = wind_m_s, y = 100 * aero_fraction,
                   label = sprintf("%.1f m s⁻¹\n%.0f%%", wind_m_s, 100 * aero_fraction),
                   colour = label),
      size = 2.5, show.legend = FALSE, nudge_y = 8, min.segment.length = 0.2
    ) +
    ggplot2::scale_colour_manual(
      values = c("Open field" = "grey15", "VAPV centre (Wind 0%)" = "#1B7837"), name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::labs(x = expression("Wind speed at 2 m, "*u[2]*" (m s"^-1*")"),
                  y = "Aerodynamic share of ET"[0], tag = "a") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                    legend.position = "bottom", legend.direction = "vertical",
                    legend.margin = ggplot2::margin(t = 0), legend.box.spacing = ggplot2::unit(4, "pt"),
                    legend.key.size = ggplot2::unit(0.4, "cm"),
                    legend.text = ggplot2::element_text(size = 7),
                    legend.title = ggplot2::element_text(size = 8))
}

plot_fig_s07_b <- function(fig_s07_b_data) {
  ggplot2::ggplot(fig_s07_b_data,
                  ggplot2::aes(x = shelter_pct, y = mean_relative_pct,
                               colour = flux, shape = flux)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_relative_pct - sd_relative_pct,
                   ymax = mean_relative_pct + sd_relative_pct),
      width = 1.5, linewidth = 0.35, alpha = 0.7
    ) +
    ggplot2::geom_point(size = 2.1) +
    ggplot2::scale_colour_manual(
      values = c("Reference ET0" = "grey45", "Potential ET" = "#0072B2", "Actual ET" = "#D55E00"),
      name = NULL) +
    ggplot2::scale_shape_manual(values = c(15, 16, 17), name = NULL) +
    ggplot2::scale_x_continuous(labels = \(x) paste0("−", x, "%")) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::labs(x = "Wind shelter", y = "Change vs open field", tag = "b") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                    legend.position = "bottom", legend.direction = "vertical",
                    legend.margin = ggplot2::margin(t = 0), legend.box.spacing = ggplot2::unit(4, "pt"),
                    legend.key.size = ggplot2::unit(0.4, "cm"),
                    legend.text = ggplot2::element_text(size = 7),
                    legend.title = ggplot2::element_text(size = 8))
}

plot_fig_s07_c <- function(fig_s07_c_data) {
  ggplot2::ggplot(fig_s07_c_data,
                  ggplot2::aes(x = shelter_pct, y = mean_relative_pct, colour = crop_renamed)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = mean_relative_pct - sd_relative_pct,
                   ymax = mean_relative_pct + sd_relative_pct, fill = crop_renamed),
      alpha = 0.15, colour = NA
    ) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_colour_manual(values = crop_palette, name = "Crop") +
    ggplot2::scale_fill_manual(values = crop_palette, guide = "none") +
    ggplot2::scale_x_continuous(labels = \(x) paste0("−", x, "%")) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::labs(x = "Wind shelter", y = "AGB vs open field", tag = "c") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                    legend.position = "bottom", legend.direction = "vertical",
                    legend.margin = ggplot2::margin(t = 0), legend.box.spacing = ggplot2::unit(4, "pt"),
                    legend.key.size = ggplot2::unit(0.4, "cm"),
                    legend.text = ggplot2::element_text(size = 7),
                    legend.title = ggplot2::element_text(size = 8))
}

plot_fig_s07 <- function(fig_s07_a_data, fig_s07_b_data, fig_s07_c_data) {
  patchwork::wrap_plots(
    plot_fig_s07_a(fig_s07_a_data),
    plot_fig_s07_b(fig_s07_b_data),
    plot_fig_s07_c(fig_s07_c_data),
    nrow = 1
  )
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S8 - wind benchmarked against radiation and temperature.
# (b) demand-vs-use decomposition; (c) yield response range per driver.
#
# Panel (a) of the draft's S8 (soil water / pF / stress ladders) is not
# reproduced here - see docs/workflow.md.
# ===========================================================================

plot_fig_s08_b <- function(fig_s08_b_data) {
  ggplot2::ggplot(fig_s08_b_data, ggplot2::aes(x = wind_m_s, y = delta_mm, colour = term)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    ggplot2::geom_point(alpha = 0.10, size = 0.5) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE, linewidth = 0.8) +
    ggplot2::scale_colour_manual(
      values = c("ΔPET (demand)" = "#0072B2", "ΔAET (crop water use)" = "#D55E00"), name = NULL) +
    ggplot2::labs(
      x = expression("Open-field daily mean wind speed (m s"^-1*")"),
      y = "Open field − Wind −70% (mm day⁻¹)", tag = "b"
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                    legend.position = "bottom")
}

plot_fig_s08_c <- function(fig_s08_c_data) {
  ggplot2::ggplot(fig_s08_c_data,
                  ggplot2::aes(x = crop_renamed, y = mean_response, fill = driver)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_response - sd_response, ymax = mean_response + sd_response),
      position = ggplot2::position_dodge(width = 0.8), width = 0.25, linewidth = 0.35
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f", response_range),
                   y = mean_response + sign(mean_response) * (abs(sd_response) + 4)),
      position = ggplot2::position_dodge(width = 0.8), size = 2.2, colour = "grey25"
    ) +
    ggplot2::scale_fill_manual(values = driver_palette, name = "Driver") +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::labs(x = NULL, y = "Mean AGB response vs open field", tag = "c",
                  caption = "Annotated value = full response range across that driver's ladder (percentage points)") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1, size = 8),
                    legend.position = "bottom")
}

plot_fig_s08_a <- function(fig_s08_a_data) {
  labels <- levels(fig_s08_a_data$scen_label)
  ramp <- c("Reference" = "grey10",
            s09_driver_ramp(setdiff(labels, "Reference"), "Wind"))

  wilting <- tibble::tibble(variable = "Root-zone pF", y = WILTING_POINT_PF)

  ggplot2::ggplot(fig_s08_a_data,
                  ggplot2::aes(x = day_of_year, y = value,
                               colour = scen_label, group = scen_label)) +
    ggplot2::geom_hline(data = wilting, ggplot2::aes(yintercept = y),
                         linetype = "dashed", colour = "grey45", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::scale_colour_manual(values = ramp, name = "Wind shelter",
                                  labels = \(x) sub("^Wind ", "", x)) +
    ggplot2::facet_grid(variable ~ crop_renamed, scales = "free_y", switch = "y") +
    ggplot2::labs(x = "Day of year", y = NULL, tag = "a",
                   caption = "Dashed line in the pF panel = wilting point (pF 4.2). Each crop shown in its driest simulated season.") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7),
                    strip.placement = "outside",
                    strip.text.y.left = ggplot2::element_text(angle = 90, size = 7.5),
                    legend.position = "right")
}

plot_fig_s08 <- function(fig_s08_a_data, fig_s08_b_data, fig_s08_c_data) {
  patchwork::wrap_plots(
    plot_fig_s08_a(fig_s08_a_data),
    plot_fig_s08_b(fig_s08_b_data),
    plot_fig_s08_c(fig_s08_c_data),
    ncol = 1, heights = c(1.5, 1, 1.1)
  )
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S14 - continuous SOC dynamics with crop calendar.
# ===========================================================================

plot_fig_s14 <- function(fig_s14_data, crop_calendar, zoom_years = 1998:2004, tags = c("a", "b")) {
  scen_cols <- c(
    "Reference" = "grey10",
    "Rad 0%" = "#FDBE85", "Rad -10%" = "#FD8D3C", "Rad -30%" = "#A63603",
    "Tmp 0degC" = "#C6DBEF", "Tmp -3degC" = "#1A3D6F", "Tmp +3degC" = "#B2182B"
  )

  soc_panel <- function(d, tag, title) {
    ggplot2::ggplot(d, ggplot2::aes(x = date, y = total_soc_kgC_ha / 1000,
                                     colour = scen_label, group = scen_label)) +
      ggplot2::geom_line(linewidth = 0.45) +
      ggplot2::scale_colour_manual(values = scen_cols, name = "Scenario",
                                    labels = scen_axis_label) +
      ggplot2::scale_y_continuous(name = expression("Total SOC, 0–30 cm (t C ha"^-1*")")) +
      # Independent y-axes per driver family: the temperature response is large
      # enough to flatten the radiation differences on a shared scale.
      ggplot2::facet_wrap(~family, ncol = 1, scales = "free_y") +
      ggplot2::labs(x = NULL, tag = tag, title = title) +
      theme_manuscript() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7),
                      plot.title = ggplot2::element_text(face = "bold", size = 9),
                      legend.position = "right")
  }

  calendar_panel <- function(cal, date_range) {
    cal <- cal |> dplyr::filter(start_date >= date_range[1], end_date <= date_range[2])
    ggplot2::ggplot(cal) +
      ggplot2::geom_segment(
        ggplot2::aes(x = start_date, xend = end_date,
                     y = crop_renamed, yend = crop_renamed, colour = crop_renamed),
        linewidth = 3.2
      ) +
      ggplot2::scale_colour_manual(values = crop_palette, guide = "none") +
      ggplot2::scale_x_date(limits = date_range) +
      ggplot2::labs(x = NULL, y = NULL) +
      theme_manuscript() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                      axis.text.y = ggplot2::element_text(size = 7),
                      panel.grid.major.y = ggplot2::element_blank())
  }

  full <- fig_s14_data |> dplyr::filter(family != "Open field" | TRUE)
  zoom <- fig_s14_data |> dplyr::filter(lubridate::year(date) %in% zoom_years)

  full_range <- range(full$date, na.rm = TRUE)
  zoom_range <- range(zoom$date, na.rm = TRUE)

  # The open-field reference belongs in both driver facets as the anchor.
  add_reference <- function(d) {
    ref <- d |> dplyr::filter(family == "Open field") |> dplyr::select(-family)
    if (nrow(ref) == 0) return(d |> dplyr::filter(family != "Open field"))
    dplyr::bind_rows(
      d |> dplyr::filter(family %in% c("Radiation", "Temperature")),
      tidyr::crossing(ref, family = c("Radiation", "Temperature"))
    )
  }

  (calendar_panel(crop_calendar, full_range) /
     soc_panel(add_reference(full), tags[1], "Full evaluation period (1998–2024)") /
     calendar_panel(crop_calendar, zoom_range) /
     soc_panel(add_reference(zoom), tags[2], "One rotation cycle, zoomed")) +
    patchwork::plot_layout(heights = c(0.5, 3, 0.5, 3), guides = "collect")
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S24 - seasonal nitrate-leaching dynamics with crop
# calendar and residual-N crop-phase attribution. Design rationale, the
# 1-April vs 1-September agrohydrological-year decision, and the
# percolation-not-drain-flow basis are all in R/nitrogen.R, immediately above
# prepare_fig_s24_a_data() - read that before changing this.
# ===========================================================================

plot_fig_s24 <- function(fig_s24_a_data, fig_s24_b_data, fig_s24_percolation,
                          fig_s24_windows, crop_calendar,
                          zoom_years = s24_zoom_years, tags = c("a", "b")) {
  scen_cols <- c(
    "Reference" = "grey10",
    "Rad 0%" = "#FDBE85", "Rad -10%" = "#FD8D3C", "Rad -30%" = "#A63603",
    "Tmp 0degC" = "#C6DBEF", "Tmp -3degC" = "#1A3D6F", "Tmp +3degC" = "#B2182B"
  )

  # The open-field reference belongs in both driver facets as the black
  # anchor (same approach as plot_fig_s14's add_reference()).
  add_reference <- function(d) {
    ref <- d |> dplyr::filter(family == "Open field") |> dplyr::select(-family)
    if (nrow(ref) == 0) return(d |> dplyr::filter(family != "Open field"))
    dplyr::bind_rows(
      d |> dplyr::filter(family %in% c("Radiation", "Temperature")),
      tidyr::crossing(ref, family = c("Radiation", "Temperature"))
    )
  }

  calendar_panel <- function(cal, date_range) {
    cal <- cal |> dplyr::filter(start_date >= date_range[1], end_date <= date_range[2])
    ggplot2::ggplot(cal) +
      ggplot2::geom_segment(
        ggplot2::aes(x = start_date, xend = end_date,
                     y = crop_renamed, yend = crop_renamed, colour = crop_renamed),
        linewidth = 3.2
      ) +
      ggplot2::scale_colour_manual(values = crop_palette, guide = "none") +
      ggplot2::scale_x_date(limits = date_range) +
      ggplot2::labs(x = NULL, y = NULL) +
      theme_manuscript() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                      axis.text.y = ggplot2::element_text(size = 7),
                      panel.grid.major.y = ggplot2::element_blank())
  }

  # Second calendar-like row, directly under the standing-crop row: which
  # crop is the residual-N source for each drainage window, which is not
  # always the crop the row above shows as standing (see the header note).
  attribution_panel <- function(windows, date_range) {
    # Clamp rather than rely on scale-level censoring: a window that straddles
    # the panel edge (e.g. the last zoom-year's Oct-Mar window running past
    # the zoom cutoff) has one endpoint outside date_range, and geom_segment
    # drops the WHOLE row when either endpoint is censored to NA - leaving a
    # gap for the months that ARE in range rather than a clipped segment.
    windows <- windows |>
      dplyr::filter(window_end >= date_range[1], window_start <= date_range[2]) |>
      dplyr::mutate(window_start = pmax(window_start, date_range[1]),
                    window_end = pmin(window_end, date_range[2]))
    ggplot2::ggplot(windows) +
      ggplot2::geom_segment(
        ggplot2::aes(x = window_start, xend = window_end,
                     y = "Residual-N\nsource", yend = "Residual-N\nsource",
                     colour = preceding_crop),
        linewidth = 3.2
      ) +
      ggplot2::scale_colour_manual(values = crop_palette, guide = "none") +
      ggplot2::scale_x_date(limits = date_range) +
      ggplot2::labs(x = NULL, y = NULL) +
      theme_manuscript() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                      axis.text.y = ggplot2::element_text(size = 6.5),
                      panel.grid.major.y = ggplot2::element_blank())
  }

  # Panel (a): one point per agrohydrological year, not a continuous line -
  # leaching is an annual flow, unlike Fig. S14's continuous SOC stock.
  annual_panel <- function(d, tag, title) {
    ggplot2::ggplot(d, ggplot2::aes(x = year, y = leaching_kgN_ha,
                                     colour = scen_label, group = scen_label)) +
      # Line only, matching monthly_panel's single geom_line layer (and
      # Fig. S14's soc_panel): patchwork's guides = "collect" only merges two
      # panels' legends into one when their guides match, which in practice
      # means the colour-mapped LAYERS have to match too, not just the scale -
      # a line+point panel and a line-only panel produced two stacked
      # "Scenario" legends instead of one, even with an identical scale.
      ggplot2::geom_line(linewidth = 0.55) +
      ggplot2::scale_colour_manual(values = scen_cols, name = "Scenario",
                                    labels = scen_axis_label) +
      ggplot2::scale_x_continuous(breaks = scales::breaks_pretty()) +
      ggplot2::scale_y_continuous(
        name = expression("N leaching, 100 cm (kg N ha"^-1*" yr"^-1*")")) +
      ggplot2::facet_wrap(~family, ncol = 1, scales = "free_y") +
      ggplot2::labs(x = NULL, tag = tag, title = title,
                    subtitle = "Agrohydrological year (1 Apr–31 Mar), Rotation 1, Dig–Rem") +
      theme_manuscript() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7),
                      plot.title = ggplot2::element_text(face = "bold", size = 9),
                      legend.position = "right")
  }

  # Panel (b): monthly leaching, zoomed, with the drainage-season bands and
  # the open-field percolation reference from the header note.
  monthly_panel <- function(d, perc, windows, tag, title, date_range) {
    perc_in_range <- perc |> dplyr::filter(month_date >= date_range[1], month_date <= date_range[2])
    perc_scale <- max(d$monthly_leaching_kgN_ha, na.rm = TRUE) /
      max(perc_in_range$monthly_percolation_mm, na.rm = TRUE)
    perc_both <- tidyr::crossing(perc_in_range, family = c("Radiation", "Temperature"))
    windows_both <- windows |>
      dplyr::filter(window_end >= date_range[1], window_start <= date_range[2]) |>
      dplyr::mutate(window_start = pmax(window_start, date_range[1]),
                    window_end = pmin(window_end, date_range[2])) |>
      tidyr::crossing(family = c("Radiation", "Temperature"))

    ggplot2::ggplot(d, ggplot2::aes(x = month_date, y = monthly_leaching_kgN_ha,
                                     colour = scen_label, group = scen_label)) +
      ggplot2::geom_rect(
        data = windows_both,
        ggplot2::aes(xmin = window_start, xmax = window_end, ymin = -Inf, ymax = Inf),
        fill = "grey80", alpha = 0.35, inherit.aes = FALSE
      ) +
      ggplot2::geom_area(
        data = perc_both,
        ggplot2::aes(x = month_date, y = monthly_percolation_mm * perc_scale),
        fill = "grey50", alpha = 0.3, inherit.aes = FALSE
      ) +
      ggplot2::geom_line(linewidth = 0.55) +
      ggplot2::scale_colour_manual(values = scen_cols, name = "Scenario",
                                    labels = scen_axis_label) +
      ggplot2::scale_x_date(limits = date_range) +
      ggplot2::scale_y_continuous(
        name = expression("N leaching, profile-bottom (kg N ha"^-1*" month"^-1*")"),
        sec.axis = ggplot2::sec_axis(~ . / perc_scale,
                                      name = expression(atop("Open-field percolation", "(mm month"^-1*")")))
      ) +
      ggplot2::facet_wrap(~family, ncol = 1, scales = "free_y") +
      ggplot2::labs(
        x = NULL, tag = tag, title = title,
        caption = paste(
          "Shaded bands: nominal 1 Oct–31 Mar drainage season (63–71% of annual leaching, Fig. S13). Grey area: open-field percolation, the flux leaching is carried on (drain flow is zero throughout, Fig. S10).",
          "Profile-bottom basis — not directly comparable to panel (a)'s 100 cm annual totals (Fig. 5 basis; see Fig. S13).",
          sep = "\n"
        )
      ) +
      theme_manuscript() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7),
                      plot.title = ggplot2::element_text(face = "bold", size = 9),
                      plot.caption = ggplot2::element_text(size = 6.5),
                      legend.position = "right")
  }

  full_range <- range(fig_s24_a_data$year, na.rm = TRUE)
  full_date_range <- as.Date(sprintf(c("%d-01-01", "%d-12-31"), full_range))
  zoom_date_range <- range(fig_s24_b_data$month_date, na.rm = TRUE)
  zoom_date_range[2] <- lubridate::ceiling_date(zoom_date_range[2], "month") - 1

  (calendar_panel(crop_calendar, full_date_range) /
     annual_panel(add_reference(fig_s24_a_data), tags[1], "Full evaluation period (1998–2024)") /
     calendar_panel(crop_calendar, zoom_date_range) /
     attribution_panel(fig_s24_windows, zoom_date_range) /
     monthly_panel(add_reference(fig_s24_b_data), fig_s24_percolation, fig_s24_windows,
                   tags[2], "One rotation cycle, zoomed", zoom_date_range)) +
    patchwork::plot_layout(heights = c(0.5, 3, 0.5, 0.45, 3), guides = "collect")
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S25 - nitrogen-supply mechanism behind the radiation
# ladder's leaching response, and why it has the opposite sign from the real
# substrip shading pattern. Motivation, the verified numbers, and what this
# figure does and doesn't establish are all in R/nitrogen.R, immediately
# above prepare_fig_s25_a_data() - read that before changing this.
# ===========================================================================

plot_fig_s25 <- function(fig_s25_a_data, fig_s25_b_data, fig_s25_c_data, crop_calendar,
                          zoom_years = s24_zoom_years, tags = c("a", "b")) {
  scen_cols <- c("Reference" = "grey10", "Rad 0%" = "#FDBE85",
                  "Rad -10%" = "#FD8D3C", "Rad -30%" = "#A63603")
  flux_cols <- c("Biological N fixation" = "#1B7837", "Mineralisation" = "#8C6D31",
                  "Crop N uptake" = "#2166AC", "N leaching" = "#B2182B")

  calendar_panel <- function(cal, date_range) {
    cal <- cal |> dplyr::filter(start_date >= date_range[1], end_date <= date_range[2])
    ggplot2::ggplot(cal) +
      ggplot2::geom_segment(
        ggplot2::aes(x = start_date, xend = end_date,
                     y = crop_renamed, yend = crop_renamed, colour = crop_renamed),
        linewidth = 3.2
      ) +
      ggplot2::scale_colour_manual(values = crop_palette, guide = "none") +
      ggplot2::scale_x_date(limits = date_range) +
      ggplot2::labs(x = NULL, y = NULL) +
      theme_manuscript() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                      axis.text.y = ggplot2::element_text(size = 7),
                      panel.grid.major.y = ggplot2::element_blank())
  }

  # Panel (a): four N-budget terms as rows, radiation scenarios as coloured
  # lines, one rotation cycle - fixation and mineralisation pulling down
  # alongside uptake is the seasonal face of the mechanism.
  seasonal_panel <- function(d, tag, date_range) {
    ggplot2::ggplot(d, ggplot2::aes(x = month_date, y = monthly_kgN_ha,
                                     colour = scen_label, group = scen_label)) +
      ggplot2::geom_line(linewidth = 0.55) +
      ggplot2::scale_colour_manual(values = scen_cols, name = "Scenario",
                                    labels = scen_axis_label) +
      ggplot2::scale_x_date(limits = date_range) +
      ggplot2::facet_wrap(~flux, ncol = 1, scales = "free_y") +
      ggplot2::labs(
        x = NULL, y = expression("kg N ha"^-1*" month"^-1), tag = tag,
        title = "Seasonal N-budget dynamics, one rotation cycle",
        subtitle = "Rotation 1, Dig–Rem; profile-bottom basis, as Fig. S24b"
      ) +
      theme_manuscript() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7),
                      plot.title = ggplot2::element_text(face = "bold", size = 9),
                      strip.text = ggplot2::element_text(size = 8),
                      legend.position = "right")
  }

  # Panel (b): the ladder and the real substrip response for the same four
  # terms, side by side on one % axis - the direct sign-flip comparison.
  # facet_grid(scales/space = "free") gives the 4-level ladder and the
  # 2-level substrip group proportional width rather than equal panels, the
  # same technique plot_fig04() uses for uneven scenario counts per facet.
  mechanism_panel <- function(d, tag) {
    ggplot2::ggplot(d, ggplot2::aes(x = condition, y = pct_change, fill = flux)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.45) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.82), width = 0.78,
                         colour = "grey20", linewidth = 0.15) +
      ggplot2::scale_fill_manual(values = flux_cols, name = NULL) +
      ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%")) +
      ggplot2::facet_grid(~comparison_group, scales = "free_x", space = "free_x") +
      ggplot2::labs(
        x = NULL, y = "Change from own 0-level reference", tag = tag,
        title = "Same driver, opposite sign on leaching: ladder vs real substrip shading",
        caption = "Ladder: % change from Rad 0% (Centre). Substrip: % change from Centre, at each driver's 0-level (the actually-simulated microclimate). Both Biogas digestate, Residue Removed, 1998–2024, 4-rotation mean."
      ) +
      theme_manuscript() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 8),
                      strip.text = ggplot2::element_text(size = 8),
                      plot.title = ggplot2::element_text(face = "bold", size = 9),
                      plot.caption = ggplot2::element_text(size = 6.5),
                      legend.position = "bottom")
  }

  zoom_date_range <- range(fig_s25_a_data$month_date, na.rm = TRUE)
  zoom_date_range[2] <- lubridate::ceiling_date(zoom_date_range[2], "month") - 1
  bc_data <- dplyr::bind_rows(fig_s25_b_data, fig_s25_c_data) |>
    dplyr::mutate(
      condition = factor(condition, levels = c("Rad -5%", "Rad -10%", "Rad -20%", "Rad -30%",
                                                 "West", "East")),
      comparison_group = factor(comparison_group, levels = c(
        "Uniform radiation ladder\n(Centre strip)", "Real substrip position\n(0-level, vs Centre)"))
    )

  (calendar_panel(crop_calendar, zoom_date_range) /
     seasonal_panel(fig_s25_a_data, tags[1], zoom_date_range) /
     mechanism_panel(bc_data, tags[2])) +
    patchwork::plot_layout(heights = c(0.35, 3.5, 2.2))
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S9 - canopy size and cumulative water use.
#
# The mechanistic close of the wind argument. Radiation and temperature each
# produce graded, dose-dependent divergence in both LAI and cumulative
# transpiration; the wind curves lie on top of the open-field reference. That
# is why wind cuts demand without moving yield - it never reaches the canopy.
#
# Colour ramps are graded WITHIN each driver so dose-response is visible, and
# the drivers are separated by facet so the three ramps are not confused.
# ===========================================================================

s09_driver_ramp <- function(labels, family) {
  n <- length(labels)
  pal <- switch(family,
    "Radiation" = colorRampPalette(c("#FEE8C8", "#A63603"))(n),
    "Wind" = colorRampPalette(c("#D5F5E3", "#0B5345"))(n),
    "Temperature" = colorRampPalette(c("#1A3D6F", "#C6DBEF", "#B2182B"))(n),
    rep("grey40", n)
  )
  stats::setNames(pal, labels)
}

plot_fig_s09_row <- function(d, y_col, y_lab, tag, show_x = TRUE) {
  families <- c("Radiation", "Wind", "Temperature")
  d <- d |>
    dplyr::mutate(family = classify_scenario_family(scen_label)) |>
    dplyr::filter(!is.na(family))

  # The open-field reference belongs in every driver facet as the black anchor.
  reference <- d |> dplyr::filter(family == "Open field") |> dplyr::select(-family)
  ref_all <- if (nrow(reference) > 0) {
    tidyr::crossing(reference, family = families)
  } else {
    reference
  }
  scenarios <- d |> dplyr::filter(family %in% families)

  ordered_labels <- scenarios |>
    dplyr::distinct(family, scen_label) |>
    dplyr::arrange(family, scen_label)
  ramp <- unlist(lapply(families, \(f) {
    labs <- ordered_labels$scen_label[ordered_labels$family == f]
    s09_driver_ramp(labs, f)
  }))

  ggplot2::ggplot(scenarios, ggplot2::aes(x = day_of_year, y = .data[[y_col]],
                                           colour = scen_label, group = scen_label)) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::geom_line(
      data = ref_all, ggplot2::aes(x = day_of_year, y = .data[[y_col]]),
      colour = "grey10", linewidth = 0.8, inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(values = ramp, guide = "none") +
    ggplot2::facet_grid(crop_renamed ~ family, scales = "free") +
    ggplot2::labs(x = if (show_x) "Day of year" else NULL, y = y_lab, tag = tag) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7),
                    strip.text = ggplot2::element_text(size = 8))
}

plot_fig_s09 <- function(fig_s09_data) {
  lai <- fig_s09_data$lai
  transp <- fig_s09_data$transpiration |>
    dplyr::inner_join(dplyr::distinct(lai, crop_renamed, year), by = "year")

  patchwork::wrap_plots(
    plot_fig_s09_row(lai, "lai", expression("LAI (m"^2~"m"^-2*")"), "a", show_x = FALSE),
    plot_fig_s09_row(transp, "cumulative_transpiration_mm",
                      "Cumulative actual\ntranspiration (mm)", "b"),
    ncol = 1
  )
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S1 - open-field weather forcing and crop calendar.
# Documents the driver record and the rotation it drove, over one complete
# traversal. Management events are not shown - see prepare_fig_s01_weather().
# ===========================================================================

plot_fig_s01 <- function(fig_s01_weather, fig_s01_calendar) {
  date_limits <- range(fig_s01_weather$date, na.rm = TRUE)
  x_scale <- ggplot2::scale_x_date(limits = date_limits, date_breaks = "6 months",
                                    date_labels = "%b\n%Y", expand = ggplot2::expansion(mult = 0.01))

  base_theme <- theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7),
                    axis.title.y = ggplot2::element_text(size = 7.5),
                    legend.position = "none")

  p_cal <- ggplot2::ggplot(fig_s01_calendar) +
    ggplot2::geom_segment(
      ggplot2::aes(x = start_date, xend = end_date, y = crop_renamed, yend = crop_renamed,
                   colour = crop_renamed), linewidth = 4
    ) +
    ggplot2::geom_point(ggplot2::aes(x = end_date, y = crop_renamed),
                         shape = 23, size = 1.7, fill = "white", colour = "grey20") +
    ggplot2::scale_colour_manual(values = crop_palette, guide = "none") +
    x_scale + ggplot2::labs(x = NULL, y = NULL, tag = "a") +
    base_theme +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7),
                    panel.grid.major.y = ggplot2::element_blank())

  # Cumulative precipitation is rescaled onto the daily-bar axis so both fit one
  # panel; the secondary axis carries the true cumulative values.
  precip_scale <- max(fig_s01_weather$cumulative_precip_mm, na.rm = TRUE) /
    max(fig_s01_weather$precipitation_mm, na.rm = TRUE)

  p_precip <- ggplot2::ggplot(fig_s01_weather, ggplot2::aes(x = date)) +
    ggplot2::geom_col(ggplot2::aes(y = precipitation_mm), fill = "#2166AC", width = 1) +
    ggplot2::geom_line(ggplot2::aes(y = cumulative_precip_mm / precip_scale),
                        colour = "grey25", linewidth = 0.5) +
    ggplot2::scale_y_continuous(
      name = "Daily precip.\n(mm)",
      sec.axis = ggplot2::sec_axis(~ . * precip_scale, name = "Cumulative (mm)")
    ) +
    x_scale + ggplot2::labs(x = NULL, tag = "b") + base_theme

  p_temp <- ggplot2::ggplot(fig_s01_weather, ggplot2::aes(x = date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = air_temp_min_c, ymax = air_temp_max_c),
                          fill = "#D55E00", alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = air_temp_c), colour = "#B2182B", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", colour = "grey40") +
    ggplot2::scale_y_continuous(name = "Air temp.\n(°C)") +
    x_scale + ggplot2::labs(x = NULL) + base_theme

  p_wind <- ggplot2::ggplot(fig_s01_weather, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = wind_max_m_s), colour = "#7FBC41", linewidth = 0.3, alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = wind_m_s), colour = "#1B7837", linewidth = 0.45) +
    ggplot2::scale_y_continuous(name = expression(atop("Wind speed", "(m s"^-1*")"))) +
    x_scale + ggplot2::labs(x = NULL) + base_theme

  p_rad <- ggplot2::ggplot(fig_s01_weather, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = global_rad_w_m2), colour = "#E69F00", linewidth = 0.4) +
    ggplot2::scale_y_continuous(name = expression(atop("Global rad.", "(W m"^-2*")"))) +
    x_scale + ggplot2::labs(x = NULL) + base_theme

  patchwork::wrap_plots(p_cal, p_precip, p_temp, p_wind, p_rad,
                        ncol = 1, heights = c(1.1, 1, 1, 1, 1))
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S5 - grass-clover calibration.
# Distribution of simulated annual AGB across the Latin-hypercube parameter
# ensemble, against the Foulum 2024 field target.
# ===========================================================================

plot_fig_s05 <- function(fig_s05_data, tags = c("a", "b")) {
  if (is.null(fig_s05_data) || nrow(fig_s05_data) == 0) return(patchwork::plot_spacer())

  best <- fig_s05_data |> dplyr::slice_min(abs(deviation_pct), n = 1, with_ties = FALSE)

  p_a <- ggplot2::ggplot(fig_s05_data, ggplot2::aes(x = annual_agb_t_dm_ha)) +
    ggplot2::geom_histogram(bins = 45, fill = crop_palette[["Grass-Clover"]],
                             colour = "white", linewidth = 0.15, alpha = 0.9) +
    ggplot2::geom_vline(xintercept = GC_FIELD_TARGET_T_DM_HA,
                         colour = "#B2182B", linewidth = 0.7, linetype = "dashed") +
    ggplot2::annotate("text", x = GC_FIELD_TARGET_T_DM_HA, y = Inf,
                       label = sprintf("Field target\n%.2f t DM ha⁻¹", GC_FIELD_TARGET_T_DM_HA),
                       hjust = 1.05, vjust = 1.2, size = 2.6, colour = "#B2182B") +
    ggplot2::labs(x = expression("Simulated annual AGB (t DM ha"^-1~"yr"^-1*")"),
                   y = "Parameter sets", tag = tags[1]) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))

  p_b <- ggplot2::ggplot(fig_s05_data, ggplot2::aes(x = deviation_pct)) +
    ggplot2::geom_histogram(bins = 45, fill = "grey55", colour = "white", linewidth = 0.15) +
    ggplot2::geom_vline(xintercept = 0, colour = "#B2182B", linewidth = 0.7, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = best$deviation_pct, colour = "#1B7837", linewidth = 0.6) +
    ggplot2::annotate("text", x = best$deviation_pct, y = Inf,
                       label = sprintf("Selected: %+.2f%%", best$deviation_pct),
                       hjust = -0.05, vjust = 1.6, size = 2.6, colour = "#1B7837") +
    ggplot2::scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::labs(x = "Deviation from field target", y = "Parameter sets", tag = tags[2]) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))

  patchwork::wrap_plots(p_a, p_b, ncol = 2)
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S4 - soybean parameterisation diagnostic.
# Single version (soypea_opt_v3) - see prepare_fig_s04_data() for why the
# draft's two-version comparison is not reproduced.
# ===========================================================================

plot_fig_s04 <- function(fig_s04_data, fig_s04_season) {
  point_panel <- function(mean_col, sd_col, y_lab, tag, hline = NULL) {
    p <- ggplot2::ggplot(fig_s04_data,
                          ggplot2::aes(x = scen_label, y = .data[[mean_col]])) +
      ggplot2::geom_vline(xintercept = 1.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data[[mean_col]] - .data[[sd_col]],
                     ymax = .data[[mean_col]] + .data[[sd_col]]),
        width = 0.18, linewidth = 0.35, alpha = 0.75, colour = crop_palette[["Soybean"]]
      ) +
      ggplot2::geom_point(size = 1.9, colour = crop_palette[["Soybean"]]) +
      ggplot2::scale_x_discrete(labels = scen_axis_label) +
      ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
      ggplot2::labs(x = NULL, y = y_lab, tag = tag) +
      theme_manuscript() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7))
    if (!is.null(hline)) {
      p <- p + ggplot2::geom_hline(yintercept = hline, linetype = "dashed",
                                    colour = "grey45", linewidth = 0.4)
    }
    p
  }

  p_yield <- ggplot2::ggplot(fig_s04_data, ggplot2::aes(x = scen_label)) +
    ggplot2::geom_vline(xintercept = 1.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = agb_mean - agb_sd, ymax = agb_mean + agb_sd),
                            width = 0.18, linewidth = 0.35, alpha = 0.7, colour = "grey35") +
    ggplot2::geom_point(ggplot2::aes(y = agb_mean, shape = "Total AGB"), size = 1.9, colour = "grey25") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = grain_mean - grain_sd, ymax = grain_mean + grain_sd),
                            width = 0.18, linewidth = 0.35, alpha = 0.7,
                            colour = crop_palette[["Soybean"]]) +
    ggplot2::geom_point(ggplot2::aes(y = grain_mean, shape = "Grain"), size = 1.9,
                         colour = crop_palette[["Soybean"]]) +
    ggplot2::scale_shape_manual(values = c("Total AGB" = 17, "Grain" = 16), name = NULL) +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, y = expression("Soybean yield (t DM ha"^-1~"yr"^-1*")"), tag = "a") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                    legend.position = "right")

  p_hi <- point_panel("harvest_index_mean", "harvest_index_sd", "Harvest index", "b")

  stress_long <- fig_s04_data |>
    dplyr::select(scen_label, driver,
                  Water = water_stress_days_mean, Nitrogen = n_stress_days_mean,
                  Water_sd = water_stress_days_sd, Nitrogen_sd = n_stress_days_sd) |>
    tidyr::pivot_longer(c(Water, Nitrogen), names_to = "stress", values_to = "days") |>
    dplyr::mutate(sd = dplyr::if_else(stress == "Water", Water_sd, Nitrogen_sd))

  p_stress <- ggplot2::ggplot(stress_long,
                               ggplot2::aes(x = scen_label, y = days, colour = stress, shape = stress)) +
    ggplot2::geom_vline(xintercept = 1.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = pmax(days - sd, 0), ymax = days + sd),
                            width = 0.18, linewidth = 0.32, alpha = 0.7,
                            position = ggplot2::position_dodge(width = 0.45)) +
    ggplot2::geom_point(size = 1.8, position = ggplot2::position_dodge(width = 0.45)) +
    ggplot2::scale_colour_manual(values = c("Water" = "#2166AC", "Nitrogen" = "#7FBC41"), name = "Stress") +
    ggplot2::scale_shape_manual(values = c("Water" = 16, "Nitrogen" = 17), name = "Stress") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, y = "Stress days per season", tag = "c") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                    legend.position = "right")

  p_season <- ggplot2::ggplot(fig_s04_season,
                               ggplot2::aes(x = scen_label, y = season_length_days)) +
    ggplot2::geom_boxplot(fill = crop_palette[["Soybean"]], alpha = 0.5,
                           outlier.size = 0.6, linewidth = 0.3) +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, y = "Season length (days)", tag = "d") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7))

  patchwork::wrap_plots(p_yield, p_hi, p_stress, p_season, ncol = 1)
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S06 - rotation yield composition.
# ===========================================================================

plot_fig_s06 <- function(fig_s06_data) {
  p_abs <- ggplot2::ggplot(fig_s06_data,
                            ggplot2::aes(x = scen_label, y = mean_contribution, fill = crop_renamed)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_fill_manual(values = crop_palette, name = "Crop") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, y = expression("Contribution to system ASY (t DM ha"^-1~"yr"^-1*")"), tag = "a") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                    legend.position = "right")

  p_share <- ggplot2::ggplot(fig_s06_data,
                              ggplot2::aes(x = scen_label, y = share_pct, fill = crop_renamed)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_fill_manual(values = crop_palette, name = "Crop") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, y = "Share of system ASY", tag = "b") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                    legend.position = "right")

  patchwork::wrap_plots(p_abs, p_share, ncol = 1) + patchwork::plot_layout(guides = "collect")
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S12 - complete nitrogen budget.
# Inputs above the axis, outputs below; the gap between them is the surplus
# available to leach.
# ===========================================================================

plot_fig_s12 <- function(fig_s12_data) {
  term_cols <- c(
    "Mineral fertiliser" = "#998EC3", "Organic fertiliser" = "#F5A641",
    "Biological fixation" = "#0072B2", "Net mineralisation" = "#8C6D31",
    "Crop uptake" = "#4EA72E", "Leaching" = "#B2182B"
  )

  ggplot2::ggplot(fig_s12_data,
                  ggplot2::aes(x = scen_label, y = mean_kgN_ha, fill = term)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.5) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_fill_manual(values = term_cols, name = NULL) +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::facet_grid(management_label ~ driver, scales = "free_x", space = "free_x",
                         labeller = ggplot2::labeller(management_label = ggplot2::label_wrap_gen(18))) +
    ggplot2::labs(x = NULL, y = expression("N flux (kg N ha"^-1~"yr"^-1*")"),
                   caption = "Inputs plotted positive, outputs negative. Denitrification, volatilisation and deposition are omitted; the bars are not a closed budget.") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                    strip.text.y = ggplot2::element_text(size = 7, angle = 0),
                    legend.position = "right")
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S19 - rotation-permutation spread vs scenario step.
# Evidences the Discussion claim that weather-year assignment alone moves
# yield by an amount comparable to one radiation-scenario step.
# ===========================================================================

plot_fig_s19 <- function(fig_s19_data) {
  ggplot2::ggplot(fig_s19_data,
                  ggplot2::aes(x = crop_renamed, y = pct, fill = source)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.0f", pct)),
      position = ggplot2::position_dodge(width = 0.8), vjust = -0.4, size = 2.4, colour = "grey25"
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(c("#7A7A7A", "#E69F00"), unique(fig_s19_data$source)), name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1),
                                 expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Change in mean AGB",
                   caption = "If the two bars are comparable for a crop, a single fixed rotation could mis-state that crop's VAPV penalty by about one scenario step.") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1, size = 8),
                    legend.position = "bottom")
}

# ===========================================================================
# FIGURE 8 (main-text candidate) - three-domain trade-off.
#
# Companion to Figure 7. Same yield axis, but nitrogen, carbon and water each
# on a common "% of open field" scale, so one scenario can be followed across
# all three panels. This is what tests the Results 3.3 claim that no scenario
# improves yield, N retention and SOC together - Figure 7 alone cannot, because
# a point sits in only one panel's quadrant.
#
# The favourable direction differs by outcome (carbon is a stock to build,
# leaching and water use are losses to avoid), so each panel is shaded and
# labelled rather than relying on a shared convention.
# ===========================================================================

plot_fig_three_domain <- function(three_domain_data, win_win = NULL) {
  # Jeroen asked for change in ASY alongside the environmental outcomes. It is
  # derived here from the same yield column (not added to three_domain_data), so
  # find_win_win_scenarios() still tests only the three environmental domains.
  asy_lab <- "Δ ASY AGB"
  asy_rows <- three_domain_data |>
    dplyr::distinct(management_label, scen_label, driver, relative_yield_pct) |>
    dplyr::mutate(value = relative_yield_pct - 100, outcome = asy_lab)
  plot_data <- three_domain_data |>
    dplyr::mutate(outcome = as.character(outcome)) |>
    dplyr::bind_rows(asy_rows) |>
    dplyr::mutate(outcome = factor(outcome, levels = c(asy_lab, levels(three_domain_data$outcome))))

  # Shading marks the favourable region: yield at or above openfield parity,
  # and the outcome on its better side of zero.
  shade <- tibble::tibble(
    outcome = factor(levels(plot_data$outcome), levels = levels(plot_data$outcome)),
    ymin = c(0, -Inf, 0, -Inf),
    ymax = c(Inf, 0, Inf, 0)
  )
  labels <- tibble::tibble(
    outcome = shade$outcome,
    label = c("yield gain", "less leaching", "SOC gain", "less water use"),
    y = c(Inf, -Inf, Inf, -Inf),
    vjust = c(1.4, -0.6, 1.4, -0.6)
  )

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = relative_yield_pct, y = value)) +
    ggplot2::geom_rect(
      data = shade, inherit.aes = FALSE,
      ggplot2::aes(xmin = 100, xmax = Inf, ymin = ymin, ymax = ymax),
      fill = "#1B7837", alpha = 0.07
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 100, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(colour = driver, shape = driver), size = 2, alpha = 0.9) +
    ggplot2::geom_text(
      data = labels, inherit.aes = FALSE,
      ggplot2::aes(x = Inf, y = y, label = label, vjust = vjust),
      hjust = 1.1, size = 2.4, fontface = "italic", colour = "#1B7837"
    ) +
    ggplot2::scale_colour_manual(values = tradeoff_driver_palette, name = "Driver") +
    ggplot2::scale_shape_manual(values = c("Radiation" = 16, "Temperature" = 17, "Wind" = 3),
                                 name = "Driver") +
    ggplot2::scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::facet_grid(outcome ~ management_label, scales = "free_y",
                         labeller = ggplot2::labeller(
                           management_label = ggplot2::label_wrap_gen(18),
                           outcome = ggplot2::label_wrap_gen(16))) +
    ggplot2::labs(
      x = "Relative all-crop ASY AGB (% of openfield)",
      y = "Change vs openfield",
      caption = "Shaded region = yield at or above openfield parity with the outcome improved. A scenario benefiting all three environmental domains would fall in the shaded region of the lower three rows."
    ) +
    theme_manuscript() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
      strip.text.y = ggplot2::element_text(size = 7, angle = 0),
      strip.text.x = ggplot2::element_text(size = 7.5),
      legend.position = "right"
    )
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S15 - Total SOC by depth layer.
# (a) absolute stocks through time; (b) end-of-simulation change by depth.
# Complements Figure 6, which shows only 0-30/30-60 cm and only relative change.
# ===========================================================================

plot_fig_s15 <- function(fig_s15_a_data, fig_s15_b_data, tags = c("a", "b")) {
  p_a <- ggplot2::ggplot(fig_s15_a_data,
                          ggplot2::aes(x = year, y = mean_soc_tC_ha,
                                       colour = scen_label, fill = scen_label)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = mean_soc_tC_ha - sd_soc_tC_ha, ymax = mean_soc_tC_ha + sd_soc_tC_ha),
      alpha = 0.15, colour = NA
    ) +
    ggplot2::geom_line(linewidth = 0.65) +
    ggplot2::scale_colour_manual(values = c("Reference" = "grey15", soc_scenario_palette),
                                  name = "Scenario", labels = scen_axis_label) +
    ggplot2::scale_fill_manual(values = c("Reference" = "grey15", soc_scenario_palette),
                                name = "Scenario", labels = scen_axis_label) +
    ggplot2::facet_wrap(~depth, ncol = 3, scales = "free_y") +
    ggplot2::labs(x = NULL, y = expression("Total SOC (t C ha"^-1*")"), tag = tags[1]) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7),
                    legend.position = "right")

  mgmt_cols <- c(
    "Mineral fertiliser | Residue Removed" = "#998EC3",
    "Mineral fertiliser | Residue Retained" = "#542788",
    "Biogas digestate | Residue Removed" = "#F5A641",
    "Biogas digestate | Residue Retained" = "#B35806"
  )

  p_b <- ggplot2::ggplot(fig_s15_b_data,
                          ggplot2::aes(x = scen_label, y = mean_delta_soc_pct,
                                       colour = management_label, shape = management_label)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_delta_soc_pct - sd_delta_soc_pct,
                   ymax = mean_delta_soc_pct + sd_delta_soc_pct),
      width = 0.18, linewidth = 0.3, alpha = 0.7,
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_point(size = 1.7, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::scale_colour_manual(values = mgmt_cols, name = "Management") +
    ggplot2::scale_shape_manual(values = c(16, 17, 15, 18), name = "Management") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::facet_grid(depth ~ driver, scales = "free", space = "free_x") +
    ggplot2::labs(x = NULL, y = "ΔTotal SOC at 2024 (% of open field)", tag = tags[2]) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                    legend.position = "right")

  patchwork::wrap_plots(p_a, p_b, ncol = 1, heights = c(1, 1.5))
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S10 - annual water balance components.
# Percolation (vertical, below profile) and drain flow (lateral, intercepted)
# are separate DAISY outputs; showing them side by side settles the
# terminology question raised in review.
# ===========================================================================

plot_fig_s10 <- function(fig_s10_data) {
  ggplot2::ggplot(fig_s10_data,
                  ggplot2::aes(x = scen_label, y = mean_mm, colour = component, shape = component)) +
    ggplot2::geom_vline(xintercept = 1.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = pmax(mean_mm - sd_mm, 0), ymax = mean_mm + sd_mm),
      width = 0.18, linewidth = 0.3, alpha = 0.6,
      position = ggplot2::position_dodge(width = 0.5)
    ) +
    ggplot2::geom_point(size = 1.8, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::scale_colour_manual(
      values = c("Precipitation" = "#2166AC", "Potential ET" = "#B2182B",
                 "Actual ET" = "#D55E00", "Percolation (below profile)" = "#1B7837",
                 "Drain flow (lateral)" = "#7FBC41", "Surface runoff" = "grey45"),
      name = NULL) +
    ggplot2::scale_shape_manual(values = c(16, 17, 15, 18, 8, 3), name = NULL) +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, y = expression("Annual water flux (mm yr"^-1*")"),
                   caption = "Percolation = vertical flux past the base of the profile; drain flow = lateral flux intercepted by field drains. Separate model outputs; not additive.") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7.5),
                    legend.position = "right")
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S18 - substrip (West/Centre/East) effects.
# Shows once, across all three outcome domains, that substrip position is a
# minor axis of variation - which is what justifies the main-text figures
# using the Centre strip alone.
# ===========================================================================

plot_fig_s18 <- function(fig_s18_data) {
  ggplot2::ggplot(fig_s18_data,
                  ggplot2::aes(x = strip, y = mean_v, colour = driver, shape = driver)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_v - sd_v, ymax = mean_v + sd_v),
      width = 0.16, linewidth = 0.35, alpha = 0.75,
      position = ggplot2::position_dodge(width = 0.55)
    ) +
    ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = 0.55)) +
    ggplot2::scale_colour_manual(
      values = c("Open field" = "grey20", driver_palette), name = "Driver 0-level") +
    ggplot2::scale_shape_manual(values = c("Open field" = 15, "Radiation" = 16,
                                            "Temperature" = 17, "Wind" = 18),
                                 name = "Driver 0-level") +
    ggplot2::facet_wrap(~outcome, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = NULL, y = NULL,
                   caption = "Each driver's VAPV 0-level at its three substrip positions, against the open field. Error bars = ±1 SD across the four rotations.") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                    strip.text = ggplot2::element_text(size = 8),
                    legend.position = "right")
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S11 - N fluxes and leaching, all four management regimes.
# Companion to main-text Figure 5, which draws the fluxes for Dig-Rem only.
#
# Panel (a) repeats the fluxes for every fertilisation x residue regime, one row
# each, on a shared y axis so the regimes can be compared directly. Panel (b) is
# the leaching series of Figure 5 (fig_05_b_data) on its own axis: the merged
# Figure 5 draws it at a third of its true vertical scale on the shared secondary
# axis, and this panel is where it can be read at full resolution.
# ===========================================================================

# Panel (b): leaching for every regime in the table it is given. Encodings come
# from fig_05_regime_cols / fig_05_regime_shapes (Figure 5's block above).
plot_fig_s11_b_leaching <- function(fig_05_b_data) {
  regimes <- levels(fig_05_b_data$management_label)
  # 0.15 of a tick per series, so the group widens with the number of regimes
  # instead of crowding the markers.
  dodge <- ggplot2::position_dodge(width = 0.15 * length(regimes))

  ggplot2::ggplot(
    fig_05_b_data,
    ggplot2::aes(x = scen_label, y = mean_kgN_ha, colour = management_label, shape = management_label)
  ) +
    ggplot2::geom_vline(xintercept = 1.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_kgN_ha - sd_kgN_ha, ymax = mean_kgN_ha + sd_kgN_ha),
      width = 0.18, linewidth = 0.35, alpha = 0.7,
      position = dodge
    ) +
    ggplot2::geom_point(size = 2, position = dodge) +
    ggplot2::scale_colour_manual(values = fig_05_regime_cols[regimes], name = "Management") +
    ggplot2::scale_shape_manual(values = fig_05_regime_shapes[regimes], name = "Management") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(name = expression("N leaching (kg N ha"^-1~"yr"^-1*")")) +
    ggplot2::facet_grid(~driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, tag = "b") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
                    legend.position = "right")
}

# Panel (a): the three fluxes, one row per regime, on a shared y axis.
plot_fig_s11_a_fluxes <- function(fig_s11_a_data) {
  flux_cols <- c("Mineralisation" = "#8C6D31",
                 "Crop N uptake" = "#4EA72E",
                 "Biological N fixation" = "#0072B2")

  ggplot2::ggplot(
    fig_s11_a_data,
    ggplot2::aes(x = scen_label, y = mean_kgN_ha, colour = flux, shape = flux)
  ) +
    ggplot2::geom_vline(xintercept = 1.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_kgN_ha - sd_kgN_ha, ymax = mean_kgN_ha + sd_kgN_ha),
      width = 0.18, linewidth = 0.3, alpha = 0.7,
      position = ggplot2::position_dodge(width = 0.45)
    ) +
    ggplot2::geom_point(size = 1.7, position = ggplot2::position_dodge(width = 0.45)) +
    ggplot2::scale_colour_manual(values = flux_cols, name = NULL) +
    ggplot2::scale_shape_manual(values = c(16, 17, 15), name = NULL) +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(name = expression("N flux (kg N ha"^-1~"yr"^-1*")")) +
    ggplot2::facet_grid(management_label ~ driver, scales = "free_x", space = "free_x",
                         labeller = ggplot2::labeller(management_label = ggplot2::label_wrap_gen(18))) +
    ggplot2::labs(x = NULL, tag = "a") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                    strip.text.y = ggplot2::element_text(size = 7, angle = 0),
                    legend.position = "right")
}

plot_fig_s11 <- function(fig_s11_a_data, fig_05_b_data) {
  patchwork::wrap_plots(
    plot_fig_s11_a_fluxes(fig_s11_a_data),
    plot_fig_s11_b_leaching(fig_05_b_data),
    ncol = 1, heights = c(2.3, 1)
  )
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S17 - yield-SOC trade-off.
#
# Companion to main-text Figure 7 (yield vs N leaching): same axes and
# quadrant logic, SOC substituted for leaching. The fitted OLS slope per
# management x driver is printed in-panel, since that number is quoted in the
# text and currently has two contradictory values in the draft.
# ===========================================================================

plot_fig_s17_panel <- function(d, slopes, y_lab, tag) {
  slope_labels <- slopes |>
    dplyr::group_by(management_label) |>
    dplyr::summarise(
      label = paste(sprintf("%s: %.2f", driver, slope), collapse = "\n"),
      .groups = "drop"
    )

  ggplot2::ggplot(d, ggplot2::aes(x = relative_yield_pct, y = delta_soc_total_pct,
                                   colour = driver, shape = driver)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = 100, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, alpha = 0.8) +
    ggplot2::geom_point(size = 2, alpha = 0.9) +
    ggplot2::geom_text(
      data = slope_labels, inherit.aes = FALSE,
      ggplot2::aes(x = -Inf, y = Inf, label = label),
      hjust = -0.1, vjust = 1.2, size = 2.3, fontface = "italic", colour = "grey25"
    ) +
    ggplot2::scale_colour_manual(values = tradeoff_driver_palette, name = "Driver") +
    ggplot2::scale_shape_manual(values = c("Radiation" = 16, "Temperature" = 17), name = "Driver") +
    ggplot2::scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::facet_wrap(~management_label, nrow = 1) +
    ggplot2::labs(x = y_lab, y = "ΔTotal SOC, 0–30 cm (% of open field)", tag = tag) +
    theme_manuscript() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      strip.text = ggplot2::element_text(size = 8),
      legend.position = "right"
    )
}

plot_fig_s17 <- function(fig_s17_a_data, fig_s17_b_data, yield_soc_slopes) {
  slopes_a <- yield_soc_slopes |> dplyr::filter(yield_metric == "All-crop ASY AGB")
  slopes_b <- yield_soc_slopes |> dplyr::filter(yield_metric == "Grain ASY (WW + SY)")

  patchwork::wrap_plots(
    plot_fig_s17_panel(fig_s17_a_data, slopes_a,
                        "Relative all-crop ASY AGB (% of open field)", "a"),
    plot_fig_s17_panel(fig_s17_b_data, slopes_b,
                        "Relative grain ASY, WW + SY (% of open field)", "b"),
    ncol = 1
  ) + patchwork::plot_layout(guides = "collect")
}

# ===========================================================================
# Shared scenario palette for the S20-S13 block.
#
# Extends soc_scenario_palette (Figures 6/S14/S15) with the wind extremes and
# the two 0-levels, so a given scenario keeps one colour across every figure in
# the supplement. Diverging within each driver: temperature cool-to-warm,
# radiation light-to-dark, wind on its own blue.
# ===========================================================================
scenario_palette_extended <- c(
  "Reference"  = "grey15",
  "Rad 0%"     = "#FDBE85",
  "Rad -10%"   = "#E6550D",
  "Rad -30%"   = "#A63603",
  "Wind 0%"    = "#9ECAE1",
  "Wind -70%"  = "#08519C",
  "Tmp 0degC"  = "#BDBDBD",
  "Tmp -3degC" = "#2166AC",
  "Tmp +3degC" = "#B2182B"
)

# ===========================================================================
# SUPPLEMENTARY FIGURE S20 - interannual yield stability and downside risk.
#
# (a) coefficient of variation of crop yield across the simulated crop-years
# (b) risk-return plane: does VAPV trade yield level against yield stability?
# (c) worst-year test: are bad years protected more, or less, than average ones?
#
# See R/resilience.R for why variability is computed within crop rather than on
# the whole-system annual total.
# ===========================================================================

plot_fig_s20_a <- function(fig_s20_data) {
  drivers <- c("Radiation", "Temperature", "Wind")
  d <- add_reference_to_each_driver(fig_s20_data, drivers) |>
    dplyr::mutate(
      scen_label = factor(scen_label, levels = scen_order_center),
      driver = factor(driver, levels = drivers)
    ) |>
    dplyr::filter(!is.na(scen_label))

  ggplot2::ggplot(d, ggplot2::aes(x = scen_label, y = cv_pct,
                                   colour = crop_phase, shape = crop_phase)) +
    # Open-field CV for each crop phase, as a horizontal anchor. Without it the
    # reader cannot tell whether a 20% CV is high or normal for that crop.
    ggplot2::geom_hline(
      data = dplyr::filter(d, scen_label == "Reference") |>
        dplyr::select(crop_phase, ref_cv = cv_pct) |> dplyr::distinct(),
      ggplot2::aes(yintercept = ref_cv, colour = crop_phase),
      linetype = "dotted", linewidth = 0.4, alpha = 0.7, inherit.aes = FALSE
    ) +
    ggplot2::geom_point(size = 1.9, position = ggplot2::position_dodge(width = 0.6)) +
    ggplot2::scale_colour_manual(values = s20_crop_phase_palette, name = "Crop phase") +
    ggplot2::scale_shape_manual(values = c(16, 17, 15, 18, 8), name = "Crop phase") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::facet_grid(. ~ driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(
      x = NULL,
      y = "Interannual CV of harvested AGB (%)",
      tag = "a",
      subtitle = "Dotted line = that crop phase's open-field CV"
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                    legend.position = "right")
}

# Panels (b) and (c) share one driver shape scale, declared identically in both
# with drop = FALSE. Without that, panel (c) - which has no open-field row -
# builds a three-level scale, panel (b) a four-level one, and patchwork's
# guide collection emits two separate "Driver" legends instead of merging them.
s20_driver_levels <- c("Reference", "Radiation", "Temperature", "Wind")
s20_driver_shapes <- c("Reference" = 4, "Radiation" = 16,
                       "Temperature" = 17, "Wind" = 15)

plot_fig_s20_b <- function(fig_s20_relative, scenarios = s20_scenarios_highlight) {
  d <- fig_s20_relative |>
    dplyr::filter(scen_label %in% scenarios) |>
    dplyr::mutate(driver = factor(driver, levels = s20_driver_levels))

  ggplot2::ggplot(d, ggplot2::aes(x = mean_pct_of_openfield, y = cv_pct)) +
    ggplot2::geom_vline(xintercept = 100, linetype = "dashed", colour = "grey50",
                         linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(colour = crop_phase, shape = driver),
                         size = 2.2, alpha = 0.9) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = scen_axis_label(scen_label), colour = crop_phase),
      size = 2.1, show.legend = FALSE, max.overlaps = 20, seed = 1,
      min.segment.length = 0.2, segment.size = 0.2
    ) +
    ggplot2::scale_colour_manual(values = s20_crop_phase_palette, name = "Crop phase") +
    ggplot2::scale_shape_manual(values = s20_driver_shapes, name = "Driver",
                                 limits = s20_driver_levels, drop = FALSE) +
    ggplot2::scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::labs(
      x = "Mean yield (% of open field)",
      y = "Interannual CV (%)",
      tag = "b",
      subtitle = "Down and to the right = more yield, less variable"
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 8),
                    legend.position = "right")
}

plot_fig_s20_c <- function(fig_s20_relative, scenarios = s20_scenarios_highlight) {
  d <- fig_s20_relative |>
    dplyr::filter(scen_label %in% scenarios, scen_label != "Reference") |>
    dplyr::mutate(driver = factor(driver, levels = s20_driver_levels))

  ggplot2::ggplot(d, ggplot2::aes(x = mean_pct_of_openfield, y = p10_pct_of_openfield)) +
    # The 1:1 line is the whole test: above it, the poor years lost
    # proportionally less than the average year.
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                          colour = "grey45", linewidth = 0.45) +
    ggplot2::geom_point(ggplot2::aes(colour = crop_phase, shape = driver),
                         size = 2.2, alpha = 0.9) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = scen_axis_label(scen_label), colour = crop_phase),
      size = 2.1, show.legend = FALSE, max.overlaps = 20, seed = 1,
      min.segment.length = 0.2, segment.size = 0.2
    ) +
    ggplot2::scale_colour_manual(values = s20_crop_phase_palette, name = "Crop phase") +
    # Shape guide suppressed: panels (b) and (c) sit side by side and share one
    # collected legend area, and the two shape scales are not byte-identical
    # (panel (c) has no open-field rows), so patchwork emits a second "Driver"
    # legend instead of merging them. Panel (b)'s guide covers both.
    ggplot2::scale_shape_manual(values = s20_driver_shapes,
                                 limits = s20_driver_levels, drop = FALSE,
                                 guide = "none") +
    ggplot2::scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::labs(
      x = "Mean yield (% of open-field mean)",
      y = "Poor-year yield (% of open-field p10)",
      tag = "c",
      subtitle = "Above the 1:1 line = poor years buffered (p10 of that crop's years)"
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 8),
                    legend.position = "right")
}

plot_fig_s20 <- function(fig_s20_data, fig_s20_relative) {
  patchwork::wrap_plots(
    plot_fig_s20_a(fig_s20_data),
    patchwork::wrap_plots(
      plot_fig_s20_b(fig_s20_relative),
      plot_fig_s20_c(fig_s20_relative),
      ncol = 2
    ) + patchwork::plot_layout(guides = "collect"),
    ncol = 1, heights = c(1, 1.15)
  )
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S21 - distance from carbon equilibrium.
#
# (a) simulated SOC (solid) with the fitted first-order approach projected
#     forward (dashed); (b) fraction of the total change achieved by 2024;
# (c) time to close 95% of the gap, against the 27-year simulated record.
#
# The point of the figure is the SHORTFALL, not the projection: it quantifies
# the Discussion's "not yet at equilibrium" caveat. See R/soc.R for the model
# and for why unidentifiable fits are labelled rather than plotted.
# ===========================================================================

plot_fig_s21_a <- function(fig_s21_data, soc_projection) {
  ggplot2::ggplot(mapping = ggplot2::aes(x = year, y = soc_tC_ha, colour = scen_label)) +
    ggplot2::geom_line(data = soc_projection, linetype = "22", linewidth = 0.45,
                        alpha = 0.85) +
    ggplot2::geom_line(data = fig_s21_data, linewidth = 0.75) +
    ggplot2::geom_vline(xintercept = 2024, linetype = "dotted", colour = "grey45",
                         linewidth = 0.4) +
    ggplot2::annotate("text", x = 2024, y = Inf, label = " end of simulation",
                       hjust = 0, vjust = 1.6, size = 2.3, colour = "grey35",
                       fontface = "italic") +
    # Full scenario labels, not scen_axis_label: this legend spans two drivers,
    # so a stripped "0%" would be indistinguishable from "0degC" at a glance.
    ggplot2::scale_colour_manual(values = scenario_palette_extended,
                                  name = "Scenario") +
    ggplot2::facet_wrap(~depth, ncol = 2, scales = "free_y") +
    ggplot2::labs(
      x = NULL, y = expression("Total SOC (t C ha"^-1*")"), tag = "a",
      subtitle = "Solid = simulated; dashed = fitted projection (50 yr)"
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 8),
                    legend.position = "right")
}

# Panel (b) is the robust diagnostic and, as it turns out, the main result:
# comparing the rate of SOC change in the first half of the record with the
# second. Equal slopes mean the trajectory has not begun to level off.
# Verdict is encoded as POINT COLOUR rather than as per-point text. Scenario is
# already on the x-axis, so colouring by scenario was redundant, and six rotated
# text labels per facet collided with the points they annotated.
soc_verdict_palette <- c(
  "accelerating" = "#B2182B",
  "still linear" = "#EF8A62",
  "decelerating" = "#67A9CF",
  "near equilibrium" = "#2166AC",
  "effectively flat" = "grey55",
  "reversed direction" = "#762A83"
)

plot_fig_s21_b <- function(soc_curvature_long, soc_curvature) {
  d <- soc_curvature_long |>
    dplyr::mutate(scen_label = factor(as.character(scen_label), levels = s21_scenarios))

  ggplot2::ggplot(d, ggplot2::aes(x = scen_label, y = slope)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.45) +
    # The connector shows the CHANGE in rate; a short segment means the rate
    # barely moved between the two halves, i.e. no approach to equilibrium.
    ggplot2::geom_line(ggplot2::aes(group = scen_label), colour = "grey60",
                        linewidth = 0.45) +
    ggplot2::geom_point(ggplot2::aes(colour = verdict, shape = half), size = 2.6) +
    ggplot2::scale_colour_manual(values = soc_verdict_palette, name = "Curvature") +
    ggplot2::scale_shape_manual(values = c(1, 16), name = "Period") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::facet_wrap(~depth, ncol = 2, scales = "free_y") +
    ggplot2::labs(
      x = NULL, y = expression("SOC trend (t C ha"^-1*" yr"^-1*")"), tag = "b",
      subtitle = "Open → filled shows how the rate changed; a flat pair means no approach to equilibrium"
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
                    legend.position = "right")
}

# Panel (c) keeps every scenario on the axis, including those with no fit, so
# the reader can see how many trajectories could not be extrapolated at all -
# that count is part of the result, not a gap in the figure.
plot_fig_s21_c <- function(soc_equilibrium_fits) {
  d <- soc_equilibrium_fits |>
    dplyr::mutate(scen_label = factor(as.character(scen_label), levels = s21_scenarios))

  # One summary label per facet rather than one per unfitted point: with five
  # of six topsoil series unfitted, per-point labels were six overlapping
  # rotated strings saying the same thing.
  unfitted <- d |>
    dplyr::group_by(depth) |>
    dplyr::summarise(
      n_missing = sum(is.na(t95_years)), n_total = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n_missing > 0) |>
    dplyr::mutate(label = sprintf(
      "%d of %d trajectories have no fittable curvature", n_missing, n_total))

  ggplot2::ggplot(d, ggplot2::aes(x = scen_label, y = t95_years, colour = scen_label)) +
    ggplot2::geom_hline(yintercept = 26, linetype = "dashed", colour = "grey35",
                         linewidth = 0.45) +
    ggplot2::annotate("text", x = Inf, y = 26, label = "simulated record ",
                       hjust = 1, vjust = -0.6, size = 2.2, colour = "grey30",
                       fontface = "italic") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = 0, ymax = t95_years),
                             linewidth = 0.45, alpha = 0.6, na.rm = TRUE) +
    ggplot2::geom_point(size = 2.6, na.rm = TRUE) +
    ggplot2::geom_text(
      data = unfitted, inherit.aes = FALSE,
      ggplot2::aes(x = -Inf, y = Inf, label = label),
      hjust = -0.04, vjust = 1.6, size = 2.3, colour = "grey35", fontface = "italic"
    ) +
    ggplot2::scale_colour_manual(values = scenario_palette_extended, guide = "none") +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.18))) +
    ggplot2::facet_wrap(~depth, ncol = 2) +
    ggplot2::labs(
      x = NULL, y = "Years to 95% of fitted equilibrium", tag = "c",
      caption = "Fitted horizons far exceed the simulated record, and most topsoil trajectories have no fittable curvature at all — both indicate the simulation ends well short of a new carbon steady state."
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7))
}

plot_fig_s21 <- function(fig_s21_data, soc_projection, soc_equilibrium_fits,
                          soc_curvature) {
  patchwork::wrap_plots(
    plot_fig_s21_a(fig_s21_data, soc_projection),
    plot_fig_s21_b(soc_curvature_long(soc_curvature), soc_curvature),
    plot_fig_s21_c(soc_equilibrium_fits),
    ncol = 1, heights = c(1.25, 1, 1)
  )
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S16 - SOC change attributed to rotation phase.
#
# Evidence for the per-crop-phase accumulation rates quoted in Results 3.2,
# which currently have no figure anywhere. Note the attribution convention
# documented in R/soc.R: these are crop-years ENDING with the named crop, not
# carbon fluxes caused by it.
# ===========================================================================

plot_fig_s16 <- function(fig_s16_data) {
  ggplot2::ggplot(fig_s16_data,
                  ggplot2::aes(x = phase_crop, y = mean_d_soc_MgC_ha_yr,
                               colour = scen_label, shape = scen_label)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.45) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_d_soc_MgC_ha_yr - sd_d_soc_MgC_ha_yr,
                   ymax = mean_d_soc_MgC_ha_yr + sd_d_soc_MgC_ha_yr),
      width = 0.2, linewidth = 0.3, alpha = 0.65,
      position = ggplot2::position_dodge(width = 0.65)
    ) +
    ggplot2::geom_point(size = 2.1, position = ggplot2::position_dodge(width = 0.65)) +
    # Crop-year count per phase, so the reader can see how much each mean rests
    # on - this is what the manuscript's "21 crop-years" claim refers to.
    ggplot2::geom_text(
      data = fig_s16_data |>
        dplyr::group_by(depth, phase_crop) |>
        dplyr::summarise(n = max(n_crop_years), .groups = "drop"),
      ggplot2::aes(x = phase_crop, y = -Inf, label = paste0("n=", n)),
      inherit.aes = FALSE, vjust = -0.8, size = 2.1, colour = "grey45"
    ) +
    # Full labels: this legend mixes radiation and temperature scenarios, and a
    # stripped "0%" next to "0degC" is ambiguous about which driver it is.
    ggplot2::scale_colour_manual(values = scenario_palette_extended, name = "Scenario") +
    ggplot2::scale_shape_manual(values = c(4, 16, 17, 15, 18), name = "Scenario") +
    ggplot2::facet_wrap(~depth, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = expression("ΔTotal SOC (Mg C ha"^-1*" yr"^-1*")"),
      caption = "Crop-year = 1 Sep to 1 Sep, ending with the harvest of the named crop; error bars are ±1 SD across crop-years pooled over the four rotation permutations."
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 8.5),
                    legend.position = "right")
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S13 - seasonal distribution of nitrate leaching.
#
# (a) mean monthly leaching; (b) cumulative share of the agrohydrological year;
# (c) monthly scenario effect relative to open field.
#
# Lines ARE used across months in (a) and (b), unlike the scenario axes
# elsewhere in this manuscript: month is an ordered time axis within a
# continuous drainage season, so consecutive points are not independent
# observations and connecting them is the correct reading.
# ===========================================================================

plot_fig_s13_a <- function(fig_s13_data) {
  ggplot2::ggplot(fig_s13_data,
                  ggplot2::aes(x = month_label, y = mean_leaching_kgN_ha,
                               colour = scen_label, group = scen_label)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pmax(0, mean_leaching_kgN_ha - sd_leaching_kgN_ha),
                   ymax = mean_leaching_kgN_ha + sd_leaching_kgN_ha,
                   fill = scen_label),
      alpha = 0.12, colour = NA
    ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_colour_manual(values = scenario_palette_extended, name = "Scenario") +
    ggplot2::scale_fill_manual(values = scenario_palette_extended, guide = "none") +
    ggplot2::labs(
      x = NULL, y = expression("N leaching (kg N ha"^-1*" month"^-1*")"), tag = "a",
      subtitle = "Agrohydrological year, April–March"
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7.5),
                    legend.position = "right")
}

plot_fig_s13_b <- function(fig_s13_data) {
  ggplot2::ggplot(fig_s13_data,
                  ggplot2::aes(x = month_label, y = cumulative_share_pct,
                               colour = scen_label, group = scen_label)) +
    ggplot2::geom_hline(yintercept = c(50, 90), linetype = "dotted",
                         colour = "grey50", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_colour_manual(values = scenario_palette_extended, name = "Scenario") +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::labs(
      x = NULL, y = "Cumulative share of annual leaching", tag = "b",
      subtitle = "Dotted lines mark the 50% and 90% points"
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7.5),
                    legend.position = "right")
}

plot_fig_s13_c <- function(fig_s13_delta) {
  ggplot2::ggplot(fig_s13_delta,
                  ggplot2::aes(x = month_label, y = delta_kgN_ha, fill = scen_label)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.45) +
    ggplot2::geom_col(width = 0.7, alpha = 0.9) +
    ggplot2::scale_fill_manual(values = scenario_palette_extended, guide = "none") +
    # Full scenario labels in the strips: stripping the driver prefix here left
    # facets titled "0%" and "-30%" with no indication they were radiation.
    ggplot2::facet_wrap(~scen_label, nrow = 1) +
    ggplot2::labs(
      x = NULL, y = expression("Δ N leaching vs open field (kg N ha"^-1*" month"^-1*")"),
      tag = "c",
      caption = "Bars below zero are months in which that scenario leached less than the open field. Absolute values are profile-bottom leaching and are not directly comparable to Figure 5's 100 cm totals."
    ) +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
                    strip.text = ggplot2::element_text(size = 8))
}

plot_fig_s13 <- function(fig_s13_data, fig_s13_delta) {
  patchwork::wrap_plots(
    patchwork::wrap_plots(plot_fig_s13_a(fig_s13_data), plot_fig_s13_b(fig_s13_data),
                           ncol = 2) + patchwork::plot_layout(guides = "collect"),
    plot_fig_s13_c(fig_s13_delta),
    ncol = 1, heights = c(1, 0.9)
  )
}

# ===========================================================================
# SUPPLEMENT v2, FIGURE S7 - water and nitrogen mass balance (composite).
#
# One page-shaped figure stacking the water balance (S10), the N fluxes and
# leaching by regime, the N budget (S12) and the seasonal leaching panels (S13):
#   a  annual water-balance components            plot_fig_s10()
#   b  N fluxes and N leaching, a row per regime  plot_fig_s07_v2_b()
#   c  stacked N budget                           plot_fig_s12()
#   d  monthly leaching, e  cumulative share      plot_fig_s13_a(), plot_fig_s13_b()
#   f  monthly change against the open field      plot_fig_s13_c()
# Panel captions are dropped (the Word caption carries them) and tags run a-f.
#
# The original builder of the 14 Sept composite was lost, so this is a
# reconstruction, checked against that file: with the plain S11(a) flux panel in
# place of (b) and panel heights 1 : 1.5 : 1 : 1 : 0.9, an earlier version of this
# function reproduced it byte for byte. That pins down the canvas - 9 x 16 in at
# 320 dpi; the panels' text is drawn at native size, so canvas and dpi set how
# crowded the figure looks - and the treatment of panels a, c, d, e and f.
#
# Panel (b) was then redone in the design of Figure 5 (fluxes on the left axis,
# leaching on a right axis in a shaded band), one row per regime, and the heights
# rebalanced to make room for it. The canvas is unchanged. Panels (d)-(f) came out
# shorter as a result, so their y titles are broken over two lines (see below).
#
# At one 6 x 9 in text block per page the one-page composite has to be shrunk to
# about half size, so the Word supplement carries the figure on two pages instead,
# panels (a)-(c) on the first and (d)-(f) on the second. Each part is built on its
# own canvas, shaped for the text block: plot_fig_s07_v2_part1() and
# plot_fig_s07_v2_part2(). They reuse the panels of the composite; only the canvas
# and the panel heights differ.
# ===========================================================================

# Panel (b): Figure 5's panel repeated for every fertilisation x residue regime.
# Each row shows the three fluxes (left axis) and that regime's leaching (right
# axis; drawn at fig_05_leach_scale x its value inside the shaded band), in
# Figure 5's colours, with Figure 5's two-column legend underneath. The regime
# names sit inside the panels rather than in strips at the right: a strip and a
# right-hand axis compete for the same side of the panel.
plot_fig_s07_v2_b <- function(fig_s11_a_data, fig_05_b_data) {
  k <- fig_05_leach_scale
  band_top <- fig_05_leach_axis_max * k

  flux_cols <- c("Mineralisation" = "#8C6D31",
                 "Crop N uptake" = "#4EA72E",
                 "Biological N fixation" = "#0072B2")
  flux_shapes <- c("Mineralisation" = 16, "Crop N uptake" = 17, "Biological N fixation" = 15)
  regimes <- levels(fig_05_b_data$management_label)

  fluxes <- fig_s11_a_data |>
    dplyr::transmute(management_label, series = as.character(flux), scen_label, driver,
                     y = mean_kgN_ha, lo = mean_kgN_ha - sd_kgN_ha, hi = mean_kgN_ha + sd_kgN_ha)
  leaching <- fig_05_b_data |>
    dplyr::transmute(management_label, series = as.character(management_label), scen_label, driver,
                     y = k * mean_kgN_ha,
                     lo = k * (mean_kgN_ha - sd_kgN_ha), hi = k * (mean_kgN_ha + sd_kgN_ha))
  if (any(leaching$hi > band_top)) {
    warning("plot_fig_s07_v2_b(): a leaching error bar reaches above the right axis maximum (",
            fig_05_leach_axis_max, " kg N/ha/yr); raise fig_05_leach_axis_max.")
  }

  # Legend headings and padding are pseudo-series with no data, exactly as in
  # plot_fig_05() (see the note there).
  lv <- c("hdr_flux", names(flux_cols), "blank", "hdr_leach", regimes)
  d <- dplyr::bind_rows(fluxes, leaching) |>
    dplyr::mutate(series = factor(series, levels = lv))
  cols <- c(hdr_flux = "transparent", flux_cols, blank = "transparent",
            hdr_leach = "transparent", fig_05_regime_cols[regimes])
  shapes <- c(hdr_flux = NA, flux_shapes, blank = NA, hdr_leach = NA, fig_05_regime_shapes[regimes])
  labels <- c(hdr_flux = "**N fluxes** (left axis)",
              stats::setNames(names(flux_cols), names(flux_cols)),
              blank = "",
              hdr_leach = "**N leaching** (right axis; by management)",
              stats::setNames(regimes, regimes))
  dodge <- ggplot2::position_dodge(width = 0.86)

  # Regime names, two lines each, at the top left of the Radiation panel just
  # right of the Ref divider, above the flux markers.
  regime_labels <- data.frame(
    management_label = factor(regimes, levels = regimes),
    driver = factor("Radiation", levels = levels(d$driver)),
    y = fig_05_flux_axis_max - 8,
    label = stringr::str_replace(regimes, stringr::fixed(" | "), "\n")
  )

  ggplot2::ggplot(d, ggplot2::aes(x = scen_label, y = y, colour = series, shape = series)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = band_top, fill = "grey95") +
    ggplot2::geom_hline(yintercept = k * seq(10, fig_05_leach_axis_max - 10, 10),
                        colour = "white", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = band_top, colour = "grey75", linewidth = 0.25) +
    ggplot2::geom_vline(xintercept = 1.5, linetype = "dotted", colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi),
                           width = 0.14, linewidth = 0.3, alpha = 0.75, position = dodge) +
    ggplot2::geom_point(size = 1.7, position = dodge) +
    ggplot2::geom_text(data = regime_labels, ggplot2::aes(x = 1.62, y = y, label = label),
                       inherit.aes = FALSE, hjust = 0, vjust = 1, size = 2.2, lineheight = 0.9,
                       fontface = "bold", colour = "grey20") +
    ggplot2::scale_colour_manual(name = NULL, values = cols, breaks = lv, labels = labels, drop = FALSE) +
    ggplot2::scale_shape_manual(name = NULL, values = shapes, breaks = lv, labels = labels, drop = FALSE) +
    ggplot2::scale_x_discrete(labels = scen_axis_label) +
    ggplot2::scale_y_continuous(
      name = expression("N flux (kg N ha"^-1~"yr"^-1*")"),
      breaks = seq(0, 300, 100), limits = c(0, max(fig_05_flux_axis_max, d$hi)),
      expand = ggplot2::expansion(mult = c(0, 0.02)),
      sec.axis = ggplot2::sec_axis(~ . / k, name = expression("N leaching (kg N ha"^-1~"yr"^-1*")"),
                                   breaks = c(0, 20, 40))
    ) +
    ggplot2::facet_grid(management_label ~ driver, scales = "free_x", space = "free_x") +
    ggplot2::labs(x = NULL, tag = "b") +
    theme_manuscript() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y.right = ggplot2::element_text(size = 7),
      strip.text.y = ggplot2::element_blank(),
      strip.background.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggtext::element_markdown(size = 9),
      legend.key.height = ggplot2::unit(0.9, "lines")
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 5, byrow = FALSE),
                    shape = ggplot2::guide_legend(nrow = 5, byrow = FALSE))
}

# Panel (b) has a right-hand axis, and patchwork lines up the side columns of
# stacked plots by position: a real secondary axis in (b) is wedged in behind the
# strip column that panel (c) needs, far from its own panels, and squeezes every
# panel above and below. wrap_elements() takes (b) out of that alignment; two
# margins put its panel edges where its neighbours' are. right_in is 2.697 in in
# all versions (panel edge at 1853 px at 320 dpi); left_in depends on the widest
# left-hand decoration among the panels stacked with it, and was measured for each
# version: 0.334 in in the one-page composite (333 px; the two-line y titles of
# (d) and (f) widen the column), 0.118 in in part 1 (264 px). The left margin is
# carried by the y title's own margin, apart from the default 5.5 pt of plot
# margin, so that the panel tag stays in line with the other tags. If a neighbour's
# side decoration changes, re-measure and adjust.
s07_v2_panel_b <- function(fig_s11_a_data, fig_05_b_data, left_in, right_in = 2.697) {
  plot_fig_s07_v2_b(fig_s11_a_data, fig_05_b_data) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, right_in * 72.27, 5.5, 5.5, unit = "pt"),
                   axis.title.y = ggplot2::element_text(
                     margin = ggplot2::margin(r = 2.75, l = left_in * 72.27 - 5.5, unit = "pt")))
}

# Panels (d)-(f) are shorter in the one-page composite than they were in its first
# version (panel (b) needs the room), and their one-line y titles no longer fit
# inside them: they ran into each other and into the panel tags. Broken over two
# lines here, in the composite only; the stand-alone S13 figure keeps its one-line
# titles. The two parts use the same panels, so the text is the same throughout.
s07_v2_panels_def <- function(fig_s13_data, fig_s13_delta) {
  list(
    d = plot_fig_s13_a(fig_s13_data) +
      ggplot2::labs(tag = "d", y = expression(atop("N leaching", "(kg N ha"^-1*" month"^-1*")"))),
    e = plot_fig_s13_b(fig_s13_data) +
      ggplot2::labs(tag = "e", y = "Cumulative share of\nannual leaching"),
    f = plot_fig_s13_c(fig_s13_delta) +
      ggplot2::labs(caption = NULL, tag = "f",
                    y = expression(atop("Δ N leaching vs open field", "(kg N ha"^-1*" month"^-1*")")))
  )
}

plot_fig_s07_v2 <- function(fig_s10_data, fig_s11_a_data, fig_05_b_data, fig_s12_data,
                            fig_s13_data, fig_s13_delta) {
  p_b <- s07_v2_panel_b(fig_s11_a_data, fig_05_b_data, left_in = 0.334)
  p_a <- plot_fig_s10(fig_s10_data) + ggplot2::labs(caption = NULL, tag = "a")
  p_c <- plot_fig_s12(fig_s12_data) + ggplot2::labs(caption = NULL, tag = "c")
  def <- s07_v2_panels_def(fig_s13_data, fig_s13_delta)
  p_de <- (def$d | def$e) + patchwork::plot_layout(guides = "collect")

  # (b) is a self-contained element, so its share of the height includes its own
  # x axis and legend. Rebalanced so that its rows stay readable and panel (c)'s
  # four short rows do not lose their axis labels.
  (p_a / patchwork::wrap_elements(full = p_b) / p_c / p_de / def$f) +
    patchwork::plot_layout(heights = c(0.9, 3.5, 1.15, 0.9, 0.85))
}

# Part 1 of the two-page version: panels (a)-(c), on a canvas of the text block's
# shape. The extra height goes to the rows of (b) and (c), whose rows and tick
# labels are the tightest in the one-page composite.
plot_fig_s07_v2_part1 <- function(fig_s10_data, fig_s11_a_data, fig_05_b_data, fig_s12_data) {
  p_b <- s07_v2_panel_b(fig_s11_a_data, fig_05_b_data, left_in = 0.118)
  p_a <- plot_fig_s10(fig_s10_data) + ggplot2::labs(caption = NULL, tag = "a")
  p_c <- plot_fig_s12(fig_s12_data) + ggplot2::labs(caption = NULL, tag = "c")
  (p_a / patchwork::wrap_elements(full = p_b) / p_c) +
    patchwork::plot_layout(heights = c(2.4, 7, 3.1))
}

# Part 2: panels (d)-(f); the panels are as wide as in the one-page composite but
# have more than twice its height.
plot_fig_s07_v2_part2 <- function(fig_s13_data, fig_s13_delta) {
  def <- s07_v2_panels_def(fig_s13_data, fig_s13_delta)
  p_de <- (def$d | def$e) + patchwork::plot_layout(guides = "collect")
  (p_de / def$f) + patchwork::plot_layout(heights = c(1, 1))
}

plot_fig_02 <- function(fig_02_a_data, fig_02_b_data, fig_02_a_system_data) {
  patchwork::wrap_plots(
    plot_fig_02_a_agb_by_crop(fig_02_a_data, fig_02_a_system_data),
    plot_fig_02_b_grain_response(fig_02_b_data),
    ncol = 1, heights = c(1, 1)
  )
}
#
# Figure 4: ported with full fidelity from
# reference/legacy_snapshot/SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd, chunk
# `fig04_manuscript_heatmap_agb`.
#
# Figure 5: the underlying DATA (prepare_fig05_data()) is ported with full
# fidelity from chunk `fig05_manuscript_rel_grain_yield`. The PLOT below does
# not replicate that chunk's bespoke per-driver background-shading rectangles
# and dual hand-built legends (block_bg_data_b2_ecw_18_1, weather_leg_b2_ecw_18_1,
# etc.) - tracing those fully would mean porting a large, mostly-decorative
# side-chain of helper objects. Rendered instead with the shared
# theme_manuscript() design system: same data, same encoding (bars = mean
# response, error bars = SD across rotations, facet by crop, pattern by
# residue policy), simpler chrome. Flagged here as an open item, not a
# silent substitution - see the run-test report.

plot_fig04 <- function(fig04_data, management_filter = c(
  "Biogas digestate | Residue Removed",
  "Mineral fertiliser | Residue Removed"
)) {
  plot_data <- fig04_data |> dplyr::filter(management_label %in% management_filter)

  mgmt_labeller <- ggplot2::as_labeller(c(
    "Mineral fertiliser | Residue Removed" = "Min–Rem",
    "Biogas digestate | Residue Removed" = "Dig–Rem"
  ))
  fill_limit <- 100

  ggplot2::ggplot(plot_data, ggplot2::aes(x = scenario_axis_f, y = crop_renamed, fill = relative_effect_pct)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_tile(
      data = dplyr::filter(plot_data, is_vapv_zero),
      ggplot2::aes(x = scenario_axis_f, y = crop_renamed),
      fill = NA, colour = "grey30", linewidth = 0.85
    ) +
    ggplot2::geom_text(ggplot2::aes(label = tile_label, colour = text_colour), size = 2.5, fontface = "plain") +
    ggplot2::scale_colour_identity() +
    ggplot2::facet_grid(
      management_label ~ weather_factor,
      scales = "free_x", space = "free_x",
      labeller = ggplot2::labeller(management_label = mgmt_labeller)
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#B2182B", mid = "white", high = "#1A9850", midpoint = 0,
      limits = c(-fill_limit, fill_limit), oob = scales::squish, na.value = "grey80",
      name = "Change (%)", breaks = c(-100, -50, 0, 50, 100),
      labels = c("≤-100", "-50", "0", "+50", "≥+100")
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_manuscript() +
    ggplot2::theme(
      axis.text.x = ggtext::element_markdown(angle = 55, hjust = 1, vjust = 1, size = 7.5),
      axis.text.y = ggplot2::element_text(size = 9),
      strip.text.x = ggplot2::element_text(face = "bold", size = 9),
      strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, face = "bold", size = 8.5),
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(2.5, "cm")
    )
}

plot_fig05 <- function(fig05_data, fertiliser_filter = "Biogas digestate") {
  plot_data <- fig05_data |> dplyr::filter(fertiliser_type == fertiliser_filter)

  driver_of <- function(scen_label) {
    x <- as.character(scen_label)
    dplyr::case_when(
      stringr::str_starts(x, "Rad") ~ "Radiation",
      stringr::str_starts(x, "Wind") ~ "Wind",
      stringr::str_starts(x, "Tmp") ~ "Temperature",
      TRUE ~ "Other"
    )
  }
  plot_data <- plot_data |> dplyr::mutate(driver = factor(driver_of(scen_label), levels = names(driver_palette)))

  ggplot2::ggplot(plot_data, ggplot2::aes(x = scen_label, y = mean_resp, fill = driver, pattern = residue_policy)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
    ggpattern::geom_col_pattern(
      position = ggplot2::position_dodge(width = 0.82), width = 0.82,
      colour = "grey25", linewidth = 0.15,
      pattern_fill = "white", pattern_colour = "white",
      pattern_density = 0.5, pattern_spacing = 0.038, pattern_angle = 45
    ) +
    ggpattern::scale_pattern_manual(values = c("Residue removed" = "none", "Residue retained" = "stripe")) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_resp - sd_resp, ymax = mean_resp + sd_resp, group = residue_policy),
      position = ggplot2::position_dodge(width = 0.82), width = 0.28, linewidth = 0.4, colour = "grey12"
    ) +
    ggplot2::scale_fill_manual(values = driver_palette, name = "Driver") +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::facet_wrap(~crop_renamed, ncol = 1, scales = "free_y") +
    ggplot2::labs(x = NULL, y = "Grain yield response (% of reference)", pattern = "Residue") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 8))
}

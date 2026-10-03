# Methods figure: site climatology, rotation design, and interannual variability.
#
# ---------------------------------------------------------------------------
# WHAT THIS FIGURE IS FOR, AND HOW IT DIFFERS FROM Fig. S1
# ---------------------------------------------------------------------------
# Supplementary Fig. S1 shows the DAILY weather record - the actual series the
# model was driven with, at full resolution, for a four-year window. That is a
# provenance artefact: it documents the forcing.
#
# This figure is the opposite: it gives the CLIMATOLOGY and the DESIGN, i.e.
# the context a reader needs before any result can be interpreted. It answers
# three questions that the daily record cannot:
#
#   (a,b) What kind of site is this agronomically? A 27-year monthly climatology
#         of temperature and of the precipitation/evaporative-demand balance
#         locates the seasonal water-deficit window - which is what makes the
#         SPEI-based drought analysis meaningful rather than arbitrary.
#   (c)   What did the four rotation permutations actually do? Each permutation
#         meets a different set of weather years, so the same crop is grown
#         under a different climatic sample in each. This panel makes the
#         phase-shift design visible, and shows which crop-years were dry.
#   (d)   How variable was the record? Annual SPEI-12 places the simulated
#         period in context and identifies the extreme years.
#
# Panels (a) and (b) are purely EMPIRICAL - they describe the measured weather
# record. Panels (c) and (d) bridge to the MODELLING: (c) is the simulated
# rotation design overlaid with the empirical drought classification, and (d)
# is the index that classification is built on.
# ---------------------------------------------------------------------------

# Analysis-period bounds for the climatology. The 1997 spin-up months are
# excluded so that the monthly means describe the evaluation period, not a
# partial extra year that would bias August-December.
MM_CLIMATOLOGY_YEARS <- c(1998L, 2024L)

month_abbrev_levels <- month.abb

# --- (a) Temperature climatology ------------------------------------------
# Mean of daily means, with the mean daily minimum and maximum as an envelope.
# The envelope is the mean diurnal range, NOT a between-year spread: for an
# agronomic site description the reader wants to know how cold nights get, and
# a between-year SD would hide that inside a symmetric band.
prepare_fig_mm_temperature <- function(openfield_weather_daily,
                                        years = MM_CLIMATOLOGY_YEARS) {
  openfield_weather_daily |>
    dplyr::filter(year >= years[1], year <= years[2]) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      mean_temp_c = mean(air_temp_c, na.rm = TRUE),
      mean_tmin_c = mean(air_temp_min_c, na.rm = TRUE),
      mean_tmax_c = mean(air_temp_max_c, na.rm = TRUE),
      sd_temp_c = sd(air_temp_c, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(month_label = factor(month.abb[month], levels = month_abbrev_levels))
}

# --- (b) Agroclimatic water balance ---------------------------------------
# Monthly precipitation against potential evapotranspiration, both as means
# over the evaluation period. Where PET exceeds precipitation the crop is
# drawing on stored soil water - that window is the reason a drought index is
# needed at all, and it is what crop_spei_windows are positioned against.
prepare_fig_mm_water <- function(monthly_water_balance,
                                  years = MM_CLIMATOLOGY_YEARS) {
  monthly_water_balance |>
    dplyr::filter(year >= years[1], year <= years[2]) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      mean_precip_mm = mean(precipitation_mm, na.rm = TRUE),
      sd_precip_mm = sd(precipitation_mm, na.rm = TRUE),
      mean_pet_mm = mean(pet_mm, na.rm = TRUE),
      sd_pet_mm = sd(pet_mm, na.rm = TRUE),
      n_years = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      month_label = factor(month.abb[month], levels = month_abbrev_levels),
      deficit_mm = pmax(mean_pet_mm - mean_precip_mm, 0),
      is_deficit = mean_pet_mm > mean_precip_mm
    )
}

# --- (c) Crop sequence across the four rotation permutations ---------------
# One cell per rotation x year. Fill = the crop grown; point size = that
# crop-year's harvested biomass relative to the crop's own maximum, so a
# cereal cell and a grass-clover cell are comparable despite differing by a
# factor of three in absolute yield; point outline = the crop's own seasonal
# SPEI-3 class for that year.
#
# Normalising WITHIN crop is the load-bearing choice. Without it the panel
# would simply restate that grass-clover out-yields everything, which the
# fill colour already says, and the year-to-year signal - the thing the panel
# exists to show - would be invisible.
prepare_fig_mm_calendar <- function(harvest_annual, crop_drought_years,
                                     rotations = paste("Rotation", 1:4),
                                     start_year = ANALYSIS_START_YEAR,
                                     end_year = 2024L) {
  phases <- assign_rotation_phase_crop(harvest_annual, rotations = rotations,
                                        start_year = start_year)

  agb <- harvest_annual |>
    dplyr::filter(weather_is_baseline, rotation %in% rotations,
                  year >= start_year, year <= end_year,
                  management_label == "Biogas digestate | Residue Removed") |>
    dplyr::group_by(rotation, year) |>
    dplyr::summarise(agb = sum(harvested_agb_removed_MgDM_ha, na.rm = TRUE),
                     .groups = "drop")

  phases |>
    dplyr::filter(year >= start_year, year <= end_year) |>
    dplyr::left_join(agb, by = c("rotation", "year")) |>
    dplyr::left_join(
      dplyr::select(crop_drought_years, crop_renamed, year, drought_class),
      by = c("phase_crop" = "crop_renamed", "year")
    ) |>
    dplyr::group_by(phase_crop) |>
    dplyr::mutate(rel_agb = agb / max(agb, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      phase_crop = factor(phase_crop, levels = crop_levels_all),
      rotation = factor(rotation, levels = rev(rotations)),
      drought_class = factor(
        dplyr::coalesce(drought_class, "Not classified"),
        levels = c("Dry", "Near-normal", "Wet", "Not classified")
      )
    ) |>
    dplyr::filter(!is.na(phase_crop))
}

# --- (d) Interannual variability, SPEI-12 ----------------------------------
# SPEI-12 evaluated at December summarises the whole calendar year, so one
# value per year. Scale 12, not the scale 3 used for the crop-window
# classification: a 12-month accumulation describes the year as a whole, while
# a 3-month accumulation is what matches a crop's critical window.
prepare_fig_mm_spei12 <- function(monthly_water_balance,
                                   start_year = ANALYSIS_START_YEAR,
                                   end_year = 2024L,
                                   n_label = 3L) {
  annual <- calculate_spei(monthly_water_balance, scale = 12L) |>
    dplyr::filter(month == 12L, year >= start_year, year <= end_year) |>
    dplyr::select(year, spei12 = spei) |>
    dplyr::filter(is.finite(spei12)) |>
    dplyr::mutate(anomaly = dplyr::if_else(spei12 >= 0, "Wet / cool", "Dry / warm"))

  # Label only the few most extreme years in each direction, so the panel
  # names the years the Results text refers to without becoming a wall of text.
  extremes <- dplyr::bind_rows(
    dplyr::slice_max(annual, spei12, n = n_label),
    dplyr::slice_min(annual, spei12, n = n_label)
  )

  dplyr::mutate(annual, label_year = dplyr::if_else(year %in% extremes$year,
                                                     as.character(year), NA_character_))
}

# Compact site summary used as the figure subtitle, computed rather than
# hard-coded so it cannot drift from the weather record it describes.
summarise_site_climate <- function(openfield_weather_daily, monthly_water_balance,
                                    years = MM_CLIMATOLOGY_YEARS) {
  w <- dplyr::filter(openfield_weather_daily, year >= years[1], year <= years[2])
  wb <- dplyr::filter(monthly_water_balance, year >= years[1], year <= years[2])

  tibble::tibble(
    mean_annual_temp_c = mean(w$air_temp_c, na.rm = TRUE),
    mean_annual_precip_mm = sum(wb$precipitation_mm, na.rm = TRUE) /
      dplyr::n_distinct(wb$year),
    mean_annual_pet_mm = sum(wb$pet_mm, na.rm = TRUE) / dplyr::n_distinct(wb$year),
    mean_wind_m_s = mean(w$wind_m_s, na.rm = TRUE),
    n_years = dplyr::n_distinct(w$year)
  )
}

# ===========================================================================
# Plotting. Kept here rather than in figures.R because this figure is a
# self-contained methods artefact with no shared prep, and splitting it would
# mean opening two files to change one panel.
# ===========================================================================

mm_drought_palette <- c(
  "Dry"            = "#B2182B",
  "Near-normal"    = "grey55",
  "Wet"            = "#2166AC",
  "Not classified" = "grey85"
)

plot_fig_mm_a <- function(fig_mm_temperature) {
  ggplot2::ggplot(fig_mm_temperature, ggplot2::aes(x = month_label, group = 1)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", colour = "grey55",
                         linewidth = 0.4) +
    # Grey band = mean daily min-max range; black mean line. Both neutral so
    # nothing here is confused with the red potential-ET line in panel (b).
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_tmin_c, ymax = mean_tmax_c),
                          fill = "grey40", alpha = 0.22) +
    ggplot2::geom_line(ggplot2::aes(y = mean_temp_c), colour = "black",
                        linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = mean_temp_c), colour = "black", size = 1.5) +
    ggplot2::labs(x = NULL, y = "Air temperature (°C)", tag = "a") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7.5))
}

plot_fig_mm_b <- function(fig_mm_water) {
  # Shade only where evaporative demand exceeds supply: that window is the
  # panel's message, so it is drawn explicitly rather than left for the reader
  # to infer from two overlapping series.
  deficit <- dplyr::filter(fig_mm_water, is_deficit)

  ggplot2::ggplot(fig_mm_water, ggplot2::aes(x = month_label, group = 1)) +
    ggplot2::geom_col(ggplot2::aes(y = mean_precip_mm, fill = "Precipitation"),
                       width = 0.65, alpha = 0.85) +
    ggplot2::geom_ribbon(data = deficit,
                          ggplot2::aes(ymin = mean_precip_mm, ymax = mean_pet_mm),
                          fill = "#B2182B", alpha = 0.16) +
    ggplot2::geom_line(ggplot2::aes(y = mean_pet_mm, colour = "Potential ET"),
                        linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(y = mean_pet_mm, colour = "Potential ET"),
                         size = 1.5) +
    # Placed over the autumn months, where both series are low, rather than at
    # the deficit's own centre - there it landed on the June PET peak.
    ggplot2::annotate("text", x = 9.6, y = max(fig_mm_water$mean_pet_mm) * 0.92,
                       label = "atmospheric\nwater deficit", size = 2.5,
                       hjust = 0.5, lineheight = 0.95,
                       fontface = "italic", colour = "#B2182B") +
    ggplot2::annotate("segment", x = 9.0, xend = 7.6,
                       y = max(fig_mm_water$mean_pet_mm) * 0.90,
                       yend = max(fig_mm_water$mean_pet_mm) * 0.85,
                       colour = "#B2182B", linewidth = 0.3,
                       arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm"))) +
    ggplot2::scale_fill_manual(values = c("Precipitation" = "#4393C3"), name = NULL) +
    ggplot2::scale_colour_manual(values = c("Potential ET" = "#B2182B"), name = NULL) +
    ggplot2::labs(x = NULL, y = expression("Water flux (mm month"^-1*")"), tag = "b") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7.5),
                    legend.position = "right")
}

plot_fig_mm_c <- function(fig_mm_calendar) {
  ggplot2::ggplot(fig_mm_calendar, ggplot2::aes(x = year, y = rotation)) +
    ggplot2::geom_tile(ggplot2::aes(fill = phase_crop), colour = "white",
                        linewidth = 0.5, alpha = 0.28) +
    # show.legend suppresses only this layer's FILL key: without it the "Crop"
    # legend renders a point on top of each colour square, because tile and
    # point share the fill scale.
    ggplot2::geom_point(ggplot2::aes(size = rel_agb, colour = drought_class,
                                      fill = phase_crop),
                         shape = 21, stroke = 1.1,
                         show.legend = c(fill = FALSE, colour = TRUE, size = TRUE)) +
    ggplot2::scale_fill_manual(values = crop_palette, name = "Crop") +
    ggplot2::scale_colour_manual(values = mm_drought_palette,
                                  name = "Seasonal SPEI-3\n(point outline)") +
    ggplot2::scale_size_continuous(range = c(1.2, 5),
                                    name = "Harvested AGB\n(% of crop maximum)",
                                    labels = scales::label_percent(accuracy = 1),
                                    breaks = c(0.25, 0.5, 0.75, 1)) +
    ggplot2::scale_x_continuous(breaks = seq(1998, 2024, 4), expand = c(0.01, 0.01)) +
    ggplot2::labs(x = NULL, y = NULL, tag = "c") +
    theme_manuscript() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7.5),
      legend.position = "right",
      legend.key.size = ggplot2::unit(0.4, "cm")
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(override.aes = list(alpha = 0.6), order = 1),
      colour = ggplot2::guide_legend(override.aes = list(size = 3.2, fill = "white"),
                                      order = 2),
      size = ggplot2::guide_legend(override.aes = list(colour = "grey45", fill = "grey85"),
                                    order = 3)
    )
}

plot_fig_mm_d <- function(fig_mm_spei12) {
  ggplot2::ggplot(fig_mm_spei12, ggplot2::aes(x = year, y = spei12, fill = anomaly)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +
    ggplot2::geom_hline(yintercept = c(-1, 1), linetype = "dotted",
                         colour = "grey60", linewidth = 0.35) +
    ggplot2::geom_col(width = 0.7, alpha = 0.9) +
    ggrepel::geom_text_repel(ggplot2::aes(label = label_year), size = 2.4,
                              colour = "grey20", na.rm = TRUE, seed = 1,
                              min.segment.length = 0.3, segment.size = 0.2,
                              direction = "y", show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c("Wet / cool" = "#4393C3",
                                           "Dry / warm" = "#B2182B"),
                                name = "Annual anomaly") +
    ggplot2::scale_x_continuous(breaks = seq(1998, 2024, 4), expand = c(0.01, 0.01)) +
    ggplot2::labs(x = NULL, y = "SPEI-12", tag = "d") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7.5),
                    legend.position = "right")
}

plot_fig_mm <- function(fig_mm_temperature, fig_mm_water, fig_mm_calendar,
                         fig_mm_spei12, site_climate) {
  # No figure title or header text baked into the plot - journal style keeps
  # that in the caption. `site_climate` is kept in the signature (now unused)
  # so the _targets.R call needs no change.
  top <- patchwork::wrap_plots(plot_fig_mm_a(fig_mm_temperature),
                                plot_fig_mm_b(fig_mm_water),
                                ncol = 2, widths = c(1, 1.3))

  patchwork::wrap_plots(top,
                         plot_fig_mm_c(fig_mm_calendar),
                         plot_fig_mm_d(fig_mm_spei12),
                         ncol = 1, heights = c(1, 1.05, 0.85))
}

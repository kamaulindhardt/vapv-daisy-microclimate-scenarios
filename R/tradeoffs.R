# Productivity-environment trade-offs (manuscript Figure 7, the synthesis figure).
#
# This is the only figure that crosses domains, so it consumes the productivity
# and nitrogen targets rather than re-deriving anything: yield comes from
# calculate_system_asy() and leaching from summarise_n_fluxes(). Re-deriving
# either here is how the legacy project ended up with the same quantity
# computed several different ways.
#
# Ported from reference/legacy_snapshot: GSA_VAPV_MICROCLIMATE_TRADEOFF_MITIGATION_DAISY.Rmd,
# and SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd chunks `fig13_productivity_environment_tradeoff`, Figs A8/A8b.

# Yield as % of the management-matched open-field baseline. Management-matched
# because fertilisation and residue shift the absolute yield level, so a single
# global baseline would fold the management effect into the scenario effect -
# the same reasoning as the SOC baseline in R/soc.R.
add_relative_yield <- function(asy_table) {
  asy_table |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::group_by(fertilisation, residue) |>
    dplyr::mutate(
      reference_asy = system_asy_MgDM_ha_yr[weather_is_baseline][1],
      relative_yield_pct = 100 * system_asy_MgDM_ha_yr / reference_asy
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(reference_asy), reference_asy > 0)
}

# Join yield against N leaching on scenario x management. Both sides are already
# four-rotation means, so no further aggregation happens here.
build_yield_leaching_tradeoff <- function(asy_table, n_flux_summary) {
  leaching <- n_flux_summary |>
    dplyr::filter(flux == "N leaching") |>
    dplyr::select(fertilisation, residue, management_label, scen_label,
                  leaching_kgN_ha = mean_kgN_ha, leaching_sd_kgN_ha = sd_kgN_ha)

  add_relative_yield(asy_table) |>
    dplyr::inner_join(leaching, by = c("fertilisation", "residue", "scen_label")) |>
    dplyr::mutate(
      driver = dplyr::case_when(
        weather_is_baseline ~ "Open field",
        stringr::str_starts(scen_label, "Rad") ~ "Radiation",
        stringr::str_starts(scen_label, "Tmp") ~ "Temperature",
        stringr::str_starts(scen_label, "Wind") ~ "Wind",
        TRUE ~ NA_character_
      ),
      driver = factor(driver, levels = c("Open field", "Radiation", "Temperature", "Wind")),
      management_label = factor(management_label, levels = management_levels)
    ) |>
    dplyr::filter(!is.na(driver), !is.na(management_label))
}

# The open-field anchor per management regime, for the parity crosshairs.
# Derived from the data rather than hard-coded, so the lines cannot drift out
# of step with the points they annotate.
build_tradeoff_reference_lines <- function(tradeoff_data) {
  tradeoff_data |>
    dplyr::filter(driver == "Open field") |>
    dplyr::group_by(management_label) |>
    dplyr::summarise(
      reference_yield_pct = mean(relative_yield_pct, na.rm = TRUE),
      reference_leaching_kgN_ha = mean(leaching_kgN_ha, na.rm = TRUE),
      .groups = "drop"
    )
}

prepare_fig_07_a_data <- function(system_asy, n_flux_summary) {
  build_yield_leaching_tradeoff(system_asy, n_flux_summary) |>
    dplyr::mutate(yield_metric = "All-crop ASY AGB")
}

prepare_fig_07_b_data <- function(grain_asy, n_flux_summary) {
  build_yield_leaching_tradeoff(grain_asy, n_flux_summary) |>
    dplyr::mutate(yield_metric = "Grain ASY (WW + SY)")
}

# ===========================================================================
# Supplementary Figure S12 - yield-SOC trade-off
# ===========================================================================
# Extends the Figure 7 trade-off from N leaching to soil carbon, and supplies
# the OLS slope of %dSOC on %yield per driver per management.
#
# This slope is computed directly here (rather than read off Figure 7, which
# carries leaching only) because a yield-SOC trade-off slope is a natural
# complement to the yield-N-leaching synthesis and is not otherwise reported
# anywhere in the pipeline.
#
# Uses TOTAL SOC (SOM1+SOM2+SOM3), matching the pool definition used for
# main-text Figure 6, rather than the SLOW pool (SOM2-C + SOM3-C) alone -
# keeping one pool definition across figures avoids a spurious inconsistency
# between figures that both claim to describe "SOC". Both slopes (total and
# slow-pool) are returned so the difference is measurable rather than assumed.

# ===========================================================================
# Three-domain trade-off - the companion to Figure 7
# ===========================================================================
# Figure 7 plots yield against N leaching; S12 plots it against SOC. Neither
# can answer the question Results 3.3 actually asks - whether ANY scenario
# improves yield, nitrogen retention AND carbon at once - because a point can
# only sit in one panel's quadrant at a time.
#
# This puts all three environmental outcomes on a common "% of open field"
# axis against the same yield axis, so a reader can follow one scenario across
# panels and see whether it stays in the favourable quadrant. Water is added
# as the third resource axis: shading lowers both yield and water use, so
# whether that is a gain or a loss depends on which falls faster.
#
# Sign convention: for every outcome, DOWN is environmentally better (less
# leaching, less water consumed) EXCEPT SOC, where up is better. That is
# unavoidable - carbon is a stock to build, the others are losses to avoid -
# so the favourable quadrant is annotated per panel rather than assumed.
prepare_three_domain_tradeoff <- function(tradeoff_data, soc_relative_change,
                                           field_water_sep,
                                           end_year = 2024L,
                                           rotations = paste("Rotation", 1:4),
                                           start_year = ANALYSIS_START_YEAR) {
  soc <- soc_relative_change |>
    dplyr::filter(year == end_year, depth == "0-30 cm", rotation %in% rotations) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::group_by(management_label, scen_label) |>
    dplyr::summarise(value = mean(delta_soc_pct, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(outcome = "Δ Total SOC, 0–30 cm")

  water_raw <- field_water_sep |>
    dplyr::mutate(year = suppressWarnings(as.integer(year))) |>
    dplyr::filter(year >= start_year, rotation %in% rotations)

  water_annual <- water_raw |>
    dplyr::mutate(aet = num_col(water_raw, "Actual_evapotranspiration_mm")) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::group_by(management_label, scen_label, weather_is_baseline) |>
    dplyr::summarise(aet = mean(aet, na.rm = TRUE), .groups = "drop")

  water_ref <- water_annual |>
    dplyr::filter(weather_is_baseline) |>
    dplyr::select(management_label, ref_aet = aet)

  water <- water_annual |>
    dplyr::inner_join(water_ref, by = "management_label") |>
    dplyr::filter(!weather_is_baseline, ref_aet > 0) |>
    dplyr::mutate(value = 100 * (aet - ref_aet) / ref_aet,
                  outcome = "Δ Actual evapotranspiration") |>
    dplyr::select(management_label, scen_label, value, outcome)

  nitrogen <- tradeoff_data |>
    dplyr::filter(driver != "Open field") |>
    dplyr::group_by(management_label) |>
    dplyr::mutate(ref_leach = leaching_kgN_ha[scen_label %in% c("Rad 0%")][1]) |>
    dplyr::ungroup() |>
    dplyr::select(management_label, scen_label, leaching_kgN_ha)

  # N leaching relative to the management-matched open field, taken from the
  # same reference table Figure 7 uses so the two figures cannot disagree.
  n_ref <- tradeoff_data |>
    build_tradeoff_reference_lines() |>
    dplyr::select(management_label, ref_leach = reference_leaching_kgN_ha)

  nitrogen <- nitrogen |>
    dplyr::inner_join(n_ref, by = "management_label") |>
    dplyr::filter(ref_leach > 0) |>
    dplyr::mutate(value = 100 * (leaching_kgN_ha - ref_leach) / ref_leach,
                  outcome = "Δ N leaching") |>
    dplyr::select(management_label, scen_label, value, outcome)

  yield <- tradeoff_data |>
    dplyr::filter(driver != "Open field") |>
    dplyr::select(management_label, scen_label, driver, relative_yield_pct) |>
    dplyr::distinct()

  dplyr::bind_rows(nitrogen, soc, water) |>
    dplyr::inner_join(yield, by = c("management_label", "scen_label")) |>
    dplyr::mutate(
      outcome = factor(outcome, levels = c("Δ N leaching", "Δ Total SOC, 0–30 cm",
                                            "Δ Actual evapotranspiration")),
      driver = droplevels(factor(driver, levels = c("Radiation", "Temperature", "Wind")))
    ) |>
    dplyr::filter(!is.na(outcome), !is.na(driver), !is.na(value))
}

# Which scenarios sit in the favourable quadrant on ALL THREE axes - yield at
# or above open field, leaching and water use at or below it, SOC at or above.
# Returns an empty table if none do, which is itself the result.
find_win_win_scenarios <- function(three_domain_data, yield_threshold = 100) {
  three_domain_data |>
    dplyr::mutate(favourable = dplyr::case_when(
      outcome == "Δ Total SOC, 0–30 cm" ~ value >= 0,
      TRUE ~ value <= 0
    )) |>
    dplyr::group_by(management_label, scen_label, driver, relative_yield_pct) |>
    dplyr::summarise(n_favourable = sum(favourable), n_outcomes = dplyr::n(), .groups = "drop") |>
    dplyr::filter(relative_yield_pct >= yield_threshold, n_favourable == n_outcomes)
}

prepare_fig_s12_data <- function(tradeoff_data, soc_relative_change,
                                  end_year = 2024L, depth_layer = "0-30 cm",
                                  rotations = paste("Rotation", 1:4)) {
  soc_end <- soc_relative_change |>
    dplyr::filter(year == end_year, depth == depth_layer, rotation %in% rotations) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::group_by(management_label, scen_label) |>
    dplyr::summarise(
      delta_soc_total_pct = mean(delta_soc_pct, na.rm = TRUE),
      delta_soc_slow_pct = mean(delta_slow_pct, na.rm = TRUE),
      .groups = "drop"
    )

  tradeoff_data |>
    dplyr::select(management_label, scen_label, driver, relative_yield_pct, yield_metric) |>
    dplyr::inner_join(soc_end, by = c("management_label", "scen_label")) |>
    # Wind is excluded per the draft's own description ("wind scenarios
    # excluded") - its yield and SOC responses are both within noise, so the
    # points cluster on the origin and add no slope information.
    dplyr::filter(driver %in% c("Radiation", "Temperature")) |>
    dplyr::mutate(driver = droplevels(driver))
}

# OLS slope of %dSOC on %yield, fitted separately per management x driver.
# Returned as a table rather than computed inside the plot so the numbers can
# be quoted in the text and checked independently.
fit_yield_soc_slopes <- function(fig_s12_data, soc_col = "delta_soc_total_pct") {
  fig_s12_data |>
    dplyr::filter(!is.na(.data[[soc_col]]), !is.na(relative_yield_pct)) |>
    dplyr::group_by(yield_metric, management_label, driver) |>
    dplyr::filter(dplyr::n() >= 3) |>
    dplyr::summarise(
      slope = stats::coef(stats::lm(.data[[soc_col]] ~ relative_yield_pct))[[2]],
      r_squared = summary(stats::lm(.data[[soc_col]] ~ relative_yield_pct))$r.squared,
      n_points = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(slope = round(slope, 3), r_squared = round(r_squared, 3))
}

# Additional supplementary material proposed while reviewing the v12/v9 draft.
#
# Each of these answers a question the manuscript raises but does not currently
# show, or supplies a reproducibility artefact a reader would need:
#
#   S13  Simulation inventory. The Methods state 416 simulations but no list
#        exists. A canonical index with stable IDs makes every figure traceable
#        to the runs behind it, and is the FAIR-critical artefact for the
#        Zenodo release.
#   S14  Full annual water balance. Reviewers asked directly whether
#        "percolation" and "drainage" are distinct model outputs or used
#        interchangeably. They are distinct, and showing all components
#        together settles it.
#   S15  Substrip (West/Centre/East) effects. The text asserts that substrip
#        position has little effect and a reviewer asked for the substrip
#        columns to be shown or removed. This shows them once, across all
#        outcomes, so the assertion is evidenced rather than repeated.

# ---------------------------------------------------------------------------
# S1 - open-field weather forcing and the simulated crop calendar
# ---------------------------------------------------------------------------
# Documents what actually drove the simulations, over one complete traversal of
# the rotation. The crop calendar is detected from daily biomass presence
# rather than assumed from sowing dates, for the same reason as S10: two crops
# can occupy one calendar year, and undersown ryegrass persists across the
# boundary.
#
# Management events (cultivation, sowing, fertilisation) are NOT shown. They
# live in the DAISY management setup (CropRotation_for_Spawn.dai) and leave no
# simulated growth signal to detect, so plotting them would mean parsing the
# .dai management schedule - deliberately out of scope here rather than
# approximated from biomass.
prepare_fig_s01_weather <- function(openfield_weather_daily, window_years = 2021:2024) {
  openfield_weather_daily |>
    dplyr::filter(year %in% window_years) |>
    dplyr::arrange(date) |>
    dplyr::mutate(cumulative_precip_mm = cumsum(tidyr::replace_na(precipitation_mm, 0)))
}

prepare_fig_s01_calendar <- function(crop_calendar, window_years = 2021:2024) {
  crop_calendar |>
    dplyr::filter(year %in% window_years)
}

# ---------------------------------------------------------------------------
# S5 - grass-clover calibration
# ---------------------------------------------------------------------------
# The GC mixture is calibrated differently from the other crops because DAISY
# represents it as two interacting components (ryegrass + white clover) rather
# than one mixed crop, so total-AGB calibration alone is equifinal - the same
# annual yield can arise from different splits between the species.
#
# Reads the artefacts produced by the separate GC_AGB_SA_AND_OPT framework;
# those runs are NOT reproduced by this pipeline, they are read as fixed
# inputs the same way the raw simulation output is.
read_gc_calibration <- function(calibration_dir) {
  read_one <- function(name) {
    p <- file.path(calibration_dir, name)
    if (!file.exists(p)) {
      cli::cli_alert_warning("GC calibration artefact not found: {p}")
      return(NULL)
    }
    readRDS(p)
  }
  list(
    lhs = read_one("lhs_results_klimagrass.rds"),
    baseline_cuts = read_one("baseline_gc_cuts_klimagrass.rds"),
    final = read_one("final_calibration_klimagrass.rds")
  )
}

# Field target: Foulum 2024 second-year sward, 14.96 t DM/ha in four cuts.
GC_FIELD_TARGET_T_DM_HA <- 14.96

prepare_fig_s05_data <- function(gc_calibration) {
  lhs <- gc_calibration$lhs
  if (is.null(lhs)) return(NULL)

  # The LHS table's column naming is not guaranteed stable across framework
  # versions, so resolve the AGB column by pattern rather than assuming a name.
  agb_col <- grep("agb|AGB|total_dm|yield", names(lhs), value = TRUE)[1]
  if (is.na(agb_col)) return(NULL)

  tibble::as_tibble(lhs) |>
    dplyr::mutate(
      annual_agb_t_dm_ha = suppressWarnings(as.numeric(.data[[agb_col]])),
      deviation_pct = 100 * (annual_agb_t_dm_ha - GC_FIELD_TARGET_T_DM_HA) / GC_FIELD_TARGET_T_DM_HA
    ) |>
    dplyr::filter(!is.na(annual_agb_t_dm_ha))
}

# ---------------------------------------------------------------------------
# S4 - soybean parameterisation diagnostic (single version)
# ---------------------------------------------------------------------------
# Characterises the FINAL soybean parameterisation (soypea_opt_v3), which is
# the only one present in the main NWAPS run. The draft's S4 compares it
# against the first-round version; that comparison needs the separate
# SY_SINGLE_RUN framework and is not reproduced here.
#
# What this does show is whether the final parameterisation behaves plausibly:
# a physically sensible temperature response, a harvest index in the range
# expected for a grain legume, and stress days that respond to the drivers
# rather than sitting at zero or saturating.
prepare_fig_s04_data <- function(harvest_annual, crop = "Soybean",
                                  rotations = paste("Rotation", 1:4),
                                  fert = "Biogas digestate", residue_policy = "Residue removed") {
  hc <- prepare_harvest_center(harvest_annual, crop_filter = crop) |>
    dplyr::filter(rotation %in% rotations,
                  fertiliser_type == fert, residue_policy == !!residue_policy)

  per_rotation <- hc |>
    dplyr::group_by(scen_label, rotation, year) |>
    dplyr::summarise(
      grain = sum(grain_MgDM_ha, na.rm = TRUE),
      agb = sum(total_agb_MgDM_ha, na.rm = TRUE),
      harvest_index = mean(harvest_index, na.rm = TRUE),
      water_stress_days = mean(water_stress_days, na.rm = TRUE),
      n_stress_days = mean(n_stress_days, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(scen_label, rotation) |>
    dplyr::summarise(dplyr::across(c(grain, agb, harvest_index,
                                      water_stress_days, n_stress_days),
                                    \(x) mean(x, na.rm = TRUE)), .groups = "drop")

  per_rotation |>
    dplyr::group_by(scen_label) |>
    dplyr::summarise(dplyr::across(c(grain, agb, harvest_index,
                                      water_stress_days, n_stress_days),
                                    list(mean = \(x) mean(x, na.rm = TRUE),
                                         sd = \(x) sd(x, na.rm = TRUE))),
                     .groups = "drop") |>
    dplyr::mutate(
      driver = driver_of_scen_label(scen_label),
      scen_label = factor(scen_label, levels = scen_order_center)
    ) |>
    add_reference_to_each_driver(c("Radiation", "Temperature", "Wind")) |>
    dplyr::mutate(driver = factor(driver, levels = c("Radiation", "Temperature", "Wind"))) |>
    dplyr::filter(!is.na(driver), !is.na(scen_label))
}

# Season length from the daily record: first to last day carrying biomass.
prepare_fig_s04_season <- function(daily_crop_production_wind, crop = "Soybean") {
  daily_crop_production_wind |>
    dplyr::filter(crop_source == crop) |>
    dplyr::mutate(agb = WLeaf_Mg_DM_ha + WStem_Mg_DM_ha + WDead_Mg_DM_ha + WSOrg_Mg_DM_ha,
                  date = as.Date(sprintf("%04d-%02d-%02d", year, month, mday)),
                  day_of_year = as.integer(format(date, "%j"))) |>
    dplyr::filter(agb > 0.05) |>
    add_weather_scenario_label() |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::group_by(scen_label, year) |>
    dplyr::summarise(season_length_days = dplyr::n(),
                      emergence_doy = min(day_of_year),
                      end_doy = max(day_of_year), .groups = "drop") |>
    dplyr::mutate(
      driver = driver_of_scen_label(scen_label),
      scen_label = factor(scen_label, levels = scen_order_center)
    ) |>
    dplyr::filter(driver %in% c("Reference", "Temperature", "Radiation"), !is.na(scen_label))
}

# ---------------------------------------------------------------------------
# S16 - rotation yield composition
# ---------------------------------------------------------------------------
# Supports the opening sentence of Results, which states a system ASY and a
# grass-clover share but shows neither. Also makes visible WHICH crop each
# driver takes the yield from, which the aggregate ASY hides.
prepare_fig_s16_data <- function(harvest_annual, rotations = paste("Rotation", 1:4),
                                  fert = "Biogas digestate", residue_policy = "Residue removed") {
  prepare_harvest_center(harvest_annual) |>
    dplyr::filter(rotation %in% rotations,
                  fertiliser_type == fert, residue_policy == !!residue_policy) |>
    dplyr::group_by(scen_label, crop_renamed, rotation, year) |>
    dplyr::summarise(v = sum(harvested_agb_removed_MgDM_ha, na.rm = TRUE), .groups = "drop") |>
    # Mean across YEARS then across rotations, so each crop's contribution is
    # its share of the annual system total rather than of its own crop-years.
    dplyr::group_by(scen_label, crop_renamed, rotation) |>
    dplyr::summarise(v = sum(v, na.rm = TRUE) / dplyr::n_distinct(year), .groups = "drop") |>
    dplyr::group_by(scen_label, crop_renamed) |>
    dplyr::summarise(mean_contribution = mean(v, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(scen_label) |>
    dplyr::mutate(share_pct = 100 * mean_contribution / sum(mean_contribution)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      driver = driver_of_scen_label(scen_label),
      scen_label = factor(scen_label, levels = scen_order_center),
      crop_renamed = factor(crop_renamed, levels = crop_levels_all)
    ) |>
    add_reference_to_each_driver(c("Radiation", "Temperature", "Wind")) |>
    dplyr::mutate(driver = factor(driver, levels = c("Radiation", "Temperature", "Wind"))) |>
    dplyr::filter(!is.na(driver), !is.na(scen_label), !is.na(crop_renamed))
}

# ---------------------------------------------------------------------------
# S17 - complete nitrogen budget
# ---------------------------------------------------------------------------
# The manuscript discusses individual N fluxes but never the budget they sit
# in. Showing inputs against outputs makes the surplus explicit and lets a
# reader see whether leaching is supply-driven or demand-driven under each
# scenario - the mechanism Results 3.2 argues for verbally.
prepare_fig_s17_data <- function(n_annual, rotations = paste("Rotation", 1:4)) {
  n_annual |>
    dplyr::filter(rotation %in% rotations) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::mutate(
      `Mineral fertiliser` = fertiliser_mineral_kgN_ha,
      `Organic fertiliser` = fertiliser_organic_kgN_ha,
      `Biological fixation` = fixation_kgN_ha,
      `Net mineralisation` = mineralisation_kgN_ha - immobilisation_kgN_ha,
      `Crop uptake` = -crop_uptake_kgN_ha,
      `Leaching` = -leaching_kgN_ha
    ) |>
    tidyr::pivot_longer(
      c(`Mineral fertiliser`, `Organic fertiliser`, `Biological fixation`,
        `Net mineralisation`, `Crop uptake`, `Leaching`),
      names_to = "term", values_to = "kgN_ha"
    ) |>
    dplyr::group_by(management_label, scen_label, term) |>
    dplyr::summarise(mean_kgN_ha = mean(kgN_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      direction = dplyr::if_else(mean_kgN_ha >= 0, "Input", "Output"),
      term = factor(term, levels = c("Mineral fertiliser", "Organic fertiliser",
                                      "Biological fixation", "Net mineralisation",
                                      "Crop uptake", "Leaching")),
      driver = driver_of_scen_label(scen_label),
      scen_label = factor(scen_label, levels = scen_order_center)
    ) |>
    dplyr::filter(driver %in% c("Reference", "Radiation", "Temperature")) |>
    add_reference_to_each_driver(c("Radiation", "Temperature")) |>
    dplyr::mutate(driver = factor(driver, levels = c("Radiation", "Temperature"))) |>
    dplyr::filter(!is.na(driver), !is.na(scen_label))
}

# ---------------------------------------------------------------------------
# S18 - rotation-permutation spread against scenario effect size
# ---------------------------------------------------------------------------
# Evidences a Discussion claim that is currently asserted without a figure:
# that winter wheat AGB varies by up to ~22% across the four permutations from
# weather-year assignment alone, "comparable to one full radiation-scenario
# step". If true, it is the strongest argument in the paper for why a
# single fixed rotation would have been inadequate - and it is checkable.
prepare_fig_s18_data <- function(harvest_annual, rotations = paste("Rotation", 1:4),
                                  fert = "Biogas digestate", residue_policy = "Residue removed") {
  per_rotation <- prepare_harvest_center(harvest_annual) |>
    dplyr::filter(rotation %in% rotations,
                  fertiliser_type == fert, residue_policy == !!residue_policy) |>
    dplyr::group_by(crop_renamed, scen_label, rotation, year) |>
    dplyr::summarise(v = sum(harvested_agb_removed_MgDM_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(crop_renamed, scen_label, rotation) |>
    dplyr::summarise(rot_mean = mean(v, na.rm = TRUE), .groups = "drop")

  # Permutation spread: how much the same scenario moves depending only on
  # which weather years each crop lands on.
  permutation_spread <- per_rotation |>
    dplyr::group_by(crop_renamed, scen_label) |>
    dplyr::summarise(
      across_rotations_pct = 100 * (max(rot_mean, na.rm = TRUE) - min(rot_mean, na.rm = TRUE)) /
        mean(rot_mean, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(crop_renamed) |>
    dplyr::summarise(permutation_spread_pct = mean(across_rotations_pct, na.rm = TRUE),
                      .groups = "drop")

  # Scenario step: the yield change from one step down the radiation ladder.
  scenario_step <- per_rotation |>
    dplyr::group_by(crop_renamed, scen_label) |>
    dplyr::summarise(v = mean(rot_mean, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(scen_label %in% c("Rad 0%", "Rad -5%", "Rad -10%", "Rad -20%", "Rad -30%")) |>
    dplyr::group_by(crop_renamed) |>
    dplyr::arrange(match(scen_label, scen_order_center), .by_group = TRUE) |>
    dplyr::summarise(
      radiation_step_pct = mean(abs(diff(v)) / utils::head(v, -1) * 100, na.rm = TRUE),
      .groups = "drop"
    )

  permutation_spread |>
    dplyr::inner_join(scenario_step, by = "crop_renamed") |>
    tidyr::pivot_longer(c(permutation_spread_pct, radiation_step_pct),
                        names_to = "source", values_to = "pct") |>
    dplyr::mutate(
      source = dplyr::recode(source,
        permutation_spread_pct = "Rotation-permutation spread\n(weather-year assignment alone)",
        radiation_step_pct = "One radiation-scenario step\n(mean across the ladder)"),
      crop_renamed = factor(crop_renamed, levels = crop_levels_all)
    ) |>
    dplyr::filter(!is.na(crop_renamed))
}

# ---------------------------------------------------------------------------
# S13 - canonical simulation index
# ---------------------------------------------------------------------------
# One row per simulation with a stable SIM#### identifier. Sorted by
# management then scenario so the ID ordering is deterministic and stable
# across rebuilds - an ID that changes when the pipeline is re-run is worse
# than no ID at all.
build_simulation_index <- function(harvest_annual) {
  harvest_annual |>
    dplyr::distinct(run_id, soil, rotation, rotation_code, fertilisation, residue,
                    weather_scenario, weather_factor, weather_direction,
                    weather_change_pct, weather_signed_change_pct, strip_position,
                    weather_is_baseline) |>
    dplyr::arrange(rotation, fertilisation, residue, weather_factor,
                    weather_signed_change_pct, strip_position) |>
    dplyr::mutate(
      simulation_id = sprintf("SIM%04d", dplyr::row_number()),
      scenario_unit = dplyr::case_when(
        weather_factor == "Temperature" ~ "degC",
        weather_factor %in% c("Radiation", "Wind") ~ "percent",
        TRUE ~ NA_character_
      ),
      reference_type = dplyr::case_when(
        weather_is_baseline ~ "Open-field baseline",
        weather_signed_change_pct == 0 ~ "VAPV 0-level (modelled microclimate)",
        TRUE ~ "Perturbation from VAPV 0-level"
      ),
      .before = 1
    )
}

summarise_simulation_index <- function(simulation_index) {
  simulation_index |>
    dplyr::count(weather_factor, reference_type, name = "n_simulations") |>
    dplyr::arrange(weather_factor, reference_type)
}

# ---------------------------------------------------------------------------
# S14 - annual water balance components
# ---------------------------------------------------------------------------
# DAISY reports percolation and drainage as SEPARATE outputs and they are not
# interchangeable:
#   percolation - vertical flux past the bottom of the modelled soil column
#   drainage    - lateral flux intercepted by field drains
# Both are further split into matrix and biopore (macropore) pathways. Water
# leaving via drains does not percolate, so summing them would double-count.
prepare_fig_s14_data <- function(field_water_sep, start_year = ANALYSIS_START_YEAR,
                                  rotations = paste("Rotation", 1:4),
                                  fert = "Biogas digestate", residue_policy = "Residue Retained") {
  df <- field_water_sep |>
    dplyr::mutate(year = suppressWarnings(as.integer(year))) |>
    dplyr::filter(year >= start_year, rotation %in% rotations,
                  fertilisation == fert, residue == residue_policy)

  df |>
    dplyr::mutate(
      precipitation = num_col(df, "Precipitation_mm"),
      pet = num_col(df, "Potential_evapotranspiration_mm"),
      aet = num_col(df, "Actual_evapotranspiration_mm"),
      percolation = num_col(df, "Matrix_percolation_mm") + num_col(df, "Biopore_percolation_mm"),
      drainage = num_col(df, "Matrix_drain_flow_mm") + num_col(df, "Biopore_drain_flow_mm"),
      runoff = num_col(df, "Runoff_mm")
    ) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label)) |>
    tidyr::pivot_longer(c(precipitation, pet, aet, percolation, drainage, runoff),
                        names_to = "component", values_to = "mm") |>
    dplyr::mutate(component = factor(dplyr::recode(component,
      precipitation = "Precipitation", pet = "Potential ET", aet = "Actual ET",
      percolation = "Percolation (below profile)", drainage = "Drain flow (lateral)",
      runoff = "Surface runoff"),
      levels = c("Precipitation", "Potential ET", "Actual ET",
                  "Percolation (below profile)", "Drain flow (lateral)", "Surface runoff"))) |>
    dplyr::group_by(component, scen_label) |>
    dplyr::summarise(
      mean_mm = mean(mm, na.rm = TRUE),
      sd_mm = sd(mm, na.rm = TRUE),
      n_rotation_years = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      driver = driver_of_scen_label(scen_label),
      scen_label = factor(scen_label, levels = scen_order_center)
    ) |>
    add_reference_to_each_driver(c("Radiation", "Temperature", "Wind")) |>
    dplyr::mutate(driver = factor(driver, levels = c("Radiation", "Temperature", "Wind"))) |>
    dplyr::filter(!is.na(driver), !is.na(scen_label))
}

# ---------------------------------------------------------------------------
# S15 - substrip position effects
# ---------------------------------------------------------------------------
# Compares West / Centre / East at each driver's VAPV 0-level, against the
# open field, across productivity, nitrogen and carbon. If the three substrips
# are indistinguishable the figure says so in one place, which is what lets the
# main-text figures legitimately show the Centre strip alone.
prepare_fig_s15_data <- function(harvest_annual, n_annual, soc_relative_change,
                                  rotations = paste("Rotation", 1:4),
                                  fert = "Biogas digestate", residue_policy = "Residue Removed") {
  zero_levels <- c("Rad 0% W", "Rad 0% C", "Rad 0% E",
                   "Wind 0% W", "Wind 0% C", "Wind 0% E",
                   "Tmp 0degC W", "Tmp 0degC C", "Tmp 0degC E")

  yield <- harvest_annual |>
    dplyr::filter(rotation %in% rotations, fertilisation == fert, residue == residue_policy,
                  year >= ANALYSIS_START_YEAR) |>
    add_scenario_labels(grid = "ecw") |>
    dplyr::filter(scen_label %in% c("Reference", zero_levels)) |>
    dplyr::group_by(scen_label, rotation, year) |>
    dplyr::summarise(v = sum(harvested_agb_removed_MgDM_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(scen_label, rotation) |>
    dplyr::summarise(v = mean(v, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(outcome = "Annualised system yield (t DM ha⁻¹ yr⁻¹)")

  nitrogen <- n_annual |>
    dplyr::filter(rotation %in% rotations, fertilisation == fert, residue == residue_policy) |>
    add_scenario_labels(grid = "ecw") |>
    dplyr::filter(scen_label %in% c("Reference", zero_levels)) |>
    dplyr::group_by(scen_label, rotation) |>
    dplyr::summarise(v = mean(leaching_kgN_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(outcome = "N leaching (kg N ha⁻¹ yr⁻¹)")

  carbon <- soc_relative_change |>
    dplyr::filter(rotation %in% rotations, fertilisation == fert, residue == residue_policy,
                  depth == "0-30 cm", year == 2024L) |>
    add_scenario_labels(grid = "ecw") |>
    dplyr::filter(scen_label %in% zero_levels) |>
    dplyr::group_by(scen_label, rotation) |>
    dplyr::summarise(v = mean(delta_soc_pct, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(outcome = "ΔTotal SOC, 0–30 cm (%)")

  dplyr::bind_rows(yield, nitrogen, carbon) |>
    dplyr::group_by(outcome, scen_label) |>
    dplyr::summarise(mean_v = mean(v, na.rm = TRUE), sd_v = sd(v, na.rm = TRUE),
                     n_rotations = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(
      strip = dplyr::case_when(
        scen_label == "Reference" ~ "Open field",
        stringr::str_ends(scen_label, " W") ~ "West",
        stringr::str_ends(scen_label, " C") ~ "Centre",
        stringr::str_ends(scen_label, " E") ~ "East",
        TRUE ~ NA_character_
      ),
      driver = dplyr::case_when(
        scen_label == "Reference" ~ "Open field",
        stringr::str_starts(scen_label, "Rad") ~ "Radiation",
        stringr::str_starts(scen_label, "Wind") ~ "Wind",
        stringr::str_starts(scen_label, "Tmp") ~ "Temperature",
        TRUE ~ NA_character_
      ),
      strip = factor(strip, levels = c("Open field", "West", "Centre", "East")),
      driver = factor(driver, levels = c("Open field", "Radiation", "Temperature", "Wind")),
      outcome = factor(outcome, levels = c(
        "Annualised system yield (t DM ha⁻¹ yr⁻¹)",
        "N leaching (kg N ha⁻¹ yr⁻¹)",
        "ΔTotal SOC, 0–30 cm (%)"))
    ) |>
    dplyr::filter(!is.na(strip), !is.na(driver))
}

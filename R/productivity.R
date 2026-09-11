# Harvest/yield outcomes: species pooling, annualised system yield (ASY),
# and the 4-rotation-averaged scenario-response datasets behind Figures 4-5.
#
# Extracted from reference/legacy_snapshot/SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd:
#   - Grass-Clover pooling and harvest_annual_15: chunk 15B
#   - Figure 4 data (total AGB relative to open field): chunk `fig04_results_heatmap_agb`
#     (the "17K"-suffixed pipeline - confirmed by tracing which chunk actually
#     calls save_ms_fig("Figure_04_manuscript_heatmap_ASY_AGB_per_crop.png", ...);
#     a separate, never-fully-reconciled "18_1"-suffixed pipeline earlier in the
#     script computes something structurally similar but does NOT feed Figure 4)
#   - Figure 5 data (relative grain yield, W/C/E strip positions): chunk
#     `s18_1_6_data_4rot` (prep_harvest_ecw_18 / build_4rot_rel_ecw_18)

ANALYSIS_START_YEAR <- 1998L

split_management <- function(df) {
  df |> dplyr::mutate(
    fertiliser_type = dplyr::case_when(
      stringr::str_detect(as.character(management_label), "[Mm]ineral") ~ "Mineral fertiliser",
      stringr::str_detect(as.character(management_label), "[Bb]iogas") ~ "Biogas digestate",
      TRUE ~ as.character(management_label)
    ),
    residue_policy = dplyr::case_when(
      stringr::str_detect(as.character(management_label), "[Rr]etain") ~ "Residue retained",
      stringr::str_detect(as.character(management_label), "[Rr]emov") ~ "Residue removed",
      TRUE ~ as.character(management_label)
    ),
    fertiliser_type = factor(fertiliser_type, levels = c("Mineral fertiliser", "Biogas digestate")),
    residue_policy = factor(residue_policy, levels = c("Residue removed", "Residue retained"))
  )
}

crop_levels_all <- c("Winter Wheat", "Spring Barley", "Soybean", "Ryegrass (undersown)", "Grass-Clover")
crop_levels_grain <- c("Winter Wheat", "Soybean")

# Scenario grid actually simulated (confirmed from the legacy script's own
# scenario palette definition, not re-derived): Radiation and Wind in percent,
# Temperature in degC, each including a zero/no-perturbation level at three
# strip positions (West/Center/East) plus non-zero perturbation levels at
# Center only.
rad_levels <- c(0, -5, -10, -20, -30)
wind_levels <- c(0, -20, -30, -40, -50, -60, -70)
tmp_levels <- c(-3, -2, 0, 0.5, 1, 2, 3)

scen_order_ecw <- c(
  "Reference",
  "Rad 0% W", "Rad 0% C", "Rad 0% E",
  paste0("Rad ", rad_levels[rad_levels != 0], "% C"),
  "Wind 0% W", "Wind 0% C", "Wind 0% E",
  paste0("Wind ", wind_levels[wind_levels != 0], "% C"),
  "Tmp 0degC W", "Tmp 0degC C", "Tmp 0degC E",
  paste0("Tmp ", tmp_levels[tmp_levels < 0], "degC C"),
  paste0("Tmp +", tmp_levels[tmp_levels > 0], "degC C")
)

# ---------------------------------------------------------------------------
# Harvest annual: species pooling (Grass-Clover ley = 2 species summed to 1
# row per year x scenario), then per-scenario/annualised summaries.
# ---------------------------------------------------------------------------

prepare_harvest_annual <- function(harvest_prepped) {
  base <- harvest_prepped |>
    dplyr::mutate(
      year = suppressWarnings(as.integer(year)),
      crop_renamed = as.character(crop_renamed),
      grain_MgDM_ha = suppressWarnings(as.numeric(grain_MgDM_ha)),
      residue_MgDM_ha = suppressWarnings(as.numeric(residue_MgDM_ha)),
      residue_removed_MgDM_ha = suppressWarnings(as.numeric(residue_removed_MgDM_ha)),
      harvested_agb_removed_MgDM_ha = suppressWarnings(as.numeric(harvested_agb_removed_MgDM_ha))
    ) |>
    make_scenario_label()

  gc_id_cols <- unique(c(
    scenario_id_cols, "year", "crop_renamed",
    "weather_is_baseline", "weather_label", "management_label", "scenario_label"
  ))

  base |>
    dplyr::group_by(dplyr::across(dplyr::any_of(gc_id_cols))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::any_of(c("grain_MgDM_ha", "residue_MgDM_ha", "residue_removed_MgDM_ha",
                         "harvested_agb_removed_MgDM_ha", "total_agb_MgDM_ha")),
        \(x) sum(x, na.rm = TRUE)
      ),
      # Stress days, harvest index and water productivity are intensive
      # quantities: summing them across the two pooled grass-clover species
      # would be meaningless, so they are averaged.
      dplyr::across(
        dplyr::any_of(c("water_stress_days", "n_stress_days",
                         "harvest_index", "water_productivity_kg_m3")),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Centre-strip scenario grid (manuscript Figure 2).
#
# Unlike prepare_harvest_ecw() below - which keeps West/Centre/East at each
# driver's 0-level - this keeps the open-field baseline plus Centre-strip
# scenarios only, which is what Figure 2's "Ref = open field; Rad 0%/Tmp
# 0degC/Wind 0% = VAPV centre-strip reference level" axis means.
#
# Ported from reference/legacy_snapshot/SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd,
# chunk `s18_1_6_data_4rot` (prep_harvest_18 / build_4rot_asy_18).
# ---------------------------------------------------------------------------

# Scenario levels actually simulated, Centre strip only (see Methods Table 2:
# 7 radiation + 9 temperature + 9 wind scenarios, of which the non-zero levels
# are Centre-only). Order here drives the x-axis order in Figure 2.
scen_order_center <- c(
  "Reference",
  paste0("Rad ", rad_levels, "%"),
  paste0("Wind ", wind_levels, "%"),
  paste0("Tmp ", ifelse(tmp_levels > 0, "+", ""), tmp_levels, "degC")
)

prepare_harvest_center <- function(harvest_annual, crop_filter = NULL) {
  df <- harvest_annual |>
    dplyr::filter(year >= ANALYSIS_START_YEAR) |>
    dplyr::filter(weather_is_baseline | strip_position == "Center") |>
    split_management() |>
    dplyr::mutate(
      crop_renamed = dplyr::case_when(
        crop_renamed %in% c("Ryegrass (GC)", "White Clover (GC)") ~ "Grass-Clover",
        TRUE ~ as.character(crop_renamed)
      ),
      signed_level = dplyr::case_when(
        stringr::str_detect(as.character(weather_scenario), "0p5up") ~ 0.5,
        stringr::str_detect(as.character(weather_scenario), "0p5dwn") ~ -0.5,
        TRUE ~ suppressWarnings(as.numeric(weather_signed_change_pct))
      ),
      wf = dplyr::case_when(weather_is_baseline ~ "Reference", TRUE ~ as.character(weather_factor)),
      scen_label = dplyr::case_when(
        weather_is_baseline ~ "Reference",
        wf == "Temperature" & signed_level > 0 ~ paste0("Tmp +", signed_level, "degC"),
        wf == "Temperature" & signed_level < 0 ~ paste0("Tmp ", signed_level, "degC"),
        wf == "Temperature" ~ "Tmp 0degC",
        wf == "Radiation" & signed_level > 0 ~ paste0("Rad +", signed_level, "%"),
        wf == "Radiation" ~ paste0("Rad ", signed_level, "%"),
        wf == "Wind" & signed_level > 0 ~ paste0("Wind +", signed_level, "%"),
        wf == "Wind" ~ paste0("Wind ", signed_level, "%"),
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(weather_is_baseline | !is.na(signed_level)) |>
    dplyr::filter(scen_label %in% scen_order_center)
  if (!is.null(crop_filter)) df <- dplyr::filter(df, crop_renamed %in% crop_filter)
  df
}

# Driver a scenario label belongs to, for facetting Figure 2 by driver.
driver_of_scen_label <- function(scen_label) {
  x <- as.character(scen_label)
  dplyr::case_when(
    x == "Reference" ~ "Reference",
    stringr::str_starts(x, "Rad") ~ "Radiation",
    stringr::str_starts(x, "Wind") ~ "Wind",
    stringr::str_starts(x, "Tmp") ~ "Temperature",
    TRUE ~ NA_character_
  )
}

# Three-step aggregation, in this order and no other:
#   1. sum harvest events within crop x rotation x management x scenario x year
#      (a year can hold several cuts, especially grass-clover)
#   2. mean across years within each rotation -> one ASY value per rotation
#   3. mean +/- SD across the four rotation permutations
# SD (not SE) is reported: the four rotations are a fixed phase-shift design,
# not a random sample, so SD honestly describes rotation-to-rotation spread.
build_4rot_asy <- function(harvest_center, response_col) {
  harvest_center |>
    dplyr::group_by(crop_renamed, fertiliser_type, residue_policy, scen_label, rotation, year) |>
    dplyr::summarise(annual_resp = sum(.data[[response_col]], na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(crop_renamed, fertiliser_type, residue_policy, scen_label, rotation) |>
    dplyr::summarise(rot_asy = mean(annual_resp, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(crop_renamed, fertiliser_type, residue_policy, scen_label) |>
    dplyr::summarise(
      mean_resp = mean(rot_asy, na.rm = TRUE),
      sd_resp = sd(rot_asy, na.rm = TRUE),
      n_rot = dplyr::n(),
      .groups = "drop"
    )
}

# Relative response (% change), computed per rotation against that rotation's
# own reference before averaging - unbiased with respect to rotation-specific
# baseline levels.
#
# `reference` picks what the percentage is *of*:
#   "openfield"  - the open-field baseline (weather_is_baseline)
#   "vapv_zero"  - that driver's VAPV 0-level (Rad 0% / Tmp 0degC / Wind 0%)
# These are NOT interchangeable and give materially different numbers; the
# manuscript Results text for Figure 2b states "% of VAPV 0-level", so that is
# the default here.
build_4rot_relative <- function(harvest_center, response_col,
                                 reference = c("vapv_zero", "openfield")) {
  reference <- match.arg(reference)

  step2 <- harvest_center |>
    dplyr::group_by(crop_renamed, fertiliser_type, residue_policy, scen_label,
                    weather_is_baseline, rotation, year) |>
    dplyr::summarise(annual_resp = sum(.data[[response_col]], na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(crop_renamed, fertiliser_type, residue_policy, scen_label,
                    weather_is_baseline, rotation) |>
    dplyr::summarise(rot_val = mean(annual_resp, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(driver = driver_of_scen_label(scen_label))

  if (reference == "openfield") {
    ref_rot <- step2 |>
      dplyr::filter(weather_is_baseline) |>
      dplyr::select(crop_renamed, fertiliser_type, residue_policy, rotation, ref_val = rot_val)
    joined <- step2 |>
      dplyr::filter(!weather_is_baseline) |>
      dplyr::left_join(ref_rot, by = c("crop_renamed", "fertiliser_type", "residue_policy", "rotation"))
  } else {
    # Each driver's own 0-level is the reference for that driver's scenarios.
    ref_rot <- step2 |>
      dplyr::filter(scen_label %in% c("Rad 0%", "Wind 0%", "Tmp 0degC")) |>
      dplyr::mutate(driver = driver_of_scen_label(scen_label)) |>
      dplyr::select(crop_renamed, fertiliser_type, residue_policy, rotation, driver, ref_val = rot_val)
    joined <- step2 |>
      dplyr::filter(!weather_is_baseline) |>
      dplyr::left_join(ref_rot, by = c("crop_renamed", "fertiliser_type", "residue_policy", "rotation", "driver"))
  }

  joined |>
    dplyr::filter(!is.na(ref_val), ref_val != 0) |>
    dplyr::mutate(rot_pct_resp = 100 * (rot_val - ref_val) / ref_val) |>
    dplyr::group_by(crop_renamed, fertiliser_type, residue_policy, scen_label) |>
    dplyr::summarise(
      mean_resp = mean(rot_pct_resp, na.rm = TRUE),
      sd_resp = sd(rot_pct_resp, na.rm = TRUE),
      n_rot = dplyr::n(),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Annualised system yield (ASY)
# ---------------------------------------------------------------------------
# The headline whole-system productivity number. Definition, in this exact
# order (the order matters - averaging in a different sequence weights
# rotations by their number of harvest events rather than equally):
#
#   1. Within each scenario x rotation x YEAR, sum harvested AGB across every
#      crop harvested that year. A single year can contain several harvest
#      events - notably the multiple grass-clover cuts, and the spring
#      barley + undersown ryegrass pair - and all of them count toward that
#      year's system yield.
#   2. Within each scenario x rotation, take the mean across the evaluation
#      years -> one ASY value per rotation permutation.
#   3. Across the four rotation permutations, take the mean and SD.
#
# Assumptions, all deliberate and all consequential:
#   - Evaluation period is 1998-2024 (ANALYSIS_START_YEAR onward). The
#     1988-1997 warm-up is excluded because Methods designates it as warm-up
#     for bringing soil C and N pools toward dynamic equilibrium; including
#     it lowers system ASY by ~6% (10.78 vs 11.48 t DM/ha/yr for the
#     open-field Dig-Rem case) because those years run a partial rotation
#     dominated by spring barley.
#   - "Harvested AGB removed" = grain + residue actually exported, i.e.
#     residue counts only under Residue Removed management. This is the
#     quantity leaving the field, not standing biomass.
#   - Grass-clover is the pooled ley (ryegrass + white clover summed), and
#     all five crops including undersown ryegrass contribute.
#   - Variability is the SD across the four rotation permutations. The four
#     permutations are a fixed phase-shift design, not a random sample of a
#     population, so SD describes the actual rotation-to-rotation spread;
#     an SE would imply a sampling model that does not apply to a
#     deterministic simulation ensemble.
#
# Reported open-field Dig-Rem value under this definition: 11.48 t DM/ha/yr.
# ---------------------------------------------------------------------------

calculate_system_asy <- function(harvest_annual,
                                  response_col = "harvested_agb_removed_MgDM_ha",
                                  crops = NULL,
                                  rotations = paste("Rotation", 1:4),
                                  start_year = ANALYSIS_START_YEAR) {
  harvest_annual |>
    dplyr::filter(year >= start_year, rotation %in% rotations) |>
    dplyr::filter(weather_is_baseline | strip_position == "Center") |>
    (\(d) if (is.null(crops)) d else dplyr::filter(d, crop_renamed %in% crops))() |>
    dplyr::group_by(fertilisation, residue, weather_scenario, weather_factor,
                    weather_signed_change_pct, strip_position, weather_is_baseline,
                    rotation, year) |>
    dplyr::summarise(annual_system_total = sum(.data[[response_col]], na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::group_by(fertilisation, residue, weather_scenario, weather_factor,
                    weather_signed_change_pct, strip_position, weather_is_baseline,
                    rotation) |>
    dplyr::summarise(rotation_asy = mean(annual_system_total, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(fertilisation, residue, weather_scenario, weather_factor,
                    weather_signed_change_pct, strip_position, weather_is_baseline) |>
    dplyr::summarise(
      system_asy_MgDM_ha_yr = mean(rotation_asy, na.rm = TRUE),
      sd_across_rotations = sd(rotation_asy, na.rm = TRUE),
      n_rotations = dplyr::n(),
      .groups = "drop"
    )
}

# --- Figure 2 -------------------------------------------------------------
# (a) absolute annualised harvested AGB per crop across all three drivers
# (b) grain-yield response (% of VAPV 0-level) for winter wheat + soybean

prepare_fig_02_a_data <- function(harvest_annual,
                                   management = list(fert = "Biogas digestate",
                                                     residue = "Residue removed")) {
  base <- prepare_harvest_center(harvest_annual) |>
    build_4rot_asy("harvested_agb_removed_MgDM_ha") |>
    dplyr::filter(fertiliser_type == management$fert, residue_policy == management$residue) |>
    dplyr::mutate(driver = driver_of_scen_label(scen_label))

  # The open-field reference belongs to no driver gradient, but each driver
  # facet needs it as its leftmost anchor so the reader can see open field vs
  # VAPV 0-level vs perturbation in one glance (reviewers asked for exactly
  # this distinction to be unambiguous). Duplicate it into all three facets.
  drivers <- c("Radiation", "Temperature", "Wind")
  reference_rows <- base |>
    dplyr::filter(driver == "Reference") |>
    dplyr::select(-driver) |>
    tidyr::crossing(driver = drivers)

  dplyr::bind_rows(base |> dplyr::filter(driver %in% drivers), reference_rows) |>
    dplyr::mutate(
      driver = factor(driver, levels = drivers),
      scen_label = factor(scen_label, levels = scen_order_center),
      crop_renamed = factor(crop_renamed, levels = crop_levels_all),
      is_reference = scen_label == "Reference",
      is_vapv_zero = scen_label %in% c("Rad 0%", "Wind 0%", "Tmp 0degC")
    ) |>
    dplyr::filter(!is.na(crop_renamed), !is.na(scen_label), !is.na(driver)) |>
    dplyr::arrange(driver, scen_label, crop_renamed)
}

prepare_fig_02_b_data <- function(harvest_annual, fert = "Biogas digestate") {
  prepare_harvest_center(harvest_annual, crop_filter = crop_levels_grain) |>
    build_4rot_relative("grain_MgDM_ha", reference = "vapv_zero") |>
    dplyr::filter(fertiliser_type == fert) |>
    dplyr::mutate(
      driver = factor(driver_of_scen_label(scen_label),
                      levels = c("Radiation", "Temperature", "Wind")),
      scen_label = factor(scen_label, levels = scen_order_center),
      crop_renamed = factor(crop_renamed, levels = crop_levels_grain),
      is_vapv_zero = scen_label %in% c("Rad 0%", "Wind 0%", "Tmp 0degC")
    ) |>
    dplyr::filter(!is.na(crop_renamed), !is.na(scen_label), !is.na(driver)) |>
    dplyr::arrange(driver, scen_label, crop_renamed)
}

# --- Figure 3 -------------------------------------------------------------
# Seasonal biomass and grain accumulation, one panel per crop, contrasting a
# dry and a near-normal year under open field vs VAPV (Rad 0%).
#
# Unlike the scenario-level figures, the x-axis here IS a continuum (day of
# year within a single growing season), so connecting lines are appropriate:
# they trace one simulation through time rather than interpolating between
# independent runs.

weather_display_labels <- c(
  "weatherBaselineopen" = "Open field",
  "weatherRad0Center" = "VAPV (Rad 0%)"
)

prepare_fig_03_data <- function(daily_crop_production, contrast_years) {
  # Pool the two grass-clover species to a single ley, matching the annual
  # harvest handling in harmonise_crop_names().
  pooled <- daily_crop_production |>
    dplyr::mutate(
      crop_renamed = dplyr::if_else(
        stringr::str_starts(crop_source, "Grass-Clover"), "Grass-Clover", crop_source
      )
    ) |>
    dplyr::group_by(crop_renamed, Weather, year, month, mday) |>
    dplyr::summarise(
      # AGB = leaf + stem + dead + storage organ, i.e. everything above ground.
      agb_MgDM_ha = sum(WLeaf_Mg_DM_ha + WStem_Mg_DM_ha + WDead_Mg_DM_ha + WSOrg_Mg_DM_ha, na.rm = TRUE),
      grain_MgDM_ha = sum(WSOrg_Mg_DM_ha, na.rm = TRUE),
      lai = sum(LAI_m2_m2, na.rm = TRUE),
      .groups = "drop"
    )

  # Truncate each series at its first harvest.
  #
  # Why: this figure is about biomass ACCUMULATION up to harvest in a dry vs a
  # near-normal season. Some runs regrow substantially afterwards - e.g. spring
  # barley 2001 under VAPV Rad 0%, where reduced radiation delayed development
  # enough that the crop was cut but not terminated (DS resets 0.64 -> 0.18 and
  # then climbs to 1.91, regrowing to 14 t DM/ha by year end, while the
  # open-field run reached its harvest trigger and ended). That is a real and
  # interesting model behaviour, but it is a different phenomenon from the one
  # this figure is answering, and plotted in full it dominates the panel's
  # y-axis and buries the dry-vs-near-normal contrast.
  #
  # Grass-clover is exempt: it is a multi-cut ley, so its repeated
  # cut-and-regrow cycles are exactly what its panel should show.
  # Series end at the harvest PEAK, i.e. the last day before the drop, so each
  # panel reads as an accumulation curve terminating at its yield. Including the
  # post-harvest crash adds a vertical line that carries no information here.
  truncate_at_first_harvest <- function(df) {
    df |>
      dplyr::group_by(crop_renamed, Weather, year) |>
      dplyr::arrange(date, .by_group = TRUE) |>
      dplyr::mutate(
        .drop_frac = 1 - agb_MgDM_ha / dplyr::lag(agb_MgDM_ha),
        .is_harvest = !is.na(.drop_frac) & .drop_frac > 0.5 & dplyr::lag(agb_MgDM_ha) > 1,
        .first_harvest = suppressWarnings(min(which(.is_harvest)))
      ) |>
      dplyr::filter(
        crop_renamed == "Grass-Clover" |
          is.infinite(.first_harvest) |
          dplyr::row_number() < .first_harvest
      ) |>
      dplyr::select(-.drop_frac, -.is_harvest, -.first_harvest) |>
      dplyr::ungroup()
  }

  pooled |>
    dplyr::inner_join(
      contrast_years |> dplyr::select(crop_renamed, year, year_class, window_spei, source),
      by = c("crop_renamed", "year")
    ) |>
    dplyr::mutate(date = as.Date(sprintf("%04d-%02d-%02d", year, month, mday))) |>
    truncate_at_first_harvest() |>
    dplyr::mutate(
      day_of_year = as.integer(format(date, "%j")),
      scenario = factor(unname(weather_display_labels[Weather]),
                        levels = unname(weather_display_labels)),
      year_class = factor(year_class, levels = c("Near-normal", "Dry")),
      crop_renamed = factor(crop_renamed, levels = crop_levels_all),
      series = paste0(scenario, " - ", year_class, " (", year, ")")
    ) |>
    dplyr::filter(!is.na(crop_renamed), !is.na(scenario), !is.na(date)) |>
    dplyr::arrange(crop_renamed, scenario, year_class, day_of_year)
}

# ---------------------------------------------------------------------------
# Legacy-numbered figures (retired from the main text)
#
# These two were built against the legacy script's own Figure_04/Figure_05
# filenames, which predate the v12 manuscript restructure. They are NOT
# manuscript Figures 4 and 5 - the v12 main text has 8 figures on a different
# numbering, and neither of these appears in it. Kept as supplementary
# candidates (broader scenario-grid views than any main figure shows).
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Total ASY biomass (grain + residue, ALL crops - see note below)
# relative to the open-field baseline, 4-rotation averaged.
# ---------------------------------------------------------------------------
# Uses grain + residue_MgDM_ha, not harvested_agb_removed_MgDM_ha: for
# Grass-Clover, grain is ~0 and the Residue Removed/Retained flag (designed
# for cereal straw) forces harvested_agb_removed to ~0 under Retained even
# though the ley IS harvested as hay/silage. grain + residue_MgDM_ha = total
# cut biomass for every crop, which is what "harvested ASY biomass" means for
# a forage-inclusive rotation.
prepare_fig04_data <- function(harvest_annual, included_rotations = paste("Rotation", 1:4),
                                min_reference_biomass = 0.10,
                                crop_display_order = c(
                                  "Winter Wheat", "Grass-Clover", "Spring Barley",
                                  "Ryegrass (undersown)", "Soybean"
                                )) {
  crop_order <- crop_display_order[crop_display_order %in% unique(harvest_annual$crop_renamed)]

  total_agb_per_rotation <- harvest_annual |>
    dplyr::filter(rotation %in% included_rotations, !is.na(crop_renamed)) |>
    dplyr::mutate(total_AGB_MgDM_ha = grain_MgDM_ha + residue_MgDM_ha) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c(
      "soil", "fertilisation", "residue", "rotation", "management_label",
      "weather_is_baseline", "weather_factor", "weather_signed_change_pct",
      "strip_position", "crop_renamed"
    )))) |>
    dplyr::summarise(
      n_years_in_rotation = dplyr::n_distinct(year, na.rm = TRUE),
      total_AGB_sum = sum(total_AGB_MgDM_ha, na.rm = TRUE),
      annualised_total_AGB_MgDM_ha_yr = total_AGB_sum / n_years_in_rotation,
      .groups = "drop"
    )

  crop_biomass_4rot <- total_agb_per_rotation |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c(
      "soil", "fertilisation", "residue", "management_label",
      "weather_is_baseline", "weather_factor", "weather_signed_change_pct",
      "strip_position", "crop_renamed"
    )))) |>
    dplyr::summarise(
      mean_ASY_biomass_MgDM_ha_yr = mean(annualised_total_AGB_MgDM_ha_yr, na.rm = TRUE),
      sd_ASY_biomass_MgDM_ha_yr = sd(annualised_total_AGB_MgDM_ha_yr, na.rm = TRUE),
      n_rotations = dplyr::n_distinct(rotation),
      .groups = "drop"
    ) |>
    dplyr::filter(n_rotations == 4)

  reference_keys <- intersect(
    c("soil", "fertilisation", "residue", "management_label", "crop_renamed"),
    names(crop_biomass_4rot)
  )
  reference_tbl <- crop_biomass_4rot |>
    dplyr::filter(weather_is_baseline) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(reference_keys))) |>
    dplyr::summarise(reference_ASY_biomass_MgDM_ha_yr = mean(mean_ASY_biomass_MgDM_ha_yr, na.rm = TRUE), .groups = "drop")

  crop_biomass_relative <- crop_biomass_4rot |>
    dplyr::left_join(reference_tbl, by = reference_keys) |>
    dplyr::mutate(
      absolute_effect_MgDM_ha_yr = mean_ASY_biomass_MgDM_ha_yr - reference_ASY_biomass_MgDM_ha_yr,
      relative_effect_pct = dplyr::if_else(
        reference_ASY_biomass_MgDM_ha_yr >= min_reference_biomass,
        100 * absolute_effect_MgDM_ha_yr / reference_ASY_biomass_MgDM_ha_yr,
        NA_real_
      ),
      near_zero_reference = reference_ASY_biomass_MgDM_ha_yr < min_reference_biomass
    ) |>
    dplyr::filter(!weather_is_baseline)

  crop_biomass_relative |>
    dplyr::mutate(
      crop_renamed = factor(crop_renamed, levels = crop_order),
      weather_factor = as.character(weather_factor),
      strip_position_chr = as.character(strip_position),
      weather_signed_change_pct = suppressWarnings(as.numeric(weather_signed_change_pct)),
      is_vapv_zero = weather_signed_change_pct == 0,
      strip_abbrev = dplyr::recode(strip_position_chr,
        "Open field" = "OF", "Center" = "C", "East" = "E", "West" = "W", .default = strip_position_chr
      ),
      scenario_axis_plain = dplyr::case_when(
        weather_factor == "Temperature" & weather_signed_change_pct > 0 ~ paste0("+", weather_signed_change_pct, "°C ", strip_abbrev),
        weather_factor == "Temperature" ~ paste0(weather_signed_change_pct, "°C ", strip_abbrev),
        TRUE ~ paste0(weather_signed_change_pct, "% ", strip_abbrev)
      ),
      scenario_axis = dplyr::if_else(is_vapv_zero, paste0("**", scenario_axis_plain, "**"), scenario_axis_plain),
      scenario_sort_key = weather_signed_change_pct * 10 +
        dplyr::recode(strip_position_chr, "Open field" = 0L, "West" = 1L, "Center" = 2L, "East" = 3L, .default = 9L),
      tile_label = dplyr::case_when(
        is.na(relative_effect_pct) ~ "",
        relative_effect_pct > 100 ~ ">100",
        relative_effect_pct < -100 ~ "<-100",
        abs(relative_effect_pct) >= 10 ~ sprintf("%.0f", relative_effect_pct),
        TRUE ~ sprintf("%.1f", relative_effect_pct)
      ),
      text_colour = dplyr::if_else(!is.na(relative_effect_pct) & abs(relative_effect_pct) > 55, "white", "grey15")
    ) |>
    dplyr::filter(!is.na(crop_renamed), !is.na(scenario_axis)) |>
    (\(d) {
      levels_ordered <- d |> dplyr::distinct(scenario_axis, scenario_sort_key) |>
        dplyr::arrange(scenario_sort_key) |> dplyr::pull(scenario_axis)
      d |> dplyr::mutate(scenario_axis_f = factor(scenario_axis, levels = levels_ordered))
    })()
}

# ---------------------------------------------------------------------------
# Figure 5: relative grain yield response (Winter Wheat + Soybean only),
# 4-rotation averaged, at the full W/C/E strip-position scenario grid.
# ---------------------------------------------------------------------------

prepare_harvest_ecw <- function(harvest_annual, crop_filter = NULL) {
  df <- harvest_annual |>
    dplyr::filter(year >= ANALYSIS_START_YEAR) |>
    split_management() |>
    dplyr::mutate(
      crop_renamed = dplyr::case_when(
        crop_renamed %in% c("Ryegrass (GC)", "White Clover (GC)") ~ "Grass-Clover",
        TRUE ~ as.character(crop_renamed)
      ),
      signed_level = dplyr::case_when(
        stringr::str_detect(as.character(weather_scenario), "0p5up") ~ 0.5,
        stringr::str_detect(as.character(weather_scenario), "0p5dwn") ~ -0.5,
        TRUE ~ suppressWarnings(as.numeric(weather_signed_change_pct))
      ),
      wf = dplyr::case_when(weather_is_baseline ~ "Reference", TRUE ~ as.character(weather_factor)),
      scen_label = dplyr::case_when(
        weather_is_baseline ~ "Reference",
        signed_level == 0 & strip_position == "West" & wf == "Radiation" ~ "Rad 0% W",
        signed_level == 0 & strip_position == "Center" & wf == "Radiation" ~ "Rad 0% C",
        signed_level == 0 & strip_position == "East" & wf == "Radiation" ~ "Rad 0% E",
        signed_level == 0 & strip_position == "West" & wf == "Wind" ~ "Wind 0% W",
        signed_level == 0 & strip_position == "Center" & wf == "Wind" ~ "Wind 0% C",
        signed_level == 0 & strip_position == "East" & wf == "Wind" ~ "Wind 0% E",
        signed_level == 0 & strip_position == "West" & wf == "Temperature" ~ "Tmp 0degC W",
        signed_level == 0 & strip_position == "Center" & wf == "Temperature" ~ "Tmp 0degC C",
        signed_level == 0 & strip_position == "East" & wf == "Temperature" ~ "Tmp 0degC E",
        signed_level != 0 & strip_position == "Center" & wf == "Temperature" & signed_level > 0 ~ paste0("Tmp +", signed_level, "degC C"),
        signed_level != 0 & strip_position == "Center" & wf == "Temperature" ~ paste0("Tmp ", signed_level, "degC C"),
        signed_level != 0 & strip_position == "Center" & wf == "Radiation" & signed_level > 0 ~ paste0("Rad +", signed_level, "% C"),
        signed_level != 0 & strip_position == "Center" & wf == "Radiation" ~ paste0("Rad ", signed_level, "% C"),
        signed_level != 0 & strip_position == "Center" & wf == "Wind" & signed_level > 0 ~ paste0("Wind +", signed_level, "% C"),
        signed_level != 0 & strip_position == "Center" & wf == "Wind" ~ paste0("Wind ", signed_level, "% C"),
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(weather_is_baseline | !is.na(signed_level)) |>
    dplyr::filter(scen_label %in% scen_order_ecw)
  if (!is.null(crop_filter)) df <- dplyr::filter(df, crop_renamed %in% crop_filter)
  df
}

prepare_fig05_data <- function(harvest_annual, crop_filter = crop_levels_grain) {
  base <- prepare_harvest_ecw(harvest_annual, crop_filter)

  step1 <- base |>
    dplyr::group_by(crop_renamed, fertiliser_type, residue_policy, scen_label, weather_is_baseline, rotation, year) |>
    dplyr::summarise(annual_grain = sum(grain_MgDM_ha, na.rm = TRUE), .groups = "drop")

  step2 <- step1 |>
    dplyr::group_by(crop_renamed, fertiliser_type, residue_policy, scen_label, weather_is_baseline, rotation) |>
    dplyr::summarise(rot_grain = mean(annual_grain, na.rm = TRUE), .groups = "drop")

  ref_rot <- step2 |>
    dplyr::filter(weather_is_baseline) |>
    dplyr::select(crop_renamed, fertiliser_type, residue_policy, rotation, ref_rot_asy = rot_grain)

  step2 |>
    dplyr::filter(!weather_is_baseline) |>
    dplyr::left_join(ref_rot, by = c("crop_renamed", "fertiliser_type", "residue_policy", "rotation")) |>
    dplyr::filter(!is.na(ref_rot_asy)) |>
    dplyr::mutate(rot_pct_resp = 100 * (rot_grain - ref_rot_asy) / ref_rot_asy) |>
    dplyr::group_by(crop_renamed, fertiliser_type, residue_policy, scen_label) |>
    dplyr::summarise(
      mean_resp = mean(rot_pct_resp, na.rm = TRUE),
      # SD across the four rotation permutations. This project reports SD
      # everywhere, never SE: the permutations are a fixed phase-shift design
      # in a deterministic simulation ensemble, so there is no sampling
      # distribution for an SE to describe. The legacy code carried an
      # `se_resp` column that held an SD value - that alias is deliberately
      # not reproduced here, because a column named se_ holding an sd_ value
      # is exactly how the "+/- 1 SE" caption error got into the draft.
      sd_resp = sd(rot_pct_resp, na.rm = TRUE),
      n_rot = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      scen_label = factor(scen_label, levels = scen_order_ecw[scen_order_ecw != "Reference"]),
      crop_renamed = factor(as.character(crop_renamed), levels = crop_filter),
      residue_policy = factor(as.character(residue_policy), levels = c("Residue removed", "Residue retained"))
    ) |>
    dplyr::filter(!is.na(crop_renamed), !is.na(scen_label))
}

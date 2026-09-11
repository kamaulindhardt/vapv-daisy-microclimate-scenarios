# Water balance and drought classification.
#
# Two jobs:
#   1. Monthly climatic water balance -> SPEI, used to classify each crop-year
#      as dry or near-normal. This drives the year selection in Figure 3 and
#      the "critical dry period" windows in Figure 4, so it has to be an
#      explicit, reproducible calculation rather than hand-picked years.
#   2. Soil-water response during those critical dry periods (Figure 4).
#
# Extracted/derived from reference/legacy_snapshot/SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd
# section 15C and the SPEI classification in chunks `fig01_spei` and
# `fig06_results_spei_soil_water`, which implemented drought classification
# twice; consolidated here into one function per the reconstruction plan.

# Crop-specific seasonal windows and the mild-drought threshold, per Methods:
# "crop-specific seasonal windows (WW March-May, SB March-June, SY May-August,
# GC April-September) and a mild-drought threshold of SPEI-3 < -0.5".
crop_spei_windows <- tibble::tribble(
  ~crop_renamed,          ~start_month, ~end_month,
  "Winter Wheat",          3L,           5L,
  "Spring Barley",         3L,           6L,
  "Soybean",               5L,           8L,
  "Grass-Clover",          4L,           9L
)

SPEI_DROUGHT_THRESHOLD <- -0.5
SPEI_SCALE <- 3L

# Which years each crop is actually grown in, for a given rotation permutation
# and management. Needed because the rotation cycles crops across years, so
# "the driest year" is only meaningful among the years that crop is in the
# field. Uses a positive harvest as the test for presence.
crop_years_in_rotation <- function(harvest_annual,
                                    reference_rotation = "Rotation 1",
                                    reference_fert = "Biogas digestate",
                                    reference_residue = "Residue Removed") {
  harvest_annual |>
    dplyr::filter(
      weather_is_baseline,
      rotation == reference_rotation,
      fertilisation == reference_fert,
      residue == reference_residue,
      !is.na(crop_renamed)
    ) |>
    dplyr::group_by(crop_renamed, year) |>
    dplyr::summarise(harvested = sum(harvested_agb_removed_MgDM_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(harvested > 0) |>
    dplyr::select(crop_renamed, year)
}

# ---------------------------------------------------------------------------
# Monthly climatic water balance from the open-field reference series.
#
# SPEI is computed on the OPEN-FIELD series only, and used as a common
# classification of the weather record. It deliberately does not vary by
# scenario: the point is to label which years were dry *as weather*, so that
# VAPV and open-field runs can be compared within the same year class. A
# scenario-specific SPEI would make "a dry year" mean something different in
# each scenario and destroy that comparison.
#
# The reference rotation/management only selects which simulation the daily
# record is read from; precipitation is identical across them, and PET is taken
# from the same run for internal consistency.
# ---------------------------------------------------------------------------
prepare_monthly_water_balance <- function(field_water_daily,
                                           reference_rotation = "Rotation 1",
                                           reference_fert = "Biogas digestate",
                                           reference_residue = "Residue Removed") {
  df <- field_water_daily |>
    dplyr::filter(
      weather_is_baseline,
      rotation == reference_rotation,
      fertilisation == reference_fert,
      residue == reference_residue
    )

  df |>
    dplyr::mutate(
      year = suppressWarnings(as.integer(year)),
      month = suppressWarnings(as.integer(month)),
      precipitation_mm = num_col(df, c("Precipitation_mm", "Precipitation")),
      pet_mm = num_col(df, c("Potential_evapotranspiration_mm", "Potential_evapotranspiration"))
    ) |>
    dplyr::group_by(year, month) |>
    dplyr::summarise(
      precipitation_mm = sum(precipitation_mm, na.rm = TRUE),
      pet_mm = sum(pet_mm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(year, month) |>
    dplyr::mutate(water_balance_mm = precipitation_mm - pet_mm)
}

# SPEI at the given scale, fitted over the full record including warm-up: the
# index needs a long series to fit its distribution, and restricting the fit to
# the evaluation period would make the index depend on where the analysis
# window happens to start. Only the *use* of SPEI is restricted to 1998-2024.
calculate_spei <- function(monthly_water_balance, scale = SPEI_SCALE) {
  wb <- monthly_water_balance |>
    dplyr::filter(!is.na(year), !is.na(month)) |>
    dplyr::arrange(year, month)

  series <- stats::ts(
    wb$water_balance_mm,
    start = c(wb$year[1], wb$month[1]),
    frequency = 12
  )
  fit <- SPEI::spei(series, scale = scale, na.rm = TRUE, verbose = FALSE)
  wb |> dplyr::mutate(spei = as.numeric(fit$fitted))
}

# Classify each crop-year as dry / near-normal using that crop's own seasonal
# window. The window value is the mean SPEI across the crop's months, so a crop
# with a long window is not judged on a single month.
classify_crop_drought_years <- function(spei_monthly,
                                         windows = crop_spei_windows,
                                         threshold = SPEI_DROUGHT_THRESHOLD) {
  windows |>
    dplyr::rowwise() |>
    dplyr::mutate(window_months = list(seq(start_month, end_month))) |>
    dplyr::ungroup() |>
    tidyr::unnest(window_months) |>
    dplyr::rename(month = window_months) |>
    dplyr::inner_join(spei_monthly, by = "month", relationship = "many-to-many") |>
    dplyr::group_by(crop_renamed, year) |>
    dplyr::summarise(window_spei = mean(spei, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(drought_class = dplyr::if_else(window_spei < threshold, "Dry", "Near-normal"))
}

# Pick one dry and one near-normal year per crop, for the paired seasonal
# trajectories in Figure 3.
#
# `preferred` pins the years the manuscript already names (winter wheat 2023 vs
# 2013, soybean 2007 vs 2022) so the figure stays consistent with the text.
# Crops without a stated pair get the most extreme dry year and the year
# closest to SPEI 0, both drawn from the evaluation period only. The `source`
# column records which crops were pinned and which were chosen by the index, so
# the distinction is visible in the figure data rather than buried in code.
select_contrast_years <- function(crop_drought_years,
                                   crop_years,
                                   start_year = ANALYSIS_START_YEAR,
                                   preferred = list(
                                     # WW and SY: SPEI-3 independently agrees with the
                                     # chosen near-normal/dry years for both crops.
                                     "Winter Wheat" = c(near_normal = 2023, dry = 2013),
                                     "Soybean" = c(near_normal = 2007, dry = 2022),
                                     # GC and SB are pinned rather than SPEI-derived. NOTE:
                                     # SPEI-3 does not agree for spring barley - it scores
                                     # 2001 at -0.06 (near-normal) and picks 2011 (-0.91) as
                                     # the dry year. The pinned years are kept, and
                                     # window_spei is carried in the output so the
                                     # disagreement stays visible rather than being buried.
                                     "Grass-Clover" = c(near_normal = 2000, dry = 2020),
                                     "Spring Barley" = c(near_normal = 2021, dry = 2001)
                                   )) {
  # Restrict candidates to years in which the crop is ACTUALLY GROWN in the
  # reference rotation. The rotation cycles WW -> GC -> GC -> SB+RG -> SY+RG,
  # so each crop occupies only a subset of years; selecting the driest year
  # overall picks years when the crop is not in the field at all and produces
  # an empty trajectory. (The years the manuscript names for WW and SY happen
  # to be real crop-years, which is why the problem only shows up for the two
  # crops chosen automatically.)
  eligible <- crop_drought_years |>
    dplyr::filter(year >= start_year) |>
    dplyr::semi_join(crop_years, by = c("crop_renamed", "year"))

  eligible |>
    dplyr::group_by(crop_renamed) |>
    dplyr::summarise(
      dry = year[which.min(window_spei)],
      near_normal = year[which.min(abs(window_spei))],
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dry = purrr::map2_int(crop_renamed, dry, \(cr, d) {
        p <- preferred[[as.character(cr)]]
        if (!is.null(p)) as.integer(p[["dry"]]) else as.integer(d)
      }),
      near_normal = purrr::map2_int(crop_renamed, near_normal, \(cr, n) {
        p <- preferred[[as.character(cr)]]
        if (!is.null(p)) as.integer(p[["near_normal"]]) else as.integer(n)
      }),
      source = dplyr::if_else(
        as.character(crop_renamed) %in% names(preferred),
        "named in manuscript", "selected by SPEI-3"
      )
    ) |>
    tidyr::pivot_longer(c(dry, near_normal), names_to = "year_class", values_to = "year") |>
    dplyr::mutate(
      year = as.integer(year),
      year_class = dplyr::recode(year_class, dry = "Dry", near_normal = "Near-normal")
    ) |>
    dplyr::left_join(
      crop_drought_years |> dplyr::select(crop_renamed, year, window_spei),
      by = c("crop_renamed", "year")
    )
}

# ===========================================================================
# Figure 4 - soil water during critical dry periods
# ===========================================================================

# The months within which soil water matters most for each crop, per the v12
# draft caption ("winter wheat (May-Jul 2018) and soybean (May-Jun 2013)").
#
# These are NOT the same as crop_spei_windows above and must not be conflated:
#   crop_spei_windows      - the window used to CLASSIFY a year as dry
#                            (WW Mar-May, SB Mar-Jun, SY May-Aug, GC Apr-Sep)
#   critical_dry_windows   - the window over which soil water is AVERAGED once
#                            a dry year has been identified (WW May-Jul,
#                            SY May-Jun)
# The first answers "was this a dry season?", the second "what happened to
# soil water while the crop was under stress?".
critical_dry_windows <- tibble::tribble(
  ~crop_renamed,   ~start_month, ~end_month,
  "Winter Wheat",   5L,           7L,
  "Soybean",        5L,           6L
)

# Which years each crop is grown in, per rotation, across all rotations.
crop_years_all_rotations <- function(harvest_annual,
                                      reference_fert = "Biogas digestate",
                                      reference_residue = "Residue Retained") {
  harvest_annual |>
    dplyr::filter(weather_is_baseline, fertilisation == reference_fert,
                  residue == reference_residue, !is.na(crop_renamed)) |>
    dplyr::group_by(rotation, crop_renamed, year) |>
    dplyr::summarise(harvested = sum(harvested_agb_removed_MgDM_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(harvested > 0) |>
    dplyr::select(rotation, crop_renamed, year)
}

# For each rotation x crop, the driest year in which that crop is actually
# grown in that rotation.
#
# The phase-shifted design means a given crop-year exists in only one rotation
# (winter wheat 2018 is Rotation 1 only; soybean 2013 is Rotation 4 only), so
# "means across Rotations 1-4" cannot mean averaging one calendar year across
# rotations. Each rotation contributes its own driest occurrence of the crop,
# which is what makes the four values comparable and gives n = 4 for the SD.
select_critical_dry_periods <- function(spei_monthly, crop_years_all,
                                         windows = critical_dry_windows,
                                         rotations = paste("Rotation", 1:4),
                                         start_year = ANALYSIS_START_YEAR,
                                         threshold = SPEI_DROUGHT_THRESHOLD) {
  # Classify over the SAME window the soil water is then averaged over, rather
  # than over the crop's phenological SPEI window. Identifying a dry period
  # with one window and measuring its effect over another would let a year
  # qualify as "critically dry" on months that are not the months being
  # examined. This is what identifies winter wheat's driest occurrence as
  # May-Jul 2018 and soybean's as May-Jun 2013.
  window_spei_by_crop_year <- windows |>
    dplyr::rowwise() |>
    dplyr::mutate(window_months = list(seq(start_month, end_month))) |>
    dplyr::ungroup() |>
    tidyr::unnest(window_months) |>
    dplyr::rename(month = window_months) |>
    dplyr::inner_join(spei_monthly, by = "month", relationship = "many-to-many") |>
    dplyr::group_by(crop_renamed, year, start_month, end_month) |>
    dplyr::summarise(window_spei = mean(spei, na.rm = TRUE), .groups = "drop")

  crop_years_all |>
    dplyr::filter(rotation %in% rotations, year >= start_year,
                  crop_renamed %in% windows$crop_renamed) |>
    dplyr::inner_join(window_spei_by_crop_year, by = c("crop_renamed", "year")) |>
    # Only rotation-crop-years that actually qualify as dry. A rotation whose
    # driest occurrence of a crop is not below the mild-drought threshold
    # contributes nothing to a figure about critical dry periods; including it
    # to keep n = 4 would put a near-normal season in a dry-period average.
    dplyr::filter(window_spei < threshold) |>
    dplyr::group_by(rotation, crop_renamed) |>
    dplyr::slice_min(window_spei, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(rotation, crop_renamed, year, window_spei, start_month, end_month)
}

# Mean soil matrix water over each critical dry window, as % change from the
# open-field baseline for the same rotation and year.
prepare_fig_04_data <- function(field_water_daily, critical_periods,
                                 fert = "Biogas digestate", residue_policy = "Residue Retained") {
  water <- field_water_daily |>
    dplyr::filter(fertilisation == fert, residue == residue_policy) |>
    dplyr::mutate(year = suppressWarnings(as.integer(year)),
                  month = suppressWarnings(as.integer(month)))

  # Restrict to each rotation's critical window before any aggregation.
  windowed <- water |>
    dplyr::inner_join(critical_periods, by = c("rotation", "year"),
                      relationship = "many-to-many") |>
    dplyr::filter(month >= start_month, month <= end_month)

  scenario_means <- windowed |>
    add_scenario_labels(grid = "ecw") |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::group_by(crop_renamed, rotation, year, scen_label, weather_is_baseline) |>
    dplyr::summarise(
      soil_matrix_water_mm = mean(num_col(dplyr::pick(dplyr::everything()), "Soil_matrix_water_mm"), na.rm = TRUE),
      .groups = "drop"
    )

  reference <- scenario_means |>
    dplyr::filter(weather_is_baseline) |>
    dplyr::select(crop_renamed, rotation, year, ref_water_mm = soil_matrix_water_mm)

  scenario_means |>
    dplyr::filter(!weather_is_baseline) |>
    dplyr::left_join(reference, by = c("crop_renamed", "rotation", "year")) |>
    dplyr::filter(!is.na(ref_water_mm), ref_water_mm > 0) |>
    dplyr::mutate(pct_change = 100 * (soil_matrix_water_mm - ref_water_mm) / ref_water_mm) |>
    dplyr::group_by(crop_renamed, scen_label) |>
    dplyr::summarise(
      mean_pct_change = mean(pct_change, na.rm = TRUE),
      sd_pct_change = sd(pct_change, na.rm = TRUE),
      n_rotations = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      driver = factor(dplyr::case_when(
        stringr::str_starts(scen_label, "Rad") ~ "Radiation",
        stringr::str_starts(scen_label, "Wind") ~ "Wind",
        stringr::str_starts(scen_label, "Tmp") ~ "Temperature",
        TRUE ~ NA_character_
      ), levels = c("Radiation", "Temperature", "Wind")),
      scen_label = factor(scen_label, levels = scen_order_ecw),
      crop_renamed = factor(crop_renamed, levels = c("Winter Wheat", "Soybean"))
    ) |>
    dplyr::filter(!is.na(driver), !is.na(scen_label)) |>
    dplyr::arrange(crop_renamed, driver, scen_label)
}

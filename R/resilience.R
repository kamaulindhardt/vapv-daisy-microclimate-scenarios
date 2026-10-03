# Interannual yield stability and downside risk (Supplementary Figure S20).
#
# ---------------------------------------------------------------------------
# WHY THIS FIGURE EXISTS
# ---------------------------------------------------------------------------
# Every productivity result in the manuscript - Figure 2, Figure 7, the ASY
# headline - is a MEAN over 27 simulated years. The Discussion nonetheless
# frames vertical agrivoltaics partly as a climate-adaptation measure, i.e. as
# something that changes exposure to bad years. A mean cannot support or refute
# that: a scenario that lowers average yield while removing the worst years is a
# very different agronomic proposition from one that lowers average yield and
# leaves the tail untouched.
#
# This figure separates the two. It asks three questions of the same data:
#   (a) does VAPV change year-to-year variability?          -> CV
#   (b) is variability traded against level?                -> risk-return plane
#   (c) are BAD years protected more than average years?    -> worst-year test
#
# ---------------------------------------------------------------------------
# WHY PER-CROP AND NOT AT SYSTEM LEVEL
# ---------------------------------------------------------------------------
# The obvious construction - CV of the annual whole-system harvested AGB - is
# not usable here, and it is worth stating why rather than leaving it as an
# unexplained design choice.
#
# The rotation is a fixed five-course cycle, so consecutive years grow different
# crops. A grass-clover ley year removes on the order of 10-15 Mg DM/ha while a
# soybean year removes a small fraction of that. The between-year spread of the
# system total is therefore dominated by WHICH CROP IS IN THE FIELD, not by
# weather. Its CV is large (tens of percent) in every scenario including the
# open-field baseline, and it barely moves between scenarios, because the crop
# sequence is identical in all of them. It would be a measure of the rotation
# design, not of climate risk.
#
# Computing variability WITHIN a crop - across the years that crop is grown,
# pooled over the four rotation permutations - removes the crop-phase term by
# construction. What is left is genuine weather-driven interannual variability,
# which is the quantity the resilience argument is actually about.
#
# The four rotation permutations are pooled rather than averaged here (unlike
# ASY, where they are averaged at step 3). They are phase shifts of one rotation,
# so a given crop appears in different calendar years in each - pooling them is
# what gives ~21-27 crop-years per crop instead of ~5. The rotation identity is
# retained in the intermediate table so the pooling can be checked.
# ---------------------------------------------------------------------------

# Scenarios shown. The full centre-strip grid is carried through the data prep;
# panels (b) and (c) would be unreadable with all 24, so they use the driver
# extremes plus each driver's 0-level. Panel (a) shows the full gradient.
s20_scenarios_highlight <- c("Reference",
                             "Rad 0%", "Rad -30%",
                             "Wind 0%", "Wind -70%",
                             "Tmp 0degC", "Tmp -3degC", "Tmp +3degC")

# Undersown ryegrass is excluded from the stability analysis. It is a catch
# crop whose harvested biomass is incidental to the rotation's yield, it is
# present in only a subset of years, and its relative variability is dominated
# by establishment timing rather than by season weather - including it would add
# a high-CV series that says nothing about the question being asked.
s20_crops <- c("Winter Wheat", "Spring Barley", "Soybean", "Grass-Clover")

# ---------------------------------------------------------------------------
# LEY YEAR: the same crop-phase problem, one level down.
#
# Grass-clover occupies TWO consecutive years of the five-course rotation, and
# the two are not equivalent - the establishment year and the full production
# year yield very differently by design. Pooling them gives grass-clover an
# open-field CV of about 64%, three times any other crop's, and that number
# describes the ley cycle rather than weather. It would also make every
# scenario comparison for grass-clover unreadable, because a scenario that
# shifts yield between the two ley years would show up as a change in
# "stability" that has nothing to do with interannual risk.
#
# So the within-crop grouping is extended to (crop, position within its
# consecutive run). Runs are detected from the simulated harvest record rather
# than assumed, and runs truncated by the start or end of the evaluation period
# are dropped - a ley whose first year fell in 1997 would otherwise have its
# production year counted as an establishment year.
# ---------------------------------------------------------------------------
add_ley_year_index <- function(crop_years) {
  indexed <- crop_years |>
    dplyr::arrange(crop_renamed, scen_label, rotation, year) |>
    dplyr::group_by(crop_renamed, scen_label, rotation) |>
    dplyr::mutate(
      run_seq = cumsum(dplyr::if_else(
        is.na(dplyr::lag(year)) | (year - dplyr::lag(year)) != 1L, 1L, 0L))
    ) |>
    dplyr::group_by(crop_renamed, scen_label, rotation, run_seq) |>
    dplyr::mutate(
      ley_year = dplyr::row_number(),
      run_length = dplyr::n(),
      run_first_year = min(year),
      run_last_year = max(year)
    ) |>
    dplyr::ungroup()

  # The expected run length for a crop is its modal run length across all
  # rotations and scenarios - robust to the truncated runs at either end.
  expected <- indexed |>
    dplyr::group_by(crop_renamed) |>
    dplyr::summarise(
      expected_run = as.integer(names(sort(table(run_length), decreasing = TRUE))[1]),
      .groups = "drop"
    )

  record_start <- min(indexed$year)
  record_end <- max(indexed$year)

  indexed |>
    dplyr::left_join(expected, by = "crop_renamed") |>
    dplyr::filter(
      run_length == expected_run |
        (run_first_year > record_start & run_last_year < record_end)
    ) |>
    dplyr::mutate(
      crop_phase = dplyr::if_else(
        expected_run > 1L,
        paste0(as.character(crop_renamed), " (ley yr ", ley_year, ")"),
        as.character(crop_renamed)
      )
    )
}

prepare_fig_s20_data <- function(harvest_annual,
                                  response_col = "harvested_agb_removed_MgDM_ha",
                                  crops = s20_crops,
                                  management = "Biogas digestate | Residue Removed",
                                  rotations = paste("Rotation", 1:4),
                                  start_year = ANALYSIS_START_YEAR) {
  # Step 1 - one row per crop x scenario x rotation x year. A crop can be cut
  # several times in a year (grass-clover), so the within-year sum comes first,
  # exactly as in calculate_system_asy().
  crop_years <- harvest_annual |>
    dplyr::filter(year >= start_year, rotation %in% rotations,
                  crop_renamed %in% crops) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label), management_label == management) |>
    dplyr::group_by(crop_renamed, scen_label, rotation, year) |>
    dplyr::summarise(annual_yield = sum(.data[[response_col]], na.rm = TRUE),
                     .groups = "drop") |>
    # A crop-year with no harvest is a year the crop was not grown in that
    # rotation phase, not a crop failure. Including the zeros would inflate
    # every CV by the same large constant and destroy the comparison.
    dplyr::filter(annual_yield > 0) |>
    add_ley_year_index()

  # Step 2 - stability statistics across the pooled crop-years.
  crop_years |>
    dplyr::group_by(crop_renamed, crop_phase, scen_label) |>
    dplyr::summarise(
      mean_yield = mean(annual_yield),
      sd_yield = sd(annual_yield),
      cv_pct = 100 * sd(annual_yield) / mean(annual_yield),
      # Worst-year exposure. The 10th percentile is reported alongside the
      # outright minimum because a single simulated minimum is one realisation
      # of one weather series, while p10 uses the shape of the lower tail.
      p10_yield = unname(quantile(annual_yield, 0.10, na.rm = TRUE)),
      min_yield = min(annual_yield),
      n_crop_years = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      driver = driver_of_scen_label(scen_label),
      crop_renamed = factor(crop_renamed, levels = crops)
    ) |>
    dplyr::filter(!is.na(driver)) |>
    dplyr::mutate(crop_phase = factor(crop_phase, levels = s20_crop_phase_levels(crops)))
}

# Ordered crop-phase levels, with any multi-year crop expanded in place so the
# ley years sit next to their own crop rather than at the end of the axis.
s20_crop_phase_levels <- function(crops = s20_crops) {
  unlist(lapply(crops, function(cr) {
    if (cr == "Grass-Clover") paste0(cr, " (ley yr ", 1:2, ")") else cr
  }), use.names = FALSE)
}

# Express level and tail against the open-field baseline for the SAME crop, so
# panels (b) and (c) share one axis definition. Kept separate from the summary
# above because the reference row must be removed only after it has been used.
add_s20_relative <- function(fig_s20_data) {
  # Joined on crop_phase, not crop: a grass-clover establishment year must be
  # compared against the open-field ESTABLISHMENT year, not against the pooled
  # ley mean, or the ratio would carry the ley-year difference.
  reference <- fig_s20_data |>
    dplyr::filter(scen_label == "Reference") |>
    dplyr::select(crop_phase,
                  ref_mean = mean_yield, ref_p10 = p10_yield,
                  ref_min = min_yield, ref_cv = cv_pct)

  fig_s20_data |>
    dplyr::left_join(reference, by = "crop_phase") |>
    dplyr::mutate(
      mean_pct_of_openfield = 100 * mean_yield / ref_mean,
      p10_pct_of_openfield = 100 * p10_yield / ref_p10,
      min_pct_of_openfield = 100 * min_yield / ref_min,
      # Positive = variability increased relative to open field.
      delta_cv_pp = cv_pct - ref_cv
    )
}

# Does the tail move differently from the mean?
#
# The test in panel (c): if worst-year yield as a % of the open-field worst year
# EXCEEDS mean yield as a % of the open-field mean, the scenario has compressed
# the lower tail - bad years lost proportionally less than average years, which
# is what "buffering" would mean. Below the 1:1 line is the opposite: the
# scenario hurts bad years disproportionately.
summarise_s20_buffering <- function(fig_s20_relative,
                                     scenarios = s20_scenarios_highlight) {
  fig_s20_relative |>
    dplyr::filter(scen_label %in% scenarios, scen_label != "Reference") |>
    dplyr::mutate(
      buffering_pp = p10_pct_of_openfield - mean_pct_of_openfield,
      verdict = dplyr::case_when(
        buffering_pp > 2 ~ "Tail buffered",
        buffering_pp < -2 ~ "Tail amplified",
        TRUE ~ "Neutral"
      )
    ) |>
    dplyr::arrange(crop_phase, scen_label) |>
    dplyr::select(crop_phase, scen_label, driver,
                  mean_pct_of_openfield, p10_pct_of_openfield,
                  cv_pct, delta_cv_pp, buffering_pp, verdict, n_crop_years)
}

# Crop-phase palette: the two grass-clover ley years get light/dark shades of
# the crop's own green so they read as one crop split in two, not two crops.
s20_crop_phase_palette <- c(
  "Winter Wheat" = "#F1A983",
  "Spring Barley" = "#FFD966",
  "Soybean" = "#44B3E1",
  "Grass-Clover (ley yr 1)" = "#95D07A",
  "Grass-Clover (ley yr 2)" = "#2E7D1B"
)

# Nitrogen cycle: mineralisation, crop uptake, biological fixation, leaching.
#
# Source: out_Annual-FN_100cm_Apr.csv, i.e. DAISY's nitrogen balance cumulated
# to 100 cm depth and snapshotted at a fixed 1 April checkpoint (Methods).
# The April checkpoint - not the calendar year - is the agrohydrological year
# boundary: it places the whole autumn-to-spring drainage season, when almost
# all leaching happens, inside a single reporting year. Aggregating N on
# calendar years would split each leaching season across two rows and
# understate the year-to-year contrast.
#
# NOTE on the leading-comma header: this file (and its September twin) ships a
# header row with one extra empty leading field, so a naive read shifts every
# column name one position left and silently mislabels year/month. The repair
# lives in read_nwaps_output() (R/import.R); verified here by checking that
# RotationManagement/Weather/year land in the right columns.
#
# Ported from reference/legacy_snapshot/SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd chunk 15D.

# Flux definitions. Kept as a table rather than scattered through the code so
# that "what counts as leaching" is stated once and visible.
n_flux_definitions <- tibble::tribble(
  ~flux,             ~display_label,           ~source_columns,
  "leaching",        "N leaching",             c("Matrix-Leaching_kg_N_ha", "Biopore-Leaching_kg_N_ha"),
  "mineralisation",  "Mineralisation",         c("Mineralization_kg_N_ha"),
  "crop_uptake",     "Crop N uptake",          c("Crop-Uptake_kg_N_ha"),
  "fixation",        "Biological N fixation",  c("Fixated_kg_N_ha")
)

# Panel (a) shows the supply-and-demand pathways that the leaching balance in
# panel (b) is drawn from: what the soil releases (mineralisation), what the
# crop takes up (uptake), and what the legumes add (fixation). Ordering them
# this way makes the two panels read as cause and consequence rather than as
# four unrelated metrics.
n_flux_panel_order <- c("Mineralisation", "Crop N uptake", "Biological N fixation")

prepare_n_annual <- function(field_n_apr, start_year = ANALYSIS_START_YEAR) {
  df <- field_n_apr |>
    dplyr::mutate(year = suppressWarnings(as.integer(year))) |>
    dplyr::filter(year >= start_year)

  df |>
    dplyr::mutate(
      # Total leaching is matrix + biopore: DAISY routes water through both the
      # soil matrix and macropores, and both carry nitrate past 100 cm.
      # Counting only matrix leaching would omit the preferential-flow pathway.
      leaching_kgN_ha = num_col(df, "Matrix-Leaching_kg_N_ha") + num_col(df, "Biopore-Leaching_kg_N_ha"),
      mineralisation_kgN_ha = num_col(df, "Mineralization_kg_N_ha"),
      crop_uptake_kgN_ha = num_col(df, "Crop-Uptake_kg_N_ha"),
      fixation_kgN_ha = num_col(df, "Fixated_kg_N_ha"),
      immobilisation_kgN_ha = num_col(df, "Immobilization_kg_N_ha"),
      fertiliser_mineral_kgN_ha = num_col(df, "Min-Surface-Fertilizer_kg_N_ha") +
        num_col(df, "Min-Soil-Fertilizer_kg_N_ha"),
      fertiliser_organic_kgN_ha = num_col(df, "Org-Fertilizer_kg_N_ha"),
      harvest_n_kgN_ha = num_col(df, "Harvest_kg_N_ha")
    )
}

# Mean and SD of each flux per scenario x management, using the same three-step
# aggregation as annualised system yield: annual value -> mean across years
# within each rotation -> mean +/- SD across the four rotations.
#
# The v12 draft captions say "+/- 1 SD across years", i.e. SD over the pooled
# rotation-years. That is not usable here, and the figure shows why: pooling
# years mixes the SCENARIO response with the ROTATION PHASE. Biological N
# fixation is the clearest case - a grass-clover or soybean year fixes several
# hundred kg N/ha while a cereal year fixes almost none, so the pooled
# distribution is bimodal and its SD spans from below zero to above 400 kg
# N/ha, which is both physically impossible for a strictly positive flux and
# says nothing about the scenario effect being plotted.
#
# Averaging within each rotation first removes that: every rotation covers the
# full crop cycle, so its mean is directly comparable to the others, and the SD
# across the four then describes genuine rotation-to-rotation variability.
#
# The MEANS are unchanged by this - each rotation contributes the same number
# of evaluation years, so the mean of rotation means equals the pooled mean.
# The values validated against the Results text in
# validation/check_fig_05_vs_manuscript.R therefore still hold exactly.
summarise_n_fluxes <- function(n_annual, rotations = paste("Rotation", 1:4)) {
  n_annual |>
    dplyr::filter(rotation %in% rotations) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label)) |>
    tidyr::pivot_longer(
      cols = c(leaching_kgN_ha, mineralisation_kgN_ha, crop_uptake_kgN_ha, fixation_kgN_ha),
      names_to = "flux", values_to = "value_kgN_ha"
    ) |>
    dplyr::mutate(flux = dplyr::recode(flux,
      leaching_kgN_ha = "N leaching",
      mineralisation_kgN_ha = "Mineralisation",
      crop_uptake_kgN_ha = "Crop N uptake",
      fixation_kgN_ha = "Biological N fixation"
    )) |>
    dplyr::group_by(fertilisation, residue, management_label, scen_label, flux, rotation) |>
    dplyr::summarise(rotation_mean_kgN_ha = mean(value_kgN_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(fertilisation, residue, management_label, scen_label, flux) |>
    dplyr::summarise(
      mean_kgN_ha = mean(rotation_mean_kgN_ha, na.rm = TRUE),
      sd_kgN_ha = sd(rotation_mean_kgN_ha, na.rm = TRUE),
      n_rotations = dplyr::n(),
      .groups = "drop"
    )
}

# --- Figure 5 (Figure 6 in the manuscript numbering) -------------------------
# One merged panel per driver (see plot_fig_05() in R/figures.R):
#   fig_05_a_data - supply/demand fluxes, Dig-Rem   -> left y axis
#   fig_05_b_data - N leaching, all four regimes    -> right (secondary) y axis
# Both tables stay in real kg N/ha/yr; plot_fig_05() rescales leaching for display.
#
# The leaching table used to hold three regimes: Mineral fertiliser | Residue
# Retained was missing, although the Results text says the leaching decline held
# "under all four fertilisation-residue treatments". Co-author feedback asked why
# not every option was shown (and for the two panels to be merged), so it now
# carries all four.
#
# Wind is excluded from both tables: the manuscript's nitrogen section is about
# radiation and temperature, and the wind response is negligible (Figure 2c).
# Showing a flat wind panel here would add width without adding a message.

fig_05_drivers <- c("Radiation", "Temperature")

prepare_fig_05_a_data <- function(n_flux_summary,
                                   fert = "Biogas digestate", residue_policy = "Residue Removed") {
  n_flux_summary |>
    dplyr::filter(fertilisation == fert, residue == residue_policy,
                  flux %in% n_flux_panel_order) |>
    dplyr::mutate(driver = driver_of_scen_label(scen_label)) |>
    dplyr::filter(driver %in% c("Reference", fig_05_drivers)) |>
    add_reference_to_each_driver(fig_05_drivers) |>
    dplyr::mutate(
      flux = factor(flux, levels = n_flux_panel_order),
      scen_label = factor(scen_label, levels = scen_order_center),
      driver = factor(driver, levels = fig_05_drivers)
    ) |>
    dplyr::filter(!is.na(scen_label), !is.na(driver)) |>
    dplyr::arrange(driver, scen_label, flux)
}

prepare_fig_05_b_data <- function(n_flux_summary, managements = management_levels) {
  n_flux_summary |>
    dplyr::filter(flux == "N leaching", management_label %in% managements) |>
    dplyr::mutate(driver = driver_of_scen_label(scen_label)) |>
    dplyr::filter(driver %in% c("Reference", fig_05_drivers)) |>
    add_reference_to_each_driver(fig_05_drivers) |>
    dplyr::mutate(
      management_label = factor(management_label, levels = managements),
      scen_label = factor(scen_label, levels = scen_order_center),
      driver = factor(driver, levels = fig_05_drivers)
    ) |>
    dplyr::filter(!is.na(scen_label), !is.na(driver)) |>
    dplyr::arrange(driver, scen_label, management_label)
}

# --- Supplementary Figure S11 ----------------------------------------------
# Main-text Figure 5 draws the fluxes for Dig-Rem only. S11 shows the same fluxes
# and leaching across all four management regimes, to demonstrate that the
# coupled suppression under radiation and the asymmetric amplification under
# warming hold regardless of fertiliser source or residue handling - i.e. that
# management shifts the absolute level without changing the directional
# sensitivity. (Mineral fertiliser | Residue Retained was missing here until the
# same omission was fixed in Figure 5's leaching.)
#
# Panel (b) is drawn from fig_05_b_data, so it shows exactly the leaching series
# of Figure 5 - on its own axis here, on the shared secondary one there.

prepare_fig_s11_a_data <- function(n_flux_summary, managements = management_levels) {
  n_flux_summary |>
    dplyr::filter(flux %in% n_flux_panel_order, management_label %in% managements) |>
    dplyr::mutate(driver = driver_of_scen_label(scen_label)) |>
    dplyr::filter(driver %in% c("Reference", fig_05_drivers)) |>
    add_reference_to_each_driver(fig_05_drivers) |>
    dplyr::mutate(
      flux = factor(flux, levels = n_flux_panel_order),
      scen_label = factor(scen_label, levels = scen_order_center),
      driver = factor(driver, levels = fig_05_drivers),
      management_label = factor(management_label, levels = managements)
    ) |>
    dplyr::filter(!is.na(scen_label), !is.na(driver)) |>
    dplyr::arrange(management_label, driver, scen_label, flux)
}

# ===========================================================================
# Supplementary Figure S13 - when in the year does the leaching happen?
# ===========================================================================
# Figure 5 reports one leaching total per agrohydrological year and the
# Discussion interprets scenario differences in that total. For Danish nitrate
# regulation, and for any mitigation argument built on this work, the annual
# total is the less useful half of the story: leaching is concentrated in the
# autumn-to-spring drainage season, and a management or microclimate change is
# only meaningful if it acts during that window. A scenario that reduces summer
# leaching by a large relative amount has changed almost nothing in absolute
# terms.
#
# This figure resolves the annual number into its seasonal composition, and -
# panel (c) - decomposes the scenario EFFECT by month, which is what identifies
# the mechanism. A radiation-driven reduction that appears in October-December
# is a drainage-volume effect; one that appears in April-June is an uptake
# effect.
#
# AGROHYDROLOGICAL YEAR. Months are ordered April -> March, matching the 1 April
# checkpoint that defines the annual totals in Figure 5. Using calendar-year
# ordering here would split the single autumn-to-spring drainage season across
# the two ends of the axis and make the central pattern invisible.
#
# DEPTH BASIS - stated here because it changes how the figure may be quoted.
# The daily file turns out to be PROFILE-BOTTOM leaching, not the 100 cm basis
# behind Figure 5 (established in validation/check_fn_depth_basis.R; see the
# long note in read_daily_n()). Its annual totals run 1-18% below Figure 5's,
# so the absolute kg N/ha values here are NOT interchangeable with Figure 5's
# and the caption says so. The seasonal distribution - the whole point of the
# figure - is unaffected by the depth basis.
# ===========================================================================

s13_scenarios <- c("Reference", "Rad 0%", "Rad -30%", "Tmp -3degC", "Tmp +3degC")

# April-to-March ordering; see the agrohydrological-year note above.
agrohydrological_month_levels <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                    "Oct", "Nov", "Dec", "Jan", "Feb", "Mar")

prepare_fig_s13_data <- function(daily_n, start_year = ANALYSIS_START_YEAR) {
  monthly <- daily_n |>
    add_weather_scenario_label() |>
    dplyr::filter(scen_label %in% s13_scenarios) |>
    dplyr::mutate(
      rotation = paste("Rotation", stringr::str_match(RotationManagement, "^Rotation([0-9])")[, 2]),
      # Assign each month to the agrohydrological year it drains into: Jan-Mar
      # belong to the year that started the previous April.
      agro_year = dplyr::if_else(month >= 4L, as.integer(year), as.integer(year) - 1L),
      month_label = factor(month.abb[month], levels = agrohydrological_month_levels)
    ) |>
    dplyr::filter(agro_year >= start_year) |>
    dplyr::group_by(scen_label, rotation, agro_year) |>
    # The run ends 2025-01-20, so the final agrohydrological year holds no
    # February or March and only part of January. Keeping it would pull the
    # late-winter monthly means down by averaging real months against absent
    # ones - precisely the months this figure is about.
    dplyr::filter(dplyr::n() >= 365) |>
    dplyr::group_by(scen_label, rotation, agro_year, month_label) |>
    dplyr::summarise(monthly_leaching_kgN_ha = sum(leaching_kgN_ha, na.rm = TRUE),
                     .groups = "drop")

  # Same three-step aggregation as everywhere else: within-period sum -> mean
  # across years within a rotation -> mean +/- SD across the four rotations.
  monthly |>
    dplyr::group_by(scen_label, rotation, month_label) |>
    dplyr::summarise(rotation_mean = mean(monthly_leaching_kgN_ha), .groups = "drop") |>
    dplyr::group_by(scen_label, month_label) |>
    dplyr::summarise(
      mean_leaching_kgN_ha = mean(rotation_mean),
      sd_leaching_kgN_ha = sd(rotation_mean),
      n_rotations = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(scen_label) |>
    dplyr::mutate(
      annual_total_kgN_ha = sum(mean_leaching_kgN_ha),
      share_pct = 100 * mean_leaching_kgN_ha / annual_total_kgN_ha,
      cumulative_share_pct = cumsum(share_pct)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(scen_label = factor(scen_label, levels = s13_scenarios))
}

# Monthly scenario effect: scenario minus open field, in absolute kg N/ha and as
# a share of the open-field annual total. The second is the one that matters -
# it says how much of the whole-year reduction each month contributed.
prepare_fig_s13_delta <- function(fig_s13_data) {
  reference <- fig_s13_data |>
    dplyr::filter(scen_label == "Reference") |>
    dplyr::select(month_label, ref_kgN_ha = mean_leaching_kgN_ha,
                  ref_annual = annual_total_kgN_ha)

  fig_s13_data |>
    dplyr::filter(scen_label != "Reference") |>
    dplyr::left_join(reference, by = "month_label") |>
    dplyr::mutate(
      delta_kgN_ha = mean_leaching_kgN_ha - ref_kgN_ha,
      delta_pct_of_annual = 100 * delta_kgN_ha / ref_annual,
      scen_label = factor(as.character(scen_label),
                          levels = setdiff(s13_scenarios, "Reference"))
    )
}

# Reconcile the daily file against BOTH annual tables, year by year.
#
# This is a real integrity check, not a formality: it is what established that
# out_Daily-FN.csv is the profile-bottom quantity rather than the 100 cm one,
# and that the annual tables are stamped with the CLOSING April checkpoint. If
# a future run changes either convention, this target fails loudly instead of
# silently shifting every seasonal figure by a year or a depth.
#
# The comparison is done per year and per rotation, not on 27-year means: two
# series with different year alignment can have near-identical means while
# disagreeing completely year by year, which is exactly what the 100 cm
# comparison looked like before the offset was found.
check_daily_vs_annual_leaching <- function(daily_n, field_n_apr_profile, n_annual,
                                            start_year = ANALYSIS_START_YEAR) {
  daily_totals <- daily_n |>
    add_weather_scenario_label() |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::mutate(
      rotation = paste("Rotation", stringr::str_match(RotationManagement, "^Rotation([0-9])")[, 2]),
      agro_year = dplyr::if_else(month >= 4L, as.integer(year), as.integer(year) - 1L)
    ) |>
    dplyr::filter(agro_year >= start_year) |>
    dplyr::group_by(scen_label, rotation, agro_year) |>
    dplyr::summarise(daily_kgN_ha = sum(leaching_kgN_ha, na.rm = TRUE),
                     n_days = dplyr::n(), .groups = "drop") |>
    # The run ends 2025-01-20, so the final agrohydrological year is truncated
    # and cannot be compared against a full annual record.
    dplyr::filter(n_days >= 365)

  # +1 on the year: the annual table stamped Y covers April Y-1 to April Y.
  profile_annual <- field_n_apr_profile |>
    dplyr::filter(fertilisation == "Biogas digestate", residue == "Residue Removed",
                  rotation %in% paste("Rotation", 1:4)) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::mutate(agro_year = suppressWarnings(as.integer(year)) - 1L) |>
    dplyr::select(scen_label, rotation, agro_year,
                  profile_table_kgN_ha = leaching_kgN_ha)

  cm100_annual <- n_annual |>
    dplyr::filter(fertilisation == "Biogas digestate", residue == "Residue Removed",
                  rotation %in% paste("Rotation", 1:4)) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::mutate(agro_year = suppressWarnings(as.integer(year)) - 1L) |>
    dplyr::select(scen_label, rotation, agro_year,
                  cm100_table_kgN_ha = leaching_kgN_ha)

  daily_totals |>
    dplyr::inner_join(profile_annual, by = c("scen_label", "rotation", "agro_year")) |>
    dplyr::left_join(cm100_annual, by = c("scen_label", "rotation", "agro_year")) |>
    dplyr::group_by(scen_label) |>
    dplyr::summarise(
      n_years = dplyr::n(),
      daily_mean = mean(daily_kgN_ha),
      profile_mean = mean(profile_table_kgN_ha),
      cm100_mean = mean(cm100_table_kgN_ha, na.rm = TRUE),
      # Agreement with the profile-bottom table, which is the claim being made.
      corr_vs_profile = stats::cor(daily_kgN_ha, profile_table_kgN_ha),
      max_abs_diff_vs_profile = max(abs(daily_kgN_ha - profile_table_kgN_ha)),
      # Retained for contrast: this is the basis Figure 5 uses, and it differs.
      corr_vs_100cm = stats::cor(daily_kgN_ha, cm100_table_kgN_ha),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      basis_confirmed = corr_vs_profile > 0.99 & max_abs_diff_vs_profile < 1,
      pct_below_100cm = 100 * (daily_mean - cm100_mean) / cm100_mean
    ) |>
    dplyr::arrange(scen_label)
}

# The open-field reference belongs to no driver gradient but anchors each of
# them, so duplicate it into every driver facet (same approach as Figure 2a).
add_reference_to_each_driver <- function(df, drivers) {
  reference_rows <- df |>
    dplyr::filter(driver == "Reference") |>
    dplyr::select(-driver) |>
    tidyr::crossing(driver = drivers)
  dplyr::bind_rows(df |> dplyr::filter(driver %in% drivers), reference_rows)
}

# ===========================================================================
# Supplementary Figure S24 - seasonal nitrate-leaching dynamics with crop
# calendar and residual-N crop-phase attribution.
# ===========================================================================
# Structural counterpart to Fig. S14 (continuous SOC + crop calendar): same
# full-period/zoomed-rotation-cycle layout, same Rotation 1 / Dig-Rem basis,
# same scenario set (s14_scenarios, R/wind_diagnostics.R), so the two read
# side by side. Two things differ because the underlying quantity forces it,
# not by choice - both are why this is its own figure rather than a rebuild
# of Fig. S13 or S14:
#
# (1) LEACHING IS AN ANNUAL FLOW, NOT A CONTINUOUS STOCK. Panel (a) is
#     therefore one point per agrohydrological year, not a continuous line -
#     a cumulative 27-year curve would be mathematically valid but would erase
#     the within-year pulse structure that is the point of this figure.
#
# (2) THE AGROHYDROLOGICAL YEAR IS 1 APRIL - 31 MARCH, NOT 1 SEPTEMBER -
#     31 AUGUST. This was proposed during figure design, by analogy with the
#     1 Sep crop-year used for SOC (Fig. S16). It was not adopted: 1 April is
#     the checkpoint DAISY itself reports the N balance on
#     (out_Annual-FN_100cm_Apr.csv - see this file's header note), and Fig. 5,
#     S12, S13 and S18 are all already built on it. Introducing a second,
#     1-September N-year here would make this figure's annual totals
#     unreconcilable with every other N figure in the manuscript for no
#     mechanistic gain - the SOC crop-year and the N agrohydrological year
#     answer different questions (a snapshot difference vs a flux integrated
#     over the drainage season) and were never the same convention.
#
# PRECEDING-CROP ATTRIBUTION (panel b only). Nitrate below the root zone in
# autumn/winter reflects residual soil N, residue mineralisation and legacy
# fixation from whichever crop MOST RECENTLY occupied the field - not
# necessarily the crop already establishing by the time drainage begins.
# prepare_crop_calendar() records standing-crop presence only, split at
# calendar-year boundaries for multi-year leys (a 3-year grass-clover ley is
# three separate rows), so it cannot be read directly for this.
# attribute_drainage_windows() below (a) merges those calendar-year fragments
# back into continuous growing phases, then (b) assigns each agrohydrological
# drainage window (1 Oct - 31 Mar, the window Fig. S13 established holds
# 63-71% of annual leaching) to whichever phase had most recently BEGUN as of
# 1 October - the phase that was either still standing or most recently
# harvested, which in both cases is the defensible residual-N source.
#
# WATER FLUX PAIRED WITH LEACHING IN PANEL (b) - percolation, not drain flow.
# DAISY reports these as distinct, non-additive pathways (Fig. S10), and
# Matrix/Biopore-Leaching_kg_N_ha (n_flux_definitions, above) is carried on
# the matrix+biopore PERCOLATION flux, not on drain flow. Confirmed
# empirically as well as by naming: Matrix_drain_flow_mm and
# Biopore_drain_flow_mm are identically zero in every row of
# field_water_daily, exactly as Fig. S10's caption already states - pairing
# leaching with drain flow would plot a flat zero line.
# ===========================================================================

s24_zoom_years <- 1998:2004

prepare_fig_s24_a_data <- function(n_annual, rotation = "Rotation 1",
                                    fert = "Biogas digestate",
                                    residue_policy = "Residue Removed",
                                    start_year = ANALYSIS_START_YEAR) {
  n_annual |>
    dplyr::filter(rotation == !!rotation, fertilisation == fert,
                  residue == residue_policy, year >= start_year) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(scen_label %in% s14_scenarios) |>
    dplyr::mutate(family = classify_scenario_family(scen_label)) |>
    dplyr::filter(!is.na(family)) |>
    dplyr::select(year, scen_label, family, leaching_kgN_ha) |>
    dplyr::arrange(scen_label, year)
}

prepare_fig_s24_b_data <- function(daily_n, rotation_management = "Rotation1DigRem",
                                    zoom_years = s24_zoom_years) {
  daily_n |>
    dplyr::filter(RotationManagement == rotation_management) |>
    add_weather_scenario_label() |>
    dplyr::filter(scen_label %in% s14_scenarios) |>
    dplyr::mutate(month_date = lubridate::floor_date(date, "month")) |>
    dplyr::filter(lubridate::year(month_date) %in% zoom_years) |>
    dplyr::group_by(scen_label, month_date) |>
    dplyr::summarise(monthly_leaching_kgN_ha = sum(leaching_kgN_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(family = classify_scenario_family(scen_label)) |>
    dplyr::filter(!is.na(family))
}

# Open-field percolation only (not per-scenario): this is background
# mechanistic context for WHEN water leaves the profile, not a second
# scenario comparison - the leaching lines already carry that comparison.
prepare_fig_s24_percolation <- function(field_water_daily, rotation_management = "Rotation1DigRem",
                                         zoom_years = s24_zoom_years) {
  field_water_daily |>
    dplyr::filter(RotationManagement == rotation_management) |>
    add_weather_scenario_label() |>
    dplyr::filter(scen_label == "Reference") |>
    dplyr::mutate(
      percolation_mm = Matrix_percolation_mm + Biopore_percolation_mm,
      month_date = lubridate::floor_date(Date, "month")
    ) |>
    dplyr::filter(lubridate::year(month_date) %in% zoom_years) |>
    dplyr::group_by(month_date) |>
    dplyr::summarise(monthly_percolation_mm = sum(percolation_mm, na.rm = TRUE), .groups = "drop")
}

# Merge calendar-year-fragmented crop_calendar rows for the same crop back
# into continuous growing phases (see the figure header note above). A short
# gap threshold (5 d) absorbs the Dec-31-to-Jan-1 split from year-grouping
# without merging genuinely separate visits of the same crop a rotation cycle
# apart.
merge_crop_phases <- function(crop_calendar, max_gap_days = 5L) {
  crop_calendar |>
    dplyr::arrange(crop_renamed, start_date) |>
    dplyr::group_by(crop_renamed) |>
    dplyr::mutate(
      gap_days = as.integer(start_date - dplyr::lag(end_date)),
      new_phase = dplyr::coalesce(gap_days > max_gap_days, TRUE)
    ) |>
    dplyr::mutate(phase_id = cumsum(new_phase)) |>
    dplyr::group_by(crop_renamed, phase_id) |>
    dplyr::summarise(start_date = min(start_date), end_date = max(end_date), .groups = "drop") |>
    dplyr::select(-phase_id) |>
    dplyr::arrange(start_date)
}

# One row per agrohydrological drainage window (1 Oct - 31 Mar), labelled with
# the crop phase that most recently BEGAN as of 1 October - i.e. whichever
# phase was standing, or most recently harvested, when drainage starts. See
# the PRECEDING-CROP ATTRIBUTION note above for why this is the defensible
# residual-N source, rather than the crop standing once drainage is already
# under way.
attribute_drainage_windows <- function(crop_calendar, years) {
  phases <- merge_crop_phases(crop_calendar)
  purrr::map_dfr(years, function(y) {
    window_start <- as.Date(sprintf("%04d-10-01", y))
    window_end <- as.Date(sprintf("%04d-03-31", y + 1L))
    preceding <- phases |>
      dplyr::filter(start_date <= window_start) |>
      dplyr::slice_max(start_date, n = 1, with_ties = FALSE)
    if (nrow(preceding) == 0) return(NULL)
    tibble::tibble(window_start = window_start, window_end = window_end,
                   preceding_crop = preceding$crop_renamed[1])
  }) |>
    dplyr::mutate(preceding_crop = factor(as.character(preceding_crop), levels = crop_levels_all))
}

prepare_fig_s24_windows_data <- function(crop_calendar, start_year = ANALYSIS_START_YEAR,
                                          end_year = 2024L) {
  attribute_drainage_windows(crop_calendar, years = start_year:end_year)
}

# ===========================================================================
# Supplementary Figure S25 - the nitrogen-supply mechanism behind the
# radiation ladder's leaching response, and why it has the OPPOSITE SIGN from
# the real substrip (West/East) shading pattern.
# ===========================================================================
# Motivating puzzle (raised directly against Fig. S24): the radiation
# SENSITIVITY LADDER (Rad 0/-5/-10/-20/-30%, a uniform, season-long cut
# applied at the Centre strip - the scenario axis behind Fig. 5/S24) shows
# LESS N leaching as the cut deepens. That looks backwards against the
# textbook chain (less biomass -> less uptake -> more residual N -> more
# leaching) and against this project's own field data, where the more-shaded
# West/East substrips leach MORE than Centre, not less.
#
# Both halves are real, verified directly against n_flux_summary/n_annual
# (2026-09-21), not assumed from the ladder alone:
#
#   LADDER, Rad 0% -> Rad -30% (Centre, Dig-Rem, 4-rotation/27-yr mean):
#     fixation        131 -> 43  kgN/ha  (-67%)
#     mineralisation  221 -> 177 kgN/ha  (-20%)
#     crop N uptake   239 -> 190 kgN/ha  (-21%)
#     N leaching     25.3 -> 21.1 kgN/ha (-17%)
#
#   SUBSTRIP 0-level, same basis, ordered by shading severity (Fig. S3j-k:
#   West -29% cumulative irradiance is the most-shaded position, Centre -18%
#   the least): leaching is WEST 26.8 > EAST 25.5 > CENTRE 25.3 kgN/ha - the
#   FIELD-CONSISTENT direction (more shade, more leaching), the opposite
#   ordering from the Centre-only ladder.
#
# THE MECHANISM (panel a): fixation is far more radiation-sensitive than crop
# N uptake in this model - grass-clover and soybean's biological fixation
# collapses about 3x faster, in relative terms, than whole-rotation N demand
# does. Under the ladder's deep, season-long cut, N SUPPLY (fixation +
# mineralisation) falls faster than N DEMAND (uptake), so the residual mineral
# N available to leach actually shrinks despite lower uptake - a supply-side
# effect dominating what looks, from the textbook chain alone, like it should
# be demand-side. Panel (a) shows this seasonally: fixation and mineralisation
# pull down together with uptake, not against it.
#
# THE SIGN FLIP (panel b): the real substrip shading is much shallower (-18 to
# -29% irradiance, vs the ladder's -30% applied uniformly through the whole
# growing season) and evidently does not suppress fixation by enough to
# overturn the textbook demand-side direction. Panel (b) puts the ladder and
# the real substrip response for the same four terms side by side, on the
# same % axis, so the sign flip on leaching (and the much smaller relative
# move in fixation) is a direct visual comparison, not an assertion.
#
# What this figure does NOT claim: WHY the real substrip's seasonal/diurnal
# shading profile suppresses fixation proportionally less than the uniform
# ladder does is not established here - that would need the shading time
# profile itself, which is upstream of this pipeline. What IS established
# from the model's own output is THAT it does, and that fixation is the
# fastest-moving term in the ladder's budget.
# ===========================================================================

s25_seasonal_scenarios <- c("Reference", "Rad 0%", "Rad -10%", "Rad -30%")

s25_flux_levels <- c("Biological N fixation", "Mineralisation", "Crop N uptake", "N leaching")

# Panel (a): monthly N-budget dynamics, one rotation cycle, radiation only -
# the seasonal counterpart to the annual means in n_flux_summary/Fig. 5a.
prepare_fig_s25_a_data <- function(daily_n, rotation_management = "Rotation1DigRem",
                                    zoom_years = s24_zoom_years,
                                    scenarios = s25_seasonal_scenarios) {
  daily_n |>
    dplyr::filter(RotationManagement == rotation_management) |>
    add_weather_scenario_label() |>
    dplyr::filter(scen_label %in% scenarios) |>
    dplyr::mutate(month_date = lubridate::floor_date(date, "month")) |>
    dplyr::filter(lubridate::year(month_date) %in% zoom_years) |>
    dplyr::group_by(scen_label, month_date) |>
    dplyr::summarise(
      `Biological N fixation` = sum(fixation_kgN_ha, na.rm = TRUE),
      `Mineralisation` = sum(mineralisation_kgN_ha, na.rm = TRUE),
      `Crop N uptake` = sum(crop_uptake_kgN_ha, na.rm = TRUE),
      `N leaching` = sum(leaching_kgN_ha, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(-c(scen_label, month_date), names_to = "flux", values_to = "monthly_kgN_ha") |>
    dplyr::mutate(
      flux = factor(flux, levels = s25_flux_levels),
      scen_label = factor(scen_label, levels = scenarios)
    )
}

# Panel (b), ladder half: % change from Rad 0% for the same four terms across
# the radiation ladder - reframes n_flux_summary (already built for Fig. 5a)
# as a relative-change comparison, which is what makes fixation's much
# steeper decline visible against uptake's.
prepare_fig_s25_b_data <- function(n_flux_summary,
                                    fert = "Biogas digestate", residue_policy = "Residue Removed",
                                    ladder = c("Rad -5%", "Rad -10%", "Rad -20%", "Rad -30%")) {
  d <- n_flux_summary |>
    dplyr::filter(management_label == paste(fert, residue_policy, sep = " | "),
                  flux %in% s25_flux_levels)
  baseline <- d |>
    dplyr::filter(scen_label == "Rad 0%") |>
    dplyr::select(flux, baseline_kgN_ha = mean_kgN_ha)
  d |>
    dplyr::filter(scen_label %in% ladder) |>
    dplyr::left_join(baseline, by = "flux") |>
    dplyr::mutate(pct_change = 100 * (mean_kgN_ha - baseline_kgN_ha) / baseline_kgN_ha) |>
    dplyr::transmute(
      condition = as.character(scen_label),
      comparison_group = "Uniform radiation ladder\n(Centre strip)",
      flux = factor(flux, levels = s25_flux_levels),
      pct_change
    )
}

# Panel (b), substrip half: the same four terms and the same %-change
# framing, but for the REAL simulated West/East substrip microclimates
# relative to Centre - the comparison that actually corresponds to the field
# experiment, unlike the uniform ladder.
prepare_fig_s25_c_data <- function(n_annual, rotations = paste("Rotation", 1:4),
                                    fert = "Biogas digestate", residue_policy = "Residue Removed") {
  d <- n_annual |>
    dplyr::filter(rotation %in% rotations, fertilisation == fert, residue == residue_policy) |>
    add_scenario_labels(grid = "ecw") |>
    dplyr::filter(scen_label %in% c("Rad 0% C", "Rad 0% E", "Rad 0% W")) |>
    dplyr::mutate(
      `Biological N fixation` = fixation_kgN_ha,
      `Mineralisation` = mineralisation_kgN_ha,
      `Crop N uptake` = crop_uptake_kgN_ha,
      `N leaching` = leaching_kgN_ha
    ) |>
    tidyr::pivot_longer(dplyr::all_of(s25_flux_levels), names_to = "flux", values_to = "kgN_ha") |>
    dplyr::group_by(scen_label, flux) |>
    dplyr::summarise(mean_kgN_ha = mean(kgN_ha, na.rm = TRUE), .groups = "drop")

  baseline <- d |>
    dplyr::filter(scen_label == "Rad 0% C") |>
    dplyr::select(flux, baseline_kgN_ha = mean_kgN_ha)

  d |>
    dplyr::filter(scen_label != "Rad 0% C") |>
    dplyr::left_join(baseline, by = "flux") |>
    dplyr::mutate(pct_change = 100 * (mean_kgN_ha - baseline_kgN_ha) / baseline_kgN_ha) |>
    dplyr::transmute(
      condition = dplyr::recode(scen_label, "Rad 0% E" = "East", "Rad 0% W" = "West"),
      comparison_group = "Real substrip position\n(0-level, vs Centre)",
      flux = factor(flux, levels = s25_flux_levels),
      pct_change
    )
}

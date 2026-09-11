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
# Pooling across years instead (SD over the pooled rotation-years) is not
# usable here, and the figure shows why: pooling years mixes the SCENARIO
# response with the ROTATION PHASE. Biological N
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

# --- Figure 5 -------------------------------------------------------------
# (a) supply/demand fluxes across radiation and temperature, Dig-Rem
# (b) leaching across the same gradients, for three management regimes
#
# Wind is excluded from both panels: the manuscript's nitrogen section is about
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

prepare_fig_05_b_data <- function(n_flux_summary,
                                   managements = c("Mineral fertiliser | Residue Removed",
                                                    "Biogas digestate | Residue Removed",
                                                    "Biogas digestate | Residue Retained")) {
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

# --- Supplementary Figure S9 ----------------------------------------------
# Main-text Figure 5 shows Dig-Rem only. S9 shows the same fluxes and leaching
# across all management regimes, to demonstrate that the coupled suppression
# under radiation and the asymmetric amplification under warming hold
# regardless of fertiliser source or residue handling - i.e. that management
# shifts the absolute level without changing the directional sensitivity.

prepare_fig_s09_a_data <- function(n_flux_summary,
                                    managements = c("Mineral fertiliser | Residue Removed",
                                                     "Biogas digestate | Residue Removed",
                                                     "Biogas digestate | Residue Retained")) {
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

# The open-field reference belongs to no driver gradient but anchors each of
# them, so duplicate it into every driver facet (same approach as Figure 2a).
add_reference_to_each_driver <- function(df, drivers) {
  reference_rows <- df |>
    dplyr::filter(driver == "Reference") |>
    dplyr::select(-driver) |>
    tidyr::crossing(driver = drivers)
  dplyr::bind_rows(df |> dplyr::filter(driver %in% drivers), reference_rows)
}

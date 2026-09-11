# Soil organic carbon.
#
# Source: out_Annual-OM_*_Sep.csv - annual post-harvest, pre-tillage snapshots
# taken on 1 September (Methods). September, not April: the snapshot has to sit
# after harvest and before tillage, so that the year's residue return is
# included but the mechanical disturbance of the following season is not.
#
# ---------------------------------------------------------------------------
# TOTAL SOC, NOT THE SLOW POOL
# ---------------------------------------------------------------------------
# Total SOC = SOM1-C + SOM2-C + SOM3-C, per Methods and the v12 Figure 7
# caption.
#
# The legacy figure plotted the SLOW pool only (SOM2-C + SOM3-C), which the v12
# draft flags itself as a pending fix. This is not a cosmetic difference: SOM1-C
# is roughly 43% of topsoil organic carbon (31,822 of 73,625 kg C/ha in the
# first record), and it is the pool that responds fastest to residue input and
# temperature, so excluding it both shrinks the stock and damps the very
# response the figure is about.
#
# The pools, for reference:
#   SOM1-C   fast-cycling soil organic matter
#   SOM2-C   intermediate
#   SOM3-C   slow / passive
#   SMB1-C, SMB2-C     microbial biomass       (not part of SOC here)
#   AOM1-C, AOM2-C, AOM3-C   added organic matter, i.e. undecomposed
#                            residue not yet incorporated (not part of SOC here)
# ---------------------------------------------------------------------------

soc_pool_columns <- c("SOM1-C_kg_C_ha", "SOM2-C_kg_C_ha", "SOM3-C_kg_C_ha")

# Depth layers. 0-30 and 60+ are read directly; 30-60 has no file of its own
# and is derived as (0-60) minus (0-30). Deriving it rather than approximating
# it matters because the temperature response reverses sign between topsoil and
# subsoil, so a mis-specified subsoil would invert a reported result.
prepare_soc_by_depth <- function(som_to30_sep, som_to60_sep, som_from60_sep,
                                  start_year = ANALYSIS_START_YEAR) {
  total_soc <- function(df, depth_label) {
    if (is.null(df)) return(NULL)
    df |>
      dplyr::mutate(
        year = suppressWarnings(as.integer(year)),
        total_soc_kgC_ha = num_col(df, "SOM1-C_kg_C_ha") +
          num_col(df, "SOM2-C_kg_C_ha") +
          num_col(df, "SOM3-C_kg_C_ha"),
        # Kept alongside so the slow-pool definition can be compared directly
        # against the total rather than reasoned about.
        slow_soc_kgC_ha = num_col(df, "SOM2-C_kg_C_ha") + num_col(df, "SOM3-C_kg_C_ha"),
        depth = depth_label
      ) |>
      dplyr::select(dplyr::any_of(c(
        "run_id", "soil", "rotation", "fertilisation", "residue",
        "weather_scenario", "weather_factor", "weather_direction",
        "weather_change_pct", "weather_signed_change_pct", "strip_position",
        "weather_is_baseline", "management_label", "year", "depth",
        "total_soc_kgC_ha", "slow_soc_kgC_ha"
      )))
  }

  to30 <- total_soc(som_to30_sep, "0-30 cm")
  to60 <- total_soc(som_to60_sep, "0-60 cm")
  from60 <- total_soc(som_from60_sep, "60+ cm")

  join_keys <- c("run_id", "rotation", "fertilisation", "residue", "weather_scenario", "year")

  # 30-60 cm = (0-60) - (0-30)
  subsoil <- to60 |>
    dplyr::select(dplyr::all_of(join_keys), soc_060 = total_soc_kgC_ha, slow_060 = slow_soc_kgC_ha) |>
    dplyr::inner_join(
      to30 |> dplyr::select(dplyr::all_of(join_keys), soc_030 = total_soc_kgC_ha, slow_030 = slow_soc_kgC_ha),
      by = join_keys
    ) |>
    dplyr::mutate(total_soc_kgC_ha = soc_060 - soc_030,
                  slow_soc_kgC_ha = slow_060 - slow_030,
                  depth = "30-60 cm") |>
    dplyr::select(dplyr::all_of(join_keys), depth, total_soc_kgC_ha, slow_soc_kgC_ha) |>
    dplyr::left_join(
      to30 |> dplyr::select(dplyr::all_of(join_keys), dplyr::any_of(c(
        "soil", "weather_factor", "weather_direction", "weather_change_pct",
        "weather_signed_change_pct", "strip_position", "weather_is_baseline",
        "management_label"
      ))),
      by = join_keys
    )

  dplyr::bind_rows(to30, subsoil, from60) |>
    dplyr::filter(year >= start_year) |>
    dplyr::mutate(depth = factor(depth, levels = c("0-30 cm", "30-60 cm", "0-60 cm", "60+ cm")))
}

# dSOC (%) = (scenario - management-matched open-field baseline) / baseline x 100
#
# "Management-matched" is load-bearing: fertilisation and residue management
# change the absolute carbon stock substantially, so a scenario must be compared
# against the open-field run under the SAME management, in the SAME rotation and
# year. Comparing against a single global baseline would fold the management
# effect into the scenario effect.
compute_soc_relative_change <- function(soc_by_depth) {
  baseline <- soc_by_depth |>
    dplyr::filter(weather_is_baseline) |>
    dplyr::select(rotation, fertilisation, residue, year, depth,
                  baseline_soc_kgC_ha = total_soc_kgC_ha,
                  baseline_slow_kgC_ha = slow_soc_kgC_ha)

  soc_by_depth |>
    dplyr::filter(!weather_is_baseline) |>
    dplyr::left_join(baseline, by = c("rotation", "fertilisation", "residue", "year", "depth")) |>
    dplyr::filter(!is.na(baseline_soc_kgC_ha), baseline_soc_kgC_ha > 0) |>
    dplyr::mutate(
      delta_soc_pct = 100 * (total_soc_kgC_ha - baseline_soc_kgC_ha) / baseline_soc_kgC_ha,
      delta_slow_pct = 100 * (slow_soc_kgC_ha - baseline_slow_kgC_ha) / baseline_slow_kgC_ha
    )
}

# --- Figure 6 -------------------------------------------------------------
# (a) dSOC trajectories 1998-2024, topsoil and subsoil
# (b) end-of-simulation (2024) dSOC by perturbation level and management
#
# Centre strip only. The West/East substrip columns present in the legacy
# figure are dropped: the Results text discusses depth x driver x management
# and never a substrip SOC contrast, and a reviewer specifically asked for
# undiscussed columns to be removed rather than shown.

fig_06_depths <- c("0-30 cm", "30-60 cm")

# Scenario levels shown as trajectories in panel (a): the VAPV 0-level and the
# strongest perturbation of each driver. Enough to show that temperature moves
# SOC in both directions while radiation only ever removes it, without drawing
# 20 overlapping lines.
fig_06_trajectory_scenarios <- c("Rad 0%", "Rad -30%", "Tmp -3degC", "Tmp +3degC")

prepare_fig_06_a_data <- function(soc_relative_change,
                                   fert = "Biogas digestate", residue_policy = "Residue Removed",
                                   rotations = paste("Rotation", 1:4)) {
  soc_relative_change |>
    dplyr::filter(fertilisation == fert, residue == residue_policy,
                  rotation %in% rotations, depth %in% fig_06_depths) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(scen_label %in% fig_06_trajectory_scenarios) |>
    dplyr::group_by(depth, scen_label, year) |>
    dplyr::summarise(
      mean_delta_soc_pct = mean(delta_soc_pct, na.rm = TRUE),
      sd_delta_soc_pct = sd(delta_soc_pct, na.rm = TRUE),
      n_rotations = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      driver = factor(driver_of_scen_label(scen_label), levels = c("Radiation", "Temperature")),
      scen_label = factor(scen_label, levels = fig_06_trajectory_scenarios),
      depth = factor(depth, levels = fig_06_depths)
    ) |>
    dplyr::filter(!is.na(driver))
}

# --- Supplementary Figure S11 ---------------------------------------------
# Total SOC by depth layer.
#
# Main-text Figure 6 shows only 0-30 and 30-60 cm, and only as relative change.
# S11 adds the two things it omits: the deeper layer (60+ cm), and the ABSOLUTE
# stocks. Both matter for interpretation - a −3% change on a 74 t C/ha topsoil
# is a very different quantity of carbon from −3% on a 34 t C/ha subsoil, and
# the relative-only view in Figure 6 cannot show that.
s11_depths <- c("0-30 cm", "30-60 cm", "60+ cm")

prepare_fig_s11_a_data <- function(soc_by_depth,
                                    fert = "Biogas digestate", residue_policy = "Residue Removed",
                                    rotations = paste("Rotation", 1:4)) {
  soc_by_depth |>
    dplyr::filter(fertilisation == fert, residue == residue_policy,
                  rotation %in% rotations, depth %in% s11_depths) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(scen_label %in% c("Reference", fig_06_trajectory_scenarios)) |>
    dplyr::group_by(depth, scen_label, year) |>
    dplyr::summarise(
      mean_soc_tC_ha = mean(total_soc_kgC_ha, na.rm = TRUE) / 1000,
      sd_soc_tC_ha = sd(total_soc_kgC_ha, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      depth = factor(depth, levels = s11_depths),
      scen_label = factor(scen_label, levels = c("Reference", fig_06_trajectory_scenarios))
    )
}

prepare_fig_s11_b_data <- function(soc_relative_change, end_year = 2024L,
                                    rotations = paste("Rotation", 1:4)) {
  soc_relative_change |>
    dplyr::filter(year == end_year, rotation %in% rotations, depth %in% s11_depths) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label), scen_label != "Reference") |>
    dplyr::group_by(depth, management_label, scen_label) |>
    dplyr::summarise(
      mean_delta_soc_pct = mean(delta_soc_pct, na.rm = TRUE),
      sd_delta_soc_pct = sd(delta_soc_pct, na.rm = TRUE),
      mean_absolute_change_tC_ha = mean(total_soc_kgC_ha - baseline_soc_kgC_ha, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      driver = factor(driver_of_scen_label(scen_label), levels = c("Radiation", "Temperature")),
      scen_label = factor(scen_label, levels = scen_order_center),
      depth = factor(depth, levels = s11_depths),
      management_label = factor(management_label, levels = management_levels)
    ) |>
    dplyr::filter(!is.na(driver), !is.na(scen_label))
}

prepare_fig_06_b_data <- function(soc_relative_change,
                                   end_year = 2024L,
                                   rotations = paste("Rotation", 1:4)) {
  soc_relative_change |>
    dplyr::filter(year == end_year, rotation %in% rotations, depth %in% fig_06_depths) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(!is.na(scen_label), scen_label != "Reference") |>
    dplyr::group_by(depth, management_label, scen_label) |>
    dplyr::summarise(
      mean_delta_soc_pct = mean(delta_soc_pct, na.rm = TRUE),
      sd_delta_soc_pct = sd(delta_soc_pct, na.rm = TRUE),
      n_rotations = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      driver = factor(driver_of_scen_label(scen_label), levels = c("Radiation", "Temperature")),
      scen_label = factor(scen_label, levels = scen_order_center),
      depth = factor(depth, levels = fig_06_depths),
      management_label = factor(management_label, levels = management_levels)
    ) |>
    dplyr::filter(!is.na(driver), !is.na(scen_label))
}

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
        # Kept alongside so the legacy definition can be compared directly
        # rather than reasoned about - see validation/check_fig_06_vs_manuscript.R
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

# --- Supplementary Figure S15 ---------------------------------------------
# Total SOC by depth layer.
#
# Main-text Figure 6 shows only 0-30 and 30-60 cm, and only as relative change.
# S15 adds the two things it omits: the deeper layer (60+ cm), and the ABSOLUTE
# stocks. Both matter for interpretation - a −3% change on a 74 t C/ha topsoil
# is a very different quantity of carbon from −3% on a 34 t C/ha subsoil, and
# the relative-only view in Figure 6 cannot show that.
s15_depths <- c("0-30 cm", "30-60 cm", "60+ cm")

prepare_fig_s15_a_data <- function(soc_by_depth,
                                    fert = "Biogas digestate", residue_policy = "Residue Removed",
                                    rotations = paste("Rotation", 1:4)) {
  soc_by_depth |>
    dplyr::filter(fertilisation == fert, residue == residue_policy,
                  rotation %in% rotations, depth %in% s15_depths) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(scen_label %in% c("Reference", fig_06_trajectory_scenarios)) |>
    dplyr::group_by(depth, scen_label, year) |>
    dplyr::summarise(
      mean_soc_tC_ha = mean(total_soc_kgC_ha, na.rm = TRUE) / 1000,
      sd_soc_tC_ha = sd(total_soc_kgC_ha, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      depth = factor(depth, levels = s15_depths),
      scen_label = factor(scen_label, levels = c("Reference", fig_06_trajectory_scenarios))
    )
}

prepare_fig_s15_b_data <- function(soc_relative_change, end_year = 2024L,
                                    rotations = paste("Rotation", 1:4)) {
  soc_relative_change |>
    dplyr::filter(year == end_year, rotation %in% rotations, depth %in% s15_depths) |>
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
      depth = factor(depth, levels = s15_depths),
      management_label = factor(management_label, levels = management_levels)
    ) |>
    dplyr::filter(!is.na(driver), !is.na(scen_label))
}

# ===========================================================================
# Supplementary Figure S21 - how far from equilibrium is the carbon?
# ===========================================================================
# The Discussion states that SOC "is not yet at equilibrium after 26 years" and
# leaves it there. That caveat is doing real work - it is the reason the
# reported dSOC values cannot be read as the long-run carbon consequence of
# vertical agrivoltaics - but as written it is unquantified, so a reader cannot
# tell whether the simulation ended near the endpoint or barely started towards
# it. This figure turns the caveat into a number.
#
# MODEL. First-order approach to a new steady state, which is the standard
# behaviour of a multi-pool decomposition model held under constant management
# and a constant climate perturbation:
#
#     SOC(t) = C_eq + (C_0 - C_eq) * exp(-k * t)
#
# fitted with stats::SSasymp(), which is self-starting and therefore needs no
# hand-supplied initial values that could bias the result. C_eq is the fitted
# equilibrium stock, k the rate constant, and the reported horizon is
#
#     t95 = ln(20) / k        (time to close 95% of the initial-to-equilibrium gap)
#
# HONEST LIMITS OF THIS FIT - read before quoting any t95 value.
# A 27-year window is short relative to the turnover time of the slow pools. If
# the simulated trajectory is still close to linear, the asymptote is only
# weakly identified: many (C_eq, k) pairs fit the observed segment almost
# equally well, and the confidence interval on C_eq is correspondingly wide.
# The extrapolation is therefore reported WITH its interval and WITH an explicit
# identifiability flag, and any fit whose t95 exceeds the simulated period by
# more than a factor given by s21_extrapolation_limit is labelled as not
# determinable from this simulation length rather than being quoted as a result.
# That labelling is itself the finding: it substantiates the Discussion's
# caveat instead of papering over it.
# ===========================================================================

s21_scenarios <- c("Reference", "Rad 0%", "Rad -30%",
                   "Tmp 0degC", "Tmp -3degC", "Tmp +3degC")

s21_depths <- c("0-30 cm", "30-60 cm")

# A fitted horizon more than this many times the simulated record is treated as
# an extrapolation the data cannot support. 5 x 27 years = 135 years: beyond
# that, the fit is being asked to resolve a curvature that 27 annual points
# simply do not contain.
s21_extrapolation_limit <- 5

prepare_fig_s21_data <- function(soc_by_depth,
                                  fert = "Biogas digestate",
                                  residue_policy = "Residue Removed",
                                  rotations = paste("Rotation", 1:4)) {
  soc_by_depth |>
    dplyr::filter(fertilisation == fert, residue == residue_policy,
                  rotation %in% rotations, depth %in% s21_depths) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(scen_label %in% s21_scenarios) |>
    # Mean across the four rotation permutations. The permutations differ only
    # in phase, so averaging them removes the crop-sequence sawtooth that would
    # otherwise be fitted as if it were part of the approach to equilibrium.
    dplyr::group_by(depth, scen_label, year) |>
    dplyr::summarise(
      soc_tC_ha = mean(total_soc_kgC_ha, na.rm = TRUE) / 1000,
      sd_tC_ha = sd(total_soc_kgC_ha, na.rm = TRUE) / 1000,
      n_rotations = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      t = year - min(year),
      depth = factor(depth, levels = s21_depths),
      scen_label = factor(scen_label, levels = s21_scenarios)
    )
}

# One first-order fit per depth x scenario. Returns the fitted equilibrium, the
# rate constant, the 95%-approach horizon, and - crucially - whether the fit is
# identifiable from the record length.
fit_soc_equilibrium <- function(fig_s21_data,
                                 extrapolation_limit = s21_extrapolation_limit) {
  record_years <- max(fig_s21_data$t)

  fig_s21_data |>
    dplyr::group_by(depth, scen_label) |>
    dplyr::group_split() |>
    purrr::map_dfr(function(d) {
      base <- tibble::tibble(
        depth = d$depth[1], scen_label = d$scen_label[1],
        soc_start_tC_ha = d$soc_tC_ha[which.min(d$t)],
        soc_end_tC_ha = d$soc_tC_ha[which.max(d$t)],
        record_years = record_years
      )

      fit <- try(stats::nls(soc_tC_ha ~ stats::SSasymp(t, Asym, R0, lrc), data = d),
                 silent = TRUE)

      if (inherits(fit, "try-error")) {
        return(dplyr::mutate(
          base, soc_equilibrium_tC_ha = NA_real_, eq_lower = NA_real_,
          eq_upper = NA_real_, k_per_yr = NA_real_, t95_years = NA_real_,
          status = "no convergence"
        ))
      }

      co <- stats::coef(fit)
      k <- exp(unname(co[["lrc"]]))
      t95 <- log(20) / k

      ci <- try(suppressWarnings(stats::confint(fit, "Asym", level = 0.95)),
                silent = TRUE)
      eq_lo <- if (inherits(ci, "try-error")) NA_real_ else unname(ci[1])
      eq_hi <- if (inherits(ci, "try-error")) NA_real_ else unname(ci[2])

      dplyr::mutate(
        base,
        soc_equilibrium_tC_ha = unname(co[["Asym"]]),
        eq_lower = eq_lo, eq_upper = eq_hi,
        k_per_yr = k,
        t95_years = t95,
        status = dplyr::if_else(
          t95 > extrapolation_limit * record_years,
          "not identifiable from 27-yr record", "fitted"
        )
      )
    }) |>
    dplyr::mutate(
      # How much of the total initial-to-equilibrium change had actually
      # happened by the end of the simulation. This is the number the
      # Discussion's caveat is really about.
      gap_closed_pct = 100 * (soc_end_tC_ha - soc_start_tC_ha) /
        (soc_equilibrium_tC_ha - soc_start_tC_ha),
      remaining_change_tC_ha = soc_equilibrium_tC_ha - soc_end_tC_ha
    )
}

# ---------------------------------------------------------------------------
# CURVATURE TEST - the robust diagnostic, and in practice the main result.
#
# The asymptotic fit above answers "where does it end up", which turns out to be
# unanswerable from 27 years for most of these trajectories: five of the six
# topsoil series do not converge at all, because a series with no visible
# curvature contains no information about an asymptote.
#
# That failure is itself informative, but only if it is measured rather than
# inferred from a convergence error. This function measures it directly: fit a
# straight line to the first half of the record and another to the second half,
# and compare the slopes.
#
#   ratio ~ 1     still linear - no detectable approach to equilibrium
#   0 < ratio < 1 decelerating - approaching a new steady state
#   ratio ~ 0     effectively arrived
#   ratio > 1     accelerating away
#
# Unlike the asymptotic fit this always returns a value, is not sensitive to
# starting conditions, and makes no assumption that the approach is first-order.
# ---------------------------------------------------------------------------
fit_soc_curvature <- function(fig_s21_data) {
  midpoint <- stats::median(fig_s21_data$year)

  fig_s21_data |>
    dplyr::mutate(half = dplyr::if_else(year <= midpoint, "first", "second")) |>
    dplyr::group_by(depth, scen_label, half) |>
    dplyr::summarise(
      slope = stats::coef(stats::lm(soc_tC_ha ~ year))[["year"]],
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(names_from = half, values_from = slope,
                       names_prefix = "slope_") |>
    dplyr::mutate(
      deceleration_ratio = slope_second / slope_first,
      verdict = dplyr::case_when(
        # A ratio computed from two near-zero slopes is numerically unstable
        # and means nothing; test the absolute rate first.
        abs(slope_first) < 0.02 & abs(slope_second) < 0.02 ~ "effectively flat",
        deceleration_ratio < 0 ~ "reversed direction",
        # ACCELERATING is a separate category and must not be folded into
        # "still linear": a trajectory whose rate is growing is moving AWAY
        # from equilibrium, so the simulated change is a lower bound on the
        # long-run change rather than an approximation of it.
        deceleration_ratio > 1.15 ~ "accelerating",
        deceleration_ratio > 0.85 ~ "still linear",
        deceleration_ratio > 0.3 ~ "decelerating",
        TRUE ~ "near equilibrium"
      )
    )
}

# Long form for the paired first-half/second-half panel.
soc_curvature_long <- function(soc_curvature) {
  soc_curvature |>
    tidyr::pivot_longer(c(slope_first, slope_second),
                        names_to = "half", values_to = "slope") |>
    dplyr::mutate(half = factor(
      dplyr::recode(half, slope_first = "1998–2011", slope_second = "2012–2024"),
      levels = c("1998–2011", "2012–2024")
    ))
}

# Fitted curves projected forward, for the panel (a) overlay. Only fits marked
# "fitted" are projected: drawing a 400-year curve through an unidentifiable
# asymptote would assert precision the data do not contain.
# horizon_years = 50, not the fitted t95 (145-328 years). Drawing the curves out
# to their asymptote would compress the 26 years of ACTUAL simulated data into
# a tenth of the panel, hiding the very trajectories the figure is about. Fifty
# years is enough to show the curvature; the full horizons are quantified in
# panel (c), which is the right place for a number that large.
project_soc_equilibrium <- function(soc_equilibrium_fits, horizon_years = 50) {
  # Every fit that CONVERGED is projected, including those whose horizon
  # exceeds the identifiability limit - those are drawn dashed and captioned as
  # extrapolations. Series that did not converge get no curve at all, which is
  # the honest visual statement: there is no curvature in them to project.
  fits <- dplyr::filter(soc_equilibrium_fits, !is.na(k_per_yr))
  if (nrow(fits) == 0) {
    return(tibble::tibble(depth = character(), scen_label = character(),
                          t = numeric(), soc_tC_ha = numeric(), year = integer()))
  }

  # depth/scen_label are carried as character through pmap and re-levelled at
  # the end: iterating a factor column element-wise does not preserve its
  # levels, and the projection has to share an axis with fig_s21_data.
  purrr::pmap_dfr(
    list(as.character(fits$depth), as.character(fits$scen_label),
         fits$soc_start_tC_ha, fits$soc_equilibrium_tC_ha, fits$k_per_yr),
    function(dep, scen, c0, ceq, k) {
      t <- seq(0, horizon_years, by = 1)
      tibble::tibble(
        depth = dep, scen_label = scen, t = t,
        soc_tC_ha = ceq + (c0 - ceq) * exp(-k * t),
        year = 1998L + t
      )
    }
  ) |>
    dplyr::mutate(
      depth = factor(depth, levels = s21_depths),
      scen_label = factor(scen_label, levels = s21_scenarios)
    )
}

# ===========================================================================
# Supplementary Figure S16 - which crop phase builds or loses the carbon?
# ===========================================================================
# Results 3.2 quotes per-crop-phase SOC accumulation rates (+0.18 / -0.07 for
# the soybean phase, +0.54 / -0.45 Mg C/ha/yr for the winter wheat phase) that
# have no figure behind them anywhere in the manuscript or the legacy script.
# This provides both the evidence and the means to check them.
#
# ATTRIBUTION CONVENTION - stated explicitly because it is a choice, not a fact.
# SOC is snapshotted each 1 September (post-harvest, pre-tillage). The annual
# change attributed to a crop phase is therefore
#
#     dSOC(y) = SOC_Sep(y) - SOC_Sep(y-1)
#
# i.e. the interval running from just after the PREVIOUS crop's harvest, through
# the following autumn and winter, through this crop's whole growing season, to
# just after ITS harvest. That interval contains this crop's full growth and
# residue return, which is what makes it the right attribution window; it also
# contains the previous crop's post-harvest residue decomposition, which is a
# genuine carry-over that no annual accounting can separate. Anyone quoting
# these rates should quote them as "the crop-year ending with crop X", not as
# "caused by crop X".
#
# The phase crop for a rotation-year is the harvested crop with the largest
# above-ground biomass, excluding undersown ryegrass unless it is the only crop
# present - the undersown catch crop coexists with a main crop by design and
# would otherwise capture years that belong to spring barley.
# ===========================================================================

s16_scenarios <- c("Reference", "Rad 0%", "Rad -30%", "Tmp -3degC", "Tmp +3degC")

# Which crop defines each rotation x year phase. Derived from simulated harvest
# records rather than from the rotation definition file, so that it reflects
# what the model actually grew.
assign_rotation_phase_crop <- function(harvest_annual,
                                        rotations = paste("Rotation", 1:4),
                                        start_year = ANALYSIS_START_YEAR) {
  harvest_annual |>
    dplyr::filter(year >= start_year, rotation %in% rotations,
                  weather_is_baseline) |>
    dplyr::group_by(rotation, year, crop_renamed) |>
    dplyr::summarise(agb = sum(harvested_agb_removed_MgDM_ha, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(agb > 0) |>
    dplyr::group_by(rotation, year) |>
    dplyr::mutate(is_undersown = crop_renamed == "Ryegrass (undersown)") |>
    # Rank main crops ahead of the undersown catch crop, then by biomass.
    dplyr::arrange(is_undersown, dplyr::desc(agb), .by_group = TRUE) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(rotation, year, phase_crop = crop_renamed)
}

prepare_fig_s16_data <- function(soc_by_depth, harvest_annual,
                                  fert = "Biogas digestate",
                                  residue_policy = "Residue Removed",
                                  rotations = paste("Rotation", 1:4),
                                  depths = fig_06_depths) {
  phases <- assign_rotation_phase_crop(harvest_annual, rotations = rotations)

  soc_by_depth |>
    dplyr::filter(fertilisation == fert, residue == residue_policy,
                  rotation %in% rotations, depth %in% depths) |>
    add_scenario_labels(grid = "center") |>
    dplyr::filter(scen_label %in% s16_scenarios) |>
    dplyr::arrange(depth, scen_label, rotation, year) |>
    dplyr::group_by(depth, scen_label, rotation) |>
    dplyr::mutate(
      d_soc_MgC_ha_yr = (total_soc_kgC_ha - dplyr::lag(total_soc_kgC_ha)) / 1000,
      year_gap = year - dplyr::lag(year)
    ) |>
    dplyr::ungroup() |>
    # A gap other than one year means a missing snapshot; differencing across it
    # would silently report a multi-year change as an annual rate.
    dplyr::filter(!is.na(d_soc_MgC_ha_yr), year_gap == 1) |>
    dplyr::inner_join(phases, by = c("rotation", "year")) |>
    dplyr::group_by(depth, scen_label, phase_crop) |>
    dplyr::summarise(
      mean_d_soc_MgC_ha_yr = mean(d_soc_MgC_ha_yr),
      sd_d_soc_MgC_ha_yr = sd(d_soc_MgC_ha_yr),
      n_crop_years = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      phase_crop = factor(phase_crop, levels = crop_levels_all),
      scen_label = factor(scen_label, levels = s16_scenarios),
      depth = factor(depth, levels = depths)
    ) |>
    dplyr::filter(!is.na(phase_crop))
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

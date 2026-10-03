# Substrip microclimate driver construction and validation: Supplementary
# Figure S3 (wind speed and air temperature).
#
# Two independent pieces:
#   1. The Vigiak et al. (2003) double-exponential wind barrier-profile model
#      (pure geometry - no observational data required) and its direction-aware
#      adaptation to the West/Centre/East substrip positions.
#   2. Empirical validation of both wind and temperature against the 2023-2024
#      HOBOnet sensor-nest record (data-dependent).
#
# Ported from D:\Kamau\DAISY_modelling_agrivoltaics\PREP_WIND_TEMP_DATA_FOR_DAISY_SPAWN.Rmd
# (an 8399-line notebook where the wind model is redefined 3x - only the
# final version, at that file's lines 1384-1413 and 1546-1618, is what was
# actually used downstream; the other two are superseded drafts). Geometry
# corrected here to the as-built 11 m row pitch (the source notebook's own
# default is a stale 12 m / substrip offsets 1.5, 6.0, 10.5 m; recomputed to
# 1.5, 5.5, 9.5 m to match Note S2 and the main-text Methods).

# ===========================================================================
# Part 1: Vigiak double-exponential barrier-profile model (data-free)
# ===========================================================================

#' Vigiak et al. (2003) double-exponential wind-barrier reduction profile.
#'
#' @param D Downwind distance from the upwind barrier (m).
#' @param H Barrier (panel) height (m).
#' @param porosity Barrier optical porosity (0-1).
#' @param L1,L2 Near-/far-field raw recovery length scales, scaled by `squeeze`.
#' @param squeeze Compresses L1/L2 together, sharpening the profile without
#'   changing its shape.
#' @return U/U0, clamped to [0.05, 1.2].
ru_vigiak_squeezed <- function(D, H, porosity = 0.4, L1 = 1.2, L2 = 16, squeeze = 0.15) {
  L1_eff <- L1 * squeeze
  L2_eff <- L2 * squeeze
  x <- pmax(D, 0) / H

  Rmin_target <- dplyr::case_when(
    abs(porosity - 0.2) < 1e-6 ~ 0.60,
    abs(porosity - 0.4) < 1e-6 ~ 0.70,
    abs(porosity - 0.6) < 1e-6 ~ 0.80,
    abs(porosity - 0.8) < 1e-6 ~ 0.90,
    TRUE ~ 0.4 + 0.8 * porosity
  )

  x_min <- (L1_eff * L2_eff / (L2_eff - L1_eff)) * log(L2_eff / L1_eff)
  delta <- exp(-x_min / L1_eff) - exp(-x_min / L2_eff)
  a <- (1 - Rmin_target) / delta

  Ru <- 1 - a * (exp(-x / L1_eff) - exp(-x / L2_eff))
  Ru[D <= 0] <- 1.0
  pmax(pmin(Ru, 1.2), 0.05)
}

# Direction-aware adaptation: continuous bearing -> 8 cardinals -> 5 cases ->
# unsigned incidence angle -> downwind distance from the relevant upwind row.
dir8_from_theta <- function(theta_deg) {
  theta_deg <- theta_deg %% 360
  cuts <- c(22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5)
  labs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  idx <- findInterval(theta_deg, cuts)
  c(labs, "N")[idx + 1]
}
case_from_dir8 <- function(dir8) {
  dplyr::case_when(
    dir8 == "W" ~ "W",
    dir8 %in% c("NW", "SW") ~ "NW+SW",
    dir8 == "E" ~ "E",
    dir8 %in% c("NE", "SE") ~ "NE+SE",
    dir8 %in% c("N", "S") ~ "N+S",
    TRUE ~ NA_character_
  )
}
ia_from_case <- function(case5) {
  dplyr::case_when(
    case5 %in% c("W", "E") ~ 0,
    case5 %in% c("NW+SW", "NE+SE") ~ 45,
    case5 == "N+S" ~ 90,
    TRUE ~ NA_real_
  )
}
downwind_distance <- function(location_m, case5, pitch) {
  dplyr::case_when(
    case5 %in% c("E", "NE+SE") ~ pmax(pitch - location_m, 0),
    case5 %in% c("W", "NW+SW") ~ pmax(location_m, 0),
    TRUE ~ 0
  )
}

# As-built VAPV platform geometry (see Note S2 / main-text Methods).
VAPV_PANEL_HEIGHT_M <- 2.9
VAPV_ROW_PITCH_M <- 11
VAPV_POROSITY <- 0.40 / 2.90
VAPV_SUBSTRIP_OFFSET_M <- c(western_strip = 1.5, center_strip = 5.5, eastern_strip = 9.5)

#' Modelled wind-speed ratio (U/U0) at a substrip position for a given wind
#' bearing, using the as-built VAPV geometry.
ru_apv_windspeed <- function(location, theta, H = VAPV_PANEL_HEIGHT_M, pitch = VAPV_ROW_PITCH_M,
                              porosity = VAPV_POROSITY, squeeze = 0.2) {
  loc_map <- VAPV_SUBSTRIP_OFFSET_M
  location_m <- if (is.character(location)) {
    rep(unname(loc_map[location]), length(theta))
  } else {
    rep(as.numeric(location), length(theta))
  }
  dir8 <- dir8_from_theta(theta)
  case5 <- case_from_dir8(dir8)
  IA <- ia_from_case(case5)
  s <- downwind_distance(location_m, case5, pitch)
  Ru_perp <- ru_vigiak_squeezed(s, H, porosity = porosity, squeeze = squeeze)
  mod <- abs(cos(IA * pi / 180))
  Ru <- 1 - (1 - Ru_perp) * mod
  Ru[case5 == "N+S"] <- 1
  as.numeric(Ru)
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S3a-c - theoretical wind-barrier mechanism, with real
# observations overlaid on (a) and (c) - Centre only, since that is the only
# substrip with an actual sensor; West/East stay model-only points/lines,
# shown for context but never validated. Mirrors the original analysis's own
# convention (an "Empirical (Centre)" vs "Modelled (West/East)" legend).
# ===========================================================================
plot_fig_s03_mechanism <- function(wind_val = NULL) {
  H <- VAPV_PANEL_HEIGHT_M
  pitch <- VAPV_ROW_PITCH_M
  beta_apv <- VAPV_POROSITY
  squeeze <- 0.2

  curve_apv <- tibble::tibble(D = seq(-2, pitch + 4, by = 0.05)) |>
    dplyr::mutate(Ru = ru_vigiak_squeezed(D, H, porosity = beta_apv, squeeze = squeeze))

  p_a <- ggplot2::ggplot(curve_apv, ggplot2::aes(x = D, y = Ru)) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey50", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
    ggplot2::geom_line(colour = "#B2182B", linewidth = 0.9)

  if (!is.null(wind_val)) {
    # Perpendicular-ish incidence only (E/W/NE+SE/NW+SW), so a single
    # distance-based curve stays a fair comparison (parallel N/S winds are
    # trivially Ru=1 for both empirical and modelled and would dilute it).
    perp_ish <- wind_val |> dplyr::filter(dir8 %in% c("E", "W", "NE", "SE", "NW", "SW"))
    emp_centre <- mean(perp_ish$Ru_emp, na.rm = TRUE)
    mod_centre <- mean(perp_ish$Ru_model, na.rm = TRUE)
    mod_west <- ru_apv_windspeed("western_strip", perp_ish$Dir_open, H = H, pitch = pitch,
                                   porosity = beta_apv, squeeze = squeeze) |> mean(na.rm = TRUE)
    mod_east <- ru_apv_windspeed("eastern_strip", perp_ish$Dir_open, H = H, pitch = pitch,
                                   porosity = beta_apv, squeeze = squeeze) |> mean(na.rm = TRUE)
    overlay <- tibble::tibble(
      D = VAPV_SUBSTRIP_OFFSET_M[c("center_strip", "center_strip", "western_strip", "eastern_strip")],
      Ru = c(emp_centre, mod_centre, mod_west, mod_east),
      series = factor(c("Observed (Centre)", "Modelled (Centre)", "Modelled (West)", "Modelled (East)"),
                       levels = c("Observed (Centre)", "Modelled (Centre)", "Modelled (West)", "Modelled (East)"))
    )
    p_a <- p_a +
      ggplot2::geom_point(data = overlay, ggplot2::aes(x = D, y = Ru, colour = series, shape = series),
                           size = 2.8, inherit.aes = FALSE) +
      ggplot2::scale_colour_manual(values = c("Observed (Centre)" = "#1B7837", "Modelled (Centre)" = "grey20",
                                                "Modelled (West)" = "#1f78b4", "Modelled (East)" = "#e31a1c"), name = NULL) +
      ggplot2::scale_shape_manual(values = c("Observed (Centre)" = 17, "Modelled (Centre)" = 16,
                                               "Modelled (West)" = 1, "Modelled (East)" = 1), name = NULL) +
      ggplot2::labs(caption = "Points: mean U/U0 for perpendicular/oblique wind (E, W, NE, SE, NW, SW); parallel N/S excluded (Ru=1 by construction).")
  }

  p_a <- p_a +
    ggplot2::scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.2, 0.25)) +
    ggplot2::labs(x = "Distance from upwind PV row (m)", y = expression(U / U[0]),
                  title = expression("Barrier-profile reduction ("*beta*""[APV]*" \u2248 0.14)"),
                  tag = "a") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                   legend.position = "bottom", legend.text = ggplot2::element_text(size = 7),
                   plot.caption = ggplot2::element_text(size = 6.5))

  porosities <- c("APV \u22480.14" = beta_apv, "20%" = 0.2, "40%" = 0.4, "60%" = 0.6, "80%" = 0.8)
  curve_multi <- purrr::imap_dfr(porosities, \(p, lab) {
    tibble::tibble(x_over_H = seq(-2 / H, (pitch + 4) / H, by = 0.02)) |>
      dplyr::mutate(D = x_over_H * H, Ru = ru_vigiak_squeezed(D, H, porosity = p, squeeze = squeeze),
                    porosity_label = lab)
  })
  curve_multi$porosity_label <- factor(curve_multi$porosity_label, levels = names(porosities))

  p_b <- ggplot2::ggplot(curve_multi, ggplot2::aes(x = x_over_H, y = Ru, colour = porosity_label)) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey50", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::scale_colour_manual(values = c("#B2182B", "#FDBE85", "#FD8D3C", "#31A354", "#3182BD"), name = "Porosity") +
    ggplot2::scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.2, 0.25)) +
    ggplot2::labs(x = "Distance from barrier (x / H)", y = expression(U / U[0]),
                  title = "Sensitivity to panel porosity", tag = "b") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))

  theta_seq <- seq(0, 360, by = 1)
  dir_data <- purrr::map_dfr(names(VAPV_SUBSTRIP_OFFSET_M), \(loc) {
    tibble::tibble(theta = theta_seq, location = loc,
                   Ru = ru_apv_windspeed(loc, theta_seq, H = H, pitch = pitch, porosity = beta_apv, squeeze = squeeze))
  })
  dir_data$location <- factor(dir_data$location, levels = names(VAPV_SUBSTRIP_OFFSET_M),
                               labels = c("West", "Centre", "East"))

  p_c <- ggplot2::ggplot()

  if (!is.null(wind_val)) {
    p_c <- p_c +
      ggplot2::geom_point(data = wind_val, ggplot2::aes(x = Dir_open, y = Ru_emp),
                           colour = "grey60", alpha = 0.08, size = 0.5) +
      ggplot2::geom_smooth(data = wind_val, ggplot2::aes(x = Dir_open, y = Ru_emp),
                            colour = "black", linewidth = 0.8, se = FALSE, method = "loess", span = 0.3)
  }

  p_c <- p_c +
    ggplot2::geom_hline(yintercept = 1, colour = "grey50", linewidth = 0.3) +
    ggplot2::geom_line(data = dir_data, ggplot2::aes(x = theta, y = Ru, colour = location), linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = c("West" = "#1f78b4", "Centre" = "#33a02c", "East" = "#e31a1c"), name = "Modelled, by substrip") +
    ggplot2::scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, 45), labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")) +
    ggplot2::scale_y_continuous(limits = c(-0.1, 2.2), breaks = seq(0, 2, 0.5)) +
    ggplot2::labs(x = "Wind direction (bearing)", y = expression(U / U[0]),
                  title = "Direction-aware sheltering by substrip position",
                  subtitle = if (!is.null(wind_val)) "Grey points/black smooth: observed Centre. Coloured lines: modelled, all three substrips." else NULL,
                  tag = "c") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(size = 7.5))

  (p_a + p_b) / p_c + patchwork::plot_layout(heights = c(1, 1))
}

# ===========================================================================
# Part 2: empirical validation against the 2023-2024 HOBOnet sensor-nest
# record (data-dependent - see data/raw/nest_microclimate_validation/)
# ===========================================================================
# NOTE: which of the merged_climate_data.rds nest columns (N1-N6) corresponds
# to which physical position (West/Centre/East/open-field reference), and
# whether that assignment changed between the 2023 and 2024 seasons (the
# nests were mobile), is not recoverable from the data or file names alone -
# confirmed with the data owner (M.K.K. Lindhardt) before writing the
# position-specific prep functions below.
#
# What IS position-agnostic and safe to build first: loading the raw nest
# file, reshaping N1-N6 into long format, and cleaning obvious sensor faults.
# Exploratory check on the raw columns (2026-09-14) found N2 and N4 carry
# extreme fault values on temp_air/wind_speed respectively (means of -320 and
# -214, physically impossible), and N4/N6 have 26-52% missing wind - real
# data-quality issues the original notebook's own outlier routines existed
# to handle, not an artefact of this port.

#' Load the merged HOBOnet nest-ensemble file and reshape N1-N6 into long
#' format, one row per (datetime, nest, variable).
load_nest_data_long <- function(path = here::here("data", "raw", "nest_microclimate_validation",
                                                    "merged_climate_data.rds")) {
  d <- readRDS(path) |> tibble::as_tibble()
  nests <- paste0("N", 1:6)
  vars <- c(wind_speed = "wind_speed", wind_dir = "wind_dir",
            wind_gust = "wind_gust", temp_air = "temp_air")

  purrr::map_dfr(names(vars), \(v) {
    cols <- paste0(vars[[v]], "_", nests)
    cols <- intersect(cols, names(d))
    d |>
      dplyr::select(date_time, dplyr::all_of(cols)) |>
      tidyr::pivot_longer(-date_time, names_to = "nest", values_to = v) |>
      dplyr::mutate(nest = stringr::str_extract(nest, "N[1-6]$"))
  }) |>
    dplyr::group_by(date_time, nest) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), \(x) x[!is.na(x)][1]), .groups = "drop")
}

#' Physically-implausible-value + MAD-based outlier cleaning for one nest's
#' wind speed/gust series. Bounds and k follow the source notebook's
#' clean_wind_data() (speed in [0,30] m/s; MAD k=5).
clean_nest_wind <- function(df, speed_col = "wind_speed", gust_col = "wind_gust", k = 5) {
  remove_outliers <- function(x, k = 5) {
    med <- stats::median(x, na.rm = TRUE)
    mad_val <- stats::mad(x, na.rm = TRUE)
    if (!is.finite(mad_val) || mad_val == 0) return(x)
    dplyr::if_else(abs(x - med) > k * mad_val, NA_real_, x)
  }
  df |>
    dplyr::mutate(
      "{speed_col}" := dplyr::if_else(.data[[speed_col]] < 0 | .data[[speed_col]] > 30,
                                        NA_real_, .data[[speed_col]]),
      "{gust_col}" := dplyr::if_else(.data[[gust_col]] < 0 | .data[[gust_col]] > 40,
                                       NA_real_, .data[[gust_col]]),
      "{speed_col}" := remove_outliers(.data[[speed_col]], k = k)
    )
}

# ---------------------------------------------------------------------------
# Which nests were physically deployed on the Vertical (VAPV) system, vs the
# separate South-Oriented comparison system at the same site - confirmed
# with the data owner from V4F_agrivoltaics/MICROCLIMATE_ANALYSIS_2025.Rmd
# (a repo/file this session cannot read directly). N1/N2 are always
# South-Oriented (never relevant here). N3 and N6 are Vertical throughout;
# N4 and N5 swapped roles on 2026-04-30 (sic in source: date given as
# "2024-04-30", i.e. the site's field-season year - N4 was the third
# Vertical nest before that date, N5 after.
#
# Critically: ALL Vertical-system nests are positioned at the CENTRE
# substrip only. There is no West or East sensor at all - those two
# positions are pure Vigiak-model extrapolations from the measured Centre,
# by design (confirmed with the data owner). Any West/East panel is
# therefore theoretical throughout, never independently validated.
NEST_SWAP_DATE <- as.POSIXct("2024-04-30", tz = "UTC")  # N4 <-> N5 role swap

#' Build the Centre-position empirical wind-speed ensemble: the mean across
#' whichever Vertical-system nests were active at each timestamp (N3+N6
#' always; plus N4 before the swap date, N5 after).
build_centre_wind_ensemble <- function(nest_long_clean) {
  nest_long_clean |>
    dplyr::mutate(
      is_vertical = dplyr::case_when(
        nest %in% c("N3", "N6") ~ TRUE,
        nest == "N4" ~ date_time < NEST_SWAP_DATE,
        nest == "N5" ~ date_time >= NEST_SWAP_DATE,
        TRUE ~ FALSE
      )
    ) |>
    dplyr::filter(is_vertical) |>
    dplyr::group_by(date_time) |>
    dplyr::summarise(
      U_centre_emp = mean(wind_speed, na.rm = TRUE),
      wind_dir_emp = circular_mean_deg(wind_dir),
      n_nests = sum(!is.na(wind_speed)),
      .groups = "drop"
    ) |>
    dplyr::filter(n_nests > 0)
}

#' Build the Centre-position empirical air-temperature ensemble, same nest
#' selection rule as the wind ensemble above.
build_centre_temp_ensemble <- function(nest_long_clean) {
  nest_long_clean |>
    dplyr::mutate(
      is_vertical = dplyr::case_when(
        nest %in% c("N3", "N6") ~ TRUE,
        nest == "N4" ~ date_time < NEST_SWAP_DATE,
        nest == "N5" ~ date_time >= NEST_SWAP_DATE,
        TRUE ~ FALSE
      )
    ) |>
    dplyr::filter(is_vertical, is.finite(temp_air), temp_air > -40, temp_air < 50) |>
    dplyr::group_by(date_time) |>
    dplyr::summarise(T_centre_emp = mean(temp_air, na.rm = TRUE), n_nests = dplyr::n(), .groups = "drop")
}

circular_mean_deg <- function(deg) {
  rad <- deg * pi / 180
  ang <- atan2(mean(sin(rad), na.rm = TRUE), mean(cos(rad), na.rm = TRUE))
  (ang * 180 / pi) %% 360
}

#' Load the on-site open-field reference station (2022-2025), the pairing
#' partner the source notebook used for the nest comparison (co-located and
#' contemporaneous with the nest deployment - distinct from the long-term
#' Foulumgaard DMI station that drives the multi-decade Daisy simulations).
load_openfield_reference <- function(path = here::here("data", "raw", "nest_microclimate_validation",
                                                          "clim_ref_ensemble_imputed_data_22_24.rds")) {
  readRDS(path) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      date_time,
      U_open = avg_wind_velocity_m_s_1_combi_kalman,
      Dir_open = avg_wind_direction_deg_combi_kalman_mice,
      Gust_open = avg_wind_gust_m_s_1_kalman_mice,
      T_open = avg_ambient_temperature_deg_c_combi_kalman,
      GHI_open = avg_ghi_w_m_2_combi_kalman
    )
}

#' Build the hourly wind-validation dataset: empirical Centre/open-field
#' ensemble means, gust-adjusted empirical Ru, and the model's own Ru
#' prediction fed with the empirical open-field wind direction.
#'
#' @param nest_clean Optional pre-built `load_nest_data_long() |>
#'   clean_nest_wind()` result, to avoid redoing that (slow) reshape when
#'   this is called alongside `prepare_temperature_master_data()`.
prepare_wind_validation_data <- function(gust_weight = 0.3, nest_clean = NULL) {
  if (is.null(nest_clean)) nest_clean <- load_nest_data_long() |> clean_nest_wind()
  centre <- build_centre_wind_ensemble(nest_clean)
  openfield <- load_openfield_reference()

  hourly <- \(df, ...) {
    df |>
      dplyr::mutate(hour = lubridate::floor_date(date_time, "hour")) |>
      dplyr::group_by(hour) |>
      dplyr::summarise(..., .groups = "drop")
  }

  centre_h <- hourly(centre,
    U_centre_emp = mean(U_centre_emp, na.rm = TRUE),
    n_nests = mean(n_nests, na.rm = TRUE)
  )
  open_h <- hourly(openfield,
    U_open = mean(U_open, na.rm = TRUE),
    Dir_open = circular_mean_deg(Dir_open),
    Gust_open = mean(Gust_open, na.rm = TRUE)
  )

  dplyr::inner_join(centre_h, open_h, by = "hour") |>
    dplyr::filter(is.finite(U_centre_emp), is.finite(U_open), U_open >= 1) |>
    dplyr::mutate(
      U_open_gustadj = (1 - gust_weight) * U_open + gust_weight * Gust_open,
      Ru_emp = U_centre_emp / U_open_gustadj,
      Ru_model = ru_apv_windspeed("center_strip", Dir_open),
      U_centre_model = U_open_gustadj * Ru_model,
      dir8 = dir8_from_theta(Dir_open)
    ) |>
    dplyr::filter(is.finite(Ru_emp), Ru_emp > 0, Ru_emp < 3)
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S3d-f - empirical wind validation (Centre only; West
# and East have no independent sensor and are not shown here)
# ===========================================================================
plot_fig_s03_wind_validation <- function(wind_val) {
  fit <- stats::lm(U_centre_emp ~ U_centre_model, data = wind_val)
  r2 <- summary(fit)$r.squared
  slope <- stats::coef(fit)[2]

  p_d <- ggplot2::ggplot(wind_val, ggplot2::aes(x = U_centre_model, y = U_centre_emp)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.12, size = 0.6, colour = "#33a02c") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black", linewidth = 0.7) +
    ggplot2::annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
                       label = sprintf("R^2 == %.3f * ',' ~ slope == %.2f", r2, slope), parse = TRUE, size = 3.2) +
    ggplot2::labs(x = expression("Modelled "*U[Centre]*" (m s"^-1*")"),
                  y = expression("Observed "*U[Centre]*" (m s"^-1*")"),
                  title = "Modelled vs observed Centre wind speed", tag = "d") +
    theme_manuscript() + ggplot2::theme(legend.position = "none")

  sector_obs <- wind_val |>
    dplyr::filter(!is.na(dir8)) |>
    dplyr::mutate(dir8 = factor(dir8, levels = c("N","NE","E","SE","S","SW","W","NW")))
  sector_model <- sector_obs |>
    dplyr::distinct(dir8, Ru_model)

  p_e <- ggplot2::ggplot(sector_obs, ggplot2::aes(x = dir8, y = Ru_emp)) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey50", linetype = "dotted") +
    ggplot2::geom_boxplot(fill = "#33a02c", outlier.size = 0.3, outlier.alpha = 0.3, linewidth = 0.3, width = 0.6) +
    ggplot2::geom_point(data = sector_model, ggplot2::aes(y = Ru_model), shape = 18, size = 3.5, colour = "grey20") +
    ggplot2::labs(x = "Wind direction sector", y = expression(U[Centre] / U[open]),
                  title = "Agreement by wind-direction sector", subtitle = "Boxplots: observed. Diamonds: modelled.",
                  tag = "e") +
    theme_manuscript()

  ts_data <- wind_val |>
    dplyr::mutate(day = as.Date(hour)) |>
    dplyr::group_by(day) |>
    dplyr::summarise(Observed = mean(U_centre_emp, na.rm = TRUE),
                      Modelled = mean(U_centre_model, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_longer(c(Observed, Modelled), names_to = "source", values_to = "U")

  p_f <- ggplot2::ggplot(ts_data, ggplot2::aes(x = day, y = U, colour = source)) +
    ggplot2::geom_line(linewidth = 0.35, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = c("Observed" = "#33a02c", "Modelled" = "grey30"), name = NULL) +
    ggplot2::labs(x = NULL, y = expression("Daily mean "*U[Centre]*" (m s"^-1*")"),
                  title = "Modelled vs observed Centre wind speed over time", tag = "f") +
    theme_manuscript() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))

  (p_d + p_e) / p_f + patchwork::plot_layout(heights = c(1, 0.8))
}

# ===========================================================================
# Part 3: temperature model comparison (LM / GAM / GBM), same Centre
# ensemble + open-field reference + Centre simulated radiation as predictors.
# GBM uses fixed, reasonable hyperparameters rather than the source
# notebook's Bayesian tune_bayes() search (tidymodels/tune not installed,
# and a 40-iteration x 5-fold CV search is not a proportionate cost here) -
# noted plainly in the figure caption rather than silently reproduced.
# ===========================================================================

#' Build the hourly temperature master dataset: Centre ensemble temperature,
#' open-field temperature, Centre-substrip simulated radiation, and a
#' model-based effective-ventilation term, with cyclic time features and
#' 1-hour lags (computed before any train/test split, so no leakage).
#'
#' @param nest_clean Optional pre-built `load_nest_data_long() |>
#'   clean_nest_wind()` result, and `wind_val` an optional pre-built
#'   `prepare_wind_validation_data()` result - both pass straight through
#'   from the earlier wind-validation step when building the full Fig. S3
#'   composite, rather than each re-doing the (slow) nest reshape from raw.
prepare_temperature_master_data <- function(nest_clean = NULL, wind_val = NULL) {
  if (is.null(nest_clean)) nest_clean <- load_nest_data_long() |> clean_nest_wind()
  temp_centre <- build_centre_temp_ensemble(nest_clean) |>
    dplyr::mutate(hour = lubridate::floor_date(date_time, "hour")) |>
    dplyr::group_by(hour) |>
    dplyr::summarise(T_centre_emp = mean(T_centre_emp, na.rm = TRUE), .groups = "drop")

  openfield <- load_openfield_reference() |>
    dplyr::mutate(hour = lubridate::floor_date(date_time, "hour")) |>
    dplyr::group_by(hour) |>
    dplyr::summarise(T_open = mean(T_open, na.rm = TRUE), .groups = "drop")

  rad_path <- here::here("data", "raw", "nest_microclimate_validation",
                          "simulated_radiation_daisy_ready", "SUBSTRIP_Center_DAISY_ready.csv")
  rad <- readr::read_csv(rad_path, show_col_types = FALSE) |>
    dplyr::mutate(hour = as.POSIXct(gsub("T", " ", gsub("Z$", "", datetime)), tz = "UTC")) |>
    dplyr::select(hour, GlobRad_centre = GlobRad)

  if (is.null(wind_val)) wind_val <- prepare_wind_validation_data(nest_clean = nest_clean)
  wind_val <- wind_val |> dplyr::select(hour, U_centre_model, Dir_open)

  d <- temp_centre |>
    dplyr::inner_join(openfield, by = "hour") |>
    dplyr::inner_join(rad, by = "hour") |>
    dplyr::left_join(wind_val, by = "hour") |>
    dplyr::arrange(hour) |>
    dplyr::mutate(
      dT = T_centre_emp - T_open,
      wind_eff = U_centre_model * abs(cos(Dir_open * pi / 180)),
      month = lubridate::month(hour), hour_of_day = lubridate::hour(hour),
      sin_month = sin(2 * pi * month / 12), cos_month = cos(2 * pi * month / 12),
      sin_hour = sin(2 * pi * hour_of_day / 24), cos_hour = cos(2 * pi * hour_of_day / 24),
      lag_T = dplyr::lag(T_centre_emp, 1), lag_dT = dplyr::lag(dT, 1),
      lag_rad = dplyr::lag(GlobRad_centre, 1)
    ) |>
    dplyr::filter(dplyr::if_all(c(T_centre_emp, T_open, GlobRad_centre, wind_eff, lag_T, lag_rad), is.finite))

  d
}

#' Fit LM, GAM and GBM models predicting Centre air temperature from
#' open-field temperature, Centre radiation, model-based effective
#' ventilation, cyclic time features, and lagged predictors; evaluate all
#' three on a held-out 30% test split, stratified by month.
fit_temperature_models <- function(master, seed = 123, test_prop = 0.3) {
  set.seed(seed)
  n <- nrow(master)
  test_idx <- master |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::group_by(month) |>
    dplyr::slice_sample(prop = test_prop) |>
    dplyr::pull(.row)
  train <- master[-test_idx, ]
  test <- master[test_idx, ]

  form <- T_centre_emp ~ T_open + GlobRad_centre + wind_eff + sin_month + cos_month +
    sin_hour + cos_hour + lag_T + lag_rad

  fit_lm <- stats::lm(form, data = train)

  fit_gam <- mgcv::gam(
    T_centre_emp ~ s(T_open, k = 12) + s(GlobRad_centre, k = 12) + s(wind_eff, k = 10) +
      s(month, bs = "cc", k = 8) + s(hour_of_day, bs = "cc", k = 12) +
      s(lag_T, k = 8) + s(lag_rad, k = 8),
    data = train, knots = list(month = c(0.5, 12.5), hour_of_day = c(-0.5, 23.5))
  )

  x_cols <- c("T_open", "GlobRad_centre", "wind_eff", "sin_month", "cos_month",
              "sin_hour", "cos_hour", "lag_T", "lag_rad")
  x_train <- as.matrix(train[, x_cols])
  x_test <- as.matrix(test[, x_cols])
  dtrain <- xgboost::xgb.DMatrix(x_train, label = train$T_centre_emp)
  fit_gbm <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror", max_depth = 5, eta = 0.05,
                  subsample = 0.8, colsample_bytree = 0.8),
    data = dtrain, nrounds = 300, verbose = 0
  )

  pred <- tibble::tibble(
    hour = test$hour, month = test$month, Observed = test$T_centre_emp,
    LM = stats::predict(fit_lm, newdata = test),
    GAM = as.numeric(mgcv::predict.gam(fit_gam, newdata = test)),
    GBM = stats::predict(fit_gbm, newdata = x_test)
  )

  metrics <- pred |>
    tidyr::pivot_longer(c(LM, GAM, GBM), names_to = "Model", values_to = "Predicted") |>
    dplyr::group_by(Model) |>
    dplyr::summarise(
      R2 = stats::cor(Observed, Predicted, use = "complete.obs")^2,
      RMSE = sqrt(mean((Observed - Predicted)^2, na.rm = TRUE)),
      Bias = mean(Predicted - Observed, na.rm = TRUE),
      Slope = stats::coef(stats::lm(Observed ~ Predicted))[2],
      .groups = "drop"
    )

  list(fit_lm = fit_lm, fit_gam = fit_gam, fit_gbm = fit_gbm, pred = pred, metrics = metrics,
       n_train = nrow(train), n_test = nrow(test))
}

# ===========================================================================
# SUPPLEMENTARY FIGURE S3g-i - temperature model comparison
# ===========================================================================
plot_fig_s03_temperature_validation <- function(temp_fit) {
  pred_long <- temp_fit$pred |>
    tidyr::pivot_longer(c(LM, GAM, GBM), names_to = "Model", values_to = "Predicted") |>
    dplyr::mutate(Model = factor(Model, levels = c("LM", "GAM", "GBM")))
  metrics <- temp_fit$metrics |> dplyr::mutate(Model = factor(Model, levels = c("LM", "GAM", "GBM")))

  p_g <- ggplot2::ggplot(pred_long, ggplot2::aes(x = Predicted, y = Observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = "dashed") +
    ggplot2::geom_point(alpha = 0.15, size = 0.5, colour = "#D55E00") +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black", linewidth = 0.6) +
    ggplot2::geom_text(data = metrics, ggplot2::aes(x = -Inf, y = Inf, label = sprintf("R^2==%.3f", R2)),
                        hjust = -0.15, vjust = 1.5, parse = TRUE, size = 3, inherit.aes = FALSE) +
    ggplot2::facet_wrap(~Model, nrow = 1) +
    ggplot2::labs(x = expression("Predicted "*T[Centre]*" (°C)"), y = expression("Observed "*T[Centre]*" (°C)"),
                  title = "Held-out validation: LM vs GAM vs GBM", tag = "g") +
    theme_manuscript() + ggplot2::coord_equal()

  ts_data <- temp_fit$pred |>
    dplyr::mutate(day = as.Date(hour)) |>
    dplyr::group_by(day) |>
    dplyr::summarise(Observed = mean(Observed, na.rm = TRUE), GAM = mean(GAM, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_longer(c(Observed, GAM), names_to = "source", values_to = "T")

  p_h <- ggplot2::ggplot(ts_data, ggplot2::aes(x = day, y = T, colour = source)) +
    ggplot2::geom_line(linewidth = 0.35, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = c("Observed" = "#D55E00", "GAM" = "grey30"), name = NULL) +
    ggplot2::labs(x = NULL, y = expression("Daily mean "*T[Centre]*" (°C)"),
                  title = "GAM prediction vs observed (test split)", tag = "h") +
    theme_manuscript() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))

  month_abb_en <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  month_clim <- temp_fit$pred |>
    dplyr::mutate(month = factor(month_abb_en[lubridate::month(hour)], levels = month_abb_en)) |>
    tidyr::pivot_longer(c(Observed, GAM), names_to = "source", values_to = "T") |>
    dplyr::group_by(month, source) |>
    dplyr::summarise(mean_T = mean(T, na.rm = TRUE), se_T = sd(T, na.rm = TRUE) / sqrt(dplyr::n()), .groups = "drop")

  p_i <- ggplot2::ggplot(month_clim, ggplot2::aes(x = month, y = mean_T, colour = source, group = source)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_T - se_T, ymax = mean_T + se_T, fill = source), alpha = 0.2, colour = NA) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = c("Observed" = "#D55E00", "GAM" = "grey30"), name = NULL) +
    ggplot2::scale_fill_manual(values = c("Observed" = "#D55E00", "GAM" = "grey30"), guide = "none") +
    ggplot2::labs(x = NULL, y = expression("Mean "*T[Centre]*" (°C)"),
                  title = "Monthly climatology: GAM vs observed", tag = "i") +
    theme_manuscript()

  p_g / (p_h + p_i) + patchwork::plot_layout(heights = c(1, 1))
}

# ===========================================================================
# Full Fig. S3 assembly: mechanism (a-c) + wind validation (d-f) + temperature
# validation (g-i), continuously lettered a-i (fixes the original restart
# bug: wind used to cite S3a-e and temperature independently reused S3a-i).
# ===========================================================================
build_fig_s03 <- function() {
  nest_clean <- load_nest_data_long() |> clean_nest_wind()
  wind_val <- prepare_wind_validation_data(nest_clean = nest_clean)
  master <- prepare_temperature_master_data(nest_clean = nest_clean, wind_val = wind_val)
  temp_fit <- fit_temperature_models(master)

  p_mech <- plot_fig_s03_mechanism(wind_val)
  p_wind <- plot_fig_s03_wind_validation(wind_val)
  p_temp <- plot_fig_s03_temperature_validation(temp_fit)

  divider <- function(label) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0, label = label, fontface = "bold", size = 3.6,
                         colour = "grey20", hjust = 0.5) +
      ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(6, 0, 3, 0))
  }

  fig <- (divider("Barrier-profile mechanism (a-c)") / p_mech /
    divider("Empirical wind validation, Centre only (d-f)") / p_wind /
    divider("Temperature model comparison (g-i)") / p_temp) +
    patchwork::plot_layout(heights = c(0.03, 1.4, 0.03, 1.4, 0.03, 1.4))

  list(figure = fig, wind_val = wind_val, master = master, temp_fit = temp_fit)
}

save_fig_s03 <- function(built, out_dir = here::here("outputs", "figures", "supplementary")) {
  fs::dir_create(out_dir)
  ggplot2::ggsave(file.path(out_dir, "Fig_S03_substrip_validation.png"), built$figure,
                   width = 11, height = 20, dpi = 320, bg = "white", limitsize = FALSE)
  ggplot2::ggsave(file.path(out_dir, "Fig_S03_substrip_validation.pdf"), built$figure,
                   width = 11, height = 20, device = cairo_pdf, limitsize = FALSE)
  invisible(built)
}

# ===========================================================================
# Part B, panel (j)-(k): simulated substrip irradiance vs the real open-field
# input that drives the ray-traced model (Note S2.1). Not "validation" in the
# wind/temperature sense - radiation has no independent substrip sensor, at
# Centre or anywhere else - but a genuine, data-grounded comparison of real
# open-field measurement vs modelled substrip output, in consistent W m-2
# units (SUBSTRIP_*_DAISY_ready.csv GlobRad/DiffRad vs foulum_data_with_Ru's
# glorad_wm2/difrad_wm2, same hourly record).
# ===========================================================================
REPRESENTATIVE_DATES <- list(
  "21 March"     = c(month = 3, day = 21),
  "21 June"      = c(month = 6, day = 21),
  "22 September" = c(month = 9, day = 22),
  "21 December"  = c(month = 12, day = 21)
)

load_substrip_radiation <- function() {
  base <- here::here("data", "raw", "nest_microclimate_validation", "simulated_radiation_daisy_ready")
  purrr::map_dfr(c(West = "West", Centre = "Center", East = "East"), \(loc) {
    readr::read_csv(file.path(base, paste0("SUBSTRIP_", loc, "_DAISY_ready.csv")), show_col_types = FALSE) |>
      dplyr::mutate(hour = as.POSIXct(gsub("T", " ", gsub("Z$", "", datetime)), tz = "UTC")) |>
      dplyr::select(hour, GlobRad, DiffRad)
  }, .id = "substrip")
}

prepare_radiation_data <- function() {
  substrip_rad <- load_substrip_radiation()
  openfield <- readRDS(here::here("data", "raw", "nest_microclimate_validation", "foulum_data_with_Ru.rds")) |>
    tibble::as_tibble() |>
    dplyr::transmute(hour = lubridate::floor_date(datetime, "hour"), GlobRad_ref = glorad_wm2, DiffRad_ref = difrad_wm2) |>
    dplyr::group_by(hour) |>
    dplyr::summarise(GlobRad_ref = mean(GlobRad_ref, na.rm = TRUE), DiffRad_ref = mean(DiffRad_ref, na.rm = TRUE), .groups = "drop")

  substrip_rad |>
    dplyr::inner_join(openfield, by = "hour") |>
    dplyr::mutate(
      substrip = factor(substrip, levels = c("West", "Centre", "East")),
      DirRad = pmax(GlobRad - DiffRad, 0), DirRad_ref = pmax(GlobRad_ref - DiffRad_ref, 0),
      month = lubridate::month(hour), day = lubridate::day(hour), hour_of_day = lubridate::hour(hour),
      year = lubridate::year(hour)
    )
}

plot_fig_s03_radiation <- function(rad) {
  snapshot <- purrr::imap_dfr(REPRESENTATIVE_DATES, \(md, lab) {
    rad |> dplyr::filter(month == md[["month"]], abs(day - md[["day"]]) <= 1) |> dplyr::mutate(date_label = lab)
  })
  snapshot$date_label <- factor(snapshot$date_label, levels = names(REPRESENTATIVE_DATES))

  clim <- snapshot |>
    dplyr::group_by(date_label, hour_of_day, substrip) |>
    dplyr::summarise(mean_rad = mean(GlobRad, na.rm = TRUE), se_rad = sd(GlobRad, na.rm = TRUE) / sqrt(dplyr::n()), .groups = "drop")
  clim_ref <- snapshot |>
    dplyr::group_by(date_label, hour_of_day) |>
    dplyr::summarise(mean_rad = mean(GlobRad_ref, na.rm = TRUE), se_rad = sd(GlobRad_ref, na.rm = TRUE) / sqrt(dplyr::n()), .groups = "drop") |>
    dplyr::mutate(substrip = "Reference (open field)")

  clim_all <- dplyr::bind_rows(clim, clim_ref)
  clim_all$substrip <- factor(clim_all$substrip, levels = c("Reference (open field)", "West", "Centre", "East"))

  p_j <- ggplot2::ggplot(clim_all, ggplot2::aes(x = hour_of_day, y = mean_rad, colour = substrip)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_rad - se_rad, ymax = mean_rad + se_rad, fill = substrip), alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = c("Reference (open field)" = "black", "West" = "#1f78b4", "Centre" = "#33a02c", "East" = "#e31a1c"), name = NULL) +
    ggplot2::scale_fill_manual(values = c("Reference (open field)" = "black", "West" = "#1f78b4", "Centre" = "#33a02c", "East" = "#e31a1c"), guide = "none") +
    ggplot2::facet_wrap(~date_label, nrow = 1) +
    ggplot2::labs(x = "Hour of day", y = expression("Global radiation (W m"^-2*")"),
                  title = "Simulated substrip global radiation on representative dates", tag = "j") +
    theme_manuscript() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 7))

  budget <- rad |>
    dplyr::group_by(substrip) |>
    dplyr::summarise(Global = sum(GlobRad, na.rm = TRUE) / 1000, Direct = sum(DirRad, na.rm = TRUE) / 1000,
                      Diffuse = sum(DiffRad, na.rm = TRUE) / 1000,
                      Reference = sum(GlobRad_ref, na.rm = TRUE) / 1000, .groups = "drop") |>
    tidyr::pivot_longer(c(Global, Direct, Diffuse, Reference), names_to = "component", values_to = "total_kwh_m2")
  budget$component <- factor(budget$component, levels = c("Global", "Direct", "Diffuse", "Reference"))

  p_k <- ggplot2::ggplot(budget, ggplot2::aes(x = substrip, y = total_kwh_m2, fill = component)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::scale_fill_manual(values = c("Global" = "#3182BD", "Direct" = "#E6550D", "Diffuse" = "#FDBE85", "Reference" = "grey40"), name = NULL) +
    ggplot2::labs(x = "Substrip", y = expression("Total irradiance, full record (10"^3~"kWh m"^-2*")"),
                  title = "Cumulative radiation budget by substrip and component", tag = "k") +
    theme_manuscript()

  p_j / p_k + patchwork::plot_layout(heights = c(1, 1.1))
}

# ===========================================================================
# Part B, panel (l)-(m): additional temperature diagnostics - full-record
# multi-model time series, and a parallel Delta-T (Centre - open-field) model
# family fit alongside the absolute-T models already in Part A.
# ===========================================================================
fit_dT_models <- function(master, seed = 123, test_prop = 0.3) {
  set.seed(seed)
  test_idx <- master |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::group_by(month) |>
    dplyr::slice_sample(prop = test_prop) |>
    dplyr::pull(.row)
  train <- master[-test_idx, ]
  test <- master[test_idx, ]

  form <- dT ~ GlobRad_centre + wind_eff + sin_month + cos_month + sin_hour + cos_hour + lag_dT + lag_rad

  fit_lm <- stats::lm(form, data = train)
  fit_gam <- mgcv::gam(
    dT ~ s(GlobRad_centre, k = 12) + s(wind_eff, k = 10) + s(month, bs = "cc", k = 8) +
      s(hour_of_day, bs = "cc", k = 12) + s(lag_dT, k = 8) + s(lag_rad, k = 8),
    data = train, knots = list(month = c(0.5, 12.5), hour_of_day = c(-0.5, 23.5))
  )
  x_cols <- c("GlobRad_centre", "wind_eff", "sin_month", "cos_month", "sin_hour", "cos_hour", "lag_dT", "lag_rad")
  dtrain <- xgboost::xgb.DMatrix(as.matrix(train[, x_cols]), label = train$dT)
  fit_gbm <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror", max_depth = 5, eta = 0.05, subsample = 0.8, colsample_bytree = 0.8),
    data = dtrain, nrounds = 300, verbose = 0
  )

  pred <- tibble::tibble(
    hour = test$hour, month = test$month, Observed = test$dT,
    LM = stats::predict(fit_lm, newdata = test),
    GAM = as.numeric(mgcv::predict.gam(fit_gam, newdata = test)),
    GBM = stats::predict(fit_gbm, newdata = as.matrix(test[, x_cols]))
  )
  metrics <- pred |>
    tidyr::pivot_longer(c(LM, GAM, GBM), names_to = "Model", values_to = "Predicted") |>
    dplyr::group_by(Model) |>
    dplyr::summarise(R2 = stats::cor(Observed, Predicted, use = "complete.obs")^2,
                      RMSE = sqrt(mean((Observed - Predicted)^2, na.rm = TRUE)), .groups = "drop")

  list(fit_lm = fit_lm, fit_gam = fit_gam, fit_gbm = fit_gbm, pred = pred, metrics = metrics)
}

plot_fig_s03_temp_extra <- function(master, temp_fit, dt_fit) {
  full_pred <- tibble::tibble(
    hour = master$hour, Observed = master$T_centre_emp,
    LM = stats::predict(temp_fit$fit_lm, newdata = master),
    GAM = as.numeric(mgcv::predict.gam(temp_fit$fit_gam, newdata = master)),
    GBM = stats::predict(temp_fit$fit_gbm, newdata = as.matrix(master[, c("T_open","GlobRad_centre","wind_eff","sin_month","cos_month","sin_hour","cos_hour","lag_T","lag_rad")]))
  ) |>
    dplyr::arrange(hour) |>
    dplyr::mutate(dplyr::across(c(Observed, LM, GAM, GBM), \(x) zoo::rollmean(x, k = 7 * 24, fill = NA, align = "center")))

  ts_long <- full_pred |> tidyr::pivot_longer(c(Observed, LM, GAM, GBM), names_to = "series", values_to = "T") |>
    dplyr::filter(is.finite(T))
  ts_long$series <- factor(ts_long$series, levels = c("Observed", "LM", "GAM", "GBM"))

  p_l <- ggplot2::ggplot(ts_long, ggplot2::aes(x = hour, y = T, colour = series)) +
    ggplot2::geom_line(linewidth = 0.4, alpha = 0.9) +
    ggplot2::scale_colour_manual(values = c("Observed" = "#56B4E9", "LM" = "#000080", "GAM" = "#1B7837", "GBM" = "#B2182B"), name = NULL) +
    ggplot2::labs(x = NULL, y = expression("T"[Centre]*" (°C), 7-day moving average"),
                  title = "Full-record model comparison, all three models", tag = "l") +
    theme_manuscript()

  month_abb_en <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  dt_month <- dt_fit$pred |>
    dplyr::mutate(month_lab = factor(month_abb_en[lubridate::month(hour)], levels = month_abb_en)) |>
    tidyr::pivot_longer(c(Observed, LM, GAM, GBM), names_to = "series", values_to = "dT_val") |>
    dplyr::group_by(month_lab, series) |>
    dplyr::summarise(mean_dT = mean(dT_val, na.rm = TRUE), se_dT = sd(dT_val, na.rm = TRUE) / sqrt(dplyr::n()), .groups = "drop")
  dt_month$series <- factor(dt_month$series, levels = c("Observed", "LM", "GAM", "GBM"))

  p_m <- ggplot2::ggplot(dt_month, ggplot2::aes(x = month_lab, y = mean_dT, fill = series)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_dT - se_dT, ymax = mean_dT + se_dT),
                            position = ggplot2::position_dodge(width = 0.8), width = 0.25, linewidth = 0.3) +
    ggplot2::scale_fill_manual(values = c("Observed" = "grey20", "LM" = "#000080", "GAM" = "#1B7837", "GBM" = "#B2182B"), name = NULL) +
    ggplot2::labs(x = NULL, y = expression(Delta*"T, Centre − open-field (°C)"),
                  title = "Monthly mean ΔT: modelled vs observed (held-out test split)", tag = "m") +
    theme_manuscript()

  p_l / p_m + patchwork::plot_layout(heights = c(1, 1))
}

# ===========================================================================
# Fig. S3 Part B assembly: 2 irradiance panels (j-k) + 2 temperature panels
# (l-m), a separate figure object from Part A (a-i) - same convention this
# document already uses for Table S2 Part A / Part B.
# ===========================================================================
build_fig_s03_partB <- function(master = NULL, temp_fit = NULL) {
  if (is.null(master) || is.null(temp_fit)) {
    nest_clean <- load_nest_data_long() |> clean_nest_wind()
    wind_val <- prepare_wind_validation_data(nest_clean = nest_clean)
    master <- prepare_temperature_master_data(nest_clean = nest_clean, wind_val = wind_val)
    temp_fit <- fit_temperature_models(master)
  }
  rad <- prepare_radiation_data()
  dt_fit <- fit_dT_models(master)

  p_rad <- plot_fig_s03_radiation(rad)
  p_temp_extra <- plot_fig_s03_temp_extra(master, temp_fit, dt_fit)

  divider <- function(label) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0, label = label, fontface = "bold", size = 3.6,
                         colour = "grey20", hjust = 0.5) +
      ggplot2::theme_void() + ggplot2::theme(plot.margin = ggplot2::margin(6, 0, 3, 0))
  }

  fig <- (divider("Simulated substrip irradiance vs open-field input (j-k)") / p_rad /
    divider("Additional temperature diagnostics (l-m)") / p_temp_extra) +
    patchwork::plot_layout(heights = c(0.03, 1.5, 0.03, 1.4))

  list(figure = fig, rad = rad, dt_fit = dt_fit)
}

save_fig_s03_partB <- function(built, out_dir = here::here("outputs", "figures", "supplementary")) {
  fs::dir_create(out_dir)
  ggplot2::ggsave(file.path(out_dir, "Fig_S03_partB_irradiance_temperature.png"), built$figure,
                   width = 11, height = 14, dpi = 320, bg = "white", limitsize = FALSE)
  ggplot2::ggsave(file.path(out_dir, "Fig_S03_partB_irradiance_temperature.pdf"), built$figure,
                   width = 11, height = 14, device = cairo_pdf, limitsize = FALSE)
  invisible(built)
}

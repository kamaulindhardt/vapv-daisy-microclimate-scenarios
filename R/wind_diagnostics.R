# Wind-mechanism diagnostics: Supplementary Figures S6, S7 and S8.
#
# These three figures exist to answer one question the main text raises but
# cannot settle: wind shelter demonstrably cuts evaporative demand, so why does
# it not move yield? The sequence is deliberate -
#   S6  establishes that the demand reduction is real and quantifies it
#   S7  shows the reduction does not reach soil water, stress or yield, and
#       benchmarks its size against radiation and temperature
#   S8  isolates the mechanism: wind changes neither canopy size nor crop water
#       use, whereas radiation and temperature change both
#
# Derived from reference/legacy_snapshot/SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd wind
# diagnostic chunks W1-W12. Those chunks re-read the same multi-GB daily files
# five to eight times each, which is what made the legacy session stall; here
# each file is read once, filtered inside the reader, and cached as a target.

# Scenario ladders used across S6-S8. Wind is the subject; radiation and
# temperature are the benchmark against which its (non-)effect is judged.
wind_ladder <- c("Wind 0%", "Wind -20%", "Wind -30%", "Wind -40%",
                 "Wind -50%", "Wind -60%", "Wind -70%")
radiation_ladder <- c("Rad 0%", "Rad -5%", "Rad -10%", "Rad -20%", "Rad -30%")
temperature_ladder <- c("Tmp -3degC", "Tmp -2degC", "Tmp 0degC",
                        "Tmp +0.5degC", "Tmp +1degC", "Tmp +2degC", "Tmp +3degC")

# ---------------------------------------------------------------------------
# S6a - FAO-56 aerodynamic fraction of reference evapotranspiration
# ---------------------------------------------------------------------------
# Analytical, not simulated. FAO-56 (Allen et al. 1998) splits ET0 into a
# radiation term and an aerodynamic term:
#
#   ET0 = [0.408 D (Rn-G)  +  g (900/(T+273)) u2 (es-ea)] / [D + g (1 + 0.34 u2)]
#          \___radiation___/    \________aerodynamic________/
#
# Only the aerodynamic term contains wind speed, so the aerodynamic FRACTION
# is what wind shelter can act on at all. The curve is evaluated at
# growing-season mean conditions taken from the open-field weather record
# rather than at textbook defaults, so the two reference wind speeds sit on a
# curve built from this site's own climate.
#
# Rn is approximated as 0.5 x incoming shortwave. That is a standard
# rough-and-ready net-radiation estimate; the FRACTION is mildly sensitive to
# it, so the panel is an illustration of the mechanism's shape, not a
# site-calibrated energy balance.
fao56_aerodynamic_fraction <- function(u2, air_temp_c, vapour_pressure_pa,
                                        global_rad_w_m2) {
  # Saturation vapour pressure (kPa) and its slope (kPa/degC), FAO-56 eq. 11-13
  es <- 0.6108 * exp(17.27 * air_temp_c / (air_temp_c + 237.3))
  delta <- 4098 * es / (air_temp_c + 237.3)^2
  ea <- vapour_pressure_pa / 1000
  vpd <- pmax(es - ea, 0)
  gamma <- 0.067  # psychrometric constant at ~15 m elevation, kPa/degC

  # W/m2 -> MJ/m2/day; net radiation approximated as half of incoming shortwave
  rn_mj <- global_rad_w_m2 * 0.0864 * 0.5

  radiation_term <- 0.408 * delta * rn_mj
  aerodynamic_term <- gamma * (900 / (air_temp_c + 273)) * u2 * vpd
  aerodynamic_term / (radiation_term + aerodynamic_term)
}

prepare_fig_s06_a_data <- function(openfield_weather_daily,
                                    vapv_weather_daily,
                                    growing_season_months = 4:9) {
  gs <- openfield_weather_daily |> dplyr::filter(month %in% growing_season_months)
  conditions <- list(
    air_temp_c = mean(gs$air_temp_c, na.rm = TRUE),
    vapour_pressure_pa = mean(gs$vapour_pressure_pa, na.rm = TRUE),
    global_rad_w_m2 = mean(gs$global_rad_w_m2, na.rm = TRUE)
  )

  reference_speeds <- tibble::tibble(
    label = c("Open field", "VAPV centre (Wind 0%)"),
    wind_m_s = c(
      mean(gs$wind_m_s, na.rm = TRUE),
      mean(vapv_weather_daily$wind_m_s[vapv_weather_daily$month %in% growing_season_months],
           na.rm = TRUE)
    )
  ) |>
    dplyr::mutate(aero_fraction = fao56_aerodynamic_fraction(
      wind_m_s, conditions$air_temp_c, conditions$vapour_pressure_pa,
      conditions$global_rad_w_m2))

  curve <- tibble::tibble(wind_m_s = seq(0, 6, by = 0.05)) |>
    dplyr::mutate(aero_fraction = fao56_aerodynamic_fraction(
      wind_m_s, conditions$air_temp_c, conditions$vapour_pressure_pa,
      conditions$global_rad_w_m2))

  list(curve = curve, reference_speeds = reference_speeds, conditions = conditions)
}

# ---------------------------------------------------------------------------
# S6b - simulated PET and AET across the wind-shelter ladder
# ---------------------------------------------------------------------------
# Growing-season totals per year, expressed relative to the open-field run, so
# the panel answers "how much demand does shelter actually remove, and how much
# of that removal reaches actual water use?".
prepare_fig_s06_b_data <- function(daily_swater, growing_season_months = 4:9,
                                    start_year = ANALYSIS_START_YEAR) {
  seasonal <- daily_swater |>
    dplyr::filter(month %in% growing_season_months, year >= start_year) |>
    dplyr::group_by(Weather, year) |>
    dplyr::summarise(
      et0_total = sum(et0_mm, na.rm = TRUE),
      pet_total = sum(pet_mm, na.rm = TRUE),
      aet_total = sum(aet_mm, na.rm = TRUE),
      transpiration_total = sum(actual_transpiration_mm, na.rm = TRUE),
      .groups = "drop"
    )

  reference <- seasonal |>
    dplyr::filter(Weather == "weatherBaselineopen") |>
    dplyr::select(year, ref_et0 = et0_total, ref_pet = pet_total,
                  ref_aet = aet_total, ref_transpiration = transpiration_total)

  seasonal |>
    dplyr::inner_join(reference, by = "year") |>
    add_weather_scenario_label() |>
    dplyr::filter(scen_label %in% wind_ladder) |>
    dplyr::mutate(
      shelter_pct = abs(readr::parse_number(scen_label)),
      et0_rel = 100 * (et0_total - ref_et0) / ref_et0,
      pet_rel = 100 * (pet_total - ref_pet) / ref_pet,
      aet_rel = 100 * (aet_total - ref_aet) / ref_aet
    ) |>
    tidyr::pivot_longer(c(et0_rel, pet_rel, aet_rel),
                        names_to = "flux", values_to = "relative_pct") |>
    dplyr::mutate(flux = dplyr::recode(flux,
      et0_rel = "Reference ET0", pet_rel = "Potential ET", aet_rel = "Actual ET")) |>
    dplyr::group_by(shelter_pct, flux) |>
    dplyr::summarise(
      mean_relative_pct = mean(relative_pct, na.rm = TRUE),
      sd_relative_pct = sd(relative_pct, na.rm = TRUE),
      n_years = dplyr::n(), .groups = "drop"
    ) |>
    dplyr::mutate(flux = factor(flux, levels = c("Reference ET0", "Potential ET", "Actual ET")))
}

# Minimal scenario labelling for tables that carry only the raw `Weather`
# string (the daily readers keep it rather than the full metadata, to stay
# small). Mirrors add_scenario_labels(grid = "center").
add_weather_scenario_label <- function(df, centre_only = TRUE) {
  out <- df |>
    dplyr::mutate(
      .meta = list(NULL),
      scen_label = {
        w <- stringr::str_remove(as.character(Weather), "^weather")
        lvl <- suppressWarnings(as.numeric(stringr::str_match(w, "(Rad|Win|Tmp)([0-9]+)p5")[, 3]))
        lvl <- dplyr::if_else(!is.na(lvl), lvl + 0.5,
                              suppressWarnings(as.numeric(stringr::str_match(w, "(Rad|Win|Tmp)([0-9]+)")[, 3])))
        signed <- dplyr::case_when(
          stringr::str_detect(w, "up") ~ lvl,
          stringr::str_detect(w, "dwn") ~ -lvl,
          TRUE ~ lvl
        )
        dplyr::case_when(
          stringr::str_detect(stringr::str_to_lower(w), "baseline") ~ "Reference",
          stringr::str_starts(w, "Rad") ~ paste0("Rad ", signed, "%"),
          stringr::str_starts(w, "Win") ~ paste0("Wind ", signed, "%"),
          stringr::str_starts(w, "Tmp") & signed > 0 ~ paste0("Tmp +", signed, "degC"),
          stringr::str_starts(w, "Tmp") ~ paste0("Tmp ", signed, "degC"),
          TRUE ~ NA_character_
        )
      }
    ) |>
    dplyr::select(-.meta)

  # Strip position is NOT encoded in scen_label, so West/Centre/East variants
  # of the same driver level would collapse onto one label and be plotted as
  # if they were the same scenario. Keep the Centre strip plus the open-field
  # baseline unless explicitly told otherwise.
  if (centre_only) {
    out <- out |>
      dplyr::filter(stringr::str_detect(as.character(Weather), "Center$") |
                      stringr::str_detect(stringr::str_to_lower(as.character(Weather)), "baseline"))
  }
  out
}

# ---------------------------------------------------------------------------
# S6c / S7c - yield response to each driver, by crop
# ---------------------------------------------------------------------------
# S6c: AGB relative to open field across the wind ladder only.
# S7c: the RANGE of yield response to each of the three drivers, side by side.
#      The range is the summary statistic that matters here - a driver whose
#      full ladder moves yield by 2% is not a design lever, however tidy its
#      dose-response looks in isolation.
prepare_fig_s06_c_data <- function(harvest_annual) {
  percrop_relative_yield(harvest_annual) |>
    dplyr::filter(scen_label %in% wind_ladder) |>
    dplyr::mutate(
      shelter_pct = abs(readr::parse_number(as.character(scen_label))),
      crop_renamed = factor(crop_renamed, levels = crop_levels_all)
    ) |>
    dplyr::filter(!is.na(crop_renamed))
}

percrop_relative_yield <- function(harvest_annual, rotations = paste("Rotation", 1:4)) {
  hc <- prepare_harvest_center(harvest_annual) |>
    dplyr::filter(rotation %in% rotations,
                  fertiliser_type == "Biogas digestate",
                  residue_policy == "Residue removed")

  per_rotation <- hc |>
    dplyr::group_by(crop_renamed, scen_label, rotation, year) |>
    dplyr::summarise(annual = sum(harvested_agb_removed_MgDM_ha, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(crop_renamed, scen_label, rotation) |>
    dplyr::summarise(rot_mean = mean(annual, na.rm = TRUE), .groups = "drop")

  reference <- per_rotation |>
    dplyr::filter(scen_label == "Reference") |>
    dplyr::select(crop_renamed, rotation, ref_mean = rot_mean)

  per_rotation |>
    dplyr::inner_join(reference, by = c("crop_renamed", "rotation")) |>
    dplyr::filter(ref_mean > 0) |>
    dplyr::mutate(relative_pct = 100 * (rot_mean - ref_mean) / ref_mean) |>
    dplyr::group_by(crop_renamed, scen_label) |>
    dplyr::summarise(
      mean_relative_pct = mean(relative_pct, na.rm = TRUE),
      sd_relative_pct = sd(relative_pct, na.rm = TRUE),
      .groups = "drop"
    )
}

prepare_fig_s07_c_data <- function(harvest_annual) {
  rel <- percrop_relative_yield(harvest_annual)

  ladders <- tibble::tribble(
    ~driver,        ~scen_label,
    "Radiation",    radiation_ladder,
    "Temperature",  temperature_ladder,
    "Wind",         wind_ladder
  ) |>
    tidyr::unnest(scen_label)

  rel |>
    dplyr::inner_join(ladders, by = "scen_label") |>
    dplyr::group_by(crop_renamed, driver) |>
    dplyr::summarise(
      mean_response = mean(mean_relative_pct, na.rm = TRUE),
      sd_response = sd(mean_relative_pct, na.rm = TRUE),
      response_range = max(mean_relative_pct, na.rm = TRUE) - min(mean_relative_pct, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      driver = factor(driver, levels = c("Radiation", "Temperature", "Wind")),
      crop_renamed = factor(crop_renamed, levels = crop_levels_all)
    ) |>
    dplyr::filter(!is.na(crop_renamed))
}

# ---------------------------------------------------------------------------
# S7b - demand-versus-use decomposition
# ---------------------------------------------------------------------------
# The mechanistic core of the wind story. If shelter cuts demand (PET) but the
# crop was never drawing on that demand, actual use (AET) will not follow.
# Plotting the two differences against open-field wind speed shows whether the
# gap widens with wind - it does for demand and does not for use.
prepare_fig_s07_b_data <- function(daily_swater, openfield_weather_daily,
                                    start_year = ANALYSIS_START_YEAR,
                                    growing_season_months = 4:9) {
  wide <- daily_swater |>
    dplyr::filter(year >= start_year, month %in% growing_season_months,
                  Weather %in% c("weatherBaselineopen", "weatherWin70dwnCenter")) |>
    dplyr::select(Weather, year, month, mday,
                  pet = pet_mm,
                  aet = aet_mm) |>
    tidyr::pivot_wider(names_from = Weather, values_from = c(pet, aet))

  if (!all(c("pet_weatherBaselineopen", "pet_weatherWin70dwnCenter") %in% names(wide))) {
    return(tibble::tibble())
  }

  wide |>
    dplyr::mutate(
      delta_pet = pet_weatherBaselineopen - pet_weatherWin70dwnCenter,
      delta_aet = aet_weatherBaselineopen - aet_weatherWin70dwnCenter
    ) |>
    dplyr::inner_join(
      openfield_weather_daily |> dplyr::select(year, month, mday, wind_m_s),
      by = c("year", "month", "mday")
    ) |>
    tidyr::pivot_longer(c(delta_pet, delta_aet), names_to = "term", values_to = "delta_mm") |>
    dplyr::mutate(term = dplyr::recode(term,
      delta_pet = "ΔPET (demand)", delta_aet = "ΔAET (crop water use)")) |>
    dplyr::filter(!is.na(delta_mm), !is.na(wind_m_s))
}

# ---------------------------------------------------------------------------
# S8 - canopy size and cumulative water use, dose-response by driver
# ---------------------------------------------------------------------------
# Crop-years must exist in the rotation being read. The draft names soybean
# 2013, but Rotation 1 grows soybean in 2002/2007/2012/2017/2022 - 2013 is a
# winter-wheat year there, so reading it returns an empty soybean panel. 2022
# is Rotation 1's dry soybean year and is the one already used in Figure 3, so
# the two figures show the same season.
prepare_fig_s08_data <- function(daily_crop_production_wind, daily_swater_wind,
                                  crop_years = list("Winter Wheat" = 2018L,
                                                     "Soybean" = 2022L)) {
  lai <- daily_crop_production_wind |>
    dplyr::mutate(crop_renamed = crop_source) |>
    dplyr::filter(purrr::map2_lgl(crop_renamed, year,
                                   \(cr, y) !is.null(crop_years[[cr]]) && y == crop_years[[cr]])) |>
    add_weather_scenario_label() |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::mutate(
      date = as.Date(sprintf("%04d-%02d-%02d", year, month, mday)),
      day_of_year = as.integer(format(date, "%j")),
      lai = LAI_m2_m2
    ) |>
    dplyr::select(crop_renamed, year, scen_label, day_of_year, lai)

  transpiration <- daily_swater_wind |>
    add_weather_scenario_label() |>
    dplyr::filter(!is.na(scen_label)) |>
    dplyr::mutate(
      date = as.Date(sprintf("%04d-%02d-%02d", year, month, mday)),
      day_of_year = as.integer(format(date, "%j"))
    ) |>
    dplyr::group_by(scen_label, year) |>
    dplyr::arrange(day_of_year, .by_group = TRUE) |>
    dplyr::mutate(cumulative_transpiration_mm = cumsum(tidyr::replace_na(actual_transpiration_mm, 0))) |>
    dplyr::ungroup() |>
    dplyr::select(year, scen_label, day_of_year, cumulative_transpiration_mm)

  list(lai = lai, transpiration = transpiration)
}

# ---------------------------------------------------------------------------
# S7a - does wind shelter ever reach soil water, suction or crop stress?
# ---------------------------------------------------------------------------
# The most direct test available: take each crop's driest simulated season,
# walk the full 0-70% shelter ladder, and look at the three quantities that
# would have to move before a yield effect were possible - stored soil water,
# root-zone suction, and the model's own crop water-stress signal. If shelter
# cannot shift these even in the driest year, it cannot shift yield.
prepare_fig_s07_a_data <- function(field_water_daily, daily_pf, daily_crop_production_wind,
                                    crop_years = list("Winter Wheat" = 2018L,
                                                       "Soybean" = 2022L)) {
  keep <- tibble::tibble(
    crop_renamed = names(crop_years),
    year = unlist(crop_years, use.names = FALSE)
  )

  water <- field_water_daily |>
    dplyr::filter(rotation == "Rotation 1", fertilisation == "Biogas digestate",
                  residue == "Residue Removed") |>
    dplyr::mutate(year = as.integer(year), month = as.integer(month),
                  mday = as.integer(mday)) |>
    dplyr::select(Weather = weather_scenario, year, month, mday,
                  soil_matrix_water_mm = Soil_matrix_water_mm)

  pf <- daily_pf |>
    dplyr::mutate(year = as.integer(year), month = as.integer(month), mday = as.integer(mday)) |>
    dplyr::select(Weather, year, month, mday, root_zone_pf)

  # Crop water stress comes from the crop-production output, keyed by crop, so
  # it is joined on crop as well as date.
  stress <- daily_crop_production_wind |>
    dplyr::mutate(crop_renamed = crop_source) |>
    dplyr::select(crop_renamed, Weather, year, month, mday, water_stress)

  water |>
    dplyr::inner_join(pf, by = c("Weather", "year", "month", "mday")) |>
    add_weather_scenario_label() |>
    dplyr::filter(scen_label %in% c("Reference", wind_ladder)) |>
    dplyr::inner_join(keep, by = "year", relationship = "many-to-many") |>
    dplyr::left_join(stress, by = c("crop_renamed", "Weather", "year", "month", "mday")) |>
    dplyr::mutate(
      date = as.Date(sprintf("%04d-%02d-%02d", year, month, mday)),
      day_of_year = as.integer(format(date, "%j")),
      scen_label = factor(scen_label, levels = c("Reference", wind_ladder))
    ) |>
    tidyr::pivot_longer(c(soil_matrix_water_mm, root_zone_pf, water_stress),
                        names_to = "variable", values_to = "value") |>
    dplyr::mutate(variable = factor(dplyr::recode(variable,
      soil_matrix_water_mm = "Soil matrix water (mm)",
      root_zone_pf = "Root-zone pF",
      water_stress = "Crop water stress"),
      levels = c("Soil matrix water (mm)", "Root-zone pF", "Crop water stress"))) |>
    dplyr::filter(!is.na(value))
}

# ---------------------------------------------------------------------------
# S10 - continuous SOC dynamics with a crop-calendar overlay
# ---------------------------------------------------------------------------
# Weekly resolution, so the within-year sawtooth from residue input and
# decomposition is visible - the annual September snapshots behind Figure 6
# cannot show it.
#
# Uses TOTAL SOC (SOM1+SOM2+SOM3) rather than the draft's slow pool, for the
# same reason as Figure 6 and S12: one basis throughout. The slow pool is
# carried in the figure data for comparison.
s10_scenarios <- c("Reference", "Rad 0%", "Rad -10%", "Rad -30%",
                   "Tmp 0degC", "Tmp -3degC", "Tmp +3degC")

prepare_fig_s10_data <- function(weekly_om, start_year = ANALYSIS_START_YEAR) {
  weekly_om |>
    add_weather_scenario_label() |>
    dplyr::filter(scen_label %in% s10_scenarios, year >= start_year) |>
    dplyr::mutate(
      family = dplyr::case_when(
        scen_label == "Reference" ~ "Open field",
        stringr::str_starts(scen_label, "Rad") ~ "Radiation",
        stringr::str_starts(scen_label, "Tmp") ~ "Temperature",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(family)) |>
    dplyr::arrange(scen_label, date)
}

# Crop growing windows for the calendar strip, derived from the daily crop
# state rather than assumed calendar dates. Two crops can share a calendar
# year (undersown ryegrass persisting into the soybean year), so fixed dates
# would mis-attribute the phase.
prepare_crop_calendar <- function(daily_crop_production_all) {
  daily_crop_production_all |>
    dplyr::mutate(
      crop_renamed = dplyr::if_else(
        stringr::str_starts(crop_source, "Grass-Clover"), "Grass-Clover", crop_source),
      date = as.Date(sprintf("%04d-%02d-%02d", year, month, mday)),
      agb = WLeaf_Mg_DM_ha + WStem_Mg_DM_ha + WDead_Mg_DM_ha + WSOrg_Mg_DM_ha
    ) |>
    dplyr::filter(agb > 0.05) |>
    dplyr::group_by(crop_renamed, year) |>
    dplyr::summarise(start_date = min(date), end_date = max(date), .groups = "drop") |>
    dplyr::mutate(crop_renamed = factor(crop_renamed, levels = crop_levels_all)) |>
    dplyr::filter(!is.na(crop_renamed))
}

# Driver family and ordered position, for the graded colour ramps in S8.
classify_scenario_family <- function(scen_label) {
  x <- as.character(scen_label)
  dplyr::case_when(
    x == "Reference" ~ "Open field",
    stringr::str_starts(x, "Rad") ~ "Radiation",
    stringr::str_starts(x, "Wind") ~ "Wind",
    stringr::str_starts(x, "Tmp") ~ "Temperature",
    TRUE ~ NA_character_
  )
}

# Scenario metadata parsing/harmonisation and the baseline-reference join
# logic shared by every analytical domain (productivity, water, nitrogen, SOC).
#
# Extracted from reference/legacy_snapshot/SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd,
# chunks 14D/14E/14F/15A and the baseline-reference helpers in "Step 17A".
#
# Two functions here deliberately do NOT match R/shared_helpers - the
# shared_helpers versions are a stale, never-wired-in prototype (see the
# Phase A/B audit) and diverge from the live/authoritative monolith in ways
# that matter:
#   - parse_weather_metadata(): shared_helpers' regex only captures the
#     integer part of "Tmp0p5upCenter" -> 0 instead of 0.5, silently
#     colliding the real ±0.5°C scenarios with the 0% baseline level.
#     The monolith fixed this (with the fix documented in its own comment);
#     ported faithfully here.
#   - harmonise_crop_names(): shared_helpers keeps "Ryegrass (GC)" and
#     "White Clover (GC)" as two separate crops. The monolith pools both to
#     a single "Grass-Clover" crop, which is required for the species-pooling
#     step in productivity.R to produce one row per year instead of two.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

is_missing_chr <- function(x) {
  x <- as.character(x)
  is.na(x) | !nzchar(x) | x %in% c("NA", "NaN", "nan", "<NA>", "NULL", "null")
}

normalise_name_key <- function(x) {
  x |> as.character() |> stringr::str_to_lower() |> stringr::str_replace_all("[^a-z0-9]", "")
}

find_col_smart <- function(df, candidates) {
  if (is.null(df) || ncol(df) == 0) return(NA_character_)
  nm <- names(df)
  nm_key <- normalise_name_key(nm)
  cand_key <- normalise_name_key(candidates)
  hit <- match(cand_key, nm_key)
  hit <- hit[!is.na(hit)]
  if (length(hit) > 0) return(nm[[hit[[1]]]])
  for (cand in cand_key) {
    idx <- which(stringr::str_detect(nm_key, stringr::fixed(cand)))
    if (length(idx) > 0) return(nm[[idx[[1]]]])
  }
  NA_character_
}

get_chr_col_smart <- function(df, candidates, default = NA_character_) {
  col <- find_col_smart(df, candidates)
  if (is.na(col)) rep(default, nrow(df)) else as.character(df[[col]])
}

get_num_col_smart <- function(df, candidates, default = NA_real_) {
  col <- find_col_smart(df, candidates)
  if (is.na(col)) rep(default, nrow(df)) else suppressWarnings(readr::parse_number(as.character(df[[col]])))
}

coalesce_chr_smart <- function(..., n = NULL) {
  args <- list(...)
  if (is.null(n)) n <- max(purrr::map_int(args, length), 1L)
  args <- purrr::map(args, ~ rep_len(as.character(.x), n))
  out <- args[[1]]
  for (x in args[-1]) {
    idx <- is_missing_chr(out) & !is_missing_chr(x)
    out[idx] <- x[idx]
  }
  out[is_missing_chr(out)] <- NA_character_
  out
}

extract_run_id <- function(x) {
  x <- as.character(x) |> stringr::str_replace_all("\\\\", "/")
  stringr::str_extract(x, "[A-Za-z0-9]+_Rotation[A-Za-z0-9]+_weather[A-Za-z0-9]+")
}

parse_rotation_metadata <- function(x) {
  x <- as.character(x) |> stringr::str_remove("^Rotation")
  x[is_missing_chr(x)] <- NA_character_
  rotation_code <- stringr::str_match(x, "^(GC|C|[0-9]+)")[, 2]

  tibble::tibble(
    rotation_management = dplyr::if_else(is.na(x), NA_character_, paste0("Rotation", x)),
    rotation_fertilisation_residue = dplyr::if_else(is.na(x), NA_character_, paste0("Rotation", x)),
    rotation_code = rotation_code,
    rotation = dplyr::case_when(
      stringr::str_detect(rotation_code, "^[0-9]+$") ~ paste("Rotation", rotation_code),
      rotation_code == "C" ~ "Rotation C",
      rotation_code == "GC" ~ "Grass-Clover rotation",
      TRUE ~ NA_character_
    ),
    fertilisation = dplyr::case_when(
      stringr::str_detect(x, "Dig") ~ "Biogas digestate",
      stringr::str_detect(x, "Min") ~ "Mineral fertiliser",
      TRUE ~ NA_character_
    ),
    residue = dplyr::case_when(
      stringr::str_detect(x, "Rem") ~ "Residue Removed",
      stringr::str_detect(x, "Ret") ~ "Residue Retained",
      TRUE ~ NA_character_
    ),
    management_scheme = dplyr::case_when(
      stringr::str_detect(x, "(Min|Dig)(Rem|Ret)$") ~ "expanded",
      stringr::str_detect(x, "(Min|Dig)$") ~ "compact",
      TRUE ~ "unknown"
    )
  )
}

# Handles weatherBaseline, weatherRad20dwnCenter, weatherWin35upWest,
# weatherTmp2dwnEast, weatherRad0Center, and the "Np5" decimal encoding for
# half-degree temperature steps (weatherTmp0p5upCenter -> +0.5, ...dwn -> -0.5).
parse_weather_metadata <- function(x) {
  x <- as.character(x) |>
    stringr::str_remove("^weather") |>
    stringr::str_remove("^Weather") |>
    stringr::str_replace_all("Centre", "Center")
  x[is_missing_chr(x)] <- NA_character_
  x_low <- stringr::str_to_lower(x)

  weather_factor <- dplyr::case_when(
    stringr::str_detect(x_low, "baseline|reference") ~ "Baseline",
    stringr::str_detect(x_low, "^rad") ~ "Radiation",
    stringr::str_detect(x_low, "^win") ~ "Wind",
    stringr::str_detect(x_low, "^tmp") ~ "Temperature",
    TRUE ~ NA_character_
  )

  weather_direction <- dplyr::case_when(
    stringr::str_detect(x_low, "up") ~ "Increase",
    stringr::str_detect(x_low, "dwn|down") ~ "Decrease",
    stringr::str_detect(x_low, "baseline|reference|0") ~ "No change",
    TRUE ~ NA_character_
  )

  weather_change_pct <- dplyr::case_when(
    stringr::str_detect(x, "(Rad|Win|Tmp)[0-9]+p5") ~
      suppressWarnings(as.numeric(paste0(stringr::str_match(x, "(Rad|Win|Tmp)([0-9]+)p5")[, 3], ".5"))),
    !is.na(stringr::str_match(x, "(Rad|Win|Tmp)([0-9]+)")[, 3]) ~
      suppressWarnings(as.numeric(stringr::str_match(x, "(Rad|Win|Tmp)([0-9]+)")[, 3])),
    stringr::str_detect(x_low, "baseline|reference") ~ 0,
    TRUE ~ NA_real_
  )

  strip_position <- dplyr::case_when(
    stringr::str_detect(x, "Center$") ~ "Center",
    stringr::str_detect(x, "East$") ~ "East",
    stringr::str_detect(x, "West$") ~ "West",
    stringr::str_detect(x_low, "baseline|reference") ~ "Open field",
    TRUE ~ NA_character_
  )

  tibble::tibble(
    weather_scenario = dplyr::if_else(is.na(x), NA_character_, paste0("weather", x)),
    weather_factor = weather_factor,
    weather_direction = weather_direction,
    weather_change_pct = weather_change_pct,
    weather_signed_change_pct = dplyr::case_when(
      weather_direction == "Decrease" ~ -weather_change_pct,
      weather_direction == "Increase" ~ weather_change_pct,
      TRUE ~ weather_change_pct
    ),
    strip_position = strip_position,
    weather_is_baseline = weather_factor == "Baseline",
    perturbation_is_zero = weather_change_pct == 0
  )
}

# Ryegrass (GC) and White Clover (GC) are the two species comprising the same
# mixed grass-clover ley. Both map to "Grass-Clover" so they get pooled
# (summed) as one composite crop throughout the pipeline - see the species
# pooling step in productivity.R.
harmonise_crop_names <- function(x) {
  x_raw <- as.character(x)
  x_low <- stringr::str_to_lower(x_raw)
  dplyr::case_when(
    stringr::str_detect(x_low, "winter[_ ]?wheat|wwheat") ~ "Winter Wheat",
    stringr::str_detect(x_low, "spring[_ ]?barley|sbarley") ~ "Spring Barley",
    stringr::str_detect(x_low, "soy|edemame|edamame|pea_new|soypea") ~ "Soybean",
    stringr::str_detect(x_low, "^pea$") ~ "Soybean",
    stringr::str_detect(x_low, "ryegrass[_ ]?undersown") ~ "Ryegrass (undersown)",
    stringr::str_detect(x_low, "ryegrass[_ ]?gc") ~ "Grass-Clover",
    stringr::str_detect(x_low, "white[_ ]?clover|wclover") ~ "Grass-Clover",
    stringr::str_detect(x_low, "grass[_ ]?clover|grass-clover") ~ "Grass-Clover",
    is.na(x_raw) | !nzchar(x_raw) ~ NA_character_,
    TRUE ~ x_raw
  )
}

crop_colors <- c(
  "Winter Wheat" = "#F1A983",
  "Spring Barley" = "#FFD966",
  "Grass-Clover" = "#4EA72E",
  "Ryegrass (undersown)" = "#4EA78E",
  "Soybean" = "#44B3E1",
  "All crops" = "grey50"
)

add_scenario_metadata <- function(df, soil_default = "foulum") {
  if (is.null(df)) return(NULL)
  if (nrow(df) == 0) return(df)

  out <- df
  n <- nrow(out)

  scenario_col <- find_col_smart(out, c(
    "run_id", "scenario_id", "scenario", "program", "program_name",
    "run", "folder", "directory", "source_file", "source_path", "path"
  ))
  scenario_text <- if (!is.na(scenario_col)) as.character(out[[scenario_col]]) else rep(NA_character_, n)
  detected_run_id <- extract_run_id(scenario_text)
  existing_run_id <- get_chr_col_smart(out, c("run_id", "program", "program_name", "scenario_id", "scenario"))
  detected_run_id <- coalesce_chr_smart(detected_run_id, extract_run_id(existing_run_id), n = n)

  soil_from_run <- stringr::str_match(detected_run_id, "^([^_]+)_Rotation")[, 2]
  rotation_from_run <- stringr::str_match(detected_run_id, "_Rotation([^_]+)_weather")[, 2]
  weather_from_run <- stringr::str_match(detected_run_id, "_weather(.+)$")[, 2]

  # Exact-match only for soil/site - fuzzy matching would catch DAISY's many
  # numeric soil-process columns (Soil_matrix_water_mm, Min-Soil_kg_N_ha, ...).
  if ("Soil" %in% names(out)) {
    soil_from_col <- as.character(out[["Soil"]])
  } else if ("soil" %in% names(out)) {
    soil_from_col <- as.character(out[["soil"]])
  } else if ("Site" %in% names(out)) {
    soil_from_col <- as.character(out[["Site"]])
  } else if ("site" %in% names(out)) {
    soil_from_col <- as.character(out[["site"]])
  } else {
    soil_from_col <- rep(NA_character_, n)
  }

  rotation_from_col <- get_chr_col_smart(out, c(
    "RotationManagement", "rotation_management", "rotationmanagement",
    "RotationFertilisationResidue", "rotation_fertilisation_residue", "rotationfertilisationresidue"
  ))
  weather_from_col <- get_chr_col_smart(out, c(
    "Weather", "weather", "weather_management", "WeatherManagement", "weather_scenario"
  ))

  soil_final <- coalesce_chr_smart(soil_from_run, soil_from_col, rep(soil_default, n), n = n)
  rotation_final <- coalesce_chr_smart(rotation_from_run, rotation_from_col, n = n)
  weather_final <- coalesce_chr_smart(weather_from_run, weather_from_col, n = n)

  rotation_meta <- parse_rotation_metadata(rotation_final)
  weather_meta <- parse_weather_metadata(weather_final)
  run_id_final <- coalesce_chr_smart(
    detected_run_id,
    paste0(soil_final, "_", rotation_meta$rotation_management, "_", weather_meta$weather_scenario),
    n = n
  )

  meta <- dplyr::bind_cols(
    tibble::tibble(run_id = run_id_final, soil = soil_final, scenario_detected_from = scenario_col %||% NA_character_),
    rotation_meta, weather_meta
  )
  meta <- meta[, setdiff(names(meta), names(out)), drop = FALSE]
  out <- dplyr::bind_cols(out, meta)

  if (!"Rotation" %in% names(out)) out$Rotation <- out$rotation
  if (!"Fertilisation" %in% names(out)) out$Fertilisation <- out$fertilisation
  if (!"Residue" %in% names(out)) out$Residue <- out$residue
  if (!"Driver" %in% names(out)) out$Driver <- out$weather_factor
  if (!"Substrip" %in% names(out)) out$Substrip <- out$strip_position

  year_col <- find_col_smart(out, c("year", "Year"))
  month_col <- find_col_smart(out, c("month", "Month"))
  day_col <- find_col_smart(out, c("day", "Day", "mday", "MDay"))
  if (!is.na(year_col) && !is.na(month_col) && !is.na(day_col)) {
    out <- out |> dplyr::mutate(Date = as.Date(sprintf(
      "%04d-%02d-%02d",
      suppressWarnings(as.integer(.data[[year_col]])),
      suppressWarnings(as.integer(.data[[month_col]])),
      suppressWarnings(as.integer(.data[[day_col]]))
    )))
  }
  out
}

prepare_harvest_outcomes <- function(df) {
  if (is.null(df)) return(NULL)
  if (nrow(df) == 0) return(df)

  out <- add_scenario_metadata(df)
  crop_raw <- get_chr_col_smart(out, c("crop", "Crop", "crop_name", "crop_renamed"))
  grain <- get_num_col_smart(out, c(
    "sorg_DM_Mg_DM_ha", "sorg_DM", "sorg_dm_mg_dm_ha", "sorg_dm",
    "grain_DM_Mg_DM_ha", "grain_dm_mg_dm_ha", "storage_organ_DM", "storage_organ_dm"
  ), default = 0)
  stem <- get_num_col_smart(out, c("stem_DM_Mg_DM_ha", "stem_dm_mg_dm_ha", "stem_dm"), default = 0)
  leaf <- get_num_col_smart(out, c("leaf_DM_Mg_DM_ha", "leaf_dm_mg_dm_ha", "leaf_dm"), default = 0)
  dead <- get_num_col_smart(out, c("dead_DM_Mg_DM_ha", "dead_dm_mg_dm_ha", "dead_dm"), default = 0)

  # DAISY reports these per harvest event and they are used by the crop
  # diagnostics (Supplementary Fig. S4): cumulative days under water and
  # nitrogen stress during the season, the harvest index it computed, and
  # water productivity per unit ET.
  water_stress_days <- get_num_col_smart(out, c("WStress_d", "WStress"), default = NA_real_)
  n_stress_days <- get_num_col_smart(out, c("NStress_d", "NStress"), default = NA_real_)
  harvest_index <- get_num_col_smart(out, c("HI"), default = NA_real_)
  water_productivity <- get_num_col_smart(out, c("WP_ET_kg_m3", "WP_ET"), default = NA_real_)

  out |> dplyr::mutate(
    crop_raw = crop_raw,
    crop_renamed = harmonise_crop_names(crop_raw),
    grain_MgDM_ha = grain,
    residue_MgDM_ha = stem + leaf + dead,
    residue_removed_MgDM_ha = dplyr::if_else(residue == "Residue Removed", residue_MgDM_ha, 0),
    harvested_agb_removed_MgDM_ha = grain_MgDM_ha + residue_removed_MgDM_ha,
    total_agb_MgDM_ha = grain_MgDM_ha + residue_MgDM_ha,
    water_stress_days = water_stress_days,
    n_stress_days = n_stress_days,
    harvest_index = harvest_index,
    water_productivity_kg_m3 = water_productivity
  )
}

# Water, nitrogen and SOC outputs all take this path. It attaches scenario
# metadata AND the derived display labels (management_label, weather_label,
# scenario_label) - the labels are added here rather than per-domain because
# every domain needs them, and adding them downstream three times is exactly
# the duplication this reconstruction exists to remove.
prepare_generic_outcomes <- function(df) {
  if (is.null(df)) return(NULL)
  if (nrow(df) == 0) return(df)
  add_scenario_metadata(df) |> make_scenario_label()
}

# Exact-match column getters (legacy 15A `num_col_15`/`chr_col_15`).
# Deliberately NOT the fuzzy find_col_smart() versions: for numeric DAISY
# outputs, fuzzy matching can silently grab the wrong column (e.g. "Soil
# matrix water" when asked for "Soil"), so anything reading a physical
# quantity uses exact names.
num_col <- function(df, candidates, default = 0) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(rep(default, nrow(df)))
  suppressWarnings(as.numeric(df[[hit[1]]]))
}

chr_col <- function(df, candidates, default = NA_character_) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(rep(default, nrow(df)))
  as.character(df[[hit[1]]])
}

# ---------------------------------------------------------------------------
# Scenario labelling, shared by every domain (productivity, water, N, SOC).
#
# Two grids are used in the manuscript and they are not interchangeable:
#   "center" - open-field baseline + Centre-strip scenarios only. Used where
#              the figure shows a driver gradient (Figure 2).
#   "ecw"    - additionally keeps West/East at each driver's 0-level, i.e. the
#              three modelled VAPV substrip microclimates. Used where the
#              figure is about substrip position (Figure 4).
# In both, non-zero perturbation levels exist for the Centre strip only,
# because that is what was simulated (Methods Table 2).
# ---------------------------------------------------------------------------
add_scenario_labels <- function(df, grid = c("center", "ecw")) {
  grid <- match.arg(grid)

  out <- df |>
    dplyr::mutate(
      signed_level = dplyr::case_when(
        stringr::str_detect(as.character(weather_scenario), "0p5up") ~ 0.5,
        stringr::str_detect(as.character(weather_scenario), "0p5dwn") ~ -0.5,
        TRUE ~ suppressWarnings(as.numeric(weather_signed_change_pct))
      ),
      wf = dplyr::if_else(weather_is_baseline, "Reference", as.character(weather_factor))
    )

  if (grid == "center") {
    out <- out |>
      dplyr::filter(weather_is_baseline | strip_position == "Center") |>
      dplyr::mutate(scen_label = dplyr::case_when(
        weather_is_baseline ~ "Reference",
        wf == "Temperature" & signed_level > 0 ~ paste0("Tmp +", signed_level, "degC"),
        wf == "Temperature" & signed_level < 0 ~ paste0("Tmp ", signed_level, "degC"),
        wf == "Temperature" ~ "Tmp 0degC",
        wf == "Radiation" & signed_level > 0 ~ paste0("Rad +", signed_level, "%"),
        wf == "Radiation" ~ paste0("Rad ", signed_level, "%"),
        wf == "Wind" & signed_level > 0 ~ paste0("Wind +", signed_level, "%"),
        wf == "Wind" ~ paste0("Wind ", signed_level, "%"),
        TRUE ~ NA_character_
      ))
  } else {
    out <- out |>
      dplyr::mutate(scen_label = dplyr::case_when(
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
      ))
  }

  out |> dplyr::filter(weather_is_baseline | !is.na(scen_label))
}

scenario_id_cols <- c(
  "run_id", "soil", "rotation", "rotation_code", "fertilisation", "residue",
  "weather_scenario", "weather_factor", "weather_direction", "weather_change_pct",
  "weather_signed_change_pct", "strip_position"
)

make_scenario_label <- function(df) {
  df |> dplyr::mutate(
    scenario_label = paste(rotation, fertilisation, residue, weather_scenario, sep = " | "),
    weather_label = dplyr::case_when(
      weather_factor == "Baseline" ~ "Baseline / open field",
      !is.na(weather_signed_change_pct) ~ paste0(weather_factor, " ", weather_signed_change_pct, "% | ", strip_position),
      TRUE ~ weather_scenario
    ),
    management_label = paste(fertilisation, residue, sep = " | ")
  )
}

summarise_total_annualised <- function(df, value_col, outcome, unit) {
  df |>
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(scenario_id_cols))) |>
    dplyr::summarise(
      n_years = dplyr::n_distinct(year, na.rm = TRUE),
      total = sum(.data[[value_col]], na.rm = TRUE),
      annualised = total / n_years,
      .groups = "drop"
    ) |>
    dplyr::mutate(outcome = outcome, unit = unit) |>
    make_scenario_label()
}

# ---------------------------------------------------------------------------
# Baseline-reference join: shared by every domain's "relative to open field"
# calculation. Reference = the baseline weather scenario within the same
# soil x rotation x fertilisation x residue (+ optional extra keys).
# ---------------------------------------------------------------------------

safe_rel_change_pct <- function(value, reference) {
  dplyr::case_when(
    is.na(value) | is.na(reference) ~ NA_real_,
    reference == 0 & value == 0 ~ 0,
    reference == 0 & value != 0 ~ NA_real_,
    TRUE ~ 100 * ((value / reference) - 1)
  )
}

add_baseline_flag <- function(df) {
  out <- df
  out$weather_is_baseline_safe <- if ("weather_is_baseline" %in% names(out)) {
    dplyr::coalesce(as.logical(out$weather_is_baseline), FALSE)
  } else {
    FALSE
  }
  if ("weather_factor" %in% names(out)) {
    out$weather_is_baseline_safe <- out$weather_is_baseline_safe | as.character(out$weather_factor) == "Baseline"
  }
  if ("weather_scenario" %in% names(out)) {
    out$weather_is_baseline_safe <- out$weather_is_baseline_safe |
      stringr::str_detect(stringr::str_to_lower(as.character(out$weather_scenario)), "baseline|reference")
  }
  if ("weather_label" %in% names(out)) {
    out$weather_is_baseline_safe <- out$weather_is_baseline_safe |
      stringr::str_detect(stringr::str_to_lower(as.character(out$weather_label)), "baseline|reference|open field")
  }
  out
}

add_weather_reference <- function(df, value_col, extra_keys = character(), ref_col_name = "reference_value") {
  if (!value_col %in% names(df)) stop("The value column '", value_col, "' was not found in the input data.")

  df_ref_ready <- add_baseline_flag(df)
  join_keys <- intersect(c("soil", "rotation", "fertilisation", "residue", extra_keys), names(df_ref_ready))

  ref_tbl <- df_ref_ready |>
    dplyr::filter(weather_is_baseline_safe) |>
    dplyr::select(dplyr::all_of(join_keys), !!ref_col_name := dplyr::all_of(value_col)) |>
    dplyr::distinct()
  if (nrow(ref_tbl) == 0) {
    stop("No weather reference rows were found. Check that the input table contains a baseline/reference weather scenario.")
  }
  ref_tbl <- ref_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) |>
    dplyr::summarise(!!ref_col_name := mean(.data[[ref_col_name]], na.rm = TRUE), .groups = "drop")

  df_ref_ready |>
    dplyr::left_join(ref_tbl, by = join_keys) |>
    dplyr::mutate(
      absolute_effect = .data[[value_col]] - .data[[ref_col_name]],
      relative_effect_pct = safe_rel_change_pct(value = .data[[value_col]], reference = .data[[ref_col_name]])
    ) |>
    dplyr::select(-weather_is_baseline_safe)
}

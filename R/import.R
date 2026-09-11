# Discovery and import of NWAPS-aggregated SPAWN output.
#
# Extracted from reference/legacy_snapshot/SPAWN_NWAPS_MANUSCRIPT_VIZ.Rmd,
# chunks 14C ("Robust CSV importer") and "2.8 File registry and import" (14H).
# Deliberately NOT ported from R/shared_helpers/02_nwaps_import.R: that copy
# is missing the leading-comma header repair below, which real NWAPS exports
# need (confirmed on out_Annual-FN_100cm_Apr.csv during the Phase 3 data
# smoke test - fread() flagged the same "36 names, 35 columns" mismatch this
# repair exists to handle).

clean_daisy_names <- function(x) {
  make.unique(
    x |>
      stringr::str_remove_all("`") |>
      stringr::str_remove_all("\\[|\\]|\\^") |>
      stringr::str_replace_all("/", "_") |>
      stringr::str_replace_all("%", "pct") |>
      stringr::str_replace_all("\\s+", "_") |>
      stringr::str_replace_all("__+", "_") |>
      stringr::str_replace_all("_$", ""),
    sep = "_"
  )
}

coerce_numeric_like_columns <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  protected_keys <- normalise_name_key(c(
    "Soil", "soil", "RotationManagement", "rotation_management",
    "RotationFertilisationResidue", "rotation_fertilisation_residue",
    "Weather", "weather", "crop", "Crop", "source_file", "source_path",
    "run_id", "scenario", "program", "program_name"
  ))
  protected_cols <- names(df)[normalise_name_key(names(df)) %in% protected_keys]
  char_cols <- setdiff(names(df)[vapply(df, is.character, logical(1))], protected_cols)
  if (length(char_cols) == 0) return(df)

  num_like <- function(x) {
    x2 <- x[!is.na(x) & nzchar(x)]
    if (!length(x2)) return(FALSE)
    mean(
      stringr::str_detect(x2, "^[-+]?(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][-+]?\\d+)?$"),
      na.rm = TRUE
    ) > 0.95
  }

  to_num <- char_cols[vapply(df[char_cols], num_like, logical(1))]
  if (length(to_num) > 0) {
    df[to_num] <- lapply(df[to_num], function(x) suppressWarnings(readr::parse_double(x)))
  }
  df
}

read_nwaps_output <- function(path, quiet = FALSE) {
  if (!file.exists(path)) {
    if (!quiet) cli::cli_alert_warning("NWAPS output file not found: {path}")
    return(NULL)
  }
  if (!quiet) cli::cli_alert_info("Reading: {fs::path_file(path)}")

  scenario_cols <- c(
    "Soil", "soil", "RotationManagement", "rotation_management",
    "RotationFertilisationResidue", "rotation_fertilisation_residue",
    "Weather", "weather", "crop", "Crop", "program", "Program",
    "program_name", "run_id", "scenario"
  )
  forced_types <- setNames(rep(list(readr::col_character()), length(scenario_cols)), scenario_cols)
  col_types <- do.call(readr::cols, c(list(.default = readr::col_guess()), forced_types))
  na_vals <- c("", "NA", "NaN", "nan", "-nan", "-")

  # Guard against a malformed header with a spurious leading separator. Some
  # NWAPS exports (e.g. out_Annual-FN_100cm_Apr/Sep.csv) prepend a stray comma
  # to the HEADER row only, so the header has exactly one more field than each
  # data row, empty-named at position 1. Reading normally left-shifts every
  # column name and silently corrupts the constructed Date. When detected,
  # read with explicit column names taken from the header minus its empty
  # leading field, skipping the original header row.
  peek <- tryCatch(readLines(path, n = 2L, warn = FALSE), error = function(e) character(0))
  hdr_fields <- if (length(peek) >= 1L) strsplit(peek[1], ",", fixed = TRUE)[[1]] else character(0)
  n_data_fld <- if (length(peek) >= 2L) length(strsplit(peek[2], ",", fixed = TRUE)[[1]]) else NA_integer_
  leading_empty_header <- length(hdr_fields) >= 2L && !is.na(n_data_fld) &&
    length(hdr_fields) == n_data_fld + 1L && !nzchar(trimws(hdr_fields[1]))

  df <- if (leading_empty_header) {
    if (!quiet) cli::cli_alert_info("Repairing leading-comma header in {fs::path_file(path)}")
    readr::read_csv(
      path, col_names = hdr_fields[-1L], skip = 1L,
      col_types = col_types, progress = FALSE, guess_max = 10000, na = na_vals
    )
  } else {
    readr::read_csv(path, col_types = col_types, progress = FALSE, guess_max = 10000, na = na_vals)
  }

  names(df) <- clean_daisy_names(names(df))
  if ("...1" %in% names(df)) {
    tmp <- suppressWarnings(readr::parse_double(df[["...1"]]))
    if (all(is.na(df[["...1"]])) || identical(tmp, seq_along(tmp))) df[["...1"]] <- NULL
  }

  df |>
    coerce_numeric_like_columns() |>
    dplyr::mutate(source_file = fs::path_file(path), source_path = path, .before = 1)
}

# Registry of the "core" NWAPS outputs the manuscript pipeline consumes
# (harvest, water, nitrogen, carbon). Deliberately excludes the multi-GB
# out_Daily-*.csv files used only by the wind-diagnostic appendix figures -
# those are a separate, later target once that domain is ported.
build_nwaps_import_registry <- function(raw_dir) {
  tibble::tribble(
    ~object,              ~file,                             ~category,        ~required_for_core,
    "harvest",            "out_Annual-Harvest.csv",          "harvest",        TRUE,
    "field_water_daily",  "out_Daily-FWater.csv",            "water_daily",    TRUE,
    "field_water_apr",    "out_Annual-FWater_Apr.csv",       "water_annual",   TRUE,
    "field_water_sep",    "out_Annual-FWater_Sep.csv",       "water_annual",   TRUE,
    "field_n_apr",        "out_Annual-FN_100cm_Apr.csv",     "nitrogen",       TRUE,
    "field_n_sep",        "out_Annual-FN_100cm_Sep.csv",     "nitrogen",       TRUE,
    "som_apr",            "out_Annual-OM_Apr.csv",           "carbon",         TRUE,
    "som_sep",            "out_Annual-OM_Sep.csv",           "carbon",         TRUE,
    "som_to30_apr",       "out_Annual-OM_to30_Apr.csv",      "carbon_layer",   FALSE,
    "som_to30_sep",       "out_Annual-OM_to30_Sep.csv",      "carbon_layer",   TRUE,
    "som_to60_apr",       "out_Annual-OM_to60_Apr.csv",      "carbon_layer",   FALSE,
    "som_to60_sep",       "out_Annual-OM_to60_Sep.csv",      "carbon_layer",   TRUE,
    "som_from60_apr",     "out_Annual-OM_from60_Apr.csv",    "carbon_layer",   FALSE,
    "som_from60_sep",     "out_Annual-OM_from60_Sep.csv",    "carbon_layer",   TRUE
  ) |>
    dplyr::mutate(path = file.path(raw_dir, file), exists = file.exists(path))
}

import_nwaps_outputs <- function(registry) {
  registry |>
    dplyr::mutate(
      data = purrr::map(path, ~ if (file.exists(.x)) read_nwaps_output(.x, quiet = TRUE) else NULL),
      n_rows = purrr::map_int(data, ~ nrow(.x %||% tibble::tibble())),
      n_cols = purrr::map_int(data, ~ ncol(.x %||% tibble::tibble()))
    )
}

get_imported_object <- function(imported, object_name) {
  hit <- imported |> dplyr::filter(object == object_name) |> dplyr::pull(data)
  if (length(hit) == 0) NULL else hit[[1]]
}

# ---------------------------------------------------------------------------
# Daily crop-production output (seasonal trajectories, Figure 3)
#
# These files are 700-735 MB each and there is one per crop. Reading them the
# way the rest of the pipeline reads annual output would pull ~3.6 GB into
# memory to keep a few thousand rows. Instead: data.table::fread with an
# explicit column subset, then filter to the wanted scenarios *immediately*,
# before anything else touches the data. This is the single biggest memory
# lever in the project - the legacy script's wind-diagnostic chunks re-read
# these same files 5-8 times each, which is what made sessions stall.
# ---------------------------------------------------------------------------

daily_crop_files <- c(
  "Winter Wheat" = "out_Daily-CropProduction_Winter_wheat.csv",
  "Spring Barley" = "out_Daily-CropProduction_Spring_barley.csv",
  "Soybean" = "out_Daily-CropProduction_Soy_edemame.csv",
  "Ryegrass (undersown)" = "out_Daily-CropProduction_Ryegrass_undersown.csv",
  # Grass-clover is simulated as two species and pooled downstream, mirroring
  # the annual harvest handling in harmonise_crop_names().
  "Grass-Clover (ryegrass)" = "out_Daily-CropProduction_Ryegrass_GC.csv",
  "Grass-Clover (clover)" = "out_Daily-CropProduction_White_clover_GC.csv"
)

read_daily_crop_production <- function(raw_dir, file_name, crop_label,
                                        keep_rotation_management = "Rotation1DigRem",
                                        keep_weather = c("weatherBaselineopen", "weatherRad0Center"),
                                        keep_years = NULL) {
  path <- file.path(raw_dir, file_name)
  if (!file.exists(path)) {
    cli::cli_alert_warning("Daily crop file not found: {path}")
    return(NULL)
  }

  wanted <- c("Site", "RotationManagement", "Weather", "year", "month", "mday",
              "DS [DS]", "LAI [m^2/m^2]",
              "WLeaf [Mg DM/ha]", "WDead [Mg DM/ha]",
              "WStem [Mg DM/ha]", "WSOrg [Mg DM/ha]",
              # Crop-level water stress. The `production_stress` column in
              # out_Daily-SWater.csv is -1 in every row of this run, i.e. not
              # populated, so crop water stress has to come from here.
              "water_stress")

  dt <- data.table::fread(
    path, select = wanted, showProgress = FALSE,
    na.strings = c("", "NA", "NaN", "nan", "-nan", "-")
  )
  data.table::setnames(dt, clean_daisy_names(names(dt)))

  dt <- dt[RotationManagement == keep_rotation_management]
  # NULL means "keep every scenario". Applying `Weather %in% NULL`
  # unconditionally would evaluate to all-FALSE and silently return zero rows.
  if (!is.null(keep_weather)) dt <- dt[Weather %in% keep_weather]
  if (!is.null(keep_years)) dt <- dt[year %in% keep_years]

  tibble::as_tibble(dt) |>
    dplyr::mutate(
      crop_source = crop_label,
      year = as.integer(year), month = as.integer(month), mday = as.integer(mday),
      dplyr::across(c(DS_DS, LAI_m2_m2, WLeaf_Mg_DM_ha, WDead_Mg_DM_ha,
                      WStem_Mg_DM_ha, WSOrg_Mg_DM_ha, water_stress),
                    \(x) suppressWarnings(as.numeric(x)))
    )
}

import_daily_crop_production <- function(raw_dir, keep_years = NULL,
                                          keep_weather = c("weatherBaselineopen", "weatherRad0Center"),
                                          crops = names(daily_crop_files)) {
  purrr::imap(daily_crop_files[crops], \(fname, crop_label) {
    read_daily_crop_production(raw_dir, fname, crop_label,
                                keep_weather = keep_weather, keep_years = keep_years)
  }) |>
    purrr::compact() |>
    dplyr::bind_rows()
}

# ---------------------------------------------------------------------------
# Daily soil-water output (out_Daily-SWater.csv, ~840 MB)
#
# The only source of separated transpiration: it carries actual and potential
# TRANSPIRATION alongside evapotranspiration, plus reference ET0. The
# out_Daily-FWater.csv file used elsewhere has AET only, which conflates
# canopy transpiration with soil and interception evaporation - the
# distinction that Supplementary Figures S6-S8 exist to make.
# ---------------------------------------------------------------------------
read_daily_swater <- function(raw_dir,
                               keep_rotation_management = "Rotation1DigRem",
                               keep_weather = NULL, keep_years = NULL) {
  path <- file.path(raw_dir, "out_Daily-SWater.csv")
  if (!file.exists(path)) {
    cli::cli_alert_warning("Daily SWater file not found: {path}")
    return(NULL)
  }

  wanted <- c("Site", "RotationManagement", "Weather", "year", "month", "mday",
              "Reference evapotranspiration (dry) [mm]",
              "Potential evapotranspiration [mm]",
              "Actual Evapotranspiration [mm]",
              "Potential transpiration [mm]",
              "Actual transpiration [mm]",
              "Evaporation of soil water [mm]",
              "production_stress")

  dt <- data.table::fread(path, select = wanted, showProgress = FALSE,
                           na.strings = c("", "NA", "NaN", "nan", "-nan", "-"))

  # Explicit stable names rather than clean_daisy_names(). That helper leaves
  # parentheses intact, so "Reference evapotranspiration (dry) [mm]" would
  # become `Reference_evapotranspiration_(dry)_mm` - a name that needs backtick
  # quoting everywhere downstream and breaks tidyselect helpers. Naming these
  # explicitly here keeps the awkwardness in one place.
  data.table::setnames(dt, wanted, c(
    "Site", "RotationManagement", "Weather", "year", "month", "mday",
    "et0_mm", "pet_mm", "aet_mm",
    "potential_transpiration_mm", "actual_transpiration_mm",
    "soil_evaporation_mm", "production_stress"
  ))

  dt <- dt[RotationManagement == keep_rotation_management]
  if (!is.null(keep_weather)) dt <- dt[Weather %in% keep_weather]
  if (!is.null(keep_years)) dt <- dt[year %in% keep_years]

  tibble::as_tibble(dt) |>
    dplyr::mutate(dplyr::across(
      c(et0_mm, pet_mm, aet_mm, potential_transpiration_mm,
        actual_transpiration_mm, soil_evaporation_mm, production_stress),
      \(x) suppressWarnings(as.numeric(x))
    ))
}

# ---------------------------------------------------------------------------
# DAISY weather files (.dwf): 21 header lines, then a column-name row, a units
# row, and tab-separated hourly data. Read for wind speed, which is the driver
# behind the whole S6-S8 sequence but is an INPUT to the simulations rather
# than an output, so it appears in no NWAPS file.
# ---------------------------------------------------------------------------
read_daisy_weather <- function(path) {
  if (!file.exists(path)) {
    cli::cli_alert_warning("Weather file not found: {path}")
    return(NULL)
  }
  dt <- data.table::fread(path, skip = 21, header = TRUE, sep = "\t",
                           showProgress = FALSE)
  # Row 1 after the header is the units row ("year", "dgC", "m/s", ...).
  dt <- dt[-1]
  tibble::as_tibble(dt) |>
    dplyr::transmute(
      year = as.integer(Year), month = as.integer(Month), mday = as.integer(Day),
      hour = as.integer(Hour),
      air_temp_c = as.numeric(AirTemp), wind_m_s = as.numeric(Wind),
      global_rad_w_m2 = as.numeric(GlobRad), vapour_pressure_pa = as.numeric(VapPres),
      precipitation_mm = as.numeric(Precip)
    )
}

# ---------------------------------------------------------------------------
# Daily soil-water potential (out_Daily-pF.csv, ~2.1 GB)
#
# One column per simulated depth ("h @ -0.25 [pF]" ... "h @ -32.5 [pF]", depths
# in dm). Only the root zone is read: water stress is expressed where roots
# actually take up water, and reading all 18 depths would multiply the memory
# cost for columns no figure uses.
#
# pF is log10 of suction head in cm, so wilting point is pF 4.2 and field
# capacity around pF 2.0 - the scale is logarithmic, and a change from 2 to 3
# is a ten-fold change in suction.
# ---------------------------------------------------------------------------
root_zone_pf_columns <- c("h @ -0.25 [pF]", "h @ -0.75 [pF]", "h @ -1.5 [pF]",
                           "h @ -2.5 [pF]", "h @ -3.5 [pF]", "h @ -4.5 [pF]",
                           "h @ -6 [pF]")

WILTING_POINT_PF <- 4.2

read_daily_pf <- function(raw_dir,
                           keep_rotation_management = "Rotation1DigRem",
                           keep_weather = NULL, keep_years = NULL) {
  path <- file.path(raw_dir, "out_Daily-pF.csv")
  if (!file.exists(path)) {
    cli::cli_alert_warning("Daily pF file not found: {path}")
    return(NULL)
  }

  wanted <- c("Site", "RotationManagement", "Weather", "year", "month", "mday",
              root_zone_pf_columns)
  dt <- data.table::fread(path, select = wanted, showProgress = FALSE,
                           na.strings = c("", "NA", "NaN", "nan", "-nan", "-"))
  data.table::setnames(dt, wanted, c(
    "Site", "RotationManagement", "Weather", "year", "month", "mday",
    paste0("pf_", seq_along(root_zone_pf_columns))
  ))

  dt <- dt[RotationManagement == keep_rotation_management]
  if (!is.null(keep_weather)) dt <- dt[Weather %in% keep_weather]
  if (!is.null(keep_years)) dt <- dt[year %in% keep_years]

  tibble::as_tibble(dt) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("pf_"), \(x) suppressWarnings(as.numeric(x)))) |>
    dplyr::mutate(root_zone_pf = rowMeans(dplyr::across(dplyr::starts_with("pf_")), na.rm = TRUE)) |>
    dplyr::select(-dplyr::starts_with("pf_"))
}

# ---------------------------------------------------------------------------
# Weekly organic matter, 0-30 cm (out_Weekly-OM_to30.csv, ~126 MB)
#
# Weekly rather than annual resolution, which is what makes the continuous SOC
# trajectories in Supplementary Fig. S10 possible - the annual September
# snapshots used for Figure 6 cannot show within-year dynamics.
# ---------------------------------------------------------------------------
read_weekly_om <- function(raw_dir, file_name = "out_Weekly-OM_to30.csv",
                            keep_rotation_management = "Rotation1DigRem",
                            keep_weather = NULL) {
  path <- file.path(raw_dir, file_name)
  if (!file.exists(path)) {
    cli::cli_alert_warning("Weekly OM file not found: {path}")
    return(NULL)
  }

  wanted <- c("Site", "RotationManagement", "Weather", "year", "month", "mday",
              "SOM1-C [kg C/ha]", "SOM2-C [kg C/ha]", "SOM3-C [kg C/ha]")
  dt <- data.table::fread(path, select = wanted, showProgress = FALSE,
                           na.strings = c("", "NA", "NaN", "nan", "-nan", "-"))
  data.table::setnames(dt, wanted, c(
    "Site", "RotationManagement", "Weather", "year", "month", "mday",
    "som1_c", "som2_c", "som3_c"
  ))

  dt <- dt[RotationManagement == keep_rotation_management]
  if (!is.null(keep_weather)) dt <- dt[Weather %in% keep_weather]

  tibble::as_tibble(dt) |>
    dplyr::mutate(
      dplyr::across(c(som1_c, som2_c, som3_c), \(x) suppressWarnings(as.numeric(x))),
      total_soc_kgC_ha = som1_c + som2_c + som3_c,
      slow_soc_kgC_ha = som2_c + som3_c,
      date = as.Date(sprintf("%04d-%02d-%02d", as.integer(year),
                              as.integer(month), as.integer(mday)))
    )
}

# Daily aggregation keeps min/max alongside means: the daily temperature RANGE
# is part of the weather description in Supplementary Fig. S1. Precipitation is
# SUMMED over the day, not averaged - averaging an hourly rainfall series gives
# a mean intensity, not a daily total.
summarise_daily_weather <- function(hourly) {
  hourly |>
    dplyr::group_by(year, month, mday) |>
    dplyr::summarise(
      wind_max_m_s = max(wind_m_s, na.rm = TRUE),
      wind_m_s = mean(wind_m_s, na.rm = TRUE),
      air_temp_min_c = min(air_temp_c, na.rm = TRUE),
      air_temp_max_c = max(air_temp_c, na.rm = TRUE),
      air_temp_c = mean(air_temp_c, na.rm = TRUE),
      global_rad_w_m2 = mean(global_rad_w_m2, na.rm = TRUE),
      vapour_pressure_pa = mean(vapour_pressure_pa, na.rm = TRUE),
      precipitation_mm = sum(precipitation_mm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(date = as.Date(sprintf("%04d-%02d-%02d", year, month, mday)),
                  day_of_year = as.integer(format(date, "%j")))
}

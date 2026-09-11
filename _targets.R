library(targets)

tar_option_set(
  format = "rds",
  memory = "transient",
  garbage_collection = TRUE,
  packages = c(
    "tidyverse", "data.table", "readr", "dplyr", "tidyr", "stringr", "purrr",
    "tibble", "fs", "cli", "ggplot2", "ggtext", "ggpattern", "scales", "here",
    # patchwork must be ATTACHED, not just namespaced: its panel-composition
    # operators (/ and |) are S3 methods that are only registered on attach.
    # ggrepel is used for the non-overlapping end-of-series labels in Figure 3.
    "patchwork", "ggrepel", "SPEI"
  )
)

# here::here(), not a bare "R" string: this file can get sourced by
# tar_make() calls from callers whose working directory isn't the project
# root, and a relative path would silently fail to find anything there.
tar_source(here::here("R"))

raw_data_dir <- here::here("data", "raw", "nwaps_full_run")
figures_main_dir <- here::here("outputs", "figures", "main")
figures_supp_dir <- here::here("outputs", "figures", "supplementary")
weather_dir <- here::here("data", "raw", "spawn_common_setup_files", "WEATHER")
gc_calibration_dir <- here::here("data", "raw", "gc_calibration")

list(
  tar_target(nwaps_registry, build_nwaps_import_registry(raw_data_dir)),
  tar_target(nwaps_imported, import_nwaps_outputs(nwaps_registry)),

  tar_target(harvest_prepped, prepare_harvest_outcomes(get_imported_object(nwaps_imported, "harvest"))),
  tar_target(harvest_annual, prepare_harvest_annual(harvest_prepped)),

  # --- Drought classification (SPEI) ---------------------------------------
  # Open-field SPEI-3 is the common year classification used by Figures 3 and
  # 4. Computed once here so "a dry year" means the same thing everywhere.
  tar_target(field_water_daily, prepare_generic_outcomes(get_imported_object(nwaps_imported, "field_water_daily"))),
  tar_target(monthly_water_balance, prepare_monthly_water_balance(field_water_daily)),
  tar_target(spei_monthly, calculate_spei(monthly_water_balance)),
  tar_target(crop_drought_years, classify_crop_drought_years(spei_monthly)),
  tar_target(rotation_crop_years, crop_years_in_rotation(harvest_annual)),
  tar_target(contrast_years, select_contrast_years(crop_drought_years, rotation_crop_years)),

  # --- Whole-system productivity -------------------------------------------
  # Headline ASY - see R/productivity.R for the evaluation-period definition.
  tar_target(system_asy, calculate_system_asy(harvest_annual)),
  tar_target(system_asy_csv, {
    path <- here::here("outputs", "figure_data", "system_asy.csv")
    fs::dir_create(dirname(path)); readr::write_csv(system_asy, path); path
  }, format = "file"),

  # --- Manuscript Figure 2: crop productivity response ---------------------
  tar_target(fig_02_a_data, prepare_fig_02_a_data(harvest_annual)),
  tar_target(fig_02_b_data, prepare_fig_02_b_data(harvest_annual)),
  tar_target(fig_02_plot, plot_fig_02(fig_02_a_data, fig_02_b_data)),
  tar_target(
    fig_02_saved,
    save_manuscript_plot(
      fig_02_plot, "Fig_02_crop_productivity_response.png", figures_main_dir,
      width = figure_specs$fig_02$width, height = figure_specs$fig_02$height
    ),
    format = "file"
  ),
  tar_target(fig_02_a_csv, {
    path <- here::here("outputs", "figure_data", "Fig_02_a_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_02_a_data, path); path
  }, format = "file"),
  tar_target(fig_02_b_csv, {
    path <- here::here("outputs", "figure_data", "Fig_02_b_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_02_b_data, path); path
  }, format = "file"),

  # --- Manuscript Figure 3: seasonal trajectories under drought ------------
  # Only the selected contrast years are read out of the 700 MB/crop daily
  # files - see read_daily_crop_production() for why that filter is applied
  # inside the reader rather than after.
  tar_target(daily_crop_production,
             import_daily_crop_production(raw_data_dir, keep_years = unique(contrast_years$year))),
  tar_target(fig_03_data, prepare_fig_03_data(daily_crop_production, contrast_years)),
  tar_target(fig_03_plot, plot_fig_03(fig_03_data, contrast_years)),
  tar_target(
    fig_03_saved,
    save_manuscript_plot(
      fig_03_plot, "Fig_03_seasonal_biomass_drought.png", figures_main_dir,
      width = figure_specs$fig_03$width, height = figure_specs$fig_03$height
    ),
    format = "file"
  ),
  tar_target(fig_03_csv, {
    path <- here::here("outputs", "figure_data", "Fig_03_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_03_data, path); path
  }, format = "file"),

  # --- Manuscript Figure 4: soil water during critical dry periods ---------
  tar_target(crop_years_all, crop_years_all_rotations(harvest_annual)),
  tar_target(critical_dry_periods, select_critical_dry_periods(spei_monthly, crop_years_all)),
  tar_target(fig_04_data, prepare_fig_04_data(field_water_daily, critical_dry_periods)),
  tar_target(fig_04_plot, plot_fig_04(fig_04_data)),
  tar_target(
    fig_04_saved,
    save_manuscript_plot(
      fig_04_plot, "Fig_04_soil_water_dry_periods.png", figures_main_dir,
      width = figure_specs$fig_04$width, height = figure_specs$fig_04$height
    ),
    format = "file"
  ),
  tar_target(fig_04_csv, {
    path <- here::here("outputs", "figure_data", "Fig_04_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_04_data, path); path
  }, format = "file"),

  # --- Manuscript Figure 5: nitrogen fluxes and leaching -------------------
  tar_target(field_n_apr, prepare_generic_outcomes(get_imported_object(nwaps_imported, "field_n_apr"))),
  tar_target(n_annual, prepare_n_annual(field_n_apr)),
  tar_target(n_flux_summary, summarise_n_fluxes(n_annual)),
  tar_target(fig_05_a_data, prepare_fig_05_a_data(n_flux_summary)),
  tar_target(fig_05_b_data, prepare_fig_05_b_data(n_flux_summary)),
  tar_target(fig_05_plot, plot_fig_05(fig_05_a_data, fig_05_b_data)),
  tar_target(
    fig_05_saved,
    save_manuscript_plot(
      fig_05_plot, "Fig_05_nitrogen_fluxes_leaching.png", figures_main_dir,
      width = figure_specs$fig_05$width, height = figure_specs$fig_05$height
    ),
    format = "file"
  ),
  tar_target(fig_05_a_csv, {
    path <- here::here("outputs", "figure_data", "Fig_05_a_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_05_a_data, path); path
  }, format = "file"),
  tar_target(fig_05_b_csv, {
    path <- here::here("outputs", "figure_data", "Fig_05_b_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_05_b_data, path); path
  }, format = "file"),

  # --- Manuscript Figure 6: Total soil organic carbon ----------------------
  # September snapshots (post-harvest, pre-tillage). Total SOC = SOM1+SOM2+SOM3.
  tar_target(som_to30_sep, prepare_generic_outcomes(get_imported_object(nwaps_imported, "som_to30_sep"))),
  tar_target(som_to60_sep, prepare_generic_outcomes(get_imported_object(nwaps_imported, "som_to60_sep"))),
  tar_target(som_from60_sep, prepare_generic_outcomes(get_imported_object(nwaps_imported, "som_from60_sep"))),
  tar_target(soc_by_depth, prepare_soc_by_depth(som_to30_sep, som_to60_sep, som_from60_sep)),
  tar_target(soc_relative_change, compute_soc_relative_change(soc_by_depth)),
  tar_target(fig_06_a_data, prepare_fig_06_a_data(soc_relative_change)),
  tar_target(fig_06_b_data, prepare_fig_06_b_data(soc_relative_change)),
  tar_target(fig_06_plot, plot_fig_06(fig_06_a_data, fig_06_b_data)),
  tar_target(
    fig_06_saved,
    save_manuscript_plot(
      fig_06_plot, "Fig_06_total_soc_response.png", figures_main_dir,
      width = figure_specs$fig_06$width, height = figure_specs$fig_06$height
    ),
    format = "file"
  ),
  tar_target(fig_06_a_csv, {
    path <- here::here("outputs", "figure_data", "Fig_06_a_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_06_a_data, path); path
  }, format = "file"),
  tar_target(fig_06_b_csv, {
    path <- here::here("outputs", "figure_data", "Fig_06_b_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_06_b_data, path); path
  }, format = "file"),

  # --- Manuscript Figure 7: yield-N-leaching trade-off (synthesis) ---------
  # Crosses domains: reuses system_asy (productivity) and n_flux_summary
  # (nitrogen) rather than re-deriving either.
  tar_target(grain_asy, calculate_system_asy(harvest_annual, "grain_MgDM_ha", crops = crop_levels_grain)),
  tar_target(fig_07_a_data, prepare_fig_07_a_data(system_asy, n_flux_summary)),
  tar_target(fig_07_b_data, prepare_fig_07_b_data(grain_asy, n_flux_summary)),
  tar_target(fig_07_plot, plot_fig_07(fig_07_a_data, fig_07_b_data)),
  tar_target(
    fig_07_saved,
    save_manuscript_plot(
      fig_07_plot, "Fig_07_yield_nitrogen_tradeoff.png", figures_main_dir,
      width = figure_specs$fig_07$width, height = figure_specs$fig_07$height
    ),
    format = "file"
  ),
  tar_target(fig_07_a_csv, {
    path <- here::here("outputs", "figure_data", "Fig_07_a_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_07_a_data, path); path
  }, format = "file"),
  tar_target(fig_07_b_csv, {
    path <- here::here("outputs", "figure_data", "Fig_07_b_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_07_b_data, path); path
  }, format = "file"),

  # --- Figure 8 (main-text candidate): three-domain trade-off --------------
  # Companion to Figure 7. Also produces the win-win table, which is what
  # actually tests the Results 3.3 "no scenario improved all three" claim.
  tar_target(three_domain_data,
             prepare_three_domain_tradeoff(fig_07_a_data, soc_relative_change, field_water_sep)),
  tar_target(win_win_scenarios, find_win_win_scenarios(three_domain_data)),
  tar_target(fig_08_plot, plot_fig_three_domain(three_domain_data, win_win_scenarios)),
  tar_target(
    fig_08_saved,
    save_manuscript_plot(
      fig_08_plot, "Fig_08_three_domain_tradeoff.png", figures_main_dir,
      width = figure_specs$fig_08$width, height = figure_specs$fig_08$height
    ),
    format = "file"
  ),
  tar_target(fig_08_csv, {
    path <- here::here("outputs", "figure_data", "Fig_08_three_domain_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(three_domain_data, path); path
  }, format = "file"),

  # --- Supplementary figures ------------------------------------------------
  # S6/S7: the wind mechanism. Daily SWater is the only source of separated
  # transpiration; the .dwf weather files are the only source of wind speed
  # (an input to the simulations, so absent from every NWAPS output).
  tar_target(daily_swater, read_daily_swater(raw_data_dir)),
  tar_target(openfield_weather_daily,
             summarise_daily_weather(read_daisy_weather(
               file.path(weather_dir, "weather_baseline_substrip.dwf")))),
  # The VAPV centre WIND reference must come from the wind-scenario file.
  # weather_substrip_center_rad0.dwf is the radiation 0-level: it carries the
  # modelled between-panel radiation but leaves wind at its open-field values,
  # so reading wind from it returns the open-field speed and makes the two
  # reference lines in S6a coincide.
  tar_target(vapv_weather_daily,
             summarise_daily_weather(read_daisy_weather(
               file.path(weather_dir, "weather_substrip_center_win0.dwf")))),

  tar_target(fig_s06_a_data, prepare_fig_s06_a_data(openfield_weather_daily, vapv_weather_daily)),
  tar_target(fig_s06_b_data, prepare_fig_s06_b_data(daily_swater)),
  tar_target(fig_s06_c_data, prepare_fig_s06_c_data(harvest_annual)),
  tar_target(fig_s06_plot, plot_fig_s06(fig_s06_a_data, fig_s06_b_data, fig_s06_c_data)),
  tar_target(
    fig_s06_saved,
    save_manuscript_plot(
      fig_s06_plot, "Fig_S06_wind_hydrological_not_agronomic.png", figures_supp_dir,
      width = figure_specs$fig_s06$width, height = figure_specs$fig_s06$height
    ),
    format = "file"
  ),

  tar_target(daily_pf, read_daily_pf(raw_data_dir, keep_years = s08_crop_years)),
  tar_target(fig_s07_a_data, prepare_fig_s07_a_data(field_water_daily, daily_pf, daily_crop_production_wind)),
  tar_target(fig_s07_b_data, prepare_fig_s07_b_data(daily_swater, openfield_weather_daily)),
  tar_target(fig_s07_c_data, prepare_fig_s07_c_data(harvest_annual)),
  tar_target(fig_s07_plot, plot_fig_s07(fig_s07_a_data, fig_s07_b_data, fig_s07_c_data)),
  tar_target(
    fig_s07_saved,
    save_manuscript_plot(
      fig_s07_plot, "Fig_S07_wind_vs_radiation_temperature.png", figures_supp_dir,
      width = figure_specs$fig_s07$width, height = figure_specs$fig_s07$height
    ),
    format = "file"
  ),
  # S8: canopy size and cumulative water use, dose-response by driver.
  # Winter wheat 2018 and soybean 2013 - the critical dry seasons used in the
  # draft. Read across every Centre scenario, but only those two crop-years, so
  # the 700 MB/crop files resolve to a few thousand rows.
  # Winter wheat 2018 and soybean 2022 - both dry seasons that exist in
  # Rotation 1 (see prepare_fig_s08_data for why not the draft's SY 2013).
  tar_target(s08_crop_years, c(2018L, 2022L)),
  tar_target(daily_crop_production_wind,
             import_daily_crop_production(
               raw_data_dir, keep_years = s08_crop_years, keep_weather = NULL,
               crops = c("Winter Wheat", "Soybean"))),
  tar_target(daily_swater_s08, read_daily_swater(raw_data_dir, keep_years = s08_crop_years)),
  tar_target(fig_s08_data, prepare_fig_s08_data(daily_crop_production_wind, daily_swater_s08)),
  tar_target(fig_s08_plot, plot_fig_s08(fig_s08_data)),
  tar_target(
    fig_s08_saved,
    save_manuscript_plot(
      fig_s08_plot, "Fig_S08_canopy_and_water_use_dose_response.png", figures_supp_dir,
      width = figure_specs$fig_s08$width, height = figure_specs$fig_s08$height
    ),
    format = "file"
  ),

  # S10: continuous SOC dynamics with a crop-calendar overlay. Weekly OM
  # resolution shows the within-year sawtooth that the annual September
  # snapshots behind Figure 6 cannot.
  tar_target(weekly_om_to30, read_weekly_om(raw_data_dir)),
  tar_target(fig_s10_data, prepare_fig_s10_data(weekly_om_to30)),
  tar_target(daily_crop_production_calendar,
             import_daily_crop_production(raw_data_dir, keep_years = 1998:2024,
                                           keep_weather = "weatherBaselineopen")),
  tar_target(crop_calendar, prepare_crop_calendar(daily_crop_production_calendar)),
  tar_target(fig_s10_plot, plot_fig_s10(fig_s10_data, crop_calendar)),
  tar_target(
    fig_s10_saved,
    save_manuscript_plot(
      fig_s10_plot, "Fig_S10_soc_dynamics_crop_calendar.png", figures_supp_dir,
      width = figure_specs$fig_s10$width, height = figure_specs$fig_s10$height
    ),
    format = "file"
  ),

  # S1: open-field weather forcing and the crop calendar it drove.
  tar_target(fig_s01_weather, prepare_fig_s01_weather(openfield_weather_daily)),
  tar_target(fig_s01_calendar, prepare_fig_s01_calendar(crop_calendar)),
  tar_target(fig_s01_plot, plot_fig_s01(fig_s01_weather, fig_s01_calendar)),
  tar_target(
    fig_s01_saved,
    save_manuscript_plot(
      fig_s01_plot, "Fig_S01_weather_and_crop_calendar.png", figures_supp_dir,
      width = figure_specs$fig_s01$width, height = figure_specs$fig_s01$height
    ),
    format = "file"
  ),

  # S4: soybean parameterisation diagnostic (final version only).
  tar_target(fig_s04_data, prepare_fig_s04_data(harvest_annual)),
  tar_target(fig_s04_season, prepare_fig_s04_season(daily_crop_production_wind)),
  tar_target(fig_s04_plot, plot_fig_s04(fig_s04_data, fig_s04_season)),
  tar_target(
    fig_s04_saved,
    save_manuscript_plot(
      fig_s04_plot, "Fig_S04_soybean_parameterisation_diagnostic.png", figures_supp_dir,
      width = figure_specs$fig_s04$width, height = figure_specs$fig_s04$height
    ),
    format = "file"
  ),

  # S16-S18: additional material supporting text claims that currently have
  # no figure - the rotation composition behind the ASY headline, the N budget
  # the flux discussion sits in, and the permutation-vs-scenario comparison
  # asserted in the Discussion.
  tar_target(fig_s16_data, prepare_fig_s16_data(harvest_annual)),
  tar_target(fig_s16_plot, plot_fig_s16(fig_s16_data)),
  tar_target(
    fig_s16_saved,
    save_manuscript_plot(
      fig_s16_plot, "Fig_S16_rotation_yield_composition.png", figures_supp_dir,
      width = figure_specs$fig_s16$width, height = figure_specs$fig_s16$height
    ),
    format = "file"
  ),
  tar_target(fig_s17_data, prepare_fig_s17_data(n_annual)),
  tar_target(fig_s17_plot, plot_fig_s17(fig_s17_data)),
  tar_target(
    fig_s17_saved,
    save_manuscript_plot(
      fig_s17_plot, "Fig_S17_nitrogen_budget.png", figures_supp_dir,
      width = figure_specs$fig_s17$width, height = figure_specs$fig_s17$height
    ),
    format = "file"
  ),
  tar_target(fig_s18_data, prepare_fig_s18_data(harvest_annual)),
  tar_target(fig_s18_plot, plot_fig_s18(fig_s18_data)),
  tar_target(
    fig_s18_saved,
    save_manuscript_plot(
      fig_s18_plot, "Fig_S18_permutation_vs_scenario_effect.png", figures_supp_dir,
      width = figure_specs$fig_s18$width, height = figure_specs$fig_s18$height
    ),
    format = "file"
  ),
  tar_target(fig_s18_csv, {
    path <- here::here("outputs", "figure_data", "Fig_S18_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_s18_data, path); path
  }, format = "file"),

  # S5: grass-clover calibration. Reads artefacts from the separate
  # GC_AGB_SA_AND_OPT framework as fixed inputs - those runs are not
  # reproduced by this pipeline.
  tar_target(gc_calibration, read_gc_calibration(gc_calibration_dir)),
  tar_target(fig_s05_data, prepare_fig_s05_data(gc_calibration)),
  tar_target(fig_s05_plot, plot_fig_s05(fig_s05_data)),
  tar_target(
    fig_s05_saved,
    save_manuscript_plot(
      fig_s05_plot, "Fig_S05_grass_clover_calibration.png", figures_supp_dir,
      width = figure_specs$fig_s05$width, height = figure_specs$fig_s05$height
    ),
    format = "file"
  ),

  # S11: Total SOC by depth layer - absolute stocks and all three layers,
  # both of which Figure 6 omits.
  tar_target(fig_s11_a_data, prepare_fig_s11_a_data(soc_by_depth)),
  tar_target(fig_s11_b_data, prepare_fig_s11_b_data(soc_relative_change)),
  tar_target(fig_s11_plot, plot_fig_s11(fig_s11_a_data, fig_s11_b_data)),
  tar_target(
    fig_s11_saved,
    save_manuscript_plot(
      fig_s11_plot, "Fig_S11_total_soc_by_depth.png", figures_supp_dir,
      width = figure_specs$fig_s11$width, height = figure_specs$fig_s11$height
    ),
    format = "file"
  ),
  tar_target(fig_s11_b_csv, {
    path <- here::here("outputs", "figure_data", "Fig_S11_b_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_s11_b_data, path); path
  }, format = "file"),

  # S13: canonical simulation index (FAIR artefact; Methods cite 416 runs but
  # no list exists).
  tar_target(simulation_index, build_simulation_index(harvest_annual)),
  tar_target(simulation_index_csv, {
    path <- here::here("outputs", "tables", "supplementary", "Table_S13_simulation_index.csv")
    fs::dir_create(dirname(path)); readr::write_csv(simulation_index, path); path
  }, format = "file"),
  tar_target(simulation_index_summary, summarise_simulation_index(simulation_index)),

  # S14: full annual water balance - settles the percolation vs drainage
  # question raised in review.
  tar_target(field_water_sep, prepare_generic_outcomes(get_imported_object(nwaps_imported, "field_water_sep"))),
  tar_target(fig_s14_data, prepare_fig_s14_data(field_water_sep)),
  tar_target(fig_s14_plot, plot_fig_s14(fig_s14_data)),
  tar_target(
    fig_s14_saved,
    save_manuscript_plot(
      fig_s14_plot, "Fig_S14_annual_water_balance_components.png", figures_supp_dir,
      width = figure_specs$fig_s14$width, height = figure_specs$fig_s14$height
    ),
    format = "file"
  ),
  tar_target(fig_s14_csv, {
    path <- here::here("outputs", "figure_data", "Fig_S14_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_s14_data, path); path
  }, format = "file"),

  # S15: substrip West/Centre/East across all three outcome domains.
  tar_target(fig_s15_data, prepare_fig_s15_data(harvest_annual, n_annual, soc_relative_change)),
  tar_target(fig_s15_plot, plot_fig_s15(fig_s15_data)),
  tar_target(
    fig_s15_saved,
    save_manuscript_plot(
      fig_s15_plot, "Fig_S15_substrip_position_effects.png", figures_supp_dir,
      width = figure_specs$fig_s15$width, height = figure_specs$fig_s15$height
    ),
    format = "file"
  ),
  tar_target(fig_s15_csv, {
    path <- here::here("outputs", "figure_data", "Fig_S15_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_s15_data, path); path
  }, format = "file"),

  tar_target(fig_s06_b_csv, {
    path <- here::here("outputs", "figure_data", "Fig_S06_b_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_s06_b_data, path); path
  }, format = "file"),
  tar_target(fig_s07_c_csv, {
    path <- here::here("outputs", "figure_data", "Fig_S07_c_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_s07_c_data, path); path
  }, format = "file"),

  # S9: N fluxes across all management regimes (Fig 5 shows Dig-Rem only)
  tar_target(fig_s09_a_data, prepare_fig_s09_a_data(n_flux_summary)),
  tar_target(fig_s09_plot, plot_fig_s09(fig_s09_a_data, fig_05_b_data)),
  tar_target(
    fig_s09_saved,
    save_manuscript_plot(
      fig_s09_plot, "Fig_S09_nitrogen_fluxes_all_managements.png", figures_supp_dir,
      width = figure_specs$fig_s09$width, height = figure_specs$fig_s09$height
    ),
    format = "file"
  ),
  tar_target(fig_s09_csv, {
    path <- here::here("outputs", "figure_data", "Fig_S09_data.csv")
    fs::dir_create(dirname(path)); readr::write_csv(fig_s09_a_data, path); path
  }, format = "file"),

  # S12: yield-SOC trade-off. Also produces the OLS slope table that settles
  # the contradictory values quoted in Results 3.3 vs Discussion 4.5.
  tar_target(fig_s12_a_data, prepare_fig_s12_data(fig_07_a_data, soc_relative_change)),
  tar_target(fig_s12_b_data, prepare_fig_s12_data(fig_07_b_data, soc_relative_change)),
  tar_target(yield_soc_slopes,
             dplyr::bind_rows(fit_yield_soc_slopes(fig_s12_a_data),
                              fit_yield_soc_slopes(fig_s12_b_data))),
  tar_target(fig_s12_plot, plot_fig_s12(fig_s12_a_data, fig_s12_b_data, yield_soc_slopes)),
  tar_target(
    fig_s12_saved,
    save_manuscript_plot(
      fig_s12_plot, "Fig_S12_yield_soc_tradeoff.png", figures_supp_dir,
      width = figure_specs$fig_s12$width, height = figure_specs$fig_s12$height
    ),
    format = "file"
  ),
  tar_target(yield_soc_slopes_csv, {
    path <- here::here("outputs", "figure_data", "yield_soc_slopes.csv")
    fs::dir_create(dirname(path)); readr::write_csv(yield_soc_slopes, path); path
  }, format = "file"),

  # --- Retired legacy-numbered figures (not manuscript figures) ------------
  tar_target(fig04_data, prepare_fig04_data(harvest_annual)),
  tar_target(fig04_plot, plot_fig04(fig04_data)),
  tar_target(
    fig04_saved,
    save_manuscript_plot(
      fig04_plot, "Figure_04_heatmap_ASY_AGB_per_crop.png", figures_main_dir,
      width = figure_specs$fig04$width, height = figure_specs$fig04$height
    ),
    format = "file"
  ),
  tar_target(fig04_data_csv, {
    path <- here::here("outputs", "figure_data", "Fig_04_data.csv")
    fs::dir_create(dirname(path))
    readr::write_csv(fig04_data, path)
    path
  }, format = "file"),

  tar_target(fig05_data, prepare_fig05_data(harvest_annual)),
  tar_target(fig05_plot, plot_fig05(fig05_data)),
  tar_target(
    fig05_saved,
    save_manuscript_plot(
      fig05_plot, "Figure_05_relative_grain_yield.png", figures_main_dir,
      width = figure_specs$fig05$width, height = figure_specs$fig05$height
    ),
    format = "file"
  ),
  tar_target(fig05_data_csv, {
    path <- here::here("outputs", "figure_data", "Fig_05_data.csv")
    fs::dir_create(dirname(path))
    readr::write_csv(fig05_data, path)
    path
  }, format = "file")
)

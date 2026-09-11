source("renv/activate.R")
library(targets)
library(dplyr)

tar_load(fig04_data)

legacy <- readr::read_csv(
  "tests/fixtures/legacy_baseline/TABLES_PUBLICATION_READY/17K_crop_total_AGB_relative_ASY_4rotation_average.csv",
  show_col_types = FALSE
)

cat("New fig04_data columns:\n"); print(names(fig04_data))
cat("\nLegacy fixture columns:\n"); print(names(legacy))
cat("\nLegacy rows:", nrow(legacy), " New rows:", nrow(fig04_data), "\n")

join_keys <- c("fertilisation", "residue", "crop_renamed", "weather_factor", "weather_signed_change_pct", "strip_position")
join_keys <- intersect(join_keys, intersect(names(fig04_data), names(legacy)))
cat("\nJoin keys used:", paste(join_keys, collapse = ", "), "\n")

new_small <- fig04_data |> select(all_of(join_keys), relative_effect_pct_new = relative_effect_pct)
legacy_small <- legacy |> select(all_of(join_keys), relative_effect_pct_legacy = relative_effect_pct)

compared <- inner_join(new_small, legacy_small, by = join_keys) |>
  mutate(
    abs_diff = abs(relative_effect_pct_new - relative_effect_pct_legacy),
    rel_diff = abs_diff / pmax(abs(relative_effect_pct_legacy), 1e-6)
  )

cat("\nRows matched by join:", nrow(compared), "of", nrow(new_small), "new /", nrow(legacy_small), "legacy\n")
cat("Max absolute difference:", max(compared$abs_diff, na.rm = TRUE), "\n")
cat("Mean absolute difference:", mean(compared$abs_diff, na.rm = TRUE), "\n")
cat("Rows with abs_diff > 0.01:", sum(compared$abs_diff > 0.01, na.rm = TRUE), "\n")
cat("Rows with NA in either:", sum(is.na(compared$relative_effect_pct_new) | is.na(compared$relative_effect_pct_legacy)), "\n")

readr::write_csv(compared, "validation/fig04_numeric_comparison.csv")
cat("\nWorst 10 rows by abs_diff:\n")
print(compared |> arrange(desc(abs_diff)) |> head(10), width = 200)

# Validation fixtures

`tests/fixtures/legacy_baseline/` (not committed - see `.gitignore`) holds
ground-truth output copied from the legacy project's last successful run, used
to check the reconstructed pipeline's output against it. Regenerate with:

```powershell
$legacy = "D:\Kamau\DAISY_modelling_agrivoltaics"
$new = "D:\Kamau\vapv-daisy-microclimate-scenarios"
Copy-Item "$legacy\TABLES\PUBLICATION_READY" "$new\tests\fixtures\legacy_baseline\TABLES_PUBLICATION_READY" -Recurse -Force
Copy-Item "$legacy\OUTPUT_VISUALISATIONS\PUBLICATION_READY" "$new\tests\fixtures\legacy_baseline\OUTPUT_VISUALISATIONS_PUBLICATION_READY" -Recurse -Force
Copy-Item "$legacy\SPAWN_NWAPS_DAISY\OUTPUT_VISUALISATIONS\MANUSCRIPT_FIGURES\FULL\RESULTS" "$new\tests\fixtures\legacy_baseline\MANUSCRIPT_FIGURES_RESULTS" -Recurse -Force
```

As each domain (water, nitrogen, SOC, trade-offs) and figure gets ported, add
its specific legacy fixture files to `tests/fixtures/` (small, curated,
tracked in git) rather than relying on the full local-only copy above.

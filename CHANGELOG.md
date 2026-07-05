# Changelog

All versions of *Did Slavery Impede the Growth of American Capitalism?
Two Failed Natural Experiments*. Superseded versions are preserved under
`archive/`.

## v6.1 — July 5, 2026

Paper and replication package revised together. The current package
lives in `replication/`.

- **Outcome definition corrected.** Farm values are now the value of
  land and buildings in every census year, as the table notes state.
  Previously `01_Database.R` also summed farm implements and livestock
  into the 1880–1900 totals. This affects the 1880–1900 cells of the
  event study (Table 1), the 1880–1900 rows of the spatial RDD
  (Table 2), Figure 1, and the 1900 map.
- **RDD outcome switched from inverse hyperbolic sine to log**, matching
  the table notes (substantively equivalent: no zero farm values are
  reachable by the RDD sample).
- **Covariate adjustment now uses `rd2d` 1.0.0's native `covs.eff`
  option.** The bespoke covariate-adjusted estimator
  (`rd2d_covariate_adjusted.R`) is removed.
- **Geographic covariates pinned in the package**
  (`Data/Geology_v2/`): elevation and slope from USGS 3DEP/NED
  (1 arc-second) and clay, sand, and silt from CONUS-SOIL (Miller &
  White 1998), replacing live downloads from SRTM15+ (OpenTopography)
  and SoilGrids, whose upstream revisions changed results between runs.
  Tables 2 and 3 now reproduce without an OpenTopography key.
- **Ruggedness (TRI) dropped from the covariate set:** on the 30 m NED
  grid it is a near-duplicate of slope (county-level correlation
  0.997), which destabilized the covariate adjustment.
- **Balance tests (Table 3) computed at the outcome regression's
  bandwidths** rather than per-covariate bandwidths.
- All tables, figures, and maps regenerated from the corrected
  pipeline; final verification run July 3, 2026.
- **Repository restructured:** current paper in the root, current
  replication package in `replication/`, superseded versions in
  `archive/`, this changelog added.

## v6.0 — May 25, 2026

- Major revision of the paper.
- First version released with a full replication package in this
  repository: a single `Master.R` pipeline (NHGIS download → database →
  normalization → event study → spatial RDD → geopackages for the
  maps). Preserved in full at `archive/v6.0/`.
- Earlier versions had been PDF-only here, with working materials in a
  separate repository.

## v5.0 — June 23, 2025

- Paper revision (PDF-only release; `archive/v5.0/`).

## v4.0 — June 10, 2025

- Paper revision (PDF-only release; `archive/v4.0/`).

## v3.3 — May 26, 2025

- Paper revision (PDF-only release; `archive/v3.3/`).

## v3.1 — April 24, 2025

- First version published in this repository (uploaded May 7, 2025;
  PDF-only release; `archive/v3.1/`).

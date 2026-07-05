# Replication: Did Slavery Impede the Growth of American Capitalism?

Replication materials for Joseph Francis, *Did Slavery Impede the Growth of
American Capitalism? Two Failed Natural Experiments Using Farm Values per
Acre* (v6.1, 2026).

The pipeline produces:

- **Table 1** — Event study of abolition (`Output/Tables/Table_1_Event_Study.txt`)
- **Figure 1** — Event-study coefficient plot (`Output/Figures/Figure_1_Event_Study.pdf`)
- **Table 2** — Spatial RDD for farm values per acre (`Output/Tables/Table_2_RDD_Outcomes.csv`)
- **Table 3** — Balance tests for geographic covariates (`Output/Tables/Table_3_Balance_Tests.csv`)
- **Maps 1 and 2** — Farm values per acre in 1860 and 1900, rendered manually
  in QGIS from the geopackages written to `Output/Maps/`

## Requirements

- **R 4.2 or later** (tested on 4.4.2).
- **An IPUMS API key** for the NHGIS extract.
  Register at <https://account.ipums.org/api_keys>.
- **Roughly 5 GB of free disk space** for downloaded data.
- **Peak memory: ~30–40 GB RAM** during step 3 (`02_Normalization.R`), driven
  by the 1880 cotton-yield rasterization. The rest of the pipeline runs in a
  few GB. On a machine with less memory, the script will swap and become
  very slow.

Set the key in your environment or place it in a `.env` file in this
directory (`replication/`):

```
IPUMS_API_KEY=your_ipums_key_here
```

The scripts will prompt for a missing key the first time they run.

## Quick start

Run from this `replication/` directory:

```sh
# install package dependencies
Rscript install_dependencies.R

# run the full pipeline
Rscript Master.R
```

Run a single step with `Rscript Master.R --only=5` (event study) or pick up
from a step with `--from=3` (normalization onward). Use `--list` to see the
step numbers.

## Pipeline

| Step | Script                                | Inputs                                | Outputs                                                    |
|------|---------------------------------------|---------------------------------------|-------------------------------------------------------------|
| 1    | `Scripts/00_Download_NHGIS.R`         | IPUMS API                              | `Data/census.csv`, `Data/Shapefiles/{year}/`                |
| 2    | `Scripts/01_Database.R`               | `Data/census.csv`                      | `Data/database.csv`                                         |
| 3    | `Scripts/02_Normalization.R`          | `Data/database.csv`, shapefiles        | `Data/panel_data.csv`                                       |
| 4    | `Scripts/05_Make_Geopackages.R`       | `Data/database.csv`, shapefiles        | `Output/Maps/farmv_acre_{year}.gpkg`                        |
| 5    | `Scripts/03_Analysis_Event_Study.R`   | `Data/panel_data.csv`                  | `Output/Tables/Table_1_Event_Study.txt`, `Output/Figures/Figure_1_Event_Study.pdf` |
| 6    | `Scripts/04_Analysis_Spatial_RDD.R`   | `Data/database.csv`, `Data/Geology_v2/` (shipped), `Data/Border/1820_border/` | `Output/Tables/Table_2_RDD_Outcomes.csv`, `Output/Tables/Table_3_Balance_Tests.csv`, `Output/Tables/RDD_Combined_Output.txt` |

Step 6 uses the native covariate-adjustment option (`covs.eff`) added in
`rd2d` 1.0.0 (Cattaneo, Titiunik & Yu). Covariates enter with a common
adjustment coefficient across treatment sides, and bandwidth selection,
bias correction, and inference are all carried out by the package on the
covariate-adjusted estimator. **`rd2d` 1.0.0 or later is required**; the
script stops with an informative error on older versions.

## Data availability

This repository redistributes two data components:

- **`Data/Border/1820_border/`** — the free-slave state border shapefile
  used as the boundary for the spatial RDD. Hand-built by the author.
- **`Data/Geology_v2/`** — county means of the geographic covariates used
  in the spatial RDD and balance tests: elevation and slope from the USGS
  3DEP / National Elevation Dataset (1 arc-second), and clay, sand, and
  silt fractions of the 0–30 cm soil profile from CONUS-SOIL (Miller &
  White 1998, derived from USDA STATSGO; non-soil cells such as water
  bodies are masked before county averaging). Both sources are US
  public-domain; the county-level CSVs (~3 MB) are committed so the
  analysis reproduces exactly, without exposure to upstream API drift.
  To rebuild them from the raw rasters, see `Scripts/00_Build_Geology_v2.R`
  (a multi-hour build: NED tiles are fetched via `FedData`, and the
  CONUS-SOIL grids must be downloaded from the Penn State Earth System
  Science Center archive into `FV_SOIL_DIR`; the script's header documents
  the details).

All other inputs are downloaded by the pipeline:

- **NHGIS** (county-level census tables and shapefiles, 1820–1900). Manson,
  Schroeder, et al., *IPUMS National Historical Geographic Information
  System*, accessed via the IPUMS API. **Not redistributed** per the IPUMS
  Terms of Use; download requires a free IPUMS API key.

## Maps

Maps 1 and 2 in the paper are produced manually in QGIS:

1. Run the pipeline at least through step 4.
2. Open `Output/Maps/farmv_acre_1860.gpkg` (for Map 1) or
   `farmv_acre_1900.gpkg` (for Map 2) in QGIS.
3. Classify the `farmv_acre` attribute into deciles using a quantile
   classifier.
4. Overlay `Data/Border/1820_border/1820_border.shp` as a white line.

## Reproducibility checklist

A canonical run on a clean machine should produce:

| Output | Format | Notes |
|---|---|---|
| `Output/Tables/Table_1_Event_Study.txt` | text | TWFE event-study coefficients |
| `Output/Figures/Figure_1_Event_Study.pdf` | PDF | event-study coefficient plot |
| `Output/Tables/Table_2_RDD_Outcomes.csv` | CSV | spatial RDD across 4 specs × 6 years |
| `Output/Tables/Table_3_Balance_Tests.csv` | CSV | balance tests for 5 covariates × 6 years |
| `Output/Tables/RDD_Combined_Output.txt` | text | formatted summary of Tables 2 and 3 |
| `Output/Maps/farmv_acre_{1860,1900}.gpkg` | GeoPackage | inputs to QGIS for Maps 1 and 2 |

Run times from the reference run (Intel iMac, R 4.4.2): step 3, the
areal-interpolation bottleneck, took ~3.7 hours; step 6 ~3.5 minutes;
every other step under a minute. Machines with more memory will be
faster on step 3. End-to-end runtime including downloads also depends
on network speed.

## How to cite

Please cite the paper:

> Francis, Joseph (2026). "Did Slavery Impede the Growth of American
> Capitalism? Two Failed Natural Experiments Using Farm Values per Acre."

For the replication package itself, see `CITATION.cff`.

## License

MIT. See `LICENSE`.

## Contact

Joseph Francis — joefrancis505@gmail.com

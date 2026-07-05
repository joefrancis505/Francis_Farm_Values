# Replication: Did Slavery Impede the Growth of American Capitalism?

Replication materials for Joseph Francis, *Did Slavery Impede the Growth of
American Capitalism? Two Failed Natural Experiments* (v6.0, 2026).

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
- **An OpenTopography API key** for the SRTM15+ digital elevation model.
  Register at <https://portal.opentopography.org/myopentopo>.
- **Roughly 5 GB of free disk space** for downloaded data.
- **Peak memory: ~30–40 GB RAM** during step 4 (`02_Normalization.R`), driven
  by the 1880 cotton-yield rasterization. The rest of the pipeline runs in a
  few GB. On a machine with less memory, the script will swap and become
  very slow.

Set the keys in your environment or place them in a `.env` file at the
repository root:

```
IPUMS_API_KEY=your_ipums_key_here
OPENTOPO_KEY=your_opentopography_key_here
```

The scripts will prompt for any missing key the first time they run.

## Quick start

```sh
# install package dependencies
Rscript install_dependencies.R

# run the full pipeline
Rscript Master.R
```

Run a single step with `Rscript Master.R --only=6` (event study) or pick up
from a step with `--from=4` (normalization onward). Use `--list` to see the
step numbers.

## Pipeline

| Step | Script                                | Inputs                                | Outputs                                                    |
|------|---------------------------------------|---------------------------------------|-------------------------------------------------------------|
| 1    | `Scripts/00_Download_NHGIS.R`         | IPUMS API                              | `Data/census.csv`, `Data/Shapefiles/{year}/`                |
| 2    | `Scripts/00_Download_Geology.R`       | OpenTopography, SoilGrids              | `Data/Geology/`                                             |
| 3    | `Scripts/01_Database.R`               | `Data/census.csv`                      | `Data/database.csv`                                         |
| 4    | `Scripts/02_Normalization.R`          | `Data/database.csv`, shapefiles        | `Data/panel_data.csv`                                       |
| 5    | `Scripts/05_Make_Geopackages.R`       | `Data/database.csv`, shapefiles        | `Output/Maps/farmv_acre_{year}.gpkg`                        |
| 6    | `Scripts/03_Analysis_Event_Study.R`   | `Data/panel_data.csv`                  | `Output/Tables/Table_1_Event_Study.txt`, `Output/Figures/Figure_1_Event_Study.pdf` |
| 7    | `Scripts/04_Analysis_Spatial_RDD.R`   | `Data/database.csv`, `Data/Geology/`, `Data/Border/1820_border/` | `Output/Tables/Table_2_RDD_Outcomes.csv`, `Output/Tables/Table_3_Balance_Tests.csv`, `Output/Tables/RDD_Combined_Output.txt` |

The `Scripts/rd2d_covariate_adjusted.R` helper implements the
covariate-adjusted boundary-RD estimator used in step 7. It extends
`rd2d` (Cattaneo, Titiunik & Yu 2025) via the partialling-out approach
of Calonico et al. (2019), with both bias and variance constants
recomputed on the partialled-out outcome at each evaluation point
(the boundary-RD analogue of Calonico's 1D procedure).

## Data availability

This repository redistributes a single hand-built data file:

- **`Data/Border/1820_border/`** — the free-slave state border shapefile
  used as the boundary for the spatial RDD. Hand-built by the author.

All other inputs are downloaded by the pipeline:

- **NHGIS** (county-level census tables and shapefiles, 1820–1900). Manson,
  Schroeder, et al., *IPUMS National Historical Geographic Information
  System*, accessed via the IPUMS API. **Not redistributed** per the IPUMS
  Terms of Use; download requires a free IPUMS API key.
- **SRTM15+** elevation, accessed through OpenTopography. Slope and
  ruggedness (TRI) are derived locally from elevation. Requires a free
  OpenTopography API key.
- **SoilGrids 2.0** (clay, sand, silt fractions of the 0–30 cm soil
  profile), ISRIC, accessed through the SoilGrids WCS endpoint. No key
  required.

## Maps

Maps 1 and 2 in the paper are produced manually in QGIS:

1. Run the pipeline at least through step 5.
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
| `Output/Tables/Table_3_Balance_Tests.csv` | CSV | balance tests for 6 covariates × 6 years |
| `Output/Tables/RDD_Combined_Output.txt` | text | formatted summary of Tables 2 and 3 |
| `Output/Maps/farmv_acre_{1860,1900}.gpkg` | GeoPackage | inputs to QGIS for Maps 1 and 2 |

Approximate run time on a 2024 Apple Silicon laptop (16 GB RAM, after
the ~5 GB download has completed): step 4 ~10 minutes (the bottleneck),
step 7 ~5 minutes, all other steps under 1 minute each. End-to-end
runtime including downloads depends on network speed.

## How to cite

Please cite the paper:

> Francis, Joseph (2026). "Did Slavery Impede the Growth of American
> Capitalism? Two Failed Natural Experiments Using Farm Values per Acre."

For the replication package itself, see `CITATION.cff`.

## License

MIT. See `LICENSE`.

## Contact

Joseph Francis — joefrancis505@gmail.com

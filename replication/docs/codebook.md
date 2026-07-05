# Codebook

Columns of the two intermediate data files produced by the pipeline.

## `Data/database.csv`

Written by `Scripts/01_Database.R`. One row per county-year of the
decennial agricultural census, 1820–1900, on 1850 county boundaries
where applicable.

| Column | Type | Units | Source / derivation |
|---|---|---|---|
| `GISJOIN` | string | — | NHGIS county identifier (stable across years on 1850 boundaries where harmonized) |
| `year` | integer | calendar year | census year (1820, 1830, ..., 1900) |
| `state` | string | — | state name from NHGIS |
| `STATEA` | string | — | NHGIS state code |
| `COUNTYA` | string | — | NHGIS county code |
| `census_pop` | numeric | persons | total population from the population census |
| `enslaved` | numeric | persons | enslaved population from the slave schedule (zero in free states; zero from 1870 onwards) |
| `farmv_total` | numeric | US dollars | aggregate value of farms (land and buildings) from the agricultural census |
| `improved` | numeric | acres | improved farm acreage from the agricultural census |
| `unimproved` | numeric | acres | unimproved farm acreage from the agricultural census |
| `cotton` | numeric | bales (400 lb) | cotton production from the agricultural census |
| `corn` | numeric | bushels | corn production from the agricultural census |
| `livestock_val` | numeric | US dollars | value of livestock from the agricultural census |
| `cotton_acreage` | numeric | acres | cotton acreage (1880 only; rasterized to 1850 boundaries in `02_Normalization.R`) |

## `Data/panel_data.csv`

Written by `Scripts/02_Normalization.R`. The 1820–1900 panel
harmonized to 1850 county boundaries by areal interpolation
(assuming uniform within-county distributions). Augmented with
derived variables for the event study.

| Column | Type | Units | Source / derivation |
|---|---|---|---|
| `DECADE` | integer | calendar year | census year |
| `NHGISNAM`, `NHGISST`, `NHGISCTY` | string | — | NHGIS metadata |
| `ICPSRST`, `ICPSRCTY`, `ICPSRNAM`, `ICPSRSTI`, `ICPSRCTYI`, `ICPSRFIP` | string | — | ICPSR identifiers |
| `STATE`, `COUNTY` | string | — | NHGIS state/county labels |
| `PID` | integer | — | panel identifier (1850 county) |
| `X_CENTROID`, `Y_CENTROID` | numeric | metres (projected) | county centroid in projected CRS |
| `GISJOIN`, `GISJOIN2` | string | — | NHGIS identifiers |
| `SHAPE_AREA` | numeric | square metres | polygon area |
| `SHAPE_LEN` | numeric | metres | polygon perimeter |
| `year`, `state`, `STATEA`, `COUNTYA` | — | — | census-year fields from `database.csv` |
| `census_pop`, `enslaved`, `farmv_total`, `improved`, `unimproved`, `cotton`, `corn`, `livestock_val`, `cotton_acreage` | — | — | census-year quantities from `database.csv`, interpolated to 1850 boundaries |
| `area` | numeric | acres | total county area in acres |
| `farmv` | numeric | US dollars per acre | `farmv_total / (improved + unimproved)` |
| `pc_enslaved` | numeric | percent | `100 * enslaved / census_pop` |
| `cotton_pc` | numeric | bales per capita | `cotton / census_pop` |
| `ccratio` | numeric | dimensionless | `cotton / corn` (zero where corn is zero) |
| `cotton_share` | numeric | bales per acre | `cotton / improved` |
| `farmv_na` | logical | — | TRUE where `farmv` is missing for the county-year |
| `longitude`, `latitude` | numeric | degrees | centroid coordinates in WGS84 |

## Output files

See `README.md` for descriptions of `Table_1_Event_Study.txt`,
`Table_2_RDD_Outcomes.csv`, `Table_3_Balance_Tests.csv`, and the QGIS
geopackages.

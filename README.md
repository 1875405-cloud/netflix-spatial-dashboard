# Netflix: A Spatial View

An interactive R Shiny dashboard that explores **where Netflix content is produced** and whether production clusters geographically. Built as an MSc Spatial Statistics group project (University of the Witwatersrand).

**Live dashboard:** _add the GitHub Pages link here once it is published_

## Questions and methods

| Tab | Question | Spatial statistics method |
|---|---|---|
| 1. Growth over time | How has Netflix's catalogue grown since 2008, and is production geographically clustered? | Global Moran's I, with Moran scatterplot |
| 2. Genre hotspots | Where are the hotspots for producing specific genres (anime, kids & family, documentaries, comedy, horror)? | Local Moran's I (LISA) |
| 3. Find a show | For a searched title, where was it made, and how far away are the nearest countries producing similar content? | Nearest-neighbour distance (great-circle distance between country centroids) |

The user can choose and recalculate each method live: year, genre, number of neighbours (k), significance level, title search and home country.

## Data

- **netflix_titles.csv**: 8,807 Netflix titles added between 2008 and 2021, from the Kaggle dataset *Netflix Movies and TV Shows* (Shivam Bansal).
- **Country boundaries**: 177 country polygons from Natural Earth, via the `spData` R package (loaded inside the app).

**Data type:** areal (lattice) data, meaning counts of titles per country polygon. This suits Global and Local Moran's I. For the distance analysis, each polygon is represented by its centroid.

## Key processing steps

- Multi-country titles are split so each title counts once per production country (10,012 title-country pairs).
- Country names are matched to the map (e.g. South Korea → Republic of Korea).
- Title counts are log-transformed, log(titles + 1), because they are highly skewed.
- Spatial weights use k-nearest neighbours (default k = 5, adjustable from 3 to 10), row-standardised. Contiguity was not used because island nations would have no neighbours.

## How to run

**In the browser (no installation):**
1. Go to <https://shinylive.io/r/editor>.
2. Upload `app.R` and `netflix_titles.csv` using the upload icon above the code.
3. Click Run.

**In RStudio:**
```r
install.packages(c("shiny", "sf", "spdep", "spData", "leaflet"))
shiny::runApp()
```
Run this from the folder containing both files.

## Limitations

- The data records where a title was **produced**, not where it can be **streamed**.
- About 1 in 10 titles has no production country recorded.
- Very small territories (e.g. Hong Kong, Singapore) are not on the world map and are excluded.
- Distances are between country centroids, which can shift for countries with overseas territories.
- Local Moran's I runs one test per country, so some significant results may occur by chance (a 0.01 option is provided).

## Group members

- Nelisa
- Lebo
- Firidaus
- Muranwa

# Fisheries Catch Dashboard

An enhanced Shiny application for visualizing and analyzing fisheries catch data across multiple regions.

## Enhancements Implemented

### 1. Bug Fix: CPUE Calculation (Option #1)

**Problem:** The original CPUE (Catch Per Unit Effort) calculation divided `catch_kg` by `crew_size`, but approximately 10% of vessels have `NA` crew size values. This produced `NA` results and broke the visualization.

**Solution:**
- Filter out records with missing crew size before calculating CPUE
- Display an informative message when no valid CPUE data is available for the selected filters
- Prevents silent failures and provides clear user feedback

### 2. Performance Improvements (Option #2)

**Data Preprocessing:**
- Pre-compute and save the joined dataset to `catch_full.rds` instead of generating data at runtime
- Run `dummy_data_generator.R` once to prepare the cached data file
- Reduces app startup time significantly

**Selective Column Storage:**
- Only persist columns actually used by the application
- Reduces memory footprint and file size

**Individual Package Imports:**
- Import specific packages (`dplyr`, `tidyr`, `lubridate`, `ggplot2`) instead of loading the entire `tidyverse`
- Faster load times and reduced memory usage

**Reactive Optimization:**
- Replaced `reactive()` with `eventReactive()` for filtered data to prevent unnecessary recalculations
- Used `observeEvent()` instead of `observe()` for species dropdown updates
- Filter invalidation only triggers when specific inputs change, not on every reactive flush

**Map Rendering:**
- Initialize map once with `renderLeaflet()` containing only base tiles
- Use `leafletProxy()` for all subsequent updates (markers, legends, view changes)
- Avoids complete map re-render on every filter change

### 3. UI/UX Redesign (Option #8 - Creative Improvement)

**Full-Screen Map Interface:**
- Map now serves as the primary visual element, filling the entire viewport
- Floating, draggable control panel with opacity transitions for better focus on data

**Dynamic Map Behavior:**
- Automatic zoom calculation based on data geographic spread
- Smooth `flyTo()` animations when switching between regions
- Color-coded markers using a continuous "Reds" palette based on catch weight

**Enhanced Visualizations:**
- Fixed season ordering (Spring, Summer, Autumn, Winter) with consistent colors
- `complete()` ensures all seasons display even when data is missing for some
- Value labels on bar charts for immediate readability
- Hover tooltips on map markers showing catch details

**Edge Case Handling:**
- Graceful handling of single-value datasets (custom legend rendering)
- Species selection preserved when switching countries if still valid

### 4. Unit Testing (Option #6)

Added test suite using `testthat` to validate data integrity:
- File existence check for `catch_full.rds`
- Required columns validation
- No NA values in critical fields (country, species, catch_kg)
- Positive catch weight validation

### 5. CI/CD Pipeline (Bonus)

Implemented GitHub Actions workflow for automated deployment:
- Triggers on push to `main` branch
- SSH deployment to AWS EC2 running Shiny Server
- Automatic code sync and permission management

## Setup and Installation

### Requirements

- R version 4.4.0+
- Required packages:

```r
install.packages(c("shiny", "dplyr", "tidyr", "lubridate", "leaflet", "ggplot2", "here", "testthat"))
```

### Data Preparation

Before running the app, generate the preprocessed data file:

```r
source("dummy_data_generator.R")
```

This creates `catch_full.rds` in the project root.

### Running the Application

```r
shiny::runApp()
```

### Running Tests

```r
testthat::test_dir("tests/testthat")
```

Expected output:

```
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 6 ]
```

## Project Structure

```
.
├── app.R                    # Main Shiny application
├── dummy_data_generator.R   # Data generation and caching script
├── catch_full.rds           # Pre-computed dataset (generated)
├── www/
│   └── styles.css           # Custom CSS for floating panel UI
├── tests/
│   └── testthat/
│       └── test-data.R      # Data validation tests
├── .github/
│   └── workflows/
│       └── deploy.yml       # CI/CD pipeline configuration
└── README.md
```

## Trade-offs and Assumptions

- **RDS vs Database:** Used RDS for simplicity given the dataset size (1200 records). For larger datasets, a database with indexed queries would be more appropriate.
- **Season Definition:** Seasons are defined for Southern Hemisphere (Australian data comprises a significant portion). This affects how months map to seasons.
- **Map Tile Provider:** Uses default OpenStreetMap tiles. For production, a dedicated tile server or commercial provider might be preferred.


## Session Info

```
R version 4.4.0 (2024-04-24 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] tidyr_1.3.1     ggplot2_3.5.1   leaflet_2.2.2   lubridate_1.9.4 dplyr_1.1.4     shiny_1.10.0    testthat_3.2.3 

loaded via a namespace (and not attached):
 [1] sass_0.4.9         generics_0.1.3     digest_0.6.37      magrittr_2.0.3     RColorBrewer_1.1-3 grid_4.4.0        
 [7] timechange_0.3.0   pkgload_1.4.0      fastmap_1.2.0      rprojroot_2.0.4    jsonlite_1.8.9     brio_1.1.5        
[13] promises_1.3.2     purrr_1.0.2        crosstalk_1.2.1    scales_1.3.0       textshaping_0.4.1  jquerylib_0.1.4   
[19] cli_3.6.3          rlang_1.1.4        munsell_0.5.1      withr_3.0.2        cachem_1.1.0       tools_4.4.0       
[25] memoise_2.0.1      colorspace_2.1-1   httpuv_1.6.15      here_1.0.1         vctrs_0.6.5        R6_2.5.1          
[31] mime_0.12          lifecycle_1.0.4    htmlwidgets_1.6.4  ragg_1.3.3         pkgconfig_2.0.3    waldo_0.6.1       
[37] desc_1.4.3         pillar_1.10.1      bslib_0.8.0        later_1.4.1        gtable_0.3.6       glue_1.8.0        
[43] Rcpp_1.0.13-1      systemfonts_1.1.0  tibble_3.2.1       tidyselect_1.2.1   rstudioapi_0.17.1  farver_2.1.2      
[49] xtable_1.8-4       htmltools_0.5.8.1  labeling_0.4.3     compiler_4.4.0    
```

- **Environment Reproducibility:** Although not required, using `renv` to create an `renv.lock` file is recommended for long-term stability. This ensures all package versions are pinned, making the development and deployment environments consistent—especially useful when deploying to Shiny Server or collaborating across machines.

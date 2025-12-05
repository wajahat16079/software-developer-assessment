# Dummy Data for Fisheries Monitoring Dashboard
library(dplyr)
library(lubridate)

#' Generate dummy fisheries data
#' @return A data frame with fisheries data
generate_fisheries_data <- function() {
  set.seed(1281216)

  # Define fishing zones across multiple regions
  zones <- list(
    # North Atlantic
    north_atlantic_1 = list(
      region = "North Atlantic",
      lat_range = c(40, 45),
      lon_range = c(-60, -50),
      country = "Canada"
    ),
    north_atlantic_2 = list(
      region = "North Atlantic",
      lat_range = c(45, 50),
      lon_range = c(-50, -40),
      country = "Canada"
    ),
    north_atlantic_3 = list(
      region = "North Atlantic",
      lat_range = c(50, 55),
      lon_range = c(-40, -30),
      country = "Iceland"
    ),

    # Australia - Great Barrier Reef
    australia_gbr = list(
      region = "Australia",
      lat_range = c(-20, -15),
      lon_range = c(145, 150),
      country = "Australia"
    ),
    australia_coral_sea = list(
      region = "Australia",
      lat_range = c(-25, -20),
      lon_range = c(150, 155),
      country = "Australia"
    ),
    australia_tasmania = list(
      region = "Australia",
      lat_range = c(-44, -40),
      lon_range = c(144, 148),
      country = "Australia"
    ),
    australia_west = list(
      region = "Australia",
      lat_range = c(-35, -30),
      lon_range = c(113, 118),
      country = "Australia"
    ),

    # Pacific Northwest
    pacific_nw_1 = list(
      region = "Pacific Northwest",
      lat_range = c(45, 50),
      lon_range = c(-130, -125),
      country = "USA"
    ),
    pacific_nw_2 = list(
      region = "Pacific Northwest",
      lat_range = c(50, 55),
      lon_range = c(-135, -130),
      country = "Canada"
    ),

    # North Sea
    north_sea_1 = list(
      region = "North Sea",
      lat_range = c(54, 58),
      lon_range = c(2, 6),
      country = "UK"
    ),
    north_sea_2 = list(
      region = "North Sea",
      lat_range = c(56, 60),
      lon_range = c(0, 4),
      country = "Norway"
    )
  )

  # Define species with their typical catch weights and preferred regions
  species_info <- data.frame(
    species = c("Tuna", "Salmon", "Cod", "Haddock", "Mackerel",
                "Barramundi", "Snapper", "Prawns", "Rock Lobster", "King George Whiting"),
    avg_catch = c(150, 80, 120, 60, 40, 90, 70, 50, 45, 55),
    sd_catch = c(50, 30, 40, 20, 15, 35, 25, 20, 18, 22),
    preferred_region = c("North Atlantic", "Pacific Northwest", "North Sea", "North Atlantic", "North Sea",
                         "Australia", "Australia", "Australia", "Australia", "Australia"),
    stringsAsFactors = FALSE
  )

  # Generate 1200 catch records over the past year
  n_records <- 1200

  data <- data.frame(
    date = sample(seq(Sys.Date() - 365, Sys.Date(), by = "day"), n_records, replace = TRUE),
    vessel_id = paste0("V-", sample(1001:1030, n_records, replace = TRUE)),
    zone = sample(names(zones), n_records, replace = TRUE, prob = c(rep(0.08, 3), rep(0.12, 4), rep(0.08, 2), rep(0.06, 2)))
  ) %>%
    rowwise() %>%
    mutate(
      region = zones[[zone]]$region,
      country = zones[[zone]]$country,
      latitude = runif(1, zones[[zone]]$lat_range[1], zones[[zone]]$lat_range[2]),
      longitude = runif(1, zones[[zone]]$lon_range[1], zones[[zone]]$lon_range[2])
    ) %>%
    ungroup()

  # Assign species based on region preferences
  data <- data %>%
    rowwise() %>%
    mutate(
      species = {
        # Get species that prefer this region
        preferred_species <- species_info$species[species_info$preferred_region == region]
        if (length(preferred_species) == 0) {
          # Fallback to any species
          sample(species_info$species, 1)
        } else {
          # 70% chance of preferred species, 30% chance of any species
          if (runif(1) < 0.7) {
            sample(preferred_species, 1)
          } else {
            sample(species_info$species, 1)
          }
        }
      }
    ) %>%
    ungroup() %>%
    left_join(species_info, by = "species") %>%
    mutate(
      catch_kg = pmax(10, rnorm(n(), avg_catch, sd_catch)),
      water_temp = case_when(
        region == "Australia" ~ rnorm(n(), 22, 3),
        region == "Pacific Northwest" ~ rnorm(n(), 10, 2),
        region == "North Sea" ~ rnorm(n(), 8, 2),
        TRUE ~ rnorm(n(), 12, 3)
      ),
      depth_m = runif(n(), 50, 300),
      season = case_when(
        month(date) %in% c(12, 1, 2) ~ "Summer",
        month(date) %in% c(3, 4, 5) ~ "Autumn",
        month(date) %in% c(6, 7, 8) ~ "Winter",
        TRUE ~ "Spring"
      )
    ) %>%
    select(date, vessel_id, species, catch_kg, latitude, longitude,
           water_temp, depth_m, region, country, zone, season) %>%
    arrange(desc(date))

  return(data)
}

#' Generate vessel information
#' @return A data frame with vessel details
generate_vessel_data <- function() {
  vessel_ids <- paste0("V-", 1001:1030)

  data.frame(
    vessel_id = vessel_ids,
    vessel_name = paste("Fishing Vessel", 1001:1030),
    capacity_kg = sample(500:2500, length(vessel_ids), replace = TRUE),
    crew_size = ifelse(runif(length(vessel_ids)) < 0.10, NA, sample(5:18, length(vessel_ids), replace = TRUE)),
    registration_year = sample(2000:2023, length(vessel_ids), replace = TRUE),
    port = sample(c("Halifax", "Boston", "Portland", "St. John's",
                    "Sydney", "Perth", "Hobart", "Cairns",
                    "Seattle", "Vancouver", "Aberdeen", "Bergen"),
                  length(vessel_ids), replace = TRUE),
    vessel_type = sample(c("Trawler", "Longliner", "Seiner", "Pot/Trap"),
                         length(vessel_ids), replace = TRUE)
  )
}

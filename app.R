library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)

# Load datasets
source("dummy_data_generator.R")
catch <- generate_fisheries_data()
vessels <- generate_vessel_data()

catch_full <- catch %>%
  left_join(vessels, by = "vessel_id")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("Fisheries Catch Dashboard"),

  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country:", choices = sort(unique(catch_full$country))),
      selectInput("species", "Species:", choices = NULL)
    ),

    mainPanel(
      leafletOutput("map", height = 350),
      br(),
      plotOutput("total_catch_plot"),
      br(),
      plotOutput("cpue_plot")
    )
  )
)

server <- function(input, output, session) {


  # Update species choices when country changes, preserving selection if still valid
  observeEvent(input$country, {
    species_choices <- catch_full %>%
      filter(country == input$country) %>%
      pull(species) %>%
      unique() %>%
      sort()
    
    selected <- if (input$species %in% species_choices) input$species else species_choices[1]
    
    updateSelectInput(session, "species", choices = species_choices, selected = selected)
  })
  
  # Filter data based on selected country and species
  filtered_data <- eventReactive(c(input$country, input$species), {
    req(input$country, input$species)
    catch_full %>%
      filter(country == input$country, species == input$species)
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  # Update map when filtered data changes
  observeEvent(filtered_data(), {
    dat <- filtered_data()
    
    req(nrow(dat) > 0)
    
    bounds <- dat %>%
      summarize(
        lng = mean(longitude, na.rm = TRUE),
        lat = mean(latitude, na.rm = TRUE),
        lng_range = max(longitude, na.rm = TRUE) - min(longitude, na.rm = TRUE),
        lat_range = max(latitude, na.rm = TRUE) - min(latitude, na.rm = TRUE)
      )
    
    # Calculate zoom based on data spread
    spread <- max(bounds$lng_range, bounds$lat_range)
    zoom <- case_when(
      spread > 50 ~ 1,
      spread > 20 ~ 2,
      spread > 10 ~ 3,
      spread > 5  ~ 4,
      spread > 1  ~ 5,
      TRUE        ~ 6
    )
    
    # Color palette with fallback for single observation
    single_value <- nrow(dat) == 1 || min(dat$catch_kg, na.rm = TRUE) == max(dat$catch_kg, na.rm = TRUE)
    
    if (single_value) {
      pal <- function(x) "#B71C1C"
    } else {
      pal <- colorNumeric("Reds", domain = dat$catch_kg)
    }
    
    # Create tooltip content for popup and label
    dat <- dat %>%
      mutate(
        tooltip = paste0(
          "<b>Species:</b> ", species,
          "<br><b>Catch (kg):</b> ", round(catch_kg, 2),
          "<br><b>Vessel:</b> ", vessel_name,
          "<br><b>Port:</b> ", port
        )
      )
    
    map <- leafletProxy("map", data = dat) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = ~sqrt(catch_kg)/2.5,
        fillColor = ~pal(catch_kg),
        color = "#333",
        weight = 1,
        popup = ~tooltip,
        label = ~lapply(tooltip, htmltools::HTML),
        fillOpacity = 0.7
      ) %>%
      flyTo(lng = bounds$lng, lat = bounds$lat, zoom = zoom)
    
    # Add legend - different approach for single vs multiple values
    if (single_value) {
      map <- map %>%
        addLegend(
          position = "bottomleft",
          colors = "#B71C1C",
          labels = paste0(round(dat$catch_kg[1], 2), " kg"),
          title = paste0(input$species, " - ", input$country, "<br>Catch (kg)"),
          className = "info legend small-legend"
        )
    } else {
      map <- map %>%
        addLegend(
          position = "bottomleft",
          pal = pal,
          values = ~catch_kg,
          title = paste0(input$species, " - ", input$country, "<br>Catch (kg)"),
          className = "info legend small-legend"
        )
    }
    
    map
  })

  output$total_catch_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    # Define fixed colors for each season
    season_colors <- c(
      "Spring" = "#6B8E6B",
      "Summer" = "#C9A56C", 
      "Autumn" = "#A67B7B",
      "Winter" = "#7090A0"
    )
    
    filtered_data() %>%
      group_by(season) %>%
      summarize(total_catch = sum(catch_kg), .groups = "drop") %>%
      # Ensure all seasons present with 0 for missing
      complete(season = names(season_colors), fill = list(total_catch = 0)) %>%
      mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter"))) %>%
      ggplot(aes(x = season, y = total_catch, fill = season)) +
      geom_col() +
      geom_text(aes(label = round(total_catch, 1)), vjust = -0.5, color = "black") +
      scale_fill_manual(values = season_colors) +
      labs(
        title = paste("Total Catch per Season -", input$country),
        x = "Season",
        y = "Catch (kg)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$cpue_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    filtered_data() %>%
      mutate(cpue = catch_kg / crew_size) %>%
      filter(!is.na(cpue)) %>% # remove NAs for continuous line
      group_by(date) %>%
      summarize(avg_cpue = mean(cpue), .groups = "drop") %>%
      ggplot(aes(x = date, y = avg_cpue)) +
      geom_line() +
      geom_point() +
      labs(
        title = paste("Catch per Unit Effort (CPUE) -", input$species),
        x = "Date",
        y = "CPUE (kg per crew member)"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)

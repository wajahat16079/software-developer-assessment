library(shiny)
library(dplyr)
library(lubridate)
library(leaflet)
library(ggplot2)
library(tidyr)


# Load dataset
catch_full <- readRDS("catch_full.rds")

ui <- bootstrapPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  div(class = "outer",
      # Full screen map as background
      leafletOutput("map", width = "100%", height = "100%"),
      
      # Floating control panel
      absolutePanel(
        id = "controls",
        top = 10, right = 10,
        width = "30%",
        draggable = TRUE,
        
        h3("Fisheries Catch Dashboard"),
        selectInput("country", "Country:", choices = sort(unique(catch_full$country)), width = "100%"),
        selectInput("species", "Species:", choices = NULL, width = "100%"),
        
        hr(),
        plotOutput("total_catch_plot", height = 250),
        hr(),
        plotOutput("cpue_plot", height = 250)
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
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean Basemap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Ocean Basemap", "Satellite"),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = 0, lat = 20, zoom = 2)
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
      spread > 50 ~ 2,
      spread > 20 ~ 3,
      spread > 10 ~ 4,
      spread > 5  ~ 5,
      spread > 1  ~ 6,
      TRUE        ~ 7
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
        radius = 7,
        fillColor = ~pal(catch_kg),
        color = "#000000",
        weight = 2,
        popup = ~tooltip,
        label = ~lapply(tooltip,HTML),
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
          title = paste0(input$species, " - ", input$country, "<br>Catch (kg)")
        )
    } else {
      map <- map %>%
        addLegend(
          position = "bottomleft",
          pal = pal,
          values = ~catch_kg,
          title = paste0(input$species, " - ", input$country, "<br>Catch (kg)")
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
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = paste("Total Catch per Season -", input$country, "-", input$species),
        x = NULL,
        y = "Catch (kg)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold")
      )
  })
  
  # CPUE trend over time - assesses fishing efficiency relative to crew size
  output$cpue_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    
    common_theme <- theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold")
      )
    
    plot_title <- paste("CPUE Trend -", input$country, "-", input$species)
    plot_subtitle <- "CPUE = Catch Per Unit Effort (kg per crew member)"
    
    plot_data <- filtered_data() %>%
      filter(!is.na(crew_size)) %>%
      mutate(cpue = catch_kg / crew_size) %>%
      group_by(date) %>%
      summarize(avg_cpue = mean(cpue), .groups = "drop")
    
    if (nrow(plot_data) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No valid CPUE data\n(missing crew size)", 
                 size = 6, color = "gray50") +
        labs(title = plot_title, subtitle = plot_subtitle) +
        common_theme
    } else {
      ggplot(plot_data, aes(x = date, y = avg_cpue)) +
        geom_point(size = 3) +
        geom_line() +
        labs(title = plot_title, subtitle = plot_subtitle, x = "Date", y = "CPUE") +
        common_theme
    }
  })
}

shinyApp(ui, server)

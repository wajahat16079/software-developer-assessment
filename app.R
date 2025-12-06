library(shiny)
library(dplyr)
library(lubridate)
library(leaflet)

# Load datasets
source("dummy_data_generator.R")
catch <- generate_fisheries_data()
vessels <- generate_vessel_data()

catch_full <- catch %>%
  left_join(vessels, by = "vessel_id")

ui <- fluidPage(
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

 observe({
    species <- catch_full %>%
      filter(country == input$country) %>%
      pull(species) %>% unique() %>% sort()
    updateSelectInput(session, "species", choices =
  species, selected = species[1])
  })


  country_data <- reactive({
    catch_full %>% filter(country == input$country)
  })

  filtered_data <- reactive({
    country_data() %>% filter(species == input$species)
  })

  output$map <- renderLeaflet({
    dat <- filtered_data()

    leaflet(dat) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        radius = ~sqrt(catch_kg)/5,
        popup = ~paste0(
          "<b>Species:</b> ", species,
          "<br><b>Catch (kg):</b> ", catch_kg,
          "<br><b>Vessel:</b> ", vessel_name,
          "<br><b>Port:</b> ", port
        ),
        fillOpacity = 0.7
      )
  })

  output$total_catch_plot <- renderPlot({
    filtered_data() %>%
      group_by(season) %>%
      summarize(total_catch = sum(catch_kg), .groups = "drop") %>%
      ggplot(aes(x = season, y = total_catch, fill = season)) +
      geom_col() +
      labs(
        title = paste("Total Catch per Season -", input$country),
        x = "Season",
        y = "Catch (kg)"
      ) +
      theme_minimal()
  })

  output$cpue_plot <- renderPlot({
    filtered_data() %>%
      mutate(cpue = catch_kg / crew_size) %>%
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

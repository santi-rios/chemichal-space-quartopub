library(shiny)
library(plotly)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

######### Data Preparation #########
data_url <- "https://raw.githubusercontent.com/santi-rios/China-Chemical-Dominance/main/data/fig_1_df.csv"

# Read data with data.table
df <- fread(data_url)
df[, Country := as.factor(Country)]
df[, substance := as.factor(substance)]

######### Custom Labels #########
figure_labels <- list(
  "Figure-12-a_b" = list(y_title = "Composite Index", map_title = "Combined Metrics"),
  "Figure2-b"     = list(y_title = "Organometallics", map_title = "Organometallics Metrics"),
  "Figure2-d"     = list(y_title = "Rare-earths", map_title = "Rare Earth Elements"),
  "Figure1-d"     = list(y_title = "GDP Growth", map_title = "GDP Growth"),
  "Figure1-a"     = list(y_title = "Contribution", map_title = "Contribution Map")
)

################
## Shiny UI   ##
################
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .navbar { background-color: #222831; padding: 10px; }
      .card { margin: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .card-header {
        background-color: #00ADB5 !important;
        color: white;
        padding: 15px;
      }
      .selectize-input { border-radius: 4px; padding: 8px; }
      .plotly.html-widget { border-radius: 8px; }
    "))
  ),
  div(
    class = "navbar",
    h4("China's rise in the chemical space and the decline of US influence.", style = "color: white; margin: 0;")
  ),
  div(
    class = "card",
    div(
      class = "card-header",
      "Select a Year and up to 10 Countries. A random country is selected by default.
           Single-click to add/remove a country. Double-click a legend item to isolate it."
    ),
    div(
      class = "card-body",

      # Year Slider
      sliderInput(
                      "year", "Select Year",
                      min = min(df$Year, na.rm = TRUE) + 1,
                      max = max(df$Year, na.rm = TRUE) - 1,
                      value = max(df$Year, na.rm = TRUE) - 1,
                      step = 1,
                      width = "100%",
                      animate = TRUE),

      # Metric (figure) selection
      selectInput(
        "facet", "Select Metric:",
        choices = unique(df$source.x),
        selected = "Figure-12-a_b",
        width = "100%"
      ),

      # Countries
      selectizeInput(
        "countrySelector",
        "Select Countries:",
        choices  = NULL, # filled dynamically in server
        multiple = TRUE,
        options  = list(maxItems = 10),
        width    = "100%"
      )
    )
  ),
  div(
    class = "card",
    div(
      class = "card-header",
      "Analytical Interactive Plots. Hover for more info.
           The map shows data at the selected year.
           Cartogram (if available) only for Figure-12-a_b."
    ),
    div(
      class = "card-body",
      plotlyOutput("combinedPlot", height = "500px"),
      plotlyOutput("geoPlot", height = "400px"),
      uiOutput("cartogramImage") # show image if "Figure-12-a_b"
    )
  )
)

################
## Shiny Server
################
server <- function(input, output, session) {
  ###########################
  ## 1) Update country list ##
  ###########################
  observe({
    current_metric <- input$facet
    # Gather valid countries for that figure, sorted
    valid_countries <- df[source.x == current_metric, unique(Country)]
    valid_countries <- sort(as.character(valid_countries)) # alphabetical

    # By default, select a random country if available
    default_sel <- if (length(valid_countries) > 0) sample(valid_countries, 1) else character(0)

    # Update the input
    updateSelectizeInput(
      session, "countrySelector",
      choices = valid_countries,
      selected = default_sel,
      server = TRUE
    )
  })

  #############################################
  ## 2) Reactive subset based on user inputs ##
  #############################################
  filtered_data <- reactive({
    req(input$facet, input$countrySelector, input$year)
    df[
      source.x == input$facet &
        Country %in% input$countrySelector &
        Year <= input$year
    ]
  }) %>% debounce(300)

  ########################################
  ## 3) Helper: define color assignment ##
  ########################################
  country_colors <- function(data_subset) {
    countries <- unique(as.character(data_subset$Country))
    color_map <- c()
    if ("United States" %in% countries) {
      color_map["United States"] <- "blue"
    }
    if ("China" %in% countries) {
      color_map["China"] <- "red"
    }
    others <- setdiff(countries, c("United States", "China"))
    if (length(others) > 0) {
      pal <- colorRampPalette(brewer.pal(min(8, length(others)), name = "Set2"))(length(others))
      color_map[others] <- pal
    }
    color_map
  }

  ###########################################################
  ## 4) Combined Plot:
  ##    - Always line (Value.x) for all countries
  ##    - If "Figure-12-a_b", add points for Value.y only on rows that have it
  ###########################################################
  output$combinedPlot <- renderPlotly({
    data_subset <- filtered_data()
    if (nrow(data_subset) == 0) {
      return(plotly_empty())
    }

    color_map <- country_colors(data_subset)

    # Build the base ggplot with lines for Value.x
    gg_base <- ggplot(data_subset, aes(x = Year, color = Country)) +
      geom_line(aes(y = Value.x, linetype = Country), na.rm = TRUE) +
      scale_color_manual(values = color_map) +
      labs(
        title = paste0(
          figure_labels[[input$facet]]$y_title %||% input$facet,
          " Trend"
        ),
        x = "Year",
        y = figure_labels[[input$facet]]$y_title %||% "Value"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "#f0f0f0")
      ) +
      scale_x_continuous(breaks = seq(min(data_subset$Year), max(data_subset$Year), by = 5))

    # If we’re in "Figure-12-a_b", we also add points for Value.y
    # but only where Value.y is not missing
    if (input$facet == "Figure-12-a_b") {
      gg_base <- gg_base +
        geom_point(
          data = data_subset[!is.na(Value.y), ],
          aes(y = Value.y, shape = substance),
          size = 2, alpha = 0.8, na.rm = TRUE
        ) +
        scale_shape_manual(values = c(16, 17, 15, 18), na.translate = FALSE)
    }

    p <- ggplotly(gg_base, tooltip = c("x", "y", "colour", "shape")) %>%
      layout(
        legend    = list(orientation = "v"),
        hovermode = "closest"
      )

    # If "Figure1-d", add certain annotations
    if (input$facet == "Figure1-d") {
      p <- p %>% layout(
        annotations = list(
          list(
            x = 2020, y = 10, text = "COVID-19", showarrow = FALSE,
            font = list(size = 8)
          ),
          list(
            x = 2007, y = 11, text = "Global Financial Crisis", showarrow = FALSE,
            font = list(size = 8)
          )
        )
      )
    }

    p
  })

  ##################################
  ## 5) World Map (using Value.x) ##
  ##################################
  output$geoPlot <- renderPlotly({
    data <- filtered_data()[Year == input$year]
    if (nrow(data) == 0) {
      return(plotly_empty())
    }

    val_label <- figure_labels[[input$facet]]$y_title %||% "Value"

    plot_geo(data, height = 300) %>%
      add_trace(
        z = ~Value.x,
        color = ~Value.x,
        colorscale = "Purples",
        locations = ~iso3c,
        text = ~ paste(Country, "<br>Value.x:", round(Value.x, 2)),
        hoverinfo = "text",
        marker = list(line = list(color = "white", width = 0.3))
      ) %>%
      colorbar(title = val_label) %>%
      layout(
        title = paste(
          figure_labels[[input$facet]]$map_title %||% "Map",
          "in", input$year
        ),
        geo = list(
          showframe   = FALSE,
          projection  = list(type = "natural earth"),
          bgcolor     = "rgba(0,0,0,0)"
        )
      )
  })

  ###########################################
  ## 6) Cartogram image if "Figure-12-a_b" ##
  ###########################################
  output$cartogramImage <- renderUI({
    req(input$year)
    # Use EXACT facet name "Figure-12-a_b"
    if (input$facet == "Figure-12-a_b") {
      # If you want them from GitHub:
      # (Make sure that these .pngs exist in that exact path with same naming)
      tags$div(
        style = "text-align: center; margin-top: 10px;",
        tags$img(
          src = paste0(
            "https://raw.githubusercontent.com/santi-rios/China-Chemical-Dominance/main/cartograms/",
            input$year, ".png"
          ),
          style = "max-width: 100%; height: auto;"
        )
      )

      # If you prefer local usage (and the cartograms are in www/cartograms/):
      # tags$div(
      #   style = "text-align: center; margin-top: 10px;",
      #   tags$img(
      #     src = paste0("cartograms/", input$year, ".png"),
      #     style = "max-width: 100%; height: auto;"
      #   )
      # )
    } else {
      NULL
    }
  })
}

shinyApp(ui, server)

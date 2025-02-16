library(shiny)
library(bslib)
library(plotly)
library(data.table)
library(tidyr)
library(dplyr)
library(DT)
library(RColorBrewer)

###########################
## Data and Preparation  ##
###########################
data_url <- "https://raw.githubusercontent.com/santi-rios/China-Chemical-Dominance/refs/heads/main/data/countries_with_chemicals_joined.csv"
df <- read.csv(data_url, stringsAsFactors = FALSE)
df$Country <- as.factor(df$Country)

# Helper for original map tooltip
df_hover <- df %>%
  group_by(Country) %>%
  summarize(hover_text = paste(Country, "<br>Value:", round(Value.x, 2)))

##################################################
## 1) Define color logic for the line chart     ##
##    so that China=red, US=blue, others=distinct
##################################################
line_color_map <- function(country_vec) {
  # We'll identify the unique countries in the subset
  countries_unique <- unique(country_vec)

  # Initialize a named character vector for the colors
  color_out <- setNames(rep(NA_character_, length(countries_unique)), countries_unique)

  # Assign fixed colors to China and US if present
  if ("China" %in% countries_unique) {
    color_out["China"] <- "#FF0000" # Red
  }
  if ("United States" %in% countries_unique) {
    color_out["United States"] <- "#0000FF" # Blue
  }

  # The rest get distinct colors from a palette
  others <- setdiff(countries_unique, c("China", "United States"))
  if (length(others) > 0) {
    # e.g., "Dark2"
    pal <- brewer.pal(max(8, length(others)), name = "Dark2")[seq_along(others)]
    color_out[others] <- pal
  }

  # Now match back to the full vector order
  final_colors <- color_out[match(country_vec, names(color_out))]
  unname(final_colors)
}

###############################
## 2) Shiny App UI           ##
###############################
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Raleway"),
  primary = "#2c3e50"
)

ui <- fluidPage(
  theme = app_theme,

  # JavaScript snippet to auto-click the slider "Play" button on load
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      setTimeout(function() {
        $('.slider-animate-button').click();
      }, 1000);
    });
  ")),

  # Page Header
  fluidRow(
    style = "background-color: #2c3e50; padding: 20px; margin-bottom: 20px;",
    column(
      width = 12,
      h2("Global Chemical Collaborations", style = "color: #ffffff; margin: 0;"),
      p("Explore the contribution index by year and see a color-coded world map of selected countries.",
        style = "color: #cccccc; margin: 0;"
      )
    )
  ),

  # Main Tabset
  tabsetPanel(
    tabPanel(
      "Visualizations",
      br(),
      fluidRow(
        column(
          width = 4,
          # Controls
          div(
            class = "card",
            div(class = "card-header", style = "background-color: #2c3e50; color: white;", "Select Year and Countries"),
            div(
              class = "card-body",
              sliderInput(
                inputId = "year",
                label   = "Year",
                min     = min(df$Year, na.rm = TRUE),
                max     = max(df$Year, na.rm = TRUE),
                value   = 2022,
                step    = 1,
                animate = animationOptions(interval = 1000, loop = FALSE, playButton = "Play", pauseButton = "Pause"),
                width   = "100%"
              ),
              selectInput(
                inputId  = "countrySelector",
                label    = "Select Countries:",
                choices  = sort(unique(df$Country)),
                selected = sort(unique(df$Country)),
                multiple = TRUE,
                width    = "100%"
              )
            )
          )
        ),
        column(
          width = 8,
          # Plots
          div(
            class = "card",
            div(class = "card-header", style = "background-color: #2c3e50; color: white;", "Interactive Plots"),
            div(
              class = "card-body",
              plotlyOutput("combinedPlot", height = "350px"),
              hr(),
              plotlyOutput("worldMap", height = "350px")
            )
          )
        )
      )
    ),
    tabPanel(
      "Data Explorer",
      br(),
      div(
        class = "card",
        div(class = "card-header", style = "background-color: #2c3e50; color: white;", "Data Overview"),
        div(
          class = "card-body",
          DT::dataTableOutput("rawData")
        )
      )
    )
  )
)

###############################
## 3) Shiny Server           ##
###############################
server <- function(input, output, session) {
  # Reactive selection
  selected_countries <- reactive({
    if (is.null(input$countrySelector)) character(0) else input$countrySelector
  })

  filtered_data <- reactive({
    subset(df, Country %in% selected_countries() & Year <= input$year)
  })

  ################################
  ## A) The Combined Line Plot  ##
  ################################
  output$combinedPlot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines"))
    }

    # For labeling the last year
    data <- data %>%
      group_by(Country) %>%
      mutate(
        text_label = ifelse(Year == max(Year), as.character(Country), NA)
      ) %>%
      ungroup()

    # Determine colors for each row
    data$lineColor <- line_color_map(data$Country)

    fig <- plot_ly(
      data,
      x = ~Year,
      y = ~Value.x,
      color = I(~lineColor), # pass literal color
      type = "scatter",
      mode = "lines+markers+text",
      line = list(width = 2),
      marker = list(size = 5),
      text = ~text_label,
      textposition = "top center",
      hoverinfo = "text",
      hovertext = ~ paste0(
        "<b>", Country, "</b><br>",
        "Year: ", Year, "<br>",
        "Value: ", round(Value.x, 2)
      )
    ) %>%
      layout(
        title = list(text = "Contribution Index Over Time", x = 0.02),
        xaxis = list(title = "Year", gridcolor = "#f0f0f0"),
        yaxis = list(title = "Value.x", gridcolor = "#f0f0f0"),
        hovermode = "closest",
        showlegend = FALSE
      )

    fig
  })

  ###############################
  ## B) The World Map (Blue Scale, No Legend)
  ###############################
  output$worldMap <- renderPlotly({
    # Use only the current year
    map_data <- df[df$Year == input$year, ]
    if (nrow(map_data) == 0) {
      return(plotly_empty())
    }

    # Merge in simpler hover text
    map_data2 <- merge(map_data, df_hover, by = "Country", all.x = TRUE)

    plot_geo(map_data2, height = 350) %>%
      add_trace(
        locations = ~iso3c,
        z = ~Value.x,
        color = ~Value.x,
        colors = "Blues",
        hoverinfo = "text",
        text = ~hover_text,
        marker = list(
          line = list(color = "white", width = 0.3),
          showscale = FALSE
        ) # hide color scale
      ) %>%
      layout(
        title = list(text = paste("Global Distribution -", input$year), x = 0.02),
        geo = list(
          showframe   = FALSE,
          projection  = list(type = "natural earth")
        ),
        showlegend = FALSE # No legend
      )
  })

  ########################################
  ## C) Data Table "Data Explorer" Tab  ##
  ########################################
  output$rawData <- DT::renderDataTable({
    df_table <- df[, c("Year", "Country", "Value.x")]
    df_wide <- pivot_wider(df_table, names_from = Country, values_from = Value.x)
    DT::datatable(
      df_wide,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)

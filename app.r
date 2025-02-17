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

# A lookup for more readable source labels:
source_mapping <- c(
  "Figure-12-a_b"                = "Country Participation in the Growth of the Chemical Space",
  "Figure1-d"                    = "% of the Annual Growth Rate of the GDP Per Capita",
  # "absolute_counting_colabs_biochem" = "Biochemistry",
  "Figure1-e"                    = "Number of Researchers in R&D",
  "Figure1-a"                    = "Contribution Map",
  "Figure2-b"                    = "Organometallics Metrics",
  "Figure2-d"                    = "Rare Earth Elements",
  "Figure1-d"                    = "GDP Growth"  # just an example if you have duplicates
)

# Add a new column "SourceLabel"
df[, SourceLabel := source_mapping[source.x]]
df[is.na(SourceLabel), SourceLabel := as.character(source.x)]  # fallback if no mapping found

######### Custom Labels #########
figure_labels <- list(
  "Figure-12-a_b" = list(y_title = "Composite Index",    map_title = "Combined Metrics"),
  "Figure2-b"     = list(y_title = "Organometallics",    map_title = "Organometallics Metrics"),
  "Figure2-d"     = list(y_title = "Rare-earths",         map_title = "Rare Earth Elements"),
  "Figure1-d"     = list(y_title = "GDP Growth",          map_title = "GDP Growth"),
  "Figure1-a"     = list(y_title = "Contribution",        map_title = "Contribution Map"), 
  "Figure1-e"      = list(y_title = "Researchers in R&D",  map_title = "Number of Researchers in R&D")
)

################
## Shiny UI   ##
################
ui <- fluidPage(
  # Include a custom JS handler for fade-out reload:
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('fadeReload', function(msg) {
        $('body').fadeOut(500, function() {
          location.reload();
        });
      });
    ")),
    tags$style(HTML("
      .navbar { background-color: #222831; padding: 10px; }
      .card { margin: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .card-header {
        background-color: #49adab !important;
        color: white;
        padding: 15px;
      }
      .selectize-input { border-radius: 4px; padding: 8px; }
      .plotly.html-widget { border-radius: 8px; }
    "))
  ),
  
  # Top Navbar
  div(
    class = "navbar",
    h4("China's rise in the chemical space and the decline of US influence.", style = "color: white; margin: 0;")
  ),
  
  # Controls card
  div(
    class = "card",
    div(
      class = "card-header",
      "Select a Year and up to 10 Countries. Double-click a legend item to isolate it."
    ),
    div(
      class = "card-body",
      
      # Year Slider with fancy play/pause
      sliderInput(
        "year", "Select Year",
        min    = min(df$Year, na.rm = TRUE) + 1,
        max    = max(df$Year, na.rm = TRUE) - 1,
        value  = max(df$Year, na.rm = TRUE) - 1,
        step   = 1,
        width  = "100%",
        animate = animationOptions(
          interval    = 500,
          loop        = FALSE,
          playButton  = HTML("<span style='color:red; font-weight:bold;'>► PLAY</span>"),
          pauseButton = HTML("<span style='color:blue; font-weight:bold;'>❚❚ PAUSE</span>")
        )
      ),
      
      # Metric (figure) selection
      selectInput(
        "facet", "Select Metric:",
        choices  = unique(df$SourceLabel),
        selected = source_mapping["Figure-12-a_b"],
        width    = "100%"
      ),
      
      # Country selection
      selectizeInput(
        "countrySelector",
        "Select Countries or delete:",
        choices  = NULL, # filled dynamically in server
        multiple = TRUE,
        options  = list(maxItems = 10),
        width    = "100%"
      ),
      
      # Buttons: Deselect all, Random
      fluidRow(
        column(width = 4, actionButton("deselectAll", "Deselect All", class="btn btn-warning")),
        column(width = 4, actionButton("randomOne",   "Random Country", class="btn btn-info"))
      ),
      
      hr(),
      # Reload button now triggers the fade-out reload
      actionButton("reloadApp", "Reload App", class="btn btn-success")
    )
  ),
  
  # Plot card
  div(
    class = "card",
    div(
      class = "card-header",
      "Analytical Interactive Plots. Hover for more info. The line chart uses the slider year. 
       The map below uses built-in frames for a smoother animation. 
       Cartogram only shown for 'Figure-12-a_b' at the slider's year."
    ),
    div(
      class = "card-body",
      plotlyOutput("combinedPlot", height = "500px"),
      plotlyOutput("geoPlot",      height = "400px"),
      uiOutput("cartogramImage")   # show image if "Figure-12-a_b"
    )
  )
)

################
## Shiny Server
################
server <- function(input, output, session) {
  # A mapping from country to a fixed color (extend as needed)
  predefined_colors <- c(
    "United States" = "#0000FF",
    "China"         = "#FF0000",
    "Germany"       = "#008000",
    "Japan"         = "#8B008B",
    "France"        = "#FFA500"
    # add more if desired
  )
  
  # Reverse-lookup: from SourceLabel back to the original source.x
  label_to_source <- reactive({
    setNames(names(source_mapping), source_mapping)
  })
  
  ###########################
  ## 1) Update country list ##
  ###########################
  observe({
    chosen_source_x <- label_to_source()[[input$facet]]
    if (is.na(chosen_source_x)) chosen_source_x <- input$facet
    
    valid_countries <- df[source.x == chosen_source_x, unique(Country)]
    valid_countries <- sort(as.character(valid_countries))
    
    if (length(valid_countries) == 0) {
      updateSelectizeInput(session, "countrySelector", choices = NULL, selected = NULL, server=TRUE)
      return()
    }
    
    # By default, for the first plot, pick top 8 by max Value.x
    top_vals <- df[source.x == chosen_source_x, .(max_val = max(Value.x, na.rm=TRUE)), by=.(Country)]
    setorder(top_vals, -max_val)
    top8 <- head(top_vals$Country, 8)
    
    default_sel <- intersect(top8, valid_countries)
    if (length(default_sel) == 0) {
      default_sel <- sample(valid_countries, 1)
    }
    
    updateSelectizeInput(
      session, "countrySelector",
      choices = valid_countries,
      selected = default_sel,
      server = TRUE
    )
  })
  
  ###############################
  ## 2) "Deselect All" button  ##
  ###############################
  observeEvent(input$deselectAll, {
    updateSelectizeInput(session, "countrySelector", selected = character(0))
  })
  
  ##################################
  ## 3) "Random Country" button   ##
  ##################################
  observeEvent(input$randomOne, {
    # Re-grab the valid countries for the currently selected metric
    chosen_source_x <- label_to_source()[[input$facet]]
    if (is.na(chosen_source_x)) chosen_source_x <- input$facet
    
    valid_countries <- df[source.x == chosen_source_x, unique(Country)]
    if (length(valid_countries) == 0) {
      updateSelectizeInput(session, "countrySelector", selected = character(0))
    } else {
      # pick exactly 1 random
      rnd <- sample(valid_countries, 1)
      updateSelectizeInput(session, "countrySelector", selected = rnd)
    }
  })
  
  ###############################
  ## 4) Reload App with Fade Out ##
  ###############################
  observeEvent(input$reloadApp, {
    # Trigger the fade out animation and reload the app to its base state.
    session$sendCustomMessage("fadeReload", list())
  })
  
  #############################################
  ## 5) Reactive subset based on user inputs ##
  #############################################
  filtered_data <- reactive({
    req(input$facet, input$countrySelector, input$year)
    chosen_source_x <- label_to_source()[[input$facet]]
    if (is.na(chosen_source_x)) chosen_source_x <- input$facet
    
    df[
      source.x == chosen_source_x &
        Country %in% input$countrySelector &
        Year <= input$year
    ]
  }) %>% debounce(300)
  
  ########################################
  ## 6) Helper for line chart coloring  ##
  ########################################
  country_colors <- function(data_subset) {
    countries <- unique(as.character(data_subset$Country))
    color_map <- c()
    for (cname in countries) {
      if (cname %in% names(predefined_colors)) {
        color_map[cname] <- predefined_colors[cname]
      } else {
        color_map[cname] <- "#696969" # fallback color
      }
    }
    color_map
  }
  
  ###########################################################
  ## 7) Combined Plot: lines of Value.x for all countries  ##
  ###########################################################
  output$combinedPlot <- renderPlotly({
    data_subset <- filtered_data()
    if (nrow(data_subset) == 0) {
      return(plotly_empty())
    }
    
    chosen_source_x <- label_to_source()[[input$facet]]
    if (is.na(chosen_source_x)) chosen_source_x <- input$facet
    
    color_map <- country_colors(data_subset)
    ylab      <- figure_labels[[chosen_source_x]]$y_title %||% as.character(input$facet)
    
    gg_base <- ggplot(data_subset, aes(x = Year, color = Country)) +
      geom_line(aes(y = Value.x, linetype = Country), na.rm=TRUE) +
      scale_color_manual(values = color_map) +
      geom_text(
        data = data_subset[Year == max(data_subset$Year), ],
        aes(y = Value.x, label = Country),
        hjust = -0.1, nudge_x = 0.1, nudge_y = 0.1,
        size = 3, check_overlap = TRUE, show.legend = FALSE
      ) +
      labs(
        title = paste0(ylab, " Trend"),
        x = "Year",
        y = ylab
      ) +
      theme_classic() +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = seq(min(data_subset$Year), max(data_subset$Year), by=5))
    
    # If we’re in "Figure-12-a_b", show Value.y points if present
    if (chosen_source_x == "Figure-12-a_b") {
      gg_base <- gg_base +
        geom_point(
          data = data_subset[!is.na(Value.y), ],
          aes(y = Value.y, shape = substance),
          size = 0.6, alpha = 0.4, na.rm = TRUE
        ) +
        scale_shape_manual(values = c(16, 17, 15, 18), na.translate = FALSE)
    }
    
    p <- ggplotly(gg_base, tooltip = c("x", "y", "colour", "shape")) %>%
      layout(
        legend    = list(orientation = "v"),
        hovermode = "closest"
      )
    
    # Example of specific annotation for "Figure1-d"
    if (chosen_source_x == "Figure1-d") {
      p <- p %>% layout(
        annotations = list(
          list(
            x = 2020, y = 10, text = "COVID-19", showarrow = FALSE,
            font = list(size = 12)
          ),
          list(
            x = 2007, y = 12.7, text = "Global Financial Crisis", showarrow = FALSE,
            font = list(size = 12)
          )
        )
      )
    }
    
    p
  })
  
  ##################################################
  ## 8) World Map with frames for smooth animation ##
  ##################################################
  output$geoPlot <- renderPlotly({
    req(input$facet, input$countrySelector)
    chosen_source_x <- label_to_source()[[input$facet]]
    if (is.na(chosen_source_x)) chosen_source_x <- input$facet
    
    data_map <- df[
      source.x == chosen_source_x &
        Country %in% input$countrySelector
    ]
    if (nrow(data_map) == 0) {
      return(plotly_empty())
    }
    
    val_label <- figure_labels[[chosen_source_x]]$y_title %||% "Value"
    
    data_map[, YearStr := as.factor(Year)]
    
    fig <- plot_geo(data_map, height = 400) %>%
      add_trace(
        z         = ~Value.x,
        color     = ~Value.x,
        colorscale= "Viridis",
        locations = ~iso3c,
        text      = ~paste0("Year: ", Year, "<br>",
                            "Country: ", Country, "<br>",
                            "Value.x: ", round(Value.x, 2)),
        hoverinfo = "text",
        marker    = list(line = list(color = "white", width = 0.3)),
        frame     = ~YearStr
      ) %>%
      colorbar(title = val_label) %>%
      layout(
        title = paste(
          figure_labels[[chosen_source_x]]$map_title %||% "Map",
          "(Animated)"
        ),
        geo = list(
          showframe   = FALSE,
          projection  = list(type = "natural earth"),
          bgcolor     = "rgba(0,0,0,0)"
        ),
        updatemenus = list(
          list(
            type = "buttons",
            direction = "right",
            x = 0.1, y = 1.1,
            buttons = list(
              list(
                label = "Play Animation",
                method= "animate",
                args  = list(
                  NULL,
                  list(
                    mode = "immediate",
                    frame = list(duration = 800, redraw = FALSE),
                    transition = list(duration = 600)
                  )
                )
              )
            )
          )
        )
      ) %>%
      animation_opts(frame = 800, transition = 600, redraw = FALSE)
    
    fig
  })
  
  ###########################################
  ## 9) Cartogram image if "Figure-12-a_b" ##
  ###########################################
  output$cartogramImage <- renderUI({
    req(input$year)
    chosen_source_x <- label_to_source()[[input$facet]]
    if (is.na(chosen_source_x)) chosen_source_x <- input$facet
    
    if (chosen_source_x == "Figure-12-a_b") {
      tags$div(
        style = "text-align: center; margin-top: 10px;",
        tags$img(
          src = paste0(
            "https://raw.githubusercontent.com/santi-rios/China-Chemical-Dominance/main/cartograms/",
            input$year, ".png"
          ),
          style = "max-width: 60%; height: auto;"
        )
      )
    } else {
      NULL
    }
  })
}

shinyApp(ui, server)

# This is an application to pull, wrangle, and visualize data output from my Olympia oyster Larval Dispersal Model. This will ultimately be the companion app to an application for the model itself. However, given average runtime for the dispersal model, I started with the visualization app since it will load far faster for peer reviewers. A very similar approach and user interface will be used in that app as well.
# Begin Application Code:
# LIBRARY:
library(shiny)   # ShinyApp package
library(tidyverse)   # tidyverse suite (ggplot2, dplyr, etc.)
library(bslib)   # alternate base page for ui (`page_sidebar`)
library(sf)   # simple features package for spatial data
library(bsicons)   # icons package for tooltips
library(PNWColors)   # PNWColors package for pretty color palettes
#
#----------------------------------
# LOAD BASE DATA OBJECTS:
## check if 1 object exists in the environment already; load all if it does not
if(!exists("WA_nodes_coords")) {
  # Names, locations, and attributes of possible release sites
  load("data/releasesites_WADFW.Rdata")
  # randomly selected larval tracks from Padilla Bay and Fidalgo Bay
  load("data/oly_tracks.Rdata")
  # shapefile for Salish Sea Model boundary
  load("data/ssm_poly.Rdata")
}

## create vectors for choices of inputs
### for now, limiting to a single release site and year. will add in modifiable options depending on the year chosen
site_ids <- c("FB", "PB", "SamB", "SimB", "KH", "UR")
site_names <- data.frame(site_name = filter(releasesites_WADFW, id %in% site_ids)$site,
                         site_id = site_ids)
                         
ssm_years <- data.frame(year = c("2017"), id = c(17)) 
behaviors <- data.frame(behavior = c("No Swimming", "Phototactic", "Ontogenetic"),
                        id = c("none", "photo", "onto"))
## create color palette for maps
pal1 <- pnw_palette("Sunset2", n = 10)
#
# 
#----------------------------------
# USER INTERFACE:
ui <- page_sidebar(
  # title - needs a fancier one for sure
  title = div("Olympia oyster (", em("Ostrea lurida"), ") Larval Transport Maps"),
  # add Bootswatch theme - Cerulean for now
  theme = bs_theme(bootswatch = "cerulean"),
  # Fill in the sidebar panel - this contains the inputs to call specific datasets
  sidebar = sidebar(
    # slider input for number of tracks to display
    ## Title and tooltip info for slider input (number of tracks to display)
    span("Number of Larvae",
         tooltip(
           bs_icon("info-circle"),
           "Select which larval tracks to display. Updates automatically.",
           placement = "top"
           )
         ),
    ## create slider input with 2 values - allows for selecting any range of tracks
    sliderInput(inputId = "n_tracks", label = NULL,
                min = 1, max = 10, value = c(1,5)),
    # selection input for release site name
    ## title and tooltip for release site selection
    span("Release Site",
         tooltip(
           bs_icon("info-circle"),
           "Select release location. Click 'Update Map' to activate.",
           placement = "top"
         )
    ),
    ## create dropdown selection including all possible release sites
    selectInput(inputId = "site", label = NULL, 
                choices = site_names$site_name, selected = "Fidalgo Bay",
                width = "100%"),
    # selection input for swimming behavior
    ## title and tooltip for behavior selection
    span("Swimming Behavior",
         tooltip(
           bs_icon("info-circle"),
           "Select swimming behavior. Click 'Update Map' to activate.",
           placement = "top"
         )
    ),
    ## create dropdown selection including all three swimming behaviors
    selectInput(inputId = "behavior", label = NULL,
                choices = behaviors$behavior, selected = "No Swimming",
                width = "100%"),
    # create an action button to update the map only when triggered
    actionButton(inputId = "map_button", label = "Update Map", width = "100%")
  ),
  # App description
  fluidRow(
    p("Welcome to the Olympia oyster Larval Transport Model mapping application!"),
    p("This app maps a small subsample of data from the Olympia oyster Larval Transport Model, a biophysical transport model built in R by Luke Ghallahorne and Jake Lawlor."),
    p("Use the panel on the left to select release site, swimming behavior, and number of tracks to map the pathways that larvae travel until they reach settlement."),
  ),
  # Main Panel: Map Output
  fluidRow(
    column(12, align = "center",
           plotOutput("map", width = "800px", height = "600px"))
  ),
  fluidRow(
    column(6,     # add link to data on GitHub
           uiOutput("link"))
  ),
  # Model details
  fluidRow(
    column(8,
      span(
        accordion_panel(
          title = "Larval Transport Model Details",
          p("The Olympia oyster Larval Transport Model uses hydrodynamic output from the Salish Sea Model, an oceanographic model created by Pacific Northwest National Laboratories. Modeled oyster larvae were released from six bays throughout the Salish Sea and tracked for 21 days. Their movement is determined by currents and one of three larval swimming behaviors: no swimming (i.e., neutral buoyancy), phototactic swimming (swimming up during the day and down at night), or ontogenetic swimming (swimming up in early development and down in late development)."),
          p("An open-source application to run the model is in development, to be released Summer/Fall 2025.")
        ),
        tooltip(
          bs_icon("info-circle"),
          "Click for details about the transport model that created these data.",
          placement = "right"
        )
      )
    )
  ),
  fluidRow(
    p("Last Updated 10-June-2025 LG")
  )
)

#
#----------------------------------
# SERVER:
server <- function(input, output, session) {
  # link to github with source code
  url <- a("github.com/lukeghallahorne", href="https://github.com/lukeghallahorne/DataScience4EnviroApps/tree/main/Seminar3_Shiny_App_Development/Oly_Dispersal_MapApp")
  output$link <- renderUI({
    tagList("Link to source code:", url)
  })
  
  # set initial values for pre-loaded plot
  ## ID names for filtering dataframes
  site_id <- reactiveVal("FB")
  behav_id <- reactiveVal("none")
  ## Full names for plot labels
  site_name <- reactiveVal("Fidalgo Bay")
  behav_name <- reactiveVal("No Swimming")

  # update site and behavior when button is clicked
  ## ID names for filtering
  observe({
    site_id(as.character(filter(releasesites_WADFW, site == input$site)$id))
  }) |> bindEvent(input$map_button)
  observe({
    behav_id(filter(behaviors, behavior == input$behavior)$id)
  }) |> bindEvent(input$map_button)
  ## Full names for plot labels
  observe({
    site_name(as.character(filter(releasesites_WADFW, site == input$site)$site))
  }) |> bindEvent(input$map_button)
  observe({
    behav_name(filter(behaviors, behavior == input$behavior)$behavior)
  }) |> bindEvent(input$map_button)
  
  # filter larval tracks by bay & behavior
  tracks <- reactive({
    filter(oly_tracks, bay == site_id() & behavior == behav_id())
  })

  # filter larval tracks by number of tracks selected
  tracks2plot <- reactive({
    filter(tracks(), site_track %in% unique(tracks()$site_track)[input$n_tracks[1]:input$n_tracks[2]])
  })
  # pull starting points for each larval track
  trackStarts <- reactive({
    tracks2plot() |>
      group_by(site_track) |>
      slice_head(n = 1) |> 
      ungroup()
  })
  
  # create basemap x and y limits, ensuring a 3:4 aspect ratio
  track_lims <- reactive({
    bm_xlim <- c(min(tracks()$x), max(tracks()$x))
    bm_ylim <- c(min(tracks()$y), max(tracks()$y))
    
    bm_xrange <- bm_xlim[2] - bm_xlim[1]
    bm_yrange <- bm_ylim[2] - bm_ylim[1]
    if(bm_xrange < bm_yrange) {
      bm_width <- bm_yrange + bm_yrange/2
      bm_height <- bm_width * 3/4
    } else if (bm_xrange > bm_yrange | bm_xrange == bm_yrange) {
      bm_width <- bm_xrange + bm_xrange/2
      bm_height <- bm_width * 3/4
    }
    bm_xctr <- bm_xlim[1] + bm_xrange/2
    bm_yctr <- bm_ylim[1] + bm_yrange/2
    
    bm_xmin <- bm_xctr - bm_width/2
    bm_xmax <- bm_xctr + bm_width/2
    bm_ymin <- bm_yctr - bm_height/2
    bm_ymax <- bm_yctr + bm_height/2
    
    data.frame(xlim = c(bm_xmin, bm_xmax),
               ylim = c(bm_ymin, bm_ymax))
  })
  # Create Map of Larval Tracks on button click
  track_map <- reactive({
    ggplot() +
      geom_sf(data = ssm_poly, alpha = 0.25, fill = "turquoise") +
      geom_path(data = tracks2plot(),
                aes(x = x, y = y, 
                    group = site_track, color = site_track)) +
      geom_point(data = trackStarts(),
                 aes(x = x, y = y), shape = 8) +
      scale_color_manual(values = pal1[input$n_tracks[1]:input$n_tracks[2]]) +
      labs(x = NULL, y = NULL) +
      guides(color = "none") +
      coord_sf(xlim = track_lims()$xlim, 
               ylim = track_lims()$ylim) +
      theme_bw() +
      labs(title = paste0(site_name()," Larval Tracks"),
           subtitle = paste0(behav_name(), " Behavior")) +
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 14),
            axis.text = element_text(size = 10),
            panel.grid = element_blank())
    })
  
  output$map <- renderPlot({
    track_map()
  }, width = "auto")
  


}
#
#----------------------------------
# RUN APPLICATION:
shinyApp(ui, server)
#
#App History
#21 May 2025 LG: \nThis application uses data output from the Olympia oyster Larval Transport Model (in progress) and visualizes settlement areas or larval tracks, depending on user input. The primary output is a ggplot map, either showing the first settlement points larvae reach during the dispersal period, or the entire movement path of the larvae during dispersal.
#
#T his is a preliminary version, and due to file size, only 1 release site with 3 swimming behaviors is available to start. I intend to update it so that either different sites' data can be loaded, or uploaded individually, then run through the visualization app.
# In the first attempt, I am having trouble loading data within an observeEvent function, as well as fitting my rather large wrangling-and-plotting loop into shiny syntax. For now, I am leaving it as the basic framework, and in place of the map, a simple (and slightly nonsensical) summary table is printed instead. Once I get all of the reactive objects set up, it should be pretty simple to convert the rest of the loop into shiny."),
# 28 May 2025 LG: App Update: I selected only 10 larval particle tracks from two sites (Fidalgo Bay and Padilla Bay) across all three behaviors (none, phototactic, and ontogenetic) to get some of the multi-site functionality while keeping file size low.
# 3 June 2025
# LG: I am changing the base setup for my app. Instead of trying to load individual files as the user selects them, I am going to have the app load the whole csv as a single file from the get go, then use the user input just to filter the dataframe based on the chosen release site. This will make original loading of the app rather slow potentially, but (hopefully) it will at least be functional. From there, I will expand it to see if I can load the entire datasets, but that would require large-file storage through GitHub which I frankly do not have the time to investigate in this moment.
#
# 10 June 2025 LG: I included 10 tracks from all six sites from 2017 model runs (Fidalgo Bay, Padilla Bay, Samish Bay, Similk Bay, Kilisut Harbor, and Union River)
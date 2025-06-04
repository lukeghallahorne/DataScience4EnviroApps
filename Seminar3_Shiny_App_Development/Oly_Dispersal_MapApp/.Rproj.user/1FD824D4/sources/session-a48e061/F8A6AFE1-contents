# This is the first version of an application to pull, wrangle, and visualize data output from my Olympia oyster Larval Dispersal Model. This will ultimately be the companion app to an application for the model itself. However, given average runtime for the dispersal model, I started with the visualization app since it will load far faster for peer reviewers. A very similar approach and user interface will be used in that app as well.
#
# 3 June 2025
# LG: I am changing the base setup for my app. Instead of trying to load individual files as the user selects them, I am going to have the app load the whole Rdata as a single file from the get go, then use the user input just to filter the dataframe based on the chosen release site. This will make original loading of the app rather slow potentially, but (hopefully) it will at least be functional. From there, I will expand it to see if I can load the entire datasets, but that would require large-file storage through GitHub which I frankly do not have the time to investigate in this moment.
#
# LIBRARY:
library(shiny)   # ShinyApp package
library(tidyverse)   # tidyverse suite (ggplot2, dplyr, etc.)
library(bslib)   # alternate base page for ui (`page_sidebar`)
library(sf)   # simple features package for spatial data
#
#----------------------------------
# LOAD BASE DATA OBJECTS:
## check if 1 object exists in the environment already; load all if it does not
if(!exists("WA_nodes_coords")) {
  # Names, locations, and attributes of possible release sites
  load("data/releasesites_WADFW.Rdata")
  # Edge-point (node) coordinates of SSM grid cells
  load("data/WA_nodes_coords.Rdata")
  # Grid cell centroid (nele) coordinates of SSM grid cells
  load("data/WA_neles_coords.Rdata")
  # randomly selected larval tracks from Padilla Bay and Fidalgo Bay
  load("data/oly_tracks.Rdata")
  # distance measures for selected larval tracks
  load("data/oly_dist.Rdata")
  # shapefile for Salish Sea Model boundary
  load("data/ssm_poly.Rdata")
}

## create vectors for choices of inputs
### for now, limiting to a single release site and year. will add in modifiable options depending on the year chosen
site_names <- data.frame(site_name = filter(releasesites_WADFW, id %in% c("FB", "PB"))$site,
                         site_id = c("FB", "PB"))

ssm_years <- data.frame(year = c("2017"), id = c(17)) 
behaviors <- data.frame(behavior = c("No Swimming", "Phototactic", "Ontogenetic"),
                        id = c("none", "photo", "onto"))
#
# 
#----------------------------------
# USER INTERFACE:
ui <- page_sidebar(
  # title - needs a fancier one for sure
  title = "Visualizing Olympia Oyster Larval Transport Data",
  # Fill in the sidebar panel - this contains the inputs to call specific datasets
  sidebar = sidebar(
    # selection input for release site name
    selectInput(inputId = "site", label = "Release Site", 
                choices = site_names$site_name,
                width = "100%"),
    sliderInput(inputId = "n_tracks", label = "Number of Tracks",
                min = 1, max = 10, value = 5),
    # selection input for swimming behavior
    selectInput(inputId = "behavior", label = "Swimming Behavior",
                choices = behaviors$behavior,
                width = "100%"),
    # create an action button to update the map only when triggered
    actionButton(inputId = "map_button", label = "Create Map", width = "100%")
  ),
  # App description
  fluidRow(
    p(),
    # add link to data on GitHub
    uiOutput("link")
  ),
  # Main Panel: Map Output
  fluidRow(
    column(12, plotOutput("map"))
  ),
  
  fluidRow(
    p("App History:"),
    p("3 June 2025 LG: Updated so all distances and larval tracks are contained within a single file. User input filters this file based on release site and behavior."),
    p("Last Edited: 3-June-2025"),
    p("21 May 2025 LG: \nThis application uses data output from the Olympia oyster Larval Transport Model (in progress) and visualizes settlement areas or larval tracks, depending on user input. The primary output is a ggplot map, either showing the first settlement points larvae reach during the dispersal period, or the entire movement path of the larvae during dispersal."),
    p("28 May 2025 LG: \nApp Update: I selected only 10 larval particle tracks from two sites (Fidalgo Bay and Padilla Bay) across all three behaviors (none, phototactic, and ontogenetic) to get some of the multi-site functionality while keeping file size low."),
    p("This is a preliminary version, and due to file size, only 1 release site with 3 swimming behaviors is available to start. I intend to update it so that either different sites' data can be loaded, or uploaded individually, then run through the visualization app."),
    p("In the first attempt, I am having trouble loading data within an observeEvent function, as well as fitting my rather large wrangling-and-plotting loop into shiny syntax. For now, I am leaving it as the basic framework, and in place of the map, a simple (and slightly nonsensical) summary table is printed instead. Once I get all of the reactive objects set up, it should be pretty simple to convert the rest of the loop into shiny.")
  )
  
)
#
#----------------------------------
# SERVER:
server <- function(input, output, session) {
  # link to github with source code
  url <- a("github.com/lukeghallahorne", href="https://github.com/lukeghallahorne/")
  output$link <- renderUI({
    tagList("Link to source code:", url)
  })
  
  #set_dist_file <- reactive({paste0("oly_data/20",year_id(),"/",site_id(),"_larvatracks/",
  #      site_id(),"_477_",behav_id(),"_sz220_settled_dist.Rdata")})
  site_id <- reactive({as.character(filter(releasesites_WADFW, site == input$site)$id)})
  year_id <- reactive({filter(ssm_years, year == input$year)$id})
  behav_id <- reactive({filter(behaviors, behavior == input$behavior)$id})
  # filter larval distance measures
  distances <- reactive({
    filter(oly_dist, bay == site_id() & behavior == behav_id())
  })
  # filter larval tracks by bay & behavior
  tracks <- reactive({
    filter(oly_tracks, bay == site_id() & behavior == behav_id())
  })
  # filter larval tracks by number of tracks selected
  tracks2plot <- reactive({
    filter(tracks(), site_track %in% unique(tracks()$site_track)[1:input$n_tracks])
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
  track_map <- reactive({ggplot() +
      geom_sf(data = ssm_poly, alpha = 0.5) +
      geom_path(data = tracks2plot(),
                aes(x = x, y = y, 
                    group = site_track, color = site_track)) +
      labs(x = NULL, y = NULL) +
      guides(color = "none") +
      coord_sf(xlim = track_lims()$xlim, 
               ylim = track_lims()$ylim)}) |>
    bindEvent(input$map_button)
  
  output$map <- renderPlot({
    track_map()
  }, width = "auto")
  
  output$dimension_display <- renderText({
    paste(input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1])
  })

}
#
#----------------------------------
# RUN APPLICATION:
shinyApp(ui, server)

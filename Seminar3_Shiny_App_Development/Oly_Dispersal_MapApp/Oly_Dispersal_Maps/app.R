# This is the first version of an application to pull, wrangle, and visualize data output from my Olympia oyster Larval Dispersal Model. This will ultimately be the companion app to an application for the model itself. However, given average runtime for the dispersal model, I started with the visualization app since it will load far faster for peer reviewers. A very similar approach and user interface will be used in that app as well.
#
# LIBRARY:
library(shiny)   # ShinyApp package
library(tidyverse)   # tidyverse suite (ggplot2, dplyr, etc.)
library(bslib)   # alternate base page for ui (`page_sidebar`)
#
#----------------------------------
# LOAD BASE DATA OBJECTS:
## check if 1 object exists in the environment already; load all if it does not
if(!exists("WA_nodes_coords")) {
  # Names, locations, and attributes of possible release sites
  load("oly_data/releasesites_WADFW.Rdata")
  # Edge-point (node) coordinates of SSM grid cells
  load("oly_data/WA_nodes_coords.Rdata")
  # Grid cell centroid (nele) coordinates of SSM grid cells
  load("oly_data/WA_neles_coords.Rdata")
  # temp - load FB larval track data
  load("oly_data/2017/FB_larvatracks/FB_477_none_sz220_settled_dist.Rdata")
  load("oly_data/2017/FB_larvatracks/FB_477_none_sz220_settled_tracks.Rdata")
  load("oly_data/2017/FB_larvatracks/FB_477_photo_sz220_settled_dist.Rdata")
  load("oly_data/2017/FB_larvatracks/FB_477_photo_sz220_settled_tracks.Rdata")
  load("oly_data/2017/FB_larvatracks/FB_477_onto_sz220_settled_dist.Rdata")
  load("oly_data/2017/FB_larvatracks/FB_477_onto_sz220_settled_dist.Rdata")
}
# test function - using load() with Rdata files within an observeEvent
loading <- function(datafile) {
  e <- new.env()
  load(datafile, envir = e)
  return(e)
}
## create vectors for choices of inputs
### for now, limiting to a single release site and year. will add in modifiable options depending on the year chosen
site_names <- filter(releasesites_WADFW, id %in% "FB")$site
ssm_years <- data.frame(year = c("2017"), id = c(17)) 
behaviors <- data.frame(behavior = c("No Swimming", "Phototactic", "Ontogenetic"),
                        id = c("none", "photo", "onto"))
#
# 
#----------------------------------
# USER INTERFACE:
ui <- page_sidebar(
  # title - needs a fancier one for sure
  title = "Visualizing Olympia oyster Larval Transport Data",
  # Fill in the sidebar panel - this contains the inputs to call specific datasets
  sidebar = sidebar(
    # selection input for release site name
    selectInput(inputId = "site", label = "Release Site", 
                choices = site_names,
                width = "100%"),
    # selection input for SSM year
    selectInput(inputId = "year", label = "SSM Year",
                choices = ssm_years$year,
                width = "100%"),
    # selection input for swimming behavior
    selectInput(inputId = "behavior", label = "Swimming Behavior",
                choices = behaviors$behavior,
                width = "100%"),
    # create an action button to load the specific data
    actionButton(inputId = "data_button", label = "Load Data - NOT WORKING", width = "100%"),
    # create an action button to update the map only when triggered
    actionButton(inputId = "map_button", label = "Create Map", width = "100%")
  ),
  # App description
  fluidRow(
    p("21 May 2025 LG: \nThis application uses data output from the Olympia oyster Larval Transport Model (in progress) and visualizes settlement areas or larval tracks, depending on user input. The primary output is a ggplot map, either showing the first settlement points larvae reach during the dispersal period, or the entire movement path of the larvae during dispersal."),
    p("This is a preliminary version, and due to file size, only 1 release site with 3 swimming behaviors is available to start. I intend to update it so that either different sites' data can be loaded, or uploaded individually, then run through the visualization app."),
    p("In the first attempt, I am having trouble loading data within an observeEvent function, as well as fitting my rather large wrangling-and-plotting loop into shiny syntax. For now, I am leaving it as the basic framework, and in place of the map, a simple (and slightly nonsensical) summary table is printed instead. Once I get all of the reactive objects set up, it should be pretty simple to convert the rest of the loop into shiny."),
    p("Last Edited: 21-May-2025 LG"),
    # add link to data on GitHub
    uiOutput("link")
  ),
  # Main Panel: Map Output
  fluidRow(
    column(12, tableOutput("map"))
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
  # create reactive objects for site, year, and behavior chosen
  site_id <- reactive({as.character(filter(releasesites_WADFW, site == input$site)$id)})
  year_id <- reactive({filter(ssm_years, year == input$year)$id})
  behav_id <- reactive({filter(behaviors, behavior == input$behavior)$id})
  #set_dist_file <- reactive({paste0("oly_data/20",year_id(),"/",site_id(),"_larvatracks/",
   #      site_id(),"_477_",behav_id(),"_sz220_settled_dist.Rdata")})
  
  # load larval track data and wrangle it into shape - button not working
  ## temporarily loading before UI
  #observeEvent(input$data_button, {
  #  req(input$data_button)
   # rm(list = ls(pattern = "list"))
   # load(set_dist_file())
    #set_dist <- reactive({get(site_id(),"_477_",behav_id(),"_sz220_set_dist")}) # |>
      #mutate(behavior = factor(behavior, levels = behaviors))
#  })

  set_dist <- reactive({get(paste0(site_id(),"_477_",behav_id(),"_set_dist"))})
  # test summary to make sure the loading is working
  test_summ <- reactive({summary(set_dist())})
  
  output$map <- renderTable(test_summ())
}
#
#----------------------------------
# RUN APPLICATION:
shinyApp(ui, server)

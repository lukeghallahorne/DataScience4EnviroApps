# Library
library(shiny)
library(vroom)
library(tidyverse)

#> Check for data within environment.
#> Load if necessary.
if(!exists("injuries")) {
  injuries <- vroom::vroom("neiss/injuries.tsv.gz")
  products <- vroom::vroom("neiss/products.tsv")
  population <- vroom::vroom("neiss/population.tsv")
  
}
#> Create product codes vector with titles as names for each value.
prod_codes <- setNames(products$prod_code, products$title)
#> Create function to clean up tables
#> Counts the top five of the selected variable, then lumps the rest into other
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

#> UI
ui <- fluidPage(
  # Text for description of prototype app.
  fluidRow(
    column(8,
           p("This is a prototype app to summarize and visualize ER injuries data, based on Chapter 4 of Mastering Shiny."),
           p("Last Edited: 2-May-2025 LG")),
    # Add Mastering Shiny hyperlink
    column(4, 
           uiOutput("link"))),
  # Product Codes input
  fluidRow(
    # User chooses which product to examine.
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    # User selects either rate or count to display in the plot.
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  # Output tables for diagnosis, body part, and location
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  # Output plot for injury rate by age & sex
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  # Create Action Button to pull a random story from the selected data
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)

#> Server
server <- function(input, output, session) {
  # Mastering Shiny link
  url <- a("mastering-shiny.org", href="https://www.mastering-shiny.org/")
  output$link <- renderUI({
    tagList("Link to source:", url)
  })
  # filter data based on user input
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # Output table for diagnosis
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  # Output table for body part
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  # Output table for location
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  # Create reactive object for injury rates per 10000 individuals
  summary_age <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  # Output plot for estimated number of injuries by age + sex
  # User choice of showing injuries per 10000 or total counts
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary_age() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary_age() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  # Output a random story from the user-selected dataset
  ## Create the reactive event
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% pull(narrative) %>% sample(1)
  )
  ## render the output
  output$narrative <- renderText(narrative_sample())
}

# Run the application 
shinyApp(ui = ui, server = server)

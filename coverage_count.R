# A shiny app that calculates coverage 

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Dilution coverage calculation"),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        "total_variants",
        "Total expected variants",
        5000,
        min = 0,
        max = 100000
      ),
      numericInput(
        "dilutions",
        "Number of dilutions (1:10)",
        5,
        min = 0,
        max = 10
      ),
      numericInput(
        "plated_volume",
        "Total volume plated (in µL)",
        200,
        min = 0,
        max = 1000
      ),
      numericInput(
        "dilution_volume",
        "Total volume of each dilution (in µL)",
        1000,
        min = 0,
        max = 2000
      ),
      numericInput(
        "outgrowth_volume",
        "Starting outgrowth volume (in µL)",
        1000,
        min = 0,
        max = 10000
      ),
      numericInput(
        "colony_count_1",
        "Colony count on highest dilution plate",
        10,
        min = 0,
        max = 2000
      ),
      numericInput(
        "colony_count_2",
        "Colony count on next dilution plate",
        100,
        min = 0,
        max = 2000
      ),
      numericInput(
        "colony_count_3",
        "Colony count on third dilution plate",
        1000,
        min = 0,
        max = 2000
      )
    ),
    mainPanel(
      textOutput("text1"),
      textOutput("text2")
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {

  inferred_count_1 <- reactive ({
    input$colony_count_1 * (10^(input$dilutions)) * 
    (input$dilution_volume / input$plated_volume) * (input$outgrowth_volume / (input$dilution_volume / 10 ) )
  })
  
  inferred_count_2 <- reactive ({
    input$colony_count_2 * (10^(input$dilutions - 1)) * 
    (input$dilution_volume / input$plated_volume) * (input$outgrowth_volume / (input$dilution_volume / 10 ) )
  })
  
  inferred_count_3 <- reactive ({
    input$colony_count_3 * (10^(input$dilutions - 2)) * 
    (input$dilution_volume / input$plated_volume) * (input$outgrowth_volume / (input$dilution_volume / 10 ) )
  })
  
  inferred_count <- reactive({
    mean(c(inferred_count_1(), inferred_count_2(), inferred_count_3()))
  })
  
  inferred_coverage <- reactive ({
    inferred_count() / input$total_variants
  })
  
  output$text1 <- renderText({paste("The mean inferred count of transformants in the outgrowth is: ", inferred_count())})
  output$text2 <- renderText({paste("This implies the total coverage is ", inferred_coverage(), "X")})
  
}
# Run the application 
shinyApp(ui = ui, server = server)
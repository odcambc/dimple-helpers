# A shiny app that calculates coverage 

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Sequencing reads calculations"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("desired_coverage", "Desired coverage (per variant):",
                  min = 0, max = 1000,
                  value = 200),
      numericInput(
        "gene_length_nt",
        "Gene length (in nt):",
        900,
        min = 0,
        max = NA,
        step = NA,
        width = NULL
      ),
      numericInput(
        "total_amplicon_length_nt",
        "Total amplicon length, including non-variable regions (in nt):",
        1000,
        min = 0,
        max = NA,
        step = NA,
        width = NULL
      ),
      sliderInput("variants_per_position", "Variants per position:",
                  min = 0, max = 50,
                  value = 20),
      sliderInput("number_of_samples", "Total number of samples:",
                  min = 0, max = 384,
                  value = 1),
      selectInput("sequencer", "Sequencer: ",
                  c("NovaSeq SP 300", "NovaSeq SP 500", "NovaSeq S1", "NovaSeq S4",
                    "NovaSeq X 1.5B", "NovaSeq X 10B", "NovaSeq X 25B", 
                    "MiSeq Nano 300", "MiSeq Nano 500", "MiSeq Micro 300",
                    "MiSeq v2 300", "MiSeq v2 500", "MiSeq v3 600", 
                    "NextSeq 550 high output 300", "NextSeq 550 mid output 300",
                    "NextSeq 1000/2000 P1 300", "NextSeq 1000/2000 P2 300", "NextSeq 1000/2000 P3 300",
                    "NextSeq 1000/2000 P1 600", "NextSeq 1000/2000 P2 600", "NextSeq 1000/2000 P3 600")),
      textOutput("text5")
    ),
    mainPanel(
      textOutput("text1"),
      textOutput("text2"),
      textOutput("text3"),
      textOutput("text4")
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  sequencer_data <- data.frame(
    instrument = c("NovaSeq SP 300", "NovaSeq SP 500", "NovaSeq S1", "NovaSeq S4",
             "NovaSeq X 1.5B", "NovaSeq X 10B", "NovaSeq X 25B", 
             "MiSeq Nano 300", "MiSeq Nano 500", "MiSeq Micro 300",
             "MiSeq v2 300", "MiSeq v2 500", "MiSeq v3 600",
             "NextSeq 550 high output 300", "NextSeq 550 mid output 300",
             "NextSeq 1000/2000 P1 300", "NextSeq 1000/2000 P2 300", "NextSeq 1000/2000 P3 300",
             "NextSeq 1000/2000 P1 600", "NextSeq 1000/2000 P2 600", "NextSeq 1000/2000 P3 600"),
    reads_per_lane = c(400, 400, 800, 2500,
                       200, 1250, 3250,
                       1, 1, 4,
                       12, 12, 22,
                       400, 150,
                       100, 400, 1200,
                       100, 300, 1200),
    read_length = c(300, 500, 300, 300,
                    300, 300, 300,
                    300, 500, 300, 
                    300, 500, 600,
                    300, 300,
                    300, 300, 300,
                    600, 600, 600)*0.6
  )
  
  sequencer_flowcell_bases <- reactive({
    sequencer_data[sequencer_data$instrument == input$sequencer,]$read_length / 0.6
  })

  sequencer_reads <- reactive({
    sequencer_data[sequencer_data$instrument == input$sequencer,]$reads_per_lane
  })
  
  sequencer_mean_insert <- reactive({
    sequencer_data[sequencer_data$instrument == input$sequencer,]$read_length
  })
  
  total_variants <- reactive({
    (input$gene_length_nt/3) * input$variants_per_position
  })
  
  total_reads_required <- reactive({
    (((input$total_amplicon_length_nt / sequencer_mean_insert()) * total_variants()) * input$desired_coverage) / 1000000
  })
  
  total_reads_all_samples <- reactive({
    (total_reads_required() * input$number_of_samples)
  })
  
  output$text1 <- renderText({paste("Each sample has ", total_variants(), " total variants")})
  output$text2 <- renderText({paste("For ", input$desired_coverage, "X coverage, each sample requires ",
                                    format(total_reads_required(), digits = 2), "M reads")})
  output$text3 <- renderText({paste("For all ", input$number_of_samples, "samples, this requires ",
                                    format(total_reads_all_samples(), digits = 2), "M reads")})
  output$text4 <- renderText({paste("This will use ", format(100*(total_reads_all_samples() / sequencer_reads()), digits = 2),
                                    "percent of a ", input$sequencer, "lane" )})
  output$text5 <- renderText({
    paste("2 x ", sequencer_flowcell_bases() / 2, " bp")
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
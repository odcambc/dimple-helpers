# Combined helper app for variant-library experiments
# Tab 1: Sequencing reads calculator (originally app.R)
# Tab 2: Dilution coverage calculator (originally coverage_count.R)

library(shiny)

if (file.exists(file.path("R", "power_models.R"))) {
  source(file.path("R", "power_models.R"))
} else {
  stop("Missing R/power_models.R (required for Power / design tab).")
}

# ── Sequencer data ─────────────────────────────────────────────────────────────
# read_length is stored as effective insert length (nominal * 0.6).
# sequencer_flowcell_bases() divides by 0.6 to recover the display value.
sequencer_data <- data.frame(
  instrument = c(
    "NovaSeq SP 300", "NovaSeq SP 500", "NovaSeq S1", "NovaSeq S4",
    "NovaSeq X 1.5B", "NovaSeq X 10B", "NovaSeq X 25B",
    "MiSeq Nano 300", "MiSeq Nano 500", "MiSeq Micro 300",
    "MiSeq v2 300", "MiSeq v2 500", "MiSeq v3 600",
    "NextSeq 550 high output 300", "NextSeq 550 mid output 300",
    "NextSeq 1000/2000 P1 300", "NextSeq 1000/2000 P2 300", "NextSeq 1000/2000 P3 300",
    "NextSeq 1000/2000 P1 600", "NextSeq 1000/2000 P2 600", "NextSeq 1000/2000 P3 600"
  ),
  reads_per_lane = c(
    400, 400, 800, 2500,
    200, 1250, 3250,
    1, 1, 4,
    12, 12, 22,
    400, 150,
    100, 400, 1200,
    100, 300, 1200
  ),
  read_length = c(
    300, 500, 300, 300,
    300, 300, 300,
    300, 500, 300,
    300, 500, 600,
    300, 300,
    300, 300, 300,
    600, 600, 600
  ) * 0.6
)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Dimple Helpers",

  # ── Tab 1: Sequencing reads ────────────────────────────────────────────────
  tabPanel(
    "Sequencing reads",
    sidebarLayout(
      sidebarPanel(
        sliderInput("desired_coverage", "Desired coverage (per variant):",
                    min = 0, max = 1000, value = 200),
        numericInput("gene_length_nt", "Gene length (in nt):",
                     900, min = 0),
        numericInput("total_amplicon_length_nt",
                     "Total amplicon length, including non-variable regions (in nt):",
                     1000, min = 0),
        sliderInput("variants_per_position", "Variants per position:",
                    min = 0, max = 50, value = 20),
        sliderInput("number_of_samples", "Total number of samples:",
                    min = 0, max = 384, value = 1),
        selectInput("sequencer", "Sequencer:",
                    choices = sequencer_data$instrument),
        textOutput("text5")
      ),
      mainPanel(
        textOutput("text1"),
        textOutput("text2"),
        textOutput("text3"),
        textOutput("text4")
      )
    )
  ),

  # ── Tab 2: Dilution coverage ───────────────────────────────────────────────
  tabPanel(
    "Dilution coverage",
    sidebarLayout(
      sidebarPanel(
        numericInput("total_variants", "Total expected variants",
                     5000, min = 0, max = 100000),
        numericInput("dilutions", "Number of dilutions (1:10)",
                     5, min = 0, max = 10),
        numericInput("plated_volume", "Total volume plated (in µL)",
                     200, min = 0, max = 1000),
        numericInput("dilution_volume", "Total volume of each dilution (in µL)",
                     1000, min = 0, max = 2000),
        numericInput("outgrowth_volume", "Starting outgrowth volume (in µL)",
                     1000, min = 0, max = 10000),
        numericInput("colony_count_1", "Colony count on highest dilution plate",
                     10, min = 0, max = 2000),
        numericInput("colony_count_2", "Colony count on next dilution plate",
                     100, min = 0, max = 2000),
        numericInput("colony_count_3", "Colony count on third dilution plate",
                     1000, min = 0, max = 2000)
      ),
      mainPanel(
        textOutput("dil_text1"),
        textOutput("dil_text2")
      )
    )
  ),

  # ── Tab 3: Power / design ───────────────────────────────────────────────────
  tabPanel(
    "Power / design",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "pow_model",
          "Model",
          choices = c("Poisson LFC (delta method)" = "poisson_lfc")
        ),
        checkboxInput(
          "pow_use_seq_depth",
          "Use mean depth from Sequencing reads (= desired coverage per variant)",
          value = TRUE
        ),
        conditionalPanel(
          condition = "!input.pow_use_seq_depth",
          numericInput(
            "pow_mean_depth",
            "Mean reads per variant per replicate:",
            value = 200, min = 0.1, max = 1e6, step = 1
          )
        ),
        numericInput(
          "pow_n_rep",
          "Independent replicates per condition:",
          value = 3, min = 1, max = 100, step = 1
        ),
        numericInput(
          "pow_delta_log2",
          "True log2 fold change to detect:",
          value = 1, min = -10, max = 10, step = 0.25
        ),
        numericInput(
          "pow_alpha",
          HTML("Two-sided &alpha;:"),
          value = 0.05, min = 1e-6, max = 0.5, step = 0.005
        ),
        numericInput(
          "pow_target_power",
          "Target power (for min-depth estimate):",
          value = 0.8, min = 0.5, max = 0.99, step = 0.05
        ),
        hr(),
        h4("Model assumptions (v1)"),
        tags$ul(
          tags$li(
            "Poisson counts; delta-method SE of log2 fold change; normal two-sided test."
          ),
          tags$li(
            "Richer models (overdispersion, hierarchy, simulation) can plug into ",
            tags$code("pow_compute()"), " later without changing tab layout."
          )
        )
      ),
      mainPanel(
        plotOutput("pow_plot", height = 340),
        verbatimTextOutput("pow_text1"),
        verbatimTextOutput("pow_text2")
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output) {

  # ── Sequencing reads reactives ─────────────────────────────────────────────
  sequencer_row <- reactive({
    sequencer_data[sequencer_data$instrument == input$sequencer, ]
  })

  sequencer_flowcell_bases <- reactive({ sequencer_row()$read_length / 0.6 })
  sequencer_reads          <- reactive({ sequencer_row()$reads_per_lane })
  sequencer_mean_insert    <- reactive({ sequencer_row()$read_length })

  total_variants_seq <- reactive({
    (input$gene_length_nt / 3) * input$variants_per_position
  })

  total_reads_required <- reactive({
    (((input$total_amplicon_length_nt / sequencer_mean_insert()) *
        total_variants_seq()) * input$desired_coverage) / 1e6
  })

  total_reads_all_samples <- reactive({
    total_reads_required() * input$number_of_samples
  })

  output$text1 <- renderText({
    paste("Each sample has", total_variants_seq(), "total variants")
  })
  output$text2 <- renderText({
    paste("For", input$desired_coverage, "X coverage, each sample requires",
          format(total_reads_required(), digits = 2), "M reads")
  })
  output$text3 <- renderText({
    paste("For all", input$number_of_samples, "samples, this requires",
          format(total_reads_all_samples(), digits = 2), "M reads")
  })
  output$text4 <- renderText({
    paste("This will use",
          format(100 * (total_reads_all_samples() / sequencer_reads()), digits = 2),
          "percent of a", input$sequencer, "lane")
  })
  output$text5 <- renderText({
    paste("2 x", sequencer_flowcell_bases() / 2, "bp")
  })

  # ── Dilution coverage reactives ────────────────────────────────────────────
  # Each plate estimate corrects for its dilution factor, the volume ratio
  # between dilution tube and plated aliquot, and scales back to outgrowth volume.
  scale_factor <- reactive({
    (input$dilution_volume / input$plated_volume) *
      (input$outgrowth_volume / (input$dilution_volume / 10))
  })

  inferred_count_1 <- reactive({
    input$colony_count_1 * (10 ^ input$dilutions) * scale_factor()
  })
  inferred_count_2 <- reactive({
    input$colony_count_2 * (10 ^ (input$dilutions - 1)) * scale_factor()
  })
  inferred_count_3 <- reactive({
    input$colony_count_3 * (10 ^ (input$dilutions - 2)) * scale_factor()
  })

  inferred_count <- reactive({
    mean(c(inferred_count_1(), inferred_count_2(), inferred_count_3()))
  })

  inferred_coverage <- reactive({
    inferred_count() / input$total_variants
  })

  output$dil_text1 <- renderText({
    paste("The mean inferred count of transformants in the outgrowth is:",
          format(inferred_count(), big.mark = ",", scientific = FALSE))
  })
  output$dil_text2 <- renderText({
    paste("This implies the total coverage is",
          format(inferred_coverage(), digits = 3), "X")
  })

  # ── Power / design (between-condition LFC vs depth) ──────────────────────────
  pow_mean_depth_effective <- reactive({
    if (isTRUE(input$pow_use_seq_depth)) {
      input$desired_coverage
    } else {
      input$pow_mean_depth
    }
  })

  pow_params <- reactive({
    list(
      mean_depth = pow_mean_depth_effective(),
      n_rep = input$pow_n_rep,
      delta_log2 = input$pow_delta_log2,
      alpha = input$pow_alpha,
      target_power = input$pow_target_power
    )
  })

  pow_out <- reactive({
    validate(
      need(input$pow_n_rep >= 1, "Replicates per condition must be at least 1."),
      need(is.finite(pow_mean_depth_effective()) && pow_mean_depth_effective() > 0,
           "Mean depth must be positive (adjust Sequencing reads or manual depth)."),
      need(input$pow_delta_log2 != 0, "log2 fold change must be non-zero for this calculator."),
      need(is.finite(input$pow_alpha) && input$pow_alpha > 0 && input$pow_alpha < 1,
           "Alpha must lie in (0, 1).")
    )
    pow_compute(input$pow_model, pow_params())
  })

  output$pow_plot <- renderPlot({
    z <- pow_out()
    par(mar = c(4.2, 4.2, 1, 1))
    plot(
      z$curve$mean_depth, z$curve$power,
      type = "l", lwd = 2,
      log = "x",
      xlab = "Mean reads per variant per replicate",
      ylab = "Approximate power",
      ylim = c(0, 1),
      las = 1
    )
    abline(v = z$mean_depth, lty = 2, col = "gray35")
    if (is.finite(z$min_depth_for_target) && isTRUE(z$min_depth_ok)) {
      abline(v = z$min_depth_for_target, lty = 3, col = "steelblue4")
    }
    abline(h = z$min_depth_target_power, lty = 3, col = "steelblue4", lwd = 0.5)
    legend(
      "bottomright",
      legend = c(
        "Power vs depth",
        "Current mean depth",
        "Target power (horizontal); min-depth (vertical when shown)"
      ),
      col = c("black", "gray35", "steelblue4"),
      lty = c(1, 2, 3),
      lwd = c(2, 1, 1),
      bty = "n",
      cex = 0.85
    )
  })

  output$pow_text1 <- renderPrint({
    z <- pow_out()
    cat(z$model_label, "\n\n", sep = "")
    cat(sprintf(
      "Expected counts per condition: m1 = %.2f, m2 = %.2f (R = %g, depth = %g)\n",
      z$m1, z$m2, z$n_rep, z$mean_depth
    ))
    cat(sprintf("SE(log2 FC) ≈ %.4g\n", z$se_lfc))
    cat(sprintf("Approximate two-sided power at stated depth: %.1f%%\n", 100 * z$power))
    if (isTRUE(z$low_count_warning)) {
      cat(
        "\n** Warning: expected count < 5 in at least one arm; ",
        "normal / delta-method approximation is unreliable.\n",
        sep = ""
      )
    }
  })

  output$pow_text2 <- renderPrint({
    z <- pow_out()
    if (isTRUE(z$min_depth_ok) && is.finite(z$min_depth_for_target)) {
      cat(sprintf(
        "Mean depth per variant per replicate for ~%.0f%% power: %.3f reads\n",
        100 * z$min_depth_target_power,
        z$min_depth_for_target
      ))
    } else {
      cat("Min-depth estimate: ", z$min_depth_message, "\n", sep = "")
    }
    cat("\nAssumptions:\n")
    for (ln in z$assumptions_bullets) cat(" * ", ln, "\n", sep = "")
  })
}

shinyApp(ui = ui, server = server)

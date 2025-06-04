# Load required packages
library(shiny)
library(ggplot2)
library(scales)

# UI layout
ui <- fluidPage(
  titlePanel("Health Economics: Cost-Effectiveness Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Standard Treatment"),
      numericInput("cost_std", "Cost (£)", value = 1000, min = 0),
      numericInput("qaly_std", "QALYs", value = 1, min = 0),
      
      h4("New Treatment"),
      numericInput("cost_new", "Cost (£)", value = 2000, min = 0),
      numericInput("qaly_new", "QALYs", value = 1.5, min = 0),
      
      actionButton("calc", "Calculate ICER")
    ),
    
    mainPanel(
      h3("Results"),
      verbatimTextOutput("icer_result"),
      plotOutput("ce_plot")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  icer_data <- eventReactive(input$calc, {
    delta_cost <- input$cost_new - input$cost_std
    delta_qaly <- input$qaly_new - input$qaly_std
    icer <- ifelse(delta_qaly == 0, NA, delta_cost / delta_qaly)
    
    data.frame(
      Treatment = c("Standard", "New"),
      Cost = c(input$cost_std, input$cost_new),
      QALY = c(input$qaly_std, input$qaly_new),
      ICER = icer
    )
  })
  
  output$icer_result <- renderPrint({
    data <- icer_data()
    if (is.na(data$ICER[1])) {
      cat("Error: Cannot calculate ICER – QALY difference is 0.")
    } else {
      cat("Incremental Cost-Effectiveness Ratio (ICER): £", round(data$ICER[1], 2), " per QALY gained")
    }
  })
  
  output$ce_plot <- renderPlot({
    data <- icer_data()
    if (!is.na(data$ICER[1])) {
      ggplot(data, aes(x = QALY, y = Cost, label = Treatment)) +
        geom_point(size = 4) +
        geom_text(vjust = -1.5, fontface = "bold") +
        labs(title = "Cost-Effectiveness Plane", x = "QALYs", y = "Cost (£)") +
        theme_minimal() +
        scale_y_continuous(labels = comma) +
        scale_x_continuous(limits = c(0, max(data$QALY) + 1)) +
        geom_segment(aes(x = data$QALY[1], y = data$Cost[1],
                         xend = data$QALY[2], yend = data$Cost[2]),
                     arrow = arrow(length = unit(0.2, "cm")), color = "red", size = 1)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

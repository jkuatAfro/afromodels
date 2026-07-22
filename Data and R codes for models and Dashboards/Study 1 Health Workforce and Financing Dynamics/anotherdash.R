library(shiny)
library(ggplot2)
library(dplyr)

# ---- Prepare data ----
datr$threshold_group <- ifelse(datr$Workforce_Density < 38,
                               "Below Threshold", "Above Threshold")

datr$distance_to_threshold <- datr$Workforce_Density - 38

# ---- UI ----
ui <- fluidPage(
  titlePanel("Workforce Density Threshold Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:",
                  choices = unique(datr$Country),
                  selected = unique(datr$Country)[1]),
      
      sliderInput("densityRange", "Workforce Density Range:",
                  min = min(datr$Workforce_Density, na.rm = TRUE),
                  max = max(datr$Workforce_Density, na.rm = TRUE),
                  value = range(datr$Workforce_Density, na.rm = TRUE)),
      
      checkboxInput("show_threshold", "Show Threshold Line (38)", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Country Details", tableOutput("countryTable")),
        tabPanel("Summary", verbatimTextOutput("summaryStats"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output) {
  
  # Filtered data
  filtered_data <- reactive({
    datr %>%
      filter(Workforce_Density >= input$densityRange[1],
             Workforce_Density <= input$densityRange[2])
  })
  
  # ---- Scatter Plot ----
  output$scatterPlot <- renderPlot({
    p <- ggplot(filtered_data(),
                aes(x = Workforce_Density, y = LE, color = threshold_group)) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(title = "Workforce Density vs Life Expectancy",
           x = "Workforce Density (per 10,000)",
           y = "Life Expectancy")
    
    if (input$show_threshold) {
      p <- p + geom_vline(xintercept = 38,
                          linetype = "dashed",
                          color = "red",
                          size = 1)
    }
    
    p
  })
  
  # ---- Country Table ----
  output$countryTable <- renderTable({
    datr %>%
      filter(Country == input$country) %>%
      select(Country, Year, Workforce_Density, LE,
             threshold_group, distance_to_threshold)
  })
  
  # ---- Summary Stats ----
  output$summaryStats <- renderPrint({
    df <- filtered_data()
    
    cat("Summary Statistics:\n\n")
    cat("Average Workforce Density:", mean(df$Workforce_Density, na.rm = TRUE), "\n")
    cat("Average Life Expectancy:", mean(df$LE, na.rm = TRUE), "\n\n")
    
    cat("Below Threshold:", sum(df$threshold_group == "Below Threshold"), "\n")
    cat("Above Threshold:", sum(df$threshold_group == "Above Threshold"), "\n")
  })
}

# ---- Run App ----
shinyApp(ui = ui, server = server)

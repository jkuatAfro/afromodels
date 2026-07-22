library(shiny)
library(ggplot2)
library(dplyr)

# -----------------------------
# Load data
# -----------------------------
coef_data <- read.csv("3sls_0coefficients_by_country.csv")

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { color: #0072BC; }
      h1, h2, h3, h4 { color: #0072BC; }
      .baseline-box {
        background-color: #E6F2FA;
        padding: 15px;
        border-radius: 10px;
      }
    "))
  ),
  
  titlePanel("Country-Specific Health Workforce Impact Simulation (3 SLS-based Model)"),
  
  fluidRow(
    
    # LEFT: POLICY
    column(3,
           
           h4("Policy Scenario"),
           
           selectInput("country", "Country:",
                       choices = unique(coef_data$Country)),
           
           sliderInput("nurses", "Increase Nurses (%)", 0, 50, 5),
           sliderInput("doctors", "Increase Doctors (%)", 0, 50, 5),
           sliderInput("time", "Time Horizon", 5, 30, 15)
    ),
    
    # MIDDLE: BASELINE + TARGETS
    column(3,
           
           div(class = "baseline-box",
               
               h4("Baseline Values"),
               
               numericInput("base_nurses", "Nurses density", 3),
               numericInput("base_doctors", "Doctors density", 1),
               numericInput("base_uhc", "UHC", 50),
               numericInput("base_daly", "DALYs", 5),
               numericInput("base_prod", "Productivity", 10),
               
               hr(),
               h4("Targets"),
               
               numericInput("target_uhc", "Target UHC", 80),
               numericInput("target_daly", "Target DALYs", 3),
               numericInput("target_prod", "Target Productivity", 15)
           )
    ),
    
    # RIGHT: OUTPUTS
    column(6,
           
           tabsetPanel(
             
             tabPanel("UHC", plotOutput("uhcPlot")),
             tabPanel("DALYs", plotOutput("dalyPlot")),
             tabPanel("Productivity", plotOutput("prodPlot")),
             
             tabPanel("Workforce Impact", verbatimTextOutput("impactText")),
             
             tabPanel("Interpretation", verbatimTextOutput("interpretation")),
             
             tabPanel("Intro Notes", uiOutput("introText")),
             
             tabPanel("Summary Findings", uiOutput("summaryText"))
           )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output) {
  
  get_coef <- function(df, name) {
    if (name %in% names(df)) return(df[[name]][1])
    return(0)
  }
  
  simulate <- reactive({
    
    df <- coef_data %>% filter(Country == input$country)
    T <- input$time
    
    # Coefficients
    lag_uhc   <- get_coef(df, "uhc_lag_UHC")
    doctors_c <- get_coef(df, "uhc_Doctors")
    nurses_c  <- get_coef(df, "uhc_Nursing_Midwifery")
    
    lag_daly  <- get_coef(df, "daly_lag_dalys")
    uhc_daly  <- get_coef(df, "daly_UHC_SCI")
    
    lag_prod  <- get_coef(df, "prod_lag_prod")
    daly_prod <- get_coef(df, "prod_log_dalys")
    
    # Baseline
    UHC  <- numeric(T)
    DALY <- numeric(T)
    PROD <- numeric(T)
    
    UHC[1]  <- input$base_uhc
    DALY[1] <- log(input$base_daly)
    PROD[1] <- log(input$base_prod)
    
    nurses  <- input$base_nurses * (1 + input$nurses/100)
    doctors <- input$base_doctors * (1 + input$doctors/100)
    
    for (t in 2:T) {
      
      UHC[t] <- lag_uhc * UHC[t-1] +
        nurses_c * nurses +
        doctors_c * doctors
      
      DALY[t] <- lag_daly * DALY[t-1] +
        uhc_daly * UHC[t]
      
      PROD[t] <- lag_prod * PROD[t-1] +
        daly_prod * DALY[t]
    }
    
    data.frame(
      Time = 1:T,
      UHC = UHC,
      DALY = exp(DALY),
      PROD = exp(PROD)
    )
  })
  
  # -----------------------------
  # PLOTS
  # -----------------------------
  
  output$uhcPlot <- renderPlot({
    df <- simulate()
    
    ggplot(df, aes(Time, UHC)) +
      geom_line(color = "#0072BC", size = 1.5) +
      geom_hline(yintercept = input$target_uhc,
                 linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "UHC Path", y = "UHC")
  })
  
  output$dalyPlot <- renderPlot({
    df <- simulate()
    
    ggplot(df, aes(Time, DALY)) +
      geom_line(color = "#0072BC", size = 1.5) +
      geom_hline(yintercept = input$target_daly,
                 linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "DALYs Path", y = "DALYs")
  })
  
  output$prodPlot <- renderPlot({
    df <- simulate()
    
    ggplot(df, aes(Time, PROD)) +
      geom_line(color = "#0072BC", size = 1.5) +
      geom_hline(yintercept = input$target_prod,
                 linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Productivity Path", y = "GDP per Worker")
  })
  
  # -----------------------------
  # WORKFORCE IMPACT
  # -----------------------------
  output$impactText <- renderText({
    
    df <- coef_data %>% filter(Country == input$country)
    
    lag_uhc   <- get_coef(df, "uhc_lag_UHC")
    doctors_c <- get_coef(df, "uhc_Doctors")
    nurses_c  <- get_coef(df, "uhc_Nursing_Midwifery")
    
    delta_nurses  <- input$base_nurses * (input$nurses/100)
    delta_doctors <- input$base_doctors * (input$doctors/100)
    
    short_run <- nurses_c * delta_nurses +
      doctors_c * delta_doctors
    
    long_run <- short_run / (1 - lag_uhc)
    
    paste0(
      "Short-run UHC impact: ", round(short_run,2), "\n",
      "Long-run UHC impact: ", round(long_run,2)
    )
  })
  
  # -----------------------------
  # INTERPRETATION
  # -----------------------------
  output$interpretation <- renderText({
    
    df <- simulate()
    
    paste0(
      "Over ", input$time, " years:\n\n",
      
      "UHC: ", round(df$UHC[1],1), " → ", round(tail(df$UHC,1),1), "\n",
      ifelse(tail(df$UHC,1) > df$UHC[1],
             "Service coverage improves.\n\n",
             "Limited improvement.\n\n"),
      
      "DALYs: ", round(df$DALY[1],1), " → ", round(tail(df$DALY,1),1), "\n",
      ifelse(tail(df$DALY,1) < df$DALY[1],
             "Health outcomes improve.\n\n",
             "Limited improvement.\n\n"),
      
      "Productivity: ", round(df$PROD[1],1), " → ", round(tail(df$PROD,1),1), "\n\n",
      
      "Workforce increases improve coverage first, followed by health and economic gains."
    )
  })
  
  # -----------------------------
  # TEXT PANELS
  # -----------------------------
  
  output$introText <- renderUI({
    HTML("
    <b>Introduction</b><br><br>
    This analysis is based on a dynamic system of equations estimated using Three-Stage Least Squares (3SLS).
    The global results indicate that workforce density (particularly nursing/midwifery) has a significant positive
    effect on service coverage (UHC), while some variables such as doctors and HALE were not consistently significant.<br><br>

Country-specific coefficients were then used to simulate how changes in workforce affect UHC,
health outcomes (DALYs), and productivity over time.<br><br>

Due to data limitations, the model is unstable or could not
ated for some countries. These are listed under the Data Limitations tab.

The dashboard allows users to:<br><br>

Adjust workforce levels (doctors and nurses)<br><br>
Set country baseline values<br><br>
Define target (optimum) levels<br><br>

Results show how the system evolves over time and how quickly targets 
         can be reached under different policy scenarios.")
  })
  
  output$summaryText <- renderUI({
    HTML("
    <b>Summary Findings</b><br><br>
    Workforce increases improve UHC first, followed by health and productivity.<br><br>
    Effects are gradual due to system persistence.<br><br>
    Sustained investment is required.
    ")
  })
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui, server)
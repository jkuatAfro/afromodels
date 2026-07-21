
library(shiny)
library(shinydashboard)
library(quadprog)
library(ggplot2)
library(DT)
library(scales)

ui <- dashboardPage(
  dashboardHeader(title = "Health Workforce skill mix Optimizer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("chart-line")),
      menuItem("Efficient Frontier", tabName = "frontier", icon = icon("project-diagram")),
      menuItem("Optimal Workforce allocation", tabName = "weights", icon = icon("balance-scale")),
      menuItem("Workforce Constraints", tabName = "Constraints", icon = icon("sliders-h")),
      menuItem(" Cadre Efficiency", tabName = "efficiency", icon = icon("table")),
      menuItem(
        "About",
        tabName = "about",
        icon = icon("info-circle")
      )
    ),
    
    sliderInput(
      "n_scen", "Number of scenarios",
      min = 500, max = 10000, value = 3000, step = 500
    ),
    
    sliderInput(
      "target_q", "Target return percentile",
      min = 10, max = 90, value = 80, step = 5
    ),
    
    numericInput(
      "budget", "Total budget, USD",
      value = 100000000,
      min = 100000,
      step = 1000000
    ),
    
    checkboxInput(
      "fixed_seed", "Use fixed seed for reproducibility",
      value = TRUE
    ),
    
    actionButton(
      "run", "Run Simulation",
      icon = icon("play"),
      class = "btn-primary"
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "about",
        
        fluidRow(
          
          box(
            width = 12,
            title = "About the Health Workforce Skill Mix Optimizer",
            status = "primary",
            solidHeader = TRUE,
            
            h3("Purpose"),
            
            p("The Health Workforce Skill Mix Optimizer is a decision-support tool that adapts Modern Portfolio Theory (MPT) to optimize health workforce allocation. Rather than maximizing financial return, the model identifies workforce allocations that maximize expected health impact while accounting for uncertainty."),
            
            h3("Methodology"),
            
            p("The model simulates multiple scenarios for optimal skill mix for different cadres. The optimized workforce mix represents the allocation within the five major health worker cadres, which together account for approximately 70% of the national health workforce")
            )
          )
        ),
            
      tabItem(
        tabName = "Dashboard",
        fluidRow(
          valueBoxOutput("exp_return_box"),
          valueBoxOutput("risk_box"),
          valueBoxOutput("gain_box")
        ),
        fluidRow(
          valueBoxOutput("total_daly_box"),
          valueBoxOutput("current_daly_box"),
          valueBoxOutput("n_scen_box")
        ),
        fluidRow(
          box(
            width = 12,
            title = "Current Health Workforce Stock",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("stock_table")
          )
        )
      ),
      
      tabItem(
        tabName = "frontier",
        fluidRow(
          box(
            width = 12,
            title = "Efficient Frontier with Current and Optimal Portfolios",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("frontier_plot", height = 460)
          )
        )
      ),
      
      tabItem(
        tabName = "weights",
        fluidRow(
          box(
            width = 6,
            title = "Current vs Optimal Budget Shares",
            status = "success",
            solidHeader = TRUE,
            plotOutput("weights_plot", height = 420)
          ),
          box(
            width = 6,
            title = "Optimal skill mix Results",
            status = "success",
            solidHeader = TRUE,
            DTOutput("weights_table")
          )
        )
      ),
      
      tabItem(
        tabName = "Constraints",
        fluidRow(
          box(
            width = 12,
            title = "Feasible skill mix Bounds",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("bounds_table")
          )
        )
      ),
      
      tabItem(
        tabName = "efficiency",
        fluidRow(
          box(
            width = 12,
            title = "Cadre-Level Expected DALYs per USD",
            status = "info",
            solidHeader = TRUE,
            plotOutput("efficiency_plot", height = 420)
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Efficiency Ranking",
            status = "info",
            solidHeader = TRUE,
            DTOutput("efficiency_table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Editable stock data
  stock_data <- reactiveValues(
    df = data.frame(
      Cadre = c("Nurses",
                "Midwives",
                "Dentists",
                "Pharmacists",
                "Doctors"),
      Stock = c(5190,
                744,
                374,
                415,
                7167)
    )
  )
  results <- eventReactive(input$run, {
    
    if (input$fixed_seed) {
      set.seed(123)
    }
    
    cadres <- c("Nurses", "Midwives", "Dentists", "Pharmacists", "Doctors")
    

    stock <- stock_data$df$Stock
    names(stock) <- stock_data$df$Cadre
    stock <- stock[cadres]
    
    mix_all <- stock / sum(stock)
    
    cost_low_m <- c(
      Nurses = 300,
      Midwives = 350,
      Dentists = 800,
      Pharmacists = 900,
      Doctors = 1000
    )[cadres]
    
    cost_high_m <- c(
      Nurses = 800,
      Midwives = 900,
      Dentists = 2200,
      Pharmacists = 2500,
      Doctors = 3500
    )[cadres]
    
    cost_low <- 12 * cost_low_m
    cost_high <- 12 * cost_high_m
    
    make_scenarios <- function(
    n_scen,
    cadres_in = cadres,
    work_days = 252,
    hours_day = 8,
    util = 0.75,
    t_service = c(Nurses = 15, Midwives = 30, Dentists = 4, Pharmacists = 5, Doctors = 22),
    cv = c(Nurses = 0.20, Midwives = 0.25, Dentists = 0.30, Pharmacists = 0.20, Doctors = 0.25),
    daly_per_service_mean = c(Nurses = 0.006, Midwives = 0.012, Dentists = 0.002, Pharmacists = 0.004, Doctors = 0.010),
    daly_per_service_sd = c(Nurses = 0.002, Midwives = 0.004, Dentists = 0.001, Pharmacists = 0.0015, Doctors = 0.003),
    shock_prob = 0.12,
    shock_prod_multiplier_mean = 0.85,
    shock_prod_multiplier_sd = 0.06,
    avail_mean = c(Nurses = 0.90, Midwives = 0.89, Dentists = 0.92, Pharmacists = 0.90, Doctors = 0.88),
    avail_sd = c(Nurses = 0.05, Midwives = 0.05, Dentists = 0.04, Pharmacists = 0.05, Doctors = 0.06),
    avail_floor = 0.50
    ) {
      k <- length(cadres_in)
      out <- matrix(NA_real_, n_scen, k)
      colnames(out) <- cadres_in
      
      t_service <- t_service[cadres_in]
      cv <- cv[cadres_in]
      daly_per_service_mean <- daly_per_service_mean[cadres_in]
      daly_per_service_sd <- daly_per_service_sd[cadres_in]
      avail_mean <- avail_mean[cadres_in]
      avail_sd <- avail_sd[cadres_in]
      
      minutes_year <- work_days * hours_day * 60 * util
      prod_mean <- minutes_year / t_service
      prod_sd <- prod_mean * cv
      
      for (s in seq_len(n_scen)) {
        shock <- runif(1) < shock_prob
        
        shock_mult <- if (shock) {
          rnorm(1, shock_prod_multiplier_mean, shock_prod_multiplier_sd)
        } else {
          1
        }
        
        shock_mult <- max(0.4, min(1.1, shock_mult))
        
        prod <- pmax(0, rnorm(k, prod_mean, prod_sd))
        eff <- pmax(0, rnorm(k, daly_per_service_mean, daly_per_service_sd))
        avail <- pmin(1, pmax(avail_floor, rnorm(k, avail_mean, avail_sd)))
        
        out[s, ] <- prod * eff * avail * shock_mult
      }
      
      out
    }
    
    make_cost_scenarios <- function(n_scen, cost_low, cost_high) {
      k <- length(cost_low)
      out <- matrix(NA_real_, n_scen, k)
      colnames(out) <- names(cost_low)
      
      for (s in seq_len(n_scen)) {
        out[s, ] <- runif(k, min = cost_low, max = cost_high)
      }
      
      out
    }
    
    daly_per_worker_scen <- make_scenarios(input$n_scen)
    cost_scen <- make_cost_scenarios(input$n_scen, cost_low, cost_high)
    daly_per_usd_scen <- daly_per_worker_scen / cost_scen
    
    mu <- colMeans(daly_per_usd_scen)
    Sigma <- cov(daly_per_usd_scen)
    Sigma <- Sigma + diag(1e-12, length(cadres))
    min_floor <- 0.001
    
    lower <- pmax(min_floor, 0.60 * mix_all)
    upper <- pmin(0.80, 1.60 * mix_all)
    
    # Ensure lower does not exceed upper
    lower <- pmin(lower, upper)
    
    # Make bounds feasible
    if (sum(lower) >= 1) {
      lower <- lower / sum(lower) * 0.95
    }
    
    if (sum(upper) <= 1) {
      upper <- upper / sum(upper) * 1.05
      upper <- pmin(upper, 1)
    }
    
    names(lower) <- cadres
    names(upper) <- cadres
    
    solve_hrh_mpt <- function(mu, Sigma, lower, upper, target_return) {
      
      k <- length(mu)
      
      Dmat <- (Sigma + t(Sigma)) / 2
      dvec <- rep(0, k)
      
      # Constraints:
      # sum(w) = 1
      # mu'w >= target_return
      # w >= lower
      # w <= upper
      
      Amat <- cbind(
        rep(1, k),
        mu,
        diag(k),
        -diag(k)
      )
      
      bvec <- c(
        1,
        target_return,
        lower,
        -upper
      )
      
      sol <- quadprog::solve.QP(
        Dmat = Dmat,
        dvec = dvec,
        Amat = Amat,
        bvec = bvec,
        meq = 1
      )
      
      w <- sol$solution
      names(w) <- names(mu)
      
      list(
        w = w,
        exp_return = sum(mu * w),
        variance = as.numeric(t(w) %*% Sigma %*% w),
        sd = sqrt(as.numeric(t(w) %*% Sigma %*% w))
      )
    }
    max_return_portfolio <- function(mu, lower, upper) {
      
      w <- lower
      
      remaining <- 1 - sum(lower)
      
      ord <- order(mu, decreasing = TRUE)
      
      for (i in ord) {
        
        add <- min(upper[i] - w[i], remaining)
        
        if (add > 0) {
          w[i] <- w[i] + add
          remaining <- remaining - add
        }
        
        if (remaining <= 1e-10)
          break
      }
      
      list(
        w = w,
        ret = sum(mu * w)
      )
    }
    min_return_portfolio <- function(mu, lower, upper) {
      
      w <- lower
      
      remaining <- 1 - sum(lower)
      
      ord <- order(mu, decreasing = FALSE)
      
      for (i in ord) {
        
        add <- min(upper[i] - w[i], remaining)
        
        if (add > 0) {
          w[i] <- w[i] + add
          remaining <- remaining - add
        }
        
        if (remaining <= 1e-10)
          break
      }
      
      list(
        w = w,
        ret = sum(mu * w)
      )
    }
    min_port <- min_return_portfolio(mu, lower, upper)
    max_port <- max_return_portfolio(mu, lower, upper)
    
    targets <- seq(
      min_port$ret,
      max_port$ret,
      length.out = 30
    )
   
    current_return <- sum(mu * mix_all)
    current_variance <- as.numeric(t(mix_all) %*% Sigma %*% mix_all)
    current_sd <- sqrt(current_variance)
    
    
    
    frontier <- data.frame(
      sd = numeric(),
      exp_return = numeric()
    )
    
    best <- NULL
    
    for (tr in targets) {
      
      fit <- tryCatch(
        
        solve_hrh_mpt(
          mu,
          Sigma,
          lower,
          upper,
          tr
        ),
        
        error = function(e) NULL
        
      )
      
      if (!is.null(fit)) {
        
        frontier <- rbind(
          frontier,
          data.frame(
            sd = fit$sd,
            exp_return = fit$exp_return
          )
        )
        
        best <- fit
        
      }
      
    }
    
    frontier <- unique(frontier)
    
    # Highest-return portfolio
    frontier <- unique(frontier)
    
    # best already contains the last successful optimization
    if (is.null(best)) {
      stop("No feasible workforce mix found.")
    }
    
    optimal_total_dalys <- input$budget * best$exp_return
    current_total_dalys <- input$budget * current_return
    gain_pct <- 100 * ((best$exp_return / current_return) - 1)
    
    list(
      cadres = cadres,
      stock = stock,
      mix_all = mix_all,
      mu = mu,
      Sigma = Sigma,
      frontier = frontier,
      best = best,
      lower = lower,
      upper = upper,
      n_scen = input$n_scen,
      budget = input$budget,
      current_return = current_return,
      current_sd = current_sd,
      optimal_total_dalys = optimal_total_dalys,
      current_total_dalys = current_total_dalys,
      gain_pct = gain_pct
    )
    
  }, ignoreNULL = FALSE)
  
  output$exp_return_box <- renderValueBox({
    r <- results()
    valueBox(
      signif(r$best$exp_return, 5),
      "Optimal expected DALYs per USD",
      icon = icon("heartbeat"),
      color = "green"
    )
  })
  
  output$risk_box <- renderValueBox({
    r <- results()
    valueBox(
      signif(r$best$sd, 5),
      "Uncertainty: SD of DALYs per USD",
      icon = icon("exclamation-triangle"),
      color = "yellow"
    )
  })
  
  output$gain_box <- renderValueBox({
    r <- results()
    valueBox(
      paste0(round(r$gain_pct, 1), "%"),
      "Efficiency gain vs current mix",
      icon = icon("arrow-up"),
      color = "aqua"
    )
  })
  
  output$total_daly_box <- renderValueBox({
    r <- results()
    valueBox(
      comma(round(r$optimal_total_dalys, 0)),
      "Optimal expected DALYs from budget",
      icon = icon("plus-circle"),
      color = "green"
    )
  })
  
  output$current_daly_box <- renderValueBox({
    r <- results()
    valueBox(
      comma(round(r$current_total_dalys, 0)),
      "Current expected DALYs from budget",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$n_scen_box <- renderValueBox({
    r <- results()
    valueBox(
      comma(r$n_scen),
      "Simulation scenarios",
      icon = icon("random"),
      color = "purple"
    )
  })
 
  output$stock_table <- renderDT({
    
    r <- results()
    
    df <- data.frame(
      Cadre = stock_data$df$Cadre,
      Stock = stock_data$df$Stock,
      Current_Mix = percent(r$mix_all, accuracy = 0.1)
    )
    
    datatable(
      df,
      rownames = FALSE,
      editable = list(
        target = "cell",
        disable = list(columns = c(0, 2))
      ),
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        dom = "t"
      )
    )
    
  })
  observeEvent(input$stock_table_cell_edit, {
    
    info <- input$stock_table_cell_edit
    
    # Only Stock column is editable
    if (info$col == 1) {
      
      value <- suppressWarnings(as.numeric(info$value))
      
      if (!is.na(value) && value >= 0) {
        
        stock_data$df$Stock[info$row] <- value
        
      }
      
    }
    
  })
  
  output$frontier_plot <- renderPlot({
    
    r <- results()
    
    ggplot(r$frontier,
           aes(x = sd, y = exp_return)) +
      
      geom_line(
        linewidth = 1.2,
        colour = "steelblue"
      ) +
      
      geom_point(
        colour = "steelblue",
        size = 2
      ) +
      
      geom_point(
        aes(
          x = r$current_sd,
          y = r$current_return
        ),
        colour = "red",
        size = 4
      ) +
      
      geom_point(
        aes(
          x = r$best$sd,
          y = r$best$exp_return
        ),
        colour = "darkgreen",
        size = 4
      ) +
      
      annotate(
        "point",
        x = r$current_sd,
        y = r$current_return,
        label = "Current",
        vjust = -1
      ) +
      
      annotate(
        "point",
        x = r$best$sd,
        y = r$best$exp_return,
        label = "Optimal",
        vjust = -1
      ) +
      
      labs(
        title = "Efficient Frontier",
        x = "Risk (Uncertainty)",
        y = "Expected DALYs per USD"
      ) +
      
      theme_minimal()
    
  })
  output$weights_plot <- renderPlot({
    r <- results()
    
    df <- data.frame(
      Cadre = rep(r$cadres, 2),
      Share = c(as.numeric(r$mix_all), as.numeric(r$best$w)),
      Portfolio = rep(c("Current", "Optimal"), each = length(r$cadres))
    )
    
    ggplot(df, aes(x = reorder(Cadre, Share), y = Share, fill = Portfolio)) +
      geom_col(position = "dodge") +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = "Current vs Optimal Budget Shares",
        x = "Health worker Cadre",
        y = "Budget Share"
      ) +
      theme_minimal()
  })
  
  output$weights_table <- renderDT({
    r <- results()
    
    df <- data.frame(
      Cadre = names(r$best$w),
      Current_Share = percent(r$mix_all, accuracy = 0.01),
      Optimal_Share = percent(as.numeric(r$best$w), accuracy = 0.01),
      Change_Percentage_Points = round(100 * (as.numeric(r$best$w) - r$mix_all), 2),
      Expected_DALYs_per_USD = signif(r$mu, 5)
    )
    
    datatable(df, rownames = FALSE, options = list(pageLength = 10))
  })
  
  output$bounds_table <- renderDT({
    r <- results()
    
    df <- data.frame(
      Cadre = r$cadres,
      Current_Mix = percent(r$mix_all, accuracy = 0.01),
      Lower_Bound = percent(r$lower, accuracy = 0.01),
      Upper_Bound = percent(r$upper, accuracy = 0.01)
    )
    
    datatable(df, rownames = FALSE, options = list(pageLength = 10))
  })
  
  output$efficiency_plot <- renderPlot({
    r <- results()
    
    df <- data.frame(
      Cadre = names(r$mu),
      Expected_DALYs_per_USD = as.numeric(r$mu)
    )
    
    ggplot(df, aes(x = reorder(Cadre, Expected_DALYs_per_USD), y = Expected_DALYs_per_USD)) +
      geom_col() +
      coord_flip() +
      labs(
        title = "Cadre-Level Expected DALYs per USD ",
        x = "Health Worker Cadre",
        y = "Expected health impact(DALYs per USD)"
      ) +
      theme_minimal()
  })
  
  output$efficiency_table <- renderDT({
    r <- results()
    
    df <- data.frame(
      Cadre = names(r$mu),
      Expected_DALYs_per_USD = signif(as.numeric(r$mu), 5)
    )
    
    df <- df[order(-df$Expected_DALYs_per_USD), ]
    
    datatable(df, rownames = FALSE, options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
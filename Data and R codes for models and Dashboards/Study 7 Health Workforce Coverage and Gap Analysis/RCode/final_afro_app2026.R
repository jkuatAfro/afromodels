# =========================================================
# HRH GAP TOOL (2026)
# =========================================================
# Purpose:
#   Interactive R Shiny application for projecting health workforce supply,
#   estimating benchmark-based and health-need-linked requirements, and
#   quantifying workforce gaps, coverage, and additional staff needed.
#
# =========================================================
# Core HRH Projection Model
# =========================================================
# Purpose:
# Performs the complete HRH projection workflow for a single country,
# from data validation through workforce projections and gap analysis.
#
# Key modelling assumptions:
#   - Reported cadre headcounts are used in preference to density-derived
#     headcounts whenever available.
#   - Missing historical workforce stocks are completed using interpolation,
#     followed by forward and backward filling where necessary.
#   - Business-as-Usual (BAU) growth is estimated from the most recent valid
#     annual workforce growth rates.
#   - ScaleUp and Shock scenarios modify the BAU growth rate using
#     country-specific scenario adjustments.
#   - Health-need indicators are standardized within each country before
#     constructing the composite need index.
#   - Workforce gap = Projected supply − Projected requirement.
#   - Additional staff needed = max(0, Projected requirement − Projected supply).
#   - Workforce coverage = Projected supply / Projected requirement.
# =========================================================


library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(zoo)
library(DT)
library(scales)
library(plotly)
library(data.table)
library(rhandsontable)
library(purrr)

options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize = 1024 * 1024^2)

# =========================================================
# Plot styling
# Common plot theme and colour palettes used throughout the dashboard.
# =========================================================
# Return a consistent ggplot theme for all charts.
theme_hrh <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 13, colour = "grey30"),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "grey20"),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      plot.margin = margin(10, 20, 10, 10)
    )
}

scenario_cols <- c(
  "BAU" = "#1b9e77",
  "ScaleUp" = "#d95f02",
  "Shock" = "#7570b3"
)

status_cols <- c(
  "Shortage" = "#d73027",
  "Surplus" = "#1a9850"
)

series_cols <- c(
  "Supply" = "#1b9e77",
  "Requirement" = "#d95f02"
)

# Convert a ggplot object to Plotly and standardize hover and legend settings.
apply_plotly_style <- function(g) {
  ggplotly(g, tooltip = "text") %>%
    layout(
      hoverlabel = list(bgcolor = "white"),
      legend = list(orientation = "h", x = 0.2, y = -0.2)
    )
}

# =========================================================
# Helper functions
# Reusable utilities for growth, standardization, formatting, validation, and axes.
# =========================================================
# Estimate CAGR from the first and last valid positive observations.
# Returns NA when there are too few observations or an invalid time interval.
cagr <- function(x, years) {
  ok <- !is.na(x) & !is.na(years)
  x <- x[ok]
  years <- years[ok]
  if (length(x) < 2) return(NA_real_)
  denom <- tail(years, 1) - head(years, 1)
  if (is.na(denom) || denom <= 0) return(NA_real_)
  if (head(x, 1) <= 0 || tail(x, 1) <= 0) return(NA_real_)
  (tail(x, 1) / head(x, 1))^(1 / denom) - 1
}

# Standardize a series within country. Constant series are assigned zeros.
zscore <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}

# Format ranking values according to whether they are percentages or counts.
format_metric_value <- function(x, metric) {
  if (metric == "coverage_pct") return(paste0(round(x, 1), "%"))
  if (metric %in% c("additional_needed", "gap")) return(scales::comma(round(x, 1)))
  as.character(round(x, 2))
}

# Return the first observed year at or above a threshold within each group.
# No interpolation between annual observations is performed.
first_threshold_point <- function(data, value_col, threshold, group_cols) {
  value_col <- rlang::ensym(value_col)
  data %>%
    arrange(across(all_of(group_cols)), Year) %>%
    group_by(across(all_of(group_cols))) %>%
    filter(!is.na(!!value_col)) %>%
    filter((!!value_col) >= threshold) %>%
    slice(1) %>%
    ungroup()
}

# Construct readable five-year axis breaks while retaining key years.
get_year_axis_breaks <- function(years, target_year = NULL, horizon_year = NULL) {
  years <- sort(unique(stats::na.omit(as.numeric(years))))
  if (length(years) == 0) return(numeric(0))
  
  year_min <- min(years)
  year_max <- max(years)
  anchor_years <- stats::na.omit(as.numeric(c(target_year, horizon_year)))
  
  rounded_start <- floor(year_min / 5) * 5
  rounded_end <- ceiling(max(c(year_max, anchor_years), na.rm = TRUE) / 5) * 5
  
  five_year_breaks <- seq(rounded_start, rounded_end, by = 5)
  breaks <- sort(unique(c(five_year_breaks, year_min, year_max, target_year)))
  breaks[breaks >= year_min & breaks <= year_max]
}

# Normalize text to UTF-8 and correct known country-name encoding problems.
clean_text_encoding <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- trimws(x)
  x[x %in% c("C�te d'Ivoire", "Cte d'Ivoire")] <- "Côte d'Ivoire"
  x
}

required_cols <- c(
  "ISO", "Country", "Year", "Population", "GDP",
  "Doctors", "Nursing_Midwifery", "Dentists", "Pharmacists",
  "Doctors_Number", "Nursing_Midwifery_Number", "Dentists_Number", "Pharmacists_Number",
  "DALYS", "MMR", "NCDMR", "UHC_SCI"
)

# Create one specification row per selected country from current defaults.
make_default_specs <- function(countries, input) {
  tibble(
    Country = countries,
    lambda = input$lambda,
    b_doc = input$b_doc,
    b_nmw = input$b_nmw,
    b_pharm = input$b_pharm,
    b_dent = input$b_dent,
    s_doc = input$s_doc,
    s_nmw = input$s_nmw,
    s_pharm = input$s_pharm,
    s_dent = input$s_dent,
    delta_scaleup = input$delta_scaleup,
    delta_shock = input$delta_shock
  )
}

# Validate country specifications before model execution.
# Checks required fields, duplicates, missing countries, non-negative values,
# cadre shares summing to one, and valid scenario adjustments.
validate_specs_table <- function(specs, selected_countries) {
  req_cols <- c(
    "Country", "lambda", "b_doc", "b_nmw", "b_pharm", "b_dent",
    "s_doc", "s_nmw", "s_pharm", "s_dent", "delta_scaleup", "delta_shock"
  )
  
  miss <- setdiff(req_cols, names(specs))
  if (length(miss) > 0) {
    stop(paste("Specification table is missing:", paste(miss, collapse = ", ")))
  }
  
  specs <- specs %>%
    mutate(
      Country = clean_text_encoding(Country),
      across(-Country, as.numeric)
    )
  
  if (anyDuplicated(specs$Country) > 0) {
    dupes <- unique(specs$Country[duplicated(specs$Country)])
    stop(paste("Duplicate countries in specification table:", paste(dupes, collapse = ", ")))
  }
  
  missing_cty <- setdiff(selected_countries, specs$Country)
  if (length(missing_cty) > 0) {
    stop(paste("These selected countries are missing from the specification table:", paste(missing_cty, collapse = ", ")))
  }
  
  specs <- specs %>% filter(Country %in% selected_countries)
  
  bad_lambda <- specs %>% filter(is.na(lambda) | lambda < 0)
  if (nrow(bad_lambda) > 0) stop(paste("Invalid lambda for:", paste(bad_lambda$Country, collapse = ", ")))
  
  bench_cols <- c("b_doc", "b_nmw", "b_pharm", "b_dent")
  bad_bench <- specs %>% filter(if_any(all_of(bench_cols), ~ is.na(.) | . < 0))
  if (nrow(bad_bench) > 0) stop(paste("Invalid benchmark values for:", paste(bad_bench$Country, collapse = ", ")))
  
  specs <- specs %>% mutate(share_sum = s_doc + s_nmw + s_pharm + s_dent)
  bad_share <- specs %>% filter(is.na(share_sum) | abs(share_sum - 1) > 1e-8)
  if (nrow(bad_share) > 0) stop(paste("Cadre shares must sum to 1.00 for:", paste(bad_share$Country, collapse = ", ")))
  
  bad_delta <- specs %>% filter(is.na(delta_scaleup) | is.na(delta_shock))
  if (nrow(bad_delta) > 0) stop(paste("Invalid scenario deltas for:", paste(bad_delta$Country, collapse = ", ")))

  # Deltas themselves may be negative, but scenario-specific total growth
  # is checked after BAU growth has been estimated.
  specs %>% select(-share_sum)
}

# =========================================================
# Core model
# =========================================================
# Run the complete HRH model for one country.
# Returns processed source data, annual gap results, target-year results,
# latest observed year, and country-specific base-year information.
run_hrh_model <- function(
    data,
    k_growth = 5,
    horizon_end = 2035,
    target_year = 2030,
    bench,
    shares,
    lambda = 2.5,
    need_w_signed,
    scenario_delta = tibble(
      scenario = c("BAU", "ScaleUp", "Shock"),
      delta = c(0.00, 0.01, -0.01)
    )
) {
  # Confirm that all variables required by the model are present.
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste0("The loaded dataset is missing these required columns: ", paste(missing_cols, collapse = ", ")))
  }
  
  data <- data %>%
    mutate(
      ISO = clean_text_encoding(ISO),
      Country = clean_text_encoding(Country),
      Year = as.integer(Year),
      Population = as.numeric(Population),
      GDP = as.numeric(GDP),
      Doctors = as.numeric(Doctors),
      Nursing_Midwifery = as.numeric(Nursing_Midwifery),
      Dentists = as.numeric(Dentists),
      Pharmacists = as.numeric(Pharmacists),
      Doctors_Number = as.numeric(Doctors_Number),
      Nursing_Midwifery_Number = as.numeric(Nursing_Midwifery_Number),
      Dentists_Number = as.numeric(Dentists_Number),
      Pharmacists_Number = as.numeric(Pharmacists_Number),
      DALYS = as.numeric(DALYS),
      MMR = as.numeric(MMR),
      NCDMR = as.numeric(NCDMR),
      UHC_SCI = as.numeric(UHC_SCI)
    )
  
  # Construct cadre headcounts. Reported counts take precedence;
  # density multiplied by population is used only when counts are missing.
  data2 <- data %>%
    mutate(
      stock_doctors = ifelse(!is.na(Doctors_Number), Doctors_Number, Doctors * Population / 10000),
      stock_nmw = ifelse(!is.na(Nursing_Midwifery_Number), Nursing_Midwifery_Number, Nursing_Midwifery * Population / 10000),
      stock_dentists = ifelse(!is.na(Dentists_Number), Dentists_Number, Dentists * Population / 10000),
      stock_pharm = ifelse(!is.na(Pharmacists_Number), Pharmacists_Number, Pharmacists * Population / 10000)
    )
  
  # Reshape workforce stocks to long format and complete missing values.
  # Internal gaps are interpolated, then remaining values are forward/back filled.
  stocks_long <- data2 %>%
    select(
      ISO, Country, Year, Population, GDP,
      DALYS, MMR, NCDMR, UHC_SCI,
      stock_doctors, stock_nmw, stock_dentists, stock_pharm
    ) %>%
    pivot_longer(
      cols = starts_with("stock_"),
      names_to = "cadre_raw",
      values_to = "stock"
    ) %>%
    mutate(
      cadre = recode(
        cadre_raw,
        stock_doctors = "Doctors",
        stock_nmw = "NursesMidwives",
        stock_dentists = "Dentists",
        stock_pharm = "Pharmacists"
      )
    ) %>%
    select(-cadre_raw) %>%
    arrange(ISO, Country, cadre, Year) %>%
    group_by(ISO, Country, cadre) %>%
    mutate(stock = zoo::na.approx(stock, x = Year, na.rm = FALSE)) %>%
    mutate(stock = zoo::na.locf(stock, na.rm = FALSE)) %>%
    mutate(stock = zoo::na.locf(stock, fromLast = TRUE, na.rm = FALSE)) %>%
    ungroup()
  
  # Estimate recent BAU growth by country and cadre.
  # Invalid, non-finite, and <= -100% annual growth values are excluded.
  g_bau <- stocks_long %>%
    group_by(ISO, Country, cadre) %>%
    arrange(Year) %>%
    mutate(
      g = ifelse(
        !is.na(stock) & stock > 0 & !is.na(lead(stock)),
        (lead(stock) - stock) / stock,
        NA_real_
      )
    ) %>%
    filter(!is.na(g), is.finite(g), g > -1) %>%
    slice_tail(n = k_growth) %>%
    summarise(g_bau = mean(g, na.rm = TRUE), .groups = "drop")
  
  drivers_hist <- data2 %>%
    group_by(ISO, Country, Year) %>%
    summarise(
      Population = mean(Population, na.rm = TRUE),
      GDP = mean(GDP, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ISO, Country, Year)
  
  # Project population and GDP using recent CAGR; use zero growth if unavailable.
  drivers_future <- drivers_hist %>%
    group_by(ISO, Country) %>%
    group_modify(~{
      x <- .x %>% arrange(Year)
      last_year_cty <- max(x$Year, na.rm = TRUE)
      tail_x <- x %>% filter(Year >= last_year_cty - k_growth)
      pop_g <- cagr(tail_x$Population, tail_x$Year)
      gdp_g <- cagr(tail_x$GDP, tail_x$Year)
      if (is.na(pop_g)) pop_g <- 0
      if (is.na(gdp_g)) gdp_g <- 0
      future_years <- if (horizon_end > last_year_cty) (last_year_cty + 1):horizon_end else integer(0)
      if (length(future_years) == 0) return(tibble())
      
      tibble(
        Year = future_years,
        Population = tail(x$Population, 1) * (1 + pop_g)^(future_years - last_year_cty),
        GDP = tail(x$GDP, 1) * (1 + gdp_g)^(future_years - last_year_cty)
      )
    }) %>%
    ungroup()
  
  drivers_all <- bind_rows(drivers_hist, drivers_future) %>%
    arrange(ISO, Country, Year)
  
  # Project workforce supply under BAU, ScaleUp, and Shock scenarios.
  # Scenario growth <= -100% is rejected and projected stock cannot be negative.
  proj_supply <- function(stocks_long, g_bau, drivers_all, scenario_delta) {
    base <- stocks_long %>%
      group_by(ISO, Country, cadre) %>%
      filter(Year == max(Year, na.rm = TRUE)) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      select(ISO, Country, Year, cadre, stock)
    
    base_years <- base %>% rename(base_year = Year)
    out_list <- vector("list", length = nrow(scenario_delta))
    
    for (i in seq_len(nrow(scenario_delta))) {
      scen <- scenario_delta$scenario[i]
      delta <- scenario_delta$delta[i]
      
      g_use <- g_bau %>%
        mutate(g_use = g_bau + delta) %>%
        select(ISO, Country, cadre, g_use)

      invalid_growth <- g_use %>%
        filter(!is.na(g_use), g_use <= -1)

      if (nrow(invalid_growth) > 0) {
        stop(
          paste0(
            "Scenario '", scen, "' produces annual growth of -100% or lower for: ",
            paste(
              paste0(invalid_growth$Country, " / ", invalid_growth$cadre),
              collapse = ", "
            ),
            ". Adjust the scenario delta or review the historical workforce data."
          )
        )
      }
      
      tmp <- base %>% left_join(g_use, by = c("ISO", "Country", "cadre"))
      all_rows <- list(tmp)
      
      future_rows <- drivers_all %>%
        distinct(ISO, Country, Year) %>%
        tidyr::crossing(cadre = unique(base$cadre)) %>%
        left_join(base_years %>% select(ISO, Country, cadre, base_year, stock),
                  by = c("ISO", "Country", "cadre")) %>%
        left_join(g_use, by = c("ISO", "Country", "cadre")) %>%
        filter(Year > base_year) %>%
        arrange(ISO, Country, cadre, Year)
      
      if (nrow(future_rows) > 0) {
        future_rows <- future_rows %>%
          group_by(ISO, Country, cadre) %>%
          mutate(
            g_use = ifelse(is.na(g_use), 0, g_use),
            step = row_number(),
            stock = pmax(0, first(stock) * (1 + first(g_use))^step)
          ) %>%
          ungroup() %>%
          select(ISO, Country, Year, cadre, stock)
        
        all_rows[[length(all_rows) + 1]] <- future_rows
      }
      
      tmp_all <- bind_rows(all_rows) %>%
        left_join(drivers_all %>% select(ISO, Country, Year, Population),
                  by = c("ISO", "Country", "Year")) %>%
        mutate(
          density_per10k = stock / Population * 10000,
          scenario = scen
        )
      
      out_list[[i]] <- tmp_all %>%
        select(ISO, Country, Year, cadre, stock, density_per10k, scenario)
    }
    
    bind_rows(out_list)
  }
  
  supply_all <- proj_supply(stocks_long, g_bau, drivers_all, scenario_delta)
  
  # Benchmark requirement = benchmark density x projected population / 10,000.
  req_bench <- drivers_all %>%
    tidyr::crossing(bench) %>%
    mutate(
      req_stock = B * Population / 10000,
      req_type = "Benchmark"
    ) %>%
    select(ISO, Country, Year, Population, cadre, req_stock, req_type)
  
  # Build the within-country composite health-need index.
  # Missing indicators are carried forward/backward before standardization.
  need_vars <- data2 %>%
    group_by(ISO, Country, Year) %>%
    summarise(
      DALYS = mean(DALYS, na.rm = TRUE),
      MMR = mean(MMR, na.rm = TRUE),
      NCDMR = mean(NCDMR, na.rm = TRUE),
      UHC_SCI = mean(UHC_SCI, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(drivers_all %>% select(ISO, Country, Year),
               by = c("ISO", "Country", "Year")) %>%
    arrange(ISO, Country, Year) %>%
    group_by(ISO, Country) %>%
    mutate(across(c(DALYS, MMR, NCDMR, UHC_SCI), ~ zoo::na.locf(.x, na.rm = FALSE))) %>%
    mutate(across(c(DALYS, MMR, NCDMR, UHC_SCI), ~ zoo::na.locf(.x, fromLast = TRUE, na.rm = FALSE))) %>%
    mutate(
      N = need_w_signed["DALYS"] * zscore(DALYS) +
        need_w_signed["MMR"] * zscore(MMR) +
        need_w_signed["NCDMR"] * zscore(NCDMR) +
        need_w_signed["UHC_SCI"] * zscore(UHC_SCI)
    ) %>%
    ungroup() %>%
    select(ISO, Country, Year, N)
  
  B_base <- sum(bench$B)
  
  # Need-linked total requirement is adjusted by lambda and then allocated
  # across cadres using country-specific shares.
  req_need <- drivers_all %>%
    select(ISO, Country, Year, Population) %>%
    left_join(need_vars, by = c("ISO", "Country", "Year")) %>%
    mutate(
      req_total_density = pmax(0, B_base + lambda * N),
      req_total_stock = req_total_density * Population / 10000
    ) %>%
    tidyr::crossing(shares) %>%
    mutate(
      req_stock = share * req_total_stock,
      req_type = "NeedLinked"
    ) %>%
    select(ISO, Country, Year, Population, cadre, req_stock, req_type)
  
  req_all <- bind_rows(req_bench, req_need)
  
  # Combine supply and requirements, then calculate gap and coverage.
  gaps <- supply_all %>%
    select(ISO, Country, Year, cadre, scenario, supply_stock = stock) %>%
    tidyr::crossing(req_type = unique(req_all$req_type)) %>%
    left_join(req_all %>% select(ISO, Country, Year, cadre, req_type, req_stock),
              by = c("ISO", "Country", "Year", "cadre", "req_type")) %>%
    mutate(
      gap = supply_stock - req_stock,
      coverage = ifelse(!is.na(req_stock) & req_stock > 0, supply_stock / req_stock, NA_real_)
    )
  
  gaps_smooth <- gaps %>%
    group_by(ISO, Country, cadre, req_type, scenario, Year) %>%
    summarise(
      supply_stock = mean(supply_stock, na.rm = TRUE),
      req_stock = mean(req_stock, na.rm = TRUE),
      gap = mean(gap, na.rm = TRUE),
      coverage = mean(coverage, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ISO, Country, cadre, req_type, scenario, Year)
  
  # Extract the selected target year and compute additional staff needed.
  target_table <- gaps_smooth %>%
    filter(Year == target_year) %>%
    mutate(
      additional_needed = pmax(0, req_stock - supply_stock),
      coverage_pct = 100 * coverage
    ) %>%
    arrange(Country, req_type, cadre, scenario)
  
  list(
    source = data2,
    target_table = target_table,
    gaps_smooth = gaps_smooth,
    last_year = max(data2$Year, na.rm = TRUE),
    base_years = data2 %>%
      group_by(Country) %>%
      summarise(base_year = max(Year, na.rm = TRUE), .groups = "drop")
  )
}

# =========================================================
# User interface
# Dashboard layout, inputs, tabs, editable specifications, outputs, and downloads.
# =========================================================
ui <- dashboardPage(
  dashboardHeader(title = "HRH GAP Tool (2026)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inputs", tabName = "inputs", icon = icon("sliders-h")),
      menuItem("Specifications", tabName = "specs", icon = icon("table")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Target Year Table", tabName = "target", icon = icon("table")),
      menuItem("Coverage Plots", tabName = "coverage", icon = icon("chart-line")),
      menuItem("Gap Plots", tabName = "gap", icon = icon("chart-area")),
      menuItem("Supply vs Requirement", tabName = "supplyreq", icon = icon("balance-scale")),
      menuItem("Country Rankings", tabName = "rankings", icon = icon("chart-bar")),
      menuItem("Downloads", tabName = "downloads", icon = icon("download"))
    ),
    hr(),
    radioButtons(
      "data_source_mode",
      "Data source",
      choices = c("Use file path" = "path", "Upload file" = "upload"),
      selected = "path"
    ),
    conditionalPanel(
      condition = "input.data_source_mode == 'path'",
      textInput("file_path", "Enter full path to HRH file (.csv or .rds)", value = "")
    ),
    conditionalPanel(
      condition = "input.data_source_mode == 'upload'",
      fileInput("file_upload", "Upload HRH file (.csv or .rds)", accept = c(".csv", ".rds"))
    ),
    textInput("rds_name", "Save loaded data as .rds", value = "hrh_data"),
    actionButton("save_rds", "Save loaded file as RDS", icon = icon("save")),
    br(), br(),
    numericInput("k_growth", "Years for BAU growth estimation", value = 5, min = 2, max = 10),
    numericInput("horizon_end", "Projection horizon end year", value = 2035, min = 2025, max = 2100),
    numericInput("target_year", "Target year", value = 2030, min = 2025, max = 2100),
    numericInput("lambda", "Default lambda", value = 2.5, min = 0, step = 0.1),
    uiOutput("country_ui"),
    fluidRow(
      column(6, actionButton("select_all_countries", "Select all countries", icon = icon("check-square"))),
      column(6, actionButton("clear_countries", "Clear countries", icon = icon("times-circle")))
    ),
    checkboxInput("use_defaults_all", "Use default values for all selected countries", value = TRUE),
    hr(),
    h5("Common need-linked indicator weights"),
    numericInput("w_dalys", "DALYS weight", value = 0.25, step = 0.01, min = 0, max = 1),
    numericInput("w_mmr", "MMR weight", value = 0.25, step = 0.01, min = 0, max = 1),
    numericInput("w_ncdmr", "NCDMR weight", value = 0.25, step = 0.01, min = 0, max = 1),
    numericInput("w_uhc", "UHC_SCI weight", value = 0.25, step = 0.01, min = 0, max = 1),
    helpText("These weights remain common across countries. UHC_SCI is treated as negative in the model."),
    hr(),
    h5("Default values for country table"),
    numericInput("b_doc", "Doctors benchmark", value = 1.5, min = 0, step = 0.1),
    numericInput("b_nmw", "Nurses/Midwives benchmark", value = 20.0, min = 0, step = 0.1),
    numericInput("b_pharm", "Pharmacists benchmark", value = 2.0, min = 0, step = 0.1),
    numericInput("b_dent", "Dentists benchmark", value = 0.5, min = 0, step = 0.1),
    numericInput("s_doc", "Doctors share", value = 0.20, min = 0, max = 1, step = 0.01),
    numericInput("s_nmw", "Nurses/Midwives share", value = 0.70, min = 0, max = 1, step = 0.01),
    numericInput("s_pharm", "Pharmacists share", value = 0.07, min = 0, max = 1, step = 0.01),
    numericInput("s_dent", "Dentists share", value = 0.03, min = 0, max = 1, step = 0.01),
    numericInput("delta_scaleup", "ScaleUp growth increment", value = 0.01, step = 0.005),
    numericInput("delta_shock", "Shock growth decrement", value = -0.01, step = 0.005),
    actionButton("build_specs", "Build / Refresh country table", icon = icon("table")),
    br(), br(),
    actionButton("run_model", "Run model", icon = icon("play"), class = "btn-primary")
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "inputs",
        fluidRow(
          box(
            width = 12,
            title = "How to use",
            status = "primary",
            solidHeader = TRUE,
            tags$ol(
              tags$li("Load the data using either a file path or direct upload."),
              tags$li("Select countries or click Select all countries."),
              tags$li("Leave 'Use default values for all selected countries' checked to run all countries immediately."),
              tags$li("Uncheck it if you want to edit country-specific values in the Specifications tab."),
              tags$li("Click Run model.")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Required columns",
            status = "info",
            solidHeader = TRUE,
            verbatimTextOutput("required_cols")
          )
        )
      ),
      tabItem(
        tabName = "specs",
        fluidRow(
          box(
            width = 12,
            title = "Specification reference",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            tags$p(
              tags$b("Country"), " = country name; ",
              tags$b("lambda"), " = need adjustment strength; ",
              tags$b("b_doc"), " = doctors benchmark; ",
              tags$b("b_nmw"), " = nurses/midwives benchmark; ",
              tags$b("b_pharm"), " = pharmacists benchmark; ",
              tags$b("b_dent"), " = dentists benchmark; ",
              tags$b("s_doc"), " = doctors share; ",
              tags$b("s_nmw"), " = nurses/midwives share; ",
              tags$b("s_pharm"), " = pharmacists share; ",
              tags$b("s_dent"), " = dentists share; ",
              tags$b("delta_scaleup"), " = ScaleUp growth increment; ",
              tags$b("delta_shock"), " = Shock growth decrement."
            ),
            tags$p(
              tags$b("Note: "),
              "b_ = benchmark values, s_ = cadre shares, and the shares must sum to 1.00 for each country."
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Country-specific specifications",
            status = "warning",
            solidHeader = TRUE,
            div(
              style = "overflow-x:auto; overflow-y:auto; max-height:520px;",
              rHandsontableOutput("spec_table")
            ),
            br(),
            actionButton("reset_specs", "Reset from current defaults", icon = icon("undo"))
          )
        )
      ),
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("vb_country", width = 4),
          valueBoxOutput("vb_lastyear", width = 4),
          valueBoxOutput("vb_targetyear", width = 4)
        ),
        fluidRow(
          box(
            width = 12,
            title = "Additional staff needed by model and scenario",
            status = "info",
            solidHeader = TRUE,
            fluidRow(
              valueBoxOutput("vb_benchmark_bau", width = 4),
              valueBoxOutput("vb_benchmark_scaleup", width = 4),
              valueBoxOutput("vb_benchmark_shock", width = 4)
            ),
            fluidRow(
              valueBoxOutput("vb_needlinked_bau", width = 4),
              valueBoxOutput("vb_needlinked_scaleup", width = 4),
              valueBoxOutput("vb_needlinked_shock", width = 4)
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "Target year snapshot", status = "primary", solidHeader = TRUE, DTOutput("snapshot_table"))
        )
      ),
      tabItem(
        tabName = "target",
        fluidRow(
          box(
            width = 3,
            title = "Filters",
            status = "warning",
            solidHeader = TRUE,
            selectInput("filter_req", "Requirement type", choices = c("All", "Benchmark", "NeedLinked"), selected = "All"),
            selectInput("filter_cadre", "Cadre", choices = c("All", "Doctors", "NursesMidwives", "Pharmacists", "Dentists"), selected = "All"),
            selectInput("filter_scenario", "Scenario", choices = c("All", "BAU", "ScaleUp", "Shock"), selected = "All")
          ),
          box(width = 9, title = "Target year summary", status = "primary", solidHeader = TRUE, DTOutput("target_table"))
        )
      ),
      tabItem(
        tabName = "coverage",
        fluidRow(
          box(
            width = 3,
            title = "Controls",
            status = "warning",
            solidHeader = TRUE,
            selectInput("cov_req", "Requirement type", choices = c("Benchmark", "NeedLinked")),
            selectInput("cov_cadre", "Cadre", choices = c("Doctors", "NursesMidwives", "Pharmacists", "Dentists", "All cadres"), selected = "All cadres"),
            checkboxInput("cov_compare_countries", "Compare countries", value = FALSE),
            checkboxInput("cov_mark_threshold", "Mark first threshold reached", value = TRUE)
          ),
          box(width = 9, title = "Coverage plot", status = "primary", solidHeader = TRUE, plotlyOutput("coverage_plot", height = 500))
        )
      ),
      tabItem(
        tabName = "gap",
        fluidRow(
          box(
            width = 3,
            title = "Controls",
            status = "warning",
            solidHeader = TRUE,
            selectInput("gap_req", "Requirement type", choices = c("Benchmark", "NeedLinked")),
            selectInput("gap_cadre", "Cadre", choices = c("Doctors", "NursesMidwives", "Pharmacists", "Dentists", "All cadres"), selected = "All cadres"),
            checkboxInput("gap_compare_countries", "Compare countries", value = FALSE)
          ),
          box(width = 9, title = "Gap plot", status = "primary", solidHeader = TRUE, plotlyOutput("gap_plot", height = 500))
        )
      ),
      tabItem(
        tabName = "supplyreq",
        fluidRow(
          box(
            width = 3,
            title = "Controls",
            status = "warning",
            solidHeader = TRUE,
            selectInput("sr_req", "Requirement type", choices = c("Benchmark", "NeedLinked")),
            selectInput("sr_cadre", "Cadre", choices = c("Doctors", "NursesMidwives", "Pharmacists", "Dentists"), selected = "NursesMidwives"),
            selectInput("sr_scenario", "Scenario", choices = c("BAU", "ScaleUp", "Shock"), selected = "BAU"),
            checkboxInput("sr_mark_threshold", "Mark year supply meets requirement", value = TRUE),
            checkboxInput("sr_show_value", "Show year and value label", value = TRUE)
          ),
          box(width = 9, title = "Supply vs requirement", status = "primary", solidHeader = TRUE, plotlyOutput("supplyreq_plot", height = 500))
        )
      ),
      tabItem(
        tabName = "rankings",
        fluidRow(
          box(
            width = 3,
            title = "Ranking controls",
            status = "warning",
            solidHeader = TRUE,
            selectInput("rank_metric", "Rank countries by", choices = c("Additional needed" = "additional_needed", "Coverage %" = "coverage_pct", "Gap" = "gap"), selected = "additional_needed"),
            selectInput("rank_req", "Requirement type", choices = c("Benchmark", "NeedLinked"), selected = "Benchmark"),
            selectInput("rank_cadre", "Cadre", choices = c("Doctors", "NursesMidwives", "Pharmacists", "Dentists"), selected = "NursesMidwives"),
            selectInput("rank_scenario", "Scenario", choices = c("BAU", "ScaleUp", "Shock"), selected = "BAU"),
            selectInput("rank_order", "Sort order", choices = c("Highest first" = "desc", "Lowest first" = "asc"), selected = "desc"),
            checkboxInput("rank_show_values", "Show values on bars", value = TRUE)
          ),
          box(
            width = 9,
            title = "Country ranking",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("ranking_plot", height = 420),
            br(),
            DTOutput("ranking_table")
          )
        )
      ),
      tabItem(
        tabName = "downloads",
        fluidRow(
          box(
            width = 12,
            title = "Downloads",
            status = "success",
            solidHeader = TRUE,
            downloadButton("download_target", "Download target-year table"),
            br(), br(),
            downloadButton("download_gaps", "Download full gaps dataset"),
            br(), br(),
            downloadButton("download_specs", "Download country specification table"),
            br(), br(),
            downloadButton("download_loaded_rds", "Download loaded data as RDS")
          )
        )
      )
    )
  )
)

# =========================================================
# Server logic
# Data loading, reactive state, validation, model execution, plots, tables, and downloads.
# =========================================================
server <- function(input, output, session) {
  
  output$required_cols <- renderText({
    paste(required_cols, collapse = ", ")
  })
  
  # Load CSV or RDS data from a path or browser upload.
  raw_data <- reactive({
    mode <- input$data_source_mode
    
    out <- tryCatch(
      {
        if (mode == "path") {
          req(input$file_path)
          file_path <- trimws(input$file_path)
          validate(need(file.exists(file_path), "The file path does not exist."))
          ext <- tolower(tools::file_ext(file_path))
          
          if (ext == "csv") {
            data.table::fread(file_path, encoding = "UTF-8")
          } else if (ext == "rds") {
            readRDS(file_path)
          } else {
            stop("Please provide a .csv or .rds file.")
          }
          
        } else {
          req(input$file_upload)
          ext <- tolower(tools::file_ext(input$file_upload$name))
          
          if (ext == "csv") {
            data.table::fread(input$file_upload$datapath, encoding = "UTF-8")
          } else if (ext == "rds") {
            readRDS(input$file_upload$datapath)
          } else {
            stop("Please upload a .csv or .rds file.")
          }
        }
      },
      error = function(e) {
        showNotification(paste("File read error:", e$message), type = "error", duration = NULL)
        NULL
      }
    )
    
    validate(need(!is.null(out), "The file could not be read."))
    out <- as.data.frame(out)
    
    if ("Country" %in% names(out)) out$Country <- clean_text_encoding(out$Country)
    if ("ISO" %in% names(out)) out$ISO <- clean_text_encoding(out$ISO)
    
    out
  })
  
  observeEvent(input$save_rds, {
    req(raw_data())
    nm <- trimws(input$rds_name)
    if (nm == "") nm <- "hrh_data"
    if (!grepl("\\.rds$", nm, ignore.case = TRUE)) nm <- paste0(nm, ".rds")
    saveRDS(raw_data(), file.path(getwd(), nm))
    showNotification("RDS saved in current working directory.", type = "message")
  })
  
  output$download_loaded_rds <- downloadHandler(
    filename = function() {
      nm <- trimws(input$rds_name)
      if (nm == "") nm <- "hrh_data"
      if (!grepl("\\.rds$", nm, ignore.case = TRUE)) nm <- paste0(nm, ".rds")
      nm
    },
    content = function(file) {
      req(raw_data())
      saveRDS(raw_data(), file)
    }
  )
  
  output$country_ui <- renderUI({
    req(raw_data())
    validate(need("Country" %in% names(raw_data()), "The loaded file must contain a column named 'Country'."))
    
    countries <- raw_data() %>%
      filter(!is.na(Country), Country != "") %>%
      distinct(Country) %>%
      arrange(Country) %>%
      pull(Country)
    
    validate(need(length(countries) > 0, "No valid countries found."))
    
    selectizeInput(
      "country_select",
      "Select one or more countries",
      choices = countries,
      selected = head(countries, min(3, length(countries))),
      multiple = TRUE,
      options = list(placeholder = "Choose countries")
    )
  })
  
  observeEvent(input$select_all_countries, {
    req(raw_data())
    countries <- raw_data() %>%
      filter(!is.na(Country), Country != "") %>%
      distinct(Country) %>%
      arrange(Country) %>%
      pull(Country)
    updateSelectizeInput(session, "country_select", selected = countries, server = FALSE)
  })
  
  observeEvent(input$clear_countries, {
    updateSelectizeInput(session, "country_select", selected = character(0), server = FALSE)
  })
  
  selected_countries <- reactive({
    req(raw_data())
    clean_text_encoding(input$country_select)
  })
  
  # Store the editable country-specification table reactively.
  specs_rv <- reactiveVal(NULL)
  
  build_specs_now <- function() {
    req(length(selected_countries()) > 0)
    specs_rv(make_default_specs(selected_countries(), input))
  }
  
  observeEvent(input$build_specs, {
    build_specs_now()
  })
  
  observeEvent(input$reset_specs, {
    build_specs_now()
  })
  
  observeEvent(input$country_select, {
    req(length(selected_countries()) > 0)
    current <- specs_rv()
    
    if (is.null(current) || isTRUE(input$use_defaults_all)) {
      specs_rv(make_default_specs(selected_countries(), input))
    } else {
      keep <- current %>% filter(Country %in% selected_countries())
      missing_cty <- setdiff(selected_countries(), keep$Country)
      
      if (length(missing_cty) > 0) {
        add_rows <- make_default_specs(missing_cty, input)
        keep <- bind_rows(keep, add_rows)
      }
      
      specs_rv(keep %>% arrange(Country))
    }
  }, ignoreNULL = FALSE)
  
  observeEvent(input$use_defaults_all, {
    if (isTRUE(input$use_defaults_all) && length(selected_countries()) > 0) {
      specs_rv(make_default_specs(selected_countries(), input))
    }
  })
  
  observeEvent(input$spec_table, {
    if (!is.null(input$spec_table) && !isTRUE(input$use_defaults_all)) {
      specs_rv(hot_to_r(input$spec_table))
    }
  })
  
  output$spec_table <- renderRHandsontable({
    req(specs_rv())
    
    rhandsontable(
      specs_rv(),
      stretchH = "none",
      rowHeaders = NULL,
      width = "100%",
      height = 450
    ) %>%
      hot_col("Country", readOnly = TRUE) %>%
      hot_col("lambda", format = "0.00") %>%
      hot_col("b_doc", format = "0.00") %>%
      hot_col("b_nmw", format = "0.00") %>%
      hot_col("b_pharm", format = "0.00") %>%
      hot_col("b_dent", format = "0.00") %>%
      hot_col("s_doc", format = "0.00") %>%
      hot_col("s_nmw", format = "0.00") %>%
      hot_col("s_pharm", format = "0.00") %>%
      hot_col("s_dent", format = "0.00") %>%
      hot_col("delta_scaleup", format = "0.000") %>%
      hot_col("delta_shock", format = "0.000") %>%
      hot_validate_numeric(col = 2:12, allowInvalid = FALSE) %>%
      hot_context_menu() %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  current_specs <- reactive({
    req(specs_rv())
    validate_specs_table(specs_rv(), selected_countries())
  })
  
  # Run models only when the user clicks Run model.
  # Countries are modelled separately, then their outputs are combined.
  model_results <- eventReactive(input$run_model, {
    req(raw_data())

    observed_max_year <- suppressWarnings(max(as.numeric(raw_data()$Year), na.rm = TRUE))

    validate(
      need(is.finite(observed_max_year), "The Year column does not contain valid years."),
      need(input$horizon_end >= observed_max_year, "Projection horizon must be at or after the latest observed year."),
      need(input$target_year <= input$horizon_end, "Target year cannot be later than the projection horizon."),
      need(input$target_year >= observed_max_year, "Target year must be at or after the latest observed year.")
    )
    
    need_sum <- input$w_dalys + input$w_mmr + input$w_ncdmr + input$w_uhc
    validate(
      need(abs(need_sum - 1) < 1e-8, "Need weights must sum to 1.00."),
      need(length(selected_countries()) > 0, "Please select at least one country.")
    )
    
    specs <- if (isTRUE(input$use_defaults_all)) {
      validate_specs_table(make_default_specs(selected_countries(), input), selected_countries())
    } else {
      current_specs()
    }
    
    need_w_signed <- c(
      DALYS = input$w_dalys,
      MMR = input$w_mmr,
      NCDMR = input$w_ncdmr,
      UHC_SCI = -input$w_uhc
    )
    
    res_list <- lapply(selected_countries(), function(cty) {
      df_cty <- raw_data() %>% filter(Country == cty)
      spec <- specs %>% filter(Country == cty)
      
      bench <- tibble(
        cadre = c("Doctors", "NursesMidwives", "Pharmacists", "Dentists"),
        B = c(spec$b_doc, spec$b_nmw, spec$b_pharm, spec$b_dent)
      )
      
      shares <- tibble(
        cadre = c("Doctors", "NursesMidwives", "Pharmacists", "Dentists"),
        share = c(spec$s_doc, spec$s_nmw, spec$s_pharm, spec$s_dent)
      )
      
      scenario_delta <- tibble(
        scenario = c("BAU", "ScaleUp", "Shock"),
        delta = c(0, spec$delta_scaleup, spec$delta_shock)
      )
      
      run_hrh_model(
        data = df_cty,
        k_growth = input$k_growth,
        horizon_end = input$horizon_end,
        target_year = input$target_year,
        bench = bench,
        shares = shares,
        lambda = spec$lambda,
        need_w_signed = need_w_signed,
        scenario_delta = scenario_delta
      )
    })
    
    list(
      target_table = bind_rows(map(res_list, "target_table")),
      gaps_smooth = bind_rows(map(res_list, "gaps_smooth")),
      last_year = max(unlist(map(res_list, "last_year")), na.rm = TRUE),
      base_years = bind_rows(map(res_list, "base_years"))
    )
  })
  
  filtered_target <- reactive({
    req(model_results())
    
    df <- model_results()$target_table %>%
      filter(Country %in% selected_countries())
    
    if (input$filter_req != "All") df <- df %>% filter(req_type == input$filter_req)
    if (input$filter_cadre != "All") df <- df %>% filter(cadre == input$filter_cadre)
    if (input$filter_scenario != "All") df <- df %>% filter(scenario == input$filter_scenario)
    
    df
  })
  
  output$vb_country <- renderValueBox({
    req(model_results())
    countries <- sort(unique(selected_countries()))
    valueBox(
      value = paste(countries, collapse = ", "),
      subtitle = if (length(countries) == 1) "Country selected" else "Countries selected",
      icon = icon("globe-africa"),
      color = "aqua"
    )
  })
  
  output$vb_lastyear <- renderValueBox({
    req(model_results())
    yrs <- model_results()$base_years$base_year
    value_txt <- if (length(unique(yrs)) == 1) {
      as.character(unique(yrs))
    } else {
      paste0(min(yrs, na.rm = TRUE), "–", max(yrs, na.rm = TRUE))
    }
    subtitle_txt <- if (length(unique(yrs)) == 1) {
      "Last observed year"
    } else {
      "Country base-year range"
    }
    valueBox(value_txt, subtitle_txt, icon = icon("calendar"), color = "green")
  })
  
  output$vb_targetyear <- renderValueBox({
    req(model_results())
    valueBox(input$target_year, "Target year", icon = icon("bullseye"), color = "yellow")
  })
  
  staff_needed_total <- function(req_type_val, scenario_val) {
    req(model_results())
    model_results()$target_table %>%
      filter(
        Country %in% selected_countries(),
        req_type == req_type_val,
        scenario == scenario_val
      ) %>%
      summarise(total = sum(additional_needed, na.rm = TRUE)) %>%
      pull(total)
  }
  
  output$vb_benchmark_bau <- renderValueBox({
    x <- staff_needed_total("Benchmark", "BAU")
    valueBox(comma(round(x, 0)), "Benchmark - BAU", icon = icon("user-nurse"), color = "red")
  })
  
  output$vb_benchmark_scaleup <- renderValueBox({
    x <- staff_needed_total("Benchmark", "ScaleUp")
    valueBox(comma(round(x, 0)), "Benchmark - Scale-up", icon = icon("user-md"), color = "yellow")
  })
  
  output$vb_benchmark_shock <- renderValueBox({
    x <- staff_needed_total("Benchmark", "Shock")
    valueBox(comma(round(x, 0)), "Benchmark - Shock", icon = icon("hospital-user"), color = "maroon")
  })
  
  output$vb_needlinked_bau <- renderValueBox({
    x <- staff_needed_total("NeedLinked", "BAU")
    valueBox(comma(round(x, 0)), "NeedLinked - BAU", icon = icon("stethoscope"), color = "purple")
  })
  
  output$vb_needlinked_scaleup <- renderValueBox({
    x <- staff_needed_total("NeedLinked", "ScaleUp")
    valueBox(comma(round(x, 0)), "NeedLinked - Scale-up", icon = icon("heartbeat"), color = "green")
  })
  
  output$vb_needlinked_shock <- renderValueBox({
    x <- staff_needed_total("NeedLinked", "Shock")
    valueBox(comma(round(x, 0)), "NeedLinked - Shock", icon = icon("procedures"), color = "navy")
  })
  
  output$snapshot_table <- renderDT({
    req(model_results())
    
    model_results()$target_table %>%
      filter(Country %in% selected_countries(), req_type == "Benchmark", scenario == "BAU") %>%
      mutate(
        across(c(supply_stock, req_stock, gap, additional_needed), ~ round(.x, 1)),
        coverage = round(coverage, 3),
        coverage_pct = round(coverage_pct, 1)
      ) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$target_table <- renderDT({
    req(filtered_target())
    
    filtered_target() %>%
      mutate(
        across(c(supply_stock, req_stock, gap, additional_needed), ~ round(.x, 1)),
        coverage = round(coverage, 3),
        coverage_pct = round(coverage_pct, 1)
      ) %>%
      datatable(options = list(scrollX = TRUE, pageLength = 12), rownames = FALSE, filter = "top")
  })
  
  # Plot coverage ratios; the horizontal line at 1 indicates requirement met.
  output$coverage_plot <- renderPlotly({
    req(model_results())
    
    df <- model_results()$gaps_smooth %>%
      filter(req_type == input$cov_req, Country %in% selected_countries())
    
    threshold_df <- NULL
    
    if (isTRUE(input$cov_compare_countries) && length(selected_countries()) > 1) {
      df <- df %>% filter(scenario == "BAU")
      if (input$cov_cadre != "All cadres") df <- df %>% filter(cadre == input$cov_cadre)
      
      p <- ggplot(df, aes(
        x = Year, y = coverage,
        group = Country, colour = Country,
        text = paste0(
          "Country: ", Country,
          "<br>Cadre: ", cadre,
          "<br>Year: ", Year,
          "<br>Coverage: ", round(coverage, 3)
        )
      )) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2, alpha = 0.8) +
        geom_hline(yintercept = 1, linetype = "dashed", colour = "red", linewidth = 0.9) +
        facet_wrap(~ cadre, scales = "free_y") +
        theme_hrh() +
        labs(
          title = paste("Country comparison of coverage -", input$cov_req),
          subtitle = "BAU scenario | dashed line = threshold of 1.0",
          x = "Year",
          y = "Coverage ratio"
        )
      
      if (isTRUE(input$cov_mark_threshold)) {
        threshold_df <- first_threshold_point(df, coverage, 1, c("Country", "cadre"))
      }
    } else if (input$cov_cadre == "All cadres") {
      p <- ggplot(df, aes(
        x = Year, y = coverage,
        colour = scenario, linetype = scenario,
        group = interaction(Country, scenario),
        text = paste0(
          "Country: ", Country,
          "<br>Cadre: ", cadre,
          "<br>Scenario: ", scenario,
          "<br>Year: ", Year,
          "<br>Coverage: ", round(coverage, 3)
        )
      )) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 1.8, alpha = 0.75) +
        geom_hline(yintercept = 1, linetype = "dashed", colour = "red", linewidth = 0.9) +
        facet_wrap(~ cadre, scales = "free_y") +
        scale_colour_manual(values = scenario_cols) +
        theme_hrh() +
        labs(
          title = paste("Coverage by cadre -", input$cov_req),
          subtitle = "Dashed line = threshold of 1.0",
          x = "Year",
          y = "Coverage ratio"
        )
      
      if (isTRUE(input$cov_mark_threshold)) {
        threshold_df <- first_threshold_point(df, coverage, 1, c("Country", "scenario", "cadre"))
      }
    } else {
      df <- df %>% filter(cadre == input$cov_cadre)
      
      p <- ggplot(df, aes(
        x = Year, y = coverage,
        colour = scenario, linetype = scenario,
        group = interaction(Country, scenario),
        text = paste0(
          "Country: ", Country,
          "<br>Cadre: ", cadre,
          "<br>Scenario: ", scenario,
          "<br>Year: ", Year,
          "<br>Coverage: ", round(coverage, 3)
        )
      )) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2, alpha = 0.8) +
        geom_hline(yintercept = 1, linetype = "dashed", colour = "red", linewidth = 0.9) +
        scale_colour_manual(values = scenario_cols) +
        theme_hrh() +
        labs(
          title = paste(input$cov_cadre, ": Coverage -", input$cov_req),
          subtitle = "Dashed line = threshold of 1.0",
          x = "Year",
          y = "Coverage ratio"
        )
      
      if (isTRUE(input$cov_mark_threshold)) {
        threshold_df <- first_threshold_point(df, coverage, 1, c("Country", "scenario", "cadre"))
      }
    }
    
    if (!is.null(threshold_df) && nrow(threshold_df) > 0) {
      p <- p +
        geom_point(data = threshold_df, aes(x = Year, y = coverage), size = 3, inherit.aes = FALSE) +
        geom_text(
          data = threshold_df,
          aes(x = Year, y = coverage, label = paste0("Reached: ", Year)),
          vjust = -0.8,
          size = 3.4,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }
    
    year_breaks <- get_year_axis_breaks(df$Year, input$target_year, input$horizon_end)
    
    p <- p +
      scale_x_continuous(
        breaks = year_breaks,
        limits = c(min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE)),
        expand = expansion(mult = c(0.01, 0.05))
      )
    
    apply_plotly_style(p) %>%
      layout(xaxis = list(tickmode = "array", tickvals = year_breaks))
  })
  
  # Plot workforce gaps; negative values indicate shortages.
  output$gap_plot <- renderPlotly({
    req(model_results())
    
    df <- model_results()$gaps_smooth %>%
      filter(req_type == input$gap_req, Country %in% selected_countries())
    
    if (isTRUE(input$gap_compare_countries) && length(selected_countries()) > 1) {
      df <- df %>% filter(scenario == "BAU")
      if (input$gap_cadre != "All cadres") df <- df %>% filter(cadre == input$gap_cadre)
      
      p <- ggplot(df, aes(
        x = Year, y = gap,
        group = Country, colour = Country,
        text = paste0(
          "Country: ", Country,
          "<br>Cadre: ", cadre,
          "<br>Year: ", Year,
          "<br>Gap: ", round(gap, 2)
        )
      )) +
        geom_hline(yintercept = 0, colour = "black", linewidth = 0.9) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 1.8, alpha = 0.75) +
        facet_wrap(~ cadre, scales = "free_y") +
        theme_hrh() +
        labs(
          title = paste("Country comparison of gaps -", input$gap_req),
          subtitle = "BAU scenario",
          x = "Year",
          y = "Gap"
        )
    } else if (input$gap_cadre == "All cadres") {
      p <- ggplot(df, aes(
        x = Year, y = gap,
        colour = scenario, fill = scenario,
        group = interaction(Country, scenario),
        text = paste0(
          "Country: ", Country,
          "<br>Cadre: ", cadre,
          "<br>Scenario: ", scenario,
          "<br>Year: ", Year,
          "<br>Gap: ", round(gap, 2)
        )
      )) +
        geom_hline(yintercept = 0, colour = "black", linewidth = 0.9) +
        geom_ribbon(aes(ymin = pmin(gap, 0), ymax = pmax(gap, 0)), alpha = 0.08, colour = NA) +
        geom_line(linewidth = 1.15) +
        facet_wrap(~ cadre, scales = "free_y") +
        scale_colour_manual(values = scenario_cols) +
        scale_fill_manual(values = scenario_cols) +
        theme_hrh() +
        labs(
          title = paste("Gap by cadre -", input$gap_req),
          x = "Year",
          y = "Gap"
        )
    } else {
      df <- df %>% filter(cadre == input$gap_cadre)
      
      p <- ggplot(df, aes(
        x = Year, y = gap,
        colour = scenario, fill = scenario,
        group = interaction(Country, scenario),
        text = paste0(
          "Country: ", Country,
          "<br>Cadre: ", cadre,
          "<br>Scenario: ", scenario,
          "<br>Year: ", Year,
          "<br>Gap: ", round(gap, 2)
        )
      )) +
        geom_hline(yintercept = 0, colour = "black", linewidth = 0.9) +
        geom_ribbon(aes(ymin = pmin(gap, 0), ymax = pmax(gap, 0)), alpha = 0.08, colour = NA) +
        geom_line(linewidth = 1.15) +
        scale_colour_manual(values = scenario_cols) +
        scale_fill_manual(values = scenario_cols) +
        theme_hrh() +
        labs(
          title = paste(input$gap_cadre, ": Gap -", input$gap_req),
          x = "Year",
          y = "Gap"
        )
    }
    
    year_breaks <- get_year_axis_breaks(df$Year, input$target_year, input$horizon_end)
    
    p <- p +
      scale_x_continuous(
        breaks = year_breaks,
        limits = c(min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE)),
        expand = expansion(mult = c(0.01, 0.05))
      )
    
    apply_plotly_style(p) %>%
      layout(xaxis = list(tickmode = "array", tickvals = year_breaks))
  })
  
  # Compare projected supply and requirement headcounts over time.
  output$supplyreq_plot <- renderPlotly({
    req(model_results())
    
    df <- model_results()$gaps_smooth %>%
      filter(
        Country %in% selected_countries(),
        req_type == input$sr_req,
        cadre == input$sr_cadre,
        scenario == input$sr_scenario
      )
    
    validate(need(nrow(df) > 0, "No supply vs requirement data available."))
    
    supply_df <- df %>%
      transmute(
        Country, Year,
        series = "Supply",
        value = supply_stock,
        text = paste0(
          "Country: ", Country,
          "<br>Year: ", Year,
          "<br>Series: Supply",
          "<br>Value: ", scales::comma(round(supply_stock, 1))
        )
      )
    
    req_df <- df %>%
      transmute(
        Country, Year,
        series = "Requirement",
        value = req_stock,
        text = paste0(
          "Country: ", Country,
          "<br>Year: ", Year,
          "<br>Series: Requirement",
          "<br>Value: ", scales::comma(round(req_stock, 1))
        )
      )
    
    plot_df <- bind_rows(supply_df, req_df)
    
    p <- ggplot(plot_df, aes(
      x = Year, y = value,
      colour = series, linetype = series,
      group = interaction(Country, series),
      text = text
    )) +
      geom_line(linewidth = 1.2) +
      scale_colour_manual(values = series_cols) +
      scale_linetype_manual(values = c("Supply" = "solid", "Requirement" = "dashed")) +
      facet_wrap(~ Country, scales = "free_y") +
      theme_hrh() +
      labs(
        title = paste(input$sr_cadre, "-", input$sr_scenario, "-", input$sr_req),
        subtitle = "Solid = supply, dashed = requirement",
        x = "Year",
        y = "Headcount"
      )
    
    year_breaks <- get_year_axis_breaks(df$Year, input$target_year, input$horizon_end)
    
    p <- p +
      scale_x_continuous(
        breaks = year_breaks,
        limits = c(min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE)),
        expand = expansion(mult = c(0.01, 0.05))
      )
    
    apply_plotly_style(p) %>%
      layout(xaxis = list(tickmode = "array", tickvals = year_breaks))
  })
  
  # Rank countries and classify shortage/surplus according to the selected metric.
  ranking_data <- reactive({
    req(model_results())
    
    df <- model_results()$target_table %>%
      filter(
        Country %in% selected_countries(),
        req_type == input$rank_req,
        cadre == input$rank_cadre,
        scenario == input$rank_scenario
      ) %>%
      mutate(
        metric_value = .data[[input$rank_metric]],
        metric_label = dplyr::case_when(
          input$rank_metric == "additional_needed" ~ "Additional needed",
          input$rank_metric == "coverage_pct" ~ "Coverage %",
          input$rank_metric == "gap" ~ "Gap",
          TRUE ~ input$rank_metric
        ),
        Implication = case_when(
          input$rank_metric == "additional_needed" & additional_needed > 0 ~ "Shortage",
          input$rank_metric == "additional_needed" & additional_needed <= 0 ~ "Surplus",
          input$rank_metric == "gap" & gap < 0 ~ "Shortage",
          input$rank_metric == "gap" & gap >= 0 ~ "Surplus",
          input$rank_metric == "coverage_pct" & coverage_pct < 100 ~ "Shortage",
          input$rank_metric == "coverage_pct" & coverage_pct >= 100 ~ "Surplus",
          TRUE ~ NA_character_
        )
      ) %>%
      select(
        Country, cadre, scenario, req_type,
        supply_stock, req_stock, gap, coverage, coverage_pct, additional_needed,
        metric_value, metric_label, Implication
      )
    
    validate(need(nrow(df) > 0, "No ranking data available."))
    
    if (identical(input$rank_order, "desc")) {
      df <- df %>% arrange(desc(metric_value))
    } else {
      df <- df %>% arrange(metric_value)
    }
    
    df %>%
      mutate(
        Country = factor(Country, levels = Country),
        metric_value_label = format_metric_value(metric_value, input$rank_metric)
      )
  })
  
  output$ranking_plot <- renderPlotly({
    df <- ranking_data()
    
    p <- ggplot(df, aes(
      x = Country, y = metric_value,
      fill = Implication,
      text = paste0(
        "Country: ", Country,
        "<br>", metric_label, ": ", metric_value_label,
        "<br>Supply: ", scales::comma(round(supply_stock, 1)),
        "<br>Requirement: ", scales::comma(round(req_stock, 1)),
        "<br>Gap: ", scales::comma(round(gap, 1)),
        "<br>Coverage %: ", round(coverage_pct, 1)
      )
    )) +
      geom_col(width = 0.72, alpha = 0.92) +
      coord_flip(clip = "off") +
      scale_fill_manual(values = status_cols) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
      theme_hrh() +
      labs(
        title = paste("Target year country ranking -", input$rank_cadre),
        subtitle = paste(
          input$rank_req, "|", input$rank_scenario, "| sorted",
          ifelse(input$rank_order == "desc", "highest to lowest", "lowest to highest")
        ),
        x = NULL,
        y = dplyr::first(df$metric_label)
      )
    
    if (isTRUE(input$rank_show_values)) {
      p <- p +
        geom_text(
          aes(label = metric_value_label),
          hjust = ifelse(df$metric_value >= 0, -0.1, 1.1),
          size = 4
        )
    }
    
    apply_plotly_style(p)
  })
  
  output$ranking_table <- renderDT({
    df <- ranking_data() %>%
      mutate(
        Rank = row_number(),
        Supply = round(supply_stock, 1),
        Requirement = round(req_stock, 1),
        Gap = round(gap, 1),
        `Coverage %` = round(coverage_pct, 1),
        `Additional needed` = round(additional_needed, 1),
        `Ranking value` = metric_value_label
      ) %>%
      select(Rank, Country, `Ranking value`, Supply, Requirement, Gap, `Coverage %`, `Additional needed`)
    
    datatable(df, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$download_target <- downloadHandler(
    filename = function() paste0("hrh_target_year_table_", input$target_year, ".csv"),
    content = function(file) readr::write_csv(filtered_target(), file)
  )
  
  output$download_gaps <- downloadHandler(
    filename = function() "hrh_full_gaps_dataset.csv",
    content = function(file) {
      readr::write_csv(
        model_results()$gaps_smooth %>% filter(Country %in% selected_countries()),
        file
      )
    }
  )
  
  output$download_specs <- downloadHandler(
    filename = function() "country_specifications.csv",
    content = function(file) {
      specs_out <- if (isTRUE(input$use_defaults_all)) {
        make_default_specs(selected_countries(), input)
      } else {
        current_specs()
      }
      readr::write_csv(specs_out, file)
    }
  )
}

# Launch the application.
shinyApp(ui, server)
library(shiny)
library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)

# 1. Load data and form monthly tsibble keyed by Varietal (project-relative path)
wines_ts <- read_csv("AustralianWines.csv", show_col_types = FALSE) |>
  mutate(
    Month = yearmonth(my(Month)),
    across(-Month, ~ suppressWarnings(as.numeric(.x)))
  ) |>
  pivot_longer(cols = -Month, names_to = "Varietal", values_to = "Sales") |>
  drop_na(Sales) |>
  as_tsibble(key = Varietal, index = Month)

varietals <- sort(unique(wines_ts$Varietal))
min_dt <- as.Date(min(wines_ts$Month))
max_dt <- as.Date(max(wines_ts$Month))

# 2. UI
ui <- fluidPage(
  titlePanel("Australian Wines Forecasting"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "varietals", "Select Varietal(s):",
        choices = varietals, selected = c("Sparkling", "Red"), multiple = TRUE
      ),
      dateRangeInput(
        "dates", "Date Range:",
        start = min_dt, end = max_dt, min = min_dt, max = max_dt
      ),
      sliderInput(
        "split", "Training Split (%):",
        min = 60, max = 90, value = 80, step = 5
      ),
      numericInput(
        "horizon", "Future Forecast Horizon (Months):",
        value = 12, min = 1, max = 36, step = 1
      ),
      checkboxGroupInput(
        "models", "Models:",
        choices = c("TSLM", "ETS", "ARIMA"),
        selected = c("TSLM", "ETS", "ARIMA")
      ),
      checkboxInput("log_trans", "Apply Log Transformation (Stabilize Variance)", FALSE),
      checkboxInput("show_pi", "Show 80% & 95% Prediction Intervals", TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Forecasts & Accuracy",
          plotOutput("forecast_plot", height = "400px"),
          h4("Accuracy (Training vs. Validation)"),
          tableOutput("acc_table"),
          h4("Model Specifications"),
          tableOutput("spec_table")
        ),
        tabPanel(
          "Overview & Decomposition",
          plotOutput("overview_plot", height = "300px"),
          plotOutput("stl_plot", height = "350px")
        )
      )
    )
  )
)

# 3. Server
server <- function(input, output) {
  
  filtered <- reactive({
    req(input$varietals, input$dates)
    validate(need(input$dates[1] < input$dates[2], "Start date must be before end date."))
    wines_ts |>
      filter(
        Varietal %in% input$varietals,
        as.Date(Month) >= input$dates[1],
        as.Date(Month) <= input$dates[2]
      )
  })
  
  split_df <- reactive({
    df <- filtered()
    validate(need(nrow(df) >= 24, "Select at least 24 months of data."))
    df |>
      group_by(Varietal) |>
      mutate(
        idx = row_number(),
        cutoff = floor(n() * (input$split / 100)),
        Window = if_else(idx <= cutoff, "Training", "Validation")
      ) |>
      ungroup()
  })
  
  train_df <- reactive(split_df() |> filter(Window == "Training") |> select(Varietal, Month, Sales))
  val_df   <- reactive(split_df() |> filter(Window == "Validation") |> select(Varietal, Month, Sales))
  
  fits <- reactive({
    req(input$models)
    trn <- train_df()
    validate(need(nrow(trn) >= 12, "Training set too short."))
    
    defs <- list()
    if (input$log_trans) {
      if ("TSLM" %in% input$models)  defs$TSLM  <- TSLM(log(Sales) ~ trend() + season())
      if ("ETS" %in% input$models)   defs$ETS   <- ETS(log(Sales))
      if ("ARIMA" %in% input$models) defs$ARIMA <- ARIMA(log(Sales))
    } else {
      if ("TSLM" %in% input$models)  defs$TSLM  <- TSLM(Sales ~ trend() + season())
      if ("ETS" %in% input$models)   defs$ETS   <- ETS(Sales)
      if ("ARIMA" %in% input$models) defs$ARIMA <- ARIMA(Sales)
    }
    trn |> model(!!!defs)
  })
  
  fcs <- reactive({
    req(is.numeric(input$horizon), input$horizon >= 1)
    val_n <- val_df() |> as_tibble() |> count(Varietal) |> pull(n) |> max(0)
    fits() |> forecast(h = val_n + input$horizon)
  })
  
  output$overview_plot <- renderPlot({
    ggplot(split_df(), aes(x = Month, y = Sales, color = Window)) +
      geom_line() +
      facet_wrap(~ Varietal, scales = "free_y") +
      labs(y = "Sales", color = "Split") +
      theme_minimal()
  })
  
  output$stl_plot <- renderPlot({
    filtered() |>
      model(STL(Sales ~ season(window = "periodic"))) |>
      components() |>
      autoplot() +
      theme_minimal()
  })
  
  output$forecast_plot <- renderPlot({
    fcs() |>
      autoplot(filtered(), level = if (input$show_pi) c(80, 95) else NULL, alpha = 0.25) +
      facet_wrap(~ Varietal, scales = "free_y") +
      labs(y = "Sales", color = "Model", fill = "Model") +
      theme_minimal()
  })
  
  output$acc_table <- renderTable({
    trn_acc <- accuracy(fits()) |> mutate(Window = "Training")
    val_acc <- if (nrow(val_df()) > 0) accuracy(fcs(), val_df()) |> mutate(Window = "Validation") else tibble()
    bind_rows(trn_acc, val_acc) |>
      select(Varietal, Model = .model, Window, RMSE, MAE, MAPE) |>
      arrange(Varietal, Window, RMSE)
  }, digits = 2)
  
  output$spec_table <- renderTable({
    fits() |>
      as_tibble() |>
      pivot_longer(-Varietal, names_to = "Model", values_to = "Spec") |>
      mutate(Specification = map_chr(Spec, format)) |>
      select(Varietal, Model, Specification)
  })
}

shinyApp(ui, server)


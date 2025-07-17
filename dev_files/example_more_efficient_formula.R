okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ui.R
library(shiny)

fluidPage(
  selectInput("response_var", "Select Response Variable", choices = c("Variable1", "Variable2")),
  checkboxGroupInput("predictor_vars", "Select Predictor Variables", choices = c("PredictorA", "PredictorB", "PredictorC")),
  # ... other UI elements for your Fable model ...
  plotOutput("forecast_plot")
)

# server.R
library(shiny)
library(fable)
library(tsibble)
library(dplyr)

server <- function(input, output, session) {
  
  # Reactive formula string
  formula_string <- reactive({
    paste0(input$response_var, "~", paste(input$predictor_vars, collapse = "+"))
  })
  
  # Reactive Fable model
  my_fable_model <- reactive({
    req(input$response_var, input$predictor_vars) # Ensure inputs are selected
    
    your_tsibble_data %>% # Replace with your actual tsibble data
      model(
        my_model = ARIMA(as.formula(formula_string())) # Example ARIMA model
      )
  })
  
  # Render a forecast plot (example)
  output$forecast_plot <- renderPlot({
    req(my_fable_model()) # Ensure model is built
    
    my_fable_model() %>%
      forecast(h = "1 year") %>%
      autoplot(your_tsibble_data) # Replace with your actual tsibble data
  })
}

shinyApp(ui, server)

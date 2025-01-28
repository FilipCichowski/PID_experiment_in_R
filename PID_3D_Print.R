# Required packages
if (!require("deSolve")) install.packages("deSolve")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("shiny")) install.packages("shiny")
if (!require("GA")) install.packages("GA")

# Libraries
library(deSolve)
library(ggplot2)
library(shiny)
library(GA)

# PID model with anti-windup and physical limits
pid_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    e <- setpoint - temperature
    
    P <- Kp * e
    I <- Ki * error_sum
    D <- Kd * (e - prev_error)
    
    output <- P + I + D
    
    # Anti-windup and output saturation
    if (output > max_output) {
      output <- max_output
      error_sum <- error_sum - Ki * e
    } else if (output < min_output) {
      output <- min_output
      error_sum <- error_sum - Ki * e
    }
    
    # Physical limit: turn off heating if temperature exceeds max_temperature
    if (temperature > max_temperature) {
      output <- 0
    }
    
    # Thermal dynamics
    heat_transfer <- efficiency * output
    cooling <- decay_rate * (temperature - ambient_temp)
    d_temperature <- heat_transfer - cooling
    
    list(c(d_temperature, error_sum = error_sum + e, prev_error = e))
  })
}

# Default parameters
default_parameters <- c(
  Kp = 1, Ki = 0.02, Kd = 0.1,  # Adjusted PID values
  decay_rate = 0.1, efficiency = 0.2,  # Physical properties
  ambient_temp = 25, setpoint = 200,
  max_output = 100, min_output = 0,
  max_temperature = 250  # Physical limit
)

# Initial state
initial_state <- c(temperature = 25, error_sum = 0, prev_error = 0)

# Simulation time
time_points <- seq(0, 300, by = 0.1)

# Function to run PID simulation
run_simulation <- function(parameters, state = initial_state, times = time_points) {
  ode(y = state, times = times, func = pid_model, parms = parameters)
}

# PID optimization
optimize_pid <- function(target_setpoint, decay_rate, ambient_temp) {
  cost_function <- function(params) {
    sim_params <- c(
      Kp = params[1], Ki = params[2], Kd = params[3],
      decay_rate = decay_rate, ambient_temp = ambient_temp,
      setpoint = target_setpoint, max_output = 100, min_output = 0,
      efficiency = 0.2, max_temperature = 250
    )
    
    sim <- run_simulation(sim_params)
    sim <- as.data.frame(sim)
    
    error <- sim$temperature - target_setpoint
    overshoot_penalty <- sum(pmax(0, sim$temperature - target_setpoint)^2)
    stabilization_time <- which(abs(sim$temperature - target_setpoint) < 5)
    stabilization_penalty <- ifelse(length(stabilization_time) > 0, min(stabilization_time), length(sim$time)) * 10
    oscillation_penalty <- sum(diff(sim$temperature)^2)
    
    sum(error^2) + 10 * overshoot_penalty + stabilization_penalty + 5 * oscillation_penalty
  }
  
  ga(
    type = "real-valued",
    fitness = function(params) -cost_function(params),
    lower = c(0, 0, 0), upper = c(2, 0.05, 0.5),  # Adjusted ranges
    popSize = 50, maxiter = 200
  )
}

# Shiny app
shinyApp(
  ui = fluidPage(
    titlePanel("PID Simulation and Optimization for 3D Printer"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("Kp", "Kp (Proportional gain):", min = 0, max = 2, value = 1, step = 0.1),
        sliderInput("Ki", "Ki (Integral gain):", min = 0, max = 0.05, value = 0.02, step = 0.005),
        sliderInput("Kd", "Kd (Derivative gain):", min = 0, max = 0.5, value = 0.1, step = 0.05),
        sliderInput("setpoint", "Target temperature (°C):", min = 50, max = 250, value = 200, step = 5),
        actionButton("optimize", "Optimize PID")
      ),
      mainPanel(
        plotOutput("pid_plot"),
        verbatimTextOutput("optimization_results"),
        uiOutput("status_message")
      )
    )
  ),
  server = function(input, output, session) {
    simulation <- reactive({
      params <- c(
        Kp = input$Kp, Ki = input$Ki, Kd = input$Kd,
        decay_rate = 0.1, ambient_temp = 25,
        setpoint = input$setpoint, max_output = 100, min_output = 0,
        efficiency = 0.2, max_temperature = 250
      )
      sim <- run_simulation(params)
      as.data.frame(sim)
    })
    
    output$pid_plot <- renderPlot({
      sim <- simulation()
      ggplot(sim, aes(x = time)) +
        geom_line(aes(y = temperature, color = "Actual temperature")) +
        geom_hline(yintercept = input$setpoint, linetype = "dashed", color = "red") +
        labs(title = "PID Simulation", x = "Time (s)", y = "Temperature (°C)") +
        theme_minimal()
    })
    
    observeEvent(input$optimize, {
      output$status_message <- renderUI({
        h4("Tuning in progress... Please wait.", style = "color: blue;")
      })
      
      result <- optimize_pid(input$setpoint, decay_rate = 0.1, ambient_temp = 25)
      
      best_params <- result@solution
      updateSliderInput(session, "Kp", value = best_params[1])
      updateSliderInput(session, "Ki", value = best_params[2])
      updateSliderInput(session, "Kd", value = best_params[3])
      
      output$status_message <- renderUI({
        h4("Tuning completed.", style = "color: green;")
      })
      
      output$optimization_results <- renderText({
        paste(
          "Optimized PID parameters:",
          sprintf("Kp = %.2f", best_params[1]),
          sprintf("Ki = %.2f", best_params[2]),
          sprintf("Kd = %.2f", best_params[3])
        )
      })
    })
  }
)
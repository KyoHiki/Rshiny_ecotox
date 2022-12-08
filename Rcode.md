# Load the necessary libraries
library(ggplot2)
library(shiny)
library(drc)
library(readxl)
library(rsconnect)


# Define the Shiny app
shinyApp(
  ui = fluidPage(
    # App title
    titlePanel("Dose-Response Analysis"),
    # Input section
    sidebarLayout(
      sidebarPanel(
        # Input for selecting the data file
        fileInput("data_file", "Choose Excel file",
                  accept = c(".xlsx")),
        # Submit button for uploading the data file
        actionButton("submit", "Submit"),
        # Input for selecting the response variable
        selectInput("response_var", "Response variable",
                    choices = c("", "H24","H48","H72")),
        # Input for selecting the dose variable
        selectInput("dose_var", "Dose variable",
                    choices = c("", "CONC")),
        # Input for selecting the dose variable
        selectInput("unit_var", "Unit",
                    choices = c("", "mg/L","µg/L","ng/L")),
      ),
      # Output section
      mainPanel(
        # Output for displaying the dose-response plot
        plotOutput("dose_response_plot"),
        # Output for displaying the EC50 value
        textOutput("ec50_value")
      )
    )
  ),
  
  # Define the server function
  server = function(input, output) {
    # Reactive function for reading in the data file
    data <- eventReactive(input$submit, {
      # Return NULL if no file is selected
      if (is.null(input$data_file)) {
        return(NULL)
      }
      # Read in the data file
      read_excel(input$data_file$datapath)
    })

    # Reactive function for fitting the dose-response model
    model <- reactive({
      # Return NULL if no data is available
      if (is.null(data())) {
        return(NULL)
      }
      # Fit the model
      drm(as.formula(paste(input$response_var, "~", input$dose_var)),
          data = data(), fct = LL.4())
    })
    # Reactive function for predicting the fitted values
    fitted_values <- reactive({
      # Return NULL if no model is available
      if (is.null(model())) {
        return(NULL)
      }

      # Predict the fitted values
      predict(model())
    })
  
    # Reactive function for estimating the EC50 value and its confidence intervals
    ec50_ci <- reactive({
      # Return NULL if no model is available
      if (is.null(model())) {
        return(NULL)
      }
      # Estimate the EC50 value and its 95% confidence intervals using the ED function
      ed_result <- ED(model(), fct = LL.4(), c(50), intervals = "delta")
      # Extract the EC50 value and its confidence intervals from the result
      ec50_value <- ed_result[1]
      ec50_ci_lower <- ed_result[3]
      ec50_ci_upper <- ed_result[4]
      # Return the EC50 value and its confidence intervals
      list(ec50_value = ec50_value,
           ec50_ci_lower = ec50_ci_lower,
           ec50_ci_upper = ec50_ci_upper)
    })

    # Reactive function for plotting the dose-response curve
    output$dose_response_plot <- renderPlot({
      # Return if no data is available
      if (is.null(data())) {
        return()
      }
      # Plot the data and the fitted values
      ggplot(data(), aes_string(x = input$dose_var, y = input$response_var)) +
        geom_point() +
        geom_line(aes(y = fitted_values()))+
        scale_x_log10()+
        labs(x=paste("Concentration (",input$unit_var,")",sep=""))+
        coord_cartesian(clip = "off")+ 
        annotation_logticks(sides = "b", size = 1, outside = TRUE) +
        theme_bw(base_size=25)
    })
    # Reactive function for displaying the EC50 value
    output$ec50_value <- renderText({
     # Return if no model is available
     if (is.null(model())) {
       return("")
     }
     # Return the EC50 value
     paste("EC50 value: ", round(ec50_ci()$ec50_value, 3)," (95% CI: ",round(ec50_ci()$ec50_ci_lower, 3),"-",round(ec50_ci()$ec50_ci_upper, 3),")")
  })
  }
)

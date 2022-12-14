# Load the necessary libraries
library(ggplot2)
library(shiny)
library(drc)
library(readxl)
library(rsconnect)



# Define the Shiny app
shinyApp(
  ui = fluidPage(
    theme = bslib::bs_theme(bootswatch = "journal"),
    # App title
    titlePanel("Dose-Response Analysis"),
    # Input section
    sidebarLayout(
      sidebarPanel(
        # Input for selecting the test species
        radioButtons("Species", "Test species",
             choices = c( "Algae: TG201","Daphnia: TG202","Fish: TG203" ),
             selected = "Algae: TG201"),
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
          tabsetPanel(
            tabPanel("Plot",plotOutput("dose_response_plot")),
            tabPanel("Statistics",textOutput("ec50_value")),
          ),
          width=8
        )
      )
    ),
  
  
  # Define the server function
  server = function(input, output) {
    # Reactive function for reading in the data file
    data_up <- eventReactive(input$submit, {
      # Return NULL if no file is selected
      if (is.null(input$data_file)) {
        return(NULL)
      }
      read_excel(input$data_file$datapath) # Read in the data file
    })
 
    # Reactive function for fitting the dose-response model
    model <- reactive({
    # Return NULL if no data is available
    if (is.null(data_up())) {
      return(NULL)
    }
    # Determine which model to fit based on the selected test species
      if (input$Species == "Algae: TG201") {
        # Fit a continuous model for algae
        
        Ctr <- data_up() %>% dplyr::filter(input$dose_var == min(input$dose_var)) %>% dplyr::select(input$response_var) %>%
          summarize(Mean= mean(.)) %>% as.numeric
        Algae_model <- drm(as.formula(paste(input$response_var, "~", input$dose_var)),data=data_up(),fct = LL.2(upper=Ctr), type=c("continuous"))
        return(Algae_model)
        } else {
        # Fit a binomial model for fish and daphnia
        Other_model <- drm(as.formula(paste(input$response_var, "~", input$dose_var)),data=data_up(),fct = LL.2(), type=c("binomial") )
        return(Other_model)
        }
      })
    
    # Reactive function for predicting the fitted values
    fitted_values <- reactive({
      # Return NULL if no model is available
      if (is.null(model())) {
        return(NULL)
      }
      # Predict the fitted values
      Conc <- expand.grid(Conc=exp(seq(log(0.001), log(10), length=10000))  )  
      Pred <- predict(model(), newdata=Conc, interval="confidence") %>% cbind(Conc)
      return(Pred)
    })
  
    # Reactive function for estimating the EC50 value and its confidence intervals
    ec50_ci <- reactive({
      # Return NULL if no model is available
      if (is.null(model())) {
        return(NULL)
      }
      # Estimate the EC50 value and its 95% confidence intervals using the ED function
      ed_result <- ED(model(), c(50), intervals = "delta")
      return(data.frame(EC50=ed_result[1],Lower=ed_result[3],Upper=ed_result[4]))
    })

    # Reactive function for plotting the dose-response curve
    output$dose_response_plot <- renderPlot({
      # Return if no data is available
      if (is.null(data_up())) {
        return()
      }
      # Plot the data and the fitted line
      ggplot() +
        geom_point(data=data_up(), aes_string(x = input$dose_var, y = input$response_var), size=4,pch=19) +
        geom_line(data=fitted_values(), aes(x=Conc, y =Prediction))+
        scale_x_log10()+
        labs(x=paste("Concentration (",input$unit_var,")",sep=""))+
        coord_cartesian(clip = "off")+ 
        annotation_logticks(sides = "b", size = 1, outside = TRUE) +
        theme_bw(base_size=25)
    }, height = 500, width = 600
    )
    
    # Reactive function for displaying the EC50 value
    output$ec50_value <- renderText({
     # Return if no model is available
     if (is.null(model())) {
       return("")
     }
     # Return the EC50 value
     paste("EC50 value: ", round(ec50_ci()$EC50, 3)," (95% CI: ",round(ec50_ci()$Lower, 3),"-",round(ec50_ci()$Upper, 3),")")
  })
  }
)


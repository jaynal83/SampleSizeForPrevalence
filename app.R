# Sample Size Calculation for Cross SEctional Study to Estimate Prevalence
# Author: Jaynal Abedin <joystatru@gmail.com>
# Date: 14-Jun-2020
# Version: 0.0.1

#################################################
# Libraries
#################################################

library(shiny)
library(shinydashboard)
#library(readr)
#library(ggplot2)
#library(plotly)
#library(purrr)


jsCode = 'shinyjs.clear_warning = function(){document.getElementById("note_save_confirm").innerHTML = "";}'
# Loading Spinner Settings
options(
  spinner.color="#ffd700", 
  spinner.type = 2, 
  spinner.color.background = 'white' 
)


#################################################
# Define UI
#################################################

### Header
header <- dashboardHeader(
  title = "Sample Size Calculation"
)

### Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text = "Sample Size",
      tabName = "Home",
      icon = icon("bell")
    )
  )
)

### Body
body <- dashboardBody(
  tags$style(
    type = "text/css",
    ".shiny-output-error{visibility: hidden;}",
    ".shiny-output-error:before{visibility:hidden;}"
  ),
  tabItems(
    tabItem(
      tabName = "Home",
      fluidRow(
        column(
          width = 4,
          numericInput(
            inputId = "expectedP",
            label = "Expected Prevalence [0 to 1]",
            min = 0.001,
            max = 0.999,
            step = 0.001,
            value = 0.028
          )
        ),
        column(
          width = 4,
          numericInput(
            inputId = "sensitivity",
            label = "Sensitivity [0 to 1] (Probability of Test Positive Who Actually Has the Disease)",
            min = 0.5, max = 1, step = 0.0001,
            value = 1
          )
        ),
        column(
          width = 4,
          numericInput(
            inputId = "specificity",
            label = "Specificity [0 to 1] (Probability of Test Negative Who Actually doesn't have the Disease)",
            min = 0.5, max = 1, step = 0.0001,
            value = 0.9981
          )
        ),
        column(
          width = 4,
          numericInput(
            inputId = "precision",
            label = "Precision (Half Width of Confidence Interval: Choose a value 0.5 Times Expected Prevalence or Less)",
            min = 0.0001, max = 0.05, step = 0.0001,
            value = 0.002
          )
        ),
        column(
          width = 4,
          numericInput(
            inputId = "confidenceLevel",
            label = "Level of Confidence (Usually 95%)",
            min = 0.8, max = 0.99, step = 0.01,
            value = 0.95
          )
        )
      ),
      fluidRow(
        box(width = 12, title = "Sample Size", status = "info",
            solidHeader = TRUE, collapsible = TRUE,
            footer = "Amplify the sample size to adequately accommodate FALSE POSITIVE and FALSE NEGATIVE and DESIGN EFFECT",
            textOutput(outputId = "sampleSize")
        )
      ),
      fluidRow(
        box(width = 12, title = "Supplementary Information", status = "info",
            solidHeader = TRUE, collapsible = TRUE,
            textOutput(outputId = "suppInfo")
        )
      )
    )
  )
)

#################################################
# Define server
#################################################
server <- shinyServer(
  function(input, output){
    # Sample Size
    getN <- reactive({
      z <- qnorm(1-(1-input$confidenceLevel)/2)
      multiplier <- (z^2/input$precision^2) 
      numerator_1 <- ((input$sensitivity*input$expectedP)+(1-input$specificity)*(1-input$expectedP))
      numerator_2 <- ((1-input$sensitivity*input$expectedP)-(1-input$specificity)*(1-input$expectedP))
      denominator <- (input$sensitivity+input$specificity-1)^2
      n <- round(multiplier*((numerator_1*numerator_2)/denominator), digits = 0)
      return(n)
    })
    
    # Positive Predictive Value
    ppv <- reactive({
      (input$sensitivity*input$expectedP)/((input$sensitivity*input$expectedP)+((1-input$specificity)*(1-input$expectedP)))
    })
    
    # Negative Predictive Value
    npv <- reactive({
      (input$specificity*(1-input$expectedP))/((1-input$sensitivity)*input$expectedP+input$specificity*(1-input$expectedP))
    })
    
    # Prevalence Threshold (A minimum prevelence that can be estimated using the diagnostic test)
    pt <- reactive({
      (sqrt(input$sensitivity*(1-input$specificity))+input$specificity-1)/(input$sensitivity+input$specificity-1)
    })
    
    output$sampleSize <- renderText({
      paste0("A sample of size ", getN(), " is required to estimate the expected prevalence ", 
             input$expectedP, " with a precision of  ", input$precision, 
             " that is the expected prevalence could be between ", input$expectedP-input$precision, 
             " and ", input$expectedP+input$precision,
             " where the sensitivity of the diagnostic test is ", input$sensitivity,
             " and the specificity of it is ", input$specificity) 
    })
    
    output$suppInfo <- renderText({
      paste0("Probability of a TEST POSITIVE person who actually has the disease is ", round(ppv(), digits = 4),".",
             " That is, out of 100 TEST POSITIVE person, there will be ",round(100- 100*ppv(), digits = 2),
             " persons who actually DO NOT HAVE the disease (False Positive).",
             " On the other hand, the probability of a TEST NEGATIVE person who actually disease free is ",
             round(npv(),digits = 4), ".", " That is, out of 100 TEST NEVATIVE person, there will be ",
             round(100- 100*npv(), digits = 2), " person who actually HAVE THE DISEASE (False Negative)")
    })
  }
)

#################################################
# Run the Application
#################################################
shinyApp(
  ui =  dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
  ),
  server = server
)



library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(plotly)

# Assuming adlbc dataset is already loaded in init.R
source("init.R", local = TRUE)
source("plot_functions.R", local = TRUE)

# Define UI for application
ui <- navbarPage("Data Visualization", 
                 tabPanel("Plots",
                          column(
                            2, wellPanel(
                              id = "well_options",
                              style = "max-height: 2000px; font-size: small",
                              tags$style(
                                HTML("
                                     .radio { margin-bottom: 5px; }
                                     .form-group { margin-bottom: 5px; }
                                     ")
                              ),
                              radioButtons(inputId = "plot_type",
                                           label = "Plot Type:",
                                           choices = c("Line Plot" = "line", 
                                                       "Box Plot" = "box",
                                                       "Bar Plot" = "bar",
                                                       "Spaghetti Plot" = "spa")),
                              checkboxGroupInput(inputId = "treatment",
                                                 label = "Treatment Arm",
                                                 choices = as.character(unique(adlbc$TRTA)),
                                                 selected = "Placebo"),
                              uiOutput("age_group"),
                              uiOutput("test_selected"),
                              style = "margin-bottom: 0;"
                            )
                          ),
                          column(
                            10,
                            plotlyOutput("selected_plot"),
                            DT::dataTableOutput("data_table"),
                            align = "center"
                          )
                          
                 ),
                 tabPanel("AI Assistant")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  dataset <- reactive({
    adlbc %>% filter(TRTA %in% unlist(input$treatment), 
                     AGEGR1 %in% unlist(input$age_group),
                     PARAM %in% unlist(input$test)) %>%
      select(USUBJID, AGE, AGEGR1, RACE, 
             SEX, TRTA, ADY, AVISIT, AVISITN,
             PARAM, AVAL, BASE, 
             LBNRIND, LBSTRESN) %>% 
      filter(!is.na(LBSTRESN)) %>%
      filter(!is.na(AVISITN))
  })
  
  output$data_table <- DT::renderDataTable({
    dataset()
  })
  
  output$selected_plot <- renderPlotly({
    data <- dataset()
    
    if (input$plot_type == "line") {
      line_plot(dat = data)
    } else if (input$plot_type == "box") {
      box_plot(dat = data)
    } else if (input$plot_type == "bar") {
      bar_plot(dat = data)
    } else if (input$plot_type == "spa"){
      spaghetti_plot(dat = data)
    }
  })
  
  output$age_group <- renderUI({
    checkboxGroupInput(inputId = "age_group",
                       label = "Age Group",
                       choices = as.character(unique(adlbc$AGEGR1[adlbc$TRTA %in% input$treatment])),
                       selected = "<65")
  })
  
  output$test_selected <- renderUI({
    pickerInput(inputId = "test",
                label = "Test",
                choices = as.character(unique(adlbc$PARAM[(adlbc$TRTA %in% input$treatment) & 
                                                            (adlbc$AGEGR1 %in% input$age_group)])),
                selected = "Sodium (mmol/L)",
                options = list(`actions-box` = TRUE),
                multiple = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(RSQLite)
library(shinyjs)
library(rstudioapi)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

addResourcePath("resources", "HelpDoc")

ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  
  titlePanel("Non-Life Insurance Reserving Application"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Triangle CSV File", accept = ".csv"),
      fileInput("file2", "Vector CSV File", accept = ".csv"),
      actionButton("calculate", "Import Data"),
      br(),
      actionButton("retrieve", "Retrieve LoB from Database"),
      uiOutput("dataSelection"),
      actionButton("add_tab", "Retrieve Reserving Data"),
      actionButton("del_tabs", "Delete all tabs"),
      br(),
      actionButton("save_results", "Save results"),
      actionButton("delete_results", "Delete results"),
      actionButton("retrieve_results", "Retrieve results"),
      actionButton("Excel_Export", "Create Excel Sheets"),
      tags$a(href = "resources/tables.xlsx", 
             class = "btn btn-light",
             style = "background-color: white; color: black; border: 1px solid #ccc; margin-top: 10px;",
             download = NA,
             target = "_blank",
             "Download Excel"
      )
      
    ),
    mainPanel(
      tableOutput("contents1"),
      verbatimTextOutput("summary1"),
      plotOutput("trianglePlot")
    )
  ),
  tabsetPanel(id = "tabs",
            tabPanel("Home", "Select a dataset to display its data in a new tab."),
            tabPanel("Help", 
                     tags$iframe(
                       src = "resources/2025_07_09 Reserving tool documentation.htm",
                       width = "100%",
                       height = "600px",
                       seamless = "seamless"
                     )
            )
  )
)


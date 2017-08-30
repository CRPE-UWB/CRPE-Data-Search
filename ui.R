###################################################################################################
# Libraries Needed ----------------------------------------------------------
library(shiny) # Need to Run Shiny App
library(shinyWidgets) # Library of More Widgets like the Dropdown Button 
library(shinyjs) # Need for shinyjs
library(DT)
library(V8)
library(RMySQL)
library(dplyr)
library(leaflet)
library(htmltools)

source('scripts/list.r')
source('scripts/global.r')
## Frontend of the Shiny App-----------------------------------------------
shinyUI(
  
  fluidPage(
    # Include shinyjs in the UI
    shinyjs::useShinyjs(),  
    # JS code for Reset Button
    shinyjs::extendShinyjs(text = jsResetCode),
    tabsetPanel(
    tabPanel("Data Search",
    fluidRow(style = "padding:10px;",
             column(12, 
                    img(src="http://www.crpe.org/sites/all/themes/crpe/images/crpeLogocj2.svg", 
                        width = "150px", 
                        height = 'auto', 
                        align = 'left')
             )
    ), 
    
    fluidRow(div(id = "inputs",
      # Displays Dropdown Menu 2: Year 
      column(3, 
             # Application title
             
             pickerInput(
               inputId = "dataset", 
               label = "Select Dataset", 
               choices = DATASETS,
               selected = DATASETS[1],
               multiple = FALSE)
      ),
      
      column(3,
             pickerInput(
               inputId = "state", 
               label = "Select States", 
               choices = STATES, 
               selected = NULL,
               multiple = TRUE)
      ),
      
      column(3,
             selectInput(
               inputId = "district", 
               label = "Select District", 
               choices = "ALL",
               selected = "ALL",
               multiple = TRUE)
      )
    ),
      
    
    
    fluidRow(
      
      column(12, 
             conditionalPanel(condition ="input.dataset&&input.state",
                               
                              style = "padding-left:15px;",
                              # Displays the Submit button
                              actionButton(inputId = "submitBttn", 
                                           label = "Submit",
                                           width = '70px'))
      )
    )),
    
    
    
    # MainPanel -------------------------------------------
    # -The Output/Table Displayed Based on Input
    fluidRow(style = "padding:10px;",
             conditionalPanel(condition = "output.setupComplete",
                              textOutput(outputId = "caption"),
                              textOutput("caption1"),
                              textOutput("caption2"),
                              conditionalPanel(style="padding-left:10px;",
                                condition="output.setupComplete&&input.submitBttn",
                                                        checkboxInput(inputId = "responsive",
                                                                      label = "Display full-width table",
                                                                      value=FALSE)
                              ),
                              dataTableOutput(outputId = "myTable")
             ),
             conditionalPanel(condition = "!output.setupComplete&&input.submitBttn",
                              p("The Data Is Loading...")
             )
    ),
    
    
    fluidRow(style = "padding: 5px;",
             conditionalPanel(condition = "output.setupComplete&&input.submitBttn",
                              column(2, 
                                     # Displays Download Button
                                     downloadButton(outputId = 'downloadBttn', 
                                                    label = 'Download')
                              ),
                              column(2, 
                                     # Displays the Reset Button
                                     actionButton(inputId = "resetBttn",
                                                  label = "Reset",
                                                  width = '85px')
                              )
                              
             ) 
    )
  ),
  tabPanel("MAP",
           fluidRow(style = "padding:10px;",
                    conditionalPanel(condition="input.dataset=='Schools'&&output.setupComplete",
                      leafletOutput("myMap", width = "100%", height = 500)
                    ),
                    conditionalPanel(condition="!output.setupComplete", style="padding:10px;",
                                     leafletOutput("aMap", width = "100%", height = 500)
                      )
                    ),
           fluidRow(div(id="school data", style="float:left;",
                        column(width=6,
                           tableOutput("testData1"))),
                    div(id="dist data" , style = "float:right;",
                        column(width=6,
                           tableOutput("testData2")))
                    )
                    
                    
  ),
  tabPanel("Codelist",
           h3("Codelist"), 
           h6("Schools table:"),
           fluidRow(style="padding:15px;",
                    column(2, 
                           p("Type: "),
                           p("Chartr"))
           
                    )
           ), 
  tabPanel("About", 
           h1("About"), 
           h4("A Web Application for The Center on Reinventing Public Education"),
           p("This application was commissioned by C.R.P.E. in order to enhance current 
             research strategies."), 
           p("The consideration of a school's success has to date been about looking at the 
             numbers that they produce on annual reports- enrollment rates, graduation rates, 
             funding totals, etc; however at C.R.P.E. we know that these analyses are in no way 
             exhaustive. In order to begin to understand how certain environmental factors might 
             influence school success, this tool helps us plot schools on a map, with the option 
             to explore public resources in the vicinity, filter by charter/non charter, etc."),
           p("Developed by Simon Classon and Kevin Cha"))

  )))

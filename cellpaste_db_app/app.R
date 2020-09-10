
library(shiny)
library(tidyverse)
library(DT)

#fields to save when updating database
submission_list <- c("date_action","mod_type","batch_id","bag_number",
                     paste0("batch_id","_","bag_number"),
                     "sample_weight","material_type","operator_name",
                     "freezer_name","shelf_number","reason")





# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Cell Paste Database"),
    
    
    tabsetPanel( tabPanel("Database", 
                          mainPanel(
                              dataTableOutput('cell_db_table'))
                          ), 
                 tabPanel("New Batch Entry", 
                          dateInput(inputId = "date_action",
                                    label = "Date of Action",
                                    format = "yymmdd"),
                          
                          selectInput(inputId = "mod_type",
                                      label = "Type of Change",
                                      c("Entry","Removal","Balance")),

                          textInput(inputId = "batch_id",
                                    label = "Batch ID (PB######-SU1,2,3)"),

                          numericInput(inputId = "bag_number",
                                       label = "Bag Number",
                                       value = 1,
                                       min = 1),

                          numericInput(inputId = "sample_weight",
                                       label = "Sample Weight (minus bag tare)",
                                       value = 1,
                                       min = 1),
                          
                          radioButtons(inputId = "material_type",
                                       label = "Type of Material",
                                       choices = c("Pellet","Ferm-Sup")),

                          textInput(inputId = "operator_name",
                                    label = "Operator Name"),

                          textInput(inputId = "freezer_name",
                                    label = "Freezer Name"),
                          numericInput(inputId = "shelf_number",
                                        label = "Shelf from Top (top = 1)",
                                        value = 1,
                                        min = 1),

                          textInput(inputId = "reason",
                                     label = "Reason"),
                          
                          actionButton(inputId = "upload_entry",
                                       label = "Submit to Database"),
                          br(),
                          br()
                          ), 
                 
                 tabPanel("Graphics?", "contents"))
)
    
    
    #Thanks to ____ for this introduction which I adapted.
    #https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
    

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #ends app run when page is closed in browser
    #Thanks to the following for the tip:
    #https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
    onSessionEnded(function() {stopApp()})
    
    cell_db <- read_csv("./app_demo_data.csv") %>% 
        transform(Date_Added_YYMMDD = as.Date.character(Date_Added_YYMMDD,"%y%m%d"))
    
    output$cell_db_table <- DT::renderDataTable(cell_db)
    
    
    
    #observe event and save data to new row and re-save dataframe
    observeEvent(input$upload_entry,{
    transistion_db <- rbind(cell_db, submission_list)
    write.csv(transistion_db, file = "./app_demo_data.csv")
    })
    #this doesnt work yet.

}

# Run the application 
shinyApp(ui = ui, server = server)

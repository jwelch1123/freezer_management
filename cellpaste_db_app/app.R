
library(shiny)
library(tidyverse)
library(DT)


# Shiny App UI ####
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
                          
                          textInput(inputId = "strain_id",
                                    label = "Strain ID (PP##)"),

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
    

# Shiny App Server ####
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #ends app run when page is closed in browser
    #Thanks to the following for the tip:
    #https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
    onSessionEnded(function() {stopApp()})
    
    #Import statement ####
    cell_db <- read_csv("./app_demo_data.csv") %>% 
        transform(Date_Added = as.Date(Date_Added,"%m/%d/%y"))
    
    output$cell_db_table <- DT::renderDataTable(cell_db)
    
    #Save the database entry ####
    #observe event and save data to new row and re-save dataframe
    observeEvent(input$upload_entry,{
        
        #c() is wrong, it coerses the same class for all values
        submission_list <- list(input$date_action,
                             input$mod_type,
                             input$batch_id,
                             input$bag_number,
                             paste0(input$batch_id,"_",input$bag_number),
                             input$sample_weight,
                             input$material_type,
                             input$strain_id,
                             input$operator_name,
                             input$freezer_name,
                             input$shelf_number,
                             input$reason)

        transistion_db <- rbind(cell_db, submission_list) %>% 
            transform(Date_Added = as.character(Date_Added,"%m/%d/%y"))
        write.csv(transistion_db, file = "./app_demo_data.csv", row.names = F)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

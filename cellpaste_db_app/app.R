
library(shiny)
library(tidyverse)
library(DT)


# Shiny App UI ####
# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Cell Paste Database"),
    
    
    tabsetPanel( tabPanel("Current Inventory", 
                          mainPanel(
                              dataTableOutput("inventory_db_table"))
                          ), 
                 
                tabPanel("Overall Ledger", 
                          mainPanel(
                              dataTableOutput("ledger_db_table"))
                          ), 
                 
                 tabPanel("New Batch Input", 
                          br(),
                          dateInput(inputId = "date_action",
                                    label = "Date of Action",
                                    format = "yymmdd"),

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
                          br(),
                          br(),
                          actionButton(inputId = "upload_entry",
                                       label = "Submit to Database"),
                          br(),
                          br()
                          ), 
                 
                 tabPanel("Batch Modification", 
                          dateInput(inputId = "date_action",
                                    label = "Date of Action",
                                    format = "yymmdd"),
                          
                          selectInput(inputId = "mod_type",
                                      label = "Type of Change",
                                      c("Removal","Set Balance"),
                                      selected = "Removal"),
                          
                          selectizeInput(inputId = "unique_batch_id",
                                         label = "Unique Batch ID (PB + bag #)",
                                         choices = NULL,
                                         selected = NULL)
                                        
                          ),
                
                 tabPanel("graphics","contents")
                 
    )
)
    
    
    #Thanks to ____ for this introduction which I adapted.
    #https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
    

# Shiny App Server ####
server <- function(input, output, session) {
    #ends app run when page is closed in browser
    #Thanks to the following for the tip:
    #https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window

    # Ending the session cleanly like this doesn't allow
    # the page to be reloaded when bag is submitted.
    #onSessionEnded(function() {stopApp()})

    #Inport statements ####
    inventory_db <- read_csv("./app_inventory.csv") %>% 
        transform(Date_Modified = as.Date(Date_Modified,"%m/%d/%y"))
    
    ledger_db <- read_csv("./app_ledger.csv") %>% 
        transform(Date_Added = as.Date(Date_Added, "%m/%d/%y"))
    
    output$inventory_db_table <- DT::renderDataTable(inventory_db)
    output$ledger_db_table <- DT::renderDataTable(ledger_db)
    
    
    #Material Entry update ####
    #observe event and save data to new row and re-save dataframe
    observeEvent(input$upload_entry,{
        
        #c() is wrong, it coerses the same class for all values
        ledger_update_list <- list(input$date_action,
                             "Entry",
                             input$batch_id,
                             input$bag_number,
                             paste0(input$batch_id,"_",input$bag_number),
                             input$sample_weight,
                             input$material_type,
                             input$strain_id,
                             input$operator_name,
                             input$freezer_name,
                             input$shelf_number,
                             "Batch Harvest")
        
        ledger_update_db <- rbind(ledger_db, ledger_update_list) %>% 
            transform(Date_Added = as.character(Date_Added,"%m/%d/%y"))
        
        write.csv(ledger_update_db, file = "./app_ledger.csv", row.names = F)
    
        inventory_update_list <- list(input$date_action,
                                      paste0(input$batch_id,"_",input$bag_number),
                                      input$batch_id,
                                      input$bag_number,
                                      input$strain_id,
                                      input$material_type,
                                      input$sample_weight, #needs update
                                      input$freezer_name,
                                      input$shelf_number)        

        inventory_update_db <- rbind(inventory_db, inventory_update_list) %>% 
            transform(Date_Added = as.character(Date_Added,"%m/%d/%y"))
        
        write.csv(inventory_update_db, file = "./app_inventory.csv", row.names = F)        
        
        #reload the page to show people things happened
        #not elegant but functional.
        session$reload()
    
    })
    
    # Mod Update ####
    # observeEvent(input$upload_modification,{
    #     inventory_update_list <- list(input$date_action,
    #                                   paste0(input$batch_id,"_",input$bag_number),
    #                                   input$batch_id,
    #                                   input$bag_number,
    #                                   input$strain_id,
    #                                   input$material_type,
    #                                   input$sample_weight, #needs update
    #                                   input$freezer_name,
    #                                   input$shelf_number)
    # 
    # 
    # })
    
    # Selectize Updates
    
    #unique_batches <- updateSelectizeInput(session, "unique_batch_id", choices = inventory_db$Unique_Bag_ID)
    updateSelectizeInput(session, "unique_batch_id", 
                         choices = inventory_db$Unique_Bag_ID, 
                         selected = "", server = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

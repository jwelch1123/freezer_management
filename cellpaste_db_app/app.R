
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
                                    label = "Batch ID (PB######)"), #FLAG
                          
                          checkboxGroupInput(inputId = "input_su_checkboxes",
                                             label = "Select Relavent SU units",
                                             choiceNames = paste0("SU",1:6),
                                             choiceValues = 1:6,
                                             inline = T),

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

                          selectizeInput(inputId = "operator_name",
                                    label = "Operator Name",
                                    choices = NULL,
                                    selected = NULL,
                                    options = list( create = TRUE,
                                                    createOnBlur = TRUE)),

                          textInput(inputId = "freezer_name",
                                    label = "Freezer Name"),
                          numericInput(inputId = "shelf_number",
                                        label = "Shelf from Top (top = 1)",
                                        value = 1,
                                        min = 1),
                          br(),
                          br(),
                          actionButton(inputId = "upload_entry",
                                       label = "Submit to Database"),
                          br(),
                          br()
                          ), 
                 
                 tabPanel("Batch Modification", 
                          dateInput(inputId = "mod_date_action",
                                    label = "Date of Action",
                                    format = "yymmdd"),
                          
                          selectInput(inputId = "mod_type",
                                      label = "Type of Change",
                                      c("Removal","Set Balance"), #change?
                                      selected = "Removal"),
                          
                          selectizeInput(inputId = "mod_unique_batch_id",
                                         label = "Unique Batch ID (PB + bag #)",
                                         choices = NULL,
                                         selected = NULL),
                          
                          numericInput(inputId = "mod_value_change",
                                      label = "How much is removed? (g)",
                                      value = 0),
                          
                          selectizeInput(inputId = "mod_operator",
                                      label = "Operator preforming action",
                                      choices = NULL,
                                      selected = NULL,
                                      options = list( create = TRUE,
                                                      createOnBlur = TRUE) ),
                          
                          textInput(inputId = "mod_reason",
                                      label = "Reason for Change",
                                      value = ""),
                          
                          checkboxInput(inputId = "mod_remove_unique_id",
                                      label = "Should this Unique bag be removed?",
                                      value = FALSE),
                          br(),
                          br(),
                          actionButton(inputId = "mod_update_entry",
                                       label = "Submit Change to Database"),                          
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
    
    
    # Entry update ####
    #observe event and save data to new row and re-save dataframe
    observeEvent(input$upload_entry,{
        
        #c() is wrong, it coerses the same class for all values
        ledger_update_list <- list(input$date_action,
                             "Entry",
                             paste0(input$batch_id,"_SU",input$input_su_checkboxes),
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
                                      input$sample_weight, 
                                      input$freezer_name,
                                      input$shelf_number)        

        inventory_update_db <- rbind(inventory_db, inventory_update_list) %>% 
            transform(Date_Added = as.character(Date_Added,"%m/%d/%y"))
        
        write.csv(inventory_update_db, file = "./app_inventory.csv", row.names = F)        
        
        #reload the page to show people things happened
        #not elegant but functional.
        session$reload()
    
    })
    
    # Modification Update ####
    observeEvent(input$mod_update_entry,{
        
        # issue ####
        # need to check for unique bag ID before assigniing
        if(input$mod_unique_batch_id == ""){
            session$reload()
        }
        
        inventory_update_row <- (inventory_db %>%  
                            filter(., inventory_db$Unique_Bag_ID == input$mod_unique_batch_id) %>% 
                            split(., seq(nrow(.))))[[1]]
        ledger_update_row <- (ledger_db %>% 
                            filter(., inventory_db$Unique_Bag_ID == input$mod_unique_batch_id) %>% 
                            split(., seq(nrow(.))))[[1]]
        
        # If removing, drop row and change ledger value to negative of weight
        # If not removing, update the inventory values, bind to inventory db, and update ledger weight.
        if({input$mod_remove_unique_id}){ 
            inventory_db <- inventory_db %>% subset(.,Unique_Bag_ID != input$mod_unique_batch_id)
            ledger_update_row$Weight = 0 - ledger_update_row$Weight
            
        } else{
            inventory_update_row$Date_Modified = input$mod_date_action
            inventory_update_row$Weight = inventory_update_row$Weight - input$mod_value_change
            
            inventory_db <- inventory_db %>% subset(.,Unique_Bag_ID != input$mod_unique_batch_id)
            inventory_db <- rbind(inventory_db, inventory_update_row)
            
            ledger_update_row$Weight = ledger_update_row$Weight - input$mod_value_change
        }
        
        # Update ledger values and bind row to dataframe: happens even no matter the change.
        ledger_update_row$Date_Added = input$mod_date_action
        ledger_update_row$Operator = input$mod_operator
        ledger_update_row$Purpose = input$mod_reason
        print('ledger_db')
        print(ncol(ledger_db))
        print(head(ledger_db))
        print("ledger_update_row")
        print(length(ledger_update_row))
        print(ledger_update_row)
        
        ledger_db <- rbind(ledger_db, ledger_update_row)

        # Write inventory and ledger to CSV files
        write.csv(inventory_db, file = "./app_inventory.csv", row.names = F)
        write.csv(ledger_db, file = "./app_ledger.csv", row.names = F)
        
        session$reload()
    })
    
    # Selectize Updates ####
    # list of inventory batches
    updateSelectizeInput(session, "mod_unique_batch_id", 
                         choices = inventory_db$Unique_Bag_ID, 
                         selected = "", server = TRUE)
    # Names for batch changes
    updateSelectizeInput(session, "mod_operator", 
                         choices = ledger_db$Operator, 
                         selected = "", server = TRUE)
    # Names for batch additions
    updateSelectizeInput(session, "operator_name", 
                         choices = ledger_db$Operator, 
                         selected = "", server = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

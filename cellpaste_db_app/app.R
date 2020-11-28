
library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)

# Shiny App UI ####
ui <- navbarPage(
    title = "Cell Paste Database",
             # Inventory & Ledger ####
             tabPanel("Inventory & Ledger", 
                      
                      selectInput(inputId = "table_type",
                                  label = "Select table to View: ",
                                  choices = c("Current Inventory", "Overall Ledger"),
                                  selected = "Current Inventory"),
                      dataTableOutput("selected_table")
                      ), 
             # Batch Input ####
             tabPanel("New Batch Input", 
                      br(),
                      dateInput(inputId = "date_action",
                                label = "Date of Action",
                                format = "yymmdd"),

                      textInput(inputId = "batch_id",
                                label = "Batch ID (PB######)"),
                      
                      checkboxGroupInput(inputId = "input_su_checkboxes",
                                         label = "Select Source SU units",
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
                      
                      selectizeInput(inputId = "strain_id",
                                     label = "Strain ID (PP##)",
                                     choices = NULL,
                                     selected = NULL,
                                     options = list( create = TRUE,
                                                     createOnBlur = TRUE)),
                      
                      selectizeInput(inputId = "operator_name",
                                label = "Operator Name",
                                choices = NULL,
                                selected = NULL,
                                options = list( create = TRUE,
                                                createOnBlur = TRUE)),
                      
                      selectizeInput(inputId = "freezer_name",
                                     label = "Freezer Name",
                                     choices = NULL,
                                     selected = NULL,
                                     options = list( create = TRUE,
                                                     createOnBlur = TRUE)),
                      
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
             # Batch Modification ####
             tabPanel("Batch Modification", 
                      dateInput(inputId = "mod_date_action",
                                label = "Date of Action",
                                format = "yymmdd"),
                      
                      selectInput(inputId = "mod_type",
                                  label = "Type of Change",
                                  c("Removal","Set Balance"),
                                  selected = "Removal"),
                      
                      selectizeInput(inputId = "mod_unique_batch_id",
                                     label = "Unique Batch ID (PB _ bag #)",
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
                      
                      selectizeInput(inputId = "mod_op_group",
                                     label = "Group of Operator (DSP, Experimental, etc)",
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
                                   label = "Submit Change to Database")                          
                      ),
             # Graphics ####
             
             tabPanel("Strain Overview","By Strain, look at current stored batchs. Looks at overall production, 
                      pie chart of usage by group. ",
                      fluidRow(
                          column( width = 4,
                              selectizeInput(inputId = "s_overview_strain_id",
                                     label = "Select Strain to View",
                                     choices = NULL,
                                     selected = NULL)
                                ),
                          column( width = 3,
                               checkboxInput(inputId = "s_showtable",
                                    label = "Show Modification Table",
                                    value = FALSE)
                                ),
                          column( width = 3,
                                  checkboxInput(inputId = "s_show_op",
                                                label = "Strain usage by Operator",
                                                value = FALSE)
                                )
                          ),
                      fluidRow(
                          column(width = 8,
                               plotOutput(outputId = "strain_overview_plot")
                               ),
                          column(width = 4,
                                plotOutput(outputId = "strain_usage_plot"))
                      ),
                      
                      
                      br(),
                      dataTableOutput("s_strainledgertable")
                      ),
             tabPanel("Batch Overview","Explore by Batch, see all modifications, current inventory and ledger"),
             tabPanel("Search","Bring up all information on selected batches")
             
)


#Thanks to this source for this which helped on guide my thinking
#https://deanattali.com/2015/06/14/mimicking-google-form-shiny/



# Shiny App Server ####
server <- function(input, output, session) {
    # Inport statements ####
    inventory_db <- read_csv("./app_inventory.csv") %>% 
        transform(Date_Modified = as.Date(Date_Modified,"%m/%d/%y"))
    
    ledger_db <- read_csv("./app_ledger.csv") %>% 
        transform(Date_Added = as.Date(Date_Added, "%m/%d/%y"))
    
    # Render Tables ####
    observeEvent(input$table_type, {
        output$selected_table <- DT::renderDataTable({
            if(input$table_type == "Current Inventory"){
                inventory_db
            } else if (input$table_type == "Overall Ledger"){
                ledger_db
            }  })
    })
    
    
    # Entry update ####
    observeEvent(input$upload_entry,{
        
        ledger_update_list <- list(input$date_action,
                             "Entry",
                             paste0(toupper(input$batch_id),"_SU",input$input_su_checkboxes),
                             input$bag_number,
                             paste0(toupper(input$batch_id),"_",input$bag_number),
                             input$sample_weight,
                             input$material_type,
                             toupper(input$strain_id),
                             input$operator_name,
                             input$freezer_name,
                             input$shelf_number,
                             "Batch Harvest")
        
        ledger_update_db <- rbind(ledger_db, ledger_update_list) %>% 
            transform(Date_Added = as.character(Date_Added,"%m/%d/%y"))
        
        write.csv(ledger_update_db, file = "./app_ledger.csv", row.names = F)
    
        inventory_update_list <- list(input$date_action,
                                      paste0(toupper(input$batch_id),"_",input$bag_number),
                                      toupper(input$batch_id),
                                      input$bag_number,
                                      toupper(input$strain_id),
                                      input$material_type,
                                      input$sample_weight, 
                                      input$freezer_name,
                                      input$shelf_number)        

        inventory_update_db <- rbind(inventory_db, inventory_update_list) %>% 
            transform(Date_Added = as.character(Date_Added,"%m/%d/%y"))
        
        write.csv(inventory_update_db, file = "./app_inventory.csv", row.names = F)        
        
        session$reload()
    
    })
    
    # Modification Update ####
    observeEvent(input$mod_update_entry,{
        
        if(input$mod_unique_batch_id == ""){
            session$reload()
            #dont love this solution but it works
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
            ledger_update_row$Type = "Removal"
            
        } else if(input$mod_type == 'Set Balance'){
            inventory_update_row$Date_Modified = input$mod_date_action
            inventory_update_row$Weight = input$mod_value_change
            
            inventory_db <- inventory_db %>% subset(.,Unique_Bag_ID != input$mod_unique_batch_id)
            inventory_db <- rbind(inventory_db, inventory_update_row)
            
            ledger_update_row$Weight = input$mod_value_change
            ledger_update_row$Type = "Set Balance"
            
            
        } else{
            inventory_update_row$Date_Modified = input$mod_date_action
            inventory_update_row$Weight = inventory_update_row$Weight - input$mod_value_change
            
            inventory_db <- inventory_db %>% subset(.,Unique_Bag_ID != input$mod_unique_batch_id)
            inventory_db <- rbind(inventory_db, inventory_update_row)
            
            ledger_update_row$Weight = ledger_update_row$Weight - input$mod_value_change
            ledger_update_row$Type = "Removal"
        }
        
        # Update ledger values and bind row to dataframe: happens no matter the change.
        ledger_update_row$Date_Added = input$mod_date_action
        ledger_update_row$Operator = input$mod_operator
        ledger_update_row$Operator_Group = input$mod_op_group
        ledger_update_row$Purpose = input$mod_reason
        
        #drop all of this?
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
    
    # list of all previous strains
    updateSelectizeInput(session, "strain_id", 
                         choices = ledger_db$Strain, 
                         selected = "", server = TRUE)
    
    # list of inventory batches
    updateSelectizeInput(session, "mod_unique_batch_id", 
                         choices = inventory_db$Unique_Bag_ID, 
                         selected = "", server = TRUE)
    # Operator for batch modifications
    updateSelectizeInput(session, "mod_operator", 
                         choices = ledger_db$Operator, 
                         selected = "", server = TRUE)
    # Operator for batch entry
    updateSelectizeInput(session, "operator_name", 
                         choices = ledger_db$Operator, 
                         selected = "", server = TRUE)
    #Freezers for batch entry
    updateSelectizeInput(session, "freezer_name",
                         choices = ledger_db$Freezer_ID,
                         selected ="", server = TRUE)
    #Operator Group batch modifications
    updateSelectizeInput(session, "mod_op_group",
                         choices = ledger_db$Operator_Group,
                         selected ="", server = TRUE)
    
    #Strains for Overview Graph
    updateSelectizeInput(session, "s_overview_strain_id", 
                         choices = ledger_db$Strain, 
                         selected = "", server = TRUE)
    
    # Strain Overview Graphics ####
    
    observeEvent(input$s_overview_strain_id, {
        
        # s_startdate <- ledger_db %>% 
        #     rowwise() %>% 
        #     mutate(., Running_Tally = 
        #                if(Type == "Entry"){
        #                    Weight * 1
        #                } else if(Type =="Removal") {
        #                    Weight * -1
        #                } else {0}) %>% 
        #     ungroup() %>% 
        #     filter(., Strain == input$s_overview_strain_id) %>% 
        #     filter(., Type != "Balance") %>% 
        #     select(., Date_Added) %>% 
        #     slice_min(.,Date_Added, n=1, with_ties = FALSE) %>% 
        #     pull()
        # 
        # s_enddate <-ledger_db %>% 
        #     rowwise() %>% 
        #     mutate(., Running_Tally = 
        #                if(Type == "Entry"){
        #                    Weight * 1
        #                } else if(Type =="Removal") {
        #                    Weight * -1
        #                } else {0}) %>% 
        #     ungroup() %>% 
        #     filter(., Strain == input$s_overview_strain_id) %>% 
        #     filter(., Type != "Balance") %>% 
        #     select(., Date_Added) %>% 
        #     slice_max(.,Date_Added, n=1, with_ties= FALSE) %>% 
        #     pull()

        strain_overview_ggplot <- ledger_db %>% 
            rowwise() %>% 
            mutate(., Running_Tally = 
                       if(Type == "Entry"){
                           Weight * 1
                       } else if(Type =="Removal") {
                           Weight * -1
                       } else {0}) %>% 
            filter(., Type != "Balance") %>% 
            filter(., Strain == input$s_overview_strain_id) %>% 
            ggplot(., aes(x=Date_Added, y = Running_Tally, fill = Type)) +
            geom_col(width = 0.9) +
            scale_fill_brewer(palette = "Dark2") +
            labs(title = paste0("Production History of ",input$s_overview_strain_id),
                 x = "Cell Mass Changes",
                 y = "Date Of Modification")
        
        output$strain_overview_plot <- renderPlot(strain_overview_ggplot)
        
        strain_usage_ggplot <- ledger_db %>% 
            filter(., Type != "Balance") %>% 
            filter(., Strain == input$s_overview_strain_id) %>% 
            filter(., !is.na(Operator_Group)) %>% 
            ggplot(., aes(x= Strain, fill = Operator_Group)) +
            geom_bar(stat = "count", width = 0.9) +
            scale_fill_brewer(palette = "Set2") +
            labs(title = paste0("Usage of ", input$s_overview_strain_id," by Group"),
                 x = input$s_overview_strain_id,
                 y = "Usage by Group")
        
        output$strain_usage_plot <- renderPlot((strain_usage_ggplot))
        
    })
    
    observeEvent(input$s_showtable,{
        output$s_strainledgertable <- DT::renderDataTable({
            if(input$s_showtable){
                ledger_db %>% 
                    rowwise() %>% 
                    filter(., Type != "Balance") %>% 
                    filter(., Strain == input$s_overview_strain_id) %>% 
                    mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_")) %>%
                    ungroup() %>% 
                    select(., c(-PB_Number, -Bag_Number, -Strain, -Freezer_ID, -Shelf_Number)) %>% 
                    select(., c(Date_Added, Type, Unique_Bag_ID, Weight, Material_type, Operator, Operator_Group, Purpose, Location ))
                } 
            })
    })
    
    # Issues ####
        # add by user for strain graph (to top?)
    
    # Notes #### 
        #take the ledger data table
        #graph with line for current amount at that time, and bar for inputs/outputs?
        # filter for date and strain
    # Filter for several strains? Possible?
    # Weight of current batches in the freezer
    # number of batches in the freezer, number of bags in the freezer (in text?)
    # Add table reset option.
    
    # Batch Overview ####
        # select one batch,
    # see successive removals
    # topline freezer thaw numbers
    # 
    
    # Other? ####
}

# Run the application 
shinyApp(ui = ui, server = server)

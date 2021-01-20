# Packages ####
library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(RColorBrewer)

# Shiny App UI ####
ui <- dashboardPage(
    skin = 'black',
    # Header & Sidebar ####
    dashboardHeader(title = "Cell Paste Database"),
    dashboardSidebar( id = "", 
        sidebarMenu(
            menuItem("Inventory & Ledger", tabName = 'inventory_ledger', icon = icon('boxes')),
            menuItem("Batch Input",tabName = "batch_input",icon = icon('plus-circle')),
            menuItem("Batch Modification",tabName = "batch_mod",icon = icon('exchange-alt')),
            menuItem("Strain View",tabName = 'strain_view',icon = icon('binoculars')),
            menuItem("Batch View",tabName = 'batch_view',icon = icon('layer-group')),
            menuItem("Freezer View",tabName = 'freezer_view',icon = icon('snowflake')),
            menuItem("Search",tabName = 'search',icon = icon('search')),
            menuItem("Flex",tabName = 'flex_page',icon = icon('search')),
            menuItem("Settings",tabName = 'settings',icon = icon('cog'))
        )
    ),
    # Body and Items ####
    dashboardBody(
        tabItems(
            # Inventory & Ledger ####
            tabItem(tabName = "inventory_ledger",
                fluidRow(
                column(width = 11,
                    
                    h1("Inventory & Ledger"),
                    selectInput(
                        inputId = "table_type",
                        label = "Select table to View: ",
                        choices = c("Current Inventory", "Overall Ledger"),
                        selected = "Current Inventory"),
                    dataTableOutput("selected_table")
                )
            )), 
            # Batch Input ####
            tabItem(tabName = "batch_input",
                    h1("Input at New Batch"),
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
            tabItem(tabName = "batch_mod",
                    h1("Modify a Batch"),
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
                               label = "Submit Change to Database"),
                    br(),
                    br()
            ),
            # Strain Overview ####
            tabItem(tabName = "strain_view",
                    h1("Strain Information"),
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
                              selectInput(inputId = "s_show_other",
                                            label = "Strain usage by:",
                                            choices = c("Operator_Group","Operator","Location") ,
                                            selected = "Operator_Group")
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
            # Batch Overview ####
            tabItem(tabName = "batch_view",
                    h1("Batch Information"),
                    br(),
                    br(),
                    fluidRow(column(width = 3,
                                  selectizeInput(inputId = "b_batch_id",
                                                 label = "Select Batch to View",
                                                 choices = NULL,
                                                 selected = NULL),
                                  checkboxInput(inputId = "b_limit_extant",
                                                label = "Limit to Extant Batches",
                                                value = TRUE),
                                  checkboxInput(inputId = "b_showtable",
                                                label = "Show History Table",
                                                value = FALSE)),
                    
                           column(width = 3, valueBoxOutput('numb_containers_box')),

                           column(width = 3, valueBoxOutput('total_weight_box')),

                           column(width = 3, valueBoxOutput('numb_freeze_thaw_box'))
                    ),
                    
                    fluidRow(
                      column(width = 12,
                             plotOutput(outputId = "batch_overview_plot")
                      )
                    ),
                    fluidRow(
                     column(width = 12,
                            dataTableOutput("b_batch_history")
                      )
                    )
            ),
            # Freezer View ####
            tabItem(tabName = "freezer_view",
                    h1("Freezer View"),
                    fluidRow(column(4,
                                    selectizeInput(inputId = "freezer_choice",
                                   label = "Select Freezer to View Contents",
                                   choices = NULL,
                                   selected = NULL))),
                    fluidRow(column(8,
                                    dataTableOutput("freezer_choice_table")))
            ),
            # Search ####
            tabItem(tabName = "search",
                    box(title = "Inputs",
                        selectInput(inputId = "flex_timeframe",
                                    label = "View Current inventory or All?",
                                    choices = c("Current Inventory", "Overall Ledger"),
                                    selected = "Current Inventory"),
                        
                        selectInput(inputId = "flex_batch_or_strain",
                                    label = "View by Strain or Batch?",
                                    choices = c("Strain","Batch"),
                                    selected = NULL),
                        
                        selectizeInput(inputId = "flex_ids",
                                       label = NULL,
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = TRUE)
                    ),
                    
                    #box(title = "Stats",dataTableOutput("flex_summary")),
                    box(title = "Tester",textOutput("test_text")),
                    
                    # tabBox(title = "Run Chart",
                    #        side = "right",
                    #        id = "run_chart",
                    #        tabPanel("Table",dataTableOutput("flex_table") ),
                    #        tabPanel("Graph",plotlyOutput("flex_plot")) 
                    # ),
            ),
            # FlexPage ####
            tabItem(tabName = "flex_page",
                    fluidRow(
                        box(title = "Inputs",
                            width = 4,
                            selectInput(
                                inputId = "flex_database",
                                label = "View Current or All Information?",
                                choices = c("Current Inventory" = "inventory_db",
                                            "Overall Ledger" = "ledger_db"),
                                selected = "Current Inventory"),
                            
                            selectizeInput(
                                inputId = "flex_strain",
                                label = "Select one or more Strains",
                                choices = NULL,
                                selected = NULL,
                                multiple = TRUE),
                            
                            selectizeInput(
                                inputId = "flex_batch",
                                label = "Select one or more Batches",
                                choices = NULL,
                                selected = NULL,
                                multiple = TRUE
                            )
                        ),
                        
                        box(title = "Stats", dataTableOutput("flex_summary"))
                    ),
                    fluidRow(
                        tabBox(
                            width = 11,
                            title = "Run Chart",
                            side = "right",
                            id = "run_chart",
                            tabPanel("Table", dataTableOutput("flex_table")),
                            tabPanel("Graph",plotlyOutput("flex_plot"))
                        )
                    )
            ),
            
            # Settings ####
            tabItem(tabName = 'settings',
                    h1("Settings"),
                    h3("Database Reset"),
                
                    "Resetting the database will create a 'balance' entry in the Ledger Database.",
                    br(),
                    "Inventory database will be saved as 'Inventory_Archieve_DATE'.",
                    br(),
                    "If the Ledger is also reset, a similar file will be created.",
                    br(),
                    "All new files will exist in the same directory as the app & old databases.",
                    br(),
                    "Reimporting old databases is not supported.",
                    br(),
                    
                    checkboxInput(inputId = 'ledger_reset',
                                  label = 'Include Ledger in reset?',
                                  value = FALSE),
                    checkboxInput(inputId = 'reset_confirmation',
                                  label = 'Check to confirm reset of Inventory (and Ledger)',
                                  value = FALSE),
                    actionButton(inputId = "reset_databases",
                                 label = "Reset Database(s)"))
        )
    )
)
    
#Thanks to this source which helped guide my thinking
#https://deanattali.com/2015/06/14/mimicking-google-form-shiny/



# Shiny App Server ####
server <- function(input, output, session) {
    # Import statements ####
    inventory_db <- read_csv("./app_inventory.csv") %>% 
        transform(Date_Modified = as.Date(Date_Modified,"%m/%d/%y"))
    
    
    ledger_db <- read_csv("./app_ledger.csv") %>% 
        transform(Date_Added = as.Date(Date_Added, "%m/%d/%y"))
    
    # Render Inventory and Ledger ####
    observeEvent(input$table_type, {
        output$selected_table <- DT::renderDataTable(options = list(autoWidth = FALSE),{
            if(input$table_type == "Current Inventory"){
                inventory_db
            } else if (input$table_type == "Overall Ledger"){
                ledger_db
            }  }
            )
    })
    
    
    # Entry update ####
    observeEvent(input$upload_entry,{
        
        formatted_PB <- if(!is.null(input_su_checkbox)){
            paste0(toupper(input$batch_id),"_SU",input$input_su_checkboxes)
        } else {toupper(input$batch_id)}
        
        ledger_update_list <- list(input$date_action,
                             "Entry",
                             formatted_PB,
                             input$bag_number,
                             paste0(formatted_PB,"_",input$bag_number),
                             input$sample_weight,
                             input$material_type,
                             toupper(input$strain_id),
                             input$operator_name,
                             "Harvest",
                             input$freezer_name,
                             input$shelf_number,
                             "Batch Harvest")
        
        ledger_update_db <- rbind(ledger_db, ledger_update_list) %>% 
            transform(Date_Added = as.character(Date_Added,"%m/%d/%y"))
        
        write.csv(ledger_update_db, file = "./app_ledger.csv", row.names = F, na = "")
    
        inventory_update_list <- list(input$date_action,
                                      paste0(formatted_PB,"_",input$bag_number),
                                      toupper(input$batch_id),
                                      input$bag_number,
                                      toupper(input$strain_id),
                                      input$material_type,
                                      input$sample_weight, 
                                      input$freezer_name,
                                      input$shelf_number)

        inventory_update_db <- rbind(inventory_db, inventory_update_list) %>% 
            transform(Date_Modified = as.character(Date_Modified,"%m/%d/%y"))
        
        write.csv(inventory_update_db, file = "./app_inventory.csv", row.names = F, na = "")        
        
        session$reload()
    
    })
    
    # Modification Update ####
    observeEvent(input$mod_update_entry,{
        
        if(input$mod_unique_batch_id == ""){
            session$reload()
            #don't love this solution but it works
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
    #Batches for Batches Page, dependent on 
    observeEvent(input$b_limit_extant,{
        updateSelectizeInput(session, "b_batch_id",
                          choices = if(input$b_limit_extant == FALSE){
                              ledger_db$PB_Number
                          }else if(input$b_limit_extant == TRUE){
                              inventory_db$PB_Number},
                          selected = "")
    })
    #Freezer View Page
    updateSelectizeInput(session, 'freezer_choice',
                         choices = inventory_db$Freezer_ID,
                         selected = "", server = TRUE)
    
    #Flex selection
    observeEvent(c(input$flex_timeframe, input$flex_batch_or_strain) ,{
        updateSelectizeInput(session, 'flex_ids',
                             choices = if(input$flex_batch_or_strain == "Batch"){
                                 if(input$flex_timeframe == "Current Inventory"){
                                     inventory_db$PB_Number
                                 } else if(input$flex_timeframe == "Overall Ledger"){
                                     ledger_db$PB_Number
                                 }
                             } else if(input$flex_batch_or_strain == "Strain"){
                                 if(input$flex_timeframe == "Current Inventory"){
                                     inventory_db$Strain
                                 } else if(input$flex_timeframe == "Overall Ledger"){
                                     ledger_db$Strain
                                 }
                             } else{""},
                             label = paste0("Select ",input$flex_batch_or_strain))
    })
    

    # Update Flex Page Selections
    # working version: commented out so batches of different strains can be selected.
    # observeEvent(input$flex_batch, {
    #     if(is.null(input$flex_strain)){
    #         updateSelectizeInput(session, "flex_strain",
    #                              choices = get(input$flex_database) %>%
    #                                  filter(.,if(!is.null(input$flex_batch)){PB_Number == input$flex_batch
    #                                      } else {!is.na(Strain)}) %>%
    #                                  pull(Strain),
    #                              selected = "", server = TRUE)
    #     }
    # })
    
    
    
    observeEvent(input$flex_strain,{
        if(is.null(input$flex_batch)){
            updateSelectizeInput(session, "flex_batch",
                                 choices = get(input$flex_database) %>%
                                     filter(., if(!is.null(input$flex_strain)){Strain == input$flex_strain
                                         } else {!is.na(PB_Number)}) %>%
                                     pull(PB_Number),
                                 selected = "", server = TRUE)
        }
    })
    observeEvent(input$flex_database,{
        updateSelectizeInput(session, "flex_strain",
                             choices = get(input$flex_database)$Strain,
                             selected = "", server = TRUE)
        updateSelectizeInput(session, "flex_batch",
                             choices = get(input$flex_database)$PB_Number,
                             selected = "", server = TRUE)
    })

    
    # Strain Overview Graphics ####
    
    observeEvent(c(input$s_overview_strain_id, input$s_show_other), {

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
            mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_")) %>%
            ggplot(., aes(x= Strain, fill = get(input$s_show_other))) +
            geom_bar(stat = "count", width = 0.5) +
            scale_fill_brewer(palette = "Set2") +
            labs(title = paste0("Usage of ", input$s_overview_strain_id," by ", input$s_show_other),
                 x = input$s_overview_strain_id,
                 y = paste0("Usage by ", input$s_show_other))
        
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
                    select(., c(Date_Added, Type, Unique_Bag_ID, 
                                Weight, Material_type, Operator, Operator_Group, Purpose, Location ))
                } 
            })
    })
    
    # Batch Overview Graphics ####
    observeEvent(input$b_batch_id, {
        
        output$numb_containers_box <- renderValueBox({
            batch_numb_containers <- ledger_db %>%
                group_by(., PB_Number) %>%
                summarise(., count = n(), .groups = 'drop') %>%
                filter(., PB_Number == input$b_batch_id) %>%
                pull()
            
            valueBox(value = batch_numb_containers,
                     subtitle = "Number of Containers",
                     icon = icon("box-open"),
                     color = 'navy')
        })
        
        output$total_weight_box <- renderValueBox({
            total_weight <- ledger_db %>% 
                group_by(., PB_Number) %>% 
                filter(., Type == "Entry") %>% 
                summarise(., Total_Weight = sum(), .groups = 'drop') %>% 
                filter(., PB_Number == input$b_batch_id) %>% 
                pull()
            
            valueBox(subtitle = "Total Weight",
                     value = total_weight,
                     icon = icon('weight-hanging'),
                     color = 'light-blue')
        })
                
        output$numb_freeze_thaw_box <- renderValueBox({
            freeze_thaws <- ledger_db %>% 
                group_by(., PB_Number) %>% 
                filter(., Type == "Removal") %>% 
                summarise(., Total_Weight = n(), .groups = 'drop') %>% 
                filter(., PB_Number == input$b_batch_id) %>% 
                pull()
            
            valueBox(subtitle = 'Freeze-Thaws',
                     value = freeze_thaws,
                     icon = icon('snowflake'),
                     color = 'blue')
        })
        
        batch_overview_ggplot <- ledger_db %>% 
            rowwise() %>% 
            mutate(., Running_Tally = 
                       if(Type == "Entry"){
                           Weight * 1
                       } else if(Type =="Removal") {
                           Weight * -1
                       } else {0}) %>% 
            filter(., Type != "Balance") %>% 
            filter(., PB_Number == input$b_batch_id) %>% 
            ggplot(., aes(x=Date_Added, y = Running_Tally, fill = Type)) +
            geom_col(width = 0.9) +
            scale_fill_brewer(palette = "Dark2") +
            labs(title = paste0("Production History of ",input$b_batch_id),
                 x = "Cell Mass Changes",
                 y = "Date Of Modification")
        output$batch_overview_plot <- renderPlot(batch_overview_ggplot)
    })

    observeEvent(input$b_showtable,{
        output$b_batch_history <- DT::renderDataTable({
            if(input$b_showtable){
                ledger_db %>% 
                    rowwise() %>% 
                    filter(., PB_Number == input$b_batch_id) %>% 
                    mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_")) %>%
                    ungroup() %>% 
                    select(., c(-PB_Number, -Bag_Number, -Freezer_ID, -Shelf_Number)) %>% 
                    select(., c(Date_Added, Type, Unique_Bag_ID, Weight, 
                                Operator, Operator_Group, Purpose, Location ))
            } 
        })
    })
    
    # Freezer View Graphics ####
    observeEvent(input$freezer_choice,{
        output$freezer_choice_table <- DT::renderDataTable({
            inventory_db %>% 
                filter(., Freezer_ID == input$freezer_choice) %>% 
                mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_")) %>% 
                select(., c(Date_Modified, Unique_Bag_ID, Location, 
                            Material_type, Weight))
        })
    })
    
    # Search Page ####
    #testing - delete me
    output$test_text <- renderText(c(input$flex_timeframe,input$flex_batch_or_strain))

    observeEvent(input$flex_ids, {
        # make it easy to handle
        choice <- switch (paste0(input$flex_timeframe,"_",input$flex_batch_or_strain),
                          "Current Inventory_Strain" = "I_S",
                          "Overall Ledger_Strain" = "L_S",
                          "Current Inventory_Batch" = "I_B",
                          "Overall Ledger_Batch" = "L_B"
        ) # can I load the inputs into a list? reactiveValuesToList()?

        flex_database <- switch (input$flex_timeframe,
                       "Current Inventory" = inventory_db,
                       "Overall Ledger" = ledger_db
                       )

        flex_group <- input$flex_strain_or_batch #rewrite this


        #render summary table
        output$flex_table <- DT::renderDataTable({
            flex_database %>%
                filter(., Strain %in% input$flex_ids | Batch %in% input$flex_ids) %>%
                mutate(., if(flex_group == "Batch"){"hi"}) %>%
                group_by(., flex_group)


        })

        #summary inventory _ batch
        output$flex_table <- DT::renderDataTable({
            flex_database %>%
                filter(., Batch %in% input$flex_ids)


        })

        freeze_thaws <- ledger_db %>%
            group_by(., PB_Number) %>%
            filter(., Type == "Removal") %>%
            summarise(., Total_Weight = n(), .groups = 'drop') %>%
            filter(., PB_Number == input$b_batch_id) %>%
            pull()

        
        # observe tab choice
            # render run chart
            # render run table 
    })
    
    # Flex Page Graphics ####
    
    
    observeEvent(c(input$flex_strain, input$flex_batch),{
        
        if(input$flex_database == "inventory_db"){
            
            output$flex_table <- DT::renderDataTable(
                options = list(autoWidth = FALSE),
                {
                get(input$flex_database) %>% 
                    filter(.,if(!is.null(input$flex_strain)){ Strain %in% input$flex_strain}else{!is.na(Strain)},
                           if(!is.null(input$flex_batch)){PB_Number %in% input$flex_batch}else{!is.na(PB_Number)}) %>% 
                    mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" )) %>% 
                    select(., Unique_Bag_ID,Date_Modified, Weight, Material_type, Location)
                }
            )
            
            #inventory_by_______
            if(is.null(input$flex_batch)){
                # Inventory_by_Strain
                output$flex_summary <- DT::renderDataTable( 
                    get(input$flex_database) %>% 
                        filter(., Strain %in% input$flex_strain) %>% 
                        group_by(., Strain) %>% 
                        mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" ) ) %>%
                        summarise(., 
                                  `Number of Batches` = length(unique(PB_Number)),
                                  `Number of Bags` = length(unique(Unique_Bag_ID)),
                                `Total Weight` = sum(Weight), 
                                `Freezer Locations` = list(unique(Location)),
                                .groups = 'drop') %>% 
                        mutate(.,across(.cols = everything(), as.character),
                                `Freezer Locations` = gsub("c(","", `Freezer Locations`, fixed = TRUE),
                               `Freezer Locations` = gsub(")","", `Freezer Locations`, fixed = TRUE)) %>% 
                        pivot_longer(., cols = everything(), names_to = "Statistic", values_to = "Value"),
                    rownames = FALSE
                    
                )
                        
            } else{
                # Inventory_by_batch
                output$flex_summary <- DT::renderDataTable( 
                    get(input$flex_database) %>% 
                        filter(., PB_Number %in% input$flex_batch) %>% 
                        mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" )) %>% 
                        group_by(., Unique_Bag_ID) %>% 
                        mutate(.,`Number of Freeze-Thaws` = sum(ledger_db[PB_Number %in% input$flex_batch]$Type == "Removal")) %>% 
                        summarise(., 
                                  `Strain` = unique(Strain)[which.max(tabulate(match(Strain, unique(Strain))))],
                                  `Number of Batches` = length(unique(PB_Number)),
                                  `Number of Bags` = length(unique(Unique_Bag_ID)),
                                  `Total Weight` = sum(Weight), 
                                  `Freezer Locations` = unique(Location),
                                  `Number of Freeze-Thaws` = mean(`Number of Freeze-Thaws`),
                                  .groups = 'drop') %>% 
                        mutate(., across(.cols = everything(), as.character),
                               `Freezer Locations` = gsub("c(","", `Freezer Locations`,fixed = TRUE),
                               `Freezer Locations` = gsub(")","", `Freezer Locations`,fixed = TRUE)) %>% 
                        pivot_longer(., cols = -c(Unique_Bag_ID), names_to = "Statistics", values_to = "Values") %>% 
                        pivot_wider(., names_from = Unique_Bag_ID, values_from = Values),
                    rownames = FALSE
                )
            }
            
            
            fig_flex_strain <-  get(input$flex_database) %>%
                rowwise() %>%
                filter(.,if(!is.null(input$flex_strain)){ Strain %in% input$flex_strain}else{!is.na(Strain)},
                       if(!is.null(input$flex_batch)){PB_Number %in% input$flex_batch}else{!is.na(PB_Number)}) %>% 
                plot_ly(., x = ~PB_Number, 
                        y = ~Weight, 
                        color = ~as.factor(Bag_Number), 
                        colors = brewer.pal(n = length(unique(get(input$flex_database)$Bag_Number)),name = "Set1"), 
                        type = 'bar') %>% 
                layout(yaxis = list(title = "Weight of Material (g)"), barmode = 'stack')
            
            output$flex_plot <- renderPlotly(fig_flex_strain)

        
            
            
        } else if(input$flex_database == "ledger_db"){
            
            output$flex_table <- DT::renderDataTable(
                get(input$flex_database) %>% 
                    filter(.,if(!is.null(input$flex_strain)){ Strain %in% input$flex_strain}else{!is.na(Strain)},
                           if(!is.null(input$flex_batch)){PB_Number %in% input$flex_batch}else{!is.na(PB_Number)}) %>% 
                    mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" )) %>% 
                    select(., Unique_Bag_ID, Strain, Type, Weight, Material_type, Location, Operator, Operator_Group, Purpose)
                
            )
            # ledger_by________
            if(is.null(input$flex_batch)){ #fix table output, can this be batched into one statement?
                #ledger_by_strain
                output$flex_summary <- DT::renderDataTable( 
                    get(input$flex_database) %>% 
                        filter(., Strain %in% input$flex_strain) %>% 
                        group_by(., Strain) %>% 
                        mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" )) %>% 
                        summarise(.,
                                  `Strain` = unique(Strain)[which.max(tabulate(match(Strain, unique(Strain))))],
                                  `Number of Batches Produced` = length(unique(PB_Number[Type == 'Removal'])),
                                  `Total Weight Produced` = sum(Weight[Type == 'Entry']), 
                                  `Group Using Most` = unique(Operator_Group[Type == 'Removal'])[which.max(tabulate(match(Operator_Group, unique(Operator_Group))))],
                                  `Operator Using Most` = unique(Operator[Type == 'Removal'])[which.max(tabulate(match(Operator, unique(Operator))))],
                                  .groups = 'drop') %>% 
                        mutate(., across(.cols = everything(), as.character)) %>% 
                        pivot_longer(., cols = everything(), names_to = "Statistic", values_to = "Value"),
                    rownames = FALSE
                    
                )
                
                
            } else{ #fix the table output
                #ledger_by_batch
                output$flex_summary <- DT::renderDataTable( 
                    get(input$flex_database) %>% 
                        filter(., PB_Number %in% input$flex_batch) %>% 
                        mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" )) %>% 
                        group_by(., Unique_Bag_ID) %>% 
                        mutate(.,`Number of Freeze-Thaws` = sum(ledger_db[PB_Number %in% input$flex_batch]$Type == "Removal")) %>% 
                        summarise(., 
                                  `Total Weight Produced` = sum(Weight[Type == 'Entry']),  
                                  `Total Freeze-Thaws` = mean(`Number of Freeze-Thaws`),
                                  `Group Using Most` = unique(Operator_Group[Type == 'Removal'])[which.max(tabulate(match(Operator_Group, unique(Operator_Group))))],
                                  `Operator Using Most` = unique(Operator[Type == 'Removal'])[which.max(tabulate(match(Operator, unique(Operator))))],
                                  .groups = 'drop') %>% 
                        mutate(., across(.cols = everything(), as.character)) %>% 
                        pivot_longer(., cols = -c(Unique_Bag_ID), names_to = "Statistics", values_to = "Values") %>% 
                        pivot_wider(., names_from = Unique_Bag_ID, values_from = Values), 
                    rownames = FALSE
                )
                
            }
            
            
            fig_flex_batch <- get(input$flex_database) %>% 
                rowwise() %>% 
                mutate(., Running_Tally = 
                           if(Type == "Entry"){
                               Weight * 1
                           } else if(Type =="Removal") {
                               Weight * -1
                           } else {0}) %>% 
                filter(., Type != "Balance") %>% 
                filter(.,if(!is.null(input$flex_strain)){ Strain %in% input$flex_strain}else{!is.na(Strain)},
                       if(!is.null(input$flex_batch)){PB_Number %in% input$flex_batch}else{!is.na(PB_Number)}) %>% 
                plot_ly(., x = ~Date_Added,
                        y = ~Running_Tally,
                        color = ~Type,
                        colors = brewer.pal(n = length(unique(get(input$flex_database)$Bag_Number)),name = "Dark2"),
                        type = 'bar') %>%  
                layout(yaxis = list(title = "Weight of Modification"))
            
            output$flex_plot <- renderPlotly(fig_flex_batch)
            
        }  
    })
    
    
    
    # Setting Options ####
    observeEvent(input$reset_databases,{
        if(input$reset_confirmation){
            #Create data frame to hold modified inventory_db
            inventory_reset_holder <- inventory_db %>% 
                mutate(.,Date_Added = Date_Modified, .keep = 'unused') %>% 
                mutate(.,Type = "Balance",
                       Operator = "Reset Admin",
                       Operator_Group = "Reset Admin",
                       Purpose = "Record of Inventory batches before reset") %>% 
                select(., Date_Added, Type, PB_Number, Bag_Number,
                       Unique_Bag_ID, Weight, Material_Type, Strain, Operator,
                       Operator_Group, Freezer_ID, Shelf_Number, Purpose)
            
            #Bind modified inventory to ledger and save to ledger.csv
            ledger_db <- bind_rows(ledger_db, inventory_reset_holder)
            write.csv(ledger_db, file = "./app_ledger.csv", row.names = F)
            
            #create archive_inventory_Date file
            write.csv(inventory_db, file = paste0("./inventory_archive_",Sys.Date(),".csv"), row.names = F)
            
            #Create an empty dataframe, types are only for aesthetic,
            # lost when saved to CSV
            inventory_db_empty <- data.frame(Date_Modified = character(), 
                                             Unique_Bag_ID = character(),
                                             PB_Number = character(), 
                                             Bag_Number = numeric(),
                                             Strain = character(),
                                             Material_Type = character(), 
                                             Weight = numeric(), 
                                             Freezer_ID = character(), 
                                             Shelf_Number = numeric(), 
                                             stringsAsFactors = FALSE)
                
            write.csv(inventory_db_empty, file = "./app_inventory.csv", row.names = F)
            
            # if ledger is reset, save ledger to an archive, create empty DF, overwrite app_ledger.csv
            if(input$ledger_reset){
                write.csv(ledger_db, file = paste0("./ledger_archive_",Sys.Date(),".csv"), row.names = F)
                
                ledger_db_empty <- data.frame(Date_Added = character(), 
                                                 Type = character(), 
                                                 PB_Number = character(), 
                                                 Bag_Number = numeric(),
                                                 Unique_Bag_ID = character(), 
                                                 Weight = numeric(), 
                                                 Material_Type = character(), 
                                                 Strain = character(), 
                                                 Operator = character(),
                                                 Operator_Group = character(), 
                                                 Freezer_ID = character(), 
                                                 Shelf_Number = numeric(), 
                                                 Purpose = character(), 
                                                 stringsAsFactors = FALSE)
                
                write.csv(ledger_db_empty, file = "./app_ledger.csv", row.names = F)
                
            }
            
        }
    })
    
    
    # Spacer ####
    
    
    
    
    # Notes #### 
        #take the ledger data table
        #graph with line for current amount at that time, and bar for inputs/outputs?
        # filter for date and strain
        #Different graphs 
        # https://www.r-graph-gallery.com/time-series.html
    # Filter for several strains? Possible?
    # Weight of current batches in the freezer
    # number of batches in the freezer, number of bags in the freezer (in text?)
    # Add table reset option.
    
    
    # Other? ####
}


# Run the application 
shinyApp(ui = ui, server = server)

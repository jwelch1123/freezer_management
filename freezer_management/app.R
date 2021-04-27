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
    # Includes title, sidebar and icons.
    dashboardHeader(title = "Cell Paste Database"),
    dashboardSidebar( id = "", 
        sidebarMenu(
            menuItem("Viewer",tabName = 'flex_page',icon = icon('search')),
            menuItem("Inventory & Ledger", tabName = 'inventory_ledger', icon = icon('boxes')),
            menuItem("Batch Input",tabName = "batch_input",icon = icon('plus-circle')),
            menuItem("Batch Modification",tabName = "batch_mod",icon = icon('exchange-alt')),
            menuItem("Settings",tabName = 'settings',icon = icon('cog'))
        )
    ),
    # Body & Items ####
    dashboardBody(
        tabItems(
            # Inventory & Ledger ####
            #   Static Tables which are there for reference
            #   table_type switches between the displayed tables 
            #   Tables are rendered under header "Render Static Tables"
            tabItem(tabName = "inventory_ledger",
                fluidRow(
                column(width = 11,
                    
                    h1("Inventory & Ledger"),
                    selectInput(
                        inputId = "table_type",
                        label = "Select table to View: ",
                        choices = c("Current Inventory" = "inventory_db",
                                    "Overall Ledger" = "ledger_db"),
                        selected = "Current Inventory"),
                    dataTableOutput("selected_table")
                )
            )), 
            # Batch Input ####
            #   For users who are submitting a new batch to the database
            #   All relevant pieces of information are collected from the operator
            #   Drop down menu's help with filling in routine information but allow 
            #   new entries. Stored to ledger and inventory when submitted
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
                               min = 1,
                               step = 100),
                    
                    radioButtons(inputId = "material_type",
                               label = "Type of Material",
                               choices = c("Solid","Liquid")),
                    
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
            #   For users who are changing a batch by setting or removing weight
            #   Unique Bag ID is used to pull other information not entered.
            #   Most information entered gets entered into the ledger.
            #   Date, weight, and location are updated in the inventory as well.
            tabItem(tabName = "batch_mod",
                    h1("Modify a Batch"),
                    dateInput(inputId = "mod_date_action",
                            label = "Date of Action (YYMMDD)",
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
                    
                    selectizeInput(inputId = "mod_freezer_name",
                                   label = "Storage Location: Change if needed",
                                   choices = NULL,
                                   selected = NULL,
                                   options = list( create = TRUE,
                                                   createOnBlur = TRUE)),
                    
                    numericInput(inputId = "mod_shelf_number",
                                label = "Shelf Number: Change if needed",
                                value = 1,
                                min = 1),
                    
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
            
            # Viewer ####
            #   This is the generalized viewing page. You can select Inventory or Ledger
            #   This determines the data base used and graphs/tables presented.
            #   You can also select Strains and Batches to filter the viewer
            #   To further filter viewing, you can select other features of the data set
            #   dependent on which is selected, and then filter for values of that data.
            tabItem(tabName = "flex_page",
                    h1("Strain and Batch Viewer"),
                    fluidRow(
                               box(
                                   title = "Inputs",
                                   width = 4,
                                   selectInput(
                                       inputId = "flex_database",
                                       label = "View Current or All Information?",
                                       choices = c("Current Inventory" = "inventory_db",
                                                   "Overall Ledger" = "ledger_db"),
                                       selected = "Current Inventory"
                                   ),
                                   
                                   selectizeInput(
                                       inputId = "flex_strain",
                                       label = "Select one or more Strains",
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = TRUE
                                   ),
                                   
                                   selectizeInput(
                                       inputId = "flex_batch",
                                       label = "Select one or more Batches",
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = TRUE
                                   ),
                                   selectizeInput(
                                     inputId = "flex_filter",
                                     label = "Select Values to Filter By",
                                     choices = NULL,
                                     selected = NULL,
                                     multiple = TRUE
                                   ),
                               ),
                               box(title = "Quick Look",
                                   width = 8,
                                   DT::dataTableOutput("flex_summary")
                        )
                    ),
                    fluidRow(
                        tabBox(
                            width = 12,
                            title = "Information",
                            side = "right",
                            id = "run_chart",
                            tabPanel("Graph",
                                     plotlyOutput("flex_plot")),
                            tabPanel("Table", dataTableOutput("flex_table"))
                            
                        )
                    )
            ),
            
            # Settings ####
            #   This page exists for 1 edge case though may be needed for others later.
            #   The Inventory and ledger can be reset, generally this will only be used for the inventory
            #   To reset the data bases you need to select a checkbox to confirm.
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
                    hr(), 
                    
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
  
    # Import Statements ####
    #   Get inventory and ledger from local directory
    #   Column types are coerced to prevent some errors.
    #   Should include a 'non csv' catcher as that might be a large issue. 
    inventory_db <- read_csv("./app_inventory.csv",
                             col_types = "cccnccncn") %>% 
        transform(Date_Modified = as.Date(Date_Modified,"%m/%d/%Y"))
    
    ledger_db <- read_csv("./app_ledger.csv",
                          col_types = "cccncncccccnc") %>% 
        transform(Date_Added = as.Date(Date_Added, "%m/%d/%Y"))
    
    # Render Static Tables ####
    #   This is the output for the static tables on the "Inventory & Ledger" page
    #   Options are to deal with some issues regarding page width
    observeEvent(input$table_type, {
        output$selected_table <- DT::renderDataTable(
                options = list(autoWidth = FALSE, scrollX = T),{get(input$table_type)}
            )
    })
    
    
    # Input Update ####
    #   This covers the updates to inventory and ledger when a new batch is entered via
    #     the "batch input" page. 
    #   The process is repeated for the ledger and inventory.
    #   First: the PB and SU are concatenated to save some lines of code later
    #   Second: Lists are created for the ledger/inventory of the information provided
    #       and some inferred from the type of activity (using batch input is always an "Entry"
    #       never a removal.)
    #   Third: the list is appended to the existing dataframe, the date column is coerced to prevent
    #       some errors with date formatting in excel.
    #   Fourth: the new dataframe is saved to the pertinent file and the page is refreshed.
    
    observeEvent(input$upload_entry,{
      
        # standardizes and combines PB and SU selections
        formatted_PB <- if(!is.null(input$input_su_checkboxes)){
            paste0(toupper(input$batch_id),
                   "_SU",
                   paste0(input$input_su_checkboxes,collapse = ""))
        } else {toupper(input$batch_id)}
        
        # Ledger update, bind and save
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
            mutate(Date_Added = as.character(Date_Added,"%m/%d/%y"), .keep = "unused" )
        
        write.csv(ledger_update_db, file = "./app_ledger.csv", row.names = F, na = "")
        
        # Inventory update, bind and save
        inventory_update_list <- list(input$date_action,
                                      paste0(formatted_PB,"_",input$bag_number),
                                      formatted_PB,
                                      input$bag_number,
                                      toupper(input$strain_id),
                                      input$material_type,
                                      input$sample_weight, 
                                      input$freezer_name,
                                      input$shelf_number)

        inventory_update_db <- rbind(inventory_db, inventory_update_list) %>% 
            mutate(Date_Modified = as.character(Date_Modified,"%m/%d/%y"), .keep = "unused" )
        
        write.csv(inventory_update_db, file = "./app_inventory.csv", row.names = F, na = "")        
        
        # Session is re-loaded to make it look like something 
        #   happened and refresh views of inventory
        session$reload()
    
    })
    
    # Modification Update ####
    #   Following a similar pattern as the Entry Update section above...
    #   First: there is an attempt to catch invalid unique bag IDs and reload the session to 
    #     prevent erroneous database changes.
    #   Second: The unique or most recent entries for the Unique ID are stored from each database
    #   Third: Consider the type of action taken, if the batch is being removed, the balance set, or a simple removal
    #     different updates are preformed on the row to update.
    #   Fourth: some metadata values are updated: date, operator, group, purpose...
    #   Fifth: new rows are bound to existing dataframe and saved to csv files.
    observeEvent(input$mod_update_entry,{
        
        # Catches empty submissions so they don't get uploaded to database
        if(!(input$mod_unique_batch_id %in% inventory_db$Unique_Bag_ID)){
            session$reload()
        }
        
        # getting most previous entry to infer information.
        inventory_update_row <- (inventory_db %>%  
                            filter(., inventory_db$Unique_Bag_ID == input$mod_unique_batch_id))
        
        ledger_update_row <- (ledger_db %>% 
                            filter(., ledger_db$Unique_Bag_ID == input$mod_unique_batch_id) %>% 
                            filter(., Date_Added == max(Date_Added)))
        
        # If removing, drop row and change ledger value to negative of weight
        # If setting balance, update the inventory values, bind to inventory db, and update ledger weight.
        # If a simple removal, update the inventory values, bind to inventory db, and update ledger weight. 
        if({input$mod_remove_unique_id}){ 
            inventory_db <- inventory_db %>% subset(.,Unique_Bag_ID != input$mod_unique_batch_id)
            ledger_update_row$Weight = 0 - ledger_update_row$Weight
            ledger_update_row$Type = "Removal"
            
        } else if(input$mod_type == 'Set Balance'){
            inventory_update_row$Date_Modified = input$mod_date_action
            inventory_update_row$Weight = input$mod_value_change
            inventory_update_row$Freezer_ID = input$mod_freezer_name
            inventory_update_row$Shelf_Number = input$mod_shelf_number
              
            inventory_db <- inventory_db %>% subset(.,Unique_Bag_ID != input$mod_unique_batch_id)
            inventory_db <- rbind(inventory_db, inventory_update_row)
            
            ledger_update_row$Weight = input$mod_value_change
            ledger_update_row$Type = "Set Balance"
            
        } else{
            inventory_update_row$Date_Modified = input$mod_date_action
            inventory_update_row$Weight = inventory_update_row$Weight - input$mod_value_change
            inventory_update_row$Freezer_ID = input$mod_freezer_name
            inventory_update_row$Shelf_Number = input$mod_shelf_number
            
            
            inventory_db <- inventory_db %>% subset(.,Unique_Bag_ID != input$mod_unique_batch_id)
            inventory_db <- rbind(inventory_db, inventory_update_row)
            
            ledger_update_row$Weight = input$mod_value_change
            ledger_update_row$Type = "Removal"
        }
        
        
        # Update ledger values and bind row to dataframe: happens no matter the change.
        ledger_update_row$Date_Added = input$mod_date_action
        ledger_update_row$Operator = input$mod_operator
        ledger_update_row$Operator_Group = input$mod_op_group
        ledger_update_row$Freezer_ID = input$mod_freezer_name
        ledger_update_row$Shelf_Number = input$mod_shelf_number
        ledger_update_row$Purpose = input$mod_reason
       
        
        ledger_db <- rbind(ledger_db, ledger_update_row)

        # Write inventory and ledger to CSV files
        write.csv(inventory_db, file = "./app_inventory.csv", row.names = F)
        write.csv(ledger_db, file = "./app_ledger.csv", row.names = F)
        
        # session reloaded to give apperence of submission and reload visualizations
        session$reload()
    })
    
    # Selectize Updates ####
    #   This is a list of Selectize updates for the Batch Input/Modification
    #   Also near the bottom are the Viewer page updates which are triggered and
    #     conditional based on other selected values.
    #   The filters refer to the 'other data' which can be selected from a drop down list
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
    
    observeEvent(input$mod_unique_batch_id,{
      #Freezers for batch Modification
      updateSelectizeInput(session,"mod_freezer_name",
                           choices = ledger_db$Freezer_ID,
                           selected = inventory_db %>% 
                             filter(., Unique_Bag_ID == input$mod_unique_batch_id) %>% 
                             pull(Freezer_ID), server = TRUE)
      
      #Freezer Shelf for batch modification
        updateNumericInput(session, "mod_shelf_number", 
                         value = as.numeric(inventory_db %>% 
                           filter(., Unique_Bag_ID == input$mod_unique_batch_id) %>% 
                           pull(Shelf_Number) ))
    })
    
    #Operator Group batch modifications
    updateSelectizeInput(session, "mod_op_group",
                         choices = ledger_db$Operator_Group,
                         selected ="", server = TRUE)
  
    ## Update Viewer Page Selections
    # updates batch/strain when database is changed
    observeEvent(input$flex_database, {
        if (!is.null(input$flex_batch)) {
            if (!(input$flex_batch %in% get(input$flex_database)$PB_Number)) {
                updateSelectizeInput(
                    session,
                    "flex_batch",
                    choices = get(input$flex_database)$PB_Number,
                    selected = "",
                    server = TRUE
                )
            } else(
                updateSelectizeInput(
                    session,
                    "flex_batch",
                    choices = get(input$flex_database)$PB_Number,
                    selected = input$flex_batch,
                    server = TRUE
                )
            )
        }
        if (!is.null(input$flex_strain)) {
            if (!(input$flex_strain %in% get(input$flex_database)$Strain)) {
                updateSelectizeInput(
                    session,
                    "flex_strain",
                    choices = get(input$flex_database)$Strain,
                    selected = "",
                    server = TRUE
                )
                
            } else(
                updateSelectizeInput(
                    session,
                    "flex_strain",
                    choices = get(input$flex_database)$Strain,
                    selected = input$flex_strain,
                    server = TRUE
                )
            ) 
        }
        if(is.null(input$flex_strain) & is.null(input$flex_batch)){
            updateSelectizeInput(
                session,
                "flex_strain",
                choices = get(input$flex_database)$Strain,
                selected = "",
                server = TRUE
            )
            updateSelectizeInput(
                session,
                "flex_batch",
                choices = get(input$flex_database)$PB_Number,
                selected = "",
                server = TRUE
            )
        }
    })
    
    # update Batch when strain is changed
    observeEvent(input$flex_strain, {
        updateSelectizeInput(
            session,
            "flex_batch",
            choices = get(input$flex_database) %>%
                filter(., if (!is.null(input$flex_strain)) {
                    `Strain` %in% input$flex_strain
                } else {
                    !is.na(PB_Number)
                }) %>%
                pull(PB_Number),
            #selected = input$flex_batch,
            selected = "",
            server = TRUE
        )
    }, ignoreNULL = F)
    
    
    # Depending on the database chosen, display the unique values for each column with 
    #  the column name as the header (manually entered). This always nice visualization 
    #  but is sensitive to identical data accross multiple columns.
    observeEvent(input$flex_database, {
      
      if (input$flex_database == "inventory_db") {
        alt_filter_options = list(
          "Material Type" = sort(unique(get(input$flex_database)$Material_Type)),
          "Freezer ID" = sort(unique(get(input$flex_database)$Freezer_ID))
          )

      } else if (input$flex_database == "ledger_db") {
        alt_filter_options = list(
          "Type" = sort(unique(get(input$flex_database)$Type)),
          "Material Type" = sort(unique(get(input$flex_database)$Material_Type)),
          "Operator" = sort(unique(get(input$flex_database)$Operator)),
          "Operator Group" = sort(unique(get(input$flex_database)$Operator_Group)),
          "Freezer ID" = sort(unique(get(input$flex_database)$Freezer_ID))
        )
      }
      
      updateSelectInput(
        session,
        "flex_filter",
        choices = alt_filter_options)
      
      #print(alt_filter_options$`Material Type`)
      #print(names(alt_filter_options))
    })
    
    # Flex Page Graphics ####
    #   This contains the outputs for all the displays on the Viewer page.
    #   First we define a helper function to help our filtering over multiple columns.
    #   Then we observe Strain/Batch/Filter and the database
    #   When if Strain/Batch/Filter change, a filter is applied to the presented data
    #   If database is changed, a different set of graphs is displayed 
    #   The Inventory and Ledger displays follow the same format
    #     The flex_table (same box as graph) is rendered
    #     Then depending of if Strain or batch have values, different statistics are shown
    #       (by strain, by batch, or neither) in the top right box (summary table)
    #     Finally, the graph is rendered, in the inventory it is a bar plot, in ledger
    #       a run chart.
    
    
    # this is a helper function to filter for rows of X which are within input$flex_filter list
    # it is used in many of the filter functions in this app. This implementation is sensitive to identical data in multiple columns.
    match_selection <- function(x) {
      return(if (!any(x %in% input$flex_filter)){T} else{x %in% input$flex_filter})
    }
    
    # two mirrored updates depending on the database being used.
    observeEvent(c(input$flex_filter, input$flex_strain, input$flex_batch, input$flex_database),{

        if(input$flex_database == "inventory_db"){
          
            # Table of batches matching description, behind graph
            output$flex_table <- DT::renderDataTable(
                options = list(autoWidth = FALSE),
                {
                get(input$flex_database) %>% 
                    filter(.,if(!is.null(input$flex_strain)){ Strain %in% input$flex_strain}else{!is.na(Strain)},
                           if(!is.null(input$flex_batch)){PB_Number %in% input$flex_batch}else{!is.na(PB_Number)}
                           ) %>% 
                    filter(., if(!is.null(input$flex_filter)){
                      across(everything(), match_selection)
                      }else{!is.na(Unique_Bag_ID)}) %>% 
                    mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" )) %>% 
                    select(., Unique_Bag_ID,Date_Modified, Weight, Material_Type, Location)
                }
            )
    
            # Summary Table Inventory_by_Strain
            if(!is.null(input$flex_strain)){
                output$flex_summary <- DT::renderDataTable( 
                    get(input$flex_database) %>% 
                        filter(., Strain %in% input$flex_strain) %>% 
                        filter(., if(!is.null(input$flex_filter)){
                          across(everything(), match_selection)
                          }else{!is.na(Unique_Bag_ID)}) %>% 
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
                        group_by(., Strain) %>% 
                        pivot_longer(., cols = -c(Strain), names_to = "Statistic", values_to = "Value") %>% 
                        pivot_wider(.,
                                    id_cols = c(`Statistic`),
                                    names_from = `Strain` ,
                                    values_from = `Value`),
                    rownames = FALSE,
                    options = list(scrollX = T, pageLength = 6)
                )
            } 
            # Summary Table Inventory_by_batch
            if(!is.null(input$flex_batch)){
                
                output$flex_summary <- DT::renderDataTable( 
                    get(input$flex_database) %>% 
                        filter(., PB_Number %in% input$flex_batch) %>%
                        filter(., if(!is.null(input$flex_filter)){
                          across(everything(), match_selection)
                        }else{!is.na(Unique_Bag_ID)}) %>% 
                        mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" )) %>% 
                        group_by(., Unique_Bag_ID) %>% 
                        summarise(., 
                                  `Strain` = unique(Strain)[which.max(tabulate(match(Strain, unique(Strain))))],
                                  `Number of Batches` = length(unique(PB_Number)),
                                  `Number of Bags` = length(unique(Unique_Bag_ID)),
                                  `Total Weight` = sum(Weight), 
                                  `Freezer Locations` = unique(Location),
                                  .groups = 'drop') %>% 
                        mutate(., across(.cols = everything(), as.character),
                               `Freezer Locations` = gsub("c(","", `Freezer Locations`,fixed = TRUE),
                               `Freezer Locations` = gsub(")","", `Freezer Locations`,fixed = TRUE)) %>% 
                        pivot_longer(., cols = -c(Unique_Bag_ID), names_to = "Statistics", values_to = "Values") %>% 
                        pivot_wider(., names_from = Unique_Bag_ID, values_from = Values),
                    rownames = FALSE,
                    options = list(scrollX = T, pageLength = 6)
                )
            }
            #Summary Table Inventory when no selected strain or batch
            if(is.null(input$flex_batch) & is.null(input$flex_strain)){
                output$flex_summary <- DT::renderDataTable( 
                    get(input$flex_database) %>% 
                      filter(., if(!is.null(input$flex_filter)){
                        across(everything(), match_selection)
                      }else{!is.na(Unique_Bag_ID)}) %>% 
                        group_by(., PB_Number) %>% 
                        summarise(., 
                                  `Strains Available` = unique(Strain),
                                  `Total Weight Avalible` = sum(Weight),
                                  .groups = 'drop'),
                    options = list(scrollX = T, pageLength = 6)
                )
            }
            
            # Graph for inventory based on selected filters
            fig_flex_inventory <-  get(input$flex_database) %>%
                filter(., if(!is.null(input$flex_filter)){
                  across(everything(), match_selection)
                }else{!is.na(Unique_Bag_ID)}) %>% 
                rowwise() %>%
                filter(.,if(!is.null(input$flex_strain)){ Strain %in% input$flex_strain}else{!is.na(Strain)},
                       if(!is.null(input$flex_batch)){PB_Number %in% input$flex_batch}else{!is.na(PB_Number)}) %>% 

                plot_ly(., x = if(is.null(input$flex_batch)) ~Strain else ~PB_Number, 
                        y = ~Weight, 
                        color = if(is.null(input$flex_batch)) ~PB_Number else ~as.factor(Bag_Number), 
                        colors = brewer.pal(8, name = "Set3")[1:length(unique(get(input$flex_database)$Bag_Number))],
                        type = 'bar',
                        marker = list(line = list(width = 1, color = "#000000")),
                        hoverinfo = 'text',
                        text = ~paste('</br> Unique ID: ', Unique_Bag_ID,
                                      '</br> Location: ', Freezer_ID, "_", Shelf_Number,
                                      '</br> Weight: ', Weight,"(g)",
                                      '</br> Material Type: ', Material_Type)
                        ) %>% 
                layout(yaxis = list(title = "Weight of Material (g)"), barmode = 'stack')
            
            output$flex_plot <- renderPlotly({fig_flex_inventory})

        
            
            
        } else if(input$flex_database == "ledger_db"){
            
            # Table of batches matching filters (behind graph)
            output$flex_table <- DT::renderDataTable(
                get(input$flex_database) %>% 
                    filter(.,if(!is.null(input$flex_strain)){ Strain %in% input$flex_strain}else{!is.na(Strain)},
                           if(!is.null(input$flex_batch)){PB_Number %in% input$flex_batch}else{!is.na(PB_Number)}) %>% 
                    filter(., if(!is.null(input$flex_filter)){
                      across(everything(), match_selection)
                    }else{!is.na(Unique_Bag_ID)}) %>% 
                    mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" )) %>% 
                    select(., Unique_Bag_ID, Strain, Type, Weight, Material_Type, Location, Operator, Operator_Group, Purpose)
                
            )
            
            # Summary table of Ledger by Strain
            if(!is.null(input$flex_strain) & is.null(input$flex_batch)){ 
                output$flex_summary <- DT::renderDataTable( 
                    get(input$flex_database) %>% 
                        filter(., Strain %in% input$flex_strain) %>% 
                        filter(., if(!is.null(input$flex_filter)){
                          across(everything(), match_selection)
                        }else{!is.na(Unique_Bag_ID)}) %>% 
                        group_by(., Strain) %>% 
                        mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" )) %>% 
                        summarise(.,
                                  `Strain` = unique(Strain)[which.max(tabulate(match(Strain, unique(Strain))))],
                                  `Number of Batches Produced` = length(unique(PB_Number[Type == 'Removal'])),
                                  `Total Weight Produced` = sum(Weight[Type == 'Entry']), 
                                  `Total Weight Used` = sum(Weight[Type == 'Removal']),
                                  `Group Using Most` = unique(Operator_Group[Type == 'Removal'])[which.max(tabulate(match(Operator_Group[Type == 'Removal'], unique(Operator_Group[Type == 'Removal']))))],
                                  `Operator Using Most` = unique(Operator[Type == 'Removal'])[which.max(tabulate(match(Operator[Type == 'Removal'], unique(Operator[Type == 'Removal']))))],
                                  .groups = 'drop') %>% 
                        mutate(., across(.cols = everything(), as.character)) %>% 
                        group_by(., Strain) %>% 
                        pivot_longer(., 
                                    cols = -c(Strain),
                                    #cols = everything(),
                                    names_to = "Statistic",
                                    values_to = "Value"
                                     ) %>% 
                        pivot_wider(.,
                                    id_cols = c(`Statistic`),
                                    names_from = `Strain` ,
                                    values_from = `Value`),
                    rownames = FALSE,
                    options = list(scrollX = T, pageLength = 6)
                    
                )
                
                
            } 
            # Summary table of Ledger by batch
            if(!is.null(input$flex_batch)){ 
                
                output$flex_summary <- DT::renderDataTable( 
                    get(input$flex_database) %>% 
                        filter(., PB_Number %in% input$flex_batch) %>% 
                        filter(., if(!is.null(input$flex_filter)){
                          across(everything(), match_selection)
                        }else{!is.na(Unique_Bag_ID)}) %>% 
                        mutate(., Location = paste(Freezer_ID,Shelf_Number,sep = "_" )) %>% 
                        group_by(., Unique_Bag_ID) %>% 
                        summarise(., 
                                  `Strain` = unique(`Strain`)[which.max(tabulate(match(`Strain`, unique(`Strain`))))],
                                  `Total Weight Produced` = sum(Weight[Type == 'Entry']),  
                                  `Total Weight Used` = sum(Weight[Type == 'Removal']),
                                  `Weight Remaining` = sum(Weight[Type == 'Entry']) - sum(Weight[Type == 'Removal']),
                                  
                                  `Time in Freezer` = if(sum(Weight[Type == 'Entry']) - sum(Weight[Type == 'Removal']) == 0){
                                    paste(difftime(max(`Date_Added`), min(`Date_Added`), units = c('days')), "days")
                                    } else {paste(difftime(Sys.Date(), min(`Date_Added`), units = c('days')), "days")},
                                  `Location` = unique(`Location`)[which.max(tabulate(match(`Location`, unique(`Location`))))],
                                  `Group Using Most` = unique(Operator_Group[Type == 'Removal'])[which.max(tabulate(match(Operator_Group, unique(Operator_Group))))],
                                  `Operator Using Most` = unique(Operator[Type == 'Removal'])[which.max(tabulate(match(Operator, unique(Operator))))],
                                  .groups = 'drop') %>% 
                        mutate(., across(.cols = everything(), as.character)) %>% 
                        pivot_longer(., cols = -c(Unique_Bag_ID), names_to = "Statistics", values_to = "Values") %>% 
                        pivot_wider(., names_from = Unique_Bag_ID, values_from = Values), 
                    rownames = FALSE,
                    options = list(scrollX = T, pageLength = 6)
                )
                
            }
            # Summary table of ledger when neither batch or strain is selected
            if(is.null(input$flex_batch) & is.null(input$flex_strain)){
                output$flex_summary <- DT::renderDataTable( 
                    get(input$flex_database) %>% 
                        filter(., if(!is.null(input$flex_filter)){
                          across(everything(), match_selection)
                        }else{!is.na(Unique_Bag_ID)}) %>% 
                        group_by(., PB_Number) %>% 
                        summarise(., 
                                  `Strains Available` = unique(Strain),
                                  .groups = 'drop') %>% 
                        ungroup() %>% 
                        mutate(., `Batches Available` = PB_Number) %>% 
                        select(., `Batches Available`, `Strains Available`),
                    options = list(scrollX = T, pageLength = 6)
                )
            }
            
            # Graph of ledger with filters applied.
            fig_flex_ledger <- get(input$flex_database) %>%
                filter(., if(!is.null(input$flex_filter)){
                  across(everything(), match_selection)
                }else{!is.na(Unique_Bag_ID)}) %>%
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
                        colors = rev(brewer.pal(n = 8,name = "Set1")[1:2]),
                        type = 'bar',
                        marker = list(line = list(width = 1, color = "#000000")),
                        hoverinfo = 'text',
                        text = ~paste('</br> Date of Action: ', Date_Added,
                                      '</br> Unique_ID: ', Unique_Bag_ID,
                                      '</br> Freezer Location: ', Freezer_ID, "_", Shelf_Number,
                                      '</br> Operator: ', Operator,
                                      '</br> Weight: ', Weight,"(g)")
                        ) %>%
                layout(yaxis = list(title = "Weight of Modification"),
                       xaxis = list(
                                    rangeselector = list(
                                        buttons = list(
                                            list(
                                            count = 3,
                                            label = "3 mo",
                                            step = "month",
                                            stepmode = "backward"),
                                            list(
                                                count = 6,
                                                label = "6 mo",
                                                step = "month",
                                                stepmode = "backward"),
                                            list(
                                                count = 1,
                                                label = "1 yr",
                                                step = "year",
                                                stepmode = "backward"),
                                            list(
                                                count = 1,
                                                label = "YTD",
                                                step = "year",
                                                stepmode = "todate"),
                                            list(step = "all"))),
                                    rangeslider = list(type = 'date')
                                    ))

          output$flex_plot <- renderPlotly({fig_flex_ledger})
            

            
        }  
    })
    
    # Setting Updates ####
    #   This contains the events for the Settings page.
    #   If the reset_databases button is pressed and the confirmation is selected
    #   First: the inventory is slightly modified so it can be appended to the ledger
    #     this includes renaming and ordering columns, and assining certain values
    #   Second: the modified inventory is bound to the ledger and saved to the ledger
    #   Third: a archieve of the inventory is created and labeled with the current date
    #   Fourth: an empty inventory is created and saved to the app_inventory.csv location
    #   Fifth: if the ledger was selected to be reset, create and save an archive, 
    #     create an empty ledger_database, and save to the app_ledger.csv location.
    #   Finally: the session is reloaded to show a change has been made. 
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
            session$reload()
        }
    })
    
    

} 
# End ####
# Run App #### 
shinyApp(ui = ui, server = server)

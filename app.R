# Load libraries
library(shiny)
library(DT)
library(writexl)
library(rio)
library(dplyr)
library(shinythemes)
library(shinyjs)
library(blockrand)

################################################################################

# Application UI
ui <- fluidPage(
  
  # Enable shinyjs
  useShinyjs(),  
  
  # App theme
  theme = shinytheme("flatly"),
  
  # App title on the browser tab
  tags$head(
    tags$title("RandomTeamsApp"),
    tags$style(
      HTML("
        .custom-title {
          font-family: 'Fredericka the Great', serif;
          text-align: left;
          font-size: 48px;
          font-weight: normal;
          color: black;
          padding: 10px;
          background-color: white;
        }
        
        .custom-subtitle {
          font-family: 'Fredericka the Great', serif;
          text-align: left;
          font-size: 24px;
          font-weight: normal;
          color: black;
          padding: 0px;
          background-color: transparent !important;
          margin-top: 5px;
          margin-bottom: 15px;
        }
          
        .header {
          width: 100%;
          background-color: #f8f9fa;
          padding: 10px;
          text-align: center;
          border-bottom: 1px solid #e7e7e7;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        
        .custom-line-height {
          line-height: 2.5;
        }
        
        .small-math {
          font-size: 0.9em;
        }
        
      ")
    ),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Fredericka+the+Great&display=swap", rel = "stylesheet")
  ),
  
  # Header section at the top of the app
  div(
    class = "header",
    div(
      style = "display: flex; align-items: center; margin: 0;",
      actionButton("reset_app", "Restart App", class = "btn btn-danger", style = "margin: 0;"),
      p(style = "margin: 0; margin-left: 20px;", 'Developer:', a("Raúl Hileno, PhD", href = "https://orcid.org/0000-0003-3447-395X", target = "_blank")),
      p(style = "margin: 0; margin-left: 20px;", 'Project:', a("EasySportsApps", href = "https://github.com/EasySportsApps", target = "_blank")),
      p(style = "margin: 0; margin-left: 20px;", 'License:', a("CC BY-NC-ND 4.0", href = "https://creativecommons.org/licenses/by-nc-nd/4.0/", target = "_blank"))
    )
  ),
  
  # Application title in the interface
  titlePanel(
    div(class = "custom-title", 
        "RandomTeamsApp v1.0")
  ),
  
  # Area to display status messages
  uiOutput("status_message"),
  
  tabsetPanel(
    # Tab to load players from an XLSX, XLS, or CSV file
    tabPanel("Load players",
             sidebarLayout(
               sidebarPanel(
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          fileInput("file", "Open file", accept = c(".xlsx", ".xls", ".csv")),
                          actionButton("load_players_btn", "Load players", class = "btn btn-success")
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          helpText("In this section, you can open a data file from your device in XLSX, XLS, or CSV format."),
                          helpText("After selecting your file, click 'Load players' to add the player information to the data table."),
                          helpText("This step is optional and is useful if you have previously created a data file with the following columns:"),
                          helpText("- ID: the unique identification number assigned to each player."),
                          helpText("- Player: the full name of the player."),
                          helpText("- Sport: the team sport in which the player participates."),
                          helpText("- Team: the name of the team the player is a part of."),
                          helpText("- Category: the age category of the player."),
                          helpText("- Position: the specific position the player holds within the team.")
                 )
               ),
               mainPanel(
                 DTOutput("loaded_player_table")
               )
             )
    ),
    
    # Tab to add new players
    tabPanel("Add players",
             sidebarLayout(
               sidebarPanel(
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          numericInput("player_id_manual", "ID", value = NULL, min = 1, step = 1),
                          textInput("player_name", "Player", ""),
                          selectInput("sport", "Sport", choices = c("Basketball", "Field hockey", "Handball", "Roller hockey", "Rugby", 
                                                                    "Soccer", "Volleyball", "Water polo", "Other")),
                          conditionalPanel(
                            condition = "input.sport == 'Other'",
                            textInput("other_sport", "Enter another sport", "")
                          ),
                          textInput("team_name", "Team", ""),
                          selectInput("age_category", "Category", 
                                      choices = c("Senior", "U-20", "U-18", "U-16", "U-14", "U-12", "U-10", "U-8", "Other"), 
                                      selected = "Senior"),
                          conditionalPanel(
                            condition = "input.age_category == 'Other'",
                            textInput("other_age_category", "Enter another category", "")
                          ),
                          uiOutput("position_ui"),
                          conditionalPanel(
                            condition = "input.position == 'Other'",
                            textInput("other_position", "Enter another position", "")
                          ),
                          actionButton("register", "Add player", class = "btn btn-success")
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          dateInput("create_date", "Select date", value = Sys.Date()),
                          downloadButton("download_xlsx_create", "Download data table")
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          helpText("In this section, you can add a new player to the data table."),
                          helpText("Fill in the required fields and click 'Add player'."),
                          helpText("Ensure that the ID is unique and not already assigned to another player.")
                 )
               ),
               mainPanel(
                 DTOutput("player_table")
               )
             )
    ),
    
    # Tab to modify players
    tabPanel("Modify players",
             sidebarLayout(
               sidebarPanel(
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          numericInput("player_id", "Input player ID to modify", value = NA, min = 1, step = 1)),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          numericInput("player_id_edit", "ID", value = NA, min = 1, step = 1),
                          textInput("player_name_edit", "Player", ""),
                          textInput("sport_edit", "Sport", ""),
                          textInput("team_name_edit", "Team", ""),
                          textInput("age_category_edit", "Category", ""),
                          textInput("position_edit", "Position", ""),
                          actionButton("update", "Modify player", class = "btn btn-success"),
                          actionButton("renumber_ids", "Renumber IDs", class = "btn btn-info")),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          dateInput("modify_date", "Select date", value = Sys.Date()),
                          downloadButton("download_xlsx_update", "Download data table")),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          helpText("In this section, you can modify the details of an existing player."),
                          helpText("Input the ID of the player you wish to modify."),
                          helpText("After making the necessary changes, click 'Modify player' to save."),
                          helpText("You can also click 'Renumber IDs' to automatically renumber player IDs according to their current position in the table.")
                 )
               ),
               mainPanel(
                 DTOutput("edit_player_table")
               )
             )
    ),
    
    # Tab to delete players
    tabPanel("Delete players",  
             sidebarLayout(
               sidebarPanel(
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          textInput("delete_player_id", "ID", value = ""),
                          actionButton("delete", "Delete player/s", class = "btn btn-danger"),
                          actionButton("undo_delete", "Undo delete", class = "btn btn-info")
                 ),
                 
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          dateInput("delete_date", "Select date", value = Sys.Date()),
                          downloadButton("download_xlsx_delete", "Download data table")
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          helpText("In this section, you can delete one or more players by their ID number."),
                          helpText("Simply enter the player ID(s), separating multiple IDs with a comma."),
                          helpText("Once you've entered the IDs, click 'Delete player/s' to remove them from the data table."),
                          helpText("If you accidentally delete one or more unwanted players, you can click 'Undo delete' to restore them.")
                 )
               ),
               mainPanel(
                 DTOutput("delete_player_table")
               )
             )
    ),
    
    # Tab to create randomized groups
    tabPanel("Randomize groups",
             sidebarLayout(
               sidebarPanel(
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          selectInput("filter_sport_random", "Filter players by sport", choices = NULL),
                          selectInput("filter_team_random", "Filter players by team", choices = NULL),
                          selectInput("filter_category_random", "Filter players by category", choices = NULL),
                          selectInput("filter_position_random", "Filter players by position", choices = NULL),
                          numericInput("num_teams_random", "Number of groups to create", value = 1, min = 1, step = 1),
                          actionButton("randomization_button", "Randomize groups", class = "btn btn-success")
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          dateInput("random_groups_date", "Select date", value = Sys.Date()),
                          downloadButton("download_xlsx_random_groups", "Download data table")
                 ),
                 tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
                          helpText("In this section, you can create randomized groups of players based on selected criteria."),
                          helpText("Use the optional filters to specify which players to include in the randomization by sport, team, category, and position."),
                          helpText("For position filtering, 'All positions (unequal distribution)' allows random assignment without balancing positions, while 'All positions (equal distribution)' ensures balanced representation across groups."),
                          helpText("Specify the number of groups you want to create and click 'Randomize groups' to proceed.")
                 )),
               mainPanel(
                 textOutput("num_players_available"),
                 br(),
                 textOutput("name_players_available"),
                 br(),
                 DTOutput("randomization_result")
               )
             )
    )
  )
)

################################################################################

# Application server
server <- function(input, output, session) {
  
  # Initialize reactiveValues to store player data
  players <- reactiveValues(data = data.frame(ID = integer(),
                                              Player = character(),
                                              Sport = character(),
                                              Team = character(),
                                              Category = character(),
                                              Position = character(),
                                              stringsAsFactors = FALSE),
                            temp_data = NULL)
  
  # Display status messages
  output$status_message <- renderUI({
    if (exists("message", envir = session$userData)) {
      message_type <- ifelse(grepl("successfully", session$userData$message), "green", "red")
      div(style = paste("color:", message_type, ";"), session$userData$message)
    }
  })
  
  # Define positions based on the selected sport
  sport_positions <- reactive({
    switch(input$sport,
           "Basketball" = c("Point guard", "Shooting guard", "Small forward", "Power forward", "Center", "Other"),
           "Field hockey" = c("Goalkeeper", 
                              "Left defender", "Right defender",
                              "Left midfielder", "Center midfielder", "Right midfielder",
                              "Left inner", "Right inner",
                              "Left forward", "Center forward", "Right forward", "Other"),
           "Handball" = c("Goalkeeper", "Left back", "Center back", "Right back", "Left wing", "Right wing", "Pivot", "Other"),
           "Roller hockey" = c("Goalie", "Left defender", "Right defender", "Left wing", "Right wing", "Center", "Other"),
           "Rugby" = c("Prop", "Hooker", "Lock", "Flanker", "Number 8", "Scrum half", "Fly half", "Wing", "Centre", "Full back", "Other"),
           "Soccer" = c("Goalkeeper", 
                        "Left back", "Center back", "Right back", 
                        "Defensive midfield", "Central midfield", "Attacking midfield", 
                        "Left wing", "Right wing", 
                        "Striker", "Other"),
           "Volleyball" = c("Setter", "Opposite hitter", "Outside hitter", "Middle blocker", "Libero", "Other"),
           "Water polo" = c("Goalkeeper", "Left flat", "Point", "Right flat", "Left wing", "Right wing", "Hole set", "Other"),
           "Other" = "Other",
           character(0))
  })
  
  # Render the position selector for creating players
  output$position_ui <- renderUI({
    if (is.null(input$sport)) return(NULL)
    selectInput("position", "Position", choices = sport_positions())
  })
  
  ###LOAD PLAYERS TAB###
  
  # Function to validate imported data
  validate_players_data <- function(data) {
    if (!is.data.frame(data)) {
      stop("The imported file is not a data frame.")
    }
    required_columns <- c("ID", "Player", "Sport", "Team", "Category", "Position")
    if (!all(required_columns %in% names(data))) {
      stop(paste("File does not contain the required columns (", paste(required_columns, collapse = ", "), ").", sep = ""))
    }
    data <- data %>%
      mutate(
        ID = as.integer(ID),
        Player = as.character(Player),
        Sport = as.character(Sport),
        Team = as.character(Team),
        Category = as.character(Category),
        Position = as.character(Position)
      )
    if (any(is.na(data$ID))) {
      stop("The ID column contains NA or non-numeric values.")
    }
    if (any(duplicated(data$ID))) {
      stop("The ID column contains duplicate values.")
    }
    return(data)
  }
  
  # Load players button logic
  observeEvent(input$load_players_btn, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        players$temp_data <- import(input$file$datapath)
      } else if (ext == "csv") {
        players$temp_data <- read.table(input$file$datapath, header = TRUE, sep = ifelse(grepl(";", readLines(input$file$datapath, n = 1)), ";", ","), stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file type.")
      }
      if (nrow(players$data) > 0) {
        showModal(modalDialog(
          title = "Data already exists",
          "Choose an option: cancel, remove previous players, or append new players.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("remove_players", "Remove"),
            actionButton("append_players", "Append")
          )
        ))
        return()
      }
      players$temp_data <- validate_players_data(players$temp_data)
      players$data <- players$temp_data  
      players$data <- players$data[order(players$data$ID), ]
      showNotification("Players loaded successfully.", type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Error loading file.", e$message), type = "error", duration = 5)
    })
  })
  
  # Modal button actions
  observeEvent(input$remove_players, {
    previous_data <- players$data
    players$data <- data.frame(ID = integer(),
                               Player = character(),
                               Sport = character(),
                               Team = character(),
                               Category = character(),
                               Position = character(),
                               stringsAsFactors = FALSE)
    removeModal()  
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        players$temp_data <- import(input$file$datapath)
      } else if (ext == "csv") {
        players$temp_data <- read.table(input$file$datapath, header = TRUE, sep = ifelse(grepl(";", readLines(input$file$datapath, n = 1)), ";", ","), stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file type.")
      }
      players$temp_data <- validate_players_data(players$temp_data)
      players$data <- players$temp_data[order(players$temp_data$ID), ]
      showNotification("Players loaded successfully after removal.", type = "message", duration = 5)
    }, error = function(e) {
      players$data <- previous_data
      showNotification(paste("Error loading file after removal.", e$message, "Previous data preserved."), type = "error", duration = 10)
    })
  })
  
  # Append new players to the existing dataset if there are no duplicate IDs
  observeEvent(input$append_players, {
    removeModal()
    if (any(players$data$ID %in% players$temp_data$ID)) {
      showNotification("Cannot append. Duplicate IDs found.", type = "error", duration = 5)
      return()
    }
    players$data <- rbind(players$data, players$temp_data)
    players$data <- players$data[order(players$data$ID), ]
    showNotification("Players appended successfully.", type = "message", duration = 5)
  })
  
  # Display the table of loaded players
  output$loaded_player_table <- renderDT({
    datatable(
      players$data, 
      options = list(
        pageLength = nrow(players$data),
        lengthMenu = list(c(5, 10, 15, 20, 25, -1), c(5, 10, 15, 20, 25, "All")),
        language = list(
          search = "<i class='glyphicon glyphicon-search'></i>"
        ),
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ), 
      rownames = FALSE
    )
  })
  
  ###ADD PLAYERS TAB###
  
  # Logic to add a new player
  observeEvent(input$register, {
    if (is.null(input$player_name) || input$player_name == "" ||
        is.null(input$sport) || input$sport == "" ||
        is.null(input$team_name) || input$team_name == "" ||
        is.null(input$age_category) || input$age_category == "" ||
        is.null(input$player_id_manual) || input$player_id_manual == "") {
      showNotification("Please, fill in the empty fields.", type = "error")
      return()
    }
    current_list <- players$data
    if (any(current_list$ID == input$player_id_manual)) {
      showNotification("The player ID already exists. Please, choose a different ID.", type = "error")
      return()
    }
    sport_value <- if (input$sport == "Other" && input$other_sport != "") {
      input$other_sport 
    } else {
      input$sport 
    }
    category_value <- if (input$age_category == "Other" && input$other_age_category != "") {
      input$other_age_category
    } else {
      input$age_category
    }
    position_value <- if (input$position == "Other" && input$other_position != "") {
      input$other_position
    } else {
      input$position
    }
    new_player <- data.frame(
      ID = input$player_id_manual,
      Player = input$player_name,
      Sport = sport_value,
      Team = input$team_name,
      Category = category_value,
      Position = position_value,
      stringsAsFactors = FALSE
    )
    players$data <- bind_rows(current_list, new_player)
    players$data <- players$data[order(players$data$ID), ]
    showNotification("Player added successfully.", type = "message")
  })
  
  # Display table of added players
  output$player_table <- renderDT({
    datatable(
      players$data, 
      options = list(
        pageLength = nrow(players$data),
        lengthMenu = list(c(5, 10, 15, 20, 25, -1), c(5, 10, 15, 20, 25, "All")),
        language = list(
          search = "<i class='glyphicon glyphicon-search'></i>"
        ),
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ), 
      rownames = FALSE
    )
  })
  
  # Download the player list as an XLSX file from "Add players" with date
  output$download_xlsx_create <- downloadHandler(
    filename = function() {
      selected_date <- format(input$create_date, "%Y_%m_%d")
      current_time <- format(Sys.time(), "%H_%M")
      paste0("players_list_", selected_date, "_", current_time, ".xlsx")
    },
    content = function(file) {
      write_xlsx(players$data, path = file)
    }
  )
  
  ###MODIFY PLAYERS TAB###
  
  # Update an existing player's data by ID with confirmation
  observeEvent(input$update, {
    if (is.null(input$player_id_edit) || input$player_id_edit == "" ||
        is.null(input$player_name_edit) || input$player_name_edit == "" ||
        is.null(input$sport_edit) || input$sport_edit == "" ||
        is.null(input$team_name_edit) || input$team_name_edit == "" ||
        is.null(input$age_category_edit) || input$age_category_edit == "" ||
        is.null(input$position_edit) || input$position_edit == "") {
      showNotification("Please, fill in the empty fields.", type = "error")
      return()
    }
    current_list <- players$data
    player_id <- input$player_id
    if (!player_id %in% current_list$ID) {
      showNotification("No player found with the provided ID. Please, select a valid ID.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = "Confirm update",
      paste("Are you sure you want to update the information for player with ID", player_id, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_update", "Yes, update")
      )
    ))
  })
  
  # Update input fields according to selected ID
  observeEvent(input$player_id, {
    req(input$player_id)
    current_list <- players$data
    player_id <- input$player_id
    if (player_id %in% current_list$ID) {
      player_data <- current_list[current_list$ID == player_id, ]
      updateNumericInput(session, "player_id_edit", value = player_data$ID)
      updateTextInput(session, "player_name_edit", value = player_data$Player)
      updateTextInput(session, "sport_edit", value = player_data$Sport)
      updateTextInput(session, "team_name_edit", value = player_data$Team)
      updateTextInput(session, "age_category_edit", value = player_data$Category)
      updateTextInput(session, "position_edit", value = player_data$Position)
    } else {
      updateNumericInput(session, "player_id_edit", value = NA)
      updateTextInput(session, "player_name_edit", value = "")
      updateTextInput(session, "sport_edit", value = "")
      updateTextInput(session, "team_name_edit", value = "")
      updateTextInput(session, "age_category_edit", value = "")
      updateTextInput(session, "position_edit", value = "")
      showNotification("No player found with the provided ID.", type = "error")
    }
  })
  
  # Confirm player update
  observeEvent(input$confirm_update, {
    req(input$player_id, input$player_id_edit, input$player_name_edit, input$sport_edit, input$team_name_edit, input$age_category_edit, input$position_edit)
    current_list <- players$data
    player_id <- input$player_id
    current_list[current_list$ID == player_id, ] <- c(
      input$player_id_edit, 
      input$player_name_edit, 
      input$sport_edit, 
      input$team_name_edit, 
      input$age_category_edit, 
      input$position_edit
    )
    players$data <- current_list
    showNotification(paste("Player with ID", player_id, "updated successfully."), type = "message")
    removeModal() 
  })
  
  # Event to renumber IDs
  observeEvent(input$renumber_ids, {
    showModal(modalDialog(
      title = "Confirm renumbering",
      "Are you sure you want to renumber all player IDs according to the current order in the table?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_renumber", "Yes, renumber")
      )
    ))
  })
  
  # Confirm ID renumbering
  observeEvent(input$confirm_renumber, {
    current_list <- players$data
    if (nrow(current_list) > 0) {
      current_order <- input$edit_player_table_rows_current
      players$data <- current_list[current_order, ] %>%
        mutate(ID = row_number())
      showNotification("Player IDs have been successfully renumbered.", type = "message")
    } else {
      showNotification("No players to renumber.", type = "error")
    }
    removeModal()
  })
  
  # Download the player list as an XLSX file from "Modify players" tab with date
  output$download_xlsx_update <- downloadHandler(
    filename = function() {
      selected_date <- format(input$modify_date, "%Y_%m_%d")
      current_time <- format(Sys.time(), "%H_%M")
      paste0("players_list_", selected_date, "_", current_time, ".xlsx")
    },
    content = function(file) {
      write_xlsx(players$data, path = file)
    }
  )
  
  # Display player table for the modify players tab
  output$edit_player_table <- renderDT({
    datatable(
      players$data, 
      options = list(
        pageLength = nrow(players$data),
        lengthMenu = list(c(5, 10, 15, 20, 25, -1), c(5, 10, 15, 20, 25, "All")),
        language = list(
          search = "<i class='glyphicon glyphicon-search'></i>"
        ),
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ), 
      rownames = FALSE
    )
  })
  
  ### DELETE PLAYERS TAB ###
  
  # List to store deleted players for undo functionality
  deleted_players <- reactiveVal(list())
  
  # Event to delete players
  observeEvent(input$delete, {
    if (is.null(input$delete_player_id) || input$delete_player_id == "") {
      showNotification("Please, enter a player ID to delete.", type = "error")
      return()
    }
    ids_to_delete <- unlist(strsplit(input$delete_player_id, ","))
    ids_to_delete <- as.numeric(trimws(ids_to_delete))
    existing_players <- players$data[players$data$ID %in% ids_to_delete, ]
    if (nrow(existing_players) == 0) {
      showNotification("No players found with the provided IDs.", type = "error")
    } else {
      showModal(modalDialog(
        title = "Confirm deletion",
        paste("Are you sure you want to delete the following players:", 
              paste(existing_players$ID, collapse = ", "), "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete", "Yes, delete")
        )
      ))
    }
  })
  
  # Confirm player deletion
  observeEvent(input$confirm_delete, {
    req(input$delete_player_id)
    ids_to_delete <- unlist(strsplit(input$delete_player_id, ","))
    ids_to_delete <- as.numeric(trimws(ids_to_delete))
    current_deleted <- deleted_players()
    deleted_players(c(list(players$data[players$data$ID %in% ids_to_delete, ]), current_deleted))
    players$data <- players$data[!players$data$ID %in% ids_to_delete, ]
    showNotification("Players deleted successfully.", type = "message")
    removeModal()
  })
  
  # Button to undo deletion
  observeEvent(input$undo_delete, {
    if (length(deleted_players()) > 0) {
      last_deleted <- deleted_players()[[1]]
      players$data <- rbind(players$data, last_deleted)
      deleted_players(deleted_players()[-1])
      players$data <- players$data[order(players$data$ID), ]
      showNotification("Player restored successfully.", type = "message")
    } else {
      showNotification("No players to undo deletion.", type = "error")
    }
  })
  
  # Download the player list in an XLSX file from the "Delete players" tab with the date
  output$download_xlsx_delete <- downloadHandler(
    filename = function() {
      selected_date <- format(input$delete_date, "%Y_%m_%d")
      current_time <- format(Sys.time(), "%H_%M")
      paste0("players_list_", selected_date, "_", current_time, ".xlsx")
    },
    content = function(file) {
      write_xlsx(players$data, path = file) 
    }
  )
  
  # Display the player table for the delete players tab
  output$delete_player_table <- renderDT({
    datatable(
      players$data, 
      options = list(
        pageLength = nrow(players$data),
        lengthMenu = list(c(5, 10, 15, 20, 25, -1), c(5, 10, 15, 20, 25, "All")),
        language = list(
          search = "<i class='glyphicon glyphicon-search'></i>"
        ),
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ), 
      rownames = FALSE
    )
  })
  
  ###RANDOMIZE GROUPS TAB###
  
  # Create a reactive variable to store the combined teams
  randomized_teams <- reactiveVal()
  
  # Dynamically update filter selectors
  observe({
    updateSelectInput(session, "filter_sport_random", 
                      choices = c("All sports", sort(unique(players$data$Sport))))
    updateSelectInput(session, "filter_team_random", 
                      choices = c("All teams", sort(unique(players$data$Team))))
    updateSelectInput(session, "filter_category_random", 
                      choices = c("All categories", sort(unique(players$data$Category))))
    updateSelectInput(session, "filter_position_random", 
                      choices = c("All positions (unequal distribution)", 
                                  "All positions (equal distribution)", 
                                  sort(unique(players$data$Position))))
  })
  
  # Display the number of players available for randomization
  output$num_players_available <- renderText({
    filtered_data <- players$data
    if (input$filter_sport_random != "All sports") {
      filtered_data <- filtered_data[filtered_data$Sport == input$filter_sport_random, ]
    }
    if (input$filter_team_random != "All teams") {
      filtered_data <- filtered_data[filtered_data$Team == input$filter_team_random, ]
    }
    if (input$filter_category_random != "All categories") {
      filtered_data <- filtered_data[filtered_data$Category == input$filter_category_random, ]
    }
    if (input$filter_position_random != "All positions (unequal distribution)") {
      if (input$filter_position_random != "All positions (equal distribution)") {
        filtered_data <- filtered_data[filtered_data$Position == input$filter_position_random, ]
      }
    }
    num_players <- nrow(filtered_data)
    paste("Number of players available for randomization:", num_players)
  })
  
  # Display the list of names of available players
  output$name_players_available <- renderText({
    filtered_data <- players$data
    if (input$filter_sport_random != "All sports") {
      filtered_data <- filtered_data[filtered_data$Sport == input$filter_sport_random, ]
    }
    if (input$filter_team_random != "All teams") {
      filtered_data <- filtered_data[filtered_data$Team == input$filter_team_random, ]
    }
    if (input$filter_category_random != "All categories") {
      filtered_data <- filtered_data[filtered_data$Category == input$filter_category_random, ]
    }
    if (input$filter_position_random == "All positions (unequal distribution)") {
    } else if (input$filter_position_random != "All positions (equal distribution)") {
      filtered_data <- filtered_data[filtered_data$Position == input$filter_position_random, ]
    }
    player_names <- filtered_data$Player
    if (length(player_names) > 0) {
      name_parts <- strsplit(player_names, " ")
      first_names <- sapply(name_parts, `[`, 1)
      last_names <- sapply(name_parts, function(x) if (length(x) > 1) x[length(x)] else "")
      sorted_indices <- order(first_names, last_names)
      sorted_names <- player_names[sorted_indices]
      paste("Name of the players available for randomization:", paste(sorted_names, collapse = ", "))
    } else {
      "Name of the players available for randomization: no name available"
    }
  })
  
  # Logic for randomization
  observeEvent(input$randomization_button, {
    req(players$data)
    filtered_data <- players$data
    if (input$filter_sport_random != "All sports") {
      filtered_data <- filtered_data[filtered_data$Sport == input$filter_sport_random, ]
    }
    if (input$filter_team_random != "All teams") {
      filtered_data <- filtered_data[filtered_data$Team == input$filter_team_random, ]
    }
    if (input$filter_category_random != "All categories") {
      filtered_data <- filtered_data[filtered_data$Category == input$filter_category_random, ]
    }
    if (input$filter_position_random == "All positions (unequal distribution)") {
    } else if (input$filter_position_random != "All positions (equal distribution)") {
      filtered_data <- filtered_data[filtered_data$Position == input$filter_position_random, ]
    }
    num_players <- nrow(filtered_data)
    updateNumericInput(session, "num_teams_random", 
                       min = 1,
                       max = num_players,
                       value = min(input$num_teams_random, num_players))
    if (input$num_teams_random > num_players) {
      showNotification("Number of groups cannot exceed the number of players available for randomization.", type = "error")
      return()
    }
    if (input$num_teams_random < 1) {
      showNotification("Number of groups must be at least 1.", type = "error")
      return()
    } 
    num_teams <- input$num_teams_random
    if (input$filter_position_random == "All positions (equal distribution)") {
      position_list <- split(filtered_data, filtered_data$Position)
      for (position in names(position_list)) {
        if (nrow(position_list[[position]]) < num_teams) {
          showNotification(paste("At least one player from each position is needed to do this type of random."), type = "error")
          return(NULL)
        }
      }
      team_list <- vector("list", num_teams)
      for (i in seq_along(team_list)) {
        team_list[[i]] <- data.frame()
      }
      for (position in names(position_list)) {
        players_for_position <- position_list[[position]]
        player_indices <- sample(1:nrow(players_for_position))
        team_sizes <- rep(floor(nrow(players_for_position) / num_teams), num_teams)
        remainder <- nrow(players_for_position) %% num_teams
        
        if (remainder > 0) {
          extra_teams <- sample(1:num_teams, remainder)
          team_sizes[extra_teams] <- team_sizes[extra_teams] + 1
        }
        index <- 1
        for (i in seq_along(team_sizes)) {
          team_list[[i]] <- rbind(team_list[[i]], players_for_position[player_indices[index:(index + team_sizes[i] - 1)], ])
          index <- index + team_sizes[i]
        }
      }
    } else {
      player_indices <- sample(1:num_players)
      team_sizes <- rep(floor(num_players / num_teams), num_teams)
      remainder <- num_players %% num_teams
      if (remainder > 0) {
        extra_teams <- sample(1:num_teams, remainder)
        team_sizes[extra_teams] <- team_sizes[extra_teams] + 1
      }
      team_list <- split(filtered_data[player_indices, ], rep(1:num_teams, times = team_sizes))
    }
    if (input$filter_position_random == "All positions (equal distribution)") {
      team_list <- lapply(team_list, function(team) {
        team[order(team$Position), ]
      })
    } else {
      team_list <- lapply(team_list, function(team) {
        team[order(team$ID), ]
      })
    }
    output$randomization_result <- renderDT({
      combined_teams <- do.call(rbind, lapply(seq_along(team_list), function(i) {
        team <- team_list[[i]]
        team$`Assigned group` <- paste("Group", i)
        return(team)
      }))
      randomized_teams(combined_teams)
      datatable(combined_teams, 
                options = list(
                  headerCallback = JS(c("function(thead, data, start, end, display){",
                                        "  $('th', thead).css('border-top', '1px solid #b3b3b3');","}")),
                  autoWidth = TRUE,
                  buttons = list(),
                  lengthChange = TRUE,
                  searching = TRUE,
                  info = TRUE,
                  paging = TRUE,
                  pageLength = nrow(combined_teams),
                  lengthMenu = list(c(5, 10, 15, 20, 25, nrow(combined_teams)), 
                                    c(5, 10, 15, 20, 25, "All")),
                  language = list(
                    search = "<i class='glyphicon glyphicon-search'></i>"
                  ),
                  columnDefs = list(list(className = 'dt-left', targets = "_all"))
                  
                ),
                rownames = FALSE)
    })
    showNotification("Randomization completed successfully.", type = "message")
  })
  
  # Observe changes in the filter selectors to update the maximum value of num_teams_random
  observe({
    filtered_data <- players$data
    if (input$filter_sport_random != "All sports") {
      filtered_data <- filtered_data[filtered_data$Sport == input$filter_sport_random, ]
    }
    if (input$filter_team_random != "All teams") {
      filtered_data <- filtered_data[filtered_data$Team == input$filter_team_random, ]
    }
    if (input$filter_category_random != "All categories") {
      filtered_data <- filtered_data[filtered_data$Category == input$filter_category_random, ]
    }
    if (input$filter_position_random == "All positions (unequal distribution)") {
    } else if (input$filter_position_random != "All positions (equal distribution)") {
      filtered_data <- filtered_data[filtered_data$Position == input$filter_position_random, ]
    }
    num_players <- nrow(filtered_data)
    updateNumericInput(session, "num_teams_random", max = num_players)
  })
  
  # Download the randomized groups as an XLSX file from "Randomize groups" with the selected date
  output$download_xlsx_random_groups <- downloadHandler(
    filename = function() {
      selected_date <- format(input$random_groups_date, "%Y_%m_%d")
      current_time <- format(Sys.time(), "%H_%M")
      paste0("groups_randomized_", selected_date, "_", current_time, ".xlsx")
    },
    content = function(file) {
      write_xlsx(randomized_teams(), path = file)
    }
  )
  
  ### RESTART APP ###
  
  # Restart the application
  observeEvent(input$reset_app, {
    showModal(modalDialog(
      title = "Restart the application",
      "Are you sure you want to restart the application? This will delete all players.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_restart", "Yes, restart")
      )
    ))
  })
  
  # Confirm application restart
  observeEvent(input$confirm_restart, {
    players$data <- data.frame(ID = integer(),
                               Player = character(),
                               Sport = character(),
                               Team = character(),
                               Position = character(),
                               stringsAsFactors = FALSE)
    updateNumericInput(session, "player_id_manual", value = "")
    updateTextInput(session, "player_name", value = "")
    updateSelectInput(session, "sport", selected = "Basketball")
    updateTextInput(session, "other_sport", value = "")
    updateTextInput(session, "team_name", value = "")
    updateSelectInput(session, "age_category", selected = "Senior")
    updateTextInput(session, "other_age_category", value = "")
    updateSelectInput(session, "position", selected = NULL)
    updateTextInput(session, "other_position", value = "")
    updateNumericInput(session, "player_id", value = "")
    updateNumericInput(session, "player_id_edit", value = "")
    updateTextInput(session, "player_name_edit", value = "")
    updateTextInput(session, "sport_edit", value = "")
    updateTextInput(session, "team_name_edit", value = "")
    updateTextInput(session, "age_category_edit", value = "")
    updateTextInput(session, "position_edit", value = "")
    updateTextInput(session, "delete_player_id", value = "")
    updateSelectInput(session, "filter_sport_random", selected = NULL)
    updateSelectInput(session, "filter_team_random", selected = NULL)
    updateSelectInput(session, "filter_category_random", selected = NULL)
    updateSelectInput(session, "filter_position_random", selected = NULL)
    updateNumericInput(session, "num_teams_random", value = 1)
    output$randomization_result <- renderDT({
      combined_teams <- data.frame()
      datatable(combined_teams,
                options = list(
                  headerCallback = JS(c("function(thead, data, start, end, display){",
                                        "  $('th', thead).css('border-top', '1px solid #b3b3b3');","}")),
                  autoWidth = TRUE,
                  buttons = list(),
                  lengthChange = TRUE,
                  searching = TRUE,
                  info = TRUE,
                  paging = TRUE,
                  pageLength = nrow(combined_teams),
                  lengthMenu = list(c(5, 10, 15, 20, 25, nrow(combined_teams)), 
                                    c(5, 10, 15, 20, 25, "All")),
                  language = list(
                    search = "<i class='glyphicon glyphicon-search'></i>"
                  ),
                  columnDefs = list(list(className = 'dt-left', targets = "_all"))
                ),
                rownames = FALSE)
    })
    deleted_players(list())
    showNotification("Application has been restarted successfully.", type = "message", duration = 5)
    removeModal()
  })
}

################################################################################

# Run the application
shinyApp(ui = ui, server = server)

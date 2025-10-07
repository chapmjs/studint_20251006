library(shiny)
library(googlesheets4)
library(DT)
library(dplyr)
library(lubridate)

# Authenticate with Google Sheets (see instructions below)
# For deployment, use gs4_auth(cache = ".secrets", email = "your-email@gmail.com")
# For local testing, use gs4_auth() for interactive authentication
gs4_deauth() # Use this line initially to create sheets without auth

# Replace this with your actual Google Sheet URL after creating it
SHEET_URL <- "https://docs.google.com/spreadsheets/d/1LV4ZnSPR60cAJRDf23v9weDGt1BGhRknI54zIJ6q2ys/edit?gid=782361108#gid=782361108"

# Function to initialize Google Sheets if they don't exist
initialize_sheets <- function() {
  tryCatch({
    # Try to read existing sheets
    students <- read_sheet(SHEET_URL, sheet = "Students")
    interactions <- read_sheet(SHEET_URL, sheet = "Interactions")
  }, error = function(e) {
    # If sheets don't exist, create them with headers
    students_init <- data.frame(
      student_id = integer(),
      first_name = character(),
      last_name = character(),
      phone = character(),
      email = character(),
      graduation_month = character(),
      graduation_year = integer(),
      hometown = character(),
      major = character(),
      linkedin_url = character(),
      social_media = character(),
      stringsAsFactors = FALSE
    )
    
    interactions_init <- data.frame(
      interaction_id = integer(),
      student_id = integer(),
      student_name = character(),
      interaction_date = character(),
      interaction_time = character(),
      place = character(),
      notes = character(),
      stringsAsFactors = FALSE
    )
    
    sheet_write(students_init, ss = SHEET_URL, sheet = "Students")
    sheet_write(interactions_init, ss = SHEET_URL, sheet = "Interactions")
  })
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .main-header {
        background-color: #2c3e50;
        color: white;
        padding: 15px;
        margin-bottom: 20px;
      }
      .student-list {
        border-right: 2px solid #ecf0f1;
        height: 80vh;
        overflow-y: auto;
      }
      .student-card {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 10px;
        cursor: pointer;
        transition: background-color 0.2s;
      }
      .student-card:hover {
        background-color: #e9ecef;
      }
      .student-card.active {
        background-color: #3498db;
        color: white;
        border-color: #2980b9;
      }
      .student-details {
        background-color: #ffffff;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 20px;
        margin-bottom: 20px;
      }
      .interaction-card {
        background-color: #f8f9fa;
        border-left: 4px solid #3498db;
        padding: 15px;
        margin-bottom: 15px;
        border-radius: 5px;
      }
      .section-title {
        color: #2c3e50;
        border-bottom: 2px solid #3498db;
        padding-bottom: 10px;
        margin-bottom: 20px;
      }
    "))
  ),
  
  div(class = "main-header",
    h2("Student Interactions Tracker"),
    actionButton("add_student_btn", "➕ Add New Student", 
                 class = "btn-success btn-lg")
  ),
  
  fluidRow(
    # Left Sidebar - Student List
    column(3,
      div(class = "student-list",
        textInput("search_students", "🔍 Search Students", 
                  placeholder = "Type name..."),
        hr(),
        uiOutput("student_list")
      )
    ),
    
    # Right Main Area - Student Details & Interactions
    column(9,
      uiOutput("main_content")
    )
  ),
  
  # Modal for Adding New Student
  bslib::modal_ui(
    id = "add_student_modal",
    title = "Add New Student",
    size = "large",
    textInput("new_first_name", "First Name*"),
    textInput("new_last_name", "Last Name*"),
    textInput("new_phone", "Phone"),
    textInput("new_email", "Email"),
    selectInput("new_grad_month", "Graduation Month", 
                choices = c("", month.name)),
    numericInput("new_grad_year", "Graduation Year", 
                 value = year(Sys.Date()), min = 2020, max = 2035),
    textInput("new_hometown", "Hometown"),
    textInput("new_major", "Major"),
    textInput("new_linkedin", "LinkedIn URL"),
    textInput("new_social", "Instagram/Social Media"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("save_student", "Save Student", class = "btn-primary")
    )
  ),
  
  # Modal for Adding New Interaction
  bslib::modal_ui(
    id = "add_interaction_modal",
    title = "Log New Interaction",
    dateInput("new_int_date", "Date", value = Sys.Date()),
    textInput("new_int_time", "Time", 
              value = format(Sys.time(), "%H:%M"),
              placeholder = "HH:MM"),
    textInput("new_int_place", "Place/Location"),
    textAreaInput("new_int_notes", "Notes", rows = 5,
                  placeholder = "What did you discuss?"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("save_interaction", "Save Interaction", class = "btn-primary")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    students = data.frame(),
    interactions = data.frame(),
    selected_student_id = NULL,
    refresh_trigger = 0
  )
  
  # Load data from Google Sheets
  load_data <- function() {
    tryCatch({
      rv$students <- read_sheet(SHEET_URL, sheet = "Students")
      rv$interactions <- read_sheet(SHEET_URL, sheet = "Interactions")
    }, error = function(e) {
      showNotification("Error loading data from Google Sheets", type = "error")
    })
  }
  
  # Initialize on app start
  observe({
    load_data()
  })
  
  # Refresh data periodically
  observe({
    invalidateLater(30000) # Refresh every 30 seconds
    load_data()
  })
  
  # Filtered student list based on search
  filtered_students <- reactive({
    if (nrow(rv$students) == 0) return(data.frame())
    
    students <- rv$students
    search_term <- tolower(input$search_students)
    
    if (search_term != "") {
      students <- students %>%
        filter(
          grepl(search_term, tolower(first_name)) |
          grepl(search_term, tolower(last_name)) |
          grepl(search_term, tolower(paste(first_name, last_name)))
        )
    }
    
    students %>%
      arrange(last_name, first_name)
  })
  
  # Render student list
  output$student_list <- renderUI({
    students <- filtered_students()
    
    if (nrow(students) == 0) {
      return(div(style = "text-align: center; color: #95a5a6; padding: 20px;",
                 "No students found"))
    }
    
    lapply(1:nrow(students), function(i) {
      student <- students[i, ]
      is_active <- !is.null(rv$selected_student_id) && 
                   rv$selected_student_id == student$student_id
      
      div(
        class = if(is_active) "student-card active" else "student-card",
        onclick = sprintf("Shiny.setInputValue('select_student', %d, {priority: 'event'})", 
                         student$student_id),
        tags$strong(paste(student$first_name, student$last_name)),
        tags$br(),
        tags$small(student$major)
      )
    })
  })
  
  # Handle student selection
  observeEvent(input$select_student, {
    rv$selected_student_id <- input$select_student
  })
  
  # Render main content area
  output$main_content <- renderUI({
    if (is.null(rv$selected_student_id)) {
      return(
        div(style = "text-align: center; padding: 100px; color: #95a5a6;",
          h3("👈 Select a student from the list"),
          p("Or click 'Add New Student' to get started")
        )
      )
    }
    
    student <- rv$students %>% 
      filter(student_id == rv$selected_student_id)
    
    if (nrow(student) == 0) {
      rv$selected_student_id <- NULL
      return(NULL)
    }
    
    student <- student[1, ]
    
    # Get interactions for this student
    student_interactions <- rv$interactions %>%
      filter(student_id == rv$selected_student_id) %>%
      arrange(desc(interaction_date), desc(interaction_time))
    
    tagList(
      # Student Details Card
      div(class = "student-details",
        h3(class = "section-title", 
           paste(student$first_name, student$last_name)),
        fluidRow(
          column(6,
            tags$p(tags$strong("📧 Email: "), student$email),
            tags$p(tags$strong("📱 Phone: "), student$phone),
            tags$p(tags$strong("🎓 Major: "), student$major),
            tags$p(tags$strong("📅 Graduation: "), 
                   paste(student$graduation_month, student$graduation_year))
          ),
          column(6,
            tags$p(tags$strong("🏠 Hometown: "), student$hometown),
            if (!is.na(student$linkedin_url) && student$linkedin_url != "") {
              tags$p(tags$strong("💼 LinkedIn: "), 
                     tags$a(href = student$linkedin_url, target = "_blank", "Profile"))
            },
            if (!is.na(student$social_media) && student$social_media != "") {
              tags$p(tags$strong("📱 Social: "), student$social_media)
            }
          )
        ),
        actionButton("add_interaction_btn", "➕ Log New Interaction", 
                     class = "btn-primary btn-lg")
      ),
      
      # Interactions History
      h3(class = "section-title", 
         paste("Interaction History (", nrow(student_interactions), ")")),
      
      if (nrow(student_interactions) == 0) {
        div(style = "text-align: center; padding: 40px; color: #95a5a6;",
            "No interactions recorded yet")
      } else {
        lapply(1:nrow(student_interactions), function(i) {
          int <- student_interactions[i, ]
          div(class = "interaction-card",
            tags$strong(paste("📅", int$interaction_date, "at", int$interaction_time)),
            tags$br(),
            tags$em(paste("📍", int$place)),
            tags$br(),
            tags$p(int$notes, style = "margin-top: 10px;")
          )
        })
      }
    )
  })
  
  # Show Add Student Modal
  observeEvent(input$add_student_btn, {
    bslib::modal_show("add_student_modal")
  })
  
  # Save New Student
  observeEvent(input$save_student, {
    req(input$new_first_name, input$new_last_name)
    
    # Generate new student ID
    new_id <- if (nrow(rv$students) == 0) 1 else max(rv$students$student_id, na.rm = TRUE) + 1
    
    new_student <- data.frame(
      student_id = new_id,
      first_name = input$new_first_name,
      last_name = input$new_last_name,
      phone = input$new_phone,
      email = input$new_email,
      graduation_month = input$new_grad_month,
      graduation_year = input$new_grad_year,
      hometown = input$new_hometown,
      major = input$new_major,
      linkedin_url = input$new_linkedin,
      social_media = input$new_social,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      sheet_append(SHEET_URL, new_student, sheet = "Students")
      load_data()
      bslib::modal_hide("add_student_modal")
      rv$selected_student_id <- new_id
      showNotification("Student added successfully!", type = "message")
      
      # Clear form
      updateTextInput(session, "new_first_name", value = "")
      updateTextInput(session, "new_last_name", value = "")
      updateTextInput(session, "new_phone", value = "")
      updateTextInput(session, "new_email", value = "")
      updateSelectInput(session, "new_grad_month", selected = "")
      updateTextInput(session, "new_hometown", value = "")
      updateTextInput(session, "new_major", value = "")
      updateTextInput(session, "new_linkedin", value = "")
      updateTextInput(session, "new_social", value = "")
      
    }, error = function(e) {
      showNotification(paste("Error saving student:", e$message), type = "error")
    })
  })
  
  # Show Add Interaction Modal
  observeEvent(input$add_interaction_btn, {
    bslib::modal_show("add_interaction_modal")
  })
  
  # Save New Interaction
  observeEvent(input$save_interaction, {
    req(rv$selected_student_id)
    
    student <- rv$students %>% 
      filter(student_id == rv$selected_student_id)
    
    # Generate new interaction ID
    new_int_id <- if (nrow(rv$interactions) == 0) 1 else max(rv$interactions$interaction_id, na.rm = TRUE) + 1
    
    new_interaction <- data.frame(
      interaction_id = new_int_id,
      student_id = rv$selected_student_id,
      student_name = paste(student$first_name, student$last_name),
      interaction_date = as.character(input$new_int_date),
      interaction_time = input$new_int_time,
      place = input$new_int_place,
      notes = input$new_int_notes,
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      sheet_append(SHEET_URL, new_interaction, sheet = "Interactions")
      load_data()
      bslib::modal_hide("add_interaction_modal")
      showNotification("Interaction logged successfully!", type = "message")
      
      # Clear form
      updateDateInput(session, "new_int_date", value = Sys.Date())
      updateTextInput(session, "new_int_time", value = format(Sys.time(), "%H:%M"))
      updateTextInput(session, "new_int_place", value = "")
      updateTextInput(session, "new_int_notes", value = "")
      
    }, error = function(e) {
      showNotification(paste("Error saving interaction:", e$message), type = "error")
    })
  })
}

shinyApp(ui = ui, server = server)

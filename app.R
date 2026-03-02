library(shiny)
library(shinyjs)
library(bslib)
library(openxlsx)
library(shinydashboard)
library(DT)
library(dplyr)
library(readxl)
library(RMySQL)
library(DBI)
library(uuid)
library(shinyvalidate)
library(fresh)
library(config)
library(shinymanager)
library(scrypt)
library(aws.s3)

#testing github branch
# --- 1. THEME DEFINITION ---
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#2C3E50", 
    aqua = "#18BC9C", 
    green = "#27ae60", 
    red = "#e74c3c"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#2C3E50", 
    dark_hover_bg = "#1A252F",
    dark_color = "#ecf0f1"
  ),
  adminlte_global(
    content_bg = "#ECF0F5", 
    box_bg = "#FFFFFF", 
    info_box_bg = "#FFFFFF"
  )
)

# --- 2. GLOBAL CONSTANTS ---
# Define choices here so they are consistent in both Add and Edit forms
PLAN_TYPE_CHOICES <- c(
  "",
  "25%",
  "50%",
  "75%",
  "Design",
  "Record / As-Built",
  "100% / Bid Document",
  "Other / Unknown"
)


# Load values from config.yml
consts <- config::get()


# --- 3. DB HELPER FUNCTION ---


get_db_conn <- function() {
  dbConnect(
    RMySQL::MySQL(),
    dbname = consts$db$db_name,
    host = consts$db$db_host,
    port = consts$db$db_port,
    user = consts$db$db_user,
    password = consts$db$db_pass
  )
}

# S3 Bucket secrets loading from config.yml file

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = config::get("s3")$access_key,
  "AWS_SECRET_ACCESS_KEY" = config::get("s3")$secret_key,
  "AWS_DEFAULT_REGION" = config::get("s3")$region,
  "S3_BUCKET_NAME" = config::get("s3")$bucket_name
)
#S3_BUCKET_NAME <- config::get("s3")$bucket_name


# --- S3 HELPER FUNCTIONS ---

upload_to_s3 <- function(file_path, file_name, tenant_id, plan_id) {
  # Creates a folder structure: tenant_id/plan_id/filename.pdf
  s3_key <- paste0(tenant_id, "/", plan_id, "/", file_name)
  
  tryCatch({
    aws.s3::put_object(file = file_path, object = s3_key, bucket = S3_BUCKET_NAME)
    return(list(success = TRUE, key = s3_key, bucket = S3_BUCKET_NAME))
  }, error = function(e) {
    return(list(success = FALSE, error = e$message))
  })
}

# get_s3_link <- function(object_key) {
#   # Generates a temporary, secure link (valid for 1 hour)
#   tryCatch({
#     aws.s3::get_presigned_url(object = object_key, bucket = S3_BUCKET_NAME, expiration = 3600)
#   }, error = function(e) return("#"))
# }



# --- HELPER: Download and Serve S3 File (Robust Fix) ---
get_s3_link <- function(obj_key) {
  req(obj_key)
  bucket <- Sys.getenv("S3_BUCKET_NAME")
  if (bucket == "") return("#")
  
  # 1. Define Cache Directory inside 'www'
  # Shiny serves everything in 'www' at the root URL
  cache_dir <- file.path("www", "s3_cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  # 2. Generate a safe local filename
  # We use a hash of the key to avoid issues with slashes or duplicate names
  ext <- tools::file_ext(obj_key)
  safe_name <- paste0(digest::digest(obj_key), ".", ext)
  local_path <- file.path(cache_dir, safe_name)
  
  # 3. Download if not already cached
  if (!file.exists(local_path)) {
    print(paste("DEBUG S3: Downloading to cache:", obj_key))
    
    tryCatch({
      aws.s3::save_object(
        object = obj_key,
        bucket = bucket,
        file = local_path,
        # Credentials are picked up automatically from Sys.getenv
        region = Sys.getenv("AWS_DEFAULT_REGION")
      )
    }, error = function(e) {
      print(paste("DEBUG S3: Download Failed -", e$message))
      return("#")
    })
  }
  
  # 4. Return the Web Path
  # Since 'www' is the root, we return 's3_cache/filename.ext'
  return(paste0("s3_cache/", safe_name))
}

#
# # --- HELPER: Generate Secure S3 Link (Fixed) ---
# get_s3_link <- function(obj_key) {
#   # 1. Get Bucket Name from Environment
#   bucket_name <- Sys.getenv("S3_BUCKET_NAME")
#
#   if (bucket_name == "") {
#     print("DEBUG S3: Error - S3_BUCKET_NAME env var is missing!")
#     return("#")
#   }
#
#   if (is.null(obj_key) || is.na(obj_key) || obj_key == "") {
#     return("#")
#   }
#
#   # 2. Generate URL
#   url <- tryCatch({
#     aws.s3::get_object_url(
#       object = obj_key,
#       bucket = bucket_name,
#       expiration = 3600,
#       https = TRUE
#     )
#   }, error = function(e) {
#     print(paste("DEBUG S3: AWS Error -", e$message))
#     return("#")
#   })
#
#   return(url)
# }

# --- HELPER: Generate Secure S3 Link (Debug Version) ---
get_s3_link <- function(obj_key) {
  # 1. Check if key exists
  if (is.null(obj_key) || is.na(obj_key) || obj_key == "") {
    print("DEBUG S3: Error - Object Key is NULL or Empty in Database")
    return("#")
  }

  # 2. Check if Bucket Name is loaded
  if (!exists("S3_BUCKET_NAME") || is.null(S3_BUCKET_NAME)) {
    print("DEBUG S3: Error - S3_BUCKET_NAME variable is missing!")
    return("#")
  }

  print(paste("DEBUG S3: Generating link for key:", obj_key))

  # 3. Generate URL
  url <- tryCatch({
    aws.s3::get_object_url(
      object = obj_key,
      bucket = S3_BUCKET_NAME,
      expiration = 3600, # Link valid for 1 hour
      https = TRUE
    )
  }, error = function(e) {
    print(paste("DEBUG S3: AWS Error -", e$message))
    return("#")
  })

  return(url)
}


# --- AUTH CHECK FUNCTION ---

# my_auth_check <- function(user, pass) {
#
#   # 1. Connect
#   conn <- tryCatch({ get_db_conn() }, error = function(e) { return(NULL) })
#
#   if(is.null(conn)) return(list(result = FALSE))
#   print("Testing Log in error!")

# Define the authentication function with DEBUG prints
my_auth_check <- function(user, pass) {
  
  print("------------------------------------------------------")
  print(paste("DEBUG: 1. Attempting login for user:", user))
  
  # 1. Connect to Database
  conn <- tryCatch({
    get_db_conn()
  }, error = function(e) {
    print(paste("DEBUG: CRITICAL - Database connection failed:", e$message))
    return(NULL)
  })
  
  if (is.null(conn)) {
    print("DEBUG: Connection is NULL. Returning FALSE.")
    return(list(result = FALSE))
  }
  print("DEBUG: 2. DB Connection Successful.")
  
  on.exit(dbDisconnect(conn))
  
  # 2. Fetch User Data
  # Note: Explicitly selecting columns to avoid 'unrecognized field type 7' warnings
  safe_user <- dbEscapeStrings(conn, trimws(user))
  
  query <- sprintf(
    "SELECT user_id, tenant_id, email, password_hash, role FROM users WHERE email = '%s' AND is_active = 1", 
    safe_user
  )
  
  print(paste("DEBUG: 3. Running Query:", query))
  
  user_data <- tryCatch({
    dbGetQuery(conn, query)
  }, error = function(e) {
    print(paste("DEBUG: SQL Query Failed:", e$message))
    return(NULL)
  })
  
  print(paste("DEBUG: 4. Rows returned:", nrow(user_data)))
  
  if (is.null(user_data) || nrow(user_data) == 0) {
    print("DEBUG: Login Failed - No user found with that email.")
    return(list(result = FALSE))
  }
  
  # 3. Verify Password
  db_pass <- user_data$password_hash[1]
  print(paste("DEBUG: 5. Found stored password hash (Length:", nchar(db_pass), ")"))
  
  # Check A: Try Scrypt
  is_valid <- FALSE
  try({
    is_valid <- scrypt::verifyPassword(db_pass, pass)
    print(paste("DEBUG: Scrypt verification result:", is_valid))
  }, silent = TRUE)
  
  # Check B: Fallback to Plain Text (Only for migration/testing)
  if (!is_valid && db_pass == pass) {
    print("DEBUG: Plain text password matched (Warning: Not Secure)")
    is_valid <- TRUE
  }
  
  if (is_valid) {
    print("DEBUG: 6. SUCCESS! User authenticated.")
    print("------------------------------------------------------")
    return(list(
      result = TRUE,
      user = user_data$email,
      permissions = user_data$role,
      user_info = user_data # Pass full row for tenant_id access later
    ))
  } else {
    print("DEBUG: 6. FAILURE! Password incorrect.")
    print("------------------------------------------------------")
    return(list(result = FALSE))
  }
}


logo_exists <- file.exists("www/town_logo.png")

# --- 4. UI DEFINITION ---

ui <- dashboardPage(
  
  
  dashboardHeader(
    title = "Plan Database",
    
    # 1. Change Password Button (Top Right)
    tags$li(class = "dropdown",
            actionButton("change_pwd_btn", "Change Password", icon = icon("key"),
                         class = "btn-primary",
                         style = "margin-top: 8px; margin-right: 5px; color: #fff;")
    ),
    
    # 2. Logout Button (Top Right)
    tags$li(class = "dropdown",
            actionButton("logout_btn", "Logout", icon = icon("sign-out-alt"), 
                         class = "btn-danger",
                         style = "margin-top: 8px; margin-right: 15px; color: #fff;")
    )
  ),
  dashboardSidebar(
    tags$div(
      style = "text-align: center; padding-top: 20px; padding-bottom: 20px;",
      # Use an if/else statement to conditionally show the image or a fallback
      if (logo_exists) {
        tags$img(src = "town_logo.png", width = "120px") 
      } else {
        tagList(
          icon("building", class = "fa-3x", style = "color: #ecf0f1;")#,
          #tags$h4("Plan Portal", style = "color: #ecf0f1; font-weight: bold; margin-top: 10px;")
        )
      }
    ),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Browse Plans", tabName = "plan_data", icon = icon("search")),
      menuItem("New Data Entry", tabName = "plan_form", icon = icon("plus-circle")),
      menuItem("User Management", tabName = "user_manage", icon = icon("users-cog"))
    )
  ),
  dashboardBody(
    use_theme(my_theme),
    useShinyjs(),
    tags$head(tags$style(HTML("
      .box { box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24); }
      .box-header { border-bottom: 1px solid #f4f4f4; }
      .btn-primary { color: #ffffff !important; background-color: #2C3E50; border-color: #202d3b; }
      .btn-primary:hover, .btn-primary:focus { background-color: #1a252f !important; color: #ffffff !important; }
      .btn-success { font-weight: bold; }
      .modal-lg { width: 95% !important; max-width: 1400px; }
      .modal-body { background-color: #ECF0F5; padding: 15px; }
      .modal-header { background-color: #2C3E50; color: white; border-top-left-radius: 5px; border-top-right-radius: 5px; }
      .close { color: white; opacity: 0.8; }
      table.dataTable tbody tr { cursor: pointer; }
    "))),
    
    tabItems(
      # --- TAB 1: BROWSE PLANS ---
      tabItem(
        tabName = "plan_data",
        fluidRow(
          box(
            title = "Filter Options", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
            # Street Search Input
            column(4, selectizeInput("search_street", "Search Street:", 
                                     choices = c(""), # <--- Key Change: Initialize with an empty string
                                     selected = "",   # <--- Key Change: Explicitly select the empty string
                                     multiple = FALSE, 
                                     options = list(placeholder = 'Type to search street name...'))),            
            # Focus Checkboxes (Primary, Secondary, Tertiary)
            # inline = TRUE makes them appear side-by-side
            column(6, checkboxGroupInput("filter_focus", "Include Focus Types:", 
                                         choices = c("Primary", "Secondary", "Tertiary"),
                                         selected = c("Primary", "Secondary", "Tertiary"),
                                         inline = TRUE))
          )
        ),
        fluidRow(
          box(width = 12, status = "primary", 
              div(class = "alert alert-info", icon("info-circle"), " Click on any row to view full details, access the plan image, or edit data."),
              DTOutput("table")
          )
        )
      ),
      
      # --- TAB 2: NEW DATA ENTRY FORM ---
      tabItem(
        tabName = "plan_form",
        div(id = "new_plan_form_div",
        # Section 1: Basic Info
        fluidRow(
          box(
            title = "1. Basic Information", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(2, textInput("plan_number", "Plan #")),
              column(3, textInput("plan_name", "Plan Name", "")),
              
              # [UPDATED] Plan Type is now a Dropdown
              column(2, selectInput("plan_type", "Plan Type", choices = PLAN_TYPE_CHOICES, selected = "")),
              
              column(3, selectInput("department", "Department", choices = c("", "Public Works", "Engineering"), selected = "")),
              
              column(2, dateInput("date_on_plan", "Date on Plan", value = NULL))
            ),
            fluidRow(
          
              column(4, selectInput("doc_category", "Document Category", 
                                    choices = c("Plan Image", "Supporting Doc", "Permit"), 
                                    selected = "Plan Image")),
              column(8, fileInput("plan_files", "Upload Documents", 
                                  multiple = TRUE, 
                                  accept = c(".pdf", ".jpg", ".png", ".tif")))
            )
          )
        ),
        # Section 2 & 3: Columns
        fluidRow(
          column(width = 6,
                 box(
                   title = "2. Storage & Technical Specs", width = NULL, status = "info", solidHeader = TRUE,
                   tags$h5("Physical Location", style="font-weight:bold; color:#2c3e50; border-bottom: 1px solid #eee; padding-bottom: 5px;"),
                   fluidRow(
                     column(4, textInput("cabinet_number", "Cabinet #")),
                     column(4, textInput("drawer_number", "Drawer #")),
                     column(4, textInput("plan_in_drawer", "Plan # in Drawer"))
                   ),
                   br(),
                   tags$h5("Dimensions & Scale", style="font-weight:bold; color:#2c3e50; border-bottom: 1px solid #eee; padding-bottom: 5px;"),
                   fluidRow(
                     column(4, numericInput("num_pages", "# of Pages", value = NULL, min = 0)),
                     column(4, numericInput("num_sheets", "# of Sheets", value = NULL, min = 0)),
                     column(4, textInput("scale", "Scale", value = NULL))
                   )
                 )
          ),
          column(width = 6,
                 box(
                   title = "3. Professional Details & Notes", width = NULL, status = "success", solidHeader = TRUE,
                   fluidRow(
                     column(6, textInput("consulting_firm_id", "Consulting Firm")),
                     column(6, textInput("town_bid", "Town Bid"))
                   ),
                   fluidRow(
                     column(6, textInput("engineer_name", "Engineer Name")),
                     column(6, textInput("engineer_stamp", "Engineer Stamp #"))
                   ),
                   fluidRow(
                     column(6, textInput("surveyor_name", "Surveyor Name")),
                     column(6, textInput("surveyor_stamp", "Surveyor Stamp #"))
                   ),
                   textInput("content_of_plan", "Content of Plan"),
                   textAreaInput("notes", "General Notes", rows = 2)
                 )
          )
        ),
        # Section 4: Streets
        fluidRow(
          box(
            title = "4. Associated Streets", width = 12, status = "warning", solidHeader = TRUE,
            div(class = "alert alert-info", icon("info-circle"), " Click 'Associate Street' to link this plan to specific locations."),
            div(id = "dynamicRows"),
            br(),
            fluidRow(
              column(12, 
                     actionButton("add_row", "Associate Street", icon = icon("plus"), class = "btn-primary", style="color:white; margin-right: 10px;"),
                     actionButton("save", "Save Plan Data", icon = icon("save"), class = "btn-success pull-right", style="padding-left: 20px; padding-right: 20px;")
              )
            )
          )
        )
        )
      ),
      # --- TAB 3: USER MANAGEMENT ---
      tabItem(
        tabName = "user_manage",
        fluidRow(
          box(
            title = "User Management", status = "danger", solidHeader = TRUE, width = 12,
            div(class = "pull-left", actionButton("add_user_btn", "Add New User", icon = icon("user-plus"), class = "btn-success")),
            # Hidden button to trigger table refreshes
            shinyjs::hidden(actionButton("user_refresh_trigger", "Refresh")),
            br(), br(),
            DTOutput("users_table")
          )
        )
      )
    )
  )
)

# --- 5. SERVER LOGIC ---
server <- function(input, output, session) {
  # --- AUTHENTICATION ---
  res_auth <- secure_server(check_credentials = my_auth_check)
  
  
  # --- NEW: REACTIVE TENANT ID ---
  current_tenant_id <- reactive({
    req(res_auth$user_id)
  
    # We extract the tenant_id from the user info we fetched during login
    return(res_auth$tenant_id)
  })
  
  # --- NEW: MASTER ROLE CHECKER ---
  # This creates a single, safe way to check permissions throughout the app
  current_user_role <- reactive({
    
    req(res_auth$user_id)
    
    # 1. Try to find the 'role' column in user_info
    # We use user_info$role because your DB column is named 'role'
    role_val <- tryCatch({
      as.character(res_auth$role)
    }, error = function(e) { 
      return("viewer") # Fallback if something breaks
    })
    
    # 2. Handle NULLs or Empty strings safely
    if (length(role_val) == 0 || is.na(role_val) || role_val == "") {
      return("viewer")
    }
    
    return(tolower(role_val))
  })
  
  # --- DEBUG SPY ---
  observe({
    
    # This converts the reactiveValues to a standard list so we can see everything
    val_list <- reactiveValuesToList(res_auth)
    
  })
  
  # --- STOP IF NOT LOGGED IN ---
  # This creates a "gate". Code below won't run properly until login is verified.
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  observe({
    req(res_auth$user_id) # Stop here if login fails
    # You can print a message to console to verify it worked
    
  })
  
 
  # --- LOGIN SUCCESS & PERMISSIONS (VISUAL CONTROL) ---
  observeEvent(res_auth$user_id, {
    req(res_auth$user_id)
    
    # FIX: Use the new Master Checker
    user_role <- current_user_role()
    
    # Default: Hide Restricted Tabs
    shinyjs::runjs("$('a[data-value=\"plan_form\"]').parent().hide();")
    shinyjs::runjs("$('a[data-value=\"user_manage\"]').parent().hide();")
    
    # Reveal based on Role
    if (user_role == "admin") {
      shinyjs::runjs("$('a[data-value=\"plan_form\"]').parent().show();")
      shinyjs::runjs("$('a[data-value=\"user_manage\"]').parent().show();")
    } else if (user_role == "editor") {
      shinyjs::runjs("$('a[data-value=\"plan_form\"]').parent().show();")
    }
    
    # Force resize
    shinyjs::delay(500, shinyjs::runjs("$(window).trigger('resize');"))
  })
  
  data_refresh_trigger <- reactiveVal(0)
  
  # --- HELPERS: Load Streets for Dropdown ---
  observe({
    req(current_tenant_id()) # Wait for login
    conn <- get_db_conn()
    
    tryCatch({
      # New Schema: 'streets' table, 'street_name' column
      t_id <- dbEscapeStrings(conn, current_tenant_id())
      q <- sprintf("SELECT street_name FROM streets WHERE tenant_id = '%s' ORDER BY street_name", t_id)
      
      s <- dbGetQuery(conn, q)
      updateSelectizeInput(session, "search_street", choices = c("", s$street_name))
    }, error = function(e){}, finally = {dbDisconnect(conn)})
  })
  
  # =========================================================================
  # --- 1. VALIDATION SETUP ---
  # =========================================================================
  iv <- InputValidator$new()
  
  iv$add_rule("plan_name", sv_required())
  iv$add_rule("plan_type", sv_required())
  iv$add_rule("department", sv_required())
  iv$add_rule("date_on_plan", sv_required())
  iv$add_rule("plan_image_url", sv_required())
  iv$add_rule("scale", sv_required())
  iv$add_rule("num_pages", sv_required()); iv$add_rule("num_pages", sv_numeric())
  iv$add_rule("num_sheets", sv_required()); iv$add_rule("num_sheets", sv_numeric())
  
  iv$enable()
  
  # --- EDIT FORM VALIDATION ---
  iv_edit <- InputValidator$new()
  iv_edit$add_rule("edit_plan_name", sv_required())
  iv_edit$add_rule("edit_plan_type", sv_required())
  iv_edit$add_rule("edit_department", sv_required())
  iv_edit$add_rule("edit_date_on_plan", sv_required())
  iv_edit$add_rule("edit_plan_image_url", sv_required())
  iv_edit$add_rule("edit_scale", sv_required())
  iv_edit$add_rule("edit_num_pages", sv_required()); iv_edit$add_rule("edit_num_pages", sv_numeric())
  iv_edit$add_rule("edit_num_sheets", sv_required()); iv_edit$add_rule("edit_num_sheets", sv_numeric())
  
  
  # --- REACTIVE STORAGE ---
  values <- reactiveValues(
    dynamic_rows = list(),      
    edit_dynamic_rows = list(),
    current_table_data = NULL
  )
  row_counter <- reactiveVal(0)
  
  # --- MAIN FORM: ADD STREET ROW ---
  observeEvent(input$add_row, {
    row_id <- paste0("row", row_counter() + 1)
    conn <- get_db_conn(); s_list <- dbReadTable(conn, "streets")$streets; dbDisconnect(conn)
    
    insertUI(selector = "#dynamicRows", ui = fluidRow(id = row_id,
                                                      column(5, selectizeInput(paste0("streetname_", row_id), "Street Name:", choices = s_list)),
                                                      column(5, selectizeInput(paste0("streetfocus_", row_id), "Focus:", choices = c("", "Primary", "Secondary", "Tertiary"))),
                                                      column(2, actionButton(paste0("delete_", row_id), "", icon = icon("trash"), class = "btn-danger", style = "margin-top: 25px;"))
    ))
    row_counter(row_counter() + 1)
    values$dynamic_rows <- c(values$dynamic_rows, row_id)
    
    observeEvent(input[[paste0("delete_", row_id)]], {
      removeUI(selector = paste0("#", row_id))
      values$dynamic_rows <- values$dynamic_rows[!values$dynamic_rows %in% row_id]
    })
  })
 
  
  
  # --- SAVE PLAN DATA ---
  observeEvent(input$save, {
    req(input$plan_name)
    conn <- get_db_conn()
    
    # 1. Helper functions to clean/escape data safely
    # This replaces the '?' functionality
    clean <- function(x) {
      if (is.null(x) || is.na(x) || x == "") return("NULL")
      paste0("'", dbEscapeStrings(conn, as.character(x)), "'")
    }
    
    clean_num <- function(x) {
      if (is.null(x) || is.na(x) || x == "") return("NULL")
      # Ensure it's a number, then stringify
      as.character(as.numeric(x))
    }
    
    tryCatch({
      dbBegin(conn) # Start Transaction
      
      # 2. Insert Basic Plan Info
      # We use sprintf() to inject the cleaned values into the string
      q_plan <- sprintf(
        "INSERT INTO plans (tenant_id, plan_number, plan_name, plan_type, department, num_of_pages, num_of_sheets, cabinet_number, drawer_number, plan_in_drawer, town_bid, consulting_firm, scale, engineer_name, engineer_stamp, surveyor_name, surveyor_stamp, content_of_plan, notes, date_on_plan) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)",
        clean(current_tenant_id()),
        clean(input$plan_number),
        clean(input$plan_name),
        clean(input$plan_type),
        clean(input$department),
        clean_num(input$num_pages),
        clean_num(input$num_sheets),
        clean(input$cabinet_number),
        clean(input$drawer_number),
        clean(input$plan_in_drawer),
        clean(input$town_bid),
        clean(input$consulting_firm_id),
        clean(input$scale),
        clean(input$engineer_name),
        clean(input$engineer_stamp),
        clean(input$surveyor_name),
        clean(input$surveyor_stamp),
        clean(input$content_of_plan),
        clean(input$notes),
        clean(as.character(input$date_on_plan))
      )
      
      dbExecute(conn, q_plan)
      
      # 3. Get the new Plan ID
      new_plan_id <- dbGetQuery(conn, "SELECT LAST_INSERT_ID() as id")$id[1]
      
      # 4. Handle S3 File Uploads
      if (!is.null(input$plan_files)) {
        # Loop through each uploaded file
        for (i in 1:nrow(input$plan_files)) {
          file_row <- input$plan_files[i, ]
          
          # Upload to AWS
          res <- upload_to_s3(file_row$datapath, file_row$name, current_tenant_id(), new_plan_id)
          
          if (res$success) {
            # Record in Database
            # We must also format this query with sprintf
            q_doc <- sprintf(
              "INSERT INTO plan_documents (document_id, tenant_id, plan_id, category, display_name, original_file_name, storage_provider, bucket, object_key, mime_type, size_bytes, uploaded_by) VALUES (UUID(), %s, %d, %s, %s, %s, 's3', %s, %s, %s, %s, %s)",
              clean(current_tenant_id()),
              new_plan_id,
              clean(input$doc_category),
              clean(file_row$name),
              clean(file_row$name),
              clean(res$bucket),
              clean(res$key),
              clean(file_row$type),
              clean_num(file_row$size),
              clean(res_auth$user_id)
            )
            
            dbExecute(conn, q_doc)
          }
        }
      }
      
      dbCommit(conn) # Commit Transaction
      showNotification("Plan Saved Successfully!", type="message")
      
      # Reset form
      shinyjs::reset("new_plan_form_div")
      values$dynamic_rows <- list()
      removeUI(selector = "#dynamicRows > div", multiple = TRUE)
      
      # Trigger refresh on Browse tab
      data_refresh_trigger(data_refresh_trigger() + 1)
      
    }, error = function(e) {
      dbRollback(conn) # Rollback if anything failed
      showNotification(paste("Error:", e$message), type="error")
    }, finally = {
      dbDisconnect(conn)
    })
  })
  
  # =========================================================================
  # --- TABLE RENDER WITH FILTERING ---
  # =========================================================================
  
  # Reactive that handles the filtering logic
  filtered_data <- reactive({
    data_refresh_trigger() 
    req(current_tenant_id())
    
    conn <- get_db_conn()
    t_id <- dbEscapeStrings(conn, current_tenant_id())
    
    # 1. Get Plans (Filtered by Tenant)
    # Using 'plan_id' instead of 'unique_planID' as per your new schema
    #q_plans <- sprintf("SELECT * FROM plans WHERE tenant_id = '%s' ORDER BY plan_id DESC", t_id)
    
    
    
    
    q_plans <- sprintf("
  SELECT p.plan_id, p.plan_number, p.plan_name, p.plan_type, p.department, p.date_on_plan,
         GROUP_CONCAT(DISTINCT s.street_name ORDER BY s.street_name SEPARATOR ', ') as associated_streets
  FROM plans p
  LEFT JOIN plan_streets ps ON p.plan_id = ps.plan_id
  LEFT JOIN streets s ON ps.street_id = s.street_id
  WHERE p.tenant_id = '%s'
  GROUP BY p.plan_id
", dbEscapeStrings(conn, current_tenant_id()))
    plans <- dbGetQuery(conn, q_plans)
    
    # 2. Get Street Associations (Joined to get names)
    # New Schema: plan_streets links to streets
    q_streets <- sprintf("
      SELECT ps.plan_id, s.street_name, ps.focus 
      FROM plan_streets ps
      JOIN streets s ON ps.street_id = s.street_id
      WHERE ps.tenant_id = '%s'", t_id)
    
    street_associations <- dbGetQuery(conn, q_streets)
    
    dbDisconnect(conn)
    
    # --- Step D: Prepare Primary Streets Column ---
    # Note: Using 'plan_id' and 'focus' (lowercase) based on new schema
    primaries <- street_associations %>%
      filter(focus == "Primary") %>%
      group_by(plan_id) %>%
      summarise(Primary_Street = paste(street_name, collapse = ", "))
    
    # Join back to plans (Change unique_planID to plan_id)
    plans <- plans %>%
      left_join(primaries, by = "plan_id") 
    
    plans$Primary_Street[is.na(plans$Primary_Street)] <- ""
    
    # --- FILTERING LOGIC ---
    has_street_filter <- !is.null(input$search_street) && input$search_street != ""
    
    if (has_street_filter) {
      valid_associations <- street_associations %>%
        filter(street_name == input$search_street) %>%
        filter(focus %in% input$filter_focus)
      
      matching_ids <- valid_associations$plan_id
      
      plans <- plans %>% 
        filter(plan_id %in% matching_ids)
    }
    
    values$current_table_data <- plans
    return(plans)
  })
  
  # =========================================================================
  # --- 3. USER MANAGEMENT SERVER LOGIC ---
  # =========================================================================

  # A. Fetch User List
  users_reactive <- reactive({
    
    # 1. Check Login & Refresh
    req(res_auth$user_id)
    input$user_refresh_trigger 
    
    # 2. Check Permissions (Using our new Master Checker)
    if (current_user_role() != "admin") {
      return(NULL)
    }
    
    # 3. Fetch Data
    
    conn <- get_db_conn()
    df <- tryCatch({
      t_id <- dbEscapeStrings(conn, current_tenant_id())
      
      # FIX: Select specific columns to match what the UI expects
      # We alias 'role' -> 'permissions' so the rest of your UI code works
      q <- sprintf("
        SELECT 
          user_id,
          tenant_id,
          email,
          full_name,
          role, 
          full_name as name, 
          CAST(is_active AS CHAR) as is_active,
          created_at
        FROM users 
        WHERE tenant_id = '%s'", t_id)
      
      dbGetQuery(conn, q) 
    }, error = function(e) {
      print(paste("Query Error:", e$message))
      return(NULL)
    }, finally = { dbDisconnect(conn) })
    
    return(df)
  })
  
  # =========================================================================
  # --- RENDER USER TABLE (Vectorized & Crash-Proof) ---
  # =========================================================================
  output$users_table <- renderDT({
    req(res_auth$user_id)
    
    # 1. Get Data
    df <- users_reactive()
    
    # 2. Return Empty if NULL or No Rows
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Status = "No users found."), options = list(dom = 't')))
    }
    
    # 3. Prepare Variables for Vectorization
    ids <- df$user_id
    current_user <- res_auth$user_id
    
    # Safety: Handle NAs in user_id to prevent crashes
    ids[is.na(ids)] <- 0 
    
    # 4. Generate Buttons (Vectorized)
    
    # -- A. Edit Buttons (Same for everyone) --
    edit_btns <- paste0(
      '<button class="btn btn-warning btn-sm" id="edit_user_', ids, '" ',
      'onclick="Shiny.setInputValue(\'edit_user_trigger\', this.id, {priority: \'event\'})" ',
      'title="Edit User"><i class="fa fa-pencil"></i></button>'
    )
    
    # -- B. Delete Buttons (Conditional) --
    # Identify which rows belong to the current user
    is_me <- (ids == current_user)
    is_me[is.na(is_me)] <- FALSE # Treat NA as FALSE to be safe
    
    del_btns <- ifelse(is_me,
                       # If it IS me: Disabled Button
                       '<button class="btn btn-danger btn-sm" disabled title="You cannot delete yourself"><i class="fa fa-trash"></i></button>',
                       # If it is NOT me: Active Button
                       paste0(
                         '<button class="btn btn-danger btn-sm" id="del_user_', ids, '" ',
                         'onclick="Shiny.setInputValue(\'delete_user\', this.id, {priority: \'event\'})" ',
                         'title="Delete User"><i class="fa fa-trash"></i></button>'
                       )
    )
    
    # -- C. Combine into Action Column --
    df$Actions <- paste0('<div class="btn-group" role="group">', edit_btns, " ", del_btns, '</div>')
    
    # 5. Select & Display Columns
    # Ensure these column names exist in your DB. If 'created_at' is missing, remove it.
    display_df <- df %>% 
      select(user_id, tenant_id, full_name, email, role, is_active, created_at, Actions)
    
    # 6. Render DataTable
    datatable(display_df, 
              escape = FALSE,
              selection = "none",
              rownames = FALSE,
              options = list(
                dom = 'rtip',
                pageLength = 10,
                autoWidth = FALSE,
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all"),
                  list(width = '60px', targets = 0),  # User ID width
                  list(width = '100px', targets = 4), # Actions width
                  list(orderable = FALSE, targets = 4)
                )
              ))
  })
  
  # --- FIX: Force Resize when switching tabs ---
  observeEvent(input$sidebar_menu, {
    if(input$sidebar_menu == "user_manage") {
      shinyjs::delay(400, shinyjs::runjs("$(window).trigger('resize');"))
    }
  })
  
  # C. Add User Logic
  observeEvent(input$add_user_btn, {
    # Security Check
    user_role <- tolower(as.character(res_auth$role))
    
    if (user_role != "admin") {
      showNotification("⛔ ACCESS DENIED: Only admins can perform this action.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = "Add New User",
      textInput("new_user_name", "Full Name"),
      textInput("new_user_id", "Username (Login)"),
      passwordInput("new_user_pass", "Password"),
      #selectInput("new_user_perm", "Permissions", choices = c("", "editor", "viewer", "admin")),
      selectInput("new_user_perm", "Permissions", 
                  choices = c("Select a Role" = "", "editor", "viewer", "admin"), 
                  selected = ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_user", "Save User", class = "btn-primary")
      )
    ))
  })
  
  # C. Add User Logic (Fixed for RMySQL)
  observeEvent(input$confirm_add_user, {
    req(input$new_user_id, input$new_user_pass)
    
    # FINAL SECURITY CHECK
    user_role <- tolower(as.character(res_auth$role))
    if (user_role != "admin") {
      removeModal()
      showNotification("⛔ SECURITY ALERT: You do not have permission to add users.", type = "error")
      return()
    }
    removeModal()
    
    # Hash the password
    hashed_pass <- scrypt::hashPassword(input$new_user_pass)
    
    conn <- get_db_conn()
    tryCatch({
      # 1. Escape inputs to prevent SQL Injection
      safe_tenant <- dbEscapeStrings(conn, current_tenant_id())
      safe_user   <- dbEscapeStrings(conn, input$new_user_id) # This is the email
      safe_pass   <- dbEscapeStrings(conn, hashed_pass)
      safe_perm   <- dbEscapeStrings(conn, input$new_user_perm) # This is the role
      safe_name   <- dbEscapeStrings(conn, input$new_user_name)
      
      # New Schema Insert
      query <- sprintf(
        "INSERT INTO users (user_id, tenant_id, email, password_hash, role, full_name, is_active) 
         VALUES (UUID(), '%s', '%s', '%s', '%s', '%s', 1)",
        safe_tenant, safe_user, safe_pass, safe_perm, safe_name
      )
      
      # 3. Execute
      dbExecute(conn, query)
      
      showNotification("User added successfully!", type = "message")
      shinyjs::click("user_refresh_trigger") # Refresh table
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    }, finally = { dbDisconnect(conn) })
  })
  
  
  # =========================================================================
  # --- # E. Edit User Logic (Open Modal) ---
  # =========================================================================
  observeEvent(input$edit_user_trigger, {
    req(input$edit_user_trigger) # Ensure button was actually clicked
    
    # 1. Security Check
    # We use %in% to allow both "admin" and "super admin" if you have that role
    user_role <- tolower(as.character(res_auth$role))
    
    # FIX: Corrected print syntax (using paste)
    print(paste("DEBUG: Edit Triggered. Current Role:", user_role))
    
    if (!user_role %in% c("admin", "super admin")) {
      showNotification("⛔ ACCESS DENIED.", type = "error")
      return()
    }
    
    # 2. Get the User ID from the button ID
    raw_id <- input$edit_user_trigger
    print(paste("DEBUG: Raw Trigger ID:", raw_id))
    
    # Remove the prefix to get the integer ID
    selected_user_id <- sub("edit_user_", "", raw_id)
    print(paste("DEBUG: Target User ID:", selected_user_id))
    
    # 3. Fetch current details from DB
    conn <- get_db_conn()
    on.exit(dbDisconnect(conn)) # Ensure disconnect even if errors occur
    
    user_data <- tryCatch({
      # FIX: Explicitly select columns. 
      # Do NOT use SELECT * (it breaks on timestamps with RMySQL)
      query <- sprintf(
        "SELECT user_id, email, role FROM users WHERE user_id = '%s'", 
        dbEscapeStrings(conn, selected_user_id)
      )
      dbGetQuery(conn, query)
    }, error = function(e) {
      print(paste("DEBUG: SQL Error -", e$message))
      showNotification(paste("Error fetching user:", e$message), type = "error")
      return(NULL)
    })
    
    # Check if we actually found the user
    if (is.null(user_data) || nrow(user_data) == 0) {
      showNotification("Error: User not found in database.", type = "error")
      return()
    }
    
    # 4. Show Modal
    showModal(modalDialog(
      title = paste("Edit User:", user_data$email),
      
      # Hidden input to track who we are editing (Using the ID from DB)
      div(style = "display:none;", 
          textInput("edit_target_user_id", "", value = user_data$user_id)),
      
      # FIX: Mapped to 'email' because 'name' likely doesn't exist in your table
      textInput("edit_user_name_input", "Email / Username", value = user_data$email),
      
      # FIX: Mapped to 'role' because 'permissions' likely doesn't exist in your table
      selectInput("edit_user_perm_input", "Permissions", 
                  choices = c("editor", "viewer", "admin"), 
                  selected = user_data$role),
      
      hr(),
      tags$p("Change Password (leave blank to keep current):", style = "font-weight: bold;"),
      passwordInput("edit_user_pass_input", "New Password", 
                    placeholder = "Leave blank to keep existing password"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit_user_save", "Save Changes", class = "btn-primary")
      )
    ))
  }) 
  
  
  # E. Save Edited User Logic
  observeEvent(input$confirm_edit_user_save, {
    req(input$edit_target_user_id)
    
    # 1. Security Check (Server Side)
    user_role <- tolower(as.character(res_auth$role))
    if (user_role != "admin") {
      removeModal()
      showNotification("⛔ SECURITY ALERT: Permission Denied.", type = "error")
      return()
    }
    
    removeModal()
    
    conn <- get_db_conn()
    tryCatch({
      # Prepare inputs
      safe_target <- dbEscapeStrings(conn, input$edit_target_user_id)
      safe_name   <- dbEscapeStrings(conn, input$edit_user_name_input)
      safe_perm   <- dbEscapeStrings(conn, input$edit_user_perm_input)
      
      # 2. Construct Query based on whether password changed
      if (input$edit_user_pass_input != "") {
        # CASE A: Update Password + Info
        new_hash <- scrypt::hashPassword(input$edit_user_pass_input)
        safe_pass <- dbEscapeStrings(conn, new_hash)
        
        query <- sprintf(
          "UPDATE users SET `name` = '%s', `permissions` = '%s', `password` = '%s' WHERE `user` = '%s'",
          safe_name, safe_perm, safe_pass, safe_target
        )
      } else {
        # CASE B: Update Info Only (Keep old password)
        query <- sprintf(
          "UPDATE users SET `name` = '%s', `permissions` = '%s' WHERE `user` = '%s'",
          safe_name, safe_perm, safe_target
        )
      }
      
      # 3. Execute
      dbExecute(conn, query)
      
      showNotification("User updated successfully.", type = "message")
      shinyjs::click("user_refresh_trigger") # Refresh table
      
    }, error = function(e) {
      showNotification(paste("Update Error:", e$message), type = "error")
    }, finally = { dbDisconnect(conn) })
  })
  
  # E. Delete User Logic
  observeEvent(input$del_user_trigger, {
    
    # Security Check
    user_role <- tolower(as.character(res_auth$role))
    if (user_role != "admin") {
      showNotification("⛔ ACCESS DENIED.", type = "error")
      return()
    }
    
    raw_id <- input$del_user_trigger
    selected_user <- sub("del_user_", "", raw_id)
    
    # Prevent deleting yourself
    if(selected_user == res_auth$user_id) {
      showNotification("You cannot delete your own account!", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Are you sure you want to permanently delete user:", selected_user, "?"),
      div(style = "display:none;", textInput("del_user_id_hidden", "", value = selected_user)),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_del_user", "Delete User", class = "btn-danger"))
    ))
  })
  
  # E. Delete User Logic (Fixed for RMySQL)
  observeEvent(input$confirm_del_user, {
    req(input$del_user_id_hidden)
    removeModal()
    
    conn <- get_db_conn()
    tryCatch({
      # 1. Escape input
      safe_id <- dbEscapeStrings(conn, input$del_user_id_hidden)
      
      # 2. Construct Query
      query <- sprintf("DELETE FROM users WHERE user_id = '%s'", safe_id)
      
      # 3. Execute
      dbExecute(conn, query)
      
      showNotification("User deleted.", type = "warning")
      shinyjs::click("user_refresh_trigger")
      
    }, error = function(e) { 
      showNotification(paste("Error:", e$message), type = "error") 
    }, finally = { dbDisconnect(conn) })
  })
  
  # Render the DataTable
  output$table <- renderDT({
    # Use the filtered data reactive created above
    df <- filtered_data()
    
    # Select columns for display
    df_display <- df %>% 
      select(plan_id, plan_number, plan_name, Primary_Street, department, date_on_plan)
    
    colnames(df_display) <- c("ID", "Plan #", "Plan Name", "Primary Street", "Department", "Date")
    
    datatable(df_display, 
              options = list(
                scrollX = TRUE,
                columnDefs = list(list(targets = 0, visible = FALSE)) # Hide ID column
              ), 
              escape = FALSE, 
              selection = "single", 
              rownames = FALSE)
  })
  
  # =========================================================================
  # --- VIEW PLAN DETAILS (Fixed: Explicit SELECT to avoid Timestamp Warnings) ---
  # =========================================================================
  observeEvent(input$table_rows_selected, {
    selected_idx <- input$table_rows_selected
    req(selected_idx)
    
    # 1. Get the Plan ID from the selected row
    df <- filtered_data() 
    selected_row <- df[selected_idx, ]
    pk_id <- selected_row$plan_id 
    
    # Helper to get current tenant
    tenant_id <- current_tenant_id() 
    
    conn <- get_db_conn()
    on.exit(dbDisconnect(conn))
    
    # 2. Fetch Plan Details (Explicit Columns)
    # FIX: We select ONLY the columns we need to display, avoiding 'created_at' (Timestamp)
    plan_sql <- sprintf(
      "SELECT plan_number, plan_name, plan_type, department, date_on_plan, 
              cabinet_number, drawer_number, plan_in_drawer, notes 
       FROM plans WHERE plan_id = %s AND tenant_id = '%s'", 
      dbEscapeStrings(conn, as.character(pk_id)), 
      dbEscapeStrings(conn, tenant_id)
    )
    plan <- dbGetQuery(conn, plan_sql)
    
    # 3. Fetch Associated Streets (Explicit Columns)
    street_sql <- sprintf(
      "SELECT s.street_name, ps.focus 
       FROM plan_streets ps 
       JOIN streets s ON ps.street_id = s.street_id 
       WHERE ps.plan_id = %s", 
      dbEscapeStrings(conn, as.character(pk_id))
    )
    streets <- dbGetQuery(conn, street_sql)
    
    # 4. Fetch Documents (Explicit Columns)
    # FIX: We select ONLY needed columns, avoiding 'uploaded_at' (Timestamp)
    doc_sql <- sprintf(
      "SELECT object_key, display_name, category 
       FROM plan_documents WHERE plan_id = %s", 
      dbEscapeStrings(conn, as.character(pk_id))
    )
    docs <- dbGetQuery(conn, doc_sql)
    
    # 5. Generate HTML List of Links
    doc_html <- tags$em("No documents attached.")
    
    if (nrow(docs) > 0) {
      links <- lapply(1:nrow(docs), function(i) {
        d <- docs[i, ]
        url <- get_s3_link(d$object_key) 
        
        tags$li(
          style = "margin-bottom: 5px;",
          tags$a(href = url, target = "_blank", class = "btn btn-default btn-xs", 
                 icon("cloud-download"), 
                 paste0(" ", d$display_name)),
          tags$small(class = "text-muted", paste0(" (", d$category, ")"))
        )
      })
      doc_html <- tags$ul(style = "list-style: none; padding-left: 0;", links)
    }
    
    # 6. Show the Modal
    showModal(modalDialog(
      # title = paste("Plan Details:", plan$plan_number),
      # size = "l", 
      title = tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
        tags$span(paste("Plan Details:", plan$plan_name), style = "font-weight: bold;"),
        # The close button
        tags$button(
          type = "button", 
          class = "close", 
          `data-dismiss` = "modal", 
          icon("times"),
          style = "font-size: 24px; color: #FFFFFF; opacity: 0.7; margin-top: -5px;"
        )
      ),
      size = "l",
      easyClose = TRUE,
      fade = TRUE,
      fluidRow(
        column(6, 
               h4("Basic Info"),
               p(strong("Plan Name:"), plan$plan_name),
               p(strong("Type:"), plan$plan_type),
               p(strong("Department:"), plan$department),
               p(strong("Date:"), plan$date_on_plan),
               hr(),
               h4("Attached Documents"),
               doc_html 
        ),
        column(6,
               h4("Storage & Notes"),
               p(strong("Location:"), paste("Cab:", plan$cabinet_number, "/ Drw:", plan$drawer_number)),
               p(strong("Notes:"), plan$notes),
               hr(),
               h4("Associated Streets"),
               if(nrow(streets) > 0) {
                 HTML(paste(apply(streets, 1, function(x) paste0("<b>", x['street_name'], "</b> (", x['focus'], ")")), collapse = "<br>"))
               } else {
                 "No streets recorded."
               }
        )
      ),
      footer = modalButton("Close")
    ))
  })
  # observeEvent(input$table_rows_selected, {
  #   selected_idx <- input$table_rows_selected
  #   req(selected_idx)
  #   
  #   df <- filtered_data()
  #   selected_row <- df[selected_idx, ]
  #   pk_id <- selected_row$plan_id
  #   
  #   # --- 1. GENERATE IMAGE URL ---
  #   base_url <- "http://192.168.1.27/dpwplans/"
  #   sub_folder <- "Scanned Plans and Documents/"
  #   
  #   clean_path <- function(x) {
  #     if (is.na(x) || x == "") return("#")
  #     x <- gsub("\\\\", "/", x)
  #     parts <- unlist(strsplit(x, "/"))
  #     cleaned_parts <- sapply(parts, function(p) {
  #       raw <- utils::URLdecode(p)
  #       utils::URLencode(raw, reserved = TRUE)
  #     })
  #     return(paste(cleaned_parts, collapse = "/"))
  #   }
  #   
  #   full_img_url <- paste0(base_url, sub_folder, clean_path(selected_row$plan_image_url))
  #   
  #   # --- 2. FETCH STREETS ---
  #   conn <- get_db_conn()
  #   streets <- dbGetQuery(conn, paste0("SELECT * FROM dpw.street_focus WHERE plan_id = ", pk_id))
  #   dbDisconnect(conn)
  #   
  #   street_html <- if(nrow(streets) > 0) {
  #     paste(apply(streets, 1, function(x) paste0("<b>", x['street_name'], "</b> (", x['street_focus'], ")")), collapse = "<br>")
  #   } else {
  #     "<em>No streets associated</em>"
  #   }
  #   
  #   # --- 3. DETERMINE PERMISSIONS (NEW SECURITY LOGIC) ---
  #   user_role <- tolower(as.character(res_auth$role))
  #   
  #   # Default Footer: Only "Close" button
  #   modal_footer <- tagList(modalButton("Close"))
  #   
  #   # Admin/Data Entry Footer: "Close" + "Edit" button
  #   if (user_role %in% c("admin", "manager", "data entry")) {
  #     modal_footer <- tagList(
  #       modalButton("Close"),
  #       actionButton("trigger_edit_from_view", "Edit Record", icon = icon("pencil"), class = "btn-warning", 
  #                    onclick = sprintf("Shiny.setInputValue('edit_trigger', 'edit_%s', {priority: 'event'})", pk_id))
  #     )
  #   }
  #   
  #   # --- 4. SHOW MODAL ---
  #   showModal(modalDialog(
  #     # UPDATED TITLE WITH 'X' BUTTON
  #     title = tagList(
  #       paste("Plan Details:", selected_row$plan_number),
  #       tags$button(
  #         type = "button",
  #         class = "close",
  #         "data-dismiss" = "modal", 
  #         "aria-label" = "Close",
  #         tags$span("aria-hidden" = "true", HTML("&times;"))
  #       )
  #     ),
  #     size = "l",
  #     fluidRow(
  #       column(6, 
  #              h4("Basic Info"),
  #              p(strong("Plan Name:"), selected_row$plan_name),
  #              p(strong("Plan Type:"), selected_row$plan_type),
  #              p(strong("Department:"), selected_row$department),
  #              p(strong("Date:"), selected_row$date_on_plan),
  #              
  #              br(),
  #              tags$a(href = full_img_url, target = "_blank", class = "btn btn-info", icon("eye"), " View Plan Image"),
  #              br(), br(),
  #              hr(),
  #              
  #              h4("Storage"),
  #              p(strong("Cabinet:"), selected_row$cabinet_number),
  #              p(strong("Drawer:"), selected_row$drawer_number),
  #              p(strong("Plan #:"), selected_row$plan_in_drawer),
  #              p(strong("Scale:"), selected_row$scale)
  #       ),
  #       column(6,
  #              h4("Professional Info"),
  #              p(strong("Consultant:"), selected_row$consulting_firm),
  #              p(strong("Engineer:"), selected_row$engineer_name),
  #              p(strong("Surveyor:"), selected_row$surveyor_name),
  #              hr(),
  #              h4("Content & Notes"),
  #              p(strong("Content:"), selected_row$content_of_plan),
  #              p(strong("Notes:"), selected_row$notes),
  #              hr(),
  #              h4("Streets"),
  #              HTML(street_html)
  #       )
  #     ),
  #     footer = modal_footer # <--- THIS IS THE KEY CHANGE
  #   ))
  # })
  
  # =========================================================================
  # --- EDIT PLAN LOGIC (TRIGGER) ---
  # =========================================================================
  observeEvent(input$edit_trigger, {
    
    # --- SECURITY CHECK START ---
    user_role <- tolower(as.character(res_auth$role))
    if (user_role == "viewer") {
      showNotification("⛔ Permission Denied: Standard users cannot edit records.", type = "error")
      return()
    }
    # --- SECURITY CHECK END ---
    
    removeModal() # Close View Modal
    iv_edit$disable()
    
    # Parse the ID from the input string (format: "edit_123")
    clicked_id <- sub("edit_", "", input$edit_trigger)
    
    conn <- get_db_conn()
    safe_id <- dbEscapeStrings(conn, clicked_id)
    
    # Fetch existing data
    plan_data <- dbGetQuery(conn, paste0("SELECT * FROM plans WHERE plan_id = ", safe_id))
    street_data <- dbGetQuery(conn, paste0("SELECT * FROM plan_streets WHERE plan_id = ", safe_id))
    
    # Fetch street list for dropdowns
    s_list <- c("", dbReadTable(conn, "ws_streets")$street)
    dbDisconnect(conn)
    
    req(nrow(plan_data) > 0)
    main <- plan_data[1, ]
    
    # Initialize reactive list for dynamic rows
    values$edit_dynamic_rows <- list()
    
    showModal(modalDialog(
      # UPDATED TITLE WITH 'X' BUTTON
      title = tagList(
        paste("Edit Plan:", main$plan_number),
        tags$button(
          type = "button",
          class = "close",
          "data-dismiss" = "modal",
          "aria-label" = "Close",
          tags$span("aria-hidden" = "true", HTML("&times;"))
        )
      ),
      size = "l",
      
      # Hidden input to store ID
      textInput("edit_unique_id", label = NULL, value = main$plan_id, width = "1px"),
      tags$style("#edit_unique_id { display:none; }"),
      
      fluidRow(
        box(title = "1. Basic Information", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(2, textInput("edit_plan_number", "Plan #", value = main$plan_number)),
              column(3, textInput("edit_plan_name", "Plan Name", value = main$plan_name)),
              column(2, selectInput("edit_plan_type", "Plan Type", choices = PLAN_TYPE_CHOICES, selected = main$plan_type)),
              column(3, selectInput("edit_department", "Department", choices = c("", "Public Works", "Engineering"), selected = main$department)),
              column(2, dateInput("edit_date_on_plan", "Date on Plan", value = main$date_on_plan))
            ),
            fluidRow(
              column(12, textAreaInput("edit_plan_image_url", "Plan Image URL", value = main$plan_image_url, placeholder = "e.g. Folder name/Plan Name.pdf", rows = 1))
            )
        )
      ),
      
      fluidRow(
        column(width = 6,
               box(title = "2. Storage & Technical Specs", width = NULL, status = "info", solidHeader = TRUE,
                   fluidRow(
                     column(4, textInput("edit_cabinet_number", "Cabinet #", value = main$cabinet_number)),
                     column(4, textInput("edit_drawer_number", "Drawer #", value = main$drawer_number)),
                     column(4, textInput("edit_plan_in_drawer", "Plan # in Drawer", value = main$plan_in_drawer))
                   ),
                   br(),
                   fluidRow(
                     column(4, numericInput("edit_num_pages", "# of Pages", value = main$num_of_pages, min = 0)),
                     column(4, numericInput("edit_num_sheets", "# of Sheets", value = main$num_of_sheets, min = 0)),
                     column(4, textInput("edit_scale", "Scale", value = main$scale))
                   )
               )
        ),
        column(width = 6,
               box(title = "3. Professional Details & Notes", width = NULL, status = "success", solidHeader = TRUE,
                   fluidRow(
                     column(6, textInput("edit_consulting_firm_id", "Consulting Firm", value = main$consulting_firm)), 
                     column(6, textInput("edit_town_bid", "Town Bid", value = main$town_bid))
                   ),
                   fluidRow(
                     column(6, textInput("edit_engineer_name", "Engineer Name", value = main$engineer_name)), 
                     column(6, textInput("edit_engineer_stamp", "Engineer Stamp #", value = main$engineer_stamp))
                   ),
                   fluidRow(
                     column(6, textInput("edit_surveyor_name", "Surveyor Name", value = main$surveyor_name)), 
                     column(6, textInput("edit_surveyor_stamp", "Surveyor Stamp #", value = main$surveyor_stamp))
                   ),
                   textInput("edit_content_of_plan", "Content of Plan", value = main$content_of_plan),
                   textAreaInput("edit_notes", "General Notes", value = main$notes, rows = 2)
               )
        )
      ),
      
      fluidRow(
        box(title = "4. Associated Streets", width = 12, status = "warning", solidHeader = TRUE,
            div(id = "edit_modal_streets"),
            br(),
            actionButton("edit_add_row", "Add Street", icon = icon("plus"), class = "btn-primary", style="color:white;")
        )
      ),
      
      footer = tagList(
        div(style = "display: flex; justify-content: space-between;",
            actionButton("delete_plan_init", "Delete Entry", icon = icon("trash"), class = "btn-danger"),
            div(
              modalButton("Cancel"),
              actionButton("save_edits", "Save Changes", class = "btn-success", icon = icon("save"))
            )
        )
      )
    ))
    
    # --- Re-populate existing street rows ---
    if(nrow(street_data) > 0) {
      edit_ctr <- 0
      for(i in 1:nrow(street_data)) {
        edit_ctr <- edit_ctr + 1
        r_id <- paste0("edit_row_", edit_ctr)
        
        insertUI(selector = "#edit_modal_streets", ui = fluidRow(id = r_id,
                                                                 column(5, selectizeInput(paste0("edit_streetname_", r_id), "Street Name:", choices = s_list, selected = street_data$street_name[i])),
                                                                 column(5, selectizeInput(paste0("edit_streetfocus_", r_id), "Focus:", choices = c("", "Primary", "Secondary", "Tertiary"), selected = street_data$street_focus[i])),
                                                                 column(2, actionButton(paste0("delete_edit_", r_id), "", icon = icon("trash"), class = "btn-danger", style = "margin-top: 25px;"))
        ))
        
        values$edit_dynamic_rows <- c(values$edit_dynamic_rows, r_id)
        
        local({
          lid <- r_id
          observeEvent(input[[paste0("delete_edit_", lid)]], {
            removeUI(selector = paste0("#", lid))
            values$edit_dynamic_rows <- values$edit_dynamic_rows[!values$edit_dynamic_rows %in% lid]
          }, ignoreInit=TRUE, once=TRUE)
        })
      }
    }
  })
  
  observeEvent(input$edit_add_row, {
    r_id <- paste0("edit_row_new_", runif(1, 1, 100000))
    
    conn <- get_db_conn()
    s_list <- c("", dbReadTable(conn, "ws_streets")$street)
    dbDisconnect(conn)
    
    insertUI(selector = "#edit_modal_streets", ui = fluidRow(id = r_id,
                                                             column(5, selectizeInput(paste0("edit_streetname_", r_id), "Street Name:", choices = s_list)),
                                                             column(5, selectizeInput(paste0("edit_streetfocus_", r_id), "Focus:", choices = c("", "Primary", "Secondary", "Tertiary"))),
                                                             column(2, actionButton(paste0("delete_edit_", r_id), "", icon = icon("trash"), class = "btn-danger", style = "margin-top: 25px;"))
    ))
    values$edit_dynamic_rows <- c(values$edit_dynamic_rows, r_id)
    observeEvent(input[[paste0("delete_edit_", r_id)]], {
      removeUI(selector = paste0("#", r_id))
      values$edit_dynamic_rows <- values$edit_dynamic_rows[!values$edit_dynamic_rows %in% r_id]
    }, ignoreInit=TRUE, once=TRUE)
  })
  
  # =========================================================================
  # --- SAVE EDITS LOGIC ---
  # =========================================================================
  observeEvent(input$save_edits, {
    
    # --- SECURITY CHECK START ---
    user_role <- tolower(as.character(res_auth$role))
    if (user_role == "viewer") {
      removeModal()
      showNotification("⛔ Permission Denied: Standard users cannot save changes.", type = "error")
      return()
    }
    # --- SECURITY CHECK END ---
    
    iv_edit$enable() 
    req(iv_edit$is_valid()) 
    req(input$edit_unique_id) 
    
    conn <- get_db_conn()
    tryCatch({
      # 1. Update Main Table
      query_main <- sprintf(
        "UPDATE dpw.plan_data SET plan_number='%s', plan_name='%s', plan_type='%s', department='%s', date_on_plan='%s', cabinet_number='%s', drawer_number='%s', plan_in_drawer='%s', num_of_pages=%d, num_of_sheets=%d, scale='%s', consulting_firm='%s', engineer_name='%s', engineer_stamp='%s', surveyor_name='%s', surveyor_stamp='%s', content_of_plan='%s', notes='%s', plan_image_url='%s', town_bid='%s' WHERE plan_id=%s",
        dbEscapeStrings(conn, input$edit_plan_number),
        dbEscapeStrings(conn, input$edit_plan_name),
        dbEscapeStrings(conn, input$edit_plan_type),
        dbEscapeStrings(conn, input$edit_department),
        as.character(input$edit_date_on_plan),
        dbEscapeStrings(conn, input$edit_cabinet_number),
        dbEscapeStrings(conn, input$edit_drawer_number),
        dbEscapeStrings(conn, input$edit_plan_in_drawer),
        as.integer(input$edit_num_pages),
        as.integer(input$edit_num_sheets),
        dbEscapeStrings(conn, input$edit_scale),
        dbEscapeStrings(conn, input$edit_consulting_firm_id),
        dbEscapeStrings(conn, input$edit_engineer_name),
        dbEscapeStrings(conn, input$edit_engineer_stamp),
        dbEscapeStrings(conn, input$edit_surveyor_name),
        dbEscapeStrings(conn, input$edit_surveyor_stamp),
        dbEscapeStrings(conn, input$edit_content_of_plan),
        dbEscapeStrings(conn, input$edit_notes),
        dbEscapeStrings(conn, gsub("\\\\", "/", input$edit_plan_image_url)),
        dbEscapeStrings(conn, input$edit_town_bid),
        dbEscapeStrings(conn, input$edit_unique_id)
      )
      dbExecute(conn, query_main)
      
      # 2. Update Streets (Delete old, Insert new)
      dbExecute(conn, paste0("DELETE FROM dpw.street_focus WHERE plan_id = ", input$edit_unique_id))
      
      current_rows <- values$edit_dynamic_rows
      if (length(current_rows) > 0) {
        for (rid in current_rows) {
          s_name <- input[[paste0("edit_streetname_", rid)]]
          s_focus <- input[[paste0("edit_streetfocus_", rid)]]
          
          if (!is.null(s_name) && s_name != "") {
            q_s <- sprintf("INSERT INTO plan_streets (plan_id, street_name, street_focus) VALUES (%s, '%s', '%s')",
                           input$edit_unique_id, dbEscapeStrings(conn, s_name), dbEscapeStrings(conn, s_focus))
            dbExecute(conn, q_s)
          }
        }
      }
      
      removeModal()
      showNotification("Changes saved successfully!", type = "message")
      data_refresh_trigger(data_refresh_trigger() + 1)
      
    }, error = function(e) {
      showNotification(paste("Error saving:", e$message), type = "error")
    }, finally = {
      dbDisconnect(conn)
    })
  })
  
  # =========================================================================
  # --- DELETE PLAN LOGIC ---
  # =========================================================================
  
  # A. Initial Trigger (Opens Confirmation)
  observeEvent(input$delete_plan_init, {
    
    # --- SECURITY CHECK START ---
    user_role <- tolower(as.character(res_auth$role))
    if (user_role == "viewer") {
      showNotification("⛔ Permission Denied: You cannot delete records.", type = "error")
      return()
    }
    # --- SECURITY CHECK END ---
    
    showModal(modalDialog(
      title = "Confirm Deletion",
      tags$div(
        style = "color: red; font-weight: bold;",
        "Are you sure you want to permanently delete this plan?"
      ),
      tags$br(),
      "This will remove the plan and all associated streets from the database.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("delete_plan_confirm", "Yes, Delete Permanently", class = "btn-danger")
      )
    ))
  })
  
  # B. Final Confirmation (Execute Delete)
  observeEvent(input$delete_plan_confirm, {
    
    # --- SECURITY CHECK START ---
    user_role <- tolower(as.character(res_auth$role))
    if (user_role == "viewer") {
      removeModal()
      showNotification("⛔ Security Alert: Permission Denied.", type = "error")
      return()
    }
    # --- SECURITY CHECK END ---
    
    req(input$edit_unique_id)
    conn <- get_db_conn()
    id_to_del <- input$edit_unique_id
    
    tryCatch({
      dbSendQuery(conn, paste0("DELETE FROM plan_streets WHERE plan_id = ", id_to_del))
      dbSendQuery(conn, paste0("DELETE FROM plans WHERE plan_id = ", id_to_del))
      
      removeModal() 
      showModal(modalDialog(title = "Success", "Plan deleted successfully.", easyClose = TRUE, footer = modalButton("Close")))
      data_refresh_trigger(data_refresh_trigger() + 1)
      
    }, error = function(e) {
      showModal(modalDialog(title = "Error Deleting", paste("Could not delete record:", e$message), easyClose = TRUE))
    }, finally = { dbDisconnect(conn) })
  })
  
  
  # =========================================================================
  # --- CHANGE PASSWORD (SELF-SERVICE) ---
  # =========================================================================
  
  # 1. Open Change Password Modal
  observeEvent(input$change_pwd_btn, {
    req(res_auth$user_id) # Ensure user is logged in
    
    showModal(modalDialog(
      title = "Change My Password",
      passwordInput("cp_current_pass", "Current Password (Required)"),
      hr(),
      passwordInput("cp_new_pass", "New Password"),
      passwordInput("cp_confirm_pass", "Confirm New Password"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("cp_save_btn", "Update Password", class = "btn-success")
      )
    ))
  })
  
  # 2. Verify and Save New Password
  observeEvent(input$cp_save_btn, {
    req(input$cp_current_pass, input$cp_new_pass, input$cp_confirm_pass)
    
    # A. Check if new passwords match
    if (input$cp_new_pass != input$cp_confirm_pass) {
      showNotification("New passwords do not match!", type = "error")
      return()
    }
    
    # B. Database Operations
    conn <- get_db_conn()
    tryCatch({
      # 1. Fetch the CURRENT hash from DB to verify identity
      safe_user <- dbEscapeStrings(conn, res_auth$user_id)
      query <- sprintf("SELECT password_hash FROM users WHERE user_id = '%s'", safe_user)
      user_data <- dbGetQuery(conn, query)
      
      if (nrow(user_data) == 0) {
        showNotification("User not found.", type = "error")
        return()
      }
      
      # 2. Verify the OLD password matches DB hash
      is_valid_old <- tryCatch({
        scrypt::verifyPassword(as.character(user_data$password), input$cp_current_pass)
      }, error = function(e) FALSE)
      
      if (!is_valid_old) {
        showNotification("Current password is incorrect.", type = "error")
        return()
      }
      
      # 3. Hash the NEW password and Update DB
      new_hash <- scrypt::hashPassword(input$cp_new_pass)
      safe_hash <- dbEscapeStrings(conn, new_hash)
      
      update_query <- sprintf("UPDATE users SET password_hash = '%s' WHERE user_id = '%s'", 
                              safe_hash, safe_user)
      dbExecute(conn, update_query)
      
      removeModal()
      showNotification("Password updated successfully! Logging you out...", type = "message")
      
      # 4. Force Logout after password change (Security Best Practice)
      shinyjs::delay(2000, session$reload())
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    }, finally = {
      dbDisconnect(conn)
    })
  })
  
  # =========================================================================
  # --- LOGOUT LOGIC ---
  # =========================================================================
  observeEvent(input$logout_btn, {
    session$reload()
  })
  
  # Server function ends here  
}

# Define the UI Wrapper
ui_auth <- secure_app(ui, 
                      tags_top = tags$div(
                        # Conditionally show the logo or a folder icon on the login screen
                        if (logo_exists) {
                          tags$img(src = "town_logo.png", width = 100)
                        } else {
                          #icon("folder-open", class = "fa-3x", style = "color: white; margin-bottom: 15px;")
                        },
                        tags$h4("West Springfield Plan Database", style = "color: white;")
                      ),
                      background = "linear-gradient(to bottom, #2C3E50, #4CA1AF)"
)

# Call the app with the wrapped UI
shinyApp(ui_auth, server)
#############################################
# Intro to Shiny Course
# The purpose of the script 
# The author of the script
# The date the script was last updated  
#############################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  rio,             # for importing data
  here,            # for locating files
  shiny,           # for creating dashboards  
  shinycssloaders, # for css loaders
  shinydashboard,  # dashboard layouts for Shiny
  shinyWidgets,    # cool HTML widgets for Shiny
  fontawesome,     # font awesome icons
  htmltools,       # html generation tools
  plotly,          # for interactive plots
  leaflet,         # for interactive maps
  sf,              # simple features for spatial data
  epikit,          # for easy inline code
  apyramid,        # for age/sex pyramids
  DT,              # for data tables
  janitor,         # for data cleaning 
  rsconnect,       # deploy to shinyapps.io
  tidyverse        # for data management and visualization
)


# Import data ------------------------------------------------------------------
surv <- import(here("data", "clean", "surveillance_linelist_clean_20141201.rds"))
sr_shapefiles <- st_read(here("data", "shp"), 
                         layer = "sle_adm3")

# Variables --------------------------------------------------------------------

last_update_date <- format(max(surv$date_report, na.rm = TRUE), "%B %d, %Y")

# Unique districts plus "All" category
list_districts <- c("All", sort(unique(surv$district)))
list_districts <- list_districts[!is.na(list_districts)] # remove NA

# Age range
range_ages <- c(min(surv$age_years, na.rm = TRUE),
                max(surv$age_years, na.rm = TRUE))

# Date range
range_dates <- c(min(surv$date_onset, na.rm = TRUE),
                 max(surv$date_onset, na.rm = TRUE))

# Functions --------------------------------------------------------------------

# Format any number (x) with (n) decimal digits
format_num <- function(x, n = 0) {
  x[is.na(x)] = 0
  f_num <- format(round(as.numeric(x),n), nsmall=n, big.mark=",")
  return(f_num)
}

# User interface (UI) ----------------------------------------------------------
ui <- fluidPage(
  dashboardPage(
    skin = "blue",
    title = "Ebola dashboard",
    
    ## Header ------------------------------------------------------------------
    header = dashboardHeader(
      titleWidth = "250px",
      title = HTML('<img style="padding: 5px;" src="logo.png" height="50"> Ebola situation')
    ),
    
    
    ## Sidebar -----------------------------------------------------------------
    dashboardSidebar(
      width = 250,
      
      sidebarMenu( 
        id = "sidebarid",
        
        # SitRep menu item
        menuItem("Situation report",          # Section name
                 tabName = "tab_sitrep",      # Section code (must be unique)
                 icon = icon("globe-europe")  # Icon (fontawesome)
        ),
        
        # Side panel filters for tab_sitrep
        conditionalPanel('input.sidebarid == "tab_sitrep"',
                         # Sex dropdown
                         selectInput("select_sex",
                                     label = "Sex",
                                     
                                     # TEXT = VALUE
                                     choices = c("Both"="both",
                                                 "Female"="female",
                                                 "Male"="male"),
                                     selected = "Both"),
                         
                         # District dropdown
                         selectInput("select_district",
                                     label = "District",
                                     choices = list_districts,
                                     selected = "All"),
                         
                         # Age range slider 
                         sliderInput("select_age_years",
                                     label = "Age range (years)",
                                     min = range_ages[1],
                                     max = range_ages[2],
                                     value = range_ages),
                         
                         # Date range
                         dateRangeInput("dates", 
                                        label = "Date Range",
                                        start = range_dates[1],
                                        end = range_dates[2],
                                        min = range_dates[1],
                                        max = range_dates[2]
                         )
        ),

        ### About ---- 
        menuItem("About",
                 tabName = "tab_about",
                 icon = icon("info-circle")
        )
        
      )
    ),
    
    ## Body --------------------------------------------------------------------
    body = dashboardBody(
      
      ### CSS ----
      tags$head(tags$link(rel = "stylesheet",
                          type = "text/css", href = "style.css")),
      
      # Update date
      p(str_glue("Last updated: {last_update_date}"),
        style = "font-size: 12px; color: #5c5c5c"),
      
      tabItems(
        ### SitRep ---- 
        tabItem(tabName = "tab_sitrep",
                
                fluidRow(
                  column(width = 12,
                         
                         # Simple H1 HTML title
                         h1("Ebola situation in Sierra Leone"),
                         
                         # Information text box
                         box(
                           width = 12, collapsible = T, collapsed = T,
                           # status = "primary", # info, primary, success, info, warning or danger
                           title = "Data anonymization disclaimer",
                           p("The data presented in this Ebola dashboard has been carefully anonymized, ensuring that no personal identifiers are included. All datasets used are completely randomized to maintain privacy and confidentiality. ",
                             style="text-align: justify;" # CSS alignment
                           )
                         ),
                         
                         # Value boxes with general statistics
                         fluidRow(
                           column(width = 12,
                                  valueBoxOutput("box_cases",width = 4),
                                  valueBoxOutput("box_child",width = 4),
                                  valueBoxOutput("box_hosp",width = 4)
                           )
                         ),
                         
                         # Full page box epicurve
                         box(
                           width = 12, status = "primary",
                           solidHeader = TRUE, collapsible = TRUE,
                           title = "Confirmed cases epicurve",
                           checkboxInput(
                             inputId = "epicurve_months",      # Input ID
                             label = "Date of onset by month", # Label of checkbox
                             value = F                         # Initial value
                           ),
                           shinycssloaders::withSpinner(
                             plotlyOutput("surv_epicurve"),
                             color = "#6c68ab", 
                             type = "8", 
                             size = 0.5
                           )
                         ),
                         
                         # Half page box for bar plot
                         box(
                           width = 6, status = "primary", 
                           solidHeader = TRUE, collapsible = TRUE,
                           title = "Identified symptoms",
                           plotlyOutput("surv_bar")
                         ),
                         
                         # Half page box for map
                         box(
                           width = 6, status = "primary", 
                           solidHeader = TRUE, collapsible = TRUE,
                           title = "Confirmed cases by district",
                           leafletOutput("surv_map")
                         ),
                         
                         # Full page box for table
                         box(
                           width = 12, status = "primary", 
                           solidHeader = TRUE, collapsible = TRUE,
                           title = "Confirmed cases data table",
                           dataTableOutput("surv_datatable") # Datatable output
                         )
                  )
                )
        ),
        
        ### About ----
        tabItem(tabName = "tab_about",
                
                fluidRow(
                  column(width = 12,
                         # Full page boxes for about, data source and disclaimer information
                         box(width = 12,
                             h1("About the dashboard"),
                             p("The Ebola Situation Dashboard is designed to provide a comprehensive, dynamic, and interactive platform for monitoring and understanding the Ebola case study in Sierra Leone. Our primary goal is to make crucial data accessible to everyone, from healthcare professionals to the general public, to support evidence-based decision-making.",
                               style="text-align: justify;"),
                             p("By presenting real-time data, trends, and analytics, the dashboard aims to enhance awareness and facilitate informed responses to the Ebola crisis. The platform features a variety of visualization tools, including maps, graphs, and charts, allowing users to explore the data in multiple dimensions. Through this resource, we hope to contribute to more effective outbreak management and public health interventions.",
                               style="text-align: justify;")
                         ),
                         
                         box(width = 6, 
                             h1("Data sources"),
                             p("The ebola dataset used to build this dashboard comes from the Ebola case study in Sierra Leone. Linelist data was provided from 5 hospitals, population data is stratified by demographic groups and admin levels. Sierra Leone shapefiles are stratified by admin levels.",
                               style="text-align: justify;")
                         ),
                         
                         box(width = 6, 
                             h1("Disclaimer"),
                             p("The data presented on this Ebola dashboard has been carefully anonymized, ensuring that no personal identifiers are included.Please note that the information is for educational purposes only and should not be used for clinical or public health decision-making without consulting official sources.",
                               style="text-align: justify;")
                         ),
                         
                         # Download button
                         column(width = 4, offset = 4, 
                                box(width = 12,
                                    downloadButton("download_data", "Download data",icon=icon('download',style='font-size: 25px;'),style = "width:100%;")
                                )
                         )
                  )
                ),
                
                hr(),
                column(width = 12,
                       HTML('<center><img style="padding: 5px;" src="logo.png" height="50"> <i>Ebola situation dashboard</i> </center>')
                )
        )
        
      )
    )
  )
)

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  
  #### Reactive data filtering ----
  get_surv_data <- function() {
    # Create a copy the all 'surv' data
    surv_filtered <- surv
    
    # If sex is selected, then filter it.
    if (input$select_sex != "both") { # if they select 'both', don't filter
      surv_filtered <- surv_filtered %>% 
        filter(sex == input$select_sex)
    }
    
    # If a district is selected, then filter it.
    if (input$select_district != "All") {
      surv_filtered <- surv_filtered %>% filter(district == input$select_district)
    }
    
    # Filter age to be in range
    surv_filtered <- surv_filtered %>% 
      filter(age_years >= input$select_age_years[1] & age_years <= input$select_age_years[2])
    
    # Filter date range
    surv_filtered <- surv_filtered %>%
      filter(date_onset >= input$dates[1] & date_onset <= input$dates[2])
    
    return(surv_filtered)
  }
  
  #### surv DT ----
  output$surv_datatable <- renderDataTable(server=T, {
    surv_data <- get_surv_data() # Get data reactively
    
    # Select desired columns
    surv_data <- surv_data %>% select(date_onset,date_report,sex,age_years,hospital,district)
    
    # Interactive table with specific column names
    datatable(surv_data,
              
              # Specify your own column names
              colnames = c("Onset date", "Report date",
                           "Sex", "Age (years)", "Hospital", "District")
    )
  })
  
  #### Value Boxes ----
  output$box_cases <- renderValueBox({
    # Calculate reactive box value
    surv_data <- get_surv_data() # Get reactive data
    box_value <- nrow(surv_data)
    
    valueBox(
      value = format_num(box_value),     # NUMERIC VALUE
      subtitle = "Total cases reported", # LABEL
      icon = icon("head-side-virus"),    # ICON
      color = "purple"                   # COLOR
    )
  })
  
  # BOOKMARK: Percent is not correct -----
  output$box_child <- renderValueBox({
    
    # Calculate reactive box value
    surv_data <- get_surv_data() # Get filtered data
    box_value <- fmt_count(surv_data, age_years < 5)
    
    valueBox(
      value = box_value,
      subtitle = "Cases in under 5 year olds",
      icon = icon("child"),
      color = "purple"
    )
  })
  
  output$box_hosp <- renderValueBox({
    # Calculate reactive box value
    surv_data <- get_surv_data() # Get filtered data
    box_value <- surv_data %>%
      pull(hospital) %>%
      unique() %>%
      na.omit() %>%
      length() %>%
      format_num()
    
    
    valueBox(
      value = box_value,
      subtitle = "Hospitals reporting cases",
      icon = icon("hospital"),
      color = "purple"
    )
  })
  
  #### Epi curve ----
  output$surv_epicurve <- renderPlotly({
    surv_data <- get_surv_data() # Get filtered data
    
    # Remove rows without onset date
    surv_data <- surv_data %>% drop_na(date_onset)
    
    # Checkbox for months
    if (input$epicurve_months) {
      date_break_type <- "month" # If TRUE, break by month
    } else {
      date_break_type <- "week"  # if FALSE, break by week
    }
    
    ebola_breaks <- seq.Date(
      from = floor_date(min(surv_data$date_onset, na.rm=T),
                        unit = date_break_type),
      to =   ceiling_date(max(surv_data$date_onset, na.rm=T),
                          unit = date_break_type),
      by =   date_break_type)
    
    fig <- ggplot(data = surv_data, aes(x = date_onset, fill = district)) +
      geom_histogram(breaks = ebola_breaks, closed = "left") +
      scale_x_date(
        date_breaks = date_break_type,
        labels = scales::label_date_short()) +
      scale_y_continuous(n.breaks = 8,
                         expand = c(0,0)) +
      scale_fill_manual(
        values = c("Central I" = "#087F1F",
                   "Central II" = "#1F894C",
                   "East I" = "#359278",
                   "East II" = "#499E8A",
                   "East III" = "#5DA99C",
                   "Mountain Rural" = "#79B7C5",
                   "West I" = "#5AA9BC",
                   "West II" = "#428DA0",
                   "West III" = "#297183"
        ),
        na.value = "grey") +
      theme_classic() +
      labs(
        y = "Incidence",
        x = "Date of onset",
        fill = "District"
      )
    
    fig <- ggplotly(fig) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("pan2d", "select2d", "drawclosedpath",
                                        "autoScale2d", "lasso2d",
                                        "zoomIn2d", "zoomOut2d", 
                                        "toggleSpikelines"))
  })
  
  #### Symptom Bar plot ----
  output$surv_bar <- renderPlotly({
    surv_data <- get_surv_data() # Get filtered data
    
    surv_data <- surv_data %>% 
      # Select ID column and symptoms
      select(case_id,fever,chills,cough,aches,vomit) %>%
      
      # Pivot to long format
      pivot_longer(!case_id, names_to = "symptom", values_to = "present") %>%
      
      group_by(symptom) %>% 
      summarise(
        n_yes = sum(present == "yes", na.rm = T),       # Symptom present count
        n_no = sum(present == "no", na.rm = T),         # Symptom not present count
        n_valid_total = n_yes + n_no,                   # Total symptom reported cases
        present_pct = round(n_yes/n_valid_total*100,1)  # Symptom present proportion
      ) %>% 
      mutate(symptom = str_to_title(symptom))           # Title case symptom names
    
    fig <- ggplot(data = surv_data, aes(x = symptom, y = present_pct)) + 
      geom_col(fill = "#4a4780") +
      scale_y_continuous(n.breaks = 8,
                         label = scales::label_number(suffix = "%"), # Add % suffix to axis tick labels
                         expand = c(0,0)) +
      theme_classic() +
      labs(
        y = "Presence",
        x = "Symptom",
        fill = "District"
      )
  })
  
  #### Map {Leaflet} ----
  output$surv_map <- renderLeaflet({
    surv_data <- get_surv_data() # Using reactive data to build map
    
    # Calculate total cases by district (code)
    surv_data_districts <- surv_data %>% 
      group_by(admin3pcod) %>% 
      summarise(cases = n())
    
    # Join case data to district shapefiles using the admin3 code
    surv_map_data <- sr_shapefiles %>% 
      left_join(surv_data_districts, by = c("admin3Pcod"="admin3pcod")) %>% 
      drop_na(cases) # Drop districts with no cases
    
    # Create a gradient from 0 to max cases
    pal_gradient = colorNumeric(
      palette = "Purples",reverse = F,
      domain = c(0,max(surv_map_data$cases,na.rm=T))
    )
    
    # Create a dynamic HTML label for each shape (district)
    label_map <- sprintf(
      "<strong>%s</strong><br/>Confirmed cases: %s",
      surv_map_data$admin3Name, # First %s
      surv_map_data$cases       # Second %s
    ) %>% lapply(HTML)          # Convert to HTML all
    
    leaflet(data = surv_map_data) %>%
      
      # Add base map from provider
      addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
      
      # Add polygons from shapefiles and fill with gradient on cases 
      addPolygons(
        group = "Choropleth",
        fillColor = ~pal_gradient(cases), # Set color dynamically based on 'cases'
        fillOpacity = 0.8, dashArray = "", weight = 1,
        color = "black", opacity = 0.3,
        highlight = highlightOptions(     # Settings when shape hovered
          weight = 2, color = "black", opacity = 0.6,
          dashArray = "", fillOpacity = 1, bringToFront = F),
        label = label_map,                # Apply the HTML labels to each shape
        labelOptions = labelOptions(      # Label aesthetics
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto")
      ) %>%
      
      # Add map legend using the 'pal_gradient' colors and limits.
      addLegend(title = "Confirmed <br>cases", group = "Choropleth",
                pal = pal_gradient, values = c(0,max(surv_map_data$cases,na.rm=T)),
                opacity = 0.8, position = 'topright')
    
  })
  
  #### Data download -----
  output$download_data <- downloadHandler(
    filename = function() {"ebola_linelist.csv"}, # Set filename 
    content = function(file) {
      rio::export(surv, file)                # Export as CSV file
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

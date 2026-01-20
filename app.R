
# Load pacakges with library (efficient Shiny app loading)
library(tidyverse)
library(rio)
library(here)
library(DT)
library(shiny)
library(plotly)
library(openxlsx)
library(countrycode)
library(forestplot)
library(reactable)
library(htmltools)
library(stringi)
library(shinyWidgets)
library(shinyjs)
library(readr)


# Import Data ------------------------------------------------------------------

cleaned_df <- import(here("data", "app_data.csv"))

# Calculations needed for app --------------------------------------------------
GLOBAL_MAX_N <- max(as.numeric(cleaned_df$study_number_participants), na.rm = TRUE)
measure_domains <- cleaned_df %>% distinct(effect_outcome) %>% pull(effect_outcome) %>% sort()
outcome_domains <- cleaned_df %>% distinct(study_outcome_domain) %>% pull(study_outcome_domain) %>% sort()
intervention_domains <- cleaned_df %>% distinct(effect_intervention_group) %>% pull(effect_intervention_group) %>% sort()


school_level_choices <- cleaned_df %>%
  distinct(clean_grade_band) %>%
  pull(clean_grade_band) %>%
  intersect(c("K-8", "9-12", "K-12", "Unclear"))

urbanicity_choices <- cleaned_df %>%
  distinct(clean_school_area) %>%
  pull(clean_school_area) %>%
  discard(is.na)  # or .[!is.na(.)]

urbanicity_choices <- urbanicity_choices %>%
  strsplit("\\+") %>%           # split on "+"
  unlist() %>%
  trimws() %>%
  unique()

# Put "Unclear" last if present
urbanicity_choices <- c(
  sort(setdiff(urbanicity_choices, "Unclear")),
  if ("Unclear" %in% urbanicity_choices) "Unclear"
)


individual_grade_choices <- c("K", as.character(1:12), "Unclear")
individual_school_type_choices <- c("Public", "Private", "Parochial", "Unclear")


outcome_measure_family_choices <- cleaned_df %>%
  distinct(processed_outcome_measure_roots) %>% 
  pull() %>%
  unname() %>%           # <- strip the long names
  unique() %>%           # in case there are duplicates
  sort()


# intervention_group_choices <- sort(unique(merged$processed_intervention_roots))
intervention_group_choices <- sort(unique(cleaned_df$processed_intervention_roots))


# country_choices <- sort(unique(merged$country))
country_choices <- sort(unique(cleaned_df$clean_country))





# Helper functions

grade_band_match_filter <- function(band_val, selected_vals) {
  band_val %in% selected_vals
}

school_types_match_filter <- function(clean_school_type, selected_school_types) {
  if (clean_school_type %in% c("Unclear", "Cannot Tell")) {
    return(any(c("Unclear", "Cannot Tell") %in% selected_school_types))
  }
  
  # Split "Public+Private" → c("Public","Private")
  study_school_types <- unlist(strsplit(clean_school_type, "\\+"))
  any(study_school_types %in% selected_school_types)
}

school_area_match_filter <- function(clean_school_area, selected_areas) {
  if (clean_school_area %in% c("Unclear", "Cannot Tell")) {
    return(any(c("Unclear", "Cannot Tell") %in% selected_areas))
  }
  
  # Split "Urban+Suburban" → c("Urban","Suburban")
  study_areas <- unlist(strsplit(clean_school_area, "\\+"))
  study_areas <- trimws(study_areas)
  
  any(study_areas %in% selected_areas)
}


outcome_measures_match_filter_roots <- function(processed_outcome, selected_outcomes) {
  processed_outcome %in% selected_outcomes
}

grades_match_filter_row <- function(row, selected_grades) {
  # If nothing selected, don't restrict
  if (length(selected_grades) == 0) return(TRUE)
  
  # If this "row" is actually empty (0-row tibble), nothing can match
  if (nrow(row) == 0) return(FALSE)
  
  # Identify grade columns on this row
  has_k <- "study_grade_level_k" %in% names(row)
  grade_cols <- paste0("study_grade_level_", 1:12)
  grade_cols <- grade_cols[grade_cols %in% names(row)]
  
  # Numeric grades selected (1–12)
  # --- NEW: separate K and numeric grades ---
  sel_k <- "K" %in% selected_grades
  sel_numbers <- suppressWarnings(
    as.integer(selected_grades[!selected_grades %in% c("Unclear", "K")])
  )
  sel_numbers <- sel_numbers[!is.na(sel_numbers)]
  
  
  has_any_grade <- FALSE
  
  # Check if any selected numeric grade is present
  if (length(sel_numbers) > 0) {
    for (g in sel_numbers) {
      colname <- paste0("study_grade_level_", g)
      if (colname %in% names(row) && length(row[[colname]]) > 0) {
        val <- row[[colname]][1]   # use [1], not [[1]]
        if (!is.na(val) && trimws(as.character(val)) != "") {
          has_any_grade <- TRUE
          break
        }
      }
    }
  }
  
  # --- NEW: handle K explicitly ---
  if (sel_k && has_k && length(row[["study_grade_level_k"]]) > 0) {
    val_k <- row[["study_grade_level_k"]][1]
    if (!is.na(val_k) && trimws(as.character(val_k)) != "") {
      has_any_grade <- TRUE
    }
  }
  
  
  # Handle "Unclear" option
  if ("Unclear" %in% selected_grades) {
    cannot_col <- "study_grade_level_cannot_tell"
    has_cannot <- (
      cannot_col %in% names(row) &&
        length(row[[cannot_col]]) > 0 &&
        !is.na(row[[cannot_col]][1]) &&
        trimws(as.character(row[[cannot_col]][1])) != ""
    )
    
    all_grade_cols <- c(if (has_k) "study_grade_level_k", grade_cols)
    
    # Check if NO explicit grades are marked
    none_selected <- TRUE
    for (col in all_grade_cols) {
      if (col %in% names(row) && length(row[[col]]) > 0) {
        val <- row[[col]][1]
        if (!is.na(val) && trimws(as.character(val)) != "") {
          none_selected <- FALSE
          break
        }
      }
    }
    
    # If they checked "Unclear" and this row looks unclear, include it
    if (has_cannot || none_selected) {
      return(TRUE)
    }
  }
  
  return(has_any_grade)
}


# UI----------------------------------------------------------------------------
# Define UI for application
ui <- fluidPage(
  useShinyjs(),
  # HTML Customization
  tags$head(
    tags$style(
      HTML('
        .title-panel {
          text-align: left;
          padding-left: 10px; 
          font-size: 40px;
          color: #007030;
          font-weight: 700;
          font-family: "Open Sans", sans-serif;
        }
        body {
          font-family: "Source Sans", sans-serif; 
        }
        .studies-panel {
          background-color: #fff;
          padding: 16px;
          border-radius: 8px;
        }
        .studies-count {
          font-size: 60px;
          color: #007030;
          display: block;
          margin-top: 8px;
        }
        .tooltiptext {
          visibility: hidden;
          width: 320px;
          background-color: #fff;
          color: #000000;
          text-align: left;
          border-radius: 6px;
          border: 1px solid #000;
          padding: 8px;
          position: absolute;
          z-index: 1;
          top: 100%;
          left: 0;
          opacity: 0;
          transition: opacity 0.3s;
          font-size: 15px;
        }
        .studies-panel:hover .tooltiptext {
          visibility: visible;
          opacity: 1;
        }
        .reactable-table th, .reactable-table td { 
          box-sizing: border-box !important;
        }
        .reactable thead th {
          position: sticky;
          top: 0;
          background-color: #fff;
          z-index: 2;
        }
        
          .text-tooltip, .svg-tooltip {
          display: none;
          position: fixed;
          z-index: 10000;
          background: white;
          color: black;
          padding: 15px;
          border-radius: 8px;
          font-size: 13px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.3);
          border: 2px solid #333;
          pointer-events: none;
        }

      body.hover-disabled .text-tooltip,
      body.hover-disabled .svg-tooltip,
      body.hover-disabled .text-cell-hover:hover .text-tooltip,
      body.hover-disabled .svg-container:hover .svg-tooltip {
        display: none !important;
      }
    
    body, * {
  cursor: default !important;
    }

      /* Pointer cursor ONLY for interactive/filter elements */
      button, .btn, a, label[for], .selectpicker, select, input[type="checkbox"], input[type="radio"],
      .dropdown-menu, .bootstrap-select, .nav-tabs, #toggle_hover, #reset_filters, .picker, summary, details,
      .reactable .rt-th, .reactable .rt-td, .reactable .rt-tr, .reactable .rt-tbody {
        cursor: pointer !important;
      }
      
      /* Text cursor for text inputs */
      input[type="text"], textarea, input[type="search"], input[type="password"], [contenteditable] {
        cursor: text !important;
      }
      
      /* Force arrow cursor on all plotly containers and SVGs */
      .plotly, .plotly > div, .svg-container, .main-svg, .plotly-graph-div, .plot-container, g, rect, .hoverlayer, .subplot, .cartesianlayer, .layer-above, .layer-below {
        cursor: default !important;
      }
      
      /* For reactable table body and headers, default cursor unless .rt-th/.rt-td are interactive */
      .reactable, .reactable-table, .rt-table, .rt-thead, .rt-tbody, .rt-tr-group, .rt-tr {
        cursor: default !important;
      }
     /* Custom tooltip that floats on top of everything */
        .forest-tooltip {
          position: relative;
        }
        
        .forest-tooltip .tooltip-content {
          visibility: hidden;
          position: fixed;
          background: white;
          color: black;
          padding: 12px 16px;
          border-radius: 8px;
          border: 2px solid #333;
          font-size: 13px;
          line-height: 1.5;
          font-family: "Source Sans", sans-serif;
          z-index: 999999;
          box-shadow: 0 8px 24px rgba(0,0,0,0.5);
          max-width: 600px;
          min-width: 300px;
          width: auto;
          word-wrap: break-word;
          white-space: normal;
          opacity: 0;
          transition: opacity 0.2s ease-in-out;
          pointer-events: none;
        }
        
        .forest-tooltip:hover .tooltip-content {
          visibility: visible;
          opacity: 1;
        }
  ')
    ),
    tags$style(HTML(".plotly .bg { fill: transparent !important; }")), #Doesn't work to remove background from treemap
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans"),
    HTML('<!-- Google tag (gtag.js) -->
      <script async src="https://www.googletagmanager.com/gtag/js?id=G-N53SZJ2RLV"></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "G-N53SZJ2RLV");
      </script>')
  ),
  
  HTML('
<script>
document.addEventListener("DOMContentLoaded", function() {
  let currentTooltip = null;
  
  document.addEventListener("mouseover", function(e) {
    if (e.target.closest(".forest-tooltip")) {
      const tooltip = e.target.closest(".forest-tooltip").querySelector(".tooltip-content");
      if (tooltip) {
        currentTooltip = tooltip;
        updateTooltipPosition(e, tooltip);
      }
    }
  });
  
  document.addEventListener("mousemove", function(e) {
    if (currentTooltip) {
      updateTooltipPosition(e, currentTooltip);
    }
  });
  
  document.addEventListener("mouseout", function(e) {
    if (!e.target.closest(".forest-tooltip")) {
      currentTooltip = null;
    }
  });
  
  function updateTooltipPosition(e, tooltip) {
    const rect = tooltip.getBoundingClientRect();
    let x = e.clientX + 15;
    let y = e.clientY + 10;
    
    // Adjust if tooltip would go off screen
    if (x + rect.width > window.innerWidth) {
      x = e.clientX - rect.width - 0;
    }
    if (y + rect.height > window.innerHeight) {
      y = e.clientY - rect.height - 0;
    }
    
    tooltip.style.left = x + "px";
    tooltip.style.top = y + "px";
  }
});
</script>
'),
  uiOutput("outcomes_tab_triggers"),
  ## Title-----
  fluidRow(
    column(12,
           div(class = "title-panel", "Findings from School-Based Anxiety Prevention Research")
    )
  ),
  
  ## Instructions---------
  fluidRow(
    column(12,
           div(
             style = "margin-left: 10px; margin-top: 14px; font-size: 20px",
             HTML("
        Explore information for all included studies, or use the
        drop-downs to filter studies based on specific criteria. 
        Scroll below the dashboard for additional information on what's reported<br><br>
      ")
           )
    )
  ),
  # fluidRow(
  #   column(12,
  #          div(
  #            style = "margin-left: 10px; margin-top: 18px; font-size: 20px",
  #            HTML("
  #       <b>Overview:</b>This dashboard presents data from studies included in our <a href='https://github.com/HEDCO-Institute/Anxiety_prevention_overview' target='_blank'>
  #         meta-analysis of school-based anxiety prevention programs</a>. The Forest Plot tab shows
  #       information about each primary study (intervention, comparison, outcome, timing,
  #       and effect size). The Visualizations tab provides plots of descriptive summaries
  #       across studies.<br><br>
  # 
  #       <b>Instructions:</b> Explore information for all included studies, or use the
  #       drop-downs to filter studies based on specific criteria. Changing any
  #       filter will update the number of studies, as well as data in the Forest Plot and Visualizations tab.<br><br>
  # 
  #       Intervention effects will display as a dot in the middle column. Hover over to see more details.<br><br>
  # 
  #       <b>Note:</b> The total number of studies shown reflects only those included in the
  #       meta-analysis, not all studies for which we have descriptive data.
  #     ")
  #            )
  #   )
  # ),
  ## Filter dropdowns ----------------
  fluidRow(
    # Count of studies on the left
    column(2,
           uiOutput("studies_panel")
    ),
    
    # Filter area in the middle 
    column(8,
           # First row of filters
           fluidRow(
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "country_filter",
                      label = "Location",
                      choices = country_choices,
                      selected = country_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(country_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "school_level_filter",
                      label = "School Level",
                      choices = school_level_choices,
                      selected = school_level_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(school_level_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "intervention_group_filter",
                      label = "Intervention Name",
                      choices = intervention_group_choices,
                      selected = intervention_group_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(intervention_group_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    div(style = "margin-top: 22px;"),
                    pickerInput(
                      inputId = "urbanicity_filter",
                      label = "Urbanicity",
                      choices = urbanicity_choices,
                      selected = urbanicity_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(urbanicity_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
           ),
           
           # Second row of filters
           fluidRow(
             column(3,
                    div(style = "margin-top: 12px;"),
                    pickerInput(
                      inputId = "individual_grade_filter",
                      label = "Individual Grades",
                      choices = individual_grade_choices,
                      selected = individual_grade_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(individual_grade_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    div(style = "margin-top: 12px;"),
                    pickerInput(
                      inputId = "individual_school_type_filter",
                      label = "School Type",
                      choices = individual_school_type_choices,
                      selected = individual_school_type_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(individual_school_type_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    div(style = "margin-top: 12px;"),
                    pickerInput(
                      inputId = "outcome_family_filter",
                      label = "Outcome Measure",
                      choices = outcome_measure_family_choices,
                      selected = outcome_measure_family_choices,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(outcome_measure_family_choices) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
             column(3,
                    div(style = "margin-top: 12px;"),
                    pickerInput(
                      inputId = "outcome_domains",
                      label = "Outcome Domain",
                      choices = outcome_domains,
                      selected = outcome_domains,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = paste0("count > ", length(outcome_domains) - 1),
                        `count-selected-text` = "All"
                      ),
                      width = "100%"
                    )
             ),
           )
    ),
    
    ## Reset button on the right ---------------
    column(2,
           div(style = "margin-top: 35px;",
               actionButton(
                 "reset_filters", 
                 tagList(icon("times-circle"), HTML("&nbsp;Clear All Filters")), 
                 class = "btn-warning btn-sm", 
                 style = "background-color: #8ABB40 ; border-color: #8ABB40 ; color: white; font-size: clamp(10px, 1.8vw, 14px); padding: 15px 8px; white-space: nowrap; width: 90%; margin-left: -20px; height: 60px; display: flex; align-items: center; justify-content: center;"
               )
           ),
           div(style = "margin-top: 30px;",
               actionButton(
                 "toggle_hover", 
                 tagList(
                   icon("info-circle", style = "margin-right:8px;"), 
                   "Hover Information: ON"
                 ), 
                 class = "btn-info btn-sm", 
                 style = "background-color: #007030 ; 
                 border-color: #007030 ;
                 color: white;
                 font-size: clamp(10px, 1.8vw, 14px);
                 padding: 15px 8px; 
                 white-space: normal;
                 text-align: center;
                 width: 90%; margin-left: -20px; 
                 height: 60px; display: flex; align-items: center; 
                 justify-content: center;"
               )
           )
    )
  ),
  
  ## Tabs ------------------------
  tabsetPanel(
    tabPanel("Forest Plot",
             fluidRow(
               column(12,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "Standardized Mean Difference (SMD) from Anxiety Prevention Interventions"
                      ),
                      div(
                        style = "margin-left: 10px; margin-top: 6px; font-size: 16px",
                        "A negative SMD indicates an intervention benefit"
                      ),
                      div(
                        style = "margin-left: 10px; margin-top: 6px; margin-bottom: 6px; font-size: 14px",
                        "* Outcomes where effect size was originally reported as categorical in our meta-analysis (e.g., odds ratio) but re-computed to SMD for comparison in this table."
                      ),
                      reactableOutput("forest_tbl", width = "100%")
               )
             )
    ),
    
    tabPanel("Visualizations",
             fluidRow(
               # World Map
               column(6,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "Location"
                      ),
                      plotlyOutput("world_map", height="350px"),
                      uiOutput("country_note") 
               ),
               
               # School Level Bar Chart
               column(3,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "School Level"
                      ),
                      plotlyOutput("school_level", height= "420px")
               ),
               column(3, 
                      div(style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                          "Urbanicity"
                      ),
                      plotlyOutput("urbanicity", height= "370px")
               )
             ),
             
             ### Row 2
             fluidRow(
               column(2,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "No. of Schools"
                      ),
                      plotlyOutput("num_schools_plot", height = "390px")),
               column(2,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "No. of Classsrooms"
                      ),
                      plotlyOutput("num_class_plot", height = "390px")),
               column(4,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "No. of Students"
                      ),
                      plotlyOutput("num_students_tile", height = "350px")),
               column(2,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "Average Age"
                      ),
                      plotlyOutput("avg_age", height = "390px")),
               column(2,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "Female Percentages"
                      ),
                      plotlyOutput("pct_fem", height = "390px"))
             ),
             
             ### Row 3
             fluidRow(
               column(3,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "Race/Ethnicity Percentages Across Selected Studies"
                      ),
                      plotlyOutput("race_ethnicity_graph", height = "350px")),
               column(2,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "FRPL Percentages"
                      ),
                      plotlyOutput("frpl_graph", height = "390px")),
               column(2,
                      div(
                        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
                        "ELL Percentages"
                      ),
                      plotlyOutput("ell_graph", height = "390px")),
               column(5,
                      uiOutput("outcomes_navigation"),
                      plotlyOutput("outcomes_graph", height = "360px")
               )
             )
    )  
  )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Hover state reactive
  hover_enabled <- reactiveVal(TRUE)
  
  # Robust hover toggle observer
  observeEvent(input$toggle_hover, {
    current_state <- hover_enabled()
    hover_enabled(!current_state)
    if (hover_enabled()) {
      shinyjs::runjs("
      document.body.classList.remove('hover-disabled');
      $('#toggle_hover').html('<i class=\"fa fa-info-circle\" style=\"margin-right:8px;\"></i>Hover Information: ON');
    ")
    } else {
      shinyjs::runjs("
      document.body.classList.add('hover-disabled');
      $('#toggle_hover').html('<i class=\"fa fa-info-circle\" style=\"margin-right:8px;\"></i>Hover Information: OFF');
    ")
    }
  })
  
  # Filtered data reactive ----------------
  filtered_data <- reactive({
    # If ANY filter is empty, return empty data frame
    if (
      is.null(input$country_filter) || length(input$country_filter) == 0 ||
      is.null(input$school_level_filter) || length(input$school_level_filter) == 0 ||
      is.null(input$urbanicity_filter) || length(input$urbanicity_filter) == 0 ||
      is.null(input$individual_grade_filter) || length(input$individual_grade_filter) == 0 ||
      is.null(input$individual_school_type_filter) || length(input$individual_school_type_filter) == 0 ||
      is.null(input$outcome_family_filter) || length(input$outcome_family_filter) == 0 ||
      is.null(input$intervention_group_filter) || length(input$intervention_group_filter) == 0 ||
      is.null(input$outcome_domains) || length(input$outcome_domains) == 0
    ) {
      return(cleaned_df[0,])
    }
    
    data <- cleaned_df %>%
      filter(clean_grade_band %in% input$school_level_filter) %>%
      filter(study_outcome_domain %in% input$outcome_domains) %>%
      filter(clean_country %in% input$country_filter) %>%
      filter(
        vapply(
          clean_school_area,
          function(a) school_area_match_filter(a, input$urbanicity_filter),
          logical(1)
        )
      ) %>%
      filter(processed_outcome_measure_roots %in% input$outcome_family_filter) %>%
      filter(processed_intervention_roots %in% input$intervention_group_filter) %>% 
      rowwise() %>%
      filter(
        grades_match_filter_row(cur_data(), input$individual_grade_filter)
      ) %>%
      ungroup() %>%
      filter(
        vapply(
          clean_school_type,
          function(ct) school_types_match_filter(ct, input$individual_school_type_filter),
          logical(1)
        )
      )
    
    data
  })
  
  # Reset button observer -----
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "country_filter", selected = country_choices)
    updatePickerInput(session, "school_level_filter", selected = school_level_choices)
    updatePickerInput(session, "urbanicity_filter", selected = urbanicity_choices)
    updatePickerInput(session, "individual_grade_filter", selected = individual_grade_choices)
    updatePickerInput(session, "individual_school_type_filter", selected = individual_school_type_choices)
    updatePickerInput(session, "outcome_family_filter", selected = outcome_measure_family_choices)
    updatePickerInput(session, "intervention_group_filter", selected = intervention_group_choices)
    updatePickerInput(session, "outcome_domains", selected = outcome_domains)
  })
  
  
  # Improved no data plot function with better text wrapping and sizing
  create_compact_no_data_plot <- function() {
    plot_ly(type = "scatter",
            mode = "markers") %>% #ST update to silence error
      add_annotations(
        text = "No studies meet<br>selected filters",  # Shorter message
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(
          size = 11,
          color = "#666666",
          family = "Arial, sans-serif"
        ),
        align = "center",
        xanchor = "center",
        yanchor = "middle"
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          showline = FALSE,
          range = c(0, 1),
          fixedrange = TRUE
        ),
        yaxis = list(
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          showline = FALSE,
          range = c(0, 1),
          fixedrange = TRUE
        ),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        margin = list(l = 5, r = 5, t = 5, b = 5),
        autosize = TRUE,
        showlegend = FALSE
      ) %>%
      config(
        displayModeBar = FALSE,
        scrollZoom = FALSE,
        doubleClick = FALSE,
        staticPlot = TRUE,
        responsive = TRUE
      )
  }
  
  
  # Studies panel: Number of unique studies 
  output$studies_panel <- renderUI({
    n_studies <- length(unique(filtered_data()$refid))
    hover_is_on <- hover_enabled()
    div(class = "studies-panel",
        style = "padding: 8px 12px 25px 12px; border-radius: 5px; margin-top: 22px; width: 180px;",
        h4("No. of Studies", style = "margin: 0 0 5px 0;"),
        span(class = "studies-count", n_studies),
        if (hover_is_on) span(class = "tooltiptext", HTML(sprintf("Number of Studies Currently Filtered To: <b>%d</b>", n_studies)))
        # Tooltip is only rendered in the UI when hover is enabled
    )
  })
  
  # Count studies per country  
  country_counts <- reactive({
    filtered_data() %>%
      distinct(refid, clean_country) %>%
      count(clean_country)
  })
  
  # Reactive to count studies in Hong Kong and Mauritius
  hk_mauritius_counts <- reactive({
    fd <- filtered_data()
    tibble(
      clean_country = c("Hong Kong", "Mauritius"),
      count = sapply(c("Hong Kong", "Mauritius"), function(cntry) {
        n_distinct(fd$refid[fd$clean_country == cntry])
      })
    )
  })
  
  country_map_data <- reactive({
    cc <- country_counts()
    country_names <- cc$clean_country
    country_counts_vec <- cc$n
    country_iso3 <- countrycode(country_names, origin = "country.name", destination = "iso3c")
    country_name_lookup <- setNames(country_names, country_iso3)
    
    all_iso3 <- na.omit(countrycode::codelist$iso3c)
    all_iso3 <- setdiff(all_iso3, "ATA") # Remove Antarctica
    
    country_vals <- setNames(rep(0, length(all_iso3)), all_iso3)
    country_vals[country_iso3] <- country_counts_vec
    
    studied_iso3 <- names(country_vals)[country_vals > 0]
    studied_counts <- country_vals[studied_iso3]
    studied_names <- country_names[match(studied_iso3, country_iso3)]
    
    list(
      country_vals = country_vals,
      country_name_lookup = country_name_lookup,
      studied_iso3 = studied_iso3,
      studied_counts = studied_counts,
      studied_names = studied_names
    )
  })
  
  green_scale <- list(
    c(0, "#8ABB40"),
    c(0.33, "#489D46"),
    c(0.66, "#007030"),
    c(1,   "#104735")
  )
  
  green_scale_plotly <- c(
    "#8ABB40",  
    "#489D46",  
    "#007030",  
    "#104735"
  )
  
  output$world_map <- renderPlotly({
    d <- country_map_data()
    hover_is_on <- hover_enabled()  # This creates the reactive dependency
    
    plot_geo() %>%
      # Choropleth trace (country polygons)
      add_trace(
        z = ifelse(d$country_vals > 0, d$country_vals, NA),
        locations = names(d$country_vals),
        type = "choropleth",
        locationmode = "ISO-3",
        colorscale = green_scale,
        marker = list(line = list(color = 'black', width = 2)),
        zmin = 0,
        zmax = max(d$country_vals, na.rm = TRUE),
        showscale = FALSE,
        text = d$country_name_lookup[names(d$country_vals)],
        hoverinfo = if (hover_is_on) "text+z" else "none",
        hovertemplate = if (hover_is_on) paste0(
          "<span style='font-size:18px; font-family: \"Open Sans\", sans-serif;'>Country: <b>%{text}</b><br>",
          "No. of Studies: <b>%{z}</b></span><extra></extra>"
        ) else NULL,
        autocolorscale = FALSE
      ) %>%
      # Markers+Text trace (numbers over countries)
      add_trace(
        type = "scattergeo",
        mode = "markers+text",
        locations = d$studied_iso3,
        locationmode = "ISO-3",
        marker = list(
          size = 20,
          color = "black",
          opacity = 0.3
        ),
        text = d$studied_counts,
        textfont = list(
          size = 11,
          color = "white",
          family = "Open Sans, sans-serif"
        ),
        textposition = "middle center",
        showlegend = FALSE,
        hoverinfo = if (hover_is_on) "text" else "none",  # CHANGED: Make this conditional
        hovertext = if (hover_is_on) paste0(  # CHANGED: Make this conditional too
          "<span style='font-size:18px; font-family: \"Open Sans\", sans-serif;'>Country: <b>", d$studied_names, "</b><br>",
          "No. of Studies: <b>", d$studied_counts, "</b></span>"
        ) else NULL
      ) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",
          bordercolor = "black",
          font = list(
            family = "Open Sans, sans-serif",
            size = 15,
            color = "black"
          )
        ),
        geo = list(
          scope = "world",
          showland = TRUE,
          landcolor = "white",
          showocean = TRUE,
          oceancolor = "#C5DEDF",
          bgcolor = "rgb(180,205,250)",
          projection = list(type = "equirectangular"),
          lonaxis = list(range = c(-150, 150)),
          lataxis = list(range = c(-50, 70))
        ),
        margin = list(l = 20, r = 0, t = 0, b = 0)
      ) %>%
      config(
        displayModeBar = FALSE,
        scrollZoom = TRUE,
        doubleClick = FALSE
      )
  })
  
  output$country_note <- renderUI({
    counts <- hk_mauritius_counts()
    hk_count <- counts$count[counts$clean_country == "Hong Kong"]
    maur_count <- counts$count[counts$clean_country == "Mauritius"]
    
    pluralize <- function(n, name) {
      verb <- if (n == 1) "was" else "were"
      word <- if (n == 1) "study" else "studies"
      sprintf("<b>%d</b> %s %s set in <b>%s</b>", n, word, verb, name)
    }
    
    main_text <- sprintf("%s and %s.", 
                         pluralize(hk_count, "Hong Kong"),
                         pluralize(maur_count, "Mauritius"))
    
    HTML(sprintf(
      "<span style='font-size:12px; color:#444; margin-left:40px; display: block;'><b>Note:</b> Countries under 1,000 sq miles (including Hong Kong and Mauritius) are not visible on this map.<br>
     With the current filters: %s", main_text))
  })
  
  #School Level 
  output$school_level <- renderPlotly({
    hover_is_on <- hover_enabled()
    data <- filtered_data()
    if (!"clean_grade_band" %in% names(data) || nrow(data) == 0) {
      return(create_compact_no_data_plot())
    } else {
      school_level_plot_data <- data %>%
        distinct(refid, clean_grade_band) %>%
        mutate(
          clean_grade_band = factor(
            clean_grade_band,
            levels = rev(c("K-8", "9-12", "K-12", "Unclear"))
          )
        ) %>%
        group_by(clean_grade_band) %>%
        summarise(grade_n = n(), .groups = "drop") %>%
        mutate(
          selected = as.character(clean_grade_band) %in% input$school_level_filter,
          fill_color = case_when(
            selected & grade_n <= 5  ~ "#8ABB40",
            selected & grade_n <= 10 ~ "#489D46",
            selected & grade_n <= 15 ~ "#007030",
            selected                 ~ "#104735",
            !selected                ~ "#CCCCCC60"
          ),
          hover_text = paste0(
            "Grade Level: <b>", clean_grade_band, "</b><br>",
            "Number of Studies: <b>", grade_n, "</b>"
          )
        )
      
      p <- ggplot(school_level_plot_data, aes(
        x = clean_grade_band,
        y = grade_n,
        fill = I(fill_color),
        text = hover_text
      )) +
        geom_col() +
        coord_flip() +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 0, hjust = 0.5)
        ) +
        labs(x = NULL, y = NULL)
      
      ggplotly(p, tooltip = if (hover_is_on) "text" else NULL) %>%
        layout(
          margin = list(l = 5, r = 5, t = 5, b = 90),
          hoverlabel = list(bgcolor = "white", font = list(color = "black")),
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE),
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE, scrollZoom = FALSE, doubleClick = FALSE)
    }
  })
  
  
  # Urbanicity graph  
  output$urbanicity <- renderPlotly({
    hover_is_on <- hover_enabled()
    data <- filtered_data()
    if (!"clean_school_area" %in% names(data) || nrow(data) == 0) {
      return(create_compact_no_data_plot())
    } else {
      urbanicity_plot_data <- data %>%
        mutate(clean_school_area = ifelse(is.na(clean_school_area), "Unclear", clean_school_area)) %>%
        distinct(refid, clean_school_area) %>%
        mutate(
          clean_school_area = factor(
            clean_school_area,
            levels = c("Rural", "Suburban", "Urban",
                       "Rural+Suburban", "Rural+Urban", "Suburban+Urban",
                       "Rural+Suburban+Urban", "Unclear")
          )
        ) %>%
        group_by(clean_school_area) %>%
        summarise(urbanicity_n = n(), .groups = "drop") %>%
        filter(urbanicity_n > 0) %>%
        mutate(
          hover = sprintf(
            "Urbanicity: <b>%s</b><br>Number of Studies: <b>%d</b>",
            clean_school_area, urbanicity_n
          )
        )
      
      
      urbanicity_plot <- ggplot(urbanicity_plot_data, aes(
        x = clean_school_area,
        y = urbanicity_n,
        fill = urbanicity_n,
        text = hover
      )) +
        geom_col() +
        coord_flip() +
        theme_minimal() +
        scale_fill_gradientn(colors = c("#8ABB40", "#489D46", "#007030", "#104735")) +
        theme(legend.position = "none") +
        labs(x = NULL, y = NULL, fill = NULL) +
        scale_x_discrete(limits = rev)
      
      ggplotly(urbanicity_plot, tooltip = if (hover_is_on) "text" else NULL ) %>%
        layout(
          margin = list(l = 90, r = 5, t = 5, b = 5),
          hoverlabel = list(bgcolor = "white", font = list(color = "black")),
          autosize = TRUE
        ) %>%
        config(displayModeBar = FALSE, scrollZoom = FALSE, doubleClick = FALSE)
    }
  })
  
  
  # Number of Schools Graph 
  output$num_schools_plot <- renderPlotly({
    hover_is_on <- hover_enabled()
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>%
      mutate(study_number_schools = as.numeric(ifelse(study_number_schools == -999, NA, study_number_schools))) %>%
      filter(!is.na(study_number_schools)) %>%
      distinct(refid, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Schools: <b>", study_number_schools, "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(123)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(study_number_schools) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(study_number_schools, study_number_schools, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        study_number_schools_adj = study_number_schools + y_offset
      )
    
    mean_schools <- round(mean(studies_clean$study_number_schools), 1)
    y_range <- range(studies_clean$study_number_schools_adj)
    y_buffer <- diff(y_range) * 0.1
    y_limits <- c(max(0, y_range[1] - y_buffer), y_range[2] + y_buffer)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_schools, mean_schools),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_schools),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~study_number_schools_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Schools"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_schools, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  
  # Number of classrooms graph 
  output$num_class_plot <- renderPlotly({
    hover_is_on <- hover_enabled()
    data <- filtered_data()
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>%
      mutate(study_number_classrooms = as.numeric(ifelse(study_number_classrooms == -999, NA, study_number_classrooms))) %>%
      filter(!is.na(study_number_classrooms)) %>%
      distinct(refid, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Classrooms: <b>", study_number_classrooms, "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(456)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(study_number_classrooms) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(study_number_classrooms, study_number_classrooms, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        study_number_classrooms_adj = study_number_classrooms + y_offset
      )
    
    mean_classrooms <- round(mean(studies_clean$study_number_classrooms), 1)
    y_range <- range(studies_clean$study_number_classrooms_adj)
    y_buffer <- diff(y_range) * 0.1
    y_limits <- c(max(0, y_range[1] - y_buffer), y_range[2] + y_buffer)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_classrooms, mean_classrooms),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_classrooms),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~study_number_classrooms_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Classrooms"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_classrooms, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  
  # Number of students plot - UPDATED
  output$num_students_tile <- renderPlotly({
    hover_is_on <- hover_enabled()
    data <- filtered_data()
    if (nrow(data) == 0) return(create_compact_no_data_plot())
    
    studies_clean <- data %>%
      mutate(study_number_participants = as.numeric(ifelse(study_number_participants == -999, NA, study_number_participants))) %>%
      filter(!is.na(study_number_participants) & study_number_participants > 0) %>%
      distinct(refid, .keep_all = TRUE) %>%
      mutate(
        label = paste0(study_author_year, "\nn=", study_number_participants),
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>No. of Students: <b>", study_number_participants, "</b>")
      )
    
    if (nrow(studies_clean) == 0) return(create_compact_no_data_plot())
    
    plot_ly(
      data = studies_clean,
      type = "treemap",
      labels = ~label,
      values = ~study_number_participants,
      parents = rep("", nrow(studies_clean)),   # top-level tiles
      textinfo = "label",
      pathbar = list(visible = FALSE),          # <-- move here
      root = list(color = "rgba(0,0,0,0)"),   # ← remove the grey frame/background
      marker = list(
        line = list(width = 2, color = "white"),
        colors = ~study_number_participants,
        colorscale = list(c(0, "#8ABB40"), c(1, "#104735")),
        reversescale = FALSE
      ),
      hoverinfo = if (hover_is_on) "text" else "none",
      text = ~hover_text,
      tiling = list(packing = "squarify")
    ) %>%
      layout(
        margin = list(t = 0, l = 0, r = 0, b = 22),
        paper_bgcolor = "rgba(0,0,0,0)",   # <— removes the grey frame
        plot_bgcolor  = "rgba(0,0,0,0)",   # <— removes inner grey
        font = list(family = "Open Sans", size = 16),
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE) %>%
      htmlwidgets::onRender("
      function(el, x){
        el.on('plotly_treemapclick', function(){ return false; });
      }
    ")
  })
  
  
  
  # Average age graph - UPDATED
  output$avg_age <- renderPlotly({
    hover_is_on <- hover_enabled()
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }

    studies_clean <- data %>%
      mutate(study_age_average = as.numeric(ifelse(study_age_average == -999, NA, study_age_average))) %>%
      filter(!is.na(study_age_average)) %>%
      distinct(study_author_year, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Average Age: <b>", study_age_average, "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(789)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    

    studies_clean <- studies_clean %>%
      arrange(study_age_average) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(study_age_average, study_age_average, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        study_age_average_adj = study_age_average + y_offset
      )
    
    mean_age <- round(mean(studies_clean$study_age_average), 1)
    y_range <- range(studies_clean$study_age_average_adj)
    y_buffer <- diff(y_range) * 0.1
    y_limits <- c(max(0, y_range[1] - y_buffer), y_range[2] + y_buffer)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_age, mean_age),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_age),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~study_age_average_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Age, years"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_age, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  
  # Female graph - UPDATED
  output$pct_fem <- renderPlotly({
    hover_is_on <- hover_enabled()
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>%
      mutate(study_percent_female = as.numeric(ifelse(study_percent_female == -999, NA, study_percent_female))) %>%
      filter(!is.na(study_percent_female)) %>%
      distinct(refid, .keep_all = TRUE) %>%
      mutate(study_percent_female = study_percent_female * 100) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Percent Female: <b>", study_percent_female, "%", "</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(101)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(study_percent_female) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(study_percent_female, study_percent_female, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        study_percent_female_adj = study_percent_female + y_offset
      )
    
    mean_female <- round(mean(studies_clean$study_percent_female), 1)
    y_limits <- c(0, 100)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_female, mean_female),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_female, "%"),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~study_percent_female_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Percent",
          tickvals = c(0, 25, 50, 75, 100),
          ticktext = c("0%", "25%", "50%", "75%", "100%")
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_female, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  
  # Race/ethnicity graph - UPDATED
  output$race_ethnicity_graph <- renderPlotly({
    hover_is_on <- hover_enabled()
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    race_plot_data <- data %>%
      group_by(refid) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        percent_white  = as.numeric(ifelse(study_percent_white == -999, NA,  study_percent_white)),
        percent_black  = as.numeric(ifelse(study_percent_black == -999, NA,  study_percent_black)),
        percent_aian   = as.numeric(ifelse(study_percent_aian == -999, NA,   study_percent_aian)),
        percent_nhpi   = as.numeric(ifelse(study_percent_nhpi == -999, NA,   study_percent_nhpi)),
        percent_asian  = as.numeric(ifelse(study_percent_asian == -999, NA,  study_percent_asian)),
        percent_latinx = as.numeric(ifelse(study_percent_hispanic == -999, NA, study_percent_hispanic)),
        percent_other  = as.numeric(ifelse(study_percent_other == -999, NA,  study_percent_other))
      ) %>%
      summarise(
        White = mean(percent_white, na.rm = TRUE),
        Black = mean(percent_black, na.rm = TRUE),
        `American Indian/\nAlaska Native` = mean(percent_aian, na.rm = TRUE),
        `Native Hawaiian/\nPacific Islander` = mean(percent_nhpi, na.rm = TRUE),
        Asian = mean(percent_asian, na.rm = TRUE),
        Latinx = mean(percent_latinx, na.rm = TRUE),
        Other = mean(percent_other, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = everything(),
        names_to = "race_ethnicity",
        values_to = "proportion"
      ) %>%
      filter(!is.na(proportion) & is.finite(proportion)) %>%
      arrange(proportion) %>%
      mutate(
        race_ethnicity = factor(race_ethnicity, levels = race_ethnicity),
        hover = sprintf(
          "Race/Ethnicity: <b>%s</b><br>Average Proportion: <b>%.1f%%</b>",
          gsub("\n", " ", race_ethnicity), proportion * 100
        )
      )
    
    if (nrow(race_plot_data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    race_plot <- ggplot(race_plot_data, aes(
      x = race_ethnicity,
      y = proportion,
      fill = proportion,
      text = hover
    )) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      scale_fill_gradientn(colors = green_scale_plotly) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL, fill = NULL)
    
    ggplotly(race_plot, tooltip = if (hover_is_on) "text" else NULL ) %>%
      layout(
        margin = list(l = 150, r = 5, t = 5, b = 5),
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  
  # FRPL Graph - UPDATED
  output$frpl_graph <- renderPlotly({
    hover_is_on <- hover_enabled()
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>% 
      mutate(study_percent_frpl = as.numeric(ifelse(study_percent_frpl == -999, NA, study_percent_frpl))) %>%
      filter(!is.na(study_percent_frpl)) %>%
      mutate(study_percent_frpl = study_percent_frpl * 100) %>%
      distinct(refid, .keep_all = TRUE) %>%
      mutate(
        hover_text = paste0("Study: <b>", study_author_year, "</b><br>Percent FRPL: <b>", round(study_percent_frpl, 1), "%</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(202)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(study_percent_frpl) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(study_percent_frpl, study_percent_frpl, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        study_percent_frpl_adj = study_percent_frpl + y_offset
      )
    
    mean_frpl <- round(mean(studies_clean$study_percent_frpl), 1)
    y_range <- range(studies_clean$study_percent_frpl_adj)
    y_buffer <- diff(y_range) * 0.1
    y_limits <- c(max(0, y_range[1] - y_buffer), y_range[2] + y_buffer)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_frpl, mean_frpl),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_frpl, "%"),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~study_percent_frpl_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Percent FRPL (%)"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_frpl, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  
  # ELL Graph - UPDATED
  output$ell_graph <- renderPlotly({
    hover_is_on <- hover_enabled()
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    studies_clean <- data %>% 
      mutate(study_percent_ell = as.numeric(ifelse(study_percent_ell == -999, NA, study_percent_ell))) %>%
      filter(!is.na(study_percent_ell)) %>%
      filter(is.finite(study_percent_ell)) %>%
      mutate(study_percent_ell = study_percent_ell * 100) %>%
      distinct(refid, .keep_all = TRUE) %>%
      mutate(
        study_percent_ell_rounded = round(study_percent_ell, 1),
        hover_text = paste0("Study: <b>", as.character(study_author_year), "</b><br>Percent ELL: <b>", study_percent_ell_rounded, "%</b>")
      )
    
    if (nrow(studies_clean) == 0) {
      return(create_compact_no_data_plot())
    }
    
    set.seed(303)
    n_points <- nrow(studies_clean)
    x_positions <- 1.0 + runif(n_points, -0.25, 0.25)
    
    studies_clean <- studies_clean %>%
      arrange(study_percent_ell) %>%
      mutate(
        x_pos = x_positions,
        y_offset = ave(study_percent_ell, study_percent_ell, FUN = function(x) {
          if(length(x) > 1) seq(-0.1, 0.1, length.out = length(x)) else 0
        }),
        study_percent_ell_adj = study_percent_ell + y_offset
      )
    
    mean_ell <- round(mean(studies_clean$study_percent_ell, na.rm = TRUE), 1)
    if (!is.finite(mean_ell)) mean_ell <- 0
    
    y_range <- range(studies_clean$study_percent_ell_adj, na.rm = TRUE)
    y_buffer <- diff(y_range) * 0.1
    y_limits <- c(max(0, y_range[1] - y_buffer), y_range[2] + y_buffer)
    
    p <- plot_ly() %>%
      add_trace(
        x = c(0.6, 1.4),
        y = c(mean_ell, mean_ell),
        type = "scatter",
        mode = "lines",
        line = list(color = "#8ABB40", dash = "dot", width = 4),
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        text = paste0("Mean = ", mean_ell, "%"),
        showlegend = FALSE
      ) %>%
      add_trace(
        data = studies_clean,
        x = ~x_pos,
        y = ~study_percent_ell_adj,
        text = ~hover_text,
        hoverinfo = if (hover_is_on) "text" else "none",  # <-- toggles hover
        type = "scatter",
        mode = "markers",
        marker = list(size = 16, color = "#007030", opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE,
          showline = TRUE, linecolor = "black", range = c(0.6, 1.4),
          title = ""
        ),
        yaxis = list(
          showgrid = FALSE, showline = TRUE, linecolor = "black",
          range = y_limits, title = "Percent ELL (%)"
        ),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        autosize = TRUE,
        annotations = list(
          list(
            x = 1.35, y = mean_ell, xref = "x", yref = "y",
            text = "Mean", showarrow = FALSE,
            font = list(size = 14, color = "#104735", family = "Arial", bold = TRUE),
            align = "center", yshift = 18
          )
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  
  # Outcome families 
  # Add reactive value to track which plot is shown
  # Reactive value to track which plot is shown
  outcomes_page_selected <- reactiveVal(1)  # 1 for ">2 studies", 2 for "<2 studies"
  
  output$outcomes_navigation <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    # Prepare outcome families
    outcomes_base_data <- data %>%
      mutate(
        outcome_family_clean = ifelse(
          is.na(processed_outcome_measure_roots) | trimws(processed_outcome_measure_roots) == "",
          "Other/Unclear",
          trimws(processed_outcome_measure_roots)
        )
      ) %>%
      distinct(refid, outcome_family_clean)
    
    family_counts <- outcomes_base_data %>%
      group_by(outcome_family_clean) %>%
      summarise(outcome_n = n_distinct(refid), .groups = "drop")
    
    # Grouping counts for navigation
    gt2_count <- sum(family_counts$outcome_n > 2)
    lt2_count <- sum(family_counts$outcome_n <= 2)
    current_page <- outcomes_page_selected()
    
    # Navigation bar
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px",
      div(style = "width: 80px;"), # Empty div for layout spacing
      div(
        style = "margin-left: 10px; margin-top: 22px; font-size: 18px",
        if (current_page == 1) {
          paste0("Outcome Measures Used in >2 Studies")
        } else {
          paste0("Outcome Measures Used in ≤2 Studies")
        }
      ),
      div(
        if (current_page == 1 && lt2_count > 0) {
          actionButton("outcomes_next", HTML("Next &#8250;"), class = "btn btn-outline-secondary btn-sm", style = "font-size: 12px;")
        } else if (current_page == 2) {
          actionButton("outcomes_back_to_first", HTML("Previous &#8249;"), class = "btn btn-outline-secondary btn-sm", style = "font-size: 12px;")
        } else {
          span()
        }
      )
    )
  })
  
  # Navigation button observers
  observeEvent(input$outcomes_prev, {
    outcomes_page_selected(max(1, outcomes_page_selected() - 1))
  })
  observeEvent(input$outcomes_next, {
    outcomes_page_selected(2)
  })
  observeEvent(input$outcomes_back_to_first, {
    outcomes_page_selected(1)
  })
  
  output$outcomes_graph <- renderPlotly({
    hover_is_on <- hover_enabled()
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(create_compact_no_data_plot())
    }
    
    outcomes_base_data <- data %>%
      mutate(
        outcome_family_clean = ifelse(
          is.na(processed_outcome_measure_roots) | trimws(processed_outcome_measure_roots) == "",
          "Other/Unclear",
          trimws(processed_outcome_measure_roots)
        )
      ) %>%
      distinct(refid, outcome_family_clean)
    
    family_counts <- outcomes_base_data %>%
      group_by(outcome_family_clean) %>%
      summarise(outcome_n = n_distinct(refid), .groups = "drop")
    
    current_page <- outcomes_page_selected()
    
    if (current_page == 1) {
      # Show outcome families used in >2 studies
      plot_data <- family_counts %>%
        filter(outcome_n > 2) %>%
        arrange(desc(outcome_n), outcome_family_clean)
      plot_title <- "Outcome families used in >2 studies"
      y_label <- "Number of Studies"
    } else {
      # Show outcome families used in <2 studies
      plot_data <- family_counts %>%
        filter(outcome_n <= 2) %>%
        arrange(desc(outcome_n), outcome_family_clean)
      plot_title <- "Outcome families used in <2 studies"
      y_label <- "Number of Studies"
    }
    
    if (nrow(plot_data) == 0) {
      empty_plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = if(current_page == 1) "No outcome families used in >2 studies" else "No outcome families used in <2 studies",
                 size = 5, color = "gray60") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      return(ggplotly(empty_plot, tooltip = "none") %>%
               layout(showlegend = FALSE, 
                      margin = list(l = 20, r = 20, t = 20, b = 20)) %>%
               config(displayModeBar = FALSE, responsive = TRUE))
    }
    
    if (current_page == 1) {
      # Show outcome families used in >2 studies
      plot_data <- family_counts %>%
        filter(outcome_n > 2) %>%
        arrange(desc(outcome_n), outcome_family_clean)
      plot_title <- "Outcome families used in >2 studies"
      y_label <- "Number of Studies"
      y_breaks <- seq(0, max(plot_data$outcome_n, na.rm = TRUE), by = 5)
    } else {
      # Show outcome families used in <2 studies
      plot_data <- family_counts %>%
        filter(outcome_n <= 2) %>%
        arrange(desc(outcome_n), outcome_family_clean)
      plot_title <- "Outcome families used in <2 studies"
      y_label <- "Number of Studies"
      y_breaks <- seq(1, 2, by = 1)
    }
    
    plot_data <- plot_data %>%
      mutate(
        display_name = factor(outcome_family_clean, levels = outcome_family_clean),
        hover = sprintf(
          "Outcome Family: <b>%s</b><br>Studies: <b>%g</b>",
          outcome_family_clean, 
          as.numeric(outcome_n)
        )
      )
    
    outcomes_plot <- ggplot(plot_data, aes(
      x = display_name,
      y = outcome_n,
      fill = outcome_n,
      text = hover
    )) +
      geom_col() +
      coord_flip() +
      theme_minimal() +
      scale_fill_gradientn(colors = green_scale_plotly) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
      ) +
      labs(x = NULL, y = y_label) +
      scale_x_discrete(limits = rev)+
      scale_y_continuous(breaks = y_breaks)
    
    
    p <- ggplotly(outcomes_plot, tooltip = if (hover_is_on) "text" else NULL) %>%
      layout(
        margin = list(l = 150, r = 5, t = 20, b = 30),
        hoverlabel = list(bgcolor = "white", font = list(color = "black")),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    return(p)
  })
  
  ### Forest Plot Code with Hover Tooltips and Categorical Outcome Handling-----
  
  # 1. HELPER FUNCTIONS
  safe_format_list <- function(list_value, default_text = "Not specified") {
    tryCatch({
      if (is.null(list_value) || length(list_value) == 0) {
        return(default_text)
      }
      if (is.list(list_value) && length(list_value) == 1) {
        list_value <- list_value[[1]]
      }
      if (length(list_value) == 0 || all(is.na(list_value))) {
        return(default_text)
      }
      clean_values <- as.character(list_value[!is.na(list_value)])
      if (length(clean_values) == 0) {
        return(default_text)
      }
      return(paste(clean_values, collapse = ", "))
    }, error = function(e) {
      return(default_text)
    })
  }
  
  format_viz_value <- function(value, is_percentage = FALSE, default_text = "Not specified") {
    tryCatch({
      if (is.null(value) || length(value) == 0) {
        return(default_text)
      }
      if (is.na(value) || value == -999 || value == "" || value == "NA") {
        return(default_text)
      }
      if (is_percentage && is.numeric(value)) {
        return(paste0(round(value * 100, 1), "%"))
      }
      if (is.numeric(value)) {
        return(as.character(round(value, 1)))
      }
      return(as.character(value))
    }, error = function(e) {
      return(default_text)
    })
  }
  
  format_race_ethnicity_breakdown <- function(white, black, aian, nhpi, asian, latinx, other) {
    tryCatch({
      race_components <- c()
      if (!is.na(white) && white != -999 && white > 0) {
        race_components <- c(race_components, paste0("White: ", round(white * 100, 1), "%"))
      }
      if (!is.na(black) && black != -999 && black > 0) {
        race_components <- c(race_components, paste0("Black: ", round(black * 100, 1), "%"))
      }
      if (!is.na(aian) && aian != -999 && aian > 0) {
        race_components <- c(race_components, paste0("AIAN: ", round(aian * 100, 1), "%"))
      }
      if (!is.na(nhpi) && nhpi != -999 && nhpi > 0) {
        race_components <- c(race_components, paste0("NHPI: ", round(nhpi * 100, 1), "%"))
      }
      if (!is.na(asian) && asian != -999 && asian > 0) {
        race_components <- c(race_components, paste0("Asian: ", round(asian * 100, 1), "%"))
      }
      if (!is.na(latinx) && latinx != -999 && latinx > 0) {
        race_components <- c(race_components, paste0("Latinx: ", round(latinx * 100, 1), "%"))
      }
      if (!is.na(other) && other != -999 && other > 0) {
        race_components <- c(race_components, paste0("Other: ", round(other * 100, 1), "%"))
      }
      if (length(race_components) == 0) {
        return("Not specified")
      }
      return(paste(race_components, collapse = ", "))
    }, error = function(e) {
      return("Not specified")
    })
  }
  
  format_processed_grades <- function(processed_grades) {
    if (is.null(processed_grades) || is.na(processed_grades) || processed_grades == "" || processed_grades == "Unclear") {
      return("Not specified")
    }
    grades <- unlist(strsplit(as.character(processed_grades), ","))
    return(paste(grades, collapse = ", "))
  }
  
  # Function to create cell with hover tooltip for original content
  create_cell_with_hover <- function(display_text, original_text, column_name, row_index) {
    cell_id <- paste0("cell_", column_name, "_", row_index)
    
    # If display text is empty/blank but original text exists, show tooltip
    if ((is.null(display_text) || display_text == "" || is.na(display_text)) && 
        !is.null(original_text) && original_text != "" && !is.na(original_text)) {
      
      return(sprintf(
        '<div class="hover-cell" id="%s" data-original="%s" style="width: 100%%; height: 100%%; cursor: pointer; position: relative;">
        <span class="display-text">%s</span>
        <div class="cell-tooltip" style="display: none; position: fixed; z-index: 10000; background: white; color: black; padding: 10px; border-radius: 6px; font-size: 13px; max-width: 400px; box-shadow: 0 4px 8px rgba(0,0,0,0.2); white-space: normal; line-height: 1.4; border: 1px solid #ccc; pointer-events: none;">%s</div>
      </div>',
        cell_id,
        htmltools::htmlEscape(as.character(original_text)),
        htmltools::htmlEscape(as.character(display_text)),
        htmltools::htmlEscape(as.character(original_text))
      ))
    } else {
      # Regular cell without hover
      return(htmltools::htmlEscape(as.character(display_text)))
    }
  }
  
  # 2. HIERARCHICAL BLANKING FUNCTION
  hierarchical_blanker <- function(cleaned_df, group_cols) {
    n <- nrow(cleaned_df)
    if (n < 2) return(cleaned_df)
    original <- cleaned_df
    cleaned_df$border_top <- FALSE
    
    for (i in 2:n) {
      first_diff_col <- NA
      for (col_idx in seq_along(group_cols)) {
        col <- group_cols[col_idx]
        if (!identical(original[[col]][i], original[[col]][i-1])) {
          first_diff_col <- col_idx
          break
        }
      }
      
      if (!is.na(first_diff_col) && first_diff_col == 1) {
        cleaned_df$border_top[i] <- TRUE
      }
      
      if (is.na(first_diff_col)) {
        for (col_idx in seq_along(group_cols)) {
          col <- group_cols[col_idx]
          cleaned_df[[col]][i] <- ""
        }
      } else {
        if (first_diff_col > 1) {
          for (col_idx in 1:(first_diff_col-1)) {
            col <- group_cols[col_idx]
            cleaned_df[[col]][i] <- ""
          }
        }
      }
    }
    cleaned_df
  }
  
  # 3. ROW HEIGHT CALCULATION
  calculate_dynamic_row_height <- function(merged_forest) {
    if (nrow(merged_forest) == 0) return(80)
    
    text_complexity <- sapply(1:nrow(merged_forest), function(i) {
      text_lengths <- c(
        nchar(as.character(merged_forest$`Study Author Year`[i])),
        nchar(as.character(merged_forest$Intervention[i])),
        nchar(as.character(merged_forest$Comparison[i])),
        nchar(as.character(merged_forest$`Outcome Measure`[i]))
      )
      weighted_lengths <- text_lengths / c(20, 25, 20, 30)
      max(weighted_lengths, na.rm = TRUE)
    })
    
    base_height <- 80
    content_factor <- pmax(1, text_complexity)
    heights <- base_height + (content_factor - 1) * 25
    heights <- pmax(80, pmin(heights, 200))
    
    return(round(mean(heights)))
  }
  
  # 4. FOREST SVG FUNCTION 
  make_forest_svg <- function(yi, lower, upper, n, max_n = NULL, row_height = 70, show_axis = FALSE, tooltip_html = NULL, hover_enabled = TRUE) {
    min_x <- -3.5; max_x <- 3.5
    ref_width <- 500
    svg_height <- if (show_axis) 48 else row_height
    center_y <- svg_height / 2
    
    bubble_radius <- function(n, min_r = 2, max_r = 8) {
      # convert both to numeric, safe + minimal
      n     <- suppressWarnings(as.numeric(n))
      max_n <- suppressWarnings(as.numeric(max_n))
      
      if (is.na(n) || n <= 0 || is.na(max_n) || max_n == 0) return(min_r)
      prop <- sqrt(n / max_n)
      r <- min_r + (max_r - min_r) * prop
      return(r)
    }
    r <- bubble_radius(n)
    
    scale <- function(x) ref_width * (x - min_x) / (max_x - min_x)
    
    axis_svg <- if (show_axis) {
      axis_y <- 25
      tick_label_y <- 45
      axis_label_y <- 10
      ticks <- seq(-3, 3, by = 1)
      tick_x <- scale(ticks)
      axis_g <- htmltools::tagList(
        htmltools::tags$line(
          x1 = scale(min_x), x2 = scale(max_x), y1 = axis_y, y2 = axis_y,
          stroke = "#444", "stroke-width" = 2
        ),
        lapply(seq_along(tick_x), function(i) {
          htmltools::tags$g(
            htmltools::tags$line(
              x1 = tick_x[i], x2 = tick_x[i], y1 = axis_y, y2 = axis_y + 8,
              stroke = "#444", "stroke-width" = 2
            ),
            htmltools::tags$text(
              x = tick_x[i], y = tick_label_y,
              text.anchor = "middle", font.size = 13, font.family = "Arial",
              fill = "#333", font.weight = "bold", ticks[i]
            )
          )
        }),
        htmltools::tags$text(
          x = ref_width / 2, y = axis_label_y,
          "Standardized Mean Difference",
          font.size = 14, font.family = "Arial", text.anchor = "middle",
          fill = "#333", font.weight = "bold"
        )
      )
      htmltools::tags$g(axis_g)
    } else {
      NULL
    }
    
    forest_geom <- NULL
    if (!is.na(yi) && !is.na(lower) && !is.na(upper) && !is.na(n)) {
      forest_geom <- list(
        htmltools::tags$line(
          x1 = scale(lower), x2 = scale(upper), y1 = center_y, y2 = center_y,
          stroke = "#333", "stroke-width" = 2
        ),
        htmltools::tags$circle(
          cx = scale(yi), cy = center_y, r = r,
          fill = ifelse(yi < -0.03, "#8ABB40",
                        ifelse(yi > 0.03, "#8D1D58", "#B0B0B0")),
          stroke = "#222", "stroke-width" = 1
        )
      )
    }
    
    svg_content <- as.character(
      htmltools::tags$svg(
        width = "100%",
        height = svg_height,
        viewBox = sprintf("0 0 %d %d", ref_width, svg_height),
        preserveAspectRatio = "xMidYMid meet",
        htmltools::tags$rect(x=0, y=0, width=ref_width, height=svg_height, fill="white"),
        forest_geom,
        htmltools::tags$line(x1 = scale(0), x2 = scale(0), y1 = 0, y2 = svg_height, stroke = "#888", "stroke-dasharray" = "2,2", "stroke-width" = 1),
        axis_svg
      )
    )
    
    if (!is.null(tooltip_html) && tooltip_html != "" && hover_enabled) {
      return(sprintf(
        '<div class="forest-tooltip" style="cursor: pointer; height: 100%%; position: relative;">
        %s
        <div class="tooltip-content" id="tooltip-%s">%s</div>
      </div>', 
        svg_content, 
        sample(1:999999, 1),
        tooltip_html
      ))
    } else {
      return(svg_content)
    }
  }
  
  # 5. FILTERED DATA REACTIVE
  filtered_merged_forest <- reactive({
    filtered <- filtered_data()
    if (nrow(filtered) == 0) {
      tibble(
        `Study Author Year` = character(),
        `Intervention` = character(),
        `Comparison` = character(),
        `Outcome Measure` = character(),
        `Weeks` = character(),
        `n` = numeric(),
        `SMD` = numeric(),
        lower = numeric(),
        upper = numeric(),
        clean_grade_band = list(),
        clean_school_type = list(),
        outcome_measure = character(),
        effect_outcome_type = character()
      )
    } else {
      se <- sqrt(filtered$vi)
      filtered$lower <- filtered$yi - 1.96 * se
      filtered$upper <- filtered$yi + 1.96 * se
      
      
      filtered_forest <- filtered %>%
        transmute(
          `Study Author Year` = study_author_year,
          Intervention         = effect_intervention_group,
          Comparison           = effect_comparison_group,
          `Outcome Domain`     = study_outcome_domain,
          `Outcome Measure`    = effect_outcome,
          Weeks                = as.character(effect_timing),
          
          # Effect size and N (all numeric)
          n                    = study_number_participants,
          SMD                  = round(yi, 3),
          # If you already computed lower/upper earlier, keep them.
          # If not, you can compute them here:
          lower                = yi - 1.96 * sqrt(vi),
          upper                = yi + 1.96 * sqrt(vi),
          
          # Aggregation type
          effect_outcome_type  = effect_outcome_type,
          overall_rating = overall_rating,
          
          # Cleaned study-level descriptors
          clean_grade_band           = clean_grade_band,      # K-8 / 9-12 / K-12 / Unclear
          clean_school_type          = clean_school_type,     # e.g., "Public", "Public+Private"
          clean_school_area           = clean_school_area,     # e.g., "Urban", "Rural+Suburban"
          clean_country              = clean_country,
          
          # Counts
          study_number_schools       = if ("study_number_schools"    %in% names(filtered)) study_number_schools    else NA_real_,
          study_number_classrooms    = if ("study_number_classrooms" %in% names(filtered)) study_number_classrooms else NA_real_,
          study_number_participants  = if ("study_number_participants" %in% names(filtered)) study_number_participants else NA_real_,
          
          # Demographics
          study_average_age          = if ("study_age_average"    %in% names(filtered)) study_age_average    else NA_real_,
          study_percent_female       = if ("study_percent_female" %in% names(filtered)) study_percent_female else NA_real_,
          study_percent_frpl         = if ("study_percent_frpl"   %in% names(filtered)) study_percent_frpl   else NA_real_,
          study_percent_ell          = if ("study_percent_ell"    %in% names(filtered)) study_percent_ell    else NA_real_,
          
          study_percent_white        = if ("study_percent_white"  %in% names(filtered)) study_percent_white  else NA_real_,
          study_percent_black        = if ("study_percent_black"  %in% names(filtered)) study_percent_black  else NA_real_,
          study_percent_aian         = if ("study_percent_aian"   %in% names(filtered)) study_percent_aian   else NA_real_,
          study_percent_nhpi         = if ("study_percent_nhpi"   %in% names(filtered)) study_percent_nhpi   else NA_real_,
          study_percent_asian        = if ("study_percent_asian"  %in% names(filtered)) study_percent_asian  else NA_real_,
          study_percent_hispanic       = if ("study_percent_hispanic" %in% names(filtered)) study_percent_hispanic else NA_real_,
          study_percent_other        = if ("study_percent_other"  %in% names(filtered)) study_percent_other  else NA_real_
        )
      
      filtered_forest
    }
  })
  
  # 6. MAIN FOREST PLOT OUTPUT
  output$forest_tbl <- renderReactable({
    merged_forest <- filtered_merged_forest()
    max_n <- GLOBAL_MAX_N
    hover_state <- hover_enabled()
    
    # If no data, return empty reactable with message
    if (nrow(merged_forest) == 0) {
      empty_data <- tibble(
        Message = "No studies meet selected filters."
      )
      return(
        reactable(
          empty_data,
          columns = list(
            Message = colDef(
              name = "",
              style = list(
                textAlign = "center",
                fontSize = "1.5em",
                color = "#333",
                padding = "60px"
              ),
              headerStyle = list(display = "none")
            )
          ),
          showPagination = FALSE,
          style = list(
            border = "none"
          ),
          bordered = FALSE,
          highlight = FALSE
        )
      )
    }
    else {
      merged_forest$border_top <- FALSE
      
      merged_forest <- merged_forest %>%
        dplyr::mutate(Weeks_sort = suppressWarnings(as.numeric(Weeks)))
      
      group_cols <- c("Study Author Year", "Intervention", "Comparison", "Outcome Domain", "Outcome Measure", "Weeks_sort")
      if (nrow(merged_forest) > 1) {
        merged_forest <- merged_forest[do.call(order, merged_forest[group_cols]), ]
      }
      
      original_forest_pre_blank <- merged_forest
      merged_forest <- hierarchical_blanker(merged_forest, group_cols)
      
      merged_forest <- merged_forest %>% dplyr::select(-Weeks_sort)
      
      row_height <- calculate_dynamic_row_height(merged_forest)
      
      ################################################################################
      ### CATEGORICAL OUTCOME HANDLING - START (SECTION 1 of 3)
      ################################################################################
      is_categorical <- original_forest_pre_blank$effect_outcome_type == "Categorical"
      
      ################################################################################
      ### CATEGORICAL OUTCOME HANDLING - END (SECTION 1 of 3)
      ################################################################################
      
      # Create tooltips for SVG
      tooltip_html_texts <- sapply(1:nrow(merged_forest), function(i) {
        tryCatch({
          orig_data <- original_forest_pre_blank[i, ]
          
          ################################################################################
          ### CATEGORICAL OUTCOME HANDLING - START (SECTION 2 of 3)
          ### For categorical outcomes, show tooltip but replace SMD/CI/Effect Size with *
          ################################################################################
          is_categorical_outcome <- !is.na(orig_data$effect_outcome_type) && 
            orig_data$effect_outcome_type == "Categorical"
          
            # Continuous outcome: normal processing
            if (is.na(orig_data$SMD) || orig_data$SMD == "" || is.na(orig_data$n)) {
              return("")
            }
            
            smd_val <- as.numeric(orig_data$SMD)
            smd_display <- sprintf("%.3f", smd_val)
            ci_display <- paste0("[", round(orig_data$lower, 3), ", ", round(orig_data$upper, 3), "]")
            
            # If categorical, annotate with *
            if (is_categorical_outcome) {
              smd_display <- paste0(smd_display, "*")
              ci_display  <- paste0(ci_display, " *")
            }
            
            effect_size_display <- if (abs(smd_val) >= 2) {
              "Very Large"
            } else if (abs(smd_val) >= 1) {
              "Large"
            } else if (abs(smd_val) >= 0.5) {
              "Medium"
            } else if (abs(smd_val) >= 0.2) {
              "Small"
            } else {
              "Very Small/Null"
            }
          
          ################################################################################
          ### CATEGORICAL OUTCOME HANDLING - END (SECTION 2 of 3)
          ################################################################################
          
          grades_display <- format_processed_grades(orig_data$clean_grade_band)
          school_types_display <- safe_format_list(orig_data$clean_school_type)
          
          race_ethnicity_display <- format_race_ethnicity_breakdown(
            orig_data$study_percent_white, 
            orig_data$study_percent_black, 
            orig_data$study_percent_aian,
            orig_data$study_percent_aian, 
            orig_data$study_percent_asian, 
            orig_data$study_percent_hispanic,
            orig_data$study_percent_other
          )
          
          tooltip_parts <- c(
            paste0("SMD: <b>", smd_display, "</b>"),
            paste0("95% CI: <b>", ci_display, "</b>"),
            #paste0("Effect Size: <b>", effect_size_display, "</b>"), #removing until we have agreed magnitude rules
            paste0("Risk of Bias: <b>", orig_data$overall_rating, "</b>")
          )
          
          # Add note for categorical rows
          if (is_categorical_outcome) {
            tooltip_parts <- c(
              tooltip_parts,
              "",
              "<i>Note: This effect was originally reported as categorical but re-computed as an SMD.</i>"
            )
          }
          
          tooltip_parts <- c(
            tooltip_parts,
            #paste0("Effect Size: <b>", effect_size_display, "</b>"),
            "",
            paste0("Country: <b>", format_viz_value(orig_data$clean_country), "</b>"),
            paste0("School Level: <b>", format_viz_value(orig_data$clean_grade_band), "</b>"),
            paste0("Urbanicity: <b>", format_viz_value(orig_data$clean_school_area), "</b>"),
            paste0("Individual Grades: <b>", grades_display, "</b>"),
            paste0("School Type: <b>", school_types_display, "</b>"),
            paste0("No. of Schools: <b>", format_viz_value(orig_data$study_number_schools), "</b>"),
            paste0("No. of Classrooms: <b>", format_viz_value(orig_data$study_number_classrooms), "</b>"),
            paste0("No. of Students: <b>", format_viz_value(orig_data$study_number_participants), "</b>"),
            paste0("Average Age: <b>", format_viz_value(orig_data$study_average_age), "</b>"),
            paste0("% Female: <b>", format_viz_value(orig_data$study_percent_female, TRUE), "</b>"),
            paste0("% FRPL: <b>", format_viz_value(orig_data$study_percent_frpl, TRUE), "</b>"),
            paste0("% ELL: <b>", format_viz_value(orig_data$study_percent_ell, TRUE), "</b>"),
            paste0("Race/Ethnicity: <b>", race_ethnicity_display, "</b>")
            
          )
          
          return(paste(tooltip_parts, collapse = "<br>"))
          
        }, error = function(e) {
          return(paste0("Study: <b>", as.character(orig_data$`Study Author Year`), "</b><br>Error loading details"))
        })
      }, USE.NAMES = FALSE)
      
      svg_list <- mapply(
        make_forest_svg,
        merged_forest$SMD, merged_forest$lower, merged_forest$upper, merged_forest$n,
        max_n, row_height, FALSE, tooltip_html_texts, hover_state,
        SIMPLIFY = FALSE
      )
      
      # ################################################################################
      # ### CATEGORICAL OUTCOME HANDLING - START (SECTION 3 of 3)
      # ### Replace forest plots with "*" for categorical outcomes BUT keep tooltips
      # ################################################################################
      # for (i in which(is_categorical)) {
      #   # Get the tooltip HTML for this row
      #   tooltip_content <- tooltip_html_texts[i]
      #   
      #   # Create a full-cell div with * that includes hover tooltip functionality
      #   if (!is.null(tooltip_content) && tooltip_content != "" && hover_state) {
      #     svg_list[[i]] <- sprintf(
      #       '<div class="forest-tooltip" style="width: 100%%; height: 100%%; display: flex; align-items: center; justify-content: center; cursor: pointer; position: relative;">
      #   <span style="font-size: 24px; font-weight: bold;">*</span>
      #   <div class="tooltip-content" id="tooltip-%s">%s</div>
      # </div>',
      #       sample(1:999999, 1),
      #       tooltip_content
      #     )
      #   } else {
      #     # No tooltip - just show the asterisk
      #     svg_list[[i]] <- '<div style="width: 100%; height: 100%; display: flex; align-items: center; justify-content: center;"><span style="font-size: 24px; font-weight: bold;">*</span></div>'
      #   }
      # }
      
      # merged_forest$` ` <- as.list(svg_list)
      # 
      # # NOW convert SMD to character and replace categorical with "*"
      # merged_forest$SMD <- as.character(merged_forest$SMD)
      # merged_forest$SMD[is_categorical] <- "*"      
      ################################################################################
      ### CATEGORICAL OUTCOME HANDLING - END (SECTION 3 of 3)
      ################################################################################
      
      merged_forest$` ` <- as.list(svg_list)
      
      # Make a display version of SMD with * for categorical
      merged_forest$SMD_display <- ifelse(
        is_categorical & !is.na(merged_forest$SMD),
        paste0(sprintf("%.3f", merged_forest$SMD), "*"),
        sprintf("%.3f", merged_forest$SMD)
      )
      
      display_cols <- c(
        "Intervention",
        "Outcome Domain",
        "Weeks",
        " ",
        "SMD",
        "Outcome Measure",
        "Comparison",
        "Study Author Year",
        "border_top"
      )
      
      merged_forest <- merged_forest[, display_cols, drop = FALSE]
      merged_forest <- tibble::as_tibble(merged_forest)
    }
    
    # Reactable with hover-enabled cells
    # The reactable definition should end properly
    rt <- reactable(
      merged_forest,
      columns = list(
        `Intervention` = colDef(
          name = "Intervention",
          minWidth = 220,
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            original_value <- original_forest_pre_blank$Intervention[index]
            create_cell_with_hover(value, original_value, "intervention", index)
          },
          style = function(value, index) {
            style_list <- list(
              whiteSpace = "normal",
              wordWrap = "break-word",
              lineHeight = "1.4",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "top"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        
        `Outcome Domain` = colDef(
          name = "Outcome Domain",
          minWidth = 120,
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            original_value <- original_forest_pre_blank$`Outcome Domain`[index]
            create_cell_with_hover(value, original_value, "domain", index)
          },
          style = function(value, index) {
            style_list <- list(
              whiteSpace = "normal",
              wordWrap = "break-word",
              lineHeight = "1.4",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "top"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        
        `Weeks` = colDef(
          name = "Weeks Post-Intervention",
          width = 95,
          sortable = FALSE,
          style = function(value, index) {
            style_list <- list(
              textAlign = "center",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "middle"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        
        ` ` = colDef(
          html = TRUE,
          minWidth = 400,
          maxWidth = 800,
          sortable = FALSE,
          style = function(value, index) {
            style_list <- list(
              padding = "8px",
              verticalAlign = "middle"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          header = htmltools::HTML(
            make_forest_svg(
              NA, NA, NA, NA, max_n, 400, show_axis = TRUE, tooltip_html = NULL, hover_enabled = FALSE
            )
          ),
          headerStyle = list(
            borderBottom = "3px solid #333",
            borderTop = "3px solid #333",
            padding = "0px",
            verticalAlign = "top",
            background = "#fff"
          )
        ),
        
        `SMD` = colDef(
          name = "SMD",
          width = 80,
          sortable = FALSE,
          style = function(value, index) {
            style_list <- list(
              textAlign = "center",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "middle"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        
        `Outcome Measure` = colDef(
          name = "Outcome Measure",
          minWidth = 200,
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            display_value <- value
            if (!is.null(display_value) && !is.na(display_value) && display_value != "" &&
                !is.null(is_categorical) && length(is_categorical) >= index && is_categorical[index]) {
              display_value <- paste0(display_value, " *")
            }
            original_value <- original_forest_pre_blank$`Outcome Measure`[index]
            create_cell_with_hover(display_value, original_value, "measure", index)
          },
          style = function(value, index) {
            style_list <- list(
              whiteSpace = "normal",
              wordWrap = "break-word",
              lineHeight = "1.4",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "top"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        
        `Comparison` = colDef(
          name = "Comparison",
          minWidth = 120,
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            original_value <- original_forest_pre_blank$Comparison[index]
            create_cell_with_hover(value, original_value, "comparison", index)
          },
          style = function(value, index) {
            style_list <- list(
              whiteSpace = "normal",
              wordWrap = "break-word",
              lineHeight = "1.4",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "top"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        
        `Study Author Year` = colDef(
          name = "Study Author Year",
          minWidth = 130,
          sortable = FALSE,
          html = TRUE,
          cell = function(value, index) {
            original_value <- original_forest_pre_blank$`Study Author Year`[index]
            create_cell_with_hover(value, original_value, "study", index)
          },
          style = function(value, index) {
            style_list <- list(
              whiteSpace = "normal",
              wordWrap = "break-word",
              lineHeight = "1.4",
              padding = "12px",
              fontSize = "13px",
              verticalAlign = "top"
            )
            if (!is.null(merged_forest$border_top) && length(merged_forest$border_top) >= index && merged_forest$border_top[index]) {
              style_list$borderTop <- "2px solid #ccc"
            }
            style_list
          },
          headerStyle = list(borderBottom = "3px solid #333", borderTop = "3px solid #333")
        ),
        
        border_top = colDef(show = FALSE)
      ),
      bordered = TRUE,
      highlight = TRUE,
      resizable = FALSE,
      style = list(
        fontFamily = "Arial, sans-serif",
        fontSize = "13px",
        height = "calc(100vh - 315px)",
        overflowY = "auto",
        width = "100%"
      ),
      rowStyle = function(index) {
        list(
          height = paste0(row_height, "px"),
          minHeight = "80px"
        )
      },
      fullWidth = TRUE,
      defaultPageSize = nrow(merged_forest)
    )
    
    # Add JavaScript for hover functionality
    shinyjs::runjs('
  // Add CSS styles for hover tooltips
  if (!document.getElementById("forest-hover-styles")) {
    const style = document.createElement("style");
    style.id = "forest-hover-styles";
    style.textContent = `
      .hover-cell {
        width: 100% !important;
        height: 100% !important;
        position: relative;
        padding: 8px;
        box-sizing: border-box;
      }
      
      .cell-tooltip {
        display: none !important;
        position: fixed !important;
        z-index: 10000 !important;
        background: white !important;
        color: black !important;
        padding: 15px !important;
        border-radius: 8px !important;
        font-size: 13px !important;
        min-width: 200px !important;
        max-width: 500px !important;
        box-shadow: 0 4px 12px rgba(0,0,0,0.3) !important;
        white-space: normal !important;
        line-height: 1.5 !important;
        border: 2px solid #333 !important;
        pointer-events: none !important;
        word-wrap: break-word !important;
      }
      
      body:not(.hover-disabled) .hover-cell:hover {
        cursor: pointer !important;
      }
      
      body:not(.hover-disabled) .hover-cell:hover .cell-tooltip {
        display: block !important;
      }
      
      body.hover-disabled .hover-cell {
        cursor: default !important;
      }
      
      body.hover-disabled .cell-tooltip {
        display: none !important;
      }
    `;
    document.head.appendChild(style);
  }
  
  function positionTooltip(tooltip, event) {
    const viewportWidth = window.innerWidth;
    const viewportHeight = window.innerHeight;
    
    tooltip.style.visibility = "hidden";
    tooltip.style.display = "block";
    tooltip.style.left = "-9999px";
    tooltip.style.top = "-9999px";
    
    const rect = tooltip.getBoundingClientRect();
    const tooltipWidth = rect.width;
    const tooltipHeight = rect.height;
    
    let x = event.clientX + 10;
    let y = event.clientY - tooltipHeight - 10;
    
    if (x + tooltipWidth > viewportWidth) {
      x = event.clientX - tooltipWidth - 10;
    }
    
    if (x < 10) {
      x = 10;
    }
    
    if (y < 10) {
      y = event.clientY + 20;
    }
    
    if (y + tooltipHeight > viewportHeight) {
      y = viewportHeight - tooltipHeight - 10;
    }
    
    tooltip.style.left = x + "px";
    tooltip.style.top = y + "px";
    tooltip.style.visibility = "visible";
  }
  
  document.addEventListener("mousemove", function(event) {
    if (!document.body.classList.contains("hover-disabled")) {
      document.querySelectorAll(".cell-tooltip").forEach(function(tooltip) {
        const computedStyle = window.getComputedStyle(tooltip);
        if (computedStyle.display === "block") {
          const parent = tooltip.closest(".hover-cell");
          if (parent && parent.matches(":hover")) {
            positionTooltip(tooltip, event);
          }
        }
      });
    }
  });
')
    
    return(rt)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)


# # Deploying the app
# rsconnect::deployApp()




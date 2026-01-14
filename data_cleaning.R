# Data Cleaning for Anxiety Descriptive Dashboard 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, janitor, xml2, metafor, rmarkdown)

# Import Data ------------------------------------------------------------------
## Study characteristics ------------------------------------------------------
d_study <- import(here("data", "APO_study_level.xlsx")) %>% 
  janitor::clean_names()

# Import and clean ROB data
d_rob <- import(here("data", "APO_study_ROB.xlsx")) %>% 
  janitor::clean_names() %>% 
  select(refid, contains("overall"), -contains("explanation")) %>% 
  mutate(
    overall_rating = coalesce(
      na_if(robins_overall_judgment, ""),
      na_if(crob_overall_judgment, ""),
      na_if(irob_overall_decision, "")
    )
  ) %>% 
  select(refid, overall_rating)

## Meta-Analysis data files ----------------------------------------------------
d_anxiety <- import(here("data", "Anxiety_Symptoms.xlsx"))
d_depression <- import(here("data", "Depression_Symptoms.xlsx"))
d_wellbeing <- import(here("data", "Well_being.xlsx"))
d_adiagnosis <- import(here("data", "Anxiety_diagnosis.xlsx"))
d_educational <- import(here("data", "Educational_achievement.xlsx"))
d_suicidal <- import(here("data", "Suicidal_ideation.xlsx"))

# NOTE: 5 Refids removed to create these _d data files in the merge_anxiety script
## remove Refid 10481 because no valid ES data
## QEDs removed due to “critical” risk of bias: 10301, 10304, and 10349 
## 10362 removed due to insufficient information to calculate effect size
# 10062 removed in analysis script (intervention vs active comparison and depression focused intervention)


# Data Cleaning ----------------------------------------------------------------
## Study characteristics file --------------------------------------------------
# 1. Check to make sure study_author_year labels are unique--------------------#
# 1. Clean variables for use/presentation in app-------------------------------#

# 1. Check to make sure study_author_year labels are unique--------------------#
check <- d_study %>% 
  group_by(study_author_year) %>% 
  summarize(num_distinct = n_distinct(refid)) %>% 
  filter(num_distinct > 1) %>% 
  arrange(desc(num_distinct))

if (nrow(check) > 0) {
  warning("Duplicate study_author_years detected in raw study level data that have multiple distinct refid values.\n\n",
       "Offending rows:\n", 
       paste(capture.output(print(check)), collapse = "\n"))
}

# Correct study_author year labels (flagged with study1/2 in DistillerSR)
td_study <- d_study %>% 
  mutate(study_author_year = case_when(study_author_year == "Miller 2011" & refid == 10092 ~ paste0(study_author_year, "a"),
                                       study_author_year == "Miller 2011" & refid == 10479 ~ paste0(study_author_year, "b"),
                                       study_author_year == "Miller 2011" & refid == 10031 ~ paste0(study_author_year, "c"),
                                       study_author_year == "Calear 2016" & refid == 10120 ~ paste0(study_author_year, "a"),
                                       study_author_year == "Calear 2016" & refid == 10121 ~ paste0(study_author_year, "b"),
                                       TRUE ~ study_author_year))
  

# Another check to make sure study_author_year labels are correct now after cleaning
td_study %>%
  group_by(study_author_year) %>%
  summarize(num_distinct = n_distinct(refid), .groups = "drop") %>%
  filter(num_distinct > 1) %>%
  arrange(desc(num_distinct)) %>%
  {
    if (nrow(.) > 0) {
      stop(
        "Error: Duplicate study_author_year values with multiple distinct refid detected.\n\n",
        "Offending rows:\n",
        paste(capture.output(print(.)), collapse = "\n")
      )
    } else {
      message("Duplicate study_author_year values have been resolved.")
    }
    .
  }

# 1. Clean variables for use/presentation in app-------------------------------#

# Helper functions used for deriving columns-----------------------------------#

# Create grade_band variable (k-8, k-12, 9-12)
clean_grade_band <- function(df) {
  # Find which grade columns exist
  has_k <- "study_grade_level_k" %in% names(df)
  grade_cols <- paste0("study_grade_level_", 1:12)
  grade_cols <- grade_cols[grade_cols %in% names(df)]
  has_cannot <- "study_grade_level_cannot_tell" %in% names(df)
  
  n <- nrow(df)
  out <- character(n)
  
  for(i in seq_len(n)) {
    grades <- integer(0)
    
    # Kindergarten as grade 0 (if col exists)
    if(has_k) {
      val_k <- df[["study_grade_level_k"]][i]
      if(!is.na(val_k)) {
        grades <- c(grades, 0)
      }
    }
    
    # Grades 1–12 (over existing cols)
    for(col in grade_cols) {
      val <- df[[col]][i]
      if(!is.na(val)) {
        g_num <- as.integer(sub("study_grade_level_", "", col))
        grades <- c(grades, g_num)
      }
    }
    
    # Cannot tell flag
    sel_cannot <- FALSE
    if(has_cannot) {
      val_c <- df[["study_grade_level_cannot_tell"]][i]
      sel_cannot <- !is.na(val_c)
    }
    
    # Decision logic 
    
    # Case 1: nothing selected
    if(length(grades) == 0 && !sel_cannot) {
      out[i] <- "Unclear"
      
      # Case 2: only cannot_tell
    } else if(length(grades) == 0 && sel_cannot) {
      out[i] <- "Unclear"
      
      # Case 3: grades + cannot_tell = Unclear 
    } else if(length(grades) > 0 && sel_cannot) { #TODO: DECISION NEEDED - do we want this conservative logic?
      out[i] <- "Unclear"
      
      # Case 4: apply band logic
    } else {
      min_g <- min(grades)
      max_g <- max(grades)
      
      if(min_g >= 0 && max_g <= 8) {
        out[i] <- "K-8"
      } else if(min_g >= 9 && max_g <= 12) {
        out[i] <- "9-12"
      } else if(min_g <= 8 && max_g >= 9) {
        out[i] <- "K-12"
      } else {
        out[i] <- "Unclear"
      }
    }
  }
  
  df$clean_grade_band <- out
  df
}

# Create a single school_area variable
clean_school_area <- function(df) {
  df |>
    rowwise() |>
    mutate(
      clean_school_area = {
        sel_rural    <- !is.na(study_school_area_rural) &&
          trimws(study_school_area_rural) != ""
        sel_suburban <- !is.na(study_school_area_suburban) &&
          trimws(study_school_area_suburban) != ""
        sel_urban    <- !is.na(study_school_area_urban) &&
          trimws(study_school_area_urban) != ""
        
        selected <- c(
          if(sel_rural)    "rural"    else NULL,
          if(sel_suburban) "suburban" else NULL,
          if(sel_urban)    "urban"    else NULL
        )
        
        selected <- unique(selected)
        
        if(length(selected) == 0) {
          "Unclear"
        } else {
          label <- paste(sort(selected), collapse = "+")
          stringr::str_to_title(label)
        }
      }
    ) |>
    ungroup()
}




# Create a single school_type variable
clean_school_type <- function(df) {
  df |>
    rowwise() |>
    mutate(
      clean_school_type = {
        sel_public     = !is.na(study_school_type_public)     && trimws(study_school_type_public)     != ""
        sel_private    = !is.na(study_school_type_private)    && trimws(study_school_type_private)    != ""
        sel_parochial  = !is.na(study_school_type_parochial)  && trimws(study_school_type_parochial)  != ""
        sel_cannot     = !is.na(study_school_type_cannot_tell) && trimws(study_school_type_cannot_tell) != ""
        
        selected <- c(
          if (sel_public)    "Public"    else NULL,
          if (sel_private)   "Private"   else NULL,
          if (sel_parochial) "Parochial" else NULL
        )
        
        # Case 1: nothing selected at all = "Cannot Tell"
        if (length(selected) == 0 && !sel_cannot) {
          "Cannot Tell"
          
          # Case 2: only cannot_tell selected = "Cannot Tell"
        } else if (length(selected) == 0 && sel_cannot) {
          "Cannot Tell"
          
          # Case 3: one or more real types selected = collapse them (ignore cannot_tell)
        } else {
          paste(selected, collapse = "+")
        }
      }
    ) |>
    ungroup()
}

# Create a single country variable (if multiple, "more than one")
clean_country <- function(df) {
  # Find all country indicator columns
  country_cols <- grep("^study_country_", names(df), value = TRUE)
  
  # If no country columns, set to Unclear
  if (length(country_cols) == 0) {
    df$clean_country <- "Unclear"
    return(df)
  }
  
  n <- nrow(df)
  out <- character(n)
  
  # Helper to convert column name to country label
  col_to_country <- function(colname) {
    #remove prefix
    raw <- sub("^study_country_", "", colname)
    #split on "_" and title case 
    parts <- strsplit(raw, "_")[[1]]
    parts <- gsub("\\b([a-z])", "\\U\\1", parts, perl = TRUE)  #capitalize first letter
    paste(parts, collapse = " ")
  }
  
  #Pre-compute lookup for speed
  country_labels <- setNames(vapply(country_cols, col_to_country, character(1)), country_cols)
  
  for (i in seq_len(n)) {
    row_vals <- df[i, country_cols, drop = FALSE]
    
    selected_cols <- country_cols[
      !vapply(row_vals, function(x) {
        x <- as.character(x)
        is.na(x) | trimws(x) == ""
      }, logical(1))
    ]
    
    if (length(selected_cols) == 0) {
      out[i] <- "Unclear"
    } else if (length(selected_cols) == 1) {
      out[i] <- country_labels[[selected_cols]]
    } else {
      out[i] <- "More than one"
    }
  }
  
  df$clean_country <- out
  df
}

## Apply cleaning from helper functions
td_study <- td_study %>% 
  clean_school_type() %>%
  clean_school_area() %>%
  clean_grade_band() %>%
  clean_country()


## Meta-Analysis file ----------------------------------------------------------
# 1. Check which variables are in each data tab--------------------------------#
# 1. Remove TAU_m and school_mod variables (from above check)------------------#
# 1. Bind all dataframes-------------------------------------------------------#
# 1. Apply cleaning steps from Maria's MA analysis script----------------------#
# 1. Apply any additional cleaning needed to re-format variables---------------#

# 1. Check which dfs have different variable names for binding-----------------#

dfs <- list(
  d_adiagnosis,
  d_anxiety,
  d_depression,
  d_educational,
  d_suicidal,
  d_wellbeing
)

names(dfs) <- c("diagnosis","anxiety","depression","educational","suicidal","wellbeing")

var_matrix <- map(dfs, names) %>%
  enframe(name = "dataset", value = "var") %>%
  unnest(var) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = dataset, values_from = present, values_fill = 0)

var_matrix
#TAU_m is not in d_adiagnosis; school_mod is ONLY in d_anxiety (missing from all others).
#Solution: exclude those variables since they are not needed for this dashboard

# 1. Remove TAU_m and school_mod variables (if present), add source, and bind--#
full_ma_df <- imap_dfr(
  dfs,
  ~ .x %>%
    select(-any_of(c("TAU_m","school_mod"))) %>%  #Remove vars if present
    mutate(source = .y)                           #Add source column
)

# 1. Apply cleaning steps from Maria's MA analysis script----------------------#
## Filter the intervention vs. active comparison to remove per Sean's input 
## removing one row for depression focused intervention 


td_full_ma <- full_ma_df %>% 
  filter(refid != 10062) %>%
  filter(effect_intervention_group != "2-Thiswayup Schools: Combating Depression")

# 1. Apply any additional cleaning needed to re-format variables
# Helper functions ------------------------------------------------------------#
# Clean up effect size keys (string leading index and re-code HTML)
clean_es_key <- function(x) {
  x %>% 
    str_remove("^[0-9]+-") %>% 
    map_chr(~ xml_text(read_html(paste0("<x>", .x, "</x>"))))
}

# Create variable that aggregates outcome measures (specific to Anxiety outcomes as of 11/26/25)
process_outcome_measures_roots_anxiety <- function(all_outcome_measures) {
  if (length(all_outcome_measures) == 0) return(character(0))
  
  normalize_instrument_name <- function(measure) {
    if (is.na(measure) || trimws(measure) == "") {
      return("Other/Unclear")
    }
    
    lower_measure <- tolower(measure)
    
    # Pattern-based normalization (edit/extend as needed)
    instrument_patterns <- list(
      #check MASC before EAN to not results MASC with EAN selection
      "Multidimensional Anxiety Scale for Children (MASC)" = c("multidimensional anxiety scale for children", "masc"),
      
      "Centre for Epidemiological Studies – Depression Scale (CES-D)" = c("ces-d", "cesd", "centre for epidemiological studies"),
      "Children's Depression Inventory (CDI)" = c("children's depression inventory", "childrens depression inventory", "cdi"),
      "Culture-free Self- Esteem Questionnaire (CFSEI)" = c("culture-free self esteem questionnaire", "culture free self esteem questionnaire"),
      "Depression Questionnaire for Children (CDN)" = c("depression questionnaire for children", "cdn"),
      "Diagnostic Interview for Children and Adolescents (DICA-IV)" = c("dica-iv", "diagnostic interview for children and adolescents"),
      "Generalized Anxiety Disorder Scale (GAD-7)" = c("gad-7", "gad 7", "generalized anxiety disorder"),
      "Kessler Psychological Distress Scale (K6)" = c("k6", "kessler psychological distress scale"),
      
      "Patient Health Questionnaire (PHQ)" = c("phq-5", "patient health questionnaire"),
      "Revised Child Anxiety and Depression Scale (RCADS)" = c("revised child anxiety and depression scale", "rcads"),
      "Revised Children’s Manifest Anxiety Scale (RCMAS)" = c("revised children’s manifest anxiety scale", "rcmas"),
      "Screen for Child Anxiety Related Disorders (SCARED)" = c("screen for child anxiety related disorders", "scared"),
      "Short Mood and Feelings Questionnaire (SMFQ)" = c("smfq"),
      "Social Anxiety Scale for Adolescents (SAS-A)" = c("social anxiety scale for adolescents", "sas-a"),
      "Social Phobia and Anxiety Inventory for Children (SPAI-C)" = c("social phobia and anxiety inventory for children", "spai-c"),
      "Spence Children’s Anxiety Scale (SCAS)" = c("spence children’s anxiety scale", "spence children’s anxiety", "scas"),
      "Teacher-Rated School Performance" = c("teacher-rated school performance", "school performance"),
      "Total Life Satisfaction (TLS)" = c("total life satisfaction", "tls"),
      "Warwick-Edinburgh Mental Well-being Scale (WEMWBS)" = c("warwick-edinburgh mental well-being scale", "wemwbs"),
      "SAT" = c("sat-sat", "sat - maths", "sat - reading", "sat - writing"),
      "Wechsler Individual Achievement Test" = c("wiat-ii", "wechsler individual achievement test"),
      "Anxiety Scale for Children (EAN)" = c("anxiety scale for children", "ean")
    )
    
    for (instrument_name in names(instrument_patterns)) {
      for (pattern in instrument_patterns[[instrument_name]]) {
        if (grepl(pattern, lower_measure, fixed = TRUE)) {
          return(instrument_name)
        }
      }
    }
    
    # Fallback: use root before first "-" in the original measure text
    root <- strsplit(measure, "-")[[1]][1]
    root <- gsub("\\s+", " ", trimws(root))
    
    if (root == "") "Other/Unclear" else root
  }
  
  vapply(all_outcome_measures, normalize_instrument_name, FUN.VALUE = character(1))
}

# Create variable that aggregates intervention names (specific to Anxiety project 11/26/25)

process_intervention_roots_anxiety <- function(all_interventions) {
  if (length(all_interventions) == 0) return(character(0))
  
  normalize_intervention_name <- function(intervention) {
    if (is.na(intervention) || trimws(intervention) == "") {
      return("Other/Unclear")
    }
    
    lower_int <- tolower(intervention)
    
    # Pattern-based grouping (edit/extend as needed)
    intervention_patterns <- list(
      "AMISTAD"                                    = c("amistad"),
      "Aussie Optimism Program"                    = c("aussie optimism"),
      "Cognitive Behavioural Intervention (CBI)"   = c("cognitive behavioural intervention", "cognitive behavioral intervention", "cbi"),
      "Cool Kids Program"                          = c("cool kids"),
      "e-couch Anxiety and Worry program"          = c("e-couch"),
      "e-GAD"                                      = c("e-gad"),
      "FRIENDS Program"                            = c("friends"),
      "Lessons for Living: Think well, do well"    = c("lessons for living"),
      "Norwegian Universal Preventive Program for Social Anxiety (NUPP-SA)" = c("nupp-sa", "norwegian universal preventive program for social anxiety"),
      "Positive Search Training (PST)"             = c("positive search training", "pst"),
      "School-based anxiety prevention program (SBAPP)" = c("sbapp", "school-based anxiety prevention program"),
      "Taming Worry Dragons"                       = c("taming worry dragons"),
      "Think, Feel, Do"                            = c("think, feel, do"),
      "Thiswayup Schools: Overcoming Anxiety"      = c("thiswayup schools: overcoming anxiety"),
      "Unified Protocol (UP-A)"                    = c("unified protocol for transdiagnostic treatment of emotional disorders", "up-a")
    )
    
    for (root_name in names(intervention_patterns)) {
      for (pattern in intervention_patterns[[root_name]]) {
        if (grepl(pattern, lower_int, fixed = TRUE)) {
          return(root_name)
        }
      }
    }
    
    # Fallback: use root before first "-" in the original measure text
    root <- intervention
    root <- strsplit(root, ":", fixed = TRUE)[[1]][1]
    root <- strsplit(root, "\\(", perl = TRUE)[[1]][1]
    root <- strsplit(root, "-", fixed = TRUE)[[1]][1]
    
    root <- gsub("\\s+", " ", trimws(root))
    
    if (root == "") "Other/Unclear" else root
  }
  
  # Apply to the whole vector: vector in → vector out
  vapply(all_interventions, normalize_intervention_name, FUN.VALUE = character(1))
}


# Apply additional cleaning functions -----------------------------------------#

td_full_ma <- td_full_ma %>% 
  mutate(effect_outcome = clean_es_key(effect_outcome),
         effect_intervention_group = clean_es_key(effect_intervention_group),
         effect_comparison_group = clean_es_key(effect_comparison_group),
         processed_outcome_measure_roots = process_outcome_measures_roots_anxiety(effect_outcome),
         processed_intervention_roots = process_intervention_roots_anxiety(effect_intervention_group),
         yi_raw = yi, 
         vi_raw = vi
         )

## Merge study and MA data -----------------------------------------------------
td_merged <- left_join(td_full_ma , td_study, by = "refid") %>% 
  left_join(d_rob , by = "refid")


# ST update to calculate SMD for categorical variables -------------------------
# Function to convert 2x2 data to SMD via log OR with metafor
convert_2x2_to_smd_anxiety <- function(int_succ, int_unsucc,
                                       comp_succ, comp_unsucc) {
  esc <- metafor::escalc(
    measure = "OR",
    ai = int_succ,
    bi = int_unsucc,
    ci = comp_succ,
    di = comp_unsucc,
    add = 0.5, to = "all"  # continuity correction if needed
  )
  
  d   <- esc$yi * sqrt(3) / pi
  v_d <- esc$vi * (3 / (pi^2))
  
  tibble(
    yi_smd = d,
    vi_smd = v_d
  )
}

# Apply
is_categorical <- td_merged$effect_outcome_type == "Categorical"

has_2x2 <- with(
  td_merged,
  !is.na(effect_intervention_n_successful)   &
    !is.na(effect_intervention_n_unsuccessful) &
    !is.na(effect_comparison_n_successful)     &
    !is.na(effect_comparison_n_unsuccessful)
)

rows_to_convert <- which(is_categorical & has_2x2)

if (length(rows_to_convert) > 0) {
  conv <- convert_2x2_to_smd_anxiety(
    int_succ  = td_merged$effect_intervention_n_successful[rows_to_convert],
    int_unsucc = td_merged$effect_intervention_n_unsuccessful[rows_to_convert],
    comp_succ  = td_merged$effect_comparison_n_successful[rows_to_convert],
    comp_unsucc = td_merged$effect_comparison_n_unsuccessful[rows_to_convert]
  )
  
  # Overwrite yi/vi with SMD & variance for those rows
  td_merged$yi[rows_to_convert] <- conv$yi_smd
  td_merged$vi[rows_to_convert] <- conv$vi_smd
}

# Check if there are any variables without 2x2 data
no_2x2 <- with(
  td_merged,
  is.na(effect_intervention_n_successful)   &
    is.na(effect_intervention_n_unsuccessful) &
    is.na(effect_comparison_n_successful)     &
    is.na(effect_comparison_n_unsuccessful)
)

rows_without_logic <- which(is_categorical & no_2x2)

# Stop with error
if (length(rows_without_logic) > 0) {
  stop(
    "Categorical ES missing n_successful / n_unsuccessful data.\n",
    "No code currently to compute SMD for categorical effects without 2×2 counts.\n\n",
    "Offending refids:\n",
    paste(td_merged$refid[rows_without_logic], collapse = ", ")
  )
}


# Export -----------------------------------------------------------------------
#rio::export(td_merged, here("data", "app_data.csv"))


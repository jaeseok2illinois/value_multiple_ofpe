######################################
# Collection of functions for organizing data
######################################
#/*=================================================*/
#' # Make data availability check  
#/*=================================================*/
make_data_report <- function() {
  temp <- read_rmd("DataProcessing/data_availability_check.Rmd")
  writeLines(temp, con = here("Reports/ProjectTeam/data_availability_check.Rmd"))
  render(here("Reports/ProjectTeam/data_availability_check.Rmd"))
}

#/*=================================================*/
#' # Create Grower Data Folders
#/*=================================================*/

make_grower_folders <- function(field_data) {

  #--- get the field-year combinations ---#
  field_year_ls <- field_data$field_year
  
  make_indiv_growers_folder <- function(ffy) {

    root_dir <- paste0(here(), "/Data/Growers/", ffy) 
    
    #--- Parent ---#
    dir.create(root_dir)

    #--- Data  ---#
    dir.create(paste0(root_dir, "/Raw"))
    dir.create(paste0(root_dir, "/OriginalRawData"))
    dir.create(paste0(root_dir, "/Intermediate"))
    dir.create(paste0(root_dir, "/TrialDesign"))
    dir.create(paste0(root_dir, "/Analysis-Ready"))
    dir.create(paste0(root_dir, "/DataProcessingReport"))
    dir.create(paste0(root_dir, "/Conversations"))
    file.create(file.path(root_dir, "notes.md"))

    #--- Reports ---#
    dir.create(paste0(here(), "/Reports/Growers/", ffy))

    #=== copy the word template for reports ===#
    file.copy(
      here("Data/CommonData/word_template.docx"),
      paste0(here(), "/Reports/Growers/", ffy)
    ) 

  }

  lapply(field_year_ls, make_indiv_growers_folder)

}

#/*=================================================*/
#' # Create DataRequest Folders
#/*=================================================*/

make_td_folders <- function(field_data) {

  w_year <- Sys.Date() %>% year() 

  field_year_ls <- field_data[year == w_year, ]$field_year 
  
  make_indiv_folders <- function(ffy) {

    root_dir <- paste0(here(), "/Data/DataRequest/", ffy)

    if (!file.exists(root_dir)) {

      #--- Parent ---#
      dir.create(root_dir)
    }

  }

  lapply(field_year_ls, make_indiv_folders)

}

#/*=================================================*/
#' # Create a new entry with input data
#/*=================================================*/

gen_fp_template <- function(farm, field, year, crop, input_ls, strategy_ls, json_file = NULL) {

  temp_data <- data.table(
    trial_supervisor = "supervisor name",
    researcher = "institution name (e.g., UIUC, UMT, LSU)",
    farm = farm,
    field = field,
    year = year,
    crop = crop,
    crop_price = "numeric (no double quotes needed)",
    crop_unit = "bu or kg",
    raw_yield = "not received, lost, received",
    land_unit = "ac or ha",
    reporting_unit = "imperial or metric",
    h_width = list("numeric (no double quotes needed)"),
    ec = "not available, exists (not received), received",
    soil_sampling = "not available, exists (not received), received",
    trial_notes = "true or false (no double quotes needed)"
  )

  # i <- 1  
  for (i in seq_len(length(input_ls))){

    if (strategy_ls[i] == "trial"){
      #--- if trial data ---#
      if (input_ls[i] == "seed"){
        #--- if seed ---#
        temp_input_data <- data.table(
          form = "seed",
          strategy = "trial",
          unit = "seeds, thou_seeds",
          price = "numeric (no double quotes needed)",
          date = "mm/dd/yyyy",
          data = "not received, lost, file_name in the Raw data folder if received",
          sq_rate = "numeric (no double quotes needed) or (Rx) prescription file name",
          min_rate = "numeric (no double quotes needed)",
          max_rate = "numeric (no double quotes needed)",
          Rx_exists = "not available, exists (not received), received",
          machine_width = "numeric (no double quotes needed)",
          section_num = "numeric (no double quotes needed)",
          input_plot_width = "numeric (no double quotes needed)",
          use_target_rate_instead = "true or false (no double quotes needed)"
        )
      } else {
        #--- if not seed ---#
        temp_input_data <- data.table(
          form = input_ls[i],
          strategy = "trial",
          unit = "gallons, lbs, Mg, kg, bales",
          price = "numeric (no double quotes needed)",
          date = "mm/dd/yyyy",
          data = "not received, lost, file_name in the Raw data folder if received",
          sq_rate = "numeric (no double quotes needed) or (Rx) prescription file name",
          min_rate = "numeric (no double quotes needed)",
          max_rate = "numeric (no double quotes needed)",
          Rx_exists = "not available, exists (not received), received",
          machine_width = "numeric (no double quotes needed)",
          section_num = "numeric (no double quotes needed)",
          input_plot_width = "numeric (no double quotes needed)",
          use_target_rate_instead = "true or false (not double quotes needed)"
        )
      }
    } else {
      #--- if base ---#
      temp_input_data <- data.table(
        form = input_ls[i],
        strategy = "base",
        data = "Rx file name, as-applied file name, or NA",
        unit = "gallons, lbs, Mg, kg, bales",
        price = "numeric (no double quotes needed)",
        date = "mm/dd/yyyy",
        rate = "numeric (no double quotes needed)" 
      )
    }

    eval(parse(text=paste("temp_data[, input.", i , ":= list(temp_input_data)]", sep = "")))

  }

  if (!is.null(json_file)) {
    existing_data <- jsonlite::fromJSON(
      file.path(
        here("Data", "CommonData"),
        json_file
      ),
      flatten = TRUE
    ) %>%
    data.table() 
    temp_data <- rbind(existing_data, temp_data, fill = TRUE)
  } else {
    json_file <- "fp_template"
  }

  jsonlite::write_json(
    temp_data, 
    file.path(
      here("Data", "CommonData"),
      json_file
    ),
    pretty = TRUE
  )
}


#/*----------------------------------*/
#' ## Example
#/*----------------------------------*/
# gen_fp_template(
#   farm = "Paul",
#   field = "UIUC",
#   year = 2021,
#   crop = "corn",
#   input_ls = c("seed", "urea", "N_equiv"),
#   strategy_ls = c("trial", "trial", "base"),
#   fp_file = "fp_new_DSB.json"
# )

#/*=================================================*/
#' # Initiate field parameter entries for a farm-field-year 
#/*=================================================*/
# This function add new field parameter templates WITHOUT input data
# for a specified field

initiate_fp_entry <- function(farm, field, year, json_file = NULL) {
  
  temp_data <- data.table(
    trial_supervisor = "supervisor name",
    researcher = "institution name (e.g., UIUC, UMT, LSU)",
    farm = farm,
    field = field,
    year = year,
    crop = "corn, soy, wheat, or cotton",
    crop_price = "numeric (no double quotes needed)",
    crop_unit = "bu or kg",
    raw_yield = "not received, lost, received",
    land_unit = "ac or ha",
    reporting_unit = "imperial or metric",
    h_width = list("numeric (no double quotes needed)"),
    ec = "not available, exists (not received), received",
    soil_sampling = "not available, exists (not received), received",
    trial_notes = "true or false (no double quotes needed)"
  )

  if (!is.null(json_file)) {
    existing_data <- jsonlite::fromJSON(
      file.path(
        here("Data", "CommonData"),
        paste0(json_file, ".json")
      ),
      flatten = TRUE
    ) %>%
    data.table() 
    temp_data <- rbind(existing_data, temp_data, fill = TRUE)
  } else {
    json_file <- "fp_template"
  }

  jsonlite::write_json(
    temp_data, 
    file.path(
      here("Data", "CommonData"),
      paste0(json_file, ".json")
    ),
    pretty = TRUE
  )
}

#/*----------------------------------*/
#' ## Example
#/*----------------------------------*/
# initiate_fp_entry(
#   farm = "Paul",
#   field = "UIUC",
#   year = 2021,
#   json_file = "field_parameter_example.json"
# )

#/*=================================================*/
#' # Add inputs to the template
#/*=================================================*/
# Note: this code adds input data to an existing farm-field-year in 
# an existing field parameter file


add_inputs <- function(json_file, farm, field, year, input_ls, strategy_ls) {

  ffy <- paste(farm, field, year, sep = "_")

  existing_data <-  here("Data", "CommonData", json_file) %>% 
    jsonlite::fromJSON(
      .,
      flatten = TRUE
    ) %>%
    data.table() %>% 
    .[, field_year := paste(farm, field, year, sep = "_")]
  
  w_data <- existing_data[field_year == ffy, ]

  if (nrow(w_data) != 1) {
    print(
      "No (or duplicate) records for the specified farm-field-year are found. Check if the specified parameters are correct."
    )
    break
  }

  if (length(input_ls) != length(strategy_ls)) {
    print(
      "The number of elements provided to input_ls and stratefy_ls do not match."
    ) 
    break
  }

  input_data <- dplyr::select(w_data, starts_with("input.")) %>% 
    lapply(., function(x) x[[1]]) %>% 
    rbindlist(fill = TRUE)

  for (i in seq_len(length(input_ls))) {

    input_num <- nrow(input_data) + i

    if (strategy_ls[i] == "trial"){
      #--- if trial data ---#
      if (input_ls[i] == "seed"){
        #--- if seed ---#
        temp_input_data <- data.table(
          form = "seed",
          strategy = "trial",
          unit = "seeds, thou_seeds",
          price = "numeric (no double quotes needed)",
          date = "mm/dd/yyyy",
          data = "not received, lost, file_name in the Raw data folder if received",
          sq_rate = "numeric (no double quotes needed) or (Rx) prescription file name",
          min_rate = "numeric (no double quotes needed)",
          max_rate = "numeric (no double quotes needed)",
          Rx_exists = "not available, exists (not received), received",
          machine_width = "numeric (no double quotes needed)",
          input_plot_width = "numeric (no double quotes needed)",
          use_target_rate_instead = "true or false (no double quotes needed)"
        )
      } else {
        #--- if not seed ---#
        temp_input_data <- data.table(
          form = input_ls[i],
          strategy = "trial",
          unit = "gallons, lbs, Mg, kg, bales",
          price = "numeric (no double quotes needed)",
          date = "mm/dd/yyyy",
          data = "not received, lost, file_name in the Raw data folder if received",
          sq_rate = "numeric (no double quotes needed) or (Rx) prescription file name",
          min_rate = "numeric (no double quotes needed)",
          max_rate = "numeric (no double quotes needed)",
          Rx_exists = "not available, exists (not received), received",
          machine_width = "numeric (no double quotes needed)",
          input_plot_width = "numeric (no double quotes needed)",
          use_target_rate_instead = "true or false (not double quotes needed)"
        )
      }
    } else if (strategy_ls[i] == "base") {
      #--- if base ---#
      temp_input_data <- data.table(
        form = input_ls[i],
        strategy = "base",
        data = "Rx file name or as-applied file name",
        unit = "gallons, lbs, Mg, kg, bales",
        price = "numeric (no double quotes needed)",
        date = "mm/dd/yyyy",
        rate = "numeric (no double quotes needed)" 
      )
    } else {
      print(
        paste0("The specified strategy type for the ", i, "th element of the strategy_ls you provided is invalid.")
      )
      break
    }

    eval(parse(text=paste("w_data[, input.", input_num , ":= list(temp_input_data)]", sep = "")))

  }

  out_data <- rbind(
    existing_data[field_year != ffy, ],
    w_data,
    fill = TRUE
  ) %>% 
  .[order(field_year),] %>% 
  .[, field_year := NULL]

  jsonlite::write_json(
    out_data, 
    file.path(
      here("Data", "CommonData"),
      json_file
    ),
    pretty = TRUE
  )

}

#/*----------------------------------*/
#' ## Example
#/*----------------------------------*/
# add_inputs(
#   json_file = "field_parameter_example.json",
#   farm = "DodsonAg",
#   field = "Windmill",
#   year = "2019",
#   input_ls = c("urea", "N_equiv"),
#   strategy_ls = c("trial", "base")
# )

#/*=================================================*/
#' # Add a variable to a field parameter json file
#/*=================================================*/

add_var_to_fp <- function(file_name, var_name, var_value = NULL, overwrite = FALSE) {

  field_data <- jsonlite::fromJSON(
    file.path(
      here("Data", "CommonData"),
      file_name
    ),
    flatten = TRUE
  ) %>%
  data.table() %>%
  .[, field_year := paste(farm, field, year, sep = "_")] 

  if (!var_name %in% names(field_data)) {
    if (!is.null(var_value)) {
      suppressWarnings(field_data[, (var_name) := var_value])
    } else {
      suppressWarnings(field_data[, (var_name) := "NA"])
    }
  } else {
    stop("A variable of the same name already exists. Aborting the requested operation.")
  }

  if (overwrite == TRUE) {
    jsonlite::write_json(
      field_data, 
      file.path(
        here("Data", "CommonData"),
        file_name
      ),
      pretty = TRUE
    )
  } else {
    cat("A new variable was added. Confirm this is indeed what you want. If so, use overwrite = TRUE option to overwrite the file.")
    return(field_data)
  }

}

#/*~~~~~~~~~~~~~~~~~~~~~~*/
#' ### Example
#/*~~~~~~~~~~~~~~~~~~~~~~*/
#--- check the data with a new variable ---#
# data_check <- add_var_to_fp(
#   file_name = "field_parameter_example.json", 
#   var_name = "new", 
#   var_value = "temp",
#   overwrite = FALSE
# )

#--- overwrite the file ---#
# add_var_to_fp(
#   file_name = "field_parameter_example.json", 
#   var_name = "new", 
#   var_value = "temp",
#   overwrite = TRUE
# )




source("facet_plot_entries.R")

# read raw qTower files
read_qtower <- function( file_path,
                         start_temp = 25,
                         inc_temp = 1,
                         channel_levels  = c("FAM", "JOE", "TAMRA", "ROX", "Cy5", "Cy5.5", "SyproOrange")
) {
  #____Determine the length of the metadata header
  # define the possible wells (on the fly bc its cheap)
  wells <-
    expand.grid(LETTERS[1:16], c(1:24)) %>%
    tidyr::unite("well", sep = "") %>%
    dplyr::pull(.data$well)
  
  # identify first row that contains a well
  first_well_row <-
    vroom::vroom(file_path, delim = ",", col_select = c(1)) %>%
    purrr::set_names(c("col")) %>%
    dplyr::mutate(is_well = .data$col %in% wells,
                  first_well = .data$is_well == TRUE & !duplicated(.data$is_well == TRUE),
                  row_num = dplyr::row_number()) %>%
    dplyr::filter(.data$first_well == TRUE) %>%
    dplyr::pull(.data$row_num) %>%
    base::suppressMessages() %>% # suppressing column specification message
    base::suppressWarnings()
  
  #_____Read the file in_____
  df <-
    vroom::vroom(file_path,
                 delim = ",",
                 skip = first_well_row-1,
                 col_names =  FALSE,
                 show_col_types = FALSE) %>%
    purrr::set_names(c("well_channel", .[1,][-1])) %>%
    dplyr::mutate(channel = dplyr::if_else(.data$well_channel %in% channel_levels,
                                           true = .data$well_channel,
                                           false = "well"),
                  channel = na_if(.data$channel, "well")) %>%
    tidyr::fill(.data$channel, .direction = "down") %>%
    dplyr::filter(.data$well_channel != .data$channel) %>%
    tidyr::unite("variable", c(.data$well_channel, .data$channel), sep = "_") %>%
    tidyr::pivot_longer(-.data$variable, names_to = "Temperature", values_to = "value") %>%
    dplyr::mutate(dplyr::across(.cols = c(.data$Temperature, .data$value), as.numeric),
                  Temperature = (start_temp - 1) + (.data$Temperature * inc_temp)) %>% # convert cycle to temp
    tidyr::separate(.data$variable, into = c("well", "channel"), sep = "_", remove = FALSE) %>%
    dplyr::mutate(channel_f = factor(.data$channel, levels = channel_levels),
                  value_norm = scales::rescale(.data$value, to = c(0,1))) %>%
    dplyr::select(.data$variable, .data$well, .data$channel_f, .data$Temperature, .data$value, .data$value_norm)
  
}

# Function to prepare valdiation screens for plotting
prep_validation <- function(validation_raw, min_value = 10000) {
  validation_raw |> 
    mutate(id = "placeholder",
           subset = "placeholder") |> 
    rename(dye_conc_uM = conc) |>
    select(-volume) |>
    mutate(type = if_else(protein != "Buffer",
                          true = "protein",
                          false = "buffer")) |>
    filter(type == "protein") |>
    unite(variable, c(well, channel_f, protein), remove = FALSE) |>
    group_by(variable) |>
    filter(any(value > min_value)) |>
    ungroup()
  
}


# Helper function to do some of the repetetive steps to prepare raw files for DSFbase 
prep_dsfbase <- function(dsf,
                         .id = "placeholder",
                         .subset = "placeholder",
                         .buffer_name = "Buffer",
                         .add_variable = FALSE,
                         .add_variable_from = c("well", "channel_f")) {
  dsf <- dsf |> 
    mutate(id = "placeholder",
           subset = "canon",
           type = if_else(protein != .buffer_name,
                          true = "protein",
                          false = "buffer")) |>
    filter(protein != "Buffer") 
  
  if(.add_variable){
    dsf <- dsf |> 
      unite(variable, .add_variable_from, remove = FALSE)
  }
  
  dsf
  
}


# function to test a dataframe, plot it's variables, and save the data by subset
# test_plot_save <- function(dsf, 
#                            save_path = "../02_aggregated_data/", 
#                            use_external_name = FALSE,
#                            external_name = "",
#                            save_data = TRUE,
#                            ...) {
#   if(use_external_name) {
#     dataset_name <- external_name
#   } else {
#     dataset_name <- deparse(substitute(dsf))
#   }
#   
#   subset <- unique(dsf$subset)
#   
#   test_dataset(dsf)
#   print_tallies(dsf)
#   
#   save_plot_by_var(dsf,
#                    use_external_name = TRUE,
#                    external_name = dataset_name,
#                    save_path = save_path)
#   
#   if(save_data) {
#     # rds path
#     file_name <- glue::glue("{dataset_name}")
#     save_rds_to <- glue::glue("{save_path}/{subset}/{file_name}.rds")
#     
#     write_rds(dsf, save_rds_to)
#   }
#   
# }
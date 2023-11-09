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

tally_dsfbase <- function(TALLY_DIR = "../02_aggregated_data"){
  
  all_data_dirs <- fs::dir_ls(path = TALLY_DIR, 
                              regexp = ".rds", recurse = 1)
  
  # initialize
  n_SYPRO_canon_proteins <- 0
  n_SYPRO_noncanon_proteins <- 0
  n_canon_proteins <- 0
  n_noncanon_proteins <- 0
  n_SYPRO_canon_datasets <- 0
  n_SYPRO_noncanon_datasets <- 0
  n_canon_datasets <- 0
  n_noncanon_datasets <- 0
  
  for(dsf in all_data_dirs){
    int <- read_rds(dsf)
    
    if(grepl("/SYPRO_canon/", dsf)) {
      n_SYPRO_canon_proteins <- n_SYPRO_canon_proteins + n_distinct(int$protein)
      n_SYPRO_canon_datasets <- n_SYPRO_canon_datasets + n_distinct(int$variable)
    } else if (grepl("/SYPRO_noncanon/|/noncanon/",dsf)) {
      n_noncanon_proteins <- n_noncanon_proteins+ n_distinct(int$protein)
      n_noncanon_datasets <- n_noncanon_datasets + n_distinct(int$variable)
      
    } else if (grepl("/canon/",dsf)) {
      n_canon_proteins <- n_canon_proteins + n_distinct(int$protein)
      n_canon_datasets <- n_canon_datasets + n_distinct(int$variable)
    }
  }
  
  msg <- glue::glue("{n_SYPRO_canon_proteins} proteins and {n_SYPRO_canon_datasets} datasets in SYPRO cannon,
                   {n_canon_proteins} proteins and {n_canon_datasets} in cannon (not SYPRO),
                   {n_noncanon_proteins} proteins and {n_noncanon_datasets} datasets in non-cannon")
  
  print(msg)
}

test_dataset <- function(dsf) {
  # expectation function doc
  # https://testthat.r-lib.org/reference/expect_setequal.html
  
  test_that("data is a tibble", {
    expect_s3_class(dsf, "tbl_df")
  })
  
  # dataset value contains one of the accepted names
  test_that("dataset contains one of the accepted values", {
    subset_names <- c(
      "SYPRO_canon",
      "SYPRO_noncanon",
      "canon",
      "noncanon",
      "gotcha",
      "buffer"
    )
    expect_in(dsf$subset, subset_names)
  })
  
  # dataset value contains one of the accepted names
  test_that("data includes reqiured columns", {
    correct_names <- c("id", "subset", "protein", "well", "variable", "Temperature", "value")
    expect_in(correct_names, names(dsf))
  })
  
  test_that("required columns have correct types", {
    dsf_order <- dsf |>
      select(all_of(c("id", "subset", "protein", "well", "variable", "Temperature", "value")))
    
    expect_equal(
      unname(unlist(lapply(dsf_order, class))),
      c(
        "character", # "id",
        "character", # ""subset"
        "character", # protein
        "character", # "well",
        "character", # "variable"
        "numeric", # "Temperature",
        "numeric" # "value"
      )
    )
  })
  
  test_that("data has 'variable' column which is genuinely unique to a single trace", {
    temp_dup <- dsf %>%
      dplyr::group_by(variable) %>%
      dplyr::mutate(unique_temps = !duplicated(Temperature))
    
    expect_true(all(temp_dup$unique_temps))
  })
  
  
  test_that("all datasets contains 69 temperature measurements", {
    temp_dup <- dsf %>%
      dplyr::group_by(variable) %>%
      dplyr::mutate(n_unique_temps = n_distinct(Temperature)) |> 
      pull(n_unique_temps)
    
    expect_equal(unique(temp_dup), 70)
  })
  
  test_that("dataset contains no NAs", {
    required_cols <- c("id", "subset", "protein", "well", "variable", "Temperature", "value")
    req <- dsf |> select(all_of(required_cols))
    
    # no NAs in the required columns. 
    # permit NA in other random experimental condition columns 
    expect_false(any(is.na(req)))
  })
  
}


save_plot_by_var <- function(dsf,
                             save_path = "../02_aggregated_data/",
                             use_external_name = FALSE,
                             external_name = "",
                             ...){
  
  # for plot annotations
  # so it can be used inside other functions as well
  if(use_external_name) {
    dataset_name <- external_name
  } else {
    dataset_name <- deparse(substitute(dsf))
  }
  
  tallies <- print_tallies(dsf)
  subset <- unique(dsf$subset)
  subset_mg <- glue::glue("Subset: {subset}")
  
  # make plot
  p <- 
    dsf |> 
    ggplot(aes(x = Temperature, y = value, group = variable)) +
    geom_line(linewidth = 0.5) +
    facet_wrap(~variable, scales = "free", ncol = 5) +
    hrbrthemes::theme_ipsum(base_family = "Helvetica",
                            base_size = 5,
                            strip_text_size = 8) +
    theme(aspect.ratio = 1,
          axis.text.y=element_blank(),
          axis.text.x=element_blank())
  
  p <- p +
    labs(title = dataset_name,
         subtitle = str_wrap(glue::glue("{subset_mg} \n 
                                 {glue::glue_collapse(tallies[c(2,3)], sep = '\n')}"), 80))
  
  
  ## get save name
  save_dir <- glue::glue("{save_path}/{subset}/")
  fs::dir_create(save_dir)
  save_name <- glue::glue("{save_dir}{dataset_name}.pdf")
  save_height <- 1.3*(n_distinct(dsf$variable)/5) + 3
  
  ggsave(save_name,
         p,
         width = 7,
         height = save_height,
         limitsize = FALSE
  )
}

test_plot_save <- function(dsf, 
                           save_path = "../02_aggregated_data/", 
                           use_external_name = FALSE,
                           external_name = "",
                           save_data = TRUE,
                           ...) {
  if(use_external_name) {
    dataset_name <- external_name
  } else {
    dataset_name <- deparse(substitute(dsf))
  }
  
  subset <- unique(dsf$subset)
  
  test_dataset(dsf)
  print_tallies(dsf)
  
  save_plot_by_var(dsf,
                   use_external_name = TRUE,
                   external_name = dataset_name,
                   save_path = save_path)
  
  if(save_data) {
    # rds path
    file_name <- glue::glue("{dataset_name}")
    save_rds_to <- glue::glue("{save_path}/{subset}/{file_name}.rds")
    
    write_rds(dsf, save_rds_to)
  }
  
}

print_tallies <- function(dsf) {
  dataset_name <- deparse(substitute(dsf))
  proteins <- unique(dsf$protein)
  n_protein <- n_distinct(dsf$protein)
  n_var <- n_distinct(dsf$variable)

  header <- glue::glue("`{dataset_name}` contains")
  msg1 <- glue::glue("Number of unique proteins: {n_protein} ({glue::glue_collapse(proteins, sep = ', ')})")
  msg2 <- glue::glue("Number of unique datasets: {n_var}")

  print(header, msg1, msg2, "\n")
  out <- c(header, msg1, msg2, "\n")
  #c(header, msg1, msg2, "\n")
}



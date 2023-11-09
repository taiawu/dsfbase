
## read and tidy a directory from Exp1252 containing the dye screen for one protein
tidy_1252 <- function(PATH, is_caspase = FALSE, subset = "canon", ...){
  
  if(is_caspase) { # caspase directories are only the tidied tibble
    screen <- read_rds(PATH)
  } else { # Exp1252 directories have a list, with tidied tibble as first element
    screen <- read_rds(PATH)[[1]] 
  }
  
  screen <- 
    screen |> 
    
    # add well
    separate(variable, into = c("well", "channel_drop", "type_drop"), 
             sep = "_", # ignore period in Cy5.5
             remove = FALSE) |> 
    select(-c("channel_drop", "type_drop")) |> 
    
    # add DSFbase-specific columns
    mutate(id = "placeholder",
           subset = subset) |> 
    
    rename(protein = identity) |> 
    
    # keep only with-protein hits and sensitives
    filter(type == "protein",
           assignment != "none",
           fit_channel == "fit")
  
  # print some in
  n_datasets <- n_distinct(screen$variable)
  print(glue::glue("Datasets: {n_datasets}"))
  
  screen
}


## read, test, plot, save, and manually edit subsets from a dye screen
test_plot_save_1252 <- function(PATH, 
                                save_path = "../02_aggregated_data/dye_screen_tests/", 
                                save_data = TRUE,
                                min_value = 10000,
                                min_range = 10000,
                                add_to_canon = c(),
                                drop_from_canon = c(),
                                
                                add_to_noncanon = c(),
                                drop_from_noncanon = c(),
                                
                                add_to_SYPRO_canon = c(),
                                drop_from_SYPRO_canon = c(),
                                
                                add_to_SYPRO_noncanon = c(),
                                drop_from_SYPRO_noncanon = c(),
                                
                                add_to_gotcha = c(),
                                is_caspase = FALSE,
                                ...) {
  
  # read in and tidy the Exp1252 dataset

  dsf <- tidy_1252(PATH, is_caspase = is_caspase, subset = "placeholder", ...) |> 
    group_by(dye) |> 
    
    # drop low-signal curves, for simplicity
    filter(any(value > min_value), 
           any(max(value) - min(value) > min_range)) |> 
    ungroup()
  
  # make name for saving
  exp_num <- unique(dsf$exp_num)
  protein_name <- unique(dsf$protein)
  dataset_name <- glue::glue("{exp_num}_{protein_name}") 
  
  #### save SYPRO data
  sypro <-  dsf |> 
    filter(dye == "SYPRO")
  
  save_subset(sypro,
              keep_assignment = "hit", 
              keep_variable = add_to_SYPRO_canon,
              drop_variable = drop_from_SYPRO_canon,
              .subset = "SYPRO_canon",
              save_data = save_data,
              save_path = save_path)
  
  save_subset(sypro,
              keep_assignment = "sensitive", 
              keep_variable = add_to_SYPRO_noncanon,
              drop_variable = drop_from_SYPRO_noncanon,
              .subset = "SYPRO_noncanon",
              save_data = save_data,
              save_path = save_path)
  
  #### save aurora data
  aurora <-  dsf |> 
    filter(dye != "SYPRO")
  
  save_subset(aurora,
              keep_assignment = "hit", 
              keep_variable = add_to_canon,
              drop_variable = drop_from_canon,
              .subset = "canon",
              save_data = save_data,
              save_path = save_path)
  
  save_subset(aurora,
              keep_assignment = "sensitive", 
              keep_variable = add_to_noncanon,
              drop_variable = drop_from_noncanon,
              .subset = "noncanon",
              save_data = save_data,
              save_path = save_path)
  
  # save gotchas
  if(length(add_to_gotcha) > 0) {
    drop_from_gotcha <- dsf$variable[!dsf$variable %in% add_to_gotcha]
    save_subset(dsf,
                keep_assignment = c("hit", "sensitive"),
                keep_variable = add_to_gotcha,
                drop_variable = drop_from_gotcha,
                .subset = "gotcha",
                save_data = save_data,
                save_path = save_path) 
  }
}

## helper function for `test_plot_save_1252()` to manually edit subsets from a dataframe 
save_subset <-
  function(dsf,
           keep_assignment = "hit", 
           keep_variable = c(),
           drop_variable = c(),
           .subset,
           save_data = TRUE,
           save_path = "../02_aggregated_data/dye_screen_tests/",
           ...) {
    
    if(nrow(dsf) == 0) {
      msg <- glue::glue("No rows in input data for {.subset}. Not saving.")
      print(msg)
      return
    }
    
    # make name for saving
    exp_num <- unique(dsf$exp_num)
    protein_name <- unique(dsf$protein)
    
    dataset_name <- glue::glue("{exp_num}_{protein_name}") 
    save_name <- glue::glue("{dataset_name}_{.subset}")
    
    dsf_subset <-  dsf |> 
      filter(assignment %in% keep_assignment | variable %in% keep_variable,
             !variable %in% drop_variable) |> 
      mutate(subset = .subset)
    
    ## helpful print statements
    n_datasets <- n_distinct(dsf_subset$variable)
    msg <- glue::glue("{dataset_name}, {.subset}: {n_datasets} datasets")
    subset_contents <- glue::glue("{glue::glue_collapse(double_quote(sort(unique(dsf_subset$variable))), sep = ', ')}")
    
    print(msg, subset_contents)
    
    # only save if data remains post filtering
    if(nrow(dsf_subset) > 0) {
      test_plot_save(dsf_subset,
                     use_external_name = TRUE,
                     external_name = save_name,
                     save_path = save_path,
                     save_data = save_data) 
    } else { # notify when not saving
      print(glue::glue("No data in: {save_name}. Not saving."))
    }
  }


move_and_tally <- function(canon_from = "/Users/taiaseanwu/Desktop/programming/dsfbase/02_aggregated_data/dye_screen_tests/canon",
                           noncanon_from = "/Users/taiaseanwu/Desktop/programming/dsfbase/02_aggregated_data/dye_screen_tests/noncanon",
                           SYPRO_canon_from = "/Users/taiaseanwu/Desktop/programming/dsfbase/02_aggregated_data/dye_screen_tests/SYPRO_canon",
                           SYPRO_noncanon_from =  "/Users/taiaseanwu/Desktop/programming/dsfbase/02_aggregated_data/dye_screen_tests/SYPRO_noncanon",
                           gotcha_from = "/Users/taiaseanwu/Desktop/programming/dsfbase/02_aggregated_data/dye_screen_tests/gotcha",
                           
                           canon_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/02_aggregated_data/canon",
                           noncanon_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/02_aggregated_data/noncanon",
                           SYPRO_canon_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/02_aggregated_data/SYPRO_canon",
                           SYPRO_noncanon_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/02_aggregated_data/SYPRO_noncanon",
                           gotcha_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/02_aggregated_data/gotcha",
                           tally_new = TRUE) {
  # move canon
  fs::file_move(path = fs::dir_ls(canon_from),
                new_path = canon_to)
  
  # move noncanon
  fs::file_move(path = fs::dir_ls(noncanon_from),
                new_path = noncanon_to)
  
  # move SYPRO canon
  fs::file_move(path = fs::dir_ls(SYPRO_canon_from),
                new_path = SYPRO_canon_to)
  
  # move SYPRO noncanon
  fs::file_move(path = fs::dir_ls(SYPRO_noncanon_from),
                new_path = SYPRO_noncanon_to)
  
  # move gotcha 
  fs::file_move(path = fs::dir_ls(gotcha_from),
                new_path = gotcha_to)
  
  if(tally_new){ # report the new tallies
    tally_dsfbase()
  }
}



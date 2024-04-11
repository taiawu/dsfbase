# function to add IDs, and save relevant forms, of DSFbase
update_dsfbase <- function(annotated,
                           subset_order = c("canon", "noncanon", "speculative", "errata"),
                           save_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/",
                           ...
){
  # add IDs
  print("Adding IDs and testing")
  dsfbase_id <- add_IDs(annotated, subset_order = subset_order) # gets subset order, when supplied to update_dsdfbase ... argument
  
  if(all(test_annotated_dataset(dsfbase_id))) {
    fs::dir_create(save_to)
    dsfbase_rds_to <- glue::glue("{save_to}dsfbase_annotated.rds")
    
    ## save the complete, annotated, id'd data as an RDS
    msg <- glue::glue("Saving DSFbase, with all columns, to `{dsfbase_rds_to}`")
    print(msg)
    write_rds(dsfbase_id, file = dsfbase_rds_to)
    
    ## save the data-only CSVs
    dsfcase_csv_to <- glue::glue("{save_to}csvs/")
    msg <- glue::glue("Saving DSFbase to csv, both normalized and raw data, in `{dsfcase_csv_to}`")
    print(msg)
    fs::dir_create(dsfcase_csv_to)
    
    dsfbase_to_csv(dsfbase_id, value_col = "value",.save_to = dsfcase_csv_to ,...)
    dsfbase_to_csv(dsfbase_id, value_col = "value_norm", .save_to = dsfcase_csv_to ,...)
    
    ## save the experimental conditions
    print("Extracting and savign experimental conditions file")
    conditions_dir <- glue::glue("{save_to}experimental_conditions/")
    fs::dir_create(conditions_dir)
    conditions <- write_experimental_conditions(dsfbase_id, save_to = conditions_dir, ...)
    
    ## save plots of the full DSFbase
    print("Plotting all data in each subset")
    plot_dir <- glue::glue("{save_to}plots/")
    fs::dir_create(plot_dir)
   # all_subsets <-  c("SYPROcanon", "canon", "SYPROmidcanon", "midcanon","SYPROnoncanon", "noncanon", "SYPROlatenoncanon","latenoncanon", "errata", "drop")
    use_subsets <- subset_order[subset_order %in% dsfbase_id$subset]
    
    lapply(use_subsets, 
           plot_subset, 
           save_path = plot_dir,
           .test_subset = FALSE,
           dsfbase = dsfbase_id)
    
    # patch <- patchwork::wrap_plots(plotlist, ncol = 1)
    # 
    # 
    # plot_subset(dsfbase_id, 
    #                         .subset, 
    #                         save_path = "../03_combined_data/plots",
    #                         .test_subset = TRUE,
    #                         .facet_by = "short_id")
    
  } else {
    print("dsfbase with IDs failed tests. Returning without saving.")
    return(NULL)
  }
  
}


write_experimental_conditions <- function(dsfbase_id, save_to, ...){
  .use_cols <- c("additional_notes", "additive", "additive_concentration_uM", "additive_description", "dye", "dye_concentration_uM", "exp_num", "exp_summary", "fluorescent_channel", "id", "instrument", "protein", "protein_concentration_uM", "protein_original", "subset", "subsubset","thermocycling_protocol", "variable", "well")
  
  conditions <- dsfbase_id |> 
    select(all_of(c(.use_cols))) |> 
    distinct()
  
  csv_name <- glue::glue("{save_to}dsfbase_experimental_conditions.csv")
  rds_name <- glue::glue("{save_to}dsfbase_experimental_conditions.rds")
  
  write_rds(conditions, rds_name)
  write_csv(conditions, csv_name)
  
}

# save DSFbase files
make_dsfbase_wide <- function(dsf, value_col, ...){
  dsf_wide <- dsf |> 
    select(all_of(c("Temperature", "id", value_col))) |> 
    pivot_wider(id_cols = "Temperature", names_from = "id", values_from = value_col)
}

dsfbase_to_csv <- function(dsfbase, 
                           by_subset = TRUE,
                           value_col = "value_norm",
                           .save_to = "../02_combined_dsfbase/",
                           .save_full = TRUE,
                           .save_subset = TRUE,
                           ...){
  
  subsets <- as.character(sort(unique(dsfbase$subset)))
  
  # for saving the output file
  .value_name <- switch(value_col,
                        "value_norm" = "normalized",
                        "value" = "nonnormalized")
  
  dsfbase_l <- dsfbase |> 
    arrange(subset) |> 
    group_by(subset) |> 
    group_split() 
  
  names(dsfbase_l) <- subsets
  
  save_paths <- glue()
  
  wide_l <- lapply(dsfbase_l, make_dsfbase_wide, value_col = value_col)
  
  
  if(.save_full) {
    
    dsfbase_wide <- dsfbase |> make_dsfbase_wide(value_col = value_col)
    n_curves <- ncol(dsfbase_wide) - 1
    save_name <- glue("{.save_to}dsfbase_v001_{.value_name}_{n_curves}_entries.csv")
    write_csv(x = dsfbase_wide, save_name)
  }
  
  if(.save_subset){
    i <- 0
    for(.subset in subsets){
      i <- i + 1
      dsf <- wide_l[[.subset]]
      n_curves <- ncol(dsf) - 1
      save_name <- glue("{.save_to}dsfbase_v001_{.value_name}_{.subset}_{n_curves}_entries.csv")
      write_csv(x = dsf, save_name)
    }
  }
  
}
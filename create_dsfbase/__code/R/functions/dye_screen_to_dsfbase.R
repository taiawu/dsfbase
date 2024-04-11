
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
dye_screen_to_dsfbase <- function(PATH,
                                  save_path, # = "../02_aggregated_data/dye_screen_tests/",
                                  save_data = TRUE,
                                  min_value = 10000,
                                  min_range = 10000,
                                  move_to_canon = c(),
                                  move_to_noncanon = c(),
                                  move_to_errata = c(),
                                  drop_from_dsfbase = c(),
                                  
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
                                  drop_full_screen = FALSE,
                                  ...) {
  
  # do nothing, if the goal is to drop everything from the screen
  if(drop_full_screen) {
    # alert user
    print("adding no entries from screen")
    print(PATH)
    # and then do nothing more
    return()
  }
  
  #------------ read in and tidy the Exp1252 dataset
  dsf <- tidy_1252(PATH, is_caspase = is_caspase, subset = "placeholder", ...) |>
    group_by(dye) |>
    
    # drop low-signal curves, for simplicity
    filter(any(value > min_value),
           any(max(value) - min(value) > min_range)) |>
    ungroup() |>
    
    # make the original, hit-call based assignments
    # not keeping any of the nones
    filter(assignment != "none") |>
    
    # sensitives to go non-canon, and hits go to canon
    mutate(subset = case_when(assignment == "hit" & dye != "SYPRO" ~ "canon",
                              assignment == "sensitive" & dye != "SYPRO" ~ "noncanon",
                              assignment == "hit" & dye == "SYPRO" ~ "SYPROcanon",
                              assignment == "sensitive" & dye == "SYPRO" ~ "SYPROnoncanon",
    )) |> 
    
    # update the subset assignments base on the old reassignment approach
    mutate(subset = case_when(variable %in% drop_from_canon ~ NA, # remove an overwritten assignment
                              variable %in% drop_from_noncanon ~ NA,  # remove an overwritten assignment
                              variable %in% drop_from_SYPRO_canon ~ NA,  # remove an overwritten assignment
                              variable %in% drop_from_SYPRO_noncanon ~ NA, # remove an overwritten assignment
                              
                              variable %in% add_to_canon ~ "canon", # add new assignments
                              variable %in% add_to_noncanon ~ "noncanon", # add new assignments
                              variable %in% add_to_SYPRO_canon ~ "SYPROcanon", # add new assignments
                              variable %in% add_to_SYPRO_noncanon ~ "SYPROnoncanon", # add new assignments
                              variable %in% add_to_gotcha ~ "errata",
                              
                              .default = subset)) |>  # add new assignments
    
    # assignments dropped and not re-added will be removed from the dataset entirely 
    
    # update the subset assignments according to the new approach to manual edits
    mutate(subset = case_when(variable %in% move_to_canon & dye != "SYPRO" ~ "canon",
                              variable %in% move_to_noncanon & dye != "SYPRO" ~ "noncanon",
                              variable %in% move_to_canon & dye == "SYPRO" ~ "SYPROcanon",
                              variable %in% move_to_noncanon & dye == "SYPRO" ~ "SYPROnoncanon",
                              variable %in% move_to_errata ~ "errata",
                              
                              .default = subset)) |> 
    
    # drop the variables to be removed
    filter(!variable %in% drop_from_dsfbase) |> 
    
    # all must have an assignment
    filter(!is.na(subset)) 

  if(nrow(dsf) == 0 ){
    print("No entries saved from this screen.")
    return(NULL)
  }
  
  #### ---------- test the data
  # dataset value contains one of the accepted names
  # test for a pre-subset assignment dsfbase tibble
  test_dataset(dsf)
  
  ### ---------- save the data
  # make name for saving
  exp_num <- unique(dsf$exp_num)
  protein_name <- unique(dsf$protein)
  dataset_name <- glue::glue("{exp_num}_{protein_name}")
  
  ##### write a test to ensure
  # there are no duplicate assignments
  .save_name <- glue::glue("{save_path}{dataset_name}_all_subsets.rds")
  write_rds(dsf, .save_name)

  plot_by_subset(dsf, save_path, dataset_name)
  
  ## subsumed into `plot_by_subset()`
  ## code left here because the data was generated prior to the creation of the 
  ## `plot_by_subset()` function
  
  # # save each plot
  # .subsets <- c("SYPROcanon", "SYPROnoncanon", "canon", "noncanon", "errata")
  # .saved_pdfs <- c()
  # for(.each_subset in .subsets){
  #   
  #   dsf_sub <- dsf |> 
  #     filter(subset == .each_subset) 
  #   
  #   if(nrow(dsf_sub) > 0){
  #     head(dsf_sub)
  #     
  #     ## in plotting .R function script?
  #     save_plot_by_var(dsf_sub,
  #                      save_path = save_path,
  #                      use_external_name = TRUE,
  #                      external_name = glue::glue("{dataset_name}_{.each_subset}"))
  #     
  #     .saved_pdfs[.each_subset] <- glue::glue("{save_path}{dataset_name}_{.each_subset}.pdf")
  #   }
  # }
  # 
  # # annoying work around ,because hrbrthemes and patchwork don't work together
  # # combine the subsets in to a single pdf
  # qpdf::pdf_combine(input = .saved_pdfs,
  #                   output = glue::glue("{save_path}{dataset_name}_all_subsets.pdf"))
  # 
  # # remove the individual subset pdfs 
  # fs::file_delete(.saved_pdfs)
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
          axis.text.x=element_blank(),
          plot.title = element_text(size = 12,
                                    hjust = 0.5),
          plot.subtitle = element_text(size = 10,
                                       hjust = 0.5))
  
  p <- p +
    labs(title = dataset_name,
         subtitle = str_wrap(glue::glue("{subset_mg} \n 
                                 {glue::glue_collapse(tallies[c(2,3)], sep = '\n')}"), 80))
  
  
  ## get save name
  #save_dir <- glue::glue("{save_path}/{subset}/")
  #fs::dir_create(save_dir)
  #save_name <- glue::glue("{save_dir}{dataset_name}.pdf")
  
  save_name <- glue::glue("{save_path}{dataset_name}.pdf")
  save_height <- 1.3*(n_distinct(dsf$variable)/5) + 3
  
  ggsave(save_name,
         p,
         width = 7,
         height = save_height,
         limitsize = FALSE
  )
}


plot_by_subset <- function(dsf, save_path, dataset_name){
  
  # save each plot
  .subsets <- c("SYPROcanon", "SYPROnoncanon", "canon", "noncanon", "errata")
  .saved_pdfs <- c()
  for(.each_subset in .subsets){
    
    dsf_sub <- dsf |> 
      filter(subset == .each_subset) 
    
    if(nrow(dsf_sub) > 0){
      head(dsf_sub)
      
      ## in plotting .R function script?
      save_plot_by_var(dsf_sub,
                       save_path = save_path,
                       use_external_name = TRUE,
                       external_name = glue::glue("{dataset_name}_{.each_subset}"))
      
      .saved_pdfs[.each_subset] <- glue::glue("{save_path}{dataset_name}_{.each_subset}.pdf")
    }
  }
  
  # annoying work around ,because hrbrthemes and patchwork don't work together
  # combine the subsets in to a single pdf
  qpdf::pdf_combine(input = .saved_pdfs,
                    output = glue::glue("{save_path}{dataset_name}_all_subsets.pdf"))
  
  # remove the individual subset pdfs 
  fs::file_delete(.saved_pdfs)
  
}

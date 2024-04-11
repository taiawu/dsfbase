#helper function to reformat dsfbase for umap

prep_for_UMAP <- function(dsfbase, value_col, drop_mac1 = TRUE) {
  if(drop_mac1){
    print("removing `SP150_Nsp3` protein from results")
    dsfbase <- dsfbase |> 
      filter(protein_original != "SP150_Nsp3")
  }
  
  out <- dsfbase |> 
    select(all_of(c("id", "protein",  "subset", "Temperature", value_col))) |> 
    pivot_wider(id_cols = c("id", "protein", "subset"),
                names_from = "Temperature",
                values_from = value_col)
  
}


# Wrapper function to perform, plot, and save multiple UMAPs iterating through various parameters

explore_UMAP_pars <- function(dsfbase,
                              .n_neighbors,
                              .min_dists,
                              .save_name = "dsfbase_umap_",
                              .save_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/05_analyses/1_UMAP/optimizations/",
                              .save_input = TRUE,
                              .save_input_to = "input/",
                              ...){
  
  fs::dir_create(glue::glue("{.save_to}{.save_input_to}"))
  write_rds(dsfbase, glue("{.save_to}{.save_input_to}dsfbase_umap_input.rds"))
  
  map2(.x = .n_neighbors, 
       .y = .min_dists,
       .f = create_and_save_UMAP,
        dsfbase = dsfbase,
       .save_name = .save_name,
       .save_to = .save_to,
       ...
  )
  
}


# Wrapper function to perform, plot, and save UMAPs based on input parameters

create_and_save_UMAP <- function(dsfbase, 
                                 .n_neighbors = 10,
                                 .min_dist = 0.01,
                                 .save_name = "dsfbase_umap_",
                                 .save_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/05_analyses/1_UMAP/optimizations/",
                                 .save_facet = TRUE,
                                 .save_nonfacet = FALSE,
                                 ...){
  
  .save_data_to <- glue("{.save_to}results/")
  fs::dir_create(.save_data_to)
  
  UMAP_prep <- 
    dsfbase |> # remove, will overwhelm
    prep_UMAP(.n_neighbors = .n_neighbors,
              .min_dist = .min_dist,
              ...) # e.g. could set a different seed than default 
  
  UMAP_juice <- UMAP_prep |> 
    juice() 
  
  write_rds(UMAP_prep, glue("{.save_data_to}{.save_name}prep_neighbors_{.n_neighbors}_mindist_{.min_dist}.rds"))
  
  write_rds(UMAP_juice, glue("{.save_data_to}{.save_name}juice_neighbors_{.n_neighbors}_mindist_{.min_dist}.rds"))
  
  # plot and save UMAP
  
  # if(.save_facet){
  #   
  # }
  plot_UMAP(UMAP_juice |> filter(subset != "gotcha"), 
            .n_neighbors = .n_neighbors, 
            .min_dist = .min_dist,
            .facet = TRUE,
            .save_to = glue("{.save_to}plots/"),
            ...) # faceted by subset
  
  if(.save_nonfacet){
    plot_UMAP(UMAP_juice |> filter(subset != "gotcha"), 
              .n_neighbors = .n_neighbors, 
              .min_dist = .min_dist,
              .facet = FALSE,
              .save_to = glue("{.save_to}plots/"),
              ...) # and not faceted by subset
  }
  
}


# Function to perform UMAP with given hyperparameters

prep_UMAP <- function(dsfbase, 
                      .n_neighbors = 10, 
                      .min_dist = 0.01,
                      .seed = c(21712, 79097),
                      .print_msg = TRUE,
                      ...){
  
  if(.print_msg){
      msg <- glue("Performing UMAP with {.n_neighbors} neighbors and a min distance of {.min_dist}. \n UMAP seed: {glue_collapse(.seed, sep = ', ')}")
      print(msg)
  }
  
  umap_rec <- recipe(~., data = dsfbase) %>%
    update_role(all_of(c("id", "protein", "subset")), new_role = "id") %>%
    #update_role(id, protein, subset, new_role = "id") %>%
    step_normalize(all_predictors()) %>%
    step_umap(all_predictors(),
              seed = .seed,
              neighbors = .n_neighbors,
              min_dist = .min_dist)
  
  umap_prep <- prep(umap_rec) # ~10 secs for 
}


# --- Function to plot UMAP results with given hyperparameters

plot_UMAP <- function(UMAP_data,
                      .n_neighbors,
                      .min_dist,
                      .facet = TRUE,
                      .other_notes = "Nsp3 mac1 screen excluded from analysis.",
                      .x = "UMAP1",
                      .y = "UMAP2",
                      .save_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/05_analyses/1_UMAP/optimizations/plots/",
                      .save_name = "dsfbase_umap_",
                      .dsfbase_colors = c("SYPRO_canon" = "#FABA39",
                                          "canon" = "#E6450C",
                                          "SYPRO_noncanon" = "#1BE4B7",
                                          "noncanon" = "#4686FA"),
                      .return_plot = FALSE,
                      ...) {
  
  ## --- create plot names and annotations
  .plot_title <- glue("UMAP of DSFbase. \n {.n_neighbors} neighbors; {.min_dist} min dist.")
  .plot_subtitle <- glue("Parameters: neighbors = {.n_neighbors}, min_dist = {.min_dist}. \n Gotcha subset not displayed. \n Notes: {.other_notes}")
  
  # create the save location, if it isn't already there
  fs::dir_create(.save_to)
  
  
  ## --- make plot
  p_UMAP <- 
    UMAP_data |> 
    ggplot(aes(.data[[.x]], .data[[.y]])) +
    
    labs(color = NULL) +
    
    # color by subset, manually
    scale_color_manual(values = .dsfbase_colors) +
    
    # add labels
    labs(title = .plot_title,
         subtitle = str_wrap(.plot_subtitle, 50)) +
    
    # adjust theme
    hrbrthemes::theme_ipsum(base_size = 20,
                            strip_text_size = 20) +
    theme(aspect.ratio = 1) +
    
    # increase size and alpha in guides to make them visible
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) # don't make legend really transparent
  
  ### # facet by subset
  if(.facet) {
    p_UMAP <- p_UMAP + 
      # add grey background points for full UMAP result
      geom_point(data = select(UMAP_data, -subset),
                 color = "grey", 
                 size = 0.05, 
                 alpha = 0.5) +
      
      # color points for each subset
      geom_point(aes(color = subset), 
                 alpha = 0.8, 
                 size = 0.5) +
      
      facet_wrap(~subset, ncol = 2, scales= "free") 
    
    .save_width <- 15
    .save_height <- 15
    
    .save_name <-glue("{.save_to}{.save_name}{.n_neighbors}_min_dist_{.min_dist}_facet.pdf")
    
  } else {
    
    p_UMAP <- p_UMAP + 
      # color points for each subset
      geom_point(aes(color = subset), 
                 alpha = 0.2, 
                 size = 0.1) 
    
    .save_name <-glue("{.save_to}{.save_name}{.n_neighbors}_min_dist_{.min_dist}.pdf")
    
    .save_width <- 10
    .save_height <- 10
    
  }
  
  # return the plot without saving, if desired
  # used this to make the main panel figure
  if(.return_plot){
    return(p_UMAP)
  }
  
  # defualt to saving
  ggsave(.save_name,
         width = .save_width, # facet plots get saved larger
         height = .save_height, # facet plots get saved larger
         device = cairo_pdf)
  
}


plot_UMAP_by_protein <- function(UMAP_data,
                                 .n_neighbors,
                                 .min_dist,
                                 .facet = TRUE,
                                 .other_notes = "Nsp3 mac1 screen excluded from analysis.",
                                 .x = "UMAP1",
                                 .y = "UMAP2",
                                 .save_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/05_analyses/1_UMAP/optimizations/plots/",
                                 .save_name = "dsfbase_umap_",
                                 ...) {
  
  ## --- create plot names and annotations
  .plot_title <- glue("UMAP of DSFbase. \n {.n_neighbors} neighbors; {.min_dist} min dist.")
  .plot_subtitle <- glue("Parameters: neighbors = {.n_neighbors}, min_dist = {.min_dist}. \n Gotcha subset not displayed. \n Notes: {.other_notes}")
  
  
  
  ## --- make plot
  p_UMAP <- 
    UMAP_data |> 
    ggplot(aes(x = .data[[.x]], y = .data[[.y]])) +
    
    geom_point(data = select(UMAP_data, -protein),
               color = "grey", 
               size = 0.05, 
               alpha = 0.2) +
    
    # color points for each subset
    geom_point(color = "red",
               alpha = 0.8,
               size = 0.5) +
    # geom_point(aes(color = protein), 
    #            alpha = 0.8, 
    #            size = 0.2) +
    
    facet_wrap(~protein, ncol = 6, scales = "fixed",
               labeller = label_wrap_gen(width = 25, multi_line = TRUE))  +
    
    
    # # color points for each subset
    # geom_point(aes(color = protein), 
    #            alpha = 0.3, 
    #            size = 0.4) +
    
    labs(color = NULL) +
    
    # color by subset, manually
    scale_color_viridis_d(option = "turbo") +
    #scale_color_manual(values = .dsfbase_colors) +
    
    # add labels
    labs(title = .plot_title,
         subtitle = str_wrap(.plot_subtitle, 50)) +
    
    # adjust theme
    hrbrthemes::theme_ipsum(base_size = 16,
                            strip_text_size = 12) +
    theme(aspect.ratio = 1,
          legend.position = "none",
          legend.text = element_text(size = 6)) +
    
    # increase size and alpha in guides to make them visible
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1)))  # don't make legend really transparent
  
  
  
  .save_name <-glue("{.save_to}{.save_name}{.n_neighbors}_min_dist_{.min_dist}_by_protein.pdf")
  
  ggsave(.save_name,
         width = 15, # facet plots get saved larger
         height = 60, # facet plots get saved larger
         limitsize = FALSE,
         device = cairo_pdf)
  
}


plot_UMAP_with_density <- function(UMAP_data,
                                   .n_neighbors,
                                   .min_dist,
                                   .facet = TRUE,
                                   .other_notes = "Nsp3 mac1 screen excluded from analysis.",
                                   .x = "UMAP1",
                                   .y = "UMAP2",
                                   .save_to = "/Users/taiaseanwu/Desktop/programming/dsfbase/05_analyses/1_UMAP/optimizations/plots/",
                                   .save_name = "dsfbase_umap_",
                                   .dsfbase_colors = c("SYPRO_canon" = "#FABA39",
                                                       "canon" = "#E6450C",
                                                       "SYPRO_noncanon" = "#1BE4B7",
                                                       "noncanon" = "#4686FA"),
                                   ...) {
  
  ## --- create plot names and annotations
  .plot_title <- glue("UMAP of DSFbase. \n {.n_neighbors} neighbors; {.min_dist} min dist.")
  .plot_subtitle <- glue("Parameters: neighbors = {.n_neighbors}, min_dist = {.min_dist}. \n Gotcha subset not displayed. \n Notes: {.other_notes}")
  
  
  
  ## --- make plot
  p_UMAP <- 
    UMAP_data |> 
    ggplot(aes(.data[[.x]], .data[[.y]])) +
    
    labs(color = NULL) +
    
    # color by subset, manually
    scale_color_manual(values = .dsfbase_colors) +
    
    # add labels
    labs(title = .plot_title,
         subtitle = str_wrap(.plot_subtitle, 50)) +
    
    # adjust theme
    hrbrthemes::theme_ipsum(base_size = 20,
                            strip_text_size = 20) +
    theme(aspect.ratio = 1) +
    
    # increase size and alpha in guides to make them visible
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) # don't make legend really transparent
  
  ### # facet by subset
  if(.facet) {
    p_UMAP <- p_UMAP + 
      # add grey background points for full UMAP result
      geom_point(data = select(UMAP_data, -subset),
                 color = "grey", 
                 size = 0.05, 
                 alpha = 0.5) +
      
      # color points for each subset
      geom_point(aes(color = subset), 
                 alpha = 0.8, 
                 size = 0.5) +
      
      
      facet_wrap(~subset, ncol = 2, scales= "free") 
    
    .save_width <- 15
    .save_height <- 15
    
    .save_name <-glue("{.save_to}{.save_name}{.n_neighbors}_min_dist_{.min_dist}_facet.pdf")
    
  } else {
    
    p_UMAP <- p_UMAP + 
      # color points for each subset
      geom_point(aes(color = subset), 
                 alpha = 0.2, 
                 size = 0.1) 
    
    .save_name <-glue("{.save_to}{.save_name}{.n_neighbors}_min_dist_{.min_dist}.pdf")
    
    .save_width <- 10
    .save_height <- 10
    
  }
  
  
  ggsave(.save_name,
         width = .save_width, # facet plots get saved larger
         height = .save_height, # facet plots get saved larger
         device = cairo_pdf)
  
}


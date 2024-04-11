# plot all entries for a given subsert
plot_subset <- function(dsfbase, 
                        .subset, 
                        save_path = "../03_combined_data/plots",
                        .test_subset = TRUE,
                        .facet_by = "short_id"){
  
  
  
  dsf <- dsfbase |> 
    mutate(short_id = str_sub(id, start = -8, end = -1)) |> 
    filter(subset == .subset) |> 
    group_by(id) |> 
    mutate(value_norm = scales::rescale(value, c(0,1))) 
  
  
  if(.test_subset) { # take only the first 20
    dsf <- dsf |> 
      filter(id %in% unique(dsf$id)[c(1:20)])
  }
  
  plot_title <- glue("DSFbase, all datasets in `{.subset}` subset")
  n_prot <- n_distinct(dsf$protein)
  n_ids <- n_distinct(dsf$id)
  plot_subtitle <- glue("{n_ids} datasets from {n_prot} proteins")
  
  p <- dsf |>  ggplot(aes(x = Temperature, y = value_norm)) +
    geom_line(linewidth = 0.3) +
    facet_wrap(~.data[[.facet_by]], scales = "free", ncol = 20) +
    hrbrthemes::theme_ipsum(base_family = "Helvetica",
                            base_size = 5,
                            strip_text_size = 8) +
    hrbrthemes::theme_ipsum(base_family = "Helvetica") +
    theme(aspect.ratio = 1,
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          panel.spacing = unit(.2, "lines"),
          strip.text = element_text(size = 5, margin = margin(0.2,0,0,0, "cm"))) +
    facet_wrap(~short_id, ncol = 10) +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         y = "Normalized RFU",
         x = "Temperature (ºC)")
  
  ## get save name
  save_name <- glue::glue("{save_path}/{.subset}.pdf")
  save_height <- (7/10)*(n_distinct(dsf$id)/10) + 3
  
  ggsave(save_name,
         p,
         width = 7,
         height = save_height,
         limitsize = FALSE
  )
  
  p
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
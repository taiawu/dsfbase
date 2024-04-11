# function
.save_outputs_to <- "/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/02_subset_amendments/reassigned_data/"
.final_plot_subfolder <- "reassigned_plots/"
.final_data_subfolder <- "reassigned_data/"
.save_full_database_plots_to <- glue::glue("{.save_outputs_to}reassigned_plots/")
.save_in_progress_data_to <- glue::glue("{.save_outputs_to}reassigned_data/")

.BACKUP_save_full_database_plots_to <- glue::glue("{.save_outputs_to}previous_reassignment_backup/reassigned_plots/")
.BACKUP_save_in_progress_data_to <- glue::glue("{.save_outputs_to}previous_reassignment_backup/reassigned_data/")
# .save_outputs_to <- "/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/updatd_subset_assignments/"
# .final_plot_subfolder <- "reassigned_plots/"
# .final_data_subfolder <- "reassigned_data/"
# .save_full_database_plots_to <- glue::glue("{.save_outputs_to}reassigned_plots/")
# .save_in_progress_data_to <- glue::glue("{.save_outputs_to}reassigned_data/")
# .BACKUP_save_full_database_plots_to <- glue::glue("{.save_outputs_to}previous_reassignment_backup/reassigned_plots/")
# .BACKUP_save_in_progress_data_to <- glue::glue("{.save_outputs_to}previous_reassignment_backup/reassigned_data/")
# input_choices <- c("SYPROcanon", "canon", "SYPROmidcanon", "midcanon","SYPROnoncanon", "noncanon", "SYPROlatenoncanon","latenoncanon", "errata", "drop")

save_reassignments <- function(reassigned,
                               dsfbase_raw,
                               save_full_database_plots_to = .save_full_database_plots_to,
                               save_in_progress_data_to = .save_in_progress_data_to,
                               BACKUP_save_full_database_plots_to = .BACKUP_save_full_database_plots_to,
                               BACKUP_save_in_progress_data_to = .BACKUP_save_in_progress_data_to){
  
  # create any dirs that don't exist  
  fs::dir_create(.save_full_database_plots_to)
  fs::dir_create(.save_in_progress_data_to)
  fs::dir_create(.BACKUP_save_full_database_plots_to)
  fs::dir_create(.BACKUP_save_in_progress_data_to)
  
  # copy the previous version into a "backup" directory, just in case
  # copy the current version to a backup folder
  fs::dir_copy(path = .save_full_database_plots_to, 
               new_path = .BACKUP_save_full_database_plots_to,
               overwrite = TRUE)
  
  fs::dir_copy(path = .save_in_progress_data_to, 
               new_path = .BACKUP_save_in_progress_data_to,
               overwrite = TRUE)
  
  # restore_previous_reassignments()
  # restore_previous_reassignments()
  
  ## iterate each time
  reassigned <- reassigned |>  
    mutate(reviewed = TRUE)
  
  # update dsfbase with the new subset assignments
  dsfbase_updated <- dsfbase_raw |> 
    filter(!id %in% reassigned$id) |> 
    bind_rows(reassigned)
  
  # # save all of the plots
  # plot_all_subsets(dsfbase_updated, 
  #                  ncol = 10, 
  #                  .save = TRUE,
  #                  .save_to = .save_full_database_plots_to)
  
  # save the updated data
  write_rds(x = dsfbase_updated,
            file = glue::glue("{.save_in_progress_data_to}dsfbase_updated_subset_labels.rds"))
}


reassign_ids <- function(dsfbase, 
                         single_click, double_click, brush_click,
                         single_click_meaning, double_click_meaning, brush_meaning){
  
  dsfbase_reassign <- dsfbase |> 
    
    # uses the first match only when all are given in a single case_when
    mutate(subset = case_when(id %in% single_click ~ single_click_meaning, .default = subset)) |> 
    mutate(subset = case_when(id %in% double_click ~ double_click_meaning, .default = subset)) |> 
    mutate(subset = case_when(id %in% brush_click  ~ brush_meaning, .default = subset)) 
}

take_entries <- function(dsfbase, .start_entry, .n_entries, .subset = "all"){
  
  if(.subset != "all"){ # use all subsets, if that's desired
    dsfbase <- dsfbase |> filter(subset %in% .subset) 
  }
  
  .start_entry <- as.numeric(.start_entry)
  .n_entries <- as.numeric(.n_entries)
  
  id_list <-  sort(unique(dsfbase$id)) #[c(.start_id:(.start_id+.n_ids))])
  
  if(.start_entry + .n_entries < length(id_list) ) {
    end_entry <- .start_entry + .n_entries
  } else {
    end_entry <- length(id_list) 
  }
  
  .use_ids <- id_list[.start_entry:end_entry]
  
  dsfbase_sub <- dsfbase |> 
    filter(id %in% .use_ids)
}

plot_subsets_by_color <- function(dsfbase, ncol = 10){
  
  dsfbase <- dsfbase |>  
    #mutate(subset_f = factor(subset, levels = c("SYPROcanon", "canon", "SYPROnoncanon", "noncanon", "errata", "drop"))) |> 
    arrange(subset_f)
  
  subset_colors <- c("SYPROcanon" = "#E6450C",
                     "canon" = "#E6450C", 
                     "SYPROmidcanon" = "#FABA39",
                     "midcanon" = "#FABA39",
                     "SYPROnoncanon" = "#4686FA",
                     "noncanon" = "#4686FA", 
                     "SYPROlatenoncanon" = "#1BE4B7", 
                     "latenoncanon" = "#1BE4B7", 
                     "errata" = "black", 
                     "drop" = "grey")
  
  p <- dsfbase |>  
    #filter(subset == .subset) |> 
    ggplot(aes(x = Temperature, y = value_norm, color = subset)) +
    scale_color_manual(values = subset_colors) +
    geom_line(linewidth = 0.3) +
    facet_wrap(~id, scales = "free", ncol = ncol) +
    hrbrthemes::theme_ipsum(base_family = "Helvetica",
                            base_size = 5,
                            strip_text_size = 8) +
    hrbrthemes::theme_ipsum(base_family = "Helvetica") +
    theme(aspect.ratio = 1,
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          panel.spacing = unit(.2, "lines"),
          strip.text = element_text(size = 5, margin = margin(0.2,0,0,0, "cm"))) +
    labs(y = "Normalized RFU",
         x = "Temperature (ºC)")
  
}

plot_by_subset <- function(dsfbase, .subset, ncol = 10){
  
  dsfbase <- dsfbase |>  
    filter(subset == .subset)
  
  p <- dsfbase |>  
    ggplot(aes(x = Temperature, y = value_norm)) +
    geom_line(linewidth = 0.3) +
    facet_wrap(~id, scales = "free", ncol = ncol) +
    hrbrthemes::theme_ipsum(base_family = "Helvetica",
                            base_size = 5,
                            strip_text_size = 8) +
    hrbrthemes::theme_ipsum(base_family = "Helvetica") +
    theme(aspect.ratio = 1,
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          panel.spacing = unit(.2, "lines"),
          strip.text = element_text(size = 5, margin = margin(0.2,0,0,0, "cm"))) +
    labs(title = .subset,
         y = "Normalized RFU",
         x = "Temperature (ºC)")
  
  p 
}

plot_all_subsets <- function(reassigned, ncol = 10, 
                             .save = FALSE, 
                             .save_to = .save_full_database_plots_to ){
  
  all_subsets <-  c("SYPROcanon", "canon", "SYPROmidcanon", "midcanon","SYPROnoncanon", "noncanon", "SYPROlatenoncanon","latenoncanon", "errata", "drop")
  use_subsets <- all_subsets[all_subsets %in% reassigned$subset]
  
  plotlist <- lapply(use_subsets, plot_by_subset, ncol = ncol, dsfbase = reassigned)
  
  patch <- patchwork::wrap_plots(plotlist, ncol = 1)
  
  if(.save){
    ## get save name
    save_name <- glue::glue("{.save_to}/full_dsfbase_updated_subsets.pdf")
    save_height <- (7/10)*(n_distinct(reassigned$id)/10) + 10
    
    ggsave(save_name,
           patch,
           width = 7,
           height = save_height,
           limitsize = FALSE)
  }
  
  patch
}

get_subset_table <- function(reassigned){
  reassigned |> 
    select(id, subset) |> 
    distinct() 
}

read_dsfbase <- function(DSFBASE_DIR){
  dsfbase <- read_rds(DSFBASE_DIR)
  if(!"reviewed" %in% names(dsfbase)) {
    dsfbase <- dsfbase |> 
      mutate(reviewed = FALSE)
  }
  
  dsfbase
}

restore_previous_reassignments <- function(save_full_database_plots_to = .save_full_database_plots_to, 
                                           BACKUP_save_full_database_plots_to = .BACKUP_save_full_database_plots_to,
                                           save_in_progress_data_to = .save_in_progress_data_to,
                                           BACKUP_save_in_progress_data_to = .BACKUP_save_in_progress_data_to){
  
  # copy the current version to a backup folder
  fs::dir_copy(path = BACKUP_save_full_database_plots_to, 
               new_path = save_full_database_plots_to, 
               overwrite = TRUE)
  
  fs::dir_copy(path = BACKUP_save_in_progress_data_to,
               new_path = save_in_progress_data_to, 
               overwrite = TRUE)
}
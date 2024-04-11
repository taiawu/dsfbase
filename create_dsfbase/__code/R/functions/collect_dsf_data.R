## all functions have been divided into other, smaller and more specific scripts.

# ## for dye screens
# # ## read, test, plot, save, and manually edit subsets from a dye screen
# dye_screen_to_dsfbase <- function(PATH,
#                                   save_path = "../02_aggregated_data/dye_screen_tests/",
#                                   save_data = TRUE,
#                                   min_value = 10000,
#                                   min_range = 10000,
#                                   move_to_canon = c(),
#                                   move_to_noncanon = c(),
#                                   move_to_errata = c(),
#                                   drop_from_dsfbase = c(),
#                                   
#                                   add_to_canon = c(),
#                                   drop_from_canon = c(),
#                                   
#                                   add_to_noncanon = c(),
#                                   drop_from_noncanon = c(),
#                                   
#                                   add_to_SYPRO_canon = c(),
#                                   drop_from_SYPRO_canon = c(),
#                                   
#                                   add_to_SYPRO_noncanon = c(),
#                                   drop_from_SYPRO_noncanon = c(),
#                                   
#                                   add_to_gotcha = c(),
#                                   
#                                   is_caspase = FALSE,
#                                   drop_full_screen = FALSE,
#                                   ...) {
#   
#   # do nothing, if the goal is to drop everything from the screen
#   if(drop_full_screen) {
#     # alert user
#     print("adding no entries from screen")
#     print(PATH)
#     # and then do nothing more
#     return()
#   }
#   
#   #------------ read in and tidy the Exp1252 dataset
#   dsf <- tidy_1252(PATH, is_caspase = is_caspase, subset = "placeholder", ...) |>
#     group_by(dye) |>
#     
#     # drop low-signal curves, for simplicity
#     filter(any(value > min_value),
#            any(max(value) - min(value) > min_range)) |>
#     ungroup() |>
#     
#     # make the original, hit-call based assignments
#     # not keeping any of the nones
#     filter(assignment != "none") |>
#     
#     # sensitives to go non-canon, and hits go to canon
#     mutate(subset = case_when(assignment == "hit" & dye != "SYPRO" ~ "canon",
#                               assignment == "sensitive"& dye != "SYPRO" ~ "noncanon",
#                               assignment == "hit" & dye == "SYPRO" ~ "SYPROcanon",
#                               assignment == "sensitive" & dye == "SYPRO" ~ "SYPROnoncanon",
#     )) |> 
#     
#     # update the subset assignments base on the old reassignment approach
#     mutate(subset = case_when(variable %in% drop_from_canon ~ NA, # remove an overwritten assignment
#                               variable %in% drop_from_noncanon ~ NA,  # remove an overwritten assignment
#                               variable %in% drop_from_SYPRO_canon ~ NA,  # remove an overwritten assignment
#                               variable %in% drop_from_SYPRO_noncanon ~ NA, # remove an overwritten assignment
#                               
#                               variable %in% add_to_canon ~ "canon", # add new assignments
#                               variable %in% add_to_noncanon ~ "noncanon", # add new assignments
#                               variable %in% add_to_SYPRO_canon ~ "SYPROcanon", # add new assignments
#                               variable %in% add_to_SYPRO_noncanon ~ "SYPROnoncanon", # add new assignments
#                               variable %in% add_to_gotcha ~ "errata",
#                               
#                               .default = subset)) |>  # add new assignments
#     
#     # assignments dropped and not re-added will be removed from the dataset entirely 
#     
#     # update the subset assignments according to the new approach to manual edits
#     mutate(subset = case_when(variable %in% move_to_canon & dye != "SYPRO" ~ "canon",
#                               variable %in% move_to_noncanon & dye != "SYPRO" ~ "noncanon",
#                               variable %in% move_to_canon & dye == "SYPRO" ~ "SYPROcanon",
#                               variable %in% move_to_noncanon & dye == "SYPRO" ~ "SYPROnoncanon",
#                               variable %in% move_to_errata ~ "errata",
#                               
#                               .default = subset)) |> 
#     
#     # drop the variables to be removed
#     filter(!variable %in% drop_from_dsfbase) |> 
#     
#     # all must have an assignment
#     filter(!is.na(subset)) 
#     
#   # |>  add_dsfbase_variable_col()
#     
#    
#   
#   if(nrow(dsf) == 0 ){
#     print("No entries saved from this screen.")
#     return(NULL)
#   }
#   
#   #### ---------- test the data
#   # dataset value contains one of the accepted names
#   # test for a pre-subset assignment dsfbase tibble
#   test_dataset(dsf)
#   
#   ### ---------- save the data
#   # make name for saving
#   exp_num <- unique(dsf$exp_num)
#   protein_name <- unique(dsf$protein)
#   dataset_name <- glue::glue("{exp_num}_{protein_name}")
#   
#   ##### write a test to ensure
#   # there are no duplicate assignments
#   .save_name <- glue::glue("{save_path}{dataset_name}_all_subsets.rds")
#   write_rds(dsf, .save_name)
#   
#   # save each plot
#   .subsets <- c("SYPROcanon", "SYPROnoncanon", "canon", "noncanon", "errata")
#   .saved_pdfs <- c()
#   for(.each_subset in .subsets){
#     
#     dsf_sub <- dsf |> 
#       filter(subset == .each_subset) 
#     
#     if(nrow(dsf_sub) > 0){
#       head(dsf_sub)
#       save_plot_by_var(dsf_sub,
#                        save_path = save_path,
#                        use_external_name = TRUE,
#                        external_name = glue::glue("{dataset_name}_{.each_subset}"))
#       
#       .saved_pdfs[.each_subset] <- glue::glue("{save_path}{dataset_name}_{.each_subset}.pdf")
#     }
#   }
#   
#   # annoying work around ,because hrbrthemes and patchwork don't work together
#   # combine the subsets in to a single pdf
#   qpdf::pdf_combine(input = .saved_pdfs,
#                     output = glue::glue("{save_path}{dataset_name}_all_subsets.pdf"))
#   
#   # remove the individual subset pdfs 
#   fs::file_delete(.saved_pdfs)
# }
# 
# ## read and tidy a directory from Exp1252 containing the dye screen for one protein
# tidy_1252 <- function(PATH, is_caspase = FALSE, subset = "canon", ...){
#   
#   if(is_caspase) { # caspase directories are only the tidied tibble
#     screen <- read_rds(PATH)
#   } else { # Exp1252 directories have a list, with tidied tibble as first element
#     screen <- read_rds(PATH)[[1]] 
#   }
#   
#   screen <- 
#     screen |> 
#     
#     # add well
#     separate(variable, into = c("well", "channel_drop", "type_drop"), 
#              sep = "_", # ignore period in Cy5.5
#              remove = FALSE) |> 
#     select(-c("channel_drop", "type_drop")) |> 
#     
#     # add DSFbase-specific columns
#     mutate(id = "placeholder",
#            subset = subset) |> 
#     
#     rename(protein = identity) |> 
#     
#     # keep only with-protein hits and sensitives
#     filter(type == "protein",
#            assignment != "none",
#            fit_channel == "fit")
#   
#   # print some in
#   n_datasets <- n_distinct(screen$variable)
#   print(glue::glue("Datasets: {n_datasets}"))
#   
#   screen
# }

### 


# read_qtower <- function( file_path,
#                          start_temp = 25,
#                          inc_temp = 1,
#                          channel_levels  = c("FAM", "JOE", "TAMRA", "ROX", "Cy5", "Cy5.5", "SyproOrange")
# ) {
#   #____Determine the length of the metadata header
#   # define the possible wells (on the fly bc its cheap)
#   wells <-
#     expand.grid(LETTERS[1:16], c(1:24)) %>%
#     tidyr::unite("well", sep = "") %>%
#     dplyr::pull(.data$well)
#   
#   # identify first row that contains a well
#   first_well_row <-
#     vroom::vroom(file_path, delim = ",", col_select = c(1)) %>%
#     purrr::set_names(c("col")) %>%
#     dplyr::mutate(is_well = .data$col %in% wells,
#                   first_well = .data$is_well == TRUE & !duplicated(.data$is_well == TRUE),
#                   row_num = dplyr::row_number()) %>%
#     dplyr::filter(.data$first_well == TRUE) %>%
#     dplyr::pull(.data$row_num) %>%
#     base::suppressMessages() %>% # suppressing column specification message
#     base::suppressWarnings()
#   
#   #_____Read the file in_____
#   df <-
#     vroom::vroom(file_path,
#                  delim = ",",
#                  skip = first_well_row-1,
#                  col_names =  FALSE,
#                  show_col_types = FALSE) %>%
#     purrr::set_names(c("well_channel", .[1,][-1])) %>%
#     dplyr::mutate(channel = dplyr::if_else(.data$well_channel %in% channel_levels,
#                                            true = .data$well_channel,
#                                            false = "well"),
#                   channel = na_if(.data$channel, "well")) %>%
#     tidyr::fill(.data$channel, .direction = "down") %>%
#     dplyr::filter(.data$well_channel != .data$channel) %>%
#     tidyr::unite("variable", c(.data$well_channel, .data$channel), sep = "_") %>%
#     tidyr::pivot_longer(-.data$variable, names_to = "Temperature", values_to = "value") %>%
#     dplyr::mutate(dplyr::across(.cols = c(.data$Temperature, .data$value), as.numeric),
#                   Temperature = (start_temp - 1) + (.data$Temperature * inc_temp)) %>% # convert cycle to temp
#     tidyr::separate(.data$variable, into = c("well", "channel"), sep = "_", remove = FALSE) %>%
#     dplyr::mutate(channel_f = factor(.data$channel, levels = channel_levels),
#                   value_norm = scales::rescale(.data$value, to = c(0,1))) %>%
#     dplyr::select(.data$variable, .data$well, .data$channel_f, .data$Temperature, .data$value, .data$value_norm)
#   
# }
# 
# tally_dsfbase <- function(TALLY_DIR = "../02_aggregated_data"){
#   
#   all_data_dirs <- fs::dir_ls(path = TALLY_DIR, 
#                               regexp = ".rds", recurse = 1)
#   
#   # initialize
#   n_SYPRO_canon_proteins <- 0
#   n_SYPRO_noncanon_proteins <- 0
#   n_canon_proteins <- 0
#   n_noncanon_proteins <- 0
#   n_SYPRO_canon_datasets <- 0
#   n_SYPRO_noncanon_datasets <- 0
#   n_canon_datasets <- 0
#   n_noncanon_datasets <- 0
#   
#   for(dsf in all_data_dirs){
#     int <- read_rds(dsf)
#     
#     if(grepl("/SYPRO_canon/", dsf)) {
#       n_SYPRO_canon_proteins <- n_SYPRO_canon_proteins + n_distinct(int$protein)
#       n_SYPRO_canon_datasets <- n_SYPRO_canon_datasets + n_distinct(int$variable)
#     } else if (grepl("/SYPRO_noncanon/|/noncanon/",dsf)) {
#       n_noncanon_proteins <- n_noncanon_proteins+ n_distinct(int$protein)
#       n_noncanon_datasets <- n_noncanon_datasets + n_distinct(int$variable)
#       
#     } else if (grepl("/canon/",dsf)) {
#       n_canon_proteins <- n_canon_proteins + n_distinct(int$protein)
#       n_canon_datasets <- n_canon_datasets + n_distinct(int$variable)
#     }
#   }
#   
#   msg <- glue::glue("{n_SYPRO_canon_proteins} proteins and {n_SYPRO_canon_datasets} datasets in SYPRO cannon,
#                    {n_canon_proteins} proteins and {n_canon_datasets} in cannon (not SYPRO),
#                    {n_noncanon_proteins} proteins and {n_noncanon_datasets} datasets in non-cannon")
#   
#   print(msg)
# }
# 
# test_dataset <- function(dsf) {
#   # expectation function doc
#   # https://testthat.r-lib.org/reference/expect_setequal.html
#   
#   test_that("data is a tibble", {
#     expect_s3_class(dsf, "tbl_df")
#   })
#   
#   # dataset value contains one of the accepted names
#   test_that("dataset contains one of the accepted values", {
#     subset_names <- c(
#       "SYPROcanon",
#       "SYPROnoncanon",
#       "canon",
#       "noncanon",
#       "errata",
#       "buffer"
#     )
#     expect_in(dsf$subset, subset_names)
#   })
#   
#   # test for post-assignments
#   test_that("all variables have a single assignment", {
#     int <- dsf |> 
#       select(variable, subset) |> 
#       distinct() |> 
#       group_by(variable) |> 
#       tally()
#     expect_true(all(int$n == 1)) })
#   
#   # dataset value contains one of the accepted names
#   test_that("data includes reqiured columns", {
#     correct_names <- c("id", "subset", "protein", "well", "variable", "Temperature", "value")
#     expect_in(correct_names, names(dsf))
#   })
#   
#   test_that("required columns have correct types", {
#     dsf_order <- dsf |>
#       select(all_of(c("id", "subset", "protein", "well", "variable", "Temperature", "value")))
#     
#     expect_equal(
#       unname(unlist(lapply(dsf_order, class))),
#       c(
#         "character", # "id",
#         "character", # ""subset"
#         "character", # protein
#         "character", # "well",
#         "character", # "variable"
#         "numeric", # "Temperature",
#         "numeric" # "value"
#       )
#     )
#   })
#   
#   test_that("data has 'variable' column which is genuinely unique to a single trace", {
#     temp_dup <- dsf %>%
#       dplyr::group_by(variable) %>%
#       dplyr::mutate(unique_temps = !duplicated(Temperature))
#     
#     expect_true(all(temp_dup$unique_temps))
#   })
#   
#   
#   test_that("all datasets contains 69 temperature measurements", {
#     temp_dup <- dsf %>%
#       dplyr::group_by(variable) %>%
#       dplyr::mutate(n_unique_temps = n_distinct(Temperature)) |> 
#       pull(n_unique_temps)
#     
#     expect_equal(unique(temp_dup), 70)
#   })
#   
#   test_that("dataset contains no NAs", {
#     required_cols <- c("id", "subset", "protein", "well", "variable", "Temperature", "value")
#     req <- dsf |> select(all_of(required_cols))
#     
#     # no NAs in the required columns. 
#     # permit NA in other random experimental condition columns 
#     expect_false(any(is.na(req)))
#   })
#   
# }

# save_plot_by_var <- function(dsf,
#                              save_path = "../02_aggregated_data/",
#                              use_external_name = FALSE,
#                              external_name = "",
#                              ...){
#   
#   # for plot annotations
#   # so it can be used inside other functions as well
#   if(use_external_name) {
#     dataset_name <- external_name
#   } else {
#     dataset_name <- deparse(substitute(dsf))
#   }
#   
#   tallies <- print_tallies(dsf)
#   subset <- unique(dsf$subset)
#   subset_mg <- glue::glue("Subset: {subset}")
#   
#   # make plot
#   p <- 
#     dsf |> 
#     ggplot(aes(x = Temperature, y = value, group = variable)) +
#     geom_line(linewidth = 0.5) +
#     facet_wrap(~variable, scales = "free", ncol = 5) +
#     hrbrthemes::theme_ipsum(base_family = "Helvetica",
#                             base_size = 5,
#                             strip_text_size = 8) +
#     theme(aspect.ratio = 1,
#           axis.text.y=element_blank(),
#           axis.text.x=element_blank(),
#           plot.title = element_text(size = 12,
#                                     hjust = 0.5),
#           plot.subtitle = element_text(size = 10,
#                                     hjust = 0.5))
#   
#   p <- p +
#     labs(title = dataset_name,
#          subtitle = str_wrap(glue::glue("{subset_mg} \n 
#                                  {glue::glue_collapse(tallies[c(2,3)], sep = '\n')}"), 80))
#   
#   
#   ## get save name
#   #save_dir <- glue::glue("{save_path}/{subset}/")
#   #fs::dir_create(save_dir)
#   #save_name <- glue::glue("{save_dir}{dataset_name}.pdf")
#   
#   save_name <- glue::glue("{save_path}{dataset_name}.pdf")
#   save_height <- 1.3*(n_distinct(dsf$variable)/5) + 3
#   
#   ggsave(save_name,
#          p,
#          width = 7,
#          height = save_height,
#          limitsize = FALSE
#   )
# }

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

# # helpfer function to print tallies for a given tibble
# print_tallies <- function(dsf) {
#   dataset_name <- deparse(substitute(dsf))
#   proteins <- unique(dsf$protein)
#   n_protein <- n_distinct(dsf$protein)
#   n_var <- n_distinct(dsf$variable)
# 
#   header <- glue::glue("`{dataset_name}` contains")
#   msg1 <- glue::glue("Number of unique proteins: {n_protein} ({glue::glue_collapse(proteins, sep = ', ')})")
#   msg2 <- glue::glue("Number of unique datasets: {n_var}")
# 
#   print(header, msg1, msg2, "\n")
#   out <- c(header, msg1, msg2, "\n")
#   #c(header, msg1, msg2, "\n")
# }


# # Helper function to print the variables so I can make 
# # reassignment vectors manually by copy-pasting
# print_quote <- function(vec, .sort_unique = TRUE){
#   if(.sort_unique) {
#     vec <- sort(unique(vec))
#   }
#   glue("{glue_collapse(double_quote(as.character(unique(vec))), sep = ', ')}")
# }


# # Helper function to do some of the repetetive steps to prepare raw files for DSFbase 
# prep_dsfbase <- function(dsf,
#                          .id = "placeholder",
#                          .subset = "placeholder",
#                          .buffer_name = "Buffer",
#                          .add_variable = FALSE,
#                          .add_variable_from = c("well", "channel_f")) {
#   dsf <- dsf |> 
#     mutate(id = "placeholder",
#            subset = "canon",
#            type = if_else(protein != .buffer_name,
#                           true = "protein",
#                           false = "buffer")) |>
#     filter(protein != "Buffer") 
#   
#   if(.add_variable){
#     dsf <- dsf |> 
#       unite(variable, .add_variable_from, remove = FALSE)
#   }
#   
#   dsf
#   
# }

# 
# # Function to prepare valdiation screens for plotting
# 
# prep_validation <- function(validation_raw, min_value = 10000) {
#   validation_raw |> 
#     mutate(id = "placeholder",
#            subset = "placeholder") |> 
#     rename(dye_conc_uM = conc) |>
#     select(-volume) |>
#     mutate(type = if_else(protein != "Buffer",
#                           true = "protein",
#                           false = "buffer")) |>
#     filter(type == "protein") |>
#     unite(variable, c(well, channel_f, protein), remove = FALSE) |>
#     group_by(variable) |>
#     filter(any(value > min_value)) |>
#     ungroup()
#   
# }



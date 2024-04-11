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

# helpfer function to print tallies for a given tibble
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

# Helper function to print the variables so I can make 
# reassignment vectors manually by copy-pasting
print_quote <- function(vec, .sort_unique = TRUE){
  if(.sort_unique) {
    vec <- sort(unique(vec))
  }
  glue("{glue_collapse(double_quote(as.character(unique(vec))), sep = ', ')}")
}

# # outdated function that doesn't work with the new file structure
# # to tally the current state of DSFbase 
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
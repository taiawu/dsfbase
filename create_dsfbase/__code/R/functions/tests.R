test_dataset <- function(dsf) {
  # expectation function doc
  # https://testthat.r-lib.org/reference/expect_setequal.html
  
  test_that("data is a tibble", {
    expect_s3_class(dsf, "tbl_df")
  })
  
  # dataset value contains one of the accepted names
  test_that("dataset contains one of the accepted values", {
    subset_names <- c(
      "SYPROcanon",
      "SYPROmidcanon",
      "SYPROnoncanon",
      "SYPROlatenoncanon",
      "canon",
      "midcanon",
      "noncanon",
      "latenoncanon",
      "errata",
      "buffer"
    )
    expect_in(dsf$subset, subset_names)
  })
  
  # test for post-assignments
  test_that("all variables have a single assignment", {
    int <- dsf |> 
      select(variable, subset) |> 
      distinct() |> 
      group_by(variable) |> 
      tally()
    expect_true(all(int$n == 1)) })
  
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


## old test of annotations -- needs work
test_annotated_dataset <- function(annotated, 
                            .use_template = FALSE, 
                            .external_template = NULL,
                            .id_col = "variable",
                            ...) {
  
  if(.use_template) {
    template <- .external_template
  } else {
    template <- tibble("id" = character(),
                       "well" = character(),
                       "exp_num" = character(),
                       "exp_summary" = character(),
                       "variable" = character(),
                       "protein" = character(),
                       "protein_concentration_uM" = numeric(),
                       
                       "dye" = character(),
                       "dye_concentration_uM" = numeric(),
                       "buffer" = character(),
                       "additive" = character(),
                       "additive_description" = character(),
                       "additive_concentration_uM" = numeric(),
                       "thermocycling_protocol" = character(),
                       "fluorescent_channel" = character(),
                       "instrument" = character(),
                       "additional_notes" = character(),
                       "directory" = character(),
                       "subset" = character(),
                       "subsubset" = character())
  }
  
  # ----- Column names
  test_that("annotations includes reqiured columns", {
    expect_in(names(template), names(annotated))
  })
  
  test_that("data has 'variable' column which is genuinely unique to a single trace", {
    temp_dup <- annotated %>%
      dplyr::group_by(.data[[.id_col]]) %>%
      dplyr::mutate(unique_temps = !duplicated(Temperature))
    
    expect_true(all(temp_dup$unique_temps))
  })
  
  test_that("all datasets contains 69 temperature measurements", {
    temp_dup <- annotated %>%
      dplyr::group_by(.data[[.id_col]]) %>%
      dplyr::mutate(n_unique_temps = n_distinct(Temperature)) |> 
      pull(n_unique_temps)
    
    expect_equal(unique(temp_dup), 70)
  })
  
  # dataset value contains one of the accepted names
  test_that("dataset contains one of the accepted values", {
    subset_names <- c(
      "SYPROcanon",
      "SYPROmidcanon",
      "SYPROnoncanon",
      "SYPROlatenoncanon",
      "canon",
      "midcanon",
      "noncanon",
      "latenoncanon",
      "speculative",
      "errata",
      "buffer"
    )
    
    testthat::expect_in(annotated$subset, subset_names)
  })
  
  # test for post-assignments
  test_that("all variables have a single assignment", {
    int <- annotated |> 
      select(all_of(c(.id_col, "subset"))) |> 
      distinct() |> 
      group_by(.data[[.id_col]]) |> 
      tally()
    expect_true(all(int$n == 1)) })
  
  # # ---- no duplicate ID annotations
  # test_that("Only one annotation per ID", {
  #   expect_true(n_distinct(annotated$id) == length(annotated$id))
  # })
  
}



### old test of annotations -- needs work
# test_annotation <- function(annotated, .use_template = FALSE, .external_template = NULL) {
#   
#   if(.use_template) {
#     template <- .external_template
#   } else {
#     template <- tibble("id" = character(),
#                        "well" = character(),
#                        "exp_num" = character(),
#                        "exp_summary" = character(),
#                        "variable" = character(),
#                        "protein" = character(),
#                        "protein_concentration_uM" = numeric(),
#                        
#                        "dye" = character(),
#                        "dye_concentration_uM" = numeric(),
#                        "buffer" = character(),
#                        "additive" = character(),
#                        "additive_description" = character(),
#                        "additive_concentration_uM" = numeric(),
#                        "thermocycling_protocol" = character(),
#                        "fluorescent_channel" = character(),
#                        "instrument" = character(),
#                        "additional_notes" = character(),
#                        "directory" = character(),
#                        "subset" = character())
#   }
#   
#   # ----- Column names
#   test_that("annotations includes reqiured columns", {
#     expect_in(names(template), names(annotated))
#   })
#   
#   # ---- no duplicate ID annotations
#   test_that("Only one annotation per ID", {
#     expect_true(n_distinct(annotated$id) == length(annotated$id))
#   })
#   
# }
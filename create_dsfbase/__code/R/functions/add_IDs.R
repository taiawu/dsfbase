# function to take a complete dsfbase RDS and add unique IDs
add_IDs <- 
  function(dsfbase,
           subset_order = c("SYPROcanon", "canon", "SYPROnoncanon", "noncanon", "errata"),
           dsfbase_version = 1,
           ...
  ){
    
    dsfbase_version <- stringr::str_pad(dsfbase_version, 2, pad = "0")
    
    # row_number() gives all 1s for the nested data so ... do it this way
    annotated_row_num <- dsfbase |> 
      
      select(variable, id, subset, subsubset_f, exp_num) |> 
      distinct() |> 
      arrange(subsubset_f, exp_num, variable) |> # adding IDs after subsubsets
      mutate(id_num = row_number())
    
    out <- dsfbase |> 
      left_join(annotated_row_num, by = join_by(variable, id, subset, subsubset_f, exp_num)) |> 
      group_by(variable, id, id_num, subset, exp_num) |> 
      nest() |> 
      mutate(subset_f = factor(subset, levels = subset_order)) |> 
      
      mutate(id_num = stringr::str_pad(id_num, 6, pad = "0")) |> 
      mutate(id = glue("DSFbase{dsfbase_version}_{subset}_ID{id_num}"),
             id = as.character(id)) |>
      unnest(cols = c(data)) |> 
      ungroup() |> 
      select(-id_num) |> 
      arrange(id)
  }
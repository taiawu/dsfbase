#
# This is a Shiny Web Application
# It is meant to run only locally -- it reads and writes files from hard-coded local directories. 
# It plots raw DSF data, faceted by curve, and then the user can edit the subset labels by clicking on the plots.  

library(glue)
library(shiny)
library(tidyverse)
library(patchwork)
library(fs)
source("R/app_functions.R")

# # user inputs, which will be updated, but initialize to NULL
# single_click_meaning <- "canon"
# double_click_meaning <- "noncanon"
# brush_meaning <- "drop"
# 
# .use_subset <- "all"
# .start_entry <- 0
# .n_entries <- 100

# user inputs, with defaults
DSFBASE_DIR <- "/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/02_subset_amendments/to_reassign/dsfbase_to_update.rds"
  # "/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/02_subset_amendments/reassigned_data/dsfbase_updated_subset_labels.rds"

### COPY THESE TO app_functions.R
.save_outputs_to <- "/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/02_subset_amendments/reassigned_data/"
.final_plot_subfolder <- "reassigned_plots/"
.final_data_subfolder <- "reassigned_data/"
.save_full_database_plots_to <- glue::glue("{.save_outputs_to}reassigned_plots/")
.save_in_progress_data_to <- glue::glue("{.save_outputs_to}reassigned_data/")

.BACKUP_save_full_database_plots_to <- glue::glue("{.save_outputs_to}previous_reassignment_backup/reassigned_plots/")
.BACKUP_save_in_progress_data_to <- glue::glue("{.save_outputs_to}previous_reassignment_backup/reassigned_data/")
  
#"/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/updatd_subset_assignments/reassigned_data/dsfbase_updated_subset_labels.rds"


# "/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/updatd_subset_assignments/previous_reassignment_backup/reassigned_data/dsfbase_updated_subset_labels_repeat_relabel.rds"
# "/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/updatd_subset_assignments/reassigned_data/dsfbase_updated_subset_labels.rds"
#"/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/updatd_subset_assignments/previous_reassignment_backup/reassigned_data/dsfbase_updated_subset_labels_repeat_relabel.rds"
#"/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/updatd_subset_assignments/reassigned_data/dsfbase_updated_subset_labels.rds"
#"/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/dsfbase_annotated.rds"


# 
# .save_outputs_to <- "/Users/taiaseanwu/Desktop/programming/dsfbase/20231205_version/02_combined_dsfbase/updatd_subset_assignments/"
# .final_plot_subfolder <- "reassigned_plots/"
# .final_data_subfolder <- "reassigned_data/"
# 
# .save_full_database_plots_to <- glue::glue("{.save_outputs_to}reassigned_plots/")
# .save_in_progress_data_to <- glue::glue("{.save_outputs_to}reassigned_data/")
# .BACKUP_save_full_database_plots_to <- glue::glue("{.save_outputs_to}previous_reassignment_backup/reassigned_plots/")
# .BACKUP_save_in_progress_data_to <- glue::glue("{.save_outputs_to}previous_reassignment_backup/reassigned_data/")
#input_choices <- c("SYPROcanon", "canon", "SYPROmidcanon", "midcanon","SYPROnoncanon", "noncanon", "SYPROlatenoncanon","latenoncanon", "errata", "drop")


# save_reassignments(values$reassigned, 
#                    values$dsfbase_raw,
#                    save_full_database_plots_to = .save_full_database_plots_to,
#                    save_in_progress_data_to = .save_in_progress_data_to,
#                    BACKUP_save_full_database_plots_to = .BACKUP_save_full_database_plots_to,
#                    BACKUP_save_in_progress_data_to = .BACKUP_save_in_progress_data_to)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Click to (re)label DSF curves"),

    # Sidebar with a slider input for number of bins 
    fluidPage(
        column(3,
          selectInput("subset", label = "Plot subset:", choices = c("SYPROcanon", "SYPROmidcanon","canon", "midcanon", "SYPROnoncanon", "SYPROlatenoncanon", "noncanon", "latenoncanon","errata", "drop")),
          numericInput("start_entry", "Start at entry number", 0, min = 1, max = 10000), # never will actually reach 10K
          numericInput("n_entries", "Plot curves in groups of", 250, min = 1, max = 10000), # never will actually reach 10K
          
          h4("Plot interaction designations"),
          h6("SYPROcanon, canon, SYPROmidcanon, midcanon, SYPROnoncanon, noncanon, SYPROlatenoncanon, latenoncanon, errata, drop"),
          textInput("single_click_meaning", "Single-click designation", "noncanon"),
          textInput("double_click_meaning", "Double-click designation", "canon"),
          textInput("brush_meaning", "Click-and-drag designation", "drop"),
          
          h4("Directories for reading and saving"),
          textInput("DSFBASE_DIR", "Read DSFbase from", DSFBASE_DIR),
          
          actionButton("read_data", label = "(re)load data"),
          actionButton("make_plot", label = "(re)plot data subset"),
          actionButton("reset_assignments", label = "reset assignments to original"),
          actionButton("save_results", label = "Save updated results"),
         
          h4("Current labels by ID"),
          tableOutput("subset_table")),

        # Show a plot of the generated distribution
        column(9,
               plotOutput("plot",
                          click = "plot_click",
                          dblclick = "plot_dblclick",
                          brush = "plot_brush",
                          height = "5000px"),
               style = "overflow-y:scroll; max-height: 600px")

        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # will contain the re-assignments
  values <- reactiveValues()

  # read data in response to button click
  observeEvent(input$read_data, {
    values$dsfbase_raw <- read_dsfbase(input$DSFBASE_DIR) # reactive
    values$unreviewed <- values$dsfbase_raw |> filter(reviewed == FALSE) # reactive
    
    # refresh these values each time you make the plot
    values$single_click <- c()
    values$double_click <- c()
    values$brush_click <- c()
  
  })
  
  # allow user to reset assignments if they want to start over
  observeEvent(input$reset_assignments, {
    # refresh these values each time you make the plot
    values$single_click <- c()
    values$double_click <- c()
    values$brush_click <- c()})
  
  # generate plot and associated reassignment table when button is clicked
  observeEvent(input$make_plot, {
    
    # the first subset
    values$unreviewed <-  values$unreviewed |> filter(reviewed == FALSE) # reactive
    
  # reactive
   values$to_review <- take_entries(values$unreviewed, 
                              input$start_entry, 
                              input$n_entries, 
                              .subset = input$subset)
   
   # pre-reassignments
   values$reassigned <- values$to_review # reactive
   
   # reactive
   values$subset_table <- get_subset_table(values$reassigned)
   
   # reactive
   values$reassigned <- reassign_ids(dsfbase = values$reassigned, 
                              
                              # reactive lists of ids from plot clicks
                              single_click = values$single_click, 
                              double_click = values$double_click, 
                              brush_click = values$brush_click,
                              
                              # inputs from the GUI
                              single_click_meaning = input$single_click_meaning,
                              double_click_meaning = input$double_click_meaning, 
                              brush_meaning = input$brush_meaning)
   
   # show the assignment table
   values$subset_table <- get_subset_table(values$reassigned)
   
   # show the plot
   values$raw_plot <- plot_subsets_by_color(values$reassigned, ncol = 10)
   
   ### result of test: we know that we need to either figure out how to make this work with patchwork, 
   # or more easily, just not use patchwork for the shiny-displayed plots
   # values$raw_plot <- values$reassigned |> ggplot(aes(x = Temperature, y = value_norm)) + geom_line() + facet_wrap(~id) + theme_void()
   
  })
  
  # update what has been reviewed
  observeEvent(input$save_results, {
    # reactive
    values$reassigned <- reassign_ids(dsfbase = values$reassigned, 
                                      
                                      # reactive lists of ids from plot clicks
                                      single_click = values$single_click, 
                                      double_click = values$double_click, 
                                      brush_click = values$brush_click,
                                      
                                      # inputs from the GUI
                                      single_click_meaning = input$single_click_meaning,
                                      double_click_meaning = input$double_click_meaning, 
                                      brush_meaning = input$brush_meaning)
    
    # update the unreviewed results
      values$unreviewed <-  values$unreviewed |> 
        mutate(reviewed = case_when(id %in% values$reassigned ~ TRUE, 
                                    .default = reviewed))
      # and save the results
      save_reassignments(values$reassigned, 
                         values$dsfbase_raw,
                         save_full_database_plots_to = .save_full_database_plots_to,
                         save_in_progress_data_to = .save_in_progress_data_to,
                         BACKUP_save_full_database_plots_to = .BACKUP_save_full_database_plots_to,
                         BACKUP_save_in_progress_data_to = .BACKUP_save_in_progress_data_to)
})
  
  ### Render display items
    output$plot <- renderPlot(values$raw_plot)
    output$subset_table <- renderTable(values$subset_table)
    
    
    ## update based on plot interactions
    # click action
    observeEvent(input$plot_click$panelvar1, {
      print("plot click")
      values$single_click <- c(values$single_click, input$plot_click$panelvar1)
      
      # remove from other reassignment lists
      values$double_click <- values$double_click[!values$double_click %in%values$single_click]
      values$brush_click <- values$brush_click[!values$brush_click %in%values$single_click]
      
      })
    
    # double click action
    observeEvent(input$plot_dblclick$panelvar1, {
      print("plot double click")
      values$double_click <- c(values$double_click, input$plot_dblclick$panelvar1)
      
      # remove from other reassignment lists
      values$single_click <- values$single_click[!values$single_click %in%values$double_click]
      values$brush_click <- values$brush_click[!values$brush_click %in%values$double_click]
      })
    
    # drag action
    observeEvent(input$plot_brush$panelvar1, {
      print("plot brush")
      values$brush_click <- c(values$brush_click, input$plot_brush$panelvar1)
      
      # remove from other reassignment lists
      values$single_click <- values$single_click[!values$single_click %in%values$brush_click]
      values$double_click <- values$double_click[!values$double_click %in%values$brush_click]
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

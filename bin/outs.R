# code to generate the output files
library(data.table)
library(kableExtra)
library(flextable)
library(grid)

set_flextable_defaults(
  font.size = 11,
  font.family = "Arial",
  font.color = "black",
  border.color = "gray",
  padding = 0.1,
  line_spacing = 1,
  theme_fun = theme_apa
)


save_compressed_tsv <- function(data, output_file) {
  # Create the output directory if it doesn't exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save the data as a compressed .tsv file
  data.table::fwrite(data, output_file, sep = "\t", compress = "gzip")
}

save_tsv <- function(data, output_file) {
  # Create the output directory if it doesn't exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save the data as a .tsv file
  data.table::fwrite(data, output_file, sep = "\t")
}

save_csv <- function(data, output_file) {
  # Create the output directory if it doesn't exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save the data as a .csv file
  data.table::fwrite(data, output_file, sep = ",")
}

#' Save Data as HTML
#'
#' This function saves a given data frame as an HTML file.
#'
#' @param data A data frame to be saved as HTML.
#' @param output_file A string specifying the path to the output HTML file.
#'
#' @return No return value, called for side effects.
#'
#' @details
#' The function creates the output directory if it does not exist and then
#' saves the data frame as an HTML table using the `htmlTable` package.
#'

save_html_table <- function(data, output_file) {
  # Create the output directory if it doesn't exist
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save the data as an HTML file
  kableExtra::kable(data, format = "html") %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      bootstrap_options = c("condensed")
    ) %>%
    kableExtra::kable_paper() %>%
    kableExtra::save_kable(file = output_file, self_contained = TRUE)
}

# Function to save the plots as .png and .eps files
save_plot <- function(tplot, fn_list, w_in, h_in) {
  # Create the output directory if it doesn't exist
  for (fn in fn_list) {
    folder <- dirname(fn)
    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }
  }

  fn_png <- fn_list$png
  fn_eps <- fn_list$eps
  fn_jpg <- fn_list$jpg

  # save eps plot
  ggplot2::ggsave(
    filename = fn_eps,
    plot = tplot,
    width = w_in,
    height = h_in,
    units = "in",
    dpi = 300
  )
  # save png plot
  ggplot2::ggsave(
    filename = fn_png,
    plot = tplot,
    width = w_in,
    height = h_in,
    units = "in",
    dpi = 300
  )
  # save jpg plot
  ggplot2::ggsave(
    filename = fn_jpg,
    plot = tplot,
    width = w_in,
    height = h_in,
    units = "in",
    dpi = 300
  )
}

#' Save Analysis Plot
#'
#' This function saves a plot to the appropriate analysis output directory based on the source script location.
#'
#' @param plot A ggplot object to save
#' @param script_path The path to the script generating the plot (e.g., __FILE__)
#' @param filename The name of the output file (without extension)
#' @param width Width of the plot in inches
#' @param height Height of the plot in inches
#' @param date_prefix Whether to add today's date as a prefix to the filename
#'
#' @return No return value, called for side effects
#'
#' @details
#' The function:
#' 1. Extracts the analysis directory name from the script path
#' 2. Creates the output directory in plots/analysis_name if it doesn't exist
#' 3. Saves the plot in both .png and .eps formats
#'
#' @examples
#' save_analysis_plot(my_plot, "code/candidate_genes/plot_candidate_semantic_similarity.R", "candidate_semSim_heatmap")
save_analysis_plot <- function(plot, script_path, filename, width, height, date_prefix = TRUE) {
  # Extract analysis directory name from script path
  analysis_dir <- basename(dirname(script_path))
  
  # Create output directory
  output_dir <- file.path("plots", analysis_dir)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Add date prefix if requested
  if (date_prefix) {
    today <- format(Sys.time(), "%Y%m%d")
    filename <- paste(today, filename, sep = "_")
  }
  
  # Create file paths
  fn_list <- list(
    png = file.path(output_dir, paste0(filename, ".png")),
    eps = file.path(output_dir, paste0(filename, ".eps"))
  )
  
  # Save the plot
  save_plot(plot, fn_list, width, height)
}

#' Save Supplementary Table as Word Document
#'
#' Saves a flextable as a Word document in the appropriate supplementary table directory.
#'
#' @param flex_table A flextable object to save
#' @param table_name The name of the table as defined in materials_key.R
#'
#' @return No return value, called for side effects
#'
#' @details
#' This function saves a flextable as a formatted Word document (.docx)
#' in the directory structure defined by materials_key.R using the
#' standardized table naming system.
#'
#' @examples
#' \dontrun{
#' save_supp_table_docx(flex_table = my_table, table_name = "table_S1")
#' }
# save_supp_table_docx <- function(flex_table, fn) {
#   # Save the flextable as a Word document
#   flextable::save_as_docx(
#     flex_table,
#     path = fn
#   )
# }

#' Save Supplementary Table as CSV
#'
#' Saves a data frame as a CSV file in the appropriate supplementary table directory.
#'
#' @param data A data frame to save
#' @param table_name The name of the table as defined in materials_key.R
#'
#' @return No return value, called for side effects
#'
#' @details
#' This function saves a data frame as a CSV file in the directory structure
#' defined by materials_key.R using the standardized table naming system.
#'
#' @examples
#' \dontrun{
#' save_supp_table_csv(data = my_data, table_name = "table_S1")
#' }
save_supp_table_csv <- function(data, table_name) {
  # Get the file paths for this table
  table_paths <- table_fns[[table_name]]

  # Save the data as a CSV file
  data.table::fwrite(data, table_paths$csv)
}

#' Save Supplementary Table as Flextable
#'
#' Saves a flextable object in multiple formats using the standardized naming system.
#'
#' @param flex_table A flextable object to save
#' @param table_name The name of the table key from materials_key.R
#'
#' @return No return value, called for side effects
#'
#' @details
#' This function saves a flextable in multiple formats (docx, html) using the
#' directory structure and naming defined in materials_key.R.
#'
#' @examples
#' \dontrun{
#' save_supp_table_flextable(flex_table = my_table, table_name = "overlap_ft")
#' }
save_supp_table_flextable <- function(flex_table, table_name) {
  # Get the file paths for this table
  table_paths <- table_fns[[table_name]]

  # Save as Word document
  flextable::save_as_docx(flex_table, path = table_paths$docx)

  # Save as HTML with error handling
  tryCatch({
    flextable::save_as_html(flex_table, path = table_paths$html)
  }, error = function(e) {
    warning("Failed to save HTML version of table: ", e$message)
  })
}

#' Save Combined Semantic Similarity Heatmap
#'
#' This function saves a combined heatmap plot as jpg, eps, and png files with proper panel labels.
#' Note that this function is specific to the semantic similarity heatmap plots.
#'
#' @param combined_heatmaps A grid arrangement of heatmaps to save
#' @param fn_list List of filenames with png, eps, and jpg paths
#' @param width Width of the plot in inches
#' @param height Height of the plot in inches
#'
#' @return No return value, called for side effects
#'
#' @examples
#' fn_list <- list(
#'   png = "plots/semantic_similarity/heatmap.png",
#'   eps = "plots/semantic_similarity/heatmap.eps",
#'   jpg = "plots/semantic_similarity/heatmap.jpg"
#' )
#' save_combined_semsim_heatmap(combined_heatmaps, fn_list, 7.5, 10)
save_combined_semsim_heatmap <- function(combined_heatmaps, fn_list, width = 7.5, height = 10) {
  # Create the output directories if they don't exist
  for (fn in fn_list) {
    output_dir <- dirname(fn)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
  }

  # Function to draw the plot with labels
  draw_plot_with_labels <- function() {
    grid.draw(combined_heatmaps)
    # Add panel labels A and B
    grid.text("A",
      x = unit(0.02, "npc"), y = unit(0.95, "npc"),
      just = "left", gp = gpar(fontsize = 14, fontface = "bold")
    )
    grid.text("B",
      x = unit(0.02, "npc"), y = unit(0.45, "npc"),
      just = "left", gp = gpar(fontsize = 14, fontface = "bold")
    )
  }

  # Save as JPG
  jpeg(fn_list$jpg, width = width, height = height, units = "in", res = 300)
  draw_plot_with_labels()
  dev.off()

  # Save as PNG
  png(fn_list$png, width = width, height = height, units = "in", res = 300)
  draw_plot_with_labels()
  dev.off()

  # Save as EPS
  postscript(fn_list$eps, width = width, height = height, horizontal = FALSE, paper = "special")
  draw_plot_with_labels()
  dev.off()
}

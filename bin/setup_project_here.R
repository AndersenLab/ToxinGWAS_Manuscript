# Portable function to find and set the ToxinGWAS project root
# This function can be sourced by any script in the project to set up proper paths
#
# Usage:
#   source("bin/setup_project_here.R")  # If already in project
#   OR source relative to where the script finds the project
#
#   project_root <- find_project_root()
#   if (!is.null(project_root)) {
#     here::set_here(project_root)
#   }

#' Find the ToxinGWAS_Manuscript project root directory
#'
#' Walk up the directory tree from the current file until we find ToxinGWAS_Manuscript.
#' This works portably across different machines and directory structures.
#'
#' @param marker The name of the project directory to find (default: "ToxinGWAS_Manuscript")
#' @return The full path to the project root, or NULL if not found
find_project_root <- function(marker = "ToxinGWAS_Manuscript") {
  # Start from the current input file if available
  start_path <- tryCatch(
    dirname(normalizePath(knitr::current_input())),
    error = function(e) getwd()
  )

  # Walk up the directory tree from the script location
  current <- start_path
  while (current != dirname(current)) { # Stop at filesystem root
    if (basename(current) == marker) {
      return(current)
    }
    current <- dirname(current)
  }

  # If not found in path from script, check if we're already inside the project
  # Look for key project files to confirm we're in the right place
  key_files <- c("_quarto.yml", "bin", "data", "paper")
  current <- start_path
  while (current != dirname(current)) {
    # Check if all key project files exist in current directory
    if (all(file.exists(file.path(current, key_files)))) {
      return(current)
    }
    current <- dirname(current)
  }

  # If still not found, search from working directory for projects structure
  search_paths <- c(
    file.path(getwd(), "projects", marker),
    file.path(dirname(getwd()), "projects", marker),
    file.path(getwd()) # Check if we're already in the right place
  )

  for (path in search_paths) {
    if (dir.exists(path)) {
      # Verify this is actually the right project by checking for key files
      if (all(file.exists(file.path(path, key_files)))) {
        return(normalizePath(path))
      }
    }
  }

  return(NULL)
}

#' Set up the project root using here package (convenience wrapper)
#'
#' Finds the project root and sets it for the here package.
#'
#' @param marker The name of the project directory to find (default: "ToxinGWAS_Manuscript")
#' @return The full path to the project root, or NULL if not found
setup_toxin_project_here <- function(marker = "ToxinGWAS_Manuscript") {
  # Ensure here package is loaded
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("The 'here' package is required. Please install it with: install.packages('here')")
  }

  # Make sure here is attached (not just loaded)
  if (!"package:here" %in% search()) {
    attachNamespace("here")
  }

  project_root <- find_project_root(marker)

  if (!is.null(project_root)) {
    here::set_here(project_root)
    return(project_root)
  } else {
    warning("Could not find ", marker, " project root. Please ensure the project structure is intact.")
    return(NULL)
  }
}

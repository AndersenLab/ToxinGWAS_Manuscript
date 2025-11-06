#!/usr/bin/env Rscript --vanilla

# Set CRAN mirror for non-interactive use
options(repos = c(CRAN = "https://cloud.r-project.org"))

# install required package managment tools
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")

# search directory for dependencies
dir <- getwd()

# Verify we're in the correct Git repository folder
if (basename(dir) != "ToxinGWAS_Manuscript") {
  stop(paste0(
    "Error: Not in the ToxinGWAS_Manuscript repository!\n",
    "Current directory: ", dir, "\n",
    "Expected directory name: ToxinGWAS_Manuscript\n",
    "Please run this script from the project root directory."
  ))
} else {
  message("Verified: In ToxinGWAS_Manuscript repository.")
}

# get all dependencies
pkgs <- renv::dependencies(path = dir, progress = FALSE, errors = "ignore")$Package |> unique()

# Get all available CRAN and Bioconductor packages
cran_avail <- rownames(available.packages())
bioc_avail <- BiocManager::available()

# base/recommended come with R; not on CRAN listing
base_core <- rownames(installed.packages(priority = c("base", "recommended")))

# Classify detected packages
cran_pkgs <- intersect(pkgs, cran_avail) # all on CRAN
bioc_pkgs <- setdiff(intersect(pkgs, bioc_avail), cran_avail) # Bioc-only (not on CRAN)
base_pkgs <- intersect(pkgs, base_core) # all in base R
unknown <- setdiff(pkgs, union(union(cran_avail, bioc_avail), base_core)) # e.g., GitHub/archived

# Install missing CRAN pkgs
missing_cran <- setdiff(cran_pkgs, rownames(installed.packages()))
if (length(missing_cran)) {
  install.packages(missing_cran, quiet = TRUE, upgrade = "never")
}

# Install missing Bioc pkgs
missing_bioc <- setdiff(bioc_pkgs, rownames(installed.packages()))
if (length(missing_bioc)) {
  BiocManager::install(missing_bioc, ask = FALSE, update = FALSE)
}

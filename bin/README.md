# Project Setup Utilities

## setup_project_here.R

This file contains portable functions for finding and setting the ToxinGWAS_Manuscript project root directory, regardless of where the project is cloned or how files are included.

### Functions

#### `find_project_root(marker = "ToxinGWAS_Manuscript")`

Finds the project root directory by walking up the directory tree from the current file location.

**Returns:** The full path to the project root, or `NULL` if not found.

**Usage:**
```r
source("bin/setup_project_here.R")
project_root <- find_project_root()
```

#### `setup_toxin_project_here(marker = "ToxinGWAS_Manuscript")`

Convenience wrapper that finds the project root and automatically sets it using the `here` package.

**Returns:** The full path to the project root, or `NULL` if not found.

**Usage:**
```r
library(here)
source("bin/setup_project_here.R")
project_root <- setup_toxin_project_here()

# Now you can use here() to reference project files
data <- readRDS(here("data", "processed", "file.rds"))
```

### How It Works

The function uses a simple algorithm:

1. **Primary method:** Gets the current file path from `knitr::current_input()` (works when included in Quarto documents) and walks up the directory tree until it finds a directory named `ToxinGWAS_Manuscript`

2. **Fallback method:** If the primary method fails, it searches in common relative locations from the current working directory

This approach is:
- **Portable:** Works regardless of where the project is cloned
- **Flexible:** Works whether you're running scripts directly or including them in other documents
- **Simple:** Just source the file and call the function

### Example Usage in Quarto Documents

When including scripts in your dissertation that need to access resources in the ToxinGWAS project:

```r
library(here)

# Source the setup function relative to the current script
script_dir <- tryCatch(
  dirname(normalizePath(knitr::current_input())),
  error = function(e) getwd()
)

setup_file <- file.path(script_dir, "..", "bin", "setup_project_here.R")
source(setup_file)

# Set up the project root
project_root <- setup_toxin_project_here()

if (is.null(project_root)) {
  stop("Could not locate project root")
}

# Use file.path() with project_root for file references
# Note: When files are included in Quarto, here() may not work correctly,
# so use direct file paths with project_root instead
source(file.path(project_root, "bin", "helper_functions.R"))
data <- readRDS(file.path(project_root, "data", "processed", "data.rds"))
```

**Important Note for Quarto Includes:**
When files are included using `{{< include >}}` in Quarto documents, the `here::set_here()` function may not persist correctly across the included file boundary. Therefore, it's recommended to use `file.path(project_root, ...)` for all file references instead of `here()`.

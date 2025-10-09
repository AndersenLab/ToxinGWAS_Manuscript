# code to save to supplementary tables folder in Google Drive
library(googledrive)
library(googlesheets4)

library(googledrive)

save_to_google_sheets <- function(data, spread_name, sheet_name) {
    parent_folder <- googledrive::drive_get("Toxin GWAS")
    
    tables_folder <- googledrive::drive_ls(parent_folder, pattern = "Tables")
    
    # Check if a spreadsheet with the same name already exists
    existing_ss <- googledrive::drive_ls(tables_folder, pattern = spread_name)
    
    if (nrow(existing_ss) > 0) {
        print(
            glue::glue(
                "A spreadsheet with the name '{spread_name}' already exists in the 'Tables' folder."
            )
        )
        # Overwrite the existing spreadsheet
        ss <- googlesheets4::as_sheets_id(existing_ss$id)
        googlesheets4::sheet_write(data, ss, sheet = sheet_name)
    } else {
        # Create a new spreadsheet with the specified sheet name
        ss <- googlesheets4::gs4_create(
          spread_name,
          sheets = rlang::set_names(list(data), sheet_name))
        
        # Move the spreadsheet to the nested folder
        drive_mv(file = as_id(ss), path = as_id(tables_folder))
    }
}



# Example usage
# save_to_google_sheets(h2_summary_clean, "table_S3_test", "Sheet1")

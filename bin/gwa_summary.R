#==============================================================================#
# GWA SUMMARY FUNCTIONS
#==============================================================================#
combine.mappings <- function(x){
  
  alg.index <- length(strsplit(x = strsplit(x = x,split = "[.]")[[1]][1], split = "_")[[1]])
  kinship.method <- stringr::str_to_title(strsplit(x = strsplit(x = x,split = "[.]")[[1]][1], split = "_")[[1]][alg.index])
  
  data.table::fread(file = x) %>%
    dplyr::filter(aboveBF == 1,
                  !is.na(peak_id)) %>%
    dplyr::mutate(algorithm = kinship.method)
}

combine.mappings.all.markers <- function(x){
  
  alg.index <- length(strsplit(x = strsplit(x = x,split = "[.]")[[1]][1], split = "_")[[1]])
  kinship.method <- stringr::str_to_title(strsplit(x = strsplit(x = x,split = "[.]")[[1]][1], split = "_")[[1]][alg.index])
  
  data.table::fread(file = x) %>%
    dplyr::filter(aboveBF == 1)%>%
    dplyr::mutate(algorithm = kinship.method)
}
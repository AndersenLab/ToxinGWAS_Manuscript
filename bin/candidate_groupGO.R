# functions to get the GO terms for the candidate genes
# Load the required libraries
library(clusterProfiler)
library(org.Ce.eg.db)
#' Prepare candidate gene clusters for semantic similarity analysis
#' @param candidate_df A dataframe with candidate gene data
#' @return A list of gene clusters by trait
prepare_trait_clusters <- function(candidate_df) {
    trait.clusters <- NULL
    
    # Get unique traits
    traits <- unique(candidate_df$trait)
    for (trait_id in traits) {
        # Get genes for this trait
        genes <- candidate_df %>%
            dplyr::filter(trait == trait_id) %>%
            dplyr::pull(WBGeneID) %>%
            unique()
        
        # Convert WBGeneID to Entrez IDs
        genes.entrez <- clusterProfiler::bitr(
          geneID = genes,
          fromType = "ENSEMBL",
          toType = "ENTREZID",
          OrgDb = "org.Ce.eg.db"
        ) %>%
          dplyr::pull(ENTREZID)
        # Add to list
        trait.clusters[[trait_id]] <- genes.entrez
    }
    
    return(trait.clusters)
}

# Function to perform groupGO analysis
#' Perform groupGO analysis
#'
#' @param candidates A vector of candidate gene IDs
#' @param keyType The key type for the gene IDs (e.g., "ENSEMBL", "SYMBOL")
#' @param ont The ontology type (e.g., "BP", "MF", "CC")
#' @param level The level of GO terms
#' @return A list with the GO term results and the proportion of candidate genes with GO terms
perform_groupGO <- function(candidates, keyType, ont, level) {
  candidate_goterms <- groupGO(
    gene = candidates,
    OrgDb = org.Ce.eg.db,
    keyType = keyType,
    ont = ont,
    level = level,
    readable = TRUE
  )

  # Pull the results into a dataframe
  candidate_goterms_df <- candidate_goterms@result

  # Filter to counts > 0
  candidate_goterms_df <- candidate_goterms_df %>%
    dplyr::filter(Count > 0)

  # Calculate the proportion of candidate genes with GO terms
  proportion_with_go <- n_distinct(candidate_goterms_df$geneID) / length(candidates)

  # Return the results
  list(
    keyType = keyType,
    ont = ont,
    level = level,
    proportion_with_go = proportion_with_go,
    candidate_goterms_df = candidate_goterms_df,
    gene_key = candidate_goterms@gene2Symbol
  )
}

# Function to create a long dataframe with GO terms attached to candidate genes
#' Create a long dataframe with GO terms attached to candidate genes
#'
#' @param candidate_goterms_df A dataframe with GO term results
#' @param gene_key A named vector to translate between gene IDs and gene names
#' @return A long dataframe with GO terms attached to candidate genes
create_candidate_goterms_long <- function(candidate_goterms_df, gene_key) {
  candidate_goterms_long <- candidate_goterms_df %>%
    tidyr::separate_rows(geneID, sep = "/") %>%
    dplyr::mutate(WBGeneID = names(gene_key)[match(geneID, gene_key)]) %>%
    dplyr::select(WBGeneID, everything()) %>%
    tibble::column_to_rownames(var = "WBGeneID")

  return(candidate_goterms_long)
}

# Function to join the long candidate gene data to the original candidate gene input dataframe
#' Join the long candidate gene data to the original candidate gene input dataframe
#'
#' @param candidate_goterms_long A long dataframe with GO terms attached to candidate genes
#' @param candidate_raw The original candidate gene input dataframe
#' @return A dataframe with the joined data
join_candidate_goterms <- function(candidate_goterms_long, candidate_raw) {
  candidate_raw_filtered <- candidate_raw %>%
    dplyr::select(trait, interval, WBGeneID)

  joined_df <- candidate_raw_filtered %>%
    dplyr::left_join(candidate_goterms_long, by = "WBGeneID")

  return(joined_df)
}

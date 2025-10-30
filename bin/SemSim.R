# Common Functions to analyze semantic similarity of genes within QTLs

library(tidyverse)
library(clusterProfiler) ## BiocManager::install("clusterProfiler")
library(org.Ce.eg.db) ## BiocManager::install("org.Ce.eg.db")
library(biomaRt) ## BiocManager::install("biomaRt")
library(enrichplot) ## BiocManager::install("enrichplot"
library(GOSemSim) ## BiocManager::install("GOSemSim") v2.24.0

#### Functions ####




# # load the test data
# test_df <- data.table::fread(
#   "code/semantic_similarity/test_data/inbred_test_data.tsv"
# )

# # get genes for 2_4_D in test_data

# test_genes <- get_genes_all_intervals(
#   qtl = test_df,
#   trait_id = "length_2_4_D",
#   gff_df = gff,
#   cEGO = cEGO
# )

# test_genes2 <- get_genes_all_intervals(
#   qtl = test_df,
#   trait_id = "length_Methyl_mercury",
#   gff_df = gff,
#   cEGO = cEGO
# )

# Function to calculate semantic similarity between
#' @param a_genes list: each element is the peakmarker of an interval
#' and the value is a character vector of ENTREZID gene ids
#' if the list contains more than one element, the genes are combined
#' @param b_genes list: each element is the peakmarker of an interval
#' and the value is a character vector of ENTREZID gene ids
#' if the list contains more than one element, the genes are combined
#' a_genes and b_genes are input into GOSemSim::clusterSim
#' @return numeric: semantic similarity between the two sets of genes
traitpair_semsim <- function(a_genes, b_genes, cEGO) {
  # Combine the genes for each trait
  a_genes <- unlist(a_genes)
  b_genes <- unlist(b_genes)

  sem <- GOSemSim::clusterSim(
    a_genes,
    b_genes,
    semData = cEGO,
    measure = "Wang",
    combine = "BMA"
  )

  return(sem)
}

# test
# traitpair_semsim(test_genes, test_genes2, cEGO)


# # Function to calculate semantic similarity between all
# #' pairs of traits in the qtl data
# #' @param qtl data.frame: qtl data loaded from `QTL_peaks_{inbred/loco }.tsv
# #' and preprocessed`
# #' @param gff_df data.frame: gff data
# #' @param cEGO GOSemSim object: GO term annotation data
# #' @param exclude_intervals character vector: list of intervals
# #' (listed by peak_marker) to exclude default is NULL`
# #' @return data.frame: data.frame with columns trait_a, trait_b, similarity
# #' similarity is the semantic similarity between the two traits
# #' calculated using GOSemSim::clusterSim
# #' with the "Wang" measure and "BMA" combine
# #'
# trait_semsim <- function(
#     qtl,
#     gff_df,
#     cEGO,
#     exclude_intervals = NULL) {
#   # Get unique traits
#   traits <- unique(qtl$trait)

#   # Initialize result data frame
#   result <- data.frame(
#     trait_a = character(),
#     trait_b = character(),
#     similarity = numeric()
#   )

#   # Iterate over all pairs of traits
#   for (i in 1:length(traits)) {
#     for (j in 1:length(traits)) {
#       # Skip if the same trait
#       if (i == j) {
#         next
#       }

#       trait_a <- traits[i]
#       trait_b <- traits[j]

#       cat("Processing traits:", trait_a, "and", trait_b, "\n")

#       cat("Getting genes for trait:", trait_a, "\n")
#       # Get genes for trait A
#       genes_a <- get_genes_all_intervals(
#         qtl = qtl,
#         trait_id = trait_a,
#         gff_df = gff_df,
#         cEGO = cEGO,
#         exclude_intervals = exclude_intervals
#       )

#       cat("Getting genes for trait:", trait_b, "\n")
#       # Get genes for trait B
#       genes_b <- get_genes_all_intervals(
#         qtl = qtl,
#         trait_id = trait_b,
#         gff_df = gff_df,
#         cEGO = cEGO,
#         exclude_intervals = exclude_intervals
#       )

#       # Calculate semantic similarity
#       similarity <- traitpair_semsim(genes_a, genes_b, cEGO)

#       cat("Similarity between", trait_a, "and", trait_b, ":", similarity, "\n")

#       # Add to result data frame
#       result <- dplyr::bind_rows(
#         result,
#         data.frame(
#           trait_a = trait_a,
#           trait_b = trait_b,
#           similarity = similarity
#         )
#       )

#       # print progress
#       cat("Processed", i, "of", length(traits), "traits\n")
#       print(result)
#     }
#   }
#   return(result)
# }

# # test
# test_all_traits_semsim <- trait_semsim(test_df, gff, cEGO)


# Function to calculate semantic similarity between all
# pairs of traits in the qtl data
#' @param genes_list named list: each element is the trait id and the value is a character vector of ENTREZID gene ids
#' @param cEGO GOSemSim object: GO term annotation data
#' @return data.frame: data.frame with columns trait_a, trait_b, similarity
#' similarity is the semantic similarity between the two traits
#' calculated using GOSemSim::clusterSim
#' with the "Wang" measure and "BMA" combine
trait_semsim <- function(genes_list, cEGO) {
  # Get unique traits
  traits <- names(genes_list)

  # Initialize result data frame
  result <- data.frame(
    trait_a = character(),
    trait_b = character(),
    similarity = numeric()
  )

  # Iterate over all pairs of traits
  for (i in 1:length(traits)) {
    for (j in 1:length(traits)) {
      # Skip if the same trait
      if (i == j) {
        next
      }

      trait_a <- traits[i]
      trait_b <- traits[j]

      cat("Processing traits:", trait_a, "and", trait_b, "\n")

      # Get genes for trait A
      genes_a <- genes_list[[trait_a]]

      # Get genes for trait B
      genes_b <- genes_list[[trait_b]]

      # Calculate semantic similarity
      similarity <- traitpair_semsim(list(genes_a), list(genes_b), cEGO)

      cat("Similarity between", trait_a, "and", trait_b, ":", similarity, "\n")

      # Add to result data frame
      result <- dplyr::bind_rows(
        result,
        data.frame(
          trait_a = trait_a,
          trait_b = trait_b,
          similarity = similarity
        )
      )

      # print progress
      cat("Processed", i, "of", length(traits), "traits\n")
      print(result)
    }
  }
  return(result)
}

# Example usage
# # Create a named list of genes for all traits
# genes_list <- list(
#   "length_2_4_D" = get_genes_all_intervals(
#     test_df,
#     "length_2_4_D",
#     gff,
#     cEGO
#   ),
#   "length_Methyl_mercury" = get_genes_all_intervals(
#     test_df,
#     "length_Methyl_mercury",
#     gff,
#     cEGO
#   ),
#   "length_Chlorothalonil" = get_genes_all_intervals(
#     test_df,
#     "length_Chlorothalonil",
#     gff,
#     cEGO
#   ),
#   "length_Arsenic_trioxide" = get_genes_all_intervals(
#     test_df,
#     "length_Arsenic_trioxide",
#     gff,
#     cEGO
#   )
# )

# Calculate semantic similarity between all pairs of traits
# test_all_traits_semsim <- trait_semsim(genes_list, cEGO)
# Example usage
# Create a named list of genes for all traits
# genes_list <- list(
#   "length_2_4_D" = get_genes_all_intervals(
#     test_df,
#     "length_2_4_D",
#     gff,
#     cEGO
#   ),
#   "length_Methyl_mercury" = get_genes_all_intervals(
#     test_df,
#     "length_Methyl_mercury",
#     gff,
#     cEGO
#   ),
#   "length_Chlorothalonil" = get_genes_all_intervals(
#     test_df,
#     "length_Chlorothalonil",
#     gff,
#     cEGO
#   ),
#   "length_Arsenic_trioxide" = get_genes_all_intervals(
#     test_df,
#     "length_Arsenic_trioxide",
#     gff,
#     cEGO
#   )
# )

# # Calculate semantic similarity between all pairs of traits
# test_all_traits_semsim <- trait_semsim(genes_list, cEGO)

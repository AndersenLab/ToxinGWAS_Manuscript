library(tidyverse)
library(clusterProfiler) ## BiocManager::install("clusterProfiler")
library(org.Ce.eg.db) ## BiocManager::install("org.Ce.eg.db")
library(biomaRt) ## BiocManager::install("biomaRt")
library(enrichplot) ## BiocManager::install("enrichplot"
library(GOSemSim) ## BiocManager::install("GOSemSim") v2.24.0

#### Load the gene data ####
gff_file <- "data/raw/c_elegans.PRJNA13758.WS283.csq.gff3.gz"

# read in gff
gff <- data.table::fread(gff_file) %>%
  dplyr::rename(
    seqname = V1,
    source = V2,
    feature = V3,
    start = V4,
    stop = V5,
    score = V6,
    strand = V7,
    frame = V8,
    attribute = V9
  ) %>%
  # filter to only genes
  dplyr::filter(feature == "gene")

# load the Goterm annotation data
cEGO <- GOSemSim::godata("org.Ce.eg.db", ont = "BP")


#### Functions ####
#' filters gff to genes within the interval
#' and converts the WBGeneID to ENTREZID
#' @param gff_df data.frame: gff data
#' @param peak_marker character: peak marker
#' @param chromosome_id character: chromosome id
#' @param start_pos numeric: start position
#' @param stop_pos numeric: stop position
#' @param cEGO GOSemSim object: GO term annotation data
#' @return list: named list with peak marker as name and converted gene IDS
#' in a character vector as the value
get_genes_interval <- function(
    gff_df,
    peak_marker,
    chromosome_id,
    start_pos,
    stop_pos,
    cEGO) {
  # get the genes in the interval from gff
  test_int_genes <- gff_df %>%
    dplyr::filter(
      seqname == chromosome_id &
        start >= start_pos &
        stop <= stop_pos &
        feature == "gene"
    ) %>%
    dplyr::mutate(
      WBGeneID = stringr::str_extract(attribute, pattern = regex("(?<=ID=gene:)(.*)(?=;Name=)"))
    )

  # Convert the WBGeneID to ENTREZID
  converted_genes <- clusterProfiler::bitr(
    geneID = test_int_genes$WBGeneID,
    fromType = "ENSEMBL",
    toType = "ENTREZID",
    OrgDb = "org.Ce.eg.db"
  ) %>%
    dplyr::pull(ENTREZID)

  # # Create a named list with the peak_marker as the name and the converted gene IDs as the value
  # result <- list()
  # result[[peak_marker]] <- converted_genes

  return(converted_genes)
}
# test
# test_g1 <- get_genes_interval(
#   gff_df = gff,
#   peak_marker = "I:2994073",
#   chromosome_id = "I",
#   start_pos = 2643819,
#   stop_pos = 3157610,
#   cEGO = cEGO
# )


#' Function to create a list of genes in all intervals
#' detected for a trait
#' @param qtl data.frame: qtl data loaded from `QTL_peaks_{inbred/loco }.tsv
#' and preprocessed`
#' @param trait_id character: trait id in the 'trait' column of the qtl data
#' @param gff_df data.frame: gff data
#' @param cEGO GOSemSim object: GO term annotation data
#' @param exclude_intervals character vector: list of intervals
#' (listed by peak_marker) to exclude default is NULL`
#' @return list: named list where each element is named by the peak marker ID
#' and contains the genes found in that interval
get_genes_each_interval <- function(
    qtl,
    trait_id,
    gff_df,
    cEGO,
    exclude_intervals = NULL) {
  # Filter QTL data for the specified trait
  qtl_trait <- qtl %>%
    dplyr::filter(trait == trait_id)

  # Initialize result list
  all_genes_list <- list()

  # Iterate over each row in the filtered QTL data
  for (i in 1:nrow(qtl_trait)) {
    peak_marker <- qtl_trait$marker[i]

    # Skip excluded intervals
    if (!is.null(exclude_intervals) && peak_marker %in% exclude_intervals) {
      next
    }

    chromosome_id <- qtl_trait$CHROM[i]
    start_pos <- qtl_trait$startPOS[i]
    stop_pos <- qtl_trait$endPOS[i]

    # Get genes for the interval
    genes_interval <- get_genes_interval(
      gff_df = gff_df,
      peak_marker = peak_marker,
      chromosome_id = chromosome_id,
      start_pos = start_pos,
      stop_pos = stop_pos,
      cEGO = cEGO
    )

    # Add genes to result list with peak marker as name
    all_genes_list[[peak_marker]] <- genes_interval
  }

  return(all_genes_list)
}

#' Function to create a list of genes in all intervals
#' detected for a trait
#' @param qtl data.frame: qtl data loaded from `QTL_peaks_{inbred/loco }.tsv
#' and preprocessed`
#' @param trait_id character: trait id in the 'trait' column of the qtl data
#' @param gff_df data.frame: gff data
#' @param cEGO GOSemSim object: GO term annotation data
#' @param exclude_intervals character vector: list of intervals
#' (listed by peak_marker) to exclude default is NULL`
#' @return list: named list with peak marker as name and converted gene IDS
#' for all intervals in the trait
get_genes_all_intervals <- function(
    qtl,
    trait_id,
    gff_df,
    cEGO,
    exclude_intervals = NULL) {
  # Filter QTL data for the specified trait
  qtl_trait <- qtl %>%
    dplyr::filter(trait == trait_id)

  # Initialize result vector
  all_genes <- c()

  # Iterate over each row in the filtered QTL data
  for (i in 1:nrow(qtl_trait)) {
    peak_marker <- qtl_trait$marker[i]

    # Skip excluded intervals
    if (!is.null(exclude_intervals) && peak_marker %in% exclude_intervals) {
      next
    }

    chromosome_id <- qtl_trait$CHROM[i]
    start_pos <- qtl_trait$startPOS[i]
    stop_pos <- qtl_trait$endPOS[i]

    # Get genes for the interval
    genes_interval <- get_genes_interval(
      gff_df = gff_df,
      peak_marker = peak_marker,
      chromosome_id = chromosome_id,
      start_pos = start_pos,
      stop_pos = stop_pos,
      cEGO = cEGO
    )

    # Add genes to result vector
    all_genes <- c(all_genes, genes_interval)
  }

  # Remove any duplicate genes
  all_genes <- unique(all_genes)

  return(all_genes)
}



#' Convert a list of genes from get_genes_each_interval into a dataframe
#' @param genes_list list: output from get_genes_each_interval where each element
#' is named by the peak marker and contains a character vector of genes
#' @param trait_id character: the trait identifier to be added as a column
#' @return data.frame: dataframe with columns peak_marker, gene, and trait
genes_list_to_df <- function(genes_list, trait_id) {
  # Create a dataframe from the list
  df <- do.call(rbind, lapply(names(genes_list), function(peak_marker) {
    data.frame(
      peak_marker = peak_marker,
      entrez_id = genes_list[[peak_marker]],
      trait = trait_id
    )
  }))

  return(df)
}

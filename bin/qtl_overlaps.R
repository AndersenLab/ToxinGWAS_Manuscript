library(tidyverse)
library(GenomicRanges)

#### Functions #### -----------------------------------------------------------

# Function to convert all QTL for a trait to GRanges
# test_traitA <- trait_granges("length_Methyl_mercury", qtl_peaks_df)
trait_granges <- function(trait_id, qtl_peaks) {
    # subset the qtl peaks data to just the trait of interest
    trait_peaks <- qtl_peaks %>%
        dplyr::filter(trait == trait_id)

    # convert the qtl peaks data to a gRanges object
    qtl_granges <- GenomicRanges::GRanges(
        seqnames = trait_peaks$CHROM,
        ranges = IRanges::IRanges(
            start = trait_peaks$startPOS,
            end = trait_peaks$endPOS
        ),
        strand = "*",
        peakid = trait_peaks$marker,
        trait = trait_peaks$trait,
        group = trait_peaks$group
    )
    # Add the number of QTL for the trait to the qtl_granges object
    qtl_granges$nQTL <- length(qtl_granges)

    return(qtl_granges)
}

# create a function that calculates the JI index for a pair of traits
# test_pair <- trait_pair_overlaps(test_traitA, test_traitB)
trait_pair_overlaps <- function(trait_A_gr, trait_B_gr) {
    pair_overlaps <- suppressWarnings(GenomicRanges::findOverlaps(trait_A_gr, trait_B_gr))
    n_overlaps <- length(pair_overlaps)
    print(glue::glue("Number of overlaps: {n_overlaps}"))
    if (n_overlaps == 0) {
        pair_overlaps_df <- data.frame(
            traitA = unique(trait_A_gr$trait),
            traitB = unique(trait_B_gr$trait),
            peakidA = NA,
            peakidB = NA,
            nQTLA = unique(trait_A_gr$nQTL),
            nQTLB = unique(trait_B_gr$nQTL),
            groupA = unique(trait_A_gr$group),
            groupB = unique(trait_B_gr$group),
            n_overlaps = length(pair_overlaps)
        )
        # calculate the JI index
        pair_overlaps_df <- pair_overlaps_df %>%
            mutate(JI = n_overlaps / (nQTLA + nQTLB))
    } else {
        pair_overlaps_df <- data.frame(
            traitA = trait_A_gr$trait[queryHits(pair_overlaps)],
            traitB = trait_B_gr$trait[subjectHits(pair_overlaps)],
            peakidA = trait_A_gr$peakid[queryHits(pair_overlaps)],
            peakidB = trait_B_gr$peakid[subjectHits(pair_overlaps)],
            nQTLA = trait_A_gr$nQTL[queryHits(pair_overlaps)],
            nQTLB = trait_B_gr$nQTL[subjectHits(pair_overlaps)],
            groupA = trait_A_gr$group[queryHits(pair_overlaps)],
            groupB = trait_B_gr$group[subjectHits(pair_overlaps)],
            n_overlaps = length(pair_overlaps)
        )
        # calculate the JI index
        pair_overlaps_df <- pair_overlaps_df %>%
            mutate(JI = n_overlaps / (nQTLA + nQTLB - n_overlaps))
    }
    return(pair_overlaps_df)
}

# Function that does a trait pair analysis on the QTL peaks data
# real_overlaps <- pairwise_qtl_overlaps(qtl_fil)

pairwise_qtl_overlaps <- function(qtl_df) {
    # function to generate all possible pairs of traits
    # trait_pairs <- generate_pairs(unique(qtl_fil$trait))
    generate_pairs <- function(input_list) {
        pairs <- combn(input_list, 2, simplify = FALSE)
        return(pairs)
    }
    # make sure the qtl_df is a data frame
    # qtl_df <- as.data.frame(qtl_df)

    print("Generating all possible trait pairs")

    trait_pairs <- generate_pairs(unique(qtl_df$trait))

    print("Generated all possible trait pairs")

    all_trait_pairs_list <- list()

    print("Starting to iterate over the pairs")
    # Iterate over the pairs
    for (i in seq_along(trait_pairs)) {
        print("starting new pair")
        # Get the pair
        pair <- trait_pairs[[i]]
        print(pair)

        # Trait A Granges
        print(
            glue::glue("Creating gRanges for trait {pair[1]}")
        )
        trait_A_gr <- trait_granges(pair[1], qtl_df)

        print(
            glue::glue("Creating gRanges for trait {pair[2]}")
        )

        # Trait B Granges
        trait_B_gr <- trait_granges(pair[2], qtl_df)
        print("created both trait gRanges")

        print(
            glue::glue("Calculating overlaps for {pair[1]} and {pair[2]}")
        )

        # Apply the function to the pair
        pair_overlaps_df <- trait_pair_overlaps(trait_A_gr, trait_B_gr)
        print("calculated overlaps")
        # Append the dataframe to the list
        all_trait_pairs_list[[i]] <- pair_overlaps_df
    }
    all_pairs_df <- do.call(rbind, all_trait_pairs_list)

    return(all_pairs_df)
}

# create a function that randomly shuffels QTL across the traits.
# Function that take a qtl peaks data frame, each row in the data is a qtl. for each row, randomly assign one of the unqiue values from the trait column
# return the shuffled data frame
# shuffel_test <- shuffle_qtl(qtl_fil)
shuffle_qtl <- function(qtl_df) {
    # set.seed(123)

    # Get the trait grouping key from the qtl data frame
    trait_grouping_key <- qtl_df %>%
        dplyr::select(trait, group) %>%
        dplyr::distinct()

    # get the unique traits
    unique_traits <- unique(qtl_df$trait)
    n_qtl <- nrow(qtl_df)
    print(glue::glue("{n_qtl} QTL in the data frame"))
    print(glue::glue("{length(unique_traits)} unique traits in the data frame"))

    # shuffle the traits
    shuffled_traits <- sample(unique_traits, nrow(qtl_df), replace = TRUE)

    # add the shuffled traits to the data frame
    qtl_df$shuffeled <- shuffled_traits

    # get rid of the shuffeled column
    shuffeled_df <- qtl_df %>%
        dplyr::select(-trait, -group) %>%
        dplyr::rename(trait = shuffeled) %>%
        dplyr::left_join(trait_grouping_key, by = "trait")

    return(shuffeled_df)
}

# function that will measure the within group JI and the between group JI for any overlap pairs data frame
# calculate_JI(real_overlaps)
calculate_JI <- function(overlap_pairs_df) {
    # calculate the average within group JI
    within_group_JI <- overlap_pairs_df %>%
        dplyr::filter(groupA == groupB) %>%
        dplyr::summarise(mean_JI = mean(JI)) %>%
        dplyr::rename(within = mean_JI) %>%
        dplyr::pull()

    # calculate the average between group JI
    between_group_JI <- overlap_pairs_df %>%
        dplyr::filter(groupA != groupB) %>%
        dplyr::summarise(mean_JI = mean(JI)) %>%
        dplyr::rename(between = mean_JI) %>%
        dplyr::pull()

    # calculate the absolute difference between the within group JI and the between group JI
    JI_diff <- abs(within_group_JI - between_group_JI)

    return(
        data.frame(
            within = within_group_JI,
            between = between_group_JI,
            abs_diff = JI_diff
        )
    )
}

# create a function that shuffels the qtl data frame and calculates the JI index for each pair of traits
# # run the function 1000 times
# JI_results <- replicate(10, shuffle_and_JI(qtl_fil), simplify = TRUE)
# JI_results_df <- do.call(rbind, JI_results)
shuffle_and_JI <- function(qtl_df) {
    # shuffle the qtl data frame
    shuffel_test <- shuffle_qtl(qtl_df)

    # calculate the JI index for each pair of traits
    shuffel_test_pairs <- pairwise_qtl_overlaps(shuffel_test)

    # calculate the average within group JI and the average between group JI
    JI_results <- calculate_JI(shuffel_test_pairs)

    return(JI_results)
}

library(foreach)
library(doParallel)

#' function that runs the shuffle_and_JI function in parallel
#' @param qtl_df A data frame of QTL intervals detected that contains a
#' trait column and a group column

run_permutation <- function(qtl_df) {
    # Register parallel backend
    num_cores <- detectCores()
    print(
        glue::glue("There are {num_cores} cores available")
    )
    registerDoParallel(cores = num_cores)

    # Run function in parallel and handle errors
    results <- foreach(i = 1:1000, .combine = rbind) %dopar% {
        tryCatch(
            {
                shuffle_and_JI(qtl_df)
            },
            error = function(e) {
                data.frame(
                    within = NA,
                    between = paste("Error in iteration", i, ":", e$message),
                    abs_diff = NA
                )
            }
        )
    }

    # Stop parallel backend
    stopImplicitCluster()

    return(results)
}


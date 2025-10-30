require(tidyverse)
require(easyXpress)
require(RColorBrewer)
require(cowplot)
require(magrittr)
require(ggbeeswarm)
require(ggrepel)
require(grid)
require(gridExtra)
require(sommer)
require(data.table)
require(ggh4x)
require(valr)

# set working directory
#setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/.."))

# get the date
today <- format(Sys.Date(), "%Y%m%d")

# set a universal drug class pal

#FUNCTION to identify outlier wells
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, 
                         ...)
  H <- 1.5 * stats::IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}



library(sommer)

#==============================================================================#
# Heritability functions                                                       #
#==============================================================================#
# semi-parametric bootstrap for data with strain and value vars.
# reps is passed to nsim parameter in boot::bootMer function
# ci.level is passed to stats::confint function as level
H2.semi.parametric <- function(data, reps = 1000, ci.level = 0.95) {
  
  message(glue::glue("Performing semi-parametric bootstrap to calculate heritability {ci.level} CIs (nsim = {reps})"))
  
  # make a function to extract the heritability from the model m, when the model is
  ex <- function(m) {
    # get the variance correlation componets 
    h <- as.data.frame(lme4::VarCorr(m))
    
    # return the broad-sense heritability
    return(h$vcov[1]/(h$vcov[1]+h$vcov[2]))
  }
  
  # setup the model 
  m <- lme4::lmer(value ~ 1|strain, data = data)
  
  # get the point estimate
  H2.point <- ex(m)
  
  # Perform model-based (semi-)parametric bootstrap for mixed models. 
  sp <- lme4::bootMer(x = m, FUN = ex, nsim = reps)
  
  # get CIs using stats::confint
  H2.CI.vec <- stats::confint(sp, level = ci.level)
  
  # get the boot vector
  H2.boots.vec <- sp$t[,1]
  
  # wrap it up
  H2.CI <- tibble::tibble(H2.point = H2.point, 
                          H2.CI.lower = H2.CI.vec[[1]],
                          H2.CI.upper = H2.CI.vec[[2]],
                          H2.CI.level = ci.level)
  
  # return it
  return(list(H2.CI,H2.boots.vec))
  
}

# d = a data frame with strain (RILs) and replicate values for a trait. A strain mean will be calculated and used as the response variable.
# geno_matrix = a matrix with strains as column names and long markers - output from NemaScan geomatrix profile
h2 <- function(d, geno_matrix){
  # narrow-sense Heritability
  pheno_strains <- unique(d$strain)
  A <- sommer::A.mat(t(geno_matrix %>% dplyr::select(dplyr::one_of(pheno_strains))))
  
  df_y <- d %>%
    dplyr::arrange(strain) %>%
    dplyr::select(strain, value) %>%
    dplyr::mutate(strain = as.character(strain)) %>%
    dplyr::group_by(strain) %>%
    dplyr::mutate(mean = mean(value)) %>%
    dplyr::distinct(strain, mean) %>%
    dplyr::ungroup()
  
  h2_res <- sommer::mmer(mean~1,
                         random=~vsr(strain,Gu=A),
                         data=df_y)
  
  h2.point <- vpredict(h2_res, h2 ~ (V1) / ( V1+V2))[[1]][1]
  h2.SE <- vpredict(h2_res, h2 ~ (V1) / ( V1+V2))[[2]][1]
  
  h2.df <- data.frame(h2.point) %>%
    dplyr::mutate(h2.upper = h2.point + h2.SE,
                  h2.lower = h2.point - h2.SE)
  
  return(h2.df)
}

#==============================================================================#
# Gene ontogeny enrichment tests                                               #
#==============================================================================#
#library(clusterProfiler) ## BiocManager::install("clusterProfiler")
#library(org.Ce.eg.db) ##BiocManager::install("org.Ce.eg.db")
#library(biomaRt) ##BiocManager::install("biomaRt")
#library(enrichplot)

# qtl A qtl is a dataframe with QTL peaks output from NemaScan with big_class variable to calssify toxins
# gff gff is a dataframe with typical gff column names. In this case we want c_elegans.PRJNA13758.WS283.csq.gff3
# To see the names of the groups used for enrichment analysis use names(<your output>[[1]]).
# To see results use <your output>[[1]]$<trait>[[2]]@result, which is the output of the 
# clusterProfiler::enrichGO function. Note, this object only exists if significant results are found.
enrichGO_trait <- function(qtl, gff) {
  # look at all enrichment by trait
  c.list <- NULL
  
  # message
  message("Searching for GO term enrichment within QTL by trait")
  
  pb <- progress::progress_bar$new(format = "searching [:bar] :percent eta: :eta",
                                   clear = FALSE,
                                   total = length(unique(qtl$trait)))
  for(i in unique(qtl$trait)) {
    # get qtl in trait
    c <- qtl %>%
      dplyr::filter(trait == i)
    
    # get genes in the all qtl within the trait
    g.list <- NULL
    for(j in unique(c$qtl.id)){
      # get qtl
      q <- c %>%
        dplyr::filter(qtl.id == j)
      
      # get chrom and range
      g <- gff %>%
        dplyr::filter(seqname == q$CHROM &
                        start >= q$startPOS &
                        stop <= q$endPOS &
                        feature == "gene") %>%
        dplyr::mutate(WBGeneID = stringr::str_extract(attribute, pattern = regex("(?<=ID=gene:)(.*)(?=;Name=)")),
                      qtl.id = j,
                      trait = i) %>%
        dplyr::mutate(qtl.n.genes = n()) %>%
        dplyr::select(WBGeneID, qtl.id, qtl.n.genes, trait)
      
      # add it to the list
      g.list[[j]] <- g
    }
    # bind list together
    cg <- data.table::rbindlist(g.list)
    
    bp <- clusterProfiler::enrichGO(
      gene = cg$WBGeneID,
      OrgDb = org.Ce.eg.db,
      ont = "BP",
      pAdjustMethod = "bonferroni",
      keyType = 'ENSEMBL',
      pvalueCutoff = 0.05,
      qvalueCutoff = 0.05,
      readable = T)
    
    bp.dotplot <- enrichplot::dotplot(bp, showCategory=10) +
      ggplot2::labs(title = glue::glue("{i}"))
    #enrichplot::goplot(testbp)
    
    # add to class list
    c.list[[i]] <- list(cg, bp, bp.dotplot)
    
    # make a tick
    pb$tick()
  }
  # get all the plots together
  p.list <- NULL
  for(i in 1:length(c.list)) {
    # skip empty plots
    if(TRUE %in% c(c.list[[i]][[2]]@result$p.adjust <= 0.05)){
      p <- c.list[[i]][[3]]  
      p.list[[i]] <- p
    }
    else{
      next()
    }
    # drop the null elements
    p.list <- Filter(Negate(is.null), p.list) 
  }
  
  # combine em
  plot <- cowplot::plot_grid(plotlist = p.list, nrow = ceiling(sqrt(length(p.list))), align = "l")
  
  # return it
  return(list(c.list, plot))
}

# Process the results and make plot
procEn_trait <- function(qtl, enGO2List, plot.group = "big_class") {
  # make a list to hold the results
  l2 <- NULL
  # add existing data to new list
  for(i in 1:length(enGO2List[[1]])) {
    # skip null results
    if(is.null(enGO2List[[1]][[i]][[2]])){
      next()
    } else {
      # get the name of the group
      t.name <- unique(enGO2List[[1]][[i]][[1]]$trait)
      # get the enrichGO result data and add the group name
      enGO.df <- enGO2List[[1]][[i]][[2]]@result %>%
        dplyr::mutate(trait = t.name)
      # add it to the list
      l2[[t.name]] <- enGO.df
    }
  }
  # bind up the significant results
  enGO.out <- data.table::rbindlist(l2) %>%
    dplyr::filter(p.adjust < 0.05)
  
  # Get the qtl data we need to get the plot.group
  vars <- c("trait", plot.group)
  # select what's needed
  qtl.sel <- qtl %>%
    dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::distinct(dplyr::across(vars))
  
  # now join to the enGO.out
  plot.df <- left_join(enGO.out, qtl.sel) %>%
    dplyr::group_by(dplyr::across(vars)) %>%
    dplyr::mutate(n = dplyr::n(),
                  frac.GO = 1/n()) %>%
    dplyr::ungroup() %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches(plot.group)), ~ "plot.group")
    
  # plot it
  plot.list <- NULL
  for(i in unique(plot.df$plot.group)) {
    # filter the data
    pd <- plot.df %>%
      dplyr::filter(plot.group == i)
    # make a nice plot
    p <- ggplot2::ggplot(pd) +
      aes(x = trait, y = frac.GO, fill = Description) +
      ggplot2::geom_col(color = "black", linewidth = 0.15) +
      #ggplot2::geom_text(position = ggplot2::position_stack(vjust = 0.5)) +
      ggplot2::geom_text(aes(x = trait, y = 1.05, label = n)) +
      ggplot2::ylim(0,1.05) +
      theme_bw() +
      labs(y = "fraction", title = i) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    # add the plot to the list
    plot.list[[i]] <- p
  }
  
  # return some good stuff
  return(list(plot.df, plot.list))
  
}


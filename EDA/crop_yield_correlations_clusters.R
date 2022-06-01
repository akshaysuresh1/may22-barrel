


library( "corrplot" )
library( "tidyverse" )
library( "hrbrthemes")
library( "viridis" )
library( "gplots" )


# FOLDER STRUCTURE
maindir <- "~/Desktop/crop_yield_prediction"
indir <- "~/Desktop/crop_yield_prediction/input"
outdir <- "~/Desktop/crop_yield_prediction/output"


# SIGNIFICANCE LEVEL (P-VALUE CUTOFF)
sig_level <- 0.05
# IN CASE WE WANT TO LOOK AT CONFIDENCE INTERVALS FOR CORRELATION COEFFICIENTS LATER
conf_level <- 0.95
# TYPE OF CORRELATION COEFFICIENTS -- OPTIONS ARE: "pearson", "kendall", "spearman"
corr_method <- "spearman"


# READ DATA FRAME
rice_yield_clusters_df <- read.csv( file = paste0( indir, "/rice_yield_clusters.csv" ), header = TRUE, stringsAsFactors = FALSE )


# REDEFINE THE NPK VARIABLES AS THE AMOUNTS PER UNIT AREA (KG/HA)
rice_yield_clusters_df$nitrogen <- rice_yield_clusters_df$nitrogen/rice_yield_clusters_df$RICE.AREA..1000.ha.
rice_yield_clusters_df$phosphate <- rice_yield_clusters_df$phosphate/rice_yield_clusters_df$RICE.AREA..1000.ha.
rice_yield_clusters_df$potash <- rice_yield_clusters_df$potash/rice_yield_clusters_df$RICE.AREA..1000.ha.


# MAKE A SORTED LIST OF CLUSTER LABELS
cluster_labels <- sort( unique( rice_yield_clusters_df$Cluster ) )
# NUMBER OF CLUSTERS
n_clusters <- length( cluster_labels )
# NUMBER OF COVARIATES
n_covariates <- dim( rice_yield_clusters_df )[2] - 7



# REPLACE RICE.IRRIGATED.AREA BY RICE.IRRIGATED.AREA/RICE.AREA
rice_yield_clusters_df$RICE.IRRIGATED.AREA..1000.ha. <- rice_yield_clusters_df$RICE.IRRIGATED.AREA..1000.ha./rice_yield_clusters_df$RICE.AREA..1000.ha.
colnames( rice_yield_clusters_df )[which( colnames( rice_yield_clusters_df ) == "RICE.IRRIGATED.AREA..1000.ha." )] <- "RELATIVE.IRRIGATED.AREA"



# CREATE A LIST OF SAMPLES FOR EACH CLUSTER (SORTED IN ALPHABETICAL ORDER)
clusters_all <- vector( "list", length = n_clusters )
for( i in 1:length( cluster_labels ) ){
  clusters_all[[i]] <- subset( rice_yield_clusters_df, Cluster == cluster_labels[i] )
}




# COMPUTE CORRELATION AND P-VALUE MATRICES
corr_matrix <- matrix( rep( NA, n_clusters*n_covariates ), nrow = n_covariates, ncol = n_clusters )
pvalue_matrix <- matrix( rep( NA, n_clusters*n_covariates ), nrow = n_covariates, ncol = n_clusters )
for( i in 1:length( cluster_labels ) ){
  for( j in 1:n_covariates ){
    corr_matrix[j,i] <- cor( clusters_all[[i]][,6+j], clusters_all[[i]]$RICE.YIELD..kg.per.ha., use = 'na.or.complete', method = corr_method )
    pvalue_matrix[j,i] <- cor.test( clusters_all[[i]][,6+j], clusters_all[[i]]$RICE.YIELD..kg.per.ha., conf.level = conf_level, method = corr_method )$p.value
  }
}
colnames( corr_matrix ) <- cluster_labels
rownames( corr_matrix ) <- colnames( rice_yield_clusters_df )[7:(6+n_covariates)]
colnames( pvalue_matrix ) <- cluster_labels
rownames( pvalue_matrix ) <- colnames( rice_yield_clusters_df )[7:(6+n_covariates)]



# CLUSTER COVARIATES BY THEIR CORRELATIONS WITH YIELD DATA BY CLUSTER
# COMPUTE DISTANCE MATRIX FOR THE COVARIATES BY THEIR CORRELATIONS TO THE RICE YIELD DATA ON DIFFERENT CLUSTERS
distance_covariates <- dist( corr_matrix, method = "euclidian" )
# CLUSTER THE COVARIATES BY HIERARCHICAL CLUSTERING
cluster_covariates <- hclust( distance_covariates, method = "ward.D2" )



# CLUSTER CLUSTERS BY THEIR CORRELATIONS BETWEEN THEIR YIELD VALUES AND THE COVARIATES
# COMPUTE DISTANCE MATRIX FOR THE CLUSTERS BY THEIR CORRELATIONS TO THE COVARIATES
distance_clusters <- dist( t( corr_matrix ), method = "euclidian" )
# CLUSTER THE CLUSTERS BY HIERARCHICAL CLUSTERING
cluster_clusters <- hclust( distance_clusters, method = "ward.D2" )





# DEFINE FUNCTION TO PLOT A WELL-ADJUSTED HEATMAP
plot_heatmap <- function( input_matrix, color_list, color_scale_breaks, matrix_name ){
  
  heatmap.2( input_matrix,
             
             # DEFINE CLUSTERING OF BOTH ROWS AND COLUMNS
             Rowv = as.dendrogram( cluster_clusters ),
             Colv = as.dendrogram( cluster_covariates ),
             # dendrogram = "row",
             
             # revC = TRUE,
             trace = "none",
             # key = FALSE,
             # na.rm = FALSE,
             na.color = "black",
             
             # COLOR MAP
             col = color_list,
             breaks = color_scale_breaks,
             
             sepwidth = c(0.03, 0.03),  # width of the borders
             sepcolor='black',        # color of the separation lines
             colsep=1:ncol( input_matrix ),
             rowsep=1:nrow( input_matrix ),
             cexRow=1.5,
             labRow = rownames( input_matrix ),
             cexCol=1.5,
             labCol = colnames( input_matrix ),
             adjCol = c(NA, 0.5) , #shift column labels
             # offsetRow = -0.2,
             # offsetCol = -0.2,
             
             
             # ADJUST KEY PARAMETERS
             keysize =  0.5,  # alter key size
             key.title = NA,
             key.xlab = matrix_name, # add label to key
             key.par = list( mar = c( 5, 0.5, 2.5, 9.7 ),
                             # mar = c( "bottom.margin", "left.margin", "top.margin", "right.margin" )
                             cex=1,
                             cex.lab = 1.2,
                             cex.axis = 1.2 ),
             
             density.info='none',
             key.xtickfun = function() {
               breaks = pretty( parent.frame()$breaks, n = 10 )
               list( at = parent.frame()$scale01( breaks ),
                     labels = breaks )
             },
             
             # DEFINE LATTICE SECTIONS
             lmat = rbind( c(0, 3), c(2, 1), c(0, 4) ),   # 1=heatmap, 2=row dendrogram, 3=col dendrogram, 4= key
             lhei = c( 2, 19, 4 ) , # Alter dimensions of display array cell heighs
             lwid = c( 2, 17 ) , # Alter dimensions of display array cell widths
             mar = c( 18, 15 )  # bottom margin, left margin
             
  )
  
}



# DEFINE COLORS FOR CORRELATION MATRIX HEATMAP
color_vector_corr_coeff <- colorRampPalette( c("blue", "white", "red") )


# DEFINE COLORS FOR GOOD AND BAD P VALUES
# GOOD P VALUE < 0.05, I.E., -log10(p value) > 1.301
color_vector_pvalues_orange <- colorRampPalette( c("orange", "white") )   # BAD P-VALUES ARE ORANGE
color_vector_pvalues_green <- colorRampPalette( c("white", "green4") )   # GOOD P-VALUES ARE GREEN



# PLOT CORRELATION MATRIX HEATMAP
pdf( file = paste0( outdir, "/corr_matrix_heatmap_clusters_", corr_method, ".pdf" ), width = 8.5, height = 11 )
plot_heatmap( input_matrix = t( corr_matrix ),
              matrix_name = paste0( corr_method, " Correlation Coefficients" ),
              color_list = color_vector_corr_coeff(200),
              color_scale_breaks = (-100:100)/100 )
dev.off()



# PLOT P-VALUE MATRIX HEATMAP
pdf( file = paste0( outdir, "/pvalue_matrix_heatmap_clusters_", corr_method, ".pdf" ), width = 8.5, height = 11 )
plot_heatmap( input_matrix = t( -log10( pvalue_matrix ) ),
              matrix_name = "-log10 ( p value )",
              color_list = c( color_vector_pvalues_orange(13), color_vector_pvalues_green(199-13) ),
              color_scale_breaks = (1:200)/10 )
dev.off()





# MULTIPLE HYPOTHESIS TESTING CORRECTION


# MAKE A LIST OF ALL P VALUES IN THE P-VALUE MATRIX
pvalue_list <- NULL
for( i in 1:dim( pvalue_matrix )[2] ){
  pvalue_list <- append( pvalue_list, pvalue_matrix[,i] )
}


# ADJUST P-VALUES TO CONTROL THE FALSE DISCOVERY RATE (FDR)
pvalue_list_fdr_corrected <- as.numeric( p.adjust( pvalue_list, method = "fdr" ) )


# DEFINE NEW P-VALUE MATRIX AFTER MULTIPLE-TESTING CORRECTION
pvalue_matrix_fdr_corrected <- NULL
for( i in 1:( dim( pvalue_matrix )[2] ) ){
  pvalue_matrix_fdr_corrected <- cbind( pvalue_matrix_fdr_corrected, pvalue_list_fdr_corrected[( 1+(i-1)*dim( pvalue_matrix )[1] ):( i*dim( pvalue_matrix )[1] )] )
}
colnames( pvalue_matrix_fdr_corrected ) <- colnames( pvalue_matrix )
rownames( pvalue_matrix_fdr_corrected ) <- rownames( pvalue_matrix )



# DEFINE MATRIX OF SIGNIFICANT CORRELATIONS
corr_matrix_signif <- matrix( rep( NA, n_clusters*n_covariates ), nrow = n_covariates, ncol = n_clusters )
for( j in 1:dim( pvalue_matrix_fdr_corrected )[1] ){
  for( i in 1:dim( pvalue_matrix_fdr_corrected )[2] ){
    if( pvalue_matrix_fdr_corrected[j,i] < 0.05 ){
      corr_matrix_signif[j,i] <- corr_matrix[j,i]
    }
  }
}
colnames( corr_matrix_signif ) <- colnames( corr_matrix )
rownames( corr_matrix_signif ) <- rownames( corr_matrix )


# DEFINE P-VALUE MATRIX SHOWING ONLY THE SIGNIFICANT ONES
pvalue_matrix_fdr_corrected_signif <- matrix( rep( NA, n_clusters*n_covariates ), nrow = n_covariates, ncol = n_clusters )
for( j in 1:dim( pvalue_matrix_fdr_corrected )[1] ){
  for( i in 1:dim( pvalue_matrix_fdr_corrected )[2] ){
    if( pvalue_matrix_fdr_corrected[j,i] < 0.05 ){
      pvalue_matrix_fdr_corrected_signif[j,i] <- pvalue_matrix_fdr_corrected[j,i]
    }
  }
}
colnames( pvalue_matrix_fdr_corrected_signif ) <- colnames( pvalue_matrix )
rownames( pvalue_matrix_fdr_corrected_signif ) <- rownames( pvalue_matrix )





# PLOT CORRELATION MATRIX HEATMAP, DISPLAYING ONLY THE SIGNIFICANT CORRELATIONS
pdf( file = paste0( outdir, "/corr_matrix_signif_heatmap_clusters_", corr_method, ".pdf" ), width = 8.5, height = 11 )
plot_heatmap( input_matrix = t( corr_matrix_signif ),
              matrix_name = paste0( corr_method, " Correlation Coefficients" ),
              color_list = color_vector_corr_coeff(200),
              color_scale_breaks = (-100:100)/100 )
dev.off()




# PLOT P-VALUE MATRIX HEATMAP, DISPLAYING ONLY P VALUES < 0.05
pdf( file = paste0( outdir, "/pvalue_matrix_fdr_corrected_signif_heatmap_clusters_", corr_method, ".pdf" ), width = 8.5, height = 11 )
plot_heatmap( input_matrix = t( -log10( pvalue_matrix_fdr_corrected_signif ) ),
              matrix_name = "-log10 ( p value )",
              color_list = c( color_vector_pvalues_orange(13), color_vector_pvalues_green(199-13) ),
              color_scale_breaks = (1:200)/10 )
dev.off()











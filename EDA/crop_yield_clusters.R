


library( "corrplot" )
library( "tidyverse" )
library( "hrbrthemes")
library( "viridis" )
library( "ggfortify" )
library( "ggplot2" )
library( "RColorBrewer" )
library( "sets" )
library( "dendextend" )



# FOLDER STRUCTURE
maindir <- "~/Desktop/crop_yield_prediction"
indir <- "~/Desktop/crop_yield_prediction/input"
outdir <- "~/Desktop/crop_yield_prediction/output"


# DEFINE NUMBER OF CLUSTERS
n_clusters <- 6

# DEFINE A CORRELATION METHOD FOR THE DISTANCE METRIC -- OPTIONS ARE: "pearson", "kendall", "spearman"
corr_method <- "pearson"

# DEFINE CLIMATE-RELATED VARIABLES
clim_features <- c( "prec_grow",
                    "prec_harvest",
                    "et_grow",
                    "et_harvest",
                    "maxT_grow",
                    "maxT_harvest",
                    "minT_grow",
                    "minT_harvest",
                    "windspeed_grow",
                    "windspeed_harvest" )



# READ DATA FRAME
rice_yield_df <- read.csv( file = paste0( indir, "/rice_yield.csv" ), header = TRUE, stringsAsFactors = FALSE )


# SORTED LIST OF UNIQUE DISTRICT CODES FROM THE DATA FRAME
district_codes <- sort( unique( rice_yield_df$Dist.Code ) )
# NUMBER OF DISTRICTS
n_districts <- length( district_codes )
# NUMBER OF COVARIATES IN THE DATA SET
n_covariates <- dim( rice_yield_df )[2] - 6
# NUMBER OF CLIMATE-RELATED FEATURES
n_clim_feat <- length( clim_features )




# MAKE A NEW DATA FRAME CONTAINING ONLY THE CLIMATE-RELATED FEATURES
climate_df <- rice_yield_df[,which( colnames( rice_yield_df ) %in% clim_features )]

# DO PCA ON THE CLIMATE-RELATED FEATURES
pca_climate_res <- prcomp( climate_df, scale. = TRUE )
# COMPUTE A LIST SPECIFYING THE FRACTION OF THE VARIANCE EXPLAINED BY EACH PRINCIPAL COMPONENT
variance_expl <- ( ( pca_climate_res$sdev )^2 )/sum( ( pca_climate_res$sdev )^2 )





# FUNCTION TO MAKE PCA PLOTS COLOR-CODED BY STATE OR CLUSTER

make_PCA_plot <- function( full_data, pca_res, comp1, comp2, colored_by, color_scheme ){
  
  variance_expl <- ( ( pca_res$sdev )^2 )/sum( ( pca_res$sdev )^2 )
  
  if( colored_by == "state" ){
    p_pca <- ggplot( full_data,
                     aes( x = pca_res$x[,comp1],
                          y = pca_res$x[,comp2],
                          color = factor( State.Name ) ) )
  }else if( colored_by == "cluster" ){
    p_pca <- ggplot( full_data,
                     aes( x = pca_res$x[,comp1],
                          y = pca_res$x[,comp2],
                          color = factor( Cluster ) ) )
  }
  
  p_pca <- p_pca + xlab( paste0( "PC", comp1, " (", round( 100*variance_expl[comp1], digits = 2 ), "%)" ) ) +
    ylab( paste0( "PC", comp2, " (", round( 100*variance_expl[comp2], digits = 2 ), "%)" ) ) +
    geom_point()
  
  if( !is.null( color_scheme ) ){
    p_pca <- p_pca + scale_color_manual( values = color_scheme )
  }
  return( p_pca )

}



# FUNCTION TO MAKE SCATTERPLOTS COLOR-CODED BY STATE OR CLUSTER

make_scatterplot <- function( full_data, var1, var2, colored_by, color_scheme ){
  
  if( colored_by == "state" ){
    p_scat <- ggplot( full_data,
                      aes( x = get( var1 ),
                           y = get( var2 ),
                           color = factor( State.Name ) ) )
  }else if( colored_by == "cluster" ){
    p_scat <- ggplot( full_data,
                      aes( x = get( var1 ),
                           y = get( var2 ),
                           color = factor( Cluster ) ) )
  }
  
  p_scat <- p_scat + geom_point() + xlab( var1 ) + ylab( var2 )
  
  if( !is.null( color_scheme ) ){
    p_scat <- p_scat + scale_color_manual( values = color_scheme )
  }
  return( p_scat )
  
}




# PCA PLOTS OF THE CLIMATE-RELATED FEATURES COLOR-CODED BY STATE

pdf( file = paste0( outdir, "/scatterplots/colored_by_state/scatterplot_PC1_vs_PC2_colored_by_state.pdf" ) )
  make_PCA_plot( full_data = rice_yield_df, pca_res = pca_climate_res, comp1 = 1, comp2 = 2, colored_by = "state", color_scheme = NULL )
dev.off()

pdf( file = paste0( outdir, "/scatterplots/colored_by_state/scatterplot_PC1_vs_PC3_colored_by_state.pdf" ) )
  make_PCA_plot( full_data = rice_yield_df, pca_res = pca_climate_res, comp1 = 1, comp2 = 3, colored_by = "state", color_scheme = NULL )
dev.off()

pdf( file = paste0( outdir, "/scatterplots/colored_by_state/scatterplot_PC2_vs_PC3_colored_by_state.pdf" ) )
  make_PCA_plot( full_data = rice_yield_df, pca_res = pca_climate_res, comp1 = 2, comp2 = 3, colored_by = "state", color_scheme = NULL )
dev.off()




# A FEW SCATTERPLOTS OF ALL THE DATA POINTS, COLOR-CODED BY STATE

pdf( file = paste0( outdir, "/scatterplots/colored_by_state/scatterplot_prec_grow_vs_maxT_grow_colored_by_state.pdf" ) )
  make_scatterplot( full_data = rice_yield_df, var1 = "prec_grow", var2 = "maxT_grow", colored_by = "state", color_scheme = NULL )
dev.off()


pdf( file = paste0( outdir, "/scatterplots/colored_by_state/scatterplot_prec_grow_vs_minT_grow_colored_by_state.pdf" ) )
  make_scatterplot( full_data = rice_yield_df, var1 = "prec_grow", var2 = "minT_grow", colored_by = "state", color_scheme = NULL )
dev.off()


pdf( file = paste0( outdir, "/scatterplots/colored_by_state/scatterplot_maxT_grow_vs_minT_grow_colored_by_state.pdf" ) )
  make_scatterplot( full_data = rice_yield_df, var1 = "maxT_grow", var2 = "minT_grow", colored_by = "state", color_scheme = NULL )
dev.off()


pdf( file = paste0( outdir, "/scatterplots/colored_by_state/scatterplot_prec_grow_vs_et_grow_colored_by_state.pdf" ) )
  make_scatterplot( full_data = rice_yield_df, var1 = "prec_grow", var2 = "et_grow", colored_by = "state", color_scheme = NULL )
dev.off()


pdf( file = paste0( outdir, "/scatterplots/colored_by_state/scatterplot_et_grow_vs_maxT_grow_colored_by_state.pdf" ) )
  make_scatterplot( full_data = rice_yield_df, var1 = "et_grow", var2 = "maxT_grow", colored_by = "state", color_scheme = NULL )
dev.off()




# MAKE INFORMATION LISTS FOR ALL THE DISTRICTS, SPECIFYING DISTRICT
# NAMES AND THE CORRESPONDING STATES, ALL SORTED BY THE DISTRICT CODE
district_names_ordered_by_district_code <- rep( NA, n_districts )
states_list_by_district_code <- rep( NA, n_districts )
for( i in 1:length( district_codes ) ){
  district_names_ordered_by_district_code[i] <- unique( rice_yield_df$Dist.Name[which( rice_yield_df$Dist.Code == district_codes[i] )] )
  states_list_by_district_code[i] <- unique( rice_yield_df$State.Name[which( rice_yield_df$Dist.Code == district_codes[i] )] )
}



# DEFINE A MATRIX OF CLIMATE-RELATED FEATURES FOR EVERY DISTRICT,
# AVERAGED OVER ALL THE YEARS IN THE DATA SET (1990-2015)

climate_matrix <- matrix( rep( NA, n_clim_feat*n_districts ), nrow = n_districts, ncol = n_clim_feat )
for( i in 1:length( district_codes ) ){
  climate_matrix[i,] <- apply( subset( rice_yield_df, Dist.Code == district_codes[i] )[,which( colnames( rice_yield_df ) %in% clim_features )], 2, mean )
}




# COMPUTE DISTANCE MATRIX FOR EVERY PAIR OF DISTRICTS, USING THE METRIC dist(v1,v2) = sqrt( 2*( 1-r(v1,v2) ) ),
# WHERE r(v1,v2) IS THE CORRELATION COEFFICIENT BETWEEN VECTORS v1 AND v2.

distance_districts_climate <- matrix( rep( NA, n_districts^2 ), nrow = n_districts, ncol = n_districts )
for( i in 1:n_districts ){
  for( j in 1:n_districts ){
    distance_districts_climate[i,j] <- sqrt( 2*( 1 - cor( climate_matrix[i,], climate_matrix[j,], use = 'na.or.complete', method = corr_method ) ) )
  }
}

# LABEL DISTRICTS BY THEIR NAMES IN THE DISTANCE MATRIX
rownames( distance_districts_climate ) <- district_names_ordered_by_district_code
colnames( distance_districts_climate ) <- district_names_ordered_by_district_code


# USE HIERARCHICAL CLUSTERING TO COMPUTE THE DENDROGRAM FOR ALL INDIAN DISTRICTS IN THE DATA SET
cluster_districts_climate <- hclust( as.dist( distance_districts_climate ), method = "ward.D2" )


# PLOT CLUSTER DENDROGRAM TO VISUALIZE CLUSTERS, SHOWING DISTRICT NAMES
pdf( file = paste0( outdir, "/scatterplots/districts_dendrogram_district_names.pdf" ) )
dend <- as.dendrogram( cluster_districts_climate ) %>%
  set( "branches_k_color", k = n_clusters ) %>% set( "branches_lwd", 0.5 ) %>%
  set( "labels_colors" ) %>% set( "labels_cex", 0.05 )
plot( dend,
      xlab = "Height",
      main = "Cluster dendrogram for Indian districts",
      horiz = TRUE )
dev.off()


# LABEL DISTRICTS BY THEIR STATES IN THE DISTANCE MATRIX
rownames( distance_districts_climate ) <- states_list_by_district_code
colnames( distance_districts_climate ) <- states_list_by_district_code


# COMPUTE THE DENDROGRAM ONE MORE TIME, JUST TO MAKE A NEW PLOT SHOWING DISTRICTS LABELED BY THEIR STATES
cluster_districts_climate <- hclust( as.dist( distance_districts_climate ), method = "ward.D2" )


# PLOT CLUSTER DENDROGRAM TO VISUALIZE CLUSTERS, SHOWING DISTRICTS LABELED BY THEIR STATES
pdf( file = paste0( outdir, "/scatterplots/districts_dendrogram_state_names.pdf" ) )
dend <- as.dendrogram( cluster_districts_climate ) %>%
  set( "branches_k_color", k = n_clusters ) %>% set( "branches_lwd", 0.5 ) %>%
  set( "labels_colors" ) %>% set( "labels_cex", 0.05 )
plot( dend,
      xlab = "Height",
      main = "Cluster dendrogram for Indian districts",
      horiz = TRUE )
dev.off()




# CUT THE HIERARCHICAL CLUSTERING TREE TO GET THE DESIRED NUMBER OF CLUSTERS AND
# COMPUTE THE CLUSTER CLASSIFICATION OF ALL INDIAN DISTRICTS IN THE DATA SET
cluster_classification <- cutree( tree = cluster_districts_climate, k = n_clusters )


# MAKE A LIST OF CLUSTERS FOR EVERY ROW OF THE CSV FILE
clusters_list <- rep( NA, dim( rice_yield_df )[1] )
for( i in 1:length( district_codes ) ){
  clusters_list[which( rice_yield_df$Dist.Code == district_codes[i] )] <- cluster_classification[i]
}


# CREATE A NEW DATA FRAME WITH AN ADDED COLUMN SPECIFYING THE CLUSTER LABEL FOR EVERY ROW OF THE CSV FILE
rice_yield_clusters_df <- data.frame( rice_yield_df, Cluster = clusters_list )


# CREATE A DATA FRAME CONTAINING BASIC DISTRICT INFORMATION, INCLUDING THE CLUSTER LABELS
district_clusters_df <- data.frame( Dist.Code = district_codes,
                                    Dist.Name = district_names_ordered_by_district_code,
                                    State.Name = states_list_by_district_code,
                                    Cluster = cluster_classification )


# SAVE THE NEW DATA FRAMES AS CSV FILES
write.table( district_clusters_df, file = paste0( outdir, "/districts_clusters.csv" ), sep = ",", row.names = FALSE, col.names = TRUE )
write.table( rice_yield_clusters_df, file = paste0( outdir, "/rice_yield_clusters.csv" ), sep = ",", row.names = FALSE, col.names = TRUE )



# CREATE A COLOR MAP FOR THE CLUSTERS, TO BE USED IN THE CLUSTER-COLORED PLOTS
cols <- brewer.pal( n_clusters, "Set1" )
names( cols ) <- 1:n_clusters



# PCA PLOTS OF THE CLIMATE-RELATED FEATURES COLOR-CODED BY CLUSTERS

pdf( file = paste0( outdir, "/scatterplots/colored_by_cluster/scatterplot_PC1_vs_PC2_colored_by_cluster.pdf" ) )
  make_PCA_plot( full_data = rice_yield_clusters_df, pca_res = pca_climate_res, comp1 = 1, comp2 = 2, colored_by = "cluster", color_scheme = cols )
dev.off()

pdf( file = paste0( outdir, "/scatterplots/colored_by_cluster/scatterplot_PC1_vs_PC3_colored_by_cluster.pdf" ) )
  make_PCA_plot( full_data = rice_yield_clusters_df, pca_res = pca_climate_res, comp1 = 1, comp2 = 3, colored_by = "cluster", color_scheme = cols )
dev.off()

pdf( file = paste0( outdir, "/scatterplots/colored_by_cluster/scatterplot_PC2_vs_PC3_colored_by_cluster.pdf" ) )
  make_PCA_plot( full_data = rice_yield_clusters_df, pca_res = pca_climate_res, comp1 = 2, comp2 = 3, colored_by = "cluster", color_scheme = cols )
dev.off()



# A FEW SCATTERPLOTS OF ALL THE DATA POINTS, COLOR-CODED BY CLUSTER

pdf( file = paste0( outdir, "/scatterplots/colored_by_cluster/scatterplot_prec_grow_vs_maxT_grow_colored_by_cluster.pdf" ) )
  make_scatterplot( full_data = rice_yield_clusters_df, var1 = "prec_grow", var2 = "maxT_grow", colored_by = "cluster", color_scheme = cols )
dev.off()

pdf( file = paste0( outdir, "/scatterplots/colored_by_cluster/scatterplot_prec_grow_vs_minT_grow_colored_by_cluster.pdf" ) )
  make_scatterplot( full_data = rice_yield_clusters_df, var1 = "prec_grow", var2 = "minT_grow", colored_by = "cluster", color_scheme = cols )
dev.off()

pdf( file = paste0( outdir, "/scatterplots/colored_by_cluster/scatterplot_maxT_grow_vs_minT_grow_colored_by_cluster.pdf" ) )
  make_scatterplot( full_data = rice_yield_clusters_df, var1 = "maxT_grow", var2 = "minT_grow", colored_by = "cluster", color_scheme = cols )
dev.off()

pdf( file = paste0( outdir, "/scatterplots/colored_by_cluster/scatterplot_prec_grow_vs_et_grow_colored_by_cluster.pdf" ) )
  make_scatterplot( full_data = rice_yield_clusters_df, var1 = "prec_grow", var2 = "et_grow", colored_by = "cluster", color_scheme = cols )
dev.off()

pdf( file = paste0( outdir, "/scatterplots/colored_by_cluster/scatterplot_et_grow_vs_maxT_grow_colored_by_cluster.pdf" ) )
  make_scatterplot( full_data = rice_yield_clusters_df, var1 = "et_grow", var2 = "maxT_grow", colored_by = "cluster", color_scheme = cols )
dev.off()






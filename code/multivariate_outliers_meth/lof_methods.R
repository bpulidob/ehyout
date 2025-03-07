outlier_lof <- function(data, nrow_data, min_pts_ratio = .75, cutoff = .9){
  
  data_lof <- dbscan::lof(data, minPts = min_pts_ratio*nrow_data)
  data_lof_cutoff <- quantile(data_lof, cutoff)
  
  data_lof_pos <- data.frame(Position = 1:nrow_data,
                             LOF = data_lof) %>%
    arrange(LOF) %>%
    filter(LOF > data_lof_cutoff)
  
  return(list("values"=data_lof, "outliers"=data_lof_pos$Position))
}

outlier_mds_lof <- function(dataset, metric = "euclidean",
                            # min_pts_ratio taken from "geometric perspective on functional outlier detection"
                            min_pts_ratio = .75,
                            embed_dim = 0, cutoff = .9, ...) {
  distances <- as.matrix(dist(dataset, method = metric, ...))
  if(embed_dim > 0){
    #get metric MDS coordinates & use L2-distance in embedding space
    embeddings <- cmdscale(distances, k = embed_dim)
    distances <- as.matrix(dist(embeddings))
  }
  
  distances_lof <- outlier_lof(distances, nrow(distances), min_pts_ratio, cutoff)
  return(list("values"=distances_lof$values,"outliers"=distances_lof$outliers))
}
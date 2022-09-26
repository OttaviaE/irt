library(psych)
library(ggplot2)
library(TAM)

rm(list = ls())
set.seed(666)

cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}

load("model_theta_item.RData")
hist(true_theta)
lab_item <- 1:ncol(data) # la i è il numero di item 
num_item <- seq(10, 90, by = 20)  # la J che è il numero per cui dividere l'intervallo
# num_item <- c(2:9)
# parametri "veri" degli item 


# kmeans funziona solo su matrici quindi: 
# costruire una matrice con due colonne dove sulle due colonne ci sono gli stessi
# identici theta 

num_clusters <- num_item
# la divisione in cluster è fatta sulla base dei theta simulati 
theta_mat <- matrix(true_theta, ncol = 1)
info_start <- mean(IRT.informationCurves(m2pl, 
                                         theta = seq(-3,3, length = 1000))$test_info_curve)
# generate clusters

cluster <- list() 

for (i in 1:length(num_clusters)) {
  cluster[[i]] <- kmeans(theta_mat, 
                         centers = num_clusters[i])
}

# calcola l'informatività per ogni theta per ogni item

temp_k <- list()
value_k  <- list()
temp_k_data   <- NULL
info_data_k <- NULL

for (j in 1:length(cluster)) {
  value_k[[j]] <- cluster[[j]]$centers[, 1]
  
  for(i in 1:length(lab_item)) {
    for(m in 1:length(value_k[[j]])) {
      
      temp_k_data   <- data.frame(theta_target = IRT.informationCurves(m2pl, 
                                                                   theta = value_k[[j]][m], 
                                                                   iIndex = lab_item[i])$theta,
                              test_info = mean(IRT.informationCurves(m2pl, 
                                                                theta = value_k[[j]][m], 
                                                                iIndex = lab_item[i])$test_info_curve), 
                              item_info = colSums(IRT.informationCurves(m2pl, 
                                                                theta = value_k[[j]][m], 
                                                                iIndex = lab_item[i])$info_curves_item),
                              item = lab_item[i],
                              num_item = paste("number", length(value_k[[j]]), sep = ""))
      
      info_data_k <- rbind(info_data_k, temp_k_data  )
    }
  }
}

# per ogni cluster, selezione l'item con informatività massima
# seleziono i data set per ognuno dei vari cluster che ho selezionato


temp_data_k <- NULL
temp_maxcluster <- NULL
temp <- NULL
max_temp_k <- NULL

for (i in 1:length(unique(info_data_k$num_item))){
  temp_data <- info_data_k[info_data_k$num_item %in% unique(info_data_k$num_item)[i], ]
  temp_maxcluster <- aggregate(test_info ~ item + theta_target, 
                             data = temp_data, max)
  temp_maxcluster$cluster_name <- unique(temp_data$num_item)
  
  for (j in 1:length(unique(temp_maxcluster$theta_target))) {
    temp <- temp_maxcluster[which(temp_maxcluster$test_info == max(temp_maxcluster$test_info)), ]
    temp_maxcluster <- temp_maxcluster[which(temp_maxcluster$item != temp$item & 
                                           temp_maxcluster$theta_target != temp$theta_target), ]
    max_temp_k <-rbind(max_temp_k, temp)
    
  }
}


out_cluster <- list()
model_out_cluster <- list()
info_out_cluster <- list()

for (i in 1:length(unique(max_temp_k$cluster_name))) {
  out_cluster[[i]] <- data[, c(max_temp_k[max_temp_k$cluster_name %in% unique(max_temp_k$cluster_name)[i], 
                                      "item"])]
  model_out_cluster[[i]] <- tam.mml.2pl(out_cluster[[i]])
  info_out_cluster[[i]] <- IRT.informationCurves(model_out_cluster[[i]], 
                                               theta = seq(-3, 3, length = 1000))
  names(info_out_cluster)[[i]] <- unique(max_temp_k$cluster_name)[i]
}

out_cluster_theta <- list()
model_out_cluster_theta <- list()
info_out_cluster_theta <- list()



for (i in 1:length(unique(max_temp_k$cluster_name))) {
  out_cluster_theta[[i]] <- data[, c(max_temp_k[max_temp_k$cluster_name %in% unique(max_temp_k$cluster_name)[i], 
                                          "item"])]
  model_out_cluster_theta[[i]] <- tam.mml(out_cluster_theta[[i]], 
                                        xsi.fixed = cbind(1:ncol(out_cluster_theta[[i]]), 
                                                         diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                   colnames(out_cluster_theta[[i]]))), 2]),
                                                          B= array(c(rep(0, ncol(out_cluster_theta[[i]])), 
                                                                     discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                                     colnames(out_cluster_theta[[i]])))]), 
                                                                   c(ncol(out_cluster_theta[[i]]),2,1), 
                                                                   dimnames = list(colnames(out_cluster_theta[[i]]), 
                                                                                   c("Cat0", "Cat1"), 
                                                                                   "Dim01"))) 
  info_out_cluster_theta[[i]] <- IRT.informationCurves(model_out_cluster_theta[[i]], 
                                                 theta = seq(-3, 3, length = 1000))
  names(info_out_cluster_theta)[[i]] <- unique(max_temp_k$cluster_name)[i]
}


# cluster ----
info_summary_cluster <- NULL

temp <- NULL
for(i in 1:length(info_out_cluster)) {
  temp <- data.frame(info_test = mean(info_out_cluster[[i]]$test_info_curve), 
                     
                     
                     cluster_name = names(info_out_cluster)[[i]], 
                     item = paste(colnames(out_cluster[[i]]), collapse = ","))
  
  info_summary_cluster <- rbind(info_summary_cluster, 
                              temp)
}

info_summary_cluster$rel <- 1 - (1/sqrt(info_summary_cluster$info_test))^2

info_summary_cluster <-  rbind(info_summary_cluster, 
                             data.frame(info_test = info_start,
                                        cluster_name = "all", 
                                        item = "all", 
                                        rel = 1 - (1/sqrt(info_start))^2))
info_summary_cluster$selection <- "cluster"


info_summary_cluster_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_cluster_theta)) {
  temp <- data.frame(info_test = mean(info_out_cluster_theta[[i]]$test_info_curve), 
                     
                     
                     cluster_name = names(info_out_cluster_theta)[[i]], 
                     item = paste(colnames(out_cluster_theta[[i]]), collapse = ","))
  
  info_summary_cluster_theta <- rbind(info_summary_cluster_theta, 
                                      temp)
}

info_summary_cluster_theta$rel <- 1 - (1/sqrt(info_summary_cluster_theta$info_test))^2

info_summary_cluster_theta <-  rbind(info_summary_cluster_theta, 
                                     data.frame(info_test = info_start,
                                                cluster_name = "all", 
                                                item = "all", 
                                                rel = 1 - (1/sqrt(info_start))^2))
info_summary_cluster_theta$selection <- "cluster_theta"

ggplot(info_summary_cluster_theta, 
       aes(x = cluster_name, y = info_test)) + geom_point()
 

ggplot(info_summary_cluster_theta, 
       aes(x = cluster_name, y = rel)) + geom_point()


ggplot(info_summary_cluster, 
       aes(x = cluster_name, y = info_test)) + geom_point()


ggplot(info_summary_cluster, 
       aes(x = cluster_name, y = rel)) + geom_point()


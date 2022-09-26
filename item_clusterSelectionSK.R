library(psych)
library(ggplot2)

library(TAM)

rm(list = ls())
set.seed(999)
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}
# per sicurezza, meghlio usare ltm::factor.scores()

# Random -----

load("model_theta_itemSK.RData")
hist(true_theta_sk)
info_start_sk <- mean(IRT.informationCurves(m2pl_sk, 
                                       theta = seq(-3, 3, length = 1000))$test_info_curve)



lab_item_sk <- 1:ncol(data_sk) # la i è il numero di item 
#  #num_item_sk <- seq(5, 95, by = 5)  # la J che è il numero per cui dividere l'intervallo
# num_item_sk <- c(2:9)
# generate clusters

num_clusters_sk <- seq(10, 90, by = 20) 
# num_clusters_uni <- c(2:9)
theta_mat_sk <- matrix(true_theta_sk, ncol = 1)
info_start_sk <- mean(IRT.informationCurves(m2pl_sk, 
                                             theta = seq(-3,3, length = 1000))$test_info_curve)

cluster_sk <- list() 

for (i in 1:length(num_clusters_sk)) {
  cluster_sk[[i]] <- kmeans(theta_mat_sk, 
                            centers = num_clusters_sk[i])
}



# calcola l'informatività per ogni theta per ogni item

temp_k_sk <- list()
value_k_sk  <- list()
temp_k_data_sk   <- NULL
info_data_k_sk <- NULL

for (j in 1:length(cluster_sk)) {
  value_k_sk  [[j]] <- cluster_sk[[j]]$centers[, 1]
  
  for(i in 1:length(lab_item_sk)) {
    for(m in 1:length(value_k_sk  [[j]])) {
      
      temp_k_data_sk   <- data.frame(theta_target = IRT.informationCurves(m2pl_sk, 
                                                                           theta = value_k_sk  [[j]][m], 
                                                                           iIndex = lab_item_sk[i])$theta,
                                      test_info = mean(IRT.informationCurves(m2pl_sk, 
                                                                             theta = value_k_sk  [[j]][m], 
                                                                             iIndex = lab_item_sk[i])$test_info_curve), 
                                      item_info = colSums(IRT.informationCurves(m2pl_sk, 
                                                                                theta = value_k_sk  [[j]][m], 
                                                                                iIndex = lab_item_sk[i])$info_curves_item),
                                      item = lab_item_sk[i],
                                      num_item_sk = paste("number", length(value_k_sk  [[j]]), sep = ""))
      
      info_data_k_sk <- rbind(info_data_k_sk, temp_k_data_sk  )
    }
  }
}

# per ogni cluster, selezione l'item con informatività massima
# seleziono i data set per ognuno dei vari cluster che ho selezionato


temp_data_k_sk <- NULL
temp_maxcluster_sk <- NULL
temp <- NULL
max_temp_k_sk <- NULL

for (i in 1:length(unique(info_data_k_sk$num_item_sk))){
  temp_data <- info_data_k_sk[info_data_k_sk$num_item_sk %in% unique(info_data_k_sk$num_item_sk)[i], ]
  temp_maxcluster_sk <- aggregate(test_info ~ item + theta_target, 
                                   data = temp_data, max)
  temp_maxcluster_sk$cluster_name <- unique(temp_data$num_item_sk)
  
  for (j in 1:length(unique(temp_maxcluster_sk$theta_target))) {
    temp <- temp_maxcluster_sk[which(temp_maxcluster_sk$test_info == max(temp_maxcluster_sk$test_info)), ]
    temp_maxcluster_sk <- temp_maxcluster_sk[which(temp_maxcluster_sk$item != temp$item & 
                                                       temp_maxcluster_sk$theta_target != temp$theta_target), ]
    max_temp_k_sk <-rbind(max_temp_k_sk, temp)
    
  }
}


out_cluster_sk <- list()
model_out_cluster_sk <- list()
info_out_cluster_sk <- list()

for (i in 1:length(unique(max_temp_k_sk$cluster_name))) {
  out_cluster_sk[[i]] <- data_sk[, c(max_temp_k_sk[max_temp_k_sk$cluster_name %in% unique(max_temp_k_sk$cluster_name)[i], 
                                                      "item"])]
  model_out_cluster_sk[[i]] <- tam.mml.2pl(out_cluster_sk[[i]])
  info_out_cluster_sk[[i]] <- IRT.informationCurves(model_out_cluster_sk[[i]], 
                                                     theta = seq(-3, 3, length = 1000))
  names(info_out_cluster_sk)[[i]] <- unique(max_temp_k_sk$cluster_name)[i]
}

# cluster ----
info_summary_cluster_sk <- NULL
temp <- NULL
for(i in 1:length(info_out_cluster_sk)) {
  temp <- data.frame(info_test = mean(info_out_cluster_sk[[i]]$test_info_curve), 
                     
                     
                     cluster_name = names(info_out_cluster_sk)[[i]], 
                     item = paste(colnames(out_cluster_sk[[i]]), collapse = ","))
  
  info_summary_cluster_sk <- rbind(info_summary_cluster_sk, 
                                    temp)
}

info_summary_cluster_sk$rel <- 1 - (1/sqrt(info_summary_cluster_sk$info_test))^2

info_summary_cluster_sk <-  rbind(info_summary_cluster_sk, 
                                   data.frame(info_test = info_start_sk,
                                              cluster_name = "all", 
                                              item = "all", 
                                              rel = 1 - (1/sqrt(info_start_sk))^2))
info_summary_cluster_sk$selection <- "clustersk"


ggplot(info_summary_cluster_sk, 
       aes(x = cluster_name, y = info_test)) + geom_point()

ggplot(info_summary_cluster_sk, 
       aes(x = cluster_name, y = rel)) + geom_point()

out_cluster_sk <- list()
model_out_cluster_sk <- list()
info_out_cluster_sk <- list()

for (i in 1:length(unique(max_temp_k_sk$cluster_name))) {
  out_cluster_sk[[i]] <- data_sk[, c(max_temp_k_sk[max_temp_k_sk$cluster_name %in% unique(max_temp_k_sk$cluster_name)[i], 
                                                      "item"])]
  model_out_cluster_sk[[i]] <- tam.mml.2pl(out_cluster_sk[[i]])
  info_out_cluster_sk[[i]] <- IRT.informationCurves(model_out_cluster_sk[[i]], 
                                                     theta = seq(-3, 3, length = 1000))
  names(info_out_cluster_sk)[[i]] <- unique(max_temp_k_sk$cluster_name)[i]
}


# Parametri fissi ----
out_cluster_sk_theta <- list()
model_out_cluster_sk_theta <- list()
info_out_cluster_sk_theta <- list()

for (i in 1:length(unique(max_temp_k_sk$cluster_name))) {
  out_cluster_sk_theta[[i]] <- data_sk[, c(max_temp_k_sk[max_temp_k_sk$cluster_name %in% unique(max_temp_k_sk$cluster_name)[i], 
                                                            "item"])]
  model_out_cluster_sk_theta[[i]] <- tam.mml(out_cluster_sk_theta[[i]], 
                                              xsi.fixed = cbind(1:ncol(out_cluster_sk_theta[[i]]), 
                                                                diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                          colnames(out_cluster_sk_theta[[i]]))), 2]), 
                                             B= array(c(rep(0, ncol(out_cluster_sk_theta[[i]])), 
                                                        discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                        colnames(out_cluster_sk_theta[[i]])))]), 
                                                      c(ncol(out_cluster_sk_theta[[i]]),2,1), 
                                                      dimnames = list(colnames(out_cluster_sk_theta[[i]]), 
                                                                      c("Cat0", "Cat1"), 
                                                                      "Dim01")))
  info_out_cluster_sk_theta[[i]] <- IRT.informationCurves(model_out_cluster_sk_theta[[i]], 
                                                           theta = seq(-3, 3, length = 1000))
  names(info_out_cluster_sk_theta)[[i]] <- unique(max_temp_k_sk$cluster_name)[i]
}


# cluster con theta e parametri fissi ----
info_summary_cluster_sk_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_cluster_sk)) {
  temp <- data.frame(info_test = mean(info_out_cluster_sk_theta[[i]]$test_info_curve), 
                     
                     
                     cluster_name = names(info_out_cluster_sk_theta)[[i]], 
                     item = paste(colnames(out_cluster_sk_theta[[i]]), collapse = ","))
  
  info_summary_cluster_sk_theta <- rbind(info_summary_cluster_sk_theta, 
                                          temp)
}

info_summary_cluster_sk_theta$rel <- 1 - (1/sqrt(info_summary_cluster_sk_theta$info_test))^2

info_summary_cluster_sk_theta <-  rbind(info_summary_cluster_sk_theta, 
                                         data.frame(info_test = info_start_sk,
                                                    cluster_name = "all", 
                                                    item = "all", 
                                                    rel = 1 - (1/sqrt(info_start_sk))^2))
info_summary_cluster_sk_theta$selection <- "clustersk"


ggplot(info_summary_cluster_sk_theta, 
       aes(x = cluster_name, y = info_test)) + geom_point()

ggplot(info_summary_cluster_sk_theta, 
       aes(x = cluster_name, y = rel)) + geom_point()

ggplot(info_summary_cluster_sk, 
       aes(x = cluster_name, y = info_test)) + geom_point()

ggplot(info_summary_cluster_sk, 
       aes(x = cluster_name, y = rel)) + geom_point()






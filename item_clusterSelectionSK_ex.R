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
# per sicurezza, meghlio usare ltm::factor.scores()

# Random -----

load("model_theta_itemSK_ex.RData")
hist(true_theta_sk_ex)
info_start_sk_ex <- mean(IRT.informationCurves(m2pl_sk_ex, 
                                       theta = seq(-3, 3, length = 1000))$test_info_curve)



lab_item_sk_ex <- 1:ncol(data_sk_ex) # la i è il numero di item 
#  #num_item_sk_ex <- seq(5, 95, by = 5)  # la J che è il numero per cui dividere l'intervallo
# num_item_sk_ex <- c(2:9)
# generate clusters

num_clusters_sk_ex <- seq(10, 90, by = 10) 
# num_clusters_uni <- c(2:9)
theta_mat_sk_ex <- matrix(true_theta_sk_ex, ncol = 1)
info_start_sk_ex <- mean(IRT.informationCurves(m2pl_sk_ex, 
                                             theta = seq(-3,3, length = 1000))$test_info_curve)

cluster_sk_ex <- list() 

for (i in 1:length(num_clusters_sk_ex)) {
  cluster_sk_ex[[i]] <- kmeans(theta_mat_sk_ex, 
                            centers = num_clusters_sk_ex[i])
}



# calcola l'informatività per ogni theta per ogni item

temp_k_sk_ex <- list()
value_k_sk_ex  <- list()
temp_k_data_sk_ex   <- NULL
info_data_k_sk_ex <- NULL

for (j in 1:length(cluster_sk_ex)) {
  value_k_sk_ex  [[j]] <- cluster_sk_ex[[j]]$centers[, 1]
  
  for(i in 1:length(lab_item_sk_ex)) {
    for(m in 1:length(value_k_sk_ex  [[j]])) {
      
      temp_k_data_sk_ex   <- data.frame(theta_target = IRT.informationCurves(m2pl_sk_ex, 
                                                                           theta = value_k_sk_ex  [[j]][m], 
                                                                           iIndex = lab_item_sk_ex[i])$theta,
                                      test_info = mean(IRT.informationCurves(m2pl_sk_ex, 
                                                                             theta = value_k_sk_ex  [[j]][m], 
                                                                             iIndex = lab_item_sk_ex[i])$test_info_curve), 
                                      item_info = colSums(IRT.informationCurves(m2pl_sk_ex, 
                                                                                theta = value_k_sk_ex  [[j]][m], 
                                                                                iIndex = lab_item_sk_ex[i])$info_curves_item),
                                      item = lab_item_sk_ex[i],
                                      num_item_sk_ex = paste("number", length(value_k_sk_ex  [[j]]), sep = ""))
      
      info_data_k_sk_ex <- rbind(info_data_k_sk_ex, temp_k_data_sk_ex  )
    }
  }
}

# per ogni cluster, selezione l'item con informatività massima
# seleziono i data set per ognuno dei vari cluster che ho selezionato


temp_data_k_sk_ex <- NULL
temp_maxcluster_sk_ex <- NULL
temp <- NULL
max_temp_k_sk_ex <- NULL

for (i in 1:length(unique(info_data_k_sk_ex$num_item_sk_ex))){
  temp_data <- info_data_k_sk_ex[info_data_k_sk_ex$num_item_sk_ex %in% unique(info_data_k_sk_ex$num_item_sk_ex)[i], ]
  temp_maxcluster_sk_ex <- aggregate(test_info ~ item + theta_target, 
                                   data = temp_data, max)
  temp_maxcluster_sk_ex$cluster_name <- unique(temp_data$num_item_sk_ex)
  
  for (j in 1:length(unique(temp_maxcluster_sk_ex$theta_target))) {
    temp <- temp_maxcluster_sk_ex[which(temp_maxcluster_sk_ex$test_info == max(temp_maxcluster_sk_ex$test_info)), ]
    temp_maxcluster_sk_ex <- temp_maxcluster_sk_ex[which(temp_maxcluster_sk_ex$item != temp$item & 
                                                       temp_maxcluster_sk_ex$theta_target != temp$theta_target), ]
    max_temp_k_sk_ex <-rbind(max_temp_k_sk_ex, temp)
    
  }
}


out_cluster_sk_ex <- list()
model_out_cluster_sk_ex <- list()
info_out_cluster_sk_ex <- list()

for (i in 1:length(unique(max_temp_k_sk_ex$cluster_name))) {
  out_cluster_sk_ex[[i]] <- data_sk_ex[, c(max_temp_k_sk_ex[max_temp_k_sk_ex$cluster_name %in% unique(max_temp_k_sk_ex$cluster_name)[i], 
                                                      "item"])]
  model_out_cluster_sk_ex[[i]] <- tam.mml.2pl(out_cluster_sk_ex[[i]])
  info_out_cluster_sk_ex[[i]] <- IRT.informationCurves(model_out_cluster_sk_ex[[i]], 
                                                     theta = seq(-3, 3, length = 1000))
  names(info_out_cluster_sk_ex)[[i]] <- unique(max_temp_k_sk_ex$cluster_name)[i]
}

# cluster ----
info_summary_cluster_sk_ex <- NULL
temp <- NULL
for(i in 1:length(info_out_cluster_sk_ex)) {
  temp <- data.frame(info_test = mean(info_out_cluster_sk_ex[[i]]$test_info_curve), 
                     
                     
                     cluster_name = names(info_out_cluster_sk_ex)[[i]], 
                     item = paste(colnames(out_cluster_sk_ex[[i]]), collapse = ","))
  
  info_summary_cluster_sk_ex <- rbind(info_summary_cluster_sk_ex, 
                                    temp)
}

info_summary_cluster_sk_ex$rel <- 1 - (1/sqrt(info_summary_cluster_sk_ex$info_test))^2

info_summary_cluster_sk_ex <-  rbind(info_summary_cluster_sk_ex, 
                                   data.frame(info_test = info_start_sk_ex,
                                              cluster_name = "all", 
                                              item = "all", 
                                              rel = 1 - (1/sqrt(info_start_sk_ex))^2))
info_summary_cluster_sk_ex$selection <- "clustersk_ex"


ggplot(info_summary_cluster_sk_ex, 
       aes(x = cluster_name, y = info_test)) + geom_point()

ggplot(info_summary_cluster_sk_ex, 
       aes(x = cluster_name, y = rel)) + geom_point()

out_cluster_sk_ex <- list()
model_out_cluster_sk_ex <- list()
info_out_cluster_sk_ex <- list()

for (i in 1:length(unique(max_temp_k_sk_ex$cluster_name))) {
  out_cluster_sk_ex[[i]] <- data_sk_ex[, c(max_temp_k_sk_ex[max_temp_k_sk_ex$cluster_name %in% unique(max_temp_k_sk_ex$cluster_name)[i], 
                                                      "item"])]
  model_out_cluster_sk_ex[[i]] <- tam.mml.2pl(out_cluster_sk_ex[[i]])
  info_out_cluster_sk_ex[[i]] <- IRT.informationCurves(model_out_cluster_sk_ex[[i]], 
                                                     theta = seq(-3, 3, length = 1000))
  names(info_out_cluster_sk_ex)[[i]] <- unique(max_temp_k_sk_ex$cluster_name)[i]
}


# Parametri fissi ----
out_cluster_sk_ex_theta <- list()
model_out_cluster_sk_ex_theta <- list()
info_out_cluster_sk_ex_theta <- list()

for (i in 1:length(unique(max_temp_k_sk_ex$cluster_name))) {
  out_cluster_sk_ex_theta[[i]] <- data_sk_ex[, c(max_temp_k_sk_ex[max_temp_k_sk_ex$cluster_name %in% unique(max_temp_k_sk_ex$cluster_name)[i], 
                                                            "item"])]
  model_out_cluster_sk_ex_theta[[i]] <- tam.mml(out_cluster_sk_ex_theta[[i]], 
                                              xsi.fixed = cbind(1:ncol(out_cluster_sk_ex_theta[[i]]), 
                                                                diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                          colnames(out_cluster_sk_ex_theta[[i]]))), 2]), 
                                             B= array(c(rep(0, ncol(out_cluster_sk_ex_theta[[i]])), 
                                                        discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                        colnames(out_cluster_sk_ex_theta[[i]])))]), 
                                                      c(ncol(out_cluster_sk_ex_theta[[i]]),2,1), 
                                                      dimnames = list(colnames(out_cluster_sk_ex_theta[[i]]), 
                                                                      c("Cat0", "Cat1"), 
                                                                      "Dim01")))
  info_out_cluster_sk_ex_theta[[i]] <- IRT.informationCurves(model_out_cluster_sk_ex_theta[[i]], 
                                                           theta = seq(-3, 3, length = 1000))
  names(info_out_cluster_sk_ex_theta)[[i]] <- unique(max_temp_k_sk_ex$cluster_name)[i]
}


# cluster con theta e parametri fissi ----
info_summary_cluster_sk_ex_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_cluster_sk_ex)) {
  temp <- data.frame(info_test = mean(info_out_cluster_sk_ex_theta[[i]]$test_info_curve), 
                     
                     
                     cluster_name = names(info_out_cluster_sk_ex_theta)[[i]], 
                     item = paste(colnames(out_cluster_sk_ex_theta[[i]]), collapse = ","))
  
  info_summary_cluster_sk_ex_theta <- rbind(info_summary_cluster_sk_ex_theta, 
                                          temp)
}

info_summary_cluster_sk_ex_theta$rel <- 1 - (1/sqrt(info_summary_cluster_sk_ex_theta$info_test))^2

info_summary_cluster_sk_ex_theta <-  rbind(info_summary_cluster_sk_ex_theta, 
                                         data.frame(info_test = info_start_sk_ex,
                                                    cluster_name = "all", 
                                                    item = "all", 
                                                    rel = 1 - (1/sqrt(info_start_sk_ex))^2))
info_summary_cluster_sk_ex_theta$selection <- "clustersk_ex"


ggplot(info_summary_cluster_sk_ex_theta, 
       aes(x = cluster_name, y = info_test)) + geom_point()

ggplot(info_summary_cluster_sk_ex_theta, 
       aes(x = cluster_name, y = rel)) + geom_point()

ggplot(info_summary_cluster_sk_ex, 
       aes(x = cluster_name, y = info_test)) + geom_point()

ggplot(info_summary_cluster_sk_ex, 
       aes(x = cluster_name, y = rel)) + geom_point()






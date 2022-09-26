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

load("model_theta_itemUNI.RData")
hist(true_theta_uni)
# info_start <- mean((IRT.informationCurves(m2pl_uni,
#                                           theta =
#                                             seq(-3,3, length = 1000))$test_info_curve))


# data_all <- sim.irt(nvar = 10, 
#                     n = 1000, 
#                     low = -3, 
#                     high = 3, 
#                     a = c(runif(10, 0.4, 1)))
# 
# data <- data.frame(data_all$items)
# 
# 
# 
# 
# # va fittato un modello per ottenere i theta
# m2pl_uni <- tam.mml(data)
# info_start <- IRT.informationCurves(m2pl_uni) 
# # estraggo i theta 
# theta <- m2pl_uni$person$EAP
# 
# # convieve usare m2pl_uni$item_irt per i parametri degli item 
# # perché provved la IRT parametrization (default di ltm)
# item <- m2pl_uni$item_irt

lab_item_uni <- 1:ncol(data_uni) # la i è il numero di item 
#num_item_uni <- seq(5, 95, by = 5)  # la J che è il numero per cui dividere l'intervallo
#num_item_uni <- c(2:9)



# kmeans funziona solo su matrici quindi: 
# costruire una matrice con due colonne dove sulle due colonne ci sono gli stessi
# identici theta 

num_clusters_uni <- seq(10, 90, by = 20) 
# num_clusters_uni <- c(2:9)
theta_mat_uni <- matrix(true_theta_uni, ncol = 1)
info_start_uni <- mean(IRT.informationCurves(m2pl_uni, 
                                        theta = seq(-3,3, length = 1000))$test_info_curve)
# generate clusters

cluster_uni <- list() 

for (i in 1:length(num_clusters_uni)) {
  cluster_uni[[i]] <- kmeans(theta_mat_uni, 
                            centers = num_clusters_uni[i])
}

# calcola l'informatività per ogni theta per ogni item

temp_k_uni <- list()
value_k_uni  <- list()
temp_k_data_uni   <- NULL
info_data_k_uni <- NULL

for (j in 1:length(cluster_uni)) {
  value_k_uni  [[j]] <- cluster_uni[[j]]$centers[, 1]
  
  for(i in 1:length(lab_item_uni)) {
    for(m in 1:length(value_k_uni  [[j]])) {
      
      temp_k_data_uni   <- data.frame(theta_target = IRT.informationCurves(m2pl_uni, 
                                                                          theta = value_k_uni  [[j]][m], 
                                                                          iIndex = lab_item_uni[i])$theta,
                                     test_info = mean(IRT.informationCurves(m2pl_uni, 
                                                                            theta = value_k_uni  [[j]][m], 
                                                                            iIndex = lab_item_uni[i])$test_info_curve), 
                                     item_info = colSums(IRT.informationCurves(m2pl_uni, 
                                                                               theta = value_k_uni  [[j]][m], 
                                                                               iIndex = lab_item_uni[i])$info_curves_item),
                                     item = lab_item_uni[i],
                                     num_item_uni = paste("number", length(value_k_uni  [[j]]), sep = ""))
      
      info_data_k_uni <- rbind(info_data_k_uni, temp_k_data_uni  )
    }
  }
}

# per ogni cluster, selezione l'item con informatività massima
# seleziono i data set per ognuno dei vari cluster che ho selezionato


temp_data_k_uni <- NULL
temp_maxcluster_uni <- NULL
temp <- NULL
max_temp_k_uni <- NULL

for (i in 1:length(unique(info_data_k_uni$num_item_uni))){
  temp_data <- info_data_k_uni[info_data_k_uni$num_item_uni %in% unique(info_data_k_uni$num_item_uni)[i], ]
  temp_maxcluster_uni <- aggregate(test_info ~ item + theta_target, 
                                  data = temp_data, max)
  temp_maxcluster_uni$cluster_name <- unique(temp_data$num_item_uni)
  
  for (j in 1:length(unique(temp_maxcluster_uni$theta_target))) {
    temp <- temp_maxcluster_uni[which(temp_maxcluster_uni$test_info == max(temp_maxcluster_uni$test_info)), ]
    temp_maxcluster_uni <- temp_maxcluster_uni[which(temp_maxcluster_uni$item != temp$item & 
                                                     temp_maxcluster_uni$theta_target != temp$theta_target), ]
    max_temp_k_uni <-rbind(max_temp_k_uni, temp)
    
  }
}


out_cluster_uni <- list()
model_out_cluster_uni <- list()
info_out_cluster_uni <- list()

for (i in 1:length(unique(max_temp_k_uni$cluster_name))) {
  out_cluster_uni[[i]] <- data_uni[, c(max_temp_k_uni[max_temp_k_uni$cluster_name %in% unique(max_temp_k_uni$cluster_name)[i], 
                                                   "item"])]
  model_out_cluster_uni[[i]] <- tam.mml.2pl(out_cluster_uni[[i]])
  info_out_cluster_uni[[i]] <- IRT.informationCurves(model_out_cluster_uni[[i]], 
                                                    theta = seq(-3, 3, length = 1000))
  names(info_out_cluster_uni)[[i]] <- unique(max_temp_k_uni$cluster_name)[i]
}

# cluster ----
info_summary_cluster_uni <- NULL
temp <- NULL
for(i in 1:length(info_out_cluster_uni)) {
  temp <- data.frame(info_test = mean(info_out_cluster_uni[[i]]$test_info_curve), 
                     
                     
                     cluster_name = names(info_out_cluster_uni)[[i]], 
                     item = paste(colnames(out_cluster_uni[[i]]), collapse = ","))
  
  info_summary_cluster_uni <- rbind(info_summary_cluster_uni, 
                                   temp)
}

info_summary_cluster_uni$rel <- 1 - (1/sqrt(info_summary_cluster_uni$info_test))^2

info_summary_cluster_uni <-  rbind(info_summary_cluster_uni, 
                                  data.frame(info_test = info_start_uni,
                                             cluster_name = "all", 
                                             item = "all", 
                                             rel = 1 - (1/sqrt(info_start_uni))^2))
info_summary_cluster_uni$selection <- "clusteruni"


ggplot(info_summary_cluster_uni, 
       aes(x = cluster_name, y = info_test)) + geom_point()

ggplot(info_summary_cluster_uni, 
       aes(x = cluster_name, y = rel)) + geom_point()

out_cluster_uni <- list()
model_out_cluster_uni <- list()
info_out_cluster_uni <- list()

for (i in 1:length(unique(max_temp_k_uni$cluster_name))) {
  out_cluster_uni[[i]] <- data_uni[, c(max_temp_k_uni[max_temp_k_uni$cluster_name %in% unique(max_temp_k_uni$cluster_name)[i], 
                                                      "item"])]
  model_out_cluster_uni[[i]] <- tam.mml.2pl(out_cluster_uni[[i]])
  info_out_cluster_uni[[i]] <- IRT.informationCurves(model_out_cluster_uni[[i]], 
                                                     theta = seq(-3, 3, length = 1000))
  names(info_out_cluster_uni)[[i]] <- unique(max_temp_k_uni$cluster_name)[i]
}


# Parametri fissi ----
out_cluster_uni_theta <- list()
model_out_cluster_uni_theta <- list()
info_out_cluster_uni_theta <- list()

for (i in 1:length(unique(max_temp_k_uni$cluster_name))) {
  out_cluster_uni_theta[[i]] <- data_uni[, c(max_temp_k_uni[max_temp_k_uni$cluster_name %in% unique(max_temp_k_uni$cluster_name)[i], 
                                                            "item"])]
  model_out_cluster_uni_theta[[i]] <- tam.mml(out_cluster_uni_theta[[i]], 
                                                  xsi.fixed = cbind(1:ncol(out_cluster_uni_theta[[i]]), 
                                                                    diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                                  colnames(out_cluster_uni_theta[[i]]))), 2]), 
                                              B= array(c(rep(0, ncol(out_cluster_uni_theta[[i]])), 
                                                         discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                         colnames(out_cluster_uni_theta[[i]])))]), 
                                                       c(ncol(out_cluster_uni_theta[[i]]),2,1), 
                                                       dimnames = list(colnames(out_cluster_uni_theta[[i]]), 
                                                                       c("Cat0", "Cat1"), 
                                                                       "Dim01")))
  info_out_cluster_uni_theta[[i]] <- IRT.informationCurves(model_out_cluster_uni_theta[[i]], 
                                                           theta = seq(-3, 3, length = 1000))
  names(info_out_cluster_uni_theta)[[i]] <- unique(max_temp_k_uni$cluster_name)[i]
}

# cluster con theta e parametri fissi ----
info_summary_cluster_uni_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_cluster_uni)) {
  temp <- data.frame(info_test = mean(info_out_cluster_uni_theta[[i]]$test_info_curve), 
                     
                     
                     cluster_name = names(info_out_cluster_uni_theta)[[i]], 
                     item = paste(colnames(out_cluster_uni_theta[[i]]), collapse = ","))
  
  info_summary_cluster_uni_theta <- rbind(info_summary_cluster_uni_theta, 
                                          temp)
}

info_summary_cluster_uni_theta$rel <- 1 - (1/sqrt(info_summary_cluster_uni_theta$info_test))^2

info_summary_cluster_uni_theta <-  rbind(info_summary_cluster_uni_theta, 
                                         data.frame(info_test = info_start_uni,
                                                    cluster_name = "all", 
                                                    item = "all", 
                                                    rel = 1 - (1/sqrt(info_start_uni))^2))
info_summary_cluster_uni_theta$selection <- "clusteruni"


ggplot(info_summary_cluster_uni_theta, 
       aes(x = cluster_name, y = info_test)) + geom_point()

ggplot(info_summary_cluster_uni_theta, 
       aes(x = cluster_name, y = rel)) + geom_point()

ggplot(info_summary_cluster_uni, 
       aes(x = cluster_name, y = info_test)) + geom_point()

ggplot(info_summary_cluster_uni, 
       aes(x = cluster_name, y = rel)) + geom_point()
 

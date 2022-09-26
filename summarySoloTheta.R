# Dati skewness

rm(list = ls())
load("SKrandom.RData")
load("SKsmart.RData")
load("SKguided.RData")
load("SKguidedNEW.RData")
load("SKcluster.RData")

# # Stessa cosa item con theta sk_theta PARAMETRI FISSI -----
data_random_sk_theta_summary$selection <- "random"
all_data_sk_theta <- rbind(data_random_sk_theta_summary,
                           data.frame(num_item = info_summary_range_sk_theta$range_name,
                                      mean_info = info_summary_range_sk_theta$info_test,
                                      sd_info = 0,
                                      selection = info_summary_range_sk_theta$selection,
                                      mean_rel = info_summary_range_sk_theta$rel,
                                      sd_rel = 0),
                           data.frame(num_item = info_summary_range_new_sk_theta$range_new_name,
                                      mean_info = info_summary_range_new_sk_theta$info_test,
                                      sd_info = 0,
                                      selection = info_summary_range_new_sk_theta$selection,
                                      mean_rel = info_summary_range_new_sk_theta$rel,
                                      sd_rel = 0),
                           data.frame(num_item = info_summary_cluster_sk_theta$cluster_name,
                                      mean_info = info_summary_cluster_sk_theta$info_test,
                                      sd_info = 0,
                                      selection = info_summary_cluster_sk_theta$selection,
                                      mean_rel = info_summary_cluster_sk_theta$rel,
                                      sd_rel = 0),

                           data.frame(num_item = info_summary_smart_sk_theta$smart_name,
                                      mean_info = info_summary_smart_sk_theta$info_test,
                                      sd_info = 0,
                                      selection = info_summary_smart_sk_theta$selection,
                                      mean_rel = info_summary_smart_sk_theta$rel,
                                      sd_rel = 0))
all_data_sk_theta$item_temp <- gsub("number", "", all_data_sk_theta$num_item)
all_data_sk_theta$item_temp <- gsub("all", 0, all_data_sk_theta$item_temp)
all_data_sk_theta$item_temp <- as.integer(all_data_sk_theta$item_temp)
all_data_sk_theta <- all_data_sk_theta[order(all_data_sk_theta$item_temp), ]
all_data_sk_theta$selection <- gsub("sk_theta", '', all_data_sk_theta$selection)
ggplot(all_data_sk_theta[!all_data_sk_theta$num_item %in%"all", ],
       aes(x=as.factor(item_temp), y=mean_info,
           group=selection, color=selection)) +
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info),
                width=.2,
                position=position_dodge(0.05)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels =  unique(all_data_sk_theta[!all_data_sk_theta$num_item %in%"all", "num_item"])) +
  geom_hline(yintercept = all_data_sk_theta[all_data_sk_theta$num_item %in% "all",
                                            "mean_info"])

# grafico info ski prendendo solo la prima combo di item da numerosità di item

sk_random_theta = NULL 
temp = NULL
for(i in 1:length(unique(data_random_sk_theta$num_item))) {
  temp = data_random_sk_theta[data_random_sk_theta$num_item %in% unique(data_random_sk_theta$num_item)[i], ]
  temp = temp[1, ]
  sk_random_theta = rbind(sk_random_theta, temp)
}
sk_random_theta$selection = "random_theta" 
# unisco al data set 
data_sk_random_theta_unique = rbind(data.frame(num_item = sk_random_theta$num_item,
                                               mean_info = sk_random_theta$info_total,
                                               sd_info = 0,
                                               selection = sk_random_theta$selection,
                                               mean_rel = sk_random_theta$rel,
                                               sd_rel = 0), 
                                    data.frame(num_item = info_summary_range_sk_theta$range_name, 
                                               mean_info = info_summary_range_sk_theta$info_test, 
                                               sd_info = 0, 
                                               selection = info_summary_range_sk_theta$selection, 
                                               mean_rel = info_summary_range_sk_theta$rel, 
                                               sd_rel = 0), 
                                    data.frame(num_item = info_summary_range_new_sk_theta$range_new_name, 
                                               mean_info = info_summary_range_new_sk_theta$info_test, 
                                               sd_info = 0, 
                                               selection = info_summary_range_new_sk_theta$selection, 
                                               mean_rel = info_summary_range_new_sk_theta$rel, 
                                               sd_rel = 0), 
                                    data.frame(num_item = info_summary_cluster_sk_theta$cluster_name, 
                                               mean_info = info_summary_cluster_sk_theta$info_test, 
                                               sd_info = 0, 
                                               selection = info_summary_cluster_sk_theta$selection, 
                                               mean_rel = info_summary_cluster_sk_theta$rel, 
                                               sd_rel = 0), 
                                    
                                    data.frame(num_item = info_summary_smart_sk_theta$smart_name, 
                                               mean_info = info_summary_smart_sk_theta$info_test, 
                                               sd_info = 0, 
                                               selection = info_summary_smart_sk_theta$selection, 
                                               mean_rel = info_summary_smart_sk_theta$rel, 
                                               sd_rel = 0))
ggplot(data_sk_random_theta_unique[!data_sk_random_theta_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(data_sk_random_theta_unique[!data_sk_random_theta_unique$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = data_sk_random_theta_unique[data_sk_random_theta_unique$num_item %in% "all", 
                                      "mean_info"])


# reliability solo prima selezione ---- 
ggplot(data_sk_random_theta_unique[!data_sk_random_theta_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(data_sk_random_theta_unique[!data_sk_random_theta_unique$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = data_sk_random_theta_unique[data_sk_random_theta_unique$num_item %in% "all", 
                                      "mean_rel"])


# stesso garfico ma con le reliability 

ggplot(all_data_sk_theta[!all_data_sk_theta$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1) +
  geom_point(aes(shape=selection), size =2)+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels = unique(all_data_sk_theta[!all_data_sk_theta$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk_theta[all_data_sk_theta$num_item %in% "all", 
                                            "mean_rel"]) + ylim(0,1)

grid.arrange(ggplot(all_data_sk_theta[!all_data_sk_theta$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_info, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "top") + 
               scale_x_discrete(labels =  unique(all_data_sk_theta[!all_data_sk_theta$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_sk_theta[all_data_sk_theta$num_item %in% "all", 
                                                         "mean_info"]), 
             ggplot(all_data_sk_theta[!all_data_sk_theta$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_rel, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1) +
               geom_point(aes(shape=selection), size =2)+
               geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "none") + 
               scale_x_discrete(labels = unique(all_data_sk_theta[!all_data_sk_theta$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_sk_theta[all_data_sk_theta$num_item %in% "all", 
                                                         "mean_rel"]))

# confronto TIF dati sk_theta -----

temp_cluster_sk_theta<- NULL
cluster_data_sk_theta <- NULL

temp_smart_sk_theta <- NULL
smart_data_sk_theta <- NULL


temp_range_sk_theta <- NULL
range_data_sk_theta  <- NULL

temp_range_new_sk_theta <- NULL
range_new_data_sk_theta  <- NULL


for (i in 1:length(info_out_smart_sk_theta)) {
  temp_cluster_sk_theta<- data.frame(theta = info_out_cluster_sk_theta[[i]]$theta,
                                     info = info_out_cluster_sk_theta[[i]]$test_info_curve, 
                                     num_item = names(info_out_cluster_sk_theta)[[i]], 
                                     sel = "cluster")
  cluster_data_sk_theta <- rbind(temp_cluster_sk_theta, 
                                 cluster_data_sk_theta)
  
  temp_smart_sk_theta <- data.frame(theta = info_out_smart_sk_theta[[i]]$theta,
                                    info = info_out_smart_sk_theta[[i]]$test_info_curve, 
                                    num_item = names(info_out_smart_sk_theta)[[i]], 
                                    sel = "smart")
  smart_data_sk_theta <- rbind(temp_smart_sk_theta, 
                               smart_data_sk_theta)
  temp_range_sk_theta <- data.frame(theta = info_out_range_sk_theta[[i]]$theta,
                                    info = info_out_range_sk_theta[[i]]$test_info_curve, 
                                    num_item = names(info_out_range_sk_theta)[[i]], 
                                    sel = "guided")
  range_data_sk_theta  <- rbind(temp_range_sk_theta, 
                                range_data_sk_theta )
  temp_range_new_sk_theta <- data.frame(theta = info_out_range_new_sk_theta[[i]]$theta,
                                        info = info_out_range_new_sk_theta[[i]]$test_info_curve, 
                                        num_item = names(info_out_range_new_sk_theta)[[i]], 
                                        sel = "guidedNEW")
  range_new_data_sk_theta  <- rbind(temp_range_new_sk_theta, 
                                    range_new_data_sk_theta )
  
}

# questo prende tutte le TIF random
temp_random_sk_theta <- NULL
random_data_sk_theta <- NULL
for (i in 1:length(info_test_random_sk_theta)) {
  temp_random_sk_theta <- data.frame(theta = info_test_random_sk_theta[[i]]$theta,
                                     info = info_test_random_sk_theta[[i]]$test_info_curve, 
                                     num_item = paste0("number", 
                                                       nrow(info_test_random_sk_theta[[i]]$info_curves_item)), 
                                     sel = "random")
  random_data_sk_theta <- rbind(temp_random_sk_theta, 
                                random_data_sk_theta)
}

# questo seleziona sono una delle curve per ogni numerosità 

temp = NULL
new_random_sk_theta = NULL
for (i in 1:length(unique(random_data_sk_theta$num_item))) {
  temp = random_data_sk_theta[random_data_sk_theta$num_item %in% unique(random_data_sk_theta$num_item)[i], ]
  temp = temp[1:1000, ]
  new_random_sk_theta = rbind(new_random_sk_theta, temp)
}

# questo calcola la media delle informatività 
mean_random_sk_theta<- aggregate(info ~ theta + num_item, data = random_data_sk_theta, mean)
mean_random_sk_theta<- mean_random_sk_theta[, c("theta", "info", "num_item")]
mean_random_sk_theta$sel <- "random"
start_data_sk_theta <- data.frame(theta = IRT.informationCurves(m2pl_sk, 
                                                                theta = seq(-3, 3, 
                                                                            length = 1000))$theta,
                                  info = info_start_sk, 
                                  num_item = "all", 
                                  sel = "start")

data_info_sk_theta <- rbind(
  cluster_data_sk_theta,
  range_data_sk_theta , range_new_data_sk_theta , 
  smart_data_sk_theta , new_random_sk_theta #mean_random_sk_theta
)

data_info_sk_theta_all <- rbind(
  cluster_data_sk_theta,
  range_data_sk_theta , range_new_data_sk_theta , 
  smart_data_sk_theta , mean_random_sk_theta
)


plots_sk_theta <- list()
graph_start_sk <- data.frame(theta = IRT.informationCurves(m2pl_sk, 
                                                           theta = seq(-3,3,length = 1000))$theta, 
                             info = (IRT.informationCurves(m2pl_sk, 
                                                           theta = seq(-3,3,length = 1000))$test_info_curve), 
                             num_item = "all",
                             sel = "start")

for(i in 1:length(unique(data_info_sk_theta$num_item))) {
  
  plots_sk_theta[[i]] <-  ggplot(rbind(data_info_sk_theta[data_info_sk_theta$num_item %in% unique(data_info_sk_theta$num_item)[i], ], 
                                       graph_start_sk), 
                                 aes(x = theta, y = info, group = sel, 
                                     col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4) + 
    ggtitle(unique(data_info_sk_theta$num_item)[i]) +
    theme(legend.position = "none")
}


ggplot(rbind(data_info_sk_theta[data_info_sk_theta$num_item %in% "number10", ], 
             graph_start_sk), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4)

do.call(grid.arrange, plots_sk_theta)

# stessa cosa ma questa volta con la TIF media
for(i in 1:length(unique(data_info_sk_theta_all$num_item))) {
  
  plots_sk_theta[[i]] <-  ggplot(rbind(data_info_sk_theta_all[data_info_sk_theta_all$num_item %in% unique(data_info_sk_theta_all$num_item)[i], ], 
                                       graph_start_sk), 
                                 aes(x = theta, y = info, group = sel, 
                                     col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4) + 
    ggtitle(unique(data_info_sk_theta_all$num_item)[i]) +
    theme(legend.position = "none")
}


ggplot(rbind(data_info_sk_theta_all[data_info_sk_theta_all$num_item %in% "number10", ], 
             graph_start_sk), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4)

do.call(grid.arrange, plots_sk_theta)

# Costruisco un dataframe per il calcolo del bias per le stime di theta sk_theta ----

for (i in 1:length(model_fit_random_sk_theta)) {
  names(model_fit_random_sk_theta)[[i]] <- paste0("number", 
                                                  nrow(model_fit_random_sk_theta[[i]]$xsi))
}

temp <- NULL
random_theta <- NULL
list_temp <- NULL
theta_random_sk_theta <- list()

for (i in 1:length(unique(names(model_fit_random_sk_theta)))) {
  random_theta <- NULL
  temp <- model_fit_random_sk_theta[names(model_fit_random_sk_theta) == unique(names(model_fit_random_sk_theta))[i]]
  
  for (j in 1:length(temp)) {
    list_temp <- temp[[j]]$person$EAP
    random_theta <- data.frame(cbind(random_theta, list_temp))
    theta_random_sk_theta[[i]] <- random_theta
    names(theta_random_sk_theta)[[i]] <- unique(names(model_fit_random_sk_theta))[i]
  }
}

# # devo fare la media attraverso el colonne per oguno dei number item 
# devo fare la media attraverso el colonne per oguno dei number item
temp_theta_random_mean <- data.frame(matrix(ncol = length(unique(names(theta_random_sk_theta))),
                                       nrow = 1000))
for (i in 1:length(unique(names(theta_random_sk_theta)))) {
  temp_theta_random_mean[, i] <- c(rowMeans(theta_random_sk_theta[[i]]))
  colnames(temp_theta_random_mean)[i] <- unique(names(theta_random_sk_theta))[i]
}

random_all_theta = NULL
temp = NULL
# devo mettere tutti i theta in un unico dataframe, unirci i theta osservati e 
# i theta ottenuti su tutto il set di item (che sono sempre 1'000)
for (i in 1:length(names(theta_random_sk_theta))) {
  temp = data.frame(theta_random_sk_theta[names(theta_random_sk_theta) == unique(names(theta_random_sk_theta))[i]])
  
  for (j in 1:length(temp)) {
    random_mean_temp = data.frame(num_item = unique(names(theta_random_sk_theta))[i], theta_est = temp[,j])
    random_all_theta = rbind(random_all_theta, 
                             random_mean_temp)
  }
  
}
# devono esserci 10'000 osservazioni in per ogni numerosità di item

table(random_all_theta$num_item)

# creo il data set con l'info media per ogni combinazione di item

temp_theta_random_sk_theta = data.frame(matrix(ncol = 
                                                 length(unique(names(theta_random_sk_theta))), 
                                               nrow = 1000))
for (i in 1:length(theta_random_sk_theta)) {
  temp_theta_random_sk_theta[,i] = theta_random_sk_theta[[i]][,1]
  colnames(temp_theta_random_sk_theta)[i] <- unique(names(theta_random_sk_theta))[i]
}

random_mean = data.frame(selection = rep("random", nrow(temp_theta_random_sk_theta)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random_sk_theta_mean = cbind(sbj, random_mean, temp_theta_random_sk_theta)
theta_random_sk_theta_mean <- reshape(temp_theta_random_sk_theta_mean, 
                                 idvar = "sbj", 
                                 varying = list(3:(ncol(temp_theta_random_sk_theta_mean))), 
                                 v.names = "theta_est", 
                                 direction = "long", 
                                 times = (names(temp_theta_random_sk_theta_mean)[-c(1:2)]), 
                                 timevar = "num_item")
theta_random_sk_theta_mean$obs <- true_theta_sk
theta_random_sk_theta_mean$all <- theta_all_sk
head(theta_random_sk_theta_mean)

# siccome prima ho preso la prima selezione di item per ognuna delle
# numerosità, prendo la prima selezione per ognuno

random = data.frame(selection = rep("random", nrow(temp_theta_random_sk_theta)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random_sk_theta = cbind(sbj, random, temp_theta_random_sk_theta)
theta_random_sk_theta <- reshape(temp_theta_random_sk_theta, 
                                 idvar = "sbj", 
                                 varying = list(3:(ncol(temp_theta_random_sk_theta))), 
                                 v.names = "theta_est", 
                                 direction = "long", 
                                 times = (names(temp_theta_random_sk_theta)[-c(1:2)]), 
                                 timevar = "num_item")
theta_random_sk_theta$obs <- true_theta_sk
theta_random_sk_theta$all <- theta_all_sk
head(theta_random_sk_theta)

# crea il dataframe per tutte le altre strategie
for (i in 1:length(model_out_cluster_sk_theta)) {
  names(model_out_cluster_sk_theta)[[i]] <- paste0("number", 
                                                   nrow(model_out_cluster_sk_theta[[i]]$xsi))
  names(model_out_range_sk_theta)[[i]] <- paste0("number", 
                                                 nrow(model_out_range_sk_theta[[i]]$xsi))
  names(model_out_range_new_sk_theta)[[i]] <- paste0("number", 
                                                     nrow(model_out_range_new_sk_theta[[i]]$xsi))
  names(model_out_smart_sk_theta)[[i]] <- paste0("number", 
                                                 nrow(model_out_smart_sk_theta[[i]]$xsi))
}

temp_theta_cluster_sk_theta <- data.frame(matrix(ncol = length(unique(names(model_out_cluster_sk_theta))), 
                                                 nrow = 1000))

temp_theta_range_sk_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_sk_theta))), 
                                               nrow = 1000))
temp_theta_range_new_sk_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_new_sk_theta))), 
                                                   nrow = 1000))
temp_theta_smart_sk_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_sk_theta))), 
                                               nrow = 1000))

for (i in 1:length(unique(names(model_out_cluster_sk_theta)))) {
  temp_theta_cluster_sk_theta[, i] <- model_out_cluster_sk_theta[[i]]$person$EAP
  colnames(temp_theta_cluster_sk_theta)[i] <- unique(names(model_out_cluster_sk_theta))[i]
  
  temp_theta_range_sk_theta[, i] <- model_out_range_sk_theta[[i]]$person$EAP
  colnames(temp_theta_range_sk_theta)[i] <- unique(names(model_out_range_sk_theta))[i]
  
  temp_theta_range_new_sk_theta[, i] <- model_out_range_new_sk_theta[[i]]$person$EAP
  colnames(temp_theta_range_new_sk_theta)[i] <- unique(names(model_out_range_new_sk_theta))[i]
  
  temp_theta_smart_sk_theta[, i] <- model_out_smart_sk_theta[[i]]$person$EAP
  colnames(temp_theta_smart_sk_theta)[i] <- unique(names(model_out_smart_sk_theta))[i]
  
}


sbj <- data.frame(sbj =1:1000)

cluster = data.frame(selection = rep("cluster", nrow(sbj)))
range = data.frame(selection = rep("range", nrow(sbj)))
range_new = data.frame(selection = rep("rangeNew", nrow(sbj)))
smart = data.frame(selection = rep("smart", nrow(sbj)))


temp_theta_cluster_sk_theta <- cbind(sbj, cluster,  temp_theta_cluster_sk_theta)

theta_cluster_sk_theta <- reshape(temp_theta_cluster_sk_theta, 
                                  idvar = "sbj", 
                                  varying = list(3:(ncol(temp_theta_cluster_sk_theta))), 
                                  v.names = "theta_est", 
                                  direction = "long", 
                                  times = (names(temp_theta_cluster_sk_theta)[-c(1:2)]), 
                                  timevar = "num_item")
theta_cluster_sk_theta$obs <- true_theta_sk
theta_cluster_sk_theta$all <- theta_all_sk
head(theta_cluster_sk_theta)

temp_theta_range_sk_theta <- cbind(sbj, range,  temp_theta_range_sk_theta)

theta_range_sk_theta <- reshape(temp_theta_range_sk_theta, 
                                idvar = "sbj", 
                                varying = list(3:(ncol(temp_theta_range_sk_theta))), 
                                v.names = "theta_est", 
                                direction = "long", 
                                times = (names(temp_theta_range_sk_theta)[-c(1:2)]), 
                                timevar = "num_item")
theta_range_sk_theta$obs <- true_theta_sk
theta_range_sk_theta$all <- theta_all_sk
head(theta_range_sk_theta)


temp_theta_range_new_sk_theta <- cbind(sbj, range_new,  temp_theta_range_new_sk_theta)

theta_range_new_sk_theta <- reshape(temp_theta_range_new_sk_theta, 
                                    idvar = "sbj", 
                                    varying = list(3:(ncol(temp_theta_range_new_sk_theta))), 
                                    v.names = "theta_est", 
                                    direction = "long", 
                                    times = (names(temp_theta_range_new_sk_theta)[-c(1:2)]), 
                                    timevar = "num_item")
theta_range_new_sk_theta$obs <- true_theta_sk
theta_range_new_sk_theta$all <- theta_all_sk
head(theta_range_new_sk_theta)

temp_theta_smart_sk_theta <- cbind(sbj, smart,  temp_theta_smart_sk_theta)

theta_smart_sk_theta <- reshape(temp_theta_smart_sk_theta, 
                                idvar = "sbj", 
                                varying = list(3:(ncol(temp_theta_smart_sk_theta))), 
                                v.names = "theta_est", 
                                direction = "long", 
                                times = (names(temp_theta_smart_sk_theta)[-c(1:2)]), 
                                timevar = "num_item")
theta_smart_sk_theta$obs <- true_theta_sk
theta_smart_sk_theta$all <- theta_all_sk
head(theta_smart_sk_theta)

# random obs ----
theta_random_sk_theta$bias_obs = with(theta_random_sk_theta, 
                                      theta_est - obs)
theta_random_sk_theta$bias_obs_sq = theta_random_sk_theta$bias_obs^2 
theta_random_sk_theta$bias_obs_abs = with(theta_random_sk_theta, 
                                          abs(theta_est - obs))
theta_random_sk_theta$bias_all = with(theta_random_sk_theta, 
                                      theta_est - all)
theta_random_sk_theta$bias_all_sq = theta_random_sk_theta$bias_all^2 
theta_random_sk_theta$bias_all_abs = with(theta_random_sk_theta, 
                                          abs(theta_est - all))

random_bias_obs_theta = aggregate(bias_obs ~num_item, 
                                  data = theta_random_sk_theta, 
                                  mean)
random_bias_obs_theta$selection = "random"
random_bias_obs_theta$type = "bias_obs"
random_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_random_sk_theta, 
                                  mean)
random_rmse_obs_theta$selection = "random"
random_rmse_obs_theta$type = "rmse_obs"

random_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_random_sk_theta, 
                                      mean)
random_bias_obs_theta_abs$selection = "random"
random_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_random_sk_theta$group <- ifelse(theta_random_sk_theta$obs  <= cut_val[1, "start"], 
                                      group_name[1], 
                                      ifelse(theta_random_sk_theta$obs > cut_val[1, "start"] & theta_random_sk_theta$obs <= cut_val[1, "end"], 
                                             group_name[2], 
                                             ifelse(theta_random_sk_theta$obs > cut_val[2, "start"] & theta_random_sk_theta$obs <= cut_val[2, "end"], 
                                                    group_name[3], 
                                                    ifelse(theta_random_sk_theta$obs > cut_val[3, "start"] & theta_random_sk_theta$obs <= cut_val[3, "end"], 
                                                           group_name[4], 
                                                           ifelse(theta_random_sk_theta$obs > cut_val[4, "start"] & theta_random_sk_theta$obs <= cut_val[4, "end"], 
                                                                  group_name[5], 
                                                                  ifelse(theta_random_sk_theta$obs > cut_val[4, "end"], 
                                                                         group_name[6], "error")
                                                           )))))
random_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                    data = theta_random_sk_theta, 
                                    mean)

random_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                        data = theta_random_sk_theta, 
                                        mean)

random_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                        data = theta_random_sk_theta, 
                                        mean)

random_bias_group_theta$selection = "random"
random_bias_abs_group_theta$selection = "random"
random_rmse_obs_group_theta$selection = "random"

# random all 

theta_random_sk_theta$bias_all = with(theta_random_sk_theta, 
                                      theta_est - all)
theta_random_sk_theta$bias_all_sq = theta_random_sk_theta$bias_all^2 
theta_random_sk_theta$bias_all_abs = with(theta_random_sk_theta, 
                                          abs(theta_est - all))
theta_random_sk_theta$bias_all = with(theta_random_sk_theta, 
                                      theta_est - all)
theta_random_sk_theta$bias_all_sq = theta_random_sk_theta$bias_all^2 
theta_random_sk_theta$bias_all_abs = with(theta_random_sk_theta, 
                                          abs(theta_est - all))

random_bias_all_theta = aggregate(bias_all ~num_item, data = theta_random_sk_theta, 
                                  mean)
random_bias_all_theta$selection = "random"
random_bias_all_theta$type = "bias_all"
random_rmse_all_theta = aggregate(bias_all_sq ~num_item, 
                                  data = theta_random_sk_theta, 
                                  mean)
random_rmse_all_theta$selection = "random"
random_rmse_all_theta$type = "rmse_all"
random_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_random_sk_theta, 
                                      mean)
random_bias_all_theta_abs$selection = "random"
random_bias_all_theta_abs$type = "bias_all_abs"

# random MEAN obs ----
# il bias non lo devo calcolare già sulle stime di theta medie, ma sulle stime 
# pulite di ogni possibile giro di ripescaggio e DOPO fare la media 
random_all_theta$obs = true_theta_sk
random_all_theta$all = theta_all_sk
random_all_theta$bias_obs = with(random_all_theta, 
                                 theta_est - obs)
random_all_theta$bias_obs_sq = random_all_theta$bias_obs^2 
random_all_theta$bias_obs_abs = with(random_all_theta, 
                                     abs(theta_est - obs))
random_all_theta$bias_all = with(random_all_theta, 
                                 theta_est - all)
random_all_theta$bias_all_sq = random_all_theta$bias_all^2 
random_all_theta$bias_all_abs = with(random_all_theta, 
                                     abs(theta_est - all))

random_bias_obs_theta_mean = aggregate(bias_obs ~num_item, 
                                       data = random_all_theta, 
                                       mean)
random_bias_obs_theta_mean$selection = "random"
random_bias_obs_theta_mean$type = "bias_obs"
random_rmse_obs_theta_mean = aggregate(bias_obs_sq ~num_item, 
                                       data = random_all_theta, 
                                       mean)
random_rmse_obs_theta_mean$selection = "random"
random_rmse_obs_theta_mean$type = "rmse_obs"

random_bias_obs_theta_abs_mean = aggregate(bias_obs_abs ~num_item, data = random_all_theta, 
                                           mean)
random_bias_obs_theta_abs_mean$selection = "random"
random_bias_obs_theta_abs_mean$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


random_all_theta$group <- ifelse(random_all_theta$obs  <= cut_val[1, "start"], 
                                 group_name[1], 
                                 ifelse(random_all_theta$obs > cut_val[1, "start"] & random_all_theta$obs <= cut_val[1, "end"], 
                                        group_name[2], 
                                        ifelse(random_all_theta$obs > cut_val[2, "start"] & random_all_theta$obs <= cut_val[2, "end"], 
                                               group_name[3], 
                                               ifelse(random_all_theta$obs > cut_val[3, "start"] & random_all_theta$obs <= cut_val[3, "end"], 
                                                      group_name[4], 
                                                      ifelse(random_all_theta$obs > cut_val[4, "start"] & random_all_theta$obs <= cut_val[4, "end"], 
                                                             group_name[5], 
                                                             ifelse(random_all_theta$obs > cut_val[4, "end"], 
                                                                    group_name[6], "error")
                                                      )))))
random_bias_group_theta_mean = aggregate(bias_obs ~num_item + group, 
                                         data = random_all_theta, 
                                         mean)

random_bias_abs_group_theta_mean = aggregate(bias_obs_abs ~num_item + group, 
                                             data = random_all_theta, 
                                             mean)

random_rmse_obs_group_theta_mean = aggregate(bias_obs_sq ~num_item + group, 
                                             data = random_all_theta, 
                                             mean)

random_bias_group_theta_mean$selection = "random"
random_bias_abs_group_theta_mean$selection = "random"
random_rmse_obs_group_theta_mean$selection = "random"

# random all MEAN ----

random_all_theta$bias_all = with(random_all_theta, 
                                 theta_est - all)
random_all_theta$bias_all_sq = random_all_theta$bias_all^2 
random_all_theta$bias_all_abs = with(random_all_theta, 
                                     abs(theta_est - all))
random_all_theta$bias_all = with(random_all_theta, 
                                 theta_est - all)
random_all_theta$bias_all_sq = random_all_theta$bias_all^2 
random_all_theta$bias_all_abs = with(random_all_theta, 
                                     abs(theta_est - all))

random_bias_all_theta_mean = aggregate(bias_all ~num_item, data = random_all_theta, 
                                       mean)
random_bias_all_theta_mean$selection = "random"
random_bias_all_theta_mean$type = "bias_all"
random_rmse_all_theta_mean = aggregate(bias_all_sq ~num_item, data = random_all_theta, 
                                       mean)
random_rmse_all_theta_mean$selection = "random"
random_rmse_all_theta_mean$type = "rmse_all"
random_bias_all_theta_abs_mean = aggregate(bias_all_abs ~num_item, 
                                           data = random_all_theta, 
                                           mean)
random_bias_all_theta_abs_mean$selection = "random"
random_bias_all_theta_abs_mean$type = "bias_all_abs"

# cluster obs ----
theta_cluster_sk_theta$bias_obs = with(theta_cluster_sk_theta, 
                                       theta_est - obs)
theta_cluster_sk_theta$bias_obs_sq = theta_cluster_sk_theta$bias_obs^2 
theta_cluster_sk_theta$bias_obs_abs = with(theta_cluster_sk_theta, 
                                           abs(theta_est - obs))
theta_cluster_sk_theta$bias_all = with(theta_cluster_sk_theta, 
                                       theta_est - all)
theta_cluster_sk_theta$bias_all_sq = theta_cluster_sk_theta$bias_all^2 
theta_cluster_sk_theta$bias_all_abs = with(theta_cluster_sk_theta, 
                                           abs(theta_est - all))

cluster_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_cluster_sk_theta, 
                                   mean)
cluster_bias_obs_theta$selection = "cluster"
cluster_bias_obs_theta$type = "bias_obs"
cluster_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, 
                                   data = theta_cluster_sk_theta, 
                                   mean)
cluster_rmse_obs_theta$selection = "cluster"
cluster_rmse_obs_theta$type = "rmse_obs"
cluster_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_cluster_sk_theta, 
                                       mean)
cluster_bias_obs_theta_abs$selection = "cluster"
cluster_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_cluster_sk_theta$group <- ifelse(theta_cluster_sk_theta$obs  <= cut_val[1, "start"], 
                                       group_name[1], 
                                       ifelse(theta_cluster_sk_theta$obs > cut_val[1, "start"] & theta_cluster_sk_theta$obs <= cut_val[1, "end"], 
                                              group_name[2], 
                                              ifelse(theta_cluster_sk_theta$obs > cut_val[2, "start"] & theta_cluster_sk_theta$obs <= cut_val[2, "end"], 
                                                     group_name[3], 
                                                     ifelse(theta_cluster_sk_theta$obs > cut_val[3, "start"] & theta_cluster_sk_theta$obs <= cut_val[3, "end"], 
                                                            group_name[4], 
                                                            ifelse(theta_cluster_sk_theta$obs > cut_val[4, "start"] & theta_cluster_sk_theta$obs <= cut_val[4, "end"], 
                                                                   group_name[5], 
                                                                   ifelse(theta_cluster_sk_theta$obs > cut_val[4, "end"], 
                                                                          group_name[6], "error")
                                                            )))))
cluster_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                     data = theta_cluster_sk_theta, 
                                     mean)

cluster_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                         data = theta_cluster_sk_theta, 
                                         mean)

cluster_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                         data = theta_cluster_sk_theta, 
                                         mean)

cluster_bias_group_theta$selection = "cluster"
cluster_bias_abs_group_theta$selection = "cluster"
cluster_rmse_obs_group_theta$selection = "cluster"

# cluster all 

theta_cluster_sk_theta$bias_all = with(theta_cluster_sk_theta, 
                                       theta_est - all)
theta_cluster_sk_theta$bias_all_sq = theta_cluster_sk_theta$bias_all^2 
theta_cluster_sk_theta$bias_all_abs = with(theta_cluster_sk_theta, 
                                           abs(theta_est - all))
theta_cluster_sk_theta$bias_all = with(theta_cluster_sk_theta, 
                                       theta_est - all)
theta_cluster_sk_theta$bias_all_sq = theta_cluster_sk_theta$bias_all^2 
theta_cluster_sk_theta$bias_all_abs = with(theta_cluster_sk_theta, 
                                           abs(theta_est - all))

cluster_bias_all_theta = aggregate(bias_all ~num_item, data = theta_cluster_sk_theta, 
                                   mean)
cluster_bias_all_theta$selection = "cluster"
cluster_bias_all_theta$type = "bias_all"
cluster_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_cluster_sk_theta, 
                                   mean)
cluster_rmse_all_theta$selection = "cluster"
cluster_rmse_all_theta$type = "rmse_all"
cluster_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_cluster_sk_theta, 
                                       mean)
cluster_bias_all_theta_abs$selection = "cluster"
cluster_bias_all_theta_abs$type = "bias_all_abs"

# range obs ----
theta_range_sk_theta$bias_obs = with(theta_range_sk_theta, 
                                     theta_est - obs)
theta_range_sk_theta$bias_obs_sq = theta_range_sk_theta$bias_obs^2 
theta_range_sk_theta$bias_obs_abs = with(theta_range_sk_theta, 
                                         abs(theta_est - obs))
theta_range_sk_theta$bias_all = with(theta_range_sk_theta, 
                                     theta_est - all)
theta_range_sk_theta$bias_all_sq = theta_range_sk_theta$bias_all^2 
theta_range_sk_theta$bias_all_abs = with(theta_range_sk_theta, 
                                         abs(theta_est - all))

range_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_range_sk_theta, 
                                 mean)
range_bias_obs_theta$selection = "range"
range_bias_obs_theta$type = "bias_obs"
range_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_range_sk_theta, 
                                 mean)
range_rmse_obs_theta$selection = "range"
range_rmse_obs_theta$type = "rmse_obs"
range_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_range_sk_theta, 
                                     mean)
range_bias_obs_theta_abs$selection = "range"
range_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_range_sk_theta$group <- ifelse(theta_range_sk_theta$obs  <= cut_val[1, "start"], 
                                     group_name[1], 
                                     ifelse(theta_range_sk_theta$obs > cut_val[1, "start"] & theta_range_sk_theta$obs <= cut_val[1, "end"], 
                                            group_name[2], 
                                            ifelse(theta_range_sk_theta$obs > cut_val[2, "start"] & theta_range_sk_theta$obs <= cut_val[2, "end"], 
                                                   group_name[3], 
                                                   ifelse(theta_range_sk_theta$obs > cut_val[3, "start"] & theta_range_sk_theta$obs <= cut_val[3, "end"], 
                                                          group_name[4], 
                                                          ifelse(theta_range_sk_theta$obs > cut_val[4, "start"] & theta_range_sk_theta$obs <= cut_val[4, "end"], 
                                                                 group_name[5], 
                                                                 ifelse(theta_range_sk_theta$obs > cut_val[4, "end"], 
                                                                        group_name[6], "error")
                                                          )))))
range_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                   data = theta_range_sk_theta, 
                                   mean)

range_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                       data = theta_range_sk_theta, 
                                       mean)

range_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                       data = theta_range_sk_theta, 
                                       mean)

range_bias_group_theta$selection = "range"
range_bias_abs_group_theta$selection = "range"
range_rmse_obs_group_theta$selection = "range"

# range all 

theta_range_sk_theta$bias_all = with(theta_range_sk_theta, 
                                     theta_est - all)
theta_range_sk_theta$bias_all_sq = theta_range_sk_theta$bias_all^2 
theta_range_sk_theta$bias_all_abs = with(theta_range_sk_theta, 
                                         abs(theta_est - all))
theta_range_sk_theta$bias_all = with(theta_range_sk_theta, 
                                     theta_est - all)
theta_range_sk_theta$bias_all_sq = theta_range_sk_theta$bias_all^2 
theta_range_sk_theta$bias_all_abs = with(theta_range_sk_theta, 
                                         abs(theta_est - all))

range_bias_all_theta = aggregate(bias_all ~num_item, data = theta_range_sk_theta, 
                                 mean)
range_bias_all_theta$selection = "range"
range_bias_all_theta$type = "bias_all"
range_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_range_sk_theta, 
                                 mean)
range_rmse_all_theta$selection = "range"
range_rmse_all_theta$type = "rmse_all"
range_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_range_sk_theta, 
                                     mean)
range_bias_all_theta_abs$selection = "range"
range_bias_all_theta_abs$type = "bias_all_abs"

# range_new obs ----
theta_range_new_sk_theta$bias_obs = with(theta_range_new_sk_theta, 
                                         theta_est - obs)
theta_range_new_sk_theta$bias_obs_sq = theta_range_new_sk_theta$bias_obs^2 
theta_range_new_sk_theta$bias_obs_abs = with(theta_range_new_sk_theta, 
                                             abs(theta_est - obs))
theta_range_new_sk_theta$bias_all = with(theta_range_new_sk_theta, 
                                         theta_est - all)
theta_range_new_sk_theta$bias_all_sq = theta_range_new_sk_theta$bias_all^2 
theta_range_new_sk_theta$bias_all_abs = with(theta_range_new_sk_theta, 
                                             abs(theta_est - all))

range_new_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_range_new_sk_theta, 
                                     mean)
range_new_bias_obs_theta$selection = "range_new"
range_new_bias_obs_theta$type = "bias_obs"
range_new_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_range_new_sk_theta, 
                                     mean)
range_new_rmse_obs_theta$selection = "range_new"
range_new_rmse_obs_theta$type = "rmse_obs"
range_new_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_range_new_sk_theta, 
                                         mean)
range_new_bias_obs_theta_abs$selection = "range_new"
range_new_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_range_new_sk_theta$group <- ifelse(theta_range_new_sk_theta$obs  <= cut_val[1, "start"], 
                                         group_name[1], 
                                         ifelse(theta_range_new_sk_theta$obs > cut_val[1, "start"] & theta_range_new_sk_theta$obs <= cut_val[1, "end"], 
                                                group_name[2], 
                                                ifelse(theta_range_new_sk_theta$obs > cut_val[2, "start"] & theta_range_new_sk_theta$obs <= cut_val[2, "end"], 
                                                       group_name[3], 
                                                       ifelse(theta_range_new_sk_theta$obs > cut_val[3, "start"] & theta_range_new_sk_theta$obs <= cut_val[3, "end"], 
                                                              group_name[4], 
                                                              ifelse(theta_range_new_sk_theta$obs > cut_val[4, "start"] & theta_range_new_sk_theta$obs <= cut_val[4, "end"], 
                                                                     group_name[5], 
                                                                     ifelse(theta_range_new_sk_theta$obs > cut_val[4, "end"], 
                                                                            group_name[6], "error")
                                                              )))))
range_new_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                       data = theta_range_new_sk_theta, 
                                       mean)

range_new_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                           data = theta_range_new_sk_theta, 
                                           mean)

range_new_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                           data = theta_range_new_sk_theta, 
                                           mean)

range_new_bias_group_theta$selection = "range_new"
range_new_bias_abs_group_theta$selection = "range_new"
range_new_rmse_obs_group_theta$selection = "range_new"

# range_new all 

theta_range_new_sk_theta$bias_all = with(theta_range_new_sk_theta, 
                                         theta_est - all)
theta_range_new_sk_theta$bias_all_sq = theta_range_new_sk_theta$bias_all^2 
theta_range_new_sk_theta$bias_all_abs = with(theta_range_new_sk_theta, 
                                             abs(theta_est - all))
theta_range_new_sk_theta$bias_all = with(theta_range_new_sk_theta, 
                                         theta_est - all)
theta_range_new_sk_theta$bias_all_sq = theta_range_new_sk_theta$bias_all^2 
theta_range_new_sk_theta$bias_all_abs = with(theta_range_new_sk_theta, 
                                             abs(theta_est - all))

range_new_bias_all_theta = aggregate(bias_all ~num_item, data = theta_range_new_sk_theta, 
                                     mean)
range_new_bias_all_theta$selection = "range_new"
range_new_bias_all_theta$type = "bias_all"
range_new_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_range_new_sk_theta, 
                                     mean)
range_new_rmse_all_theta$selection = "range_new"
range_new_rmse_all_theta$type = "rmse_all"
range_new_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_range_new_sk_theta, 
                                         mean)
range_new_bias_all_theta_abs$selection = "range_new"
range_new_bias_all_theta_abs$type = "bias_all_abs"

# smart obs ----
theta_smart_sk_theta$bias_obs = with(theta_smart_sk_theta, 
                                     theta_est - obs)
theta_smart_sk_theta$bias_obs_sq = theta_smart_sk_theta$bias_obs^2 
theta_smart_sk_theta$bias_obs_abs = with(theta_smart_sk_theta, 
                                         abs(theta_est - obs))
theta_smart_sk_theta$bias_all = with(theta_smart_sk_theta, 
                                     theta_est - all)
theta_smart_sk_theta$bias_all_sq = theta_smart_sk_theta$bias_all^2 
theta_smart_sk_theta$bias_all_abs = with(theta_smart_sk_theta, 
                                         abs(theta_est - all))

smart_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_smart_sk_theta, 
                                 mean)
smart_bias_obs_theta$selection = "smart"
smart_bias_obs_theta$type = "bias_obs"
smart_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_smart_sk_theta, 
                                 mean)
smart_rmse_obs_theta$selection = "smart"
smart_rmse_obs_theta$type = "rmse_obs"
smart_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_smart_sk_theta, 
                                     mean)
smart_bias_obs_theta_abs$selection = "smart"
smart_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_smart_sk_theta$group <- ifelse(theta_smart_sk_theta$obs  <= cut_val[1, "start"], 
                                     group_name[1], 
                                     ifelse(theta_smart_sk_theta$obs > cut_val[1, "start"] & theta_smart_sk_theta$obs <= cut_val[1, "end"], 
                                            group_name[2], 
                                            ifelse(theta_smart_sk_theta$obs > cut_val[2, "start"] & theta_smart_sk_theta$obs <= cut_val[2, "end"], 
                                                   group_name[3], 
                                                   ifelse(theta_smart_sk_theta$obs > cut_val[3, "start"] & theta_smart_sk_theta$obs <= cut_val[3, "end"], 
                                                          group_name[4], 
                                                          ifelse(theta_smart_sk_theta$obs > cut_val[4, "start"] & theta_smart_sk_theta$obs <= cut_val[4, "end"], 
                                                                 group_name[5], 
                                                                 ifelse(theta_smart_sk_theta$obs > cut_val[4, "end"], 
                                                                        group_name[6], "error")
                                                          )))))
smart_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                   data = theta_smart_sk_theta, 
                                   mean)

smart_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                       data = theta_smart_sk_theta, 
                                       mean)

smart_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                       data = theta_smart_sk_theta, 
                                       mean)

smart_bias_group_theta$selection = "smart"
smart_bias_abs_group_theta$selection = "smart"
smart_rmse_obs_group_theta$selection = "smart"

# smart all 

theta_smart_sk_theta$bias_all = with(theta_smart_sk_theta, 
                                     theta_est - all)
theta_smart_sk_theta$bias_all_sq = theta_smart_sk_theta$bias_all^2 
theta_smart_sk_theta$bias_all_abs = with(theta_smart_sk_theta, 
                                         abs(theta_est - all))
theta_smart_sk_theta$bias_all = with(theta_smart_sk_theta, 
                                     theta_est - all)
theta_smart_sk_theta$bias_all_sq = theta_smart_sk_theta$bias_all^2 
theta_smart_sk_theta$bias_all_abs = with(theta_smart_sk_theta, 
                                         abs(theta_est - all))

smart_bias_all_theta = aggregate(bias_all ~num_item, data = theta_smart_sk_theta, 
                                 mean)
smart_bias_all_theta$selection = "smart"
smart_bias_all_theta$type = "bias_all"
smart_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_smart_sk_theta, 
                                 mean)
smart_rmse_all_theta$selection = "smart"
smart_rmse_all_theta$type = "rmse_all"
smart_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_smart_sk_theta, 
                                     mean)
smart_bias_all_theta_abs$selection = "smart"
smart_bias_all_theta_abs$type = "bias_all_abs"


# ora unisco i bias con i bias e gli rmse con gli rmse 
random_bias_obs_theta$selection = "random"
random_bias_obs_theta$type = "bias_obs"

sk_bias_obs_theta = rbind(random_bias_obs_theta, 
                          cluster_bias_obs_theta, 
                          range_new_bias_obs_theta, 
                          range_bias_obs_theta, 
                          smart_bias_obs_theta)

ggplot(sk_bias_obs_theta, 
       aes(x = num_item, y = bias_obs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2) + ylim(-1, 1)

# random mean
sk_bias_obs_theta_mean = rbind(random_bias_obs_theta_mean, 
                          cluster_bias_obs_theta, 
                          range_new_bias_obs_theta, 
                          range_bias_obs_theta, 
                          smart_bias_obs_theta)

ggplot(sk_bias_obs_theta_mean, 
       aes(x = num_item, y = bias_obs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2) + ylim(-1, 1)


sk_bias_obs_abs_theta = rbind(random_bias_obs_theta_abs, 
                              cluster_bias_obs_theta_abs, 
                              range_new_bias_obs_theta_abs, 
                              range_bias_obs_theta_abs, 
                              smart_bias_obs_theta_abs)

ggplot(sk_bias_obs_abs_theta, 
       aes(x = num_item, y = bias_obs_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean
sk_bias_obs_abs_theta_mean = rbind(random_bias_obs_theta_abs_mean, 
                              cluster_bias_obs_theta_abs, 
                              range_new_bias_obs_theta_abs, 
                              range_bias_obs_theta_abs, 
                              smart_bias_obs_theta_abs)

ggplot(sk_bias_obs_abs_theta_mean, 
       aes(x = num_item, y = bias_obs_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)


# bias all estimates

sk_bias_all_theta = rbind(random_bias_all_theta, 
                          cluster_bias_all_theta, 
                          range_new_bias_all_theta, 
                          range_bias_all_theta, 
                          smart_bias_all_theta)

ggplot(sk_bias_all_theta, 
       aes(x = num_item, y = bias_all, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

sk_bias_all_abs_theta = rbind(random_bias_all_theta_abs, 
                              cluster_bias_all_theta_abs, 
                              range_new_bias_all_theta_abs, 
                              range_bias_all_theta_abs, 
                              smart_bias_all_theta_abs)

ggplot(sk_bias_all_abs_theta, 
       aes(x = num_item, y = bias_all_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean

sk_bias_all_abs_theta_mean = rbind(random_bias_all_theta_abs_mean, 
                              cluster_bias_all_theta_abs, 
                              range_new_bias_all_theta_abs, 
                              range_bias_all_theta_abs, 
                              smart_bias_all_theta_abs)

ggplot(sk_bias_all_abs_theta_mean, 
       aes(x = num_item, y = bias_all_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)


# rmse obs 

sk_rmse_obs_theta = rbind(random_rmse_obs_theta, 
                          cluster_rmse_obs_theta, 
                          range_new_rmse_obs_theta, 
                          range_rmse_obs_theta, 
                          smart_rmse_obs_theta)
sk_rmse_obs_theta$rmse = sqrt(sk_rmse_obs_theta$bias_obs_sq)

ggplot(sk_rmse_obs_theta, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean 
sk_rmse_obs_theta_mean = rbind(random_rmse_obs_theta_mean, 
                          cluster_rmse_obs_theta, 
                          range_new_rmse_obs_theta, 
                          range_rmse_obs_theta, 
                          smart_rmse_obs_theta)
sk_rmse_obs_theta_mean$rmse = sqrt(sk_rmse_obs_theta_mean$bias_obs_sq)

ggplot(sk_rmse_obs_theta_mean, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)


# rmse all estimates 

sk_rmse_all_theta = rbind(random_rmse_all_theta, 
                          cluster_rmse_all_theta, 
                          range_new_rmse_all_theta, 
                          range_rmse_all_theta, 
                          smart_rmse_all_theta)
sk_rmse_all_theta$rmse = sqrt(sk_rmse_all_theta$bias_all_sq)

ggplot(sk_rmse_all_theta, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean

sk_rmse_all_theta_mean = rbind(random_rmse_all_theta_mean, 
                          cluster_rmse_all_theta, 
                          range_new_rmse_all_theta, 
                          range_rmse_all_theta, 
                          smart_rmse_all_theta)
sk_rmse_all_theta_mean$rmse = sqrt(sk_rmse_all_theta_mean$bias_all_sq)

ggplot(sk_rmse_all_theta_mean, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)
# gruppi latenti rmse 

sk_rmse_groups_theta = rbind(random_rmse_obs_group_theta, 
                             smart_rmse_obs_group_theta, 
                             cluster_rmse_obs_group_theta, 
                             range_rmse_obs_group_theta, 
                             range_new_rmse_obs_group_theta)
sk_rmse_groups_theta$rmse = sqrt(sk_rmse_groups_theta$bias_obs_sq)

ggplot(sk_rmse_groups_theta, 
       aes(x=group, y = rmse, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# random mean 

sk_rmse_groups_theta_mean = rbind(random_rmse_obs_group_theta_mean, 
                             smart_rmse_obs_group_theta, 
                             cluster_rmse_obs_group_theta, 
                             range_rmse_obs_group_theta, 
                             range_new_rmse_obs_group_theta)
sk_rmse_groups_theta_mean$rmse = sqrt(sk_rmse_groups_theta_mean$bias_obs_sq)

ggplot(sk_rmse_groups_theta_mean, 
       aes(x=group, y = rmse, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

sk_bias_abs_groups_theta = rbind(random_bias_abs_group_theta, 
                                 smart_bias_abs_group_theta, 
                                 cluster_bias_abs_group_theta, 
                                 range_bias_abs_group_theta, 
                                 range_new_bias_abs_group_theta)

ggplot(sk_bias_abs_groups_theta, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# random mean

sk_bias_abs_groups_theta_mean = rbind(random_bias_abs_group_theta_mean, 
                                 smart_bias_abs_group_theta, 
                                 cluster_bias_abs_group_theta, 
                                 range_bias_abs_group_theta, 
                                 range_new_bias_abs_group_theta)

ggplot(sk_bias_abs_groups_theta_mean, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)


sk_bias_groups_theta = rbind(random_bias_group_theta, 
                             smart_bias_group_theta, 
                             cluster_bias_group_theta, 
                             range_bias_group_theta, 
                             range_new_bias_group_theta)

ggplot(sk_bias_groups_theta, 
       aes(x=group, y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# random mean 
sk_bias_groups_theta_mean = rbind(random_bias_group_theta_mean, 
                             smart_bias_group_theta, 
                             cluster_bias_group_theta, 
                             range_bias_group_theta, 
                             range_new_bias_group_theta)

ggplot(sk_bias_groups_theta_mean, 
       aes(x=group, y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# ATTENZIONE ----------
rm(list= ls()[!(ls() %in% c("m2pl_sk",
                            "all_data_sk_theta", 
                            "data_sk_random_theta_unique", 
                            "temp_random_sk_theta", 
                            "new_random_sk_theta", 
                            "mean_random_sk_theta", 
                            "data_info_sk_theta", 
                            "data_info_sk_theta_all", 
                            "theta_random_sk_theta", 
                            "theta_random_sk_theta_mean", 
                            "random_all_theta", 
                            "sk_bias_obs_theta", 
                            "sk_bias_obs_theta_mean", 
                            "sk_bias_obs_abs_theta", 
                            "sk_bias_obs_abs_theta_mean", 
                            "sk_bias_all_theta", 
                            "sk_bias_all_abs_theta_mean", 
                            "sk_bias_all_abs_theta", 
                            "sk_bias_all_abs_theta_mean", 
                            "sk_rmse_all_theta", 
                            "sk_rmse_all_theta_mean", 
                            "sk_rmse_obs_theta", 
                            "sk_rmse_obs_theta_mean", 
                            "sk_rmse_groups_theta", 
                            "sk_rmse_groups_theta_mean", 
                            "sk_bias_groups_theta", 
                            "sk_bias_groups_theta_mean", 
                            "sk_bias_abs_groups_theta", 
                            "sk_bias_abs_groups_theta_mean"))])

# Dati uniform -------

rm(list = ls())
load("UNIrandom.RData")
load("UNIsmart.RData")
load("UNIguided.RData")
load("UNIguidedNEW.RData")
load("UNIcluster.RData")

# # Stessa cosa item con theta uni_theta PARAMETRI FISSI -----
data_random_uni_theta_summary$selection <- "random"
all_data_uni_theta <- rbind(data_random_uni_theta_summary,
                            data.frame(num_item = info_summary_range_uni_theta$range_name,
                                       mean_info = info_summary_range_uni_theta$info_test,
                                       sd_info = 0,
                                       selection = info_summary_range_uni_theta$selection,
                                       mean_rel = info_summary_range_uni_theta$rel,
                                       sd_rel = 0),
                            data.frame(num_item = info_summary_range_new_uni_theta$range_new_name,
                                       mean_info = info_summary_range_new_uni_theta$info_test,
                                       sd_info = 0,
                                       selection = info_summary_range_new_uni_theta$selection,
                                       mean_rel = info_summary_range_new_uni_theta$rel,
                                       sd_rel = 0),
                            data.frame(num_item = info_summary_cluster_uni_theta$cluster_name,
                                       mean_info = info_summary_cluster_uni_theta$info_test,
                                       sd_info = 0,
                                       selection = info_summary_cluster_uni_theta$selection,
                                       mean_rel = info_summary_cluster_uni_theta$rel,
                                       sd_rel = 0),
                            
                            data.frame(num_item = info_summary_smart_uni_theta$smart_name,
                                       mean_info = info_summary_smart_uni_theta$info_test,
                                       sd_info = 0,
                                       selection = info_summary_smart_uni_theta$selection,
                                       mean_rel = info_summary_smart_uni_theta$rel,
                                       sd_rel = 0))
all_data_uni_theta$item_temp <- gsub("number", "", all_data_uni_theta$num_item)
all_data_uni_theta$item_temp <- gsub("all", 0, all_data_uni_theta$item_temp)
all_data_uni_theta$item_temp <- as.integer(all_data_uni_theta$item_temp)
all_data_uni_theta <- all_data_uni_theta[order(all_data_uni_theta$item_temp), ]
all_data_uni_theta$selection <- gsub("uni_theta", '', all_data_uni_theta$selection)
ggplot(all_data_uni_theta[!all_data_uni_theta$num_item %in%"all", ],
       aes(x=as.factor(item_temp), y=mean_info,
           group=selection, color=selection)) +
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info),
                width=.2,
                position=position_dodge(0.05)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels =  unique(all_data_uni_theta[!all_data_uni_theta$num_item %in%"all", "num_item"])) +
  geom_hline(yintercept = all_data_uni_theta[all_data_uni_theta$num_item %in% "all",
                                             "mean_info"])

# grafico info unii prendendo solo la prima combo di item da numerosità di item

uni_random_theta = NULL 
temp = NULL
for(i in 1:length(unique(data_random_uni_theta$num_item))) {
  temp = data_random_uni_theta[data_random_uni_theta$num_item %in% unique(data_random_uni_theta$num_item)[i], ]
  temp = temp[1, ]
  uni_random_theta = rbind(uni_random_theta, temp)
}
uni_random_theta$selection = "random_theta" 
# unisco al data set 
data_uni_random_theta_unique = rbind(data.frame(num_item = uni_random_theta$num_item,
                                                mean_info = uni_random_theta$info_total,
                                                sd_info = 0,
                                                selection = uni_random_theta$selection,
                                                mean_rel = uni_random_theta$rel,
                                                sd_rel = 0), 
                                     data.frame(num_item = info_summary_range_uni_theta$range_name, 
                                                mean_info = info_summary_range_uni_theta$info_test, 
                                                sd_info = 0, 
                                                selection = info_summary_range_uni_theta$selection, 
                                                mean_rel = info_summary_range_uni_theta$rel, 
                                                sd_rel = 0), 
                                     data.frame(num_item = info_summary_range_new_uni_theta$range_new_name, 
                                                mean_info = info_summary_range_new_uni_theta$info_test, 
                                                sd_info = 0, 
                                                selection = info_summary_range_new_uni_theta$selection, 
                                                mean_rel = info_summary_range_new_uni_theta$rel, 
                                                sd_rel = 0), 
                                     data.frame(num_item = info_summary_cluster_uni_theta$cluster_name, 
                                                mean_info = info_summary_cluster_uni_theta$info_test, 
                                                sd_info = 0, 
                                                selection = info_summary_cluster_uni_theta$selection, 
                                                mean_rel = info_summary_cluster_uni_theta$rel, 
                                                sd_rel = 0), 
                                     
                                     data.frame(num_item = info_summary_smart_uni_theta$smart_name, 
                                                mean_info = info_summary_smart_uni_theta$info_test, 
                                                sd_info = 0, 
                                                selection = info_summary_smart_uni_theta$selection, 
                                                mean_rel = info_summary_smart_uni_theta$rel, 
                                                sd_rel = 0))
ggplot(data_uni_random_theta_unique[!data_uni_random_theta_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(data_uni_random_theta_unique[!data_uni_random_theta_unique$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = data_uni_random_theta_unique[data_uni_random_theta_unique$num_item %in% "all", 
                                                       "mean_info"])


# reliability solo prima selezione ---- 
ggplot(data_uni_random_theta_unique[!data_uni_random_theta_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(data_uni_random_theta_unique[!data_uni_random_theta_unique$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = data_uni_random_theta_unique[data_uni_random_theta_unique$num_item %in% "all", 
                                                       "mean_rel"])


# stesso garfico ma con le reliability 

ggplot(all_data_uni_theta[!all_data_uni_theta$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1) +
  geom_point(aes(shape=selection), size =2)+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels = unique(all_data_uni_theta[!all_data_uni_theta$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_uni_theta[all_data_uni_theta$num_item %in% "all", 
                                             "mean_rel"]) + ylim(0,1)

grid.arrange(ggplot(all_data_uni_theta[!all_data_uni_theta$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_info, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "top") + 
               scale_x_discrete(labels =  unique(all_data_uni_theta[!all_data_uni_theta$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_uni_theta[all_data_uni_theta$num_item %in% "all", 
                                                          "mean_info"]), 
             ggplot(all_data_uni_theta[!all_data_uni_theta$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_rel, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1) +
               geom_point(aes(shape=selection), size =2)+
               geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "none") + 
               scale_x_discrete(labels = unique(all_data_uni_theta[!all_data_uni_theta$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_uni_theta[all_data_uni_theta$num_item %in% "all", 
                                                          "mean_rel"]))

# confronto TIF dati uni_theta -----

temp_cluster_uni_theta<- NULL
cluster_data_uni_theta <- NULL

temp_smart_uni_theta <- NULL
smart_data_uni_theta <- NULL


temp_range_uni_theta <- NULL
range_data_uni_theta  <- NULL

temp_range_new_uni_theta <- NULL
range_new_data_uni_theta  <- NULL


for (i in 1:length(info_out_smart_uni_theta)) {
  temp_cluster_uni_theta<- data.frame(theta = info_out_cluster_uni_theta[[i]]$theta,
                                      info = info_out_cluster_uni_theta[[i]]$test_info_curve, 
                                      num_item = names(info_out_cluster_uni_theta)[[i]], 
                                      sel = "cluster")
  cluster_data_uni_theta <- rbind(temp_cluster_uni_theta, 
                                  cluster_data_uni_theta)
  
  temp_smart_uni_theta <- data.frame(theta = info_out_smart_uni_theta[[i]]$theta,
                                     info = info_out_smart_uni_theta[[i]]$test_info_curve, 
                                     num_item = names(info_out_smart_uni_theta)[[i]], 
                                     sel = "smart")
  smart_data_uni_theta <- rbind(temp_smart_uni_theta, 
                                smart_data_uni_theta)
  temp_range_uni_theta <- data.frame(theta = info_out_range_uni_theta[[i]]$theta,
                                     info = info_out_range_uni_theta[[i]]$test_info_curve, 
                                     num_item = names(info_out_range_uni_theta)[[i]], 
                                     sel = "guided")
  range_data_uni_theta  <- rbind(temp_range_uni_theta, 
                                 range_data_uni_theta )
  temp_range_new_uni_theta <- data.frame(theta = info_out_range_new_uni_theta[[i]]$theta,
                                         info = info_out_range_new_uni_theta[[i]]$test_info_curve, 
                                         num_item = names(info_out_range_new_uni_theta)[[i]], 
                                         sel = "guidedNEW")
  range_new_data_uni_theta  <- rbind(temp_range_new_uni_theta, 
                                     range_new_data_uni_theta )
  
}

# questo prende tutte le TIF random
temp_random_uni_theta <- NULL
random_data_uni_theta <- NULL
for (i in 1:length(info_test_random_uni_theta)) {
  temp_random_uni_theta <- data.frame(theta = info_test_random_uni_theta[[i]]$theta,
                                      info = info_test_random_uni_theta[[i]]$test_info_curve, 
                                      num_item = paste0("number", 
                                                        nrow(info_test_random_uni_theta[[i]]$info_curves_item)), 
                                      sel = "random")
  random_data_uni_theta <- rbind(temp_random_uni_theta, 
                                 random_data_uni_theta)
}

# questo seleziona sono una delle curve per ogni numerosità 

temp = NULL
new_random_uni_theta = NULL
for (i in 1:length(unique(random_data_uni_theta$num_item))) {
  temp = random_data_uni_theta[random_data_uni_theta$num_item %in% unique(random_data_uni_theta$num_item)[i], ]
  temp = temp[1:1000, ]
  new_random_uni_theta = rbind(new_random_uni_theta, temp)
}

# questo calcola la media delle informatività 
mean_random_uni_theta<- aggregate(info ~ theta + num_item, data = random_data_uni_theta, mean)
mean_random_uni_theta<- mean_random_uni_theta[, c("theta", "info", "num_item")]
mean_random_uni_theta$sel <- "random"
start_data_uni_theta <- data.frame(theta = IRT.informationCurves(m2pl_uni, 
                                                                 theta = seq(-3, 3, 
                                                                             length = 1000))$theta,
                                   info = info_start_uni, 
                                   num_item = "all", 
                                   sel = "start")

data_info_uni_theta <- rbind(
  cluster_data_uni_theta,
  range_data_uni_theta , range_new_data_uni_theta , 
  smart_data_uni_theta , new_random_uni_theta #mean_random_uni_theta
)

data_info_uni_theta_all <- rbind(
  cluster_data_uni_theta,
  range_data_uni_theta , range_new_data_uni_theta , 
  smart_data_uni_theta , mean_random_uni_theta
)


plots_uni_theta <- list()
graph_start_uni <- data.frame(theta = IRT.informationCurves(m2pl_uni, 
                                                            theta = seq(-3,3,length = 1000))$theta, 
                              info = (IRT.informationCurves(m2pl_uni, 
                                                            theta = seq(-3,3,length = 1000))$test_info_curve), 
                              num_item = "all",
                              sel = "start")

for(i in 1:length(unique(data_info_uni_theta$num_item))) {
  
  plots_uni_theta[[i]] <-  ggplot(rbind(data_info_uni_theta[data_info_uni_theta$num_item %in% unique(data_info_uni_theta$num_item)[i], ], 
                                        graph_start_uni), 
                                  aes(x = theta, y = info, group = sel, 
                                      col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4) + 
    ggtitle(unique(data_info_uni_theta$num_item)[i]) +
    theme(legend.position = "none")
}


ggplot(rbind(data_info_uni_theta[data_info_uni_theta$num_item %in% "number10", ], 
             graph_start_uni), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4)

do.call(grid.arrange, plots_uni_theta)

# stessa cosa ma questa volta con la TIF media
for(i in 1:length(unique(data_info_uni_theta_all$num_item))) {
  
  plots_uni_theta[[i]] <-  ggplot(rbind(data_info_uni_theta_all[data_info_uni_theta_all$num_item %in% unique(data_info_uni_theta_all$num_item)[i], ], 
                                        graph_start_uni), 
                                  aes(x = theta, y = info, group = sel, 
                                      col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4) + 
    ggtitle(unique(data_info_uni_theta_all$num_item)[i]) +
    theme(legend.position = "none")
}


ggplot(rbind(data_info_uni_theta_all[data_info_uni_theta_all$num_item %in% "number10", ], 
             graph_start_uni), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4)

do.call(grid.arrange, plots_uni_theta)

# Costruisco un dataframe per il calcolo del bias per le stime di theta uni_theta ----

for (i in 1:length(model_fit_random_uni_theta)) {
  names(model_fit_random_uni_theta)[[i]] <- paste0("number", 
                                                   nrow(model_fit_random_uni_theta[[i]]$xsi))
}

temp <- NULL
random_theta <- NULL
list_temp <- NULL
theta_random_uni_theta <- list()

for (i in 1:length(unique(names(model_fit_random_uni_theta)))) {
  random_theta <- NULL
  temp <- model_fit_random_uni_theta[names(model_fit_random_uni_theta) == unique(names(model_fit_random_uni_theta))[i]]
  
  for (j in 1:length(temp)) {
    list_temp <- temp[[j]]$person$EAP
    random_theta <- data.frame(cbind(random_theta, list_temp))
    theta_random_uni_theta[[i]] <- random_theta
    names(theta_random_uni_theta)[[i]] <- unique(names(model_fit_random_uni_theta))[i]
  }
}

# # devo fare la media attraverso el colonne per oguno dei number item 
# devo fare la media attraverso el colonne per oguno dei number item
temp_theta_random_mean <- data.frame(matrix(ncol = length(unique(names(theta_random_uni_theta))),
                                            nrow = 1000))
for (i in 1:length(unique(names(theta_random_uni_theta)))) {
  temp_theta_random_mean[, i] <- c(rowMeans(theta_random_uni_theta[[i]]))
  colnames(temp_theta_random_mean)[i] <- unique(names(theta_random_uni_theta))[i]
}

random_all_theta = NULL
temp = NULL
# devo mettere tutti i theta in un unico dataframe, unirci i theta osservati e 
# i theta ottenuti su tutto il set di item (che sono sempre 1'000)
for (i in 1:length(names(theta_random_uni_theta))) {
  temp = data.frame(theta_random_uni_theta[names(theta_random_uni_theta) == unique(names(theta_random_uni_theta))[i]])
  
  for (j in 1:length(temp)) {
    random_mean_temp = data.frame(num_item = unique(names(theta_random_uni_theta))[i], theta_est = temp[,j])
    random_all_theta = rbind(random_all_theta, 
                             random_mean_temp)
  }
  
}
# devono esserci 10'000 osservazioni in per ogni numerosità di item

table(random_all_theta$num_item)

# creo il data set con l'info media per ogni combinazione di item

temp_theta_random_uni_theta = data.frame(matrix(ncol = 
                                                  length(unique(names(theta_random_uni_theta))), 
                                                nrow = 1000))
for (i in 1:length(theta_random_uni_theta)) {
  temp_theta_random_uni_theta[,i] = theta_random_uni_theta[[i]][,1]
  colnames(temp_theta_random_uni_theta)[i] <- unique(names(theta_random_uni_theta))[i]
}

random_mean = data.frame(selection = rep("random", nrow(temp_theta_random_uni_theta)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random_uni_theta_mean = cbind(sbj, random_mean, temp_theta_random_uni_theta)
theta_random_uni_theta_mean <- reshape(temp_theta_random_uni_theta_mean, 
                                       idvar = "sbj", 
                                       varying = list(3:(ncol(temp_theta_random_uni_theta_mean))), 
                                       v.names = "theta_est", 
                                       direction = "long", 
                                       times = (names(temp_theta_random_uni_theta_mean)[-c(1:2)]), 
                                       timevar = "num_item")
theta_random_uni_theta_mean$obs <- true_theta_uni
theta_random_uni_theta_mean$all <- theta_all_uni
head(theta_random_uni_theta_mean)

# siccome prima ho preso la prima selezione di item per ognuna delle
# numerosità, prendo la prima selezione per ognuno

random = data.frame(selection = rep("random", nrow(temp_theta_random_uni_theta)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random_uni_theta = cbind(sbj, random, temp_theta_random_uni_theta)
theta_random_uni_theta <- reshape(temp_theta_random_uni_theta, 
                                  idvar = "sbj", 
                                  varying = list(3:(ncol(temp_theta_random_uni_theta))), 
                                  v.names = "theta_est", 
                                  direction = "long", 
                                  times = (names(temp_theta_random_uni_theta)[-c(1:2)]), 
                                  timevar = "num_item")
theta_random_uni_theta$obs <- true_theta_uni
theta_random_uni_theta$all <- theta_all_uni
head(theta_random_uni_theta)

# crea il dataframe per tutte le altre strategie
for (i in 1:length(model_out_cluster_uni_theta)) {
  names(model_out_cluster_uni_theta)[[i]] <- paste0("number", 
                                                    nrow(model_out_cluster_uni_theta[[i]]$xsi))
  names(model_out_range_uni_theta)[[i]] <- paste0("number", 
                                                  nrow(model_out_range_uni_theta[[i]]$xsi))
  names(model_out_range_new_uni_theta)[[i]] <- paste0("number", 
                                                      nrow(model_out_range_new_uni_theta[[i]]$xsi))
  names(model_out_smart_uni_theta)[[i]] <- paste0("number", 
                                                  nrow(model_out_smart_uni_theta[[i]]$xsi))
}

temp_theta_cluster_uni_theta <- data.frame(matrix(ncol = length(unique(names(model_out_cluster_uni_theta))), 
                                                  nrow = 1000))

temp_theta_range_uni_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_uni_theta))), 
                                                nrow = 1000))
temp_theta_range_new_uni_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_new_uni_theta))), 
                                                    nrow = 1000))
temp_theta_smart_uni_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_uni_theta))), 
                                                nrow = 1000))

for (i in 1:length(unique(names(model_out_cluster_uni_theta)))) {
  temp_theta_cluster_uni_theta[, i] <- model_out_cluster_uni_theta[[i]]$person$EAP
  colnames(temp_theta_cluster_uni_theta)[i] <- unique(names(model_out_cluster_uni_theta))[i]
  
  temp_theta_range_uni_theta[, i] <- model_out_range_uni_theta[[i]]$person$EAP
  colnames(temp_theta_range_uni_theta)[i] <- unique(names(model_out_range_uni_theta))[i]
  
  temp_theta_range_new_uni_theta[, i] <- model_out_range_new_uni_theta[[i]]$person$EAP
  colnames(temp_theta_range_new_uni_theta)[i] <- unique(names(model_out_range_new_uni_theta))[i]
  
  temp_theta_smart_uni_theta[, i] <- model_out_smart_uni_theta[[i]]$person$EAP
  colnames(temp_theta_smart_uni_theta)[i] <- unique(names(model_out_smart_uni_theta))[i]
  
}


sbj <- data.frame(sbj =1:1000)

cluster = data.frame(selection = rep("cluster", nrow(sbj)))
range = data.frame(selection = rep("range", nrow(sbj)))
range_new = data.frame(selection = rep("rangeNew", nrow(sbj)))
smart = data.frame(selection = rep("smart", nrow(sbj)))


temp_theta_cluster_uni_theta <- cbind(sbj, cluster,  temp_theta_cluster_uni_theta)

theta_cluster_uni_theta <- reshape(temp_theta_cluster_uni_theta, 
                                   idvar = "sbj", 
                                   varying = list(3:(ncol(temp_theta_cluster_uni_theta))), 
                                   v.names = "theta_est", 
                                   direction = "long", 
                                   times = (names(temp_theta_cluster_uni_theta)[-c(1:2)]), 
                                   timevar = "num_item")
theta_cluster_uni_theta$obs <- true_theta_uni
theta_cluster_uni_theta$all <- theta_all_uni
head(theta_cluster_uni_theta)

temp_theta_range_uni_theta <- cbind(sbj, range,  temp_theta_range_uni_theta)

theta_range_uni_theta <- reshape(temp_theta_range_uni_theta, 
                                 idvar = "sbj", 
                                 varying = list(3:(ncol(temp_theta_range_uni_theta))), 
                                 v.names = "theta_est", 
                                 direction = "long", 
                                 times = (names(temp_theta_range_uni_theta)[-c(1:2)]), 
                                 timevar = "num_item")
theta_range_uni_theta$obs <- true_theta_uni
theta_range_uni_theta$all <- theta_all_uni
head(theta_range_uni_theta)


temp_theta_range_new_uni_theta <- cbind(sbj, range_new,  temp_theta_range_new_uni_theta)

theta_range_new_uni_theta <- reshape(temp_theta_range_new_uni_theta, 
                                     idvar = "sbj", 
                                     varying = list(3:(ncol(temp_theta_range_new_uni_theta))), 
                                     v.names = "theta_est", 
                                     direction = "long", 
                                     times = (names(temp_theta_range_new_uni_theta)[-c(1:2)]), 
                                     timevar = "num_item")
theta_range_new_uni_theta$obs <- true_theta_uni
theta_range_new_uni_theta$all <- theta_all_uni
head(theta_range_new_uni_theta)

temp_theta_smart_uni_theta <- cbind(sbj, smart,  temp_theta_smart_uni_theta)

theta_smart_uni_theta <- reshape(temp_theta_smart_uni_theta, 
                                 idvar = "sbj", 
                                 varying = list(3:(ncol(temp_theta_smart_uni_theta))), 
                                 v.names = "theta_est", 
                                 direction = "long", 
                                 times = (names(temp_theta_smart_uni_theta)[-c(1:2)]), 
                                 timevar = "num_item")
theta_smart_uni_theta$obs <- true_theta_uni
theta_smart_uni_theta$all <- theta_all_uni
head(theta_smart_uni_theta)

# random obs ----
theta_random_uni_theta$bias_obs = with(theta_random_uni_theta, 
                                       theta_est - obs)
theta_random_uni_theta$bias_obs_sq = theta_random_uni_theta$bias_obs^2 
theta_random_uni_theta$bias_obs_abs = with(theta_random_uni_theta, 
                                           abs(theta_est - obs))
theta_random_uni_theta$bias_all = with(theta_random_uni_theta, 
                                       theta_est - all)
theta_random_uni_theta$bias_all_sq = theta_random_uni_theta$bias_all^2 
theta_random_uni_theta$bias_all_abs = with(theta_random_uni_theta, 
                                           abs(theta_est - all))

random_bias_obs_theta = aggregate(bias_obs ~num_item, 
                                  data = theta_random_uni_theta, 
                                  mean)
random_bias_obs_theta$selection = "random"
random_bias_obs_theta$type = "bias_obs"
random_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_random_uni_theta, 
                                  mean)
random_rmse_obs_theta$selection = "random"
random_rmse_obs_theta$type = "rmse_obs"

random_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_random_uni_theta, 
                                      mean)
random_bias_obs_theta_abs$selection = "random"
random_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_random_uni_theta$group <- ifelse(theta_random_uni_theta$obs  <= cut_val[1, "start"], 
                                       group_name[1], 
                                       ifelse(theta_random_uni_theta$obs > cut_val[1, "start"] & theta_random_uni_theta$obs <= cut_val[1, "end"], 
                                              group_name[2], 
                                              ifelse(theta_random_uni_theta$obs > cut_val[2, "start"] & theta_random_uni_theta$obs <= cut_val[2, "end"], 
                                                     group_name[3], 
                                                     ifelse(theta_random_uni_theta$obs > cut_val[3, "start"] & theta_random_uni_theta$obs <= cut_val[3, "end"], 
                                                            group_name[4], 
                                                            ifelse(theta_random_uni_theta$obs > cut_val[4, "start"] & theta_random_uni_theta$obs <= cut_val[4, "end"], 
                                                                   group_name[5], 
                                                                   ifelse(theta_random_uni_theta$obs > cut_val[4, "end"], 
                                                                          group_name[6], "error")
                                                            )))))
random_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                    data = theta_random_uni_theta, 
                                    mean)

random_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                        data = theta_random_uni_theta, 
                                        mean)

random_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                        data = theta_random_uni_theta, 
                                        mean)

random_bias_group_theta$selection = "random"
random_bias_abs_group_theta$selection = "random"
random_rmse_obs_group_theta$selection = "random"

# random all 

theta_random_uni_theta$bias_all = with(theta_random_uni_theta, 
                                       theta_est - all)
theta_random_uni_theta$bias_all_sq = theta_random_uni_theta$bias_all^2 
theta_random_uni_theta$bias_all_abs = with(theta_random_uni_theta, 
                                           abs(theta_est - all))
theta_random_uni_theta$bias_all = with(theta_random_uni_theta, 
                                       theta_est - all)
theta_random_uni_theta$bias_all_sq = theta_random_uni_theta$bias_all^2 
theta_random_uni_theta$bias_all_abs = with(theta_random_uni_theta, 
                                           abs(theta_est - all))

random_bias_all_theta = aggregate(bias_all ~num_item, data = theta_random_uni_theta, 
                                  mean)
random_bias_all_theta$selection = "random"
random_bias_all_theta$type = "bias_all"
random_rmse_all_theta = aggregate(bias_all_sq ~num_item, 
                                  data = theta_random_uni_theta, 
                                  mean)
random_rmse_all_theta$selection = "random"
random_rmse_all_theta$type = "rmse_all"
random_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_random_uni_theta, 
                                      mean)
random_bias_all_theta_abs$selection = "random"
random_bias_all_theta_abs$type = "bias_all_abs"

# random MEAN obs ----
# il bias non lo devo calcolare già sulle stime di theta medie, ma sulle stime 
# pulite di ogni possibile giro di ripescaggio e DOPO fare la media 
random_all_theta$obs = true_theta_uni
random_all_theta$all = theta_all_uni
random_all_theta$bias_obs = with(random_all_theta, 
                                 theta_est - obs)
random_all_theta$bias_obs_sq = random_all_theta$bias_obs^2 
random_all_theta$bias_obs_abs = with(random_all_theta, 
                                     abs(theta_est - obs))
random_all_theta$bias_all = with(random_all_theta, 
                                 theta_est - all)
random_all_theta$bias_all_sq = random_all_theta$bias_all^2 
random_all_theta$bias_all_abs = with(random_all_theta, 
                                     abs(theta_est - all))

random_bias_obs_theta_mean = aggregate(bias_obs ~num_item, 
                                       data = random_all_theta, 
                                       mean)
random_bias_obs_theta_mean$selection = "random"
random_bias_obs_theta_mean$type = "bias_obs"
random_rmse_obs_theta_mean = aggregate(bias_obs_sq ~num_item, 
                                       data = random_all_theta, 
                                       mean)
random_rmse_obs_theta_mean$selection = "random"
random_rmse_obs_theta_mean$type = "rmse_obs"

random_bias_obs_theta_abs_mean = aggregate(bias_obs_abs ~num_item, data = random_all_theta, 
                                           mean)
random_bias_obs_theta_abs_mean$selection = "random"
random_bias_obs_theta_abs_mean$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


random_all_theta$group <- ifelse(random_all_theta$obs  <= cut_val[1, "start"], 
                                 group_name[1], 
                                 ifelse(random_all_theta$obs > cut_val[1, "start"] & random_all_theta$obs <= cut_val[1, "end"], 
                                        group_name[2], 
                                        ifelse(random_all_theta$obs > cut_val[2, "start"] & random_all_theta$obs <= cut_val[2, "end"], 
                                               group_name[3], 
                                               ifelse(random_all_theta$obs > cut_val[3, "start"] & random_all_theta$obs <= cut_val[3, "end"], 
                                                      group_name[4], 
                                                      ifelse(random_all_theta$obs > cut_val[4, "start"] & random_all_theta$obs <= cut_val[4, "end"], 
                                                             group_name[5], 
                                                             ifelse(random_all_theta$obs > cut_val[4, "end"], 
                                                                    group_name[6], "error")
                                                      )))))
random_bias_group_theta_mean = aggregate(bias_obs ~num_item + group, 
                                         data = random_all_theta, 
                                         mean)

random_bias_abs_group_theta_mean = aggregate(bias_obs_abs ~num_item + group, 
                                             data = random_all_theta, 
                                             mean)

random_rmse_obs_group_theta_mean = aggregate(bias_obs_sq ~num_item + group, 
                                             data = random_all_theta, 
                                             mean)

random_bias_group_theta_mean$selection = "random"
random_bias_abs_group_theta_mean$selection = "random"
random_rmse_obs_group_theta_mean$selection = "random"

# random all MEAN ----

random_all_theta$bias_all = with(random_all_theta, 
                                 theta_est - all)
random_all_theta$bias_all_sq = random_all_theta$bias_all^2 
random_all_theta$bias_all_abs = with(random_all_theta, 
                                     abs(theta_est - all))
random_all_theta$bias_all = with(random_all_theta, 
                                 theta_est - all)
random_all_theta$bias_all_sq = random_all_theta$bias_all^2 
random_all_theta$bias_all_abs = with(random_all_theta, 
                                     abs(theta_est - all))

random_bias_all_theta_mean = aggregate(bias_all ~num_item, data = random_all_theta, 
                                       mean)
random_bias_all_theta_mean$selection = "random"
random_bias_all_theta_mean$type = "bias_all"
random_rmse_all_theta_mean = aggregate(bias_all_sq ~num_item, data = random_all_theta, 
                                       mean)
random_rmse_all_theta_mean$selection = "random"
random_rmse_all_theta_mean$type = "rmse_all"
random_bias_all_theta_abs_mean = aggregate(bias_all_abs ~num_item, 
                                           data = random_all_theta, 
                                           mean)
random_bias_all_theta_abs_mean$selection = "random"
random_bias_all_theta_abs_mean$type = "bias_all_abs"

# cluster obs ----
theta_cluster_uni_theta$bias_obs = with(theta_cluster_uni_theta, 
                                        theta_est - obs)
theta_cluster_uni_theta$bias_obs_sq = theta_cluster_uni_theta$bias_obs^2 
theta_cluster_uni_theta$bias_obs_abs = with(theta_cluster_uni_theta, 
                                            abs(theta_est - obs))
theta_cluster_uni_theta$bias_all = with(theta_cluster_uni_theta, 
                                        theta_est - all)
theta_cluster_uni_theta$bias_all_sq = theta_cluster_uni_theta$bias_all^2 
theta_cluster_uni_theta$bias_all_abs = with(theta_cluster_uni_theta, 
                                            abs(theta_est - all))

cluster_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_cluster_uni_theta, 
                                   mean)
cluster_bias_obs_theta$selection = "cluster"
cluster_bias_obs_theta$type = "bias_obs"
cluster_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, 
                                   data = theta_cluster_uni_theta, 
                                   mean)
cluster_rmse_obs_theta$selection = "cluster"
cluster_rmse_obs_theta$type = "rmse_obs"
cluster_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_cluster_uni_theta, 
                                       mean)
cluster_bias_obs_theta_abs$selection = "cluster"
cluster_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_cluster_uni_theta$group <- ifelse(theta_cluster_uni_theta$obs  <= cut_val[1, "start"], 
                                        group_name[1], 
                                        ifelse(theta_cluster_uni_theta$obs > cut_val[1, "start"] & theta_cluster_uni_theta$obs <= cut_val[1, "end"], 
                                               group_name[2], 
                                               ifelse(theta_cluster_uni_theta$obs > cut_val[2, "start"] & theta_cluster_uni_theta$obs <= cut_val[2, "end"], 
                                                      group_name[3], 
                                                      ifelse(theta_cluster_uni_theta$obs > cut_val[3, "start"] & theta_cluster_uni_theta$obs <= cut_val[3, "end"], 
                                                             group_name[4], 
                                                             ifelse(theta_cluster_uni_theta$obs > cut_val[4, "start"] & theta_cluster_uni_theta$obs <= cut_val[4, "end"], 
                                                                    group_name[5], 
                                                                    ifelse(theta_cluster_uni_theta$obs > cut_val[4, "end"], 
                                                                           group_name[6], "error")
                                                             )))))
cluster_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                     data = theta_cluster_uni_theta, 
                                     mean)

cluster_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                         data = theta_cluster_uni_theta, 
                                         mean)

cluster_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                         data = theta_cluster_uni_theta, 
                                         mean)

cluster_bias_group_theta$selection = "cluster"
cluster_bias_abs_group_theta$selection = "cluster"
cluster_rmse_obs_group_theta$selection = "cluster"

# cluster all 

theta_cluster_uni_theta$bias_all = with(theta_cluster_uni_theta, 
                                        theta_est - all)
theta_cluster_uni_theta$bias_all_sq = theta_cluster_uni_theta$bias_all^2 
theta_cluster_uni_theta$bias_all_abs = with(theta_cluster_uni_theta, 
                                            abs(theta_est - all))
theta_cluster_uni_theta$bias_all = with(theta_cluster_uni_theta, 
                                        theta_est - all)
theta_cluster_uni_theta$bias_all_sq = theta_cluster_uni_theta$bias_all^2 
theta_cluster_uni_theta$bias_all_abs = with(theta_cluster_uni_theta, 
                                            abs(theta_est - all))

cluster_bias_all_theta = aggregate(bias_all ~num_item, data = theta_cluster_uni_theta, 
                                   mean)
cluster_bias_all_theta$selection = "cluster"
cluster_bias_all_theta$type = "bias_all"
cluster_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_cluster_uni_theta, 
                                   mean)
cluster_rmse_all_theta$selection = "cluster"
cluster_rmse_all_theta$type = "rmse_all"
cluster_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_cluster_uni_theta, 
                                       mean)
cluster_bias_all_theta_abs$selection = "cluster"
cluster_bias_all_theta_abs$type = "bias_all_abs"

# range obs ----
theta_range_uni_theta$bias_obs = with(theta_range_uni_theta, 
                                      theta_est - obs)
theta_range_uni_theta$bias_obs_sq = theta_range_uni_theta$bias_obs^2 
theta_range_uni_theta$bias_obs_abs = with(theta_range_uni_theta, 
                                          abs(theta_est - obs))
theta_range_uni_theta$bias_all = with(theta_range_uni_theta, 
                                      theta_est - all)
theta_range_uni_theta$bias_all_sq = theta_range_uni_theta$bias_all^2 
theta_range_uni_theta$bias_all_abs = with(theta_range_uni_theta, 
                                          abs(theta_est - all))

range_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_range_uni_theta, 
                                 mean)
range_bias_obs_theta$selection = "range"
range_bias_obs_theta$type = "bias_obs"
range_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_range_uni_theta, 
                                 mean)
range_rmse_obs_theta$selection = "range"
range_rmse_obs_theta$type = "rmse_obs"
range_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_range_uni_theta, 
                                     mean)
range_bias_obs_theta_abs$selection = "range"
range_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_range_uni_theta$group <- ifelse(theta_range_uni_theta$obs  <= cut_val[1, "start"], 
                                      group_name[1], 
                                      ifelse(theta_range_uni_theta$obs > cut_val[1, "start"] & theta_range_uni_theta$obs <= cut_val[1, "end"], 
                                             group_name[2], 
                                             ifelse(theta_range_uni_theta$obs > cut_val[2, "start"] & theta_range_uni_theta$obs <= cut_val[2, "end"], 
                                                    group_name[3], 
                                                    ifelse(theta_range_uni_theta$obs > cut_val[3, "start"] & theta_range_uni_theta$obs <= cut_val[3, "end"], 
                                                           group_name[4], 
                                                           ifelse(theta_range_uni_theta$obs > cut_val[4, "start"] & theta_range_uni_theta$obs <= cut_val[4, "end"], 
                                                                  group_name[5], 
                                                                  ifelse(theta_range_uni_theta$obs > cut_val[4, "end"], 
                                                                         group_name[6], "error")
                                                           )))))
range_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                   data = theta_range_uni_theta, 
                                   mean)

range_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                       data = theta_range_uni_theta, 
                                       mean)

range_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                       data = theta_range_uni_theta, 
                                       mean)

range_bias_group_theta$selection = "range"
range_bias_abs_group_theta$selection = "range"
range_rmse_obs_group_theta$selection = "range"

# range all 

theta_range_uni_theta$bias_all = with(theta_range_uni_theta, 
                                      theta_est - all)
theta_range_uni_theta$bias_all_sq = theta_range_uni_theta$bias_all^2 
theta_range_uni_theta$bias_all_abs = with(theta_range_uni_theta, 
                                          abs(theta_est - all))
theta_range_uni_theta$bias_all = with(theta_range_uni_theta, 
                                      theta_est - all)
theta_range_uni_theta$bias_all_sq = theta_range_uni_theta$bias_all^2 
theta_range_uni_theta$bias_all_abs = with(theta_range_uni_theta, 
                                          abs(theta_est - all))

range_bias_all_theta = aggregate(bias_all ~num_item, data = theta_range_uni_theta, 
                                 mean)
range_bias_all_theta$selection = "range"
range_bias_all_theta$type = "bias_all"
range_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_range_uni_theta, 
                                 mean)
range_rmse_all_theta$selection = "range"
range_rmse_all_theta$type = "rmse_all"
range_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_range_uni_theta, 
                                     mean)
range_bias_all_theta_abs$selection = "range"
range_bias_all_theta_abs$type = "bias_all_abs"

# range_new obs ----
theta_range_new_uni_theta$bias_obs = with(theta_range_new_uni_theta, 
                                          theta_est - obs)
theta_range_new_uni_theta$bias_obs_sq = theta_range_new_uni_theta$bias_obs^2 
theta_range_new_uni_theta$bias_obs_abs = with(theta_range_new_uni_theta, 
                                              abs(theta_est - obs))
theta_range_new_uni_theta$bias_all = with(theta_range_new_uni_theta, 
                                          theta_est - all)
theta_range_new_uni_theta$bias_all_sq = theta_range_new_uni_theta$bias_all^2 
theta_range_new_uni_theta$bias_all_abs = with(theta_range_new_uni_theta, 
                                              abs(theta_est - all))

range_new_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_range_new_uni_theta, 
                                     mean)
range_new_bias_obs_theta$selection = "range_new"
range_new_bias_obs_theta$type = "bias_obs"
range_new_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_range_new_uni_theta, 
                                     mean)
range_new_rmse_obs_theta$selection = "range_new"
range_new_rmse_obs_theta$type = "rmse_obs"
range_new_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_range_new_uni_theta, 
                                         mean)
range_new_bias_obs_theta_abs$selection = "range_new"
range_new_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_range_new_uni_theta$group <- ifelse(theta_range_new_uni_theta$obs  <= cut_val[1, "start"], 
                                          group_name[1], 
                                          ifelse(theta_range_new_uni_theta$obs > cut_val[1, "start"] & theta_range_new_uni_theta$obs <= cut_val[1, "end"], 
                                                 group_name[2], 
                                                 ifelse(theta_range_new_uni_theta$obs > cut_val[2, "start"] & theta_range_new_uni_theta$obs <= cut_val[2, "end"], 
                                                        group_name[3], 
                                                        ifelse(theta_range_new_uni_theta$obs > cut_val[3, "start"] & theta_range_new_uni_theta$obs <= cut_val[3, "end"], 
                                                               group_name[4], 
                                                               ifelse(theta_range_new_uni_theta$obs > cut_val[4, "start"] & theta_range_new_uni_theta$obs <= cut_val[4, "end"], 
                                                                      group_name[5], 
                                                                      ifelse(theta_range_new_uni_theta$obs > cut_val[4, "end"], 
                                                                             group_name[6], "error")
                                                               )))))
range_new_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                       data = theta_range_new_uni_theta, 
                                       mean)

range_new_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                           data = theta_range_new_uni_theta, 
                                           mean)

range_new_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                           data = theta_range_new_uni_theta, 
                                           mean)

range_new_bias_group_theta$selection = "range_new"
range_new_bias_abs_group_theta$selection = "range_new"
range_new_rmse_obs_group_theta$selection = "range_new"

# range_new all 

theta_range_new_uni_theta$bias_all = with(theta_range_new_uni_theta, 
                                          theta_est - all)
theta_range_new_uni_theta$bias_all_sq = theta_range_new_uni_theta$bias_all^2 
theta_range_new_uni_theta$bias_all_abs = with(theta_range_new_uni_theta, 
                                              abs(theta_est - all))
theta_range_new_uni_theta$bias_all = with(theta_range_new_uni_theta, 
                                          theta_est - all)
theta_range_new_uni_theta$bias_all_sq = theta_range_new_uni_theta$bias_all^2 
theta_range_new_uni_theta$bias_all_abs = with(theta_range_new_uni_theta, 
                                              abs(theta_est - all))

range_new_bias_all_theta = aggregate(bias_all ~num_item, data = theta_range_new_uni_theta, 
                                     mean)
range_new_bias_all_theta$selection = "range_new"
range_new_bias_all_theta$type = "bias_all"
range_new_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_range_new_uni_theta, 
                                     mean)
range_new_rmse_all_theta$selection = "range_new"
range_new_rmse_all_theta$type = "rmse_all"
range_new_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_range_new_uni_theta, 
                                         mean)
range_new_bias_all_theta_abs$selection = "range_new"
range_new_bias_all_theta_abs$type = "bias_all_abs"

# smart obs ----
theta_smart_uni_theta$bias_obs = with(theta_smart_uni_theta, 
                                      theta_est - obs)
theta_smart_uni_theta$bias_obs_sq = theta_smart_uni_theta$bias_obs^2 
theta_smart_uni_theta$bias_obs_abs = with(theta_smart_uni_theta, 
                                          abs(theta_est - obs))
theta_smart_uni_theta$bias_all = with(theta_smart_uni_theta, 
                                      theta_est - all)
theta_smart_uni_theta$bias_all_sq = theta_smart_uni_theta$bias_all^2 
theta_smart_uni_theta$bias_all_abs = with(theta_smart_uni_theta, 
                                          abs(theta_est - all))

smart_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_smart_uni_theta, 
                                 mean)
smart_bias_obs_theta$selection = "smart"
smart_bias_obs_theta$type = "bias_obs"
smart_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_smart_uni_theta, 
                                 mean)
smart_rmse_obs_theta$selection = "smart"
smart_rmse_obs_theta$type = "rmse_obs"
smart_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_smart_uni_theta, 
                                     mean)
smart_bias_obs_theta_abs$selection = "smart"
smart_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_smart_uni_theta$group <- ifelse(theta_smart_uni_theta$obs  <= cut_val[1, "start"], 
                                      group_name[1], 
                                      ifelse(theta_smart_uni_theta$obs > cut_val[1, "start"] & theta_smart_uni_theta$obs <= cut_val[1, "end"], 
                                             group_name[2], 
                                             ifelse(theta_smart_uni_theta$obs > cut_val[2, "start"] & theta_smart_uni_theta$obs <= cut_val[2, "end"], 
                                                    group_name[3], 
                                                    ifelse(theta_smart_uni_theta$obs > cut_val[3, "start"] & theta_smart_uni_theta$obs <= cut_val[3, "end"], 
                                                           group_name[4], 
                                                           ifelse(theta_smart_uni_theta$obs > cut_val[4, "start"] & theta_smart_uni_theta$obs <= cut_val[4, "end"], 
                                                                  group_name[5], 
                                                                  ifelse(theta_smart_uni_theta$obs > cut_val[4, "end"], 
                                                                         group_name[6], "error")
                                                           )))))
smart_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                   data = theta_smart_uni_theta, 
                                   mean)

smart_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                       data = theta_smart_uni_theta, 
                                       mean)

smart_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                       data = theta_smart_uni_theta, 
                                       mean)

smart_bias_group_theta$selection = "smart"
smart_bias_abs_group_theta$selection = "smart"
smart_rmse_obs_group_theta$selection = "smart"

# smart all 

theta_smart_uni_theta$bias_all = with(theta_smart_uni_theta, 
                                      theta_est - all)
theta_smart_uni_theta$bias_all_sq = theta_smart_uni_theta$bias_all^2 
theta_smart_uni_theta$bias_all_abs = with(theta_smart_uni_theta, 
                                          abs(theta_est - all))
theta_smart_uni_theta$bias_all = with(theta_smart_uni_theta, 
                                      theta_est - all)
theta_smart_uni_theta$bias_all_sq = theta_smart_uni_theta$bias_all^2 
theta_smart_uni_theta$bias_all_abs = with(theta_smart_uni_theta, 
                                          abs(theta_est - all))

smart_bias_all_theta = aggregate(bias_all ~num_item, data = theta_smart_uni_theta, 
                                 mean)
smart_bias_all_theta$selection = "smart"
smart_bias_all_theta$type = "bias_all"
smart_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_smart_uni_theta, 
                                 mean)
smart_rmse_all_theta$selection = "smart"
smart_rmse_all_theta$type = "rmse_all"
smart_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_smart_uni_theta, 
                                     mean)
smart_bias_all_theta_abs$selection = "smart"
smart_bias_all_theta_abs$type = "bias_all_abs"


# ora unisco i bias con i bias e gli rmse con gli rmse 
random_bias_obs_theta$selection = "random"
random_bias_obs_theta$type = "bias_obs"

uni_bias_obs_theta = rbind(random_bias_obs_theta, 
                           cluster_bias_obs_theta, 
                           range_new_bias_obs_theta, 
                           range_bias_obs_theta, 
                           smart_bias_obs_theta)

ggplot(uni_bias_obs_theta, 
       aes(x = num_item, y = bias_obs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean
uni_bias_obs_theta_mean = rbind(random_bias_obs_theta_mean, 
                                cluster_bias_obs_theta, 
                                range_new_bias_obs_theta, 
                                range_bias_obs_theta, 
                                smart_bias_obs_theta)

ggplot(uni_bias_obs_theta_mean, 
       aes(x = num_item, y = bias_obs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2) 


uni_bias_obs_abs_theta = rbind(random_bias_obs_theta_abs, 
                               cluster_bias_obs_theta_abs, 
                               range_new_bias_obs_theta_abs, 
                               range_bias_obs_theta_abs, 
                               smart_bias_obs_theta_abs)

ggplot(uni_bias_obs_abs_theta, 
       aes(x = num_item, y = bias_obs_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean
uni_bias_obs_abs_theta_mean = rbind(random_bias_obs_theta_abs_mean, 
                                    cluster_bias_obs_theta_abs, 
                                    range_new_bias_obs_theta_abs, 
                                    range_bias_obs_theta_abs, 
                                    smart_bias_obs_theta_abs)

ggplot(uni_bias_obs_abs_theta_mean, 
       aes(x = num_item, y = bias_obs_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)


# bias all estimates

uni_bias_all_theta = rbind(random_bias_all_theta, 
                           cluster_bias_all_theta, 
                           range_new_bias_all_theta, 
                           range_bias_all_theta, 
                           smart_bias_all_theta)

ggplot(uni_bias_all_theta, 
       aes(x = num_item, y = bias_all, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

uni_bias_all_abs_theta = rbind(random_bias_all_theta_abs, 
                               cluster_bias_all_theta_abs, 
                               range_new_bias_all_theta_abs, 
                               range_bias_all_theta_abs, 
                               smart_bias_all_theta_abs)

ggplot(uni_bias_all_abs_theta, 
       aes(x = num_item, y = bias_all_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean

uni_bias_all_abs_theta_mean = rbind(random_bias_all_theta_abs_mean, 
                                    cluster_bias_all_theta_abs, 
                                    range_new_bias_all_theta_abs, 
                                    range_bias_all_theta_abs, 
                                    smart_bias_all_theta_abs)

ggplot(uni_bias_all_abs_theta_mean, 
       aes(x = num_item, y = bias_all_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)


# rmse obs 

uni_rmse_obs_theta = rbind(random_rmse_obs_theta, 
                           cluster_rmse_obs_theta, 
                           range_new_rmse_obs_theta, 
                           range_rmse_obs_theta, 
                           smart_rmse_obs_theta)
uni_rmse_obs_theta$rmse = sqrt(uni_rmse_obs_theta$bias_obs_sq)

ggplot(uni_rmse_obs_theta, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean 
uni_rmse_obs_theta_mean = rbind(random_rmse_obs_theta_mean, 
                                cluster_rmse_obs_theta, 
                                range_new_rmse_obs_theta, 
                                range_rmse_obs_theta, 
                                smart_rmse_obs_theta)
uni_rmse_obs_theta_mean$rmse = sqrt(uni_rmse_obs_theta_mean$bias_obs_sq)

ggplot(uni_rmse_obs_theta_mean, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)


# rmse all estimates 

uni_rmse_all_theta = rbind(random_rmse_all_theta, 
                           cluster_rmse_all_theta, 
                           range_new_rmse_all_theta, 
                           range_rmse_all_theta, 
                           smart_rmse_all_theta)
uni_rmse_all_theta$rmse = sqrt(uni_rmse_all_theta$bias_all_sq)

ggplot(uni_rmse_all_theta, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean

uni_rmse_all_theta_mean = rbind(random_rmse_all_theta_mean, 
                                cluster_rmse_all_theta, 
                                range_new_rmse_all_theta, 
                                range_rmse_all_theta, 
                                smart_rmse_all_theta)
uni_rmse_all_theta_mean$rmse = sqrt(uni_rmse_all_theta_mean$bias_all_sq)

ggplot(uni_rmse_all_theta_mean, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)
# gruppi latenti rmse 

uni_rmse_groups_theta = rbind(random_rmse_obs_group_theta, 
                              smart_rmse_obs_group_theta, 
                              cluster_rmse_obs_group_theta, 
                              range_rmse_obs_group_theta, 
                              range_new_rmse_obs_group_theta)
uni_rmse_groups_theta$rmse = sqrt(uni_rmse_groups_theta$bias_obs_sq)

ggplot(uni_rmse_groups_theta, 
       aes(x=group, y = rmse, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# random mean 

uni_rmse_groups_theta_mean = rbind(random_rmse_obs_group_theta_mean, 
                                   smart_rmse_obs_group_theta, 
                                   cluster_rmse_obs_group_theta, 
                                   range_rmse_obs_group_theta, 
                                   range_new_rmse_obs_group_theta)
uni_rmse_groups_theta_mean$rmse = sqrt(uni_rmse_groups_theta_mean$bias_obs_sq)

ggplot(uni_rmse_groups_theta_mean, 
       aes(x=group, y = rmse, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

uni_bias_abs_groups_theta = rbind(random_bias_abs_group_theta, 
                                  smart_bias_abs_group_theta, 
                                  cluster_bias_abs_group_theta, 
                                  range_bias_abs_group_theta, 
                                  range_new_bias_abs_group_theta)

ggplot(uni_bias_abs_groups_theta, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# random mean

uni_bias_abs_groups_theta_mean = rbind(random_bias_abs_group_theta_mean, 
                                       smart_bias_abs_group_theta, 
                                       cluster_bias_abs_group_theta, 
                                       range_bias_abs_group_theta, 
                                       range_new_bias_abs_group_theta)

ggplot(uni_bias_abs_groups_theta_mean, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)


uni_bias_groups_theta = rbind(random_bias_group_theta, 
                              smart_bias_group_theta, 
                              cluster_bias_group_theta, 
                              range_bias_group_theta, 
                              range_new_bias_group_theta)

ggplot(uni_bias_groups_theta, 
       aes(x=group, y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# random mean 
uni_bias_groups_theta_mean = rbind(random_bias_group_theta_mean, 
                                   smart_bias_group_theta, 
                                   cluster_bias_group_theta, 
                                   range_bias_group_theta, 
                                   range_new_bias_group_theta)

ggplot(uni_bias_groups_theta_mean, 
       aes(x=group, y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# ATTENZIONE ----------
rm(list= ls()[!(ls() %in% c("m2pl_uni",
                            "all_data_uni_theta", 
                            "data_uni_random_theta_unique", 
                            "temp_random_uni_theta", 
                            "new_random_uni_theta", 
                            "mean_random_uni_theta", 
                            "data_info_uni_theta", 
                            "data_info_uni_theta_all", 
                            "theta_random_uni_theta", 
                            "theta_random_uni_theta_mean", 
                            "random_all_theta", 
                            "uni_bias_obs_theta", 
                            "uni_bias_obs_theta_mean", 
                            "uni_bias_obs_abs_theta", 
                            "uni_bias_obs_abs_theta_mean", 
                            "uni_bias_all_theta", 
                            "uni_bias_all_abs_theta_mean", 
                            "uni_bias_all_abs_theta", 
                            "uni_bias_all_abs_theta_mean", 
                            "uni_rmse_all_theta", 
                            "uni_rmse_all_theta_mean", 
                            "uni_rmse_obs_theta", 
                            "uni_rmse_obs_theta_mean", 
                            "uni_rmse_groups_theta", 
                            "uni_rmse_groups_theta_mean", 
                            "uni_bias_groups_theta", 
                            "uni_bias_groups_theta_mean", 
                            "uni_bias_abs_groups_theta", 
                            "uni_bias_abs_groups_theta_mean"))])


## Distribuzione normale -----
rm(list = ls())
load("item_clusterSelection.RData")
load("item_smartSelection.RData")
load("item_guidedSelection.RData")
load("item_guidedSelectionNew.RData")
load("item_randomSelection.RData")
info_summary_range_new_theta$selection = "guidedNew"
data_random_theta_summary$selection <- "random"
all_data_theta <- rbind(data_random_theta_summary,
                            data.frame(num_item = info_summary_range_theta$range_theta_name,
                                       mean_info = info_summary_range_theta$info_test,
                                       sd_info = 0,
                                       selection = info_summary_range_theta$selection,
                                       mean_rel = info_summary_range_theta$rel,
                                       sd_rel = 0),
                            data.frame(num_item = info_summary_range_new_theta$range_new_theta_name,
                                       mean_info = info_summary_range_new_theta$info_test,
                                       sd_info = 0,
                                       selection = info_summary_range_new_theta$selection,
                                       mean_rel = info_summary_range_new_theta$rel,
                                       sd_rel = 0),
                            data.frame(num_item = info_summary_cluster_theta$cluster_name,
                                       mean_info = info_summary_cluster_theta$info_test,
                                       sd_info = 0,
                                       selection = info_summary_cluster_theta$selection,
                                       mean_rel = info_summary_cluster_theta$rel,
                                       sd_rel = 0),
                            
                            data.frame(num_item = info_summary_smart_theta$smart_theta_name,
                                       mean_info = info_summary_smart_theta$info_test,
                                       sd_info = 0,
                                       selection = info_summary_smart_theta$selection,
                                       mean_rel = info_summary_smart_theta$rel,
                                       sd_rel = 0))
all_data_theta$item_temp <- gsub("number", "", all_data_theta$num_item)
all_data_theta$item_temp <- gsub("all", 0, all_data_theta$item_temp)
all_data_theta$item_temp <- as.integer(all_data_theta$item_temp)
all_data_theta <- all_data_theta[order(all_data_theta$item_temp), ]
all_data_theta$selection <- gsub("uni_theta", '', all_data_theta$selection)
ggplot(all_data_theta[!all_data_theta$num_item %in%"all", ],
       aes(x=as.factor(item_temp), y=mean_info,
           group=selection, color=selection)) +
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info),
                width=.2,
                position=position_dodge(0.05)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels =  unique(all_data_theta[!all_data_theta$num_item %in%"all", "num_item"])) +
  geom_hline(yintercept = all_data_theta[all_data_theta$num_item %in% "all",
                                             "mean_info"])

# grafico info unii prendendo solo la prima combo di item da numerosità di item

random_theta = NULL 
temp = NULL
for(i in 1:length(unique(data_random_theta$num_item))) {
  temp = data_random_theta[data_random_theta$num_item %in% unique(data_random_theta$num_item)[i], ]
  temp = temp[1, ]
  random_theta = rbind(random_theta, temp)
}
random_theta$selection = "random_theta" 
# unisco al data set 
data_random_theta_unique = rbind(data.frame(num_item = random_theta$num_item,
                                                mean_info = random_theta$info_total,
                                                sd_info = 0,
                                                selection = random_theta$selection,
                                                mean_rel = random_theta$rel,
                                                sd_rel = 0), 
                                 data.frame(num_item = info_summary_range_theta$range_theta_name,
                                            mean_info = info_summary_range_theta$info_test,
                                            sd_info = 0,
                                            selection = info_summary_range_theta$selection,
                                            mean_rel = info_summary_range_theta$rel,
                                            sd_rel = 0),
                                 data.frame(num_item = info_summary_range_new_theta$range_new_theta_name,
                                            mean_info = info_summary_range_new_theta$info_test,
                                            sd_info = 0,
                                            selection = info_summary_range_new_theta$selection,
                                            mean_rel = info_summary_range_new_theta$rel,
                                            sd_rel = 0),
                                 data.frame(num_item = info_summary_cluster_theta$cluster_name,
                                            mean_info = info_summary_cluster_theta$info_test,
                                            sd_info = 0,
                                            selection = info_summary_cluster_theta$selection,
                                            mean_rel = info_summary_cluster_theta$rel,
                                            sd_rel = 0),
                                 
                                 data.frame(num_item = info_summary_smart_theta$smart_theta_name,
                                            mean_info = info_summary_smart_theta$info_test,
                                            sd_info = 0,
                                            selection = info_summary_smart_theta$selection,
                                            mean_rel = info_summary_smart_theta$rel,
                                            sd_rel = 0))
ggplot(data_random_theta_unique[!data_random_theta_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(data_random_theta_unique[!data_random_theta_unique$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = data_random_theta_unique[data_random_theta_unique$num_item %in% "all", 
                                                       "mean_info"])


# reliability solo prima selezione ---- 
ggplot(data_random_theta_unique[!data_random_theta_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(data_random_theta_unique[!data_random_theta_unique$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = data_random_theta_unique[data_random_theta_unique$num_item %in% "all", 
                                                       "mean_rel"])


# stesso garfico ma con le reliability 

ggplot(all_data_theta[!all_data_theta$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1) +
  geom_point(aes(shape=selection), size =2)+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels = unique(all_data_theta[!all_data_theta$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_theta[all_data_theta$num_item %in% "all", 
                                             "mean_rel"]) + ylim(0,1)

grid.arrange(ggplot(all_data_theta[!all_data_theta$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_info, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "top") + 
               scale_x_discrete(labels =  unique(all_data_theta[!all_data_theta$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_theta[all_data_theta$num_item %in% "all", 
                                                          "mean_info"]), 
             ggplot(all_data_theta[!all_data_theta$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_rel, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1) +
               geom_point(aes(shape=selection), size =2)+
               geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "none") + 
               scale_x_discrete(labels = unique(all_data_theta[!all_data_theta$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_theta[all_data_theta$num_item %in% "all", 
                                                          "mean_rel"]))

# confronto TIF dati theta -----

temp_cluster_theta<- NULL
cluster_data_theta <- NULL

temp_smart_theta <- NULL
smart_data_theta <- NULL


temp_range_theta <- NULL
range_data_theta  <- NULL

temp_range_new_theta <- NULL
range_new_data_theta  <- NULL


for (i in 1:length(info_out_smart_theta)) {
  temp_cluster_theta<- data.frame(theta = info_out_cluster_theta[[i]]$theta,
                                      info = info_out_cluster_theta[[i]]$test_info_curve, 
                                      num_item = names(info_out_cluster_theta)[[i]], 
                                      sel = "cluster")
  cluster_data_theta <- rbind(temp_cluster_theta, 
                                  cluster_data_theta)
  
  temp_smart_theta <- data.frame(theta = info_out_smart_theta[[i]]$theta,
                                     info = info_out_smart_theta[[i]]$test_info_curve, 
                                     num_item = names(info_out_smart_theta)[[i]], 
                                     sel = "smart")
  smart_data_theta <- rbind(temp_smart_theta, 
                                smart_data_theta)
  temp_range_theta <- data.frame(theta = info_out_range_theta[[i]]$theta,
                                     info = info_out_range_theta[[i]]$test_info_curve, 
                                     num_item = names(info_out_range_theta)[[i]], 
                                     sel = "guided")
  range_data_theta  <- rbind(temp_range_theta, 
                                 range_data_theta )
  temp_range_new_theta <- data.frame(theta = info_out_range_new_theta[[i]]$theta,
                                         info = info_out_range_new_theta[[i]]$test_info_curve, 
                                         num_item = names(info_out_range_new_theta)[[i]], 
                                         sel = "guidedNEW")
  range_new_data_theta  <- rbind(temp_range_new_theta, 
                                     range_new_data_theta )
  
}

# questo prende tutte le TIF random
temp_random_theta <- NULL
random_data_theta <- NULL
for (i in 1:length(info_test_random_theta)) {
  temp_random_theta <- data.frame(theta = info_test_random_theta[[i]]$theta,
                                      info = info_test_random_theta[[i]]$test_info_curve, 
                                      num_item = paste0("number", 
                                                        nrow(info_test_random_theta[[i]]$info_curves_item)), 
                                      sel = "random")
  random_data_theta <- rbind(temp_random_theta, 
                                 random_data_theta)
}

# questo seleziona sono una delle curve per ogni numerosità 

temp = NULL
new_random_theta = NULL
for (i in 1:length(unique(random_data_theta$num_item))) {
  temp = random_data_theta[random_data_theta$num_item %in% unique(random_data_theta$num_item)[i], ]
  temp = temp[1:1000, ]
  new_random_theta = rbind(new_random_theta, temp)
}

# questo calcola la media delle informatività 
mean_random_theta<- aggregate(info ~ theta + num_item, data = random_data_theta, mean)
mean_random_theta<- mean_random_theta[, c("theta", "info", "num_item")]
mean_random_theta$sel <- "random"
start_data_theta <- data.frame(theta = IRT.informationCurves(m2pl, 
                                                                 theta = seq(-3, 3, 
                                                                             length = 1000))$theta,
                                   info = info_start, 
                                   num_item = "all", 
                                   sel = "start")

data_info_theta <- rbind(
  cluster_data_theta,
  range_data_theta , range_new_data_theta , 
  smart_data_theta , new_random_theta #mean_random_theta
)

data_info_theta_all <- rbind(
  cluster_data_theta,
  range_data_theta , range_new_data_theta , 
  smart_data_theta , mean_random_theta
)


plots_theta <- list()
graph_start <- data.frame(theta = IRT.informationCurves(m2pl, 
                                                            theta = seq(-3,3,length = 1000))$theta, 
                              info = (IRT.informationCurves(m2pl, 
                                                            theta = seq(-3,3,length = 1000))$test_info_curve), 
                              num_item = "all",
                              sel = "start")

for(i in 1:length(unique(data_info_theta$num_item))) {
  
  plots_theta[[i]] <-  ggplot(rbind(data_info_theta[data_info_theta$num_item %in% unique(data_info_theta$num_item)[i], ], 
                                        graph_start), 
                                  aes(x = theta, y = info, group = sel, 
                                      col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4) + 
    ggtitle(unique(data_info_theta$num_item)[i]) +
    theme(legend.position = "none")
}


ggplot(rbind(data_info_theta[data_info_theta$num_item %in% "number10", ], 
             graph_start), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4)

do.call(grid.arrange, plots_theta)

# stessa cosa ma questa volta con la TIF media
for(i in 1:length(unique(data_info_theta_all$num_item))) {
  
  plots_theta[[i]] <-  ggplot(rbind(data_info_theta_all[data_info_theta_all$num_item %in% unique(data_info_theta_all$num_item)[i], ], 
                                        graph_start), 
                                  aes(x = theta, y = info, group = sel, 
                                      col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4) + 
    ggtitle(unique(data_info_theta_all$num_item)[i]) +
    theme(legend.position = "none")
}


ggplot(rbind(data_info_theta_all[data_info_theta_all$num_item %in% "number10", ], 
             graph_start), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4)

do.call(grid.arrange, plots_theta)

# Costruisco un dataframe per il calcolo del bias per le stime di theta theta ----

for (i in 1:length(model_fit_random_theta)) {
  names(model_fit_random_theta)[[i]] <- paste0("number", 
                                                   nrow(model_fit_random_theta[[i]]$xsi))
}

temp <- NULL
random_theta <- NULL
list_temp <- NULL
theta_random_theta <- list()

for (i in 1:length(unique(names(model_fit_random_theta)))) {
  random_theta <- NULL
  temp <- model_fit_random_theta[names(model_fit_random_theta) == unique(names(model_fit_random_theta))[i]]
  
  for (j in 1:length(temp)) {
    list_temp <- temp[[j]]$person$EAP
    random_theta <- data.frame(cbind(random_theta, list_temp))
    theta_random_theta[[i]] <- random_theta
    names(theta_random_theta)[[i]] <- unique(names(model_fit_random_theta))[i]
  }
}

# # devo fare la media attraverso el colonne per oguno dei number item 
# devo fare la media attraverso el colonne per oguno dei number item
temp_theta_random_mean <- data.frame(matrix(ncol = length(unique(names(theta_random_theta))),
                                            nrow = 1000))
for (i in 1:length(unique(names(theta_random_theta)))) {
  temp_theta_random_mean[, i] <- c(rowMeans(theta_random_theta[[i]]))
  colnames(temp_theta_random_mean)[i] <- unique(names(theta_random_theta))[i]
}

random_all_theta = NULL
temp = NULL
# devo mettere tutti i theta in un unico dataframe, unirci i theta osservati e 
# i theta ottenuti su tutto il set di item (che sono sempre 1'000)
for (i in 1:length(names(theta_random_theta))) {
  temp = data.frame(theta_random_theta[names(theta_random_theta) == unique(names(theta_random_theta))[i]])
  
  for (j in 1:length(temp)) {
    random_mean_temp = data.frame(num_item = unique(names(theta_random_theta))[i], theta_est = temp[,j])
    random_all_theta = rbind(random_all_theta, 
                             random_mean_temp)
  }
  
}
# devono esserci 10'000 osservazioni in per ogni numerosità di item

table(random_all_theta$num_item)

# creo il data set con l'info media per ogni combinazione di item

temp_theta_random_theta = data.frame(matrix(ncol = 
                                                  length(unique(names(theta_random_theta))), 
                                                nrow = 1000))
for (i in 1:length(theta_random_theta)) {
  temp_theta_random_theta[,i] = theta_random_theta[[i]][,1]
  colnames(temp_theta_random_theta)[i] <- unique(names(theta_random_theta))[i]
}

random_mean = data.frame(selection = rep("random", nrow(temp_theta_random_theta)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random_theta_mean = cbind(sbj, random_mean, temp_theta_random_theta)
theta_random_theta_mean <- reshape(temp_theta_random_theta_mean, 
                                       idvar = "sbj", 
                                       varying = list(3:(ncol(temp_theta_random_theta_mean))), 
                                       v.names = "theta_est", 
                                       direction = "long", 
                                       times = (names(temp_theta_random_theta_mean)[-c(1:2)]), 
                                       timevar = "num_item")
theta_random_theta_mean$obs <- true_theta
theta_random_theta_mean$all <- theta_all
head(theta_random_theta_mean)

# siccome prima ho preso la prima selezione di item per ognuna delle
# numerosità, prendo la prima selezione per ognuno

random = data.frame(selection = rep("random", nrow(temp_theta_random_theta)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random_theta = cbind(sbj, random, temp_theta_random_theta)
theta_random_theta <- reshape(temp_theta_random_theta, 
                                  idvar = "sbj", 
                                  varying = list(3:(ncol(temp_theta_random_theta))), 
                                  v.names = "theta_est", 
                                  direction = "long", 
                                  times = (names(temp_theta_random_theta)[-c(1:2)]), 
                                  timevar = "num_item")
theta_random_theta$obs <- true_theta
theta_random_theta$all <- theta_all
head(theta_random_theta)

# crea il dataframe per tutte le altre strategie
for (i in 1:length(model_out_cluster_theta)) {
  names(model_out_cluster_theta)[[i]] <- paste0("number", 
                                                    nrow(model_out_cluster_theta[[i]]$xsi))
  names(model_out_range_theta)[[i]] <- paste0("number", 
                                                  nrow(model_out_range_theta[[i]]$xsi))
  names(model_out_range_new_theta)[[i]] <- paste0("number", 
                                                      nrow(model_out_range_new_theta[[i]]$xsi))
  names(model_out_smart_theta)[[i]] <- paste0("number", 
                                                  nrow(model_out_smart_theta[[i]]$xsi))
}

temp_theta_cluster_theta <- data.frame(matrix(ncol = length(unique(names(model_out_cluster_theta))), 
                                                  nrow = 1000))

temp_theta_range_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_theta))), 
                                                nrow = 1000))
temp_theta_range_new_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_new_theta))), 
                                                    nrow = 1000))
temp_theta_smart_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_theta))), 
                                                nrow = 1000))

for (i in 1:length(unique(names(model_out_cluster_theta)))) {
  temp_theta_cluster_theta[, i] <- model_out_cluster_theta[[i]]$person$EAP
  colnames(temp_theta_cluster_theta)[i] <- unique(names(model_out_cluster_theta))[i]
  
  temp_theta_range_theta[, i] <- model_out_range_theta[[i]]$person$EAP
  colnames(temp_theta_range_theta)[i] <- unique(names(model_out_range_theta))[i]
  
  temp_theta_range_new_theta[, i] <- model_out_range_new_theta[[i]]$person$EAP
  colnames(temp_theta_range_new_theta)[i] <- unique(names(model_out_range_new_theta))[i]
  
  temp_theta_smart_theta[, i] <- model_out_smart_theta[[i]]$person$EAP
  colnames(temp_theta_smart_theta)[i] <- unique(names(model_out_smart_theta))[i]
  
}


sbj <- data.frame(sbj =1:1000)

cluster = data.frame(selection = rep("cluster", nrow(sbj)))
range = data.frame(selection = rep("range", nrow(sbj)))
range_new = data.frame(selection = rep("rangeNew", nrow(sbj)))
smart = data.frame(selection = rep("smart", nrow(sbj)))


temp_theta_cluster_theta <- cbind(sbj, cluster,  temp_theta_cluster_theta)

theta_cluster_theta <- reshape(temp_theta_cluster_theta, 
                                   idvar = "sbj", 
                                   varying = list(3:(ncol(temp_theta_cluster_theta))), 
                                   v.names = "theta_est", 
                                   direction = "long", 
                                   times = (names(temp_theta_cluster_theta)[-c(1:2)]), 
                                   timevar = "num_item")
theta_cluster_theta$obs <- true_theta
theta_cluster_theta$all <- theta_all
head(theta_cluster_theta)

temp_theta_range_theta <- cbind(sbj, range,  temp_theta_range_theta)

theta_range_theta <- reshape(temp_theta_range_theta, 
                                 idvar = "sbj", 
                                 varying = list(3:(ncol(temp_theta_range_theta))), 
                                 v.names = "theta_est", 
                                 direction = "long", 
                                 times = (names(temp_theta_range_theta)[-c(1:2)]), 
                                 timevar = "num_item")
theta_range_theta$obs <- true_theta
theta_range_theta$all <- theta_all
head(theta_range_theta)


temp_theta_range_new_theta <- cbind(sbj, range_new,  temp_theta_range_new_theta)

theta_range_new_theta <- reshape(temp_theta_range_new_theta, 
                                     idvar = "sbj", 
                                     varying = list(3:(ncol(temp_theta_range_new_theta))), 
                                     v.names = "theta_est", 
                                     direction = "long", 
                                     times = (names(temp_theta_range_new_theta)[-c(1:2)]), 
                                     timevar = "num_item")
theta_range_new_theta$obs <- true_theta
theta_range_new_theta$all <- theta_all
head(theta_range_new_theta)

temp_theta_smart_theta <- cbind(sbj, smart,  temp_theta_smart_theta)

theta_smart_theta <- reshape(temp_theta_smart_theta, 
                                 idvar = "sbj", 
                                 varying = list(3:(ncol(temp_theta_smart_theta))), 
                                 v.names = "theta_est", 
                                 direction = "long", 
                                 times = (names(temp_theta_smart_theta)[-c(1:2)]), 
                                 timevar = "num_item")
theta_smart_theta$obs <- true_theta
theta_smart_theta$all <- theta_all
head(theta_smart_theta)

# random obs ----
theta_random_theta$bias_obs = with(theta_random_theta, 
                                   theta_est - obs)
theta_random_theta$bias_obs_sq = theta_random_theta$bias_obs^2 
theta_random_theta$bias_obs_abs = with(theta_random_theta, 
                                       abs(theta_est - obs))
theta_random_theta$bias_all = with(theta_random_theta, 
                                   theta_est - all)
theta_random_theta$bias_all_sq = theta_random_theta$bias_all^2 
theta_random_theta$bias_all_abs = with(theta_random_theta, 
                                       abs(theta_est - all))

random_bias_obs_theta = aggregate(bias_obs ~num_item, 
                                  data = theta_random_theta, 
                                  mean)
random_bias_obs_theta$selection = "random"
random_bias_obs_theta$type = "bias_obs"
random_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_random_theta, 
                                  mean)
random_rmse_obs_theta$selection = "random"
random_rmse_obs_theta$type = "rmse_obs"

random_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_random_theta, 
                                      mean)
random_bias_obs_theta_abs$selection = "random"
random_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_random_theta$group <- ifelse(theta_random_theta$obs  <= cut_val[1, "start"], 
                                   group_name[1], 
                                   ifelse(theta_random_theta$obs > cut_val[1, "start"] & theta_random_theta$obs <= cut_val[1, "end"], 
                                          group_name[2], 
                                          ifelse(theta_random_theta$obs > cut_val[2, "start"] & theta_random_theta$obs <= cut_val[2, "end"], 
                                                 group_name[3], 
                                                 ifelse(theta_random_theta$obs > cut_val[3, "start"] & theta_random_theta$obs <= cut_val[3, "end"], 
                                                        group_name[4], 
                                                        ifelse(theta_random_theta$obs > cut_val[4, "start"] & theta_random_theta$obs <= cut_val[4, "end"], 
                                                               group_name[5], 
                                                               ifelse(theta_random_theta$obs > cut_val[4, "end"], 
                                                                      group_name[6], "error")
                                                        )))))
random_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                    data = theta_random_theta, 
                                    mean)

random_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                        data = theta_random_theta, 
                                        mean)

random_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                        data = theta_random_theta, 
                                        mean)

random_bias_group_theta$selection = "random"
random_bias_abs_group_theta$selection = "random"
random_rmse_obs_group_theta$selection = "random"

# random all 

theta_random_theta$bias_all = with(theta_random_theta, 
                                   theta_est - all)
theta_random_theta$bias_all_sq = theta_random_theta$bias_all^2 
theta_random_theta$bias_all_abs = with(theta_random_theta, 
                                       abs(theta_est - all))
theta_random_theta$bias_all = with(theta_random_theta, 
                                   theta_est - all)
theta_random_theta$bias_all_sq = theta_random_theta$bias_all^2 
theta_random_theta$bias_all_abs = with(theta_random_theta, 
                                       abs(theta_est - all))

random_bias_all_theta = aggregate(bias_all ~num_item, data = theta_random_theta, 
                                  mean)
random_bias_all_theta$selection = "random"
random_bias_all_theta$type = "bias_all"
random_rmse_all_theta = aggregate(bias_all_sq ~num_item, 
                                  data = theta_random_theta, 
                                  mean)
random_rmse_all_theta$selection = "random"
random_rmse_all_theta$type = "rmse_all"
random_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_random_theta, 
                                      mean)
random_bias_all_theta_abs$selection = "random"
random_bias_all_theta_abs$type = "bias_all_abs"

# random MEAN obs ----
# il bias non lo devo calcolare già sulle stime di theta medie, ma sulle stime 
# pulite di ogni possibile giro di ripescaggio e DOPO fare la media 
random_all_theta$obs = true_theta
random_all_theta$all = theta_all
random_all_theta$bias_obs = with(random_all_theta, 
                                 theta_est - obs)
random_all_theta$bias_obs_sq = random_all_theta$bias_obs^2 
random_all_theta$bias_obs_abs = with(random_all_theta, 
                                     abs(theta_est - obs))
random_all_theta$bias_all = with(random_all_theta, 
                                 theta_est - all)
random_all_theta$bias_all_sq = random_all_theta$bias_all^2 
random_all_theta$bias_all_abs = with(random_all_theta, 
                                     abs(theta_est - all))

random_bias_obs_theta_mean = aggregate(bias_obs ~num_item, 
                                       data = random_all_theta, 
                                       mean)
random_bias_obs_theta_mean$selection = "random"
random_bias_obs_theta_mean$type = "bias_obs"
random_rmse_obs_theta_mean = aggregate(bias_obs_sq ~num_item, 
                                       data = random_all_theta, 
                                       mean)
random_rmse_obs_theta_mean$selection = "random"
random_rmse_obs_theta_mean$type = "rmse_obs"

random_bias_obs_theta_abs_mean = aggregate(bias_obs_abs ~num_item, data = random_all_theta, 
                                           mean)
random_bias_obs_theta_abs_mean$selection = "random"
random_bias_obs_theta_abs_mean$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


random_all_theta$group <- ifelse(random_all_theta$obs  <= cut_val[1, "start"], 
                                 group_name[1], 
                                 ifelse(random_all_theta$obs > cut_val[1, "start"] & random_all_theta$obs <= cut_val[1, "end"], 
                                        group_name[2], 
                                        ifelse(random_all_theta$obs > cut_val[2, "start"] & random_all_theta$obs <= cut_val[2, "end"], 
                                               group_name[3], 
                                               ifelse(random_all_theta$obs > cut_val[3, "start"] & random_all_theta$obs <= cut_val[3, "end"], 
                                                      group_name[4], 
                                                      ifelse(random_all_theta$obs > cut_val[4, "start"] & random_all_theta$obs <= cut_val[4, "end"], 
                                                             group_name[5], 
                                                             ifelse(random_all_theta$obs > cut_val[4, "end"], 
                                                                    group_name[6], "error")
                                                      )))))
random_bias_group_theta_mean = aggregate(bias_obs ~num_item + group, 
                                         data = random_all_theta, 
                                         mean)

random_bias_abs_group_theta_mean = aggregate(bias_obs_abs ~num_item + group, 
                                             data = random_all_theta, 
                                             mean)

random_rmse_obs_group_theta_mean = aggregate(bias_obs_sq ~num_item + group, 
                                             data = random_all_theta, 
                                             mean)

random_bias_group_theta_mean$selection = "random"
random_bias_abs_group_theta_mean$selection = "random"
random_rmse_obs_group_theta_mean$selection = "random"

# random all MEAN ----

random_all_theta$bias_all = with(random_all_theta, 
                                 theta_est - all)
random_all_theta$bias_all_sq = random_all_theta$bias_all^2 
random_all_theta$bias_all_abs = with(random_all_theta, 
                                     abs(theta_est - all))
random_all_theta$bias_all = with(random_all_theta, 
                                 theta_est - all)
random_all_theta$bias_all_sq = random_all_theta$bias_all^2 
random_all_theta$bias_all_abs = with(random_all_theta, 
                                     abs(theta_est - all))

random_bias_all_theta_mean = aggregate(bias_all ~num_item, data = random_all_theta, 
                                       mean)
random_bias_all_theta_mean$selection = "random"
random_bias_all_theta_mean$type = "bias_all"
random_rmse_all_theta_mean = aggregate(bias_all_sq ~num_item, data = random_all_theta, 
                                       mean)
random_rmse_all_theta_mean$selection = "random"
random_rmse_all_theta_mean$type = "rmse_all"
random_bias_all_theta_abs_mean = aggregate(bias_all_abs ~num_item, 
                                           data = random_all_theta, 
                                           mean)
random_bias_all_theta_abs_mean$selection = "random"
random_bias_all_theta_abs_mean$type = "bias_all_abs"

# cluster obs ----
theta_cluster_theta$bias_obs = with(theta_cluster_theta, 
                                    theta_est - obs)
theta_cluster_theta$bias_obs_sq = theta_cluster_theta$bias_obs^2 
theta_cluster_theta$bias_obs_abs = with(theta_cluster_theta, 
                                        abs(theta_est - obs))
theta_cluster_theta$bias_all = with(theta_cluster_theta, 
                                    theta_est - all)
theta_cluster_theta$bias_all_sq = theta_cluster_theta$bias_all^2 
theta_cluster_theta$bias_all_abs = with(theta_cluster_theta, 
                                        abs(theta_est - all))

cluster_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_cluster_theta, 
                                   mean)
cluster_bias_obs_theta$selection = "cluster"
cluster_bias_obs_theta$type = "bias_obs"
cluster_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, 
                                   data = theta_cluster_theta, 
                                   mean)
cluster_rmse_obs_theta$selection = "cluster"
cluster_rmse_obs_theta$type = "rmse_obs"
cluster_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_cluster_theta, 
                                       mean)
cluster_bias_obs_theta_abs$selection = "cluster"
cluster_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_cluster_theta$group <- ifelse(theta_cluster_theta$obs  <= cut_val[1, "start"], 
                                    group_name[1], 
                                    ifelse(theta_cluster_theta$obs > cut_val[1, "start"] & theta_cluster_theta$obs <= cut_val[1, "end"], 
                                           group_name[2], 
                                           ifelse(theta_cluster_theta$obs > cut_val[2, "start"] & theta_cluster_theta$obs <= cut_val[2, "end"], 
                                                  group_name[3], 
                                                  ifelse(theta_cluster_theta$obs > cut_val[3, "start"] & theta_cluster_theta$obs <= cut_val[3, "end"], 
                                                         group_name[4], 
                                                         ifelse(theta_cluster_theta$obs > cut_val[4, "start"] & theta_cluster_theta$obs <= cut_val[4, "end"], 
                                                                group_name[5], 
                                                                ifelse(theta_cluster_theta$obs > cut_val[4, "end"], 
                                                                       group_name[6], "error")
                                                         )))))
cluster_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                     data = theta_cluster_theta, 
                                     mean)

cluster_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                         data = theta_cluster_theta, 
                                         mean)

cluster_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                         data = theta_cluster_theta, 
                                         mean)

cluster_bias_group_theta$selection = "cluster"
cluster_bias_abs_group_theta$selection = "cluster"
cluster_rmse_obs_group_theta$selection = "cluster"

# cluster all 

theta_cluster_theta$bias_all = with(theta_cluster_theta, 
                                    theta_est - all)
theta_cluster_theta$bias_all_sq = theta_cluster_theta$bias_all^2 
theta_cluster_theta$bias_all_abs = with(theta_cluster_theta, 
                                        abs(theta_est - all))
theta_cluster_theta$bias_all = with(theta_cluster_theta, 
                                    theta_est - all)
theta_cluster_theta$bias_all_sq = theta_cluster_theta$bias_all^2 
theta_cluster_theta$bias_all_abs = with(theta_cluster_theta, 
                                        abs(theta_est - all))

cluster_bias_all_theta = aggregate(bias_all ~num_item, data = theta_cluster_theta, 
                                   mean)
cluster_bias_all_theta$selection = "cluster"
cluster_bias_all_theta$type = "bias_all"
cluster_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_cluster_theta, 
                                   mean)
cluster_rmse_all_theta$selection = "cluster"
cluster_rmse_all_theta$type = "rmse_all"
cluster_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_cluster_theta, 
                                       mean)
cluster_bias_all_theta_abs$selection = "cluster"
cluster_bias_all_theta_abs$type = "bias_all_abs"

# range obs ----
theta_range_theta$bias_obs = with(theta_range_theta, 
                                  theta_est - obs)
theta_range_theta$bias_obs_sq = theta_range_theta$bias_obs^2 
theta_range_theta$bias_obs_abs = with(theta_range_theta, 
                                      abs(theta_est - obs))
theta_range_theta$bias_all = with(theta_range_theta, 
                                  theta_est - all)
theta_range_theta$bias_all_sq = theta_range_theta$bias_all^2 
theta_range_theta$bias_all_abs = with(theta_range_theta, 
                                      abs(theta_est - all))

range_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_range_theta, 
                                 mean)
range_bias_obs_theta$selection = "range"
range_bias_obs_theta$type = "bias_obs"
range_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_range_theta, 
                                 mean)
range_rmse_obs_theta$selection = "range"
range_rmse_obs_theta$type = "rmse_obs"
range_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_range_theta, 
                                     mean)
range_bias_obs_theta_abs$selection = "range"
range_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_range_theta$group <- ifelse(theta_range_theta$obs  <= cut_val[1, "start"], 
                                  group_name[1], 
                                  ifelse(theta_range_theta$obs > cut_val[1, "start"] & theta_range_theta$obs <= cut_val[1, "end"], 
                                         group_name[2], 
                                         ifelse(theta_range_theta$obs > cut_val[2, "start"] & theta_range_theta$obs <= cut_val[2, "end"], 
                                                group_name[3], 
                                                ifelse(theta_range_theta$obs > cut_val[3, "start"] & theta_range_theta$obs <= cut_val[3, "end"], 
                                                       group_name[4], 
                                                       ifelse(theta_range_theta$obs > cut_val[4, "start"] & theta_range_theta$obs <= cut_val[4, "end"], 
                                                              group_name[5], 
                                                              ifelse(theta_range_theta$obs > cut_val[4, "end"], 
                                                                     group_name[6], "error")
                                                       )))))
range_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                   data = theta_range_theta, 
                                   mean)

range_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                       data = theta_range_theta, 
                                       mean)

range_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                       data = theta_range_theta, 
                                       mean)

range_bias_group_theta$selection = "range"
range_bias_abs_group_theta$selection = "range"
range_rmse_obs_group_theta$selection = "range"

# range all 

theta_range_theta$bias_all = with(theta_range_theta, 
                                  theta_est - all)
theta_range_theta$bias_all_sq = theta_range_theta$bias_all^2 
theta_range_theta$bias_all_abs = with(theta_range_theta, 
                                      abs(theta_est - all))
theta_range_theta$bias_all = with(theta_range_theta, 
                                  theta_est - all)
theta_range_theta$bias_all_sq = theta_range_theta$bias_all^2 
theta_range_theta$bias_all_abs = with(theta_range_theta, 
                                      abs(theta_est - all))

range_bias_all_theta = aggregate(bias_all ~num_item, data = theta_range_theta, 
                                 mean)
range_bias_all_theta$selection = "range"
range_bias_all_theta$type = "bias_all"
range_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_range_theta, 
                                 mean)
range_rmse_all_theta$selection = "range"
range_rmse_all_theta$type = "rmse_all"
range_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_range_theta, 
                                     mean)
range_bias_all_theta_abs$selection = "range"
range_bias_all_theta_abs$type = "bias_all_abs"

# range_new obs ----
theta_range_new_theta$bias_obs = with(theta_range_new_theta, 
                                      theta_est - obs)
theta_range_new_theta$bias_obs_sq = theta_range_new_theta$bias_obs^2 
theta_range_new_theta$bias_obs_abs = with(theta_range_new_theta, 
                                          abs(theta_est - obs))
theta_range_new_theta$bias_all = with(theta_range_new_theta, 
                                      theta_est - all)
theta_range_new_theta$bias_all_sq = theta_range_new_theta$bias_all^2 
theta_range_new_theta$bias_all_abs = with(theta_range_new_theta, 
                                          abs(theta_est - all))

range_new_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_range_new_theta, 
                                     mean)
range_new_bias_obs_theta$selection = "range_new"
range_new_bias_obs_theta$type = "bias_obs"
range_new_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_range_new_theta, 
                                     mean)
range_new_rmse_obs_theta$selection = "range_new"
range_new_rmse_obs_theta$type = "rmse_obs"
range_new_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_range_new_theta, 
                                         mean)
range_new_bias_obs_theta_abs$selection = "range_new"
range_new_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_range_new_theta$group <- ifelse(theta_range_new_theta$obs  <= cut_val[1, "start"], 
                                      group_name[1], 
                                      ifelse(theta_range_new_theta$obs > cut_val[1, "start"] & theta_range_new_theta$obs <= cut_val[1, "end"], 
                                             group_name[2], 
                                             ifelse(theta_range_new_theta$obs > cut_val[2, "start"] & theta_range_new_theta$obs <= cut_val[2, "end"], 
                                                    group_name[3], 
                                                    ifelse(theta_range_new_theta$obs > cut_val[3, "start"] & theta_range_new_theta$obs <= cut_val[3, "end"], 
                                                           group_name[4], 
                                                           ifelse(theta_range_new_theta$obs > cut_val[4, "start"] & theta_range_new_theta$obs <= cut_val[4, "end"], 
                                                                  group_name[5], 
                                                                  ifelse(theta_range_new_theta$obs > cut_val[4, "end"], 
                                                                         group_name[6], "error")
                                                           )))))
range_new_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                       data = theta_range_new_theta, 
                                       mean)

range_new_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                           data = theta_range_new_theta, 
                                           mean)

range_new_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                           data = theta_range_new_theta, 
                                           mean)

range_new_bias_group_theta$selection = "range_new"
range_new_bias_abs_group_theta$selection = "range_new"
range_new_rmse_obs_group_theta$selection = "range_new"

# range_new all 

theta_range_new_theta$bias_all = with(theta_range_new_theta, 
                                      theta_est - all)
theta_range_new_theta$bias_all_sq = theta_range_new_theta$bias_all^2 
theta_range_new_theta$bias_all_abs = with(theta_range_new_theta, 
                                          abs(theta_est - all))
theta_range_new_theta$bias_all = with(theta_range_new_theta, 
                                      theta_est - all)
theta_range_new_theta$bias_all_sq = theta_range_new_theta$bias_all^2 
theta_range_new_theta$bias_all_abs = with(theta_range_new_theta, 
                                          abs(theta_est - all))

range_new_bias_all_theta = aggregate(bias_all ~num_item, data = theta_range_new_theta, 
                                     mean)
range_new_bias_all_theta$selection = "range_new"
range_new_bias_all_theta$type = "bias_all"
range_new_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_range_new_theta, 
                                     mean)
range_new_rmse_all_theta$selection = "range_new"
range_new_rmse_all_theta$type = "rmse_all"
range_new_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_range_new_theta, 
                                         mean)
range_new_bias_all_theta_abs$selection = "range_new"
range_new_bias_all_theta_abs$type = "bias_all_abs"

# smart obs ----
theta_smart_theta$bias_obs = with(theta_smart_theta, 
                                  theta_est - obs)
theta_smart_theta$bias_obs_sq = theta_smart_theta$bias_obs^2 
theta_smart_theta$bias_obs_abs = with(theta_smart_theta, 
                                      abs(theta_est - obs))
theta_smart_theta$bias_all = with(theta_smart_theta, 
                                  theta_est - all)
theta_smart_theta$bias_all_sq = theta_smart_theta$bias_all^2 
theta_smart_theta$bias_all_abs = with(theta_smart_theta, 
                                      abs(theta_est - all))

smart_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_smart_theta, 
                                 mean)
smart_bias_obs_theta$selection = "smart"
smart_bias_obs_theta$type = "bias_obs"
smart_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_smart_theta, 
                                 mean)
smart_rmse_obs_theta$selection = "smart"
smart_rmse_obs_theta$type = "rmse_obs"
smart_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_smart_theta, 
                                     mean)
smart_bias_obs_theta_abs$selection = "smart"
smart_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_smart_theta$group <- ifelse(theta_smart_theta$obs  <= cut_val[1, "start"], 
                                  group_name[1], 
                                  ifelse(theta_smart_theta$obs > cut_val[1, "start"] & theta_smart_theta$obs <= cut_val[1, "end"], 
                                         group_name[2], 
                                         ifelse(theta_smart_theta$obs > cut_val[2, "start"] & theta_smart_theta$obs <= cut_val[2, "end"], 
                                                group_name[3], 
                                                ifelse(theta_smart_theta$obs > cut_val[3, "start"] & theta_smart_theta$obs <= cut_val[3, "end"], 
                                                       group_name[4], 
                                                       ifelse(theta_smart_theta$obs > cut_val[4, "start"] & theta_smart_theta$obs <= cut_val[4, "end"], 
                                                              group_name[5], 
                                                              ifelse(theta_smart_theta$obs > cut_val[4, "end"], 
                                                                     group_name[6], "error")
                                                       )))))
smart_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                   data = theta_smart_theta, 
                                   mean)

smart_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                       data = theta_smart_theta, 
                                       mean)

smart_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                       data = theta_smart_theta, 
                                       mean)

smart_bias_group_theta$selection = "smart"
smart_bias_abs_group_theta$selection = "smart"
smart_rmse_obs_group_theta$selection = "smart"

# smart all 

theta_smart_theta$bias_all = with(theta_smart_theta, 
                                  theta_est - all)
theta_smart_theta$bias_all_sq = theta_smart_theta$bias_all^2 
theta_smart_theta$bias_all_abs = with(theta_smart_theta, 
                                      abs(theta_est - all))
theta_smart_theta$bias_all = with(theta_smart_theta, 
                                  theta_est - all)
theta_smart_theta$bias_all_sq = theta_smart_theta$bias_all^2 
theta_smart_theta$bias_all_abs = with(theta_smart_theta, 
                                      abs(theta_est - all))

smart_bias_all_theta = aggregate(bias_all ~num_item, data = theta_smart_theta, 
                                 mean)
smart_bias_all_theta$selection = "smart"
smart_bias_all_theta$type = "bias_all"
smart_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_smart_theta, 
                                 mean)
smart_rmse_all_theta$selection = "smart"
smart_rmse_all_theta$type = "rmse_all"
smart_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_smart_theta, 
                                     mean)
smart_bias_all_theta_abs$selection = "smart"
smart_bias_all_theta_abs$type = "bias_all_abs"


# ora unisco i bias con i bias e gli rmse con gli rmse 
random_bias_obs_theta$selection = "random"
random_bias_obs_theta$type = "bias_obs"

normal_bias_obs_theta = rbind(random_bias_obs_theta, 
                              cluster_bias_obs_theta, 
                              range_new_bias_obs_theta, 
                              range_bias_obs_theta, 
                              smart_bias_obs_theta)

ggplot(normal_bias_obs_theta, 
       aes(x = num_item, y = bias_obs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2) 

# random mean
normal_bias_obs_theta_mean = rbind(random_bias_obs_theta_mean, 
                                   cluster_bias_obs_theta, 
                                   range_new_bias_obs_theta, 
                                   range_bias_obs_theta, 
                                   smart_bias_obs_theta)

ggplot(normal_bias_obs_theta_mean, 
       aes(x = num_item, y = bias_obs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2) 


normal_bias_obs_abs_theta = rbind(random_bias_obs_theta_abs, 
                                  cluster_bias_obs_theta_abs, 
                                  range_new_bias_obs_theta_abs, 
                                  range_bias_obs_theta_abs, 
                                  smart_bias_obs_theta_abs)

ggplot(normal_bias_obs_abs_theta, 
       aes(x = num_item, y = bias_obs_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean
normal_bias_obs_abs_theta_mean = rbind(random_bias_obs_theta_abs_mean, 
                                       cluster_bias_obs_theta_abs, 
                                       range_new_bias_obs_theta_abs, 
                                       range_bias_obs_theta_abs, 
                                       smart_bias_obs_theta_abs)

ggplot(normal_bias_obs_abs_theta_mean, 
       aes(x = num_item, y = bias_obs_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)


# bias all estimates

normal_bias_all_theta = rbind(random_bias_all_theta, 
                              cluster_bias_all_theta, 
                              range_new_bias_all_theta, 
                              range_bias_all_theta, 
                              smart_bias_all_theta)

ggplot(normal_bias_all_theta, 
       aes(x = num_item, y = bias_all, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

normal_bias_all_abs_theta = rbind(random_bias_all_theta_abs, 
                                  cluster_bias_all_theta_abs, 
                                  range_new_bias_all_theta_abs, 
                                  range_bias_all_theta_abs, 
                                  smart_bias_all_theta_abs)

ggplot(normal_bias_all_abs_theta, 
       aes(x = num_item, y = bias_all_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean

normal_bias_all_abs_theta_mean = rbind(random_bias_all_theta_abs_mean, 
                                       cluster_bias_all_theta_abs, 
                                       range_new_bias_all_theta_abs, 
                                       range_bias_all_theta_abs, 
                                       smart_bias_all_theta_abs)

ggplot(normal_bias_all_abs_theta_mean, 
       aes(x = num_item, y = bias_all_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)


# rmse obs 

normal_rmse_obs_theta = rbind(random_rmse_obs_theta, 
                              cluster_rmse_obs_theta, 
                              range_new_rmse_obs_theta, 
                              range_rmse_obs_theta, 
                              smart_rmse_obs_theta)
normal_rmse_obs_theta$rmse = sqrt(normal_rmse_obs_theta$bias_obs_sq)

ggplot(normal_rmse_obs_theta, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean 
normal_rmse_obs_theta_mean = rbind(random_rmse_obs_theta_mean, 
                                   cluster_rmse_obs_theta, 
                                   range_new_rmse_obs_theta, 
                                   range_rmse_obs_theta, 
                                   smart_rmse_obs_theta)
normal_rmse_obs_theta_mean$rmse = sqrt(normal_rmse_obs_theta_mean$bias_obs_sq)

ggplot(normal_rmse_obs_theta_mean, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)


# rmse all estimates 

normal_rmse_all_theta = rbind(random_rmse_all_theta, 
                              cluster_rmse_all_theta, 
                              range_new_rmse_all_theta, 
                              range_rmse_all_theta, 
                              smart_rmse_all_theta)
normal_rmse_all_theta$rmse = sqrt(normal_rmse_all_theta$bias_all_sq)

ggplot(normal_rmse_all_theta, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# random mean

normal_rmse_all_theta_mean = rbind(random_rmse_all_theta_mean, 
                                   cluster_rmse_all_theta, 
                                   range_new_rmse_all_theta, 
                                   range_rmse_all_theta, 
                                   smart_rmse_all_theta)
normal_rmse_all_theta_mean$rmse = sqrt(normal_rmse_all_theta_mean$bias_all_sq)

ggplot(normal_rmse_all_theta_mean, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)
# gruppi latenti rmse 

normal_rmse_groups_theta = rbind(random_rmse_obs_group_theta, 
                                 smart_rmse_obs_group_theta, 
                                 cluster_rmse_obs_group_theta, 
                                 range_rmse_obs_group_theta, 
                                 range_new_rmse_obs_group_theta)
normal_rmse_groups_theta$rmse = sqrt(normal_rmse_groups_theta$bias_obs_sq)

ggplot(normal_rmse_groups_theta, 
       aes(x=group, y = rmse, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# random mean 

normal_rmse_groups_theta_mean = rbind(random_rmse_obs_group_theta_mean, 
                                      smart_rmse_obs_group_theta, 
                                      cluster_rmse_obs_group_theta, 
                                      range_rmse_obs_group_theta, 
                                      range_new_rmse_obs_group_theta)
normal_rmse_groups_theta_mean$rmse = sqrt(normal_rmse_groups_theta_mean$bias_obs_sq)

ggplot(normal_rmse_groups_theta_mean, 
       aes(x=group, y = rmse, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

normal_bias_abs_groups_theta = rbind(random_bias_abs_group_theta, 
                                     smart_bias_abs_group_theta, 
                                     cluster_bias_abs_group_theta, 
                                     range_bias_abs_group_theta, 
                                     range_new_bias_abs_group_theta)

ggplot(normal_bias_abs_groups_theta, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# random mean

normal_bias_abs_groups_theta_mean = rbind(random_bias_abs_group_theta_mean, 
                                          smart_bias_abs_group_theta, 
                                          cluster_bias_abs_group_theta, 
                                          range_bias_abs_group_theta, 
                                          range_new_bias_abs_group_theta)

ggplot(normal_bias_abs_groups_theta_mean, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)


normal_bias_groups_theta = rbind(random_bias_group_theta, 
                                 smart_bias_group_theta, 
                                 cluster_bias_group_theta, 
                                 range_bias_group_theta, 
                                 range_new_bias_group_theta)

ggplot(normal_bias_groups_theta, 
       aes(x=group, y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# random mean 
normal_bias_groups_theta_mean = rbind(random_bias_group_theta_mean, 
                                      smart_bias_group_theta, 
                                      cluster_bias_group_theta, 
                                      range_bias_group_theta, 
                                      range_new_bias_group_theta)

ggplot(normal_bias_groups_theta_mean, 
       aes(x=group, y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)


# ATTENZIONE ----
# ATTENZIONE ----------
rm(list= ls()[!(ls() %in% c("m2pl",
                            "all_data_theta", 
                            "data_random_theta_unique", 
                            "temp_random_theta", 
                            "new_random_theta", 
                            "mean_random_theta", 
                            "data_info_theta", 
                            "data_info_theta_all", 
                            "theta_random_theta", 
                            "theta_random_theta_mean", 
                            "random_all_theta", 
                            "normal_bias_obs_theta", 
                            "normal_bias_obs_theta_mean", 
                            "normal_bias_obs_abs_theta", 
                            "normal_bias_obs_abs_theta_mean", 
                            "normal_bias_all_theta", 
                            "normal_bias_all_abs_theta_mean", 
                            "normal_bias_all_abs_theta", 
                            "normal_bias_all_abs_theta_mean", 
                            "normal_rmse_all_theta", 
                            "normal_rmse_all_theta_mean", 
                            "normal_rmse_obs_theta", 
                            "normal_rmse_obs_theta_mean", 
                            "normal_rmse_groups_theta", 
                            "normal_rmse_groups_theta_mean", 
                            "normal_bias_groups_theta", 
                            "normal_bias_groups_theta_mean", 
                            "normal_bias_abs_groups_theta", 
                            "normal_bias_abs_groups_theta_mean"))])


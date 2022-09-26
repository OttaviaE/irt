# tutti insieme per i confronti ----
rm(list = ls())
library(ggplot2)
library(TAM)
library(gridExtra)
#setwd("D:/irt")
load("item_clusterSelection.RData")
load("item_smartSelection.RData")
load("item_guidedSelection.RData")
load("item_guidedSelectionNew.RData")
load("item_randomSelection.RData")
# # load("UNIrandom.RData")
# load("UNIsmart.RData")
# load("UNIguided.RData")
# load("UNIcluster.RData")
# load("SKrandom.RData")
# load("SKsmart.RData")
# load("SKguided.RData")
# load("SKcluster.RData")
# load("SKrandomEX.RData")
# load("SKsmartEX.RData")
# load("SKguidedEX.RData")
# load("SKclusterEX.RData")


# per dividere il tratto latente ---- 
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}

# Dati normali -----
# info_summary_target_theory$selection <- "guidedTheory"
# info_summary_cluster_theory$selection <- "clusterTheory"
info_summary_range_new$selection = "guidedNew"
data_random_summary$selection <- "random"
# qui lascio la media della strategia random 

 all_data_normal <- rbind(data_random_summary, 
  data.frame(num_item = info_summary_range$range_name,
             mean_info = info_summary_range$info_test,
             sd_info = 0,
             selection = info_summary_range$selection,
             mean_rel = info_summary_range$rel,
             sd_rel = 0),
  data.frame(num_item = info_summary_range_new$range_new_name,
             mean_info = info_summary_range_new$info_test,
             sd_info = 0,
             selection = info_summary_range_new$selection,
             mean_rel = info_summary_range_new$rel,
             sd_rel = 0),
  data.frame(num_item = info_summary_cluster$cluster_name, 
             mean_info = info_summary_cluster$info_test, 
             sd_info = 0, 
             selection = info_summary_cluster$selection, 
             mean_rel = info_summary_cluster$rel, 
             sd_rel = 0), 
  data.frame(num_item = info_summary_smart$smart_name,
             mean_info = info_summary_smart$info_test,
             sd_info = 0,
             selection = info_summary_smart$selection,
             mean_rel = info_summary_smart$rel,
             sd_rel = 0))
all_data_normal$item_temp <- gsub("number", "", all_data_normal$num_item)
all_data_normal$item_temp <- gsub("all", 0, all_data_normal$item_temp)
all_data_normal$item_temp <- as.integer(all_data_normal$item_temp)
all_data_normal <- all_data_normal[order(all_data_normal$item_temp), ]


# Grafico info normali ----
ggplot(all_data_normal[!all_data_normal$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_normal[!all_data_normal$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_normal[all_data_normal$num_item %in% "all", 
                                          "mean_info"])
# grafico info normali prendendo solo la prima combo di item da numerosità di item

normal_random = NULL 
temp = NULL
for(i in 1:length(unique(data_random$num_item))) {
  temp = data_random[data_random$num_item %in% unique(data_random$num_item)[i], ]
  temp = temp[1, ]
  normal_random = rbind(normal_random, temp)
}
normal_random$selection = "random" 
# unisco al data set 

data_normal_random_unique = rbind(data.frame(num_item = normal_random$num_item,
                                             mean_info = normal_random$info_total,
                                             sd_info = 0,
                                             selection = normal_random$selection,
                                             mean_rel = normal_random$rel,
                                             sd_rel = 0), 
                                  data.frame(num_item = info_summary_range$range_name,
                                             mean_info = info_summary_range$info_test,
                                             sd_info = 0,
                                             selection = info_summary_range$selection,
                                             mean_rel = info_summary_range$rel,
                                             sd_rel = 0),
                                  data.frame(num_item = info_summary_range_new$range_new_name,
                                             mean_info = info_summary_range_new$info_test,
                                             sd_info = 0,
                                             selection = info_summary_range_new$selection,
                                             mean_rel = info_summary_range_new$rel,
                                             sd_rel = 0),
                                  data.frame(num_item = info_summary_cluster$cluster_name, 
                                             mean_info = info_summary_cluster$info_test, 
                                             sd_info = 0, 
                                             selection = info_summary_cluster$selection, 
                                             mean_rel = info_summary_cluster$rel, 
                                             sd_rel = 0), 
                                  data.frame(num_item = info_summary_smart$smart_name,
                                             mean_info = info_summary_smart$info_test,
                                             sd_info = 0,
                                             selection = info_summary_smart$selection,
                                             mean_rel = info_summary_smart$rel,
                                             sd_rel = 0))
ggplot(data_normal_random_unique[!data_normal_random_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_normal[!all_data_normal$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_normal[all_data_normal$num_item %in% "all", 
                                          "mean_info"])


# Grafico reliability normali ----

ggplot(all_data_normal[!all_data_normal$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1) +
  geom_point(aes(shape=selection), size =2)+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels = unique(all_data_normal[!all_data_normal$num_item %in%"all",
                                                   "num_item"])) + 
  geom_hline(yintercept = all_data_normal[all_data_normal$num_item %in% "all", 
                                          "mean_rel"]) + ylim(0,1)

# reliability solo prima selezione ---- 
ggplot(data_normal_random_unique[!data_normal_random_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_normal[!all_data_normal$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_normal[all_data_normal$num_item %in% "all", 
                                          "mean_rel"])

grid.arrange(ggplot(all_data_normal[!all_data_normal$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_info, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "top") + 
               scale_x_discrete(labels =  unique(all_data_normal[!all_data_normal$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_normal[all_data_normal$num_item %in% "all", 
                                                       "mean_info"]) +
               ggtitle("Info normali"), 
             ggplot(all_data_normal[!all_data_normal$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_rel, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1) +
               geom_point(aes(shape=selection), size =2)+
               geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "none") + 
               scale_x_discrete(labels = unique(all_data_normal[!all_data_normal$num_item %in%"all",
                                                                "num_item"])) + 
               geom_hline(yintercept = all_data_normal[all_data_normal$num_item %in% "all", 
                                                       "mean_rel"]) +
               ggtitle("Reliability normali"), 
             nrow=2)




# Confronto delle TIF per ogni numero di item dati normali ----

temp_cluster <- NULL
cluster_data <- NULL

temp_smart <- NULL
smart_data <- NULL


temp_range <- NULL
range_data <- NULL

temp_range_new <- NULL
range_new_data <- NULL


for (i in 1:length(info_out_smart)) {
  temp_cluster <- data.frame(theta = info_out_cluster[[i]]$theta,
                             info = info_out_cluster[[i]]$test_info_curve, 
                             num_item = names(info_out_cluster)[[i]], 
                             sel = "cluster")
  cluster_data <- rbind(temp_cluster, 
                        cluster_data)
  
  temp_smart <- data.frame(theta = info_out_smart[[i]]$theta,
                           info = info_out_smart[[i]]$test_info_curve, 
                           num_item = names(info_out_smart)[[i]], 
                           sel = "smart")
  smart_data <- rbind(temp_smart, 
                      smart_data)
  temp_range <- data.frame(theta = info_out_range[[i]]$theta,
                           info = info_out_range[[i]]$test_info_curve, 
                           num_item = names(info_out_range)[[i]], 
                           sel = "guided")
  range_data <- rbind(temp_range, 
                      range_data)
  temp_range_new <- data.frame(theta = info_out_range_new[[i]]$theta,
                           info = info_out_range_new[[i]]$test_info_curve, 
                           num_item = names(info_out_range_new)[[i]], 
                           sel = "guidedNEW")
  range_new_data <- rbind(temp_range_new, 
                      range_new_data)
  
}

for (i in 1:length(info_test_random)) {
  names(info_test_random)[[i]] <- paste0("number", 
                                         nrow(info_test_random[[i]]$info_curves_item))
}

# crea dataset dei dati random 
temp_random <- NULL
random_data <- NULL
for (i in 1:length((info_test_random))) {
  temp_random <- data.frame(theta = info_test_random[[i]]$theta,
                            info = info_test_random[[i]]$test_info_curve,
                            num_item = paste0("number",
                                              nrow(info_test_random[[i]]$info_curves_item)),
                            sel = "random")
  random_data <- rbind(temp_random,
                       random_data)
}
# questo fa la media delle infor per ogni numerosità di item
# 
# mean_random <- aggregate(info ~ theta + num_item, data = random_data, mean)
# mean_random <- mean_random[, c("theta", "info", "num_item")]
# mean_random$sel <- "random"


# questo seleziona sono una delle curve per ogni numerosità 

temp = NULL
new_random = NULL
for (i in 1:length(unique(random_data$num_item))) {
  temp = random_data[random_data$num_item %in% unique(random_data$num_item)[i], ]
  temp = temp[1:1000, ]
  new_random = rbind(new_random, temp)
}

start_data <- data.frame(theta = IRT.informationCurves(m2pl, 
                                                       theta = seq(-3, 3, 
                                                                   length = 1000))$theta,
                         info = IRT.informationCurves(m2pl, 
                                                      theta = seq(-3, 3, 
                                                                  length = 1000))$test_info_curve, 
                         num_item = "all", 
                         sel = "start")

data_info <- rbind(
  cluster_data,
  range_data,  range_new_data, 
  smart_data , new_random#mean_random
)

graph_start_normal <- data.frame(theta = IRT.informationCurves(m2pl, 
                                                               theta = seq(-3,3,length = 1000))$theta, 
                                 info = (IRT.informationCurves(m2pl, 
                                                               theta = seq(-3,3,length = 1000))$test_info_curve), 
                                 num_item = "all",
                                 sel = "start")



ggplot(rbind(data_info[data_info$num_item %in% "number10", ], 
             graph_start_normal), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 2)


library(gridExtra)

plots <- list()
for(i in 1:length(unique(data_info$num_item))) {
  
  plots[[i]] <-  ggplot(rbind(data_info[data_info$num_item %in% unique(data_info$num_item)[i], ], 
                              graph_start_normal), 
                        aes(x = theta, y = info, group = sel, 
                            col = sel)) + geom_line(aes(linetype = sel), lwd = 1.3)  + 
    ggtitle(unique(data_info$num_item)[i]) +
    theme(legend.position = "none")
}

do.call(grid.arrange, plots)


# Costruisco un dataframe per il calcolo del bias per le stime di theta NORMALI ----

for (i in 1:length(model_fit_random)) {
  names(model_fit_random)[[i]] <- paste0("number", 
                                         nrow(model_fit_random[[i]]$xsi))
}


# stime della strategia random per ogni numerosità di item

temp <- NULL
random_theta <- NULL
list_temp <- NULL
theta_random_normal <- list()

for (i in 1:length(unique(names(model_fit_random)))) {
  random_theta = NULL
  temp <- model_fit_random[names(model_fit_random) == unique(names(model_fit_random))[i]]
  
  for (j in 1:length(temp)) {
    list_temp <- temp[[j]]$person$EAP
    random_theta <- data.frame(cbind(random_theta, list_temp))
    theta_random_normal[[i]] <- random_theta
    names(theta_random_normal)[[i]] <- unique(names(model_fit_random))[i]
  }
}

# siccome prima ho preso la prima selezione di item per ognuna delle 
# numerosità, prendo la prima selezione per ognuno
temp_theta_random = data.frame(matrix(ncol = length(unique(names(theta_random_normal))), 
                                                                              nrow = 1000))
for (i in 1:length(theta_random_normal)) {
  temp_theta_random[,i] = theta_random_normal[[i]][,1]
  colnames(temp_theta_random)[i] <- unique(names(theta_random_normal))[i]
}

# # Questo serve per costruire una media del bias di theta per la strategia random
 
# # devo fare la media attraverso el colonne per oguno dei number item 
# temp_theta_random <- data.frame(matrix(ncol = length(unique(names(theta_random_normal))), 
#                                        nrow = 1000))
# for (i in 1:length(unique(names(theta_random_normal)))) {
#   temp_theta_random[, i] <- c(rowMeans(theta_random_normal[[i]]))
#   colnames(temp_theta_random)[i] <- unique(names(theta_random_normal))[i]
# }
 theta_obs = true_theta

random = data.frame(selection = rep("random", nrow(temp_theta_random)))
sbj <- data.frame(sbj = 1:1000)
temp_theta_random = cbind(sbj, random, temp_theta_random)
theta_random_normal <- reshape(temp_theta_random, 
                               idvar = "sbj", 
                               varying = list(3:(ncol(temp_theta_random))), 
                               v.names = "theta_est", 
                               direction = "long", 
                               times = (names(temp_theta_random)[-c(1:2)]), 
                               timevar = "num_item")
theta_random_normal$obs <- theta_obs
theta_random_normal$all <- theta_all
head(theta_random_normal)

for (i in 1:length(model_out_cluster)) {
  names(model_out_cluster)[[i]] <- paste0("number", 
                                          nrow(model_out_cluster[[i]]$xsi))
  names(model_out_range)[[i]] <- paste0("number", 
                                        nrow(model_out_range[[i]]$xsi))
  names(model_out_range_new)[[i]] <- paste0("number", 
                                        nrow(model_out_range_new[[i]]$xsi))
  names(model_out_smart)[[i]] <- paste0("number", 
                                        nrow(model_out_smart[[i]]$xsi))
}

temp_theta_cluster_normal <- data.frame(matrix(ncol = length(unique(names(model_out_cluster))), 
                                               nrow = 1000))

temp_theta_range_normal <- data.frame(matrix(ncol = length(unique(names(model_out_range))), 
                                             nrow = 1000))
temp_theta_range_new_normal <- data.frame(matrix(ncol = length(unique(names(model_out_range_new))), 
                                             nrow = 1000))
temp_theta_smart_normal <- data.frame(matrix(ncol = length(unique(names(model_out_range))), 
                                             nrow = 1000))

for (i in 1:length(unique(names(model_out_cluster)))) {
  temp_theta_cluster_normal[, i] <- model_out_cluster[[i]]$person$EAP
  colnames(temp_theta_cluster_normal)[i] <- unique(names(model_out_cluster))[i]
  
  temp_theta_range_normal[, i] <- model_out_range[[i]]$person$EAP
  colnames(temp_theta_range_normal)[i] <- unique(names(model_out_range))[i]
  
  temp_theta_range_new_normal[, i] <- model_out_range[[i]]$person$EAP
  colnames(temp_theta_range_new_normal)[i] <- unique(names(model_out_range_new))[i]
  
  temp_theta_smart_normal[, i] <- model_out_smart[[i]]$person$EAP
  colnames(temp_theta_smart_normal)[i] <- unique(names(model_out_smart))[i]
  
}


sbj <- data.frame(sbj =1:1000)

cluster = data.frame(selection = rep("cluster", nrow(sbj)))
range = data.frame(selection = rep("range", nrow(sbj)))
range_new = data.frame(selection = rep("rangeNew", nrow(sbj)))
smart = data.frame(selection = rep("smart", nrow(sbj)))

theta_obs = true_theta
theta_all = m2pl$person$EAP

temp_start_normal = data.frame(sbj = sbj, 
                               selection = rep("start", nrow(sbj)), 
                               theta_est = m2pl$person$EAP)

temp_theta_cluster_normal <- cbind(sbj, cluster,  temp_theta_cluster_normal)

theta_cluster_normal <- reshape(temp_theta_cluster_normal, 
                                idvar = "sbj", 
                                varying = list(3:(ncol(temp_theta_cluster_normal))), 
                                v.names = "theta_est", 
                                direction = "long", 
                                times = (names(temp_theta_cluster_normal)[-c(1:2)]), 
                                timevar = "num_item")
theta_cluster_normal$obs <- theta_obs
theta_cluster_normal$all <- theta_all
head(theta_cluster_normal)
nrow(theta_cluster_normal)


temp_theta_range_normal <- cbind(sbj, range,  temp_theta_range_normal)

theta_range_normal <- reshape(temp_theta_range_normal, 
                              idvar = "sbj", 
                              varying = list(3:(ncol(temp_theta_range_normal))), 
                              v.names = "theta_est", 
                              direction = "long", 
                              times = (names(temp_theta_range_normal)[-c(1:2)]), 
                              timevar = "num_item")
theta_range_normal$obs <- theta_obs
theta_range_normal$all <- theta_all
head(theta_range_normal)

temp_theta_range_new_normal <- cbind(sbj, range_new,  temp_theta_range_new_normal)

theta_range_new_normal <- reshape(temp_theta_range_new_normal, 
                              idvar = "sbj", 
                              varying = list(3:(ncol(temp_theta_range_new_normal))), 
                              v.names = "theta_est", 
                              direction = "long", 
                              times = (names(temp_theta_range_new_normal)[-c(1:2)]), 
                              timevar = "num_item")
theta_range_new_normal$obs <- theta_obs
theta_range_new_normal$all <- theta_all
head(theta_range_new_normal)


temp_theta_smart_normal <- cbind(sbj, smart,  temp_theta_smart_normal)

theta_smart_normal <- reshape(temp_theta_smart_normal, 
                              idvar = "sbj", 
                              varying = list(3:(ncol(temp_theta_smart_normal))), 
                              v.names = "theta_est", 
                              direction = "long", 
                              times = (names(temp_theta_smart_normal)[-c(1:2)]), 
                              timevar = "num_item")
theta_smart_normal$obs <- theta_obs
theta_smart_normal$all <- theta_all
head(theta_smart_normal)
theta_smart_normal$bias_obs = with(theta_smart_normal, 
                                   theta_est - theta_obs)
mean(theta_smart_normal[theta_smart_normal$num_item %in% "number10", "bias_obs"])
mean(theta_smart_normal[theta_smart_normal$num_item %in% "number20", "bias_obs"])
mean(theta_smart_normal[theta_smart_normal$num_item %in% "number30", "bias_obs"])

aggregate(bias_obs ~ num_item, data = theta_smart_normal, mean)


# ad ognuno dei dataframe aggiungo i theta osservati, i theta totali, calcolo i bias
# alla fine unisco questi dataframe 

# random obs ----
theta_random_normal$bias_obs = with(theta_random_normal, 
                                    theta_est - obs)
theta_random_normal$bias_obs_sq = theta_random_normal$bias_obs^2 
theta_random_normal$bias_obs_abs = with(theta_random_normal, 
                                    abs(theta_est - obs))
theta_random_normal$bias_all = with(theta_random_normal, 
                                    theta_est - all)
theta_random_normal$bias_all_sq = theta_random_normal$bias_all^2 
theta_random_normal$bias_all_abs = with(theta_random_normal, 
                                        abs(theta_est - all))

random_bias_obs = aggregate(bias_obs ~num_item, data = theta_random_normal, 
                            mean)
random_bias_obs$selection = "random"
random_bias_obs$type = "bias_obs"
random_rmse_obs = aggregate(bias_obs_sq ~num_item, data = theta_random_normal, 
                            mean)
random_rmse_obs$selection = "random"
random_rmse_obs$type = "rmse_obs"
random_bias_obs_abs = aggregate(bias_obs_abs ~num_item, data = theta_random_normal, 
                            mean)
random_bias_obs_abs$selection = "random"
random_bias_obs_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_random_normal$group <- ifelse(theta_random_normal$obs  <= cut_val[1, "start"], 
                                    group_name[1], 
                                    ifelse(theta_random_normal$obs > cut_val[1, "start"] & theta_random_normal$obs <= cut_val[1, "end"], 
                                           group_name[2], 
                                           ifelse(theta_random_normal$obs > cut_val[2, "start"] & theta_random_normal$obs <= cut_val[2, "end"], 
                                                  group_name[3], 
                                                  ifelse(theta_random_normal$obs > cut_val[3, "start"] & theta_random_normal$obs <= cut_val[3, "end"], 
                                                         group_name[4], 
                                                         ifelse(theta_random_normal$obs > cut_val[4, "start"] & theta_random_normal$obs <= cut_val[4, "end"], 
                                                                group_name[5], 
                                                                ifelse(theta_random_normal$obs > cut_val[4, "end"], 
                                                                       group_name[6], "error")
                                                         )))))
random_bias_group = aggregate(bias_obs ~num_item + group, 
                              data = theta_random_normal, 
                            mean)

random_bias_abs_group = aggregate(bias_obs_abs ~num_item + group, 
                              data = theta_random_normal, 
                              mean)

random_rmse_obs_group = aggregate(bias_obs_sq ~num_item + group, 
                                  data = theta_random_normal, 
                            mean)

random_bias_group$selection = "random"
random_bias_abs_group$selection = "random"
random_rmse_obs_group$selection = "random"

# random all 

theta_random_normal$bias_all = with(theta_random_normal, 
                                    theta_est - all)
theta_random_normal$bias_all_sq = theta_random_normal$bias_all^2 
theta_random_normal$bias_all_abs = with(theta_random_normal, 
                                        abs(theta_est - all))
theta_random_normal$bias_all = with(theta_random_normal, 
                                    theta_est - all)
theta_random_normal$bias_all_sq = theta_random_normal$bias_all^2 
theta_random_normal$bias_all_abs = with(theta_random_normal, 
                                        abs(theta_est - all))

random_bias_all = aggregate(bias_all ~num_item, data = theta_random_normal, 
                            mean)
random_bias_all$selection = "random"
random_bias_all$type = "bias_all"
random_rmse_all = aggregate(bias_all_sq ~num_item, data = theta_random_normal, 
                            mean)
random_rmse_all$selection = "random"
random_rmse_all$type = "rmse_all"
random_bias_all_abs = aggregate(bias_all_abs ~num_item, data = theta_random_normal, 
                                mean)
random_bias_all_abs$selection = "random"
random_bias_all_abs$type = "bias_all_abs"


# cluster obs ----
theta_cluster_normal$bias_obs = with(theta_cluster_normal, 
                                     theta_est - obs)
theta_cluster_normal$bias_obs_sq = theta_cluster_normal$bias_obs^2 
theta_cluster_normal$bias_obs_abs = with(theta_cluster_normal, 
                                         abs(theta_est - obs))
theta_cluster_normal$bias_all = with(theta_cluster_normal, 
                                     theta_est - all)
theta_cluster_normal$bias_all_sq = theta_cluster_normal$bias_all^2 
theta_cluster_normal$bias_all_abs = with(theta_cluster_normal, 
                                         abs(theta_est - all))

cluster_bias_obs = aggregate(bias_obs ~num_item, data = theta_cluster_normal, 
                             mean)
cluster_bias_obs$selection = "cluster"
cluster_bias_obs$type = "bias_obs"
cluster_rmse_obs = aggregate(bias_obs_sq ~num_item, data = theta_cluster_normal, 
                             mean)
cluster_rmse_obs$selection = "cluster"
cluster_rmse_obs$type = "rmse_obs"
cluster_bias_obs_abs = aggregate(bias_obs_abs ~num_item, data = theta_cluster_normal, 
                                 mean)
cluster_bias_obs_abs$selection = "cluster"
cluster_bias_obs_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_cluster_normal$group <- ifelse(theta_cluster_normal$obs  <= cut_val[1, "start"], 
                                     group_name[1], 
                                     ifelse(theta_cluster_normal$obs > cut_val[1, "start"] & theta_cluster_normal$obs <= cut_val[1, "end"], 
                                            group_name[2], 
                                            ifelse(theta_cluster_normal$obs > cut_val[2, "start"] & theta_cluster_normal$obs <= cut_val[2, "end"], 
                                                   group_name[3], 
                                                   ifelse(theta_cluster_normal$obs > cut_val[3, "start"] & theta_cluster_normal$obs <= cut_val[3, "end"], 
                                                          group_name[4], 
                                                          ifelse(theta_cluster_normal$obs > cut_val[4, "start"] & theta_cluster_normal$obs <= cut_val[4, "end"], 
                                                                 group_name[5], 
                                                                 ifelse(theta_cluster_normal$obs > cut_val[4, "end"], 
                                                                        group_name[6], "error")
                                                          )))))
cluster_bias_group = aggregate(bias_obs ~num_item + group, 
                               data = theta_cluster_normal, 
                               mean)

cluster_bias_abs_group = aggregate(bias_obs_abs ~num_item + group, 
                                   data = theta_cluster_normal, 
                                   mean)

cluster_rmse_obs_group = aggregate(bias_obs_sq ~num_item + group, 
                                   data = theta_cluster_normal, 
                                   mean)

cluster_bias_group$selection = "cluster"
cluster_bias_abs_group$selection = "cluster"
cluster_rmse_obs_group$selection = "cluster"

# cluster all 

theta_cluster_normal$bias_all = with(theta_cluster_normal, 
                                     theta_est - all)
theta_cluster_normal$bias_all_sq = theta_cluster_normal$bias_all^2 
theta_cluster_normal$bias_all_abs = with(theta_cluster_normal, 
                                         abs(theta_est - all))
theta_cluster_normal$bias_all = with(theta_cluster_normal, 
                                     theta_est - all)
theta_cluster_normal$bias_all_sq = theta_cluster_normal$bias_all^2 
theta_cluster_normal$bias_all_abs = with(theta_cluster_normal, 
                                         abs(theta_est - all))

cluster_bias_all = aggregate(bias_all ~num_item, data = theta_cluster_normal, 
                             mean)
cluster_bias_all$selection = "cluster"
cluster_bias_all$type = "bias_all"
cluster_rmse_all = aggregate(bias_all_sq ~num_item, data = theta_cluster_normal, 
                             mean)
cluster_rmse_all$selection = "cluster"
cluster_rmse_all$type = "rmse_all"
cluster_bias_all_abs = aggregate(bias_all_abs ~num_item, data = theta_cluster_normal, 
                                 mean)
cluster_bias_all_abs$selection = "cluster"
cluster_bias_all_abs$type = "bias_all_abs"

# range obs ----
theta_range_normal$bias_obs = with(theta_range_normal, 
                                   theta_est - obs)
theta_range_normal$bias_obs_sq = theta_range_normal$bias_obs^2 
theta_range_normal$bias_obs_abs = with(theta_range_normal, 
                                       abs(theta_est - obs))
theta_range_normal$bias_all = with(theta_range_normal, 
                                   theta_est - all)
theta_range_normal$bias_all_sq = theta_range_normal$bias_all^2 
theta_range_normal$bias_all_abs = with(theta_range_normal, 
                                       abs(theta_est - all))

range_bias_obs = aggregate(bias_obs ~num_item, data = theta_range_normal, 
                           mean)
range_bias_obs$selection = "range"
range_bias_obs$type = "bias_obs"
range_rmse_obs = aggregate(bias_obs_sq ~num_item, data = theta_range_normal, 
                           mean)
range_rmse_obs$selection = "range"
range_rmse_obs$type = "rmse_obs"
range_bias_obs_abs = aggregate(bias_obs_abs ~num_item, data = theta_range_normal, 
                               mean)
range_bias_obs_abs$selection = "range"
range_bias_obs_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_range_normal$group <- ifelse(theta_range_normal$obs  <= cut_val[1, "start"], 
                                   group_name[1], 
                                   ifelse(theta_range_normal$obs > cut_val[1, "start"] & theta_range_normal$obs <= cut_val[1, "end"], 
                                          group_name[2], 
                                          ifelse(theta_range_normal$obs > cut_val[2, "start"] & theta_range_normal$obs <= cut_val[2, "end"], 
                                                 group_name[3], 
                                                 ifelse(theta_range_normal$obs > cut_val[3, "start"] & theta_range_normal$obs <= cut_val[3, "end"], 
                                                        group_name[4], 
                                                        ifelse(theta_range_normal$obs > cut_val[4, "start"] & theta_range_normal$obs <= cut_val[4, "end"], 
                                                               group_name[5], 
                                                               ifelse(theta_range_normal$obs > cut_val[4, "end"], 
                                                                      group_name[6], "error")
                                                        )))))
range_bias_group = aggregate(bias_obs ~num_item + group, 
                             data = theta_range_normal, 
                             mean)

range_bias_abs_group = aggregate(bias_obs_abs ~num_item + group, 
                                 data = theta_range_normal, 
                                 mean)

range_rmse_obs_group = aggregate(bias_obs_sq ~num_item + group, 
                                 data = theta_range_normal, 
                                 mean)

range_bias_group$selection = "range"
range_bias_abs_group$selection = "range"
range_rmse_obs_group$selection = "range"


# range all 

theta_range_normal$bias_all = with(theta_range_normal, 
                                   theta_est - all)
theta_range_normal$bias_all_sq = theta_range_normal$bias_all^2 
theta_range_normal$bias_all_abs = with(theta_range_normal, 
                                       abs(theta_est - all))
theta_range_normal$bias_all = with(theta_range_normal, 
                                   theta_est - all)
theta_range_normal$bias_all_sq = theta_range_normal$bias_all^2 
theta_range_normal$bias_all_abs = with(theta_range_normal, 
                                       abs(theta_est - all))

range_bias_all = aggregate(bias_all ~num_item, data = theta_range_normal, 
                           mean)
range_bias_all$selection = "range"
range_bias_all$type = "bias_all"
range_rmse_all = aggregate(bias_all_sq ~num_item, data = theta_range_normal, 
                           mean)
range_rmse_all$selection = "range"
range_rmse_all$type = "rmse_all"
range_bias_all_abs = aggregate(bias_all_abs ~num_item, data = theta_range_normal, 
                               mean)
range_bias_all_abs$selection = "range"
range_bias_all_abs$type = "bias_all_abs"



# ora unisco i bias con i bias e gli rmse con gli rmse 

normal_bias_obs = rbind(random_bias_obs, 
                    cluster_bias_obs, 
                    range_new_bias_obs, 
                    range_bias_obs, 
                    smart_bias_obs)

ggplot(normal_bias_obs, 
       aes(x = num_item, y = bias_obs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

normal_bias_obs_abs = rbind(random_bias_obs_abs, 
                            cluster_bias_obs_abs, 
                            range_new_bias_obs_abs, 
                            range_bias_obs_abs, 
                            smart_bias_obs_abs)

ggplot(normal_bias_obs_abs, 
       aes(x = num_item, y = bias_obs_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# range_new obs ----
theta_range_new_normal$bias_obs = with(theta_range_new_normal, 
                                       theta_est - obs)
theta_range_new_normal$bias_obs_sq = theta_range_new_normal$bias_obs^2 
theta_range_new_normal$bias_obs_abs = with(theta_range_new_normal, 
                                           abs(theta_est - obs))
theta_range_new_normal$bias_all = with(theta_range_new_normal, 
                                       theta_est - all)
theta_range_new_normal$bias_all_sq = theta_range_new_normal$bias_all^2 
theta_range_new_normal$bias_all_abs = with(theta_range_new_normal, 
                                           abs(theta_est - all))

range_new_bias_obs = aggregate(bias_obs ~num_item, data = theta_range_new_normal, 
                               mean)
range_new_bias_obs$selection = "range_new"
range_new_bias_obs$type = "bias_obs"
range_new_rmse_obs = aggregate(bias_obs_sq ~num_item, data = theta_range_new_normal, 
                               mean)
range_new_rmse_obs$selection = "range_new"
range_new_rmse_obs$type = "rmse_obs"
range_new_bias_obs_abs = aggregate(bias_obs_abs ~num_item, data = theta_range_new_normal, 
                                   mean)
range_new_bias_obs_abs$selection = "range_new"
range_new_bias_obs_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_range_new_normal$group <- ifelse(theta_range_new_normal$obs  <= cut_val[1, "start"], 
                                       group_name[1], 
                                       ifelse(theta_range_new_normal$obs > cut_val[1, "start"] & theta_range_new_normal$obs <= cut_val[1, "end"], 
                                              group_name[2], 
                                              ifelse(theta_range_new_normal$obs > cut_val[2, "start"] & theta_range_new_normal$obs <= cut_val[2, "end"], 
                                                     group_name[3], 
                                                     ifelse(theta_range_new_normal$obs > cut_val[3, "start"] & theta_range_new_normal$obs <= cut_val[3, "end"], 
                                                            group_name[4], 
                                                            ifelse(theta_range_new_normal$obs > cut_val[4, "start"] & theta_range_new_normal$obs <= cut_val[4, "end"], 
                                                                   group_name[5], 
                                                                   ifelse(theta_range_new_normal$obs > cut_val[4, "end"], 
                                                                          group_name[6], "error")
                                                            )))))
range_new_bias_group = aggregate(bias_obs ~num_item + group, 
                                 data = theta_range_new_normal, 
                                 mean)

range_new_bias_abs_group = aggregate(bias_obs_abs ~num_item + group, 
                                     data = theta_range_new_normal, 
                                     mean)

range_new_rmse_obs_group = aggregate(bias_obs_sq ~num_item + group, 
                                     data = theta_range_new_normal, 
                                     mean)

range_new_bias_group$selection = "range_new"
range_new_bias_abs_group$selection = "range_new"
range_new_rmse_obs_group$selection = "range_new"

# range_new all 

theta_range_new_normal$bias_all = with(theta_range_new_normal, 
                                       theta_est - all)
theta_range_new_normal$bias_all_sq = theta_range_new_normal$bias_all^2 
theta_range_new_normal$bias_all_abs = with(theta_range_new_normal, 
                                           abs(theta_est - all))
theta_range_new_normal$bias_all = with(theta_range_new_normal, 
                                       theta_est - all)
theta_range_new_normal$bias_all_sq = theta_range_new_normal$bias_all^2 
theta_range_new_normal$bias_all_abs = with(theta_range_new_normal, 
                                           abs(theta_est - all))

range_new_bias_all = aggregate(bias_all ~num_item, data = theta_range_new_normal, 
                               mean)
range_new_bias_all$selection = "range_new"
range_new_bias_all$type = "bias_all"
range_new_rmse_all = aggregate(bias_all_sq ~num_item, data = theta_range_new_normal, 
                               mean)
range_new_rmse_all$selection = "range_new"
range_new_rmse_all$type = "rmse_all"
range_new_bias_all_abs = aggregate(bias_all_abs ~num_item, data = theta_range_new_normal, 
                                   mean)
range_new_bias_all_abs$selection = "range_new"
range_new_bias_all_abs$type = "bias_all_abs"

# smart obs ----
theta_smart_normal$bias_obs = with(theta_smart_normal, 
                                   theta_est - obs)
theta_smart_normal$bias_obs_sq = theta_smart_normal$bias_obs^2 
theta_smart_normal$bias_obs_abs = with(theta_smart_normal, 
                                       abs(theta_est - obs))
theta_smart_normal$bias_all = with(theta_smart_normal, 
                                   theta_est - all)
theta_smart_normal$bias_all_sq = theta_smart_normal$bias_all^2 
theta_smart_normal$bias_all_abs = with(theta_smart_normal, 
                                       abs(theta_est - all))

smart_bias_obs = aggregate(bias_obs ~num_item, data = theta_smart_normal, 
                           mean)
smart_bias_obs$selection = "smart"
smart_bias_obs$type = "bias_obs"
smart_rmse_obs = aggregate(bias_obs_sq ~num_item, data = theta_smart_normal, 
                           mean)
smart_rmse_obs$selection = "smart"
smart_rmse_obs$type = "rmse_obs"
smart_bias_obs_abs = aggregate(bias_obs_abs ~num_item, data = theta_smart_normal, 
                               mean)
smart_bias_obs_abs$selection = "smart"
smart_bias_obs_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_smart_normal$group <- ifelse(theta_smart_normal$obs  <= cut_val[1, "start"], 
                                   group_name[1], 
                                   ifelse(theta_smart_normal$obs > cut_val[1, "start"] & theta_smart_normal$obs <= cut_val[1, "end"], 
                                          group_name[2], 
                                          ifelse(theta_smart_normal$obs > cut_val[2, "start"] & theta_smart_normal$obs <= cut_val[2, "end"], 
                                                 group_name[3], 
                                                 ifelse(theta_smart_normal$obs > cut_val[3, "start"] & theta_smart_normal$obs <= cut_val[3, "end"], 
                                                        group_name[4], 
                                                        ifelse(theta_smart_normal$obs > cut_val[4, "start"] & theta_smart_normal$obs <= cut_val[4, "end"], 
                                                               group_name[5], 
                                                               ifelse(theta_smart_normal$obs > cut_val[4, "end"], 
                                                                      group_name[6], "error")
                                                        )))))
smart_bias_group = aggregate(bias_obs ~num_item + group, 
                             data = theta_smart_normal, 
                             mean)

smart_bias_abs_group = aggregate(bias_obs_abs ~num_item + group, 
                                 data = theta_smart_normal, 
                                 mean)

smart_rmse_obs_group = aggregate(bias_obs_sq ~num_item + group, 
                                 data = theta_smart_normal, 
                                 mean)

smart_bias_group$selection = "smart"
smart_bias_abs_group$selection = "smart"
smart_rmse_obs_group$selection = "smart"

# smart all 

theta_smart_normal$bias_all = with(theta_smart_normal, 
                                   theta_est - all)
theta_smart_normal$bias_all_sq = theta_smart_normal$bias_all^2 
theta_smart_normal$bias_all_abs = with(theta_smart_normal, 
                                       abs(theta_est - all))
theta_smart_normal$bias_all = with(theta_smart_normal, 
                                   theta_est - all)
theta_smart_normal$bias_all_sq = theta_smart_normal$bias_all^2 
theta_smart_normal$bias_all_abs = with(theta_smart_normal, 
                                       abs(theta_est - all))

smart_bias_all = aggregate(bias_all ~num_item, data = theta_smart_normal, 
                           mean)
smart_bias_all$selection = "smart"
smart_bias_all$type = "bias_all"
smart_rmse_all = aggregate(bias_all_sq ~num_item, data = theta_smart_normal, 
                           mean)
smart_rmse_all$selection = "smart"
smart_rmse_all$type = "rmse_all"
smart_bias_all_abs = aggregate(bias_all_abs ~num_item, data = theta_smart_normal, 
                               mean)
smart_bias_all_abs$selection = "smart"
smart_bias_all_abs$type = "bias_all_abs"

# ora unisco i bias con i bias e gli rmse con gli rmse 

normal_bias_obs = rbind(random_bias_obs, 
                        cluster_bias_obs, 
                        range_new_bias_obs, 
                        range_bias_obs, 
                        smart_bias_obs)

ggplot(normal_bias_obs, 
       aes(x = num_item, y = bias_obs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

normal_bias_obs_abs = rbind(random_bias_obs_abs, 
                            cluster_bias_obs_abs, 
                            range_new_bias_obs_abs, 
                            range_bias_obs_abs, 
                            smart_bias_obs_abs)

ggplot(normal_bias_obs_abs, 
       aes(x = num_item, y = bias_obs_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# bias all estimates

normal_bias_all = rbind(random_bias_all, 
                        cluster_bias_all, 
                        range_new_bias_all, 
                        range_bias_all, 
                        smart_bias_all)

ggplot(normal_bias_all, 
       aes(x = num_item, y = bias_all, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

normal_bias_all_abs = rbind(random_bias_all_abs, 
                            cluster_bias_all_abs, 
                            range_new_bias_all_abs, 
                            range_bias_all_abs, 
                            smart_bias_all_abs)

ggplot(normal_bias_all_abs, 
       aes(x = num_item, y = bias_all_abs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# rmse obs 

normal_rmse_obs = rbind(random_rmse_obs, 
                        cluster_rmse_obs, 
                        range_new_rmse_obs, 
                        range_rmse_obs, 
                        smart_rmse_obs)
normal_rmse_obs$rmse = sqrt(normal_rmse_obs$bias_obs_sq)

ggplot(normal_rmse_obs, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# rmse all estimates 

normal_rmse_all = rbind(random_rmse_all, 
                        cluster_rmse_all, 
                        range_new_rmse_all, 
                        range_rmse_all, 
                        smart_rmse_all)
normal_rmse_all$rmse = sqrt(normal_rmse_all$bias_all_sq)

ggplot(normal_rmse_all, 
       aes(x = num_item, y = rmse, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

# gruppi latenti rmse 

normal_rmse_groups = rbind(random_rmse_obs_group, 
                    smart_rmse_obs_group, 
                    cluster_rmse_obs_group, 
                    range_rmse_obs_group, 
                    range_new_rmse_obs_group)
normal_rmse_groups$rmse = sqrt(normal_rmse_groups$bias_obs_sq)

ggplot(normal_rmse_groups, 
       aes(x=group, y = rmse, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

normal_bias_abs_groups = rbind(random_bias_abs_group, 
                    smart_bias_abs_group, 
                    cluster_bias_abs_group, 
                    range_bias_abs_group, 
                    range_new_bias_abs_group)

ggplot(normal_bias_abs_groups, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

normal_bias_groups = rbind(random_bias_group, 
                               smart_bias_group, 
                               cluster_bias_group, 
                               range_bias_group, 
                               range_new_bias_group)

ggplot(normal_bias_groups, 
       aes(x=group, y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

# theta_normal <- rbind(theta_cluster_normal, 
#                       theta_range_normal, theta_range_new_normal, 
#                       theta_smart_normal , theta_random_normal
# )

# # calcolo bias distr normale -----
# 
# theta_normal$distribution <- "normal"
# theta_normal <- theta_normal[order(theta_normal$obs), ]
# 
# theta_normal$bias_obs <- with(theta_normal, 
#                               theta_est - theta_obs)
# theta_normal$bias_obs_abs <- with(theta_normal, 
#                               abs(theta_est - theta_obs))
# 
# theta_normal$bias_all <- with(theta_normal, 
#                                   (theta_est - theta_all))
# 
# theta_normal$bias_all_abs <- with(theta_normal, 
#                               abs(theta_est - theta_all))
# 
# obs_bias_normal <- aggregate(bias_obs ~ num_item + selection, 
#                              data = theta_normal, 
#                              mean)
# obs_bias_normal$type <- "bias"
# obs_bias_normal_abs <- aggregate(bias_obs_abs ~ num_item + selection, 
#                              data = theta_normal, 
#                              mean)
# obs_bias_normal_abs$type <- "bias_abs"
# colnames(obs_bias_normal_abs)[3] <- "bias_obs"
# 
# obs_bias_normal <- rbind(obs_bias_normal, obs_bias_normal_abs)
# 
# obs_bias_normal$temp <- as.integer(gsub("number", "", 
#                                         obs_bias_normal$num_item))
# obs_bias_normal = obs_bias_normal[order(obs_bias_normal$temp), ]
# ggplot(obs_bias_normal, 
#        aes(x=as.factor(temp), y = bias_obs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_grid(~type)
# 
# 
# all_bias_normal <- aggregate(bias_all ~ num_item + selection, 
#                              data = theta_normal, 
#                              mean)
# all_bias_normal$type <- "bias"
# all_bias_normal_abs <- aggregate(bias_all_abs ~ num_item + selection, 
#                                  data = theta_normal, 
#                                  mean)
# all_bias_normal_abs$type <- "bias_abs"
# colnames(all_bias_normal_abs)[3] <- "bias_all"
# 
# all_bias_normal <- rbind(all_bias_normal, all_bias_normal_abs)
# 
# all_bias_normal$temp <- as.integer(gsub("number", "", 
#                                         all_bias_normal$num_item))
# all_bias_normal = all_bias_normal[order(all_bias_normal$temp), ]
# ggplot(all_bias_normal, 
#        aes(x=as.factor(temp), y = bias_all, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(~type)
# 
# # calcolo rmsea ----
# 
# theta_normal$bias_obs_sq <- theta_normal$bias_obs^2
# theta_normal$bias_obs_abs_sq <- theta_normal$bias_obs_abs^2
# 
# theta_normal$bias_all_sq <- theta_normal$bias_all^2
# theta_normal$bias_all_abs_sq <- theta_normal$bias_all_abs^2
# 
# 
# obs_bias_normal_sq <- aggregate(bias_obs_sq ~ num_item + selection, 
#                              data = theta_normal, 
#                              mean)
# obs_bias_normal_sq$type <- "bias_sq_obs"
# obs_bias_normal_sq$rmsea = sqrt(obs_bias_normal_sq$bias_obs_sq)
# 
# ggplot(obs_bias_normal_sq, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 
# 
# 
# all_bias_normal_sq <- aggregate(bias_all_sq ~ num_item + selection, 
#                                 data = theta_normal, 
#                                 mean)
# all_bias_normal_sq$type <- "bias_sq_all"
# all_bias_normal_sq$rmsea = sqrt(all_bias_normal_sq$bias_all)
# 
# ggplot(all_bias_normal_sq, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 


# # per gruppi latenti ----- 
# theta_lat <- seq(-2.5, 2.5, length.out = 4) 
# g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
# cut_val = cut_borders(g)
# 
# 
# group_name <- letters[1:(nrow(cut_val)+2)]
# 
# 
# theta_normal$group <- ifelse(theta_normal$obs  <= cut_val[1, "start"], 
#                              group_name[1], 
#                              ifelse(theta_normal$obs > cut_val[1, "start"] & theta_normal$obs <= cut_val[1, "end"], 
#                                     group_name[2], 
#                                     ifelse(theta_normal$obs > cut_val[2, "start"] & theta_normal$obs <= cut_val[2, "end"], 
#                                            group_name[3], 
#                                            ifelse(theta_normal$obs > cut_val[3, "start"] & theta_normal$obs <= cut_val[3, "end"], 
#                                                   group_name[4], 
#                                                   ifelse(theta_normal$obs > cut_val[4, "start"] & theta_normal$obs <= cut_val[4, "end"], 
#                                                          group_name[5], 
#                                                          ifelse(theta_normal$obs > cut_val[4, "end"], 
#                                                                 group_name[6], "error")
#                                                          )))))
# 
# bias_abs_normal_group <- aggregate(bias_obs_abs ~ selection + group + num_item, 
#                                    data = theta_normal, mean)
# bias_abs_normal_group$type = "bias"
# 
# ggplot(bias_abs_normal_group, 
#        aes(x=group, y = bias_obs_abs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), 
#                                            lwd =1.3) + 
#   theme(legend.position = "top") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)
# 
# bias_normal_group <- aggregate(bias_obs ~ selection + group + num_item, 
#                                    data = theta_normal, mean)
# bias_normal_group$type = "bias"
# 
# ggplot(bias_normal_group, 
#        aes(x=group, y = bias_obs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), 
#                                            lwd =1.3) + 
#   theme(legend.position = "top") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)
# 
# rmsea_normal_group <- aggregate(bias_obs_sq ~ selection + group + num_item, 
#                                data = theta_normal, mean)
# rmsea_normal_group$rmsea = sqrt(rmsea_normal_group$bias_obs_sq)
# 
# ggplot(rmsea_normal_group, 
#        aes(x=group, y = rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), 
#                                            lwd =1.3) + 
#   theme(legend.position = "top") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)
# 
# info_summary_range_new_theta$selection = "guidedNewTheta"
# normal distirbution theta fixed stimuli -----
data_random_theta_summary$selection <- "randomTheta"
all_data_normal_theta <-  rbind(data_random_theta_summary, 
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

all_data_normal_theta$item_temp <- gsub("number", "", all_data_normal_theta$num_item)
all_data_normal_theta$item_temp <- gsub("all", 0, all_data_normal_theta$item_temp)
all_data_normal_theta$item_temp <- as.integer(all_data_normal_theta$item_temp)
all_data_normal_theta <- all_data_normal_theta[order(all_data_normal_theta$item_temp), ]

# Grafico info normali fixed stimuli ----
ggplot(all_data_normal_theta[!all_data_normal_theta$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_normal_theta[!all_data_normal_theta$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_normal_theta[all_data_normal_theta$num_item %in% "all", 
                                                "mean_info"])

# grafico info normal_thetai prendendo solo la prima combo di item da numerosità di item

normal_random_theta = NULL 
temp = NULL
for(i in 1:length(unique(data_random_theta$num_item))) {
  temp = data_random_theta[data_random_theta$num_item %in% unique(data_random_theta$num_item)[i], ]
  temp = temp[1, ]
  normal_random_theta = rbind(normal_random_theta, temp)
}
normal_random_theta$selection = "random_theta" 
# unisco al data set 

data_normal_theta_random_unique = rbind(data.frame(num_item = normal_random_theta$num_item,
                                                         mean_info = normal_random_theta$info_total,
                                                         sd_info = 0,
                                                         selection = normal_random_theta$selection,
                                                         mean_rel = normal_random_theta$rel,
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
ggplot(data_normal_theta_random_unique[!data_normal_theta_random_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_normal_theta[!all_data_normal_theta$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_normal_theta[all_data_normal_theta$num_item %in% "all", 
                                                "mean_info"])


# reliability solo prima selezione ---- 
ggplot(data_normal_theta_random_unique[!data_normal_theta_random_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_normal_theta[!all_data_normal_theta$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_normal_theta[all_data_normal_theta$num_item %in% "all", 
                                                "mean_rel"])


# Grafico reliability normali fixed stimuli ----
ggplot(all_data_normal_theta[!all_data_normal_theta$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_normal_theta[!all_data_normal_theta$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_normal_theta[all_data_normal_theta$num_item %in% "all", 
                                                "mean_rel"]) + ylim(0,1)

# Confronto delle TIF per ogni numero di item dati normali PARAMETRI FISSI ----

temp_cluster_theta <- NULL
cluster_theta_data <- NULL

temp_smart_theta <- NULL
smart_theta_data <- NULL


temp_range_theta <- NULL
range_theta_data <- NULL

temp_range_new_theta <- NULL
range_new_theta_data <- NULL


for (i in 1:length(info_out_smart_theta)) {
  temp_cluster_theta <- data.frame(theta = info_out_cluster_theta[[i]]$theta,
                                   info = info_out_cluster_theta[[i]]$test_info_curve, 
                                   num_item = names(info_out_cluster_theta)[[i]], 
                                   sel = "cluster_theta")
  cluster_theta_data <- rbind(temp_cluster_theta, 
                              cluster_theta_data)
  
  temp_smart_theta <- data.frame(theta = info_out_smart_theta[[i]]$theta,
                                 info = info_out_smart_theta[[i]]$test_info_curve, 
                                 num_item = names(info_out_smart_theta)[[i]], 
                                 sel = "smart_theta")
  smart_theta_data <- rbind(temp_smart_theta, 
                            smart_theta_data)
  temp_range_theta <- data.frame(theta = info_out_range_theta[[i]]$theta,
                                 info = info_out_range_theta[[i]]$test_info_curve, 
                                 num_item = names(info_out_range_theta)[[i]], 
                                 sel = "guided")
  range_theta_data <- rbind(temp_range_theta, 
                            range_theta_data)
  temp_range_new_theta <- data.frame(theta = info_out_range_new_theta[[i]]$theta,
                                 info = info_out_range_new_theta[[i]]$test_info_curve, 
                                 num_item = names(info_out_range_new_theta)[[i]], 
                                 sel = "guidedNewTheta")
  range_new_theta_data <- rbind(temp_range_new_theta, 
                            range_new_theta_data)
  
}

temp_random_theta <- NULL
random_theta_data <- NULL
for (i in 1:length(info_test_random_theta)) {
  temp_random_theta <- data.frame(theta = info_test_random_theta[[i]]$theta,
                                  info = info_test_random_theta[[i]]$test_info_curve, 
                                  num_item = paste0("number", 
                                                    nrow(info_test_random_theta[[i]]$info_curves_item)), 
                                  sel = "random_theta")
  random_theta_data <- rbind(temp_random_theta, 
                             random_theta_data)
}

# seleziona sono una delle curve per ogni numerosità ------

temp = NULL
new_random_theta = NULL
for (i in 1:length(unique(random_theta_data$num_item))) {
  temp = random_theta_data[random_theta_data$num_item %in% unique(random_theta_data$num_item)[i], ]
  temp = temp[1:1000, ]
  new_random_theta = rbind(new_random_theta, temp)
}
# calcola l'info media 

# mean_random_theta <- aggregate(info ~ theta + num_item, data = random_theta_data, mean)
# mean_random_theta <- mean_random_theta[, c("theta", "info", "num_item")]
# mean_random_theta$sel <- "random_theta"
# start_data <- data.frame(theta = IRT.informationCurves(m2pl, 
#                                                        theta = seq(-3, 3, 
#                                                                    length = 1000))$theta,
#                          info = IRT.informationCurves(m2pl, 
#                                                       theta = seq(-3, 3, 
#                                                                   length = 1000))$test_info_curve, 
#                          num_item = "all", 
#                          sel = "start")
# 
data_info_theta <- rbind(
  cluster_theta_data,
  range_theta_data, range_new_theta_data, 
  smart_theta_data , new_random_theta# mean_random_theta
)


ggplot(rbind(data_info_theta[data_info_theta$num_item %in% "number10", ], 
             graph_start_normal), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 2)

plots_theta <- list()
for(i in 1:length(unique(data_info_theta$num_item))) {
  
  plots_theta[[i]] <-  ggplot(rbind(data_info_theta[data_info_theta$num_item %in% unique(data_info_theta$num_item)[i], ],
                                    graph_start_normal), 
                              aes(x = theta, y = info, group = sel, 
                                  col = sel)) + geom_line(aes(linetype = sel), lwd = 1.3)  + 
    ggtitle(unique(data_info_theta$num_item)[i]) +
    theme(legend.position = "none")
}

do.call(grid.arrange, plots_theta)

# bias stime di theta per i parametri fissi degli item distr normale con parametri fissi ---- 

for (i in 1:length(model_fit_random_theta)) {
  names(model_fit_random_theta)[[i]] <- paste0("number", 
                                         nrow(model_fit_random_theta[[i]]$xsi))
}



temp <- NULL
random_theta_theta <- NULL
list_temp <- NULL
theta_random_normal_theta <- list()

for (i in 1:length(unique(names(model_fit_random_theta)))) {
  random_theta_theta <- NULL
  temp <- model_fit_random_theta[names(model_fit_random_theta) == unique(names(model_fit_random_theta))[i]]
  for (j in 1:length(temp)) {
    list_temp <- temp[[j]]$person$EAP
    random_theta_theta <- data.frame(cbind(random_theta_theta, list_temp))
    theta_random_normal_theta[[i]] <- random_theta_theta
    theta_random_normal_theta[[i]] = cbind(theta_random_normal_theta[[i]], theta_obs)
    theta_random_normal_theta[[i]] = cbind(theta_random_normal_theta[[i]], theta_all)
    names(theta_random_normal_theta)[[i]] <- unique(names(model_fit_random_theta))[i]

  }
}

# siccome prima ho preso la prima selezione di item per ognuna delle 
# numerosità, prendo la prima selezione per ognuno


temp_theta_random_theta = data.frame(matrix(ncol = length(unique(names(theta_random_normal_theta))), 
                                      nrow = 1000))
for (i in 1:length(theta_random_normal_theta)) {
  temp_theta_random_theta[,i] = theta_random_normal_theta[[i]][,1]
  colnames(temp_theta_random_theta)[i] <- unique(names(theta_random_normal_theta))[i]
}
 
# siccome prima ho preso la prima selezione di item per ognuna delle 
# numerosità, prendo la prima selezione per ognuno


temp_theta_random_theta = data.frame(matrix(ncol = length(unique(names(theta_random_normal_theta))), 
                                      nrow = 1000))
for (i in 1:length(theta_random_normal_theta)) {
  temp_theta_random_theta[,i] = theta_random_normal_theta[[i]][,1]
  colnames(temp_theta_random_theta)[i] <- unique(names(theta_random_normal_theta))[i]
}

# questa cosa non so cosa faccia
# for (i in 1:length(theta_random_normal_theta)) {
#   for(j in 1:(ncol(theta_random_normal_theta[[i]])- 2)) {
#     theta_random_normal_theta[[i]][, paste0("bias_obs", colnames(theta_random_normal_theta[[i]][j]))] = 
#       theta_random_normal_theta[[i]][,j] - theta_random_normal_theta[[i]][, "theta_obs"]
#     theta_random_normal_theta[[i]][, paste0("bias_all", colnames(theta_random_normal_theta[[i]][j]))] = 
#       theta_random_normal_theta[[i]][,j] - theta_random_normal_theta[[i]][, "theta_all"]
#   }
# }

# sbj = data.frame(sbj = 1:1000)
# devo fare la media attraverso el colonne per oguno dei number item 
# temp_all = NULL
# temp_obs = NULL
# temp_random_bias_all_theta = data.frame(matrix(ncol = length(unique(names(theta_random_normal_theta))), 
#                                                nrow = 1000))
# temp_random_bias_obs_theta = data.frame(matrix(ncol = length(unique(names(theta_random_normal_theta))), 
#                                                nrow = 1000))
# 
# for (i in 1:length(theta_random_normal_theta)) {
#   temp_all = theta_random_normal_theta[[i]][, grepl("bias_all", 
#                                                     colnames(theta_random_normal_theta[[i]]))]
#   temp_obs = theta_random_normal_theta[[i]][, grepl("bias_obs", 
#                                                     colnames(theta_random_normal_theta[[i]]))]
#   temp_random_bias_all_theta[, i] = c(rowMeans(temp_all))
#   colnames(temp_random_bias_all_theta)[i] = unique(names(theta_random_normal_theta))[i]
#   
#   temp_random_bias_obs_theta[, i] = c(rowMeans(temp_obs))
#   colnames(temp_random_bias_obs_theta)[i] = unique(names(theta_random_normal_theta))[i]
#   
# }

# ora devo girare in long i due dataset di bias per il bias osservato e per il bias 
# con tutti gli item, una volta girati li unisco. A questo punto è impossibile fare il 
# robind con gli altri data set ma ci penso dopo 
random = data.frame(selection = rep("random", nrow(temp_theta_random_theta)))
# temp_random_bias_all_theta = cbind(sbj, random, temp_random_bias_all_theta)
# temp_random_bias_obs_theta = cbind(sbj, random, temp_random_bias_obs_theta)

sbj <- data.frame(sbj = 1:1000)
temp_theta_random_theta = cbind(sbj, random, temp_theta_random_theta)
theta_random_normal_theta <- reshape(temp_theta_random_theta, 
                               idvar = "sbj", 
                               varying = list(3:(ncol(temp_theta_random_theta))), 
                               v.names = "theta_est", 
                               direction = "long", 
                               times = (names(temp_theta_random_theta)[-c(1:2)]), 
                               timevar = "num_item")
theta_random_normal_theta$obs <- theta_obs
theta_random_normal_theta$all <- theta_all
head(theta_random_normal_theta)

# theta_random_normal_theta_all = reshape(temp_random_bias_all_theta, 
#                                         idvar = "sbj", 
#                                         varying = list(3:(ncol(temp_random_bias_all_theta))), 
#                                         v.names = "bias_all", 
#                                         direction = "long", 
#                                         times = (names(temp_random_bias_all_theta)[-c(1:2)]), 
#                                         timevar = "num_item")
# theta_random_normal_theta_obs = reshape(temp_random_bias_obs_theta, 
#                                         idvar = "sbj", 
#                                         varying = list(3:(ncol(temp_random_bias_obs_theta))), 
#                                         v.names = "bias_obs", 
#                                         direction = "long", 
#                                         times = (names(temp_random_bias_obs_theta)[-c(1:2)]), 
#                                         timevar = "num_item")

# questo c'è e ha il bias MEDIO ottenuto dalla strategia random, sia per i valori osservati, 
# sia per i valori ottenuti con tutti gli item
# theta_random_normal_theta = merge(theta_random_normal_theta_all, 
#                                   theta_random_normal_theta_obs)
# 
# for (i in 1:length(unique(names(theta_random_normal_theta)))) {
#   temp_theta_random_theta[, i] <- c(rowMeans(theta_random_normal_theta[[i]]))
#   colnames(temp_theta_random_theta)[i] <- unique(names(theta_random_normal_theta))[i]
# }
# 
# theta_obs_theta = true_theta
# theta_all_theta = m2pl$person$EAP
# temp_theta_random_theta = cbind(sbj, random, temp_theta_random_theta)
# theta_random_normal_theta <- reshape(temp_theta_random_theta, 
#                                idvar = "sbj", 
#                                varying = list(3:(ncol(temp_theta_random_theta))), 
#                                v.names = "theta_est", 
#                                direction = "long", 
#                                times = (names(temp_theta_random_theta)[-c(1:2)]), 
#                                timevar = "num_item")
# theta_random_normal_theta$obs <- theta_obs_theta
# theta_random_normal_theta$all <- theta_all_theta
# head(theta_random_normal_theta)

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

temp_theta_cluster_normal_theta <- data.frame(matrix(ncol = length(unique(names(model_out_cluster_theta))), 
                                               nrow = 1000))

temp_theta_range_normal_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_theta))), 
                                             nrow = 1000))

temp_theta_range_new_normal_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_new_theta))), 
                                                   nrow = 1000))

temp_theta_smart_normal_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_theta))), 
                                             nrow = 1000))

for (i in 1:length(unique(names(model_out_cluster_theta)))) {
  temp_theta_cluster_normal_theta[, i] <- model_out_cluster_theta[[i]]$person$EAP
  colnames(temp_theta_cluster_normal_theta)[i] <- unique(names(model_out_cluster_theta))[i]
  
  temp_theta_range_normal_theta[, i] <- model_out_range_theta[[i]]$person$EAP
  colnames(temp_theta_range_normal_theta)[i] <- unique(names(model_out_range_theta))[i]
  
  temp_theta_range_new_normal_theta[, i] <- model_out_range_new_theta[[i]]$person$EAP
  colnames(temp_theta_range_new_normal_theta)[i] <- unique(names(model_out_range_new_theta))[i]
  
  temp_theta_smart_normal_theta[, i] <- model_out_smart_theta[[i]]$person$EAP
  colnames(temp_theta_smart_normal_theta)[i] <- unique(names(model_out_smart_theta))[i]
  
}


sbj <- data.frame(sbj =1:1000)

cluster = data.frame(selection = rep("cluster", nrow(sbj)))
range = data.frame(selection = rep("range", nrow(sbj)))
range_new = data.frame(selection = rep("rangeNEW", nrow(sbj)))
smart = data.frame(selection = rep("smart", nrow(sbj)))

theta_obs = true_theta
theta_all = m2pl$person$EAP

temp_theta_cluster_normal_theta <- cbind(sbj, cluster,  temp_theta_cluster_normal_theta)

theta_cluster_normal_theta <- reshape(temp_theta_cluster_normal_theta, 
                                idvar = "sbj", 
                                varying = list(3:(ncol(temp_theta_cluster_normal_theta))), 
                                v.names = "theta_est", 
                                direction = "long", 
                                times = (names(temp_theta_cluster_normal_theta)[-c(1:2)]), 
                                timevar = "num_item")
theta_cluster_normal_theta$obs <- theta_obs
theta_cluster_normal_theta$all <- theta_all
head(theta_cluster_normal_theta)

temp_theta_range_normal_theta <- cbind(sbj, range,  temp_theta_range_normal_theta)

theta_range_normal_theta <- reshape(temp_theta_range_normal_theta, 
                              idvar = "sbj", 
                              varying = list(3:(ncol(temp_theta_range_normal_theta))), 
                              v.names = "theta_est", 
                              direction = "long", 
                              times = (names(temp_theta_range_normal_theta)[-c(1:2)]), 
                              timevar = "num_item")
theta_range_normal_theta$obs <- theta_obs
theta_range_normal_theta$all <- theta_all
head(theta_range_normal_theta)

temp_theta_range_new_normal_theta <- cbind(sbj, range_new,  temp_theta_range_new_normal_theta)

theta_range_new_normal_theta <- reshape(temp_theta_range_new_normal_theta, 
                                    idvar = "sbj", 
                                    varying = list(3:(ncol(temp_theta_range_new_normal_theta))), 
                                    v.names = "theta_est", 
                                    direction = "long", 
                                    times = (names(temp_theta_range_new_normal_theta)[-c(1:2)]), 
                                    timevar = "num_item")
theta_range_new_normal_theta$obs <- theta_obs
theta_range_new_normal_theta$all <- theta_all
head(theta_range_new_normal_theta)

temp_theta_smart_normal_theta <- cbind(sbj, smart,  temp_theta_smart_normal_theta)

theta_smart_normal_theta <- reshape(temp_theta_smart_normal_theta, 
                              idvar = "sbj", 
                              varying = list(3:(ncol(temp_theta_smart_normal_theta))), 
                              v.names = "theta_est", 
                              direction = "long", 
                              times = (names(temp_theta_smart_normal_theta)[-c(1:2)]), 
                              timevar = "num_item")
theta_smart_normal_theta$obs <- theta_obs
theta_smart_normal_theta$all <- theta_all
head(theta_smart_normal_theta)

# random obs ----
theta_random_normal_theta$bias_obs = with(theta_random_normal_theta, 
                                          theta_est - obs)
theta_random_normal_theta$bias_obs_sq = theta_random_normal_theta$bias_obs^2 
theta_random_normal_theta$bias_obs_abs = with(theta_random_normal_theta, 
                                              abs(theta_est - obs))
theta_random_normal_theta$bias_all = with(theta_random_normal_theta, 
                                          theta_est - all)
theta_random_normal_theta$bias_all_sq = theta_random_normal_theta$bias_all^2 
theta_random_normal_theta$bias_all_abs = with(theta_random_normal_theta, 
                                              abs(theta_est - all))

random_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_random_normal_theta, 
                                  mean)
random_bias_obs_theta$selection = "random"
random_bias_obs_theta$type = "bias_obs"
random_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_random_normal_theta, 
                            mean)
random_rmse_obs_theta$selection = "random"
random_rmse_obs_theta$type = "rmse_obs"

random_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_random_normal_theta, 
                                      mean)
random_bias_obs_theta_abs$selection = "random"
random_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_random_normal_theta$group <- ifelse(theta_random_normal_theta$obs  <= cut_val[1, "start"], 
                                          group_name[1], 
                                          ifelse(theta_random_normal_theta$obs > cut_val[1, "start"] & theta_random_normal_theta$obs <= cut_val[1, "end"], 
                                                 group_name[2], 
                                                 ifelse(theta_random_normal_theta$obs > cut_val[2, "start"] & theta_random_normal_theta$obs <= cut_val[2, "end"], 
                                                        group_name[3], 
                                                        ifelse(theta_random_normal_theta$obs > cut_val[3, "start"] & theta_random_normal_theta$obs <= cut_val[3, "end"], 
                                                               group_name[4], 
                                                               ifelse(theta_random_normal_theta$obs > cut_val[4, "start"] & theta_random_normal_theta$obs <= cut_val[4, "end"], 
                                                                      group_name[5], 
                                                                      ifelse(theta_random_normal_theta$obs > cut_val[4, "end"], 
                                                                             group_name[6], "error")
                                                               )))))
random_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                    data = theta_random_normal_theta, 
                                    mean)

random_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                        data = theta_random_normal_theta, 
                                        mean)

random_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                        data = theta_random_normal_theta, 
                                        mean)

random_bias_group_theta$selection = "random"
random_bias_abs_group_theta$selection = "random"
random_rmse_obs_group_theta$selection = "random"

# random all 

theta_random_normal_theta$bias_all = with(theta_random_normal_theta, 
                                          theta_est - all)
theta_random_normal_theta$bias_all_sq = theta_random_normal_theta$bias_all^2 
theta_random_normal_theta$bias_all_abs = with(theta_random_normal_theta, 
                                              abs(theta_est - all))
theta_random_normal_theta$bias_all = with(theta_random_normal_theta, 
                                          theta_est - all)
theta_random_normal_theta$bias_all_sq = theta_random_normal_theta$bias_all^2 
theta_random_normal_theta$bias_all_abs = with(theta_random_normal_theta, 
                                              abs(theta_est - all))

random_bias_all_theta = aggregate(bias_all ~num_item, data = theta_random_normal_theta, 
                                  mean)
random_bias_all_theta$selection = "random"
random_bias_all_theta$type = "bias_all"
random_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_random_normal_theta, 
                            mean)
random_rmse_all_theta$selection = "random"
random_rmse_all_theta$type = "rmse_all"
random_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_random_normal_theta, 
                                      mean)
random_bias_all_theta_abs$selection = "random"
random_bias_all_theta_abs$type = "bias_all_abs"


# cluster obs ----
theta_cluster_normal_theta$bias_obs = with(theta_cluster_normal_theta, 
                                           theta_est - obs)
theta_cluster_normal_theta$bias_obs_sq = theta_cluster_normal_theta$bias_obs^2 
theta_cluster_normal_theta$bias_obs_abs = with(theta_cluster_normal_theta, 
                                               abs(theta_est - obs))
theta_cluster_normal_theta$bias_all = with(theta_cluster_normal_theta, 
                                           theta_est - all)
theta_cluster_normal_theta$bias_all_sq = theta_cluster_normal_theta$bias_all^2 
theta_cluster_normal_theta$bias_all_abs = with(theta_cluster_normal_theta, 
                                               abs(theta_est - all))

cluster_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_cluster_normal_theta, 
                                   mean)
cluster_bias_obs_theta$selection = "cluster"
cluster_bias_obs_theta$type = "bias_obs"
cluster_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, 
                                         data = theta_cluster_normal_theta, 
                             mean)
cluster_rmse_obs_theta$selection = "cluster"
cluster_rmse_obs_theta$type = "rmse_obs"
cluster_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_cluster_normal_theta, 
                                       mean)
cluster_bias_obs_theta_abs$selection = "cluster"
cluster_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_cluster_normal_theta$group <- ifelse(theta_cluster_normal_theta$obs  <= cut_val[1, "start"], 
                                           group_name[1], 
                                           ifelse(theta_cluster_normal_theta$obs > cut_val[1, "start"] & theta_cluster_normal_theta$obs <= cut_val[1, "end"], 
                                                  group_name[2], 
                                                  ifelse(theta_cluster_normal_theta$obs > cut_val[2, "start"] & theta_cluster_normal_theta$obs <= cut_val[2, "end"], 
                                                         group_name[3], 
                                                         ifelse(theta_cluster_normal_theta$obs > cut_val[3, "start"] & theta_cluster_normal_theta$obs <= cut_val[3, "end"], 
                                                                group_name[4], 
                                                                ifelse(theta_cluster_normal_theta$obs > cut_val[4, "start"] & theta_cluster_normal_theta$obs <= cut_val[4, "end"], 
                                                                       group_name[5], 
                                                                       ifelse(theta_cluster_normal_theta$obs > cut_val[4, "end"], 
                                                                              group_name[6], "error")
                                                                )))))
cluster_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                     data = theta_cluster_normal_theta, 
                                     mean)

cluster_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                         data = theta_cluster_normal_theta, 
                                         mean)

cluster_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                         data = theta_cluster_normal_theta, 
                                         mean)

cluster_bias_group_theta$selection = "cluster"
cluster_bias_abs_group_theta$selection = "cluster"
cluster_rmse_obs_group_theta$selection = "cluster"

# cluster all 

theta_cluster_normal_theta$bias_all = with(theta_cluster_normal_theta, 
                                           theta_est - all)
theta_cluster_normal_theta$bias_all_sq = theta_cluster_normal_theta$bias_all^2 
theta_cluster_normal_theta$bias_all_abs = with(theta_cluster_normal_theta, 
                                               abs(theta_est - all))
theta_cluster_normal_theta$bias_all = with(theta_cluster_normal_theta, 
                                           theta_est - all)
theta_cluster_normal_theta$bias_all_sq = theta_cluster_normal_theta$bias_all^2 
theta_cluster_normal_theta$bias_all_abs = with(theta_cluster_normal_theta, 
                                               abs(theta_est - all))

cluster_bias_all_theta = aggregate(bias_all ~num_item, data = theta_cluster_normal_theta, 
                                   mean)
cluster_bias_all_theta$selection = "cluster"
cluster_bias_all_theta$type = "bias_all"
cluster_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_cluster_normal_theta, 
                             mean)
cluster_rmse_all_theta$selection = "cluster"
cluster_rmse_all_theta$type = "rmse_all"
cluster_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_cluster_normal_theta, 
                                       mean)
cluster_bias_all_theta_abs$selection = "cluster"
cluster_bias_all_theta_abs$type = "bias_all_abs"

# range obs ----
theta_range_normal_theta$bias_obs = with(theta_range_normal_theta, 
                                         theta_est - obs)
theta_range_normal_theta$bias_obs_sq = theta_range_normal_theta$bias_obs^2 
theta_range_normal_theta$bias_obs_abs = with(theta_range_normal_theta, 
                                             abs(theta_est - obs))
theta_range_normal_theta$bias_all = with(theta_range_normal_theta, 
                                         theta_est - all)
theta_range_normal_theta$bias_all_sq = theta_range_normal_theta$bias_all^2 
theta_range_normal_theta$bias_all_abs = with(theta_range_normal_theta, 
                                             abs(theta_est - all))

range_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_range_normal_theta, 
                                 mean)
range_bias_obs_theta$selection = "range"
range_bias_obs_theta$type = "bias_obs"
range_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_range_normal_theta, 
                           mean)
range_rmse_obs_theta$selection = "range"
range_rmse_obs_theta$type = "rmse_obs"
range_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_range_normal_theta, 
                                     mean)
range_bias_obs_theta_abs$selection = "range"
range_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_range_normal_theta$group <- ifelse(theta_range_normal_theta$obs  <= cut_val[1, "start"], 
                                         group_name[1], 
                                         ifelse(theta_range_normal_theta$obs > cut_val[1, "start"] & theta_range_normal_theta$obs <= cut_val[1, "end"], 
                                                group_name[2], 
                                                ifelse(theta_range_normal_theta$obs > cut_val[2, "start"] & theta_range_normal_theta$obs <= cut_val[2, "end"], 
                                                       group_name[3], 
                                                       ifelse(theta_range_normal_theta$obs > cut_val[3, "start"] & theta_range_normal_theta$obs <= cut_val[3, "end"], 
                                                              group_name[4], 
                                                              ifelse(theta_range_normal_theta$obs > cut_val[4, "start"] & theta_range_normal_theta$obs <= cut_val[4, "end"], 
                                                                     group_name[5], 
                                                                     ifelse(theta_range_normal_theta$obs > cut_val[4, "end"], 
                                                                            group_name[6], "error")
                                                              )))))
range_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                   data = theta_range_normal_theta, 
                                   mean)

range_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                       data = theta_range_normal_theta, 
                                       mean)

range_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                       data = theta_range_normal_theta, 
                                       mean)

range_bias_group_theta$selection = "range"
range_bias_abs_group_theta$selection = "range"
range_rmse_obs_group_theta$selection = "range"

# range all 

theta_range_normal_theta$bias_all = with(theta_range_normal_theta, 
                                         theta_est - all)
theta_range_normal_theta$bias_all_sq = theta_range_normal_theta$bias_all^2 
theta_range_normal_theta$bias_all_abs = with(theta_range_normal_theta, 
                                             abs(theta_est - all))
theta_range_normal_theta$bias_all = with(theta_range_normal_theta, 
                                         theta_est - all)
theta_range_normal_theta$bias_all_sq = theta_range_normal_theta$bias_all^2 
theta_range_normal_theta$bias_all_abs = with(theta_range_normal_theta, 
                                             abs(theta_est - all))

range_bias_all_theta = aggregate(bias_all ~num_item, data = theta_range_normal_theta, 
                                 mean)
range_bias_all_theta$selection = "range"
range_bias_all_theta$type = "bias_all"
range_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_range_normal_theta, 
                           mean)
range_rmse_all_theta$selection = "range"
range_rmse_all_theta$type = "rmse_all"
range_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_range_normal_theta, 
                                     mean)
range_bias_all_theta_abs$selection = "range"
range_bias_all_theta_abs$type = "bias_all_abs"

# range_new obs ----
theta_range_new_normal_theta$bias_obs = with(theta_range_new_normal_theta, 
                                             theta_est - obs)
theta_range_new_normal_theta$bias_obs_sq = theta_range_new_normal_theta$bias_obs^2 
theta_range_new_normal_theta$bias_obs_abs = with(theta_range_new_normal_theta, 
                                                 abs(theta_est - obs))
theta_range_new_normal_theta$bias_all = with(theta_range_new_normal_theta, 
                                             theta_est - all)
theta_range_new_normal_theta$bias_all_sq = theta_range_new_normal_theta$bias_all^2 
theta_range_new_normal_theta$bias_all_abs = with(theta_range_new_normal_theta, 
                                                 abs(theta_est - all))

range_new_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_range_new_normal_theta, 
                                     mean)
range_new_bias_obs_theta$selection = "range_new"
range_new_bias_obs_theta$type = "bias_obs"
range_new_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_range_new_normal_theta, 
                               mean)
range_new_rmse_obs_theta$selection = "range_new"
range_new_rmse_obs_theta$type = "rmse_obs"
range_new_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_range_new_normal_theta, 
                                         mean)
range_new_bias_obs_theta_abs$selection = "range_new"
range_new_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_range_new_normal_theta$group <- ifelse(theta_range_new_normal_theta$obs  <= cut_val[1, "start"], 
                                             group_name[1], 
                                             ifelse(theta_range_new_normal_theta$obs > cut_val[1, "start"] & theta_range_new_normal_theta$obs <= cut_val[1, "end"], 
                                                    group_name[2], 
                                                    ifelse(theta_range_new_normal_theta$obs > cut_val[2, "start"] & theta_range_new_normal_theta$obs <= cut_val[2, "end"], 
                                                           group_name[3], 
                                                           ifelse(theta_range_new_normal_theta$obs > cut_val[3, "start"] & theta_range_new_normal_theta$obs <= cut_val[3, "end"], 
                                                                  group_name[4], 
                                                                  ifelse(theta_range_new_normal_theta$obs > cut_val[4, "start"] & theta_range_new_normal_theta$obs <= cut_val[4, "end"], 
                                                                         group_name[5], 
                                                                         ifelse(theta_range_new_normal_theta$obs > cut_val[4, "end"], 
                                                                                group_name[6], "error")
                                                                  )))))
range_new_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                       data = theta_range_new_normal_theta, 
                                       mean)

range_new_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                           data = theta_range_new_normal_theta, 
                                           mean)

range_new_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                           data = theta_range_new_normal_theta, 
                                           mean)

range_new_bias_group_theta$selection = "range_new"
range_new_bias_abs_group_theta$selection = "range_new"
range_new_rmse_obs_group_theta$selection = "range_new"

# range_new all 

theta_range_new_normal_theta$bias_all = with(theta_range_new_normal_theta, 
                                             theta_est - all)
theta_range_new_normal_theta$bias_all_sq = theta_range_new_normal_theta$bias_all^2 
theta_range_new_normal_theta$bias_all_abs = with(theta_range_new_normal_theta, 
                                                 abs(theta_est - all))
theta_range_new_normal_theta$bias_all = with(theta_range_new_normal_theta, 
                                             theta_est - all)
theta_range_new_normal_theta$bias_all_sq = theta_range_new_normal_theta$bias_all^2 
theta_range_new_normal_theta$bias_all_abs = with(theta_range_new_normal_theta, 
                                                 abs(theta_est - all))

range_new_bias_all_theta = aggregate(bias_all ~num_item, data = theta_range_new_normal_theta, 
                                     mean)
range_new_bias_all_theta$selection = "range_new"
range_new_bias_all_theta$type = "bias_all"
range_new_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_range_new_normal_theta, 
                               mean)
range_new_rmse_all_theta$selection = "range_new"
range_new_rmse_all_theta$type = "rmse_all"
range_new_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_range_new_normal_theta, 
                                         mean)
range_new_bias_all_theta_abs$selection = "range_new"
range_new_bias_all_theta_abs$type = "bias_all_abs"

# smart obs ----
theta_smart_normal_theta$bias_obs = with(theta_smart_normal_theta, 
                                         theta_est - obs)
theta_smart_normal_theta$bias_obs_sq = theta_smart_normal_theta$bias_obs^2 
theta_smart_normal_theta$bias_obs_abs = with(theta_smart_normal_theta, 
                                             abs(theta_est - obs))
theta_smart_normal_theta$bias_all = with(theta_smart_normal_theta, 
                                         theta_est - all)
theta_smart_normal_theta$bias_all_sq = theta_smart_normal_theta$bias_all^2 
theta_smart_normal_theta$bias_all_abs = with(theta_smart_normal_theta, 
                                             abs(theta_est - all))

smart_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_smart_normal_theta, 
                                 mean)
smart_bias_obs_theta$selection = "smart"
smart_bias_obs_theta$type = "bias_obs"
smart_rmse_obs_theta = aggregate(bias_obs_sq ~num_item, data = theta_smart_normal_theta, 
                           mean)
smart_rmse_obs_theta$selection = "smart"
smart_rmse_obs_theta$type = "rmse_obs"
smart_bias_obs_theta_abs = aggregate(bias_obs_abs ~num_item, data = theta_smart_normal_theta, 
                                     mean)
smart_bias_obs_theta_abs$selection = "smart"
smart_bias_obs_theta_abs$type = "bias_obs_abs"

# gruppi latenti 
theta_lat <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat, length(theta_lat), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name <- letters[1:(nrow(cut_val)+2)]


theta_smart_normal_theta$group <- ifelse(theta_smart_normal_theta$obs  <= cut_val[1, "start"], 
                                         group_name[1], 
                                         ifelse(theta_smart_normal_theta$obs > cut_val[1, "start"] & theta_smart_normal_theta$obs <= cut_val[1, "end"], 
                                                group_name[2], 
                                                ifelse(theta_smart_normal_theta$obs > cut_val[2, "start"] & theta_smart_normal_theta$obs <= cut_val[2, "end"], 
                                                       group_name[3], 
                                                       ifelse(theta_smart_normal_theta$obs > cut_val[3, "start"] & theta_smart_normal_theta$obs <= cut_val[3, "end"], 
                                                              group_name[4], 
                                                              ifelse(theta_smart_normal_theta$obs > cut_val[4, "start"] & theta_smart_normal_theta$obs <= cut_val[4, "end"], 
                                                                     group_name[5], 
                                                                     ifelse(theta_smart_normal_theta$obs > cut_val[4, "end"], 
                                                                            group_name[6], "error")
                                                              )))))
smart_bias_group_theta = aggregate(bias_obs ~num_item + group, 
                                   data = theta_smart_normal_theta, 
                                   mean)

smart_bias_abs_group_theta = aggregate(bias_obs_abs ~num_item + group, 
                                       data = theta_smart_normal_theta, 
                                       mean)

smart_rmse_obs_group_theta = aggregate(bias_obs_sq ~num_item + group, 
                                       data = theta_smart_normal_theta, 
                                       mean)

smart_bias_group_theta$selection = "smart"
smart_bias_abs_group_theta$selection = "smart"
smart_rmse_obs_group_theta$selection = "smart"

# smart all 

theta_smart_normal_theta$bias_all = with(theta_smart_normal_theta, 
                                         theta_est - all)
theta_smart_normal_theta$bias_all_sq = theta_smart_normal_theta$bias_all^2 
theta_smart_normal_theta$bias_all_abs = with(theta_smart_normal_theta, 
                                             abs(theta_est - all))
theta_smart_normal_theta$bias_all = with(theta_smart_normal_theta, 
                                         theta_est - all)
theta_smart_normal_theta$bias_all_sq = theta_smart_normal_theta$bias_all^2 
theta_smart_normal_theta$bias_all_abs = with(theta_smart_normal_theta, 
                                             abs(theta_est - all))

smart_bias_all_theta = aggregate(bias_all ~num_item, data = theta_smart_normal_theta, 
                                 mean)
smart_bias_all_theta$selection = "smart"
smart_bias_all_theta$type = "bias_all"
smart_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_smart_normal_theta, 
                           mean)
smart_rmse_all_theta$selection = "smart"
smart_rmse_all_theta$type = "rmse_all"
smart_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_smart_normal_theta, 
                                     mean)
smart_bias_all_theta_abs$selection = "smart"
smart_bias_all_theta_abs$type = "bias_all_abs"


# ora unisco i bias con i bias e gli rmse con gli rmse 

normal_bias_obs_theta = rbind(random_bias_obs_theta, 
                        cluster_bias_obs_theta, 
                        range_new_bias_obs_theta, 
                        range_bias_obs_theta, 
                        smart_bias_obs_theta)

ggplot(normal_bias_obs_theta, 
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

normal_bias_abs_groups_theta = rbind(random_bias_abs_group_theta, 
                               smart_bias_abs_group_theta, 
                               cluster_bias_abs_group_theta, 
                               range_bias_abs_group_theta, 
                               range_new_bias_abs_group_theta)

ggplot(normal_bias_abs_groups, 
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

# theta_normal_theta <- rbind(theta_cluster_normal_theta, 
#                       theta_range_normal_theta,  theta_range_new_normal_theta, 
#                       theta_smart_normal_theta, theta_random_normal_theta
# )
# 
# 
# theta_normal_theta$distribution <- "normal"
# 
# theta_normal_theta$bias_obs <- with(theta_normal_theta, 
#                                     theta_est - theta_obs)
# theta_normal_theta$bias_obs_abs <- with(theta_normal_theta, 
#                                         abs(theta_est - theta_obs))
# 
# theta_normal_theta$bias_all <- with(theta_normal_theta, 
#                                     (theta_est - theta_all))
# 
# theta_normal_theta$bias_all_abs <- with(theta_normal_theta, 
#                                         abs(theta_est - theta_all))
# 
# 
# # preparo il dataset in modo da poter unire la random comodamente
# 
# obs_bias_normal_theta <- aggregate(bias_obs ~ num_item + selection, 
#                                    data = theta_normal_theta, 
#                                    mean)
# 
# obs_bias_normal_theta$type <- "bias"
# obs_bias_normal_theta_abs <- aggregate(bias_obs_abs ~ num_item + selection, 
#                                        data = theta_normal_theta, 
#                                        mean)
# obs_bias_normal_theta_abs$type <- "bias_abs"
# colnames(obs_bias_normal_theta_abs)[3] <- "bias_obs"
# 
# obs_bias_normal_theta <- rbind(obs_bias_normal_theta, obs_bias_normal_theta_abs)
# 
# obs_bias_normal_theta$temp <- as.integer(gsub("number", "", 
#                                               obs_bias_normal_theta$num_item))
# obs_bias_normal_theta = obs_bias_normal_theta[order(obs_bias_normal_theta$temp), ]
# ggplot(obs_bias_normal_theta, 
#        aes(x=as.factor(temp), y = bias_obs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_grid(~type)
# 
# 
# all_bias_normal_theta <- aggregate(bias_all ~ num_item + selection, 
#                                    data = theta_normal_theta, 
#                                    mean)
# all_bias_normal_theta$type <- "bias"
# all_bias_normal_theta_abs <- aggregate(abs(bias_all) ~ num_item + selection, 
#                                        data = theta_normal_theta, 
#                                        mean)
# all_bias_normal_theta_abs$type <- "bias_abs"
# colnames(all_bias_normal_theta_abs)[3] <- "bias_all"
# 
# all_bias_normal_theta <- rbind(all_bias_normal_theta, all_bias_normal_theta_abs)
# 
# all_bias_normal_theta$temp <- as.integer(gsub("number", "", 
#                                               all_bias_normal_theta$num_item))
# all_bias_normal_theta = all_bias_normal_theta[order(all_bias_normal_theta$temp), ]
# ggplot(all_bias_normal_theta, 
#        aes(x=as.factor(temp), y = bias_all, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(~type)
# # calcolo rmsea theta con stimoli fissi -----
# 
# theta_normal_theta$bias_obs_sq <- theta_normal_theta$bias_obs^2
# 
# theta_normal_theta$bias_all_sq <- theta_normal_theta$bias_all^2
# 
# obs_bias_normal_sq_theta <- aggregate(bias_obs_sq ~ num_item + selection, 
#                                 data = theta_normal_theta, 
#                                 mean)
# obs_bias_normal_sq_theta$type <- "bias_sq_obs"
# obs_bias_normal_sq_theta$rmsea = sqrt(obs_bias_normal_sq_theta$bias_obs_sq)
# 
# ggplot(obs_bias_normal_sq_theta, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 
# 
# 
# all_bias_normal_sq_theta <- aggregate(bias_all_sq ~ num_item + selection, 
#                                 data = theta_normal_theta, 
#                                 mean)
# all_bias_normal_sq_theta$type <- "bias_sq_all"
# all_bias_normal_sq_theta$rmsea = sqrt(all_bias_normal_sq_theta$bias_all)
# 
# ggplot(all_bias_normal_sq_theta, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 
# 
# # con gruppi latenti normal theta  -----
# theta_lat_theta <- seq(-2.5, 2.5, length.out = 4) 
# g <- cut(theta_lat_theta, length(theta_lat_theta), include.lowest = TRUE)
# cut_val = cut_borders(g)
# 
# 
# 
# group_name <- letters[1:(nrow(cut_val)+2)]
# 
# theta_normal_theta$group <- ifelse(theta_normal_theta$obs  <= cut_val[1, "start"], 
#                                    group_name[1], 
#                                    ifelse(theta_normal_theta$obs > cut_val[1, "start"] & theta_normal_theta$obs <= cut_val[1, "end"], 
#                                           group_name[2], 
#                                           ifelse(theta_normal_theta$obs > cut_val[2, "start"] & theta_normal_theta$obs <= cut_val[2, "end"], 
#                                                  group_name[3], 
#                                                  ifelse(theta_normal_theta$obs > cut_val[3, "start"] & theta_normal_theta$obs <= cut_val[3, "end"], 
#                                                         group_name[4], 
#                                                         ifelse(theta_normal_theta$obs > cut_val[4, "start"] & theta_normal_theta$obs <= cut_val[4, "end"], 
#                                                                group_name[5], group_name[6])))))
# 
# theta_normal_theta$bias_obs_abs = abs(theta_normal_theta$bias_obs)
# bias_abs_normal_group_theta <- aggregate(bias_obs_abs ~ selection + group + num_item, 
#                                          data = theta_normal_theta, mean)
# bias_abs_normal_group_theta$type = "bias"
# 
# ggplot(bias_abs_normal_group_theta, 
#        aes(x=group, y = bias_obs_abs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)
# 
# rmsea_normal_group_theta <- aggregate(bias_obs_sq ~ selection + group + num_item, 
#                                 data = theta_normal_theta, mean)
# rmsea_normal_group_theta$rmsea = sqrt(rmsea_normal_group_theta$bias_obs_sq)
# 
# ggplot(rmsea_normal_group_theta, 
#        aes(x=group, y = rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), 
#                                            lwd =1.3) + 
#   theme(legend.position = "top") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)


rm(list= ls()[!(ls() %in% c("m2pl",
                            'obs_bias_normal_theta',
                            'all_bias_normal_theta', 
                            "theta_normal_theta", 
                            "theta_normal",
                            "all_data_normal_theta",
                            "all_data_normal",
                            "data_info",
                            "data_info_theta",
                            'obs_bias_normal','all_bias_normal', 
                            "plots", "plots_theta", 
                            
                            "bias_abs_normal_group", 
                            "bias_abs_normal_group_theta", 
                            "obs_bias_normal_sq", "obs_bias_normal_sq_theta", 
                            "all_bias_normal_sq", "all_bias_normal_sq_theta", 
                            "rmsea_normal_group", "rmsea_normal_group_theta", 
                            "data_normal_random_unique", 
                            "data_normal_theta_random_unique", 
                            "normal_bias_obs",
                            "normal_bias_obs_abs",
                            "normal_bias_all",
                            "normal_bias_all_abs",
                            "normal_rmse_obs",
                            "normal_rmse_all",
                            "normal_rmse_groups",
                            "normal_bias_abs_groups",
                            "normal_bias_groups", 
                            "normal_bias_obs_theta", 
                            "normal_bias_obs_abs_theta", 
                            "normal_bias_all_theta", 
                            "normal_bias_all_abs_theta", 
                            "normal_rmse_obs_theta", 
                            "normal_rmse_all_theta", 
                            "normal_rmse_groups_theta", 
                            "normal_bias_abs_groups_theta", 
                            "normal_bias_groups_theta"))])

# Dati SKEWNEES ----
rm(list = ls())
load("SKrandom.RData")
load("SKsmart.RData")
load("SKguided.RData")
load("SKguidedNEW.RData")
load("SKcluster.RData")
# Stessa cosa item con theta sk -----
data_random_sk_summary$selection <- "random"
info_summary_range_new_sk$selection = "guidedNew"
all_data_sk <- rbind(data_random_sk_summary, 
                     data.frame(num_item = info_summary_range_sk$range_name, 
                                mean_info = info_summary_range_sk$info_test, 
                                sd_info = 0, 
                                selection = info_summary_range_sk$selection, 
                                mean_rel = info_summary_range_sk$rel, 
                                sd_rel = 0), 
                     data.frame(num_item = info_summary_range_new_sk$range_new_name, 
                                mean_info = info_summary_range_new_sk$info_test, 
                                sd_info = 0, 
                                selection = info_summary_range_new_sk$selection, 
                                mean_rel = info_summary_range_new_sk$rel, 
                                sd_rel = 0), 
                     data.frame(num_item = info_summary_cluster_sk$cluster_name, 
                                mean_info = info_summary_cluster_sk$info_test, 
                                sd_info = 0, 
                                selection = info_summary_cluster_sk$selection, 
                                mean_rel = info_summary_cluster_sk$rel, 
                                sd_rel = 0), 
                     
                     data.frame(num_item = info_summary_smart_sk$smart_name, 
                                mean_info = info_summary_smart_sk$info_test, 
                                sd_info = 0, 
                                selection = info_summary_smart_sk$selection, 
                                mean_rel = info_summary_smart_sk$rel, 
                                sd_rel = 0))
all_data_sk$item_temp <- gsub("number", "", all_data_sk$num_item)
all_data_sk$item_temp <- gsub("all", 0, all_data_sk$item_temp)
all_data_sk$item_temp <- as.integer(all_data_sk$item_temp)
all_data_sk <- all_data_sk[order(all_data_sk$item_temp), ]
all_data_sk$selection <- gsub("SK", '', all_data_sk$selection)
ggplot(all_data_sk[!all_data_sk$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_sk[!all_data_sk$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk[all_data_sk$num_item %in% "all", 
                                      "mean_info"])

# grafico info ski prendendo solo la prima combo di item da numerosità di item

sk_random = NULL 
temp = NULL
for(i in 1:length(unique(data_random_sk$num_item))) {
  temp = data_random_sk[data_random_sk$num_item %in% unique(data_random_sk$num_item)[i], ]
  temp = temp[1, ]
  sk_random = rbind(sk_random, temp)
}
sk_random$selection = "random" 
# unisco al data set 

data_sk_random_unique = rbind(data.frame(num_item = sk_random$num_item,
                                         mean_info = sk_random$info_total,
                                         sd_info = 0,
                                         selection = sk_random$selection,
                                         mean_rel = sk_random$rel,
                                         sd_rel = 0), 
                              data.frame(num_item = info_summary_range_sk$range_name, 
                                         mean_info = info_summary_range_sk$info_test, 
                                         sd_info = 0, 
                                         selection = info_summary_range_sk$selection, 
                                         mean_rel = info_summary_range_sk$rel, 
                                         sd_rel = 0), 
                              data.frame(num_item = info_summary_range_new_sk$range_new_name, 
                                         mean_info = info_summary_range_new_sk$info_test, 
                                         sd_info = 0, 
                                         selection = info_summary_range_new_sk$selection, 
                                         mean_rel = info_summary_range_new_sk$rel, 
                                         sd_rel = 0), 
                              data.frame(num_item = info_summary_cluster_sk$cluster_name, 
                                         mean_info = info_summary_cluster_sk$info_test, 
                                         sd_info = 0, 
                                         selection = info_summary_cluster_sk$selection, 
                                         mean_rel = info_summary_cluster_sk$rel, 
                                         sd_rel = 0), 
                              
                              data.frame(num_item = info_summary_smart_sk$smart_name, 
                                         mean_info = info_summary_smart_sk$info_test, 
                                         sd_info = 0, 
                                         selection = info_summary_smart_sk$selection, 
                                         mean_rel = info_summary_smart_sk$rel, 
                                         sd_rel = 0))
ggplot(data_sk_random_unique[!data_sk_random_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_sk[!all_data_sk$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk[all_data_sk$num_item %in% "all", 
                                      "mean_info"])


# reliability solo prima selezione ---- 
ggplot(data_sk_random_unique[!data_sk_random_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_sk[!all_data_sk$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk[all_data_sk$num_item %in% "all", 
                                      "mean_rel"])

# stesso garfico ma con le reliability 

ggplot(all_data_sk[!all_data_sk$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1) +
  geom_point(aes(shape=selection), size =2)+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels = unique(all_data_sk[!all_data_sk$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk[all_data_sk$num_item %in% "all", 
                                      "mean_rel"]) + ylim(0,1)

grid.arrange(ggplot(all_data_sk[!all_data_sk$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_info, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "top") + 
               scale_x_discrete(labels =  unique(all_data_sk[!all_data_sk$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_sk[all_data_sk$num_item %in% "all", 
                                                   "mean_info"]), 
             ggplot(all_data_sk[!all_data_sk$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_rel, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1) +
               geom_point(aes(shape=selection), size =2)+
               geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "none") + 
               scale_x_discrete(labels = unique(all_data_sk[!all_data_sk$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_sk[all_data_sk$num_item %in% "all", 
                                                   "mean_rel"]))

# confronto TIF dati sk -----

temp_cluster_sk<- NULL
cluster_data_sk <- NULL

temp_smart_sk <- NULL
smart_data_sk <- NULL


temp_range_sk <- NULL
range_data_sk  <- NULL

temp_range_new_sk <- NULL
range_new_data_sk  <- NULL


for (i in 1:length(info_out_smart_sk)) {
  temp_cluster_sk<- data.frame(theta = info_out_cluster_sk[[i]]$theta,
                               info = info_out_cluster_sk[[i]]$test_info_curve, 
                               num_item = names(info_out_cluster_sk)[[i]], 
                               sel = "cluster")
  cluster_data_sk <- rbind(temp_cluster_sk, 
                           cluster_data_sk)
  
  temp_smart_sk <- data.frame(theta = info_out_smart_sk[[i]]$theta,
                              info = info_out_smart_sk[[i]]$test_info_curve, 
                              num_item = names(info_out_smart_sk)[[i]], 
                              sel = "smart")
  smart_data_sk <- rbind(temp_smart_sk, 
                         smart_data_sk)
  temp_range_sk <- data.frame(theta = info_out_range_sk[[i]]$theta,
                              info = info_out_range_sk[[i]]$test_info_curve, 
                              num_item = names(info_out_range_sk)[[i]], 
                              sel = "guided")
  range_data_sk  <- rbind(temp_range_sk, 
                          range_data_sk )
  temp_range_new_sk <- data.frame(theta = info_out_range_new_sk[[i]]$theta,
                              info = info_out_range_new_sk[[i]]$test_info_curve, 
                              num_item = names(info_out_range_new_sk)[[i]], 
                              sel = "guidedNew")
  range_new_data_sk  <- rbind(temp_range_new_sk, 
                          range_new_data_sk )
  
}
temp_random_sk <- NULL
random_data_sk <- NULL
for (i in 1:length(info_test_random_sk)) {
  temp_random_sk <- data.frame(theta = info_test_random_sk[[i]]$theta,
                               info = info_test_random_sk[[i]]$test_info_curve, 
                               num_item = paste0("number", 
                                                 nrow(info_test_random_sk[[i]]$info_curves_item)), 
                               sel = "random")
  random_data_sk <- rbind(temp_random_sk, 
                          random_data_sk)
}

# questo calcola la media attraverso le 50 simulazioni per ognuna delle numerosità di item
# mean_random_sk<- aggregate(info ~ theta + num_item, data = random_data_sk, mean)
# mean_random_sk<- mean_random_sk[, c("theta", "info", "num_item")]
# mean_random_sk$sel <- "random"
# questo seleziona sono una delle curve per ogni numerosità 

temp = NULL
new_random_sk = NULL
for (i in 1:length(unique(random_data_sk$num_item))) {
  temp = random_data_sk[random_data_sk$num_item %in% unique(random_data_sk$num_item)[i], ]
  temp = temp[1:1000, ]
  new_random_sk = rbind(new_random_sk, temp)
}


start_data_sk <- data.frame(theta = IRT.informationCurves(m2pl_sk, 
                                                          theta = seq(-3, 3, 
                                                                      length = 1000))$theta,
                            info = info_start_sk, 
                            num_item = "all", 
                            sel = "start")

data_info_sk <- rbind(
  cluster_data_sk,
  range_data_sk , range_new_data_sk , 
  smart_data_sk ,new_random_sk #mean_random_sk
)


graph_start_sk <- data.frame(theta = IRT.informationCurves(m2pl_sk, 
                                                               theta = seq(-3,3,length = 1000))$theta, 
                                 info = (IRT.informationCurves(m2pl_sk, 
                                                               theta = seq(-3,3,length = 1000))$test_info_curve), 
                                 num_item = "all",
                                 sel = "start")


plots_sk <- list()

for(i in 1:length(unique(data_info_sk$num_item))) {
  
  plots_sk[[i]] <-  ggplot(rbind(data_info_sk[data_info_sk$num_item %in% unique(data_info_sk$num_item)[i], ], 
                                 graph_start_sk), 
                           aes(x = theta, y = info, group = sel, 
                               col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4) + 
    ggtitle(unique(data_info_sk$num_item)[i]) +
    theme(legend.position = "none")
}


ggplot(rbind(data_info_sk[data_info_sk$num_item %in% "number10", ], 
             graph_start_sk), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4)

do.call(grid.arrange, plots_sk)



# Costruisco un dataframe per il calcolo del bias per le stime di theta sk ----

for (i in 1:length(model_fit_random_sk)) {
  names(model_fit_random_sk)[[i]] <- paste0("number", 
                                            nrow(model_fit_random_sk[[i]]$xsi))
}

temp <- NULL
random_theta <- NULL
list_temp <- NULL
theta_random_sk <- list()

for (i in 1:length(unique(names(model_fit_random_sk)))) {
  random_theta <- NULL
  temp <- model_fit_random_sk[names(model_fit_random_sk) == unique(names(model_fit_random_sk))[i]]
  
  for (j in 1:length(temp)) {
    list_temp <- temp[[j]]$person$EAP
    random_theta <- data.frame(cbind(random_theta, list_temp))
    theta_random_sk[[i]] <- random_theta
    names(theta_random_sk)[[i]] <- unique(names(model_fit_random_sk))[i]
  }
}

# # devo fare la media attraverso el colonne per oguno dei number item 
# temp_theta_random <- data.frame(matrix(ncol = length(unique(names(theta_random_sk))), 
#                                        nrow = 1000))
# for (i in 1:length(unique(names(theta_random_sk)))) {
#   temp_theta_random[, i] <- c(rowMeans(theta_random_sk[[i]]))
#   colnames(temp_theta_random)[i] <- unique(names(theta_random_sk))[i]
# }

# siccome prima ho preso la prima selezione di item per ognuna delle 
# numerosità, prendo la prima selezione per ognuno

temp_theta_random_sk = data.frame(matrix(ncol = 
                                           length(unique(names(theta_random_sk))), 
                                      nrow = 1000))
# prendo solo la prima selezione di ogni numerosità 
for (i in 1:length(theta_random_sk)) {
  temp_theta_random_sk[,i] = theta_random_sk[[i]][,1]
  colnames(temp_theta_random_sk)[i] <- unique(names(theta_random_sk))[i]
}

random = data.frame(selection = rep("random", nrow(temp_theta_random_sk)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random_sk = cbind(sbj, random, temp_theta_random_sk)
theta_random_sk <- reshape(temp_theta_random_sk, 
                           idvar = "sbj", 
                           varying = list(3:(ncol(temp_theta_random_sk))), 
                           v.names = "theta_est", 
                           direction = "long", 
                           times = (names(temp_theta_random_sk)[-c(1:2)]), 
                           timevar = "num_item")
theta_random_sk$obs <- true_theta_sk
theta_random_sk$all <- theta_all_sk
head(theta_random_sk)

for (i in 1:length(model_out_cluster_sk)) {
  names(model_out_cluster_sk)[[i]] <- paste0("number", 
                                             nrow(model_out_cluster_sk[[i]]$xsi))
  names(model_out_range_sk)[[i]] <- paste0("number", 
                                           nrow(model_out_range_sk[[i]]$xsi))
  names(model_out_range_new_sk)[[i]] <- paste0("number", 
                                           nrow(model_out_range_new_sk[[i]]$xsi))
  names(model_out_smart_sk)[[i]] <- paste0("number", 
                                           nrow(model_out_smart_sk[[i]]$xsi))
}

temp_theta_cluster_sk <- data.frame(matrix(ncol = length(unique(names(model_out_cluster_sk))), 
                                           nrow = 1000))

temp_theta_range_sk <- data.frame(matrix(ncol = length(unique(names(model_out_range_sk))), 
                                         nrow = 1000))
temp_theta_range_new_sk <- data.frame(matrix(ncol = length(unique(names(model_out_range_new_sk))), 
                                         nrow = 1000))
temp_theta_smart_sk <- data.frame(matrix(ncol = length(unique(names(model_out_range_sk))), 
                                         nrow = 1000))

for (i in 1:length(unique(names(model_out_cluster_sk)))) {
  temp_theta_cluster_sk[, i] <- model_out_cluster_sk[[i]]$person$EAP
  colnames(temp_theta_cluster_sk)[i] <- unique(names(model_out_cluster_sk))[i]
  
  temp_theta_range_sk[, i] <- model_out_range_sk[[i]]$person$EAP
  colnames(temp_theta_range_sk)[i] <- unique(names(model_out_range_sk))[i]
  
  temp_theta_range_new_sk[, i] <- model_out_range_new_sk[[i]]$person$EAP
  colnames(temp_theta_range_new_sk)[i] <- unique(names(model_out_range_new_sk))[i]
  
  temp_theta_smart_sk[, i] <- model_out_smart_sk[[i]]$person$EAP
  colnames(temp_theta_smart_sk)[i] <- unique(names(model_out_smart_sk))[i]
  
}


sbj <- data.frame(sbj =1:1000)

cluster = data.frame(selection = rep("cluster", nrow(sbj)))
range = data.frame(selection = rep("range", nrow(sbj)))
range_new = data.frame(selection = rep("rangeNew", nrow(sbj)))
smart = data.frame(selection = rep("smart", nrow(sbj)))


temp_theta_cluster_sk <- cbind(sbj, cluster,  temp_theta_cluster_sk)

theta_cluster_sk <- reshape(temp_theta_cluster_sk, 
                            idvar = "sbj", 
                            varying = list(3:(ncol(temp_theta_cluster_sk))), 
                            v.names = "theta_est", 
                            direction = "long", 
                            times = (names(temp_theta_cluster_sk)[-c(1:2)]), 
                            timevar = "num_item")
theta_cluster_sk$obs <- true_theta_sk
theta_cluster_sk$all <- theta_all_sk
head(theta_cluster_sk)

temp_theta_range_sk <- cbind(sbj, range,  temp_theta_range_sk)

theta_range_sk <- reshape(temp_theta_range_sk, 
                          idvar = "sbj", 
                          varying = list(3:(ncol(temp_theta_range_sk))), 
                          v.names = "theta_est", 
                          direction = "long", 
                          times = (names(temp_theta_range_sk)[-c(1:2)]), 
                          timevar = "num_item")
theta_range_sk$obs <- true_theta_sk
theta_range_sk$all <- theta_all_sk
head(theta_range_sk)

temp_theta_range_new_sk <- cbind(sbj, range_new,  temp_theta_range_new_sk)

theta_range_new_sk <- reshape(temp_theta_range_new_sk, 
                          idvar = "sbj", 
                          varying = list(3:(ncol(temp_theta_range_new_sk))), 
                          v.names = "theta_est", 
                          direction = "long", 
                          times = (names(temp_theta_range_new_sk)[-c(1:2)]), 
                          timevar = "num_item")
theta_range_new_sk$obs <- true_theta_sk
theta_range_new_sk$all <- theta_all_sk
head(theta_range_new_sk)



temp_theta_smart_sk <- cbind(sbj, smart,  temp_theta_smart_sk)

theta_smart_sk <- reshape(temp_theta_smart_sk, 
                          idvar = "sbj", 
                          varying = list(3:(ncol(temp_theta_smart_sk))), 
                          v.names = "theta_est", 
                          direction = "long", 
                          times = (names(temp_theta_smart_sk)[-c(1:2)]), 
                          timevar = "num_item")
theta_smart_sk$obs <- true_theta_sk
theta_smart_sk$all <- theta_all_sk
head(theta_smart_sk)

# theta_sk <- rbind(theta_cluster_sk, 
#                   theta_range_sk,  theta_range_new_sk, 
#                   theta_smart_sk , theta_random_sk
# )
# 
# theta_sk$distribution <- "sk"
# theta_sk <- theta_sk[order(theta_sk$obs), ]
# 
# # calcolo bias 
# 
# theta_sk$bias_obs <- with(theta_sk, 
#                           theta_est - obs)
# theta_sk$bias_obs_abs <- with(theta_sk, 
#                               abs(theta_est - obs))
# 
# theta_sk$bias_all <- with(theta_sk, 
#                           (theta_est - all))
# 
# theta_sk$bias_all_abs <- with(theta_sk, 
#                               abs(theta_est - all))
# 
# obs_bias_sk <- aggregate(bias_obs ~ num_item + selection, 
#                          data = theta_sk, 
#                          mean)
# obs_bias_sk$type <- "bias"
# obs_bias_sk_abs <- aggregate(bias_obs_abs ~ num_item + selection, 
#                              data = theta_sk, 
#                              mean)
# obs_bias_sk_abs$type <- "bias_abs"
# colnames(obs_bias_sk_abs)[3] <- "bias_obs"
# 
# obs_bias_sk <- rbind(obs_bias_sk, obs_bias_sk_abs)
# 
# obs_bias_sk$temp <- as.integer(gsub("number", "", 
#                                     obs_bias_sk$num_item))
# obs_bias_sk = obs_bias_sk[order(obs_bias_sk$temp), ]
# ggplot(obs_bias_sk, 
#        aes(x=as.factor(temp), y = bias_obs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_grid(~type)
# 
# 
# all_bias_sk <- aggregate(bias_all ~ num_item + selection, 
#                          data = theta_sk, 
#                          mean)
# all_bias_sk$type <- "bias"
# all_bias_sk_abs <- aggregate(bias_all_abs ~ num_item + selection, 
#                              data = theta_sk, 
#                              mean)
# all_bias_sk_abs$type <- "bias_abs"
# colnames(all_bias_sk_abs)[3] <- "bias_all"
# 
# all_bias_sk <- rbind(all_bias_sk, all_bias_sk_abs)
# 
# all_bias_sk$temp <- as.integer(gsub("number", "", 
#                                     all_bias_sk$num_item))
# all_bias_sk = all_bias_sk[order(all_bias_sk$temp), ]
# ggplot(all_bias_sk, 
#        aes(x=as.factor(temp), y = bias_all, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(~type)
# 
# # calcolo rmsea -----
# 
# theta_sk$bias_obs_sq <- theta_sk$bias_obs^2
# 
# theta_sk$bias_all_sq <- theta_sk$bias_all^2
# 
# obs_bias_sk_sq <- aggregate(bias_obs_sq ~ num_item + selection, 
#                             data = theta_sk, 
#                             mean)
# obs_bias_sk_sq$type <- "bias_sq_obs"
# obs_bias_sk_sq$rmsea = sqrt(obs_bias_sk_sq$bias_obs_sq)
# 
# ggplot(obs_bias_sk_sq, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 
# all_bias_sk_sq <- aggregate(bias_all_sq ~ num_item + selection, 
#                             data = theta_sk, 
#                             mean)
# all_bias_sk_sq$type <- "bias_sq_all"
# all_bias_sk_sq$rmsea = sqrt(all_bias_sk_sq$bias_all)
# 
# ggplot(all_bias_sk_sq, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 
# 
# # per gruppi latenti ----- 
# theta_lat_sk <- seq(-2.5, 2.5, length.out = 4) 
# g <- cut(theta_lat_sk, length(theta_lat_sk), include.lowest = TRUE)
# cut_val = cut_borders(g)
# 
# 
# 
# group_name <- letters[1:(nrow(cut_val)+2)]
# 
# 
# theta_sk$group <- ifelse(theta_sk$obs  <= cut_val[1, "start"], 
#                          group_name[1], 
#                          ifelse(theta_sk$obs > cut_val[1, "start"] & theta_sk$obs <= cut_val[1, "end"], 
#                                 group_name[2], 
#                                 ifelse(theta_sk$obs > cut_val[2, "start"] & theta_sk$obs <= cut_val[2, "end"], 
#                                        group_name[3], 
#                                        ifelse(theta_sk$obs > cut_val[3, "start"] & theta_sk$obs <= cut_val[3, "end"], 
#                                               group_name[4], 
#                                               ifelse(theta_sk$obs > cut_val[4, "start"] & theta_sk$obs <= cut_val[4, "end"], 
#                                                      group_name[5], group_name[6])))))
# 
# bias_abs_sk_group <- aggregate(bias_obs_abs ~ selection + group + num_item, 
#                                data = theta_sk, mean)
# bias_abs_sk_group$type = "bias"
# 
# ggplot(bias_abs_sk_group, 
#        aes(x=group, y = bias_obs_abs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), 
#                                            lwd =1.3) + 
#   theme(legend.position = "top") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)
# 
# bias_abs_sk_group <- aggregate(bias_obs_abs ~ selection + group + num_item, 
#                                data = theta_sk, mean)
# bias_abs_sk_group$type = "bias"
# 
# # rmsea ----
# rmsea_sk_group <- aggregate(bias_obs_sq ~ selection + group + num_item, 
#                             data = theta_sk, mean)
# rmsea_sk_group$rmsea = sqrt(rmsea_sk_group$bias_obs_sq)
# 
# ggplot(rmsea_sk_group, 
#        aes(x=group, y = rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), 
#                                            lwd =1.3) + 
#   theme(legend.position = "top") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)
# 
# 
# 
# # Stessa cosa item con theta sk_theta PARAMETRI FISSI -----
# data_random_sk_theta_summary$selection <- "random"
# all_data_sk_theta <- rbind(data_random_sk_theta_summary, 
#                            data.frame(num_item = info_summary_range_sk_theta$range_name, 
#                                       mean_info = info_summary_range_sk_theta$info_test, 
#                                       sd_info = 0, 
#                                       selection = info_summary_range_sk_theta$selection, 
#                                       mean_rel = info_summary_range_sk_theta$rel, 
#                                       sd_rel = 0), 
#                            data.frame(num_item = info_summary_range_new_sk_theta$range_new_name, 
#                                       mean_info = info_summary_range_new_sk_theta$info_test, 
#                                       sd_info = 0, 
#                                       selection = info_summary_range_new_sk_theta$selection, 
#                                       mean_rel = info_summary_range_new_sk_theta$rel, 
#                                       sd_rel = 0), 
#                            data.frame(num_item = info_summary_cluster_sk_theta$cluster_name, 
#                                       mean_info = info_summary_cluster_sk_theta$info_test, 
#                                       sd_info = 0, 
#                                       selection = info_summary_cluster_sk_theta$selection, 
#                                       mean_rel = info_summary_cluster_sk_theta$rel, 
#                                       sd_rel = 0), 
#                            
#                            data.frame(num_item = info_summary_smart_sk_theta$smart_name, 
#                                       mean_info = info_summary_smart_sk_theta$info_test, 
#                                       sd_info = 0, 
#                                       selection = info_summary_smart_sk_theta$selection, 
#                                       mean_rel = info_summary_smart_sk_theta$rel, 
#                                       sd_rel = 0))
# all_data_sk_theta$item_temp <- gsub("number", "", all_data_sk_theta$num_item)
# all_data_sk_theta$item_temp <- gsub("all", 0, all_data_sk_theta$item_temp)
# all_data_sk_theta$item_temp <- as.integer(all_data_sk_theta$item_temp)
# all_data_sk_theta <- all_data_sk_theta[order(all_data_sk_theta$item_temp), ]
# all_data_sk_theta$selection <- gsub("sk_theta", '', all_data_sk_theta$selection)
# ggplot(all_data_sk_theta[!all_data_sk_theta$num_item %in%"all", ], 
#        aes(x=as.factor(item_temp), y=mean_info, 
#            group=selection, color=selection)) + 
#   geom_line(aes(linetype = selection), lwd = 1.5) +
#   geom_point(aes(shape=selection))+
#   geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
#                 width=.2,
#                 position=position_dodge(0.05)) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   scale_x_discrete(labels =  unique(all_data_sk_theta[!all_data_sk_theta$num_item %in%"all", "num_item"])) + 
#   geom_hline(yintercept = all_data_sk_theta[all_data_sk_theta$num_item %in% "all", 
#                                             "mean_info"])

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
  scale_x_discrete(labels =  unique(all_data_sk[!all_data_sk$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk[all_data_sk$num_item %in% "all", 
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
  scale_x_discrete(labels =  unique(all_data_sk[!all_data_sk$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk[all_data_sk$num_item %in% "all", 
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
# mean_random_sk_theta<- aggregate(info ~ theta + num_item, data = random_data_sk_theta, mean)
# mean_random_sk_theta<- mean_random_sk_theta[, c("theta", "info", "num_item")]
# mean_random_sk_theta$sel <- "random"
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


plots_sk_theta <- list()

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
# temp_theta_random <- data.frame(matrix(ncol = length(unique(names(theta_random_sk_theta))), 
#                                        nrow = 1000))
# for (i in 1:length(unique(names(theta_random_sk_theta)))) {
#   temp_theta_random[, i] <- c(rowMeans(theta_random_sk_theta[[i]]))
#   colnames(temp_theta_random)[i] <- unique(names(theta_random_sk_theta))[i]
# }
# siccome prima ho preso la prima selezione di item per ognuna delle 
# numerosità, prendo la prima selezione per ognuno

temp_theta_random_sk_theta = data.frame(matrix(ncol = 
                                                 length(unique(names(theta_random_sk_theta))), 
                                      nrow = 1000))
for (i in 1:length(theta_random_sk_theta)) {
  temp_theta_random_sk_theta[,i] = theta_random_sk_theta[[i]][,1]
  colnames(temp_theta_random_sk_theta)[i] <- unique(names(theta_random_sk_theta))[i]
}

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

random_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_random_sk_theta, 
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
random_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_random_sk_theta, 
                                  mean)
random_rmse_all_theta$selection = "random"
random_rmse_all_theta$type = "rmse_all"
random_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_random_sk_theta, 
                                      mean)
random_bias_all_theta_abs$selection = "random"
random_bias_all_theta_abs$type = "bias_all_abs"


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

sk_bias_obs_theta = rbind(random_bias_obs_theta, 
                          cluster_bias_obs_theta, 
                          range_new_bias_obs_theta, 
                          range_bias_obs_theta, 
                          smart_bias_obs_theta)

ggplot(sk_bias_obs_theta, 
       aes(x = num_item, y = bias_obs, group =selection, color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.2)

sk_bias_obs_abs_theta = rbind(random_bias_obs_theta_abs, 
                              cluster_bias_obs_theta_abs, 
                              range_new_bias_obs_theta_abs, 
                              range_bias_obs_theta_abs, 
                              smart_bias_obs_theta_abs)

ggplot(sk_bias_obs_abs_theta, 
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

# theta_sk_theta <- rbind(theta_cluster_sk_theta, 
#                         theta_range_sk_theta, theta_range_new_sk_theta,
#                         theta_smart_sk_theta , theta_random_sk_theta
# )
# 
# theta_sk_theta$distribution <- "sk_theta"
# 
# # calcolo bais theta con stimoli fissi ----
# theta_sk_theta$bias_obs <- with(theta_sk_theta, 
#                                 theta_est - obs)
# theta_sk_theta$bias_obs_abs <- with(theta_sk_theta, 
#                                     abs(theta_est - obs))
# 
# theta_sk_theta$bias_all <- with(theta_sk_theta, 
#                                 (theta_est - all))
# 
# theta_sk_theta$bias_all_abs <- with(theta_sk_theta, 
#                                     abs(theta_est - all))
# 
# obs_bias_sk_theta <- aggregate(bias_obs ~ num_item + selection, 
#                                data = theta_sk_theta, 
#                                mean)
# obs_bias_sk_theta$type <- "bias"
# obs_bias_sk_theta_abs <- aggregate(bias_obs_abs ~ num_item + selection, 
#                                    data = theta_sk_theta, 
#                                    mean)
# obs_bias_sk_theta_abs$type <- "bias_abs"
# colnames(obs_bias_sk_theta_abs)[3] <- "bias_obs"
# 
# obs_bias_sk_theta <- rbind(obs_bias_sk_theta, obs_bias_sk_theta_abs)
# 
# obs_bias_sk_theta$temp <- as.integer(gsub("number", "", 
#                                           obs_bias_sk_theta$num_item))
# obs_bias_sk_theta = obs_bias_sk_theta[order(obs_bias_sk_theta$temp), ]
# ggplot(obs_bias_sk_theta, 
#        aes(x=as.factor(temp), y = bias_obs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_grid(~type)
# 
# 
# all_bias_sk_theta <- aggregate(bias_all ~ num_item + selection, 
#                                data = theta_sk_theta, 
#                                mean)
# all_bias_sk_theta$type <- "bias"
# all_bias_sk_theta_abs <- aggregate(bias_all_abs ~ num_item + selection, 
#                                    data = theta_sk_theta, 
#                                    mean)
# all_bias_sk_theta_abs$type <- "bias_abs"
# colnames(all_bias_sk_theta_abs)[3] <- "bias_all"
# 
# all_bias_sk_theta <- rbind(all_bias_sk_theta, all_bias_sk_theta_abs)
# 
# all_bias_sk_theta$temp <- as.integer(gsub("number", "", 
#                                           all_bias_sk_theta$num_item))
# all_bias_sk_theta = all_bias_sk_theta[order(all_bias_sk_theta$temp), ]
# ggplot(all_bias_sk_theta, 
#        aes(x=as.factor(temp), y = bias_all, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(~type)
# 
# # calcolo rmsea -----
# theta_sk_theta$bias_obs_sq <- theta_sk_theta$bias_obs^2
# theta_sk_theta$bias_all_sq <- theta_sk_theta$bias_all^2
# 
# 
# obs_bias_sk_sq_theta <- aggregate(bias_obs_sq ~ num_item + selection, 
#                             data = theta_sk_theta, 
#                             mean)
# obs_bias_sk_sq_theta$type <- "bias_sq_obs"
# obs_bias_sk_sq_theta$rmsea = sqrt(obs_bias_sk_sq_theta$bias_obs_sq)
# 
# ggplot(obs_bias_sk_sq_theta, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 
# 
# 
# all_bias_sk_sq_theta <- aggregate(bias_all_sq ~ num_item + selection, 
#                             data = theta_sk_theta, 
#                             mean)
# all_bias_sk_sq_theta$type <- "bias_sq_all"
# all_bias_sk_sq_theta$rmsea = sqrt(all_bias_sk_sq_theta$bias_all)
# 
# ggplot(all_bias_sk_sq_theta, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 
# 
# 
# # con gruppi latenti sk theta  -----
# theta_lat_theta_sk <- seq(-2.5, 2.5, length.out = 4) 
# g <- cut(theta_lat_theta_sk, length(theta_lat_theta_sk), include.lowest = TRUE)
# cut_val = cut_borders(g)
# 
# 
# 
# group_name <- letters[1:(nrow(cut_val)+2)]
# 
# 
# theta_sk_theta$group <- ifelse(theta_sk_theta$obs  <= cut_val[1, "start"], 
#                                group_name[1], 
#                                ifelse(theta_sk_theta$obs > cut_val[1, "start"] & theta_sk_theta$obs <= cut_val[1, "end"], 
#                                       group_name[2], 
#                                       ifelse(theta_sk_theta$obs > cut_val[2, "start"] & theta_sk_theta$obs <= cut_val[2, "end"], 
#                                              group_name[3], 
#                                              ifelse(theta_sk_theta$obs > cut_val[3, "start"] & theta_sk_theta$obs <= cut_val[3, "end"], 
#                                                     group_name[4], 
#                                                     ifelse(theta_sk_theta$obs > cut_val[4, "start"] & theta_sk_theta$obs <= cut_val[4, "end"], 
#                                                            group_name[5], group_name[6])))))
# 
# bias_abs_sk_group_theta <- aggregate(bias_obs_abs ~ selection + group + num_item, 
#                                      data = theta_sk_theta, mean)
# bias_abs_sk_group_theta$type = "bias"
# 
# ggplot(bias_abs_sk_group_theta, 
#        aes(x=group, y = bias_obs_abs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)
# 
# rmsea_sk_group_theta <- aggregate(bias_obs_sq ~ selection + group + num_item, 
#                             data = theta_sk_theta, mean)
# rmsea_sk_group_theta$rmsea = sqrt(rmsea_sk_group_theta$bias_obs_sq)
# 
# ggplot(rmsea_sk_group_theta, 
#        aes(x=group, y = rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), 
#                                            lwd =1.3) + 
#   theme(legend.position = "top") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)

all_data_sk_theta = data_sk_random_theta_unique
rm(list= ls()[!(ls() %in% c("m2pl_sk",
                            'obs_bias_sk_theta','all_bias_sk_theta', 
                             "theta_sk_theta", "all_data_sk_theta", 
                            "data_info_sk_theta",
                            'obs_bias_sk','all_bias_sk', 
                            "theta_sk", "all_data_sk", 
                            "data_info_sk", 
                            "bias_abs_sk_group", 
                            "bias_abs_sk_group_theta", 
                            "obs_bias_sk_sq", "obs_bias_sk_sq_theta", 
                            "all_bias_sk_sq", "all_bias_sk_sq_theta", 
                            "rmsea_sk_group", "rmsea_sk_group_theta", 
                            "sk_random", "sk_random_theta", 
                            "data_sk_random_theta_unique", 
                            "data_sk_random_unique", 
                            "sk_bias_obs_theta",
                            "sk_bias_obs_abs_theta",
                            "sk_bias_all_theta",
                            "sk_bias_all_abs_theta",
                            "sk_rmse_obs_theta",
                            "sk_rmse_all_theta",
                            "sk_rmse_groups_theta",
                            "sk_bias_abs_groups_theta",
                            "sk_bias_groups_theta"))])
# Dati uniform ----
# Stessa cosa item con theta uni -----
rm(list = ls())
load("UNIrandom.RData")
load("UNIsmart.RData")
load("UNIguided.RData")
load("UNIguidedNew.RData")
load("UNIcluster.RData")
data_random_uni_summary$selection <- "random"
info_summary_range_new_uni$selection = "guidedNew"
all_data_uni <- rbind(data_random_uni_summary, 
                      data.frame(num_item = info_summary_range_uni$range_name, 
                                 mean_info = info_summary_range_uni$info_test, 
                                 sd_info = 0, 
                                 selection = info_summary_range_uni$selection, 
                                 mean_rel = info_summary_range_uni$rel, 
                                 sd_rel = 0), 
                      data.frame(num_item = info_summary_range_new_uni$range_new_name, 
                                 mean_info = info_summary_range_new_uni$info_test, 
                                 sd_info = 0, 
                                 selection = info_summary_range_new_uni$selection, 
                                 mean_rel = info_summary_range_new_uni$rel, 
                                 sd_rel = 0), 
                      data.frame(num_item = info_summary_cluster_uni$cluster_name, 
                                 mean_info = info_summary_cluster_uni$info_test, 
                                 sd_info = 0, 
                                 selection = info_summary_cluster_uni$selection, 
                                 mean_rel = info_summary_cluster_uni$rel, 
                                 sd_rel = 0), 
                      
                      data.frame(num_item = info_summary_smart_uni$smart_name, 
                                 mean_info = info_summary_smart_uni$info_test, 
                                 sd_info = 0, 
                                 selection = info_summary_smart_uni$selection, 
                                 mean_rel = info_summary_smart_uni$rel, 
                                 sd_rel = 0))
all_data_uni$item_temp <- gsub("number", "", all_data_uni$num_item)
all_data_uni$item_temp <- gsub("all", 0, all_data_uni$item_temp)
all_data_uni$item_temp <- as.integer(all_data_uni$item_temp)
all_data_uni <- all_data_uni[order(all_data_uni$item_temp), ]
all_data_uni$selection <- gsub("uni", '', all_data_uni$selection)
ggplot(all_data_uni[!all_data_uni$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_uni[!all_data_uni$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_uni[all_data_uni$num_item %in% "all", 
                                       "mean_info"])

# grafico info unii prendendo solo la prima combo di item da numerosità di item

uni_random = NULL 
temp = NULL
for(i in 1:length(unique(data_random_uni$num_item))) {
  temp = data_random_uni[data_random_uni$num_item %in% unique(data_random_uni$num_item)[i], ]
  temp = temp[1, ]
  uni_random = rbind(uni_random, temp)
}
uni_random$selection = "random" 
# unisco al data set 

data_uni_random_unique = rbind(data.frame(num_item = uni_random$num_item,
                                          mean_info = uni_random$info_total,
                                          sd_info = 0,
                                          selection = uni_random$selection,
                                          mean_rel = uni_random$rel,
                                          sd_rel = 0), 
                               data.frame(num_item = info_summary_range_uni$range_name, 
                                          mean_info = info_summary_range_uni$info_test, 
                                          sd_info = 0, 
                                          selection = info_summary_range_uni$selection, 
                                          mean_rel = info_summary_range_uni$rel, 
                                          sd_rel = 0), 
                               data.frame(num_item = info_summary_range_new_uni$range_new_name, 
                                          mean_info = info_summary_range_new_uni$info_test, 
                                          sd_info = 0, 
                                          selection = info_summary_range_new_uni$selection, 
                                          mean_rel = info_summary_range_new_uni$rel, 
                                          sd_rel = 0), 
                               data.frame(num_item = info_summary_cluster_uni$cluster_name, 
                                          mean_info = info_summary_cluster_uni$info_test, 
                                          sd_info = 0, 
                                          selection = info_summary_cluster_uni$selection, 
                                          mean_rel = info_summary_cluster_uni$rel, 
                                          sd_rel = 0), 
                               
                               data.frame(num_item = info_summary_smart_uni$smart_name, 
                                          mean_info = info_summary_smart_uni$info_test, 
                                          sd_info = 0, 
                                          selection = info_summary_smart_uni$selection, 
                                          mean_rel = info_summary_smart_uni$rel, 
                                          sd_rel = 0))
ggplot(data_uni_random_unique[!data_uni_random_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_uni[!all_data_uni$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_uni[all_data_uni$num_item %in% "all", 
                                       "mean_info"])


# reliability solo prima selezione ---- 
ggplot(data_uni_random_unique[!data_uni_random_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_uni[!all_data_uni$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_uni[all_data_uni$num_item %in% "all", 
                                       "mean_rel"])


# stesso garfico ma con le reliability 

ggplot(all_data_uni[!all_data_uni$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1) +
  geom_point(aes(shape=selection), size =2)+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels = unique(all_data_uni[!all_data_uni$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_uni[all_data_uni$num_item %in% "all", 
                                       "mean_rel"]) + ylim(0,1)

grid.arrange(ggplot(all_data_uni[!all_data_uni$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_info, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "top") + 
               scale_x_discrete(labels =  unique(all_data_uni[!all_data_uni$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_uni[all_data_uni$num_item %in% "all", 
                                                    "mean_info"]), 
             ggplot(all_data_uni[!all_data_uni$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_rel, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1) +
               geom_point(aes(shape=selection), size =2)+
               geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "none") + 
               scale_x_discrete(labels = unique(all_data_uni[!all_data_uni$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_uni[all_data_uni$num_item %in% "all", 
                                                    "mean_rel"]))

# confronto TIF dati uni -----

temp_cluster_uni<- NULL
cluster_data_uni <- NULL

temp_smart_uni <- NULL
smart_data_uni <- NULL


temp_range_uni <- NULL
range_data_uni  <- NULL

temp_range_new_uni <- NULL
range_new_data_uni  <- NULL


for (i in 1:length(info_out_smart_uni)) {
  temp_cluster_uni<- data.frame(theta = info_out_cluster_uni[[i]]$theta,
                                info = info_out_cluster_uni[[i]]$test_info_curve, 
                                num_item = names(info_out_cluster_uni)[[i]], 
                                sel = "cluster")
  cluster_data_uni <- rbind(temp_cluster_uni, 
                            cluster_data_uni)
  
  temp_smart_uni <- data.frame(theta = info_out_smart_uni[[i]]$theta,
                               info = info_out_smart_uni[[i]]$test_info_curve, 
                               num_item = names(info_out_smart_uni)[[i]], 
                               sel = "smart")
  smart_data_uni <- rbind(temp_smart_uni, 
                          smart_data_uni)
  temp_range_uni <- data.frame(theta = info_out_range_uni[[i]]$theta,
                               info = info_out_range_uni[[i]]$test_info_curve, 
                               num_item = names(info_out_range_uni)[[i]], 
                               sel = "guided")
  range_data_uni  <- rbind(temp_range_uni, 
                           range_data_uni )
  temp_range_new_uni <- data.frame(theta = info_out_range_new_uni[[i]]$theta,
                               info = info_out_range_new_uni[[i]]$test_info_curve, 
                               num_item = names(info_out_range_new_uni)[[i]], 
                               sel = "guidedNEW")
  range_new_data_uni  <- rbind(temp_range_new_uni, 
                           range_new_data_uni )
}
temp_random_uni <- NULL
random_data_uni <- NULL
for (i in 1:length(info_test_random_uni)) {
  temp_random_uni <- data.frame(theta = info_test_random_uni[[i]]$theta,
                                info = info_test_random_uni[[i]]$test_info_curve, 
                                num_item = paste0("number", 
                                                  nrow(info_test_random_uni[[i]]$info_curves_item)), 
                                sel = "random")
  random_data_uni <- rbind(temp_random_uni, 
                           random_data_uni)
}

# questo calcola la media delle info per ogni numerosità di item
# mean_random_uni<- aggregate(info ~ theta + num_item, data = random_data_uni, mean)
# mean_random_uni<- mean_random_uni[, c("theta", "info", "num_item")]
# mean_random_uni$sel <- "random"
temp = NULL
new_random_uni = NULL
for (i in 1:length(unique(random_data_uni$num_item))) {
  temp = random_data_uni[random_data_uni$num_item %in% unique(random_data_uni$num_item)[i], ]
  temp = temp[1:1000, ]
  new_random_uni = rbind(new_random_uni, temp)
}

start_data_uni <- data.frame(theta = IRT.informationCurves(m2pl_uni, 
                                                           theta = seq(-3, 3, 
                                                                       length = 1000))$theta,
                             info = info_start_uni, 
                             num_item = "all", 
                             sel = "start")

data_info_uni <- rbind(
  cluster_data_uni,
  range_data_uni , range_new_data_uni , 
  smart_data_uni , new_random_uni#mean_random_uni
)


plots_uni <- list()

graph_start_uni <- data.frame(theta = IRT.informationCurves(m2pl_uni, 
                                                               theta = seq(-3,3,length = 1000))$theta, 
                                 info = (IRT.informationCurves(m2pl_uni, 
                                                               theta = seq(-3,3,length = 1000))$test_info_curve), 
                                 num_item = "all",
                                 sel = "start")


for(i in 1:length(unique(data_info_uni$num_item))) {
  
  plots_uni[[i]] <-  ggplot(rbind(data_info_uni[data_info_uni$num_item %in% unique(data_info_uni$num_item)[i], ], 
                                  graph_start_uni), 
                            aes(x = theta, y = info, group = sel, 
                                col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4) + 
    ggtitle(unique(data_info_uni$num_item)[i]) +
    theme(legend.position = "none")
}



ggplot(rbind(data_info_uni[data_info_uni$num_item %in% "number10", ], 
             graph_start_uni), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4)

do.call(grid.arrange, plots_uni)



# Costruisco un dataframe per il calcolo del bias per le stime di theta uni ----

for (i in 1:length(model_fit_random_uni)) {
  names(model_fit_random_uni)[[i]] <- paste0("number", 
                                             nrow(model_fit_random_uni[[i]]$xsi))
}

temp <- NULL
random_theta <- NULL
list_temp <- NULL
theta_random_uni <- list()

for (i in 1:length(unique(names(model_fit_random_uni)))) {
  random_theta <- NULL
  temp <- model_fit_random_uni[names(model_fit_random_uni) == unique(names(model_fit_random_uni))[i]]
  
  for (j in 1:length(temp)) {
    list_temp <- temp[[j]]$person$EAP
    random_theta <- data.frame(cbind(random_theta, list_temp))
    theta_random_uni[[i]] <- random_theta
    names(theta_random_uni)[[i]] <- unique(names(model_fit_random_uni))[i]
  }
}

# devo fare la media attraverso el colonne per oguno dei number item 
# temp_theta_random <- data.frame(matrix(ncol = length(unique(names(theta_random_uni))), 
#                                        nrow = 1000))
# for (i in 1:length(unique(names(theta_random_uni)))) {
#   temp_theta_random[, i] <- c(rowMeans(theta_random_uni[[i]]))
#   colnames(temp_theta_random)[i] <- unique(names(theta_random_uni))[i]
# }

# siccome prima ho preso la prima selezione di item per ognuna delle 
# numerosità, prendo la prima selezione per ognuno

temp_theta_random = data.frame(matrix(ncol = length(unique(names(theta_random_uni))), 
                                      nrow = 1000))
for (i in 1:length(theta_random_uni)) {
  temp_theta_random[,i] = theta_random_uni[[i]][,1]
  colnames(temp_theta_random)[i] <- unique(names(theta_random_uni))[i]
}


random = data.frame(selection = rep("random", nrow(temp_theta_random)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random = cbind(sbj, random, temp_theta_random)
theta_random_uni <- reshape(temp_theta_random, 
                            idvar = "sbj", 
                            varying = list(3:(ncol(temp_theta_random))), 
                            v.names = "theta_est", 
                            direction = "long", 
                            times = (names(temp_theta_random)[-c(1:2)]), 
                            timevar = "num_item")
theta_random_uni$obs <- true_theta_uni
theta_random_uni$all <- theta_all_uni
head(theta_random_uni)

for (i in 1:length(model_out_cluster_uni)) {
  names(model_out_cluster_uni)[[i]] <- paste0("number", 
                                              nrow(model_out_cluster_uni[[i]]$xsi))
  names(model_out_range_uni)[[i]] <- paste0("number", 
                                            nrow(model_out_range_uni[[i]]$xsi))
  names(model_out_range_new_uni)[[i]] <- paste0("number", 
                                            nrow(model_out_range_new_uni[[i]]$xsi))
  names(model_out_smart_uni)[[i]] <- paste0("number", 
                                            nrow(model_out_smart_uni[[i]]$xsi))
}

temp_theta_cluster_uni <- data.frame(matrix(ncol = length(unique(names(model_out_cluster_uni))), 
                                            nrow = 1000))

temp_theta_range_uni <- data.frame(matrix(ncol = length(unique(names(model_out_range_uni))), 
                                          nrow = 1000))
temp_theta_range_new_uni <- data.frame(matrix(ncol = length(unique(names(model_out_range_new_uni))), 
                                          nrow = 1000))
temp_theta_smart_uni <- data.frame(matrix(ncol = length(unique(names(model_out_range_uni))), 
                                          nrow = 1000))

for (i in 1:length(unique(names(model_out_cluster_uni)))) {
  temp_theta_cluster_uni[, i] <- model_out_cluster_uni[[i]]$person$EAP
  colnames(temp_theta_cluster_uni)[i] <- unique(names(model_out_cluster_uni))[i]
  
  temp_theta_range_uni[, i] <- model_out_range_uni[[i]]$person$EAP
  colnames(temp_theta_range_uni)[i] <- unique(names(model_out_range_uni))[i]
  
  temp_theta_range_new_uni[, i] <- model_out_range_new_uni[[i]]$person$EAP
  colnames(temp_theta_range_new_uni)[i] <- unique(names(model_out_range_new_uni))[i]
  
  temp_theta_smart_uni[, i] <- model_out_smart_uni[[i]]$person$EAP
  colnames(temp_theta_smart_uni)[i] <- unique(names(model_out_smart_uni))[i]
  
}


sbj <- data.frame(sbj =1:1000)

cluster = data.frame(selection = rep("cluster", nrow(sbj)))
range = data.frame(selection = rep("range", nrow(sbj)))
range_new = data.frame(selection = rep("rangeNew", nrow(sbj)))
smart = data.frame(selection = rep("smart", nrow(sbj)))


temp_theta_cluster_uni <- cbind(sbj, cluster,  temp_theta_cluster_uni)

theta_cluster_uni <- reshape(temp_theta_cluster_uni, 
                             idvar = "sbj", 
                             varying = list(3:(ncol(temp_theta_cluster_uni))), 
                             v.names = "theta_est", 
                             direction = "long", 
                             times = (names(temp_theta_cluster_uni)[-c(1:2)]), 
                             timevar = "num_item")
theta_cluster_uni$obs <- true_theta_uni
theta_cluster_uni$all <- theta_all_uni
head(theta_cluster_uni)

temp_theta_range_uni <- cbind(sbj, range,  temp_theta_range_uni)

theta_range_uni <- reshape(temp_theta_range_uni, 
                           idvar = "sbj", 
                           varying = list(3:(ncol(temp_theta_range_uni))), 
                           v.names = "theta_est", 
                           direction = "long", 
                           times = (names(temp_theta_range_uni)[-c(1:2)]), 
                           timevar = "num_item")
theta_range_uni$obs <- true_theta_uni
theta_range_uni$all <- theta_all_uni
head(theta_range_uni)

temp_theta_range_new_uni <- cbind(sbj, range_new,  temp_theta_range_new_uni)

theta_range_new_uni <- reshape(temp_theta_range_new_uni, 
                           idvar = "sbj", 
                           varying = list(3:(ncol(temp_theta_range_new_uni))), 
                           v.names = "theta_est", 
                           direction = "long", 
                           times = (names(temp_theta_range_new_uni)[-c(1:2)]), 
                           timevar = "num_item")
theta_range_new_uni$obs <- true_theta_uni
theta_range_new_uni$all <- theta_all_uni
head(theta_range_new_uni)


temp_theta_smart_uni <- cbind(sbj, smart,  temp_theta_smart_uni)

theta_smart_uni <- reshape(temp_theta_smart_uni, 
                           idvar = "sbj", 
                           varying = list(3:(ncol(temp_theta_smart_uni))), 
                           v.names = "theta_est", 
                           direction = "long", 
                           times = (names(temp_theta_smart_uni)[-c(1:2)]), 
                           timevar = "num_item")
theta_smart_uni$obs <- true_theta_uni
theta_smart_uni$all <- theta_all_uni

# theta_uni <- rbind(theta_cluster_uni, 
#                    theta_range_uni, theta_range_new_uni,
#                    theta_smart_uni , theta_random_uni
# )
# 
# theta_uni$distribution <- "uni"
# theta_uni <- theta_uni[order(theta_uni$obs), ]
# 
# # calcolo bias ----
# 
# theta_uni$bias_obs <- with(theta_uni, 
#                            theta_est - obs)
# theta_uni$bias_obs_abs <- with(theta_uni, 
#                                abs(theta_est - obs))
# 
# theta_uni$bias_all <- with(theta_uni, 
#                            (theta_est - all))
# 
# theta_uni$bias_all_abs <- with(theta_uni, 
#                                abs(theta_est - all))
# 
# obs_bias_uni <- aggregate(bias_obs ~ num_item + selection, 
#                           data = theta_uni, 
#                           mean)
# obs_bias_uni$type <- "bias"
# obs_bias_uni_abs <- aggregate(bias_obs_abs ~ num_item + selection, 
#                               data = theta_uni, 
#                               mean)
# obs_bias_uni_abs$type <- "bias_abs"
# colnames(obs_bias_uni_abs)[3] <- "bias_obs"
# 
# obs_bias_uni <- rbind(obs_bias_uni, obs_bias_uni_abs)
# 
# obs_bias_uni$temp <- as.integer(gsub("number", "", 
#                                      obs_bias_uni$num_item))
# obs_bias_uni = obs_bias_uni[order(obs_bias_uni$temp), ]
# ggplot(obs_bias_uni, 
#        aes(x=as.factor(temp), y = bias_obs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_grid(~type)
# 
# 
# all_bias_uni <- aggregate(bias_all ~ num_item + selection, 
#                           data = theta_uni, 
#                           mean)
# all_bias_uni$type <- "bias"
# all_bias_uni_abs <- aggregate(bias_all_abs ~ num_item + selection, 
#                               data = theta_uni, 
#                               mean)
# all_bias_uni_abs$type <- "bias_abs"
# colnames(all_bias_uni_abs)[3] <- "bias_all"
# 
# all_bias_uni <- rbind(all_bias_uni, all_bias_uni_abs)
# 
# all_bias_uni$temp <- as.integer(gsub("number", "", 
#                                      all_bias_uni$num_item))
# all_bias_uni = all_bias_uni[order(all_bias_uni$temp), ]
# ggplot(all_bias_uni, 
#        aes(x=as.factor(temp), y = bias_all, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(~type)
# 
# # calcolo RMSEA -----
# theta_uni$bias_obs_sq <- theta_uni$bias_obs^2
# 
# theta_uni$bias_all_sq <- theta_uni$bias_all^2
# 
# obs_bias_uni_sq <- aggregate(bias_obs_sq ~ num_item + selection, 
#                              data = theta_uni, 
#                              mean)
# obs_bias_uni_sq$type <- "bias_sq_obs"
# obs_bias_uni_sq$rmsea = sqrt(obs_bias_uni_sq$bias_obs_sq)
# 
# ggplot(obs_bias_uni_sq, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 
# 
# 
# all_bias_uni_sq <- aggregate(bias_all_sq ~ num_item + selection, 
#                              data = theta_uni, 
#                              mean)
# all_bias_uni_sq$type <- "bias_sq_all"
# all_bias_uni_sq$rmsea = sqrt(all_bias_uni_sq$bias_all)
# 
# ggplot(all_bias_uni_sq, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 

# Stessa cosa item con theta uni_theta PARAMETRI FISSI -----
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
# grafico info uni_thetai prendendo solo la prima combo di item da numerosità di item

uni_theta_random = NULL 
temp = NULL
for(i in 1:length(unique(data_random_uni_theta$num_item))) {
  temp = data_random_uni_theta[data_random_uni_theta$num_item %in% unique(data_random_uni_theta$num_item)[i], ]
  temp = temp[1, ]
  uni_theta_random = rbind(uni_theta_random, temp)
}
uni_theta_random$selection = "random" 
# uni_thetasco al data set 

data_uni_theta_random_unique = rbind(data.frame(num_item = uni_theta_random$num_item,
                                                      mean_info = uni_theta_random$info_total,
                                                      sd_info = 0,
                                                      selection = uni_theta_random$selection,
                                                      mean_rel = uni_theta_random$rel,
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
ggplot(data_uni_theta_random_unique[!data_uni_theta_random_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_info, 
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


# reliability solo prima selezione ---- 
ggplot(data_uni_theta_random_unique[!data_uni_theta_random_unique$num_item %in%"all", ], 
       aes(x=as.factor(num_item), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_uni_theta[!all_data_uni_theta$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_uni_theta[all_data_uni_theta$num_item %in% "all", 
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
                                     sel = "guidedNew")
  range_new_data_uni_theta  <- rbind(temp_range_new_uni_theta, 
                                 range_new_data_uni_theta )
}
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
# questo fa la media delle informatività 
# mean_random_uni_theta<- aggregate(info ~ theta + num_item, data = random_data_uni_theta, mean)
# mean_random_uni_theta<- mean_random_uni_theta[, c("theta", "info", "num_item")]
# mean_random_uni_theta$sel <- "random"

# questo seleziona sono una delle curve per ogni numerosità 

temp = NULL
new_random_uni_theta = NULL
for (i in 1:length(unique(random_data_uni_theta$num_item))) {
  temp = random_data_uni[random_data_uni_theta$num_item %in% unique(random_data_uni_theta$num_item)[i], ]
  temp = temp[1:1000, ]
  new_random_uni_theta = rbind(new_random_uni_theta, temp)
}


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


plots_uni_theta <- list()

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

# devo fare la media attraverso el colonne per oguno dei number item 
# temp_theta_random <- data.frame(matrix(ncol = length(unique(names(theta_random_uni_theta))), 
#                                        nrow = 1000))
# for (i in 1:length(unique(names(theta_random_uni_theta)))) {
#   temp_theta_random[, i] <- c(rowMeans(theta_random_uni_theta[[i]]))
#   colnames(temp_theta_random)[i] <- unique(names(theta_random_uni_theta))[i]
# }
temp_theta_random_theta = data.frame(matrix(ncol = length(unique(names(theta_random_uni))), 
                                      nrow = 1000))
for (i in 1:length(theta_random_uni_theta)) {
  temp_theta_random_theta[,i] = theta_random_uni_theta[[i]][,1]
  colnames(temp_theta_random_theta)[i] <- unique(names(theta_random_uni_theta))[i]
}

random = data.frame(selection = rep("random", nrow(temp_theta_random_theta)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random = cbind(sbj, random, temp_theta_random_theta)
theta_random_uni_theta <- reshape(temp_theta_random, 
                                  idvar = "sbj", 
                                  varying = list(3:(ncol(temp_theta_random))), 
                                  v.names = "theta_est", 
                                  direction = "long", 
                                  times = (names(temp_theta_random)[-c(1:2)]), 
                                  timevar = "num_item")
theta_random_uni_theta$obs <- true_theta_uni
theta_random_uni_theta$all <- theta_all_uni
head(theta_random_uni_theta)

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

random_bias_obs_theta = aggregate(bias_obs ~num_item, data = theta_random_uni_theta, 
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
random_rmse_all_theta = aggregate(bias_all_sq ~num_item, data = theta_random_uni_theta, 
                                  mean)
random_rmse_all_theta$selection = "random"
random_rmse_all_theta$type = "rmse_all"
random_bias_all_theta_abs = aggregate(bias_all_abs ~num_item, data = theta_random_uni_theta, 
                                      mean)
random_bias_all_theta_abs$selection = "random"
random_bias_all_theta_abs$type = "bias_all_abs"


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

uni_bias_obs_theta = rbind(random_bias_obs_theta, 
                           cluster_bias_obs_theta, 
                           range_new_bias_obs_theta, 
                           range_bias_obs_theta, 
                           smart_bias_obs_theta)

ggplot(uni_bias_obs_theta, 
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

uni_bias_abs_groups_theta = rbind(random_bias_abs_group_theta, 
                                  smart_bias_abs_group_theta, 
                                  cluster_bias_abs_group_theta, 
                                  range_bias_abs_group_theta, 
                                  range_new_bias_abs_group_theta)

ggplot(uni_bias_abs_groups, 
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

# theta_uni_theta <- rbind(theta_cluster_uni_theta, 
#                          theta_range_uni_theta, theta_range_new_uni_theta,
#                          theta_smart_uni_theta , theta_random_uni_theta
# )
# 
# theta_uni_theta$distribution <- "uni_theta"
# 
# # calcolo BIAS ----
# 
# theta_uni_theta$bias_obs <- with(theta_uni_theta, 
#                                  theta_est - obs)
# theta_uni_theta$bias_obs_abs <- with(theta_uni_theta, 
#                                      abs(theta_est - obs))
# 
# theta_uni_theta$bias_all <- with(theta_uni_theta, 
#                                  (theta_est - all))
# 
# theta_uni_theta$bias_all_abs <- with(theta_uni_theta, 
#                                      abs(theta_est - all))
# 
# obs_bias_uni_theta <- aggregate(bias_obs ~ num_item + selection, 
#                                 data = theta_uni_theta, 
#                                 mean)
# obs_bias_uni_theta$type <- "bias"
# obs_bias_uni_theta_abs <- aggregate(bias_obs_abs ~ num_item + selection, 
#                                     data = theta_uni_theta, 
#                                     mean)
# obs_bias_uni_theta_abs$type <- "bias_abs"
# colnames(obs_bias_uni_theta_abs)[3] <- "bias_obs"
# 
# obs_bias_uni_theta <- rbind(obs_bias_uni_theta, obs_bias_uni_theta_abs)
# 
# obs_bias_uni_theta$temp <- as.integer(gsub("number", "", 
#                                            obs_bias_uni_theta$num_item))
# obs_bias_uni_theta = obs_bias_uni_theta[order(obs_bias_uni_theta$temp), ]
# ggplot(obs_bias_uni_theta, 
#        aes(x=as.factor(temp), y = bias_obs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_grid(~type)
# 
# 
# all_bias_uni_theta <- aggregate(bias_all ~ num_item + selection, 
#                                 data = theta_uni_theta, 
#                                 mean)
# all_bias_uni_theta$type <- "bias"
# all_bias_uni_theta_abs <- aggregate(bias_all_abs ~ num_item + selection, 
#                                     data = theta_uni_theta, 
#                                     mean)
# all_bias_uni_theta_abs$type <- "bias_abs"
# colnames(all_bias_uni_theta_abs)[3] <- "bias_all"
# 
# all_bias_uni_theta <- rbind(all_bias_uni_theta, all_bias_uni_theta_abs)
# 
# all_bias_uni_theta$temp <- as.integer(gsub("number", "", 
#                                            all_bias_uni_theta$num_item))
# all_bias_uni_theta = all_bias_uni_theta[order(all_bias_uni_theta$temp), ]
# ggplot(all_bias_uni_theta, 
#        aes(x=as.factor(temp), y = bias_all, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(~type)
# 
# all_bias_uni_theta$rmsea <- sqrt(all_bias_uni_theta$bias_all)
# ggplot(all_bias_uni_theta[all_bias_uni_theta$type %in% "bias_abs", ], 
#        aes(x=as.factor(temp), y = rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(~type)
# 
# 
# # calcolo RMSEA ----- 
# theta_uni_theta$bias_obs_sq <- theta_uni_theta$bias_obs^2
# 
# theta_uni_theta$bias_all_sq <- theta_uni_theta$bias_all^2
# 
# obs_bias_uni_sq_theta <- aggregate(bias_obs_sq ~ num_item + selection, 
#                              data = theta_uni_theta, 
#                              mean)
# obs_bias_uni_sq_theta$type <- "bias_sq_obs"
# obs_bias_uni_sq_theta$rmsea = sqrt(obs_bias_uni_sq_theta$bias_obs_sq)
# 
# ggplot(obs_bias_uni_sq_theta, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 
# 
# 
# all_bias_uni_sq_theta <- aggregate(bias_all_sq ~ num_item + selection, 
#                              data = theta_uni_theta, 
#                              mean)
# all_bias_uni_sq_theta$type <- "bias_sq_all"
# all_bias_uni_sq_theta$rmsea = sqrt(all_bias_uni_sq_theta$bias_all)
# 
# ggplot(all_bias_uni_sq_theta, 
#        aes(x=num_item, y=rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)
# 
# # con gruppi latenti theta con parametri liberi ---- 
# theta_lat_uni <- seq(-2.5, 2.5, length.out = 4) 
# g <- cut(theta_lat_uni, length(theta_lat_uni), include.lowest = TRUE)
# cut_val = cut_borders(g)
# 
# 
# 
# group_name <- letters[1:(nrow(cut_val)+2)]
# 
# 
# theta_uni$group <- ifelse(theta_uni$obs  <= cut_val[1, "start"], 
#                           group_name[1], 
#                           ifelse(theta_uni$obs > cut_val[1, "start"] & theta_uni$obs <= cut_val[1, "end"], 
#                                  group_name[2], 
#                                  ifelse(theta_uni$obs > cut_val[2, "start"] & theta_uni$obs <= cut_val[2, "end"], 
#                                         group_name[3], 
#                                         ifelse(theta_uni$obs > cut_val[3, "start"] & theta_uni$obs <= cut_val[3, "end"], 
#                                                group_name[4], 
#                                                ifelse(theta_uni$obs > cut_val[4, "start"] & theta_uni$obs <= cut_val[4, "end"], 
#                                                       group_name[5], group_name[6])))))
# 
# bias_abs_uni_group <- aggregate(bias_obs_abs ~ selection + group + num_item, 
#                                 data = theta_uni, mean)
# bias_abs_uni_group$type = "bias"
# 
# ggplot(bias_abs_uni_group, 
#        aes(x=group, y = bias_obs_abs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)
# 
# rmsea_uni_group <- aggregate(bias_obs_sq ~ selection + group + num_item, 
#                              data = theta_uni, mean)
# rmsea_uni_group$rmsea = sqrt(rmsea_uni_group$bias_obs_sq)
# 
# ggplot(rmsea_uni_group, 
#        aes(x=group, y = rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), 
#                                            lwd =1.3) + 
#   theme(legend.position = "top") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)
# 
# # con gruppi latenti uni theta  -----
# theta_lat_theta_uni <- seq(-2.5, 2.5, length.out = 4) 
# g <- cut(theta_lat_theta_uni, length(theta_lat_theta_uni), include.lowest = TRUE)
# cut_val = cut_borders(g)
# 
# 
# 
# group_name <- letters[1:(nrow(cut_val)+2)]
# 
# 
# theta_uni_theta$group <- ifelse(theta_uni_theta$obs  <= cut_val[1, "start"], 
#                                 group_name[1], 
#                                 ifelse(theta_uni_theta$obs > cut_val[1, "start"] & theta_uni_theta$obs <= cut_val[1, "end"], 
#                                        group_name[2], 
#                                        ifelse(theta_uni_theta$obs > cut_val[2, "start"] & theta_uni_theta$obs <= cut_val[2, "end"], 
#                                               group_name[3], 
#                                               ifelse(theta_uni_theta$obs > cut_val[3, "start"] & theta_uni_theta$obs <= cut_val[3, "end"], 
#                                                      group_name[4], 
#                                                      ifelse(theta_uni_theta$obs > cut_val[4, "start"] & theta_uni_theta$obs <= cut_val[4, "end"], 
#                                                             group_name[5], group_name[6])))))
# 
# bias_abs_uni_group_theta <- aggregate(bias_obs_abs ~ selection + group + num_item, 
#                                       data = theta_uni_theta, mean)
# bias_abs_uni_group_theta$type = "bias"
# 
# ggplot(bias_abs_uni_group_theta, 
#        aes(x=group, y = bias_obs_abs, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)
# 
# rmsea_uni_group_theta <- aggregate(bias_obs_sq ~ selection + group + num_item, 
#                              data = theta_uni_theta, mean)
# rmsea_uni_group_theta$rmsea = sqrt(rmsea_uni_group_theta$bias_obs_sq)
# 
# ggplot(rmsea_uni_group_theta, 
#        aes(x=group, y = rmsea, group = selection, 
#            color = selection)) + geom_line(aes(linetype = selection), 
#                                            lwd =1.3) + 
#   theme(legend.position = "top") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
#   facet_wrap(~num_item)



rm(list= ls()[!(ls() %in% c("m2pl_uni",
                            'obs_bias_uni_theta','all_bias_uni_theta', 
                            "theta_uni_theta", "all_data_uni_theta", 
                            "data_info_uni_theta",
                            'obs_bias_uni','all_bias_uni', 
                            "theta_uni", "all_data_uni", 
                            "data_info_uni", 
                            "bias_abs_uni_group", 
                            "bias_abs_uni_group_theta", 
                            "obs_bias_uni_sq", "obs_bias_uni_sq_theta", 
                            "all_bias_uni_sq", "all_bias_uni_sq_theta", 
                            "rmsea_uni_group", "rmsea_uni_group_theta", 
                            "data_random_uni", "data_random_uni_theta", 
                            "data_uni_random_unique", 
                            "data_uni_theta_random_unique", 
                            "uni_bias_obs_theta",
                            "uni_bias_obs_abs_theta",
                            "uni_bias_all_theta",
                            "uni_bias_all_abs_theta",
                            "uni_rmse_obs_theta",
                            "uni_rmse_all_theta",
                            "uni_rmse_groups_theta",
                            "uni_bias_abs_groups_theta",
                            "uni_bias_groups_theta"))])

# dati skewness extreme ----- 
# Dati sk_exewness ----
rm(list = ls())
load("SKrandomEX.RData")
load("SKsmartEX.RData")
load("SKguidedEX.RData")
load("SKguidedEXnew.RData")
load("SKclusterEX.RData")
# Stessa cosa item con theta sk_ex -----
data_random_sk_ex_summary$selection <- "random"
info_summary_range_new_sk_ex$selection = "guidedNew"
all_data_sk_ex <- rbind(data_random_sk_ex_summary, 
                        data.frame(num_item = info_summary_range_sk_ex$range_name, 
                                   mean_info = info_summary_range_sk_ex$info_test, 
                                   sd_info = 0, 
                                   selection = info_summary_range_sk_ex$selection, 
                                   mean_rel = info_summary_range_sk_ex$rel, 
                                   sd_rel = 0), 
                        data.frame(num_item = info_summary_range_new_sk_ex$range_new_name, 
                                   mean_info = info_summary_range_new_sk_ex$info_test, 
                                   sd_info = 0, 
                                   selection = info_summary_range_new_sk_ex$selection, 
                                   mean_rel = info_summary_range_new_sk_ex$rel, 
                                   sd_rel = 0), 
                        data.frame(num_item = info_summary_cluster_sk_ex$cluster_name, 
                                   mean_info = info_summary_cluster_sk_ex$info_test, 
                                   sd_info = 0, 
                                   selection = info_summary_cluster_sk_ex$selection, 
                                   mean_rel = info_summary_cluster_sk_ex$rel, 
                                   sd_rel = 0), 
                        
                        data.frame(num_item = info_summary_smart_sk_ex$smart_name, 
                                   mean_info = info_summary_smart_sk_ex$info_test, 
                                   sd_info = 0, 
                                   selection = info_summary_smart_sk_ex$selection, 
                                   mean_rel = info_summary_smart_sk_ex$rel, 
                                   sd_rel = 0))
all_data_sk_ex$item_temp <- gsub("number", "", all_data_sk_ex$num_item)
all_data_sk_ex$item_temp <- gsub("all", 0, all_data_sk_ex$item_temp)
all_data_sk_ex$item_temp <- as.integer(all_data_sk_ex$item_temp)
all_data_sk_ex <- all_data_sk_ex[order(all_data_sk_ex$item_temp), ]
all_data_sk_ex$selection <- gsub("sk_ex", '', all_data_sk_ex$selection)
ggplot(all_data_sk_ex[!all_data_sk_ex$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_sk_ex[!all_data_sk_ex$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk_ex[all_data_sk_ex$num_item %in% "all", 
                                         "mean_info"])

# stesso garfico ma con le reliability 

ggplot(all_data_sk_ex[!all_data_sk_ex$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1) +
  geom_point(aes(shape=selection), size =2)+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels = unique(all_data_sk_ex[!all_data_sk_ex$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk_ex[all_data_sk_ex$num_item %in% "all", 
                                         "mean_rel"]) + ylim(0,1)

grid.arrange(ggplot(all_data_sk_ex[!all_data_sk_ex$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_info, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "top") + 
               scale_x_discrete(labels =  unique(all_data_sk_ex[!all_data_sk_ex$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_sk_ex[all_data_sk_ex$num_item %in% "all", 
                                                      "mean_info"]), 
             ggplot(all_data_sk_ex[!all_data_sk_ex$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_rel, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1) +
               geom_point(aes(shape=selection), size =2)+
               geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "none") + 
               scale_x_discrete(labels = unique(all_data_sk_ex[!all_data_sk_ex$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_sk_ex[all_data_sk_ex$num_item %in% "all", 
                                                      "mean_rel"]))

# confronto TIF dati sk_ex -----

temp_cluster_sk_ex<- NULL
cluster_data_sk_ex <- NULL

temp_smart_sk_ex <- NULL
smart_data_sk_ex <- NULL


temp_range_sk_ex <- NULL
range_data_sk_ex  <- NULL

temp_range_new_sk_ex <- NULL
range_new_data_sk_ex  <- NULL


for (i in 1:length(info_out_smart_sk_ex)) {
  temp_cluster_sk_ex<- data.frame(theta = info_out_cluster_sk_ex[[i]]$theta,
                                  info = info_out_cluster_sk_ex[[i]]$test_info_curve, 
                                  num_item = names(info_out_cluster_sk_ex)[[i]], 
                                  sel = "cluster")
  cluster_data_sk_ex <- rbind(temp_cluster_sk_ex, 
                              cluster_data_sk_ex)
  
  temp_smart_sk_ex <- data.frame(theta = info_out_smart_sk_ex[[i]]$theta,
                                 info = info_out_smart_sk_ex[[i]]$test_info_curve, 
                                 num_item = names(info_out_smart_sk_ex)[[i]], 
                                 sel = "smart")
  smart_data_sk_ex <- rbind(temp_smart_sk_ex, 
                            smart_data_sk_ex)
  temp_range_sk_ex <- data.frame(theta = info_out_range_sk_ex[[i]]$theta,
                                 info = info_out_range_sk_ex[[i]]$test_info_curve, 
                                 num_item = names(info_out_range_sk_ex)[[i]], 
                                 sel = "guided")
  range_data_sk_ex  <- rbind(temp_range_sk_ex, 
                             range_data_sk_ex )
  
  temp_range_new_sk_ex <- data.frame(theta = info_out_range_new_sk_ex[[i]]$theta,
                                 info = info_out_range_new_sk_ex[[i]]$test_info_curve, 
                                 num_item = names(info_out_range_new_sk_ex)[[i]], 
                                 sel = "guidedNew")
  range_new_data_sk_ex  <- rbind(temp_range_new_sk_ex, 
                             range_new_data_sk_ex )
  
}
temp_random_sk_ex <- NULL
random_data_sk_ex <- NULL
for (i in 1:length(info_test_random_sk_ex)) {
  temp_random_sk_ex <- data.frame(theta = info_test_random_sk_ex[[i]]$theta,
                                  info = info_test_random_sk_ex[[i]]$test_info_curve, 
                                  num_item = paste0("number", 
                                                    nrow(info_test_random_sk_ex[[i]]$info_curves_item)), 
                                  sel = "random")
  random_data_sk_ex <- rbind(temp_random_sk_ex, 
                             random_data_sk_ex)
}

mean_random_sk_ex<- aggregate(info ~ theta + num_item, data = random_data_sk_ex, mean)
mean_random_sk_ex<- mean_random_sk_ex[, c("theta", "info", "num_item")]
mean_random_sk_ex$sel <- "random"
start_data_sk_ex <- data.frame(theta = IRT.informationCurves(m2pl_sk_ex, 
                                                             theta = seq(-3, 3, 
                                                                         length = 1000))$theta,
                               info = info_start_sk_ex, 
                               num_item = "all", 
                               sel = "start")

data_info_sk_ex <- rbind(
  cluster_data_sk_ex,
  range_data_sk_ex ,range_new_data_sk_ex , 
  smart_data_sk_ex ,mean_random_sk_ex
)


graph_start_sk_ex <- data.frame(theta = IRT.informationCurves(m2pl_sk_ex, 
                                                              theta = seq(-3,3,length = 1000))$theta, 
                                info = (IRT.informationCurves(m2pl_sk_ex, 
                                                              theta = seq(-3,3,length = 1000))$test_info_curve), 
                                num_item = "all",
                                sel = "start")


plots_sk_ex <- list()

for(i in 1:length(unique(data_info_sk_ex$num_item))) {
  
  plots_sk_ex[[i]] <-  ggplot(rbind(data_info_sk_ex[data_info_sk_ex$num_item %in% unique(data_info_sk_ex$num_item)[i], ], 
                                    graph_start_sk_ex), 
                              aes(x = theta, y = info, group = sel, 
                                  col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4) + 
    ggtitle(unique(data_info_sk_ex$num_item)[i]) +
    theme(legend.position = "none")
}


ggplot(rbind(data_info_sk_ex[data_info_sk_ex$num_item %in% "number10", ], 
             graph_start_sk_ex), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4)

do.call(grid.arrange, plots_sk_ex)



# Costruisco un dataframe per il calcolo del bias per le stime di theta sk_ex ----

for (i in 1:length(model_fit_random_sk_ex)) {
  names(model_fit_random_sk_ex)[[i]] <- paste0("number", 
                                               nrow(model_fit_random_sk_ex[[i]]$xsi))
}

temp <- NULL
random_theta <- NULL
list_temp <- NULL
theta_random_sk_ex <- list()

for (i in 1:length(unique(names(model_fit_random_sk_ex)))) {
  temp <- model_fit_random_sk_ex[names(model_fit_random_sk_ex) == unique(names(model_fit_random_sk_ex))[i]]
  
  for (j in 1:length(temp)) {
    list_temp <- temp[[j]]$person$EAP
    random_theta <- data.frame(cbind(random_theta, list_temp))
    theta_random_sk_ex[[i]] <- random_theta
    names(theta_random_sk_ex)[[i]] <- unique(names(model_fit_random_sk_ex))[i]
  }
}

# devo fare la media attraverso el colonne per oguno dei number item 
temp_theta_random <- data.frame(matrix(ncol = length(unique(names(theta_random_sk_ex))), 
                                       nrow = 1000))
for (i in 1:length(unique(names(theta_random_sk_ex)))) {
  temp_theta_random[, i] <- c(rowMeans(theta_random_sk_ex[[i]]))
  colnames(temp_theta_random)[i] <- unique(names(theta_random_sk_ex))[i]
}
random = data.frame(selection = rep("random", nrow(temp_theta_random)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random = cbind(sbj, random, temp_theta_random)
theta_random_sk_ex <- reshape(temp_theta_random, 
                              idvar = "sbj", 
                              varying = list(3:(ncol(temp_theta_random))), 
                              v.names = "theta_est", 
                              direction = "long", 
                              times = (names(temp_theta_random)[-c(1:2)]), 
                              timevar = "num_item")
theta_random_sk_ex$obs <- true_theta_sk_ex
theta_random_sk_ex$all <- theta_all_sk_ex

for (i in 1:length(model_out_cluster_sk_ex)) {
  names(model_out_cluster_sk_ex)[[i]] <- paste0("number", 
                                                nrow(model_out_cluster_sk_ex[[i]]$xsi))
  names(model_out_range_sk_ex)[[i]] <- paste0("number", 
                                              nrow(model_out_range_sk_ex[[i]]$xsi))
  names(model_out_range_new_sk_ex)[[i]] <- paste0("number", 
                                              nrow(model_out_range_new_sk_ex[[i]]$xsi))
  names(model_out_smart_sk_ex)[[i]] <- paste0("number", 
                                              nrow(model_out_smart_sk_ex[[i]]$xsi))
}

temp_theta_cluster_sk_ex <- data.frame(matrix(ncol = length(unique(names(model_out_cluster_sk_ex))), 
                                              nrow = 1000))

temp_theta_range_sk_ex <- data.frame(matrix(ncol = length(unique(names(model_out_range_sk_ex))), 
                                            nrow = 1000))
temp_theta_range_new_sk_ex <- data.frame(matrix(ncol = length(unique(names(model_out_range_new_sk_ex))), 
                                            nrow = 1000))
temp_theta_smart_sk_ex <- data.frame(matrix(ncol = length(unique(names(model_out_range_sk_ex))), 
                                            nrow = 1000))

for (i in 1:length(unique(names(model_out_cluster_sk_ex)))) {
  temp_theta_cluster_sk_ex[, i] <- model_out_cluster_sk_ex[[i]]$person$EAP
  colnames(temp_theta_cluster_sk_ex)[i] <- unique(names(model_out_cluster_sk_ex))[i]
  
  temp_theta_range_sk_ex[, i] <- model_out_range_sk_ex[[i]]$person$EAP
  colnames(temp_theta_range_sk_ex)[i] <- unique(names(model_out_range_sk_ex))[i]
  
  temp_theta_range_new_sk_ex[, i] <- model_out_range_new_sk_ex[[i]]$person$EAP
  colnames(temp_theta_range_new_sk_ex)[i] <- unique(names(model_out_range_new_sk_ex))[i]
  
  temp_theta_smart_sk_ex[, i] <- model_out_smart_sk_ex[[i]]$person$EAP
  colnames(temp_theta_smart_sk_ex)[i] <- unique(names(model_out_smart_sk_ex))[i]
  
}


sbj <- data.frame(sbj =1:1000)

cluster = data.frame(selection = rep("cluster", nrow(sbj)))
range = data.frame(selection = rep("range", nrow(sbj)))
range_new = data.frame(selection = rep("rangeNew", nrow(sbj)))
smart = data.frame(selection = rep("smart", nrow(sbj)))


temp_theta_cluster_sk_ex <- cbind(sbj, cluster,  temp_theta_cluster_sk_ex)

theta_cluster_sk_ex <- reshape(temp_theta_cluster_sk_ex, 
                               idvar = "sbj", 
                               varying = list(3:(ncol(temp_theta_cluster_sk_ex))), 
                               v.names = "theta_est", 
                               direction = "long", 
                               times = (names(temp_theta_cluster_sk_ex)[-c(1:2)]), 
                               timevar = "num_item")
theta_cluster_sk_ex$obs <- true_theta_sk_ex
theta_cluster_sk_ex$all <- theta_all_sk_ex
head(theta_cluster_sk_ex)

temp_theta_range_sk_ex <- cbind(sbj, range,  temp_theta_range_sk_ex)

theta_range_sk_ex <- reshape(temp_theta_range_sk_ex, 
                             idvar = "sbj", 
                             varying = list(3:(ncol(temp_theta_range_sk_ex))), 
                             v.names = "theta_est", 
                             direction = "long", 
                             times = (names(temp_theta_range_sk_ex)[-c(1:2)]), 
                             timevar = "num_item")
theta_range_sk_ex$obs <- true_theta_sk_ex
theta_range_sk_ex$all <- theta_all_sk_ex
head(theta_range_sk_ex)

temp_theta_range_new_sk_ex <- cbind(sbj, range_new,  temp_theta_range_new_sk_ex)

theta_range_new_sk_ex <- reshape(temp_theta_range_new_sk_ex, 
                             idvar = "sbj", 
                             varying = list(3:(ncol(temp_theta_range_new_sk_ex))), 
                             v.names = "theta_est", 
                             direction = "long", 
                             times = (names(temp_theta_range_new_sk_ex)[-c(1:2)]), 
                             timevar = "num_item")
theta_range_new_sk_ex$obs <- true_theta_sk_ex
theta_range_new_sk_ex$all <- theta_all_sk_ex
head(theta_range_new_sk_ex)

temp_theta_smart_sk_ex <- cbind(sbj, smart,  temp_theta_smart_sk_ex)

theta_smart_sk_ex <- reshape(temp_theta_smart_sk_ex, 
                             idvar = "sbj", 
                             varying = list(3:(ncol(temp_theta_smart_sk_ex))), 
                             v.names = "theta_est", 
                             direction = "long", 
                             times = (names(temp_theta_smart_sk_ex)[-c(1:2)]), 
                             timevar = "num_item")
theta_smart_sk_ex$obs <- true_theta_sk_ex
theta_smart_sk_ex$all <- theta_all_sk_ex

theta_sk_ex <- rbind(theta_cluster_sk_ex, 
                     theta_range_sk_ex, theta_range_new_sk_ex, 
                     theta_smart_sk_ex , theta_random_sk_ex
)

theta_sk_ex$distribution <- "sk_ex"
theta_sk_ex <- theta_sk_ex[order(theta_sk_ex$obs), ]

# calcolo bias 

theta_sk_ex$bias_obs <- with(theta_sk_ex, 
                             theta_est - obs)
theta_sk_ex$bias_obs_abs <- with(theta_sk_ex, 
                                 abs(theta_est - obs))

theta_sk_ex$bias_all <- with(theta_sk_ex, 
                             (theta_est - all))

theta_sk_ex$bias_all_abs <- with(theta_sk_ex, 
                                 abs(theta_est - all))

obs_bias_sk_ex <- aggregate(bias_obs ~ num_item + selection, 
                            data = theta_sk_ex, 
                            mean)
obs_bias_sk_ex$type <- "bias"
obs_bias_sk_ex_abs <- aggregate(bias_obs_abs ~ num_item + selection, 
                                data = theta_sk_ex, 
                                mean)
obs_bias_sk_ex_abs$type <- "bias_abs"
colnames(obs_bias_sk_ex_abs)[3] <- "bias_obs"

obs_bias_sk_ex <- rbind(obs_bias_sk_ex, obs_bias_sk_ex_abs)

obs_bias_sk_ex$temp <- as.integer(gsub("number", "", 
                                       obs_bias_sk_ex$num_item))
obs_bias_sk_ex = obs_bias_sk_ex[order(obs_bias_sk_ex$temp), ]
ggplot(obs_bias_sk_ex, 
       aes(x=as.factor(temp), y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_grid(~type)


all_bias_sk_ex <- aggregate(bias_all ~ num_item + selection, 
                            data = theta_sk_ex, 
                            mean)
all_bias_sk_ex$type <- "bias"
all_bias_sk_ex_abs <- aggregate(bias_all_abs ~ num_item + selection, 
                                data = theta_sk_ex, 
                                mean)
all_bias_sk_ex_abs$type <- "bias_abs"
colnames(all_bias_sk_ex_abs)[3] <- "bias_all"

all_bias_sk_ex <- rbind(all_bias_sk_ex, all_bias_sk_ex_abs)

all_bias_sk_ex$temp <- as.integer(gsub("number", "", 
                                       all_bias_sk_ex$num_item))
all_bias_sk_ex = all_bias_sk_ex[order(all_bias_sk_ex$temp), ]
ggplot(all_bias_sk_ex, 
       aes(x=as.factor(temp), y = bias_all, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(~type)

# calcolo rmsea -----

theta_sk_ex$bias_obs_sq <- theta_sk_ex$bias_obs^2

theta_sk_ex$bias_all_sq <- theta_sk_ex$bias_all^2

obs_bias_sk_ex_sq <- aggregate(bias_obs_sq ~ num_item + selection, 
                               data = theta_sk_ex, 
                               mean)
obs_bias_sk_ex_sq$type <- "bias_sq_obs"
obs_bias_sk_ex_sq$rmsea = sqrt(obs_bias_sk_ex_sq$bias_obs_sq)

ggplot(obs_bias_sk_ex_sq, 
       aes(x=num_item, y=rmsea, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)

all_bias_sk_ex_sq <- aggregate(bias_all_sq ~ num_item + selection, 
                               data = theta_sk_ex, 
                               mean)
all_bias_sk_ex_sq$type <- "bias_sq_all"
all_bias_sk_ex_sq$rmsea = sqrt(all_bias_sk_ex_sq$bias_all)

ggplot(all_bias_sk_ex_sq, 
       aes(x=num_item, y=rmsea, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)


# per gruppi latenti ----- 
theta_lat_sk_ex <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat_sk_ex, length(theta_lat_sk_ex), include.lowest = TRUE)
cut_val = cut_borders(g)



group_name <- letters[1:(nrow(cut_val)+2)]


theta_sk_ex$group <- ifelse(theta_sk_ex$obs  <= cut_val[1, "start"], 
                            group_name[1], 
                            ifelse(theta_sk_ex$obs > cut_val[1, "start"] & theta_sk_ex$obs <= cut_val[1, "end"], 
                                   group_name[2], 
                                   ifelse(theta_sk_ex$obs > cut_val[2, "start"] & theta_sk_ex$obs <= cut_val[2, "end"], 
                                          group_name[3], 
                                          ifelse(theta_sk_ex$obs > cut_val[3, "start"] & theta_sk_ex$obs <= cut_val[3, "end"], 
                                                 group_name[4], 
                                                 ifelse(theta_sk_ex$obs > cut_val[4, "start"] & theta_sk_ex$obs <= cut_val[4, "end"], 
                                                        group_name[5], group_name[6])))))

bias_abs_sk_ex_group <- aggregate(bias_obs_abs ~ selection + group + num_item, 
                                  data = theta_sk_ex, mean)
bias_abs_sk_ex_group$type = "bias"

ggplot(bias_abs_sk_ex_group, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

bias_abs_sk_ex_group <- aggregate(bias_obs_abs ~ selection + group + num_item, 
                                  data = theta_sk_ex, mean)
bias_abs_sk_ex_group$type = "bias"

# rmsea ----
rmsea_sk_ex_group <- aggregate(bias_obs_sq ~ selection + group + num_item, 
                               data = theta_sk_ex, mean)
rmsea_sk_ex_group$rmsea = sqrt(rmsea_sk_ex_group$bias_obs_sq)

ggplot(rmsea_sk_ex_group, 
       aes(x=group, y = rmsea, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)



# Stessa cosa item con theta sk_ex_theta PARAMETRI FISSI -----
data_random_sk_ex_theta_summary$selection <- "random"
all_data_sk_ex_theta <- rbind(data_random_sk_ex_theta_summary, 
                              data.frame(num_item = info_summary_range_sk_ex_theta$range_name, 
                                         mean_info = info_summary_range_sk_ex_theta$info_test, 
                                         sd_info = 0, 
                                         selection = info_summary_range_sk_ex_theta$selection, 
                                         mean_rel = info_summary_range_sk_ex_theta$rel, 
                                         sd_rel = 0), 
                              data.frame(num_item = info_summary_range_new_sk_ex_theta$range_new_name, 
                                         mean_info = info_summary_range_new_sk_ex_theta$info_test, 
                                         sd_info = 0, 
                                         selection = info_summary_range_new_sk_ex_theta$selection, 
                                         mean_rel = info_summary_range_new_sk_ex_theta$rel, 
                                         sd_rel = 0),
                              data.frame(num_item = info_summary_cluster_sk_ex_theta$cluster_name, 
                                         mean_info = info_summary_cluster_sk_ex_theta$info_test, 
                                         sd_info = 0, 
                                         selection = info_summary_cluster_sk_ex_theta$selection, 
                                         mean_rel = info_summary_cluster_sk_ex_theta$rel, 
                                         sd_rel = 0), 
                              
                              data.frame(num_item = info_summary_smart_sk_ex_theta$smart_name, 
                                         mean_info = info_summary_smart_sk_ex_theta$info_test, 
                                         sd_info = 0, 
                                         selection = info_summary_smart_sk_ex_theta$selection, 
                                         mean_rel = info_summary_smart_sk_ex_theta$rel, 
                                         sd_rel = 0))
all_data_sk_ex_theta$item_temp <- gsub("number", "", all_data_sk_ex_theta$num_item)
all_data_sk_ex_theta$item_temp <- gsub("all", 0, all_data_sk_ex_theta$item_temp)
all_data_sk_ex_theta$item_temp <- as.integer(all_data_sk_ex_theta$item_temp)
all_data_sk_ex_theta <- all_data_sk_ex_theta[order(all_data_sk_ex_theta$item_temp), ]
all_data_sk_ex_theta$selection <- gsub("sk_ex_theta", '', all_data_sk_ex_theta$selection)
ggplot(all_data_sk_ex_theta[!all_data_sk_ex_theta$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels =  unique(all_data_sk_ex_theta[!all_data_sk_ex_theta$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk_ex_theta[all_data_sk_ex_theta$num_item %in% "all", 
                                               "mean_info"])

# stesso garfico ma con le reliability 

ggplot(all_data_sk_ex_theta[!all_data_sk_ex_theta$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1) +
  geom_point(aes(shape=selection), size =2)+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_discrete(labels = unique(all_data_sk_ex_theta[!all_data_sk_ex_theta$num_item %in%"all", "num_item"])) + 
  geom_hline(yintercept = all_data_sk_ex_theta[all_data_sk_ex_theta$num_item %in% "all", 
                                               "mean_rel"]) + ylim(0,1)

grid.arrange(ggplot(all_data_sk_ex_theta[!all_data_sk_ex_theta$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_info, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "top") + 
               scale_x_discrete(labels =  unique(all_data_sk_ex_theta[!all_data_sk_ex_theta$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_sk_ex_theta[all_data_sk_ex_theta$num_item %in% "all", 
                                                            "mean_info"]), 
             ggplot(all_data_sk_ex_theta[!all_data_sk_ex_theta$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_rel, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1) +
               geom_point(aes(shape=selection), size =2)+
               geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "none") + 
               scale_x_discrete(labels = unique(all_data_sk_ex_theta[!all_data_sk_ex_theta$num_item %in%"all", "num_item"])) + 
               geom_hline(yintercept = all_data_sk_ex_theta[all_data_sk_ex_theta$num_item %in% "all", 
                                                            "mean_rel"]))

# confronto TIF dati sk_ex_theta -----

temp_cluster_sk_ex_theta<- NULL
cluster_data_sk_ex_theta <- NULL

temp_smart_sk_ex_theta <- NULL
smart_data_sk_ex_theta <- NULL


temp_range_sk_ex_theta <- NULL
range_data_sk_ex_theta  <- NULL

temp_range_new_sk_ex_theta <- NULL
range_new_data_sk_ex_theta  <- NULL


for (i in 1:length(info_out_smart_sk_ex_theta)) {
  temp_cluster_sk_ex_theta<- data.frame(theta = info_out_cluster_sk_ex_theta[[i]]$theta,
                                        info = info_out_cluster_sk_ex_theta[[i]]$test_info_curve, 
                                        num_item = names(info_out_cluster_sk_ex_theta)[[i]], 
                                        sel = "cluster")
  cluster_data_sk_ex_theta <- rbind(temp_cluster_sk_ex_theta, 
                                    cluster_data_sk_ex_theta)
  
  temp_smart_sk_ex_theta <- data.frame(theta = info_out_smart_sk_ex_theta[[i]]$theta,
                                       info = info_out_smart_sk_ex_theta[[i]]$test_info_curve, 
                                       num_item = names(info_out_smart_sk_ex_theta)[[i]], 
                                       sel = "smart")
  smart_data_sk_ex_theta <- rbind(temp_smart_sk_ex_theta, 
                                  smart_data_sk_ex_theta)
  temp_range_sk_ex_theta <- data.frame(theta = info_out_range_sk_ex_theta[[i]]$theta,
                                       info = info_out_range_sk_ex_theta[[i]]$test_info_curve, 
                                       num_item = names(info_out_range_sk_ex_theta)[[i]], 
                                       sel = "guided")
  range_data_sk_ex_theta  <- rbind(temp_range_sk_ex_theta, 
                                   range_data_sk_ex_theta )
  
  temp_range_new_sk_ex_theta <- data.frame(theta = info_out_range_new_sk_ex_theta[[i]]$theta,
                                       info = info_out_range_new_sk_ex_theta[[i]]$test_info_curve, 
                                       num_item = names(info_out_range_new_sk_ex_theta)[[i]], 
                                       sel = "guidedNew")
  range_new_data_sk_ex_theta  <- rbind(temp_range_new_sk_ex_theta, 
                                   range_new_data_sk_ex_theta )
  
}
temp_random_sk_ex_theta <- NULL
random_data_sk_ex_theta <- NULL
for (i in 1:length(info_test_random_sk_ex_theta)) {
  temp_random_sk_ex_theta <- data.frame(theta = info_test_random_sk_ex_theta[[i]]$theta,
                                        info = info_test_random_sk_ex_theta[[i]]$test_info_curve, 
                                        num_item = paste0("number", 
                                                          nrow(info_test_random_sk_ex_theta[[i]]$info_curves_item)), 
                                        sel = "random")
  random_data_sk_ex_theta <- rbind(temp_random_sk_ex_theta, 
                                   random_data_sk_ex_theta)
}

mean_random_sk_ex_theta<- aggregate(info ~ theta + num_item, data = random_data_sk_ex_theta, mean)
mean_random_sk_ex_theta<- mean_random_sk_ex_theta[, c("theta", "info", "num_item")]
mean_random_sk_ex_theta$sel <- "random"
start_data_sk_ex_theta <- data.frame(theta = IRT.informationCurves(m2pl_sk_ex, 
                                                                   theta = seq(-3, 3, 
                                                                               length = 1000))$theta,
                                     info = info_start_sk_ex, 
                                     num_item = "all", 
                                     sel = "start")

data_info_sk_ex_theta <- rbind(
  cluster_data_sk_ex_theta,
  range_data_sk_ex_theta , range_new_data_sk_ex_theta , 
  smart_data_sk_ex_theta ,mean_random_sk_ex_theta
)


plots_sk_ex_theta <- list()

for(i in 1:length(unique(data_info_sk_ex_theta$num_item))) {
  
  plots_sk_ex_theta[[i]] <-  ggplot(rbind(data_info_sk_ex_theta[data_info_sk_ex_theta$num_item %in% unique(data_info_sk_ex_theta$num_item)[i], ], 
                                          graph_start_sk_ex), 
                                    aes(x = theta, y = info, group = sel, 
                                        col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4) + 
    ggtitle(unique(data_info_sk_ex_theta$num_item)[i]) +
    theme(legend.position = "none")
}


ggplot(rbind(data_info_sk_ex_theta[data_info_sk_ex_theta$num_item %in% "number10", ], 
             graph_start_sk_ex), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.4)

do.call(grid.arrange, plots_sk_ex_theta)


# Costruisco un dataframe per il calcolo del bias per le stime di theta sk_ex_theta ----

for (i in 1:length(model_fit_random_sk_ex_theta)) {
  names(model_fit_random_sk_ex_theta)[[i]] <- paste0("number", 
                                                     nrow(model_fit_random_sk_ex_theta[[i]]$xsi))
}

temp <- NULL
random_theta <- NULL
list_temp <- NULL
theta_random_sk_ex_theta <- list()

for (i in 1:length(unique(names(model_fit_random_sk_ex_theta)))) {
  temp <- model_fit_random_sk_ex_theta[names(model_fit_random_sk_ex_theta) == unique(names(model_fit_random_sk_ex_theta))[i]]
  
  for (j in 1:length(temp)) {
    list_temp <- temp[[j]]$person$EAP
    random_theta <- data.frame(cbind(random_theta, list_temp))
    theta_random_sk_ex_theta[[i]] <- random_theta
    names(theta_random_sk_ex_theta)[[i]] <- unique(names(model_fit_random_sk_ex_theta))[i]
  }
}

# devo fare la media attraverso el colonne per oguno dei number item 
temp_theta_random <- data.frame(matrix(ncol = length(unique(names(theta_random_sk_ex_theta))), 
                                       nrow = 1000))
for (i in 1:length(unique(names(theta_random_sk_ex_theta)))) {
  temp_theta_random[, i] <- c(rowMeans(theta_random_sk_ex_theta[[i]]))
  colnames(temp_theta_random)[i] <- unique(names(theta_random_sk_ex_theta))[i]
}
random = data.frame(selection = rep("random", nrow(temp_theta_random)))
sbj = data.frame(sbj = 1:1000)
temp_theta_random = cbind(sbj, random, temp_theta_random)
theta_random_sk_ex_theta <- reshape(temp_theta_random, 
                                    idvar = "sbj", 
                                    varying = list(3:(ncol(temp_theta_random))), 
                                    v.names = "theta_est", 
                                    direction = "long", 
                                    times = (names(temp_theta_random)[-c(1:2)]), 
                                    timevar = "num_item")
theta_random_sk_ex_theta$obs <- true_theta_sk_ex
theta_random_sk_ex_theta$all <- theta_all_sk_ex

for (i in 1:length(model_out_cluster_sk_ex_theta)) {
  names(model_out_cluster_sk_ex_theta)[[i]] <- paste0("number", 
                                                      nrow(model_out_cluster_sk_ex_theta[[i]]$xsi))
  names(model_out_range_sk_ex_theta)[[i]] <- paste0("number", 
                                                    nrow(model_out_range_sk_ex_theta[[i]]$xsi))
  
  names(model_out_range_new_sk_ex_theta)[[i]] <- paste0("number", 
                                                    nrow(model_out_range_new_sk_ex_theta[[i]]$xsi))
  
  names(model_out_smart_sk_ex_theta)[[i]] <- paste0("number", 
                                                    nrow(model_out_smart_sk_ex_theta[[i]]$xsi))
}

temp_theta_cluster_sk_ex_theta <- data.frame(matrix(ncol = length(unique(names(model_out_cluster_sk_ex_theta))), 
                                                    nrow = 1000))

temp_theta_range_sk_ex_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_sk_ex_theta))), 
                                                  nrow = 1000))

temp_theta_range_new_sk_ex_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_new_sk_ex_theta))), 
                                                  nrow = 1000))

temp_theta_smart_sk_ex_theta <- data.frame(matrix(ncol = length(unique(names(model_out_range_sk_ex_theta))), 
                                                  nrow = 1000))

for (i in 1:length(unique(names(model_out_cluster_sk_ex_theta)))) {
  temp_theta_cluster_sk_ex_theta[, i] <- model_out_cluster_sk_ex_theta[[i]]$person$EAP
  colnames(temp_theta_cluster_sk_ex_theta)[i] <- unique(names(model_out_cluster_sk_ex_theta))[i]
  
  temp_theta_range_sk_ex_theta[, i] <- model_out_range_sk_ex_theta[[i]]$person$EAP
  colnames(temp_theta_range_sk_ex_theta)[i] <- unique(names(model_out_range_sk_ex_theta))[i]
  
  temp_theta_range_new_sk_ex_theta[, i] <- model_out_range_new_sk_ex_theta[[i]]$person$EAP
  colnames(temp_theta_range_new_sk_ex_theta)[i] <- unique(names(model_out_range_new_sk_ex_theta))[i]
  
  temp_theta_smart_sk_ex_theta[, i] <- model_out_smart_sk_ex_theta[[i]]$person$EAP
  colnames(temp_theta_smart_sk_ex_theta)[i] <- unique(names(model_out_smart_sk_ex_theta))[i]
  
}


sbj <- data.frame(sbj =1:1000)

cluster = data.frame(selection = rep("cluster", nrow(sbj)))
range = data.frame(selection = rep("range", nrow(sbj)))
range_new = data.frame(selection = rep("rangeNew", nrow(sbj)))
smart = data.frame(selection = rep("smart", nrow(sbj)))


temp_theta_cluster_sk_ex_theta <- cbind(sbj, cluster,  temp_theta_cluster_sk_ex_theta)

theta_cluster_sk_ex_theta <- reshape(temp_theta_cluster_sk_ex_theta, 
                                     idvar = "sbj", 
                                     varying = list(3:(ncol(temp_theta_cluster_sk_ex_theta))), 
                                     v.names = "theta_est", 
                                     direction = "long", 
                                     times = (names(temp_theta_cluster_sk_ex_theta)[-c(1:2)]), 
                                     timevar = "num_item")
theta_cluster_sk_ex_theta$obs <- true_theta_sk_ex
theta_cluster_sk_ex_theta$all <- theta_all_sk_ex
head(theta_cluster_sk_ex_theta)

temp_theta_range_sk_ex_theta <- cbind(sbj, range,  temp_theta_range_sk_ex_theta)

theta_range_sk_ex_theta <- reshape(temp_theta_range_sk_ex_theta, 
                                   idvar = "sbj", 
                                   varying = list(3:(ncol(temp_theta_range_sk_ex_theta))), 
                                   v.names = "theta_est", 
                                   direction = "long", 
                                   times = (names(temp_theta_range_sk_ex_theta)[-c(1:2)]), 
                                   timevar = "num_item")
theta_range_sk_ex_theta$obs <- true_theta_sk_ex
theta_range_sk_ex_theta$all <- theta_all_sk_ex
head(theta_range_sk_ex_theta)


temp_theta_range_new_sk_ex_theta <- cbind(sbj, range_new,  temp_theta_range_new_sk_ex_theta)

theta_range_new_sk_ex_theta <- reshape(temp_theta_range_new_sk_ex_theta, 
                                   idvar = "sbj", 
                                   varying = list(3:(ncol(temp_theta_range_new_sk_ex_theta))), 
                                   v.names = "theta_est", 
                                   direction = "long", 
                                   times = (names(temp_theta_range_new_sk_ex_theta)[-c(1:2)]), 
                                   timevar = "num_item")
theta_range_new_sk_ex_theta$obs <- true_theta_sk_ex
theta_range_new_sk_ex_theta$all <- theta_all_sk_ex
head(theta_range_new_sk_ex_theta)


temp_theta_smart_sk_ex_theta <- cbind(sbj, smart,  temp_theta_smart_sk_ex_theta)

theta_smart_sk_ex_theta <- reshape(temp_theta_smart_sk_ex_theta, 
                                   idvar = "sbj", 
                                   varying = list(3:(ncol(temp_theta_smart_sk_ex_theta))), 
                                   v.names = "theta_est", 
                                   direction = "long", 
                                   times = (names(temp_theta_smart_sk_ex_theta)[-c(1:2)]), 
                                   timevar = "num_item")
theta_smart_sk_ex_theta$obs <- true_theta_sk_ex
theta_smart_sk_ex_theta$all <- theta_all_sk_ex

theta_sk_ex_theta <- rbind(theta_cluster_sk_ex_theta, 
                           theta_range_sk_ex_theta, theta_range_new_sk_ex_theta,
                           theta_smart_sk_ex_theta , theta_random_sk_ex_theta
)

theta_sk_ex_theta$distribution <- "sk_ex_theta"

# calcolo bais theta con stimoli fissi ----
theta_sk_ex_theta$bias_obs <- with(theta_sk_ex_theta, 
                                   theta_est - obs)
theta_sk_ex_theta$bias_obs_abs <- with(theta_sk_ex_theta, 
                                       abs(theta_est - obs))

theta_sk_ex_theta$bias_all <- with(theta_sk_ex_theta, 
                                   (theta_est - all))

theta_sk_ex_theta$bias_all_abs <- with(theta_sk_ex_theta, 
                                       abs(theta_est - all))

obs_bias_sk_ex_theta <- aggregate(bias_obs ~ num_item + selection, 
                                  data = theta_sk_ex_theta, 
                                  mean)
obs_bias_sk_ex_theta$type <- "bias"
obs_bias_sk_ex_theta_abs <- aggregate(bias_obs_abs ~ num_item + selection, 
                                      data = theta_sk_ex_theta, 
                                      mean)
obs_bias_sk_ex_theta_abs$type <- "bias_abs"
colnames(obs_bias_sk_ex_theta_abs)[3] <- "bias_obs"

obs_bias_sk_ex_theta <- rbind(obs_bias_sk_ex_theta, obs_bias_sk_ex_theta_abs)

obs_bias_sk_ex_theta$temp <- as.integer(gsub("number", "", 
                                             obs_bias_sk_ex_theta$num_item))
obs_bias_sk_ex_theta = obs_bias_sk_ex_theta[order(obs_bias_sk_ex_theta$temp), ]
ggplot(obs_bias_sk_ex_theta, 
       aes(x=as.factor(temp), y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_grid(~type)


all_bias_sk_ex_theta <- aggregate(bias_all ~ num_item + selection, 
                                  data = theta_sk_ex_theta, 
                                  mean)
all_bias_sk_ex_theta$type <- "bias"
all_bias_sk_ex_theta_abs <- aggregate(bias_all_abs ~ num_item + selection, 
                                      data = theta_sk_ex_theta, 
                                      mean)
all_bias_sk_ex_theta_abs$type <- "bias_abs"
colnames(all_bias_sk_ex_theta_abs)[3] <- "bias_all"

all_bias_sk_ex_theta <- rbind(all_bias_sk_ex_theta, all_bias_sk_ex_theta_abs)

all_bias_sk_ex_theta$temp <- as.integer(gsub("number", "", 
                                             all_bias_sk_ex_theta$num_item))
all_bias_sk_ex_theta = all_bias_sk_ex_theta[order(all_bias_sk_ex_theta$temp), ]
ggplot(all_bias_sk_ex_theta, 
       aes(x=as.factor(temp), y = bias_all, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_grid(~type)

# calcolo rmsea -----
theta_sk_ex_theta$bias_obs_sq <- theta_sk_ex_theta$bias_obs^2
theta_sk_ex_theta$bias_all_sq <- theta_sk_ex_theta$bias_all^2


obs_bias_sk_ex_sq_theta <- aggregate(bias_obs_sq ~ num_item + selection, 
                                     data = theta_sk_ex_theta, 
                                     mean)
obs_bias_sk_ex_sq_theta$type <- "bias_sq_obs"
obs_bias_sk_ex_sq_theta$rmsea = sqrt(obs_bias_sk_ex_sq_theta$bias_obs_sq)

ggplot(obs_bias_sk_ex_sq_theta, 
       aes(x=num_item, y=rmsea, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)



all_bias_sk_ex_sq_theta <- aggregate(bias_all_sq ~ num_item + selection, 
                                     data = theta_sk_ex_theta, 
                                     mean)
all_bias_sk_ex_sq_theta$type <- "bias_sq_all"
all_bias_sk_ex_sq_theta$rmsea = sqrt(all_bias_sk_ex_sq_theta$bias_all)

ggplot(all_bias_sk_ex_sq_theta, 
       aes(x=num_item, y=rmsea, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3)


# con gruppi latenti sk_ex theta  -----
theta_lat_theta_sk_ex <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat_theta_sk_ex, length(theta_lat_theta_sk_ex), include.lowest = TRUE)
cut_val = cut_borders(g)



group_name <- letters[1:(nrow(cut_val)+2)]


theta_sk_ex_theta$group <- ifelse(theta_sk_ex_theta$obs  <= cut_val[1, "start"], 
                                  group_name[1], 
                                  ifelse(theta_sk_ex_theta$obs > cut_val[1, "start"] & theta_sk_ex_theta$obs <= cut_val[1, "end"], 
                                         group_name[2], 
                                         ifelse(theta_sk_ex_theta$obs > cut_val[2, "start"] & theta_sk_ex_theta$obs <= cut_val[2, "end"], 
                                                group_name[3], 
                                                ifelse(theta_sk_ex_theta$obs > cut_val[3, "start"] & theta_sk_ex_theta$obs <= cut_val[3, "end"], 
                                                       group_name[4], 
                                                       ifelse(theta_sk_ex_theta$obs > cut_val[4, "start"] & theta_sk_ex_theta$obs <= cut_val[4, "end"], 
                                                              group_name[5], group_name[6])))))

bias_abs_sk_ex_group_theta <- aggregate(bias_obs_abs ~ selection + group + num_item, 
                                        data = theta_sk_ex_theta, mean)
bias_abs_sk_ex_group_theta$type = "bias"

ggplot(bias_abs_sk_ex_group_theta, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)

rmsea_sk_ex_group_theta <- aggregate(bias_obs_sq ~ selection + group + num_item, 
                                     data = theta_sk_ex_theta, mean)
rmsea_sk_ex_group_theta$rmsea = sqrt(rmsea_sk_ex_group_theta$bias_obs_sq)

ggplot(rmsea_sk_ex_group_theta, 
       aes(x=group, y = rmsea, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1.3) + 
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~num_item)


rm(list= ls()[!(ls() %in% c("m2pl_sk_ex",
                            'obs_bias_sk_ex_theta','all_bias_sk_ex_theta', 
                            "theta_sk_ex_theta", "all_data_sk_ex_theta", 
                            "data_info_sk_ex_theta",
                            'obs_bias_sk_ex','all_bias_sk_ex', 
                            "theta_sk_ex", "all_data_sk_ex", 
                            "data_info_sk_ex", 
                            "bias_abs_sk_ex_group", 
                            "bias_abs_sk_ex_group_theta", 
                            "obs_bias_sk_ex_sq", "obs_bias_sk_ex_sq_theta", 
                            "all_bias_sk_ex_sq", "all_bias_sk_ex_sq_theta", 
                            "rmsea_sk_ex_group", "rmsea_sk_ex_group_theta"))])

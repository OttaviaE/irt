library(psych)
library(ggplot2)
library(TAM)

rm(list = ls())
set.seed(666)

# min -max / num item ------
# questa funzione serve per definire gli intervalli
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}
# simulo i dati (che sono gli stessi dell'altro script)
# 
# 
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
# m2pl_sk_ex <- tam.mml(data)
# info_start <- IRT.informationCurves(m2pl_sk_ex) 
# # estraggo i theta 
# theta <- m2pl_sk_ex$person$EAP
# 
# # convieve usare m2pl_sk_ex$item_irt per i parametri degli item 
# # perché provved la IRT parametrization (default di ltm)
# item <- m2pl_sk_ex$item_irt

load("model_theta_itemSK_ex.RData")
hist(true_theta_sk_ex)
lab_item_sk_ex <- 1:ncol(data_sk_ex) # la i è il numero di item 
 num_item_sk_ex <- seq(10, 90, by = 10)  # la J che è il numero per cui dividere l'intervallo
#num_item_sk_ex <- c(2:9)

info_start_sk_ex <- mean(IRT.informationCurves(m2pl_sk_ex, 
                                            theta = seq(-3,3, length = 1000))$test_info_curve)
# divido gli intervalli del tratto latente
ranges_sk_ex <- NULL
groups_sk_ex <- NULL

cut_value_sk_ex <- list()

for (i in 1:(length(num_item_sk_ex))) {
  ranges_sk_ex <- seq(min(true_theta_sk_ex), max(true_theta_sk_ex), 
                length =num_item_sk_ex[i])
  groups_sk_ex <- cut(ranges_sk_ex, num_item_sk_ex[i], include.lowest = TRUE)
  cut_value_sk_ex[[i]] <- cut_borders(groups_sk_ex)
  cut_value_sk_ex[[i]]$mean_theta <- rowMeans(cut_value_sk_ex[[i]])
}
# calcola l'informatività per ogni theta per ogni item
info_test_sk_ex <- NULL
temp <- list()
value_sk_ex <- list()
temp_data_sk_ex <- NULL
info_data_sk_ex <- NULL

for (j in 1:length(cut_value_sk_ex)) {
  value_sk_ex[[j]] <- cut_value_sk_ex[[j]][1:nrow(cut_value_sk_ex[[j]]), ] 
  
  for(i in 1:length(lab_item_sk_ex)) {
    for(m in 1:nrow(value_sk_ex[[j]])) {
      
      temp_data_sk_ex <- data.frame(theta_target = IRT.informationCurves(m2pl_sk_ex, 
                                                                   theta = value_sk_ex[[j]][m,
                                                                                      "mean_theta"], 
                                                                   iIndex = lab_item_sk_ex[i])$theta,
                              test_info = mean(IRT.informationCurves(m2pl_sk_ex, 
                                                                theta = value_sk_ex[[j]][m,
                                                                                   "mean_theta"], 
                                                                iIndex = lab_item_sk_ex[i])$test_info_curve), 
                              item_info = mean(colSums(IRT.informationCurves(m2pl_sk_ex, 
                                                                theta = value_sk_ex[[j]][m,
                                                                                   "mean_theta"], 
                                                                iIndex = lab_item_sk_ex[i])$info_curves_item)),
                              item = lab_item_sk_ex[i],
                              num_item_sk_ex = paste("number", nrow(value_sk_ex[[j]]), sep = ""))
      
      info_data_sk_ex <- rbind(info_data_sk_ex, temp_data_sk_ex)
    }
  }
}
  
# per ogni range, selezione l'item con informatività massima
  # seleziono i data set per ognuno dei vari range che ho selezionato


temp_data_sk_ex <- NULL
temp_maxrange_sk_ex <- NULL
temp <- NULL
max_temp_sk_ex <- NULL

for (i in 1:length(unique(info_data_sk_ex$num_item_sk_ex))){
  temp_data_sk_ex <- info_data_sk_ex[info_data_sk_ex$num_item_sk_ex %in% unique(info_data_sk_ex$num_item_sk_ex)[i], ]
  temp_maxrange_sk_ex <- aggregate(test_info ~ item + theta_target, 
                             data = temp_data_sk_ex, max)
  temp_maxrange_sk_ex$range_name <- unique(temp_data_sk_ex$num_item_sk_ex)
  
  for (j in 1:length(unique(temp_maxrange_sk_ex$theta_target))) {
    temp <- temp_maxrange_sk_ex[which(temp_maxrange_sk_ex$test_info == max(temp_maxrange_sk_ex$test_info)), ]
    temp_maxrange_sk_ex <- temp_maxrange_sk_ex[which(temp_maxrange_sk_ex$item != temp$item & 
                                           temp_maxrange_sk_ex$theta_target != temp$theta_target), ]
    max_temp_sk_ex <-rbind(max_temp_sk_ex, temp)
    
  }
}


  out_range_sk_ex <- list()
  model_out_range_sk_ex <- list()
  info_out_range_sk_ex <- list()
  
  for (i in 1:length(unique(max_temp_sk_ex$range_name))) {
    out_range_sk_ex[[i]] <- data_sk_ex[, c(max_temp_sk_ex[max_temp_sk_ex$range_name %in%unique(max_temp_sk_ex$range_name)[i], 
                                        "item"])]
    model_out_range_sk_ex[[i]] <- tam.mml.2pl(out_range_sk_ex[[i]])
    info_out_range_sk_ex[[i]] <- IRT.informationCurves(model_out_range_sk_ex[[i]], 
                                                 theta = seq(-3, 3, length = 1000))
    names(info_out_range_sk_ex)[[i]] <- unique(max_temp_sk_ex$range_name)[i]
  }
  
  out_range_sk_ex_theta <- list()
  model_out_range_sk_ex_theta <- list()
  info_out_range_sk_ex_theta <- list()
  
  for (i in 1:length(unique(max_temp_sk_ex$range_name))) {
    out_range_sk_ex_theta[[i]] <- data_sk_ex[, c(max_temp_sk_ex[max_temp_sk_ex$range_name %in% unique(max_temp_sk_ex$range_name)[i], 
                                                           "item"])]
    model_out_range_sk_ex_theta[[i]] <- tam.mml(out_range_sk_ex_theta[[i]], 
                                                   xsi.fixed = cbind(1:ncol(out_range_sk_ex_theta[[i]]), 
                                                                     diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                                  colnames(out_range_sk_ex_theta[[i]]))), 2]), 
                                             B= array(c(rep(0, ncol(out_range_sk_ex_theta[[i]])), 
                                                        discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                        colnames(out_range_sk_ex_theta[[i]])))]), 
                                                      c(ncol(out_range_sk_ex_theta[[i]]),2,1), 
                                                      dimnames = list(colnames(out_range_sk_ex_theta[[i]]), 
                                                                      c("Cat0", "Cat1"), 
                                                                      "Dim01")))
    info_out_range_sk_ex_theta[[i]] <- IRT.informationCurves(model_out_range_sk_ex_theta[[i]], 
                                                            theta = seq(-3, 3, length = 1000))
    names(info_out_range_sk_ex_theta)[[i]] <- unique(max_temp_sk_ex$range_name)[i]
  }
  
  
  
  
  # range ----
  info_summary_range_sk_ex <- NULL
  temp <- NULL
  for(i in 1:length(info_out_range_sk_ex)) {
    temp <- data.frame(info_test_sk_ex = mean(info_out_range_sk_ex[[i]]$test_info_curve), 
                       
                       
                       range_name = names(info_out_range_sk_ex)[[i]], 
                       item = paste(colnames(out_range_sk_ex[[i]]), collapse = ","))
    
    info_summary_range_sk_ex <- rbind(info_summary_range_sk_ex, 
                                temp)
  }
  
  info_summary_range_sk_ex$rel <- 1 - (1/sqrt(info_summary_range_sk_ex$info_test_sk_ex))^2
  info_summary_range_sk_ex <-  rbind(info_summary_range_sk_ex, 
                               data.frame(info_test_sk_ex = sum(info_start_sk_ex),
                                          range_name = "all", 
                                          item = "all", 
                                          rel = 1 - (1/sqrt(info_start_sk_ex))^2))
  info_summary_range_sk_ex$selection <- "guidedsk_ex"

ggplot(info_summary_range_sk_ex, 
       aes(x=as.factor(range_name), y = info_test_sk_ex)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(info_summary_range_sk_ex, 
       aes(x=as.factor(range_name), y = rel)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

info_summary_range_sk_ex$items <- 1:nrow(info_summary_range_sk_ex)


# range con theta e parametri fissi ----
info_summary_range_sk_ex_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_range_sk_ex)) {
  temp <- data.frame(info_test = mean(info_out_range_sk_ex_theta[[i]]$test_info_curve), 
                     
                     
                     range_name = names(info_out_range_sk_ex_theta)[[i]], 
                     item = paste(colnames(out_range_sk_ex_theta[[i]]), collapse = ","))
  
  info_summary_range_sk_ex_theta <- rbind(info_summary_range_sk_ex_theta, 
                                       temp)
}

info_summary_range_sk_ex_theta$rel <- 1 - (1/sqrt(info_summary_range_sk_ex_theta$info_test))^2

info_summary_range_sk_ex_theta <-  rbind(info_summary_range_sk_ex_theta, 
                                      data.frame(info_test = info_start_sk_ex,
                                                 range_name = "all", 
                                                 item = "all", 
                                                 rel = 1 - (1/sqrt(info_start_sk_ex))^2))
info_summary_range_sk_ex_theta$selection <- "rangesk_ex"


ggplot(info_summary_range_sk_ex_theta, 
       aes(x = range_name, y = info_test)) + geom_point()

ggplot(info_summary_range_sk_ex_theta, 
       aes(x = range_name, y = rel)) + geom_point()

ggplot(info_summary_range_sk_ex, 
       aes(x = range_name, y = info_test_sk_ex)) + geom_point()

ggplot(info_summary_range_sk_ex_theta, 
       aes(x = range_name, y = rel)) + geom_point()

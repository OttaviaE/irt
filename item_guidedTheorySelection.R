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

# data_all <- sim.irt(nvar = 10,
#                     n = 1000,
#                     low = -3,
#                     high = 3,
#                     a = c(runif(10, 0.4, 1)))
# 
# data <- data.frame(data_all$items)
# # 
# # 
# # 
# # 
# # # va fittato un modello per ottenere i theta
#  m2pl <- tam.mml.2pl(data)
# info_start <- IRT.informationCurves(m2pl, 
#                                     theta = seq(-3,3, length = 100)) 
# # # estraggo i theta 
#  theta <- m2pl$person$EAP
# # 
# # # convieve usare m2pl$item_irt per i parametri degli item 
# # # perché provved la IRT parametrization (default di ltm)
#  item <- m2pl$item_irt

load("model_theta_item.RData")

lab_item <- 1:ncol(data) # la i è il numero di item 
num_item <- seq(5, 95, by = 5)  # la J che è il numero per cui dividere l'intervallo
#num_item <- c(2:9)


# divido gli intervalli del tratto latente
ranges_theory <- NULL
groups_theory <- NULL

cut_value_theory <- list()

for (i in 1:(length(num_item))) {
  ranges_theory <- seq(-3, 3, 
                length =num_item[i])
  groups_theory <- cut(ranges_theory, num_item[i], include.lowest = TRUE)
  cut_value_theory[[i]] <- cut_borders(groups_theory)
  cut_value_theory[[i]]$mean_theta <- rowMeans(cut_value_theory[[i]])
}
# calcola l'informatività per ogni theta per ogni item
info_test_theory <- NULL
temp_theory <- list()
value_theory  <- list()
temp_data_theory <- NULL
info_data_theory <- NULL

for (j in 1:length(cut_value_theory)) {
  value_theory[[j]] <- cut_value_theory[[j]][1:nrow(cut_value_theory[[j]]), ] 
  
  for(i in 1:length(lab_item)) {
    for(m in 1:nrow(value_theory[[j]])) {
      
      temp_data_theory <- data.frame(theta_target = IRT.informationCurves(m2pl, 
                                                                   theta = value_theory[[j]][m,
                                                                                      "mean_theta"], 
                                                                   iIndex = lab_item[i])$theta,
                  
                              test_info = IRT.informationCurves(m2pl, 
                                                                theta = value_theory[[j]][m,
                                                                                   "mean_theta"], 
                                                                iIndex = lab_item[i])$test_info_curve, 
                              item_info = IRT.informationCurves(m2pl, 
                                                                theta = value_theory[[j]][m,
                                                                                   "mean_theta"], 
                                                                iIndex = lab_item[i])$info_curves_item,
                              item = lab_item[i],
                              num_item = paste("number", nrow(value_theory[[j]]), sep = ""))
      
      info_data_theory <- rbind(info_data_theory, temp_data_theory)
    }
  }
}

# per ogni range, selezione l'item con informatività massima
# seleziono i data set per ognuno dei vari range che ho selezionato


temp_data_theory <- NULL
temp_maxtarget_theory <- NULL
temp_theory <- NULL
max_temp_theory <- NULL

for (i in 1:length(unique(info_data_theory$num_item))){
  temp_data_theory <- info_data_theory[info_data_theory$num_item %in% unique(info_data_theory$num_item)[i], ]
  temp_maxtarget_theory <- aggregate(test_info ~ item + theta_target, 
                             data = temp_data_theory, max)
  temp_maxtarget_theory$range_name <- unique(temp_data_theory$num_item)
  
  for (j in 1:length(unique(temp_maxtarget_theory$theta_target))) {
    temp_theory <- temp_maxtarget_theory[which(temp_maxtarget_theory$test_info == max(temp_maxtarget_theory$test_info)), ]
    temp_maxtarget_theory <- temp_maxtarget_theory[which(temp_maxtarget_theory$item != temp_theory$item & 
                                           temp_maxtarget_theory$theta_target != temp_theory$theta_target), ]
    max_temp_theory <-rbind(max_temp_theory, temp_theory)
    
  }
}


out_target_theory <- list()
model_out_target_theory <- list()
info_out_target_theory <- list()

for (i in 1:length(unique(max_temp_theory$range_name))) {
  out_target_theory[[i]] <- data[, c(max_temp_theory[max_temp_theory$range_name %in%unique(max_temp_theory$range_name)[i], 
                                      "item"])]
  model_out_target_theory[[i]] <- tam.mml.2pl(out_target_theory[[i]])
  info_out_target_theory[[i]] <- IRT.informationCurves(model_out_target_theory[[i]], 
                                               theta = seq(-3, 3, length = 100))
  names(info_out_target_theory)[[i]] <- unique(max_temp_theory$range_name)[i]
}




# range ----
info_summary_target_theory <- NULL
temp_theory <- NULL
for(i in 1:length(info_out_target_theory)) {
  temp_theory <- data.frame(info_test_theory = sum(info_out_target_theory[[i]]$info_curves_item), 
                     
                     
                     range_name = names(info_out_target_theory)[[i]], 
                     item = paste(colnames(out_target_theory[[i]]), collapse = ","))
  
  info_summary_target_theory <- rbind(info_summary_target_theory, 
                              temp_theory)
}


info_summary_target_theory <-  rbind(info_summary_target_theory, 
                             data.frame(info_test_theory = sum(info_start$info_curves_item),
                                        range_name = "all", 
                                        item = "all" ))
info_summary_target_theory$selection <- "guided"

ggplot(info_summary_target_theory, 
       aes(x=as.factor(range_name), y = info_test_theory)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


info_summary_target_theory$items <- 1:nrow(info_summary_target_theory)

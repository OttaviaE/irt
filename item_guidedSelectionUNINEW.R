library(psych)
library(ggplot2)
library(TAM)


rm(list = ls())
set.seed(999)

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
# m2pl_uni <- tam.mml(data)
# info_start <- IRT.informationCurves(m2pl_uni) 
# # estraggo i theta 
# theta <- m2pl_uni$person$EAP
# 
# # convieve usare m2pl_uni$item_irt per i parametri degli item 
# # perché provved la IRT parametrization (default di ltm)
# item <- m2pl_uni$item_irt

load("model_theta_itemUNI.RData")

lab_item_uni <- 1:ncol(data_uni) # la i è il numero di item 
num_item_uni <- seq(10, 90, by = 20)  # la J che è il numero per cui dividere l'intervallo
#num_item_uni <- c(2:9)

info_start_uni <- mean(IRT.informationCurves(m2pl_uni, 
                                             theta = seq(-3,3, length = 1000))$test_info_curve)



# divido gli intervalli del tratto latente

cut_value_uni = list()
for (i in 1:(length(num_item_uni))) {
  cut_value_uni[[i]] <- seq(min(true_theta_uni), max(true_theta_uni),
                           length = num_item_uni[i])
}


info_test_uni <- NULL
temp <- list()
value_uni <- list()
temp_data_uni <- NULL
info_data_uni <- NULL

for (j in 1:length(cut_value_uni)) {
  value_uni[[j]] <- cut_value_uni[[j]]
  
  for(i in 1:length(lab_item_uni)) {
    for(m in 1:length(value_uni[[j]])) {
      
      temp_data_uni <- data.frame(theta_target = IRT.informationCurves(m2pl_uni, 
                                                                      theta = value_uni[[j]][m], 
                                                                      iIndex = lab_item_uni[i])$theta,
                                 test_info = mean(IRT.informationCurves(m2pl_uni, 
                                                                        theta = value_uni[[j]][m], 
                                                                        iIndex = lab_item_uni[i])$test_info_curve), 
                                 item_info = mean(colSums(IRT.informationCurves(m2pl_uni, 
                                                                                theta = value_uni[[j]][m], 
                                                                                iIndex = lab_item_uni[i])$info_curves_item)),
                                 item = lab_item_uni[i],
                                 num_item_uni = paste("number", length(value_uni[[j]]), sep = ""))
      
      info_data_uni <- rbind(info_data_uni, temp_data_uni)
    }
  }
}

# per ogni range_new, selezione l'item con informatività massima
# seleziono i data set per ognuno dei vari range_new che ho selezionato


temp_data_uni <- NULL
temp_maxrange_new_uni <- NULL
temp <- NULL
max_temp_new_uni <- NULL

for (i in 1:length(unique(info_data_uni$num_item_uni))){
  temp_data_uni <- info_data_uni[info_data_uni$num_item_uni %in% unique(info_data_uni$num_item_uni)[i], ]
  temp_maxrange_new_uni <- aggregate(test_info ~ item + theta_target, 
                                 data = temp_data_uni, max)
  temp_maxrange_new_uni$range_new_name <- unique(temp_data_uni$num_item_uni)
  
  for (j in 1:length(unique(temp_maxrange_new_uni$theta_target))) {
    temp <- temp_maxrange_new_uni[which(temp_maxrange_new_uni$test_info == max(temp_maxrange_new_uni$test_info)), ]
    temp_maxrange_new_uni <- temp_maxrange_new_uni[which(temp_maxrange_new_uni$item != temp$item & 
                                                   temp_maxrange_new_uni$theta_target != temp$theta_target), ]
    max_temp_new_uni <-rbind(max_temp_new_uni, temp)
    
  }
}


out_range_new_uni <- list()
model_out_range_new_uni <- list()
info_out_range_new_uni <- list()

for (i in 1:length(unique(max_temp_new_uni$range_new_name))) {
  out_range_new_uni[[i]] <- data_uni[, c(max_temp_new_uni[max_temp_new_uni$range_new_name %in%unique(max_temp_new_uni$range_new_name)[i], 
                                                  "item"])]
  model_out_range_new_uni[[i]] <- tam.mml.2pl(out_range_new_uni[[i]])
  info_out_range_new_uni[[i]] <- IRT.informationCurves(model_out_range_new_uni[[i]], 
                                                   theta = seq(-3, 3, length = 1000))
  names(info_out_range_new_uni)[[i]] <- unique(max_temp_new_uni$range_new_name)[i]
}




# range_new ----
info_summary_range_new_uni <- NULL
temp <- NULL
for(i in 1:length(info_out_range_new_uni)) {
  temp <- data.frame(info_test_uni = mean(info_out_range_new_uni[[i]]$test_info_curve), 
                     
                     
                     range_new_name = names(info_out_range_new_uni)[[i]], 
                     item = paste(colnames(out_range_new_uni[[i]]), collapse = ","))
  
  info_summary_range_new_uni <- rbind(info_summary_range_new_uni, 
                                  temp)
}

info_summary_range_new_uni$rel <- 1 - (1/sqrt(info_summary_range_new_uni$info_test_uni))^2
info_summary_range_new_uni <-  rbind(info_summary_range_new_uni, 
                                 data.frame(info_test_uni = sum(info_start_uni),
                                            range_new_name = "all", 
                                            item = "all", 
                                            rel = 1 - (1/sqrt(info_start_uni))^2))
info_summary_range_new_uni$selection <- "guidedUNI"

ggplot(info_summary_range_new_uni, 
       aes(x=as.factor(range_new_name), y = info_test_uni)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


info_summary_range_new_uni$items <- 1:nrow(info_summary_range_new_uni)

# parametri fissi -----
out_range_new_uni_theta <- list()
model_out_range_new_uni_theta <- list()
info_out_range_new_uni_theta <- list()

for (i in 1:length(unique(max_temp_new_uni$range_new_name))) {
  out_range_new_uni_theta[[i]] <- data_uni[, c(max_temp_new_uni[max_temp_new_uni$range_new_name %in% unique(max_temp_new_uni$range_new_name)[i], 
                                                        "item"])]
  model_out_range_new_uni_theta[[i]] <- tam.mml(out_range_new_uni_theta[[i]], 
                                            xsi.fixed = cbind(1:ncol(out_range_new_uni_theta[[i]]), 
                                                              diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                        colnames(out_range_new_uni_theta[[i]]))), 2]), 
                                            B= array(c(rep(0, ncol(out_range_new_uni_theta[[i]])), 
                                                       discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                       colnames(out_range_new_uni_theta[[i]])))]), 
                                                     c(ncol(out_range_new_uni_theta[[i]]),2,1), 
                                                     dimnames = list(colnames(out_range_new_uni_theta[[i]]), 
                                                                     c("Cat0", "Cat1"), 
                                                                     "Dim01")))
  info_out_range_new_uni_theta[[i]] <- IRT.informationCurves(model_out_range_new_uni_theta[[i]], 
                                                         theta = seq(-3, 3, length = 1000))
  names(info_out_range_new_uni_theta)[[i]] <- unique(max_temp_new_uni$range_new_name)[i]
}


# range_new con theta e parametri fissi ----
info_summary_range_new_uni_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_range_new_uni)) {
  temp <- data.frame(info_test = mean(info_out_range_new_uni_theta[[i]]$test_info_curve), 
                     
                     
                     range_new_name = names(info_out_range_new_uni_theta)[[i]], 
                     item = paste(colnames(out_range_new_uni_theta[[i]]), collapse = ","))
  
  info_summary_range_new_uni_theta <- rbind(info_summary_range_new_uni_theta, 
                                        temp)
}

info_summary_range_new_uni_theta$rel <- 1 - (1/sqrt(info_summary_range_new_uni_theta$info_test))^2

info_summary_range_new_uni_theta <-  rbind(info_summary_range_new_uni_theta, 
                                       data.frame(info_test = info_start_uni,
                                                  range_new_name = "all", 
                                                  item = "all", 
                                                  rel = 1 - (1/sqrt(info_start_uni))^2))
info_summary_range_new_uni_theta$selection <- "range_newuni"


ggplot(info_summary_range_new_uni_theta, 
       aes(x = range_new_name, y = info_test)) + geom_point()

ggplot(info_summary_range_new_uni_theta, 
       aes(x = range_new_name, y = rel)) + geom_point()

ggplot(info_summary_range_new_uni, 
       aes(x = range_new_name, y = info_test_uni)) + geom_point()

ggplot(info_summary_range_new_uni, 
       aes(x = range_new_name, y = rel)) + geom_point()



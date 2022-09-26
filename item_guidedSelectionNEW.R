library(psych)
library(ggplot2)
library(TAM)

rm(list = ls())

# min -max / num item ------
# questa funzione serve per definire gli intervalli
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}

load("model_theta_item.RData")
hist(true_theta)
info_start <- mean(IRT.informationCurves(m2pl, 
                                         theta = seq(-3,3, 
                                                     length = 1000))$test_info_curve)
lab_item <- 1:ncol(data) # la i è il numero di item 
num_item <- seq(10, 90, by = 20)  # la J che è il numero per cui dividere l'intervallo
# num_item <- c(2:9)


cut_value <- list()
# tengo solo la prima selezione di item 
for (i in 1:length(num_item)) {
  cut_value[[i]] =   seq(min(true_theta), max(true_theta),
                         length =num_item[i])
}


# calcolo l'informatività per ogni theta per ogni item usando i valori theta target iniziali

info_test <- NULL
temp <- list()
value <- list()
temp_data <- NULL
info_data <- NULL

for (j in 1:length(cut_value)) {
  value[[j]] <- cut_value[[j]]
  
  for(i in 1:length(lab_item)) {
    for(m in 1:length(value[[j]])) {
      
      temp_data <- data.frame(theta_target = IRT.informationCurves(m2pl, 
                                                                   theta = value[[j]][m], 
                                                                   iIndex = lab_item[i])$theta,
                              test_info = mean(IRT.informationCurves(m2pl, 
                                                                     theta = value[[j]][m], 
                                                                     iIndex = lab_item[i])$test_info_curve), 
                              item_info = mean(colSums(IRT.informationCurves(m2pl, 
                                                                             theta = value[[j]][m], 
                                                                             iIndex = lab_item[i])$info_curves_item)),
                              item = lab_item[i],
                              num_item = paste("number", length(value[[j]]), sep = ""))
      
      info_data <- rbind(info_data, temp_data)
    }
  }
}


# per ogni range, selezione l'item con informatività massima
# seleziono i data set per ognuno dei vari range che ho selezionato
temp_data <- NULL
temp_max_range_new <- NULL
temp <- NULL
max_temp_new <- NULL

for (i in 1:length(unique(info_data$num_item))){
  temp_data <- info_data[info_data$num_item %in% unique(info_data$num_item)[i], ]
  temp_max_range_new <- aggregate(test_info ~ item + theta_target, 
                             data = temp_data, max)
  temp_max_range_new$range_new_name <- unique(temp_data$num_item)
  
  for (j in 1:length(unique(temp_max_range_new$theta_target))) {
    temp <- temp_max_range_new[which(temp_max_range_new$test_info == max(temp_max_range_new$test_info)), ]
    temp_max_range_new <- temp_max_range_new[which(temp_max_range_new$item != temp$item & 
                                                     temp_max_range_new$theta_target != temp$theta_target), ]
    max_temp_new <-rbind(max_temp_new, temp)
    
  }
}


out_range_new <- list()
model_out_range_new <- list()
info_out_range_new <- list()

for (i in 1:length(unique(max_temp_new$range_new_name))) {
  out_range_new[[i]] <- data[, c(max_temp_new[max_temp_new$range_new_name %in%unique(max_temp_new$range_new_name)[i], 
                                      "item"])]
  model_out_range_new[[i]] <- tam.mml.2pl(out_range_new[[i]])
  info_out_range_new[[i]] <- IRT.informationCurves(model_out_range_new[[i]], 
                                               theta = seq(-3, 3, length = 1000))
  names(info_out_range_new)[[i]] <- unique(max_temp_new$range_new_name)[i]
}


# con i theta stinmati con i parametri degli item fissi ----
out_range_new_theta <- list()
model_out_range_new_theta <- list()
info_out_range_new_theta <- list()

for (i in 1:length(unique(max_temp_new$range_new_name))) {
  out_range_new_theta[[i]] <- data[, c(max_temp_new[max_temp_new$range_new_name %in%unique(max_temp_new$range_new_name)[i], 
                                            "item"])]
  model_out_range_new_theta[[i]] <- tam.mml(out_range_new_theta[[i]], 
                                        xsi.fixed = cbind(1:ncol(out_range_new_theta[[i]]), 
                                                          diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                    colnames(out_range_new_theta[[i]]))), 2]), 
                                        B= array(c(rep(0, ncol(out_range_new_theta[[i]])), 
                                                   discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                   colnames(out_range_new_theta[[i]])))]), 
                                                 c(ncol(out_range_new_theta[[i]]),2,1), 
                                                 dimnames = list(colnames(out_range_new_theta[[i]]), 
                                                                 c("Cat0", "Cat1"), 
                                                                 "Dim01")))
  info_out_range_new_theta[[i]] <- IRT.informationCurves(model_out_range_new_theta[[i]], 
                                                     theta = seq(-3, 3, length = 1000))
  names(info_out_range_new_theta)[[i]] <- unique(max_temp_new$range_new_name)[i]
}


# range_new ----
info_summary_range_new <- NULL
temp <- NULL
for(i in 1:length(info_out_range_new)) {
  temp <- data.frame(info_test = mean(info_out_range_new[[i]]$test_info_curve), 
                     
                     
                     range_new_name = names(info_out_range_new)[[i]], 
                     item = paste(colnames(out_range_new[[i]]), collapse = ","))
  
  info_summary_range_new <- rbind(info_summary_range_new, 
                              temp)
}

info_summary_range_new$rel <- 1 - (1/sqrt(info_summary_range_new$info_test))^2
info_summary_range_new <-  rbind(info_summary_range_new, 
                             data.frame(info_test = sum(info_start),
                                        range_new_name = "all", 
                                        item = "all", 
                                        rel = 1 - (1/sqrt(info_start))^2))
info_summary_range_new$selection <- "guided"

ggplot(info_summary_range_new, 
       aes(x=as.factor(range_new_name), y = info_test)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


info_summary_range_new$items <- 1:nrow(info_summary_range_new)


# range_new_theta ----
info_summary_range_new_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_range_new_theta)) {
  temp <- data.frame(info_test = mean(info_out_range_new_theta[[i]]$test_info_curve), 
                     
                     
                     range_new_theta_name = names(info_out_range_new_theta)[[i]], 
                     item = paste(colnames(out_range_new_theta[[i]]), collapse = ","))
  
  info_summary_range_new_theta <- rbind(info_summary_range_new_theta, 
                                    temp)
}

info_summary_range_new_theta$rel <- 1 - (1/sqrt(info_summary_range_new_theta$info_test))^2
info_summary_range_new_theta <-  rbind(info_summary_range_new_theta, 
                                   data.frame(info_test = sum(info_start),
                                              range_new_theta_name = "all", 
                                              item = "all", 
                                              rel = 1 - (1/sqrt(info_start))^2))
info_summary_range_new_theta$selection <- "guidedTheta"

ggplot(info_summary_range_new_theta, 
       aes(x=as.factor(range_new_theta_name), y = info_test)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(info_summary_range_new_theta, 
       aes(x=as.factor(range_new_theta_name), y = rel)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


info_summary_range_new_theta$items <- 1:nrow(info_summary_range_new_theta)



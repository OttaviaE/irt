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

load("model_theta_item.RData")
info_start <- mean(IRT.informationCurves(m2pl, 
                                            theta = seq(-3,3, length = 1000))$test_info_curve)

lab_item <- 1:ncol(data) # la i è il numero di item 
num_item <- seq(10, 90, by = 20)  # la J che è il numero per cui dividere l'intervallo

# in pratica, devo prendere gli x item più informativi per il tratto latente
# quindi devo calcolare per ogni item l'informatività attraverso il tratto latente

data_info_smart <- data.frame(items = 1:(ncol(data)), 
                              info = numeric((ncol(data))))

# è qui che metto il tratto latente
for (i in 1:nrow(data_info_smart)) {
  data_info_smart[i, "info"] <- mean(IRT.informationCurves(m2pl, 
                                                          theta = true_theta, 
                                                          iIndex = lab_item[i])$info_curves_item)
  
}


# ora scrivi il codice per la procedura iterativa dove dato un certo numero di 
# item, trova il massimo e mano a mano toglie quel'item 
filtro <- list()
data_temp <- list()
for (i in 1:length((num_item))) {
  filtro[[i]] <- data_info_smart[which(data_info_smart$info == max(data_info_smart$info)), ]
  for (j in 1:(num_item[i]-1)) {
    data_temp[[j]] <- data_info_smart[!data_info_smart$items %in% filtro[[i]]$items, ]
    filtro[[i]] <- rbind(filtro[[i]], 
                    data_temp[[j]][which(data_temp[[j]]$info == max(data_temp[[j]]$info)), ])
  }
  names(filtro)[[i]] <- paste("number", num_item[i], sep = "")
  #filtro[[i]]$items <- paste0("V", filtro[[i]]$items)
  
}

# tiro fuori gli item selezionati, rifitto il modello e calcolo l'info
out_smart <- list()
model_out_smart <- list()
info_out_smart <- list()

for (i in 1:length(filtro)) {
  out_smart[[i]] <- data[, filtro[[i]]$items]
  model_out_smart[[i]] <- tam.mml.2pl(out_smart[[i]])
  info_out_smart[[i]] <- IRT.informationCurves(model_out_smart[[i]], 
                                                 theta = seq(-3, 3, length = 1000))
  names(info_out_smart)[[i]] <- names(filtro)[[i]]
}

# smart ----
info_summary_smart <- NULL
temp <- NULL
for(i in 1:length(info_out_smart)) {
  temp <- data.frame(info_test = mean(info_out_smart[[i]]$test_info_curve), 
                     
                     
                     smart_name = names(info_out_smart)[[i]], 
                     item = paste(colnames(out_smart[[i]]), collapse = ","))
  
  info_summary_smart <- rbind(info_summary_smart, 
                                       temp)
}

info_summary_smart$rel <- 1 - (1/sqrt(info_summary_smart$info_test))^2

info_summary_smart <-  rbind(info_summary_smart, 
                                      data.frame(info_test = (info_start),
                                                 smart_name = "all", 
                                                 item = "all", 
                                                 rel = 1 - (1/sqrt(info_start))^2))
info_summary_smart$selection <- "smart"


ggplot(info_summary_smart, 
       aes(x = smart_name, y = info_test)) + geom_point()

info_summary_smart <- info_summary_smart[order(info_summary_smart$smart_name), ]

plot(info_start, ylim = c(0,10))

for(i in 1:length(info_out_smart)) {
  par(new = TRUE)
  plot(info_out_smart[[i]], ylim = c(0,10))
  text(names(info_out_smart)[[i]], 
       x = -3, 
       y = mean(info_out_smart[[i]]$test_info_curve))
}

# theta stimati con i parametri fissi degli item -----

out_smart_theta <- list()
model_out_smart_theta <- list()
info_out_smart_theta <- list()

for (i in 1:length(filtro)) {
  out_smart_theta[[i]] <- data[, filtro[[i]]$items]
  model_out_smart_theta[[i]] <- tam.mml(out_smart_theta[[i]], 
                                            xsi.fixed = cbind(1:ncol(out_smart_theta[[i]]), 
                                                              diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                        colnames(out_smart_theta[[i]]))), 2]), 
                                        B= array(c(rep(0, ncol(out_smart_theta[[i]])), 
                                                   discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                   colnames(out_smart_theta[[i]])))]), 
                                                 c(ncol(out_smart_theta[[i]]),2,1), 
                                                 dimnames = list(colnames(out_smart_theta[[i]]), 
                                                                 c("Cat0", "Cat1"), 
                                                                 "Dim01")))
  
  info_out_smart_theta[[i]] <- IRT.informationCurves(model_out_smart_theta[[i]], 
                                                     theta = seq(-3, 3, length = 1000))
  names(info_out_smart_theta)[[i]] <- names(filtro)[[i]]
}

# smart_theta ----
info_summary_smart_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_smart_theta)) {
  temp <- data.frame(info_test = mean(info_out_smart_theta[[i]]$test_info_curve), 
                     
                     
                     smart_theta_name = names(info_out_smart_theta)[[i]], 
                     item = paste(colnames(out_smart_theta[[i]]), collapse = ","))
  
  info_summary_smart_theta <- rbind(info_summary_smart_theta, 
                                    temp)
}

info_summary_smart_theta$rel <- 1 - (1/sqrt(info_summary_smart_theta$info_test))^2

info_summary_smart_theta <-  rbind(info_summary_smart_theta, 
                                   data.frame(info_test = (info_start),
                                              smart_theta_name = "all", 
                                              item = "all", 
                                              rel = 1 - (1/sqrt(info_start))^2))
info_summary_smart_theta$selection <- "smart_theta"


ggplot(info_summary_smart_theta, 
       aes(x = smart_theta_name, y = info_test)) + geom_point()

info_summary_smart_theta <- info_summary_smart_theta[order(info_summary_smart_theta$smart_theta_name), ]

plot(info_start, ylim = c(0,10))

for(i in 1:length(info_out_smart_theta)) {
  par(new = TRUE)
  plot(info_out_smart_theta[[i]], ylim = c(0,10))
  text(names(info_out_smart_theta)[[i]], 
       x = -3, 
       y = mean(info_out_smart_theta[[i]]$test_info_curve))
}
ggplot(info_summary_smart, 
       aes(x = smart_name, y = info_test)) + geom_point()

ggplot(info_summary_smart, 
       aes(x = smart_name, y = rel)) + geom_point()

ggplot(info_summary_smart_theta, 
       aes(x = smart_theta_name, y = info_test)) + geom_point()

ggplot(info_summary_smart_theta, 
       aes(x = smart_theta_name, y = rel)) + geom_point()

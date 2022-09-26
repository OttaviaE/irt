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



load("model_theta_itemUNI.RData")
info_start_uni <- mean(IRT.informationCurves(m2pl_uni, 
                                             theta = seq(-3,3, length = 1000))$test_info_curve)

# data_all <- sim.irt(nvar = 10,
#                     n = 1000,
#                     low = -3,
#                     high = 3,
#                     a = c(runif(10, 0.4, 1)))
# # 
# data <- data.frame(data_all$items)
# # 
# # 
# # 
# # 
# # # va fittato un modello per ottenere i theta
#  m2pl <- tam.mml(data)
# # info_start <- IRT.informationCurves(m2pl)
# # # estraggo i theta
#  theta <- m2pl$person$EAP
# # 
# # # convieve usare m2pl$item_irt per i parametri degli item
# # # perché provved la IRT parametrization (default di ltm)
#  item <- m2pl$item_irt

lab_item_uni <- 1:ncol(data_uni) # la i è il numero di item 
num_item_uni <- seq(10, 90, by = 20)  # la J che è il numero per cui dividere l'intervallo
#num_item_uni <- c(2:9)

#info_start <- IRT.informationCurves(m2pl, 
#                                    theta = seq(-3, 3, length = 100))
#plot(info_start)

# in pratica, devo prendere gli x item più informativi per il tratto latente
# quindi devo calcolare per ogni item l'informatività attraverso il tratto latente

data_info_smart_uni <- data.frame(items = 1:ncol(data_uni), 
                                 info = numeric(ncol(data_uni)))

for (i in 1:nrow(data_info_smart_uni)) {
  data_info_smart_uni[i, "info"] <- mean(IRT.informationCurves(m2pl_uni, 
                                                              theta = true_theta_uni, 
                                                              iIndex = lab_item_uni[i])$info_curves_item)
  
}

# ora scrivi il codice per la procedura iterativa dove dato un certo numero di 
# item, trova il massimo e mano a mano toglie quel'item 
filtro_uni <- list()
data_temp_uni <- list()
for (i in 1:length((num_item_uni))) {
  filtro_uni[[i]] <- data_info_smart_uni[which(data_info_smart_uni$info == max(data_info_smart_uni$info)), ]
  for (j in 1:(num_item_uni[i]-1)) {
    data_temp_uni[[j]] <- data_info_smart_uni[!data_info_smart_uni$items %in% filtro_uni[[i]]$items, ]
    filtro_uni[[i]] <- rbind(filtro_uni[[i]], 
                            data_temp_uni[[j]][which(data_temp_uni[[j]]$info == max(data_temp_uni[[j]]$info)), ])
  }
  names(filtro_uni)[[i]] <- paste("number", num_item_uni[i], sep = "")
  #filtro_uni[[i]]$items <- paste0("V", filtro_uni[[i]]$items)
  
}

# tiro fuori gli item selezionati, rifitto il modello e calcolo l'info
out_smart_uni <- list()
model_out_smart_uni <- list()
info_out_smart_uni <- list()

for (i in 1:length(filtro_uni)) {
  out_smart_uni[[i]] <- data_uni[, filtro_uni[[i]]$items]
  model_out_smart_uni[[i]] <- tam.mml.2pl(out_smart_uni[[i]])
  info_out_smart_uni[[i]] <- IRT.informationCurves(model_out_smart_uni[[i]], 
                                                  theta = seq(-3, 3, length = 1000))
  names(info_out_smart_uni)[[i]] <- names(filtro_uni)[[i]]
}

# smart ----
info_summary_smart_uni <- NULL
temp <- NULL
for(i in 1:length(info_out_smart_uni)) {
  temp <- data.frame(info_test = mean(info_out_smart_uni[[i]]$test_info_curve), 
                     
                     
                     smart_name = names(info_out_smart_uni)[[i]], 
                     item = paste(colnames(out_smart_uni[[i]]), collapse = ","))
  
  info_summary_smart_uni <- rbind(info_summary_smart_uni, 
                                 temp)
}

info_summary_smart_uni$rel <- 1 - (1/sqrt(info_summary_smart_uni$info_test))^2

info_summary_smart_uni <-  rbind(info_summary_smart_uni, 
                                data.frame(info_test = (info_start_uni),
                                           smart_name = "all", 
                                           item = "all", 
                                           rel = 1 - (1/sqrt(info_start_uni))^2))
info_summary_smart_uni$selection <- "smartUNI"



ggplot(info_summary_smart_uni, 
       aes(x = smart_name, y = info_test)) + geom_point()

info_summary_smart_uni <- info_summary_smart_uni[order(info_summary_smart_uni$smart_name), ]


for(i in 1:length(info_out_smart_uni)) {
  par(new = TRUE)
  plot(info_out_smart_uni[[i]], ylim = c(0,10))
  text(names(info_out_smart_uni)[[i]], 
       x = -3, 
       y = mean(info_out_smart_uni[[i]]$test_info_curve))
}

# theta con paramteri item fissi ----


# tiro fuori gli item selezionati, rifitto il modello e calcolo l'info
out_smart_uni_theta <- list()
model_out_smart_uni_theta <- list()
info_out_smart_uni_theta <- list()

for (i in 1:length(filtro_uni)) {
  out_smart_uni_theta[[i]] <- data_uni[, filtro_uni[[i]]$items]
  model_out_smart_uni_theta[[i]] <- tam.mml(out_smart_uni[[i]], 
                                                xsi.fixed = cbind(1:ncol(out_smart_uni_theta[[i]]), 
                                                                  diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                                colnames(out_smart_uni_theta[[i]]))), 2]), 
                                            B= array(c(rep(0, ncol(out_smart_uni_theta[[i]])), 
                                                       discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                       colnames(out_smart_uni_theta[[i]])))]), 
                                                     c(ncol(out_smart_uni_theta[[i]]),2,1), 
                                                     dimnames = list(colnames(out_smart_uni_theta[[i]]), 
                                                                     c("Cat0", "Cat1"), 
                                                                     "Dim01")))
  info_out_smart_uni_theta[[i]] <- IRT.informationCurves(model_out_smart_uni_theta[[i]], 
                                                         theta = seq(-3, 3, length = 1000))
  names(info_out_smart_uni_theta)[[i]] <- names(filtro_uni)[[i]]
}




# smart ----
info_summary_smart_uni_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_smart_uni_theta)) {
  temp <- data.frame(info_test = mean(info_out_smart_uni_theta[[i]]$test_info_curve), 
                     
                     
                     smart_name = names(info_out_smart_uni_theta)[[i]], 
                     item = paste(colnames(out_smart_uni_theta[[i]]), collapse = ","))
  
  info_summary_smart_uni_theta <- rbind(info_summary_smart_uni_theta, 
                                        temp)
}

info_summary_smart_uni_theta$rel <- 1 - (1/sqrt(info_summary_smart_uni_theta$info_test))^2

info_summary_smart_uni_theta <-  rbind(info_summary_smart_uni_theta, 
                                       data.frame(info_test = (info_start_uni),
                                                  smart_name = "all", 
                                                  item = "all", 
                                                  rel = 1 - (1/sqrt(info_start_uni))^2))
info_summary_smart_uni_theta$selection <- "smartuni_theta"


ggplot(info_summary_smart_uni_theta, 
       aes(x = smart_name, y = info_test)) + geom_point()

ggplot(info_summary_smart_uni_theta, 
       aes(x = smart_name, y = rel)) + geom_point()

info_summary_smart_uni_theta <- info_summary_smart_uni_theta[order(info_summary_smart_uni_theta$smart_name), ]




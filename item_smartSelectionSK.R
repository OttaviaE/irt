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



load("model_theta_itemSK.RData")
info_start_sk <- mean(IRT.informationCurves(m2pl_sk, 
                                            theta = seq(-3,3, length = 1000))$test_info_curve)

hist(true_theta_sk)
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

lab_item_sk <- 1:ncol(data_sk) # la i è il numero di item 
num_item_sk <- seq(10, 90, by = 20)  # la J che è il numero per cui dividere l'intervallo
#num_item_sk <- c(2:9)

#info_start <- IRT.informationCurves(m2pl, 
#                                    theta = seq(-3, 3, length = 100))
#plot(info_start)

# in pratica, devo prendere gli x item più informativi per il tratto latente
# quindi devo calcolare per ogni item l'informatività attraverso il tratto latente

data_info_smart_sk <- data.frame(items = 1:ncol(data_sk), 
                              info = numeric(ncol(data_sk)))

for (i in 1:nrow(data_info_smart_sk)) {
  data_info_smart_sk[i, "info"] <- mean(IRT.informationCurves(m2pl_sk, 
                                                          theta = true_theta_sk, 
                                                          iIndex = lab_item_sk[i])$info_curves_item)
  
}

# ora scrivi il codice per la procedura iterativa dove dato un certo numero di 
# item, trova il massimo e mano a mano toglie quel'item 
filtro_sk <- list()
data_temp_sk <- list()
for (i in 1:length((num_item_sk))) {
  filtro_sk[[i]] <- data_info_smart_sk[which(data_info_smart_sk$info == max(data_info_smart_sk$info)), ]
  for (j in 1:(num_item_sk[i]-1)) {
    data_temp_sk[[j]] <- data_info_smart_sk[!data_info_smart_sk$items %in% filtro_sk[[i]]$items, ]
    filtro_sk[[i]] <- rbind(filtro_sk[[i]], 
                    data_temp_sk[[j]][which(data_temp_sk[[j]]$info == max(data_temp_sk[[j]]$info)), ])
  }
  names(filtro_sk)[[i]] <- paste("number", num_item_sk[i], sep = "")
 # filtro_sk[[i]]$items <- paste0("V", filtro_sk[[i]]$items)
  
}

# tiro fuori gli item selezionati, rifitto il modello e calcolo l'info
out_smart_sk <- list()
model_out_smart_sk <- list()
info_out_smart_sk <- list()

for (i in 1:length(filtro_sk)) {
  out_smart_sk[[i]] <- data_sk[, filtro_sk[[i]]$items]
  model_out_smart_sk[[i]] <- tam.mml.2pl(out_smart_sk[[i]])
  info_out_smart_sk[[i]] <- IRT.informationCurves(model_out_smart_sk[[i]], 
                                                 theta = seq(-3, 3, length = 1000))
  names(info_out_smart_sk)[[i]] <- names(filtro_sk)[[i]]
}




# smart ----
info_summary_smart_sk <- NULL
temp <- NULL
for(i in 1:length(info_out_smart_sk)) {
  temp <- data.frame(info_test = mean(info_out_smart_sk[[i]]$test_info_curve), 
                     
                     
                     smart_name = names(info_out_smart_sk)[[i]], 
                     item = paste(colnames(out_smart_sk[[i]]), collapse = ","))
  
  info_summary_smart_sk <- rbind(info_summary_smart_sk, 
                                       temp)
}

info_summary_smart_sk$rel <- 1 - (1/sqrt(info_summary_smart_sk$info_test))^2

info_summary_smart_sk <-  rbind(info_summary_smart_sk, 
                                      data.frame(info_test = (info_start_sk),
                                                 smart_name = "all", 
                                                 item = "all", 
                                                 rel = 1 - (1/sqrt(info_start_sk))^2))
info_summary_smart_sk$selection <- "smartsk"


ggplot(info_summary_smart_sk, 
       aes(x = smart_name, y = info_test)) + geom_point()

ggplot(info_summary_smart_sk, 
       aes(x = smart_name, y = rel)) + geom_point()

info_summary_smart_sk <- info_summary_smart_sk[order(info_summary_smart_sk$smart_name), ]

plot(info_start, ylim = c(0,10))

for(i in 1:length(info_out_smart_sk)) {
  par(new = TRUE)
  plot(info_out_smart_sk[[i]], ylim = c(0,10))
  text(names(info_out_smart_sk)[[i]], 
       x = -3, 
       y = mean(info_out_smart_sk[[i]]$test_info_curve))
}
# theta con paramteri item fissi ----


# tiro fuori gli item selezionati, rifitto il modello e calcolo l'info
out_smart_sk_theta <- list()
model_out_smart_sk_theta <- list()
info_out_smart_sk_theta <- list()

for (i in 1:length(filtro_sk)) {
  out_smart_sk_theta[[i]] <- data_sk[, filtro_sk[[i]]$items]
  model_out_smart_sk_theta[[i]] <- tam.mml(out_smart_sk[[i]], 
                                               xsi.fixed = cbind(1:ncol(out_smart_sk_theta[[i]]), 
                                                                 diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                              colnames(out_smart_sk_theta[[i]]))), 2]), 
                                           B= array(c(rep(0, ncol(out_smart_sk_theta[[i]])), 
                                                      discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                      colnames(out_smart_sk_theta[[i]])))]), 
                                                    c(ncol(out_smart_sk_theta[[i]]),2,1), 
                                                    dimnames = list(colnames(out_smart_sk_theta[[i]]), 
                                                                    c("Cat0", "Cat1"), 
                                                                    "Dim01")))
  info_out_smart_sk_theta[[i]] <- IRT.informationCurves(model_out_smart_sk_theta[[i]], 
                                                        theta = seq(-3, 3, length = 1000))
  names(info_out_smart_sk_theta)[[i]] <- names(filtro_sk)[[i]]
}




# smart ----
info_summary_smart_sk_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_smart_sk_theta)) {
  temp <- data.frame(info_test = mean(info_out_smart_sk_theta[[i]]$test_info_curve), 
                     
                     
                     smart_name = names(info_out_smart_sk_theta)[[i]], 
                     item = paste(colnames(out_smart_sk_theta[[i]]), collapse = ","))
  
  info_summary_smart_sk_theta <- rbind(info_summary_smart_sk_theta, 
                                       temp)
}

info_summary_smart_sk_theta$rel <- 1 - (1/sqrt(info_summary_smart_sk_theta$info_test))^2

info_summary_smart_sk_theta <-  rbind(info_summary_smart_sk_theta, 
                                      data.frame(info_test = (info_start_sk),
                                                 smart_name = "all", 
                                                 item = "all", 
                                                 rel = 1 - (1/sqrt(info_start_sk))^2))
info_summary_smart_sk_theta$selection <- "smartsk_theta"


ggplot(info_summary_smart_sk_theta, 
       aes(x = smart_name, y = info_test)) + geom_point()

ggplot(info_summary_smart_sk_theta, 
       aes(x = smart_name, y = rel)) + geom_point()

ggplot(info_summary_smart_sk, 
       aes(x = smart_name, y = info_test)) + geom_point()

ggplot(info_summary_smart_sk, 
       aes(x = smart_name, y = rel)) + geom_point()


info_summary_smart_sk_theta <- info_summary_smart_sk_theta[order(info_summary_smart_sk_theta$smart_name), ]



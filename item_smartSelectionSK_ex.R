library(psych)
library(ggplot2)
library(TAM)

rm(list = ls())
set.seed(666)

cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}
# per sicurezza, meghlio usare ltm::factor.scores()



load("model_theta_itemSK_ex.RData")
info_start_sk_ex <- mean(IRT.informationCurves(m2pl_sk_ex, 
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

lab_item_sk_ex <- 1:ncol(data_sk_ex) # la i è il numero di item 
num_item_sk_ex <- seq(10, 90, by = 10)  # la J che è il numero per cui dividere l'intervallo
#num_item_sk_ex <- c(2:9)

#info_start <- IRT.informationCurves(m2pl, 
#                                    theta = seq(-3, 3, length = 100))
#plot(info_start)

# in pratica, devo prendere gli x item più informativi per il tratto latente
# quindi devo calcolare per ogni item l'informatività attraverso il tratto latente

data_info_smart_sk_ex <- data.frame(items = 1:ncol(data_sk_ex), 
                              info = numeric(ncol(data_sk_ex)))

for (i in 1:nrow(data_info_smart_sk_ex)) {
  data_info_smart_sk_ex[i, "info"] <- mean(IRT.informationCurves(m2pl_sk_ex, 
                                                          theta = seq(-3, 3, length = 1000), 
                                                          iIndex = lab_item_sk_ex[i])$info_curves_item)
  
}

# ora scrivi il codice per la procedura iterativa dove dato un certo numero di 
# item, trova il massimo e mano a mano toglie quel'item 
filtro_sk_ex <- list()
data_temp_sk_ex <- list()
for (i in 1:length((num_item_sk_ex))) {
  filtro_sk_ex[[i]] <- data_info_smart_sk_ex[which(data_info_smart_sk_ex$info == max(data_info_smart_sk_ex$info)), ]
  for (j in 1:(num_item_sk_ex[i]-1)) {
    data_temp_sk_ex[[j]] <- data_info_smart_sk_ex[!data_info_smart_sk_ex$items %in% filtro_sk_ex[[i]]$items, ]
    filtro_sk_ex[[i]] <- rbind(filtro_sk_ex[[i]], 
                    data_temp_sk_ex[[j]][which(data_temp_sk_ex[[j]]$info == max(data_temp_sk_ex[[j]]$info)), ])
  }
  names(filtro_sk_ex)[[i]] <- paste("number", num_item_sk_ex[i], sep = "")
 # filtro_sk_ex[[i]]$items <- paste0("V", filtro_sk_ex[[i]]$items)
  
}

# tiro fuori gli item selezionati, rifitto il modello e calcolo l'info
out_smart_sk_ex <- list()
model_out_smart_sk_ex <- list()
info_out_smart_sk_ex <- list()

for (i in 1:length(filtro_sk_ex)) {
  out_smart_sk_ex[[i]] <- data_sk_ex[, filtro_sk_ex[[i]]$items]
  model_out_smart_sk_ex[[i]] <- tam.mml.2pl(out_smart_sk_ex[[i]])
  info_out_smart_sk_ex[[i]] <- IRT.informationCurves(model_out_smart_sk_ex[[i]], 
                                                 theta = seq(-3, 3, length = 1000))
  names(info_out_smart_sk_ex)[[i]] <- names(filtro_sk_ex)[[i]]
}




# smart ----
info_summary_smart_sk_ex <- NULL
temp <- NULL
for(i in 1:length(info_out_smart_sk_ex)) {
  temp <- data.frame(info_test = mean(info_out_smart_sk_ex[[i]]$test_info_curve), 
                     
                     
                     smart_name = names(info_out_smart_sk_ex)[[i]], 
                     item = paste(colnames(out_smart_sk_ex[[i]]), collapse = ","))
  
  info_summary_smart_sk_ex <- rbind(info_summary_smart_sk_ex, 
                                       temp)
}

info_summary_smart_sk_ex$rel <- 1 - (1/sqrt(info_summary_smart_sk_ex$info_test))^2

info_summary_smart_sk_ex <-  rbind(info_summary_smart_sk_ex, 
                                      data.frame(info_test = (info_start_sk_ex),
                                                 smart_name = "all", 
                                                 item = "all", 
                                                 rel = 1 - (1/sqrt(info_start_sk_ex))^2))
info_summary_smart_sk_ex$selection <- "smartsk_ex"


ggplot(info_summary_smart_sk_ex, 
       aes(x = smart_name, y = info_test)) + geom_point()

ggplot(info_summary_smart_sk_ex, 
       aes(x = smart_name, y = rel)) + geom_point()

info_summary_smart_sk_ex <- info_summary_smart_sk_ex[order(info_summary_smart_sk_ex$smart_name), ]

plot(info_start, ylim = c(0,10))

for(i in 1:length(info_out_smart_sk_ex)) {
  par(new = TRUE)
  plot(info_out_smart_sk_ex[[i]], ylim = c(0,10))
  text(names(info_out_smart_sk_ex)[[i]], 
       x = -3, 
       y = mean(info_out_smart_sk_ex[[i]]$test_info_curve))
}
# theta con paramteri item fissi ----


# tiro fuori gli item selezionati, rifitto il modello e calcolo l'info
out_smart_sk_ex_theta <- list()
model_out_smart_sk_ex_theta <- list()
info_out_smart_sk_ex_theta <- list()

for (i in 1:length(filtro_sk_ex)) {
  out_smart_sk_ex_theta[[i]] <- data_sk_ex[, filtro_sk_ex[[i]]$items]
  model_out_smart_sk_ex_theta[[i]] <- tam.mml(out_smart_sk_ex[[i]], 
                                               xsi.fixed = cbind(1:ncol(out_smart_sk_ex_theta[[i]]), 
                                                                 diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                              colnames(out_smart_sk_ex_theta[[i]]))), 2]), 
                                           B= array(c(rep(0, ncol(out_smart_sk_ex_theta[[i]])), 
                                                      discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                      colnames(out_smart_sk_ex_theta[[i]])))]), 
                                                    c(ncol(out_smart_sk_ex_theta[[i]]),2,1), 
                                                    dimnames = list(colnames(out_smart_sk_ex_theta[[i]]), 
                                                                    c("Cat0", "Cat1"), 
                                                                    "Dim01")))
  info_out_smart_sk_ex_theta[[i]] <- IRT.informationCurves(model_out_smart_sk_ex_theta[[i]], 
                                                        theta = seq(-3, 3, length = 1000))
  names(info_out_smart_sk_ex_theta)[[i]] <- names(filtro_sk_ex)[[i]]
}




# smart ----
info_summary_smart_sk_ex_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_smart_sk_ex_theta)) {
  temp <- data.frame(info_test = mean(info_out_smart_sk_ex_theta[[i]]$test_info_curve), 
                     
                     
                     smart_name = names(info_out_smart_sk_ex_theta)[[i]], 
                     item = paste(colnames(out_smart_sk_ex_theta[[i]]), collapse = ","))
  
  info_summary_smart_sk_ex_theta <- rbind(info_summary_smart_sk_ex_theta, 
                                       temp)
}

info_summary_smart_sk_ex_theta$rel <- 1 - (1/sqrt(info_summary_smart_sk_ex_theta$info_test))^2

info_summary_smart_sk_ex_theta <-  rbind(info_summary_smart_sk_ex_theta, 
                                      data.frame(info_test = (info_start_sk_ex),
                                                 smart_name = "all", 
                                                 item = "all", 
                                                 rel = 1 - (1/sqrt(info_start_sk_ex))^2))
info_summary_smart_sk_ex_theta$selection <- "smartsk_ex_theta"


ggplot(info_summary_smart_sk_ex_theta, 
       aes(x = smart_name, y = info_test)) + geom_point()

ggplot(info_summary_smart_sk_ex_theta, 
       aes(x = smart_name, y = rel)) + geom_point()

ggplot(info_summary_smart_sk_ex, 
       aes(x = smart_name, y = info_test)) + geom_point()

ggplot(info_summary_smart_sk_ex, 
       aes(x = smart_name, y = rel)) + geom_point()


info_summary_smart_sk_ex_theta <- info_summary_smart_sk_ex_theta[order(info_summary_smart_sk_ex_theta$smart_name), ]



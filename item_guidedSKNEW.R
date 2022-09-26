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
# m2pl_sk <- tam.mml(data)
# info_start <- IRT.informationCurves(m2pl_sk) 
# # estraggo i theta 
# theta <- m2pl_sk$person$EAP
# 
# # convieve usare m2pl_sk$item_irt per i parametri degli item 
# # perché provved la IRT parametrization (default di ltm)
# item <- m2pl_sk$item_irt

load("model_theta_itemSK.RData")
hist(true_theta_sk)
lab_item_sk_new <- 1:ncol(data_sk) # la i è il numero di item 
num_item_sk_new <- seq(10, 90, by = 20)  # la J che è il numero per cui dividere l'intervallo
#num_item_sk_new <- c(2:9)

info_start_sk_new <- mean(IRT.informationCurves(m2pl_sk, 
                                            theta = seq(-3,3, length = 1000))$test_info_curve)


cut_value_sk_new = list()
for (i in 1:(length(num_item_sk_new))) {
  cut_value_sk_new[[i]] <- seq(min(true_theta_sk), max(true_theta_sk),
                           length = num_item_sk_new[i])
}

# calcola l'informatività per ogni theta per ogni item
info_test_sk_new <- NULL
temp <- list()
value_sk_new <- list()
temp_data_sk_new <- NULL
info_data_sk_new <- NULL

for (j in 1:length(cut_value_sk_new)) {
  value_sk_new[[j]] <- cut_value_sk_new[[j]]
  
  for(i in 1:length(lab_item_sk_new)) {
    for(m in 1:length(value_sk_new[[j]])) {
      
      temp_data_sk_new <- data.frame(theta_target = IRT.informationCurves(m2pl_sk, 
                                                                      theta = value_sk_new[[j]][m], 
                                                                      iIndex = lab_item_sk_new[i])$theta,
                                 test_info = mean(IRT.informationCurves(m2pl_sk, 
                                                                        theta = value_sk_new[[j]][m], 
                                                                        iIndex = lab_item_sk_new[i])$test_info_curve), 
                                 item_info = mean(colSums(IRT.informationCurves(m2pl_sk, 
                                                                                theta = value_sk_new[[j]][m], 
                                                                                iIndex = lab_item_sk_new[i])$info_curves_item)),
                                 item = lab_item_sk_new[i],
                                 num_item_sk_new = paste("number", length(value_sk_new[[j]]), sep = ""))
      
      info_data_sk_new <- rbind(info_data_sk_new, temp_data_sk_new)
    }
  }
}

# per ogni range, selezione l'item con informatività massima
# seleziono i data set per ognuno dei vari range che ho selezionato


temp_data_sk_new <- NULL
temp_max_range_new_sk <- NULL
temp <- NULL
max_temp_sk_new <- NULL

for (i in 1:length(unique(info_data_sk_new$num_item_sk_new))){
  temp_data_sk_new <- info_data_sk_new[info_data_sk_new$num_item_sk_new %in% unique(info_data_sk_new$num_item_sk_new)[i], ]
  temp_max_range_new_sk <- aggregate(test_info ~ item + theta_target, 
                                data = temp_data_sk_new, max)
  temp_max_range_new_sk$range_new_name <- unique(temp_data_sk_new$num_item_sk_new)
  
  for (j in 1:length(unique(temp_max_range_new_sk$theta_target))) {
    temp <- temp_max_range_new_sk[which(temp_max_range_new_sk$test_info == max(temp_max_range_new_sk$test_info)), ]
    temp_max_range_new_sk <- temp_max_range_new_sk[which(temp_max_range_new_sk$item != temp$item & 
                                                 temp_max_range_new_sk$theta_target != temp$theta_target), ]
    max_temp_sk_new <-rbind(max_temp_sk_new, temp)
    
  }
}


out_range_new_sk <- list()
model_out_range_new_sk <- list()
info_out_range_new_sk <- list()

for (i in 1:length(unique(max_temp_sk_new$range_new_name))) {
  out_range_new_sk[[i]] <- data_sk[, c(max_temp_sk_new[max_temp_sk_new$range_new_name %in%unique(max_temp_sk_new$range_new_name)[i], 
                                               "item"])]
  model_out_range_new_sk[[i]] <- tam.mml.2pl(out_range_new_sk[[i]])
  info_out_range_new_sk[[i]] <- IRT.informationCurves(model_out_range_new_sk[[i]], 
                                                  theta = seq(-3, 3, length = 1000))
  names(info_out_range_new_sk)[[i]] <- unique(max_temp_sk_new$range_new_name)[i]
}

out_range_new_sk_theta <- list()
model_out_range_new_sk_theta <- list()
info_out_range_new_sk_theta <- list()

for (i in 1:length(unique(max_temp_sk_new$range_new_name))) {
  out_range_new_sk_theta[[i]] <- data_sk[, c(max_temp_sk_new[max_temp_sk_new$range_new_name %in% unique(max_temp_sk_new$range_new_name)[i], 
                                                     "item"])]
  model_out_range_new_sk_theta[[i]] <- tam.mml(out_range_new_sk_theta[[i]], 
                                           xsi.fixed = cbind(1:ncol(out_range_new_sk_theta[[i]]), 
                                                             diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                       colnames(out_range_new_sk_theta[[i]]))), 2]), 
                                           B= array(c(rep(0, ncol(out_range_new_sk_theta[[i]])), 
                                                      discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                      colnames(out_range_new_sk_theta[[i]])))]), 
                                                    c(ncol(out_range_new_sk_theta[[i]]),2,1), 
                                                    dimnames = list(colnames(out_range_new_sk_theta[[i]]), 
                                                                    c("Cat0", "Cat1"), 
                                                                    "Dim01")))
  info_out_range_new_sk_theta[[i]] <- IRT.informationCurves(model_out_range_new_sk_theta[[i]], 
                                                        theta = seq(-3, 3, length = 1000))
  names(info_out_range_new_sk_theta)[[i]] <- unique(max_temp_sk_new$range_new_name)[i]
}




# range_new ----
info_summary_range_new_sk <- NULL
temp <- NULL
for(i in 1:length(info_out_range_new_sk)) {
  temp <- data.frame(info_test_sk_new = mean(info_out_range_new_sk[[i]]$test_info_curve), 
                     
                     
                     range_new_name = names(info_out_range_new_sk)[[i]], 
                     item = paste(colnames(out_range_new_sk[[i]]), collapse = ","))
  
  info_summary_range_new_sk <- rbind(info_summary_range_new_sk, 
                                 temp)
}

info_summary_range_new_sk$rel <- 1 - (1/sqrt(info_summary_range_new_sk$info_test_sk_new))^2
info_summary_range_new_sk <-  rbind(info_summary_range_new_sk, 
                                data.frame(info_test_sk_new = sum(info_start_sk_new),
                                           range_new_name = "all", 
                                           item = "all", 
                                           rel = 1 - (1/sqrt(info_start_sk_new))^2))
info_summary_range_new_sk$selection <- "guidedSK"

ggplot(info_summary_range_new_sk, 
       aes(x=as.factor(range_new_name), y = info_test_sk_new)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(info_summary_range_new_sk, 
       aes(x=as.factor(range_new_name), y = rel)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

info_summary_range_new_sk$items <- 1:nrow(info_summary_range_new_sk)


# range_new con theta e parametri fissi ----
info_summary_range_new_sk_theta <- NULL
temp <- NULL
for(i in 1:length(info_out_range_new_sk)) {
  temp <- data.frame(info_test = mean(info_out_range_new_sk_theta[[i]]$test_info_curve), 
                     
                     
                     range_new_name = names(info_out_range_new_sk_theta)[[i]], 
                     item = paste(colnames(out_range_new_sk_theta[[i]]), collapse = ","))
  
  info_summary_range_new_sk_theta <- rbind(info_summary_range_new_sk_theta, 
                                       temp)
}

info_summary_range_new_sk_theta$rel <- 1 - (1/sqrt(info_summary_range_new_sk_theta$info_test))^2

info_summary_range_new_sk_theta <-  rbind(info_summary_range_new_sk_theta, 
                                      data.frame(info_test = info_start_sk_new,
                                                 range_new_name = "all", 
                                                 item = "all", 
                                                 rel = 1 - (1/sqrt(info_start_sk_new))^2))
info_summary_range_new_sk_theta$selection <- "range_newSK"


ggplot(info_summary_range_new_sk_theta, 
       aes(x = range_new_name, y = info_test)) + geom_point()

ggplot(info_summary_range_new_sk_theta, 
       aes(x = range_new_name, y = rel)) + geom_point()

ggplot(info_summary_range_new_sk, 
       aes(x = range_new_name, y = info_test_sk_new)) + geom_point()

ggplot(info_summary_range_new_sk_theta, 
       aes(x = range_new_name, y = rel)) + geom_point()


# ## confronto diretto con la strategia sk ottenuta con l'altro metodo di suddivisione 
# 
# info_summary_range_sk_new = info_summary_range_sk 
# info_summary_range_sk_new$selection = "guidedNEW"
# info_summary_range_sk_new$items = NULL
# info_summary_range_sk_theta_new = info_summary_range_sk_theta
# info_summary_range_sk_theta_new$selection = "guidedNEW"
# info_summary_range_sk_theta_new$items = NULL
# 
# load("SKguided.RData")
# load("SKsmart.RData")
# load("SKguidedEX.RData")
# load("SKsmartEX.RData")
# 
# colnames(info_summary_smart_sk) = gsub("smart", 'range', 
#                                        colnames(info_summary_smart_sk))
# 
# colnames(info_summary_smart_sk_ex) = gsub("smart", 'range', 
#                                           colnames(info_summary_smart_sk_ex))
# colnames(info_summary_smart_sk_ex) = gsub("_ex", '', 
#                                           colnames(info_summary_smart_sk_ex))
# 
# info_summary_range_sk$items = NULL
# info_summary_range_sk_ex$items = NULL
# colnames(info_summary_range_sk_new) = gsub("_sk", 
#                                            "", 
#                                            colnames(info_summary_range_sk_new))
# colnames(info_summary_range_sk) = gsub("_sk", 
#                                        "", 
#                                        colnames(info_summary_range_sk))
# colnames(info_summary_range_sk_ex) = gsub("_sk", 
#                                           "", 
#                                           colnames(info_summary_range_sk_ex))
# colnames(info_summary_range_sk_ex) = gsub("_ex", 
#                                           "", 
#                                           colnames(info_summary_range_sk_ex))
# info_summary_comparison = rbind(info_summary_range_sk_new, 
#                                 info_summary_range_sk,
#                                 info_summary_range_sk_ex,
#                                 info_summary_smart_sk, 
#                                 info_summary_smart_sk_ex)
# 
# colnames(info_summary_smart_sk_theta) = gsub("smart", 'range',
#                                              colnames(info_summary_smart_sk_theta))
# info_summary_range_sk_theta$items = NULL
# info_summary_range_sk_ex$items = NULL
# colnames(info_summary_range_sk_theta_new) = gsub("_sk", 
#                                                  "", 
#                                                  colnames(info_summary_range_sk_theta_new))
# colnames(info_summary_range_sk_theta) = gsub("_sk", 
#                                              "", 
#                                              colnames(info_summary_range_sk_theta))
# colnames(info_summary_range_sk_ex_theta) = gsub("_sk", 
#                                                 "", 
#                                                 colnames(info_summary_range_sk_ex_theta))
# colnames(info_summary_range_sk_ex_theta) = gsub("_ex", 
#                                                 "", 
#                                                 colnames(info_summary_range_sk_ex_theta))
# colnames(info_summary_smart_sk_theta) = gsub("smart", "range", 
#                                              colnames(info_summary_smart_sk_theta))
# 
# colnames(info_summary_smart_sk_ex_theta) = gsub("smart", "range", 
#                                                 colnames(info_summary_smart_sk_ex_theta))
# 
# 
# info_summary_comparison_theta = rbind(info_summary_range_sk_theta_new, 
#                                       info_summary_range_sk_theta,
#                                       info_summary_range_sk_ex_theta,
#                                       info_summary_smart_sk_theta, 
#                                       info_summary_smart_sk_ex_theta)
# 
# 
# ggplot(info_summary_comparison, 
#        aes(x=as.factor(range_name), y = info_test, 
#            col = selection)) + 
#   geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# ggplot(info_summary_comparison_theta, 
#        aes(x=as.factor(range_name), y = info_test, 
#            col = selection)) + 
#   geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# graph_start_sk <- data.frame(theta = IRT.informationCurves(m2pl_sk, 
#                                                            theta = seq(-3,3,length = 1000))$theta, 
#                              info = (IRT.informationCurves(m2pl_sk, 
#                                                            theta = seq(-3,3,length = 1000))$test_info_curve), 
#                              num_item = rep(unique(data_info_sk$num_item), 
#                                             each = 1000),
#                              sel = "start", 
#                              distributio = "sk")
# graph_start_sk_ex <- data.frame(theta = IRT.informationCurves(m2pl_sk_ex, 
#                                                               theta = seq(-3,3,length = 1000))$theta, 
#                                 info = (IRT.informationCurves(m2pl_sk_ex, 
#                                                               theta = seq(-3,3,length = 1000))$test_info_curve), 
#                                 num_item = rep(unique(data_info_sk_ex$num_item), 
#                                                each = 1000),
#                                 sel = "startEX", 
#                                 distributio = "skEX")
# 
# graph_start = rbind(graph_start_sk, graph_start_sk_ex)

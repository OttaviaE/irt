library(ltm)
library(psych)
library(ggplot2)
library(TAM)
library(sn)
rm(list = ls())
set.seed(999)

load("model_theta_itemUNI.RData")
# specifica la selezione casuale di item, 
# ognuian ripetuta per 100 volte
sampling_uni <- c(rep(seq(10, 90, by = 20), each = 50))
#sampling_uni <- c(rep(2:9, each = 10))
# prepara la selezione casuale di item 
item_random_uni <- list()
info_test_random_uni <- list()

model_fit_random_uni <- list()
info_start_uni <- mean(IRT.informationCurves(m2pl_uni, 
                                             theta = seq(-3,3, length = 1000))$test_info_curve)


# fitta i modelli 100 volte per ogni numerosità di item, scegliendo a caso 
# il numero di item dalla matrice di dati 
for (i in 1:length(sampling_uni)) {
  
  item_random_uni[[i]] <-  data.frame(data_uni[, sample(seq_len(ncol(data_uni)), 
                                                        size = sampling_uni[i])])
  
  model_fit_random_uni[[i]] <- tam.mml.2pl(item_random_uni[[i]])
  
  # calcolo solo la info totale del test
  info_test_random_uni[[i]] <- (IRT.informationCurves(model_fit_random_uni[[i]], 
                                                      theta = seq(-3, 3, length = 1000)))
}


data_random_uni <- data.frame(matrix(nrow = 1:length(item_random_uni)))

for (i in 1:length(item_random_uni)) {
  data_random_uni[i, "info_total"] <- mean(info_test_random_uni[[i]]$test_info_curve)
  data_random_uni[i, "num_item"] <- paste0("number", (model_fit_random_uni[[i]]$nitem))
  data_random_uni[i, "combo_item"] <- paste(c((model_fit_random_uni[[i]]$item$item)), 
                                            collapse = " ")
}
data_random_uni <- data_random_uni[, -1]

# non capisco eprché viene mionore di 0
data_random_uni$rel <- 1 - (1/sqrt(data_random_uni$info_total))^2

# info_start <- IRT.informationCurves(m2pl, 
#                                         theta = seq(-3, 3, length = 100))

data_random_uni  <- rbind(data_random_uni, 
                          data.frame(info_total = info_start_uni,
                                     num_item = "all", 
                                     combo_item = "all", 
                                     rel = 1 - (1/sqrt(info_start_uni))^2
                          ))

data_random_uni_summary <- cbind(aggregate(info_total ~ num_item, 
                                           data = data_random_uni,
                                           mean),
                                 aggregate(info_total ~ num_item, data = data_random_uni, sd)[,2], 
                                 aggregate(rel ~ num_item, 
                                           data = data_random_uni,
                                           mean)[,2],
                                 aggregate(rel ~ num_item, data = data_random_uni, sd)[,2])

colnames(data_random_uni_summary)[2:ncol(data_random_uni_summary)] <- c("mean_info", 
                                                                        "sd_info", 
                                                                        "mean_rel",
                                                                        "sd_rel")
data_random_uni_summary[is.na(data_random_uni_summary)] <-0



ggplot(data_random_uni_summary, 
       aes(x=num_item, y=mean_info)) + 
  # geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), width=.2,
                position=position_dodge(0.05)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# theta con parametri item fissi -----

item_random_uni_theta <- list()
info_test_random_uni_theta <- list()

model_fit_random_uni_theta <- list()

for (i in 1:length(sampling_uni)) {
  
  item_random_uni_theta[[i]] <-  data.frame(data_uni[, sample(seq_len(ncol(data_uni)), 
                                                              size = sampling_uni[i])])
  
  model_fit_random_uni_theta[[i]] <- tam.mml(item_random_uni_theta[[i]], 
                                                 xsi.fixed = cbind(1:ncol(item_random_uni_theta[[i]]), 
                                                                   diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                                 colnames(item_random_uni_theta[[i]]))), 2]), 
                                             B= array(c(rep(0, ncol(item_random_uni_theta[[i]])), 
                                                        discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                        colnames(item_random_uni_theta[[i]])))]), 
                                                      c(ncol(item_random_uni_theta[[i]]),2,1), 
                                                      dimnames = list(colnames(item_random_uni_theta[[i]]), 
                                                                      c("Cat0", "Cat1"), 
                                                                      "Dim01")))
  
  # calcolo solo la info totale del test
  info_test_random_uni_theta[[i]] <- (IRT.informationCurves(model_fit_random_uni_theta[[i]], 
                                                            theta = seq(-3, 3, length = 1000)))
}


data_random_uni_theta <- data.frame(matrix(nrow = 1:length(item_random_uni_theta)))

for (i in 1:length(item_random_uni_theta)) {
  data_random_uni_theta[i, "info_total"] <- mean(info_test_random_uni_theta[[i]]$test_info_curve)
  data_random_uni_theta[i, "num_item"] <- paste0("number", (model_fit_random_uni_theta[[i]]$nitem))
  data_random_uni_theta[i, "combo_item"] <- paste(c((model_fit_random_uni_theta[[i]]$item$item)), 
                                                  collapse = " ")
}
data_random_uni_theta <- data_random_uni_theta[, -1]

# non capisco eprché viene mionore di 0
data_random_uni_theta$rel <- 1 - (1/sqrt(data_random_uni_theta$info_total))^2


data_random_uni_theta  <- rbind(data_random_uni_theta, 
                                data.frame(info_total = info_start_uni,
                                           num_item = "all", 
                                           combo_item = "all", 
                                           rel = 1 - (1/sqrt(info_start_uni))^2
                                ))

data_random_uni_theta_summary <- cbind(aggregate(info_total ~ num_item, 
                                                 data = data_random_uni_theta,
                                                 mean),
                                       aggregate(info_total ~ num_item, data = data_random_uni_theta, sd)[,2], 
                                       aggregate(rel ~ num_item, 
                                                 data = data_random_uni_theta,
                                                 mean)[,2],
                                       aggregate(rel ~ num_item, data = data_random_uni_theta, sd)[,2])

colnames(data_random_uni_theta_summary)[2:ncol(data_random_uni_theta_summary)] <- c("mean_info", 
                                                                                    "sd_info", 
                                                                                    "mean_rel",
                                                                                    "sd_rel")
data_random_uni_theta_summary[is.na(data_random_uni_theta_summary)] <-0



ggplot(data_random_uni_theta_summary, 
       aes(x=num_item, y=mean_info)) + 
  # geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), width=.2,
                position=position_dodge(0.05)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(data_random_uni_summary, 
       aes(x=num_item, y=mean_info)) + 
  # geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), width=.2,
                position=position_dodge(0.05)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



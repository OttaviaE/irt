library(ltm)
library(psych)
library(ggplot2)
library(TAM)
library(sn)
rm(list = ls())
set.seed(666)


load("model_theta_itemSK_ex.RData")
hist(true_theta_sk_ex)
# 
# specifica la selezione casuale di item, 
# ognuian ripetuta per 100 volte
sampling_sk_ex <- c(rep(seq(10, 90, by = 10), each = 50))
#sampling_sk_ex <- c(rep(2:9, each = 10))
# prepara la selezione casuale di item 
item_random_sk_ex <- list()
info_test_random_sk_ex <- list()

model_fit_random_sk_ex <- list()

# fitta i modelli 100 volte per ogni numerosità di item, scegliendo a caso 
# il numero di item dalla matrice di dati 
for (i in 1:length(sampling_sk_ex)) {
  
  item_random_sk_ex[[i]] <-  data.frame(data_sk_ex[, sample(seq_len(ncol(data_sk_ex)), 
                                                size = sampling_sk_ex[i])])
  
  model_fit_random_sk_ex[[i]] <- tam.mml.2pl(item_random_sk_ex[[i]])
  
  # calcolo solo la info totale del test
  info_test_random_sk_ex[[i]] <- (IRT.informationCurves(model_fit_random_sk_ex[[i]], 
                                                 theta = seq(-3, 3, length = 1000)))
}

info_start_sk_ex <- mean(IRT.informationCurves(m2pl_sk_ex, 
                                            theta = seq(-3,3, length = 1000))$test_info_curve)


data_random_sk_ex <- data.frame(matrix(nrow = 1:length(item_random_sk_ex)))

for (i in 1:length(item_random_sk_ex)) {
  data_random_sk_ex[i, "info_total"] <- mean(info_test_random_sk_ex[[i]]$test_info_curve)
  data_random_sk_ex[i, "num_item"] <- paste0("number", (model_fit_random_sk_ex[[i]]$nitem))
  data_random_sk_ex[i, "combo_item"] <- paste(c((model_fit_random_sk_ex[[i]]$item$item)), 
                                        collapse = " ")
}
data_random_sk_ex <- data_random_sk_ex[, -1]

# non capisco eprché viene mionore di 0
data_random_sk_ex$rel <- 1 - (1/sqrt(data_random_sk_ex$info_total))^2


data_random_sk_ex  <- rbind(data_random_sk_ex, 
      data.frame(info_total = info_start_sk_ex,
        num_item = "all", 
                 combo_item = "all", 
        rel = 1 - (1/sqrt(info_start_sk_ex))^2
                 ))

data_random_sk_ex_summary <- cbind(aggregate(info_total ~ num_item, 
                                       data = data_random_sk_ex,
                                       mean),
aggregate(info_total ~ num_item, data = data_random_sk_ex, sd)[,2], 
aggregate(rel ~ num_item, 
          data = data_random_sk_ex,
          mean)[,2],
aggregate(rel ~ num_item, data = data_random_sk_ex, sd)[,2])

colnames(data_random_sk_ex_summary)[2:ncol(data_random_sk_ex_summary)] <- c("mean_info", 
                                                                "sd_info", 
                                                                "mean_rel",
                                                                "sd_rel")
data_random_sk_ex_summary[is.na(data_random_sk_ex_summary)] <-0



ggplot(data_random_sk_ex_summary, 
       aes(x=num_item, y=mean_info)) + 
 # geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), width=.2,
                position=position_dodge(0.05)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





# theta con parametri item fissi -----

item_random_sk_ex_theta <- list()
info_test_random_sk_ex_theta <- list()

model_fit_random_sk_ex_theta <- list()

for (i in 1:length(sampling_sk_ex)) {
  
  item_random_sk_ex_theta[[i]] <-  data.frame(data_sk_ex[, sample(seq_len(ncol(data_sk_ex)), 
                                                                  size = sampling_sk_ex[i])])
  
  model_fit_random_sk_ex_theta[[i]] <- tam.mml(item_random_sk_ex_theta[[i]], 
                                                xsi.fixed = cbind(1:ncol(item_random_sk_ex_theta[[i]]), 
                                                                  diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                               colnames(item_random_sk_ex_theta[[i]]))), 2]), 
                                            B= array(c(rep(0, ncol(item_random_sk_ex_theta[[i]])), 
                                                       discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                       colnames(item_random_sk_ex_theta[[i]])))]), 
                                                     c(ncol(item_random_sk_ex_theta[[i]]),2,1), 
                                                     dimnames = list(colnames(item_random_sk_ex_theta[[i]]), 
                                                                     c("Cat0", "Cat1"), 
                                                                     "Dim01")))
  
  # calcolo solo la info totale del test
  info_test_random_sk_ex_theta[[i]] <- (IRT.informationCurves(model_fit_random_sk_ex_theta[[i]], 
                                                           theta = seq(-3, 3, length = 1000)))
}


data_random_sk_ex_theta <- data.frame(matrix(nrow = 1:length(item_random_sk_ex_theta)))

for (i in 1:length(item_random_sk_ex_theta)) {
  data_random_sk_ex_theta[i, "info_total"] <- mean(info_test_random_sk_ex_theta[[i]]$test_info_curve)
  data_random_sk_ex_theta[i, "num_item"] <- paste0("number", (model_fit_random_sk_ex_theta[[i]]$nitem))
  data_random_sk_ex_theta[i, "combo_item"] <- paste(c((model_fit_random_sk_ex_theta[[i]]$item$item)), 
                                                 collapse = " ")
}
data_random_sk_ex_theta <- data_random_sk_ex_theta[, -1]

# non capisco eprché viene mionore di 0
data_random_sk_ex_theta$rel <- 1 - (1/sqrt(data_random_sk_ex_theta$info_total))^2


data_random_sk_ex_theta  <- rbind(data_random_sk_ex_theta, 
                               data.frame(info_total = info_start_sk_ex,
                                          num_item = "all", 
                                          combo_item = "all", 
                                          rel = 1 - (1/sqrt(info_start_sk_ex))^2
                               ))

data_random_sk_ex_theta_summary <- cbind(aggregate(info_total ~ num_item, 
                                                data = data_random_sk_ex_theta,
                                                mean),
                                      aggregate(info_total ~ num_item, data = data_random_sk_ex_theta, sd)[,2], 
                                      aggregate(rel ~ num_item, 
                                                data = data_random_sk_ex_theta,
                                                mean)[,2],
                                      aggregate(rel ~ num_item, data = data_random_sk_ex_theta, sd)[,2])

colnames(data_random_sk_ex_theta_summary)[2:ncol(data_random_sk_ex_theta_summary)] <- c("mean_info", 
                                                                                  "sd_info", 
                                                                                  "mean_rel",
                                                                                  "sd_rel")
data_random_sk_ex_theta_summary[is.na(data_random_sk_ex_theta_summary)] <-0



ggplot(data_random_sk_ex_theta_summary, 
       aes(x=num_item, y=mean_info)) + 
  # geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), width=.2,
                position=position_dodge(0.05)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data_random_sk_ex_summary, 
       aes(x=num_item, y=mean_info)) + 
  # geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), width=.2,
                position=position_dodge(0.05)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





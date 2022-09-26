library(ltm)
library(psych)
library(ggplot2)
library(TAM)
rm(list = ls())
set.seed(666)


load("model_theta_item.RData")
hist(true_theta)
# specifica la selezione casuale di item, 
# ognuian ripetuta per 100 volte
sampling <- c(rep(seq(10, 90, by = 20), each = 10))
# sampling <- c(rep(2:9, each = 10))
# prepara la selezione casuale di item 
item_random <- list()
info_test_random <- list()

model_fit_random <- list()

info_start <- mean(IRT.informationCurves(m2pl, 
                                             theta = seq(-3,3, length = 1000))$test_info_curve)


# fitta i modelli 100 volte per ogni numerosità di item, scegliendo a caso 
# il numero di item dalla matrice di dati 
for (i in 1:length(sampling)) {
  
  item_random[[i]] <-  data.frame(data[, sample(seq_len(ncol(data)), 
                                                size = sampling[i])])
  
  model_fit_random[[i]] <- tam.mml.2pl(item_random[[i]])
  
  # calcolo solo la info totale del test
  info_test_random[[i]] <- (IRT.informationCurves(model_fit_random[[i]], 
                                                 theta = seq(-3, 3, length = 1000)))
}


data_random <- data.frame(matrix(nrow = 1:length(item_random)))

for (i in 1:length(item_random)) {
  data_random[i, "info_total"] <- mean(info_test_random[[i]]$test_info_curve)
  data_random[i, "num_item"] <- paste0("number", (model_fit_random[[i]]$nitem))
  data_random[i, "combo_item"] <- paste(c((model_fit_random[[i]]$item$item)), 
                                        collapse = " ")
}
data_random <- data_random[, -1]

# non capisco eprché viene mionore di 0
data_random$rel <- 1 - (1/sqrt(data_random$info_total))^2

# info_start <- IRT.informationCurves(m2pl, 
#                                         theta = seq(-3, 3, length = 100))

data_random  <- rbind(data_random, 
      data.frame(info_total = info_start,
        num_item = "all", 
                 combo_item = "all", 
        rel = 1 - (1/sqrt(info_start))^2
                 ))

data_random_summary <- cbind(aggregate(info_total ~ num_item, 
                                       data = data_random,
                                       mean),
aggregate(info_total ~ num_item, data = data_random, sd)[,2], 
aggregate(rel ~ num_item, 
          data = data_random,
          mean)[,2],
aggregate(rel ~ num_item, data = data_random, sd)[,2])

colnames(data_random_summary)[2:ncol(data_random_summary)] <- c("mean_info", 
                                                                "sd_info", 
                                                                "mean_rel",
                                                                "sd_rel")
data_random_summary[is.na(data_random_summary)] <-0



ggplot(data_random_summary, 
       aes(x=num_item, y=mean_info)) + 
 # geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), width=.2,
                position=position_dodge(0.05)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot(info_start, xlim=c(-3,3), ylim =c(0, 1.20))
par(new = TRUE)
plot(info_test_random[[1]], col = "red")
par(new = TRUE)
plot(info_test_random[[2]], col = "blue")

# theta ottenuti con item fissati -----

item_random_theta <- list()
info_test_random_theta <- list()

model_fit_random_theta <- list()

# fitta i modelli 100 volte per ogni numerosità di item, scegliendo a caso 
# il numero di item dalla matrice di dati 
for (i in 1:length(sampling)) {
  
  item_random_theta[[i]] <-  data.frame(data[, sample(seq_len(ncol(data)), 
                                                      size = sampling[i])])
  
  model_fit_random_theta[[i]] <- tam.mml(item_random_theta[[i]], 
                                             xsi.fixed = cbind(1:ncol(item_random_theta[[i]]), 
                                                               diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                         colnames(item_random_theta[[i]]))), 2]), 
                                         B= array(c(rep(0, ncol(item_random_theta[[i]])), 
                                                    discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                    colnames(item_random_theta[[i]])))]), 
                                                  c(ncol(item_random_theta[[i]]),2,1), 
                                                  dimnames = list(colnames(item_random_theta[[i]]), 
                                                                  c("Cat0", "Cat1"), 
                                                                  "Dim01")))
  
  # calcolo solo la info totale del test
  info_test_random_theta[[i]] <- (IRT.informationCurves(model_fit_random_theta[[i]], 
                                                        theta = seq(-3, 3, length = 1000)))
}


data_random_theta <- data.frame(matrix(nrow = 1:length(item_random_theta)))

for (i in 1:length(item_random_theta)) {
  data_random_theta[i, "info_total"] <- mean(info_test_random_theta[[i]]$test_info_curve)
  data_random_theta[i, "num_item"] <- paste0("number", (model_fit_random_theta[[i]]$nitem))
  data_random_theta[i, "combo_item"] <- paste(c((model_fit_random_theta[[i]]$item$item)), 
                                              collapse = " ")
}
data_random_theta <- data_random_theta[, -1]

# non capisco eprché viene mionore di 0
data_random_theta$rel <- 1 - (1/sqrt(data_random_theta$info_total))^2

# info_start <- IRT.informationCurves(m2pl, 
#                                         theta = seq(-3, 3, length = 100))

data_random_theta  <- rbind(data_random_theta, 
                            data.frame(info_total = info_start,
                                       num_item = "all", 
                                       combo_item = "all", 
                                       rel = 1 - (1/sqrt(info_start))^2
                            ))

data_random_theta_summary <- cbind(aggregate(info_total ~ num_item, 
                                             data = data_random_theta,
                                             mean),
                                   aggregate(info_total ~ num_item, data = data_random_theta, sd)[,2], 
                                   aggregate(rel ~ num_item, 
                                             data = data_random_theta,
                                             mean)[,2],
                                   aggregate(rel ~ num_item, data = data_random_theta, sd)[,2])

colnames(data_random_theta_summary)[2:ncol(data_random_theta_summary)] <- c("mean_info", 
                                                                            "sd_info", 
                                                                            "mean_rel",
                                                                            "sd_rel")
data_random_theta_summary[is.na(data_random_theta_summary)] <-0



ggplot(data_random_theta_summary, 
       aes(x=num_item, y=mean_info)) + 
  # geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), width=.2,
                position=position_dodge(0.05)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data_random_summary, 
       aes(x=num_item, y=mean_info)) + 
  # geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), width=.2,
                position=position_dodge(0.05)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


plot(info_start, xlim=c(-3,3), ylim =c(0, 1.20))
par(new = TRUE)
plot(info_test_random_theta[[1]], col = "red")
par(new = TRUE)
plot(info_test_random_theta[[2]], col = "blue")

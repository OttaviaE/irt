library(gridExtra)
library(ggplot2)
library(TAM)
rm(list = ls())

# per dividere il tratto latente ---- 
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}

load("summaryUNI.RData")
load("summarySK.RData")
load("summaryNormal.RData")

all_data_normal_theta$selection <- gsub("_theta|Theta", "", 
                                        all_data_normal_theta$selection)
all_data_normal_theta$distribution = "normal"
all_data_normal$distribution = "normal"

all_data_sk_theta$selection <- gsub("SK", "", 
                                        all_data_sk_theta$selection)

all_data_sk_theta$distribution = "sk"
all_data_sk$distribution = "sk"

all_data_uni$selection <- gsub("UNI", "", 
                                    all_data_sk$selection)


all_data_uni_theta$distribution = "uni"
all_data_uni$distribution = "uni"

# dati information 

data <- rbind(all_data_normal, all_data_sk, 
              all_data_uni)

data_theta <-  rbind(all_data_normal_theta, all_data_sk_theta, 
                            all_data_uni_theta)
data_theta$selection <- gsub("uni", "", data_theta$selection)
mean_info <- data[data$num_item %in% "all", ]
mean_info_theta <- data_theta[data_theta$num_item %in% "all", ]        
# Informaztion
ggplot(data[!data$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "bottom") + 
  scale_x_discrete(labels =  unique(data[!data$num_item %in%"all", "num_item"])) + 
  facet_wrap(~distribution) + ylim(0,30) +
  geom_hline(data = mean_info, aes(yintercept = mean_info)) + ggtitle("Info - Parametri liberi")


ggplot(data_theta[!data_theta$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_info, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "bottom") + 
  scale_x_discrete(labels =  unique(data_theta[!data_theta$num_item %in%"all", "num_item"])) + 
  facet_wrap(~distribution) + ylim(0,30) +
  geom_hline(data = mean_info_theta, 
             aes(yintercept = mean_info)) + ggtitle("Info - Parametri fissi")


# insieme 

grid.arrange(ggplot(data[!data$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_info, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "bottom") + 
               scale_x_discrete(labels =  unique(data[!data$num_item %in%"all", "num_item"])) + 
               facet_wrap(~distribution) + ylim(0,30) +
               geom_hline(data = mean_info, aes(yintercept = mean_info)) + ggtitle("Info - Parametri liberi"),
             
             
             ggplot(data_theta[!data_theta$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_info, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "bottom") + 
               scale_x_discrete(labels =  unique(data_theta[!data_theta$num_item %in%"all", "num_item"])) + 
               facet_wrap(~distribution) + ylim(0,30) +
               geom_hline(data = mean_info_theta, 
                          aes(yintercept = mean_info)) + ggtitle("Info - Parametri fissi")
             
             , nrow = 2)

# Reliability ----

ggplot(data[!data$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "bottom") + 
  scale_x_discrete(labels =  unique(data[!data$num_item %in%"all", "num_item"])) + 
  facet_wrap(~distribution) + ylim(0,1) +
  geom_hline(data = mean_info, aes(yintercept = mean_rel)) + ggtitle("Reliability - Parametri liberi")


ggplot(data_theta[!data_theta$num_item %in%"all", ], 
       aes(x=as.factor(item_temp), y=mean_rel, 
           group=selection, color=selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.5) +
  geom_point(aes(shape=selection))+
  geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                width=.2,
                position=position_dodge(0.05)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = "bottom") + 
  scale_x_discrete(labels =  unique(data_theta[!data_theta$num_item %in%"all", "num_item"])) + 
  facet_wrap(~distribution) + ylim(0,1) +
  geom_hline(data = mean_info_theta, 
             aes(yintercept = mean_rel)) + ggtitle("Info - Parametri fissi")


# insieme 

grid.arrange(ggplot(data[!data$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_rel, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "bottom") + 
               scale_x_discrete(labels =  unique(data[!data$num_item %in%"all", "num_item"])) + 
               facet_wrap(~distribution) + ylim(0,1) +
               geom_hline(data = mean_info, aes(yintercept = mean_rel)) + ggtitle("Reliability - Parametri liberi"),
             
             
             ggplot(data_theta[!data_theta$num_item %in%"all", ], 
                    aes(x=as.factor(item_temp), y=mean_rel, 
                        group=selection, color=selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.5) +
               geom_point(aes(shape=selection))+
               geom_errorbar(aes(ymin=mean_rel-sd_rel, ymax=mean_rel+sd_rel), 
                             width=.2,
                             position=position_dodge(0.05)) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     legend.position = "bottom") + 
               scale_x_discrete(labels =  unique(data_theta[!data_theta$num_item %in%"all", "num_item"])) + 
               facet_wrap(~distribution) + ylim(0,1) +
               geom_hline(data = mean_info_theta, 
                          aes(yintercept = mean_rel)) + ggtitle("Info - Parametri fissi"), nrow = 2)


# faccio i plots delle info ma solo con 5 item per ognuna delle cose ---- 
# con la curva di all item di confronto -- 
tif_all_normal <- data.frame(theta = IRT.informationCurves(m2pl, 
                                                           theta = seq(-3, 3, 
                                                                       length = 1000))$theta,
                             info = IRT.informationCurves(m2pl, 
                                                          theta = seq(-3, 3, 
                                                                      length = 1000))$test_info_curve, 
                             num_item = "all", 
                             sel = "start", 
                             distributio = "normal")

tif_all_sk <- data.frame(theta = IRT.informationCurves(m2pl_sk, 
                                                           theta = seq(-3, 3, 
                                                                       length = 1000))$theta,
                             info = IRT.informationCurves(m2pl_sk, 
                                                          theta = seq(-3, 3, 
                                                                      length = 1000))$test_info_curve, 
                             num_item = "all", 
                             sel = "start", 
                         distributio = "sk")

tif_all_uni <- data.frame(theta = IRT.informationCurves(m2pl_uni, 
                                                       theta = seq(-3, 3, 
                                                                   length = 1000))$theta,
                         info = IRT.informationCurves(m2pl_uni, 
                                                      theta = seq(-3, 3, 
                                                                  length = 1000))$test_info_curve, 
                         num_item = "all", 
                         sel = "start", 
                         distributio = "uni")

tif_all <- rbind(tif_all_normal, 
                 tif_all_sk, tif_all_uni)


ggplot(tif_all, 
       aes(x = theta, y = info, group = distributio, 
           col = distributio)) + 
  geom_line(aes(linetype = distributio), lwd = 2) + 
  labs(title = "TIF-All item", 
       subtitle = "Parametri liberi")

# ad onguno dei dataset con le information aggiugo la colonna per la distribuzione

data_info_uni$distributio = "uni"
data_info_uni_theta$distributio = "uni"

data_info_sk$distributio = "sk"
data_info_sk_theta$distributio = "sk"

data_info$distributio = "normal"
data_info_theta$distributio = "normal"


tif <- rbind(tif_all, 
                 data_info_uni, 
                 data_info_sk, 
                 data_info)

tif_theta <- rbind(tif_all, 
                       data_info_uni_theta, 
                       data_info_sk_theta, 
                       data_info_theta)
tif_theta$sel = gsub("_theta", "", tif_theta$sel)

ggplot(rbind(tif[tif$num_item %in% "all", ], 
             tif[tif$num_item %in% "number10", ]), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 2) + facet_wrap(~distributio)


ggplot(rbind(tif_theta[tif_theta$num_item %in% "all", ], 
             tif_theta[tif_theta$num_item %in% "number10", ]), 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 2) + 
  facet_wrap(~distributio) + 
  labs(title = "TIF-10item", 
       subtitle = "Parametri liberi")

grid.arrange(ggplot(rbind(tif[tif$num_item %in% "all", ], 
                          tif[tif$num_item %in% "number10", ]), 
                    aes(x = theta, y = info, group = sel, 
                        col = sel)) + geom_line(aes(linetype = sel), lwd = 2) + 
               facet_wrap(~distributio)  + theme(legend.position = "bottom") +
               labs(title = "TIF-10item", 
                    subtitle = "Parametri fissi"),
             ggplot(rbind(tif_theta[tif_theta$num_item %in% "all", ], 
                          tif_theta[tif_theta$num_item %in% "number10", ]), 
                    aes(x = theta, y = info, group = sel, 
                        col = sel)) + geom_line(aes(linetype = sel), lwd = 2) + 
               facet_wrap(~distributio)  + theme(legend.position = "none") +
               labs(title = "TIF-10item", 
                    subtitle = "Parametri liberi"), 
             nrow = 2
)

# GRAFICI DEI BIAS ---- 
all_bias_normal$distribution = "normal"
all_bias_sk$distribution = "sk"
all_bias_uni$distribution = "uni"


bias_all = rbind(all_bias_normal, 
                 all_bias_sk, 
                 all_bias_uni)
ggplot(bias_all[bias_all$type %in% "bias_abs", ], 
       aes(x=as.factor(temp), y = bias_all, group = selection, 
           color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust=1) ,
        legend.position = "top") + facet_wrap(~distribution) +
  labs(title = "Bias stima con tutti gli item", 
       subtitle = "Parametri liberi")

ggplot(bias_all[bias_all$type %in% "bias_abs", ], 
       aes(x=as.factor(temp), y = rmsea, group = selection, 
           color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust=1) ,
        legend.position = "top") + facet_wrap(~distribution) +
  labs(title = "RMSEA stima con tutti gli item", 
       subtitle = "Parametri liberi")

# all theta 
all_bias_normal_theta$distribution = "normal"
all_bias_sk_theta$distribution = "sk"
all_bias_uni_theta$distribution = "uni"


bias_all_theta = rbind(all_bias_normal_theta, 
                 all_bias_sk_theta, 
                 all_bias_uni_theta)
ggplot(bias_all_theta[bias_all_theta$type %in% "bias_abs", ], 
       aes(x=as.factor(temp), y = bias_all, group = selection, 
           color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust=1) ,
        legend.position = "top") + facet_wrap(~distribution) +
  labs(title = "Bias stima con tutti gli item", 
       subtitle = "Parametri fissi")

ggplot(bias_all_theta[bias_all_theta$type %in% "bias_abs", ], 
       aes(x=as.factor(temp), y = rmsea, group = selection, 
           color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust=1) ,
        legend.position = "top") + facet_wrap(~distribution) +
  labs(title = "RMSEA con tutti gli item", 
       subtitle = "Parametri fissi")


# bias osservati 

obs_bias_normal$distribution = "normal"
obs_bias_sk$distribution = "sk"
obs_bias_uni$distribution = "uni"


bias_obs = rbind(obs_bias_normal, 
                 obs_bias_sk, 
                 obs_bias_uni)
bias_obs$rmsea <- sqrt(bias_obs$bias_obs)

ggplot(bias_obs[bias_obs$type %in% "bias_abs", ], 
       aes(x=as.factor(temp), y = bias_obs, group = selection, 
           color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust=1) ,
        legend.position = "top") + facet_wrap(~distribution) +
  labs(title = "Bias theta osservati", 
       subtitle = "Parametri liberi")

ggplot(bias_obs[bias_obs$type %in% "bias_abs", ], 
       aes(x=as.factor(temp), y = rmsea, group = selection, 
           color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust=1) ,
        legend.position = "top") + facet_wrap(~distribution) +
  labs(title = "RMSEA theta osservati", 
       subtitle = "Parametri liberi")


# obs theta 
obs_bias_normal_theta$distribution = "normal"
obs_bias_sk_theta$distribution = "sk"
obs_bias_uni_theta$distribution = "uni"


bias_obs_theta = rbind(obs_bias_normal_theta, 
                       obs_bias_sk_theta, 
                       obs_bias_uni_theta)
bias_obs_theta$rmsea <- sqrt(bias_obs_theta$bias_obs)

ggplot(bias_obs_theta[bias_obs_theta$type %in% "bias_abs", ], 
       aes(x=as.factor(temp), y = bias_obs, group = selection, 
           color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust=1) ,
        legend.position = "top") + facet_wrap(~distribution) +
  labs(title = "Bias theta osservati", 
       subtitle = "Parametri fissi")

ggplot(bias_obs_theta[bias_obs_theta$type %in% "bias_abs", ], 
       aes(x=as.factor(temp), y = rmsea, group = selection, 
           color = selection)) + 
  geom_line(aes(linetype = selection), lwd = 1.3) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust=1) ,
        legend.position = "top") + facet_wrap(~distribution) +
  labs(title = "RMSEA theta osservati", 
       subtitle = "Parametri fissi")


grid.arrange(ggplot(bias_obs[bias_obs$type %in% "bias_abs", ], 
                    aes(x=as.factor(temp), y = bias_obs, group = selection, 
                        color = selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.3) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                                hjust=1) ,
                     legend.position = "top") + facet_wrap(~distribution) +
               ggtitle("Bias obs-Parametri liberi"), 
             ggplot(bias_all[bias_all$type %in% "bias_abs", ], 
                    aes(x=as.factor(temp), y = bias_all, group = selection, 
                        color = selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.3) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                                hjust=1) ,
                     legend.position = "top") + facet_wrap(~distribution) + 
               ggtitle("Bias all-Parametri liberi"), 
             nrow = 2)


grid.arrange(ggplot(bias_obs_theta[bias_obs_theta$type %in% "bias_abs", ], 
                    aes(x=as.factor(temp), y = bias_obs, group = selection, 
                        color = selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.3) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                                hjust=1) ,
                     legend.position = "top") + facet_wrap(~distribution) +
               ggtitle("Bias obs-Parametri fissi"), 
             ggplot(bias_all_theta[bias_all_theta$type %in% "bias_abs", ], 
                    aes(x=as.factor(temp), y = bias_all, group = selection, 
                        color = selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.3) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                                hjust=1) ,
                     legend.position = "top") + facet_wrap(~distribution) + 
               ggtitle("Bias all-Parametri fissi"), 
             nrow = 2)


grid.arrange(ggplot(bias_obs[bias_obs$type %in% "bias_abs", ], 
                    aes(x=as.factor(temp), y = rmsea, group = selection, 
                        color = selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.3) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                                hjust=1) ,
                     legend.position = "top") + facet_wrap(~distribution) +
               ggtitle("RMSEA obs-Parametri liberi"), 
             ggplot(bias_all[bias_all$type %in% "bias_abs", ], 
                    aes(x=as.factor(temp), y = rmsea, group = selection, 
                        color = selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.3) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                                hjust=1) ,
                     legend.position = "top") + facet_wrap(~distribution) + 
               ggtitle("RMSEA all-Parametri liberi"), 
             nrow = 2)


grid.arrange(ggplot(bias_obs_theta[bias_obs_theta$type %in% "bias_abs", ], 
                    aes(x=as.factor(temp), y = rmsea, group = selection, 
                        color = selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.3) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                                hjust=1) ,
                     legend.position = "top") + facet_wrap(~distribution) +
               ggtitle("RMSEA obs-Parametri fissi"), 
             ggplot(bias_all_theta[bias_all_theta$type %in% "bias_abs", ], 
                    aes(x=as.factor(temp), y = rmsea, group = selection, 
                        color = selection)) + 
               geom_line(aes(linetype = selection), lwd = 1.3) + 
               theme(axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                                hjust=1) ,
                     legend.position = "top") + facet_wrap(~distribution) + 
               ggtitle("RMSEA all-Parametri fissi"), 
             nrow = 2)


# bias per gruppi di theta (aiuto) -----
theta_lat_theta_normal <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat_theta_normal, length(theta_lat_theta_normal), include.lowest = TRUE)
cut_val = cut_borders(g)

group_name <- letters[1:(nrow(cut_val)+2)]

ggplot(bias_abs_normal_group_theta, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
   scale_x_discrete(breaks = c(group_name), 
                     labels = c("<-2.5", cut_val[,1], ">2.5"))  + 
  facet_wrap(~num_item)

theta_lat_theta_uni <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat_theta_uni, length(theta_lat_theta_uni), include.lowest = TRUE)
cut_val = cut_borders(g)

group_name <- letters[1:(nrow(cut_val)+2)]

ggplot(bias_abs_uni_group_theta, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
  scale_x_discrete(breaks = c(group_name), 
                   labels = c("<-2.5", cut_val[,1], ">2.5"))  + 
  facet_wrap(~num_item)

theta_lat_theta_sk <- seq(-2.5, 2.5, length.out = 4) 
g <- cut(theta_lat_theta_sk, length(theta_lat_theta_sk), include.lowest = TRUE)
cut_val = cut_borders(g)

group_name <- letters[1:(nrow(cut_val)+2)]

ggplot(bias_abs_sk_group_theta, 
       aes(x=group, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
  scale_x_discrete(breaks = c(group_name), 
                   labels = c("<-2.5", cut_val[,1], ">2.5"))  + 
  facet_wrap(~num_item)


# new interval ---
theta_lat_theta <- seq(-2.5, 2.5, length.out = 10) 

g <- cut(theta_lat_theta, length(theta_lat_theta), include.lowest = TRUE)
cut_val = cut_borders(g)


group_name10 <- letters[1:(nrow(cut_val)+2)]


theta_normal_theta$group10 <- ifelse(theta_normal_theta$obs  <= cut_val[1, "start"], 
                                     group_name10[1], 
                                     ifelse(theta_normal_theta$obs > cut_val[1, "start"] & theta_normal_theta$obs <= cut_val[1, "end"], 
                                            group_name10[2], 
                                            ifelse(theta_normal_theta$obs > cut_val[2, "start"] & theta_normal_theta$obs <= cut_val[2, "end"], 
                                                   group_name10[3], 
                                                   ifelse(theta_normal_theta$obs > cut_val[3, "start"] & theta_normal_theta$obs <= cut_val[3, "end"], 
                                                          group_name10[4], 
                                                          ifelse(theta_normal_theta$obs > cut_val[4, "start"] & theta_normal_theta$obs <= cut_val[4, "end"], 
                                                                 group_name10[5],
                                                                 ifelse(theta_normal_theta$obs > cut_val[5, "start"] & theta_normal_theta$obs <= cut_val[5, "end"], 
                                                                        group_name10[6], 
                                                                        ifelse(theta_normal_theta$obs > cut_val[6, "start"] & theta_normal_theta$obs <= cut_val[6, "end"], 
                                                                               group_name10[7], 
                                                                               ifelse(theta_normal_theta$obs > cut_val[7, "start"] & theta_normal_theta$obs <= cut_val[7, "end"], 
                                                                                      group_name10[8], 
                                                                                      ifelse(theta_normal_theta$obs > cut_val[8, "start"] & theta_normal_theta$obs <= cut_val[8, "end"], 
                                                                                             group_name10[9],
                                                                                             ifelse(theta_normal_theta$obs > cut_val[9, "start"] & theta_normal_theta$obs <= cut_val[9, "end"], 
                                                                                                    group_name10[10], 
                                                                                                    ifelse(theta_normal_theta$obs > cut_val[10, "start"] & theta_normal_theta$obs <= cut_val[10, "end"], 
                                                                                                           group_name10[11], 
                                                                                                           group_name10[12])))))))))))

bias_abs_normal_group_theta10 <- aggregate(bias_obs_abs ~ selection + group10 + num_item, 
                                         data = theta_normal_theta, mean)
bias_abs_normal_group_theta10$type = "bias"
ggplot(bias_abs_normal_group_theta10, 
       aes(x=group10, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
  scale_x_discrete(breaks = c(group_name10), 
                   labels = c("<-2.5", cut_val[,1], ">2.5"))  + 
  facet_wrap(~num_item)
 
# sk 
theta_sk_theta$group10 <- ifelse(theta_sk_theta$obs  <= cut_val[1, "start"], 
                                 group_name10[1], 
                                 ifelse(theta_sk_theta$obs > cut_val[1, "start"] & theta_sk_theta$obs <= cut_val[1, "end"], 
                                        group_name10[2], 
                                        ifelse(theta_sk_theta$obs > cut_val[2, "start"] & theta_sk_theta$obs <= cut_val[2, "end"], 
                                               group_name10[3], 
                                               ifelse(theta_sk_theta$obs > cut_val[3, "start"] & theta_sk_theta$obs <= cut_val[3, "end"], 
                                                      group_name10[4], 
                                                      ifelse(theta_sk_theta$obs > cut_val[4, "start"] & theta_sk_theta$obs <= cut_val[4, "end"], 
                                                             group_name10[5],
                                                             ifelse(theta_sk_theta$obs > cut_val[5, "start"] & theta_sk_theta$obs <= cut_val[5, "end"], 
                                                                    group_name10[6], 
                                                                    ifelse(theta_sk_theta$obs > cut_val[6, "start"] & theta_sk_theta$obs <= cut_val[6, "end"], 
                                                                           group_name10[7], 
                                                                           ifelse(theta_sk_theta$obs > cut_val[7, "start"] & theta_sk_theta$obs <= cut_val[7, "end"], 
                                                                                  group_name10[8], 
                                                                                  ifelse(theta_sk_theta$obs > cut_val[8, "start"] & theta_sk_theta$obs <= cut_val[8, "end"], 
                                                                                         group_name10[9],
                                                                                         ifelse(theta_sk_theta$obs > cut_val[9, "start"] & theta_sk_theta$obs <= cut_val[9, "end"], 
                                                                                                group_name10[10], 
                                                                                                ifelse(theta_sk_theta$obs > cut_val[10, "start"] & theta_sk_theta$obs <= cut_val[10, "end"], 
                                                                                                       group_name10[11], 
                                                                                                       group_name10[12])))))))))))

bias_abs_sk_group_theta10 <- aggregate(bias_obs_abs ~ selection + group10 + num_item, 
                                       data = theta_sk_theta, mean)
bias_abs_sk_group_theta10$type = "bias"
ggplot(bias_abs_sk_group_theta10, 
       aes(x=group10, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
  scale_x_discrete(breaks = c(group_name10), 
                   labels = c("<-2.5", cut_val[,1], ">2.5"))  + 
  facet_wrap(~num_item)

# uni 
theta_uni_theta$group10 <- ifelse(theta_uni_theta$obs  <= cut_val[1, "start"], 
                                  group_name10[1], 
                                  ifelse(theta_uni_theta$obs > cut_val[1, "start"] & theta_uni_theta$obs <= cut_val[1, "end"], 
                                         group_name10[2], 
                                         ifelse(theta_uni_theta$obs > cut_val[2, "start"] & theta_uni_theta$obs <= cut_val[2, "end"], 
                                                group_name10[3], 
                                                ifelse(theta_uni_theta$obs > cut_val[3, "start"] & theta_uni_theta$obs <= cut_val[3, "end"], 
                                                       group_name10[4], 
                                                       ifelse(theta_uni_theta$obs > cut_val[4, "start"] & theta_uni_theta$obs <= cut_val[4, "end"], 
                                                              group_name10[5],
                                                              ifelse(theta_uni_theta$obs > cut_val[5, "start"] & theta_uni_theta$obs <= cut_val[5, "end"], 
                                                                     group_name10[6], 
                                                                     ifelse(theta_uni_theta$obs > cut_val[6, "start"] & theta_uni_theta$obs <= cut_val[6, "end"], 
                                                                            group_name10[7], 
                                                                            ifelse(theta_uni_theta$obs > cut_val[7, "start"] & theta_uni_theta$obs <= cut_val[7, "end"], 
                                                                                   group_name10[8], 
                                                                                   ifelse(theta_uni_theta$obs > cut_val[8, "start"] & theta_uni_theta$obs <= cut_val[8, "end"], 
                                                                                          group_name10[9],
                                                                                          ifelse(theta_uni_theta$obs > cut_val[9, "start"] & theta_uni_theta$obs <= cut_val[9, "end"], 
                                                                                                 group_name10[10], 
                                                                                                 ifelse(theta_uni_theta$obs > cut_val[10, "start"] & theta_uni_theta$obs <= cut_val[10, "end"], 
                                                                                                        group_name10[11], 
                                                                                                        group_name10[12])))))))))))

bias_abs_uni_group_theta10 <- aggregate(bias_obs_abs ~ selection + group10 + num_item, 
                                        data = theta_uni_theta, mean)
bias_abs_uni_group_theta10$type = "bias"
ggplot(bias_abs_uni_group_theta10, 
       aes(x=group10, y = bias_obs_abs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), lwd =1.3) + 
  scale_x_discrete(breaks = c(group_name10), 
                   labels = c("<-2.5", cut_val[,1], ">2.5"))  + 
  facet_wrap(~num_item)

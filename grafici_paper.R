# Grafici paper -----

rm(list = ls())
library(gridExtra)
library(ggplot2)
library(TAM)
library(psych)
library(sn)
library(sirt)
library(DescTools)
set.seed(999)
# per dividere il tratto latente ---- 
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}

load("UNIsummary.RData")
load("SKsummary.RData")
load("NORMALsummary.RData")


# TIF
# potrei fare un grafico a tre pannelli per ogni numerosità di item. 
# quindi devo prendere da ogni dataset la numerosità10, 20 eccetera, 
# attaccarci un'etichetta e dvidere i panneli per ogni roba

tif_normal = data_info_theta_all
tif_sk = data_info_sk_theta_all
tif_uni = data_info_uni_theta_all
tif_normal$distributio = "Normal"
tif_sk$distributio = "Skewed"
tif_uni$distributio = "Uniform"
tif_all = rbind(tif_normal, tif_sk, tif_uni)
tif_all$num_item = gsub("number", "", tif_all$num_item)

tif_all = tif_all[!tif_all$sel %in% "guidedNEW", ]
tif_all$sel = plyr::revalue(
  tif_all$sel, 
  c(
    "cluster" = "UI", 
    "guided" = "EI", 
    "random" = "R", 
    "smart" = "T"
  )
)
tif_all$sel = factor(tif_all$sel, 
                     levels = c("T", "EI", 
                                "UI", 
                                "R"))
library(Cairo)
tif_all$num_item = paste0(tif_all$num_item, "-item STF")
gtif =ggplot(tif_all, 
       aes(x = theta, y = info, group = sel)) + geom_line(aes(linetype = sel), lwd = 0.5) + 
  theme_bw()  + xlim(-3.5,3.5) + 
  facet_grid(num_item~distributio,   
             switch = "y") +
  ylab(expression(paste("I(", theta, ")"))) + xlab(expression(theta)) +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_text(size = 30), 
        axis.title.y = element_text(size=30),
        axis.text = element_text(size = 22),
        strip.text.y = element_text(size = 16), 
        strip.text.x = element_text(size = 16), 
        legend.key.width = unit(4,"cm"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_linetype_manual(values = c("solid", "dotted","twodash", "longdash")) +
  guides(colour = guide_legend(override.aes = list(size=2)), 
         linetype = guide_legend(override.aes = list(size = 2)))+ 
  scale_y_continuous(position = "right")
gtif

ggsave(filename="C:/Users/huawei/Dropbox/Ottavia/ShortIRT/paper/Pmet/TIF.pdf", plot=gtif, device=cairo_pdf,
       width = 8.3, height = 11.7, units = "in", dpi = 600)

ggplot(tif_all, 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1) + 
  theme_classic() + theme(legend.position = "top") + xlim(-3.5,3.5) + 
  facet_grid(distributio ~ num_item, scale = "free",  labeller = label_parsed) +
  ylab(expression(paste("I(", theta, ")"))) + xlab(expression(theta)) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.x = element_text(size = 26), 
        axis.title.y = element_text(size=26),
        axis.text = element_text(22),
        legend.text = element_text(size = 26), 
        strip.text.y = element_text(size = 18), 
        strip.text.x = element_text(size = 18))

# Bias per gruppi di theta ------
bias_normal = normal_bias_groups_theta_mean[!normal_bias_groups_theta_mean$selection %in% "range_new", ] 
bias_normal$num_item = gsub("number", "", bias_normal$num_item)
bias_normal$group =  plyr::revalue(as.character(bias_normal$group), 
                                   c(
                                     "a" = "< -2.5", 
                                     "b" = "[-2.5,-1.25)", 
                                     "c" = "[-1.25,0)", 
                                     "d" = "[0,1.25)", 
                                     "e" = "[1.25,2.5)", 
                                     "f" = "> 2.5"
                                   ))

bias_normal$selection =  plyr::revalue(as.character(bias_normal$selection), 
                                       c(
                                         "cluster" = "UI", 
                                         "random" = "R", 
                                         "range" = "EI", 
                                         "smart" = "T"
                                       ))
bias_normal$selection = factor(bias_normal$selection, 
                               levels = c("T", "EI", 
                                          "UI", 
                                          "R"))

bias_normal$group =   factor(bias_normal$group, levels = c(c("< -2.5",  
                                                             "[-2.5,-1.25)", 
                                                             "[-1.25,0)", 
                                                             "[0,1.25)", 
                                                             "[1.25,2.5)", "> 2.5" )))
bias_normal$distribution = "Normal"
ggplot(bias_normal, 
       aes(x=group, y = bias_obs, group = selection
           )) + geom_line(aes(linetype = selection), 
                                           lwd =1)  +   theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 16, 
                                   angle = 45,  hjust=1), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 26), 
        strip.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 16), 
        legend.key.width = unit(4,"cm")) + 
  facet_wrap(~num_item) + scale_color_brewer(type = "div", palette = 1) +
  scale_linetype_manual(values = c("solid", "dotted","twodash", "longdash")) +
  guides(colour = guide_legend(override.aes = list(size=2)), 
         linetype = guide_legend(override.aes = list(size = 2)))


bias_sk = sk_bias_groups_theta_mean[!sk_bias_groups_theta_mean$selection %in% "range_new", ] 
bias_sk$num_item = gsub("number", "", bias_sk$num_item)
bias_sk$group =  plyr::revalue(as.character(bias_sk$group), 
                               c(
                                 "a" = "< -2.5", 
                                 "b" = "[-2.5,-1.25)", 
                                 "c" = "[-1.25,0)", 
                                 "d" = "[0,1.25)", 
                                 "e" = "[1.25,2.5)", 
                                 "f" = "> 2.5"
                               ))

bias_sk$selection =  plyr::revalue(as.character(bias_sk$selection), 
                                   c(
                                     "cluster" = "UI", 
                                     "random" = "R", 
                                     "range" = "EI", 
                                     "smart" = "T"
                                   ))
bias_sk$selection = factor(bias_sk$selection, 
                           levels = c("T", "EI", 
                                      "UI", 
                                      "R"))

bias_sk$group =   factor(bias_sk$group, levels = c("< -2.5",  
                                                   "[-2.5,-1.25)", 
                                                   "[-1.25,0)", 
                                                   "[0,1.25)", 
                                                   "[1.25,2.5)", "> 2.5" ))

ggplot(bias_sk, 
       aes(x=group, y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1)  + theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 16, 
                                   angle = 45,  hjust=1), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 26), 
        strip.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 16), 
        legend.key.width = unit(4,"cm")) + 
  facet_wrap(~num_item) + scale_color_brewer(type = "div", palette = 1) +
  scale_linetype_manual(values = c("solid", "dotted","twodash", "longdash")) +
  guides(colour = guide_legend(override.aes = list(size=2)), 
         linetype = guide_legend(override.aes = list(size = 2))) + ylim(-3,3)

bias_sk$distribution = "Skewed"

bias_prova = rbind(bias_normal, bias_sk)
levels(bias_prova$group)
table(as.character(bias_prova$group))

ggplot(bias_prova, 
       aes(x=group, y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1)  + theme_minimal() + 
  facet_grid(num_item ~ distribution)

bias_uni = uni_bias_groups_theta_mean[!uni_bias_groups_theta_mean$selection %in% "range_new", ] 
bias_uni$num_item = gsub("number", "", bias_uni$num_item)
bias_uni$group =  plyr::revalue(as.character(bias_uni$group), 
                                c(
                                  "a" = "< -2.5", 
                                  "b" = "[-2.5,-1.25)", 
                                  "c" = "[-1.25,0)", 
                                  "d" = "[0,1.25)", 
                                  "e" = "[1.25,2.5)", 
                                  "f" = "> 2.5"
                                ))

bias_uni$selection =  plyr::revalue(as.character(bias_uni$selection), 
                                    c(
                                      "cluster" = "UI", 
                                      "random" = "R", 
                                      "range" = "EI", 
                                      "smart" = "T"
                                    ))
bias_uni$selection = factor(bias_uni$selection, 
                            levels = c("T", "EI", 
                                       "UI", 
                                       "R"))

bias_uni$group =   factor(bias_uni$group, levels = c("< -2.5",  
                                                     "[-2.5,-1.25)", 
                                                     "[-1.25,0)", 
                                                     "[0,1.25)", 
                                                     "[1.25,2.5)", "> 2.5" ))

ggplot(bias_uni, 
       aes(x=group, y = bias_obs, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1)  + theme_minimal() +
  theme(legend.position = "bottom")  +
  theme(axis.text.x = element_text(size = 16, 
                                   angle = 45,  hjust=1), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 26), 
        strip.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 16), 
        legend.key.width = unit(4,"cm")) + 
  facet_wrap(~num_item) + scale_color_brewer(type = "div", palette = 1) +
  scale_linetype_manual(values = c("solid", "dotted","twodash", "longdash")) +
  guides(colour = guide_legend(override.aes = list(size=2)), 
         linetype = guide_legend(override.aes = list(size = 2)))  

bias_uni$distribution = "Uniform"
bias_all = rbind(bias_normal, bias_sk, 
                 bias_uni)
bias_all$num_item = paste(bias_all$num_item, "items")
bias_all$num_item = gsub(" ", "-", bias_all$num_item)
bias_all$num_item = gsub("items", "item STF", bias_all$num_item)

gbias =ggplot(bias_all, 
             aes(x = group, y = bias_obs, 
                 group = selection)) + geom_line(aes(linetype = selection), 
                                                 lwd = 0.5) + 
  theme_bw()  +
  facet_grid(num_item~distribution,   
             switch = "y") + ylab("BIAS") +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 20, face = "italic"),
        axis.text.x = element_text(28, angle = 45,  hjust=1),
        axis.text = element_text(size = 22),
        strip.text.y = element_text(size = 18), 
        strip.text.x = element_text(size = 18), 
        legend.key.width = unit(4,"cm"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_linetype_manual(values = c("solid", "dotted","twodash", "longdash")) +
  guides(colour = guide_legend(override.aes = list(size=2)), 
         linetype = guide_legend(override.aes = list(size = 2)))+ 
  scale_y_continuous(position = "right") 
gbias 

ggsave(filename="C:/Users/huawei/Dropbox/Ottavia/ShortIRT/paper/Pmet/bias.pdf", plot=gbias, device=cairo_pdf,
       width = 8.3, height = 11.7, units = "in", dpi = 600)
# RMSE Gruppi di theta -----


rmse_normal = normal_rmse_groups_theta_mean[!normal_rmse_groups_theta_mean$selection %in% "range_new", ] 
rmse_normal$num_item = gsub("number", "", rmse_normal$num_item)
rmse_normal$group =  plyr::revalue(as.character(rmse_normal$group), 
                                   c(
                                     "a" = "< -2.5", 
                                     "b" = "[-2.5,-1.25)", 
                                     "c" = "[-1.25,0)", 
                                     "d" = "[0,1.25)", 
                                     "e" = "[1.25,2.5)", 
                                     "f" = "> 2.5"
                                   ))

rmse_normal$selection =  plyr::revalue(as.character(rmse_normal$selection), 
                                       c(
                                         "cluster" = "UI", 
                                         "random" = "R", 
                                         "range" = "EI", 
                                         "smart" = "T"
                                       ))
rmse_normal$selection = factor(rmse_normal$selection, 
                               levels = c("T", "EI", 
                                          "UI", 
                                          "R"))

rmse_normal$group =   factor(rmse_normal$group, levels = c("< -2.5",  
                                                           "[-2.5,-1.25)", 
                                                           "[-1.25,0)", 
                                                           "[0,1.25)", 
                                                           "[1.25,2.5)", "> 2.5" ))

ggplot(rmse_normal, 
       aes(x=group, y = rmse, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1)  + theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 16, 
                                   angle = 45,  hjust=1), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 26), 
        strip.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 16), 
        legend.key.width = unit(4,"cm")) + 
  facet_wrap(~num_item) + scale_color_brewer(type = "div", palette = 1) +
  scale_linetype_manual(values = c("solid", "dotted","twodash", "longdash")) +
  guides(colour = guide_legend(override.aes = list(size=2)), 
         linetype = guide_legend(override.aes = list(size = 2)))
rmse_normal$distribution = "Normal"

rmse_sk = sk_rmse_groups_theta_mean[!sk_rmse_groups_theta_mean$selection %in% "range_new", ] 
rmse_sk$num_item = gsub("number", "", rmse_sk$num_item)
rmse_sk$group =  plyr::revalue(as.character(rmse_sk$group), 
                               c(
                                 "a" = "< -2.5", 
                                 "b" = "[-2.5,-1.25)", 
                                 "c" = "[-1.25,0)", 
                                 "d" = "[0,1.25)", 
                                 "e" = "[1.25,2.5)", 
                                 "f" = "> 2.5"
                               ))

rmse_sk$selection =  plyr::revalue(as.character(rmse_sk$selection), 
                                   c(
                                     "cluster" = "UI", 
                                     "random" = "R", 
                                     "range" = "EI", 
                                     "smart" = "T"
                                   ))
rmse_sk$selection = factor(rmse_sk$selection, 
                           levels = c("T", "EI", 
                                      "UI", 
                                      "R"))

rmse_sk$group =   factor(rmse_sk$group, levels = c("< -2.5",  
                                                   "[-2.5,-1.25)", 
                                                   "[-1.25,0)", 
                                                   "[0,1.25)", 
                                                   "[1.25,2.5)", "> 2.5" ))
rmse_sk$distribution = "Skewed"

ggplot(rmse_sk, 
       aes(x=group, y = rmse, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1)  + theme_minimal() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 16, 
                                   angle = 45,  hjust=1), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 26), 
        strip.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 16), 
        legend.key.width = unit(4,"cm")) + 
  facet_wrap(~num_item) + scale_color_brewer(type = "div", palette = 1) +
  scale_linetype_manual(values = c("solid", "dotted","twodash", "longdash")) +
  guides(colour = guide_legend(override.aes = list(size=2)), 
         linetype = guide_legend(override.aes = list(size = 2)))


rmse_uni = uni_rmse_groups_theta_mean[!uni_rmse_groups_theta_mean$selection %in% "range_new", ] 
rmse_uni$num_item = gsub("number", "", rmse_uni$num_item)
rmse_uni$group =  plyr::revalue(as.character(rmse_uni$group), 
                                c(
                                  "a" = "< -2.5", 
                                  "b" = "[-2.5,-1.25)", 
                                  "c" = "[-1.25,0)", 
                                  "d" = "[0,1.25)", 
                                  "e" = "[1.25,2.5)", 
                                  "f" = "> 2.5"
                                ))

rmse_uni$selection =  plyr::revalue(as.character(rmse_uni$selection), 
                                    c(
                                      "cluster" = "UI", 
                                      "random" = "R", 
                                      "range" = "EI", 
                                      "smart" = "T"
                                    ))
rmse_uni$selection = factor(rmse_uni$selection, 
                            levels = c("T", "EI", 
                                       "UI", 
                                       "R"))

rmse_uni$group =   factor(rmse_uni$group, levels = c("< -2.5",  
                                                     "[-2.5,-1.25)", 
                                                     "[-1.25,0)", 
                                                     "[0,1.25)", 
                                                     "[1.25,2.5)", "> 2.5" ))
rmse_uni$distribution = "Uniform"

ggplot(rmse_uni, 
       aes(x=group, y = rmse, group = selection, 
           color = selection)) + geom_line(aes(linetype = selection), 
                                           lwd =1)  + theme_minimal() +
  theme(legend.position = "bottom")  + ylab("RMSE") +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 20, face = "italic"),
        axis.text.x = element_text(28, angle = 45,  hjust=1),
        axis.text.y = element_text(28),
        legend.text = element_text(size = 26), 
        strip.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 16), 
        legend.key.width = unit(4,"cm")) + 
  facet_wrap(~num_item) + scale_color_brewer(type = "div", palette = 1) +
  scale_linetype_manual(values = c("solid", "dotted","twodash", "longdash")) +
  guides(colour = guide_legend(override.aes = list(size=2)), 
         linetype = guide_legend(override.aes = list(size = 2))) 

rmse_all = rbind(rmse_normal, 
                 rmse_sk, 
                 rmse_uni)
rmse_all$num_item = paste0(rmse_all$num_item, "-item STF")
grmse =ggplot(rmse_all, 
              aes(x = group, y = rmse, 
                  group = selection)) + geom_line(aes(linetype = selection), 
                                                  lwd = 0.5) + 
  theme_bw()  +
  facet_grid(num_item~distribution,   
             switch = "y") + ylab("RMSE") +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 20, face = "italic"),
        axis.text.x = element_text(28, angle = 45,  hjust=1),
        axis.text = element_text(size = 22),
        legend.text = element_text(size = 26), 
        strip.text.y = element_text(size = 18), 
        strip.text.x = element_text(size = 18), 
        legend.key.width = unit(4,"cm"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_linetype_manual(values = c("solid", "dotted","twodash", "longdash")) +
  guides(colour = guide_legend(override.aes = list(size=2)), 
         linetype = guide_legend(override.aes = list(size = 2)))+ 
  scale_y_continuous(position = "right") 
grmse 


ggsave(filename="C:/Users/huawei/Dropbox/Ottavia/ShortIRT/paper/Pmet/rmse.pdf", plot=grmse, device=cairo_pdf,
       width = 8.3, height = 11.7, units = "in", dpi = 600)

# distribuzione normale con intervalli esempio ------
# esempio distribuzione -----
set.seed(999)
x <- seq(-4, 4, length=10000)
hx <- dnorm(x)
plot(hx)
p = x *hx

data = data.frame(x = x, y = hx)

ggplot(data, 
       aes(x = x, y = y)) + geom_line()

cluster = kmeans(matrix(p, ncol =1), centers = 10)
clusters = data.frame(cluster = cluster$centers[,1][order(cluster$centers[,1])], 
                      end = dnorm(cluster$centers[,1][order(cluster$centers[,1])], 
                                  mean(p), 
                                  sd(p)), 
                      distribution = "Normale")
num_item = 10
# divido gli intervalli del tratto latente
ranges <- NULL
groups <- NULL
cut_value <- NULL

for (i in 1:(length(num_item))) {
  ranges <- seq(min(p), max(p),
                length =num_item[i])
  groups <- cut(ranges, num_item[i], include.lowest = TRUE)
  cut_value <- cut_borders(groups)
  cut_value$mean_theta <- rowMeans(cut_value)
}

group_norm = data.frame(groups = cut_value$mean_theta, 
                        end = dnorm(cut_value$mean_theta, 
                                    mean(p), 
                                    sd(p)), 
                        distribution = "Normale")


ggplot(data,
       aes(x= x, y =y)) + geom_line() + theme_minimal() +
  geom_segment(data = clusters,
               aes(x =cluster, y = 0.02,
                   xend = cluster,
                   yend = -0.05,
                   linetype = "Cluster")) +
  geom_segment(aes(x=clusters$cluster[1],xend=clusters$cluster[nrow(clusters)],
                   y=-0.03,yend=-0.03)) +
  geom_segment(data = group_norm,
               aes(x =groups, y =-0.04,
                   xend = groups,
                   yend = -0.10,
                   linetype = "Guided")) +
  geom_segment(aes(x=group_norm$groups[1],xend=group_norm$groups[nrow(group_norm)],
                   y=-0.07,yend=-0.07, linetype = "Guided"))


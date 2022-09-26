## grafici presentazione -------
library(ggplot2)
library(sjPlot)
library(corrplot)
library(nnet)
library(formatR)
library(kableExtra)
library(shiny)
library(Cairo)
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}

i_info <- function(b, a=1,c=0, theta = seq(-5,5,length.out=1000)){
  
  
  P <- NULL 
  Q <- NULL
  Ii <- NULL
  
  for(i in 1:1000){
    P[i] <- 1/(1+ exp (-a*(theta[i] - b)))
    Q[i]= 1-P[i]
    Ii[i] =(a*Q[i]*(P[i]-c)^2)/(P[i]*((1-c)^2)) # (3PL)
  }
  return(Ii)
}


# Function to get all item information
item_info <- function(b,a=1){
  item <- NULL
  for(i in 1:length(b)){
    item[[i]] <- i_info(b[i],a[i])
  }
  return(item)
}
IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  y[is.na(y)] = 1
  return(y)
}
difficulty <- c(0,0, 0)
disc <- c(0.20, .70, 1.90)
theta <- seq(-7, 7, .001)
b <- difficulty
a <- disc

c <- item_info(b,a)


Theta <- matrix(seq(-4,4, length.out=1000))
check <- data.frame(Theta,
                    item_info = c[[1]],
                    item_info2 = c[[2]],
                    item_info3 = c[[3]])




d1 <- do.call('cbind',c)
sum_info2 <- rowSums(d1)
set.seed(999)
N <- 1000 # number of persons
true_theta = rnorm(N, mean=0, sd=1)

cluster = kmeans(matrix(true_theta, ncol =1), centers = 5)
data = data.frame(value = true_theta, lab = rep("x", length(true_theta)))
# per i puntini -
clusters = data.frame(var = cluster$centers[,1][order(cluster$centers[,1])])
clusters$distribution = "Normal"
clusters$y = 0.00
clusters$type = "UIP"

data$distribution = "Normal"

num_item = 5
# divido gli intervalli del tratto latente
ranges <- NULL
groups <- NULL
cut_value <- NULL

for (i in 1:(length(num_item))) {
  ranges <- seq(min(true_theta), max(true_theta),
                length =num_item[i])
  groups <- cut(ranges, num_item[i], include.lowest = TRUE)
  cut_value <- cut_borders(groups)
  cut_value$mean_theta <- rowMeans(cut_value)
}

# per i puntini -
group_norm = data.frame(var = cut_value$mean_theta)
group_norm$distribution = "Normal"
group_norm$y = 0.00
group_norm$type = "EIP"




# Skewed ----
true_theta_sk = (rbeta(1000, 1, 100)*100) - 3

data_sk = data.frame(value = true_theta_sk, 
                     lab = rep("x", length(true_theta_sk)))
cluster_sk = kmeans(matrix(true_theta_sk, ncol =1), centers = 5)

# per i puntini -
clusters_sk = data.frame(var = cluster_sk$centers[,1][order(cluster_sk$centers[,1])])
clusters_sk$distribution = "Skewed"
clusters_sk$y = 0.00
clusters_sk$type = "UIP"

ranges <- NULL
groups <- NULL
cut_value_sk <- NULL

for (i in 1:(length(num_item))) {
  ranges <- seq(min(true_theta_sk), max(true_theta_sk),
                length =num_item[i])
  groups <- cut(ranges, num_item[i], include.lowest = TRUE)
  cut_value_sk <- cut_borders(groups)
  cut_value_sk$mean_theta <- rowMeans(cut_value_sk)
}

group_sk = data.frame(groups = cut_value_sk$mean_theta, 
                      end = dnorm(cut_value_sk$mean_theta, 
                                  mean(true_theta_sk), 
                                  sd(true_theta_sk)), 
                      distribution = "Skewed")

data_sk$distribution = "Skewed"
# per i puntini -
group_sk = data.frame(var = cut_value_sk$mean_theta)
group_sk$distribution = "Skewed"
group_sk$y = 0.00
group_sk$type = "EIP"



set.seed(999)
true_theta_uni <- c(runif(1000, min = -3, max = 3))
cluster_uni = kmeans(matrix(true_theta_uni, ncol =1), centers = 5)
data_uni = data.frame(value = true_theta_uni, lab = rep("x", length(true_theta_uni)))
clusters_uni = data.frame(cluster = cluster_uni$centers[,1][order(cluster_uni$centers[,1])], 
                          end = dnorm(cluster_uni$centers[,1][order(cluster_uni$centers[,1])], 
                                      mean(true_theta_uni), 
                                      sd(true_theta_uni)), 
                          distribution = "Uniform")

# per i puntini -
clusters_uni = data.frame(var = cluster_uni$centers[,1][order(cluster_uni$centers[,1])])
clusters_uni$distribution = "Uniform"
clusters_uni$y = 0.00
clusters_uni$type = "UIP"

ranges <- NULL
groups <- NULL
cut_value_uni <- NULL

for (i in 1:(length(num_item))) {
  ranges <- seq(min(true_theta_uni), max(true_theta_uni),
                length =num_item[i])
  groups <- cut(ranges, num_item[i], include.lowest = TRUE)
  cut_value_uni <- cut_borders(groups)
  cut_value_uni$mean_theta <- rowMeans(cut_value_uni)
}

group_uni = data.frame(groups = cut_value_uni$mean_theta, 
                       end = dnorm(cut_value_uni$mean_theta, 
                                   mean(true_theta_uni), 
                                   sd(true_theta_uni)), 
                       distribution = "Uniform")
data_uni$distribution = "Uniform"


# per i puntini -
group_uni = data.frame(var = cut_value_uni$mean_theta)
group_uni$distribution = "Uniform"
group_uni$y = 0.00
group_uni$type = "EIP"


all_data = rbind(data, data_sk, data_uni)
all_clusters = rbind(clusters, clusters_sk, 
                     clusters_uni)
all_groups = rbind(group_norm, group_sk, group_uni)

prova = rbind(all_groups, all_clusters)

uip = rgb(199, 124, 255, maxColorValue = 255)
eip = rgb(121, 172, 0, maxColorValue = 255)

ggplot(all_data, 
       aes(x= value)) + geom_density() +  geom_point(data = prova, 
                                                     aes(x = var, y =y, 
                                                         shape = type, 
                                                         color = type), 
                                                     size = 7,stroke = 1.5)+
  ylim(0,0.7) + geom_hline(yintercept = 0.0) +
  scale_shape_manual(values = c(0,2)) + 
  scale_color_manual(values = c(uip, eip)) +
  facet_wrap(~distribution) +  theme_classic() + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.line = element_blank(), 
        legend.text = element_text(size = 28), 
        strip.text.x = element_text(size = 18)) +   xlab(expression(theta))


# distribuzione teorica -----

set.seed(999)
x <- seq(-3, 3, length=10000)
hx <- dnorm(x)
plot(hx)
abline(v = qnorm(.33))
p = x *hx

data = data.frame(x = x, y = hx)
library(ggplot2)

a = cut(seq(min(data$x), max(data$x), length = 3), 3)
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}

cut_value = cut_borders(a)
val_1 = rowMeans(cut_value)

ui = data.frame(cluster = c(qnorm(.30), qnorm(.60), qnorm(.90)))
ui$type = "UIP"
colnames(ui)[1] = "val"
groups = data.frame(group = val_1)
colnames(groups)[1] = "val"
groups$type = "EIP"
insieme = rbind(ui, groups)
uip = rgb(199, 124, 255, maxColorValue = 255)
eip = rgb(121, 172, 0, maxColorValue = 255)
# eip
ggplot(data, 
       aes(x = x, y = y)) + geom_line() + geom_point(data = groups, 
                                                     aes(x = val, y = 0,
                                                         shape = type, 
                                                         color = type), 
                                                     stroke = 2, size = 9) + 
  geom_point(data = groups, 
             aes(x =val, y = 0,
                 shape = type, 
                 color = type), 
             stroke = 1.3, size = 9) +
  ylim(0,0.4) + geom_hline(yintercept = 0.0) +
  scale_shape_manual(values = c(0,2)) + 
  scale_color_manual(values = c(eip)) + theme_classic() + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.line = element_blank(), 
        legend.text = element_text(size = 28), 
        strip.text.x = element_text(size = 18)) +   xlab(expression(theta))

# insieme
ggplot(data, 
       aes(x = x, y = y)) + geom_line() + geom_point(data = insieme, 
                                                     aes(x = val, y = 0,
                                                         shape = type, 
                                                         color = type), 
                                                     stroke = 2, size = 9) + 
  geom_point(data = insieme, 
             aes(x =val, y = 0,
                 shape = type, 
                 color = type), 
             stroke = 1.3, size = 9) +
  ylim(0,0.4) + geom_hline(yintercept = 0.0) +
  scale_shape_manual(values = c(0,2)) + 
  scale_color_manual(values = c(eip, uip)) + theme_classic() + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.line = element_blank(), 
        legend.text = element_text(size = 28), 
        strip.text.x = element_text(size = 18)) +   xlab(expression(theta))

# without legend
ggplot(data, 
       aes(x = x, y = y)) + geom_line() + geom_point(data = groups, 
                                                     aes(x = val, y = 0,
                                                         shape = type, 
                                                         color = type), 
                                                     stroke = 2, size = 9) + 
  geom_point(data = groups, 
             aes(x =val, y = 0,
                 shape = type, 
                 color = type), 
             stroke = 1.3, size = 9) +
  ylim(0,0.4) + geom_hline(yintercept = 0.0) +
  scale_shape_manual(values = c(0,2)) + 
  scale_color_manual(values = c(eip)) + theme_classic() + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.line = element_blank(), 
        legend.text = element_text(size = 28), 
        strip.text.x = element_text(size = 18)) +   xlab(expression(theta))

# insieme
ggplot(data, 
       aes(x = x, y = y)) + geom_line() + geom_point(data = insieme, 
                                                     aes(x = val, y = 0,
                                                         shape = type, 
                                                         color = type), 
                                                     stroke = 2, size = 9) + 
  geom_point(data = insieme, 
             aes(x =val, y = 0,
                 shape = type, 
                 color = type), 
             stroke = 1.3, size = 9) +
  ylim(0,0.4) + geom_hline(yintercept = 0.0) +
  scale_shape_manual(values = c(0,2)) + 
  scale_color_manual(values = c(eip, uip)) + theme_classic() + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.line = element_blank(), 
        legend.text = element_text(size = 28), 
        strip.text.x = element_text(size = 18)) +   xlab(expression(theta))


## results -----

rm(list = ls())
load("D:/irt/UNIsummary.RData")
load("D:/irt/SKsummary.RData")
load("D:/irt/NORMALsummary.RData")
set.seed(666)
library(ggplot2)

all_small_norm = all_data_theta[!all_data_theta$selection %in%"guidedTheta", ]
all_small_norm$distribution = "Normal"
all_small_norm$selection  = plyr::revalue(all_small_norm$selection, 
                                          c("cluster_theta" = "UIP", 
                                            "guidedNew" = "EIP", 
                                            "random" = "RP", 
                                            "smart_theta" = "BP"))
all_small_sk = all_data_sk_theta[!all_data_sk_theta$selection %in%"rangeSK", ]
all_small_sk$selection  = plyr::revalue(all_small_sk$selection, 
                                        c("clustersk" = "UIP", 
                                          "range_newSK" = "EIP", 
                                          "random" = "RP", 
                                          "smart" = "BP"))
all_small_sk$distribution = "Skewed" 
all_small_uni = all_data_uni_theta[!all_data_uni_theta$selection %in%"rangeuni", ]
all_small_uni$selection  = plyr::revalue(all_small_uni$selection, 
                                         c("clusteruni" = "UIP", 
                                           "range_newuni" = "EIP", 
                                           "random" = "RP", 
                                           "smart" = "BP"))
all_small_uni$num_item = gsub("number", "", all_small_uni$num_item)
all_small_uni$distribution = "Uniform" 

all_small = rbind(all_small_norm, all_small_sk, all_small_uni)
all_small$num_item = gsub("number", "", all_small$num_item)
dummy = data.frame(all_small[all_small$num_item %in% "all", ])

colors = c("#f87269", "#79AC00", "#C77CFF", "#00BBC0")

all_small$num_item = factor(all_small$num_item)
all_small$selection = factor(all_small$selection, 
                             levels = c("BP", "EIP", "UIP", "RP"))
levels(all_small$selection)
ginfo = ggplot(all_small[!all_small$num_item %in%"all", ],
       aes(x=as.factor(item_temp), y=mean_info,
           group=selection, color=selection)) + theme_minimal() +
  geom_line(aes(linetype = selection), lwd = 1.5) + scale_color_manual(values = colors) + 
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info),
                width=.2,
                position=position_dodge(0.05)) +
  theme(axis.text.x = element_text(size = 20), 
        legend.position = "top", 
        legend.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 32),  
        legend.text = element_text(size = 34), 
        strip.text.x = element_text(size = 28), 
        legend.key.size = unit(4,"line")) + 
  scale_x_discrete(labels =  unique(all_small[!all_small$num_item %in%"all", "num_item"])) +
  geom_hline(data = dummy, aes(yintercept = mean_info)) + 
  facet_wrap(~distribution) + ylab(expression(paste("I(", theta, ")")))
ginfo


# information 10 item -----

temp_graph_normal_theta_all10 <- data_info_theta_all[data_info_theta_all$num_item %in% "number10", ]
temp_graph_normal_theta_all10$distribution = "Normal"

temp_graph_sk_theta_all10 <- data_info_sk_theta_all[data_info_sk_theta_all$num_item %in% "number10", ]
temp_graph_sk_theta_all10$distribution = "Skewed"

temp_graph_uni_theta_all10 <- data_info_uni_theta_all[data_info_uni_theta_all$num_item %in% "number10", ]
temp_graph_uni_theta_all10$distribution = "Uniform"

graph_tif10 = rbind(temp_graph_normal_theta_all10, 
                    temp_graph_sk_theta_all10, 
                    temp_graph_uni_theta_all10)
graph_tif10 = graph_tif10[!graph_tif10$sel %in% "guided", ]

graph_tif10$sel  = plyr::revalue(graph_tif10$sel, 
                                 c("cluster" = "UIP", 
                                   "guidedNEW" = "EIP", 
                                   "random" = "RP", 
                                   "smart" = "BP"))
graph_tif10$sel = factor(graph_tif10$sel, 
                                             levels = c("BP", "EIP", "UIP", "RP"))
levels(graph_tif10$sel)
ggplot(graph_tif10, 
       aes(x = theta, y = info, group = sel, 
           col = sel)) + geom_line(aes(linetype = sel), lwd = 1.3) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16) ,
        legend.position = "top", 
        legend.title = element_blank(), 
        axis.title = element_text(size = 32),  
        legend.text = element_text(size = 34), 
        strip.text.x = element_text(size = 28), 
        legend.key.size = unit(4,"line")) +xlab(expression(theta)) + scale_color_manual(values = colors) +
  facet_wrap(~distribution) + ylab(expression(paste("I(", theta, ")")))

# Bias -----
sk_groups = sk_bias_groups_theta[sk_bias_groups_theta$num_item %in% "number10", ]
uni_groups = uni_bias_groups_theta[uni_bias_groups_theta$num_item %in% "number10", ]
normal_groups = normal_bias_groups_theta[normal_bias_groups_theta$num_item %in% "number10", ]

sk_groups = sk_groups[!sk_groups$selection %in% "range_new", ]
sk_groups$distr = "Skewed"
uni_groups = uni_groups[!uni_groups$selection %in% "range_new", ]
uni_groups$distr = "Uniform"
norm_groups = normal_groups[!normal_groups$selection %in% "range_new", ]
norm_groups$distr = "Normal"

bias_groups = rbind(uni_groups, sk_groups, norm_groups)
bias_groups$group = plyr::revalue(bias_groups$group,
                                  c("a" = "< -2.5",
                                    "b" = "[-2.5,-1.25]",
                                    "c" = "(-1.25,0]",
                                    "d" = "(0,1.25]",
                                    "e" = "(1.25,2.5]",
                                    "f" = "> 2.5"))
bias_groups$group = factor(bias_groups$group, levels = c("< -2.5",  "[-2.5,-1.25]", "(-1.25,0]", "(0,1.25]", "(1.25,2.5]", "> 2.5" ))
bias_groups$selection = plyr::revalue(bias_groups$selection,
                                      c("cluster" = "UIP",
                                        "random" = "RP",
                                        "range" = "EIP",
                                        "smart" = "BP"))
bias_groups$selection = factor(bias_groups$selection, 
                               levels = c("BP", "EIP", "UIP", "RP"))
ggplot(bias_groups,
       aes(x=group, y = bias_obs, group = selection,
           color = selection)) + geom_line(aes(linetype = selection),
                                           lwd =1.3) + theme_minimal() +
  theme(legend.position = "top") + xlab(expression(theta)) +
  scale_color_manual(values = colors) + 
  ylab(expression(theta - hat(theta))) +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=1),
        legend.title = element_blank(),
        axis.title = element_text(size = 32),  
        legend.text = element_text(size = 34), 
        strip.text.x = element_text(size = 28), 
        axis.title.y = element_text(size = 32, face = "italic"), 
        legend.key.size = unit(4,"line")) + 
  facet_grid(~distr) + geom_hline(aes(yintercept= 0), lty = 3, lwd = 0.5)


library(ggplot2)

N <- 1000 # number of persons
true_theta = rnorm(N, mean=0, sd=1)


cluster = kmeans(matrix(seq(-3,3, length.out = 1000), ncol =1), centers = 10)
data = data.frame(value = true_theta, lab = rep("x", length(true_theta)))
clusters = data.frame(cluster = cluster$centers[,1][order(cluster$centers[,1])], 
                      end = dnorm(cluster$centers[,1][order(cluster$centers[,1])], 
                                   mean(true_theta), 
                                   sd(true_theta)))
groups = (seq(-3,3, length.out = 10))
group = data.frame(groups = groups, 
                      end = dnorm(groups, 
                                  mean(true_theta), 
                                  sd(true_theta)))
ggplot(data, 
       aes(x= true_theta)) + geom_density() +  ylim(-0.25, 0.5) +
  geom_segment(data = clusters, 
               aes(x =cluster, y =-0.02, 
                   xend = cluster, 
                   yend = -0.05)) + 
  geom_segment(aes(x=clusters$cluster[1],xend=clusters$cluster[nrow(clusters)],
                   y=-0.03,yend=-0.03)) + 
  geom_segment(data = group, 
               aes(x =groups, y =-0.06, 
                   xend = groups, 
                   yend = -0.08)) + 
  geom_segment(aes(x=group$groups[1],xend=group$groups[nrow(group)],
                   y=-0.07,yend=-0.07))
  

# skewness
set.seed(666)
library(PearsonDS)
moments <- c(mean = -2,variance = 1,skewness = 0.71, kurtosis = 4)
true_theta_sk <- c(rpearson(1000, moments = moments))
true_theta_sk = (true_theta_sk*0.5396) -.9227

data_sk = data.frame(value = true_theta_sk, lab = rep("x", length(true_theta_sk)))
clusters_sk = data.frame(cluster = cluster$centers[,1][order(cluster$centers[,1])], 
                      end = dnorm(cluster$centers[,1][order(cluster$centers[,1])], 
                                  mean(true_theta_sk), 
                                  sd(true_theta_sk)))
groups = (seq(-3,3, length.out = 10))
group_sk = data.frame(groups = groups, 
                   end = dnorm(groups, 
                               mean(true_theta_sk), 
                               sd(true_theta_sk)))

ggplot(data_sk, 
       aes(x= true_theta_sk)) + geom_density() +  ylim(-0.25, 1) +
  geom_segment(data = clusters_sk, 
               aes(x =cluster, y =-0.02, 
                   xend = cluster, 
                   yend = -0.05)) + 
  geom_segment(aes(x=clusters_sk$cluster[1],xend=clusters_sk$cluster[nrow(clusters_sk)],
                   y=-0.03,yend=-0.03)) + 
  geom_segment(data = group_sk, 
               aes(x =groups, y =-0.06, 
                   xend = groups, 
                   yend = -0.08)) + 
  geom_segment(aes(x=group_sk$groups[1],xend=group_sk$groups[nrow(group_sk)],
                   y=-0.07,yend=-0.07))


## uniform
set.seed(666)
true_theta_uni <- c(runif(1000, min = -3, max = 3))

data_uni = data.frame(value = true_theta_uni, lab = rep("x", length(true_theta_uni)))
clusters_uni = data.frame(cluster = cluster$centers[,1][order(cluster$centers[,1])], 
                          end = dnorm(cluster$centers[,1][order(cluster$centers[,1])], 
                                      mean(true_theta_uni), 
                                      sd(true_theta_uni)))
groups = (seq(-3,3, length.out = 10))
group_uni = data.frame(groups = groups, 
                       end = dnorm(groups, 
                                   mean(true_theta_uni), 
                                   sd(true_theta_uni)))

ggplot(data_uni, 
       aes(x= true_theta_uni)) + geom_density() +  ylim(-0.25, 1) +
  geom_segment(data = clusters_uni, 
               aes(x =cluster, y =-0.02, 
                   xend = cluster, 
                   yend = -0.05)) + 
  geom_segment(aes(x=clusters_uni$cluster[1],xend=clusters_uni$cluster[nrow(clusters_uni)],
                   y=-0.03,yend=-0.03)) + 
  geom_segment(data = group_uni, 
               aes(x =groups, y =-0.06, 
                   xend = groups, 
                   yend = -0.08)) + 
  geom_segment(aes(x=group_uni$groups[1],xend=group_uni$groups[nrow(group_uni)],
                   y=-0.07,yend=-0.07))

# cluster che non funziona ----
set.seed(666)
theta_mat <- matrix(seq(-3,3, length = 1000), ncol = 1)
cluster <- list() 
num_clusters = seq(10, 90, by = 10)
for (i in 1:length(num_clusters)) {
  cluster[[i]] <- kmeans(theta_mat, 
                         centers = num_clusters[i])
}

cluster[[6]] # NON CONVERGE
cluster[[7]] # NON CONVERGE


set.seed(666)
theta_mat <- matrix(seq(-3,3, length = 1000), ncol = 1)
cluster <- list() 
num_clusters = seq(10, 90, by = 10)
for (i in 1:length(num_clusters)) {
  cluster[[i]] <- kmeans(theta_mat, 
                         centers = num_clusters[i], 
                         iter.max = 20)
}

cluster[[6]] # non coneverge
cluster[[7]] # non coneverge



set.seed(666)
theta_mat <- matrix(seq(-3,3, length = 1000), ncol = 1)
cluster <- list() 
num_clusters = seq(10, 90, by = 10)
for (i in 1:length(num_clusters)) {
  cluster[[i]] <- kmeans(theta_mat, 
                         centers = num_clusters[i], 
                         iter.max = 200)
}

cluster[[6]] # non converge
cluster[[7]] # non converge

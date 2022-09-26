# R commented script -----
# This is a basic R script for reproducing the results in the manuscript entitled
# "Less is more: Less is more: New Item Response Theory approaches for shortening tests"
# To change the distribution of the theta of the respondents, the corresponding 
# R code needs to be commentated
# As it is, the code simulates the data for 1000 persons with normally 
# distributed thetas and 100 items with uniform difficulty parameters from -3 to +3
# and uniform discrimination parameters between 0.04 and 2
library(TAM) # estimate the 2PL model
library(sirt) # simulate data
library(ggplot2) # plot the results
rm(list = ls())
set.seed(999)
N = 1000 # number of persons
b = runif(100, -3,3) # item difficulty parameters
a = c(runif(100, 0.4, 2)) # item discrimination parameters (uniformly distributed)

## Normal distribution of thetas ----
set.seed(999)
true_theta = rnorm(N, mean=0, sd=1)

## Skewed distribution of thetas -----
# set.seed(999)
# true_theta = (rbeta(1000, 1, 100)*100) - 3

## Uniform distribution of thetas ----
# set.seed(999)
# true_theta <- c(runif(1000, min = -3, max = 3))

## generate data and estimate 2PL model on the full-length test
## simulate observed responses 
data = sirt::sim.raschtype( true_theta, b=b, 
                            fixed.a = a)
# matrix of difficulty parameters 
diff_true <- matrix(cbind(1:length(b), 
                          b), 
                    ncol = 2)

# array of discrimination parameters 
discr_true = array(c(rep(0, length(a)), a), 
                   c(length(a),2,1), 
                   dimnames = list(paste0("I", 1:length(a)), 
                                   c("Cat0", "Cat1"), 
                                   "Dim01"))
# model estimation with all items 
m2pl = tam.mml(resp=data, xsi.fixed = diff_true, 
               B = discr_true)
# TIF 
info_start <- mean(IRT.informationCurves(m2pl, 
                                         theta = seq(-3,3, 
                                                     length = 1000))$test_info_curve)


# Benchmark procedure (BP) ----

lab_item <- 1:ncol(data) # item labels
num_item <- seq(10, 90, 
                by = 20)  # number of items included in the short test form(s). 
# Thiscan be either a vector of values or a single value. In the first case, 
# short forms composed of different numbers of items will be obtained, in the 
# second one, only one short form will be obtained with a number of items equal to 
# that indicated in the vector "num_item". The number of items in "num_item" must
# be lower than the number of items in the full-length test and equal or higher 
# than 2

# compute IIF for each item considering the latent trait 

data_info_bp <- data.frame(items = 1:(ncol(data)), 
                           info = numeric((ncol(data))))

for (i in 1:nrow(data_info_bp)) {
  data_info_bp[i, "info"] <- mean(IRT.informationCurves(m2pl, 
                                                        theta = true_theta, 
                                                        iIndex = lab_item[i])$info_curves_item)
  
}

# given the number(s) of items in num_item, the items with the highest IIFs 
# are selected. 
filter <- list()
data_temp <- list()
for (i in 1:length((num_item))) {
  filter[[i]] <- data_info_bp[which(data_info_bp$info == max(data_info_bp$info)), ]
  for (j in 1:(num_item[i]-1)) {
    data_temp[[j]] <- data_info_bp[!data_info_bp$items %in% filter[[i]]$items, ]
    filter[[i]] <- rbind(filter[[i]], 
                         data_temp[[j]][which(data_temp[[j]]$info == max(data_temp[[j]]$info)), ])
  }
  names(filter)[[i]] <- paste("number", num_item[i], sep = "")
  
}

# given the number(s) of items in num_item, filter out the selected ones from the 
# full-length test, estimate the model on the resulting short form(s), and 
# compute the IIF and TIF
# (bp = benchmark procedure)
out_bp <- list()
model_out_bp <- list()
info_out_bp <- list()

for (i in 1:length(filter)) {
  out_bp[[i]] <- data[, filter[[i]]$items]
  model_out_bp[[i]] <- tam.mml(out_bp[[i]], 
                                     xsi.fixed = cbind(1:ncol(out_bp[[i]]), 
                                                       diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                 colnames(out_bp[[i]]))), 2]), 
                                     B= array(c(rep(0, ncol(out_bp[[i]])), 
                                                discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                colnames(out_bp[[i]])))]), 
                                              c(ncol(out_bp[[i]]),2,1), 
                                              dimnames = list(colnames(out_bp[[i]]), 
                                                              c("Cat0", "Cat1"), 
                                                              "Dim01")))
  
  info_out_bp[[i]] <- IRT.informationCurves(model_out_bp[[i]], 
                                                  theta = seq(-3, 3, length = 1000))
  names(info_out_bp)[[i]] <- names(filter)[[i]]
}

# Summary 
info_summary_bp <- NULL
temp <- NULL
for(i in 1:length(info_out_bp)) {
  temp <- data.frame(info_test = mean(info_out_bp[[i]]$test_info_curve), 
                     
                     
                     bp_name = names(info_out_bp)[[i]], 
                     item = paste(colnames(out_bp[[i]]), collapse = ","))
  
  info_summary_bp <- rbind(info_summary_bp, 
                                 temp)
}

info_summary_bp$rel <- 1 - (1/sqrt(info_summary_bp$info_test))^2

info_summary_bp <-  rbind(info_summary_bp, 
                                data.frame(info_test = (info_start),
                                           bp_name = "all", 
                                           item = "all", 
                                           rel = 1 - (1/sqrt(info_start))^2))
info_summary_bp$selection <- "BP"

# plot information 
ggplot(info_summary_bp, 
       aes(x = bp_name, y = info_test)) + geom_point()


# Equal Intervals Procedure (EIP) ----
# cut borders for defining the intervals 
cut_borders <- function(x){
  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  
  start <- as.numeric(gsub(pattern,"\\2", x))
  end <- as.numeric(gsub(pattern,"\\3", x))
  
  data.frame(start, end)
}

lab_item <- 1:ncol(data) # item labels
num_item <- seq(10, 90, by = 20)  # number of items included in the short test form(s). This 
# can be either a vector of values or a single value. In the first case, 
# short forms composed of different numbers of items will be obtained, in the 
# second one, only one short form will be obtained with a number of items equal to 
# that indicated in the vector "num_item". The number of items in "num_item" must
# be lower than the number of items in the full-length test and equal or higher 
# than 2

# segment the latent trait in equal intervals and define the theta targets
intervals <- NULL
groups <- NULL
cut_value <- list()

for (i in 1:(length(num_item))) {
  intervals <- seq(min(true_theta), max(true_theta),
                length =num_item[i])
  groups <- cut(intervals, num_item[i], include.lowest = TRUE)
  cut_value[[i]] <- cut_borders(groups)
  cut_value[[i]]$mean_theta <- rowMeans(cut_value[[i]]) # define theta targets
}

# Compute IIF for each theta target
info_test <- NULL
temp <- list()
value <- list()
temp_data <- NULL
info_data <- NULL

for (j in 1:length(cut_value)) {
  value[[j]] <- cut_value[[j]][1:nrow(cut_value[[j]]), ]
  
  for(i in 1:length(lab_item)) {
    for(m in 1:nrow(value[[j]])) {
      
      temp_data <- data.frame(theta_target = IRT.informationCurves(m2pl,
                                                                   theta = value[[j]][m,
                                                                                      "mean_theta"],
                                                                   iIndex = lab_item[i])$theta,
                              test_info = mean(IRT.informationCurves(m2pl,
                                                                     theta = value[[j]][m,
                                                                                        "mean_theta"],
                                                                     iIndex = lab_item[i])$test_info_curve),
                              item_info = mean(colSums(IRT.informationCurves(m2pl,
                                                                             theta = value[[j]][m,
                                                                                                "mean_theta"],
                                                                             iIndex = lab_item[i])$info_curves_item)),
                              item = lab_item[i],
                              num_item = paste("number", nrow(value[[j]]), sep = ""))
      
      info_data <- rbind(info_data, temp_data)
    }
  }
}

# select the item with highest IIF for each theta target
temp_data <- NULL
temp_maxinfo <- NULL
temp <- NULL
max_temp <- NULL

for (i in 1:length(unique(info_data$num_item))){
  temp_data <- info_data[info_data$num_item %in% unique(info_data$num_item)[i], ]
  temp_maxinfo <- aggregate(test_info ~ item + theta_target, 
                             data = temp_data, max)
  temp_maxinfo$interval_name <- unique(temp_data$num_item)
  
  for (j in 1:length(unique(temp_maxinfo$theta_target))) {
    temp <- temp_maxinfo[which(temp_maxinfo$test_info == max(temp_maxinfo$test_info)), ]
    temp_maxinfo <- temp_maxinfo[which(temp_maxinfo$item != temp$item & 
                                           temp_maxinfo$theta_target != temp$theta_target), ]
    max_temp <-rbind(max_temp, temp)
    
  }
}

# given the number(s) of items in num_item, filter out the selected ones from the 
# full-length test, estimate the model on the resulting short form(s), and 
# compute the IIF and TIF
out_interval <- list()
model_out_interval <- list()
info_out_interval <- list()

for (i in 1:length(unique(max_temp$interval_name))) {
  out_interval[[i]] <- data[, c(max_temp[max_temp$interval_name %in%unique(max_temp$interval_name)[i], 
                                            "item"])]
  model_out_interval[[i]] <- tam.mml(out_interval[[i]], 
                                        xsi.fixed = cbind(1:ncol(out_interval[[i]]), 
                                                          diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                    colnames(out_interval[[i]]))), 2]), 
                                        B= array(c(rep(0, ncol(out_interval[[i]])), 
                                                   discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                   colnames(out_interval[[i]])))]), 
                                                 c(ncol(out_interval[[i]]),2,1), 
                                                 dimnames = list(colnames(out_interval[[i]]), 
                                                                 c("Cat0", "Cat1"), 
                                                                 "Dim01")))
  info_out_interval[[i]] <- IRT.informationCurves(model_out_interval[[i]], 
                                                     theta = seq(-3, 3, length = 1000))
  names(info_out_interval)[[i]] <- unique(max_temp$interval_name)[i]
}

# Summary 
info_summary_interval <- NULL
temp <- NULL
for(i in 1:length(info_out_interval)) {
  temp <- data.frame(info_test = mean(info_out_interval[[i]]$test_info_curve), 
                     
                     
                     interval_name = names(info_out_interval)[[i]], 
                     item = paste(colnames(out_interval[[i]]), collapse = ","))
  
  info_summary_interval <- rbind(info_summary_interval, 
                                    temp)
}

info_summary_interval <-  rbind(info_summary_interval, 
                                   data.frame(info_test = sum(info_start),
                                              interval_name = "all", 
                                              item = "all"))
info_summary_interval$selection <- "EIP"

# Plot
ggplot(info_summary_interval, 
       aes(x=as.factor(interval_name), y = info_test)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Unequal Intervals Procedure (UIP) -----
lab_item <- 1:ncol(data) # item labels 
num_item <- seq(10, 90, 
                by = 20) # number of items included in the short form(s). This 
# can be either a vector of values or a single value. In the first case, 
# short forms composed of different numbers of items will be obtained, in the 
# second one, only one short form will be obtained with a number of items equal to 
# that indicated in the vector "num_item". The number of items in "num_item" must
# be lower than the number of items in the full-length test and equal or higher 
# than 2
num_clusters <- num_item # number of cluster(s) (based on the number of item(s)
# in num_item)

# cluster the latent trait
theta_mat <- matrix(true_theta, ncol = 1)
cluster <- list() 

for (i in 1:length(num_clusters)) {
  cluster[[i]] <- kmeans(theta_mat, 
                         centers = num_clusters[i])
}

# compute the IIF for each theta target (the centroids of the clusters)
temp_cluster <- list()
value_cluster  <- list()
temp_cluster_data   <- NULL
info_data_cluster <- NULL

for (j in 1:length(cluster)) {
  value_cluster[[j]] <- cluster[[j]]$centers[, 1]
  
  for(i in 1:length(lab_item)) {
    for(m in 1:length(value_cluster[[j]])) {
      
      temp_cluster_data   <- data.frame(theta_target = IRT.informationCurves(m2pl, 
                                                                             theta = value_cluster[[j]][m], 
                                                                             iIndex = lab_item[i])$theta,
                                        test_info = mean(IRT.informationCurves(m2pl, 
                                                                               theta = value_cluster[[j]][m], 
                                                                               iIndex = lab_item[i])$test_info_curve), 
                                        item_info = colSums(IRT.informationCurves(m2pl, 
                                                                                  theta = value_cluster[[j]][m], 
                                                                                  iIndex = lab_item[i])$info_curves_item),
                                        item = lab_item[i],
                                        num_item = paste("number", length(value_cluster[[j]]), sep = ""))
      
      info_data_cluster <- rbind(info_data_cluster, temp_cluster_data  )
    }
  }
}


temp_data_cluster <- NULL
temp_maxcluster <- NULL
temp <- NULL
max_temp_cluster <- NULL

for (i in 1:length(unique(info_data_cluster$num_item))){
  temp_data <- info_data_cluster[info_data_cluster$num_item %in% 
                                   unique(info_data_cluster$num_item)[i], ]
  temp_maxcluster <- aggregate(test_info ~ item + theta_target, 
                               data = temp_data, max)
  temp_maxcluster$cluster_name <- unique(temp_data$num_item)
  
  for (j in 1:length(unique(temp_maxcluster$theta_target))) {
    temp <- temp_maxcluster[which(temp_maxcluster$test_info == max(temp_maxcluster$test_info)), ]
    temp_maxcluster <- temp_maxcluster[which(temp_maxcluster$item != temp$item & 
                                               temp_maxcluster$theta_target != temp$theta_target), ]
    max_temp_cluster <-rbind(max_temp_cluster, temp)
    
  }
}


# given the number(s) of items in num_item, filter out the selected ones from the 
# full-length test, estimate the model on the resulting short form(s), and 
# compute the IIF and TIF
out_cluster = list()
model_out_cluster = list()
info_out_cluster = list()

for (i in 1:length(unique(max_temp_cluster$cluster_name))) {
  out_cluster[[i]] <- data[, c(max_temp_cluster[max_temp_cluster$cluster_name %in% unique(max_temp_cluster$cluster_name)[i], 
                                                "item"])]
  model_out_cluster[[i]] <- tam.mml(out_cluster[[i]], 
                                          xsi.fixed = cbind(1:ncol(out_cluster[[i]]), 
                                                            diff_true[as.integer(gsub("I00|I0|I", '', 
                                                                                      colnames(out_cluster[[i]]))), 2]),
                                          B= array(c(rep(0, ncol(out_cluster[[i]])), 
                                                     discr_true[,2,][as.integer(gsub("I00|I0|I", "",
                                                                                     colnames(out_cluster[[i]])))]), 
                                                   c(ncol(out_cluster[[i]]),2,1), 
                                                   dimnames = list(colnames(out_cluster[[i]]), 
                                                                   c("Cat0", "Cat1"), 
                                                                   "Dim01"))) 
  info_out_cluster[[i]] <- IRT.informationCurves(model_out_cluster[[i]], 
                                                       theta = seq(-3, 3, length = 1000))
  names(info_out_cluster)[[i]] <- unique(max_temp_cluster$cluster_name)[i]
}

# summary 
info_summary_cluster <- NULL
temp <- NULL
for(i in 1:length(info_out_cluster)) {
  temp <- data.frame(info_test = mean(info_out_cluster[[i]]$test_info_curve), 
                     
                     
                     cluster_name = names(info_out_cluster)[[i]], 
                     item = paste(colnames(out_cluster[[i]]), collapse = ","))
  
  info_summary_cluster <- rbind(info_summary_cluster, 
                                      temp)
}


info_summary_cluster <-  rbind(info_summary_cluster, 
                                     data.frame(info_test = info_start,
                                                cluster_name = "all", 
                                                item = "all"))
info_summary_cluster$selection <- "UIP"

# Plot
ggplot(info_summary_cluster, 
       aes(x = cluster_name, y = info_test)) + geom_point()

# Random Procedure (RP)----
# given a number of items (that can be a vector with a single value or a vector 
# of values resulting in short forms of different lengths), 
# repeat the random selection 10 times each
sampling <- c(rep(seq(10, 90, 
                      by = 20), 
                  each = 10)) # the number of random selections for each short 
# form can be changed

# prepare the random selection
item_random <- list()
info_test_random <- list()

# fit the model for each random selection, according to the number os repetitions
# in sampling
model_fit_random <- list()

for (i in 1:length(sampling)) {
  
  item_random[[i]] <-  data.frame(data[, sample(seq_len(ncol(data)), 
                                                size = sampling[i])])
  
  model_fit_random[[i]] <- tam.mml.2pl(item_random[[i]])
  
  # Compute TIF
  info_test_random[[i]] <- (IRT.informationCurves(model_fit_random[[i]], 
                                                  theta = seq(-3, 3, 
                                                              length = 1000)))
}

# organize results
data_random <- data.frame(matrix(nrow = 1:length(item_random)))

for (i in 1:length(item_random)) {
  data_random[i, "info_total"] <- mean(info_test_random[[i]]$test_info_curve)
  data_random[i, "num_item"] <- paste0("number", (model_fit_random[[i]]$nitem))
  data_random[i, "combo_item"] <- paste(c((model_fit_random[[i]]$item$item)), 
                                        collapse = " ")
}
data_random <- data_random[, -1]

data_random  <- rbind(data_random, 
                      data.frame(info_total = info_start,
                                 num_item = "all", 
                                 combo_item = "all"
                      ))

# summary
data_random_summary <- cbind(aggregate(info_total ~ num_item, 
                                       data = data_random,
                                       mean),
                             aggregate(info_total ~ num_item, data = data_random, sd)[,2])

colnames(data_random_summary)[2:ncol(data_random_summary)] <- c("mean_info", 
                                                                "sd_info")
data_random_summary[is.na(data_random_summary)] <-0

# Plot
ggplot(data_random_summary, 
       aes(x=num_item, y=mean_info)) + 
  # geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=mean_info-sd_info, ymax=mean_info+sd_info), width=.2,
                position=position_dodge(0.05)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



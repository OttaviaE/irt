library(TAM)
library(psych)
library(sirt)
rm(list = ls())
set.seed(999)
# Data simulation code ----
N <- 1000 # number of persons
b <- runif(100, -3,3)
a = c(runif(100, 0.4, 2))
true_theta = rnorm(N, mean=0, sd=1)
hist(true_theta)
summary(true_theta)
data <- sirt::sim.raschtype( true_theta, b=b, 
                             fixed.a = a)


diff_true <- matrix(cbind(1:length(b), 
                         b), 
                    ncol = 2)
discr_true = array(c(rep(0, length(a)), a), 
                   c(length(a),2,1), 
          dimnames = list(paste0("I", 1:length(a)), 
                          c("Cat0", "Cat1"), 
                          "Dim01"))

m2pl <- tam.mml(resp=data, xsi.fixed = diff_true, 
                    B = discr_true)
summary(m2pl)

info_start <- mean(IRT.informationCurves(m2pl)$test_info_curve) 
# # estraggo i theta 
theta_all <- m2pl$person$EAP

# # data simulation SK -----
# rm(list = ls())
# set.seed(666)
# moments <- c(mean = -2,variance = 1,skewness = 0.71, kurtosis = 4)
# true_theta_sk <- c(rpearson(1000, moments = moments))
# summary(true_theta_sk)
# hist(true_theta_sk, xlim = c(-6,6))
# b <- runif(100, -3,3)
# a = c(runif(100, 0.4, 2))
# data_sk <- sirt::sim.raschtype( true_theta_sk, b=b, 
#                              fixed.a = a)
# 
# diff_true <- matrix(cbind(1:length(b), 
#                           b), 
#                     ncol = 2)
# discr_true <- array(c(rep(0, length(a)), a), 
#                     c(length(a),2,1), 
#                     dimnames = list(paste0("I", 1:length(a)), 
#                                     c("Cat0", "Cat1"), 
#                                     "Dim01"))
# 
# m2pl_sk <- tam.mml(resp = data_sk, xsi.fixed = diff_true, 
#                        B = discr_true)
# info_start_sk <- mean((IRT.informationCurves(m2pl_sk,
#                                           theta =
#                                             seq(-3,3, length = 1000))$test_info_curve))
# theta_all_sk <- m2pl_sk$person$EAP

# data simulation SK extreme-----
rm(list = ls())
# library("PearsonDS")
# set.seed(666)
# moments <- c(mean = -2,variance = 2,skewness = 1.71, kurtosis = 8)
# true_theta_sk <- c(rpearson(1000, moments = moments))
# true_theta_sk = (true_theta_sk*0.5396) -.9227
set.seed(999)
true_theta_sk = (rbeta(1000, 1, 100)*100) - 3
hist(true_theta_sk)
summary(true_theta_sk)
b <- runif(100, -3,3)
a = c(runif(100, 0.4, 2))
data_sk <- sirt::sim.raschtype( true_theta_sk, b=b, 
                                fixed.a = a)

diff_true <- matrix(cbind(1:length(b), 
                          b), 
                    ncol = 2)
discr_true <- array(c(rep(0, length(a)), a), 
                    c(length(a),2,1), 
                    dimnames = list(paste0("I", 1:length(a)), 
                                    c("Cat0", "Cat1"), 
                                    "Dim01"))

m2pl_sk <- tam.mml(resp = data_sk, xsi.fixed = diff_true, 
                   B = discr_true)
info_start_sk <- mean((IRT.informationCurves(m2pl_sk,
                                             theta =
                                               seq(-3,3, length = 1000))$test_info_curve))
theta_all_sk <- m2pl_sk$person$EAP

# Data simulation uni -----
#
# # # # Random -----
rm(list = ls())
set.seed(999)
true_theta_uni <- c(runif(1000, min = -3, max = 3))
b <- runif(100, -3,3)
a = c(runif(100, 0.4, 2))
hist(true_theta_uni)
diff_true <- matrix(cbind(1:length(b), 
                          b), 
                    ncol = 2)
discr_true <- array(c(rep(0, length(a)), a), 
                    c(length(a),2,1), 
                    dimnames = list(paste0("I", 1:length(a)), 
                                    c("Cat0", "Cat1"), 
                                    "Dim01"))
data_uni <- sirt::sim.raschtype( true_theta_uni, b=b, 
                                fixed.a = a)

m2pl_uni <- tam.mml(resp = data_uni, 
                        xsi.fixed = diff_true, 
                        B = discr_true)
info_start_uni <- mean((IRT.informationCurves(m2pl_uni,
                                              theta =
                                                seq(-3,3, length = 1000))$test_info_curve))
# # # # estraggo i theta
theta_all_uni <- m2pl_uni$person$EAP


### Da TAM ------
# Model 2: 2PL model
data(data.sim.rasch)

mod2 <- TAM::tam.mml.2pl(resp=data.sim.rasch,irtmodel="2PL")
# extract item parameters
mod2$xsi # item difficulties
mod2$B # item slopes

xsi0 <- mod2$xsi$xsi
xsi.fixed <- cbind( 1:(length(xsi0)), xsi0 )
b = array(c(rep(0, length(mod2$B[,2,])), 
            mod2$B[,2,]), c(length(mod2$B[,2,]),2,1), 
          dimnames = list(paste0("I", 1:length(mod2$B[,2,])), 
                          c("Cat0", "Cat1"), 
                          "Dim01"))

# define vector with fixed item difficulties
mod2a <- TAM::tam.mml( resp=data.sim.rasch, xsi.fixed=xsi.fixed,
                       B=b # fix slopes
)
summary(mod2a)
cbind(mod2$B, mod2a$B, b) # inspect used slope matrix


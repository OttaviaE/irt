IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  y[is.na(y)] = 1
  return(y)
}
difficulty <- c(0, 0)
a = c(0.40, 1.60)
theta <- theta <- seq(-7, 7, .001)
b = difficulty
# item a
par(mar = c(5,7,4,2) + 0.1)
plot(theta, IRT(theta, b=difficulty[1], a = a[1]),
     cex.lab= 2.5,
     cex.axis =2,ann = F,
     xlim = c(-5, 5), ylim = c(0, 1),
     type = "l", lwd = 3,
      yaxt="n", padj = 0.5)
# plot(theta, IRT(theta, b=difficulty[1], a = a[1]),
#      type = "l", lwd=3,  axes = FALSE, ann = FALSE, xlim = c(-5, 5))
#item B
lines(theta, IRT(theta, b = difficulty[2], a = a[2]), lty = 2, lwd=3, 
      xlim =c(-5,5))
axis(side=2, at=c(0.00, 0.50, 1), labels = FALSE)
axis(side = 1, at=c(-8, 0, 8), labels = FALSE)
text(par("usr")[1], c(0, 0.5, 1.00),  
     labels = c("0.00", "0.50", "1.00"), pos = 2, xpd = TRUE, cex = 1.8)
title(ylab =expression(paste("P(x=1|", theta, a[j], ", ", b[j], ")")), 
      line = 4, cex.lab=3.2)
title(xlab = expression(theta), cex.lab = 3.2, line =3.5)
mtext(side = 1, at = c(0.5, 1.4),
      c(expression(theta[A]), 
        expression(theta[B])), 
      cex = 2.8, padj = 1)


lines(c(0.5,0.5), c(-0.1, 
                    exp(a[2] * (0.5 - b[1])) / (1 + exp(a[2] * (0.5 - b[1])))), 
      lty = 2, col = "gray", lwd=2)
points(0.5, exp(a[1] * (0.5 - b[1])) / (1 + exp(a[1] * (0.5 - b[1]))), cex = 3)
points(1, exp(a[1] * (1 - b[1])) / (1 + exp(a[1] * (1 - b[1]))), cex = 3)
points(0.5, exp(a[2] * (0.5 - b[1])) / (1 + exp(a[2] * (0.5 - b[1]))), cex = 3, 
       pch = 0)

lines(c(1,1), c(-0.1, 
                    exp(a[2] * (1 - b[1])) / (1 + exp(a[2] * (1 - b[1])))), 
      lty = 2, col = "gray", lwd=2)
points(1, exp(a[2] * (1 - b[1])) / (1 + exp(a[2] * (1 - b[1]))), cex = 3, 
       pch = 0)
segments(-8, exp(a[1] * (0.5 - b[1])) / (1 + exp(a[1] * (0.5 - b[1]))), 
         0.5, exp(a[1] * (0.5 - b[1])) / (1 + exp(a[1] * (0.5 - b[1]))), 
         lty = 2, col = "gray", lwd=2)
segments(-8, exp(a[1] * (1 - b[1])) / (1 + exp(a[1] * (1 - b[1]))), 
         1, exp(a[1] * (1 - b[1])) / (1 + exp(a[1] * (1 - b[1]))), 
         lty = 2, col = "gray", lwd=2)
segments(-8, exp(a[2] * (0.5 - b[1])) / (1 + exp(a[2] * (0.5 - b[1]))), 
         0.5, exp(a[2] * (0.5 - b[1])) / (1 + exp(a[2] * (0.5 - b[1]))), 
         lty = 2, col = "gray", lwd=2)
segments(-8, exp(a[2] * (1 - b[1])) / (1 + exp(a[2] * (1 - b[1]))), 
         1, exp(a[2] * (1 - b[1])) / (1 + exp(a[2] * (1 - b[1]))), 
         lty = 2, col = "gray", lwd=2)




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


c <- item_info(b,a)


Theta <- matrix(seq(-4,4, length.out=1000))
check <- data.frame(Theta, 
                    item_info = c[[1]], 
                    item_info2 = c[[2]])

d1 <- do.call('cbind',c)
sum_info2 <- rowSums(d1)

plot(check$Theta, check$item_info, ylim= c(0, 0.8), cex.lab= 2.5, 
     cex.axis =2, ann=F, 
     type = "l", lwd =2, lty = 1, padj = 0.5)
lines(check$Theta, check$item_info2, lwd =2, lty = 2)

title(ylab = "IIF", 
      line = 4, cex.lab=3.2)
title(xlab = expression(theta), cex.lab = 3.2, line =3.5)



library(ggplot2)

dati = read.csv(file.choose(), header = TRUE, sep = ",")
colnames(dati)[1] = "x"

ggplot(dati, 
       aes(x= x, y = y, group = z, color = z)) + 
  geom_line() + geom_point(aes(shape = z), size = 4) + theme_bw() +
  theme(legend.position = "bottom") 


text(x = -2.5, y = 0.20, "item a", cex = 2, col = "royalblue")
text(x = -0.5, y = 0.30, "item b", cex = 2, col = "magenta")
text(x = 2.5, y = 0.50, "item c", cex = 2, col = "seagreen")
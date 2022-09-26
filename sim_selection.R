# provo nel caso più semplice, prendo 10 item, 500 persone, difficoltà 
# da - 3 a 3 
# parto con item poco discriminativi a = 1 per tutti ma stimo il 2pl lo stesso

library(psych)
library(ltm)
set.seed(666)
data <- sim.irt(nvar = 10, 
                n = 500, 
                low = -3, 
                high = 3, 
                a = 1)

colMeans(data$items)

# stimo il modello 

m2pl <-  ltm(data$items ~ z1)
summary(m2pl)
plot(m2pl, type = "ICC")
plot(m2pl, type = "IIC")
plot(m2pl, type = "IIC", items = 0)

# mi salvo i parametri degli item 

item <- data.frame(coef(m2pl))

colnames(item) <- c("difficulty", "discr")

# Theta estimation according to the obseved response pattern:
  
theta_patterns <- factor.scores(m2pl)

# theta according to the specified observed responses (for all subjects): 
  
theta_obs <- factor.scores(m2pl, method = "EAP", 
                           resp.patterns = data$items)

# utilizzo i theta per ogni soggetto e calcolo i quartili.
quantile(theta_obs$score.dat$z1)
# il tratto latente potrebbe essere suddiviso in: 
# minimo - secondo quartile: basso
# secondo-quartile quarto quartile: medio
# quarto quartile: massimo

# assegno low medium high a tutti gli item sulla base del loro b
low <- c(-3, 
         quantile(theta_obs$score.dat$z1)[2])

medium <- c(quantile(theta_obs$score.dat$z1)[2], 
         quantile(theta_obs$score.dat$z1)[4])

high <- c(quantile(theta_obs$score.dat$z1)[4], 
          3)




item$range <- 0
item$items <- as.integer(gsub("V", "", rownames(item)))
for (i in 1:nrow(item)){
  if (item[i, "difficulty"] <= low[2]) {
    item[i, "range"] <- "low"
  } else if (item[i, "difficulty"] > low[2] & 
             item[i, "difficulty"] <= medium[2]) {
    item[i, "range"] <-"medium"
  } else if (item[i, "difficulty"] > high[1]) {
    item[i, "range"] <- "high"
  }
}

item
# solo un item copre il punto medio del tratto
# ora per ogni item mi calcolo l'informazione fornita per il corrispettivo 
# punto del tratto latente



item$inforange <- 0
item$infototal <- 0
item$proprange <- 0

low_item <- item[item$range %in% "low", 
                 "items"]
medium_item <- item[item$range %in% "medium", 
                 "items"]
high_item <- item[item$range %in% "high", 
                    "items"]



for (i in 1:nrow(item)) {
if (item[i, "range"] == "low") {
      item[i, "inforange"] <- information(m2pl, 
                                          range = low, 
                                          items = item[i, "items"])[1]
      item[i, "infototal"] <- information(m2pl, 
                                          range = low, 
                                          items = item[i, "items"])[2]
      item[i, "proprange"] <- information(m2pl, 
                                          range = low, 
                                          items = item[i, "items"])[3]

   
  } else if (item[i, "range"] == "medium") {

      item[i, "inforange"] <- information(m2pl, 
                                          range = medium, 
                                          items = item[i, "items"])[1]
      item[i, "infototal"] <- information(m2pl, 
                                          range = medium, 
                                          items = item[i, "items"])[2]
      item[i, "proprange"] <- information(m2pl, 
                                          range = medium, 
                                          items = item[i, "items"])[3]

   
  } else if (item[i, "range"] == "high") {

      item[i, "inforange"] <- information(m2pl, 
                                          range = high, 
                                          items = item[i, "items"])[1]
      item[i, "infototal"] <- information(m2pl, 
                                          range = high, 
                                          items = item[i, "items"])[2]
      item[i, "proprange"] <- information(m2pl, 
                                          range = high, 
                                          items = item[i, "items"])[3]
    
  }}

# a questo punto per ogni range 
# InfoRange the amount of information in the specified interval.
# InfoTotal the total amount of information; typically this is computed as the amount of
# information in the interval (−10, 10).
# PropRange the proportion of information in the specified range, i.e., "Info in range" / "Total Info".
# l'informazione più importante è fornita da inforange. 
# il totale di infromaizone fornita dal test è 
info_test <- information(m2pl, range = c(-3, 3))
info_test$InfoRange
info_test$PropRange
info_test$InfoTotal


# a questo punto dovrei togliere per ogni gruppo l'item che ha meno info, 
# stimare di nuovo il modello e ricalcolare di nuovo tutta la info per gli item
# e la info totale del test
# a parte medium perché chiaramente è l'unico 

min(item[item$range %in% "low", "inforange"])
min(item[item$range %in% "high", "inforange"])

out1 <- c(item[which(item$inforange == min(item[item$range %in% "low", "inforange"])), "items"],
item[which(item$inforange == min(item[item$range %in% "high", "inforange"])),  "items"])

# tolgo gli item individuati dalla matrice iniziale STEP 1 -----

data_first <- data$items[, -out1]

# stimo il modello 

m2pl_first <- ltm(data_first ~ z1)
item_first <- data.frame(coef(m2pl_first))
colnames(item_first) <- c("difficulty", "discr")
info_test_first <- information(m2pl_first, range = c(-3, 3))
info_test
# sale la perentuale di infromazione nel range, quindi forse si sta andando nella direzione giusta

# rifaccio il calcolo di prima pe ogni item 
# utilizzo il theta di prima? direi di si....?

item_first$range <- 0
item_first$items <- as.integer(gsub("V", "", rownames(item_first)))
for (i in 1:nrow(item_first)){
  if (item_first[i, "difficulty"] <= low[2]) {
    item_first[i, "range"] <- "low"
  } else if (item_first[i, "difficulty"] > low[2] & 
             item_first[i, "difficulty"] <= medium[2]) {
    item_first[i, "range"] <-"medium"
  } else if (item_first[i, "difficulty"] > high[1]) {
    item_first[i, "range"] <- "high"
  }
}

plot(m2pl, type = "ICC")
plot(m2pl, type = "IIC")
plot(m2pl, type = "IIC", items = 0)
plot(m2pl_first, type = "IIC", items = 0)
plot(m2pl_first, type = "IIC")

for (i in 1:nrow(item_first)) {
  if (item_first[i, "range"] == "low") {
    item_first[i, "inforange"] <- information(m2pl, 
                                              range = low, 
                                              items = item_first[i, "items"])[1]
    item_first[i, "infototal"] <- information(m2pl, 
                                              range = low, 
                                              items = item_first[i, "items"])[2]
    item_first[i, "proprange"] <- information(m2pl, 
                                              range = low, 
                                              items = item_first[i, "items"])[3]
    
    
  } else if (item_first[i, "range"] == "medium") {
    
    item_first[i, "inforange"] <- information(m2pl, 
                                              range = medium, 
                                              items = item_first[i, "items"])[1]
    item_first[i, "infototal"] <- information(m2pl, 
                                        range = medium, 
                                        items = item[i, "items"])[2]
    item_first[i, "proprange"] <- information(m2pl, 
                                        range = medium, 
                                        items = item_first[i, "items"])[3]
    
    
  } else if (item_first[i, "range"] == "high") {
    
    item_first[i, "inforange"] <- information(m2pl, 
                                        range = high, 
                                        items = item_first[i, "items"])[1]
    item_first[i, "infototal"] <- information(m2pl, 
                                        range = high, 
                                        items = item_first[i, "items"])[2]
    item_first[i, "proprange"] <- information(m2pl, 
                                        range = high, 
                                        items = item_first[i, "items"])[3]
    
  }}

out2 <- c(item_first[which(item_first$inforange == min(item_first[item_first$range %in% "low", "inforange"])), "items"],
          item_first[which(item_first$inforange == min(item_first[item_first$range %in% "high", "inforange"])),  "items"])

# secondo giro per togliere gli item maffi -----
data_second <- data$items[, -c(out1, out2)]

# stimo il modello 

m2pl_second <- ltm(data_second ~ z1)
item_second <- data.frame(coef(m2pl_second))
colnames(item_second) <- c("difficulty", "discr")
info_test_second <- information(m2pl_second, range = c(-3, 3))
info_test_first
info_test
# sale la perentuale di infromazione nel range, quindi forse si sta andando nella direzione giusta

# rifaccio il calcolo di prima pe ogni item 
# utilizzo il theta di prima? direi di si....?

item_second$range <- 0
item_second$items <- as.integer(gsub("V", "", rownames(item_second)))
for (i in 1:nrow(item_second)){
  if (item_second[i, "difficulty"] <= low[2]) {
    item_second[i, "range"] <- "low"
  } else if (item_second[i, "difficulty"] > low[2] & 
             item_second[i, "difficulty"] <= medium[2]) {
    item_second[i, "range"] <-"medium"
  } else if (item_second[i, "difficulty"] > high[1]) {
    item_second[i, "range"] <- "high"
  }
}

plot(m2pl, type = "ICC")
plot(m2pl, type = "IIC")
plot(m2pl, type = "IIC", items = 0)
plot(m2pl_second, type = "IIC", items = 0)
plot(m2pl_second, type = "IIC")
# il test è motlo infromativo per i tartti estremi del theta, ma è motlo basso epr i valori
# centrali

for (i in 1:nrow(item_second)) {
  if (item_second[i, "range"] == "low") {
    item_second[i, "inforange"] <- information(m2pl, 
                                               range = low, 
                                               items = item_second[i, "items"])[1]
    item_second[i, "infototal"] <- information(m2pl, 
                                               range = low, 
                                               items = item_second[i, "items"])[2]
    item_second[i, "proprange"] <- information(m2pl, 
                                               range = low, 
                                               items = item_second[i, "items"])[3]
    
    
  } else if (item_second[i, "range"] == "medium") {
    
    item_second[i, "inforange"] <- information(m2pl, 
                                               range = medium, 
                                               items = item_second[i, "items"])[1]
    item_second[i, "infototal"] <- information(m2pl, 
                                               range = medium, 
                                               items = item[i, "items"])[2]
    item_second[i, "proprange"] <- information(m2pl, 
                                               range = medium, 
                                               items = item_second[i, "items"])[3]
    
    
  } else if (item_second[i, "range"] == "high") {
    
    item_second[i, "inforange"] <- information(m2pl, 
                                               range = high, 
                                               items = item_second[i, "items"])[1]
    item_second[i, "infototal"] <- information(m2pl, 
                                               range = high, 
                                               items = item_second[i, "items"])[2]
    item_second[i, "proprange"] <- information(m2pl, 
                                               range = high, 
                                               items = item_second[i, "items"])[3]
    
  }}

out3 <- c(item_second[which(item_second$inforange == min(item_second[item_second$range %in% "low", "inforange"])), "items"],
          item_second[which(item_second$inforange == min(item_second[item_second$range %in% "high", "inforange"])),  "items"])


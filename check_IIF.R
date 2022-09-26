rm(list=ls())
# definire il numero di seed
s = 1000
theta_target = list() # salva tutti i theta target generati casulamente da una distribusione uniforme
item = list() # tutte le matrici degli item simulate per ogni iterazione
p_1pl = list() # salva le prob di risp corretta 
q_1pl = list()# salva le prob di risposta errata
p_2pl = list() # salva le prob di risp corretta 
q_2pl = list()# salva le prob di risposta errata
p_3pl = list() # salva le prob di risp corretta 
q_3pl = list()# salva le prob di risposta errata
ii_1pl = list() # iif 1pl
ii_2pl= list() # iif 2pl
ii_3pl = list() # iif 3pl
n_item = 5 # n item forma lunga
n_theta = 3 # nitem forma breve
ii_1pl_long = list()  # iif per ogni theta target in formato long 1PL
ii_2pl_long = list()  # iif per ogni theta target in formato long 2PL
ii_3pl_long = list()  # iif per ogni theta target in formato long 3Pl
max_data_1pl = list() # item con max iif per ogni theta target 1pl
max_data_2pl = list() # item con max iif per ogni theta target 2pl
max_data_3pl = list() # item con max iif per ogni theta target 3pl
info_chosen_1pl = list() #
sum_chosen_1pl = list()
info_chosen_2pl = list() #
sum_chosen_2pl = list()
info_chosen_3pl = list() #
sum_chosen_3pl = list()
item_c = data.frame(combn(paste("item", 1:n_item), 
                          n_theta)) # le combinazioni di item saranno ben le stess

sel_item_1pl =list()
small_sel_1pl = list()
sel_item_2pl =list()
small_sel_2pl = list()
sel_item_3pl =list()
small_sel_3pl = list()


problem_1pl = logical(s)
problem_2pl = logical(s)
problem_3pl = logical(s)

set.seed(999)
for (m in 1:s) {
  theta_target[[m]] = runif(n_theta, -3, 3) # simula i dati 
  item[[m]] = data.frame(item = paste("item", n_item), 
                    b = c(runif(n_item, -3,3)),
                    a = c(runif(n_item, .20, 2)), 
                    g = c(runif(n_item, .05, .5 )))
  
  # compute information for each item for each theta target (cluster and guided strategies)
  theta_target[[m]] = theta_target[[m]][order(theta_target[[m]])]
  # 1pl ----
   p_1pl[[m]] = data.frame(matrix(nrow = nrow(item[[m]]),
                                 ncol = length(theta_target[[m]])))
  q_1pl[[m]] = data.frame(matrix(nrow = nrow(item[[m]]),
                                 ncol = length(theta_target[[m]])))
  ii_1pl[[m]] = data.frame(matrix(nrow = nrow(item[[m]]),
                                  ncol = length(theta_target[[m]])))
  colnames(ii_1pl[[m]]) = theta_target[[m]]
  rownames(ii_1pl[[m]]) = paste("item", 1:nrow(item[[m]]))
  # 2pl -----
  p_2pl[[m]] = data.frame(matrix(nrow = nrow(item[[m]]),
                        ncol = length(theta_target[[m]])))
  q_2pl[[m]] = data.frame(matrix(nrow = nrow(item[[m]]),
                        ncol = length(theta_target[[m]])))
  ii_2pl[[m]] = data.frame(matrix(nrow = nrow(item[[m]]),
                         ncol = length(theta_target[[m]])))
  colnames(ii_2pl[[m]]) = theta_target[[m]]
  rownames(ii_2pl[[m]]) = paste("item", 1:nrow(item[[m]]))
  # 3pl ----
  p_3pl[[m]] = data.frame(matrix(nrow = nrow(item[[m]]),
                                 ncol = length(theta_target[[m]])))
  q_3pl[[m]] = data.frame(matrix(nrow = nrow(item[[m]]),
                                 ncol = length(theta_target[[m]])))
  ii_3pl[[m]] = data.frame(matrix(nrow = nrow(item[[m]]),
                                  ncol = length(theta_target[[m]])))
  colnames(ii_3pl[[m]]) = theta_target[[m]]
  rownames(ii_3pl[[m]]) = paste("item", 1:nrow(item[[m]]))
  temp = NULL
  # calcola l'infomartion di ogni item per ogni theta target
  for (i in 1:nrow(item[[m]])) {
    temp = item[[m]][i, ]
    for (j in 1:length(theta_target[[m]])) {
      # 1pl 
      p_1pl[[m]][i, j] =  exp((theta_target[[m]][j] - temp$b))/(1 + exp((theta_target[[m]][j] - temp$b)))
      q_1pl[[m]][i, j] = 1-(p_1pl[[m]][i, j])
      # info_1pl
      ii_1pl[[m]][i,j] = p_1pl[[m]][i,j] * q_1pl[[m]][i,j]
      # 2pl 
      p_2pl[[m]][i, j] =  exp(temp$a *(theta_target[[m]][j] - temp$b))/(1 + exp(temp$a *(theta_target[[m]][j] - temp$b)))
      q_2pl[[m]][i, j] = 1-(p_2pl[[m]][i, j])
      # info 2pl 
      ii_2pl[[m]][i,j] = temp$a^2 * p_2pl[[m]][i,j] * q_2pl[[m]][i,j]
      # # 3pl 
      p_3pl[[m]][i, j] =  temp$g + (1-temp$g)*(exp(temp$a *(theta_target[[m]][j] - temp$b))/(1 + exp(temp$a *(theta_target[[m]][j] - temp$b))))
      q_3pl[[m]][i, j] = 1-(p_3pl[[m]][i, j])
      # info 3pl
      ii_3pl[[m]][i, j] = temp$a^2 * (p_3pl[[m]][i,j] / q_3pl[[m]][i,j])*((p_3pl[[m]][i,j] -temp$g)/(1-temp$g))^2
    }
  }
  # 1pl calcolo info massima per ogni theta target ---- 
  ii_1pl[[m]]$item =rownames(ii_1pl[[m]])
  ii_1pl[[m]] = ii_1pl[[m]][, c(ncol(ii_1pl[[m]]), 1:(ncol(ii_1pl[[m]])-1))]
  
  ii_1pl_long[[m]] = stats::reshape(ii_1pl[[m]],
                                    idvar = "item",
                                    direction = "long",
                                    varying = list(2:ncol(ii_1pl[[m]])),
                                    v.names = "info",
                                    timevar = "theta_target",
                                    times = names(ii_1pl[[m]])[-1])
  
  
  max_data_1pl[[m]] = stats::aggregate(info ~ item + theta_target,
                                       data = ii_1pl_long[[m]], max)
  
  max_data_1pl[[m]] =  max_data_1pl[[m]][order(as.numeric(max_data_1pl[[m]]$theta_target)), ]
  
  temp = NULL
  max_info = NULL
  
  for(i in 1:length(unique(max_data_1pl[[m]]$theta_target))) {
    temp1 = max_data_1pl[[m]][which(max_data_1pl[[m]]$info == max(max_data_1pl[[m]]$info)), ]
    max_data_1pl[[m]] = max_data_1pl[[m]][which(max_data_1pl[[m]]$item != temp1$item &
                                                  max_data_1pl[[m]]$theta_target != temp1$theta_target), ]
    max_info = rbind(max_info, temp1)
  }
  max_info$theta_target = as.numeric(max_info$theta_target)
  for (i in 2:ncol(max_info)) {
    max_info[, i] = round(max_info[, i], 2)
  }
  # qui ho la selezione finale di item che voglio
  max_info = max_info[order(max_info$theta_target), ]
  # matrice con tutte le inormatività per la forma "prescelta"
  info_chosen_1pl[[m]] = ii_1pl[[m]][ii_1pl[[m]]$item %in% max_info$item, ]
  sum_chosen_1pl[[m]] = sum(info_chosen_1pl[[m]][,-1]) # somma totale dell'informatività nella matrice
  sel_item_1pl[[m]] = list() # item selezionati 
  num_sel =  choose(n_item, n_theta) # numero totale di volte che si deve ripetere la selezione casuale
  small_sel_1pl[[m]] = data.frame(item=character(num_sel),
                                  tot = numeric(num_sel), # info totale degli item a caso
                                  item_gold = rep(paste(info_chosen_1pl[[m]]$item, collapse = " "),
                                                  num_sel), # info degl item selezionati con cognizione
                                  gold = rep(sum_chosen_1pl[[m]], num_sel)) # totale ino matrice item slezionati
  
  for (i in 1:num_sel) { # va a vedere se ci sono combo di item che risultano in infor maggiore
    sel_item_1pl[[m]][[i]] = ii_1pl[[m]][ii_1pl[[m]]$item %in% item_c[,i], ]
    sel_item_1pl[[m]][[i]][, "tot"] = sum(ii_1pl[[m]][ii_1pl[[m]]$item %in% item_c[,i], -1])
    small_sel_1pl[[m]][i, "item"] = paste(sel_item_1pl[[m]][[i]][, "item"],
                                          collapse = " ")
    small_sel_1pl[[m]][i, "tot"] = sel_item_1pl[[m]][[i]][1, "tot"]
    if (small_sel_1pl[[m]][i, "tot"] >= small_sel_1pl[[m]][i, "gold"] & small_sel_1pl[[m]][i, "item"] !=
        small_sel_1pl[[m]][i, "item_gold"]) {
      small_sel_1pl[[m]][i, "problem"] = 1
    } else {
      small_sel_1pl[[m]][i, "problem"] = 0
    }
  }
# 2pl calcolo info massima per ogni theta target----
  ii_2pl[[m]]$item =rownames(ii_2pl[[m]])
  ii_2pl[[m]] = ii_2pl[[m]][, c(ncol(ii_2pl[[m]]), 1:(ncol(ii_2pl[[m]])-1))]
  
  ii_2pl_long[[m]] = stats::reshape(ii_2pl[[m]],
                           idvar = "item",
                           direction = "long",
                           varying = list(2:ncol(ii_2pl[[m]])),
                           v.names = "info",
                           timevar = "theta_target",
                           times = names(ii_2pl[[m]])[-1])
  
  
  max_data_2pl[[m]] = stats::aggregate(info ~ item + theta_target,
                              data = ii_2pl_long[[m]], max)
  
  max_data_2pl[[m]] =  max_data_2pl[[m]][order(as.numeric(max_data_2pl[[m]]$theta_target)), ]
  
  temp = NULL
  max_info = NULL

  for(i in 1:length(unique(max_data_2pl[[m]]$theta_target))) {
    temp1 = max_data_2pl[[m]][which(max_data_2pl[[m]]$info == max(max_data_2pl[[m]]$info)), ]
    max_data_2pl[[m]] = max_data_2pl[[m]][which(max_data_2pl[[m]]$item != temp1$item &
                                          max_data_2pl[[m]]$theta_target != temp1$theta_target), ]
    max_info = rbind(max_info, temp1)
  }
  max_info$theta_target = as.numeric(max_info$theta_target)
  for (i in 2:ncol(max_info)) {
    max_info[, i] = round(max_info[, i], 2)
  }
  # qui ho la selezione finale di item che voglio
  max_info = max_info[order(max_info$theta_target), ]
  # matrice con tutte le inormatività per la forma "prescelta"
  info_chosen_2pl[[m]] = ii_2pl[[m]][ii_2pl[[m]]$item %in% max_info$item, ]
  sum_chosen_2pl[[m]] = sum(info_chosen_2pl[[m]][,-1]) # somma totale dell'informatività nella matrice
  sel_item_2pl[[m]] = list() # item selezionati 
  num_sel =  choose(n_item, n_theta) # numero totale di volte che si deve ripetere la selezione casuale
  small_sel_2pl[[m]] = data.frame(item=character(num_sel),
                         tot = numeric(num_sel), # info totale degli item a caso
                         item_gold = rep(paste(info_chosen_2pl[[m]]$item, collapse = " "),
                                         num_sel), # info degl item selezionati con cognizione
                         gold = rep(sum_chosen_2pl[[m]], num_sel)) # totale ino matrice item slezionati

  for (i in 1:num_sel) { # va a vedere se ci sono combo di item che risultano in infor maggiore
    sel_item_2pl[[m]][[i]] = ii_2pl[[m]][ii_2pl[[m]]$item %in% item_c[,i], ]
    sel_item_2pl[[m]][[i]][, "tot"] = sum(ii_2pl[[m]][ii_2pl[[m]]$item %in% item_c[,i], -1])
    small_sel_2pl[[m]][i, "item"] = paste(sel_item_2pl[[m]][[i]][, "item"],
                                      collapse = " ")
    small_sel_2pl[[m]][i, "tot"] = sel_item_2pl[[m]][[i]][1, "tot"]
    if (small_sel_2pl[[m]][i, "tot"] >= small_sel_2pl[[m]][i, "gold"] & small_sel_2pl[[m]][i, "item"] !=
        small_sel_2pl[[m]][i, "item_gold"]) {
      small_sel_2pl[[m]][i, "problem"] = 1
    } else {
      small_sel_2pl[[m]][i, "problem"] = 0
    }
  }
  # 3pl calcolo info massima per ogni theta target 
  ii_3pl[[m]]$item =rownames(ii_3pl[[m]])
  ii_3pl[[m]] = ii_3pl[[m]][, c(ncol(ii_3pl[[m]]), 1:(ncol(ii_3pl[[m]])-1))]
  
  ii_3pl_long[[m]] = stats::reshape(ii_3pl[[m]],
                                    idvar = "item",
                                    direction = "long",
                                    varying = list(2:ncol(ii_3pl[[m]])),
                                    v.names = "info",
                                    timevar = "theta_target",
                                    times = names(ii_3pl[[m]])[-1])
  
  
  max_data_3pl[[m]] = stats::aggregate(info ~ item + theta_target,
                                       data = ii_3pl_long[[m]], max)
  
  max_data_3pl[[m]] =  max_data_3pl[[m]][order(as.numeric(max_data_3pl[[m]]$theta_target)), ]
  
  temp = NULL
  max_info = NULL
  
  for(i in 1:length(unique(max_data_3pl[[m]]$theta_target))) {
    temp1 = max_data_3pl[[m]][which(max_data_3pl[[m]]$info == max(max_data_3pl[[m]]$info)), ]
    max_data_3pl[[m]] = max_data_3pl[[m]][which(max_data_3pl[[m]]$item != temp1$item &
                                                  max_data_3pl[[m]]$theta_target != temp1$theta_target), ]
    max_info = rbind(max_info, temp1)
  }
  max_info$theta_target = as.numeric(max_info$theta_target)
  for (i in 2:ncol(max_info)) {
    max_info[, i] = round(max_info[, i], 2)
  }
  # qui ho la selezione finale di item che voglio
  max_info = max_info[order(max_info$theta_target), ]
  # matrice con tutte le inormatività per la forma "prescelta"
  info_chosen_3pl[[m]] = ii_3pl[[m]][ii_3pl[[m]]$item %in% max_info$item, ]
  sum_chosen_3pl[[m]] = sum(info_chosen_3pl[[m]][,-1]) # somma totale dell'informatività nella matrice
  sel_item_3pl[[m]] = list() # item selezionati 
  num_sel =  choose(n_item, n_theta) # numero totale di volte che si deve ripetere la selezione casuale
  small_sel_3pl[[m]] = data.frame(item=character(num_sel),
                                  tot = numeric(num_sel), # info totale degli item a caso
                                  item_gold = rep(paste(info_chosen_3pl[[m]]$item, collapse = " "),
                                                  num_sel), # info degl item selezionati con cognizione
                                  gold = rep(sum_chosen_3pl[[m]], num_sel)) # totale ino matrice item slezionati
  
  for (i in 1:num_sel) { # va a vedere se ci sono combo di item che risultano in infor maggiore
    sel_item_3pl[[m]][[i]] = ii_3pl[[m]][ii_3pl[[m]]$item %in% item_c[,i], ]
    sel_item_3pl[[m]][[i]][, "tot"] = sum(ii_3pl[[m]][ii_3pl[[m]]$item %in% item_c[,i], -1])
    small_sel_3pl[[m]][i, "item"] = paste(sel_item_3pl[[m]][[i]][, "item"],
                                          collapse = " ")
    small_sel_3pl[[m]][i, "tot"] = sel_item_3pl[[m]][[i]][1, "tot"]
    if (small_sel_3pl[[m]][i, "tot"] >= small_sel_3pl[[m]][i, "gold"] & small_sel_3pl[[m]][i, "item"] !=
        small_sel_3pl[[m]][i, "item_gold"]) {
      small_sel_3pl[[m]][i, "problem"] = 1
    } else {
      small_sel_3pl[[m]][i, "problem"] = 0
    }
  }
  # crea vettore dei problemi -----
  if (any(small_sel_1pl[[m]][, "problem"] == 1) == T) {
    problem_1pl[m] = T 
  } else {
    problem_1pl[m] = F
  }
  if (any(small_sel_2pl[[m]][, "problem"] == 1) == T) {
    problem_2pl[m] = T 
  } else {
    problem_2pl[m] = F
  }
  if (any(small_sel_3pl[[m]][, "problem"] == 1) == T) {
    problem_3pl[m] = T 
  } else {
    problem_3pl[m] = F
  }

  
}



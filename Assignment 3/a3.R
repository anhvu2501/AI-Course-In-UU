learn = function(h_data) {
  print("h_data")
  print(h_data[1:10,])
  #print(h_data)
  
  network <- b_network(h_data)
  
}

b_network = function(h_data) {
  network = list()
  for(i in 1:9) {
    network[i] = matrix()
  }
  names(network) <- c("Pn", "VTB", "Sm", 
                      "TB_given_VTB", "LC_given_Sm", "Br_given_Sm", 
                      "Te_given_Pn", "XR_given_Pn_Tb_LC", "Dy_given_LC_Br")
  
  data_points = nrow(h_data)
  
  network$Pn            <- Pn_matrix(h_data)
  network$VTB           <- VTB_matrix(h_data)
  network$Sm            <- Sm_martix(h_data)
  network$TB_given_VTB  <- TB_given_VTB_matrix(h_data)
  network$LC_given_Sm   <- LC_given_Sm_matrix(h_data)
  network$Br_given_Sm   <- Br_given_Sm_matrix(h_data)
  network$Te_given_Pn   <- Te_given_Pn_matrix(h_data)
  network$XR_given_Pn_Tb_LC <- XR_given_Pn_Tb_LC_matrix(h_data)
  network$Dy_given_LC_Br  <- Dy_given_LC_Br_matrix(h_data)
  
  print("network")
  print(network)
  
  return (network)
}

Pn_matrix = function(h_data) {
  prob_Pn = (sum(h_data[,1]) + 1) / (nrow(h_data) + 2)
  Pn = matrix(nrow = 1, ncol = 2)
  Pn[1,1] = 1-prob_Pn
  Pn[1,2] = prob_Pn
  return (Pn)
}

VTB_matrix = function(h_data) {
  prob_VTB = (sum(h_data[, 3]) + 1) / (nrow(h_data) + 2)
  VTB = matrix(nrow = 1, ncol = 2)
  VTB[1,1] = 1-prob_VTB
  VTB[1,2] = prob_VTB
  return(VTB)
}

Sm_martix = function(h_data) {
  prob_Sm = (sum(h_data[, 5]) + 1) / (nrow(h_data) + 2)
  Sm = matrix(nrow = 1, ncol = 2)
  Sm[1,1] = 1-prob_Sm
  Sm[1,2] = prob_Sm
  return(Sm)
}

TB_given_VTB_matrix = function(h_data){
  VTB_true = h_data[h_data[,3] == 1,]
  VTB_false = h_data[h_data[,3] == 0,]
  prob_TB_given_VTB = (sum(VTB_true[,4]) + 1) / (nrow(VTB_true) + 2)
  prob_TB_given_false_VTB = (sum(VTB_false[,4]) + 1) / (nrow(VTB_false) + 2)
  TB_given_VTB = matrix(nrow = 2, ncol = 2)
  TB_given_VTB[1,1] = 1 - prob_TB_given_false_VTB
  TB_given_VTB[1,2] = prob_TB_given_false_VTB
  TB_given_VTB[2,1] = 1 - prob_TB_given_VTB
  TB_given_VTB[2,2] = prob_TB_given_VTB
  return(TB_given_VTB)
}

LC_given_Sm_matrix = function(h_data) {
  Sm_true = h_data[h_data[,5] == 1,]
  Sm_false = h_data[h_data[,5] == 0,]
  prob_LC_given_Sm = (sum(Sm_true[,6]) + 1) / (nrow(Sm_true) + 2)
  prob_LC_given_false_Sm = (sum(Sm_false[,6]) + 1) / (nrow(Sm_false) + 2)
  LC_given_Sm = matrix(nrow = 2, ncol = 2)
  LC_given_Sm[1,1] = 1 - prob_LC_given_false_Sm
  LC_given_Sm[1,2] = prob_LC_given_false_Sm
  LC_given_Sm[2,1] = 1 - prob_LC_given_Sm
  LC_given_Sm[2,2] = prob_LC_given_Sm
  return(LC_given_Sm)
}

Br_given_Sm_matrix = function(h_data) {
  Sm_true = h_data[h_data[,5] == 1,]
  Sm_false = h_data[h_data[,5] == 0,]
  prob_Br_given_Sm = (sum(Sm_true[,7]) + 1) / (nrow(Sm_true) + 2)
  prob_Br_given_false_Sm = (sum(Sm_false[,7]) + 1) / (nrow(Sm_false) + 2)
  Br_given_Sm = matrix(nrow = 2, ncol = 2)
  Br_given_Sm[1,1] = 1 - prob_Br_given_false_Sm
  Br_given_Sm[1,2] = prob_Br_given_false_Sm
  Br_given_Sm[2,1] = 1 - prob_Br_given_Sm
  Br_given_Sm[2,2] = prob_Br_given_Sm
  return(Br_given_Sm)
}

Te_given_Pn_matrix = function(h_data) {
  Pn_false = h_data[h_data[,1] == 0,]
  Pn_true = h_data[h_data[,1] == 1,]
  mean_Te_given_Pn_false = mean(Pn_false[,2])
  sd_Te_given_Pn_false = sd(Pn_false[,2])
  mean_Te_given_Pn_true = mean(Pn_true[,2])
  sd_Te_given_Pn_true = sd(Pn_true[,2])
  Te_given_Pn = matrix(nrow = 2, ncol = 2)
  Te_given_Pn[1,1] = mean_Te_given_Pn_false
  Te_given_Pn[1,2] = sd_Te_given_Pn_false
  Te_given_Pn[2,1] = mean_Te_given_Pn_true
  Te_given_Pn[2,2] = sd_Te_given_Pn_true
  return(Te_given_Pn)
}

XR_given_Pn_Tb_LC_matrix = function(h_data) {
  Pn_false_Tb_false_LC_false = h_data[h_data[,1] == 0 & h_data[,4] == 0 & h_data[,6] == 0,]
  Pn_false_Tb_false_LC_true = h_data[(h_data[,1] == 0) & (h_data[,4] == 0) & (h_data[,6] == 1),]
  Pn_false_Tb_true_LC_false = h_data[h_data[,1] == 0 & h_data[,4] == 1 & h_data[,6] == 0,]
  Pn_false_Tb_true_LC_true = h_data[h_data[,1] == 0 & h_data[,4] == 1 & h_data[,6] == 1,]
  Pn_true_Tb_false_LC_false = h_data[h_data[,1] == 1 & h_data[,4] == 0 & h_data[,6] == 0,]
  Pn_true_Tb_false_LC_true = h_data[h_data[,1] == 1 & h_data[,4] == 0 & h_data[,6] == 1,]
  Pn_true_Tb_true_LC_false = h_data[h_data[,1] == 1 & h_data[,4] == 1 & h_data[,6] == 0,]
  Pn_true_Tb_true_LC_true = h_data[h_data[,1] == 1 & h_data[,4] == 1 & h_data[,6] == 1,]
  
  prob_XR_given_Pn_false_Tb_false_LC_false = (sum(Pn_false_Tb_false_LC_false[,8]) + 1) / (nrow(Pn_false_Tb_false_LC_false) + 2)
  prob_XR_given_Pn_false_Tb_false_LC_true = (sum(Pn_false_Tb_false_LC_true[,8]) + 1) / (nrow(Pn_false_Tb_false_LC_true) + 2)
  prob_XR_given_Pn_false_Tb_true_LC_false = (sum(Pn_false_Tb_true_LC_false[,8]) + 1) / (nrow(Pn_false_Tb_true_LC_false) + 2)
  prob_XR_given_Pn_false_Tb_true_LC_true = (sum(Pn_false_Tb_true_LC_true[,8]) + 1) / (nrow(Pn_false_Tb_true_LC_true) + 2)
  prob_XR_given_Pn_true_Tb_false_LC_false = (sum(Pn_true_Tb_false_LC_false[,8]) + 1) / (nrow(Pn_true_Tb_false_LC_false) + 2)
  prob_XR_given_Pn_true_Tb_false_LC_true = (sum(Pn_true_Tb_false_LC_true[,8]) + 1) / (nrow(Pn_true_Tb_false_LC_true) + 2)
  prob_XR_given_Pn_true_Tb_true_LC_false = (sum(Pn_true_Tb_true_LC_false[,8]) + 1) / (nrow(Pn_true_Tb_true_LC_false) + 2)
  prob_XR_given_Pn_true_Tb_true_LC_true = (sum(Pn_true_Tb_true_LC_true[,8]) + 1)  / (nrow(Pn_true_Tb_true_LC_true) + 2)
  
  XR_given_Pn_Tb_LC = matrix(nrow = 8, ncol = 2)
  XR_given_Pn_Tb_LC[1,1] = 1 - prob_XR_given_Pn_false_Tb_false_LC_false
  XR_given_Pn_Tb_LC[1,2] = prob_XR_given_Pn_false_Tb_false_LC_false
  XR_given_Pn_Tb_LC[2,1] = 1 - prob_XR_given_Pn_false_Tb_false_LC_true
  XR_given_Pn_Tb_LC[2,2] = prob_XR_given_Pn_false_Tb_false_LC_true
  XR_given_Pn_Tb_LC[3,1] = 1 - prob_XR_given_Pn_false_Tb_true_LC_false
  XR_given_Pn_Tb_LC[3,2] = prob_XR_given_Pn_false_Tb_true_LC_false
  XR_given_Pn_Tb_LC[4,1] = 1 - prob_XR_given_Pn_false_Tb_true_LC_true
  XR_given_Pn_Tb_LC[4,2] = prob_XR_given_Pn_false_Tb_true_LC_true
  XR_given_Pn_Tb_LC[5,1] = 1 - prob_XR_given_Pn_true_Tb_false_LC_false
  XR_given_Pn_Tb_LC[5,2] = prob_XR_given_Pn_true_Tb_false_LC_false
  XR_given_Pn_Tb_LC[6,1] = 1 - prob_XR_given_Pn_true_Tb_false_LC_true
  XR_given_Pn_Tb_LC[6,2] = prob_XR_given_Pn_true_Tb_false_LC_true
  XR_given_Pn_Tb_LC[7,1] = 1 - prob_XR_given_Pn_true_Tb_true_LC_false
  XR_given_Pn_Tb_LC[7,2] = prob_XR_given_Pn_true_Tb_true_LC_false
  XR_given_Pn_Tb_LC[8,1] = 1 - prob_XR_given_Pn_true_Tb_true_LC_true
  XR_given_Pn_Tb_LC[8,2] = prob_XR_given_Pn_true_Tb_true_LC_true
  
  return(XR_given_Pn_Tb_LC)
}

Dy_given_LC_Br_matrix = function(h_data) {
  LC_false_Br_false =  h_data[h_data[,6] == 0 & h_data[,7] == 0,]
  Lc_false_Br_true = h_data[h_data[,6] == 0 & h_data[,7] == 1,]
  LC_true_Br_false = h_data[h_data[,6] == 1 & h_data[,7] == 0,]
  LC_true_Br_true = h_data[h_data[,6] == 1 & h_data[,7] == 1,]
  prob_Dy_given_LC_false_Br_false = (sum(LC_false_Br_false[,9]) + 1) / (nrow(LC_false_Br_false) + 2)
  prob_Dy_given_Lc_false_Br_true = (sum(Lc_false_Br_true[,9]) + 1) / (nrow(Lc_false_Br_true) + 2)
  prob_Dy_given_LC_true_Br_false = (sum(LC_true_Br_false[,9]) + 1) / (nrow(LC_true_Br_false) + 2)
  prob_Dy_given_LC_true_Br_true = (sum(LC_true_Br_true[,9]) + 1) / (nrow(LC_true_Br_true) + 2)
  Dy_given_LC_Br = matrix(nrow = 4, ncol = 2)
  Dy_given_LC_Br[1,1] = 1 - prob_Dy_given_LC_false_Br_false
  Dy_given_LC_Br[1,2] = prob_Dy_given_LC_false_Br_false
  Dy_given_LC_Br[2,1] = 1 - prob_Dy_given_Lc_false_Br_true
  Dy_given_LC_Br[2,2] = prob_Dy_given_Lc_false_Br_true
  Dy_given_LC_Br[3,1] = 1 - prob_Dy_given_LC_true_Br_false
  Dy_given_LC_Br[3,2] = prob_Dy_given_LC_true_Br_false
  Dy_given_LC_Br[4,1] = 1 - prob_Dy_given_LC_true_Br_true
  Dy_given_LC_Br[4,2] = prob_Dy_given_LC_true_Br_true
  return(Dy_given_LC_Br)
}

diagnose = function(network, cases) {
  for (row in 1:nrow(cases)) {
    cases[row,] = diagnose_case(network, cases[row,])
  }
  
}

diagnose_case = function(network, case) {
  print("cases before init")
  print(case)
  assigned_case = initialize_case(network, case)
  #print("case")
  #print(case)
  
  for(i in 1:10) {
    #Br
    proposed_case = assigned_case
    proposed_case[1,7] = symmetric_func(proposed_case[1,7])
    # p_old = P(Sm = assigned_case[1,5], Br=assigned_case[1,7], LC, Dy, VTB, TB, Pn, XR, Te)
    # = 
    p_old = 
    p_new = 
    
  }
  
  # TODO: should not be case!!!
  return(case)
}


initialize_case = function(network, case) {
  # Topologial sort
  # (Sm, Br, LC, Dy, VTB, TB, Pn, XR, Te)
  # Unknowns are Pn, TB, LC and Br
  
  random_u = rbinom(4,1,0.5)
  case[1,1] = random_u[1]
  case[1,4] = random_u[2]
  case[1,6] = random_u[3]
  case[1,7] = random_u[4]
  
  return(case)
}

symmetric_func = function(x){
  if(x == 1) {
    return(0)
  }
  else {
    return(1)
  }
}

test_func = function(h_data, cases) {
  network <- learn(h_data)
  
  diagnose(network, cases)
}
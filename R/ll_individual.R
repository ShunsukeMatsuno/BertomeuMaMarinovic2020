ll_individual <- function(df_individual, x_random,
                          alpha, beta, sigma_x, sigma_e_hat, p_11, p_01,
                          Pnd_splined){
  # @input: data frame of each firm
  mat_individual <- df_individual %>% 
    mutate(p = 0) %>% 
    as.matrix()
  
  # initial value of p
  mat_individual[1,"p"] <- if_else(mat_individual[1,"d"] == 1,
                                   rbeta(1, shape1 = 5, shape2 = 1),
                                   rbeta(1, shape1 = 1, shape2 = 5))
  
  # sequential computation
  ll <- numeric(NROW(mat_individual))
  for(i in 1:NROW(mat_individual)){
    P_nd <- Pnd_splined(mat_individual[i, "p"])
    if(mat_individual[i, "d"] == 1){
      # likelihood at time i
      ll[i] <- (1 - mat_individual[i,"p"]) * 
        (1 - pnorm(alpha * (P_nd - mat_individual[i, "x"]) - beta)) *
        dnorm(mat_individual[i, "x"], 
              mean = sigma_x^2/sigma_e_hat^2,
              sd = sqrt(sigma_x^2 * (1 - sigma_x^2/sigma_e_hat^2))) *
        1/sigma_e_hat * dnorm(mat_individual[i,"e"]/sigma_e_hat)
      
      # update of p
      if(i < NROW(mat_individual)){
        mat_individual[i+1, "p"] <- p_01
      }
      
    }else{
      # integral part
      I <- mean(pnorm(alpha * (P_nd - x_random) - beta))
      
      # likelihood at time i
      v <- mat_individual[i,"p"] + (1 - mat_individual[i,"p"]) * I 
      ll[i] <- v * 1/sigma_e_hat * dnorm(mat_individual[i,"e"]/sigma_e_hat)
      
      # update of p
      if(i < NROW(mat_individual)){
        mat_individual[i+1, "p"] <- mat_individual[i, "p"]/v * p_11 + 
          (1 - mat_individual[i, "p"]/v) * p_01 
      }
    }
  }
  return(sum(ll))
}

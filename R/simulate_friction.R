simulate_friction <- function(Time, p_11, p_01, p_init = p_01/(1 - p_11 + p_01), n){
  # @description
  # We first compute p_t = Pr(theta_t = 1) and then draw theta_t from Ber(p_t)
  #
  # @note
  # The argument `n` is not used inside the function and it is for the sake of repeating this 
  #   function.
  # It may be better to use transition matrix to compute the transition,
  #   given that states might be bigger if we extend the model. 
  
  # matrix for result
  res <- array(dim = c(Time, 2))
  colnames(res) <- c("p", "theta")
  
  # initial value
  res[1,1] <- p_init
  res[1,2] <- rbinom(1, size = 1, prob = p_init)
  
  # sequential update
  for(i in 2:Time){
    res[i,1] <- p_11 * res[i-1,2] + p_01 * (1 - res[i-1,2])   # update of p
    res[i,2] <- rbinom(1, size = 1, prob = res[i-1,1])   # update of theta
  }
  
  df_res <- tibble(p = res[,1],
                   theta = res[,2]) %>% 
    mutate(t = 1:Time)
  
  return(df_res)
}
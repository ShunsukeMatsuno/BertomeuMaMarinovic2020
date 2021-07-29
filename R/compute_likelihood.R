compute_likelihood <- function(param, df, xi_random, x_random){
  # @description 
  #  This function takes parameters and data as input and returns the likelihood according to 
  #   the model.
  # 
  # @note
  #  sigma_e is estimated by sd(sigma_e), so it is not included in the parameter.
  #  `xi_random` is used to compute Gamma
  #  `x_random` is to compute likelihood
  
  # param = (alpha, beta, sigma_x, p_11, p_01)
  alpha <- param[1]
  beta <- param[2]
  sigma_x <- param[3]
  p_11 <- param[4]
  p_01 <- param[5]
  
  # estimate sigma_e
  sigma_e_hat <- df %>% 
    pull(e) %>% 
    sd()
  
  # estimate P_nd
  df_Pnd <- tibble(p = ((1:20)/21)^2) %>% 
    #slice(5:n()) %>% 
    mutate(P_nd = map_dbl(.x = p, .f = compute_fixedpt_Gamma,
                          xi_random = xi_random,
                          sigma_x = sigma_x, alpha = alpha, beta = beta)) 
  Pnd_splined <- splinefun(x = df_Pnd %>% pull(p),
                           y = df_Pnd %>% pull(P_nd))
  
  # compute log likelihood for each firm
  df_observed_nested <- df_observed %>% 
    nest(data = c(t, e, x, d)) %>% 
    rename(data_col = data) 
  
  ll_all <- map_dbl(.x = df_observed_nested$data_col,
        .f = ll_individual, x_random = x_random,
        alpha = alpha, beta = beta, sigma_x = sigma_x, sigma_e_hat = sigma_e_hat,
        p_11 = p_11, p_01 = p_01, Pnd_splined = Pnd_splined)
  
  # return the sum of the likelihood
  return(sum(ll_all))
}
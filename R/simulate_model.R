simulate_model <- function(alpha, beta, sigma_x, sigma_e, p_11, p_01, p_init = .3, 
                           N = 1e3, Time = 10, xi_random){
  # Simulate data that would have been observed for a set of parameters.
  
  # simulate p and theta
  df <- tibble(i = 1:N) %>% 
    mutate(df_p_theta = map(.x = i, .f = simulate_friction,
                            p_11 = p_11, p_01 = p_01, 
                            p_init = p_init, Time = Time)) %>% 
    unnest(df_p_theta) %>% 
    select(i, t, p, theta)
  
  # simulate x and e
  df <- df %>% 
    mutate(e = rnorm(N*Time, sd = sigma_e)) %>% 
    mutate(x = rnorm(N*Time, mean = (sigma_x^2)/(sigma_e^2) * e,
                     sd   = sigma_x^2 * (1 - (sigma_x^2)/(sigma_e^2)))) %>% 
    mutate(x = if_else(theta == 0, x, NA_real_))    # delete x for uninformed
  
  
  
  # compute fixed points for each p
  df_Pnd <- tibble(p = ((1:20)/21)^2) %>% 
    mutate(P_nd = map_dbl(.x = p, .f = compute_fixedpt_Gamma,
                          sigma_x = sigma_x, alpha = alpha, beta = beta,
                          xi_random = xi_random)) 
  Pnd_splined <- splinefun(x = df_Pnd %>% pull(p),
                           y = df_Pnd %>% pull(P_nd))
  
  # compute P_nd for each p
  df <- df %>% 
    mutate(P_nd = map_dbl(.x = p, .f = Pnd_splined))
  
  # simulate disclosure decision
  df <- df %>% 
    mutate(u = alpha * (x - P_nd) + beta + rnorm(N*Time)) %>% 
    mutate(d = if_else(theta == 1 | u < 0, 0, 1)) %>% 
    mutate(x = if_else(d == 1, x, NA_real_))
  
  return(list(df = df %>% select(i, t, e, x, d),
              Pnd = Pnd_splined))
}
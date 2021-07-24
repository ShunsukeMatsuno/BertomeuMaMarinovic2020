compute_fixedpt_Gamma <- function(p, sigma_x, alpha, beta, max_iter = 500){
  # @description
  # Computes the fixed point of Gamma(y; p). In order to compute the fixed point of Gamma later,
  #  we use spline method to obtain continuous approximation of Gamma.
  # @note 
  # The initial value is important. We set y_init = -0.25 + 0.2 * p
  
  
  # spline approximation of Gamma
  df_Gamma <- tibble(y = seq(-2, .5, .1)) %>% 
    mutate(Gamma_val = map_dbl(.x = y, .f = compute_Gamma,
                               p = p, sigma_x = sigma_x, alpha = alpha, beta = beta)) 
  Gamma_splined <- splinefun(x = df_Gamma %>% pull(y),
                             y = df_Gamma %>% pull(Gamma_val))
  f <- function(x) Gamma_splined(x) - x
  res <- rootSolve::multiroot(f, start = -0.25 + 0.2 * p)
  return(res$root)
  
  # tolerance <- 1e-4
  # error <- 100
  # count <- 0
  # y_old <- -.25 + .2 * p   # initial value
  # while(error > tolerance & count < max_iter){
  #   count <- count + 1
  #   y_new <- Gamma_splined(y_old)
  #   print(y_new)
  #   error <- abs(y_new - y_old)
  #   y_old <- y_new   # update
  # }
  # if(count == max_iter){
  #   print("convergence failed!!!")
  # }
  # 
  # return(y_old)
}
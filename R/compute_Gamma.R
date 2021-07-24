compute_Gamma <- function(p, sigma_x, alpha, beta,
                          interval = c(-1.5, .5)){
  # @description
  #  Returns the function that defines P_nd as a fixed point. 
  #  Uses spline interpolation and returns the splined function
  
  
  # integrand
  g <- function(xi, y, p, sigma_x, alpha, beta){
    (xi - y/sigma_x) * pnorm(-alpha * sigma_x * xi + alpha * y - beta) * sigma_x
  }
  g <- Vectorize(g, "xi")
  
  # Integral
  compute_Gamma_val <- function(y, xi, 
                        p, sigma_x, alpha, beta){
    # returns Gamma for each y
    I <- mean(g(xi, y, p, sigma_x, alpha, beta))
    return((1-p)/p * I)
  }
  
  # simulate normal variables and take mean
  xi <- rnorm(1e4)
  df_Gamma <- tibble(y = seq(interval[1], interval[2], .1)) %>% 
    mutate(Gamma_val = map_dbl(.x = y, .f = compute_Gamma_val, xi = xi,
                               p = p, sigma_x = sigma_x, alpha = alpha, beta = beta)) 
  Gamma_splined <- splinefun(x = df_Gamma %>% pull(y),
                             y = df_Gamma %>% pull(Gamma_val))
  
  return(list(df = df_Gamma, fun = Gamma_splined))
}


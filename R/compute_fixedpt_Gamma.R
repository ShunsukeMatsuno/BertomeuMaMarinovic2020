compute_fixedpt_Gamma <- function(p, sigma_x, alpha, beta, max_iter = 500){
  # @description
  # Computes the fixed point of Gamma(y; p). In order to compute the fixed point of Gamma later,
  #  we use spline method to obtain continuous approximation of Gamma.
  # @note 
  # The initial value is important. We set y_init = -0.25 + 0.2 * p
  
  # splined gamma
  Gamma_splined <- compute_Gamma(p, sigma_x, alpha, beta)$fun
  
  f <- function(x) Gamma_splined(x) - x
  res <- rootSolve::multiroot(f, start = -0.25 + 0.2 * p)
  return(res$root)
  
}
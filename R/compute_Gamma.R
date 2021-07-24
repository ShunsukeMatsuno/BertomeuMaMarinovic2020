compute_Gamma <- function(y, p, sigma_x, alpha, beta){
  # The function that defines P_nd as a fixed point.
  
  # integrand
  g <- function(xi, y, p, sigma_x, alpha, beta){
    (xi - y/sigma_x) * pnorm(-alpha * sigma_x * xi + alpha * y - beta) * sigma_x
  }
  g <- Vectorize(g, "xi")
  
  # simulate normal variables and take mean
  xi <- rnorm(1e4)
  I <- mean(g(xi, y, p, sigma_x, alpha, beta))
  
  # return the value of gamma
  return((1-p)/p * I)
}
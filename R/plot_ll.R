plot_ll <- function(param, df_observed, plot_param_index, points_plot = 16){
  # param = (alpha, beta, sigma_x, p_11, p_01)
  
  sigma_e_hat <- sd(df_observed %>% pull(e))
  
  param_vec <- case_when(
    plot_param_index == 1 ~ seq(1, 5, length = points_plot), # alpha
    plot_param_index == 2 ~ seq(-3, 3, length = points_plot), # beta
    plot_param_index == 3 ~ seq(.1, sigma_e_hat*.99, length = points_plot), # sigma_x
    plot_param_index == 4 ~ seq(0.001, 1, length = points_plot), # p_11
    plot_param_index == 5 ~ seq(0.001, 1, length = points_plot) # p_01
  )

  # # sequential
  # res <- numeric(points_plot)
  # for(i in seq_along(res)){
  #   print(i)
  #   param_temp <- param
  #   param_temp[plot_param_index] <- param_vec[i]
  #   print(param_temp)
  #   res[i] <- compute_likelihood(param_temp, df_observed)
  # }
  
  # set up of parallel computation
  library(doParallel)
  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)
  clusterExport(cl, "df_observed")

  # compute likelihood for each parameter
  res <- foreach(i = seq_along(param_vec),
                 .packages = c("tidyverse", "BertomeuMaMarinovic"),
                 .combine = rbind) %dopar%{
                   param_temp <- param
                   param_temp[plot_param_index] <- param_vec[i]
                   BertomeuMaMarinovic::compute_likelihood(param_temp, df_observed)
                 }

  # delete finished tasks
  parallel::stopCluster(cl)
  
  
  # label of plot
  p <- tibble(x = param_vec, y = res) %>% 
    ggplot(aes(x, y)) + 
    geom_point(size = 2) +
    geom_vline(xintercept = param[plot_param_index], lty = 2) + 
    xlab(c("alpha", "beta", "sigma_x", "p_11", "p_01")[plot_param_index]) +
    theme_bw()
  plot(p)
  
}
prepare_moca_long_adj_gamma <- function (data) {
  
  # plot(density(data$moca.score))
  data$moca.score.gamma <- (data$moca.score - 42) * -1
  data$moca_baseline_gamma <- (data$moca_baseline - 42) * -1

  ##### scaling variables
  
  .num_vars <- c("age_t0", "nihss_score_w1", "aerobic_score_w1",
                 "strength_score_w1", "CCMI_score_t0", "madrs_w1")
  .scales <- build_scales(dataSet = data, cols = c(.num_vars), 
                          verbose = TRUE)
  data <- fastScale(dataSet = data, scales = .scales, 
                             verbose = TRUE)
  .moca_sc <- build_scales(dataSet = data, 
                           cols = "moca_baseline_gamma", 
                           verbose = TRUE)
  data <- fastScale(dataSet = data, 
                    scales = .moca_sc, 
                    verbose = TRUE)
  
}

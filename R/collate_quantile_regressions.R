collate_quantile_regressions <- function (data,
                                          selected_variables) {
  
  w1 <- fit_quantile_regression(data,
                                selected_variables,
                                response_variable = 'moca_score_w1')
  mo3 <- fit_quantile_regression(data,
                                 selected_variables,
                                 response_variable = 'moca_score_mo3',
                                 other_variables = 'moca_score_w1')
  mo12 <- fit_quantile_regression(data,
                                  selected_variables,
                                  response_variable = 'moca_score_mo12',
                                  other_variables = 'moca_score_w1')
  
  qr <- rbind(w1, "", mo3, "", mo12) 
  qr$p.value <- as.numeric(as.character(qr$p.value))
  
  qr_filtered <- qr %>% 
    filter(p.value < .05) %>% 
    mutate(p.value = ifelse(p.value != "" & p.value < 0.001, 
                            "<.001", p.value))
}

fit_quantile_regression <- function (data,
                                     selected_variables,
                                     response_variable,
                                     other_variables = NULL) {
  
  subsetted_data <- data %>% 
    select(response_variable,
           selected_variables,
           other_variables) %>%
    na.omit()
  
  .stats <- lapply(names(subsetted_data)[-1],
                function(var) {
                  formula <- as.formula(paste(response_variable, " ~", var))
                  res.qr <- summary(rq(formula, 
                                       data = subsetted_data, 
                                       method = "fn"), 
                                    se = "boot")
                  stats <- matrix(c(
                    res.qr$coefficients[-1, 1] +
                      qnorm(c(0.5, 0.025, 0.975)) * res.qr$coefficients[-1, 2],
                    res.qr$coefficients[-1, 4]), ncol = 4,
                    dimnames = list(c(var),
                                    c("beta", "lb", "ub", "p-value"))) %>%
                    round(3)
                  stats <- data.frame(
                    cbind("var" = var,
                          "beta" = paste0(stats[,'beta'], " (",
                                          stats[,'lb'], " to ",
                                          stats[,'ub'], ")"),
                          "p.value" = stats[,"p-value"]))
                })
  
  .stats.out <- data.frame(purrr::map_df(.stats, bind_rows))
}


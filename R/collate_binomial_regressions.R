collate_binomial_regressions <- function (data,
                                          selected_variables) {
  
  w1 <- fit_binomial_regression(data,
                                selected_variables,
                                response_variable = 'impaired_w1')
  mo3 <- fit_binomial_regression(data,
                                 selected_variables,
                                 response_variable = 'impaired_mo3',
                                 other_variables = 'moca_score_w1')
  mo12 <- fit_binomial_regression(data,
                                  selected_variables,
                                  response_variable = 'impaired_mo12',
                                  other_variables = 'moca_score_w1')
  
  blm <- rbind(w1, "", mo3, "", mo12) 
  blm$p.value <- as.numeric(as.character(blm$p.value))
  
  blm_filtered <- blm %>% 
    filter(p.value < .05) %>% 
    mutate(p.value = ifelse(p.value != "" & p.value < 0.001, 
                            "<.001", p.value))
  
}

fit_binomial_regression <- function (data,
                                     selected_variables,
                                     response_variable,
                                     other_variables = NULL) {
  
  subsetted_data <- data %>% 
    select(response_variable,
           selected_variables,
           other_variables) %>%
    na.omit()
  
  .ORs <- lapply(names(subsetted_data)[-1],
                 function (var) {
                   formula <- as.formula(
                     paste(response_variable, " ~", var))
                   res.logist <- glm(formula, 
                                     data = subsetted_data, 
                                     family = binomial)
                   # summary(res.logist, digits = 3)
                   # confint(res.logist)
                   ORs <- matrix(c(
                     exp(summary(res.logist)$coefficients[-1, 1] +
                           qnorm(c(0.5, 0.025, 0.975)) * 
                           summary(res.logist)$coefficients[-1, 2]),
                     summary(res.logist)$coefficients[-1, 4]), 
                     ncol = 4,
                     dimnames = list(c(var),
                                     c("OR", "lb", "ub", "p-value"))) %>%
                     round(3)
                   
                   ORs <- cbind(var,
                     "odds.ratio" = paste0(ORs[, 'OR'], 
                                           " (", ORs[, 'lb'],
                                           " to ", ORs[, 'ub'], 
                                           ")"),
                     "p.value" = ORs[, "p-value"])
                   
                   ORs <- data.frame(ORs)
                 })
  
  .ORS <- data.frame(purrr::map_df(.ORs, bind_rows))
}

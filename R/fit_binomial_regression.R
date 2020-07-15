binomial_regression <- function (categorized_moca,
                                 selected_variables,
                                 response_variable) {
  
  data <- categorized_moca %>% 
    select(response_variable,
           all_of(selected_variables)) %>%
    na.omit()
  
  .ORs <- lapply(names(data)[-1],
                 function (var) {
                   formula <- as.formula(
                     paste(response_variable, " ~", var))
                   res.logist <- glm(formula, 
                                     data = data, 
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

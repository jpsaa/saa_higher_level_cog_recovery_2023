#### Baseline BLR
fit_baseline_blr <- function (categorized_moca) {
  
  blr1 <- categorized_moca %>% 
    select(impaired_w1,
           .all.vars) %>%
    na.omit()
  
  .ORs <- lapply(names(blr1)[-1],
              function (var) {
                formula <- as.formula(paste("impaired_w1 ~", var))
                res.logist <- glm(formula, data = blr1, family = binomial)
                # summary(res.logist, digits = 3)
                # confint(res.logist)
                ORs <- matrix(c(
                  exp(summary(res.logist)$coefficients[-1, 1] +
                        qnorm(c(0.5, 0.025, 0.975)) * 
                        summary(res.logist)$coefficients[-1, 2]),
                  summary(res.logist)$coefficients[-1, 4]), ncol = 4,
                  dimnames = list(c(var),
                                  c("OR", "lb", "ub", "p-value"))) %>%
                  round(3)
                ORs <- cbind(var,
                             "odds.ratio" = paste0(ORs[, 'OR'], " (", ORs[, 'lb'],
                                                   " to ", ORs[, 'ub'], ")"),
                             "p.value" = ORs[, "p-value"])
                
                ORs <- data.frame(ORs)
              })
  
  .ORS.w1 <- data.frame(purrr::map_df(.ORs, bind_rows))

}
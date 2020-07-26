summarise_mixed_lqmm_adj <- function (moca.long.model) {
  
  .mixed <- lqmm(fixed = as.formula(paste("moca.score ~",
                                          paste(names(moca.long.model)[-(1:3)], 
                                                collapse = "+"), -1)),
                 random = ~ 1, group = id,
                 data = moca.long.model, tau = 0.5, nK = 11, type = "normal",
                 na.action = na.omit)
  
  lqmm.mixed.adjusted.summary <- summary(.mixed)$tTable
  
  ####adjusting p-values
  .p.adjusted <- p.adjust(lqmm.mixed.adjusted.summary[, 5], 
                          method = "bonferroni" )
  
  ##### simplified summary table
  lqmm.mixed.adjusted.summary <- matrix(c(
    lqmm.mixed.adjusted.summary[, "Value"],
    lqmm.mixed.adjusted.summary[, "lower bound"],
    lqmm.mixed.adjusted.summary[, "upper bound"],
    lqmm.mixed.adjusted.summary[, "Pr(>|t|)"]), 
    ncol = 4,
    dimnames = list(c(rownames(lqmm.mixed.adjusted.summary)),
                    c("beta", "lb", "ub", "p-value"))) %>%
    round(3)
  
  #### simplify further
  lqmm.mixed.adjusted.summary <- cbind(
    "estimate (95% CI)" = paste0(lqmm.mixed.adjusted.summary[, 'beta'], " (",
                               lqmm.mixed.adjusted.summary[, 'lb'], " to ",
                               lqmm.mixed.adjusted.summary[, 'ub'], ")"),
    "p-val" = lqmm.mixed.adjusted.summary[, "p-value"],
    "p-val adjusted" = round(.p.adjusted, 3))
  lqmm.mixed.adjusted.summary
  
} 
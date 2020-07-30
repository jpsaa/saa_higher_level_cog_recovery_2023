fit_lqmm_not_adj <- function (moca.long) {
  
  mixed.vars <- names(moca.long)[-grep("id|moca.|time",
                                       names(moca.long))]
  
  stats <- lapply(mixed.vars, model_lqmm_not_adj) %>%
    purrr::map_df(., bind_rows) %>%
    data.frame
  
  stats$p.value <- as.numeric(as.character(stats$p.value))
  
  out <- stats %>% 
    filter(p.value < .05) %>% 
    mutate(p.value = ifelse(p.value != "" & p.value < 0.001, 
                            "<.001", p.value))

}

model_lqmm_not_adj <- function (var) {
  
  formula <- as.formula(paste("moca.score ~", var))
  
  mixed <- lqmm(fixed = eval(formula), random = ~ 1, group = id,
                data = moca.long, tau = 0.5, nK = 11, type = "normal",
                na.action = na.omit
                # control = lqmmControl(LP_max_iter = 1000,
                #                       LP_tol_ll = 1e-04)
  )
  mixed$call$fixed <- formula
  stats <- summary(mixed)$tTable
  
  stats <- matrix(c(stats[-1,"Value"], 
                    stats[-1,"lower bound"], 
                    stats[-1,"upper bound"],
                    stats[-1,"Pr(>|t|)"]), 
                  ncol = 4,
                  dimnames = list(c(var),
                                  c("beta", "lb", 
                                    "ub", "p-value"))) %>%
    round(3)
  
  stats <- data.frame(
    cbind("var" = var,
          "beta" = paste0(stats[, 'beta'], " (", 
                          stats[,'lb'], " to ",
                          stats[, 'ub'], ")"),
          "p.value" = stats[, "p-value"]))
}

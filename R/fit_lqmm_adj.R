fit_lqmm_adj <- function (moca.long.adj) {
  
  mixed.vars <- names(moca.long.adj)[-grep("id|moca.|time",
                                       names(moca.long.adj))]
  
  .stats <- lapply(mixed.vars, model_lqmm_adj) %>%
    purrr::map_df(., bind_rows) %>%
    data.frame
  
  # .stats <- data.frame(purrr::map_df(.stats, bind_rows))
  out <- .stats %>% 
    filter(p.value < .05) %>% 
    mutate(p.value = ifelse(p.value!="" & p.value < 0.001, 
                            "<.001", p.value))

}

model_lqmm_adj <- function (var) {
  

    formula    <- as.formula(paste("moca.score ~", var, "+ moca_baseline"))
    mixed <- lqmm(fixed = eval(formula), random = ~ 1, group = id,
                  data = moca.long.adj, tau = 0.5, nK = 11, type = "normal",
                  na.action = na.omit
                  # control = lqmmControl(LP_max_iter = 1000,
                  #                       LP_tol_ll = 1e-04)
    )
    mixed$call$fixed <- formula
    stats=summary(mixed)$tTable
    pos = grep(var, rownames(stats))
    
    stats=matrix(c(
      stats[pos,"Value"],stats[pos,"lower bound"],stats[pos,"upper bound"],
      stats[pos,"Pr(>|t|)"]),ncol=4,
      dimnames = list(c(rownames(stats)[pos]),
                      c("beta","lb","ub","p-value"))) %>%
      round(3)
    stats=
      data.frame(cbind("var"=rownames(stats),
                       "beta"=paste0(stats[,'beta']," (",stats[,'lb']," to ",
                                     stats[,'ub'], ")"),
                       "p.value"=stats[,"p-value"]))

}

fit_gamma_adj <- function (data) {
  
  mixed.vars <- names(data)[-grep("id|moca.|time",
                                                 names(data))]
  
  .stats <- lapply(mixed.vars, model_gamma_adj, data)
  
  .stats <- data.frame(purrr::map_df(.stats, bind_rows))
  
  .stats %>% 
    filter(p.value < .05) %>% 
    mutate(p.value = ifelse(p.value!="" & 
                              p.value < 0.001, "<.001", p.value))
  
}

model_gamma_adj <- function (var, data) {
  
  mixed <- glmer(
    formula(trimws(paste("moca.score.gamma ~ ",
                         var, "+ offset(moca_baseline_gamma) + (1 | id)"))),
    data = data,
    family = Gamma(link = "identity"),
    control = glmerControl(optimizer = "optimx",
                         optCtrl = list(method = "nlminb",
                                        maxit = 1e9)))
  stats <- summary(mixed)$coefficients
  pos <- grep(var, rownames(stats))
  
  # table of estimates with 95% CI
  se <- sqrt(diag(vcov(mixed)))
  stats <- cbind("Value" = fixef(mixed), 
                 "lower bound" = fixef(mixed) - 1.96 * se, 
                 "upper bound" = fixef(mixed) + 1.96 * se, 
                 "p.value" = stats[pos, "Pr(>|z|)"])
  
  stats <- matrix(c(
    stats[pos, "Value"],
    stats[pos, "lower bound"],
    stats[pos, "upper bound"],
    stats[pos, "p.value"]),
    ncol = 4,
    dimnames = list(c(rownames(stats)[pos]),
                    c("beta", "lb", "ub", "p-value"))) %>%
    round(3)
  
  stats <- data.frame(
    cbind("var" = rownames(stats),
          "beta" = paste0(stats[, 'beta'], " (", stats[, 'lb'], " to ",
                        stats[, 'ub'], ")"),
          "p.value" = stats[, "p-value"]))
}

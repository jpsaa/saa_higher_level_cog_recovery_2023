fit_gamma_not_adj <- function(data) {
  
  data$moca.score.gamma <- (data$moca.score - 42)*-1
  
  
  .stats=lapply(mixed.vars,
                function(var) {
                  mixed <- glmer(
                    formula(trimws(paste("moca.score.gamma ~ ",
                                         var, "+ (1 | id)"))),
                    data=data,
                    family = Gamma(link = "identity"),
                    control=glmerControl(optimizer = "optimx",
                                         optCtrl = list(method = "nlminb",
                                                        maxit = 1e9)))
                  stats=summary(mixed)$coefficients
                  pos = grep(var, rownames(stats))
                  
                  
                  # table of estimates with 95% CI
                  se <- sqrt(diag(vcov(mixed)))
                  stats <- cbind("Value" = fixef(mixed), "lower bound" = fixef(mixed) - 1.96 * se, 
                                 "upper bound" = fixef(mixed) + 1.96 * se, "p.value"=stats[pos,"Pr(>|z|)"])
                  
                  stats=matrix(c(
                    stats[pos,"Value"],stats[pos,"lower bound"],stats[pos,"upper bound"],
                    stats[pos,"p.value"]),ncol=4,
                    dimnames = list(c(rownames(stats)[pos]),
                                    c("beta","lb","ub","p-value"))) %>%
                    round(3)
                  stats=
                    data.frame(cbind("var"=rownames(stats),
                                     "beta"=paste0(stats[,'beta']," (",stats[,'lb']," to ",
                                                   stats[,'ub'], ")"),
                                     "p.value"=stats[,"p-value"]))
                })
  
}

.stats <- fit_gamma_not_adj(moca.long)

fit_model <- function (pid, formula, data) {
  
  test_data <- data[data$id == pid, ]
  train_data <- data[data$id != pid, ]
  
  ##### Gamma models
  #### creating a list of optimizers to test if the first fails
  optimx_options <- c("nlminb", "bobyqa", "L-BFGS-B", "nmkb", 
                      "hjkb", "uobyqa", "newuoa", "nlm")
  
  for (i in 1:length(optimx_options)) {
    set.seed(1)
    
    mod.glmer <- glmer(
      formula(trimws(paste("moca.score.gamma ~ ",
                           formula, 
                           "+ moca_baseline_gamma + (1 | id) -1"))),
      data = train_data, 
      family = Gamma(link = "identity"),
      control=glmerControl(optimizer = "optimx",
                           optCtrl = list(method = optimx_options[i],
                                          maxit = 1e9)))
    if (is.null(mod.glmer@optinfo$conv$lme4$messages)) {
      print(paste0("Model converged with the, ", 
                   optimx_options[i], ", optimization!, pid = ",
                   pid," formula = ", formula ))
      # print(summary(mod.glmer))
      mod.glmer
      break
    }
    
  }
  
  #### getting predictions
  pred.glmer <- predict(mod.glmer, newdata = test_data, 
                        allow.new.levels = TRUE)
  
  mixed <- lqmm(fixed = as.formula(paste("moca.score ~",
                                         formula, "+ moca_baseline", -1)),
                random = ~ 1, group = id,
                data = train_data, tau = 0.5, nK = 11, type = "normal",
                na.action = na.omit)
  
  ###using the modified function
  pred.lqmm <- predict_lqmm_modified(mixed, newdata = test_data, level = 0)
  
  # rmse.fixed.ef <- RMSE(rbind(pred.lqmm.fixef), cbind(data[data$id==pid,"moca.score"])) %>% round(3)
  # rmse.raef <- RMSE(rbind(pred.lqmm.raef), cbind(data[data$id==pid,"moca.score"])) %>% round(3)
  rmse.glmer <- RMSE(pred.glmer, as.matrix(data[data$id==pid,"moca.score.gamma"])) %>% round(3)
  rmse.lqmm <- RMSE(pred.lqmm, as.matrix(data[data$id==pid,"moca.score"])) %>% round(3)
  
  out <- data.frame(formula = formula,
                    rmse.glmer = rmse.glmer,
                    rmse.lqmm = rmse.lqmm,
                    observed.gamma = as.matrix(data[data$id==pid,"moca.score.gamma"]) %>% 
                      paste(collapse = ", "),
                    predicted.gamma = pred.glmer %>% round (3) %>% paste(collapse = ", "),
                    observed.lqmm = as.matrix(data[data$id==pid,"moca.score"]) %>% 
                      paste(collapse = ", "),
                    predicted.lqmm = pred.lqmm %>% round (3) %>% 
                      paste(collapse = ", "),
                    patient = pid)
}

RMSE <- function (m, o) {
  sqrt(mean((m - o)^2))
}
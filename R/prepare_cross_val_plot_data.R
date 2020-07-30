prepare_cross_val_plot_data <- function (mod_comp) {
  
  n <- cbind(mod_comp[-grep("observed|predict", names(mod_comp))],
            "observed.gamma" = do.call(rbind, strsplit(as.character(mod_comp$observed.gamma), ", ")) %>% 
              data.frame(stringsAsFactors = FALSE) %>% 
              sapply(as.numeric) %>% 
              unname,
            "predicted.gamma" = do.call(rbind, strsplit(as.character(mod_comp$predicted.gamma), ", ")) %>% 
              data.frame(stringsAsFactors = FALSE) %>% 
              sapply(as.numeric) %>% 
              unname,
            "observed.lqmm" = do.call(rbind, strsplit(as.character(mod_comp$observed.lqmm), ", ")) %>% 
              data.frame(stringsAsFactors = FALSE) %>% 
              sapply(as.numeric) %>% 
              unname,
            "predicted.lqmm" = do.call(rbind, strsplit(as.character(mod_comp$predicted.lqmm), ", ")) %>% 
              data.frame(stringsAsFactors = FALSE) %>% 
              sapply(as.numeric) %>% 
              unname) 
  
  ### making formulas a categorical (factor) variable
  n$formula <- factor(n$formula)
  
  ## data in long format
  op <- data.frame(
    melt(n %>% select(patient, grep("obser", names(.))), id.vars = "patient"),
    melt(n %>% select(patient, grep("predict", names(.))), id.vars = "patient"),
    n %>% select(formula, rmse.glmer, rmse.lqmm))
  
  op <- op[order(op$patient),]
  
  names(op)[grep("value", names(op))] <- c("observed", "predicted")
  
  op <- op[, -grep("patient.1|variable.1", names(op))]
  names(op)[grep("variable", names(op))] <- c("model.time")
  levels(op$model.time) <- c("Gamma models at 3 months", 
                             "Gamma models at 12 months",
                             "Lqmm models at 3 months", 
                             "Lqmm models at 12 months")
  
  op <- data.frame(
    melt(op %>% select(patient, grep("rmse", names(.))), id.vars = "patient"),
    op %>% select(names(op)[-grep("rmse|patient", names(op))]))
  names(op)[grep("variable|value", names(op))] = c( "rmse.type", "rmse")
  
  op$model <- ifelse(grepl('Gamma', op$model.time), 
                     "Gamma regression", 
                     "Mixed-quantile regression" )
  
  op
}

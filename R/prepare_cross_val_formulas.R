prepare_cross_val_formulas <- function (data) {
  
  ## separating numerical and categorical variables to be analyzed (from previous model selection steps)
  names(data) <- gsub("\\.| |  ", "_", names(data))
  .num_vars <- names(data)[grep("age_|nihss_|strength",
                                        names(data))]
  .fact_vars <- names(data)[grep("gender|educ|tia_|ht_|disab_|ethn|smok",
                                         names(data))]
  
  # making all possible permutations of variables extracted above
  formulas <- expand.grid(list(
    paste(t(combn(.fact_vars, 1, simplify = TRUE))[, 1]),
    paste(t(combn(.num_vars, 2, simplify = TRUE))[, 1], "+",
          t(combn(.num_vars, 2, simplify = TRUE))[, 2])))
  
  # adding time to the formulas
  formulas <- paste("time", "+", formulas$Var1, "+", formulas$Var2)
  # formulas.no.time<- gsub("time \\+ ", "", formulas)
  
}


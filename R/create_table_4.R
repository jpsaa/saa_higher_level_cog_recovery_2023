#### Table 4 - overall improvers and improved declined

create_table_4 <- function (data,
                            columns) {
 
  sga <- data %>% 
    filter(trends == "1.overall improver" | 
             trends == "3.improved-declined") %>%
    select(trends, gender_w1, educ_binary, 
           marital_status_binary_w1, disab_prestroke, all_of(columns))
  
  sga.desc <- sga %>% 
    group_by(trends) %>%
    summarise_if(is.numeric, 
                 funs(sum(!is.na(.)), median, IQR),
                 na.rm = TRUE)
  
  counts <- names(sga.desc)[grep("_sum", names(sga.desc))]
  medians <- names(sga.desc)[grep("median", names(sga.desc))]
  iqrs <- names(sga.desc)[grep("IQR", names(sga.desc))]
  
  table4 <- matrix(nrow = nrow(sga.desc))
  
  for (i in seq_along(medians)) {
    
    #### looking for NA values and turning them into spaces
    .counts <- which(names(sga.desc) == counts[i])
    .med <- which(names(sga.desc) == medians[i])
    .iqr <- which(names(sga.desc) == iqrs[i])
    
    table4 <- cbind(
      table4, 
      cbind(
        paste0(
          round(as.numeric(unlist(sga.desc[, .counts])), 2), ", ",
          round(as.numeric(unlist(sga.desc[, .med])), 2), 
          " (", 
          paste0(round(as.numeric(unlist(sga.desc[, .iqr])), 2), 
                 ")"))))
    table4[grep("NA", table4[, i + 1]), i + 1] = ""
    
  }
  
  table4 <- table4[, -1]
  table4 <- t(table4)
  colnames(table4) <- levels(factor(sga$trends))
  
  #### applying wilcoxon test accross all columns and groupping by trend (overall improver and improver declined)
  cols <- sga %>% 
    select(-grep("gender|trends|educ_|marital_|disab_pres", 
                 names(.))) %>% 
    names
  
  sigs <- list()
  c1 <- c("Variable", "z-value 1", "95% CI 1", "p-value 1")
  .tests <- matrix(ncol = length(c1), 
                nrow = length(cols))
  colnames(.tests) <- c1
  
  for(i in seq_along(cols)) {
    sigs[[i]] <- wilcox_test(
      sga[, which(names(sga) == cols[i])] ~ factor(trends), 
      data = sga, 
      distribution = "exact", 
      zero.method = "Pratt", 
      conf.int = TRUE
    )
    
    .tests[i, "Variable"] <- cols[i]
    .tests[i, "z-value 1"] <- round(statistic(sigs[[i]]), 3)
    .tests[i, "95% CI 1"] <- paste0(
      round(confint(sigs[[i]])$conf.int[1], 3), ", ", 
      round(confint(sigs[[i]])$conf.int[2], 3))
    .tests[i, "p-value 1"] <- round(pvalue(sigs[[i]]), 3)
    
  }
  
  table4 <- cbind(.tests[, "Variable"], table4, 
                  .tests[, -which(colnames(.tests) == "Variable")])
  
  #### adding count variables
  table4 <- rbind(
    c("n", get_stats(sga, gender_w1), "", "", ""), 
    t(c("males", get_stats(sga, gender_w1, "male"),
        fisher.stats(sga, gender_w1) %>% unname)),
    t(c("education", get_stats(sga, educ_binary, "secondary or more"), 
        fisher.stats(sga, educ_binary))),
    t(c("marital status", get_stats(sga, marital_status_binary_w1, "married"), 
        fisher.stats(sga, marital_status_binary_w1) %>% unname)), 
    t(c("prestroke disab", get_stats(sga, disab_prestroke, "some disab"), 
        fisher.stats(sga, disab_prestroke) %>% unname)), 
    table4)
  
  table4[, 1] <- trimws(gsub("median|t0|.y|_", " ", table4[, 1]))
  
  ##### comparison overall improver vs overall decliner
  sga2 <- data %>% 
    filter(trends == "1.overall improver" | 
             trends == "9.overall decliner") %>%
    select(trends, gender_w1, educ_binary, 
           marital_status_binary_w1, disab_prestroke, all_of(columns))
  
  sga.desc <- sga2 %>% 
    group_by(trends) %>%
    summarise_if(is.numeric, 
                 funs(sum(!is.na(.)), median, IQR), 
                 na.rm = TRUE)
  
  counts <- names(sga.desc)[grep("_sum", names(sga.desc))]
  medians <- names(sga.desc)[grep("median", names(sga.desc))]
  iqrs <- names(sga.desc)[grep("IQR", names(sga.desc))]
  
  .table4 <- matrix(nrow = nrow(sga.desc))
  
  for (i in seq_along(medians)) {
    
    #### looking for NA values and turning them into spaces
    .counts <- which(names(sga.desc) == counts[i])
    .med <- which(names(sga.desc) == medians[i])
    .iqr <- which(names(sga.desc) == iqrs[i])
    .table4 <- cbind(
      .table4, 
      cbind(
        paste0(
          round(as.numeric(unlist(sga.desc[, .counts])), 2), ", ", 
          round(as.numeric(unlist(sga.desc[, .med])), 2), 
          " (", 
          paste0(round(as.numeric(unlist(sga.desc[, .iqr])), 2), 
                 ")"))))
    .table4[grep("NA", .table4[, i+1]), i+1] <- ""
    
  }
  
  .table4 <- .table4[, -1]
  .table4 <- t(.table4)
  colnames(.table4) <- levels(factor(sga2$trends))
  
  #### applying wilcoxon test accross all columns and groupping by trend (overall improver and improver declined)
  sigs <- list()
  c1 <- c("Variable", "z-value 2", "95% CI 2", "p-value 2")
  .tests <- matrix(ncol = length(c1), 
                nrow = length(cols))
  colnames(.tests) <- c1
  
  for(i in seq_along(cols)) {
    sigs[[i]] <- wilcox_test(
      sga2[, which(names(sga2) == cols[i])] ~ factor(trends), 
      data = sga2, 
      distribution = "exact", zero.method = "Pratt", conf.int = TRUE
    )
    .tests[i, "Variable"] <- cols[i]
    .tests[i, "z-value 2"] <- round(statistic(sigs[[i]]), 3)
    .tests[i, "95% CI 2"] <- paste0(round(confint(sigs[[i]])$conf.int[1], 3), ", ", 
                                round(confint(sigs[[i]])$conf.int[2], 3))
    .tests[i, "p-value 2"] <- round(pvalue(sigs[[i]]), 3)
    
  }
  
  .tests <- cbind(.tests[, "Variable"], .table4, 
                  .tests[, -which(colnames(.tests) == "Variable")])
  
  #### adding count variables
  .tests <- rbind(
    c("n", get_stats(sga2, gender_w1), "", "", ""), 
    t(c("males", get_stats(sga2, gender_w1, "male"),
        fisher.stats(sga2, gender_w1) %>% unname)),
    t(c("education", get_stats(sga2, educ_binary, "secondary or more"), 
        fisher.stats(sga2, educ_binary))),
    t(c("marital status", get_stats(sga2, marital_status_binary_w1, "married"), 
        fisher.stats(sga2, marital_status_binary_w1) %>% unname)), 
    t(c("prestroke disab", get_stats(sga2, disab_prestroke, "some disab"), 
        fisher.stats(sga2, disab_prestroke) %>%unname)), 
    .tests)
  
  table4 <- cbind(table4, .tests[, 3:6])
  colnames(table4)[which(colnames(table4) == "")] <- 
    c("variables", "1.overall improver", "3.improved-declined", "9.overall decliner")
  
  table4 <- apply(table4, 2, as.character)
  
}

#### count data values
fisher.stats <- function(x, var){
  calc <- x %>% 
    select(trends, {{var}}) %>% 
    group_by(trends) %>%
    table() %>% 
    t() %>% 
    fisher.test()
  est <- unname(round(calc$estimate, 3))
  lb <- round(calc$conf.int[1], 3)
  ub <- round(calc$conf.int[2], 3)
  p <- round(calc$p.value, 3)
  tibble(`z-value` = est, 
         `95% CI` = paste0(lb, ", ", ub),
         `p-value` = p)
}

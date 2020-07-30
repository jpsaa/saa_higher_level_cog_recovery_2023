# moca over time
create_table_2 <- function (data) {

  ### calculating descriptive statistics for moca domains
  moca.w1 <- data %>% 
    select(grep("moca_score|_total", names(.))) %>%
    select(grep("_w1", names(.))) %>% 
    describe(IQR = TRUE) %>%
    select(n, median, IQR)
  
  moca.mo3 <- data %>% 
    select(grep("moca_score|_total", names(.))) %>%
    select(grep("_mo3", names(.))) %>% 
    select(-grep("total_", names(.))) %>% 
    describe(IQR = TRUE) %>%
    select(n, median, IQR)
  
  moca.mo12 <- data %>% 
    select(grep("moca_score|_total", names(.))) %>%
    select(grep("_mo12", names(.))) %>% 
    select(-grep("total_", names(.))) %>%
    describe(IQR = TRUE) %>%
    select(n, median, IQR)
  
  table2 <- cbind("Baseline Median (IQR)" = 
                    paste0(moca.w1[, 2], " (", moca.w1[, 3], ")"),
                  "3 Months Median (IQR)" =
                    paste0(moca.mo3[, 2], " (", moca.mo3[, 3], ")"),
                  "z-val.1 (95% CI)" = NA,
                  "p-value 1" = NA,
                  "12 Months Median (IQR)" = 
                    paste0(moca.mo12[, 2], " (", moca.mo12[, 3], ")"),
                  "z-val.2 (95% CI)" = NA,
                  "p-value 2" = NA,
                  "z-val.3 (95% CI)" = NA,
                  "p-value 3" = NA)
  
  ### calculating differences between groups
  pratt1 <- list()
  pratt2 <- list()
  pratt3 <- list()
  
  ### loop below takes really really long
  for (i in seq_along(rownames(moca.w1))) {
    pratt1[[i]] <- wsrTest(data[, which(names(data) == rownames(moca.w1)[i])],
                           data[, which(names(data) == rownames(moca.mo3)[i])])
    
    pratt2[[i]] <- wsrTest(data[, which(names(data) == rownames(moca.mo3)[i])],
                           data[, which(names(data) == rownames(moca.mo12)[i])])
    
    pratt3[[i]] <- wsrTest(data[, which(names(data) == rownames(moca.w1)[i])],
                           data[, which(names(data) == rownames(moca.mo12)[i])])
    
    table2[i, "p-value 1"] <- round(as.numeric(pratt1[[i]]["p.value"]), 3)
    table2[i, "z-val.1 (95% CI)"] <- 
      paste0(round(unlist(pratt1[[i]]['estimate']), 2), " (",
             paste0(round(unlist(pratt1[[i]]['conf.int']), 2), 
                    collapse = ", "), ")")
    
    table2[i, "p-value 2"] <- round(as.numeric(pratt2[[i]]["p.value"]), 3)
    table2[i, "z-val.2 (95% CI)"] <- 
      paste0(round(unlist(pratt2[[i]]['estimate']), 2), " (",
             paste0(round(unlist(pratt2[[i]]['conf.int']), 2),
                    collapse = ", "), ")")
    
    table2[i, "p-value 3"] <- round(as.numeric(pratt3[[i]]["p.value"]), 3)
    table2[i, "z-val.3 (95% CI)"] <- 
      paste0(round(unlist(pratt3[[i]]['estimate']), 2), " (",
             paste0(round(unlist(pratt3[[i]]['conf.int']), 2),
                    collapse = ", "), ")")
  }
  
  
  ### Fixing p-values that are close to 0
  
  table2[, "p-value 1"] <- ifelse(table2[, "p-value 1"] == 0, 
                                  "<.001", table2[, "p-value 1"])
  table2[, "p-value 2"] <- ifelse(table2[, "p-value 2"] == 0, 
                                  "<.001", table2[, "p-value 2"])
  table2[, "p-value 3"] <- ifelse(table2[, "p-value 3"] == 0, 
                                  "<.001", table2[, "p-value 3"])
  
    ### naming the columns
  table2 <- cbind(`Item (range)` =
                    c("Overall score", "Ex. Fun./Visuosp", "Naming",
                      "Attention", "Language", "Abstraction",
                      "Recall", "Orientation"),
                  table2)
}

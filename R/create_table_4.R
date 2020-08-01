### Table 4
create_table_4 <- function (data,
                            columns) {
  
  t4 <- data %>% 
    select(columns) %>% 
    group_by(trends) %>%
    summarise_if(is.numeric, 
                 list(~ sum(., na.rm = TRUE), 
                      ~ median(., na.rm = TRUE), 
                      ~ IQR(., na.rm = TRUE)))
  
  ### getting descriptive stats for all variables
  counts <- names(t4)[grep("sum", names(t4))]
  medians <- names(t4)[grep("median", names(t4))]
  iqrs <- names(t4)[grep("IQR", names(t4))]
  table4 <- matrix(nrow = nrow(t4))
  
  for (i in seq_along(medians)) {
    
    #### looking for NA values and turning them into spaces
    .counts <- which(names(t4) == counts[i])
    .med <- which(names(t4) == medians[i])
    .iqr <- which(names(t4) == iqrs[i])
    
    table4 <- cbind(
      table4,
      cbind(
        paste0(round(as.numeric(unlist(t4[, .counts])), 2), ", ",
               round(as.numeric(unlist(t4[, .med])), 2), " (",
               paste0(round(as.numeric(unlist(t4[, .iqr])), 2), ")"))))
    table4[grep("NA", table4[, i + 1]), i + 1] = ""
    
  }
  
  table4 <- table4[, -1]
  table4 <- t(table4)
  
  table4 <- rbind(
    get_stats(data, id),
    get_stats(data, gender_w1, "male"),
    get_stats(data, educ_binary, "secondary or more"),
    get_stats(data, marital_status_binary_w1, "married"),
    get_stats(data, disab_prestroke, "some disab"),
    table4)
  colnames(table4) <- unlist(t4[, 1])
  
  ####rownames
  table4 <- cbind(Variable = c(
    "n (%)", "Males (%)", "Education",
    "Marital status", "mrs prestroke",
    trimws(gsub("median|t0|.y|_", " ", medians))
  ), table4)
  
  ### creating space between assessments
  cols <- c("stolic bp", 'moca', 'mmse', 'stroop',
            "ravens", 'time taken', 'lds', 'madrs',
            'nihss', 'barthel', 'mrs score', 
            'aerobic', 'acs', 'wsas', 'sis')
  .t4 <- matrix(ncol = ncol(table4))
  
  grab <- 1
  
  for (i in seq_along(cols)) {
    
    space <- first(grep(cols[i], table4[, 1])) - 1
    .t4 <- rbind(.t4, table4[grab:space, ], "")
    grab <- first(grep(cols[i], table4[, 1]))
    
  }
  
  #### adding the last category
  .t4 <- rbind(.t4[-1, ], 
               table4[grep("sis", table4[, 1]), ])

}

##### binding count variable stats
get_stats <- function (x, var, categ = NULL) {
  
  if (!is.null(categ)) {
    ### mapping zeroes
    miss <- cbind(x %>% select(trends), 
                  (x %>% select({{var}})) == {{categ}}) %>% 
      table %>%
      data.frame() %>% 
      filter({{var}} == TRUE, Freq == 0) %>% 
      select(trends, Freq)
    
    names(miss) <- c("trends","categ")
    
    #### getting values
    .nums <- x %>% 
      filter({{var}} == {{categ}}) %>%
      group_by(trends) %>% 
      summarize(categ = n()) %>% 
      rbind(miss) %>%
      arrange(trends) %>% 
      select(categ) %>% 
      unlist
    
    paste0(.nums, ' (', 
           round(.nums / nrow(x) * 100, 2),
           "%)")
    
  } else {
    
    paste0(
      x %>% 
        group_by(trends) %>% 
        select(trends) %>%
        summarize(var = n()) %>% 
        select(var) %>% 
        unlist(),
      " (",
      round(
        ((x %>% group_by(trends) %>% select(trends) %>%
            summarize(var = n()) %>% select(var) %>% 
            unlist()) / nrow(x)) * 100, 2),
      "%)")
  }
}

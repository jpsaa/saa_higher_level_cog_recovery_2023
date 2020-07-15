### Table 3
create_table_3 <- function (moca_with_trends,
                            table3_columns) {
  
  moca <- moca_with_trends %>% 
    select(id, moca_score_w1, moca_score_mo3,
           moca_score_mo12, trends)
  
  t3 <- moca_with_trends %>% 
    select(all_of(table3_columns)) %>% 
    group_by(trends) %>%
    summarise_if(is.numeric, 
                 funs(sum(!is.na(.)), median, IQR), 
                 na.rm = TRUE)
  
  ### getting descriptive stats for all variables
  counts <- names(t3)[grep("sum", names(t3))]
  medians <- names(t3)[grep("median", names(t3))]
  iqrs <- names(t3)[grep("IQR", names(t3))]
  table3 <- matrix(nrow = nrow(t3))
  
  for (i in seq_along(medians)) {
    
    #### looking for NA values and turning them into spaces
    .counts <- which(names(t3) == counts[i])
    .med <- which(names(t3) == medians[i])
    .iqr <- which(names(t3) == iqrs[i])
    
    table3 <- cbind(
      table3,
      cbind(
        paste0(round(as.numeric(unlist(t3[, .counts])), 2), ", ",
               round(as.numeric(unlist(t3[, .med])), 2), " (",
               paste0(round(as.numeric(unlist(t3[, .iqr])), 2), ")"))))
    table3[grep("NA", table3[, i + 1]), i + 1] = ""
    
  }
  
  table3 <- table3[, -1]
  table3 <- t(table3)
  
  table3 <- rbind(
    get_stats(moca_with_trends, id),
    get_stats(moca_with_trends, gender_w1, "male"),
    get_stats(moca_with_trends, educ_binary, "secondary or more"),
    get_stats(moca_with_trends, marital_status_binary_w1, "married"),
    get_stats(moca_with_trends, disab_prestroke, "some disab"),
    table3)
  colnames(table3) <- unlist(t3[, 1])
  
  ####rownames
  table3 <- cbind(Variable = c(
    "n (%)", "Males (%)", "Education",
    "Marital status", "mrs prestroke",
    trimws(gsub("median|t0|.y|_", " ", medians))
  ), table3)
  
  ### creating space between assessments
  cols <- c("stolic bp", 'moca', 'mmse', 'stroop',
            "ravens", 'time taken', 'lds', 'madrs',
            'nihss', 'barthel', 'mrs score', 
            'aerobic', 'acs', 'wsas', 'sis')
  .t3 <- matrix(ncol = ncol(table3))
  
  grab <- 1
  
  for (i in seq_along(cols)) {
    
    space <- first(grep(cols[i], table3[, 1])) - 1
    .t3 <- rbind(.t3, table3[grab:space, ], "")
    grab <- first(grep(cols[i], table3[, 1]))
    
  }
  
  #### adding the last category
  .t3 <- rbind(.t3[-1, ], 
               table3[grep("sis", table3[, 1]), ])

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
           round(.nums / nrow(moca_with_trends) * 100, 2),
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
            unlist()) / nrow(moca_with_trends)) * 100, 2),
      "%)")
  }
}

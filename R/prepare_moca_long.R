### mixed linear model predicting scores for lqmm 
# all variables separately, no adjustment
prepare_moca_long <- function (data,
                               selected_variables) {
  
  moca.long <- data[, c("id", "moca_score_w1",
                                    "moca_score_mo3", 
                                    "moca_score_mo12")]
  moca.long <- melt(moca.long, id.vars = "id")
  names(moca.long) <- c("id", "time", "moca.score")
  levels(moca.long$time) <- c("3-7 days", "3 months", "12 months")
  moca.long <- data.frame(moca.long,
                          data %>% select(selected_variables))
  
  moca.long <- moca.long[order(moca.long$id), ]
  
}

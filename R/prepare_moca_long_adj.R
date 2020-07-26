# lqmm adjusted by moca baseline
prepare_moca_long_adj <- function (data,
                                   selected_variables) {
  
  moca.long.adj <- data[, c("id", "moca_score_mo3",
                                        "moca_score_mo12")]
  moca.long.adj <- melt(moca.long.adj, id.vars = "id")
  names(moca.long.adj) <- c("id", "time", "moca.score")
  levels(moca.long.adj$time) <- c("90 days", "365 days")
  moca.long.adj <- data.frame(moca.long.adj,
                              data %>%
                                select(all_of(selected_variables), 
                                       moca_score_w1))
  
  names(moca.long.adj)[grep("moca_score_w1", 
                            names(moca.long.adj))] <- "moca_baseline"
  
  moca.long.adj <- moca.long.adj[order(moca.long.adj$id), ]
  
}


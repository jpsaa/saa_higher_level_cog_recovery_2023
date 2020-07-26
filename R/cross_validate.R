### cross validation function
cross_validate <- function (combination, data) {
  
  all_ids <- unique(data$id)
  lapply(all_ids, fit_model, combination, data)
  
}
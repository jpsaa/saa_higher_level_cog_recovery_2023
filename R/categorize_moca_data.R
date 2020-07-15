categorize_moca_data <- function (moca_with_impaired) {
  
  moca_with_impaired <- moca_with_impaired %>% 
    mutate(impaired_w1 = ifelse(moca_score_w1 < 24, 1, 0))
  moca_with_impaired <- moca_with_impaired %>% 
    mutate(impaired_mo3 = ifelse(moca_score_mo3 < 24, 1, 0))
  moca_with_impaired <- moca_with_impaired %>% 
    mutate(impaired_mo12 = ifelse(moca_score_mo12 < 24, 1, 0))
  
}
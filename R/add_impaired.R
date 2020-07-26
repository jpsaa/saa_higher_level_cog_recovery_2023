add_impaired <- function (data) {
  
  ##### Mcnemars test for count data ####
  data$imp.base <- data %>% 
    mutate(imp.base = ifelse(moca_score_w1 < 24, "yes", "no")) %>% 
    select(imp.base)
  data$imp.mo3 <- data %>% 
    mutate(imp.mo3 = ifelse(moca_score_mo3 < 24, "yes", "no")) %>% 
    select(imp.mo3)
  data$imp.mo12 <- data %>% 
    mutate(imp.mo12 = ifelse(moca_score_mo12 < 24, "yes", "no")) %>% 
    select(imp.mo12)

  data
}
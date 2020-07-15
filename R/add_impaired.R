add_impaired <- function (all.moca) {
  
  ##### Mcnemars test for count data ####
  all.moca$imp.base <- all.moca %>% mutate(imp.base=ifelse(moca_score_w1 < 24, "yes","no")) %>% select(imp.base)
  all.moca$imp.mo3 <- all.moca %>% mutate(imp.mo3=ifelse(moca_score_mo3 < 24, "yes","no")) %>% select(imp.mo3)
  all.moca$imp.mo12 <- all.moca %>% mutate(imp.mo12=ifelse(moca_score_mo12 < 24, "yes","no")) %>% select(imp.mo12)

  all.moca
}
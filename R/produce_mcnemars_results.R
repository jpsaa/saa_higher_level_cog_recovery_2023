#### McNemars tests
produce_mcnemars_results <- function (moca_with_impaired) {
  
  change1 <- matrix(
    c(
      all.moca %>% filter(imp.base == "yes", imp.mo3 == "yes") %>% nrow(),
      all.moca %>% filter(imp.base == "no", imp.mo3 == "yes") %>% nrow(),
      all.moca %>% filter(imp.base == "yes", imp.mo3 == "no") %>% nrow(),
      all.moca %>% filter(imp.base == "no", imp.mo3 == "no") %>% nrow()),
    dimnames = list (`week 1` = c("Cognitive impairment", "No cognitive impairment"),
                     `month 3` = c("Cognitive impairment", "No cognitive impairment")),
    nrow = 2) %>% 
    mcnemar.test()
  
  change2 <- matrix(
    c(
      all.moca %>% filter(imp.mo3 == "yes", imp.mo12 == "yes") %>% nrow(),
      all.moca %>% filter(imp.mo3 == "no", imp.mo12 == "yes") %>% nrow(),
      all.moca %>% filter(imp.mo3 == "yes", imp.mo12 == "no") %>% nrow(),
      all.moca %>% filter(imp.mo3 == "no", imp.mo12 == "no") %>% nrow()),
    dimnames = list (`month 3` = c("Cognitive impairment", "No cognitive impairment"),
                     `month 12` = c("Cognitive impairment", "No cognitive impairment")),
    nrow = 2) %>% 
    mcnemar.test()
  
  overall_change <- matrix(
    c(
      all.moca %>% filter(imp.base == "yes", imp.mo12 == "yes") %>% nrow(),
      all.moca %>% filter(imp.base == "no", imp.mo12 == "yes") %>% nrow(),
      all.moca %>% filter(imp.base == "yes", imp.mo12 == "no") %>% nrow(),
      all.moca %>% filter(imp.base == "no", imp.mo12 == "no") %>% nrow()),
    dimnames = list (`week 1` = c("Cognitive impairment", "No cognitive impairment"),
                     `month 12` = c("Cognitive impairment", "No cognitive impairment")),
    nrow = 2) %>% 
    mcnemar.test()
  
  list(change1 = change1, 
       change2 = change2, 
       overall_change = overall_change)
}

#### McNemars tests
produce_mcnemars_results <- function (data) {
  
  change1 <- matrix(
    c(
      data %>% filter(imp.base == "yes", imp.mo3 == "yes") %>% nrow(),
      data %>% filter(imp.base == "no", imp.mo3 == "yes") %>% nrow(),
      data %>% filter(imp.base == "yes", imp.mo3 == "no") %>% nrow(),
      data %>% filter(imp.base == "no", imp.mo3 == "no") %>% nrow()),
    dimnames = list (`week 1` = c("Cognitive impairment", "No cognitive impairment"),
                     `month 3` = c("Cognitive impairment", "No cognitive impairment")),
    nrow = 2) %>% 
    mcnemar.test()
  
  change2 <- matrix(
    c(
      data %>% filter(imp.mo3 == "yes", imp.mo12 == "yes") %>% nrow(),
      data %>% filter(imp.mo3 == "no", imp.mo12 == "yes") %>% nrow(),
      data %>% filter(imp.mo3 == "yes", imp.mo12 == "no") %>% nrow(),
      data %>% filter(imp.mo3 == "no", imp.mo12 == "no") %>% nrow()),
    dimnames = list (`month 3` = c("Cognitive impairment", "No cognitive impairment"),
                     `month 12` = c("Cognitive impairment", "No cognitive impairment")),
    nrow = 2) %>% 
    mcnemar.test()
  
  overall_change <- matrix(
    c(
      data %>% filter(imp.base == "yes", imp.mo12 == "yes") %>% nrow(),
      data %>% filter(imp.base == "no", imp.mo12 == "yes") %>% nrow(),
      data %>% filter(imp.base == "yes", imp.mo12 == "no") %>% nrow(),
      data %>% filter(imp.base == "no", imp.mo12 == "no") %>% nrow()),
    dimnames = list (`week 1` = c("Cognitive impairment", "No cognitive impairment"),
                     `month 12` = c("Cognitive impairment", "No cognitive impairment")),
    nrow = 2) %>% 
    mcnemar.test()
  
  list(change1 = change1, 
       change2 = change2, 
       overall_change = overall_change)
}

create_table_1 <- function (all, all.moca, 
                           moca.base, moca.incomp) {
  
  # continuous variables
  .cont.vars <- all %>% 
    select(age_t0, nihss_score_w1, madrs_w1, 
           moca_score_w1, height_t0, weight_t0, 
           bmi_t0, systolic_bp_t0, diastolic_bp_t0,
           aerobic_score_w1, strength_score_w1, 
           CCMI_score_t0) %>% 
    names
  
  table_cont_vars <- table1_cont_vars(all.moca, moca.base, 
                                      moca.incomp, .cont.vars)

  # count variables
  .freqs <- moca.base %>%
    select(gender_t0, ethnicity_binary_w1, educ_binary, 
           marital_status_binary_w1, mrs_prestroke_binary_t0,
           prev_stroke_t0, tia_t0, ht_t0, dm_t0, ihd_t0,
           af_t0, smoke_ever_w1) %>% 
    names
  
  table_count_vars <- table1_count_vars(all.moca, moca.base,
                                        moca.incomp, .freqs)
  

  table1 <- rbind(table_count_vars, table_cont_vars)
  
  #### table 1 (final) ####
  table1[duplicated(table1[, "Variables"]), 'Variables'] <- ""
  
  return(table1)
}

table1_cont_vars <- function (all.moca, moca.base, 
                              moca.incomp, .cont.vars) {
  
  all.base <- moca.base %>%
    select(all_of(.cont.vars)) %>%
    describe(IQR = TRUE) %>%
    select(n, median, IQR)
  
  comp.num <- all.moca %>%
    select(all_of(.cont.vars)) %>%
    describe(IQR = TRUE) %>%
    select(n, median, IQR)
  
  incomp.num <- moca.incomp %>%
    select(all_of(.cont.vars)) %>%
    describe(IQR = TRUE) %>%
    select(n, median, IQR)
  
  table1 <- cbind("All" = all.base[, 1],
                  "Median (IQR)" = paste0(all.base[, 2],
                                          " (", 
                                          round(all.base[, 3], 2),
                                          ")"),
                  "Included" = comp.num[, 1],
                  "Median (IQR)" = paste0(comp.num[, 2],
                                          " (",
                                          round(comp.num[,3], 2),
                                          ")"),
                  "Excluded" = incomp.num[, 1],
                  "Median (IQR)" = paste0(incomp.num[,2],
                                          " (",
                                          round(incomp.num[, 3], 2),
                                          ")"))
  
  table1 <- cbind(trimws(gsub("t0|_|w1|.x", " ",
                              rownames(comp.num))),
                  table1,
                  "Estimate (95% C.I.)" = NA,
                  "p-value" = NA)
  
  for (i in seq_along(.cont.vars)) {
    table1[i, "Estimate (95% C.I.)"] <- paste0(
      wilcox.test(moca.base[, which(names(moca.base) == .cont.vars[i])],
                  moca.incomp[, which(names(moca.incomp) == .cont.vars[i])],
                  conf.int = TRUE)$estimate %>% round(3), " (",
      wilcox.test(moca.base[, which(names(moca.base) == .cont.vars[i])],
                  moca.incomp[, which(names(moca.incomp) == .cont.vars[i])],
                  conf.int = TRUE)$conf.int[1] %>% round(3), "-",
      wilcox.test(moca.base[, which(names(moca.base) == .cont.vars[i])],
                  moca.incomp[, which(names(moca.incomp) == .cont.vars[i])],
                  conf.int = TRUE)$conf.int[2] %>% round(3), ")")
    
    table1[i, "p-value"] <- wilcox.test(moca.base[, which(names(moca.base) == .cont.vars[i])],
                                        moca.incomp[, which(names(moca.incomp) == .cont.vars[i])],
                                        conf.int = TRUE)$p.value %>% round(3)
  }
  
  table1[, "p-value"] <- ifelse(table1[, "p-value"] == 0, 
                                "<.001",
                                table1[, "p-value"])
  
  table1 <- table1[, -grep("Inc|Exc", colnames(table1))]
  
  colnames(table1)[grep("Med", colnames(table1))] <- c("Categories","Included", "Excluded")
  
  return(table1)
}

table1_count_vars <- function (all.moca, moca.base, 
                               moca.incomp, .freqs) {
  
  #### table comparing those with and without complete moca scores
  all.base <- moca.base %>%
    select(all_of(.freqs)) %>%
    summary() %>% 
    strsplit(":") %>% 
    data.frame() %>% 
    unname() %>% 
    t() %>% 
    na.omit
  all.base <- all.base[-which(trimws(all.base[, 1]) == "NA's"), ]
  
  comp <- all.moca %>%
    select(all_of(.freqs)) %>%
    summary() %>% 
    strsplit(":") %>% 
    data.frame() %>% 
    unname() %>% 
    t() %>% 
    na.omit
  comp <- comp[-which(trimws(comp[, 1]) == "NA's"), ]
  
  incomp <- moca.incomp %>%
    select(all_of(.freqs)) %>%
    summary() %>% 
    strsplit(":") %>% 
    data.frame() %>% 
    unname() %>% 
    t() %>% 
    na.omit
  
  ### matrix with values and percentages
  freqs <- matrix(c(as.numeric(all.base[, 2]),
                    round(as.numeric(all.base[, 2]) / 
                            nrow(moca.base) * 100, 2),
                    as.numeric(comp[, 2]),
                    round(as.numeric(comp[, 2]) / 
                            nrow(moca.base) * 100, 2),
                    as.numeric(incomp[, 2]),
                    round(as.numeric(incomp[, 2]) / 
                            nrow(moca.base) * 100, 2)),
                  ncol = 6,
                  dimnames = list(comp[, 1],
                                  c("All", "%", "Included",
                                    "%", "Excluded", "%%")))
  #### assigning categories
  freqs <- cbind(vars =
                   rep(trimws(gsub("t0|_|w1|.x", " ",
                                   .freqs)),
                       each = nlevels(all$educ_binary)),
                 freqs)
  
  ### Fisher test for the first variable (diabetes)
  fisher.test(
    matrix(
      as.numeric(
        unlist(
          split(freqs[, c(which(colnames(freqs) == "Included"),
                          which(colnames(freqs) == "Excluded"))],
                freqs[, "vars"])["dm"])),
      ncol = 2))$p.value
  
  ### confindence interval (lower)
  fisher.test(
    matrix(
      as.numeric(
        unlist(
          split(freqs[, c(which(colnames(freqs) == "Included"),
                          which(colnames(freqs) == "Excluded"))],
                freqs[, "vars"])[2])),
      ncol = 2))$conf.int[1]
  
  ### applying fisher test to all the variables (in alphabetical order) and getting p-values
  est <- lapply(split(freqs[,c(which(colnames(freqs) == "Included"),
                               which(colnames(freqs) == "Excluded"))],
                      freqs[,"vars"]),
                function(x)
                  fisher.test(matrix(as.numeric(unlist(x)), ncol = 2))$estimate
  ) %>% unlist %>% round(2)
  
  
  p.values <- lapply(split(freqs[, c(which(colnames(freqs) == "Included"),
                                     which(colnames(freqs) == "Excluded"))],
                           freqs[,"vars"]),
                     function(x)
                       fisher.test(matrix(as.numeric(unlist(x)), ncol = 2))$p.value
  ) %>% unlist %>% round(2)
  
  
  c.int.lb <- lapply(split(freqs[,c(which(colnames(freqs) == "Included"),
                                    which(colnames(freqs) == "Excluded"))],
                           freqs[, "vars"]),
                     function(x)
                       fisher.test(matrix(as.numeric(unlist(x)), ncol = 2))$conf.int[1]
  ) %>% unlist %>% round(2)
  
  c.int.ub <- lapply(split(freqs[,c(which(colnames(freqs)=="Included"),
                                    which(colnames(freqs)=="Excluded"))],
                           freqs[,"vars"]),
                     function(x)
                       fisher.test(matrix(as.numeric(unlist(x)), ncol = 2))$conf.int[2]
  ) %>% unlist %>% round(2)
  
  ### copying stats into the table
  freqs <- cbind(freqs,
                 "Estimate (95% C.I.)" = paste0(
                   est[match(freqs[,"vars"],
                             gsub(".odds ratio","",names(est))
                   )
                   ],
                   " (",
                   c.int.lb[match(freqs[,"vars"],names(c.int.lb))],
                   "-",
                   c.int.ub[match(freqs[,"vars"],names(c.int.ub))],
                   ")"),
                 "p-value"=p.values[match(freqs[,"vars"],names(p.values))])
  
  ### getting rid of duplicate values and adding names to table
  freqs[duplicated(freqs[,"p-value"]),c("p-value","Estimate (95% C.I.)")]=""
  
  vars <- as.character(freqs[,"vars"])
  freqs[,'vars'] <- rownames(freqs)
  
  #### making count and percentage into the same column
  freqs <- matrix(freqs,
                  ncol = ncol(freqs),
                  dimnames = list(Variables = vars,
                                  "Table 1: Baseline Characteristics"=
                                    c("Categories","All","%","Included","%%", "Excluded","%%%","c.int","p-value")))
  
  freqs[,"All"] <- paste0(freqs[, "All"], 
                          " (", freqs[,"%%"], "%)")
  freqs[,"Included"] <- paste0(freqs[, "Included"], 
                               " (", freqs[,"%%"], "%)")
  freqs[,"Excluded"] <- paste0(freqs[, "Excluded"], 
                               " (", freqs[,"%%%"], "%)")
  
  ### putting numerical values and frequencies together
  freqs <- freqs[, -grep("%", colnames(freqs))]
  freqs <- cbind("Variables" = rownames(freqs), freqs)
  rownames(freqs) <- 1:nrow(freqs)
  
  return(freqs)
}


# packages

library(dplyr)
library(psych)
library(asht) ### confidence intervals for wilcoxon signed-pratt test

library(optimx) ## optimizer for models
library(ggplot2) 
library(ggstatsplot) ###statistics on plots
library(reshape2) ### long format data
library(coin) ## wilcoxon signed-pratt test
library(ggraph) ### trees
library(igraph) ### weighted trees
library(tibble) ### tables with "flexible" column characters
library(lme4) ###generalized linear models
library(dataPreparation)
library(lqmm) ## quantile mixed regression
library(extrafont) ##embed fonts in figures
library(hrbrthemes) ## graph colors
library(quantreg) ### quantile regression

all <- read.csv("data/data-start-for-docR.csv")

#### Datasets for baseline, complete, incomplete, and no moca
all.moca <- all %>% 
  dplyr::filter (!is.na(moca_score_w1) & !is.na(moca_score_mo3) & !is.na(moca_score_mo12))
moca.base <- all %>% 
  dplyr::filter (!is.na(moca_score_w1))
moca.incomp <- moca.base %>% 
  dplyr::filter (is.na(moca_score_w1) | is.na(moca_score_mo3) | is.na(moca_score_mo12))
# no.moca = all %>% dplyr::filter (is.na(moca_score_w1) | is.na(moca_score_mo3) | is.na(moca_score_mo12))

table1 <- create_table_1(all, all.moca, moca.base, moca.incomp)
write.csv(as_tibble(table1), "tables/table1.csv", row.names = FALSE)

# this is failing at line 45
table2 <- create_table_2(all.moca)
write_csv(as_tibble(table2),"tables/table2.csv")

create_fig_S1(all.moca)
ggsave("figures/Fig-S1-bubble_plot.pdf", 
       width = 12, height = 7,
       units = 'in')

moca_with_trends <- add_moca_trends(all.moca)

.t3_cols <- moca_with_trends %>% 
  select(trends, age_t0, CCMI_score_t0,
         systolic_bp_t0, diastolic_bp_t0, 
         systolic_bp_mo3, diastolic_bp_mo3, 
         systolic_bp_mo12, diastolic_bp_mo12, 
         moca_score_w1, moca_score_mo3, moca_score_mo12, 
         mmse_score_mo3, mmse_score_mo12, 
         stroop_color_word_ratio_mo3, stroop_ratio_mo12, 
         ravens_score_mo3, ravens_total_score_mo12, 
         time_taken_mo3, tmt_time_taken_mo12, 
         ldsf_score_mo3, ldsf_score_mo12, 
         ldsb_score_mo3, ldsb_score_mo12, 
         madrs_score_w1, madrs_score_mo3, madrs_score_mo12, 
         nihss_score_w1, nihss_score_mo3,  nihss_score_mo12, 
         barthel_score_mo3, barthel_score_mo12,
         mrs_score_mo3, mrs_score_mo12,
         aerobic_score_w1,
         strength_score_w1,rapa_aerobic_score_mo3,
         rapa_strengthflexibility_score_mo3,
         rapa_aerobic_score_mo12,
         rapa_strengthflexibility_score_mo12,
         acs_1_82_raln_mo3, acs_1_82_raln_mo12,
         wsas_score_mo3, wsas_score_mo12,
         sis_total_mo3, sis_total_mo12) %>% 
  names

# failing at line 5 because all.moca doesn't have column 'id'
table3 <- create_table_3(moca_with_trends,
                         table3_columns = .t3_cols)
write.csv(as_tibble(table3), "tables/table3.csv", 
          row.names =  FALSE)

table4 <- create_table_4(moca_with_trends, 
                         table3_columns = .t3_cols)
write.csv(as_tibble(table4, .name_repair = "minimal"), 
          "tables/table4.csv", 
          row.names = FALSE)

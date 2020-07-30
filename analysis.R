
R.utils::sourceDirectory('R/')

# packages
library(dplyr)
library(psych)
library(asht) ### confidence intervals for wilcoxon signed-pratt test
library(reshape2) ### long format data
library(igraph) ### weighted trees
library(ggraph) ### trees
library(hrbrthemes) ## graph colors
library(extrafont) ##embed fonts in figures
library(quantreg) ### quantile regression
library(lqmm) ## quantile mixed regression
library(dataPreparation)
library(lme4) ###generalized linear models
library(optimx) ## optimizer for models
library(RColorBrewer)
library(ggplot2) 

all <- read.csv("data/data-start-for-docR.csv")

table1 <- create_table_1(data = all)
write.csv(as_tibble(table1), "tables/table1.csv", row.names = FALSE)

#### Datasets for baseline, complete, incomplete, and no moca
all.moca <- all %>% 
  dplyr::filter (!is.na(moca_score_w1) & !is.na(moca_score_mo3) & !is.na(moca_score_mo12))

table2 <- create_table_2(data = all.moca)
write.csv(as_tibble(table2), "tables/table2.csv")

create_fig_S1(data = all.moca)
extrafont::loadfonts()
ggsave("figures/Fig-S1-bubble_plot.pdf", 
       width = 12, height = 7,
       units = 'in')

moca.with.trends <- add_moca_trends(data = all.moca)

.t3_cols <- moca.with.trends %>% 
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

table3 <- create_table_3(data = moca.with.trends,
                         columns = .t3_cols)
write.csv(as_tibble(table3), "tables/table3.csv", 
          row.names =  FALSE)

table4 <- create_table_4(data = moca.with.trends, 
                         columns = .t3_cols)
write.csv(as_tibble(table4, .name_repair = "minimal"), 
          "tables/table4.csv", 
          row.names = FALSE)

moca <- moca.with.trends %>% 
  select(id, moca_score_w1, moca_score_mo3,
         moca_score_mo12, trends)

create_fig_2(moca, all.moca)
extrafont::loadfonts()
ggsave("figures/Fig-2-tree-cairo.pdf", width = 12, height = 7,
       units = 'in')

moca.with.impaired <- add_impaired(data = all.moca)

sink('results/mcnemars.txt')
produce_mcnemars_results(data = moca.with.impaired)
sink()

categorized.moca <- categorize_moca_data(data = moca.with.impaired)

### selecting all variables for analyses
.all.vars <- 
  categorized.moca %>%
  select(
    gender_w1, educ_binary, prev_stroke_t0,
    tia_t0, ht_t0, af_t0, dm_t0, ihd_t0,
    disab_prestroke, smoke_ever_w1,
    ethnicity_binary_w1, marital_status_binary_w1,
    age_t0, nihss_score_w1, CCMI_score_t0,
    madrs_w1, bmi_t0, aerobic_score_w1,
    strength_score_w1) %>% 
  names

sink('results/moca_summary.txt')
categorized.moca %>% select(.all.vars) %>% summary
sink()

blm <- collate_binomial_regressions(data = categorized.moca,
                                    selected_variables = .all.vars)
write.csv(blm, "results/odds-ratio.csv", 
          row.names = FALSE)

qr <- collate_quantile_regressions(data = categorized.moca,
                                   selected_variables = .all.vars)
write.csv(qr, "results/quantile-regression.csv", 
          row.names = FALSE)

moca.long <- prepare_moca_long(data = categorized.moca, 
                               selected_variables = .all.vars)
lqmm_not_adj <- fit_lqmm_not_adj(moca.long)
write.csv(lqmm_not_adj, "results/lqmm.csv", 
          row.names = FALSE)

moca.long.adj <- prepare_moca_long_adj(data = categorized.moca,
                                       selected_variables = .all.vars)
lqmm_adj <- fit_lqmm_adj(moca.long.adj)
write.csv(lqmm_adj, "results/lqmm_adj.csv", row.names = FALSE)

#### selecting only significant variables from the univariable models
moca.long.model <- moca.long %>% 
  select(id, moca.score, time, educ_binary, 
         ethnicity_binary_w1, smoke_ever_w1,
         tia_t0, ht_t0, ihd_t0, 
         disab_prestroke, age_t0, 
         strength_score_w1, nihss_score_w1)

sink('results/mixed_lqmm_adj_summary.txt')
summarise_mixed_lqmm_adj(moca.long.model)
sink()

moca.long.adj.gamma <- prepare_moca_long_adj_gamma(data = moca.long.adj)
gamma_adj <- fit_gamma_adj(data = moca.long.adj.gamma)
write.csv(gamma_adj, "results/gamma-adjusted-moca-baseline.csv", 
          row.names = FALSE)

formulas <- prepare_cross_val_formulas(data = moca.long.adj.gamma)

### making patient IDs consecutive
moca.long.adj.gamma$id <- as.character(factor(moca.long.adj.gamma$id,
                                        labels = c(1:length(unique(moca.long.adj.gamma$id)))))

#### testing function (only on a limited number of patients and with one formula)
# lapply(1:10, fit_model, formulas[3], moca.long.adj.gamma)

#### comparison between gamma and lqmm models

### results adjusted by baseline score
model.comparisons.2 <- lapply(formulas, cross_validate, 
                              data = moca.long.adj.gamma) %>%
  purrr::map_df(., bind_rows) %>%
  data.frame()

saveRDS(model.comparisons.2,
        "results/model-comparisons-lqmm-vs-gamma-adjusted-moca-baseline-NO-offset.rds")

op <- prepare_cross_val_plot_data(mod_comp = model.comparisons.2)

cors <- plyr::ddply(op, c("formula", "model.time"), 
                    summarise, cor = round(cor(observed, predicted, 
                                               method = "pearson",
                                               use = "complete.obs"), 3))

create_boxplot(data = op)
ggsave("figures/models-rmse-Gamma-vs-lqmm-both-adjusted-by-moca-baseline.png", 
       width = 8.5, height = 8.5, type = "cairo", dpi = 200 )
ggsave("figures/models-rmse-Gamma-vs-lqmm-both-adjusted-by-moca-baseline.pdf", 
       width = 8.5, height = 8.5, device = cairo_pdf)

create_fig_S2(data = op)
ggsave("figures/Fig-S2-Gamma_vs_lqmm_correlations.png", 
       width = 12, height = 11, type = "cairo", dpi = 200)
ggsave("figures/Fig-S2-Gamma_vs_lqmm_correlations.pdf", 
       width = 12, height = 11, device = cairo_pdf)

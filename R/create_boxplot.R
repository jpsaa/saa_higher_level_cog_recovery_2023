create_boxplot <- function (data) {
  
  cors <- plyr::ddply(data, c("formula", "model.time"), 
                      summarise, 
                      cor = round(cor(observed, predicted, 
                                      method = "pearson",
                                      use = "complete.obs"), 3))
  
  #### Graphing values
  # Define the number of colors you want
  nb.cols <- nlevels(data$formula)
  mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)
  
  ###### boxplot
  ggplot(data, aes(x = as.numeric(formula), y = rmse)) +
    geom_boxplot(aes(color = formula), size = .4, 
                 width = 0.6, notch = TRUE, 
                 outlier.shape = NA) +
    geom_jitter(aes(color = formula), size = .5,  
                width = 0.3, alpha = .2) +
    facet_wrap(~ model) + 
    theme_ipsum() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.key.size = unit(4, "mm")) + 
    scale_color_manual(
      name = "Variable combinations used in models \n(Model correlations: 
      Gamma 3mo | 12mo | Lqmm 3mo | 12mo)*",
      values = mycolors,
      labels =
        # sapply(mtcars,function(x) gsub("\\.",",",as.character(x)))
        unlist(
          sapply(factor(
            paste0(as.character(sort(unique(data$formula))),
                   " (",
                   lapply(split(cors[, "cor"], cors[, "formula"]),
                          paste, collapse = " | "),
                   ")")
          ),
          function(x) gsub("  +"," ",
                           gsub("diab","diabetes",
                                gsub("ihd","isch heart dis",
                                     gsub("baseline|p0|w1|t0|binary|total|bin|years","",
                                          gsub("ht","hypertension",
                                               gsub("cci","charlson cmb. index",
                                                    gsub("time \\+ |_"," ",
                                                         as.character(x)))))))))
        ),
      guide = guide_legend(
        ncol = 2, title.hjust = .5,
        title.position = "top")) +
    labs(x = "Model used", 
         y = "RMSE value (error from predicted vs. observed scores)", 
         # title = "Gamma-distribution mixed-effects models predicting 
         # frontal function performance",
         subtitle = "Pearson's correlation calculated with predicted and 
         observed scores for all models in each time-point.\nModel 
         comparisons based on Root Mean Square Error (RMSE) across evaluations", 
         caption = "Note: each dot represents model predictions for 
         each patient. Prediction estimates were obtained with the 
         leave-one-out cross-validation method (LOOCV)\n*All models 
         included time (3 months | 12 months) as a fixed effect as 
         well as baseline MoCA scores (as model offset in Gamma models; 
         and as fixed effect in quantile models)" ) 
  
}

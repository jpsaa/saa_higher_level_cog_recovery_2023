create_fig_S2 <- function (data) {
  
  cors <- plyr::ddply(data, c("formula", "model"), 
                      summarise, 
                      cor = round(cor(observed, predicted, 
                                      method = "pearson",
                                      use = "complete.obs"), 3))
  
  nb.cols <- nlevels(data$formula)
  mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)
  
  ggplot(data, aes(observed, predicted)) + 
    geom_point(color = "#69b3a2", size = 1, 
               alpha = .1, shape = 3)  +
    geom_jitter(aes(col = formula), size = .05, 
                width = 0.4, alpha = .7) +
    facet_wrap(~formula + model, nrow = 7) +
    geom_text(data = cors, aes(label = paste("r=", cor, sep = "")), 
              size = 3, x = 25, y = 11) +
    scale_color_manual(name = 
                         "Variable combinations used in models: \nFormula* (Gamma | Lqmm)",
                       values = mycolors,
                       labels =
                         # sapply(mtcars,function(x) gsub("\\.",",",as.character(x)))
                         unlist(
                           sapply(factor(
                             paste0(as.character(sort(unique(data$formula))),
                                    " (",
                                    lapply(split(cors[,"cor"], cors[,"formula"]),
                                           paste, collapse = " | "),
                                    ")")),
                           function(x) gsub("  +"," ",
                                            gsub("diab","diabetes",
                                                 gsub("ihd","isch heart dis",
                                                      gsub("baseline|p0|w1|t0|binary|total|bin|years","",
                                                           gsub("ht","hypertension",
                                                                gsub("cci","charlson cmb. index",
                                                                     gsub("time \\+ |_"," ",
                                                                          as.character(x)))))))))
                         )
                       # guide = guide_legend(
                       #   ncol=1, title.hjust = .5,
                       #   title.position = "top")
    ) +
    theme_ipsum() +
    theme(
      strip.text =  element_text(color = "transparent"),
      panel.spacing.x = unit(.5, "lines"),
      panel.spacing.y = unit(-1.9, "lines")
      # strip.text.x =element_text(size=.1)
    ) +
    labs(
      # title = "LQMM models - Predicted vs observed MoCA scores accross evaluation time-points \n(time as random effect)",
      caption = "*All formulas incorporated baseline MoCA scores as well as evaluation time (3mo | 12mo) as fixed effects.\nPearson's r scores obtained by comparing scores predicted and observed, for each subject and model tested") +
    guides(size = guide_legend(jitter = 2),
           colour = guide_legend(ncol = 1,
                                 override.aes = list(size = 3, point = 3)))
  
}

# changed title to Figure S1. 
### Supplementary Figure - sup fig 1 bubble plot - change in scores 
create_fig_S1 <- function (all.moca) {
  
  extrafont::loadfonts()
  all.moca$diff1 <- with(all.moca, moca_score_mo3 - moca_score_w1)
  all.moca$diff2 <- with(all.moca, moca_score_mo12 - moca_score_mo3)
  all.moca$moca_impaired_w1 <- ifelse(
    all.moca$moca_score_w1 < 24, "yes", "no")
  
  ggplot(all.moca, aes(x = diff1, y = diff2)) +
    geom_count(aes(col = ..n..)) +
    # scale_size_area(max_size = 7) +
    guides(col = "legend") +
    ggtitle("Figure S1 - Changes in MoCA score in the START cohort (n = 122)") +
    labs(x = "Change from week 1 to 3 months post stroke\n(points)\n\n Note: Dotted lines represent the median",
         y = "Change from 3 months to 1 year post stroke\n(points)",
         col = "Count", size = "Count") +
    scale_color_distiller(type = "div", palette = "Set3") +
    theme_bw() +
    theme(axis.line = element_line(size = 0.5, colour = "gray40"),
          panel.grid.major = element_line(colour = "gray60", size = 0.1),
          panel.grid.minor = element_blank(),
          plot.title = element_text(family = "Tahoma", face = "bold",
                                    hjust = .5, size = 12),
          text = element_text(family = "Tahoma"),
          axis.text.x = element_text(colour = "gray10", size = 8),
          axis.text.y = element_text(colour = "gray10", size = 8),
          axis.title.x = element_text(colour = "black", size = 8),
          axis.title.y = element_text(colour = "black", size = 8)) +
    geom_vline(xintercept = mean(all.moca$diff1),
               color = "red", linetype = "dashed",
               alpha = .5) +
    geom_hline(yintercept = median(all.moca$diff2),
               color = "red", linetype = "dashed",
               alpha = .5)
  
}

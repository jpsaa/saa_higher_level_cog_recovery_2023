#### Figure 2 - moca tree####
create_fig_2 <- function (moca, all.moca) {
  
  moca.scores <- moca[, -which(names(moca) == "trends")]
  
  moca.scores <- reshape2::melt(moca.scores, id.vars = 'id')
  
  names(moca.scores) <- c("id", 'time', 'score')
  moca <- cbind(moca.scores, moca$trends)
  names(moca)[4] <- "trends"
  
  # removing letters from columns
  moca$time <- as.numeric(
    as.character(factor(moca$time,
                        labels = c(7, 30, 365))))
  moca <- na.omit(moca)
  
  ###simple tree with groups and no values####
  # testing approach with data sequences
  
  moca12 <- data.frame(table(moca$trends))

  moca3 <- all.moca %>%
    mutate(moca3 = ifelse(moca_score_mo3 < moca_score_w1,
                          '3.decliner',
                          ifelse(moca_score_mo3 > moca_score_w1,
                                 '1.improver',
                                 ifelse(moca_score_mo3 == moca_score_w1,
                                        '2.stable',NA)))) %>% 
    select(moca3) %>% 
    table() %>% 
    data.frame()
  
  
  #### Asssigning actual values ####
  d1 <- data.frame(from = "onset", 
                   to = paste0(c('Improved', 
                                 'Stable', 
                                 'Declined'), 
                               ' at 3mo'))
  d2 <- data.frame(from = rep(d1$to, each = 3), 
                   to = paste0(moca12$Var1, ' (', moca12$Freq/3,')'))
  edges <- rbind(d1, d2)
  
  # adding a second data frame with information for each node!
  name <- unique(c(as.character(edges$from), 
                   as.character(edges$to)))
  
  vertices <- data.frame(
    name = name,
    group = c(NA, rep(c("Improved", "Stable", "Declined"), 4)),
    Three.Months = c(NA, rep(c("Improved", "Stable", "Declined"), 4)),
    value = as.numeric(c(158, moca3$Freq, moca12$Freq / 3))
  )
  
  ####Figure 2 trajectory####
  mygraph <- graph_from_data_frame(edges, vertices = vertices)
  
  ggraph(mygraph, layout = 'dendrogram') +
    # geom_edge_diagonal() +
    geom_edge_diagonal2(aes(colour = node.group, width = node.value, alpha = 0.8)) +
    geom_node_text(aes(label = name, filter = leaf, color = Three.Months), 
                   angle = 0, hjust = 0, nudge_y = 0.03, size = 3) +
    geom_node_point(aes(filter = leaf, size = value, color = Three.Months), 
                    alpha = 0.5) +
    
    # ggtitle(paste0('        Figure 2 - Changes in MoCA scores from week 1 to 3 and 12 months post-stroke\n',
    #                '        n = ',nrow(moca)/3)) +
    
    theme_ipsum(axis_text_size = 9) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8),
          plot.title = element_text(family = "Tahoma",
                                    face = "bold",hjust = 0.5, size = 12),
          text = element_text(family = "Tahoma", angle = 0, vjust = 0),
          legend.title = element_text(colour = "gray10",size = 8),
          axis.line = element_line(size = 1, colour = "grey100"),
          # axis.ticks.x = element_line(size = 1)
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    
    scale_x_continuous(name ="Trajectory group",
                       breaks = NULL) +
    
    scale_color_manual(
      name = "Outcome at 12 months \n (number of participants)",
      guide = "none",
      values = c("indianred1","lightseagreen","cornflowerblue")) +
    
    scale_size_continuous(name = "Number of participants at 12 months") +
    scale_edge_width(guide = 'none',
                     range = c(0.2, 6)) +
    scale_edge_color_manual(name = 'Overall trajectory',
                            labels = c('Declined', 'Improved',
                                       'Stable', '0-3-12 month trajectory'),
                            values = c("indianred1", "lightseagreen", 
                                       "cornflowerblue"),
                            na.value = "grey80") +
    scale_edge_alpha(guide = 'none') +
    coord_flip(expand = TRUE, ylim = c(2, -.3), xlim = c(9, -1)) +
    scale_x_reverse(name = "", breaks = NULL) +
    scale_y_reverse(name = "",
                    breaks = c(2, 1, 0),
                    labels = c("Baseline", "3-months", "12-months")) +
    guides(edge_color = guide_legend(ncol = 4, nrow = 1, title.position = "top"),
           # color = guide_legend(ncol = 1, nrow = 3, title.position = "top"),
           size = guide_legend(ncol = 4, nrow = 1, title.position = "top")) 
  
}


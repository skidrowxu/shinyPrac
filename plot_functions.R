# round_sas <- function(x, digits = 0){
#   posneg <- sign(x)
#   z <- abs(x) * 10^digits
#   z <- z + 0.5 + sqrt(.Machine$double.eps)
#   z <- trunc(z)
#   z <- z/10^digits
#   z * posneg
# }



# line plot function
line_plot <- function(dat){
  
  stats <- dat %>% group_by(TRTA, AGEGR1, PARAM, AVISIT) %>%
    mutate(
      q1 = quantile(LBSTRESN, 0.25, na.rm = TRUE),
      median = median(LBSTRESN, na.rm = TRUE),
      q3 = quantile(LBSTRESN, 0.75, na.rm = TRUE)
    ) %>% ungroup()
  
  title <- paste0(unique(dat$PARAM))
  
  p <- ggplot(stats, aes(x = AVISIT, y = median, color = TRTA, group = 1, 
                         text = paste(
                           "LCI:", round(q1, 2),
                           "\nUCI:", round(q3, 2)
                         ))) + 
    geom_point(aes(fill = TRTA, shape = TRTA), size = 2, position = position_dodge(width = 0.5)) + 
    geom_line(position = position_dodge(width = 0.5)) + 
    geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.3, position = position_dodge(width = 0.5)) + 
    facet_grid(AGEGR1 ~ TRTA) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 20),
          legend.position = "none", 
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplotly(p)
    
}


# box plot function
box_plot <- function(dat){
  
  title <- paste0(unique(dat$PARAM))
  
  p <- ggplot(dat, aes(x = AVISIT, y = LBSTRESN, color = TRTA)) + 
    geom_boxplot(fill = "white", outlier.color = NA,
                 outlier.size = 0, outlier.shape = NA) +
    facet_grid(AGEGR1 ~ TRTA) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 20),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
  
  ggplotly(p)
  
}

# bar plot function
bar_plot <- function(dat){
  
  title <- paste0(unique(dat$PARAM))
  
  stats <- dat %>% group_by(TRTA, AGEGR1, PARAM, AVISIT) %>%
    summarise(
      q1 = quantile(LBSTRESN, 0.25, na.rm = TRUE),
      median = median(LBSTRESN, na.rm = TRUE),
      q3 = quantile(LBSTRESN, 0.75, na.rm = TRUE)
    ) %>% ungroup()
  
  p <- ggplot(stats, aes(x = AVISIT, y = median, 
                         color = TRTA, fill = TRTA, 
                         group = AVISIT)) +
       geom_col(fill = NA) +
       geom_errorbar(data = stats, 
                     aes(ymin = q1, ymax = q3),
                     width = 0.2) +
       facet_grid(PARAM ~ TRTA, scales = "fixed") +
       scale_x_discrete() +
       theme_bw() + 
       theme(legend.position = "none",
             plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(angle = 20)) +
    ggtitle(title)
  
  ggplotly(p)
  
}

# spaghetti plot function
spaghetti_plot <- function(dat){
  
  title <- paste0(unique(dat$PARAM))
  
  p <- dat %>% ggplot(. , aes(x = AVISIT, y = LBSTRESN, color = TRTA)) + 
    geom_boxplot(outlier.shape = NA, alpha = 0.05, width = 0.02, color = "gray") +
    geom_point(aes(color = TRTA), fill = NA, shape = 21, stroke = 0.5) +
    geom_line(aes(group = USUBJID, color = TRTA, alpha = 1), linetype = "dashed") + 
    facet_grid(AGEGR1 ~ TRTA) +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 20)) +
    ggtitle(title)
  
  ggplotly(p)
  
}



#-------------------------------------------------------------------------------
# Topic: Plot Functions
# Created by: Isaac Bravo
# Source: https://twitter.com
#-------------------------------------------------------------------------------

#------------------------ Setup Libraries -------------------------------------#

load_libraries <- function(){
  
  # Load packages
  Packages <- c("tidyverse", "tidytext", "purrr", "tidyr", "hrbrthemes", 
                "marker", "htmltools", "fontawesome", "MASS", "patchwork")
  lapply(Packages, library, character.only = TRUE)
  
}

#------------------------ Plot Valence ----------------------------------------#

plot_topic_year <- function(data, legend_label){
  
  data %>%
    ggplot(aes(x = "", y = percent, fill = as.factor(year))) +
    # geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = .8) +
    ggchicklet::geom_chicklet(radius = grid::unit(6, "pt")) +
    geom_text(aes(label = label),
              size = 3.0, fontface = "bold",
              position = position_stack(vjust = .5, reverse = TRUE)) +
    facet_wrap(~ region, ncol = 1) +
    coord_flip() +
    labs(
      title = "Topic Distribution by Region",
      subtitle = paste0(legend_label),
      x = "",
      y = "",
      fill = "Year:"
    ) +
    theme_minimal() +
    theme(legend.position = "top",
          plot.title = ggtext::element_textbox_simple(
            size = 18,
            padding = margin(5.5, 5.5, 5.5, 5.5),
            margin = margin(0, 0, 15.5, 0),
            box.color = "grey40",
            r = unit(9, "pt"),
            halign = .5,
            lineheight = 1
          ),
          
          plot.title.position = "plot",
          plot.subtitle = element_text(hjust = 0.5, family = "Lora",
                                       size = 14, colour = "black"),
          plot.caption = element_text(family = "Roboto Condensed", colour = "black", face = "bold"),
          plot.background = element_rect(fill = "white", colour = "white", size = 3),
          plot.margin = unit(c(7, 10, 3, 10), units = "mm"),
          panel.background = element_rect(fill = "white", colour = "NA"),
          strip.text = element_text(family = "Roboto Condensed", colour = "black", face = "bold", size = 11),
          
          legend.title = element_text(family = "Lora", size = 12, 
                                      hjust = 0.5, face = "bold", colour = "black"),
          legend.key = element_blank(),
          legend.background = element_rect(fill = "white", colour = "white"),
          legend.key.width = unit(10, "mm"),
          legend.key.size = unit(.1, "mm"),
          legend.text = element_text(family = "Roboto Condensed", hjust = 0.5, size = 12,
                                     colour = "black"),
          legend.direction = "horizontal",
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(color="#DFDFDF"),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color="#DFDFDF"),
          axis.ticks = element_blank(),
          axis.text.y = element_text(family = "Roboto Condensed", hjust = 0.5, size = 10,
                                     colour = "black", face = "bold"),
          axis.text.x = element_blank(),
          axis.title = element_blank()) +
    scale_fill_manual(values = c("#ACC7A1", "#9EC5E2", "#DFDFDF", "#9FC1BD")) +
    guides(
      fill = guide_legend(nrow = 1)
    ) 
  
}

group_topic <- function(data){
  
  data %>%
    mutate(year = lubridate::year(date_time),
           region = str_to_title(region)) %>%
    group_by(region, year) %>%
    summarize(total = n()) %>%
    mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>%
    ungroup()
}

group_topic_region <- function(data, level){
  
  result <- data %>%
    dplyr::filter(region == tolower(level)) %>% 
    dplyr::select(date_time, region, animals, people, causes_binary,
           consequences_binary, solutions) %>%
    dplyr::mutate(category = case_when(
      animals == "animals" ~ "Animals",
      causes_binary == "Yes" ~ "Causes of CC",
      consequences_binary == "Yes" ~ "Consequences of CC",
      people == "Yes" ~ "People",
      solutions == "Climate summits" ~ "Climate summits",
      solutions == "Protests" ~ "Protests",
      TRUE ~ NA_character_
    )) %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::mutate(year = lubridate::year(date_time),
           region = str_to_title(region)) %>%
    dplyr::group_by(year, category) %>%
    dplyr::summarize(total = n()) %>%
    dplyr::mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>%
    ungroup() %>% 
    dplyr::mutate(region = str_to_title(level)) %>% 
    dplyr::select(region, everything())
  
  return(result)
}

plot_topic_region <- function(data, legend_label){
  
  data %>%
    ggplot(aes(x = "", y = percent, fill = as.factor(category))) +
    ggchicklet::geom_chicklet(radius = grid::unit(6, "pt"), width = .5) +
    geom_text(aes(label = label),
              size = 3.0, fontface = "bold",
              position = position_stack(vjust = .5, reverse = TRUE)) +
    facet_wrap(~ year, ncol = 1, strip.position = "left") +
    coord_flip() +
    labs(
      title = paste0("Topic Distribution in ", legend_label, " region by year"),
      x = "",
      y = "",
      fill = "Topics:"
    ) +
    theme_minimal() +
    theme(legend.position = "top",
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0.5, family = "Lora",
                                    size = 14, colour = "black", face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, family = "Lora",
                                       size = 14, colour = "black"),
          plot.caption = element_text(family = "Roboto Condensed", colour = "black", face = "bold"),
          plot.background = element_rect(fill = "white", colour = "white", size = 3),
          plot.margin = unit(c(7, 10, 3, 10), units = "mm"),
          panel.background = element_rect(fill = "white", colour = "NA"),
          panel.spacing=unit(0, "lines"),
          strip.text = element_text(family = "Roboto Condensed", colour = "black", 
                                    face = "bold", size = 11, angle = -90),
          legend.title = element_text(family = "Lora", size = 12,
                                      hjust = 0.5, face = "bold", colour = "black"),
          legend.key = element_blank(),
          legend.background = element_rect(fill = "white", colour = "white"),
          legend.key.width = unit(10, "mm"),
          legend.key.size = unit(.1, "mm"),
          legend.text = element_text(family = "Roboto Condensed", hjust = 0.5, size = 10,
                                     colour = "black"),
          legend.direction = "horizontal",
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(color="#DFDFDF"),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color="#DFDFDF"),
          axis.ticks = element_blank(),
          axis.text.y = element_text(family = "Roboto Condensed", hjust = 0.5, size = 10,
                                     colour = "black", face = "bold"),
          axis.text.x = element_blank(),
          axis.title = element_blank()) +
    scale_fill_manual(values = c("#ACC7A1", "#9EC5E2", "#DFDFDF", "#9FC1BD", "#B0C3CC", "#D2BCC1")) +
    guides(
      fill = guide_legend(nrow = 2, keyheight = 1)
    ) 
  
}





#------------------------ Plot Valence ----------------------------------------#

plot_valence_region <- function(data, legend_label){
  
  data %>%
    ggplot(aes(x = year, y = percent, fill = top_valence)) +
    geom_bar(position="fill", stat="identity", width = .5) +
    geom_text(aes(label = label),
              size = 2.5, fontface = "bold",
              position = position_stack(vjust = .5)) +
    coord_flip() +
    facet_wrap(~ region, ncol = 1) +
    labs(
      title = "Valence by Region",
      subtitle = paste0("Topic: ", legend_label),
      x = "",
      y = "",
      fill = "Valence Type:"
    ) +
    theme_minimal() +
    theme(legend.position = "top",
          plot.title = ggtext::element_textbox_simple(
            size = 18,
            padding = margin(5.5, 5.5, 5.5, 5.5),
            margin = margin(0, 0, 15.5, 0),
            fill = "cornsilk",
            box.color = "grey40",
            r = unit(9, "pt"),
            halign = .5,
            lineheight = 1
          ),
          
          plot.title.position = "plot",
          plot.subtitle = element_text(hjust = 0.5, family = "Lora",
                                       size = 14, colour = "black"),
          plot.caption = element_text(family = "Roboto Condensed", colour = "black", face = "bold"),
          plot.background = element_rect(fill = "white", colour = "white", size = 3),
          plot.margin = unit(c(7, 10, 3, 10), units = "mm"),
          panel.background = element_rect(fill = "white", colour = "NA"),
          strip.text = element_text(family = "Roboto Condensed", colour = "black", face = "bold"),
          
          legend.title = element_text(family = "Lora", size = 12, 
                                      hjust = 0.5, face = "bold", colour = "black"),
          legend.key = element_blank(),
          legend.key.width = unit(20, "mm"),
          legend.text = element_text(family = "Roboto Condensed", hjust = 0.5, size = 12,
                                     colour = "black"),
          legend.direction = "horizontal",
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(color="#DFDFDF"),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color="#DFDFDF"),
          axis.ticks = element_blank(),
          axis.text.y = element_text(family = "Roboto Condensed", hjust = 0.5, size = 10,
                                     colour = "black", face = "bold"),
          axis.text.x = element_blank(),
          axis.title = element_blank()) +
    scale_fill_manual(values = c("#DA9696", "#DFDFDF", "#81AB82"))
  
}

group_valence <- function(data){
  
  data %>%
    mutate(year = lubridate::year(date_time),
           region = str_to_title(region)) %>% 
    group_by(region, year, top_valence) %>%
    summarize(total = n()) %>%
    mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>%
    ungroup()
}

#------------------------ Export Object ---------------------------------------#

export_png <- function(x, legend_label){
  
  ggsave(filename = paste0("./viz/", legend_label, ".png"),
         plot = x,
         pointsize = 24, 
         width = 18 ,
         height = 14,
         scale = 0.5,
         dpi = 800)
  
}

export_prevalence_png <- function(x, legend_label){
  
  ggsave(filename = paste0("./viz/Prevalence_by_Region/", legend_label, ".png"),
         plot = x,
         pointsize = 24, 
         width = 16 ,
         height = 10,
         scale = 0.5,
         dpi = 800)
  
}

#------------------------ Plot Regression Coef --------------------------------#

plot_coef <- function(model, level){
  coefplot::coefplot(model,
                     # title = "Regression Estimates",
                     xlab = "Value",
                     ylab = "Coefficient",
                     innerCI = 1,
                     outerCI = 2,
                     color = "lightblue",
                     shape = 16,
                     cex = 0.8,
                     textAngle = 0,
                     numberAngle = 0,
                     zeroColor = "red") +
    theme_minimal() +
    labs(subtitle = paste0("Impact of ", level, " on Engagement")) +
    geom_text(aes(label = round(model$coefficients, 2  )), nudge_x = 0.2, nudge_y = 0.01, fontface = "bold") + 
    theme(legend.position = "top",
          plot.title = element_blank(
            # size = 18,
            # face = "bold"
          ),
          plot.subtitle = element_text(hjust = 0., family = "Lora",
                                       size = 14, colour = "black"),
          plot.caption = element_text(family = "Roboto Condensed", colour = "black", face = "bold"),
          plot.background = element_rect(fill = "white", colour = "white", size = 3),
          plot.margin = unit(c(7, 10, 3, 10), units = "mm"),
          panel.background = element_rect(fill = "white", colour = "NA"),
          strip.text = element_text(family = "Roboto Condensed", colour = "black", face = "bold", size = 11),
          
          legend.title = element_text(family = "Lora", size = 12, 
                                      hjust = 0.5, face = "bold", colour = "black"),
          legend.key = element_blank(),
          legend.background = element_rect(fill = "white", colour = "white"),
          legend.key.width = unit(10, "mm"),
          legend.key.size = unit(.1, "mm"),
          legend.text = element_text(family = "Roboto Condensed", hjust = 0.5, size = 12,
                                     colour = "black"),
          legend.direction = "horizontal",
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_line(color="#DFDFDF"),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color="black", linewidth = 1),
          axis.ticks = element_blank(),
          axis.text.y = element_text(family = "Roboto Condensed", hjust = 0.5, size = 10,
                                     colour = "black"),
          axis.title = element_blank())
  
}




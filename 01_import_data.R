#------------------------------------------------------------------------------
# Topic: Explore Twitter Data
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
library(readxl)
library(tidyverse)
library(purrr)
library(tidyr)
library(readr)
library(hrbrthemes)
library(patchwork)


#------------------------------- Import Data ----------------------------------#

df_labels <- readRDS("df_labels.RDS") %>% 
  mutate(id_img = sub(".*_", "", media_keys))

df_reaction_compiled <- readRDS("C:/Users/isaac/OneDrive/Escritorio/Twitter_data/vader_analysis/04_data_compiled/df_reaction_compiled.RDS")

#------------------------------- Subset Data ----------------------------------#

df_analysis <- df_reaction_compiled %>% 
  left_join(df_labels, by = c("conversation_id" = "id_original")) %>% 
  filter(!is.na(media_keys))

saveRDS(df_analysis, "df_analysis.RDS")

#------------------------------- Topic: Animals -------------------------------#
source("plot_functions.R")

df_analysis %>% 
  select(animals, top_valence, date_time, region) %>%
  filter(animals == "animals") %>%
  mutate(year = lubridate::year(date_time),
         region = str_to_title(region)) %>% 
  group_by(region, year, top_valence) %>%
  summarize(total = n()) %>%
  mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>%
  ungroup() %>%
  plot_valence_region("Animals")

df_analysis %>% 
  select(causes_binary, top_valence, date_time, region) %>%
  filter(causes_binary == "Yes") %>%
  mutate(year = lubridate::year(date_time),
         region = str_to_title(region)) %>% 
  group_by(region, year, top_valence) %>%
  summarize(total = n()) %>%
  mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>%
  ungroup() %>%
  plot_valence_region("Causes of Climate Change")


df_analysis %>% 
  select(animals, top_valence, date_time, region) %>%
  filter(animals == "animals") %>%
  group_valence() %>%
  plot_valence_region("Animals")

df_analysis %>% 
  select(causes_binary, top_valence, date_time, region) %>%
  filter(causes_binary == "Yes") %>%
  group_valence() %>%
  plot_valence_region("Causes of Climate Change")







plot_valence_region(df_analysis, "animals", "animals")

plot_valence_region(df_analysis, "causes_binary", "Yes",
                    "Chinese", "2020", "POS", "0")

df_analysis %>% 
  select(causes_binary, top_valence, date_time, region) %>%
  filter(causes_binary == "Yes") %>%
  mutate(year = lubridate::year(date_time),
         region = str_to_title(region)) %>% 
  group_by(region, year, top_valence) %>% 
  summarize(total = n()) %>% 
  # ungroup() %>% 
  # add_row(region = "Chinese", year = 2020, top_valence = "POS", total = 0) %>% 
  # group_by(region, year, top_valence) %>% 
  # summarize(total = sum(total)) %>%
  mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = percent, fill = top_valence)) +
  geom_bar(position="fill", stat="identity", width = 1) +
  geom_text(aes(label = label),
            size = 2.5, fontface = "bold",
            position = position_stack(vjust = .5)) +
  coord_flip() +
  facet_wrap(~ region, ncol = 1) +
  labs(
    title = "Prevalence of Valence by Region",
    subtitle = paste0("Topic: Causes of Climate Change"),
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
        axis.text.y = element_text(family = "Roboto Condensed", hjust = 0.5, size = 12,
                                   colour = "black", face = "bold"),
        axis.text.x = element_blank(),
        axis.title = element_blank()) +
  scale_fill_manual(values = c("#DA9696", "#DFDFDF", "#A5BBE3"))



df_analysis$causes_binary %>% table



df_analysis %>% 
  select(causes_binary, top_valence, date_time, region) %>%
  filter(causes_binary == "Yes") %>%
  mutate(year = lubridate::year(date_time),
         region = str_to_title(region)) %>% 
  group_by(region, year, top_valence) %>% 
  summarize(total = n()) %>% view


  mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>% 
  ungroup() %>% dim



df_analysis %>% 
  select(animals, top_valence, date_time, region) %>%
  filter(animals == "animals") %>%
  mutate(year = lubridate::year(date_time),
         region = str_to_title(region)) %>% 
  group_by(region, year, top_valence) %>% 
  summarize(total = n()) %>% 
  mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>% 
  ungroup() %>% dim







df_analysis %>% 
  select(animals, top_emotion, date_time, region) %>%
  filter(animals == "animals") %>% 
  filter(top_emotion != "others") %>% 
  group_by(region, top_emotion) %>% 
  summarize(total = n()) %>% 
  mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>% 
  ungroup() %>% 
  ggplot(aes(x = top_emotion, y = percent, fill = top_emotion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label),
            size = 2.5, fontface = "bold",
            position = position_stack(vjust = .5)) +
  facet_wrap(~ region) +
  labs(
    title = "Distribution of Emotion Types by Region",
    subtitle = "Topic: Animals",
    x = "Emotion Type",
    y = "Percentage",
    fill = "Emotion Type"
  ) +
  theme_minimal()

#------------------------------- Topic: Causes 1 ------------------------------#

df_analysis %>% 
  select(causes_binary, top_valence, date_time, region) %>%
  filter(causes_binary == "Yes") %>% 
  group_by(region, top_valence) %>% 
  summarize(total = n()) %>% 
  mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>% 
  ungroup() %>% 
  ggplot(aes(x = top_valence, y = percent, fill = top_valence)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label),
            size = 2.5, fontface = "bold",
            position = position_stack(vjust = .5)) +
  facet_wrap(~ region) +
  labs(
    title = "Distribution of Valence Types by Region",
    subtitle = "Topic: Causes of Climate Change",
    x = "Valence Type",
    y = "Percentage",
    fill = "Valence Type"
  ) +
  theme_minimal()


df_analysis %>% 
  select(causes_binary, top_emotion, date_time, region) %>%
  filter(causes_binary == "Yes") %>% 
  filter(top_emotion != "others") %>% 
  group_by(region, top_emotion) %>% 
  summarize(total = n()) %>% 
  mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>% 
  ungroup() %>% 
  ggplot(aes(x = top_emotion, y = percent, fill = top_emotion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label),
            size = 2.5, fontface = "bold",
            position = position_stack(vjust = .5)) +
  facet_wrap(~ region) +
  labs(
    title = "Distribution of Emotion Types by Region",
    subtitle = "Topic: Causes of Climate Change",
    x = "Emotion Type",
    y = "Percentage",
    fill = "Emotion Type"
  ) +
  theme_minimal()




df_analysis %>% 
  filter(region == "arabic") %>% 
  select(animals, top_valence, date_time, region) %>%
  mutate(year = lubridate::year(date_time)) %>% 
  group_by(year, top_valence) %>% 
  summarize(total = n()) %>% 
  mutate(percent = round((total/sum(total)), 3), label = paste0(percent*100, "%")) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = percent, fill = top_valence)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label),
            size = 2.5, fontface = "bold",
            position = position_stack(vjust = .5)) +
  facet_wrap(~ region) +
  labs(
    title = "Distribution of Valence Types by Region",
    subtitle = "Topic: Animals",
    x = "Valence Type",
    y = "Percentage",
    fill = "Valence Type"
  ) +
  theme_minimal()





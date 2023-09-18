#------------------------------------------------------------------------------
# Topic: Explore Twitter Data - Climate Change Post (2019 - 2023)
# Created by: Isaac Bravo
# Source: https://twitter.com
#------------------------------------------------------------------------------

#------------------------------- Load Libraries -------------------------------#
source("02_plot_functions.R")

load_libraries()

#------------------------------- Import Data ----------------------------------#

df_labels <- readRDS("./data/df_labels.RDS") 

#------------------------------- Prepare Data ---------------------------------#

df_full_reg <- df_labels %>% 
  dplyr::mutate_at(vars(12:21), as.factor) %>% 
  dplyr::select(-lang, -author_id, -text) %>%
  dplyr::mutate(engagement = retweet_count + like_count)

#---------------------- Run Negative Binomnial Regression ---------------------#

# Topic: Animals
m1 <- glm.nb(engagement ~ animals + region, data = df_full_reg)

# Topic: Causes Climate Change (Binary)
m2 <- glm.nb(engagement ~ causes_binary + region, data = df_full_reg)

# Topic: Causes Climate Change (Multiple)
m2.2 <- glm.nb(engagement ~ causes_multiple + region, data = df_full_reg)

# Topic: Consequences Climate Change (Binary)
m3 <- glm.nb(engagement ~ consequences_binary + region, data = df_full_reg)

# Topic: Consequences Climate Change (Multiple)
m3.2 <- glm.nb(engagement ~ consequences_multiple + region, data = df_full_reg)

# Topic: People
m4 <- glm.nb(engagement ~ people + region, data = df_full_reg)

# Topic: Food
m5 <- glm.nb(engagement ~ food + region, data = df_full_reg)

# Topic: Solutions
m6 <- glm.nb(engagement ~ solutions + region, data = df_full_reg)

# Topic: Setting
m7 <- glm.nb(engagement ~ setting + region, data = df_full_reg)

# Topic: Visuals
m8 <- glm.nb(engagement ~ visuals + region, data = df_full_reg)



#-------------------------- Save Regression Models ----------------------------#
model_list <- list()

model_list[["m1_animals"]] <- m1
model_list[["m2_causes_bin"]] <- m2
model_list[["m2.2_causes_mul"]] <- m2.2
model_list[["m3_consequences_bin"]] <- m3
model_list[["m3.2_consequences_mul"]] <- m3.2
model_list[["m4_people"]] <- m4
model_list[["m5_food"]] <- m5
model_list[["m6_solutions"]] <- m6
model_list[["m7_setting"]] <- m7
model_list[["m8_visuals"]] <- m8

saveRDS(model_list, "model_results.RDS")

#-------------------------- Plot Regression Models ----------------------------#  

plt1 <- plot_coef(m1, "Animals")
plt2 <- plot_coef(m2, "Causes of Climate Change")
plt3 <- plot_coef(m3, "Consequences of Climate Change")
plt4 <- plot_coef(m4, "People")

plt5 <- plot_coef(nb_engagement_solutions_1, "Protests")
plt6 <- plot_coef(nb_engagement_solutions_2, "Climate Summits")
plt7 <- plot_coef(nb_engagement_solutions_3, "Politics")

plt5 + plt6 + plt7

plt_total <- plt1 + plt2 + plt3 + plt4

plt_total

ggsave(filename = "./plt_total.png",
       plot = plt_total,
       pointsize = 24, 
       width = 28,
       height = 18,
       scale = 0.5,
       dpi = 800)

#------------------------------- Clean Workplace ------------------------------#  

# List all objects in the workspace
all_objects <- ls()

# Remove the selected objects
rm(list = all_objects)


plot_coef(m2.2, "Causes of Climate Change")



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

#------------------------------- Explore Data ---------------------------------#

lapply(df_labels[,12:21], function(x) {
  freq <- table(factor(x))
  freq_percentage <- round(prop.table(freq) * 100, 2)
  freq_table <- data.frame(Frequency = freq, Percentage = freq_percentage)
  freq_table <- freq_table %>% arrange(desc(Percentage.Freq))
  freq_table
})

#------------------------------- Topic: Animals -------------------------------#

df_labels %>%
  select(animals, date_time, region) %>%
  filter(animals == "animals") %>% 
  group_topic() %>% 
  plot_topic_year(., "Animals") %>% 
  export_png(., "topic_animals_year")

#------------------------------- Topic: Causes of CC --------------------------#

df_labels %>%
  select(causes_binary, date_time, region) %>%
  filter(causes_binary == "Yes") %>% 
  group_topic() %>% 
  plot_topic_year(., "Causes of Climate Change") %>% 
  export_png(., "topic_causes_CC_year")

# Causes 1: Greenhouse gases
df_labels %>%
  select(causes_multiple, date_time, region) %>%
  filter(causes_multiple == "Greenhouse gases") %>% 
  group_topic() %>% 
  plot_topic_year(., "Causes of Climate Change: Greenhouse gases") %>% 
  export_png(., "topic_causes_CC_greenhouses_year")

# Causes 2: Traffic/Pollution
df_labels %>%
  select(causes_multiple, date_time, region) %>%
  filter(causes_multiple == "Traffic/Pollution") %>% 
  group_topic() %>% 
  plot_topic_year(., "Causes of Climate Change: Traffic/Pollution") %>% 
  export_png(., "topic_causes_CC_pollution_year")


# Causes 3: Livestock production
df_labels %>%
  select(causes_multiple, date_time, region) %>%
  filter(causes_multiple == "Livestock production") %>% 
  group_topic() %>% 
  plot_topic_year(., "Causes of Climate Change: Livestock production") %>% 
  export_png(., "topic_causes_CC_livestock_year")


# Causes 4: Deforestation
df_labels %>%
  select(causes_multiple, date_time, region) %>%
  filter(causes_multiple == "Deforestation") %>% 
  group_topic() %>% 
  plot_topic_year(., "Causes of Climate Change: Deforestation") %>% 
  export_png(., "topic_causes_CC_deforestation_year")


#------------------------------- Topic: Consequences of CC --------------------#

df_labels %>%
  select(consequences_binary, date_time, region) %>%
  filter(consequences_binary == "Yes") %>% 
  group_topic() %>% 
  plot_topic_year(., "Consequences of Climate Change")

# Consequences 1: Extreme Temperatures
df_labels %>%
  select(consequences_multiple, date_time, region) %>%
  filter(consequences_multiple == "Extreme Temperatures") %>% 
  group_topic() %>% 
  plot_topic_year(., "Consequences of Climate Change: Extreme Temperatures") %>% 
  export_png(., "topic_consequences_CC_temperatures_year")

# Consequences 2: Melting Ice/Sea Level Rise
df_labels %>%
  select(consequences_multiple, date_time, region) %>%
  filter(consequences_multiple == "Melting Ice/Sea Level Rise") %>% 
  group_topic() %>% 
  plot_topic_year(., "Consequences of Climate Change: Melting Ice/Sea Level Rise") %>% 
  export_png(., "topic_consequences_CC_seallevel_year")


# Consequences 3: Biodiversity loss
df_labels %>%
  select(consequences_multiple, date_time, region) %>%
  filter(consequences_multiple == "biodiversity loss") %>% 
  group_topic() %>% 
  plot_topic_year(., "Consequences of Climate Change: Biodiversity loss") %>% 
  export_png(., "topic_consequences_CC_biodiversity_year")


# Consequences 4: Droughts
df_labels %>%
  select(consequences_multiple, date_time, region) %>%
  filter(consequences_multiple == "drought") %>% 
  group_topic() %>% 
  plot_topic_year(., "Consequences of Climate Change: Droughts") %>% 
  export_png(., "topic_consequences_CC_droughts_year")

# Consequences 5: Wildfires
df_labels %>%
  select(consequences_multiple, date_time, region) %>%
  filter(consequences_multiple == "wildfires") %>% 
  group_topic() %>% 
  plot_topic_year(., "Consequences of Climate Change: Wildfires") %>% 
  export_png(., "topic_consequences_CC_wildfires_year")

# Consequences 6: Deforestation
df_labels %>%
  select(consequences_multiple, date_time, region) %>%
  filter(consequences_multiple == "floods") %>% 
  group_topic() %>% 
  plot_topic_year(., "Consequences of Climate Change: Floods") %>% 
  export_png(., "topic_consequences_CC_floods_year")


#------------------------------- Topic: People --------------------------------#

df_labels %>%
  select(people, date_time, region) %>%
  filter(people == "Yes") %>% 
  group_topic() %>% 
  plot_topic_year(., "People")

#------------------------------- Topic: Food ----------------------------------#

df_labels %>%
  select(food, date_time, region) %>%
  filter(food == "Yes") %>% 
  group_topic() %>% 
  plot_topic_year(., "Food")

#------------------------------- Topic: Setting ----------------------------------#

# Setting 1: Nature
df_labels %>%
  select(setting, date_time, region) %>%
  filter(setting == "Nature") %>% 
  group_topic() %>% 
  plot_topic_year(., "Setting Nature") %>% 
  export_png(., "topic_setting_nature_year")

# Setting 2: Indoor space
df_labels %>%
  select(setting, date_time, region) %>%
  filter(setting == "Indoor space") %>% 
  group_topic() %>% 
  plot_topic_year(., "Setting Indoor space") %>% 
  export_png(., "topic_setting_indoor_year")

# Setting 3: Nature
df_labels %>%
  select(setting, date_time, region) %>%
  filter(setting == "Agricultural Area") %>% 
  group_topic() %>% 
  plot_topic_year(., "Setting Agricultural Area") %>% 
  export_png(., "topic_setting_agricultural_year")

# Setting 4: Nature
df_labels %>%
  select(setting, date_time, region) %>%
  filter(setting == "Commercial Area") %>% 
  group_topic() %>% 
  plot_topic_year(., "Setting Commercial Area") %>% 
  export_png(., "topic_setting_commercial_year")

# Setting 5: Nature
df_labels %>%
  select(setting, date_time, region) %>%
  filter(setting == "Industrial Area") %>% 
  group_topic() %>% 
  plot_topic_year(., "Setting Industrial Area") %>% 
  export_png(., "topic_setting_industrial_year")

# Setting 6: Nature
df_labels %>%
  select(setting, date_time, region) %>%
  filter(setting == "Residential Area") %>% 
  group_topic() %>% 
  plot_topic_year(., "Setting Residential Area") %>% 
  export_png(., "topic_setting_residential_year")

#------------------------------- Topic: Solutions -----------------------------#

# Solutions 1: Climate summits
df_labels %>%
  select(solutions, date_time, region) %>%
  filter(solutions == "Climate summits") %>% 
  group_topic() %>% 
  plot_topic_year(., "Climate summits") %>% 
  export_png(., "topic_solutions_summits_year")

# Solutions 2: Energy efficiency/recycling
df_labels %>%
  select(solutions, date_time, region) %>%
  filter(solutions == "Energy efficiency/recycling") %>% 
  group_topic() %>% 
  plot_topic_year(., "Energy efficiency/recycling") %>% 
  export_png(., "topic_solutions_recycling_year")

# Solutions 3: Energy production/storage
df_labels %>%
  select(solutions, date_time, region) %>%
  filter(solutions == "Energy production/storage") %>% 
  group_topic() %>% 
  plot_topic_year(., "Energy production/storage") %>% 
  export_png(., "topic_solutions_energy_year")

# Solutions 4: Politics
df_labels %>%
  select(solutions, date_time, region) %>%
  filter(solutions == "politics") %>% 
  group_topic() %>% 
  plot_topic_year(., "Politics") %>% 
  export_png(., "topic_solutions_politics_year")

# Solutions 5: Protests
df_labels %>%
  select(solutions, date_time, region) %>%
  filter(solutions == "Protests") %>% 
  group_topic() %>% 
  plot_topic_year(., "Protests") %>% 
  export_png(., "topic_solutions_protests_year")

# Solutions 6: Smart Grids
df_labels %>%
  select(solutions, date_time, region) %>%
  filter(solutions == "smart grids") %>% 
  group_topic() %>% 
  plot_topic_year(., "Smart Grids") %>% 
  export_png(., "topic_solutions_smartgrids_year")

# Solutions 7: Electric transports
df_labels %>%
  select(solutions, date_time, region) %>%
  filter(solutions == "Electric transport") %>% 
  group_topic() %>% 
  plot_topic_year(., "Electric transport") %>% 
  export_png(., "topic_solutions_transport_year")


#------------------------------- Topic: Visuals -------------------------------#

# Visuals 1: Events/Posters
df_labels %>%
  select(visuals, date_time, region) %>%
  filter(visuals == "Events/Posters") %>% 
  group_topic() %>% 
  plot_topic_year(., "Events/Posters") %>% 
  export_png(., "topic_visual_posters_year")

# Visuals 2: Graphs/Maps
df_labels %>%
  select(visuals, date_time, region) %>%
  filter(visuals == "Graphs/Maps") %>% 
  group_topic() %>% 
  plot_topic_year(., "Graphs/Maps") %>% 
  export_png(., "topic_visual_graphs_year")

# Visuals 3: screenshot
df_labels %>%
  select(visuals, date_time, region) %>%
  filter(visuals == "screenshot") %>% 
  group_topic() %>% 
  plot_topic_year(., "Screenshot") %>% 
  export_png(., "topic_visual_screenshot_year")

# Visuals 4: Illustration/Infographic
df_labels %>%
  select(visuals, date_time, region) %>%
  filter(visuals == "Illustration/Infographic") %>% 
  group_topic() %>% 
  plot_topic_year(., "Illustration/Infographics") %>% 
  export_png(., "topic_visual_infographics_year")

# Visuals 5: Satellite images
df_labels %>%
  select(visuals, date_time, region) %>%
  filter(visuals == "Satellite images") %>% 
  group_topic() %>% 
  plot_topic_year(., "Satellite images") %>% 
  export_png(., "topic_visual_satellite_year")

# Visuals 6: Memes
df_labels %>%
  select(visuals, date_time, region) %>%
  filter(visuals == "Meme") %>% 
  group_topic() %>% 
  plot_topic_year(., "Memes") %>% 
  export_png(., "topic_visual_meme_year")



#------------------------------- Clean Workplace ------------------------------#  

# List all objects in the workspace
all_objects <- ls()

# Remove the selected objects
rm(list = all_objects)


  
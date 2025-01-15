
# load packages
library(tidyverse)

# reading in data
sample_data <- read_csv("data/sample_data.csv")
summarize(sample_data,average_cells = mean(cells_per_ml))
sample_data %>% 
  summarize( average_cells = mean(cells_per_ml)) 

# filtering rows

sample_data %>%
  filter(env_group == "Deep") %>%
  summarize(average_cells = mean(cells_per_ml))


sample_data %>%
  filter(env_group %in% c("Deep", "Shallow_May"))


sample_data %>%
  filter(str_detect(env_group, "Shallow"))

# average chloroplyll

avg_chlorophyll = sample_data %>%
  summarize(average_chlorophyll = mean(chlorophyll) )


# average chlorophyll in Shallow_September

avg_chloroplyll_Shallow_Sep = sample_data %>%
  filter(env_group == "Shallow_September") %>%
  summarize(avgChlo_Shallow_Sep = mean(chlorophyll))

sample_data %>%
  filter(str_detect(env_group, "September")) %>%
  summarize(avgChlo_Shallow_Sep = mean(chlorophyll))

# group_by

sample_data %>%
  group_by(env_group) %>%
  summarize(avg_cells = mean(cells_per_ml),
            min_cells = min(cells_per_ml))

#  ave temp per env_group

sample_data %>%
  group_by(env_group) %>%
  summarize(avg_temp = mean(temperature))

# mutate
#TN:Tp

sample_data2 = sample_data %>%
  mutate(tn_tp = total_nitrogen / total_phosphorus)

sample_data %>%
  mutate(temp_is_hot = temperature > 8) %>% 
  group_by(env_group, temp_is_hot) %>%
  summarize(avg_tem = mean(temperature),
            avg_cells = mean(cells_per_ml))


# selecting columns

sample_data %>%
  select(sample_id, depth)

sample_data %>%
  select(-env_group)


sample_data %>%
  select(sample_id:temperature)

sample_data %>%
  select(starts_with("total"))


#dataframe with sample_id, env_group, depth, tem, cells_per_ml

sample_data %>%
  select(sample_id,env_group,depth,temperature,cells_per_ml)


sample_data %>%
  select(1:5 )

sample_data %>%
  select(-(total_nitrogen:chlorophyll))


# cleaning data

taxon_clean <- read_csv("data/taxon_abundance.csv",skip = 2) %>%
  select(-...10) %>%
  rename(sequencer = ...9) %>%
  select(-Lot_Number, -sequencer)


taxon_long <- taxon_clean %>%
  pivot_longer(cols = Proteobacteria:Cyanobacteria,
               names_to = "Phylum",
               values_to = "Abundance")

taxon_long %>%
  group_by(Phylum) %>%
  summarize(avg_abun = mean(Abundance))

taxon_long %>%
  ggplot() +
  aes(x = sample_id,
      y = Abundance,
      fill = Phylum) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))


# long to wide

taxon_long %>%
  pivot_wider(names_from = "Phylum",
              values_from = "Abundance")

# joining dataframes
head(sample_data)
head(taxon_clean)

# inner join
inner_join(sample_data,taxon_clean, by = "sample_id")


taxon_clean_goodSep <- taxon_clean %>%
  mutate(sample_id = str_replace(sample_id, pattern = "Sep","September"))


sample_and_taxon <- inner_join(sample_data,taxon_clean_goodSep, by = "sample_id")


anti_join(sample_data,taxon_clean, by = 'sample_id')
sample_data$sample_id
taxon_clean$sample_id


write_csv(sample_and_taxon, file = "data/sample_and_taxon.csv")

# make a plot
# Where does Chloroflexi like to live

sample_and_taxon %>%
  ggplot() +
  aes(x = depth,
      y = Chloroflexi) +
  geom_point() +
  labs(x = "Depth (m)",
       y = "Chloroflexi relative abundance") +
  geom_smooth(method = "lm") +
  #stat_regline_equation() +
  stat_cor() + 
  annotate(geom = "text",
           x = 25,
           y = 0.3,
           label = "This is a text label")

#install.packages("ggpubr") 
library(ggpubr)


sample_data_3 = sample_and_taxon %>%
  group_by(env_group) %>%
  summarize(avg_chloro_abun = mean(Chloroflexi), sd_chloro_abun = sd(Chloroflexi))

taxon_long %>%
  arrange(desc(Abundance))








  
  



























  














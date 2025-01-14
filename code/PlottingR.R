2+2


library(tidyverse)
sample_data <- read_csv("sample_data.csv")

#read_csv(file = "sample_data.csv")

# assign values to objects
name <- "agar"
name

year <-1881
year

name <- "Fanny Hesse"
name

Flower <- "merigold"
Flower

# comments
Sys.Date() # current data

getwd() # working directory


# plot 1

ggplot(data = sample_data) +
  aes(x = temperature) + 
  labs(x = "Temperature (C)") + 
  aes(y = cells_per_ml/1000000) +
  labs(y = "Cells (millions/mL)") +
  geom_point() +
  labs(title = "Does temperature affect microbial abundance?") +
  aes(colour = env_group) +
  aes(size = chlorophyll) +
  aes(shape = env_group) +
  labs(size = "Chlorophyll (ug/L)", 
       colour = "Environmental Group",
       shape = "Environmental Group")

ggplot(data = sample_data) +
  aes(x = temperature,
      y = cells_per_ml,
      colour = env_group,
      size = chlorophyll) +
  geom_point() + 
  labs(x = "Temperature (C)",
       y = "Cells (millions/mL)",
       title = "Does temperature affect microbial abundance?",
       size = "Chlorophyll (ug/L)",
       colour = "Environmental Group",
       shape = "Environmental Group")

# importing data sets

buoy_data <- read_csv("buoy_data.csv")
View(buoy_data)
dim(buoy_data)
head(buoy_data)
tail(buoy_data)

# plot 2 with facets
ggplot(data = buoy_data) +
  aes(x = day_of_year,
      y = temperature,
      group = sensor,
      colour = depth) +
  geom_line() +
  facet_wrap(~buoy, scales = "free_y")


buoy_data <- read_csv("buoy_data.csv")
View(buoy_data)
dim(buoy_data)
head(buoy_data)
tail(buoy_data)

# plot 2 with facet grid
ggplot(data = buoy_data) +
  aes(x = day_of_year,
      y = temperature,
      group = sensor,
      colour = depth) +
  geom_line() +
  facet_grid(rows = vars(buoy)) #rows = "buoy" also works


# structure
str(buoy_data)


# plot 3
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill = env_group)) + # color/fill = "pink"
  scale_fill_manual(values = c("pink","tomato","papayawhip"))
  #geom_violin()
  #geom_point() +
  #geom_jitter(aes(size = chlorophyll)) #sld come after the boxplot ()


ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill = env_group)) + # color/fill = "pink"
  scale_fill_brewer(palette = "Set1")


ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill = env_group)) + # color/fill = "pink"
  scale_fill_brewer(palette = "Set1")

# custom palette
#install.packages("wesanderson")
library(wesanderson)
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill = env_group)) + 
  scale_fill_manual(values = wes_palette('Cavalcanti1'))


ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) + 
  geom_boxplot(aes(fill = env_group)) +
  scale_fill_manual(values = wes_palette('Cavalcanti1'))


ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(fill = "darkblue", alpha = 0.3)

sample(colors())

RColorBrewer::display.brewer.all()


# plot 4 univariate plots
# transparency
ggplot(sample_data) +
  aes(x = cells_per_ml) +
  #geom_histogram(bins = 10)
  geom_density(aes(fill = env_group), alpha = 0.5) +
  theme_minimal() #change the background
 #theme_bw()

#rotate axis
plot1 = ggplot(sample_data) +
  aes(y = cells_per_ml,
      x = env_group) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
plot1

plot1 + theme_bw()


# saving plots
ggsave("plot1_boxplot.jpg", width = 6, height = 4, dpi = 500)








.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(tidyverse)
require("RColorBrewer")

read_csv("Data/Collection_survey_RAW.csv")
Collection_survey <- read_csv("Data/Collection_survey_RAW.csv")

#exclude obs. for which year is unknown
#convert <chr> to <dbl>
#create an age column
#rename formaldehyde concentration column 
#create a new column fixed

Collection_survey_tidy <- Collection_survey %>% 
  filter(Year != "-") %>% 
  mutate(year = as.numeric(Year)) %>% 
  select(-Year) %>% 
  mutate(age = 2019 - year) %>% 
  select(-year) %>% 
  rename(formaldehyde = "Formaldehyde concentration (mg/L)") %>% 
  mutate(fixed = ifelse(formaldehyde > 0, "Fixed", "unfixed"))


Collection_survey_tidy

#plot showing all data 
Collection_survey_full_plot <- ggplot(data = Collection_survey_tidy, 
       mapping = aes(x = pH, y = formaldehyde, color = age, shape = fixed)) +
  geom_point(size = 2) +
  scale_y_log10() +
  scale_colour_continuous(type = "viridis") +
  labs(
    title = "Figure 1",
    y = "Formaldehyde Concentration (mg/L)",
    color = "Age (yrs)",
    shape = "")

ggsave(filename = "Results/Collection_survey_full.png", plot = Collection_survey_full_plot, width = 12, height = 10, dpi = 300, units = "cm")





.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(tidyverse)
library(broom)

install.packages("cowplot")
.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(cowplot)

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
  mutate(fixed = ifelse(formaldehyde > 0, "fixed", "unfixed"))
 

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

#pie chart of fixed versus unfixed 

Collection_survey_tidy %>% 
  group_by(fixed)

piechart <- ggplot(df, aes(x = "", y = value, fill=group))+
  geom_bar(width = 1, stat = "identity")


#range of pH in fixed and unfixed specimens 
pH_fixed_unfixed <- ggplot(data = Collection_survey_tidy, 
                                      mapping = aes(y = pH, x = fixed)) +
  geom_boxplot() +
  labs(
    x = "")


t_test_A = Collection_survey_tidy %>%
  t.test( pH ~ fixed, data=.)



#range of age in fixed and unfixed specimens 
age_fixed_unfixed <- ggplot(data = Collection_survey_tidy, 
                           mapping = aes(y = age, x = fixed)) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean,
               geom = "crossbar", width = 0.2, color = "red") +
  geom_jitter() +
  labs(
    x = "")

t_test_result = Collection_survey_tidy %>%
  t.test( pH ~ fixed, data=.)


ggsave(filename = "Results/Collection_survey_full.png", plot = Collection_survey_full_plot, width = 12, height = 10, dpi = 300, units = "cm")

#Filter to plot only fixed specimens 

Collection_survey_fixed <- Collection_survey %>% 
  filter(Year != "-") %>% 
  mutate(year = as.numeric(Year)) %>% 
  select(-Year) %>% 
  mutate(age = 2019 - year) %>% 
  select(-year) %>% 
  rename(formaldehyde = "Formaldehyde concentration (mg/L)") %>% 
  mutate(fixed = ifelse(formaldehyde > 0, "fixed", "unfixed")) %>% 
  filter(fixed == "fixed")
  
  
plot_F_pH <- ggplot(data = Collection_survey_fixed, 
         mapping = aes(x = pH, y = formaldehyde, color = age)) +
  geom_point(size = 2) +
  scale_y_log10() +
  geom_smooth(method = "lm", size = 0.5) +
  scale_colour_continuous(type = "viridis") +
  labs(
    y = "Formaldehyde Concentration (mg/L)",
    color = "Age (yrs)")

ggsave(filename = "Results/Collection_survey_F_pH.png", plot = plot_F_pH, width = 12, height = 10, dpi = 300, units = "cm")


#What is the relationship between age and formaldehyde concentration?
#looks like specimens <10 years old have a lower mean formaldehyde concentration. 
#creat new column with age>10 years

categorical10yrs <- Collection_survey_fixed %>% 
  mutate(category10yrs = age > 10)

plot_F_age <- ggplot(data = categorical10yrs, 
                     mapping = aes(x = category10yrs, y = formaldehyde)) +
  geom_jitter() +
  scale_y_log10() +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean,
               geom = "crossbar", width = 0.2, color = "red") +
  labs(
    y = "Formaldehyde Concentration (mg/L)", 
    x = "Specimen > 10 years old")

ggsave(filename = "Results/Collection_survey_F_age.png", plot = plot_F_age, width = 12, height = 10, dpi = 300, units = "cm")

Collection_survey_unfixed <- Collection_survey %>% 
  filter(Year != "-") %>% 
  mutate(year = as.numeric(Year)) %>% 
  select(-Year) %>% 
  mutate(age = 2019 - year) %>% 
  select(-year) %>% 
  rename(formaldehyde = "Formaldehyde concentration (mg/L)") %>% 
  mutate(fixed = ifelse(formaldehyde > 0, "fixed", "unfixed")) %>% 
  filter(fixed == "unfixed")


plot_unfixed <- ggplot(data = Collection_survey_fixed, 
        mapping = aes(x = pH, y = age)) +
  geom_point(size = 2) +
  labs(
    title = "Figure 1",
    y = "Age (yrs)",
    shape = "")

#Combining plots into Figure 1

plot_grid(pH_fixed_unfixed, age_fixed_unfixed, plot_F_age, plot_F_pH)



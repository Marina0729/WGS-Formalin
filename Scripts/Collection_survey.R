
.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(tidyverse)
library(broom)
library(scales)
library(ggthemes)

install.packages("cowplot")
.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(cowplot)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("thomasp85/patchwork")
library(patchwork)

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

fixed_summary <- Collection_survey_tidy %>% 
  group_by(fixed) %>% 
  summarise(value = n())

piechart <- ggplot(fixed_summary, aes(x = "", y = value, fill = fixed)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(axis.text.x = element_blank (), 
        plot.title = element_text( hjust = 0.5, size = 20, lineheight = 1), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 10)) +
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = value), size=5)+
  labs(
    title = "Number of specimens",
    fill = "",
    tag = "A"
  )

ggsave(filename = "Results/Collection_survey_full.png", plot = Collection_survey_full_plot, width = 12, height = 10, dpi = 300, units = "cm")

#range of pH in fixed and unfixed specimens 
pH_fixed_unfixed <- ggplot(data = Collection_survey_tidy, 
                                      mapping = aes(y = pH, x = fixed)) +
  geom_boxplot(aes(color = fixed)) +
  labs(
    x = "",
    tag = "B")


t_test_A = Collection_survey_tidy %>%
  t.test( pH ~ fixed, data=.)



#range of age in fixed and unfixed specimens 
age_fixed_unfixed <- ggplot(data = Collection_survey_tidy, 
                           mapping = aes(y = age, x = fixed)) +
  geom_boxplot(aes(color = fixed)) +
  labs(
    x = "",
    y = "Age (yrs)",
    tag = "C")

t_test_result = Collection_survey_tidy %>%
  t.test( age ~ fixed, data=.)


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
    color = "Age (yrs)",
    tag = "E")

ggsave(filename = "Results/Collection_survey_F_pH.png", plot = plot_F_pH, width = 12, height = 10, dpi = 300, units = "cm")


#What is the relationship between age and formaldehyde concentration?
#looks like specimens <10 years old have a lower mean formaldehyde concentration. 
#creat new column with age>10 years

categorical10yrs <- Collection_survey_fixed %>% 
  mutate(category10yrs = age > 10) %>% 
  mutate(age_range = ifelse(category10yrs == TRUE, ">10 years", "<10 years"))

plot_F_age <- ggplot(data = categorical10yrs, 
                     mapping = aes(x = age_range, y = formaldehyde)) +
  geom_jitter(width = 0.3) +
  scale_y_log10() +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean,
               geom = "crossbar", width = 0.2, color = "red") +
  labs(
    y = "Formaldehyde Concentration (mg/L)", 
    x = "Specimen age",
    tag = "D")

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

plot_grid(piechart, pH_fixed_unfixed, age_fixed_unfixed, plot_F_age, plot_F_pH, ncol = 2, labels = "AUTO")

piechart + pH_fixed_unfixed


Figure1 <-piechart + {pH_fixed_unfixed + age_fixed_unfixed +plot_layout(ncol = 1)} + plot_F_age + plot_F_pH + plot_layout(nrow = 2, height =  c(8, 5), width = c(4, 5))

ggsave(filename = "Results/Figure1.png", plot = Figure1, width = 10, height = 12)

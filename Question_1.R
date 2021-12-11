raw_data <- read_csv("assignment_2_dataset_1.csv")

head(raw_data)

#Tidying our data
q1_data <- raw_data %>%
  rename(visual_quality = condition) %>% 
  mutate(visual_quality = recode(visual_quality,
                            "condition_a" = "Normal",
                            "condition_b" = "Degraded")) %>% 
  mutate(visual_quality = factor(visual_quality))

head(q1_data)

#Summarising our data
q1_data %>% 
  group_by(visual_quality) %>% 
  summarise(mean = mean(response_time), sd = sd(response_time))

#Data Visualisation
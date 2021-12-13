raw_data_2 <- read_csv("assignment_2_dataset_2.csv")

head(raw_data_2)

q2_data <- raw_data_2 %>% 
  rename(visual_quality = condition) %>% 
  mutate(visual_quality = recode(visual_quality,
                                 "condition_a" = "normal",
                                 "condition_b" = "degreaded")) %>% 
  mutate(visual_quality = factor(visual_quality))

head(q2_data)


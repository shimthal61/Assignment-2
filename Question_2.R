raw_data_2 <- read_csv("assignment_2_dataset_2.csv")

head(raw_data_2)

q2_data <- raw_data_2 %>% 
  rename(visual_quality = condition) %>% 
  mutate(visual_quality = recode(visual_quality,
                                 "condition_a" = "Normal",
                                 "condition_b" = "Degreaded")) %>%
  mutate(visual_quality = factor(visual_quality), caffeine = factor(caffeine))

head(q2_data)

# data summarising, have already done this in the question before though
q2_data %>% 
  group_by(visual_quality) %>% 
  summarise(mean = mean(response_time), sd = sd(response_time)) %>% 
  arrange(mean)

set.seed(42)

q2_data %>%
  ggplot(aes(x = caffeine, y = response_time, colour = visual_quality)) +
  geom_point(size = 1.5, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  labs(x = "Cups of coffee",
       y = "Response Time (ms)",
       colour = "Visual Quality")

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
# Use the set.seed function when running simulations to ensure all results, figures, etc are reproducible.
set.seed(42)
q1_data %>% 
  ggplot(aes(x = visual_quality, y = response_time, colour = visual_quality)) +
  geom_violin(width = 0.3) +
  geom_point(position = position_jitter(width = 0.15, seed = 42)) +
  theme_minimal() +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = 'none') +
  labs(title = "Examining the effect of visual quality on response times",
       x = "Visual Quality",
       y = "Reaction Time (ms)") +
  theme(text = element_text(size = 13))


model1 <- aov_4(response_time ~ visual_quality + (1 | participant), data = q1_data)  


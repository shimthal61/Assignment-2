font_add_google("Lato")

raw_data_1 <- read_csv("assignment_2_dataset_1.csv")

head(raw_data_1)

#Tidying our data
q1_data <- raw_data_1 %>%
  rename(visual_quality = condition) %>% 
  mutate(visual_quality = recode(visual_quality,
                            "condition_a" = "Normal",
                            "condition_b" = "Degraded")) %>% 
  mutate(visual_quality = factor(visual_quality))

head(q1_data)

#Doesn't look like there is any missing data
vis_miss(q1_data)

#Summarising our data
q1_data %>% 
  group_by(visual_quality) %>% 
  summarise(mean = mean(response_time), sd = sd(response_time)) %>% 
  arrange(mean)

q1_data %>% 
  group_by(visual_quality) %>% 
  arrange(response_time)

#It appears at a glance that participants in the normal condition has a faster response time.

#Data Visualisation
# Use the set.seed function when running simulations to ensure all results, figures, etc are reproducible.
set.seed(42)

q1_data %>% 
  mutate(visual_quality = fct_relevel(visual_quality, "Normal", "Degraded")) %>% 
  ggplot(aes(x = visual_quality, y = response_time, colour = visual_quality)) +
  geom_violin(width = 0.3) +
  geom_point(alpha = 0.8, position = position_jitter(width = 0.15, seed = 42)) +
  theme_minimal() +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = 'none') +
  scale_y_continuous(breaks = seq(925, 1075, by = 25),
                     limits = c(925, 1075)) +
  labs(title = "Examining the effect of visual quality on response times",
       x = "Visual Quality",
       y = "Response Time (ms)") +
  theme(text = element_text(size = 13))

q1_data %>% 
  mutate(visual_quality = fct_relevel(visual_quality, "Normal", "Degraded")) %>% 
  ggplot(aes(x = visual_quality, y = response_time, colour = visual_quality)) +
  geom_violin(width = 0.3) +
  geom_point(alpha = 0.8, position = position_jitter(width = 0.15, seed = 42)) +
  theme_minimal() +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = 'none') +
  scale_y_continuous(breaks = seq(925, 1075, by = 25),
                     limits = c(925, 1075)) +
  labs(title = "Examining the effect of visual quality on response times",
       x = "Visual Quality",
       y = "Response Time (ms)") +
  theme(text = element_text(family = "Lato"))


#F value is pretty large and p < 0.001. However, we don't know  what's driving the difference yet
model1 <- aov_4(response_time ~ visual_quality + (1 | participant), data = q1_data)

anova(model1)

# WE DON'T NEED TO DO PAIRWISE AS WE ONLY HAVE 2 LEVELS

# We want to use bonferroni pairwise comparions because... (refer back to vid in workshop)
emmeans(model1, pairwise ~ visual_quality, adjust = "bonferroni")

# Participants performed significantly better then the degraded group. 

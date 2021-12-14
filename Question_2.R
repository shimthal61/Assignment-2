raw_data_2 <- read_csv("assignment_2_dataset_2.csv")

head(raw_data_2)

q2_data <- raw_data_2 %>% 
  rename(visual_quality = condition) %>% 
  mutate(visual_quality = recode(visual_quality,
                                 "condition_a" = "Normal",
                                 "condition_b" = "Degreaded")) %>%
  mutate(visual_quality = factor(visual_quality))

head(q2_data)

# data summarising, have already done this in the question before though
q2_data %>% 
  group_by(visual_quality) %>% 
  summarise(mean = mean(response_time), sd = sd(response_time)) %>% 
  arrange(mean)

set.seed(42)

# Let's examine the relationship between our covariate (caffeine) on our DV (response time)

q2_data %>%
  ggplot(aes(x = caffeine, y = response_time, colour = visual_quality)) +
  geom_point(size = 1.5, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  labs(x = "Cups of coffee",
       y = "Response Time (ms)",
       colour = "Visual Quality")

# Doesn't look like there is much of a relationship between caffeine and response time


# We have to take account of our covariate - cups of coffee

# Running an ANOVA for our covariate??
model_covariate <- aov_4(response_time ~ caffeine + (1 | participant), data = q2_data)

anova(model_covariate)

model_ancova <- aov_4(response_time ~ caffeine + visual_quality + (1 | participant), data = q2_data, factorize = FALSE)

anova(model_ancova)

# When we control for the effect of our covariate (cups of coffee), we can see that visual quality is 
# no longer significant (F = 3.5730, p = 0.062). Our covariate isn't significant either.
# A lot of the variance we saw in our ANOVA model can be attributed to cups of coffee

# We need to perform pairwise to take into consideration the influence of caffeine on our group
emmeans(model_ancova, pairwise ~ visual_quality)
 


raw_data_2 <- read_csv("assignment_2_dataset_2.csv")

head(raw_data_2)

q2_data_tidied <- raw_data_2 %>% 
  rename(visual_quality = condition) %>% 
  mutate(visual_quality = recode(visual_quality,
                                 "condition_a" = "Normal",
                                 "condition_b" = "Degreaded")) %>%
  mutate(visual_quality = factor(visual_quality))

head(q2_data_tidied)

# data summarising, have already done this in the question before though

q2_summarised <- q2_data_tidied %>% 
  group_by(visual_quality, caffeine) %>% 
  summarise(mean = mean(response_time), sd = sd(response_time)) %>% 
  arrange(mean)

set.seed(42)

# Let's examine the relationship between our covariate (caffeine) on our DV (response time)

#Line of best fit
q2_data_tidied %>%
  mutate(visual_quality = fct_relevel(visual_quality, "Normal", "Degraded")) %>% 
  ggplot(aes(x = caffeine, y = response_time, colour = visual_quality)) +
  geom_smooth(aes(x = caffeine, y = response_time), method = "lm", se = FALSE, inherit.aes = FALSE) +
  geom_point(size = 1.5, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  labs(x = "Cups of coffee",
       y = "Response Time (ms)",
       colour = "Visual Quality") +
  scale_y_continuous(breaks = seq(950, 1075, by = 25),
                     limits = c(950, 1075)) +
  theme(text = element_text(family = "lato", size = 13))

q2_data_tidied %>%
  mutate(visual_quality = fct_relevel(visual_quality, "Normal", "Degraded")) %>% 
  ggplot(aes(x = caffeine, y = response_time, colour = visual_quality)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(size = 1.5, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  labs(x = "Cups of coffee",
       y = "Response Time (ms)",
       colour = "Visual Quality") +
  scale_y_continuous(breaks = seq(950, 1075, by = 25),
                     limits = c(950, 1075)) +
  theme(text = element_text(family = "lato", size = 13))

# Doesn't look like there is much of a relationship between caffeine and response time


# We have to take account of our covariate - cups of coffee

# Running an ANOVA for our covariate??
model_covariate <- aov_4(response_time ~ caffeine + (1 | participant), data = q2_data)

anova(model_covariate)

model_ancova <- aov_4(response_time ~ caffeine + visual_quality + (1 | participant),
                      data = q2_data, factorize = FALSE)

anova(model_ancova)

# When we control for the effect of our covariate (cups of coffee), we can see that visual quality is 
# no longer significant (F = 3.5730, p = 0.062). Our covariate isn't significant either.
# A lot of the variance we saw in our ANOVA model can be attributed to cups of coffee

# We need to perform pairwise to take into consideration the influence of caffeine on our group
emmeans(model_ancova, pairwise ~ visual_quality)
 
# We are now going to look at our ANOVA and ANCOVA as special cases of regression

# First, we need to use dummy coding for the levels of our experimental factor


# We want 'normal' visual quality to be our default reference level. When we build our linear model,
# the intercept of the line we're building is going to correspond to the mean of our 'normal' group
contrasts(q2_data$visual_quality)

# Our contrasts are already set so that 'normal' is our baseline - no need to change the contrasts!

# Response time = Intercept + degraded

model_lm <- lm(response_time ~ visual_quality, data = q2_data)

model_lm

# The intercept is the average for our 'normal' group and our degraded coefficient. We can use these coefficients 
# to calculate the averages for our two experimental groups. 

# We can work out the mean of reaction time in our degraded condition
print(1002.22 + 18.09)

#The means are the same as with our ANOVA!

# Now we're going to perform our ANCOVA as a special case of regression

# Adjusted means are below:
# Normal = 1005
# Degraded = 1018

model_lm_covariate <- lm(response_time ~ caffeine + visual_quality, data = q2_data)

model_lm_covariate

mean(q2_data$caffeine)

# The adjusted mean for our normal group:
print(998.564 + (2.469*2.552083))

# The adjusted mean for our degraded group:
print(998.564 + (2.469*2.552083) + 12.791)

# These are the same as the means when using the emmeans function

# Now we want to standardise to increase the interpretation of the coefficients in our linear model by centering our covariates

my_scaled_data <- q2_data %>% 
  mutate(centred_caffeine = scale(caffeine))

plot(density(my_scaled_data$caffeine))

plot(density(my_scaled_data$centred_caffeine))  

model_ancova_centred <- lm(response_time ~ centred_caffeine + visual_quality, data = my_scaled_data)

model_ancova_centred      

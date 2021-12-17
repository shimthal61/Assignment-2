raw_data_3 <- read_csv("assignment_2_dataset_3.csv")

head(raw_data_3)
str(raw_data_3)

longer_data <- raw_data_3 %>% 
  pivot_longer(cols = c(positiveprime_positivetarget, positiveprime_negativetarget, negativeprime_positivetarget, negativeprime_negativetarget),
               names_to = "Condition",
               values_to = "RT")

head(longer_data)

q3_data <- longer_data %>% 
  mutate(Condition = recode(Condition,
         "positiveprime_positivetarget" = "Positive_Positive",
         "positiveprime_negativetarget" = "Positive_Negative",
         "negativeprime_positivetarget" = "Negative_Positive",
         "negativeprime_negativetarget" = "Negative_Negative")) %>% 
  separate(col = "Condition", into = c("Prime", "Target"), sep = "_") %>% 
  mutate(Prime = factor(Prime), Target = factor(Target))

head(q3_data)
 
# Doesn't look there are any missing data!
vis_miss(q3_data)

# Generating descriptive stats
descriptive_stats <- q3_data %>% 
  group_by(Prime, Target) %>% 
  summarise(mean_RT = mean(RT), sd_RT = sd(RT)) %>% 
  arrange(mean_RT)
head(descriptive_stats)

# Making some data visualisations


q3_data %>% 
  ggplot(aes(x = Target, y = RT, colour = Prime)) +
  geom_violin(width = .5) +
  geom_point(alpha = 0.3, position = position_dodge(width = 0.5)) +
  #geom_point(alpha = 0.5, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  labs(y = "Reaction Time (ms)") +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", position = position_dodge(width = 0)) +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(breaks = seq(1400, 1750, by = 50),
                     limits = c(1400, 1750))

dodge <- position_dodge(width = 0)

set.seed(42)

q3_data %>% 
  ggplot(aes(x = Prime:Target, y = RT, colour = Target)) +
  geom_violin(width = .5) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  labs(x = "Prime", 
       y = "Reaction Time (ms)") +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme(text = element_text(size = 13)) +
  scale_y_continuous(breaks = seq(1400, 1750, by = 50),
                     limits = c(1400, 1750)) +
  scale_x_discrete(labels = c("Negative:Negative" = "Negative",
                              "Negative:Positive" = "Negative",
                              "Positive:Negative" = "Positive",
                              "Positive:Positive" = "Positive")) +
  theme(text = element_text(family = "lato", size = 20))

#This plot is wank (DO NOT SAY WANK IN MARKDOWN) so better to make an interaction plot

labels <- descriptive_stats %>%
  filter(Target == "Negative") %>% 
  mutate(label = case_when(Prime == "Negative" ~ "Negative Prime",
                           Prime == "Positive" ~ "Positive Prime"))

descriptive_stats %>%
  mutate(Target = fct_relevel(Target, "Positive", "Negative")) %>% 
  ggplot(aes(x = Target, y = mean_RT)) + 
  geom_line(size = 1.2, aes(group = Prime, colour = Prime)) +
  geom_point(size = 2.6, aes(colour = Prime), shape = 15) +
  geom_text(size = 4, aes(label = label,
                          colour = Prime),
            data = labels,
            nudge_x = 0.22,
            nudge_y = 1.2) +
  guides(colour = 'none') +
  scale_y_continuous(breaks = seq(1545, 1570, by = 5),
                     limits = c(1545, 1570)) +
  labs(x = "Target",
       y = "Reaction Time (ms)") +
  theme_minimal() 

# Have a look at the video (timestamp: 6:05, we were at 12:41) to see what Andrew has to say about this interaction.

# This graph is much better - looks like there is an interaction. It looks like there might not be any significant
# main effects, but there might be a sig. interaction effect??

# Now, we perform our repeated measures ANOVA

factorial_anova <- aov_4(RT ~ Target * Prime + (1 + Target * Prime | participant), data = q3_data)

anova(factorial_anova)

# We have a main interaction effect, but no main effects. Let's have a look which interactions were significant

emmeans(factorial_anova, pairwise ~ Target * Prime, adjust = "none")



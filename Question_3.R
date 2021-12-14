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

dodge <- position_dodge(width = 0)

set.seed(42)

q3_data %>% 
  ggplot(aes(x = Target, y = RT, colour = Prime)) +
  #geom_violin(position = dodge, width = .5) +
  #geom_boxplot(postiion = dodge, width = .5)
  geom_point(alpha = 0.9, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  labs(y = "Reaction Time (ms)") +
  theme(text = element_text(size = 13))

labels <- descriptive_stats %>%
  filter(Target == "Positive") %>% 
  mutate(label = case_when(Prime == "Negative" ~ "Negative Prime",
                           Prime == "Positive" ~ "Positive Prime"))

descriptive_stats %>% 
  ggplot(aes(Target, mean_RT)) + 
  geom_line(size = 1.2, aes(group = Prime, colour = Prime)) +
  geom_point(size = 2.6, aes(colour = Prime), shape = 15) +
  geom_text(size = 4, aes(label = label,
                          colour = Prime),
            data = labels,
            nudge_x = 0.22,
            nudge_y = 1) +
  guides(colour = 'none') +
  scale_y_continuous(breaks = seq(1545, 1570, by = 5),
                     limits = c(1545, 1570),
                     expand = c(0, 0)) +
  theme_minimal()
  


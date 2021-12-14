raw_data_3 <- read_csv("assignment_2_dataset_3.csv")

head(raw_data_3)
str(raw_data_3)

q3_data <- raw_data_3 %>% 
  pivot_longer(cols = c(positiveprime_positivetarget, positiveprime_negativetarget, negativeprime_positivetarget, negativeprime_negativetarget),
               names_to = "Condition",
               values_to = "RT")

head(q3_data)

q3_data <- q3_data %>% 
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
q3_data %>% 
  group_by(Prime, Target) %>% 
  summarise(mean_RT = mean(RT), sd_RT = sd(RT)) %>% 
  arrange(mean_RT)

# Making some data visualisations

dodge <- position_dodge(width = 0)

set.seed(42)

q3_data %>% 
  ggplot(aes(x = Target, y = RT, colour = Prime)) +
  geom_violin(position = dodge, width = .5) +
  #geom_boxplot(postiion = dodge, width = .5)
  geom_point(alpha = 0.9, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  labs(y = "Reaction Time (ms)") +
  theme(text = element_text(size = 13))

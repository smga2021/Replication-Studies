#FUNNY CODING
library(readr)
library(dplyr)
install.packages("lsr")
library(lsr)
install.packages("ggplot2")
library(ggplot2)

# 1 sample one-sided t test, very funny is equal to 5
t_test <- t.test(Funny_S24$Funny, mu=5)
t_test
pooled_sd <- sd(Funny_S24$Funny)
mean_diff <- mean(Funny_S24$Funny) - 5
cohens_d <- mean_diff / pooled_sd  
cohens_d

#independent samples t test new data frame
data_frame <- data.frame(Funny_S24$Condition2, Funny_S24$Funny)

#filter for condition 1
filtered_df_1 <- data_frame %>%
  filter(Funny_S24.Condition2 == "1")
mean(filtered_df_1$Funny_S24.Funny)

#filter for condition 2
filtered_df_2 <- data_frame %>%
  filter(Funny_S24.Condition2 == "2")
mean(filtered_df_2$Funny_S24.Funny)

#conducting the independent samples t test 
t_test2 <- t.test(filtered_df_2$Funny_S24.Funny, filtered_df_1$Funny_S24.Funny, var.equal = TRUE)
t_test2

pooled_sd2 <- sd(filtered_df_2$Funny_S24.Funny)
pooled_sd2

pooled_sd3 <- sd(filtered_df_1$Funny_S24.Funny)
pooled_sd3

cohensD(filtered_df_2$Funny_S24.Funny, filtered_df_1$Funny_S24.Funny)

#box plot
Funny_S24 <- Funny_S24 %>%
  mutate(Condition = case_when(
    Condition == "L" ~ "Lips",
    Condition == "T" ~ "Teeth"
  ))

ggplot(Funny_S24, aes(x = Condition, y = Funny, fill = Condition)) +
  geom_boxplot() +
  expand_limits(y = c(0, max(Funny_S24$Funny) + 1)) +  
  labs(x = "Condition",
       y = "Funny Ratings") +
  scale_fill_manual(values = c("blue", "red"))


#NEW RESEARCH QUESTION FOR STUDY 1

# 1 sample one-sided t test, not at all funny is equal to 1
t_test4 <- t.test(Funny_S24$Funny, mu=1)
t_test4

pooled_sd4 <- sd(Funny_S24$Funny)
mean_diff3 <- mean(Funny_S24$Funny) - 1
cohens_d3 <- mean_diff3 / pooled_sd4  
cohens_d3


#LW CODING

#Turning into numeric values

data_frame2 <- data.frame(LW_S24$Condition, LW_S24$LW)

data_frame2 <- case_match(LW_S24$Condition,
                               "S" ~ 1,
                               "O" ~ 2)

#filter for condition 1, get mean and sd
filtered_df2_1 <- data_frame2 %>%
  filter(LW_S24.Condition == "1")

mean1 <- mean(filtered_df2_1$LW_S24.LW)
mean1

sd1 <- sd(filtered_df2_1$LW_S24.LW)
sd1

#filter for condition 2, get mean and sd
filtered_df2_2 <- data_frame2 %>%
  filter(LW_S24.Condition == "2")

mean2 <- mean(filtered_df2_2$LW_S24.LW)
mean2

sd2 <- sd(filtered_df2_2$LW_S24.LW)
sd2

# 1 sample one-sided t test, nearly all the time is equal to 9 for others condition
t_test3 <- t.test(filtered_df2_2$LW_S24.LW, mu=9)
t_test3

mean_diff2 <- mean2 - 9
cohens_d2 <- mean_diff2 / sd2 
cohens_d2

#independent samples t test
t.test(filtered_df2_1$LW_S24.LW, filtered_df2_2$LW_S24.LW, var.equal = TRUE)

cohensD(filtered_df2_1$LW_S24.LW, filtered_df2_2$LW_S24.LW)

#box plot
LW_S24 <- LW_S24 %>%
  mutate(Condition = case_when(
    Condition == "O" ~ "Rated Others",
    Condition == "S" ~ "Rated Self"
  ))
  
ggplot(LW_S24, aes(x = Condition, y = LW, fill = Condition)) +
  geom_boxplot() + expand_limits(y = c(0, max(LW_S24$LW) + 1)) +
  labs(x = "Condition",
       y = "Lake Wobegon Effect") +
  scale_fill_manual(values = c("blue", "red")) 

#NEW RESEARCH QUESTION FOR STUDY 2

# 1 sample one-sided t test, almost never is equal to 1 for self condition
t_test5 <- t.test(filtered_df2_1$LW_S24.LW, mu=1)
t_test5

mean_diff4 <- mean1 - 1
cohens_d4 <- mean_diff4 / sd1 
cohens_d4

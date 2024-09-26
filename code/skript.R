#---------------------------------------------------------------
# VR-Tennis Split-Step multilevel regression analysis of hit rates

# Author: Damian Beck
# Date: July 2024
# Based on r version 4.3.2
#---------------------------------------------------------------

# Libraries ----
#---------------------------------------------------------------
#install packages recommended by Field (2012)
#install.packages("car", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("nlme", dependencies = TRUE)
#install.packages("reshape", dependencies = TRUE)
#install.packages("tidyverse", dependencies = TRUE)
#install.packages("sjPlot", dependencies = TRUE)
#install.packages("broom.mixed", dependencies = TRUE)
#install.packages("modi", dependencies = TRUE)
#install.packages("robustlmm", dependencies = TRUE)

library(car)
library(ggplot2)
library(nlme)
library(reshape)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(modi)
library(robustlmm)
library(readxl)
library(dplyr)
library(MASS)
library(lme4)

# Functions ----
#---------------------------------------------------------------
# Define the function detect and remove outliers with Cook's distance
remove_outliers <- function(data, model, cook_threshold) {
  # Calculate Cook's distance
  cd <- cooks.distance(model)
  # Identify influential points based on Cook's distance
  influential <- cd[(cd > (cook_threshold * mean(cd, na.rm = TRUE)))]
  # Get the names (row numbers) of the outliers
  names_of_outlier <- names(influential)
  # Extract the outliers from the data
  outliers <- data[names_of_outlier,]
  # Remove outliers from the data
  data_clean <- data %>%
    anti_join(outliers) 
  # Return the cleaned data and the model as a list
  return(data_clean)
}

remove_1d_mahalanobis_outliers <- function(data, threshold = 1) {
  # Ensure data is a numeric vector
  if (!is.numeric(data)) {
    stop("Data must be a numeric vector.")
  }
  
  # Calculate the mean and variance of the data
  mean_data <- mean(data)
  var_data <- var(data)
  
  # Compute Mahalanobis distance for each data point
  mahalanobis_distances <- (data - mean_data)^2 / var_data
  
  # Identify outliers
  outliers <- mahalanobis_distances > threshold^2
  
  # Remove outliers
  cleaned_data <- data[!outliers]
  
  # Return the cleaned data
  return(cleaned_data)
}



# Import data ----
#---------------------------------------------------------------
data_all <- read.csv("all_data.csv", header = TRUE, sep = ",")
View(data_all)
summary(data_all)

# Data preparation ----
#---------------------------------------------------------------
#change True->1 and False->0 for the column of hit_true_false
data_all$decision_correct <- ifelse(data_all$decision_correct == "1.0", 1,
                                    ifelse(data_all$decision_correct == "0.0", 0,
                                           ifelse(data_all$decision_correct == "True", TRUE,
                                                  ifelse(data_all$decision_correct == "False", FALSE, NA))))
data_all$decision_correct

#create a new column congruent_number
data_all$congruent_number <- ifelse(data_all$condition == "congruent", 1, 0)
data_all$incongruent_number <- ifelse(data_all$condition == "incongruent", 1, 0)

#filter data_all for all congruent trials
data_congruent <- filter(data_all, condition == "congruent")
data_incongruent <- filter(data_all, condition == "incongruent")
data_neutral <- filter(data_all, condition == "neutral", trial > 99, trial < 220 | trial > 320)
data_all <- filter(data_all, trial > 99, trial < 220 | trial > 320)

#loop trough data_congruent and change the congruent_number line number modulo 64
for (i in 1:nrow(data_congruent)){
  data_congruent$congruent_number[i] <- (i %% 64)
  #check if the modulo is 0 and change it to 64
  if (data_congruent$congruent_number[i] == 0){
    data_congruent$congruent_number[i] <- 64
  }
}

#loop trough data_incongruent and change the incongruent_number line number modulo 16
for (i in 1:nrow(data_incongruent)){
  data_incongruent$incongruent_number[i] <- (i%% 16)
  #check if the modulo is 0 and change it to 16
  if (data_incongruent$incongruent_number[i] == 0){
    data_incongruent$incongruent_number[i] <- 16
  }
}

#filter data_all in first and second day according to trial number
data_congruent_first_session <- filter(data_congruent, trial < 221)
data_congruent_second_session <- filter(data_congruent, trial > 220)
data_incongruent_first_session <- filter(data_incongruent, trial < 221)
data_incongruent_second_session <- filter(data_incongruent, trial > 220)
data_neutral_first_session <- filter(data_neutral, trial < 221)
data_neutral_second_session <- filter(data_neutral, trial > 220)

#combine all three first_Session datasets to one
data_first_session <- rbind(data_congruent_first_session, data_incongruent_first_session, 
                            data_neutral_first_session)

#combine all three second_Session datasets to one
data_second_session <- rbind(data_congruent_second_session, data_incongruent_second_session, 
                             data_neutral_second_session)

#combine all three data_congruent, data_incongruent and data neutral to one
data_all <- rbind(data_congruent, data_incongruent, data_neutral)

#multiply the congruent_number with 1.25
data_first_session$congruent_number <- data_first_session$congruent_number * 1.25 - 0.125
data_second_session$congruent_number <- data_second_session$congruent_number * 1.25 - 0.125
data_all$congruent_number <- data_all$congruent_number * 1.25 - 0.125

#replace negative values with 0
data_first_session$congruent_number[data_first_session$congruent_number < 0] <- 0
data_second_session$congruent_number[data_second_session$congruent_number < 0] <- 0
data_all$congruent_number[data_all$congruent_number < 0] <- 0

#multiply the incongruent_number with 5 and subtract 2.5
data_first_session$incongruent_number <- data_first_session$incongruent_number * 5 - 2.5
data_second_session$incongruent_number <- data_second_session$incongruent_number * 5 - 2.5
data_all$incongruent_number <- data_all$incongruent_number * 5 - 2.5

#replace negative values with 0
data_first_session$incongruent_number[data_first_session$incongruent_number < 0] <- 0
data_second_session$incongruent_number[data_second_session$incongruent_number < 0] <- 0
data_all$incongruent_number[data_all$incongruent_number < 0] <- 0

#make a new column trial_number by adding the congruent_number and incongruent_number
data_first_session$trial_number <- data_first_session$congruent_number + data_first_session$incongruent_number
data_second_session$trial_number <- data_second_session$congruent_number + data_second_session$incongruent_number
data_all$trial_number <- data_all$congruent_number + data_all$incongruent_number

#have a look at the data
View(data_first_session)
View(data_second_session)
View(data_all)



#Calculate descriptive statistics ----
#---------------------------------------------------------------

#filter data_all for hit == TRUE and left_or_right == "left" and "right"
data_all_left_hit <- filter(data_all, hit_true_false == 1 & side_played == "left")
data_all_right_hit <- filter(data_all, hit_true_false == 1 & side_played == "right")
data_all_left_response_correct <- filter(data_all, response_correct == 1 & side_played == "left")
data_all_right_response_correct <- filter(data_all, response_correct == 1 & side_played == "right")
data_neutral_response_correct <- filter(data_neutral, response_correct == 1)
data_neutral_hit <- filter(data_neutral, hit_true_false == 1)
data_all_split_performed_true <- filter(data_all, 
                                   splitstep_performed_com_5cm_below_max == "True" & 
                                     condition != "warm_up")
data_all_split_performed_false <- filter(data_all, 
                                        splitstep_performed_com_5cm_below_max == "False" & 
                                          condition != "warm_up")
data_all_feet_in_the_air_true <- filter(data_all, 
                                        feet_in_air == "True" & 
                                          condition != "warm_up")
data_all_feet_in_the_air_false <- filter(data_all, 
                                         feet_in_air == "False" & 
                                           condition != "warm_up")

#split step actions
data_all_split_Step_starts <- filter(data_all, 
                                     !is.na(splitstep_start) & 
                                       condition != "warm_up")
data_all_time_of_lateral_movement_initiation <- filter(data_all,
                                                       !is.na(lateral_movement_initiation) &
                                                         lateral_movement_initiation > -0.3 &
                                                         condition != "warm_up")
data_all_hit_time <- filter(data_all,
                              !is.na(hit_time) &
                                condition != "warm_up")


#hit rate
length(data_all$trial) #3285
length(data_all_left_response_correct$trial) #937 (57.0%)
length(data_all_right_response_correct$trial) #928 (56.5%)
length(data_all_left_hit$trial) #810 (49.3%)
length(data_all_right_hit$trial) #911 (55.5%)
length(data_neutral$trial) # 1045 
length(data_neutral_response_correct$trial) # 500 (47.8%)
length(data_neutral_hit$trial) # 352 (33.7%)
length(data_all_split_performed_true$trial) # 2376 (2376/(2376+173)=93.2%)
length(data_all_split_performed_false$trial) # 173 (173/(2376+173)=6.8%)
length(data_all_feet_in_the_air_true$trial)


data_all_split_Step_starts <- remove_1d_mahalanobis_outliers(data_all_split_Step_starts$splitstep_start)
boxplot(data_all_split_Step_starts)
summary(data_all_split_Step_starts)
mean(data_all_split_Step_starts)
sd(data_all_split_Step_starts)

data_all_time_of_lateral_movement_initiation <- remove_1d_mahalanobis_outliers(data_all_time_of_lateral_movement_initiation$lateral_movement_initiation)
boxplot(data_all_time_of_lateral_movement_initiation)
summary(data_all_time_of_lateral_movement_initiation)
mean(data_all_time_of_lateral_movement_initiation)
sd(data_all_time_of_lateral_movement_initiation)

data_all_hit_time <- remove_1d_mahalanobis_outliers(data_all_hit_time$hit_time)
boxplot(data_all_hit_time)
summary(data_all_hit_time)
mean(data_all_hit_time)
sd(data_all_hit_time)

#Calculate hit rates ----
#---------------------------------------------------------------
#calculate hit rates for first session
#count all TRUE values for each trial number and condition
hit_rates_first_session <- data_first_session %>%
  group_by(trial_number, condition) %>%
  summarise(hit_rate = sum(hit_true_false == 1)/(sum(hit_true_false == 1)+sum(hit_true_false == 0)))

hit_rates_second_session <- data_second_session %>%
  group_by(trial_number, condition) %>%
  summarise(hit_rate = sum(hit_true_false == 1)/(sum(hit_true_false == 1)+sum(hit_true_false == 0)))

hit_rates_all <- data_all %>%
  group_by(trial_number, condition) %>%
  summarise(hit_rate = sum(hit_true_false == 1)/(sum(hit_true_false == 1)+sum(hit_true_false == 0)))

#multiply the hit rates with 100 in order to have % values
hit_rates_first_session$hit_rate <- hit_rates_first_session$hit_rate * 100
hit_rates_second_session$hit_rate <- hit_rates_second_session$hit_rate * 100
hit_rates_all$hit_rate <- hit_rates_all$hit_rate * 100

#duplicate the row with condition neutral and trial number 0
hit_rates_first_session <- rbind(hit_rates_first_session, hit_rates_first_session[hit_rates_first_session$condition == "neutral" & hit_rates_first_session$trial_number == 0,])
hit_rates_second_session <- rbind(hit_rates_second_session, hit_rates_second_session[hit_rates_second_session$condition == "neutral" & hit_rates_second_session$trial_number == 0,])
hit_rates_all <- rbind(hit_rates_all, hit_rates_all[hit_rates_all$condition == "neutral" & hit_rates_all$trial_number == 0,])

#change the first entry of condition with "neutral" to "congruent" and the second to "incongruent"
hit_rates_first_session$condition[hit_rates_first_session$condition == "neutral"][1] <- "congruent"
hit_rates_first_session$condition[hit_rates_first_session$condition == "neutral"] <- "incongruent"
hit_rates_second_session$condition[hit_rates_second_session$condition == "neutral"][1] <- "congruent"
hit_rates_second_session$condition[hit_rates_second_session$condition == "neutral"] <- "incongruent"
hit_rates_all$condition[hit_rates_all$condition == "neutral"][1] <- "congruent"
hit_rates_all$condition[hit_rates_all$condition == "neutral"] <- "incongruent"


#make a new coloumn condition_dummy_code with 0 for congruent and 1 for incongruent
hit_rates_first_session$condition_dummy_code <- ifelse(hit_rates_first_session$condition == "congruent", 0, 1)
hit_rates_second_session$condition_dummy_code <- ifelse(hit_rates_second_session$condition == "congruent", 0, 1)
hit_rates_all$condition_dummy_code <- ifelse(hit_rates_all$condition == "congruent", 0, 1)

#have a look at the hit rates
#View(hit_rates_first_session)
#View(hit_rates_second_session)
#View(hit_rates_all)


#Regression analysis of hit rate---
#---------------------------------------------------------------
#delete rows with trial number 0 in hit_rates_all (because of sqrtarithmic transformation)
hit_rates_all <- hit_rates_all[hit_rates_all$trial_number != 0,]

#sqrtarithmic transformation of trial number
both_sessions_sqrt <- lm(hit_rate ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                        data = hit_rates_all,
                        method = "qr",
                        na.action = na.exclude)
tab_model(both_sessions_sqrt)
summary(both_sessions_sqrt)


both_sessions <- lm(hit_rate ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                        data = hit_rates_all,
                        method = "qr",
                        na.action = na.exclude)
tab_model(both_sessions)
summary(both_sessions)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(both_sessions_sqrt))
qqnorm(residuals(both_sessions_sqrt))
qqline(residuals(both_sessions_sqrt))
hist(residuals(both_sessions_sqrt))
shapiro.test(residuals(both_sessions_sqrt)) # not significant


plot <- ggplot(hit_rates_all, aes(x = trial_number, y = hit_rate, color = condition)) +
  geom_point() +
  geom_smooth(method = "lm",formula = y~sqrt(x), se = TRUE, level = 0.95, fullrange = TRUE)+
  labs(title = "",
       x = "trial number (#)",
       y = "hit rate (%)")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )
plot
ggsave("both_session_sqrt_hit_rate.svg", plot, width = 9, height = 6)
ggsave("both_session_sqrt_hit_rate.png", plot, width = 9, height = 6)



#first session
#delete rows with trial number 0 in hit_rates_all (because of sqrtarithmic transformation)
hit_rates_first_session <- hit_rates_first_session[hit_rates_first_session$trial_number != 0,]

#sqrtarithmic transformation of trial number
first_session_sqrt <- lm(hit_rate ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                        data = hit_rates_first_session,
                        method = "qr",
                        na.action = na.exclude)
tab_model(first_session_sqrt)
summary(first_session_sqrt)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(first_session_sqrt))
qqnorm(residuals(first_session_sqrt))
qqline(residuals(first_session_sqrt))
hist(residuals(first_session_sqrt))
shapiro.test(residuals(first_session_sqrt)) # 

plot <- ggplot(hit_rates_first_session, aes(x = trial_number, y = hit_rate, color = condition)) +
  geom_point() +
  geom_smooth(method = "lm",formula = y~sqrt(x), se = TRUE, level = 0.95, fullrange = TRUE)+
  labs(title = "",
       x = "trial number (#)",
       y = "hit rate (%)")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )
plot
ggsave("first_session_sqrt_hit_rate.svg", plot, width = 9, height = 6)
ggsave("first_session_sqrt_hit_rate.png", plot, width = 9, height = 6)


#second session
#delete rows with trial number 0 in hit_rates_all (because of sqrtarithmic transformation)
hit_rates_second_session <- hit_rates_second_session[hit_rates_second_session$trial_number != 0,]

#sqrtarithmic transformation of trial number
second_session_sqrt <- lm(hit_rate ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                        data = hit_rates_second_session,
                        method = "qr",
                        na.action = na.exclude)
tab_model(second_session_sqrt)
summary(second_session_sqrt)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(second_session_sqrt))
qqnorm(residuals(second_session_sqrt))
qqline(residuals(second_session_sqrt))
hist(residuals(second_session_sqrt))
shapiro.test(residuals(second_session_sqrt)) # 

plot <- ggplot(hit_rates_second_session, aes(x = trial_number, y = hit_rate, color = condition)) +
  geom_point() +
  geom_smooth(method = "lm",formula = y~sqrt(x), se = TRUE, level = 0.95, fullrange = TRUE)+
  labs(title = "",
       x = "trial number (#)",
       y = "hit rate (%)")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )
plot
ggsave("second_session_sqrt_hit_rate.svg", plot, width = 9, height = 6)
ggsave("second_session_sqrt_hit_rate.png", plot, width = 9, height = 6)

#Calculate correct response rates ----
#---------------------------------------------------------------
#calculate correct response rate for first session
#count all TRUE values for each trial number and condition
correct_response_rates_first_session <- data_first_session %>%
  group_by(trial_number, condition) %>%
  summarise(correct_response_rate = sum(response_correct == 1)/(sum(response_correct == 1)+sum(response_correct == 0)))

correct_response_rates_second_session <- data_second_session %>%
  group_by(trial_number, condition) %>%
  summarise(correct_response_rate = sum(response_correct == 1)/(sum(response_correct == 1)+sum(response_correct == 0)))

correct_response_rates_all <- data_all %>%
  group_by(trial_number, condition) %>%
  summarise(correct_response_rate = sum(response_correct == 1)/(sum(response_correct == 1)+sum(response_correct == 0)))

#multiply the hit rates with 100 in order to have % values
correct_response_rates_first_session$correct_response_rate <- correct_response_rates_first_session$correct_response_rate * 100
correct_response_rates_second_session$correct_response_rate <- correct_response_rates_second_session$correct_response_rate * 100
correct_response_rates_all$correct_response_rate <- correct_response_rates_all$correct_response_rate * 100

#duplicate the row with condition neutral and trial number 0
correct_response_rates_first_session <- rbind(correct_response_rates_first_session, correct_response_rates_first_session[correct_response_rates_first_session$condition == "neutral" & correct_response_rates_first_session$trial_number == 0,])
correct_response_rates_second_session <- rbind(correct_response_rates_second_session, correct_response_rates_second_session[correct_response_rates_second_session$condition == "neutral" & correct_response_rates_second_session$trial_number == 0,])
correct_response_rates_all <- rbind(correct_response_rates_all, correct_response_rates_all[correct_response_rates_all$condition == "neutral" & correct_response_rates_all$trial_number == 0,])

#change the first entry of condition with "neutral" to "congruent" and the second to "incongruent"
correct_response_rates_first_session$condition[correct_response_rates_first_session$condition == "neutral"][1] <- "congruent"
correct_response_rates_first_session$condition[correct_response_rates_first_session$condition == "neutral"] <- "incongruent"
correct_response_rates_second_session$condition[correct_response_rates_second_session$condition == "neutral"][1] <- "congruent"
correct_response_rates_second_session$condition[correct_response_rates_second_session$condition == "neutral"] <- "incongruent"
correct_response_rates_all$condition[correct_response_rates_all$condition == "neutral"][1] <- "congruent"
correct_response_rates_all$condition[correct_response_rates_all$condition == "neutral"] <- "incongruent"


#make a new coloumn condition_dummy_code with 0 for congruent and 1 for incongruent
correct_response_rates_first_session$condition_dummy_code <- ifelse(correct_response_rates_first_session$condition == "congruent", 0, 1)
correct_response_rates_second_session$condition_dummy_code <- ifelse(correct_response_rates_second_session$condition == "congruent", 0, 1)
correct_response_rates_all$condition_dummy_code <- ifelse(correct_response_rates_all$condition == "congruent", 0, 1)

#have a look at the hit rates
#View(correct_response_rates_first_session)
#View(correct_response_rates_second_session)
#View(correct_response_rates_all)


#Regression analysis of correct response rate---
#---------------------------------------------------------------
#delete rows with trial number 0 in correct_response_rates_all (because of sqrtarithmic transformation)
correct_response_rates_all <- correct_response_rates_all[correct_response_rates_all$trial_number != 0,]

#sqrtarithmic transformation of trial number
both_sessions_sqrt <- lm(correct_response_rate ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                        data = correct_response_rates_all,
                        method = "qr",
                        na.action = na.exclude)
tab_model(both_sessions_sqrt)
summary(both_sessions_sqrt)


both_sessions <- lm(correct_response_rate ~ trial_number + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                        data = correct_response_rates_all,
                        method = "qr",
                        na.action = na.exclude)
tab_model(both_sessions)
summary(both_sessions)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(both_sessions_sqrt))
qqnorm(residuals(both_sessions_sqrt))
qqline(residuals(both_sessions_sqrt))
hist(residuals(both_sessions_sqrt))
shapiro.test(residuals(both_sessions_sqrt)) # 

plot <- ggplot(correct_response_rates_all, aes(x = trial_number, y = correct_response_rate, color = condition)) +
  geom_point() +
  geom_smooth(method = "lm",formula = y~sqrt(x), se = TRUE, level = 0.95, fullrange = TRUE)+
  labs(title = "",
       x = "trial number (#)",
       y = "correct response rate (%)")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )
plot
ggsave("both_session_sqrt_correct_response_rates.svg", plot, width = 9, height = 6)
ggsave("both_session_sqrt_correct_response_rates.png", plot, width = 9, height = 6)



#delete rows with trial number 0 in correct_response_rates_all (because of sqrtarithmic transformation)
correct_response_rates_first_session <- correct_response_rates_first_session[correct_response_rates_first_session$trial_number != 0,]

#sqrtarithmic transformation of trial number
first_session_sqrt <- lm(correct_response_rate ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                        data = correct_response_rates_first_session,
                        method = "qr",
                        na.action = na.exclude)
tab_model(first_session_sqrt)
summary(first_session_sqrt)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(first_session_sqrt))
qqnorm(residuals(first_session_sqrt))
qqline(residuals(first_session_sqrt))
hist(residuals(first_session_sqrt))
shapiro.test(residuals(first_session_sqrt)) # 

plot <- ggplot(correct_response_rates_first_session, aes(x = trial_number, y = correct_response_rate, color = condition)) +
  geom_point() +
  geom_smooth(method = "lm",formula = y~sqrt(x), se = TRUE, level = 0.95, fullrange = TRUE)+
  labs(title = "",
       x = "trial number (#)",
       y = "correct response rate (%)")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )
plot
ggsave("first_session_sqrt_correct_response_rates.svg", plot, width = 9, height = 6)
ggsave("first_session_sqrt_correct_response_rates.png", plot, width = 9, height = 6)

#delete rows with trial number 0 in correct_response_rates_all (because of sqrtarithmic transformation)
correct_response_rates_second_session <- correct_response_rates_second_session[correct_response_rates_second_session$trial_number != 0,]

#sqrtarithmic transformation of trial number
second_session_sqrt <- lm(correct_response_rate ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                        data = correct_response_rates_second_session,
                        method = "qr",
                        na.action = na.exclude)
tab_model(second_session_sqrt)
summary(second_session_sqrt)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(second_session_sqrt))
qqnorm(residuals(second_session_sqrt))
qqline(residuals(second_session_sqrt))
hist(residuals(second_session_sqrt))
shapiro.test(residuals(second_session_sqrt)) # 

plot <- ggplot(correct_response_rates_second_session, aes(x = trial_number, y = correct_response_rate, color = condition)) +
  geom_point() +
  geom_smooth(method = "lm",formula = y~sqrt(x), se = TRUE, level = 0.95, fullrange = TRUE)+
  labs(title = "",
       x = "trial number (#)",
       y = "correct response rate (%)")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )
plot
ggsave("second_session_sqrt_correct_response_rates.svg", plot, width = 9, height = 6)
ggsave("second_session_sqrt_correct_response_rates.png", plot, width = 9, height = 6)








#Calculate development of side tendency ----
#developpment of side tendency over time
#filter data all such that only congruent with correct response = 1
data_tendency <- data_all %>%
  filter(trial_number > 40) #40 because the only the two last blocks are considered
for (i in 36:276) {data_tendency[,i] <- ifelse(data_tendency$side_played == "left", -data_tendency[,i], data_tendency[,i])}
summary(data_tendency)

data_directed_prior <- data_all
for (i in 36:276) {data_directed_prior[,i] <- ifelse(data_directed_prior$side_played == "left", -data_directed_prior[,i], data_directed_prior[,i])}
for (i in 36:276) {data_directed_prior[,i] <- ifelse(data_directed_prior$condition == "incongruent", -data_directed_prior[,i], data_directed_prior[,i])}
data_directed_prior$condition_dummy <- ifelse(data_directed_prior$condition == "congruent", 1, 0) #convert condition to dummy code
summary(data_directed_prior)

data_directed_prior_second_half <- data_directed_prior %>%
  filter(trial_number > 40) #40 because the only the two last blocks are considered

data_long <- data_tendency 
#rename the columns data_long 36:276 to -100, -95, ..., 995, 1000
colnames(data_long)[36:276] <- seq(-200, 1000, by = 5)
data_long <- data_long %>%
  pivot_longer(cols = 36:276,, 
               names_to = "time", 
               values_to = "weight_shift")

data_long$time <- as.integer(data_long$time)/100 #convert time to integer
View(data_long)

#logistic regression of hit_true_false and side_tendency
#---------------------------------------------------------------
#calculate logistic regression for hit_true_false and side_tendency

#all in one model
logistic_model <- glmer(hit_true_false ~ weight_shift+condition_dummy + weight_shift*condition_dummy + time + weight_shift*time + condition_dummy*time, 
                                      data = data_long, family = binomial(link = "logit"))
tab_model(logistic_model) 
summary(logistic_model)


logistic_model_rI <- glmer(hit_true_false ~ weight_shift+time + condition + weight_shift*time + (1 | vp), 
                      data = data_long, family = binomial(link = "logit"))
tab_model(logistic_model_rI) 
summary(logistic_model_rI)

logistic_model <- glmer(hit_true_false ~ weight_shift+time + condition + weight_shift*time+ (hit_true_false | vp), 
                      data = data_long, family = binomial)
tab_model(logistic_model) 
summary(logistic_model)



#evaluated at -100, 100, 300, 500
#-100
logistic_model_n_minus_100_hit <- glm(hit_true_false ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy, 
                                         data = data_directed_prior, family = binomial(link = "logit"))
tab_model(logistic_model_n_minus_100_hit)
summary(logistic_model_n_minus_100_hit)

logistic_model_rI_minus_100_hit <- glmer(hit_true_false ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy+ (1 | vp), 
                                          data = data_directed_prior, family = binomial(link = "logit"))
tab_model(logistic_model_rI_minus_100_hit)
summary(logistic_model_rI_minus_100_hit)

logistic_model_rSI_minus_100_hit <- glmer(hit_true_false ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy+ (side_tendency_minus_100 | vp), 
                                         data = data_directed_prior, family = binomial)
tab_model(logistic_model_rSI_minus_100_hit)
summary(logistic_model_rSI_minus_100_hit)

anova(logistic_model_rI_minus_100_hit, logistic_model_rSI_minus_100_hit)

#100
logistic_model_n_100_hit <- glm(hit_true_false ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy, 
                                      data = data_directed_prior, family = binomial)
tab_model(logistic_model_n_100_hit)
summary(logistic_model_n_100_hit)

logistic_model_rI_100_hit <- glmer(hit_true_false ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy+ (1 | vp), 
                                         data = data_directed_prior, family = binomial)
tab_model(logistic_model_rI_100_hit)
summary(logistic_model_rI_100_hit)

logistic_model_rSI_100_hit <- glmer(hit_true_false ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy+ (side_tendency_100 | vp), 
                                          data = data_directed_prior, family = binomial)
tab_model(logistic_model_rSI_100_hit)
summary(logistic_model_rSI_100_hit)

#300
logistic_model_n_300_hit <- glm(hit_true_false ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy, 
                                      data = data_directed_prior, family = binomial)
tab_model(logistic_model_n_300_hit)
summary(logistic_model_n_300_hit)

logistic_model_rI_300_hit <- glmer(hit_true_false ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy+ (1 | vp), 
                                         data = data_directed_prior, family = binomial)
tab_model(logistic_model_rI_300_hit)
summary(logistic_model_rI_300_hit)

logistic_model_rSI_300_hit <- glmer(hit_true_false ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy+ (side_tendency_300 | vp), 
                                          data = data_directed_prior, family = binomial)
tab_model(logistic_model_rSI_300_hit)
summary(logistic_model_rSI_300_hit)

logistic_model_n_500_hit <- glm(hit_true_false ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy, 
                                      data = data_directed_prior, family = binomial)
tab_model(logistic_model_n_500_hit)
summary(logistic_model_n_500_hit)

logistic_model_rI_500_hit <- glmer(hit_true_false ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy+ (1 | vp), 
                                         data = data_directed_prior, family = binomial)
tab_model(logistic_model_rI_500_hit)
summary(logistic_model_rI_500_hit)

logistic_model_rSI_500_hit <- glmer(hit_true_false ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy+ (side_tendency_500 | vp), 
                                          data = data_directed_prior, family = binomial)
tab_model(logistic_model_rSI_500_hit)
summary(logistic_model_rSI_500_hit)



#side tendency significant from 0?
#---------------------------------------------------------------
#minus_100
#Outlier detection with cooks distance 
#if more than 3 times more influential than an average point
mean_sessions_minus_100 <- lm( side_tendency_100 ~ 1, 
                                    data = data_directed_prior_second_half, 
                               na.action = na.omit)
summary(mean_sessions_minus_100)
tab_model(mean_sessions_minus_100)
plot(mean_sessions_minus_100,4)
cd <- cooks.distance(mean_sessions_minus_100)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- side_tendency_all_minus_100[names_of_outlier,]
side_tendency_all_minus_100 <- side_tendency_all_minus_100 %>% anti_join(outliers)
summary(side_tendency_all_minus_100)

#according to Field (2013) hierarchical model comparison
#sqrtarithmic transformation of trial number
#intercept only
intercept_only_minus_100 <- nlme::gls(side_tendency_minus_100 ~ 1, 
                                           data = data_directed_prior_second_half,
                                           correlation = corAR1(form = ~ 1 | vp),
                                           method = "ML",
                                           na.action = na.exclude)
tab_model(intercept_only_minus_100)

intercept_only_minus_100_rI <- nlme::lme(side_tendency_minus_100 ~ 1, 
                                      data = data_directed_prior_second_half,
                                      random = ~1|vp,
                                      correlation = corAR1(form = ~ 1 | vp),
                                      method = "ML",
                                      na.action = na.exclude)
tab_model(intercept_only_minus_100_rI)

intercept_only_minus_100_rI <- nlme::lme(side_tendency_minus_100 ~ 1, 
                                         data = data_directed_prior_second_half,
                                         random = 1|vp),
                                         correlation = corAR1(form = ~ 1 | vp),
                                         method = "ML",
                                         na.action = na.exclude)
tab_model(intercept_only_minus_100_rI)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(intercept_only_minus_100))
qqnorm(residuals(intercept_only_minus_100))
qqline(residuals(intercept_only_minus_100))
hist(residuals(intercept_only_minus_100))
shapiro.test(intercept_only_minus_100)) 

anova(intercept_only_sqrt_minus_100, sqrt_minus_100, random_intercept_sqrt_minus_100, random_slope_sqrt_minus_100)

#with the condition predictor only 
sqrt_minus_100_all_condition_only <- nlme::lme(side_tendency_minus_100 ~ condition_dummy_code, 
                                               data = side_tendency_all_minus_100,
                                               correlation = corAR1(form = ~ 1 | vp),
                                               random = ~sqrt(trial_number)|vp,
                                               method = "ML",
                                               na.action = na.exclude)
tab_model(sqrt_minus_100_all_condition_only)
summary(sqrt_minus_100_all_condition_only)












#mean development of side tendency over time
#---------------------------------------------------------------
data_tendency <- data_all %>%
  filter(trial_number > 40) #40 because the only the two last blocks are considered
for (i in 36:276) {data_tendency[,i] <- ifelse(data_tendency$side_played == "left", -data_tendency[,i], data_tendency[,i])}
summary(data_tendency)

data_tendency_congruent_correct <- data_tendency %>%
  filter(condition == "congruent", response_correct == 1)
data_tendency_congruent_incorrect <- data_tendency %>%
  filter(condition == "congruent", response_correct == 0)
data_tendency_incongruent_correct <- data_tendency %>%
  filter(condition == "incongruent", response_correct == 1)
data_tendency_incongruent_incorrect <- data_tendency %>%
  filter(condition == "incongruent", response_correct == 0)


time_values <- seq(from = -200, to = 1000, by = 5)

side_tendency_congruent_correct <- numeric()

for (i in seq(-200, 1000, by = 5)) {
  # Format the number with three digits, preserving the minus sign if present
  formatted_number <- sprintf("%03d", abs(i))
  column_name <- paste0("side_tendency_", ifelse(i < 0, "minus_", ""), formatted_number)
  
  if (column_name %in% colnames(data_tendency_congruent_correct)) {  # Check if the column exists
    column_data <- data_tendency_congruent_correct[[column_name]]
    if (!all(is.na(column_data))) {  # Check if the column does not contain only NA values
      side_tendency_congruent_correct <- c(side_tendency_congruent_correct, 
                                           mean(column_data, na.rm = TRUE))
    } else {
      side_tendency_congruent_correct <- c(side_tendency_congruent_correct, NA)
    }
  } else {
    side_tendency_congruent_correct <- c(side_tendency_congruent_correct, NA)
  }
}

side_tendency_incongruent_correct <- numeric()

for (i in seq(-200, 1000, by = 5)) {
  # Format the number with three digits, preserving the minus sign if present
  formatted_number <- sprintf("%03d", abs(i))
  column_name <- paste0("side_tendency_", ifelse(i < 0, "minus_", ""), formatted_number)
  
  if (column_name %in% colnames(data_tendency_incongruent_correct)) {  # Check if the column exists
    column_data <- data_tendency_incongruent_correct[[column_name]]
    if (!all(is.na(column_data))) {  # Check if the column does not contain only NA values
      side_tendency_incongruent_correct <- c(side_tendency_incongruent_correct, 
                                           mean(column_data, na.rm = TRUE))
    } else {
      side_tendency_incongruent_correct <- c(side_tendency_incongruent_correct, NA)
    }
  } else {
    side_tendency_incongruent_correct <- c(side_tendency_incongruent_correct, NA)
  }
}


side_tendency_congruent_incorrect <- numeric()

for (i in seq(-200, 1000, by = 5)) {
  # Format the number with three digits, preserving the minus sign if present
  formatted_number <- sprintf("%03d", abs(i))
  column_name <- paste0("side_tendency_", ifelse(i < 0, "minus_", ""), formatted_number)
  
  if (column_name %in% colnames(data_tendency_congruent_incorrect)) {  # Check if the column exists
    column_data <- data_tendency_congruent_incorrect[[column_name]]
    if (!all(is.na(column_data))) {  # Check if the column does not contain only NA values
      side_tendency_congruent_incorrect <- c(side_tendency_congruent_incorrect, 
                                           mean(column_data, na.rm = TRUE))
    } else {
      side_tendency_congruent_incorrect <- c(side_tendency_congruent_incorrect, NA)
    }
  } else {
    side_tendency_congruent_incorrect <- c(side_tendency_congruent_incorrect, NA)
  }
}


side_tendency_incongruent_incorrect <- numeric()

for (i in seq(-200, 1000, by = 5)) {
  # Format the number with three digits, preserving the minus sign if present
  formatted_number <- sprintf("%03d", abs(i))
  column_name <- paste0("side_tendency_", ifelse(i < 0, "minus_", ""), formatted_number)
  
  if (column_name %in% colnames(data_tendency_incongruent_incorrect)) {  # Check if the column exists
    column_data <- data_tendency_incongruent_incorrect[[column_name]]
    if (!all(is.na(column_data))) {  # Check if the column does not contain only NA values
      side_tendency_incongruent_incorrect <- c(side_tendency_incongruent_incorrect, 
                                             mean(column_data, na.rm = TRUE))
    } else {
      side_tendency_incongruent_incorrect <- c(side_tendency_incongruent_incorrect, NA)
    }
  } else {
    side_tendency_incongruent_incorrect <- c(side_tendency_incongruent_incorrect, NA)
  }
}    


data_tendency_neutral <- data_neutral 
for (i in 36:276) {data_tendency_neutral[,i] <- ifelse(data_tendency_neutral$side_played == "left", -data_tendency_neutral[,i], data_tendency_neutral[,i])}

data_tendency_neutral_correct <- data_tendency_neutral %>%
  filter(condition == "neutral", response_correct == 1)
data_tendency_neutral_incorrect <- data_tendency_neutral %>%
  filter(condition == "neutral", response_correct == 0)

time_values <- seq(from = -200, to = 1000, by = 5)

side_tendency_neutral_correct <- numeric()

for (i in seq(-200, 1000, by = 5)) {
  # Format the number with three digits, preserving the minus sign if present
  formatted_number <- sprintf("%03d", abs(i))
  column_name <- paste0("side_tendency_", ifelse(i < 0, "minus_", ""), formatted_number)
  
  if (column_name %in% colnames(data_tendency_neutral_correct)) {  # Check if the column exists
    column_data <- data_tendency_neutral_correct[[column_name]]
    if (!all(is.na(column_data))) {  # Check if the column does not contain only NA values
      side_tendency_neutral_correct <- c(side_tendency_neutral_correct, 
                                           mean(column_data, na.rm = TRUE))
    } else {
      side_tendency_neutral_correct <- c(side_tendency_neutral_correct, NA)
    }
  } else {
    side_tendency_neutral_correct <- c(side_tendency_neutral_correct, NA)
  }
}

side_tendency_neutral_incorrect <- numeric()

for (i in seq(-200, 1000, by = 5)) {
  # Format the number with three digits, preserving the minus sign if present
  formatted_number <- sprintf("%03d", abs(i))
  column_name <- paste0("side_tendency_", ifelse(i < 0, "minus_", ""), formatted_number)
  
  if (column_name %in% colnames(data_tendency_neutral_incorrect)) {  # Check if the column exists
    column_data <- data_tendency_neutral_incorrect[[column_name]]
    if (!all(is.na(column_data))) {  # Check if the column does not contain only NA values
      side_tendency_neutral_incorrect <- c(side_tendency_neutral_incorrect, 
                                         mean(column_data, na.rm = TRUE))
    } else {
      side_tendency_neutral_incorrect <- c(side_tendency_neutral_incorrect, NA)
    }
  } else {
    side_tendency_neutral_incorrect <- c(side_tendency_neutral_incorrect, NA)
  }
}


congruent_correct <- data.frame(
  time = time_values,
  side_tendency = side_tendency_congruent_correct)
#change sign of the side_tendency_ at such that a positive value indicates the 
#direction of the prior
incongruent_correct <- data.frame(
  time = time_values,
  side_tendency = -side_tendency_incongruent_correct)
congruent_incorrect <- data.frame(
  time = time_values,
  side_tendency = side_tendency_congruent_incorrect)
#change sign of the side_tendency_ at such that a positive value indicates the 
#direction of the prior
incongruent_incorrect <- data.frame(
  time = time_values,
  side_tendency = -side_tendency_incongruent_incorrect)
neutral_correct <- data.frame(
  time = time_values,
  side_tendency = side_tendency_neutral_correct)
neutral_incorrect <- data.frame(
  time = time_values,
  side_tendency = side_tendency_neutral_incorrect)


ggplot() +
  geom_line(data = congruent_correct, 
            aes(x = time, y = side_tendency, color = "Congruent correct response", linetype = "Congruent correct response")) +
  geom_line(data = incongruent_correct, 
            aes(x = time, y = side_tendency, color = "Incongruent correct response", linetype = "Incongruent correct response")) +
  geom_line(data = congruent_incorrect, 
            aes(x = time, y = side_tendency, color = "Congruent incorrect response", linetype = "Congruent incorrect response")) +
  geom_line(data = incongruent_incorrect, 
            aes(x = time, y = side_tendency, color = "Incongruent incorrect response", linetype = "Incongruent incorrect response")) +
  geom_line(data = neutral_correct, 
            aes(x = time, y = side_tendency, color = "Neutral correct response", linetype = "Neutral correct response")) +
  geom_line(data = neutral_incorrect, 
            aes(x = time, y = side_tendency, color = "Neutral incorrect response", linetype = "Neutral incorrect response")) +
  
  # Add vertical black lines at x = 0, 466, 570, and 990
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  geom_vline(xintercept = 466, color = "black", linetype = "solid") +
  geom_vline(xintercept = 570, color = "black", linetype = "solid") +
  geom_vline(xintercept = 990, color = "black", linetype = "solid") +  # "Return"
  
  # Add annotations for the vertical lines with a much lower y position for "Lateral movement initiation"
  annotate("text", x = 0, y = max(neutral_correct$side_tendency) + 0.04, 
           label = "Serve", angle = 90, vjust = -0.5, hjust = 0) +
  annotate("text", x = 466, y = max(neutral_correct$side_tendency) - 0.0, 
           label = "Bounce", angle = 90, vjust = -0.5, hjust = 0) +
  annotate("text", x = 570, y = min(neutral_correct$side_tendency) + 0.57,  # Lowered y position even more
           label = "Lateral movement initiation", angle = 90, vjust = -0.5, hjust = 0) +
  annotate("text", x = 990, y = max(neutral_correct$side_tendency) - 0.0 , 
           label = "Return", angle = 90, vjust = -0.5, hjust = 0) +
  
  # Define color mapping
  scale_color_manual(values = c(
    "Congruent correct response" = "blue", 
    "Incongruent correct response" = "red", 
    "Congruent incorrect response" = "blue", 
    "Incongruent incorrect response" = "red",
    "Neutral correct response" = "green",
    "Neutral incorrect response" = "green"
  )) +
  
  # Define linetype mapping
  scale_linetype_manual(values = c(
    "Congruent correct response" = "solid", 
    "Incongruent correct response" = "solid", 
    "Congruent incorrect response" = "dashed", 
    "Incongruent incorrect response" = "dashed",
    "Neutral correct response" = "solid",
    "Neutral incorrect response" = "dashed"
  )) +
  
  labs(
    title = "", 
    x = "Time to Serve (ms)", 
    y = "Weight Shift in Relation to the Prior (m)", 
    color = "Condition", 
    linetype = "Condition"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "right",   # Move legend to the right
    panel.grid = element_blank(),  # Remove grid lines
    axis.line = element_line(),    # Add axis lines
    axis.ticks = element_line()
  )


save_plot("prior_impact_over_second_half_of_biased_and_neutral_trials.png", width = 20, height = 12)

















#Calculate side tendency at time n300, n200, n100, 0 , 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ----
#---------------------------------------------------------------
#change sign of the side_tendency_ at ...
data_all$side_tendency_minus_300 <- ifelse(data_all$side_played == "left",
                                           -data_all$side_tendency_minus_300,
                                           data_all$side_tendency_minus_300
)

data_all$side_tendency_minus_300 <- ifelse(data_all$side_played == "left",
                                           -data_all$side_tendency_minus_300,
                                           data_all$side_tendency_minus_300
)

data_all$side_tendency_minus_100 <- ifelse(data_all$side_played == "left",
                                   -data_all$side_tendency_minus_100,
                                   data_all$side_tendency_minus_100
)

data_all$side_tendency_0 <- ifelse(data_all$side_played == "left",
  -data_all$side_tendency_0,
    data_all$side_tendency_0
)

data_all$side_tendency_100 <- ifelse(data_all$side_played == "left",
                                   -data_all$side_tendency_100,
                                   data_all$side_tendency_100
)

data_all$side_tendency_200 <- ifelse(data_all$side_played == "left",
                                   -data_all$side_tendency_200,
                                   data_all$side_tendency_200
)

data_all$side_tendency_300 <- ifelse(data_all$side_played == "left",
                                   -data_all$side_tendency_300,
                                   data_all$side_tendency_300
)

data_all$side_tendency_400 <- ifelse(data_all$side_played == "left",
                                     -data_all$side_tendency_400,
                                     data_all$side_tendency_400
)

data_all$side_tendency_500 <- ifelse(data_all$side_played == "left",
                                     -data_all$side_tendency_500,
                                     data_all$side_tendency_500
)

data_all$side_tendency_600 <- ifelse(data_all$side_played == "left",
                                     -data_all$side_tendency_600,
                                     data_all$side_tendency_600
)

data_all$side_tendency_700 <- ifelse(data_all$side_played == "left",
                                     -data_all$side_tendency_700,
                                     data_all$side_tendency_700
)

data_all$side_tendency_800 <- ifelse(data_all$side_played == "left",
                                     -data_all$side_tendency_800,
                                     data_all$side_tendency_800
)

data_all$side_tendency_900 <- ifelse(data_all$side_played == "left",
                                     -data_all$side_tendency_900,
                                     data_all$side_tendency_900
)

data_all$side_tendency_1000 <- ifelse(data_all$side_played == "left",
                                     -data_all$side_tendency_1000,
                                     data_all$side_tendency_1000
)


#calculate the side tendency at ...
side_tendency_all_minus_300 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_minus_300 = side_tendency_minus_300)

side_tendency_all_minus_200 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_minus_200 = side_tendency_minus_200)

side_tendency_all_minus_100 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_minus_100 = side_tendency_minus_100)

side_tendency_all_0 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_0 = side_tendency_0)

side_tendency_all_100 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_100 = side_tendency_100)

side_tendency_all_200 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_200 = side_tendency_200)

side_tendency_all_300 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_300 = side_tendency_300)

side_tendency_all_400 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_400 = side_tendency_400)

side_tendency_all_500 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_500 = side_tendency_500)

side_tendency_all_600 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_600 = side_tendency_600)

side_tendency_all_700 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_700 = side_tendency_700)


side_tendency_all_800 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_800 = side_tendency_800)


side_tendency_all_900 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_900 = side_tendency_900)


side_tendency_all_1000 <- data_all %>%
  group_by(trial_number, condition, vp) %>%
  summarise(side_tendency_1000 = side_tendency_1000)

#duplicate the row with condition neutral and trial number 0
side_tendency_all_minus_300 <- rbind(side_tendency_all_minus_300, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_minus_200 <- rbind(side_tendency_all_minus_200, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_minus_100 <- rbind(side_tendency_all_minus_100, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_0 <- rbind(side_tendency_all_0, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_100 <- rbind(side_tendency_all_100, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_200 <- rbind(side_tendency_all_200, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_300 <- rbind(side_tendency_all_300, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_400 <- rbind(side_tendency_all_400, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_500 <- rbind(side_tendency_all_500, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_600 <- rbind(side_tendency_all_600, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_700 <- rbind(side_tendency_all_700, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_800 <- rbind(side_tendency_all_800, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_900 <- rbind(side_tendency_all_900, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])
side_tendency_all_1000 <- rbind(side_tendency_all_1000, side_tendency_all[side_tendency_all$condition == "neutral" & side_tendency_all$trial_number == 0,])

#change the first entry of condition with "neutral" to "congruent" and the second to "incongruent"
side_tendency_all_minus_300$condition[side_tendency_all_minus_300$condition == "neutral"][1] <- "congruent"
side_tendency_all_minus_300$condition[side_tendency_all_minus_300$condition == "neutral"] <- "incongruent"
side_tendency_all_minus_200$condition[side_tendency_all_minus_200$condition == "neutral"][1] <- "congruent"
side_tendency_all_minus_200$condition[side_tendency_all_minus_200$condition == "neutral"] <- "incongruent"
side_tendency_all_minus_100$condition[side_tendency_all_minus_100$condition == "neutral"][1] <- "congruent"
side_tendency_all_minus_100$condition[side_tendency_all_minus_100$condition == "neutral"] <- "incongruent"
side_tendency_all_0$condition[side_tendency_all_0$condition == "neutral"][1] <- "congruent"
side_tendency_all_0$condition[side_tendency_all_0$condition == "neutral"] <- "incongruent"
side_tendency_all_100$condition[side_tendency_all_100$condition == "neutral"][1] <- "congruent"
side_tendency_all_100$condition[side_tendency_all_100$condition == "neutral"] <- "incongruent"
side_tendency_all_200$condition[side_tendency_all_200$condition == "neutral"][1] <- "congruent"
side_tendency_all_200$condition[side_tendency_all_200$condition == "neutral"] <- "incongruent"
side_tendency_all_300$condition[side_tendency_all_300$condition == "neutral"][1] <- "congruent"
side_tendency_all_300$condition[side_tendency_all_300$condition == "neutral"] <- "incongruent"
side_tendency_all_400$condition[side_tendency_all_400$condition == "neutral"][1] <- "congruent"
side_tendency_all_400$condition[side_tendency_all_400$condition == "neutral"] <- "incongruent"
side_tendency_all_500$condition[side_tendency_all_500$condition == "neutral"][1] <- "congruent"
side_tendency_all_500$condition[side_tendency_all_500$condition == "neutral"] <- "incongruent"
side_tendency_all_600$condition[side_tendency_all_600$condition == "neutral"][1] <- "congruent"
side_tendency_all_600$condition[side_tendency_all_600$condition == "neutral"] <- "incongruent"
side_tendency_all_700$condition[side_tendency_all_700$condition == "neutral"][1] <- "congruent"
side_tendency_all_700$condition[side_tendency_all_700$condition == "neutral"] <- "incongruent"
side_tendency_all_800$condition[side_tendency_all_800$condition == "neutral"][1] <- "congruent"
side_tendency_all_800$condition[side_tendency_all_800$condition == "neutral"] <- "incongruent"
side_tendency_all_900$condition[side_tendency_all_900$condition == "neutral"][1] <- "congruent"
side_tendency_all_900$condition[side_tendency_all_900$condition == "neutral"] <- "incongruent"
side_tendency_all_1000$condition[side_tendency_all_1000$condition == "neutral"][1] <- "congruent"
side_tendency_all_1000$condition[side_tendency_all_1000$condition == "neutral"] <- "incongruent"


#make a new coloumn condition_dummy_code with 0 for congruent and 1 for incongruent
side_tendency_all_minus_300$condition_dummy_code <- ifelse(side_tendency_all_minus_300$condition == "congruent", 0, 1)
side_tendency_all_minus_200$condition_dummy_code <- ifelse(side_tendency_all_minus_200$condition == "congruent", 0, 1)
side_tendency_all_minus_100$condition_dummy_code <- ifelse(side_tendency_all_minus_100$condition == "congruent", 0, 1)
side_tendency_all_0$condition_dummy_code <- ifelse(side_tendency_all_0$condition == "congruent", 0, 1)
side_tendency_all_100$condition_dummy_code <- ifelse(side_tendency_all_100$condition == "congruent", 0, 1)
side_tendency_all_200$condition_dummy_code <- ifelse(side_tendency_all_200$condition == "congruent", 0, 1)
side_tendency_all_300$condition_dummy_code <- ifelse(side_tendency_all_300$condition == "congruent", 0, 1)
side_tendency_all_400$condition_dummy_code <- ifelse(side_tendency_all_400$condition == "congruent", 0, 1)
side_tendency_all_500$condition_dummy_code <- ifelse(side_tendency_all_500$condition == "congruent", 0, 1)
side_tendency_all_600$condition_dummy_code <- ifelse(side_tendency_all_600$condition == "congruent", 0, 1)
side_tendency_all_700$condition_dummy_code <- ifelse(side_tendency_all_700$condition == "congruent", 0, 1)
side_tendency_all_800$condition_dummy_code <- ifelse(side_tendency_all_800$condition == "congruent", 0, 1)
side_tendency_all_900$condition_dummy_code <- ifelse(side_tendency_all_900$condition == "congruent", 0, 1)
side_tendency_all_1000$condition_dummy_code <- ifelse(side_tendency_all_1000$condition == "congruent", 0, 1)


#Regression analysis of side tendency at lateral movement initiation---
#---------------------------------------------------------------
#delete rows with trial number 0 in correct_response_rates_all (because of sqrtarithmic transformation)
side_tendency_all_minus_300 <- side_tendency_all_minus_300[side_tendency_all_minus_300$trial_number != 0,]
side_tendency_all_minus_200 <- side_tendency_all_minus_200[side_tendency_all_minus_200$trial_number != 0,]
side_tendency_all_minus_100 <- side_tendency_all_minus_100[side_tendency_all_minus_100$trial_number != 0,]
side_tendency_all_0 <- side_tendency_all_0[side_tendency_all_0$trial_number != 0,]
side_tendency_all_100 <- side_tendency_all_100[side_tendency_all_100$trial_number != 0,]
side_tendency_all_200 <- side_tendency_all_200[side_tendency_all_200$trial_number != 0,]
side_tendency_all_300 <- side_tendency_all_300[side_tendency_all_300$trial_number != 0,]
side_tendency_all_400 <- side_tendency_all_400[side_tendency_all_400$trial_number != 0,]
side_tendency_all_500 <- side_tendency_all_500[side_tendency_all_500$trial_number != 0,]
side_tendency_all_600 <- side_tendency_all_600[side_tendency_all_600$trial_number != 0,]
side_tendency_all_700 <- side_tendency_all_700[side_tendency_all_700$trial_number != 0,]
side_tendency_all_800 <- side_tendency_all_800[side_tendency_all_800$trial_number != 0,]
side_tendency_all_900 <- side_tendency_all_900[side_tendency_all_900$trial_number != 0,]
side_tendency_all_1000 <- side_tendency_all_1000[side_tendency_all_1000$trial_number != 0,]


#minus_100
#Outlier detection with cooks distance 
#if more than 3 times more influential than an average point
model_both_sessions_minus_100 <- lm(side_tendency_minus_100 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                            data = side_tendency_all_minus_100)
summary(model_both_sessions_minus_100)
tab_model(model_both_sessions_minus_100)
plot(model_both_sessions_minus_100,4)
cd <- cooks.distance(model_both_sessions_minus_100)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- side_tendency_all_minus_100[names_of_outlier,]
side_tendency_all_minus_100 <- side_tendency_all_minus_100 %>% anti_join(outliers)
summary(side_tendency_all_minus_100)

#according to Field (2013) hierarchical model comparison
#sqrtarithmic transformation of trial number
#intercept only
intercept_only_sqrt_minus_100 <- nlme::gls(side_tendency_minus_100 ~ 1, 
                                  data = side_tendency_all_minus_100,
                                  correlation = corAR1(form = ~ 1 | vp),
                                  method = "ML",
                                  na.action = na.exclude)
#with predictors 
sqrt_minus_100_all_predictors <- nlme::gls(side_tendency_minus_100 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                  data = side_tendency_all_minus_100,
                                  correlation = corAR1(form = ~ 1 | vp),
                                  method = "ML",
                                  na.action = na.exclude)

random_intercept_sqrt_minus_100 <- nlme::lme(side_tendency_minus_100 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                  data = side_tendency_all_minus_100,
                                  correlation = corAR1(form = ~ 1 | vp),
                                  random = ~1|vp,
                                  method = "ML",
                                  na.action = na.exclude)
tab_model(random_intercept_sqrt_minus_100)
summary(random_intercept_sqrt_minus_100)

#sqrtarithmic transformation of trial number
random_slope_sqrt_minus_100 <- nlme::lme(side_tendency_minus_100 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                 data = side_tendency_all_minus_100,
                                 #correlation = corAR1(form = ~ 1 | vp), #not converging
                                 random = ~sqrt(trial_number)|vp,
                                 method = "ML",
                                 na.action = na.exclude)
tab_model(random_slope_sqrt_minus_100)
summary(random_slope_sqrt_minus_100)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(random_slope_sqrt_minus_100))
qqnorm(residuals(random_slope_sqrt_minus_100))
qqline(residuals(random_slope_sqrt_minus_100))
hist(residuals(random_slope_sqrt_minus_100))
shapiro.test(residuals(random_slope_sqrt_minus_100)) 

anova(intercept_only_sqrt_minus_100, sqrt_minus_100, random_intercept_sqrt_minus_100, random_slope_sqrt_minus_100)

#with the condition predictor only 
sqrt_minus_100_all_condition_only <- nlme::lme(side_tendency_minus_100 ~ condition_dummy_code, 
                                          data = side_tendency_all_minus_100,
                                          correlation = corAR1(form = ~ 1 | vp),
                                          random = ~sqrt(trial_number)|vp,
                                          method = "ML",
                                          na.action = na.exclude)
tab_model(sqrt_minus_100_all_condition_only)
summary(sqrt_minus_100_all_condition_only)

#0
#Outlier detection with cooks distance 
#if more than 3 times more influential than an average point
model_both_sessions_0 <- lm(side_tendency_0 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                    data = side_tendency_all_0)
summary(model_both_sessions_0)
tab_model(model_both_sessions_0)
plot(model_both_sessions_0,4)
cd <- cooks.distance(model_both_sessions_0)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- side_tendency_all_0[names_of_outlier,]
side_tendency_all_0 <- side_tendency_all_0 %>% anti_join(outliers)
summary(side_tendency_all_0)

#according to Field (2013) hierarchical model comparison
#sqrtarithmic transformation of trial number
#intercept only
intercept_only_sqrt_0 <- nlme::gls(side_tendency_0 ~ 1, 
                                          data = side_tendency_all_0,
                                          correlation = corAR1(form = ~ 1 | vp),
                                          method = "ML",
                                          na.action = na.exclude)
#with predictors 
sqrt_0 <- nlme::gls(side_tendency_0 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                           data = side_tendency_all_0,
                           correlation = corAR1(form = ~ 1 | vp),
                           method = "ML",
                           na.action = na.exclude)

random_intercept_sqrt_0 <- nlme::lme(side_tendency_0 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                            data = side_tendency_all_0,
                                            correlation = corAR1(form = ~ 1 | vp),
                                            random = ~1|vp,
                                            method = "ML",
                                            na.action = na.exclude)

#sqrtarithmic transformation of trial number
random_slope_sqrt_0 <- nlme::lme(side_tendency_0 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                        data = side_tendency_all_0,
                                        #correlation = corAR1(form = ~ 1 | vp), #not converging
                                        random = ~1|vp,
                                        method = "ML",
                                        na.action = na.exclude)
#not converging take random intercept model
tab_model(random_intercept_sqrt_0)
summary(random_intercept_sqrt_0)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(random_intercept_sqrt_0))
qqnorm(residuals(random_intercept_sqrt_0))
qqline(residuals(random_intercept_sqrt_0))
hist(residuals(random_intercept_sqrt_0))
shapiro.test(residuals(random_intercept_sqrt_0)) 

anova(intercept_only_sqrt_0, sqrt_0, random_intercept_sqrt_0)

#with the condition predictor only 
all_0_condition_only <- nlme::lme(side_tendency_0 ~ condition_dummy_code, 
                                              data = side_tendency_all_0,
                                              correlation = corAR1(form = ~ 1 | vp),
                                              random = ~sqrt(trial_number)|vp,
                                              method = "ML",
                                              na.action = na.exclude)
tab_model(all_0_condition_only)
summary(all_0_condition_only)


#100
model_both_sessions_100 <- lm(side_tendency_100 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                    data = side_tendency_all_100)
summary(model_both_sessions_100)
tab_model(model_both_sessions_100)
plot(model_both_sessions_100,4)
cd <- cooks.distance(model_both_sessions_100)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- side_tendency_all_100[names_of_outlier,]
side_tendency_all_100 <- side_tendency_all_100 %>% anti_join(outliers)
summary(side_tendency_all_100)

#according to Field (2013) hierarchical model comparison
#sqrtarithmic transformation of trial number
#intercept only
intercept_only_sqrt_100 <- nlme::gls(side_tendency_100 ~ 1, 
                                          data = side_tendency_all_100,
                                          correlation = corAR1(form = ~ 1 | vp),
                                          method = "ML",
                                          na.action = na.exclude)
#with predictors 
sqrt_100 <- nlme::gls(side_tendency_100 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                           data = side_tendency_all_100,
                           correlation = corAR1(form = ~ 1 | vp),
                           method = "ML",
                           na.action = na.exclude)
tab_model(sqrt_100)

random_intercept_sqrt_100 <- nlme::lme(side_tendency_100 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                            data = side_tendency_all_100,
                                            correlation = corAR1(form = ~ 1 | vp),
                                            random = ~1|vp,
                                            method = "ML",
                                            na.action = na.exclude)

#sqrtarithmic transformation of trial number
random_slope_sqrt_100 <- nlme::lme(side_tendency_100 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                        data = side_tendency_all_100,
                                        correlation = corAR1(form = ~ 1 | vp), #not converging
                                        random = ~sqrt(trial_number)|vp,
                                        method = "ML",
                                        na.action = na.exclude)
tab_model(random_slope_sqrt_100)
summary(random_slope_sqrt_100)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(random_slope_sqrt_100))
qqnorm(residuals(random_slope_sqrt_100))
qqline(residuals(random_slope_sqrt_100))
hist(residuals(random_slope_sqrt_100))
shapiro.test(residuals(random_slope_sqrt_100)) 

anova(intercept_only_sqrt_100, sqrt_100, random_intercept_sqrt_100, random_slope_sqrt_100)

#with the condition predictor only 
all_100_condition_only <- nlme::lme(side_tendency_100 ~ condition_dummy_code, 
                                  data = side_tendency_all_100,
                                  correlation = corAR1(form = ~ 1 | vp),
                                  random = ~1|vp,
                                  method = "ML",
                                  na.action = na.exclude)
tab_model(all_100_condition_only)
summary(all_100_condition_only)

#200
model_both_sessions_200 <- lm(side_tendency_200 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                    data = side_tendency_all_200)
summary(model_both_sessions_200)
tab_model(model_both_sessions_200)
plot(model_both_sessions_200,4)
cd <- cooks.distance(model_both_sessions_200)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- side_tendency_all_200[names_of_outlier,]
side_tendency_all_200 <- side_tendency_all_200 %>% anti_join(outliers)
summary(side_tendency_all_200)

#according to Field (2013) hierarchical model comparison
#sqrtarithmic transformation of trial number
#intercept only
intercept_only_sqrt_200 <- nlme::gls(side_tendency_200 ~ 1, 
                                          data = side_tendency_all_200,
                                          correlation = corAR1(form = ~ 1 | vp),
                                          method = "ML",
                                          na.action = na.exclude)
#with predictors 
sqrt_200 <- nlme::gls(side_tendency_200 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                           data = side_tendency_all_200,
                           correlation = corAR1(form = ~ 1 | vp),
                           method = "ML",
                           na.action = na.exclude)

random_intercept_sqrt_200 <- nlme::lme(side_tendency_200 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                            data = side_tendency_all_200,
                                            correlation = corAR1(form = ~ 1 | vp),
                                            random = ~1|vp,
                                            method = "ML",
                                            na.action = na.exclude)

#sqrtarithmic transformation of trial number
random_slope_sqrt_200 <- nlme::lme(side_tendency_200 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                        data = side_tendency_all_200,
                                        correlation = corAR1(form = ~ 1 | vp), #not converging
                                        random = ~sqrt(trial_number)|vp,
                                        method = "ML",
                                        na.action = na.exclude)
tab_model(random_slope_sqrt_200)
summary(random_slope_sqrt_200)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(random_slope_sqrt_200))
qqnorm(residuals(random_slope_sqrt_200))
qqline(residuals(random_slope_sqrt_200))
hist(residuals(random_slope_sqrt_200))
shapiro.test(residuals(random_slope_sqrt_200)) 

anova(intercept_only_sqrt_200, sqrt_200, random_intercept_sqrt_200, random_slope_sqrt_200)

all_200_condition_only <- nlme::lme(side_tendency_200 ~ condition_dummy_code, 
                                    data = side_tendency_all_200,
                                    correlation = corAR1(form = ~ 1 | vp),
                                    random = ~1|vp,
                                    method = "ML",
                                    na.action = na.exclude)
tab_model(all_200_condition_only)
summary(all_200_condition_only)

#300
model_both_sessions_300 <- lm(side_tendency_300 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                    data = side_tendency_all_300)
summary(model_both_sessions_300)
tab_model(model_both_sessions_300)
plot(model_both_sessions_300,4)
cd <- cooks.distance(model_both_sessions_300)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- side_tendency_all_300[names_of_outlier,]
side_tendency_all_300 <- side_tendency_all_300 %>% anti_join(outliers)
summary(side_tendency_all_300)

#according to Field (2013) hierarchical model comparison
#sqrtarithmic transformation of trial number
#intercept only
intercept_only_sqrt_300 <- nlme::gls(side_tendency_300 ~ 1, 
                                          data = side_tendency_all_300,
                                          correlation = corAR1(form = ~ 1 | vp),
                                          method = "ML",
                                          na.action = na.exclude)
#with predictors 
sqrt_300 <- nlme::gls(side_tendency_300 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                           data = side_tendency_all_300,
                           correlation = corAR1(form = ~ 1 | vp),
                           method = "ML",
                           na.action = na.exclude)

random_intercept_sqrt_300 <- nlme::lme(side_tendency_300 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                            data = side_tendency_all_300,
                                            correlation = corAR1(form = ~ 1 | vp),
                                            random = ~1|vp,
                                            method = "ML",
                                            na.action = na.exclude)

#sqrtarithmic transformation of trial number
random_slope_sqrt_300 <- nlme::lme(side_tendency_300 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                        data = side_tendency_all_300,
                                        correlation = corAR1(form = ~ 1 | vp), #not converging
                                        random = ~sqrt(trial_number)|vp,
                                        method = "ML",
                                        na.action = na.exclude)
tab_model(random_slope_sqrt_300)
summary(random_slope_sqrt_300)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(random_slope_sqrt_300))
qqnorm(residuals(random_slope_sqrt_300))
qqline(residuals(random_slope_sqrt_300))
hist(residuals(random_slope_sqrt_300))
shapiro.test(residuals(random_slope_sqrt_300)) 

anova(intercept_only_sqrt_300, sqrt_300, random_intercept_sqrt_300, random_slope_sqrt_300)


all_300_condition_only <- nlme::lme(side_tendency_300 ~ condition_dummy_code, 
                                    data = side_tendency_all_300,
                                    correlation = corAR1(form = ~ 1 | vp),
                                    random = ~sqrt(trial_number)|vp,
                                    method = "ML",
                                    na.action = na.exclude)
tab_model(all_300_condition_only)
summary(all_300_condition_only)

#400
model_both_sessions_400 <- lm(side_tendency_400 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                    data = side_tendency_all_400)
summary(model_both_sessions_400)
tab_model(model_both_sessions_400)
plot(model_both_sessions_400,4)
cd <- cooks.distance(model_both_sessions_400)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- side_tendency_all_400[names_of_outlier,]
side_tendency_all_400 <- side_tendency_all_400 %>% anti_join(outliers)
summary(side_tendency_all_400)

#according to Field (2013) hierarchical model comparison
#sqrtarithmic transformation of trial number
#intercept only
intercept_only_sqrt_400 <- nlme::gls(side_tendency_400 ~ 1, 
                                          data = side_tendency_all_400,
                                          correlation = corAR1(form = ~ 1 | vp),
                                          method = "ML",
                                          na.action = na.exclude)
#with predictors 
sqrt_400 <- nlme::gls(side_tendency_400 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                           data = side_tendency_all_400,
                           correlation = corAR1(form = ~ 1 | vp),
                           method = "ML",
                           na.action = na.exclude)

random_intercept_sqrt_400 <- nlme::lme(side_tendency_400 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                            data = side_tendency_all_400,
                                            correlation = corAR1(form = ~ 1 | vp),
                                            random = ~1|vp,
                                            method = "ML",
                                            na.action = na.exclude)

#sqrtarithmic transformation of trial number
random_slope_sqrt_400 <- nlme::lme(side_tendency_400 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                        data = side_tendency_all_400,
                                        correlation = corAR1(form = ~ 1 | vp), #not converging
                                        random = ~sqrt(trial_number)|vp,
                                        method = "ML",
                                        na.action = na.exclude)
tab_model(random_slope_sqrt_400)
summary(random_slope_sqrt_400)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(random_slope_sqrt_400))
qqnorm(residuals(random_slope_sqrt_400))
qqline(residuals(random_slope_sqrt_400))
hist(residuals(random_slope_sqrt_400))
shapiro.test(residuals(random_slope_sqrt_400)) 

anova(intercept_only_sqrt_400, sqrt_400, random_intercept_sqrt_400, random_slope_sqrt_400)


all_400_condition_only <- nlme::lme(side_tendency_400 ~ condition_dummy_code, 
                                    data = side_tendency_all_400,
                                    correlation = corAR1(form = ~ 1 | vp),
                                    random = ~sqrt(trial_number)|vp,
                                    method = "ML",
                                    na.action = na.exclude)
tab_model(all_400_condition_only)
summary(all_400_condition_only)

#500
model_both_sessions_500 <- lm(side_tendency_500 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                    data = side_tendency_all_500)
summary(model_both_sessions_500)
tab_model(model_both_sessions_500)
plot(model_both_sessions_500,4)
cd <- cooks.distance(model_both_sessions_500)
influential <- cd[(cd > (3 * mean(cd, na.rm = TRUE)))]
names_of_outlier <- names(influential)
outliers <- side_tendency_all_500[names_of_outlier,]
side_tendency_all_500 <- side_tendency_all_500 %>% anti_join(outliers)
summary(side_tendency_all_500)

#according to Field (2013) hierarchical model comparison
#sqrtarithmic transformation of trial number
#intercept only
intercept_only_sqrt_500 <- nlme::gls(side_tendency_500 ~ 1, 
                                          data = side_tendency_all_500,
                                          correlation = corAR1(form = ~ 1 | vp),
                                          method = "ML",
                                          na.action = na.exclude)
#with predictors 
sqrt_500 <- nlme::gls(side_tendency_500 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                           data = side_tendency_all_500,
                           correlation = corAR1(form = ~ 1 | vp),
                           method = "ML",
                           na.action = na.exclude)

random_intercept_sqrt_500 <- nlme::lme(side_tendency_500 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                            data = side_tendency_all_500,
                                            correlation = corAR1(form = ~ 1 | vp),
                                            random = ~1|vp,
                                            method = "ML",
                                            na.action = na.exclude)

#sqrtarithmic transformation of trial number
random_slope_sqrt_500 <- nlme::lme(side_tendency_500 ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                                        data = side_tendency_all_500,
                                        correlation = corAR1(form = ~ 1 | vp), #not converging
                                        random = ~sqrt(trial_number)|vp,
                                        method = "ML",
                                        na.action = na.exclude)
tab_model(random_slope_sqrt_500)
summary(random_slope_sqrt_500)


#check assumptions of linear regression
#check for normality of residuals
plot(residuals(random_slope_sqrt_500))
qqnorm(residuals(random_slope_sqrt_500))
qqline(residuals(random_slope_sqrt_500))
hist(residuals(random_slope_sqrt_500))
shapiro.test(residuals(random_slope_sqrt_500)) 

anova(intercept_only_sqrt_500, sqrt_500, random_intercept_sqrt_500, random_slope_sqrt_500)


all_500_condition_only <- nlme::lme(side_tendency_500 ~ condition_dummy_code, 
                                    data = side_tendency_all_500,
                                    correlation = corAR1(form = ~ 1 | vp),
                                    random = ~sqrt(trial_number)|vp,
                                    method = "ML",
                                    na.action = na.exclude)
tab_model(all_500_condition_only)
summary(all_500_condition_only)


#plot all together
blue_scale <- hcl(h = 240, c = seq(20, 300, length.out = 7), l = 65)
orange_scale <- hcl(h = 30, c = seq(20, 300, length.out = 7), l = 65)

df <- data.frame(
  trial_number = c(1, 80),
  y = c(-0.15, 0.15),
  index = c("congruent", "incongruent")
)
p <- ggplot(data = df, aes(x = trial_number, y = y, color = factor(blue_scale)), shape = 19) +
  labs(
    x = "trial number (#)",                     # Set x-axis title
    y = "weight shifting (m)",                         # Set y-axis title
    color = "Chroma Levels"                 # Set legend title
  )

intercept <- broom.mixed::tidy(random_slope_sqrt_minus_100, effects = "fixed")$estimate[1]
slope <- broom.mixed::tidy(random_slope_sqrt_minus_100, effects = "fixed")$estimate[2]

sqrt_func <- function(x) intercept + slope * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = orange_scale[1],
  lwd = 1)
p

intercept2 <- broom.mixed::tidy(random_slope_sqrt_0, effects = "fixed")$estimate[1]
slope2 <- broom.mixed::tidy(random_slope_sqrt_0, effects = "fixed")$estimate[2]

sqrt_func <- function(x) intercept2 + slope2 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = orange_scale[2],
  lwd = 1)
p

intercept3 <- broom.mixed::tidy(random_slope_sqrt_100, effects = "fixed")$estimate[1]
slope3 <- broom.mixed::tidy(random_slope_sqrt_100, effects = "fixed")$estimate[2]

sqrt_func <- function(x) intercept3 + slope3 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = orange_scale[3],
  lwd = 1)
p

intercept4 <- broom.mixed::tidy(random_slope_sqrt_200, effects = "fixed")$estimate[1]
slope4 <- broom.mixed::tidy(random_slope_sqrt_200, effects = "fixed")$estimate[2]

sqrt_func <- function(x) intercept4 + slope4 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = orange_scale[4],
  lwd = 1)
p


intercept5 <- broom.mixed::tidy(random_slope_sqrt_300, effects = "fixed")$estimate[1]
slope5 <- broom.mixed::tidy(random_slope_sqrt_300, effects = "fixed")$estimate[2]

sqrt_func <- function(x) intercept5 + slope5 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = orange_scale[5],
  lwd = 1)
p

intercept6 <- broom.mixed::tidy(random_slope_sqrt_400, effects = "fixed")$estimate[1]
slope6 <- broom.mixed::tidy(random_slope_sqrt_400, effects = "fixed")$estimate[2]

sqrt_func <- function(x) intercept6 + slope6 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = orange_scale[6],
  lwd = 1)
p
intercept7 <- broom.mixed::tidy(random_slope_sqrt_500, effects = "fixed")$estimate[1]
slope7 <- broom.mixed::tidy(random_slope_sqrt_500, effects = "fixed")$estimate[2]

sqrt_func <- function(x) intercept7 + slope7 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = orange_scale[7],
  lwd = 1)
p

intercept8 <- broom.mixed::tidy(random_slope_sqrt_minus_100, effects = "fixed")$estimate[1]+broom.mixed::tidy(random_slope_sqrt_minus_100, effects = "fixed")$estimate[3]
slope8 <- broom.mixed::tidy(random_slope_sqrt_minus_100, effects = "fixed")$estimate[2]+broom.mixed::tidy(random_slope_sqrt_minus_100, effects = "fixed")$estimate[4]

sqrt_func <- function(x) intercept8 + slope8 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = blue_scale[1],
  lwd = 1)
p

intercept9 <- broom.mixed::tidy(random_slope_sqrt_0, effects = "fixed")$estimate[1]+broom.mixed::tidy(random_slope_sqrt_0, effects = "fixed")$estimate[3]
slope9 <- broom.mixed::tidy(random_slope_sqrt_0, effects = "fixed")$estimate[2]+broom.mixed::tidy(random_slope_sqrt_0, effects = "fixed")$estimate[4]

sqrt_func <- function(x) intercept9 + slope9 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = blue_scale[2],
  lwd = 1)
p

intercept10 <- broom.mixed::tidy(random_slope_sqrt_100, effects = "fixed")$estimate[1]+broom.mixed::tidy(random_slope_sqrt_100, effects = "fixed")$estimate[3]
slope10 <- broom.mixed::tidy(random_slope_sqrt_100, effects = "fixed")$estimate[2]+broom.mixed::tidy(random_slope_sqrt_100, effects = "fixed")$estimate[4]

sqrt_func <- function(x) intercept10 + slope10 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = blue_scale[3],
  lwd = 1)
p

intercept11 <- broom.mixed::tidy(random_slope_sqrt_200, effects = "fixed")$estimate[1]+broom.mixed::tidy(random_slope_sqrt_200, effects = "fixed")$estimate[3]
slope11 <- broom.mixed::tidy(random_slope_sqrt_200, effects = "fixed")$estimate[2]+broom.mixed::tidy(random_slope_sqrt_200, effects = "fixed")$estimate[4]

sqrt_func <- function(x) intercept11 + slope11 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = blue_scale[4],
  lwd = 1)
p


intercept12 <- broom.mixed::tidy(random_slope_sqrt_300, effects = "fixed")$estimate[1]+broom.mixed::tidy(random_slope_sqrt_300, effects = "fixed")$estimate[3]
slope12 <- broom.mixed::tidy(random_slope_sqrt_300, effects = "fixed")$estimate[2]+broom.mixed::tidy(random_slope_sqrt_300, effects = "fixed")$estimate[4]

sqrt_func <- function(x) intercept12 + slope12 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = blue_scale[5],
  lwd = 1)
p

intercept13 <- broom.mixed::tidy(random_slope_sqrt_400, effects = "fixed")$estimate[1]+broom.mixed::tidy(random_slope_sqrt_400, effects = "fixed")$estimate[3]
slope13 <- broom.mixed::tidy(random_slope_sqrt_400, effects = "fixed")$estimate[2]+broom.mixed::tidy(random_slope_sqrt_400, effects = "fixed")$estimate[4]

sqrt_func <- function(x) intercept13 + slope13 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = blue_scale[6],
  lwd = 1)
p

intercept14 <- broom.mixed::tidy(random_slope_sqrt_500, effects = "fixed")$estimate[1]+broom.mixed::tidy(random_slope_sqrt_500, effects = "fixed")$estimate[3]
slope14 <- broom.mixed::tidy(random_slope_sqrt_500, effects = "fixed")$estimate[2]+broom.mixed::tidy(random_slope_sqrt_500, effects = "fixed")$estimate[4]

sqrt_func <- function(x) intercept14 + slope14 * sqrt(x)
p <- p + stat_function(
  fun = sqrt_func,
  colour = blue_scale[7],
  lwd = 1)
p <- p + theme(
  panel.grid = element_blank(),  
  panel.background = element_rect(fill = "white", color = NA), # Set plot background to white
  plot.background = element_rect(fill = "white", color = NA),  # Set the entire plot's background to white
  legend.position = "right",               # Position legend on the right
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 8),
  legend.key.size = unit(0.5, "lines"),
  axis.line = element_line(color = "black")# Adjust the size of legend keys
)
p
ggsave("side_tendencies_at_different_time.svg", p, width = 9, height = 6)
ggsave("side_tendencies_at_different_time.png", p, width = 9, height = 6)




blue_scale <- hcl(h = 240, c = seq(20, 300, length.out = 7), l = 65)
orange_scale <- hcl(h = 30, c = seq(20, 300, length.out = 7), l = 65)

# Combine the scales into a data frame for the legend
color_data <- data.frame(
  Color = factor(c(paste0("Blue ", 1:7), paste0("Orange ", 1:7))),
  Value = 1,  # Dummy value just to plot something
  ColorValue = c(blue_scale, orange_scale)
)

# Create the ggplot object
color_plot <- ggplot(data = color_data, aes(x = Value, fill = Color)) +
  geom_bar() +
  scale_fill_manual(
    values = color_data$ColorValue,
    labels = c("incongruent 0.1 s before serve", "incongruent at serve", "incongruent 0.1 s after serve", "incongruent 0.2 s after serve", "incongruent 0.3 s after serve", "incongruent 0.4 s after serve", "incongruent 0.5 s after serve",
               "congruent 0.1 s before serve", "congruent at serve", "congruent 0.1 s after serve", "congruent 0.2 s after serve", "congruent 0.3 s after serve", "congruent 0.4 s after serve", "congruent 0.5 s after serve")
  ) +
  theme_void() +  # Remove all plot elements (axes, grid, etc.)
  theme(
    legend.position = "right",           # Position legend on the right
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "lines")   # Adjust the size of legend keys if necessary
  ) +
  guides(fill = guide_legend(title = ""))
color_plot
ggsave("legend.svg", color_plot, width = 9, height = 6)
ggsave("legend.png", color_plot, width = 9, height = 6)


















#Calculate side tendency at time one specific time----
#---------------------------------------------------------------
specified_columns <- c("side_tendency_minus_50", 
                       "side_tendency_minus_45",
                       "side_tendency_minus_40",
                       "side_tendency_minus_35",
                       "side_tendency_minus_30",
                       "side_tendency_minus_25",
                       "side_tendency_minus_20",
                       "side_tendency_minus_15",
                       "side_tendency_minus_10",
                       "side_tendency_minus_5",
                       "side_tendency_0",
                       "side_tendency_5",
                       "side_tendency_10",
                       "side_tendency_15",
                       "side_tendency_20",
                       "side_tendency_25",
                       "side_tendency_30",
                       "side_tendency_35",
                       "side_tendency_40",
                       "side_tendency_45",
                       "side_tendency_50",
                       "side_tendency_100",
                       "side_tendency_150",
                       "side_tendency_200",
                       "side_tendency_250",
                       "side_tendency_300",
                       "side_tendency_350",
                       "side_tendency_400",
                       "side_tendency_450",
                       "side_tendency_500",
                       "side_tendency_550",
                       "side_tendency_600",
                       "side_tendency_650",
                       "side_tendency_700")

specified_columns <- c("side_tendency_minus_50")

data_frames_list <- list()

#make a new column condition_dummy_code with 0 for congruent and 1 for incongruent
data_all$condition_dummy_code <- ifelse(data_all$condition == "congruent", 0, 1)

#change sign of the side_tendency
for (col in specified_columns) {
  data_all$side_tendency <- ifelse(data_all$side_played == "left",
                                   -data_all[[col]],
                                   data_all[[col]])
  data_all <- data_all[data_all$trial_number != 0,]
}

#calculate the side_tendency 
for (col in specified_columns) {
  new_col_name <- paste0(col)
  new_df <- data_all %>%
    group_by(trial_number, condition_dummy_code, vp) %>%
    summarise(!!new_col_name := data_all[[col]])
  # Append the new DataFrame to the list
  assign(col, new_df)
}

# Fit the linear model
for (col in specified_columns) {
  model <- lm(as.formula(paste(col, "~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code")), 
              data = get(col))
  result <- remove_outliers(get(col),model, 3)
  #assign(col, result)
  #sqrtarithmic transformation of trial number
  model_var_name <- paste0(col, "_model")
  hierarchical_model <- nlme::lme(as.formula(paste(col, "~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code")), 
                                  data = result,
                                  #correlation = corAR1(form = ~ 1 | vp),
                                  random = ~sqrt(trial_number)|vp,
                                  method = "ML",
                                  na.action = na.exclude)
  assign(model_var_name, hierarchical_model)
}

tab_model(side_tendency_minus_50_model)
tab_model(side_tendency_minus_45_model)
tab_model(side_tendency_minus_40_model)
tab_model(side_tendency_minus_35_model)
tab_model(side_tendency_minus_30_model)
tab_model(side_tendency_minus_25_model)
tab_model(side_tendency_minus_20_model)
tab_model(side_tendency_minus_15_model)
tab_model(side_tendency_minus_10_model)
tab_model(side_tendency_minus_5_model)
tab_model(side_tendency_0_model)
tab_model(side_tendency_5_model)
tab_model(side_tendency_10_model)
tab_model(side_tendency_15_model)
tab_model(side_tendency_20_model)
tab_model(side_tendency_25_model)
tab_model(side_tendency_30_model)
tab_model(side_tendency_35_model)
tab_model(side_tendency_40_model)
tab_model(side_tendency_45_model)
tab_model(side_tendency_50_model)
tab_model(side_tendency_100_model)
tab_model(side_tendency_150_model)
tab_model(side_tendency_200_model)
tab_model(side_tendency_250_model)
tab_model(side_tendency_300_model)
tab_model(side_tendency_350_model)
tab_model(side_tendency_400_model)
tab_model(side_tendency_450_model)
tab_model(side_tendency_500_model)
tab_model(side_tendency_550_model)
tab_model(side_tendency_600_model)
tab_model(side_tendency_650_model)
tab_model(side_tendency_700_model)

plot <- ggplot(side_tendency_all_0, aes(x = trial_number, y = side_tendency_0, color = condition)) +
  #geom_point() +
  geom_smooth(method = "lm",formula = y~sqrt(x), se = TRUE, level = 0.95, fullrange = TRUE)+
  labs(title = "",
       x = "trial number (#)",
       y = "side tendency")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )
plot
ggsave("both_session_sqrt_side_tendency_at_0.svg", plot, width = 9, height = 6)
ggsave("both_session_sqrt_side_tendency_at_0.png", plot, width = 9, height = 6)

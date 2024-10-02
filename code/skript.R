#---------------------------------------------------------------
# VR-Tennis

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
#install.packages("dplyr", dependencies = TRUE)
#install.packages("MASS", dependencies = TRUE)
#install.packages("lme4", dependencies = TRUE)
#install.packages("lmtest", dependencies = TRUE)

library(car)
library(ggplot2)
library(nlme)
library(reshape)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(modi)
library(robustlmm)
library(dplyr)
library(MASS)
library(lme4)
library(lmtest)


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
data_all <- read.csv("data/all_data.csv", header = TRUE, sep = ",")
View(data_all)
summary(data_all)

# Data preparation ----
#---------------------------------------------------------------
#change True->1 and False->0 for the column of hit_true_false
data_all$correct_response <- ifelse(data_all$correct_response == "1", 1,
                                    ifelse(data_all$correct_response == "0", 0,
                                           ifelse(data_all$correct_response == "TRUE", TRUE,
                                                  ifelse(data_all$correct_response == "FALSE", FALSE, NA))))
data_all$correct_response

data_all$splitstep_performed_com_5cm_below_max <- ifelse(data_all$splitstep_performed_com_5cm_below_max == "1", 1,
                                    ifelse(data_all$splitstep_performed_com_5cm_below_max == "0", 0,
                                           ifelse(data_all$splitstep_performed_com_5cm_below_max == "TRUE", TRUE,
                                                  ifelse(data_all$splitstep_performed_com_5cm_below_max == "FALSE", FALSE, NA))))
data_all$splitstep_performed_com_5cm_below_max

#create a new column congruent_number
data_all$congruent_number <- ifelse(data_all$condition == "congruent", 1, 0)
data_all$incongruent_number <- ifelse(data_all$condition == "incongruent", 1, 0)

#filter data_all for all congruent, incongruent, neutral, all(without warm up) trials
data_congruent <- filter(data_all, condition == "congruent")
data_incongruent <- filter(data_all, condition == "incongruent")
data_neutral <- filter(data_all, condition == "neutral", trial > 99, trial < 220 | trial > 320)
data_all <- filter(data_all, trial > 99, trial < 220 | trial > 320)

#loop trough data_congruent and change the congruent_number line number modulo 64
#in order to give a congruent_number to each trial
for (i in 1:nrow(data_congruent)){
  data_congruent$congruent_number[i] <- (i %% 64)
  #check if the modulo is 0 and change it to 64
  if (data_congruent$congruent_number[i] == 0){
    data_congruent$congruent_number[i] <- 64
  }
}

#loop trough data_incongruent and change the incongruent_number line number modulo 16
#in order to give a incongruent_number to each trial
for (i in 1:nrow(data_incongruent)){
  data_incongruent$incongruent_number[i] <- (i%% 16)
  #check if the modulo is 0 and change it to 16
  if (data_incongruent$incongruent_number[i] == 0){
    data_incongruent$incongruent_number[i] <- 16
  }
}

#combine all three data_congruent, data_incongruent and data neutral to one
data_all <- rbind(data_congruent, data_incongruent, data_neutral)

#multiply the congruent_number with 1.25 in order to get values between 1 and 80
data_all$congruent_number <- data_all$congruent_number * 1.25 - 0.125

#replace negative values with 0
data_all$congruent_number[data_all$congruent_number < 0] <- 0

#multiply the incongruent_number with 5 and subtract 2.5 in order to get values between 1 and 80
data_all$incongruent_number <- data_all$incongruent_number * 5 - 2.5

#replace negative values with 0
data_all$incongruent_number[data_all$incongruent_number < 0] <- 0

#make a new column trial_number by adding the congruent_number and incongruent_number
data_all$trial_number <- data_all$congruent_number + data_all$incongruent_number

#have a look at the data
View(data_all)

#Calculate descriptive statistics ----
#---------------------------------------------------------------
#filter data_all for hit == TRUE and left_or_right == "left" and "right"
data_all_left_hit <- filter(data_all, hit_true_false == 1 & side_played == "left")
data_all_right_hit <- filter(data_all, hit_true_false == 1 & side_played == "right")
data_all_left_correct_response <- filter(data_all, correct_response == 1 & side_played == "left")
data_all_right_correct_response <- filter(data_all, correct_response == 1 & side_played == "right")
data_neutral_correct_response <- filter(data_neutral, correct_response == 1)
data_neutral_hit <- filter(data_neutral, hit_true_false == 1)
data_all_split_performed_true <- filter(data_all, 
                                   splitstep_performed_com_5cm_below_max == 1)
data_all_split_performed_false <- filter(data_all, 
                                        splitstep_performed_com_5cm_below_max == 0)

#split step actions
data_all_split_Step_starts <- filter(data_all, 
                                     !is.na(splitstep_start))
data_all_time_of_lateral_movement_initiation <- filter(data_all,
                                                       !is.na(lateral_movement_initiation) &
                                                         lateral_movement_initiation > -0.3)
data_all_hit_time <- filter(data_all,
                              !is.na(hit_time))


#hit rate
length(data_all$trial) #3345 overall correct respons (959+953)/3345=57.2%
                      #overall hit rate (730+822)/3345=46.4%
length(data_all_left_correct_response$trial) #959 (959/953*57.2= 57.6%)
length(data_all_right_correct_response$trial) #953 (953/959*57.2= 56.8%)
length(data_all_left_hit$trial) #730 (46.4*730/822=41.2%)
length(data_all_right_hit$trial) #822 (46.4/730*822=52.2%)
length(data_neutral$trial) # 1105 
length(data_neutral_correct_response$trial) # 547 (49.5%)
length(data_neutral_hit$trial) # 392 (35.5%)
length(data_all_split_performed_true$trial) # 3030 (3030/(3030+315)=90.6%)
length(data_all_split_performed_false$trial) # 315 (315/(3030+315)=9.4%)


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
hit_rates_all <- data_all %>%
  group_by(trial_number, condition) %>%
  summarise(hit_rate = sum(hit_true_false == 1)/(sum(hit_true_false == 1)+sum(hit_true_false == 0)))

#multiply the hit rates with 100 in order to have % values
hit_rates_all$hit_rate <- hit_rates_all$hit_rate * 100

#make a new coloumn condition_dummy_code with 0 for congruent and 1 for incongruent
hit_rates_all$condition_dummy_code <- ifelse(hit_rates_all$condition == "congruent", 1, 0)

#have a look at the hit rates
View(hit_rates_all)


#Regression analysis of hit rate---
#---------------------------------------------------------------
#delete rows with trial number 0 (neutral trials)
hit_rates_all <- hit_rates_all[hit_rates_all$trial_number != 0,]

#sqrt transformation of trial number
hit_rate_sqrt <- lm(hit_rate ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                        data = hit_rates_all,
                        method = "qr",
                        na.action = na.exclude)
tab_model(hit_rate_sqrt)
summary(hit_rate_sqrt)

#plot the hit rates
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
ggsave("plots/sqrt_hit_rate.svg", plot, width = 9, height = 6)
ggsave("plots/sqrt_hit_rate.png", plot, width = 9, height = 6)



#Calculate correct response rates ----
#---------------------------------------------------------------
#calculate correct response rate for first session
#count all TRUE values for each trial number and condition
correct_response_rates_all <- data_all %>%
  group_by(trial_number, condition) %>%
  summarise(correct_response_rate = sum(correct_response == 1)/(sum(correct_response == 1)+sum(correct_response == 0)))

#multiply the hit rates with 100 in order to have % values
correct_response_rates_all$correct_response_rate <- correct_response_rates_all$correct_response_rate * 100

#duplicate the row with condition neutral and trial number 0
correct_response_rates_all <- rbind(correct_response_rates_all, correct_response_rates_all[correct_response_rates_all$condition == "neutral" & correct_response_rates_all$trial_number == 0,])

#change the first entry of condition with "neutral" to "congruent" and the second to "incongruent"
correct_response_rates_all$condition[correct_response_rates_all$condition == "neutral"][1] <- "congruent"
correct_response_rates_all$condition[correct_response_rates_all$condition == "neutral"] <- "incongruent"


#make a new coloumn condition_dummy_code with 0 for congruent and 1 for incongruent
correct_response_rates_all$condition_dummy_code <- ifelse(correct_response_rates_all$condition == "congruent", 1, 0)

#have a look at the hit rates
View(correct_response_rates_all)


#Regression analysis of correct response rate---
#---------------------------------------------------------------
#delete rows with trial number 0 (neutral trials
correct_response_rates_all <- correct_response_rates_all[correct_response_rates_all$trial_number != 0,]

#sqrt transformation of trial number
correct_response_rate_sqrt <- lm(correct_response_rate ~ sqrt(trial_number) + condition_dummy_code + sqrt(trial_number)*condition_dummy_code, 
                        data = correct_response_rates_all,
                        method = "qr",
                        na.action = na.exclude)
tab_model(correct_response_rate_sqrt)
summary(correct_response_rate_sqrt)


#plot the correct response rates
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
ggsave("plots/sqrt_correct_response_rate.svg", plot, width = 9, height = 6)
ggsave("plots/sqrt_correct_response_rate.png", plot, width = 9, height = 6)




#mean development of side tendency over time ---
#---------------------------------------------------------------
#filter data all such that only congruent with correct response = 1
data_tendency <- data_all %>%
  filter(trial_number > 40) #40 because the only the two last blocks are considered
for (i in 13:253) {data_tendency[,i] <- ifelse(data_tendency$side_played == "left", -data_tendency[,i], data_tendency[,i])}
summary(data_tendency)

data_tendency_congruent_correct <- data_tendency %>%
  filter(condition == "congruent", correct_response == 1)
data_tendency_congruent_incorrect <- data_tendency %>%
  filter(condition == "congruent", correct_response == 0)
data_tendency_incongruent_correct <- data_tendency %>%
  filter(condition == "incongruent", correct_response == 1)
data_tendency_incongruent_incorrect <- data_tendency %>%
  filter(condition == "incongruent", correct_response == 0)


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
for (i in 13:253) {data_tendency_neutral[,i] <- ifelse(data_tendency_neutral$side_played == "left", -data_tendency_neutral[,i], data_tendency_neutral[,i])}

data_tendency_neutral_correct <- data_tendency_neutral %>%
  filter(condition == "neutral", correct_response == 1)
data_tendency_neutral_incorrect <- data_tendency_neutral %>%
  filter(condition == "neutral", correct_response == 0)

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
  annotate("text", x = 0, y = max(neutral_correct$side_tendency) + 0.01, 
           label = "Serve", angle = 90, vjust = -0.5, hjust = 0) +
  annotate("text", x = 466, y = max(neutral_correct$side_tendency) - 0.02, 
           label = "Bounce", angle = 90, vjust = -0.5, hjust = 0) +
  annotate("text", x = 570, y = min(neutral_correct$side_tendency) + 0.47,  # Lowered y position even more
           label = "Lateral movement initiation", angle = 90, vjust = -0.6, hjust = 0) +
  annotate("text", x = 990, y = max(neutral_correct$side_tendency) - 0.00 , 
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
    x = "Time to serve (ms)", 
    y = "Weight shift in direction to the prior (m)", 
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


save_plot("plots/prior_impact_over_second_half_of_biased_and_neutral_trials_on_weight_shift.png", width = 20, height = 12)




#weight shift significant different from 0?
#---------------------------------------------------------------
#data directed to the prior
data_directed_prior <- data_all
for (i in 13:253) {data_directed_prior[,i] <- ifelse(data_directed_prior$side_played == "left", -data_directed_prior[,i], data_directed_prior[,i])}
for (i in 13:253) {data_directed_prior[,i] <- ifelse(data_directed_prior$condition == "incongruent", -data_directed_prior[,i], data_directed_prior[,i])}
data_directed_prior$condition_dummy <- ifelse(data_directed_prior$condition == "congruent", 1, 0) #convert condition to dummy code
summary(data_directed_prior)

data_directed_prior_second_half <- data_directed_prior %>%
  filter(trial_number > 40) #40 because the only the two last blocks are considered

#have a look at the data
View(data_directed_prior_second_half)

#minus_100
#Outlier detection with cooks distance 
#if more than 3 times more influential than an average point
mean_sessions_minus_100 <- lm(side_tendency_minus_100 ~ 1, 
                              data = data_directed_prior_second_half, 
                              na.action = na.omit)
summary(mean_sessions_minus_100)
tab_model(mean_sessions_minus_100)
data_directed_prior_second_half_mean <- remove_outliers(data_directed_prior_second_half,mean_sessions_minus_100,3)
summary(data_directed_prior_second_half_mean)

#according to Field (2013) hierarchical model comparison
#intercept only
intercept_only_minus_100 <- nlme::gls(side_tendency_minus_100 ~ 1, 
                                      data = data_directed_prior_second_half_mean,
                                      method = "ML",
                                      na.action = na.exclude)
tab_model(intercept_only_minus_100)

intercept_only_minus_100_rI <- nlme::lme(side_tendency_minus_100 ~ 1, 
                                         data = data_directed_prior_second_half_mean,
                                         random = ~1|vp,
                                         method = "ML",
                                         na.action = na.exclude)
tab_model(intercept_only_minus_100_rI)
summary(intercept_only_minus_100_rI)

#check assumptions of linear regression
#check for normality of residuals
plot(residuals(intercept_only_minus_100))
qqnorm(residuals(intercept_only_minus_100))
qqline(residuals(intercept_only_minus_100))
hist(residuals(intercept_only_minus_100))

anova(intercept_only_minus_100, intercept_only_minus_100_rI)

#100
#Outlier detection with cooks distance 
#if more than 3 times more influential than an average point
mean_sessions_100 <- lm(side_tendency_100 ~ 1, 
                        data = data_directed_prior_second_half, 
                        na.action = na.omit)
summary(mean_sessions_100)
tab_model(mean_sessions_100)
data_directed_prior_second_half_mean <- remove_outliers(data_directed_prior_second_half,mean_sessions_100,3)
summary(data_directed_prior_second_half_mean)

#according to Field (2013) hierarchical model comparison
#intercept only
intercept_only_100 <- nlme::gls(side_tendency_100 ~ 1, 
                                data = data_directed_prior_second_half_mean,
                                method = "ML",
                                na.action = na.exclude)
tab_model(intercept_only_100)

intercept_only_100_rI <- nlme::lme(side_tendency_100 ~ 1, 
                                   data = data_directed_prior_second_half_mean,
                                   random = ~1|vp,
                                   method = "ML",
                                   na.action = na.exclude)
tab_model(intercept_only_100_rI)
summary(intercept_only_100_rI)

#check assumptions of linear regression
#check for normality of residuals
plot(residuals(intercept_only_100))
qqnorm(residuals(intercept_only_100))
qqline(residuals(intercept_only_100))
hist(residuals(intercept_only_100))

anova(intercept_only_100, intercept_only_100_rI)

#300
#Outlier detection with cooks distance 
#if more than 3 times more influential than an average point
mean_sessions_300 <- lm(side_tendency_300 ~ 1, 
                        data = data_directed_prior_second_half, 
                        na.action = na.omit)
summary(mean_sessions_300)
tab_model(mean_sessions_300)
data_directed_prior_second_half_mean <- remove_outliers(data_directed_prior_second_half,mean_sessions_300,3)
summary(data_directed_prior_second_half_mean)

#according to Field (2013) hierarchical model comparison
#intercept only
intercept_only_300 <- nlme::gls(side_tendency_300 ~ 1, 
                                data = data_directed_prior_second_half_mean,
                                method = "ML",
                                na.action = na.exclude)
tab_model(intercept_only_300)

intercept_only_300_rI <- nlme::lme(side_tendency_300 ~ 1, 
                                   data = data_directed_prior_second_half_mean,
                                   random = ~1|vp,
                                   method = "ML",
                                   na.action = na.exclude)
tab_model(intercept_only_300_rI)
summary(intercept_only_300_rI)

#check assumptions of linear regression
#check for normality of residuals
plot(residuals(intercept_only_300))
qqnorm(residuals(intercept_only_300))
qqline(residuals(intercept_only_300))
hist(residuals(intercept_only_300))

anova(intercept_only_300, intercept_only_300_rI)

#500
#Outlier detection with cooks distance 
#if more than 3 times more influential than an average point
mean_sessions_500 <- lm(side_tendency_500 ~ 1, 
                        data = data_directed_prior_second_half, 
                        na.action = na.omit)
summary(mean_sessions_500)
tab_model(mean_sessions_500)
data_directed_prior_second_half_mean <- remove_outliers(data_directed_prior_second_half,mean_sessions_500,3)
summary(data_directed_prior_second_half_mean)

#according to Field (2013) hierarchical model comparison
#intercept only
intercept_only_500 <- nlme::gls(side_tendency_500 ~ 1, 
                                data = data_directed_prior_second_half_mean,
                                correlation = corAR1(form = ~ 1 | vp),
                                method = "ML",
                                na.action = na.exclude)
tab_model(intercept_only_500)

intercept_only_500_rI <- nlme::lme(side_tendency_500 ~ 1, 
                                   data = data_directed_prior_second_half_mean,
                                   random = ~1|vp,
                                   correlation = corAR1(form = ~ 1 | vp),
                                   method = "ML",
                                   na.action = na.exclude)
tab_model(intercept_only_500_rI)
summary(intercept_only_500_rI)

#check assumptions of linear regression
#check for normality of residuals
plot(residuals(intercept_only_500))
qqnorm(residuals(intercept_only_500))
qqline(residuals(intercept_only_500))
hist(residuals(intercept_only_500))

anova(intercept_only_500, intercept_only_500_rI)





#logistic regression of direction taken with the predictor weight shift at -100, 100, 300, 500
#---------------------------------------------------------------
#data second half
data_second_half <- data_tendency %>%
  filter(trial_number > 40) #40 because the only the two last blocks are considered
data_second_half$direction_taken_condition_dummy <- ifelse(data_second_half$side_played == "left" & data_second_half$condition == "congruent", 1,
                                                     ifelse(data_second_half$side_played == "left" & data_second_half$condition == "incongruent", 0,
                                                            ifelse(data_second_half$side_played == "right" & data_second_half$condition == "congruent", 0,
                                                                   ifelse(data_second_half$side_played == "right" & data_second_half$condition == "incongruent", 1, NA))))

#have a look at the data
View(data_second_half)

#evaluated at -100, 100, 300, 500
#-100
check_oultiers <- glm(direction_taken_condition_dummy ~ side_tendency_minus_100, 
                                       data = data_second_half, family = binomial(link = "logit"))
tab_model(check_oultiers)
summary(check_oultiers)
data_second_half_minus_100 <- remove_outliers(data_second_half, check_oultiers, 3)

#building models for hit_true_false with increasing complexity
intercept_model_minus_100_direction_taken <- glm(direction_taken_condition_dummy ~ 1, 
                                       data = data_second_half_minus_100, family = binomial(link = "logit"))
tab_model(intercept_model_minus_100_direction_taken)
summary(intercept_model_minus_100_direction_taken)

model_minus_100_direction_taken <- glm(direction_taken_condition_dummy ~ side_tendency_minus_100, 
                                           data = data_second_half_minus_100, family = binomial(link = "logit"))
tab_model(model_minus_100_direction_taken)
summary(model_minus_100_direction_taken)

model_minus_100_direction_taken_rI <- glmer(direction_taken_condition_dummy ~ side_tendency_minus_100 + (1 | vp), 
                                         data = data_second_half_minus_100, family = binomial(link = "logit"))
tab_model(model_minus_100_direction_taken_rI)
summary(model_minus_100_direction_taken_rI)

model_minus_100_direction_taken_rSI <- glmer(direction_taken_condition_dummy ~ side_tendency_minus_100 + (side_tendency_minus_100 | vp), 
                                      data = data_second_half_minus_100, family = binomial(link = "logit"))
tab_model(model_minus_100_direction_taken_rSI)
summary(model_minus_100_direction_taken_rSI)

lrtest(intercept_model_minus_100_direction_taken, model_minus_100_direction_taken, model_minus_100_direction_taken_rI, model_minus_100_direction_taken_rSI)
AIC(intercept_model_minus_100_direction_taken,model_minus_100_direction_taken, model_minus_100_direction_taken_rI, model_minus_100_direction_taken_rSI)
BIC(intercept_model_minus_100_direction_taken,model_minus_100_direction_taken, model_minus_100_direction_taken_rI, model_minus_100_direction_taken_rSI)


#100
check_oultiers <- glm(direction_taken_condition_dummy ~ side_tendency_100, 
                      data = data_second_half, family = binomial(link = "logit"))
tab_model(check_oultiers)
summary(check_oultiers)
data_second_half_100 <- remove_outliers(data_second_half, check_oultiers, 3)

#building models for hit_true_false with increasing complexity
intercept_model_100_direction_taken <- glm(direction_taken_condition_dummy ~ 1, 
                                     data = data_second_half_100, family = binomial(link = "logit"))
tab_model(intercept_model_100_direction_taken)
summary(intercept_model_100_direction_taken)

model_100_direction_taken <- glm(direction_taken_condition_dummy ~ side_tendency_100, 
                           data = data_second_half_100, family = binomial(link = "logit"))
tab_model(model_100_direction_taken)
summary(model_100_direction_taken)

model_100_direction_taken_rI <- glmer(direction_taken_condition_dummy ~ side_tendency_100 + (1 | vp), 
                                data = data_second_half_100, family = binomial(link = "logit"))
tab_model(model_100_direction_taken_rI)
summary(model_100_direction_taken_rI)

model_100_direction_taken_rSI <- glmer(direction_taken_condition_dummy ~ side_tendency_100 + (side_tendency_100 | vp), 
                                 data = data_second_half_100, family = binomial(link = "logit"))
tab_model(model_100_direction_taken_rSI)
summary(model_100_direction_taken_rSI)

lrtest(intercept_model_100_direction_taken, model_100_direction_taken, model_100_direction_taken_rI, model_100_direction_taken_rSI)
AIC(intercept_model_100_direction_taken,model_100_direction_taken, model_100_direction_taken_rI, model_100_direction_taken_rSI)
BIC(intercept_model_100_direction_taken,model_100_direction_taken, model_100_direction_taken_rI, model_100_direction_taken_rSI)


#300
check_oultiers <- glm(direction_taken_condition_dummy ~ side_tendency_300, 
                      data = data_second_half, family = binomial(link = "logit"))
tab_model(check_oultiers)
summary(check_oultiers)
data_second_half_300 <- remove_outliers(data_second_half, check_oultiers, 3)

#building models for hit_true_false with increasing complexity
intercept_model_300_direction_taken <- glm(direction_taken_condition_dummy ~ 1, 
                                     data = data_second_half_300, family = binomial(link = "logit"))
tab_model(intercept_model_300_direction_taken)
summary(intercept_model_300_direction_taken)

model_300_direction_taken <- glm(direction_taken_condition_dummy ~ side_tendency_300, 
                           data = data_second_half_300, family = binomial(link = "logit"))
tab_model(model_300_direction_taken)
summary(model_300_direction_taken)

model_300_direction_taken_rI <- glmer(direction_taken_condition_dummy ~ side_tendency_300 + (1 | vp), 
                                data = data_second_half_300, family = binomial(link = "logit"))
tab_model(model_300_direction_taken_rI)
summary(model_300_direction_taken_rI)

model_300_direction_taken_rSI <- glmer(direction_taken_condition_dummy ~ side_tendency_300 + (side_tendency_300 | vp), 
                                 data = data_second_half_300, family = binomial(link = "logit"))
tab_model(model_300_direction_taken_rSI)
summary(model_300_direction_taken_rSI)

lrtest(intercept_model_300_direction_taken, model_300_direction_taken, model_300_direction_taken_rI, model_300_direction_taken_rSI)
AIC(intercept_model_300_direction_taken,model_300_direction_taken, model_300_direction_taken_rI, model_300_direction_taken_rSI)
BIC(intercept_model_300_direction_taken,model_300_direction_taken, model_300_direction_taken_rI, model_300_direction_taken_rSI)


#500
check_oultiers <- glm(direction_taken_condition_dummy ~ side_tendency_500, 
                      data = data_second_half, family = binomial(link = "logit"))
tab_model(check_oultiers)
summary(check_oultiers)
data_second_half_500 <- remove_outliers(data_second_half, check_oultiers, 3)

#building models for hit_true_false with increasing complexity
intercept_model_500_direction_taken <- glm(direction_taken_condition_dummy ~ 1, 
                                     data = data_second_half_500, family = binomial(link = "logit"))
tab_model(intercept_model_500_direction_taken)
summary(intercept_model_500_direction_taken)

model_500_direction_taken <- glm(direction_taken_condition_dummy ~ side_tendency_500, 
                           data = data_second_half_500, family = binomial(link = "logit"))
tab_model(model_500_direction_taken)
summary(model_500_direction_taken)

model_500_direction_taken_rI <- glmer(direction_taken_condition_dummy ~ side_tendency_500 + (1 | vp), 
                                data = data_second_half_500, family = binomial(link = "logit"))
tab_model(model_500_direction_taken_rI)
summary(model_500_direction_taken_rI)

model_500_direction_taken_rSI <- glmer(direction_taken_condition_dummy ~ side_tendency_500 + (side_tendency_500 | vp), 
                                 data = data_second_half_500, family = binomial(link = "logit"))
tab_model(model_500_direction_taken_rSI)
summary(model_500_direction_taken_rSI)

lrtest(intercept_model_500_direction_taken, model_500_direction_taken, model_500_direction_taken_rI, model_500_direction_taken_rSI)
AIC(intercept_model_500_direction_taken,model_500_direction_taken, model_500_direction_taken_rI, model_500_direction_taken_rSI)
BIC(intercept_model_500_direction_taken,model_500_direction_taken, model_500_direction_taken_rI, model_500_direction_taken_rSI)






#logistic regression of hit_true_false and correct_response with predictors ---
#---------------------------------------------------------------
#weight shift at -100, 100, 300, 500 and condition
#data directed to the prior
data_directed_prior <- data_all
for (i in 13:253) {data_directed_prior[,i] <- ifelse(data_directed_prior$side_played == "left", -data_directed_prior[,i], data_directed_prior[,i])}
for (i in 13:253) {data_directed_prior[,i] <- ifelse(data_directed_prior$condition == "incongruent", -data_directed_prior[,i], data_directed_prior[,i])}
data_directed_prior$condition_dummy <- ifelse(data_directed_prior$condition == "congruent", 1, 0) #convert condition to dummy code
summary(data_directed_prior)

data_directed_prior_second_half <- data_directed_prior %>%
  filter(trial_number > 40) #40 because the only the two last blocks are considered

#have a look at the data
View(data_directed_prior_second_half)

#calculate logistic regression for hit_true_false
#evaluated at -100, 100, 300, 500

#correct_response
#-100
detect_oultier_minus_100_response <- glm(correct_response ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy, 
                                        data = data_directed_prior_second_half, family = binomial(link = "logit"))
tab_model(detect_oultier_minus_100_response)
summary(detect_oultier_minus_100_response)
data_minus_100_response_without_outliers <- remove_outliers(data_directed_prior_second_half, detect_oultier_minus_100_response, 3)

#building models for correct_response with increasing complexity
intercept_model_n_minus_100_response <- glm(correct_response ~ 1, 
                                           data = data_minus_100_response_without_outliers, family = binomial(link = "logit"))
tab_model(intercept_model_n_minus_100_response)
summary(intercept_model_n_minus_100_response)

logistic_model_n_minus_100_response <- glm(correct_response ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy, 
                                          data = data_minus_100_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_n_minus_100_response)
summary(logistic_model_n_minus_100_response)

logistic_model_rI_minus_100_response <- glmer(correct_response ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy+ (1 | vp), 
                                             data = data_minus_100_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rI_minus_100_response)
summary(logistic_model_rI_minus_100_response)

logistic_model_rSI_minus_100_response <- glmer(correct_response ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy+ (side_tendency_minus_100 | vp), 
                                              data = data_minus_100_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rSI_minus_100_response)
summary(logistic_model_rSI_minus_100_response)

lrtest(intercept_model_n_minus_100_response,logistic_model_n_minus_100_response, logistic_model_rI_minus_100_response, logistic_model_rSI_minus_100_response)
AIC(intercept_model_n_minus_100_response,logistic_model_n_minus_100_response, logistic_model_rI_minus_100_response, logistic_model_rSI_minus_100_response)
BIC(intercept_model_n_minus_100_response,logistic_model_n_minus_100_response, logistic_model_rI_minus_100_response, logistic_model_rSI_minus_100_response)


#100
detect_oultier_100_response <- glm(correct_response ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy, 
                                  data = data_directed_prior_second_half, family = binomial(link = "logit"))
tab_model(detect_oultier_100_response)
summary(detect_oultier_100_response)
data_100_response_without_outliers <- remove_outliers(data_directed_prior_second_half, detect_oultier_100_response, 3)

#building models for correct_response with increasing complexity
intercept_model_n_100_response <- glm(correct_response ~ 1, 
                                     data = data_100_response_without_outliers, family = binomial(link = "logit"))
tab_model(intercept_model_n_100_response)
summary(intercept_model_n_100_response)

logistic_model_n_100_response <- glm(correct_response ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy, 
                                    data = data_100_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_n_100_response)
summary(logistic_model_n_100_response)

logistic_model_rI_100_response <- glmer(correct_response ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy+ (1 | vp), 
                                       data = data_100_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rI_100_response)
summary(logistic_model_rI_100_response)

logistic_model_rSI_100_response <- glmer(correct_response ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy+ (side_tendency_100 | vp), 
                                        data = data_100_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rSI_100_response)
summary(logistic_model_rSI_100_response)

lrtest(intercept_model_n_100_response,logistic_model_n_100_response, logistic_model_rI_100_response, logistic_model_rSI_100_response)
AIC(intercept_model_n_100_response,logistic_model_n_100_response, logistic_model_rI_100_response, logistic_model_rSI_100_response)
BIC(intercept_model_n_100_response,logistic_model_n_100_response, logistic_model_rI_100_response, logistic_model_rSI_100_response)


#300
detect_oultier_300_response <- glm(correct_response ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy, 
                                  data = data_directed_prior_second_half, family = binomial(link = "logit"))
tab_model(detect_oultier_300_response)
summary(detect_oultier_300_response)
data_300_response_without_outliers <- remove_outliers(data_directed_prior_second_half, detect_oultier_300_response, 3)

#building models for correct_response with increasing complexity
intercept_model_n_300_response <- glm(correct_response ~ 1, 
                                     data = data_300_response_without_outliers, family = binomial(link = "logit"))
tab_model(intercept_model_n_300_response)
summary(intercept_model_n_300_response)

logistic_model_n_300_response <- glm(correct_response ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy, 
                                    data = data_300_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_n_300_response)
summary(logistic_model_n_300_response)

logistic_model_rI_300_response <- glmer(correct_response ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy+ (1 | vp), 
                                       data = data_300_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rI_300_response)
summary(logistic_model_rI_300_response)

logistic_model_rSI_300_response <- glmer(correct_response ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy+ (side_tendency_300 | vp), 
                                        data = data_300_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rSI_300_response)
summary(logistic_model_rSI_300_response)

lrtest(intercept_model_n_300_response,logistic_model_n_300_response, logistic_model_rI_300_response, logistic_model_rSI_300_response)
AIC(intercept_model_n_300_response,logistic_model_n_300_response, logistic_model_rI_300_response, logistic_model_rSI_300_response)
BIC(intercept_model_n_300_response,logistic_model_n_300_response, logistic_model_rI_300_response, logistic_model_rSI_300_response)


#500
detect_oultier_500_response <- glm(correct_response ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy, 
                                  data = data_directed_prior_second_half, family = binomial(link = "logit"))
tab_model(detect_oultier_500_response)
summary(detect_oultier_500_response)
data_500_response_without_outliers <- remove_outliers(data_directed_prior_second_half, detect_oultier_500_response, 3)

#building models for correct_response with increasing complexity
intercept_model_n_500_response <- glm(correct_response ~ 1, 
                                     data = data_500_response_without_outliers, family = binomial(link = "logit"))
tab_model(intercept_model_n_500_response)
summary(intercept_model_n_500_response)

logistic_model_n_500_response <- glm(correct_response ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy, 
                                    data = data_500_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_n_500_response)
summary(logistic_model_n_500_response)

logistic_model_rI_500_response <- glmer(correct_response ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy+ (1 | vp), 
                                       data = data_500_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rI_500_response)
summary(logistic_model_rI_500_response)

logistic_model_rSI_500_response <- glmer(correct_response ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy+ (side_tendency_500 | vp), 
                                        data = data_500_response_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rSI_500_response)
summary(logistic_model_rSI_500_response)

lrtest(intercept_model_n_500_response,logistic_model_n_500_response, logistic_model_rI_500_response, logistic_model_rSI_500_response)
AIC(intercept_model_n_500_response,logistic_model_n_500_response, logistic_model_rI_500_response, logistic_model_rSI_500_response)
BIC(intercept_model_n_500_response,logistic_model_n_500_response, logistic_model_rI_500_response, logistic_model_rSI_500_response)

#hit_true_false
#-100
detect_oultier_minus_100_hit <- glm(hit_true_false ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy, 
                                    data = data_directed_prior_second_half, family = binomial(link = "logit"))
tab_model(detect_oultier_minus_100_hit)
summary(detect_oultier_minus_100_hit)
data_minus_100_hit_without_outliers <- remove_outliers(data_directed_prior_second_half, detect_oultier_minus_100_hit, 3)

#building models for hit_true_false with increasing complexity
intercept_model_n_minus_100_hit <- glm(hit_true_false ~ 1, 
                                       data = data_minus_100_hit_without_outliers, family = binomial(link = "logit"))
tab_model(intercept_model_n_minus_100_hit)
summary(intercept_model_n_minus_100_hit)

logistic_model_n_minus_100_hit <- glm(hit_true_false ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy, 
                                      data = data_minus_100_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_n_minus_100_hit)
summary(logistic_model_n_minus_100_hit)

logistic_model_rI_minus_100_hit <- glmer(hit_true_false ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy+ (1 | vp), 
                                         data = data_minus_100_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rI_minus_100_hit)
summary(logistic_model_rI_minus_100_hit)

logistic_model_rSI_minus_100_hit <- glmer(hit_true_false ~ side_tendency_minus_100 + condition_dummy + side_tendency_minus_100*condition_dummy+ (side_tendency_minus_100 | vp), 
                                          data = data_minus_100_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rSI_minus_100_hit)
summary(logistic_model_rSI_minus_100_hit)

lrtest(intercept_model_n_minus_100_hit,logistic_model_n_minus_100_hit, logistic_model_rI_minus_100_hit, logistic_model_rSI_minus_100_hit)
AIC(intercept_model_n_minus_100_hit,logistic_model_n_minus_100_hit, logistic_model_rI_minus_100_hit, logistic_model_rSI_minus_100_hit)
BIC(intercept_model_n_minus_100_hit,logistic_model_n_minus_100_hit, logistic_model_rI_minus_100_hit, logistic_model_rSI_minus_100_hit)


#100
detect_oultier_100_hit <- glm(hit_true_false ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy, 
                              data = data_directed_prior_second_half, family = binomial(link = "logit"))
tab_model(detect_oultier_100_hit)
summary(detect_oultier_100_hit)
data_100_hit_without_outliers <- remove_outliers(data_directed_prior_second_half, detect_oultier_100_hit, 3)

#building models for hit_true_false with increasing complexity
intercept_model_n_100_hit <- glm(hit_true_false ~ 1, 
                                 data = data_100_hit_without_outliers, family = binomial(link = "logit"))
tab_model(intercept_model_n_100_hit)
summary(intercept_model_n_100_hit)

logistic_model_n_100_hit <- glm(hit_true_false ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy, 
                                data = data_100_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_n_100_hit)
summary(logistic_model_n_100_hit)

logistic_model_rI_100_hit <- glmer(hit_true_false ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy+ (1 | vp), 
                                   data = data_100_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rI_100_hit)
summary(logistic_model_rI_100_hit)

logistic_model_rSI_100_hit <- glmer(hit_true_false ~ side_tendency_100 + condition_dummy + side_tendency_100*condition_dummy+ (side_tendency_100 | vp), 
                                    data = data_100_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rSI_100_hit)
summary(logistic_model_rSI_100_hit)

lrtest(intercept_model_n_100_hit,logistic_model_n_100_hit, logistic_model_rI_100_hit, logistic_model_rSI_100_hit)
AIC(intercept_model_n_100_hit,logistic_model_n_100_hit, logistic_model_rI_100_hit, logistic_model_rSI_100_hit)
BIC(intercept_model_n_100_hit,logistic_model_n_100_hit, logistic_model_rI_100_hit, logistic_model_rSI_100_hit)


#300
detect_oultier_300_hit <- glm(hit_true_false ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy, 
                              data = data_directed_prior_second_half, family = binomial(link = "logit"))
tab_model(detect_oultier_300_hit)
summary(detect_oultier_300_hit)
data_300_hit_without_outliers <- remove_outliers(data_directed_prior_second_half, detect_oultier_300_hit, 3)

#building models for hit_true_false with increasing complexity
intercept_model_n_300_hit <- glm(hit_true_false ~ 1, 
                                 data = data_300_hit_without_outliers, family = binomial(link = "logit"))
tab_model(intercept_model_n_300_hit)
summary(intercept_model_n_300_hit)

logistic_model_n_300_hit <- glm(hit_true_false ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy, 
                                data = data_300_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_n_300_hit)
summary(logistic_model_n_300_hit)

logistic_model_rI_300_hit <- glmer(hit_true_false ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy+ (1 | vp), 
                                   data = data_300_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rI_300_hit)
summary(logistic_model_rI_300_hit)

logistic_model_rSI_300_hit <- glmer(hit_true_false ~ side_tendency_300 + condition_dummy + side_tendency_300*condition_dummy+ (side_tendency_300 | vp), 
                                    data = data_300_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rSI_300_hit)
summary(logistic_model_rSI_300_hit)

lrtest(intercept_model_n_300_hit,logistic_model_n_300_hit, logistic_model_rI_300_hit, logistic_model_rSI_300_hit)
AIC(intercept_model_n_300_hit,logistic_model_n_300_hit, logistic_model_rI_300_hit, logistic_model_rSI_300_hit)
BIC(intercept_model_n_300_hit,logistic_model_n_300_hit, logistic_model_rI_300_hit, logistic_model_rSI_300_hit)


#500
detect_oultier_500_hit <- glm(hit_true_false ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy, 
                              data = data_directed_prior_second_half, family = binomial(link = "logit"))
tab_model(detect_oultier_500_hit)
summary(detect_oultier_500_hit)
data_500_hit_without_outliers <- remove_outliers(data_directed_prior_second_half, detect_oultier_500_hit, 3)

#building models for hit_true_false with increasing complexity
intercept_model_n_500_hit <- glm(hit_true_false ~ 1, 
                                 data = data_500_hit_without_outliers, family = binomial(link = "logit"))
tab_model(intercept_model_n_500_hit)
summary(intercept_model_n_500_hit)

logistic_model_n_500_hit <- glm(hit_true_false ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy, 
                                data = data_500_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_n_500_hit)
summary(logistic_model_n_500_hit)

logistic_model_rI_500_hit <- glmer(hit_true_false ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy+ (1 | vp), 
                                   data = data_500_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rI_500_hit)
summary(logistic_model_rI_500_hit)

logistic_model_rSI_500_hit <- glmer(hit_true_false ~ side_tendency_500 + condition_dummy + side_tendency_500*condition_dummy+ (side_tendency_500 | vp), 
                                    data = data_500_hit_without_outliers, family = binomial(link = "logit"))
tab_model(logistic_model_rSI_500_hit)
summary(logistic_model_rSI_500_hit)

lrtest(intercept_model_n_500_hit,logistic_model_n_500_hit, logistic_model_rI_500_hit, logistic_model_rSI_500_hit)
AIC(intercept_model_n_500_hit,logistic_model_n_500_hit, logistic_model_rI_500_hit, logistic_model_rSI_500_hit)
BIC(intercept_model_n_500_hit,logistic_model_n_500_hit, logistic_model_rI_500_hit, logistic_model_rSI_500_hit)


library(ggplot2)
library(caTools)

library(RColorBrewer)


data=read.csv("D:\\PG_SEM_II\\Advance Statistics\\CA II\\Code\\data.csv")
data


#Data understanding-----------------
head(data)
tail(data)

#null values



table(data,is.na)

lapply(data,is.numeric)

lapply(data,is.factor)       


#In profession columns make 

table(data$profession)


#Conversion to factor-----
#Conversion of columns to factor


is.factor(data$quality_of_sleep)

data$gender<-as.factor(data$gender)
data$quality_of_sleep<-as.factor(data$quality_of_sleep)
data$profession<-as.factor(data$profession)
data$sleep_duration<-as.factor(data$sleep_duration)
data$sleep_disorder<-as.factor(data$sleep_disorder)
data$exercise<-as.factor(data$exercise)
data$stress_levels<-as.factor(data$stress_levels)
data$Use.of.electronic.devices.before.bed<-as.factor(data$Use.of.electronic.devices.before.bed)
data$caffeine_before_sleeping<-as.factor(data$caffeine_before_sleeping)
data$frequency.of.caffeine.consumption.before.sleep<-as.factor(data$frequency.of.caffeine.consumption.before.sleep)
data$consistent_bedtime_routine<-as.factor(data$consistent_bedtime_routine)
data$dietary_habits<-as.factor(data$dietary_habits)
data$consumption_of_heavy_meals_close_to_bedtime<-as.factor(data$consumption_of_heavy_meals_close_to_bedtime)
data$regular_meal_schedule<-as.factor(data$regular_meal_schedule)
data$emotional_state_before_bedtime<-as.factor(data$emotional_state_before_bedtime)
data$work_schedule<-as.factor(data$work_schedule)
data$naps_during_the_day<-as.factor(data$naps_during_the_day)

lapply(data,is.factor)



#Data visualization-------------
#data understanding
str(data)

ggplot(data=data, aes(x = gender,fill) )+
  geom_bar() +
  labs(x = "Gender", y = "Frequency", title = "Gender Frequency")

ggplot(data, aes(x = gender,fill=gender)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 4) +
  xlab("Gender") +
  ylab("Frequency") +
  ggtitle("Gender Frequency")
#scale_fill_manual(values = c("Male" = "blue", "Female" = "orange"))

ggplot(data, aes(x = quality_of_sleep,fill=quality_of_sleep)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 4) +
  xlab("Quality of Sleep") +
  ylab("Frequency") +
  ggtitle("Quality of Sleep")


ggplot(data, aes(x = naps_during_the_day,fill=naps_during_the_day)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 4) +
  xlab("Naps") +
  ylab("Frequency") +
  ggtitle("Naps During the day")

ggplot(data, aes(x = profession,fill=profession)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 4) +
  xlab("Profession") +
  ylab("Frequency") +
  ggtitle("Profession")
#scale_fill_manual(values = c("student" = "blue", "working " = "orange"))

ggplot(data, aes(x = work_schedule,fill=work_schedule)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 4) +
  xlab("Working Time") +
  ylab("Frequency") +
  ggtitle("Working Time")

#  scale_fill_manual(values = c("student" = "blue", "working " = "orange"))

ggplot(data, aes(x = emotional_state_before_bedtime,fill=emotional_state_before_bedtime)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 4) +
  xlab("Emotional status") +
  ylab("Frequency") +
  ggtitle("Emotional State")

#plot remaing graphs


#Fisher test for quality of sleep with all other columns------------------------------------

# #chi squared test for independence 
# contable<-table(data$gender,data$quality_of_sleep)
# fisher_result<-fisher.test(contable)
# p_value_fisher <- fisher_result$p.value
# 
# # Set significance level
# alpha <- 0.05
# i<-"rohan"
# # Print the result and conclusion
# if (p_value_fisher < alpha) {
#   print("Reject the null hypothesis.")
#   print(paset("There is a significant association between quality of sleep and",i,"."))
# } else {
#   print("Fail to reject the null hypothesis.")
#   print(paste("There is no significant association between quality of sleep and ",i,"."))
# }

#H0:There is no association between the quality of sleep and the specific variable being tested.
#H1:

colnames(data)

for(i in colnames(data)) {
  contable<-table(data$quality_of_sleep,data[[i]])
  fisher_result<-fisher.test(contable,simulate.p.value = TRUE, B = 1000)
  p_value_fisher <- fisher_result$p.value
  if (p_value_fisher < 0.05) {
    cat("Reject the null hypothesis.")
    cat(paste("There is a significant association between quality of sleep and",i,"."),"\n")
  } else {
    cat("Fail to reject the null hypothesis.")
    cat(paste("There is no significant association between quality of sleep and ",i,"\n"))}}

#Fisher test for duration of sleep with all other columns------------------------------------

for(i in colnames(data)) {
  contable<-table(data$sleep_duration,data[[i]])
  fisher_result<-fisher.test(contable,simulate.p.value = TRUE, B = 1000)
  p_value_fisher <- fisher_result$p.value
  if (p_value_fisher < 0.05) {
    cat("Reject the null hypothesis.")
    cat(paste("There is a significant association between Duration of sleep and",i,"."),"\n")
  } else {
    cat("Fail to reject the null hypothesis.")
    cat(paste("There is no significant association between Duration of sleep and ",i,"\n"))}}




#Shapiro test for age-------------------------------------------------------------------------
#H0:The data is normally distributed
#H1:The data is not normally distributed
shapiro_test_result <- shapiro.test(data$age)
print(shapiro_test_result)
p_value <- shapiro_test_result$p.value
alpha <- 0.05
# Interpretation and Conclusion
if (p_value < alpha) {
  cat("Shapiro-Wilk Test Result: Reject the null hypothesis.\n")
  cat("Conclusion: The data is not normally distributed.\n")
} else {
  cat("Shapiro-Wilk Test Result: Fail to reject the null hypothesis.\n")
  cat("Conclusion: The data is normally distributed.\n")
}
#age does not follow normal distribustion

#Convert sleep duration to numeric-------------------------------
#the target variable is sleep duration so convert it into numeric 

custom_mapping <- c(
  "1-3 hours" = 3,
  "3-5 hours" = 5,
  "5-7 hours" = 6,
  "7+ hours" = 7
)

# Apply label encoding
data$Numeric_Sleep_duration <- custom_mapping[data$sleep_duration]
data$Numeric_Sleep_duration

#Shapiro test for sleep duration------------------------------------

#H0:The data is normally distributed
#H1:The data is not normally distributed
# Perform Shapiro-Wilk test
shapiro_test_result <- shapiro.test(data$Numeric_Sleep_duration)
print(shapiro_test_result)
# Extract p-value
p_value <- shapiro_test_result$p.value
# Interpretation and Conclusion
if (p_value < 0.05) {
  #reject null hypothesis
  cat("Shapiro-Wilk Test Result: Reject the null hypothesis.\n")
  cat("Conclusion: The data is not normally distributed.\n")
} else {
  #accept null hypothesis
  cat("Shapiro-Wilk Test Result: Fail to reject the null hypothesis.\n")
  cat("Conclusion: The data does is normally distributed.\n")
}
#numeric_sleep does not follow normal distribution


#Fisher's test for sleep duration with all other columns-------------------------------------

for(i in colnames(data)) {
  contable<-table(data$Numeric_Sleep_duration,data[[i]])
  fisher_result <- fisher.test(contable, simulate.p.value = TRUE, B = 1000)
  p_value_fisher <- fisher_result$p.value
  
  # Set significance level
  alpha <- 0.05
  if (p_value_fisher < alpha) {
    cat("Reject the null hypothesis.")
    cat(paste("There is a significant association between quality of sleep and",i,"."),"\n")
  } else {
    cat("Fail to reject the null hypothesis.")
    cat(paste("There is no significant association between quality of sleep and ",i,"\n"))
  }
  
}

#Kruskal-Wallis test--------------------------------------------------------------------------------------------------
# Perform Kruskal-Wallis test
#H0:Median sleep duration for different types of stress level are equal
#H1:At least one stress level has a different median Numeric Sleep Duration.


kruskal_result <- kruskal.test(Numeric_Sleep_duration ~ stress_levels, data = data)
print(kruskal_result)
p_value_kruskal <- kruskal_result$p.value
if (p_value_kruskal < 0.05) {
  cat("Reject the null hypothesis.\n")
  cat("At least one stress level has a different median Numeric Sleep Duration.\n")
} else {
  cat("Fail to reject the null hypothesis.\n")
  cat("Median sleep duration for different types of stress level are equal.\n")
}

#POST HOC ANALYSIS FOR KRUSKAL WALLIS TEST
#install.packages("dunn.test")  # Install if not already installed
# library(dunn.test)
# dunn_result <- dunn.test(Numeric_Sleep_duration~ stress_levels,data=data ,method = "bh")
# print(dunn_result)


kruskal_result <- kruskal.test(Numeric_Sleep_duration ~ quality_of_sleep, data = data)
print(kruskal_result)
p_value_kruskal <- kruskal_result$p.value
if (p_value_kruskal < 0.05) {
  cat("Reject the null hypothesis.\n")
  cat("At least one stress level has a different median Numeric Sleep Duration.\n")
} else {
  cat("Fail to reject the null hypothesis.\n")
  cat("Median sleep duration for different types of stress level are equal.\n")
}

#Man whitney u test--------------------------------------------------------
#Man whitney u test
#H0:There is no difference in Sleep Duration between males and females.
#H1:There is a difference in Sleep Duration between males and females.
# Assuming sleep_duration is the Numeric Sleep Duration column and gender is the Gender column
wilcox_test_result <- wilcox.test(data$Numeric_Sleep_duration,as.numeric(data$gender))

# Print the results
print(wilcox_test_result)

# Interpretation based on the p-value:
if (wilcox_test_result$p.value < 0.05) {
  cat("Reject the null hypothesis.\n")
  cat("There is a significant difference in Sleep Duration between males and females.\n")
} else {
  cat("Fail to reject the null hypothesis.\n")
  cat("There is no significant difference in Sleep Duration between males and females.\n")
}

#H0:There is no difference in Sleep Duration between people of different age groups.
#H1:There is a difference in Sleep Duration between people of different age groups.
# Assuming sleep_duration is the Numeric Sleep Duration column and gender is the Gender column
wilcox_test_result <- wilcox.test(data$Numeric_Sleep_duration,data$age)

# Print the results
print(wilcox_test_result)

# Interpretation based on the p-value:
if (wilcox_test_result$p.value < 0.05) {
  cat("Reject the null hypothesis.\n")
  cat("There is a significant difference in Sleep Duration between people of different age groups \n")
} else {
  cat("Fail to reject the null hypothesis.\n")
  cat("There is no significant difference in Sleep Duration between people of different age groups\n")
}










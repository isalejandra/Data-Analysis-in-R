## STEP 3: inferential analysis
# ###############################
#First, we structure the data
list_Q1Q5Q9Q10 <- survey
#To identify responses easier, we give each respondent an ID.
list_Q1Q5Q9Q10$Respondent_id <- 1:nrow(list_Q1Q5Q9Q10)


#Create Dummy Variables for technologies.
write_technologies <- function(given_survey){ 
  subset_english <- given_survey[given_survey$'Language choice' == "English", ]
  subset_german <- given_survey[given_survey$'Language choice' == "Deutsch", ]
  # Coding English 1,2,3,4,12,5,7,8,10,9
  technologies_list_english <- subset_english$Q5_EN
  technologies_list_english <- gsub("12", "Other", technologies_list_english)
  technologies_list_english <- gsub("10", "I do not know", technologies_list_english)
  technologies_list_english <- gsub("1", "Data Analytics (Big Data, Real-Time Data, etc.)", technologies_list_english) 
  technologies_list_english <- gsub("2", "Additive Manufacturing", technologies_list_english)
  technologies_list_english <- gsub("3", "Sensors and Actuators (including Robotics and Machinery)", technologies_list_english)
  technologies_list_english <- gsub("4", "Cyber-physical systems", technologies_list_english)
  technologies_list_english <- gsub("5", "Digital Twin", technologies_list_english)
  technologies_list_english <- gsub("7", "AI", technologies_list_english)
  technologies_list_english <- gsub("8", "Computer Aided X", technologies_list_english)
  technologies_list_english <- gsub("9", "none", technologies_list_english)
  #  Coding Deutsch 1,2,3,4, 8, 5,6, 7,9,10
  technologies_list_german <- subset_german$Q5_EN
  technologies_list_german <- gsub("10", "none", technologies_list_german ) 
  technologies_list_german <- gsub("8", "Other", technologies_list_german )
  technologies_list_german <- gsub("9", "I do not know", technologies_list_german )
  technologies_list_german <- gsub("1", "Data Analytics (Big Data, Real-Time Data, etc.)", technologies_list_german ) 
  technologies_list_german<- gsub("2", "Additive Manufacturing", technologies_list_german )
  technologies_list_german <- gsub("3", "Sensors and Actuators (including Robotics and Machinery)", technologies_list_german )
  technologies_list_german <- gsub("4", "Cyber-physical systems", technologies_list_german )
  technologies_list_german <- gsub("5", "Digital Twin", technologies_list_german )
  technologies_list_german <- gsub("6", "AI", technologies_list_german )
  technologies_list_german  <- gsub("7", "Computer Aided X", technologies_list_german )
  
  technologies_list <- c(technologies_list_english,technologies_list_german)
  return(technologies_list)
  
}
# We add the technology text to each respondent row and create the dummy variables
list_Q1Q5Q9Q10$Technology_Text <- write_technologies(list_Q1Q5Q9Q10)
list_Q1Q5Q9Q10$Data_Analytics_dummy <- 0
list_Q1Q5Q9Q10$Additive_Manufacturing_dummy <- 0
list_Q1Q5Q9Q10$Sensors_Actuators_dummy <- 0
list_Q1Q5Q9Q10$CPS_dummy <- 0
list_Q1Q5Q9Q10$Other_dummy <- 0
list_Q1Q5Q9Q10$Digital_Twin_dummy <- 0
list_Q1Q5Q9Q10$none_dummy <- 0
list_Q1Q5Q9Q10$I_do_not_know_dummy <- 0
list_Q1Q5Q9Q10$AI_dummy <- 0
list_Q1Q5Q9Q10$CAX_dummy <- 0
# We fill the dummy variables columns with the respective data,
# based on the information displayed in the technology text field
for(i in 1:nrow(list_Q1Q5Q9Q10)){
  list_Q1Q5Q9Q10$AI_dummy[i] <- as.numeric(grepl("AI", list_Q1Q5Q9Q10$Technology_Text[i],fixed=TRUE))
  list_Q1Q5Q9Q10$Data_Analytics_dummy[i] <- as.numeric(grepl("Data Analytics (Big Data, Real-Time Data, etc.)", list_Q1Q5Q9Q10$Technology_Text[i],fixed=TRUE))
  list_Q1Q5Q9Q10$Additive_Manufacturing_dummy[i] <- as.numeric(grepl("Additive Manufacturing", list_Q1Q5Q9Q10$Technology_Text[i],fixed=TRUE))
  list_Q1Q5Q9Q10$Sensors_Actuators_dummy[i] <- as.numeric(grepl("Sensors and Actuators (including Robotics and Machinery)", list_Q1Q5Q9Q10$Technology_Text[i],fixed=TRUE))
  list_Q1Q5Q9Q10$CPS_dummy[i] <- as.numeric(grepl("Cyber-physical systems", list_Q1Q5Q9Q10$Technology_Text[i],fixed=TRUE))
  list_Q1Q5Q9Q10$Other_dummy[i] <- as.numeric(grepl("Other", list_Q1Q5Q9Q10$Technology_Text[i],fixed=TRUE))
  list_Q1Q5Q9Q10$Digital_Twin_dummy[i] <- as.numeric(grepl("Digital Twin", list_Q1Q5Q9Q10$Technology_Text[i],fixed=TRUE))
  list_Q1Q5Q9Q10$I_do_not_know_dummy[i] <- as.numeric(grepl("I do not know", list_Q1Q5Q9Q10$Technology_Text[i],fixed=TRUE))
  list_Q1Q5Q9Q10$none_dummy[i] <- as.numeric(grepl("none", list_Q1Q5Q9Q10$Technology_Text[i],fixed=TRUE))
  list_Q1Q5Q9Q10$CAX_dummy[i] <- as.numeric(grepl("Computer Aided X", list_Q1Q5Q9Q10$Technology_Text[i],fixed=TRUE))
  
}

#We create and fill the data of the variable "is_Impact"
#For this variable, we add the coding for positive or not positive impact
#in ramp-up. Empty responses and "I do not know" responses are treated as "NAs"
for(i in 1:nrow(list_Q1Q5Q9Q10)){
  if(list_Q1Q5Q9Q10$Q9_EN[i] == "Yes, technologies have reduced it."){
    list_Q1Q5Q9Q10$is_Impact[i] <- 1
  } else if (list_Q1Q5Q9Q10$Q9_EN[i] == "I do not know" |list_Q1Q5Q9Q10$Q9_EN[i] == "" ){
    list_Q1Q5Q9Q10$is_Impact[i] <- NA
  } else {
    list_Q1Q5Q9Q10$is_Impact[i] <- 0
  }
  if(list_Q1Q5Q9Q10$Q9_EN[i] == "" ){
    list_Q1Q5Q9Q10$Q9_EN[i] <- NA
  }
}
View(list_Q1Q5Q9Q10)

#We write the structured table in a .csv file to read it in JASP
fwrite(list_Q1Q5Q9Q10, "output_data/survey_inferential.csv")
####

# ###############################
#Inferential Statistics: correlation between Industry and Technologies (Q1 & Q5) with Cross Tabs
# We create functions to structure the data as contigency tables and apply it for each technology
fisher_test_industry_vs <- function(dummy){
  
  list_tech_industries <- data.frame(Technology = as.factor(dummy), 
                                     Industries = as.factor(list_Q1Q5Q9Q10$Q1_EN))
  View(list_tech_industries)
  contingency_table <- table(list_tech_industries)
  View(contingency_table)
  addmargins(contingency_table)
  # We perform the Fischer test to check the relationship
  fisher.test(contingency_table)

}  #Fisher Test function  including "Other" industries
fisher_test_industry_without_other <- function(dummy){
  list_without_other_industry <- list_Q1Q5Q9Q10[Q1_EN != "Other"]
  list_tech_industries <- data.frame(Technology = as.factor(dummy), 
                                     Industries = as.factor(list_without_other_industry$Q1_EN))
  View(list_tech_industries)
  contingency_table <- table(list_tech_industries)
  View(contingency_table)
  addmargins(contingency_table)
  # We perform the Fischer test to check the relationship
  fisher.test(contingency_table)
  
}  #Fisher Test function omitting "Other" industries
fisher_test_simulated_industry_vs <- function(dummy){ # Fisher Test with simulated p value function
  
  list_tech_industries <- data.frame(Technology = as.factor(dummy), 
                                     Industries = as.factor(list_Q1Q5Q9Q10$Q1_EN))
  View(list_tech_industries)
  contingency_table <- table(list_tech_industries)
  View(contingency_table)
  addmargins(contingency_table)
  # We perform the Fischer test to check the relationship
  fisher.test(contingency_table, simulate.p.value = TRUE)

}
#We test each dummy against the industries

fisher_test_industry_vs(list_Q1Q5Q9Q10$Additive_Manufacturing_dummy)

fisher_test_industry_vs(list_Q1Q5Q9Q10$AI_dummy)

fisher_test_industry_vs(list_Q1Q5Q9Q10$CAX_dummy)

fisher_test_industry_vs(list_Q1Q5Q9Q10$CPS_dummy)

fisher_test_industry_vs(list_Q1Q5Q9Q10$Data_Analytics_dummy)

fisher_test_industry_vs(list_Q1Q5Q9Q10$Digital_Twin_dummy)

fisher_test_industry_vs(list_Q1Q5Q9Q10$I_do_not_know_dummy)

fisher_test_industry_vs(list_Q1Q5Q9Q10$none_dummy)

fisher_test_industry_vs(list_Q1Q5Q9Q10$Other_dummy)

fisher_test_industry_vs(list_Q1Q5Q9Q10$Sensors_Actuators_dummy) #significant result
# we test Sensors and Actuators omitting the "Other" industry
list_without_other_industry <- list_Q1Q5Q9Q10[Q1_EN != "Other"]
fisher_test_industry_without_other(list_without_other_industry$Sensors_Actuators_dummy)

# Correlation between Industry(Q1) and ramp up reduction (Q9) with Cross Tabs
set.seed(300)
fisher_test_simulated_industry_vs(list_Q1Q5Q9Q10$Q9_EN)

# Correlation between each technology dummy and ramp up reduction Q9 with Cross Tabs
# Functions to structure data and apply the Fisher test
fisher_test_reduction_vs_technology <- function(dummy){
  
  list_tech_industries <- data.frame(Technology = as.factor(dummy), 
                                     Industries = as.factor(list_Q1Q5Q9Q10$Q9_EN))
  View(list_tech_industries)
  contingency_table <- table(list_tech_industries)
  View(contingency_table)
  addmargins(contingency_table)
  fisher.test(contingency_table)
  
} #Fisher Test function

fisher_test_reduction_vs_technology(list_Q1Q5Q9Q10$Additive_Manufacturing_dummy)

fisher_test_reduction_vs_technology(list_Q1Q5Q9Q10$AI_dummy)

fisher_test_reduction_vs_technology(list_Q1Q5Q9Q10$CAX_dummy)

fisher_test_reduction_vs_technology(list_Q1Q5Q9Q10$CPS_dummy)

fisher_test_reduction_vs_technology(list_Q1Q5Q9Q10$Data_Analytics_dummy)

fisher_test_reduction_vs_technology(list_Q1Q5Q9Q10$Digital_Twin_dummy)

fisher_test_reduction_vs_technology(list_Q1Q5Q9Q10$I_do_not_know_dummy)

fisher_test_reduction_vs_technology(list_Q1Q5Q9Q10$none_dummy)

fisher_test_reduction_vs_technology(list_Q1Q5Q9Q10$Other_dummy) # significant

fisher_test_reduction_vs_technology(list_Q1Q5Q9Q10$Sensors_Actuators_dummy)

# Correlation between each technology dummy and "is_Impact" variable with Cross Tabs
# Functions to structure data and apply the Fisher test
fisher_test_is_Impact_vs_technology <- function(dummy){
  
  list_tech_industries <- data.frame(Technology = as.factor(dummy), 
                                     Industries = as.factor(list_Q1Q5Q9Q10$is_Impact))
  View(list_tech_industries)
  contingency_table <- table(list_tech_industries)
  View(contingency_table)
  addmargins(contingency_table)
  fisher.test(contingency_table)
  
}

fisher_test_is_Impact_vs_technology(list_Q1Q5Q9Q10$Additive_Manufacturing_dummy)

fisher_test_is_Impact_vs_technology(list_Q1Q5Q9Q10$AI_dummy)

fisher_test_is_Impact_vs_technology(list_Q1Q5Q9Q10$CAX_dummy)

fisher_test_is_Impact_vs_technology(list_Q1Q5Q9Q10$CPS_dummy)

fisher_test_is_Impact_vs_technology(list_Q1Q5Q9Q10$Data_Analytics_dummy)

fisher_test_is_Impact_vs_technology(list_Q1Q5Q9Q10$Digital_Twin_dummy)

fisher_test_is_Impact_vs_technology(list_Q1Q5Q9Q10$I_do_not_know_dummy)

fisher_test_is_Impact_vs_technology(list_Q1Q5Q9Q10$none_dummy)

fisher_test_is_Impact_vs_technology(list_Q1Q5Q9Q10$Other_dummy)

fisher_test_is_Impact_vs_technology(list_Q1Q5Q9Q10$Sensors_Actuators_dummy)


# Z tests to check if the proportion of “success” is more than 50, which would mean that 
# the usage of technology has an statistically significant impact on the ramp-up
success <- list_Q1Q5Q9Q10[is_Impact == 1]
success <- sum(success$is_Impact)
success
total_number_observations <- nrow(list_Q1Q5Q9Q10[!is.na(is_Impact)])
total_number_observations
p0 <- 0.5 #hypothesized proportion under null hypothesis
prop_test_result <- prop.test(success, total_number_observations, p = p0, alternative = "greater")
prop_test_result
#we calculate the power for this test, where the sample is 61 respondents

pwr.p.test(h = "small", n = total_number_observations, sig.level= 0.05, alternative = "greater") 
pwr.p.test(h = "medium", n = total_number_observations, sig.level= 0.05, alternative = "greater") 
pwr.p.test(h = "large", n = total_number_observations, sig.level= 0.05, alternative = "greater") 

#We count and display the number of responses showing positive impact in ramp-up (is_Impact == 1) 
# per technology
filtered_Data_Analytics <- subset(list_Q1Q5Q9Q10, Data_Analytics_dummy == 1 & !is.na(is_Impact))
filtered_Additive_Manufacturing <- subset(list_Q1Q5Q9Q10, Additive_Manufacturing_dummy == 1 & !is.na(is_Impact) )
filtered_Sensors_Actuators <- subset(list_Q1Q5Q9Q10, Sensors_Actuators_dummy == 1 & !is.na(is_Impact))
filtered_CPS <- subset(list_Q1Q5Q9Q10, CPS_dummy == 1 & !is.na(is_Impact))
filtered_Other_technology <- subset(list_Q1Q5Q9Q10, Other_dummy == 1 & !is.na(is_Impact) )
filtered_Digital_Twin <- subset(list_Q1Q5Q9Q10, Digital_Twin_dummy == 1 & !is.na(is_Impact))
filtered_AI <- subset(list_Q1Q5Q9Q10, AI_dummy == 1 & !is.na(is_Impact))
filtered_CAX <- subset(list_Q1Q5Q9Q10, CAX_dummy == 1 & !is.na(is_Impact))
positive_impact_per_technology <- data.frame(
  Technology = c("Data Analytics", "Additive Manufacturing", "Sensors and Actuators", "CPS", "Other Technology", "Digital Twin", "AI","CAX"),
  Number_of_positive_responses_Impact_Ramp_up = c(sum(filtered_Data_Analytics$is_Impact),
                                                  sum(filtered_Additive_Manufacturing$is_Impact),
                                                  sum(filtered_Sensors_Actuators$is_Impact),
                                                  sum(filtered_CPS$is_Impact),
                                                  sum(filtered_Other_technology$is_Impact),
                                                  sum(filtered_Digital_Twin$is_Impact),
                                                  sum(filtered_AI$is_Impact),
                                                  sum(filtered_CAX$is_Impact)),
  Total_number_responses_impact_ramp_up = c(nrow(filtered_Data_Analytics),
                                            nrow(filtered_Additive_Manufacturing),
                                            nrow(filtered_Sensors_Actuators),
                                            nrow(filtered_CPS),
                                            nrow(filtered_Other_technology),
                                            nrow(filtered_Digital_Twin),
                                            nrow(filtered_AI),
                                            nrow(filtered_CAX)),
  Positive_Impact_Ramp_up_ratio = c(sum(filtered_Data_Analytics$is_Impact)*100/nrow(filtered_Data_Analytics),
                                    sum(filtered_Additive_Manufacturing$is_Impact)*100/nrow(filtered_Additive_Manufacturing),
                                    sum(filtered_Sensors_Actuators$is_Impact)*100/nrow(filtered_Sensors_Actuators),
                                    sum(filtered_CPS$is_Impact)*100/nrow(filtered_CPS),
                                    sum(filtered_Other_technology$is_Impact)*100/nrow(filtered_Other_technology),
                                    sum(filtered_Digital_Twin$is_Impact)*100/nrow(filtered_Digital_Twin),
                                    sum(filtered_AI$is_Impact)*100/nrow(filtered_AI),
                                    sum(filtered_CAX$is_Impact)*100/nrow(filtered_CAX))
)
positive_impact_per_technology_round_ratio <- sprintf("%.2f%%",positive_impact_per_technology$Positive_Impact_Ramp_up_ratio)
View(positive_impact_per_technology)
# Percentage of positive impact in ramp-up per technology, considering the total number of cases,
#where it is employed (dummy = 1) and is_Impact is not NA.
pie_chart_neto_positive_impact_rampup_technology <- ggplot(positive_impact_per_technology, aes(x = Number_of_positive_responses_Impact_Ramp_up , y = Technology, fill = Technology)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none")
pie_chart_neto_positive_impact_rampup_technology
pie_chart_relative_positive_impact_rampup_technology <- ggplot(positive_impact_per_technology, aes(x = Positive_Impact_Ramp_up_ratio , y = Technology, fill = Technology)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = positive_impact_per_technology_round_ratio),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  theme(legend.position = "none")
pie_chart_relative_positive_impact_rampup_technology
 

# #Random Forests 
# ###############

# The random forests will be evaluated with testing data.
#The following function structures input data in a simplified form for the random
# forest to work with.

convert_test_data_to_format <- function(data_survey){
  Random_forest_subset <- data.table( Industry = as.factor(data_survey$Q1_EN),
                                      Products = as.factor(data_survey$Q3_EN),
                                      Customers = as.factor(data_survey$Q4_EN),
                                      Frequency_Product_Release = data_survey$Q6_EN_5,
                                      Time_Ramp_Up = data_survey$Q7_EN_4,
                                      Time_to_Market = data_survey$Q8_EN_4,
                                      Amount_Reduction_Ramp_Up = data_survey$Q10_EN_1,
                                      Data_Analytics_Usage = as.factor(data_survey$Data_Analytics_dummy),
                                      Additive_Manufacturing_Usage = as.factor(data_survey$Additive_Manufacturing_dummy),
                                      Sensors_Actuators_Usage = as.factor(data_survey$Sensors_Actuators_dummy),
                                      CPS_Usage = as.factor(data_survey$CPS_dummy),
                                      Other_Usage = as.factor(data_survey$Other_dummy),
                                      AI_Usage = as.factor(data_survey$AI_dummy),
                                      CAX_Usage = as.factor(data_survey$CAX_dummy),
                                      is_Impact = as.factor(data_survey$is_Impact)
  )
  
  return(Random_forest_subset)
}  

#We impute the NAs with the method "missForest". The following function uses
# this method to fill the NAs in data.
impute_nas <-function(data_to_impute){
  set.seed(500)
  Random_forest_subset_imputed <- data.table( Industry = missForest(data_to_impute)$ximp$Industry,
                                              Products = missForest(data_to_impute)$ximp$Products,
                                              Customers = missForest(data_to_impute)$ximp$Customers,
                                              Frequency_Product_Release = missForest(data_to_impute)$ximp$Frequency_Product_Release,
                                              Time_Ramp_Up = missForest(data_to_impute)$ximp$Time_Ramp_Up,
                                              Time_to_Market = missForest(data_to_impute)$ximp$Time_to_Market,
                                              Amount_Reduction_Ramp_Up =missForest(data_to_impute)$ximp$Amount_Reduction_Ramp_Up,
                                              Data_Analytics_Usage = missForest(data_to_impute)$ximp$Data_Analytics_Usage,
                                              Additive_Manufacturing_Usage = missForest(data_to_impute)$ximp$Additive_Manufacturing_Usage,
                                              Sensors_Actuators_Usage = missForest(data_to_impute)$ximp$Sensors_Actuators_Usage,
                                              CPS_Usage = missForest(data_to_impute)$ximp$CPS_Usage,
                                              Other_Usage = missForest(data_to_impute)$ximp$Other_Usage,
                                              AI_Usage =missForest(data_to_impute)$ximp$AI_Usage,
                                              CAX_Usage = missForest(data_to_impute)$ximp$CAX_Usage,
                                              is_Impact = missForest(data_to_impute)$ximp$is_Impact
  )
  return(Random_forest_subset_imputed)
}

# We import testing data. This data is already structured in the format of 
# the "survey" variable. The imported file contains both training and testing data
# and needs to be splitted.
Data_with_Testing_data <- fread("input_data/Testing_Data.csv")
Data_with_Testing_data_formatted <- convert_test_data_to_format(Data_with_Testing_data)
#we convert the training data to the format for random forest
Random_forest_subset <- convert_test_data_to_format(list_Q1Q5Q9Q10)
#We split the training data from the testing one.
Testing_data <- setdiff(Data_with_Testing_data_formatted,Random_forest_subset)
#Now there are two datasets: training data (called "Random_forest_subset")
# and testing data( called Testing_data)

#We impute the missing data. For doing so, we employ the full dataset that contains
# both training and testing data.
Random_forest_subset_imputed <- rbind(Random_forest_subset,Testing_data )
Random_forest_subset_imputed <- impute_nas(Random_forest_subset_imputed)

# Now we split the dataset with imputed data into training set (Random_forest_subset_imputed)
# and testing set (Testing_data_imputed)
Testing_data_imputed <- Random_forest_subset_imputed[1 + nrow(Random_forest_subset):nrow(Random_forest_subset_imputed),]
Random_forest_subset_imputed <- Random_forest_subset_imputed[1:nrow(Random_forest_subset),]

View(Random_forest_subset) # training set without imputed data
View(Data_with_Testing_data) # testing set without imputed data
View(Random_forest_subset_imputed) #training set with imputed data
View(Testing_data_imputed) # testing set with imputed data

#We create the subset of data for both random forests (classification and regression)
# for the subset of data for classification task, we delete the variable related to
# the amount of reduction in ramp-up
is_Impact_subset_rf_classification <- Random_forest_subset_imputed[,-c("Amount_Reduction_Ramp_Up")]

#for the subset of data for the regression task, we only consider the rows with the
# variable is_Impact = 1, since the question related to amount of reduction
# was displayed only to participants that indicated a reduction in ramp-up.
#We delete the variable is_Impact afterwards, since it is not meaningful for this
#variable.
Amount_Ramp_Up_subset_rf_regression <- Random_forest_subset_imputed[is_Impact == 1] ###
Amount_Ramp_Up_subset_rf_regression <- Random_forest_subset_imputed[,-c("is_Impact")]

#Random Forest Classification to predict if there is an impact in ramp-up times based on 
# other variables (technology used, industry, type of customers, etc ).
View(is_Impact_subset_rf_classification)
#We define the control parameters for the random forest. In this case, Validation
#or testing of the model is done with cross-validation technique. This technique splits
# the data in 14 subsets and evaluates the model on each subset.
# This process is repeated 4 times.
set.seed(500)
control <- trainControl(method ="repeatedcv", number = 14, repeats = 4, search = "random")
# Random forest algorithm checks 15 combination of hyper parameters (tuneLength = 15), 
#such as number of trees,terminal nodes per tree, etc. and identifies the combination
#that results in the highest model performance. The validation of the model is done
# with the previously defined "control" method.The evaluation metric is based on 
#accuracy.
rf_classifier_isImpact_rf <- train(is_Impact~.,is_Impact_subset_rf_classification, method="rf", metric= "Accuracy", tuneLength=15, trControl=control)
#Result of random forest algorithm for classification:
rf_classifier_isImpact_rf


print(rf_classifier_isImpact_rf)
rf_classifier_isImpact_rf$finalModel

# We can inspect the first decision tree of the forest.
first_tree_classification <- getTree(rf_classifier_isImpact_rf$finalModel, 1, labelVar = TRUE)
View(first_tree_classification)


second_tree_classification_r1 <- getTree(rf_classifier_isImpact_r1$finalModel, 2, labelVar = TRUE)
second_tree_classification_r1
View(first_tree_classification_r1)


# Random Forest for regression. We follow the same procedure as the classification model
set.seed(500)
rf_regression_Amount_Impact <- train(Amount_Reduction_Ramp_Up~.,Amount_Ramp_Up_subset_rf_regression, method="rf", tuneLength=15, trControl=control)
print(rf_regression_Amount_Impact)
plot(rf_regression_Amount_Impact)

rf_regression_Amount_Impact$metric
rf_regression_Amount_Impact$finalModel

# We can inspect the first decision tree of this model
first_tree_regression_r1 <- getTree(rf_regression_Amount_Impact$finalModel, 1, labelVar = TRUE)
View(first_tree_regression_r1)

#Now we use the random forest models to make prediction in testing data
Testing_data_imputed <- na.omit(Testing_data_imputed)  # what is it here?
Testing_data_imputed[,Impact_Predicted := predict(rf_classifier_isImpact_rf,Testing_data_imputed)]
Testing_data_imputed[,Reduction_Predicted := predict(rf_regression_Amount_Impact,Testing_data_imputed)]
#We delete the imputed values of the variables "is_Impact" and "Amount_Reduction_Ramp_Up",
# since they were only relevant for training
Testing_data_imputed[,Amount_Reduction_Ramp_Up := Testing_data$Amount_Reduction_Ramp_Up]
Testing_data_imputed[,is_Impact := Testing_data$is_Impact]
View(Testing_data) #Original Testing Data
View(Testing_data_imputed) #Testing data with predictions and imputed variables

# We save the testing data file in an excel format
write.xlsx(Testing_data_imputed, file = "Tested_data.xlsx")

#We compare predictions and actual data of testing set with a confusion matrix.
# Here is important to clarify, that the random forest by default has considered the
# value is_Impact = 0 as "positive" class. Even though it does not change the 
# predictions of the random forest, it is necessary to specify that the "positive"
# class must be 1 for this analysis.
confusion_matrix_classification <- confusionMatrix(Testing_data_imputed$Impact_Predicted, Testing_data_imputed$is_Impact, positive = "1")
confusion_matrix_classification



#For evaluating the random forest for regression, we need to disregard the NA values
#of the variable "Amount Reduction in Ramp-up"
Testing_data_imputed_without_nas <- na.omit(Testing_data_imputed)
View(Testing_data_imputed_without_nas)

#For evaluating the predictions of the random forest for regression in the 
# testing data, we use Mean Absolute Error (MAE), Mean Squared Error(MSE)
# Root Mean Squared Error (RMSE) and R squared.
#MAE
mae <- mean(abs(Testing_data_imputed_without_nas$Reduction_Predicted - Testing_data_imputed_without_nas$Amount_Reduction_Ramp_Up))
#MSE
mse <- mean((Testing_data_imputed_without_nas$Reduction_Predicted - Testing_data_imputed_without_nas$Amount_Reduction_Ramp_Up)^2)
# RMSE
rmse <- sqrt(mse)
#R²
rsquared <- cor(Testing_data_imputed_without_nas$Reduction_Predicted, Testing_data_imputed_without_nas$Amount_Reduction_Ramp_Up)^2

mae
mse
rmse
rsquared

#Now, we check the importance of the variables in both models.
#First, we do it for the classification model
variables_importance_rf_classification <- varImp(rf_classifier_isImpact_rf,scale = FALSE)
plot(variables_importance_rf_classification)
variables_importance_rf_classification
# We continue with the regression model
variables_importance_rf_regression <- varImp(rf_regression_Amount_Impact,scale = FALSE)
plot(variables_importance_rf_regression)
variables_importance_rf_regression

#Now, we take the most importance variables resulted in the test performed before
# and we check the Partial Dependence plot (PDP) per variable to see how it affects
# the predictions of the model.
# For the classification model:
partial_plot_frequency_classification <- partial(rf_classifier_isImpact_rf, pred.var = "Frequency_Product_Release",prob = T)
partial_plot_time_ramp_up_classification <- partial(rf_classifier_isImpact_rf, pred.var = "Time_Ramp_Up",prob = T)
partial_plot_time_market_classification <- partial(rf_classifier_isImpact_rf, pred.var = "Time_to_Market",prob = T)
partial_plot_AI_classification <- partial(rf_classifier_isImpact_rf, pred.var = "AI_Usage", prob = T)
partial_plot_sensors_classification <- partial(rf_classifier_isImpact_rf, pred.var = "Sensors_Actuators_Usage", prob = T)
partial_plot_industry_classification <- partial(rf_classifier_isImpact_rf, pred.var = "Industry", prob = T)


# Since the model considered is_Impact = 0 as "positive" class, the yhat values
# of the PDP for the classification model show the probability of obtaining
# the default positive class. In other words, the probability of not resulting
# in a reduction of ramp-up (is_Impact = 0). Since, this plot is counterintuitive
# the PDP is modified to give the probabilities of obtaining (is_Impact = 1)
# So, the new yhat is "1 - yhat" of the default model.
partial_plot_frequency_classification$yhat <- 1 - partial_plot_frequency_classification$yhat
partial_plot_time_ramp_up_classification$yhat <- 1 - partial_plot_time_ramp_up_classification$yhat
partial_plot_time_market_classification$yhat <- 1 - partial_plot_time_market_classification$yhat
partial_plot_AI_classification$yhat <- 1 - partial_plot_AI_classification$yhat
partial_plot_sensors_classification$yhat <- 1 - partial_plot_sensors_classification$yhat
partial_plot_industry_classification$yhat <- 1 - partial_plot_industry_classification$yhat

#We change the names of the columns for readability
colnames(partial_plot_frequency_classification)[1] <- "Frequency of product release in months"

colnames(partial_plot_time_ramp_up_classification)[1] <- "Time to ramp-up in months"

colnames(partial_plot_time_market_classification)[1] <- "TTM in months"

colnames(partial_plot_AI_classification)[1] <- "AI usage"

colnames(partial_plot_sensors_classification)[1] <- "Sensors and actuators usage"


pdp_scatterplot_correlation(partial_plot_frequency_classification)
View(partial_plot_frequency_classification)

pdp_scatterplot_correlation(partial_plot_time_ramp_up_classification)
View(partial_plot_time_ramp_up_classification)

pdp_scatterplot_correlation(partial_plot_time_market_classification)
View(partial_plot_time_market_classification)

pdp_scatterplot_correlation(partial_plot_AI_classification)
View(partial_plot_AI_classification)

pdp_scatterplot_correlation(partial_plot_sensors_classification)
View(partial_plot_sensors_classification)

plot(partial_plot_industry_classification)
View(partial_plot_industry_classification)
# For the regression model:
partial_plot_time_ramp_up_regression <- partial(rf_regression_Amount_Impact, pred.var = "Time_Ramp_Up")
partial_plot_time_market_regression <- partial(rf_regression_Amount_Impact, pred.var = "Time_to_Market")
partial_plot_frequency_regression <- partial(rf_regression_Amount_Impact, pred.var = "Frequency_Product_Release")
partial_plot_industry_regression <- partial(rf_regression_Amount_Impact, pred.var = "Industry")
partial_plot_customer_regression <- partial(rf_regression_Amount_Impact, pred.var = "Customers")

colnames(partial_plot_time_ramp_up_regression)[1] <- "Time to ramp-up in months"
colnames(partial_plot_time_market_regression)[1] <- "Time to market in months"
colnames(partial_plot_frequency_regression)[1] <- "Frequency of product release in months"

pdp_scatterplot_regression(partial_plot_time_ramp_up_regression)
View(partial_plot_time_ramp_up_regression)

pdp_scatterplot_regression(partial_plot_time_market_regression)
View(partial_plot_time_market_regression)

pdp_scatterplot_regression(partial_plot_frequency_regression)
View(partial_plot_frequency_regression)

pdp_scatterplot_regression(partial_plot_industry_regression) 
View(partial_plot_industry_regression)

pdp_scatterplot_regression(partial_plot_customer_regression)
View(partial_plot_customer_regression)

#End Of Analysis
##################################

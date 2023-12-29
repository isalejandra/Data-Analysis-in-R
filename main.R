# clear plot pane and delete all previously saved data
# dev.off()
# rm(list = ls())

# define libraries used
library(ggplot2)
library(data.table)
library(magrittr)
library(tidyr)
library(epiDisplay)
library(MASS)   
library(ISLR)
library(tidyverse)
library(Rfast)
library(lme4)
library(randomForest)
library(caret)
library(missForest)
library(rpart)
library(openxlsx)
library(pdp)
library(pwr)

# import files
source("src/dataloader.R")
source('src/descriptive/tech_distributions.R')
source('src/descriptive/charts.R')
source('src/descriptive/time_metrics.R')


#Step 1: cleaning of data
survey <- load_data()

View(survey)
# summary(survey)

# # STEP 2: descriptive analysis
# ###############################

#Representation for Q1: we create a bar chart to see the distribution of the industries.
#We also print the table of counts.
survey %>% 
  ggplot(aes(x = Q1_EN, fill =  Q1_EN)) +
  geom_bar() +
  labs(y = "Frequency", x = "Distribution of Industries") +
  theme(legend.position = "none")
table(survey$Q1_EN)


#Representation for Q2: frequencies. Q2 refers to the years fo experience of respondents
decreasing_barchart(survey$Q2_EN)


#Representation for Q3: frequencies. Q3 refers to the type of product/service the respondent offers
decreasing_barchart(survey$Q3_EN)

#Representation for Q4: frequencies. Q4 refers to the type of customer of the respondent
decreasing_barchart(survey$Q4_EN)

#Representation for Q5: frequencies
#Q5 inquiries for the technologies employed in the company the respondent works for
# We find the distribution of used technologies for all industries and per industry
pie_chart_technology_freq(technology_distribution(survey))

# To inspect the specific values of the pie chart of one industry
table(technology_distribution_per_industry(survey,"Other"))#Only for "Other" Industry

# We display the distribution of used technologies for an specific industry with a pie chart
pie_chart_technology_freq(technology_distribution_per_industry(survey,"Advanced Electronics")) #Only for Advanced Electronics
pie_chart_technology_freq(technology_distribution_per_industry(survey,"Automotive\nand\nAerospace")) #Only for Automotive
pie_chart_technology_freq(technology_distribution_per_industry(survey,"Consumer\nGoods")) #Only for Consumer Goods
pie_chart_technology_freq(technology_distribution_per_industry(survey,"Healthcare (pharma\nand\nmedical devices)")) #Only for Healthcare
pie_chart_technology_freq(technology_distribution_per_industry(survey,"IT")) #Only for IT
pie_chart_technology_freq(technology_distribution_per_industry(survey,"Machinery Industry")) #Only for Machinery Industry
pie_chart_technology_freq(technology_distribution_per_industry(survey,"Retail")) #Only for Retail
pie_chart_technology_freq(technology_distribution_per_industry(survey,"Energy")) #Only for Energy
pie_chart_technology_freq(technology_distribution_per_industry(survey,"Other")) #Only for Other"
pie_chart_technology_freq(technology_distribution_per_industry(survey,"Construction\nEngineering and\nInfrastructure")) #Only for Construction Eng. and Infrastructure
#pie_chart_technology_freq(technology_distribution_per_industry(survey,"Chemicals and Advanced Materials")) #Only for Chemicals and Advanced Materials"



# Q6,Q7,Q8. time metrics for frequency of product introduction, ramp-up and time to market respectively.
# with each industry, we use a reusable function to draw a bar chart with the Frequency, TTM and Ramp-up.
timetable_barchart(survey,"Advanced Electronics")
TTM_time(survey,"Advanced Electronics")
Ramp_up_time(survey,"Advanced Electronics")
Frequency_time(survey,"Advanced Electronics")

timetable_barchart(survey,"Automotive\nand\nAerospace")
TTM_time(survey,"Automotive\nand\nAerospace")
Ramp_up_time(survey,"Automotive\nand\nAerospace")
Frequency_time(survey,"Automotive\nand\nAerospace")

timetable_barchart(survey,"Consumer\nGoods")
TTM_time(survey,"Consumer\nGoods")
Ramp_up_time(survey,"Consumer\nGoods")
Frequency_time(survey,"Consumer\nGoods")

timetable_barchart(survey,"Healthcare (pharma\nand\nmedical devices)")
TTM_time(survey,"Healthcare (pharma\nand\nmedical devices)")
Ramp_up_time(survey,"Healthcare (pharma\nand\nmedical devices)")
Frequency_time(survey,"Healthcare (pharma\nand\nmedical devices)")

timetable_barchart(survey,"IT")
TTM_time(survey,"IT")
Ramp_up_time(survey,"IT")
Frequency_time(survey,"IT")

timetable_barchart(survey,"Machinery Industry")
TTM_time(survey,"Machinery Industry")
Ramp_up_time(survey,"Machinery Industry")
Frequency_time(survey,"Machinery Industry")

timetable_barchart(survey,"Energy")
TTM_time(survey,"Energy")
Ramp_up_time(survey,"Energy")
Frequency_time(survey,"Energy")

timetable_barchart(survey,"Retail")
TTM_time(survey,"Retail")
Ramp_up_time(survey,"Retail")
Frequency_time(survey,"Retail")

timetable_barchart(survey,"Construction\nEngineering and\nInfrastructure")
TTM_time(survey,"Construction\nEngineering and\nInfrastructure")
Ramp_up_time(survey,"Construction\nEngineering and\nInfrastructure")
Frequency_time(survey,"Construction\nEngineering and\nInfrastructure")

timetable_barchart(survey,"Other")
TTM_time(survey,"Other")
Ramp_up_time(survey,"Other")
Frequency_time(survey,"Other")

timetable_barchart(survey,"Chemicals and Advanced Materials")

# Q7 Ramp-up time per industry  
Q7_rampup_time_industry <- survey[!is.na(Q7_EN_4) & Q7_EN_4 != "",]
Q7_rampup_time_industry <- aggregate(Q7_EN_4 ~ Q1_EN,Q7_rampup_time_industry,mean)
Q7_rampup_time_industry$Q7_EN_4 <- round(Q7_rampup_time_industry$Q7_EN_4, 2)
names(Q7_rampup_time_industry)[1] <- "Industry"
names(Q7_rampup_time_industry)[2] <- "Months_of_ramp_up"

pie_chart_mean_rampup_technology <- ggplot(Q7_rampup_time_industry, aes(x = Months_of_ramp_up , y = Industry, fill = Industry)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = Months_of_ramp_up),
             position = position_stack(vjust = 0.95),
             show.legend = FALSE) +
  coord_polar(theta = "y")+
  theme_void()
pie_chart_mean_rampup_technology

#Representation for Q9: frequencies.
# Q9 inquiries about the impact the technologies had on ramp-up time
Q9_delete_empty_entries <- survey[Q9_EN != "", ] # delete empty (NA) answers 
change_rampup_freq <- table(Q9_delete_empty_entries$Q9_EN)
change_rampup_freq<- as.data.table(change_rampup_freq)
change_rampup_freq
change_rampup_freq[, Percentage := paste0(round(N * 100 / sum(N), 2), "%")]
change_rampup_freq
labels <- change_rampup_freq$Percentage
pie_chart_Q9 <- ggplot(change_rampup_freq, aes(x = "", y = N, fill = V1)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  labs(fill = "Q9: would the use of the mentioned\ntechnologies have reduced the\ntime needed to achieve production\nefficiency for a new product? ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme_void()
pie_chart_Q9
#Industries with a positive impact of technologies on Ramp-up:

Q9_impacted_industries <- survey[survey$Q9_EN == "Yes, technologies have reduced it.", ] # filter positive responses
Q9_impacted_industries <- table(Q9_impacted_industries$Q1_EN)
Q9_impacted_industries<- as.data.table(Q9_impacted_industries)
Q9_impacted_industries[, Percentage := paste0(round(N * 100 / sum(N), 2), "%")]
Q9_impacted_industries
labels <- Q9_impacted_industries$Percentage
pie_chart_Impacted_industries <- ggplot(Q9_impacted_industries, aes(x = "", y = N, fill = V1)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.35),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  labs(fill = "Industry") + 
  labs(title = "Industries with positive impact in ramp-up") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_void() 
pie_chart_Impacted_industries

#Industries with a positive impact of technologies in Ramp-up. 
#Showing percentage of positive impact per industry considering the total number of responses in Q9
Q1_total_indutries <- survey[survey$Q9_EN != "", ] # delete empty responses in Q9
Q1_total_indutries <- table(Q1_total_indutries$Q1_EN)  # we create a table with the total number of responses per industries
Q1_total_indutries <- as.data.table(Q1_total_indutries)
names(Q1_total_indutries)[names(Q1_total_indutries) == "V1"] <- "Industry"
Q1_total_indutries
Q9_impacted_industries # we use the table with responses of positive impact on ramp-up created previously
names(Q9_impacted_industries)[names(Q9_impacted_industries) == "V1"] <- "Industry"
# We join both tables. "N.y" comes from Q9 and refers to the total number of responses per industry and
# "N.x" refers to the number of positive responses in Q9 per industry
Q9_impacted_industries <- merge(Q9_impacted_industries, Q1_total_indutries, by = "Industry", all = FALSE, sort = FALSE)
# we create the percentage of positive responses per industry
Q9_impacted_industries[, Percentage_per_industry := paste0(round(N.x * 100 /N.y, 2))] 
Q9_impacted_industries$Percentage_per_industry <- as.numeric(Q9_impacted_industries$Percentage_per_industry)
labels_Q9_impact_per_industry <- sprintf("%.2f%%",Q9_impacted_industries$Percentage_per_industry)
Q9_impacted_industries %>% 
  ggplot(aes(x = Percentage_per_industry,y= Industry, fill =  Industry)) +
  geom_bar(stat="identity") +
  geom_label(aes(label = labels_Q9_impact_per_industry),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  labs(x = "Percentage of responses suggesting positive\nimpact on ramp-up per industry", y = "Distribution of Industries") +
  theme(legend.position = "none")



#Mean reduction in ramp up per industry. Q10 inquiries about the months of reduction technologies cause in ramp-up
Q10_reduction_per_industry <- survey[!is.na(Q10_EN_1) & Q10_EN_1 != "",]  # we delete empty responses
Q10_reduction_per_industry <- aggregate(Q10_EN_1 ~ Q1_EN,Q10_reduction_per_industry,mean) # we find the mean reduction in ramp-up.
names(Q10_reduction_per_industry)[1] <- "Industry"
Q10_reduction_per_industry$Q10_EN_1 <- round(Q10_reduction_per_industry$Q10_EN_1, 2)
Q10_reduction_per_industry
#Bar chart of mean reduction per industry in months
pie_chart_mean_reduction_technology <- ggplot(Q10_reduction_per_industry, aes(x = Q10_EN_1 , y = Industry, fill = Industry)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = Q10_EN_1),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)  +
  labs(y = "Industry", x = "Average reduction in ramp-up time") +
  theme(legend.position = "none")

pie_chart_mean_reduction_technology

#Q10 reduction in ramp up per industry (as percentage of total ramp up)

Q10_reduction_per_industry
# we add a column with average months of ramp-up
Q10_reduction_per_industry <- merge(Q7_rampup_time_industry, Q10_reduction_per_industry, by = "Industry", all = FALSE, sort = FALSE) 
# we calculate "relative reduction" by calculating the percentage of reduction based on the average total
# total ramp-up time per industry.
Q10_reduction_per_industry$Relative_Reduction <-  (Q10_reduction_per_industry$Q10_EN_1/Q10_reduction_per_industry$Months_of_ramp_up)*100
labels_Q10 <- sprintf("%.2f%%",Q10_reduction_per_industry$Relative_Reduction)
Q10_reduction_per_industry$Relative_Reduction <- as.numeric(Q10_reduction_per_industry$Relative_Reduction)
Q10_reduction_per_industry
#Bar chart of percentage of reduction in ramp-up per industry
pie_chart_relative_rampup_technology <- ggplot(Q10_reduction_per_industry, aes(x = Relative_Reduction, y = Industry, fill = Industry)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = labels_Q10),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) 
pie_chart_relative_rampup_technology

#End Of Descriptive Analysis

## Step 3: inferential analysis
# ###############################
source('src/inferential/analysis.R')
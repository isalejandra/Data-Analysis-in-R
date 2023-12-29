# We create functions for finding the mean of time of the Frequency of product introduction, Ramp-up and Time to Market.
Frequency_time <- function(subset_survey,industry){
  subset_industry <- subset_survey[subset_survey$Q1_EN == industry]
  return(mean(subset_industry$Q6_EN_5,na.rm = TRUE))
}
  
Ramp_up_time <- function(subset_survey,industry){
  subset_industry <- subset_survey[subset_survey$Q1_EN == industry]
  return(mean(subset_industry$Q7_EN_4,na.rm = TRUE) )
}
TTM_time <- function(subset_survey,industry){
  subset_industry <- subset_survey[subset_survey$Q1_EN == industry]
  return(mean(subset_industry$Q8_EN_4,na.rm = TRUE))
}
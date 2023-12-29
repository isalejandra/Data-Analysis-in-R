technology_distribution <- function(given_survey){
  subset_english <- given_survey[`Language choice`%in%"English"]
  subset_german <- given_survey[`Language choice`%in%"Deutsch"]
  
  # Coding English 1,2,3,4,12,5,7,8,10,9
  technologies_english <- subset_english$Q5_EN
  technologies_english <- strsplit(technologies_english,",")
  technologies_list_english <- list(technologies_english)
  technologies_list_english <- unlist(technologies_list_english)
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
  
  #  Coding Deutsch 1,2,3,4,8,5,6,7,9,10
  technologies_german <- subset_german$Q5_EN
  technologies_german<- strsplit(technologies_german,",")
  technologies_list_german <- list(technologies_german)
  technologies_list_german <- unlist(technologies_list_german )
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
# Since each respondent can choose more than one technology, we create a list of all 
# chosen technologies for observing the distribution
technology_distribution_per_industry <- function(given_survey,given_industry){
  technology_list_industry <- technology_distribution(given_survey[Q1_EN  == given_industry])
  return(technology_list_industry)
}


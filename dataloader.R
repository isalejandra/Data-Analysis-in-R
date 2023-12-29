load_data <- function() { 
  #Import the data.table . Beginning of data cleaning
  survey_data_text <- fread("input_data/survey-data-textual.csv")
  survey_data_numeric <- fread("input_data/survey-data-numeric.csv")
  survey_data_text[, Q5_EN := survey_data_numeric$Q5_EN]
  survey_data_text[, Q5_DE := survey_data_numeric$Q5_DE]


  # Create two tables with responses in English and German
  survey_data_english <- survey_data_text[`Language choice`%in%"English"]
  survey_data_german <- survey_data_text[`Language choice`%in%"Deutsch"]
  survey_data_english <- survey_data_english[,18:31]
  survey_data_german <- survey_data_german[,c(18,32:44)]
 
  
  #Join both versions of the survey
  survey <- rbindlist(list(survey_data_english, survey_data_german))
  #Q6,Q7,Q8 and Q10 data must be type ratio. "Q" stands for "Question"
  survey$Q6_EN_5 <- as.numeric(survey$Q6_EN_5)
  survey$Q7_EN_4 <- as.numeric(survey$Q7_EN_4)
  survey$Q8_EN_4 <- as.numeric(survey$Q8_EN_4)
  survey$Q10_EN_1 <- as.numeric(survey$Q10_EN_1)
  #remove responses of people who are not working, that are empty, or were "test" responses
  #translate responses in German to English
  survey <- survey[Q1_EN != "I am not working" & Q1_EN != "Ich bin derzeit nicht beschäftigt"]
  survey[Q1_EN == "Elektronik", Q1_EN := "Advanced Electronics"]
  survey[Q1_EN == "Chemieindustrie und Werkstoffe", Q1_EN := "Chemicals and Advanced Materials"]
  survey[Q1_EN == "Bauingenieurwesen und Infrastruktur" |Q1_EN == "Construction Engineering and Infrastructure", Q1_EN := "Construction\nEngineering and\nInfrastructure"]
  survey[Q1_EN == "Maschinenbau", Q1_EN := "Machinery Industry"]
  survey[Q1_EN == "Gesundheitswessen (Pharma und Medizintechnik)" | Q1_EN == "Healthcare (pharma and medical devices)", Q1_EN := "Healthcare (pharma\nand\nmedical devices)"]
  survey[Q1_EN == "Automotive und Luft und Raumfahrt" | Q1_EN == "Automotive and Aerospace", Q1_EN := "Automotive\nand\nAerospace"]
  survey[Q1_EN == "Konsumgüter (wie Kleidung, Lebensmittel)"| Q1_EN == "Consumer Goods (such as clothes, food, and beverages)", Q1_EN := "Consumer\nGoods"]
  survey[Q1_EN == "Händler"|Q1_EN == "Haendler",Q1_EN := "Retail"]
  survey[Q1_EN == "Andere Branche. Folgende:" | Q1_EN == "Other. Which one?" , Q1_EN := "Other"]
  survey[Q1_EN_14_TEXT == "Energie" | Q1_EN_14_TEXT == "Energiewirtschaft", Q1_EN := "Energy"]
  survey[Q2_EN == "weniger als 1 Jahr", Q2_EN := "Less than 1 year"]
  survey[Q2_EN == "1 bis 2 Jahre", Q2_EN := "Between 1 to 2 years"]
  survey[Q2_EN == "2 bis 5 Jahre", Q2_EN := "Between 2 to 5 years"]
  survey[Q2_EN == "Mehr als 5 Jahre", Q2_EN := "More than 5 years"]
  survey[Q3_EN == "Dienstleistungen", Q3_EN := "Services"]
  survey[Q3_EN == "kundenspezifische Produkte (Produkte, die speziell für bestimmte Kunden entwickelt wurden)", Q3_EN := "Customized Products (Customer Tailored Products)"]
  survey[Q3_EN == "standardisierte Produkte (nicht kundenspezifisch)", Q3_EN := "Standardized Products (not Customized)"]
  survey[Q4_EN == "Privatpersonen (B2C)", Q4_EN := "People (B2C)"]
  survey[Q4_EN == "andere Unternehmen (B2B)", Q4_EN := "Companies (B2B)"]
  survey[Q4_EN == "Privatpersonen und andere Unternehmen", Q4_EN := "Both"]
  survey[Q9_EN == "Nein, die Zeit zur Produktionseffizienz blieb ungefähr gleich", Q9_EN := "No, it has stayed about the same."]
  survey[Q9_EN == "Nein, die Zeit zur Produktionseffizienz hat sich erhöht", Q9_EN := "No, it has increased."]
  survey[Q9_EN == "Ja, die Technologien haben sie verkürzt.", Q9_EN := "Yes, technologies have reduced it."]
  survey[Q9_EN == "Ich weiß nicht", Q9_EN := "I do not know"]
  survey[Q9_EN == "Nein, die Zeit wurde wegen anderer Ursachen verkürzt. Bitte nennen sie die Ursachen:"|Q9_EN == "No, time has decreased, but not because of the technologies, but for other reasons. Please state them here:", Q9_EN := "No, time has decreased\nbecause of other reasons"] 
  survey <- survey[Q1_EN != ""]
  survey <- survey[Q1_EN_14_TEXT != "Test"]
  return(survey)
}
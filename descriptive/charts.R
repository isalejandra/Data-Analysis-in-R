decreasing_barchart <- function(data) {
  return(tab1(data, sort.group = "decreasing", cum.percent = TRUE))
}

pie_chart_technology_freq <- function(tech_distribution) {
  technologies_freq <- table(tech_distribution)
  technologies_freq <- as.data.table(technologies_freq)
  technologies_freq[, Percentage := paste0(round(N * 100 / sum(N), 2), "%")]
  labels <- technologies_freq$Percentage
  pie_chart <- ggplot(technologies_freq, aes(x = "", y = N, fill = tech_distribution)) +
    geom_bar(stat = "identity") +
    geom_label(aes(label = labels),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE) +
    coord_polar(theta = "y") +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(fill = "Technologies") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme_void()
  return(pie_chart)
}

timetable_barchart <- function(survey,industry){
  Frequency_time <- Frequency_time(survey,industry)
  Ramp_up_time <- Ramp_up_time(survey,industry)
  TTM_time <- TTM_time(survey,industry)
  stages <- c("Time to Market", "Ramp-up Time", "Frequency Product Introduction")
  durations <- c(TTM_time,Ramp_up_time, Frequency_time)  # Duration in months
  data <- data.frame(Stage = stages, Months = durations)
  ggp <- ggplot(data, aes(Stage, Months, fill = Stage)) +   geom_bar(stat = "identity") + coord_flip()
  ggp 
}
pdp_scatterplot_correlation <- function(data_table){
  x_axis <- data_table[, 1]
  ggplot(data_table, aes(x= x_axis , y=yhat)) +
    geom_point() +
    ylab("Probability of positive impact on ramp-up")+
    xlab(colnames(data_table)[1])
  
}
pdp_scatterplot_regression <- function(data_table){
  x_axis <- data_table[, 1]
  ggplot(data_table, aes(x=x_axis, y=yhat)) +
    geom_point() +
    ylab("Change in reduction of ramp-up in months")+
    xlab(colnames(data_table)[1])
    
  
}

    
box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data,

)

data_choice_measures <- function(filter){
  if(filter=="Alle"){
    data <- import_data$data

  } else {
    data <- subset(import_data$data, identity==filter)
  }
  data_choice_measures <- as.data.frame(table(unlist(data$haveYouReportedYes)))
  data_choice_measures <- data_choice_measures %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))

  return(data_choice_measures)
}

data_choice_measures1 <- function(filter){
  if(filter=="Alle"){
    data <- import_data$data

  } else {
    data <- subset(import_data$data, identity==filter)
  }
  return(data)
}

box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

data_gen <- function(filter){
  if(filter=="Alle"){
    data <- import_data$data

  } else {
    data <- subset(import_data$data, identity==filter)
  }
  data <- subset(data, gender!="NULL")
  data <- subset(data, gender!="list()")
  data_gen <- as.data.frame(table(unlist(data$gender)))
  data_gen <- data_gen %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
           pct1 = paste0(percentage, "%"))

  return(data_gen)
}


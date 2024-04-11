box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

data_loc <- function(filter){
  if(filter=="Alle"){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }

  data_loc <- as.data.frame(table(data$locationOnline))
  data_loc <- data_loc %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))

  return(data_loc)
}


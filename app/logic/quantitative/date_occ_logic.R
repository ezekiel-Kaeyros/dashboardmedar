box::use(
  dplyr[mutate, if_else, summarise, group_by, select, n, rename,inner_join, filter],
  magrittr[`%>%`],stats
)

box::use(
  app/logic/import_data
)

data_temp <- function(filter){
  if(filter=="Alle"){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  data_temp <- data %>%
    dplyr::mutate(
      temporal_distance = if_else(is.na(valueDate), as.numeric(difftime(createdAt, endDate, units = "days")),
                                  as.numeric(difftime(createdAt, valueDate, units = "days")))#,
    ) %>%
    dplyr::mutate(
      temporal_distance = if_else(temporal_distance < 0, 0,temporal_distance)
    )
  data_temp <- subset(data_temp, !is.na(data_temp$temporal_distance))
  data_months <- data_temp %>%
    dplyr::mutate(
      Var1 = cut(temporal_distance,breaks = c(-Inf,90,180,270,Inf),
                 labels = c("(0-3)months","(4-6)months","(7-9)months","(10+)months"))
    ) %>%
    dplyr::select(Var1) %>%
    dplyr::group_by(Var1) %>%
    dplyr::summarise(Freq = n()) %>%
    dplyr::mutate(percentage=round(100*(Freq/sum(Freq)),2),
                  pct1=paste0(percentage,"%"))
  return(data_months)
}

data_temp1 <- function(filter){
  if (filter=='Alle'){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  data_temp <- data %>%
    dplyr::mutate(
      temporal_distance = if_else(is.na(valueDate), as.numeric(difftime(createdAt, endDate, units = "days")),
                                  as.numeric(difftime(createdAt, valueDate, units = "days")))#,
    ) %>%
    dplyr::mutate(
      temporal_distance = if_else(temporal_distance < 0, 0,temporal_distance)
    )
  data_months <- data_temp %>%
    dplyr::mutate(
      Var1 = cut(temporal_distance,breaks = c(-Inf,90,180,270,Inf),
                 labels = c("(0-3)months","(4-6)months","(7-9)months","(10+)months"))
    )
  return(data_months)
}

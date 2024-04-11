library(upstartr)
library(jsonlite)
data <- dplyr::rename(data, identity=person_affected)
#data <- dplyr::rename(data, sexualOrientation=your_sexual_orientation)
list_organizationType =c("Non-Governmental Organization (NGO)", "Self-Organization / MSO (Migrant-Self Organization)",
                         "Religious Institution","Social Institution","Cultural Institution",
                         "Association", "Other, and namely:")
data$organizationType <- ifelse(data$identity == "An organization/institution", sample(list_organizationType, nrow(data), replace = TRUE), NA)
data$valueDate <- data$time_incident$exactDate
data$dateRangeState <- paste(data$time_incident$startDate, data$time_incident$endDate, sep = ",")
data$dateRangeState <- gsub("NA,NA", "NA", data$dateRangeState)
data$causesOfDiscrimination <- data$influence_of_the_discrimination
n_employee <- c("Under 10", "10-49","50-250", "Over 250")
data$numberOfEmployees <- ifelse(data$identity=="An organization/institution",sample(n_employee, nrow(data), replace = TRUE), NA)
data$datePeriod <- ifelse(not.na(data$time_incident$startDate),"It happened over a long period of time.",NA)
data$location <- data$place_discrimination$city
data$locationOnline <- ifelse(not.na(data$place_discrimination$city),"It happened online","freeField")
data$stadtteil <- data$place_discrimination$district
data$otherForms <- sample(c("No","Yes, specify:"), nrow(data), replace=TRUE)
list_formofdiscyes <- c("Anti-Asian Racism","Antifeminism","Anti-Muslim Racism","Anti-Black Racism","Antisemitism","Anti-Slavic Racism",
                        "Antiziganism","Queerphobia","Sexism","Other, specify")
data$otherFormsYes <- ifelse(data$formOfDisc=="Yes, specify:", sample(list_formofdiscyes, nrow(data),replace=TRUE), NA)
data$haveYouReported <- sample(c("No","Yes, specify:"), nrow(data), replace=TRUE)
haveYouReportedYes_list <- c('I have reported the case to the police', 'I have visited a counceling center',
                             'I have reported the case to another reporting office, namely:','Other, namely:')
data$haveYouReportedYes <- ifelse(data$haveYouReported=="Yes, specify:", sample(haveYouReportedYes_list, nrow(data),replace=TRUE), NA)
data$gender[data$identity == "An organization/institution"] <- NA
data$age <- data$category_age
data$age[data$identity == "An organization/institution"] <- NA
# data$age[data$age == "[0 - 17]"] <- "Under 18 years"
# data$age[data$age == "[18 - 27]"] <- "18-27 years"
# data$age[data$age == "[28 - 40]"] <- "28-40 years"
# data$age[data$age == "[41 - 65]"] <- "41-65 years"
# data$age[data$age == "[66 - 100]"] <- "Over 65 years"
#data <- data[, -c(4, 5,6,9,10,11,12,13,15,16,17)]
data <- subset(data, select = -c(time_incident, report_date,place_discrimination,influence_of_the_discrimination,
                                 another_discriminations,masures_taken))
data <- subset (data,select=-c(category_age))
json_data <- toJSON(data)
write(json_data, "data.json")

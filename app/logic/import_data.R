#   mongodb+srv://miq-user:Wy1N4zclOtlnR64d@miq-db.ppexwp3.mongodb.net/miq-db
box::use(
  jsonlite[fromJSON],
  rjson[fromJSON],
  magrittr[`%>%`],
  dplyr[mutate, if_else, summarise, group_by, select, n, rename,inner_join, filter],
  leaflet[colorFactor,leafletOutput,renderLeaflet,addProviderTiles,addPolygons,
          addLegend, leaflet, setView,removeScaleBar,labelOptions,highlightOptions],
  tm[stopwords, Corpus, VectorSource, tm_map, content_transformer, removeWords, removePunctuation,
     removeNumbers, stemDocument, stripWhitespace, DocumentTermMatrix],
  sf[st_sfc, st_centroid, st_coordinates],
  topicmodels[LDA, posterior],
  wordcloud2[wordcloud2],
  leaflet.minicharts[addMinicharts],
  janitor[clean_names],topicmodels,tidytext,
  LDAvis,quanteda,readxl,
  Rmpfr,servr
)

box::use(
  app/logic/mongo_fetch
)

root <- getwd()
path_data <- paste(root, "/", "data1", sep="")

connection_string = "mongodb+srv://medar-user2:medar-pwd2@medar-db.7c6rlum.mongodb.net/?retryWrites=true&w=majority&appName=medar-db"
data <- mongo_fetch$fetch_mongodb(connection_string = connection_string, collection = "reports", db="test")


################## Secret Key
key <- "Anti-D-2024"
################# define viewer role
role <- 2

############## Fetch login datas
login_data <- mongo_fetch$fetch_mongodb(connection_string = connection_string, collection = "users", db="test")

file_data_topic <- paste(path_data, "/", "incidents_description.json", sep="")
textdata <- jsonlite::fromJSON(file_data_topic)
# Générer un échantillon aléatoire des indices de textdata
sample_indices <- sample(length(textdata), length(data$description), replace = TRUE)
# Remplacer les valeurs de medar$description par les valeurs correspondantes de textdata
data$description <- textdata[sample_indices]


for (i in 1:nrow(data)){
  data$report_id[i] <- paste ("Report_", i, sep = "")
}

file.data1 <- paste(path_data, "/germany_states.geojson", sep="")
geojson3 <-  rjson::fromJSON(file=file.data1)

data$valueDate <- as.Date(data$valueDate) #, format = "%d.%m.%Y"
data <- tidyr::separate(data, dateRangeState, into = c("startDate", "endDate"), sep = ",")
data$startDate <- as.Date(data$startDate)
data$endDate <- as.Date(data$endDate)
data$createdAt <- as.Date(data$createdAt)

data$locationOnline <- gsub("happenedOnline", "Online",data$locationOnline)
data$locationOnline <- gsub("false", "Reales Leben",data$locationOnline)

#file.dsph <- paste(path_data, "/DEU_adm_shp/DEU_adm1.shp", sep="")
file.dsph1 <- paste(path_data, "/DEU_adm_shp/DEU_adm3.shp", sep="")
# Allemagne <- sf::st_read("Data/DEU_adm_shp/DEU_adm1.shp",
#                          quiet = TRUE)
Allemagne <- sf::st_read(file.dsph1,
                         quiet = TRUE)

Allemagne <- dplyr::rename(Allemagne,  Province = `NAME_3`)

# Remplacement de la modalité "Text (free entry)" par "Other"
#table_age_inf$influence <- gsub("Text \\(free entry\\)", "Other", table_age_inf$influence)


# Remplacer "Other form, namely" par "Other form" dans la colonne "discrimination"
#table_gender_disc$discrimination <- gsub("Other form, namely", "Other form", table_gender_disc$discrimination)


################# Data to display Wordcloud #############




# Utilisation de la fonction avec les données
# characteristics <- data$description
# generate_wordcloud(characteristics)


################## Data to generate map topic ##############
# german_stopwords <- stopwords("german")
# # create corpus object
# corpus <- Corpus(VectorSource(textdata))
# # preprocessing chain
# processedCorpus <- tm_map(corpus, content_transformer(tolower))
# processedCorpus <- tm_map(processedCorpus, removeWords, german_stopwords)
# processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
# processedCorpus <- tm_map(processedCorpus, removeNumbers)
# processedCorpus <- tm_map(processedCorpus, stemDocument, language = "german")
# processedCorpus <- tm_map(processedCorpus, stripWhitespace)
#
# # compute document term matrix with terms >= minimumFrequency
# minimumFrequency <- 3
# DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency,Inf))))

# set.seed(61)
# K <- 5
# topicModel <- LDA(DTM, K, method="Gibbs", control = list(iter = 500, verbose = 25))
# lda_fit <- topicModel

# file_save_lda_model <- paste(path_data, "/", "lda_model.rds", sep="")
# # Sauvegarde du modèle au format rds
# # saveRDS(lda_fit, file = file_save_lda_model)
# # Charger le modèle LDA à partir du fichier RDS
# lda_fit <- readRDS(file_save_lda_model)
# #
# # prediction <- c()
# # for (i in 1: length(data$description)){
# #   text_to_categorize <- data$description[i]
# #   corpus_to_categorize <- Corpus(VectorSource(text_to_categorize))
# #   text_to_categorize <- tm_map(corpus_to_categorize, content_transformer(tolower))
# #   text_to_categorize <- tm_map(text_to_categorize, removeWords, german_stopwords)
# #   text_to_categorize <- tm_map(text_to_categorize, removePunctuation, preserve_intra_word_dashes = TRUE)
# #   text_to_categorize <- tm_map(text_to_categorize, removeNumbers)
# #   text_to_categorize <- tm_map(text_to_categorize, stemDocument, language = "german")
# #   text_to_categorize <- tm_map(text_to_categorize, stripWhitespace)
# #
# # #   # Créer une Document-Term Matrix (DTM) pour le texte
# #   dtm_to_categorize <- DocumentTermMatrix(corpus_to_categorize)
# #
# #   test.topics <- posterior(lda_fit,dtm_to_categorize)
# #   index_max_probability <- which.max(test.topics$topics)
# #   prediction <- c(prediction,index_max_probability)
# # }
#
# file_save_prediction <- paste(path_data, "/", "prediction.rds", sep="")
# #saveRDS(prediction, file_save_prediction)
# prediction <- readRDS(file_save_prediction)
# data$prediction <- prediction
# data$prediction[data$prediction==1] <- "topic 1"
# data$prediction[data$prediction==2] <- "topic 2"
# data$prediction[data$prediction==3] <- "topic 3"
# data$prediction[data$prediction==4] <- "topic 4"
# data$prediction[data$prediction==5] <- "topic 5"
#
# # ajout d'une colonne province sur medar avec des valeurs aleatoire des regions du Nordrhein-Westfalen
# # Générer un échantillon aléatoire des indices de data1$Province
# sample_Province_indices <- sample(length(data1$Province), length(data$description), replace = TRUE)
# data$Province <- data1$Province[sample_Province_indices]
#
# # Création d'un dataframe des fréquences des sujets par province et jointure avec data1
# province_topics <- as.data.frame.matrix(table(data$Province, data$prediction))
#
# # Réinitialiser les noms de lignes
# province_topics$Province <- rownames(province_topics)
# rownames(province_topics) <- NULL
#
# # innner_join entre province_topics et data1
# province_topics_data1 <-  data1 %>%
#   dplyr::inner_join(province_topics, by = "Province")
#
# latitude <- c()
# longitude <- c()
# for (i in 1:nrow(province_topics_data1)) {
#   # Récupérer les coordonnées du centre du polygone
#   geometry_sf <- st_sfc(province_topics_data1[i,]$geometry)
#   centroid <- st_centroid(geometry_sf)
#   latitude <- c(latitude, st_coordinates(centroid)[, "Y"])
#   longitude <- c(longitude, st_coordinates(centroid)[, "X"])
# }
# province_topics$latitude <- latitude
# province_topics$longitude <- longitude
#
# # Afficharge du map
# labels_topic <- sprintf(
#   "<strong>%s</strong>
#   <br/>topic 1: %g
#   <br/>topic 2: %g
#   <br/>topic 3: %g
#   <br/>topic 4: %g
#   <br/>topic 5: %g",
#   province_topics_data1$Province,
#   province_topics_data1$`topic 1`,
#   province_topics_data1$`topic 2`,
#   province_topics_data1$`topic 3`,
#   province_topics_data1$`topic 4`,
#   province_topics_data1$`topic 5`
# ) %>% lapply(htmltools::HTML)
#
# topic_map <- leaflet(width = "100%", height = "400px") %>%
#   addProviderTiles("CartoDB.Positron")%>%
#   addPolygons(
#     data = province_topics_data1,
#     weight = 2,
#     opacity = 1,
#     color = "#666",
#     fillOpacity = 0.1 ,
#     label = labels_topic,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 8px"),
#       textsize = "15px",
#       direction = "auto")
#   )%>%
#   addMinicharts(
#     lng = province_topics$longitude,
#     lat = province_topics$latitude,
#     type = "bar",
#     chartdata = province_topics[1:5],
#     width = 25, height = 25,
#   ) %>%
#   setView(lng=7.661594, lat=51.433237, zoom=7.4)

############### to display select input Compare page #########
#key_var <- c(names(data))
key_var <- c("identity", "gender", "age", "organizationType","locationOnline","stadtteil",
             "causesOfDiscrimination","otherFormsYes","haveYouReported","haveYouReportedYes","numberOfEmployees")


options_var <- lapply(seq_along(key_var), function(i) {
  list(key = key_var[i], text = key_var[i])
})

df <- data
df <- dplyr::mutate_all(df, as.character)

df <- df %>% dplyr::select("identity", "gender", "age", "organizationType","description","valueDate","startDate" ,"endDate","location","locationOnline","stadtteil",
                           "causesOfDiscrimination","typeOfDiscrimination","otherForms","otherFormsYes","formOfQueerphobia","formOfDiscYes",
                           "sexualOrientation","haveYouReported","haveYouReportedYes","causesOfDiscriminationFreeField",
                           "otherFormsYesFreeField","haveYouReportedYesFreeField1","haveYouReportedYesFreeField2","numberOfEmployees",
                           "genderFreeField","createdAt")

filter_list <-c("Alle",unique(data$identity))
filter_text <- c("Alle",unique(data$identity))
options_filter <- lapply(seq_along(filter_list), function(i) {
  list(key = filter_list[i], text = filter_text[i])
})
#topic modelling
# sotu_corpus <- quanteda::corpus(data$description)
# corp = quanteda::corpus_reshape(sotu_corpus, to = "sentences")
# #corp = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
# dfm = quanteda::dfm(corp, remove_punct=T, remove=stopwords("english"))
# dfm = quanteda::dfm_trim(dfm, min_docfreq = 5)
#
# dtm = quanteda::convert(dfm, to = "topicmodels")
# set.seed(1)
#
# m = topicmodels::LDA(dtm, method = "Gibbs", k = 5,  control = list(alpha = 0.1))
#
# dtm = dtm[slam::row_sums(dtm) > 0, ]
# phi = as.matrix(topicmodels::posterior(m)$terms)
# theta <- as.matrix(topicmodels::posterior(m)$topics)
# vocab <- colnames(phi)
# doc.length = slam::row_sums(dtm)
# term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]
#
# json = LDAvis::createJSON(phi = phi, theta = theta, vocab = vocab,
#                           doc.length = doc.length, term.frequency = term.freq)
# route <- paste(path_data,"/Topic_modelling", sep="")
# LDAvis::serVis(json, out.dir = route, open.browser = FALSE)

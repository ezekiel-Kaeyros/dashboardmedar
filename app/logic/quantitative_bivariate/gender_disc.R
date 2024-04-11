box::use(
  app/logic/import_data
)

data <- import_data$data
data <- subset(data, gender!="NULL")
data <- subset(data, gender!="list()")

data$gender <- as.character(data$gender)

########### Data to display gender discrimination ############
discriminations_list <- unlist(data$causesOfDiscrimination)

# Créer un data frame avec les informations des groupes d'âge et des discriminations
data_gender_disc <- data.frame(Var1 = rep(data$gender, sapply(data$causesOfDiscrimination, length)),
                               Var2 = discriminations_list)

# Calculer la fréquence des discriminations pour chaque groupe d'âge
table_gender_disc <- with(data_gender_disc, table(Var1, Var2))

# Convertir la table de fréquence en un format de données adapté à Plotly
table_gender_disc <- as.data.frame(table_gender_disc)
#rename modalities too long
table_gender_disc$Var2 <- gsub("Religiöse Ausübung \\(z.B Beten oder fasten\\)",
                                    "Religiöse Ausübung", table_gender_disc$Var2)

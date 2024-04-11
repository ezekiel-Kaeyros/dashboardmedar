box::use(
  app/logic/import_data
)

data <- import_data$data
data <- subset(data, otherFormsYes!="FALSE")
# Convertir les listes en vecteurs pour chaque groupe d'âge
discriminations_list <- unlist(data$otherFormsYes)

# Créer un data frame avec les informations des groupes d'âge et des discriminations
data_age_disc <- data.frame(Var1 = rep(data$age, sapply(data$otherFormsYes, length)),
                            Var2 = discriminations_list)

# Calculer la fréquence des discriminations pour chaque groupe d'âge
table_age_disc <- with(data_age_disc, table(Var1, Var2))

# Convertir la table de fréquence en un format de données adapté à Plotly
table_age_disc <- as.data.frame(table_age_disc)



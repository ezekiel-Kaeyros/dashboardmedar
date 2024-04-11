box::use(
  app/logic/import_data
)

data <- import_data$data
############## Data to display age_influence ###############
# Initialisation d'une liste vide pour stocker les influences
influence_list <- unlist(data$causesOfDiscrimination)

# Créer un data frame avec les informations des groupes d'âge et les types d'influence
data_age_inf <- data.frame(Var1 = character(),
                           Var2 = character(),
                           stringsAsFactors = FALSE)

# Parcourir chaque ligne de medar
for (i in seq_along(data$age)) {
  # Répéter age_cat pour chaque élément de medar$influence_of_the_influence
  category_age_repeat <- rep(data$age[i], length(data$causesOfDiscrimination[[i]]))
  # Ajouter les valeurs à data
  data_age_inf <- rbind(data_age_inf, data.frame(Var1 = category_age_repeat, Var2 = data$causesOfDiscrimination[[i]]))
}
# Réinitialiser les indices
rownames(data_age_inf) <- NULL
# Calculer la fréquence des influences pour chaque groupe d'âge
table_age_inf <- with(data_age_inf, table(Var1, Var2))

# Convertir la table de fréquence en un format de données adapté à Plotly
table_age_inf <- as.data.frame(table_age_inf)

table_age_inf$Var2 <- gsub("Religiöse Ausübung \\(z.B Beten oder fasten\\)",
                            "Religiöse Ausübung", table_age_inf$Var2)

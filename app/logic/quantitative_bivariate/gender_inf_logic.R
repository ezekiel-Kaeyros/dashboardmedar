box::use(
  app/logic/import_data
)

data <-import_data$data
data <- subset(data, gender!="NULL")
data <- subset(data, gender!="list()")
data <- subset(data, otherFormsYes!="FALSE")

all <- expand.grid(Var1 = unlist(data$gender), Var2 = unlist(data$otherFormsYes))
gender_otherforms <- as.data.frame(table(all))

gender_otherforms$Var2 <- gsub("Religiöse Ausübung \\(z.B Beten oder fasten\\)",
                               "Religiöse Ausübung", gender_otherforms$Var2)

## code to prepare `modelo` dataset goes here

library(datoscovid19mx)
library(dplyr)
library(magrittr)
library(caTools)

datos <- covid_clean %>% 
  filter(resultado == "Positivo SARS-CoV-2") %>% 
  select(tipo_paciente, sexo, edad, neumonia, diabetes, epoc, asma, inmusupr, hipertension, otra_com, obesidad, renal_cronica, 
         tabaquismo)

datos %<>% 
  mutate_at(vars("neumonia", "diabetes", "epoc", "asma", "inmusupr", 
                 "hipertension", "otra_com", "obesidad", "renal_cronica", "tabaquismo"),
            ~ifelse(.=="SI", "SI", "NO"))

datos$tipo_paciente <- ifelse(datos$tipo_paciente == "HOSPITALIZADO", 1, 0)

muestra <- sample.split(datos, SplitRatio = 0.75)
train <- subset(datos, muestra == TRUE)
test <- subset(datos, muestra == FALSE)


modelo <- glm(tipo_paciente~., data = train, family = "binomial")

usethis::use_data(modelo, overwrite = TRUE)

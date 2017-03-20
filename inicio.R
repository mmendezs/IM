
library(dplyr)

# Instalamos la librería haven si fuera necesario:
# install.packages('haven')
# Cargamos la librería haven
library(haven)
# Leemos los datos en formato SPSS
survey <- read_sav("survey.sav")
# Vemos los datos importados
View(survey)
# Realizamos un resumen de cada una de las variables
summary(survey)

survey %>% 
  select(starts_with("P")) %>% 
  

# Creamos una función para reemplazar los valores 0 con NA
repl.f <- function(x) ifelse(x%in%0, NA, x)
# Aplicamos la función a todas las variables que empiezan con "P"
survey <- cbind(mutate_each(select(survey, starts_with("P")), funs(repl.f)),
                select(survey, -starts_with("P")))
                

                
                summary(survey)

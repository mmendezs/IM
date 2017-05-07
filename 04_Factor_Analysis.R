
library(dplyr)
library(ggplot2)
library(psych)
library(gridExtra)

survey <- read.delim("survey.csv",sep = ';')

#Despues de ver los datos, elegimos eliminar las variables con más de 40 NA
survey <- survey %>%
  dplyr::select(-starts_with('P'), which(colSums(is.na(.)) < 41)) %>% 
  select(Establecimiento, starts_with('P'), starts_with('C'))
# Replace NA with median
survey[] <- lapply(survey, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

survey$RC1_Edad <- dplyr::recode(survey$C1_Edad, 21, 30, 40, 50, 60, 65)
survey$RC2_Sexo <- recode(survey$C2_Sexo, 0, 1)
survey$RC4_Compra_Media <- recode(
  survey$C4_Compra_Media, 15, 23, 38, 53, 68, 83, 103, 120)
survey$RC5 <- recode(
  survey$C5_Ingr_Mes, 600, 800, 1250, 1750, 2500, 3750, 4500)

survey$Establecimiento <- 
  factor(survey$Establecimiento, labels = c('carrefour','dia','mercadona'))
survey$C1_Edad <-
  factor(survey$C1_Edad,
         labels = c('18-24','25-35','35-44','45-54','55-64','>64'))
survey$C2_Sexo <- 
  factor(survey$C2_Sexo,labels = c('hombre','mujer'))
survey$C3_Estado_Civil <-
  factor(survey$C3_Estado_Civil,
         labels = c('soltero/a','casado/a','unido/a',
                    'separado/a','viudo/a'))
survey$C4_Compra_Media <-
  factor(survey$C4_Compra_Media,
         labels = c('<15','15-30','31-45','46-60','61-75',
                    '76-90','91-120','>120'))
survey$C5_Ingr_Mes <-
  factor(survey$C5_Ingr_Mes,
         labels = c('<600','601-1000','1001-1500',
                    '1501-2000','2001-3000','3001-4500','>4500'))


# Separamos datos por supermercados
dia <- filter(survey, Establecimiento=='dia')
carrefour <- filter(survey, Establecimiento=='carrefour')
mercadona <- filter(survey, Establecimiento=='mercadona')

## psych
# Exploratorio

#psych::scree(select(survey, starts_with('P')), pc = TRUE) # gráfico feo
# Hacemos el análisis Kaiser Meyer Olsen
survey_KMO <-psych::KMO(select(survey, starts_with('P')))
# Valor KMO
survey_KMO$MSA
# PCA con todas las columnas para elegir cuantos factores y varianza explicada
survey_principal <- psych::principal(select(survey, starts_with('P')),
  nfactors = ncol(select(survey, starts_with('P'))), rotate = 'none')
survey_principal$loadings
# Elegimos 4 factores sin rotar
# PCA con todas las columnas para elegir cuantos factores y varianza explicada
survey_principal <- psych::principal(select(survey, starts_with('P')),
  nfactors = 4, rotate = 'none')
# Estudiamos las comunalidades
survey_principal$communality
print(survey_principal$loadings, cutoff = 0.4, digits = 2)
# Extraemos las puntuaciones
survey_principal_cargas <- as.data.frame(unclass(survey_principal$loadings))
# Añadimos columnas con los nombres de las variables
survey_principal_cargas$variables <- rownames(survey_principal_cargas)
# Reordenamos las columnas
survey_principal_cargas <- select(survey_principal_cargas, variables, starts_with('P'))

# Cogemos los scores de cada individuo
survey_principal_scores <- as.data.frame(unclass(survey_principal$scores))
# Les incluimos el establecimiento
survey_principal_scores$Establecimiento <- survey$Establecimiento

# Gráfico sin rotar
ggplot(data = survey_principal_scores, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = factor(Establecimiento)), alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = survey_principal_cargas, 
            aes(PC1, PC2, label = variables),check_overlap = TRUE) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  ggtitle("Análisis Componentes por centros, incluyendo individuos") +
  scale_colour_discrete(name = "Variable")

# ROTAMOS
survey_principal_rot <- psych::principal(select(survey, starts_with('P')),
  nfactors = 4, rotate = 'varimax')
# Vemos los principales
print(survey_principal_rot$loadings, cutoff = 0.4, digits = 2)
survey_principal_cargas_rot <- as.data.frame(unclass(survey_principal_rot$loadings))
survey_principal_cargas_rot$variables <- rownames(survey_principal_cargas_rot)
survey_principal_cargas_rot <- select(survey_principal_cargas_rot, variables, starts_with('R'))

# Cogemos los scores de cada individuo
survey_principal_scores_rot <- as.data.frame(unclass(survey_principal_rot$scores))
# Les incluimos el establecimiento
survey_principal_scores_rot$Establecimiento <- survey$Establecimiento
# Gráfico Rotado
ggplot(data = survey_principal_scores_rot, aes(RC1, RC2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = factor(Establecimiento)), alpha = 0.4) +
  geom_density2d(colour = "gray80") +
  geom_text(data = survey_principal_cargas_rot, size = 3, 
            aes(RC1, RC2, label = variables), check_overlap = TRUE) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  ggtitle("Análisis Componentes por centros, incluyendo individuos") +
  scale_colour_discrete(name = "Variable")


survey_principal_scores_rot <- data.frame(survey_principal_scores_rot)
# Creamos variables temporales
a <- select(survey_principal_scores_rot, x = RC1, y = RC2, Establecimiento)
a$factor <- 'Empleados y Establecimiento'
b <- select(survey_principal_scores_rot, x = RC1, y = RC3, Establecimiento)
b$factor <- 'Empleados y Tiempos'
c <- select(survey_principal_scores_rot, x = RC1, y = RC4, Establecimiento)
c$factor <- 'Empleados y Gama Productos'
d <- dplyr::bind_rows(a, b, c)
# Gráfico de los 3 centros
ggplot(data = d, aes(x, y)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = factor(Establecimiento)), alpha = 0.4) +
  geom_density2d(colour = "gray80") +
  facet_grid(~ factor) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  ggtitle("Análisis Componentes por centros, incluyendo individuos") +
  scale_colour_discrete(name = "Variable")

## Día con 4 factores
# Hacemos el análisis Kaiser Meyer Olsen
dia_KMO <-psych::KMO(select(dia, starts_with('P')))
# Valor KMO
dia_KMO$MSA

# ROTAMOS
dia_principal_rot <- psych::principal(select(dia, starts_with('P')),
                                         nfactors = 4, rotate = 'varimax')
# Estudiamos las comunalidades
dia_principal_rot$communality
# Vemos los principales
print(dia_principal_rot$loadings, sort = TRUE ,cutoff = 0.4, digits = 2)
dia_principal_cargas_rot <- as.data.frame(unclass(dia_principal_rot$loadings))
dia_principal_cargas_rot$variables <- rownames(dia_principal_cargas_rot)
dia_principal_cargas_rot <- select(dia_principal_cargas_rot, variables, starts_with('R'))

# Cogemos los scores de cada individuo
dia_principal_scores_rot <- as.data.frame(unclass(dia_principal_rot$scores))
# Les incluimos el establecimiento
dia_principal_scores_rot$Establecimiento <- dia$Establecimiento
# Agrupamos los scores por establecimiento

ggplot(data = dia_principal_scores_rot, aes(RC1, RC2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = 'green', alpha = 0.2) +
  geom_density2d(colour = "gray80") +
  geom_text(data = dia_principal_cargas_rot, size = 3,
            aes(RC1, RC2, label = variables), check_overlap = TRUE) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  ggtitle("Análisis Componentes por centros, incluyendo individuos") +
  scale_colour_discrete(name = "Variable")


## Carrefour con 4 factores
# Hacemos el análisis Kaiser Meyer Olsen
carrefour_KMO <-psych::KMO(select(carrefour, starts_with('P')))
# Valor KMO
carrefour_KMO$MSA

# ROTAMOS
carrefour_principal_rot <- psych::principal(select(carrefour, starts_with('P')),
                                      nfactors = 4, rotate = 'varimax')
# Estudiamos las comunalidades
carrefour_principal_rot$communality
# Vemos los principales
print(carrefour_principal_rot$loadings, sort = TRUE ,cutoff = 0.4, digits = 2)
carrefour_principal_cargas_rot <- as.data.frame(unclass(carrefour_principal_rot$loadings))
carrefour_principal_cargas_rot$variables <- rownames(carrefour_principal_cargas_rot)
carrefour_principal_cargas_rot <- select(carrefour_principal_cargas_rot, variables, starts_with('R'))

# Cogemos los scores de cada individuo
carrefour_principal_scores_rot <- as.data.frame(unclass(carrefour_principal_rot$scores))
# Les incluimos el establecimiento
carrefour_principal_scores_rot$Establecimiento <- carrefour$Establecimiento
# Agrupamos los scores por establecimiento

ggplot(data = carrefour_principal_scores_rot, aes(RC1, RC2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = 'green', alpha = 0.2) +
  geom_density2d(colour = "gray80") +
  geom_text(data = carrefour_principal_cargas_rot, size = 3,
            aes(RC1, RC2, label = variables), check_overlap = TRUE) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  ggtitle("Análisis Componentes por centros, incluyendo individuos") +
  scale_colour_discrete(name = "Variable")


## Mercadona con 4 factores
# Hacemos el análisis Kaiser Meyer Olsen
mercadona_KMO <-psych::KMO(select(mercadona, starts_with('P')))
# Valor KMO
mercadona_KMO$MSA

# ROTAMOS
mercadona_principal_rot <- psych::principal(select(mercadona, starts_with('P')),
                                            nfactors = 4, rotate = 'varimax')
# Estudiamos las comunalidades
mercadona_principal_rot$communality
# Vemos los principales
print(mercadona_principal_rot$loadings, sort = TRUE ,cutoff = 0.4, digits = 2)
mercadona_principal_cargas_rot <- as.data.frame(unclass(mercadona_principal_rot$loadings))
mercadona_principal_cargas_rot$variables <- rownames(mercadona_principal_cargas_rot)
mercadona_principal_cargas_rot <- select(mercadona_principal_cargas_rot, variables, starts_with('R'))

# Cogemos los scores de cada individuo
mercadona_principal_scores_rot <- as.data.frame(unclass(mercadona_principal_rot$scores))
# Les incluimos el establecimiento
mercadona_principal_scores_rot$Establecimiento <- mercadona$Establecimiento
# Agrupamos los scores por establecimiento

ggplot(data = mercadona_principal_scores_rot, aes(RC1, RC2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = 'green', alpha = 0.2) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mercadona_principal_cargas_rot, size = 3,
            aes(RC1, RC2, label = variables), check_overlap = TRUE) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  ggtitle("Análisis Componentes por centros, incluyendo individuos") +
  scale_colour_discrete(name = "Variable")








## Explicación de la rotación
x_sin <- c(-4, -2, 1, 3, 2)+6
y_sin <- c(4,0,1,-0.5,-2.5)+6
sin_rotar <- data.frame(x = x_sin, y = y_sin, rot='sin rotar', modelo = c('A', 'B', 'C', 'D', 'E'))
x_rot <- c(4,3.8,1,-2,-6)+6
y_rot <- c(-1,1.4,0.25,-1.3,0.7)+5
rotado <- data.frame(x = x_rot, y = y_rot, rot='rotado', modelo = c('E', 'D', 'C', 'B', 'A'))
datos <- bind_rows(sin_rotar, rotado)
datos$rot <- factor(datos$rot, levels = c('sin rotar', 'rotado'))

ggplot(datos, aes(x,y)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(fill = NA, alpha = 0) +
  geom_smooth(method=lm, se=FALSE) +
  facet_grid(~ rot) +
  geom_text(aes(label = modelo)) +
  labs(x='deportividad', y = 'comodidad')



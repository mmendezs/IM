#suppressMessages(library(dplyr))
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(stringr)
library(gridExtra)

survey <- read.delim("survey.csv",sep = ';')

# Recodificamos las variables de clasificación

survey$RC1_Edad <- dplyr::recode(survey$C1_Edad, 21, 30, 40, 50, 60, 65)
survey$RC2_Sexo <- recode(survey$C2_Sexo, 0, 1)
survey$RC4_Compra_Media <- recode(
  survey$C4_Compra_Media, 15, 23, 38, 53, 68, 83, 103, 120)
survey$RC5 <- recode(
  survey$C5_Ingr_Mes, 600, 800, 1250, 1750, 2500, 3750, 4500)

survey$Establecimiento <- 
  factor(survey$Establecimiento,
         labels = c('carrefour','dia','mercadona'))

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


#### Cluster
#Seleccionamos variables p y le quitamos los NA
survey_clust <- survey %>% 
  select(Establecimiento, starts_with('P')) %>% 
  na.omit()

clust_result <- kmeans(survey_clust[,-1],3)

table(survey_clust$Establecimiento, clust_result$cluster)

plot(survey_clust[,34:35], col = clust_result$cluster)

plot(survey_clust[,34:35], col = survey_clust$Establecimiento)

ggplot(survey_clust,
       aes(Ph1_Relacion_Calidad_Precio, Ph1_Satisfaccion_Global, color = factor(clust_result$cluster))) +
  geom_point()

ggplot(survey_clust,
       aes(Ph1_Relacion_Calidad_Precio, Ph1_Satisfaccion_Global, color = Establecimiento)) +
  geom_point()


# ###
# # Sacar nombres de las variables de las filas
# mtcars %>% head()
# mtcars %>% add_rownames('model') %>% head()
# ###

# Vemos los datos
glimpse(survey)

# Calcula moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Gráfico Medias
medias<- survey %>% 
  group_by(Establecimiento) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)), starts_with('P'))

# Trash!
# medias_t <- data.frame(t(medias))
# medias_t <- medias_t[-1,]
# names(medias_t) <- c('carrefour','dia','mercadona')
# medias_t <- medias_t %>% arrange(medias_t, mercadona)
# 
# 

medias_melt <- melt(medias, id.vars = 'Establecimiento',
                    value.name = 'value', variable.name = 'preguntas')

# En tres columnas
ggplot(data = medias_melt, aes(x = preguntas,
                               y = value,
                               group = Establecimiento,
                               color = Establecimiento)) +
  geom_line() +
  geom_point(size = 4, shape = 21, fill = "white") +
  facet_grid(~ Establecimiento) +
  coord_flip()

# En una columna, USADO
ggplot(medias_melt,
       aes(reorder(preguntas, value), value, 
           group = Establecimiento, color = Establecimiento)) + 
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  theme(legend.position="bottom",legend.title=element_blank()) +
  theme(panel.grid.major.x = element_line(colour="black", size=0.5)) +
  ggtitle('Comparativa respuestas medias') +
  labs(x = '', y = "Medias de las respuestas") +
  coord_flip()

## Ojo reorder
# En una fila
ggplot(data = medias_melt, aes(x = reorder(preguntas, -value),
                               y = value,
                               group = Establecimiento,
                               color = Establecimiento)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal(base_family = "RobotoCondensed-Regular", base_size = 13) +
  theme(axis.text.x=element_text(angle=90, hjust=1))

## Medias de Día
# Seleccionamos dia y hallamos la media para cada respuesta
dia_media <- survey %>% 
  dplyr::filter(Establecimiento == 'dia') %>%
  summarise_each(funs(mean(., na.rm = TRUE)), starts_with('P'))
# Dado que obtenemos valores por filas:
glimpse(dia_media)
# Transponemos y pasamos los nombres a una columna:
dia_media <- as.data.frame(t(dia_media))
dia_media$variables <- rownames(dia_media)
# Realizamos el gráfico de las medias, ordenando de mayor a menor
# en aes, hemos de añadir `group = 1` para dibujar la linea
ggplot(dia_media,
       aes(reorder(variables, V1), V1, 
           color = variables, group = 1)) + 
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  theme(legend.position="none") +
  theme(panel.grid.major.x = element_line(colour="grey", size=0.5)) +
  ggtitle('Respuestas medias para Día') +
  labs(x = '', y = "Medias de las respuestas") +
  coord_flip()

  

# Separamos datos por supermercados
dia <- dplyr::filter(survey, Establecimiento=='dia')
carrefour <- filter(survey, Establecimiento=='carrefour')
mercadona <- filter(survey, Establecimiento=='mercadona')




# Descriptivos completos
sumario <- survey %>% 
  group_by(Establecimiento) %>% 
  summarise_each(funs(mean(., na.rm = TRUE),
                      sd(., na.rm = TRUE),
                      median(., na.rm = TRUE),
                      getmode(.)),
                 starts_with('P'))

sumario <- data.frame(t(sumario))
sumario <- sumario[-1,]
names(sumario) <- c('carrefour','dia','mercadona')


# Separamos datos por supermercados
dia <- filter(survey, Establecimiento=='dia')
carrefour <- filter(survey, Establecimiento=='carrefour')
mercadona <- filter(survey, Establecimiento=='mercadona')


# Seleccionamos variables Ph y sólo los datos mayores que 6
dia %>%
  select(Ph1_Relacion_Calidad_Precio,Ph1_Satisfaccion_Global) %>% 
  filter(Ph1_Satisfaccion_Global>6)

# Seleccionamos variables Ph ordenamos de mayor a menor
dia %>%
  select(Ph1_Relacion_Calidad_Precio,Ph1_Satisfaccion_Global) %>% 
  arrange(desc(Ph1_Satisfaccion_Global))

# Hallamos las medias por establecimiento
survey %>% 
  group_by(Establecimiento) %>% 
  summarise(media_satisfaccion=mean(Ph1_Satisfaccion_Global, na.rm=TRUE))

# Hallamos las medias de varias variables
medias <- survey %>% 
  group_by(Establecimiento) %>% 
  summarise_each(funs(mean(.,na.rm=TRUE)),
                 Ph1_Relacion_Calidad_Precio,Ph1_Satisfaccion_Global)

# Contar cuantos casos por establecimiento
survey %>% 
   group_by(Establecimiento) %>% 
   summarise(Ph1_Satisfaccion_Global=n())

# Tabla resumen por establecimento
survey %>% 
  group_by(Establecimiento) %>% 
  select(Ph1_Satisfaccion_Global) %>% 
  table()

# Seleccionar todo menos contiene un nombre
survey %>% 
  select(-starts_with('P'))


# Seleccionar cadenas con c()
cols <- c('C1_Edad','C2_Sexo')
survey %>% 
  select(one_of(cols))

# Renombrar
survey %>%
  rename(Edad = C1_Edad)


# Describimos la muestra

# # Edad
# ggplot(na.omit(survey),aes(C1_Edad, fill = C1_Edad)) +
#   geom_bar(aes(y = (..count..)/sum(..count..))) + # Pasamos a porcentaje
#   theme(legend.position="bottom",legend.direction="horizontal") +
#   theme(legend.title=element_blank()) +
#   ggtitle('Edades de los encuestados') +
#   labs(x = 'Edad', y = 'Frecuencia') +
#   scale_y_continuous(labels = percent)
# 
# ggplot(na.omit(survey),aes(C1_Edad, fill = C1_Edad)) +
#   geom_bar(aes(y = (..count..)/sum(..count..))) + # Pasamos a porcentaje
#   facet_grid(Establecimiento ~ .) +
#   theme(legend.position="bottom",legend.direction="horizontal") +
#   theme(legend.title=element_blank()) +
#   ggtitle('Edades de los encuestados') +
#   labs(x = 'Edad', y = 'Frecuencia') +
#   scale_y_continuous(labels = percent)
# 
# # Sexo
# ggplot(na.omit(survey),aes(C2_Sexo, fill = C2_Sexo)) +
#   geom_bar(aes(y = (..count..)/sum(..count..))) + # Pasamos a porcentaje
#   theme(legend.position="bottom",legend.direction="horizontal") +
#   theme(legend.title=element_blank()) +
#   ggtitle('Sexo de los encuestados') +
#   labs(x = 'Sexo', y = 'Frecuencia') +
#   scale_y_continuous(labels = percent)
# 
# # Estado civil
# ggplot(na.omit(survey),aes(C3_Estado_Civil, fill = C3_Estado_Civil)) +
#   geom_bar(aes(y = (..count..)/sum(..count..))) + # Pasamos a porcentaje
#   theme(legend.position="bottom",legend.direction="horizontal") +
#   theme(legend.title=element_blank()) +
#   facet_grid(Establecimiento ~ .) +
#   ggtitle('Estado civil de los encuestados') +
#   labs(x = 'Estado civil', y = 'Frecuencia') +
#   scale_y_continuous(labels = percent)
# 
# # Importe medio de la compra, C4_Importe_Medio_Compra
# ggplot(na.omit(survey),aes(C4_Compra_Media, fill =C4_Compra_Media)) +
#   geom_bar(aes(y = (..count..)/sum(..count..))) + # Pasamos a porcentaje
#   facet_grid(Establecimiento ~ .) +
#   theme(legend.position="bottom",legend.title=element_blank()) +
#   ggtitle('Importe medio de la compra') +
#   labs(x = 'Importe medio de la compra', y = 'Frecuencia') +
#   scale_y_continuous(labels = percent)
# 
# # Ingresos mensuales totales
# ggplot(na.omit(survey),
#   aes(C5_Ingr_Mes, fill =C5_Ingr_Mes)) +
#   geom_bar(aes(y = (..count..)/sum(..count..))) + # Pasamos a porcentaje
#   facet_grid(Establecimiento ~ .) +
#   theme(legend.position="bottom",legend.title=element_blank()) +
#   ggtitle('Ingresos mensuales totales') +
#   labs(x = 'Ingresos mensuales totales', y = 'Frecuencia') +
#   scale_y_continuous(labels = percent)
# 
# # Amabilidad de los empleados
# ggplot(na.omit(survey), aes(C1_Edad,Pc1_Actitud)) +
#   geom_boxplot(aes(fill = Establecimiento)) +
#   theme(legend.position="bottom",legend.title=element_blank()) +
#   ggtitle('Valoración amabilidad de los empleados') +
#   labs(x = 'Edad', y = 'Valoración de 1 a 10')
# 
# # Satisfacción global
# ggplot(na.omit(survey), aes(C1_Edad,Ph1_Satisfaccion_Global)) +
#   geom_boxplot(aes(fill = Establecimiento)) +
#   theme(legend.position="bottom",legend.title=element_blank()) +
#   ggtitle('Satisfacción global') +
#   labs(x = 'Edad', y = 'Valoración de 1 a 10')
# 
# # Relación calidad precio
# ggplot(na.omit(survey), aes(C1_Edad,Ph1_Relacion_Calidad_Precio)) +
#   geom_boxplot(aes(fill = Establecimiento)) +
#   theme(legend.position="bottom",legend.title=element_blank()) +
#   ggtitle('Relación calidad precio') +
#   labs(x = 'Edad', y = 'Valoración de 1 a 10')
# 



#summarise(survey,Establecimiento,Ph1_Satisfaccion_Global)

#table(survey$Ph1_Satisfaccion_Global,survey$Establecimiento)/colSums(survey$Ph1_Satisfaccion_Global)

# dplyr

















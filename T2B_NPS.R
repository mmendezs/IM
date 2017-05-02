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


#### T2B
# Separamos datos por supermercados
dia <- filter(survey, Establecimiento=='dia')
carrefour <- filter(survey, Establecimiento=='carrefour')
mercadona <- filter(survey, Establecimiento=='mercadona')
# Seleccionamos las preguntas sobre el establecimiento
dia_P <- dia %>% select(starts_with('P'))
carrefour_P <- carrefour %>% select(starts_with('P'))
mercadona_P <- mercadona %>% select(starts_with('P'))
# creamos los grupos

## Prueba dplyr
aa<- dplyr::filter(dia_P)
#(funs(mean(., na.rm = TRUE)), starts_with('P'))



# sum nos devuelve la suma de valores lógicos, es decir si cumple la condición 1 si no 0
## Dia
dia_T2B <- list()
dia_T2B[1] <- as.data.frame(apply(dia_P, 2, function(x) sum(x >= 8 & x <= 10, na.rm=TRUE)))
dia_T2B[2] <- as.data.frame(apply(dia_P, 2, function(x) sum(x >= 5 & x <= 7, na.rm=TRUE)))
dia_T2B[3] <- as.data.frame(apply(dia_P, 2, function(x) sum(x <= 4, na.rm=TRUE)))
dia_T2B <- data.frame(dia_T2B)
dia_T2B <- t(dia_T2B)
# pasamos a porcentaje
dia_T2B <- as.data.frame(t(apply( dia_T2B,1, function(x) x/sum(x, na.rm=TRUE))))*100
# ponemos nombre a cada grupo
colnames(dia_T2B) <- c('Alto','Medio','Bajo')
# añadimos una columna con los nombres de las variables
dia_T2B$variables <- (names(dia_P))
dia_T2B$centro <- 'dia'
dia_T2B_melt <- melt(subset(dia_T2B, select=- centro), id.vars = 'variables')
# gráfico
ggplot(dia_T2B_melt, aes(variables, value, fill= variable)) +
  geom_bar(position = position_stack(), stat = "identity") +
  geom_text(aes(label = paste0(round(value,0),"%")), position = position_stack(vjust = 0.5), size = 4) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  ggtitle('Valoraciones Día') +
  coord_flip()

### Prueba REMELT Funciona!!
## Dia
dia_T2B <- list()
dia_T2B[1] <- as.data.frame(apply(dia_P, 2, function(x) sum(x >= 8 & x <= 10, na.rm=TRUE)))
dia_T2B[2] <- as.data.frame(apply(dia_P, 2, function(x) sum(x >= 5 & x <= 7, na.rm=TRUE)))
dia_T2B[3] <- as.data.frame(apply(dia_P, 2, function(x) sum(x <= 4, na.rm=TRUE)))
dia_T2B <- data.frame(dia_T2B)
# pasamos a porcentaje
dia_T2B <- as.data.frame(t(apply( dia_T2B,1, function(x) x/sum(x, na.rm=TRUE))))*100
# ponemos nombre a cada grupo
colnames(dia_T2B) <- c('Alto','Medio','Bajo')
# añadimos una columna con los nombres de las variables
dia_T2B$variables <- (names(dia_P))
dia_T2B$centro <- 'dia'
dia_T2B_melt <- melt(dia_T2B, id.vars = c('variables', 'centro'))
## Carrefour
carrefour_T2B <- list()
carrefour_T2B[1] <- as.data.frame(apply(carrefour_P, 2, function(x) sum(x >= 8 & x <= 10, na.rm=TRUE)))
carrefour_T2B[2] <- as.data.frame(apply(carrefour_P, 2, function(x) sum(x >= 5 & x <= 7, na.rm=TRUE)))
carrefour_T2B[3] <- as.data.frame(apply(carrefour_P, 2, function(x) sum(x <= 4, na.rm=TRUE)))
carrefour_T2B <- data.frame(carrefour_T2B)
# pasamos a porcentaje
carrefour_T2B <- as.data.frame(t(apply( carrefour_T2B,1, function(x) x/sum(x, na.rm=TRUE))))*100
# ponemos nombre a cada grupo
colnames(carrefour_T2B) <- c('Alto','Medio','Bajo')
# añadimos una columna con los nombres de las variables
carrefour_T2B$variables <- (names(carrefour_P))
carrefour_T2B$centro <- 'carrefour'
carrefour_T2B_melt <- melt(carrefour_T2B, id.vars = c('variables', 'centro'))
## Mercadona
mercadona_T2B <- list()
mercadona_T2B[1] <- as.data.frame(apply(mercadona_P, 2, function(x) sum(x >= 8 & x <= 10, na.rm=TRUE)))
mercadona_T2B[2] <- as.data.frame(apply(mercadona_P, 2, function(x) sum(x >= 5 & x <= 7, na.rm=TRUE)))
mercadona_T2B[3] <- as.data.frame(apply(mercadona_P, 2, function(x) sum(x <= 4, na.rm=TRUE)))
mercadona_T2B <- data.frame(mercadona_T2B)
# pasamos a porcentaje
mercadona_T2B <- as.data.frame(t(apply( mercadona_T2B,1, function(x) x/sum(x, na.rm=TRUE))))*100
# ponemos nombre a cada grupo
colnames(mercadona_T2B) <- c('Alto','Medio','Bajo')
# añadimos una columna con los nombres de las variables
mercadona_T2B$variables <- (names(mercadona_P))
mercadona_T2B$centro <- 'mercadona'
mercadona_T2B_melt <- melt(mercadona_T2B, id.vars = c('variables', 'centro'))

# juntamos todos los resultados
resumen_T2B_melt <- dplyr::bind_rows(dia_T2B_melt, carrefour_T2B_melt, mercadona_T2B_melt)

# ordenamos la forma en la que aparecen los centros en el gráfico
resumen_T2B_melt$centro <- factor(resumen_T2B_melt$centro, levels = c('dia', 'carrefour', 'mercadona'))


# gráfico
ggplot(resumen_T2B_melt, aes(variables, value, fill= variable)) +
  geom_bar(position = position_stack(), stat = "identity") +
  geom_text(aes(label = paste0(round(value,0),"%")), position = position_stack(vjust = 0.5), size = 3) +
  coord_flip() +
  facet_grid(. ~ centro) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  guides(fill = guide_legend(reverse=TRUE))+
  ggtitle('Comparativa Valoraciones T3B y B3B') +
  coord_flip()








## Carrefour
carrefour_T2B <- list()
carrefour_T2B[1] <- as.data.frame(apply(carrefour_P, 2, function(x) sum(x >= 8 & x <= 10, na.rm=TRUE)))
carrefour_T2B[2] <- as.data.frame(apply(carrefour_P, 2, function(x) sum(x >= 5 & x <= 7, na.rm=TRUE)))
carrefour_T2B[3] <- as.data.frame(apply(carrefour_P, 2, function(x) sum(x <= 4, na.rm=TRUE)))
carrefour_T2B <- data.frame(carrefour_T2B)
# pasamos a porcentaje
carrefour_T2B <- as.data.frame(t(apply( carrefour_T2B,1, function(x) x/sum(x, na.rm=TRUE))))*100
# ponemos nombre a cada grupo
colnames(carrefour_T2B) <- c('Alto','Medio','Bajo')
# añadimos una columna con los nombres de las variables
carrefour_T2B$variables <- (names(carrefour_P))
carrefour_T2B$centro <- 'carrefour'
carrefour_T2B_melt <- melt(subset(carrefour_T2B, select = - centro), id.vars = 'variables')
# gráfico
ggplot(carrefour_T2B_melt, aes(variables, value, fill= variable)) +
  geom_bar(position = position_stack(), stat = "identity") +
  geom_text(aes(label = paste0(round(value,0),"%")), position = position_stack(vjust = 0.5), size = 4) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  ggtitle('Valoraciones Carrefour') +
  coord_flip()

## Mercadona
mercadona_T2B <- list()
mercadona_T2B[1] <- as.data.frame(apply(mercadona_P, 2, function(x) sum(x >= 8 & x <= 10, na.rm=TRUE)))
mercadona_T2B[2] <- as.data.frame(apply(mercadona_P, 2, function(x) sum(x >= 5 & x <= 7, na.rm=TRUE)))
mercadona_T2B[3] <- as.data.frame(apply(mercadona_P, 2, function(x) sum(x <= 4, na.rm=TRUE)))
mercadona_T2B <- data.frame(mercadona_T2B)
# pasamos a porcentaje
mercadona_T2B <- as.data.frame(t(apply( mercadona_T2B,1, function(x) x/sum(x, na.rm=TRUE))))*100
# ponemos nombre a cada grupo
colnames(mercadona_T2B) <- c('Alto','Medio','Bajo')
# añadimos una columna con los nombres de las variables
mercadona_T2B$variables <- (names(mercadona_P))
mercadona_T2B$centro <- 'mercadona'
mercadona_T2B_melt <- melt(subset(mercadona_T2B, select = - centro), id.vars = 'variables')
# gráfico
ggplot(mercadona_T2B_melt, aes(variables, value, fill= variable)) +
  geom_bar(position = position_stack(), stat = "identity") +
  geom_text(aes(label = paste0(round(value,0),"%")), position = position_stack(vjust = 0.5), size = 4) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  ggtitle('Valoraciones Mercadona') +
  coord_flip()


# T3B dia
ggplot(dia_T2B[,c('Alto', 'variables')], aes(variables, Alto, fill = variables)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  ggtitle('Top three box Día (%)') +
  coord_flip()

comparacion_T2B <- rbind(dia_T2B[,c('Alto', 'variables', 'centro')], 
                         carrefour_T2B[,c('Alto', 'variables', 'centro')], 
                         mercadona_T2B[,c('Alto', 'variables', 'centro')])




#comparacion_T2B_melt <- melt(comparacion_T2B, id.vars = c('centro', 'variables'))
ggplot(comparacion_T2B, aes(variables, Alto, fill =centro)) +
  geom_bar(stat="identity", position = "dodge") +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  ggtitle('Top three box') +
  coord_flip()

# comparación T3B en linea
ggplot(comparacion_T2B,
       aes(reorder(variables, Alto), Alto, 
           group = centro, color = centro)) + 
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  theme(legend.position="bottom",legend.title=element_blank()) +
  theme(panel.grid.major.x = element_line(colour="black", size=0.5)) +
  ggtitle('Top three box') +
  labs(x = '', y = "% Respuestas entre 8 y 10") +
  coord_flip()


# comparación T3B en linea seleccionando solo algunas variables
ggplot(dplyr::filter(comparacion_T2B, stringr::str_detect(variables, 'Pc1') ),
       aes(reorder(variables, Alto), Alto, 
           group = centro, color = centro)) + 
  geom_line(size = 0.75) +
  geom_point(size = 1) +
  theme(legend.position="bottom",legend.title=element_blank()) +
  theme(panel.grid.major.x = element_line(colour="black", size=0.5)) +
  ggtitle('Top three box') +
  labs(x = '', y = "% Respuestas entre 8 y 10") +
  coord_flip()

# NPS
# http://www.netpromotersystem.com/about/measuring-your-net-promoter-score.aspx

# NPS
# Día
dia_P_prop <-
  as.data.frame(prop.table(table(dia_P$Ph1_Satisfaccion_Global)))
dia_P_prop$Var1 <- as.integer(dia_P_prop$Var1)
dia_nps <- (sum(dia_P_prop$Freq[dia_P_prop$Var1>=9])
            - sum(dia_P_prop$Freq[dia_P_prop$Var1<=6]))

dia_P_prop$color <- ifelse(dia_P_prop$Var1 <= 6, 'Detractor'
  ,ifelse(dia_P_prop$Var1 <= 8, 'Pasivo', 'Promotor'))

dia_NPS_plot <-
  ggplot(dia_P_prop, aes(as.factor(Var1), Freq, fill = color)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(Detractor='red', 
                               Pasivo = 'yellow', Promotor = 'green')) +
  scale_y_continuous(labels=percent) +
  theme(legend.position="bottom",legend.title=element_blank()) +
  labs(title = paste0('Dia NPS ',round(dia_nps*100,2),
                      '%. Con Satisfacción Global'),
       x = 'Valoración', y = 'Porcentaje')
dia_NPS_plot

# Carrefour
carrefour_P_prop <-
  as.data.frame(prop.table(table(carrefour_P$Ph1_Satisfaccion_Global)))
carrefour_P_prop$Var1 <- as.integer(carrefour_P_prop$Var1)
carrefour_nps <- (sum(carrefour_P_prop$Freq[carrefour_P_prop$Var1>=9])
            - sum(carrefour_P_prop$Freq[carrefour_P_prop$Var1<=6]))

carrefour_P_prop$color <- ifelse(carrefour_P_prop$Var1 <= 6, 'Detractor'
  ,ifelse(carrefour_P_prop$Var1 <= 8, 'Pasivo', 'Promotor'))

carrefour_NPS_plot <-
  ggplot(carrefour_P_prop, aes(as.factor(Var1), Freq, fill = color)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(Detractor='red', 
                               Pasivo = 'yellow', Promotor = 'green')) +
  scale_y_continuous(labels=percent) +
  theme(legend.position="bottom",legend.title=element_blank()) +
  labs(title = paste0('Carrefour NPS ',round(carrefour_nps*100,2),
                      '%. Con Satisfacción Global'),
       x = 'Valoración', y = 'Porcentaje')

carrefour_NPS_plot

# Mercadona CAMBIAR el Resto NPS Preparado por si empieza por 0
mercadona_P_prop <- 
  as.data.frame(prop.table(table(mercadona_P$Ph1_Satisfaccion_Global)))
mercadona_P_prop$Var1 <- as.integer(mercadona_P_prop$Var1)
mercadona_nps <- (sum(mercadona_P_prop$Freq[mercadona_P_prop$Var1>=9])
                  - sum(mercadona_P_prop$Freq[mercadona_P_prop$Var1<=6]))

mercadona_P_prop$color <- ifelse(mercadona_P_prop$Var1 <= 6, 'Detractor'
  ,ifelse(mercadona_P_prop$Var1 <= 8, 'Pasivo', 'Promotor'))

mercadona_NPS_plot <- 
  ggplot(mercadona_P_prop, aes(as.factor(Var1), Freq, fill = color)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c(Detractor='red', 
                               Pasivo = 'yellow', Promotor = 'green')) +
  scale_y_continuous(labels=percent) +
  theme(legend.position="bottom",legend.title=element_blank()) +
  labs(title = paste0('Mercadona NPS ',round(mercadona_nps*100,2),
                      '%. Con Satisfacción Global'),
       x = 'Valoración', y = 'Porcentaje')

mercadona_NPS_plot

# Melteamos los tres NPS para Gráfico conjunto
## Funciona!!
# Día
dia_P_prop <-
  as.data.frame(prop.table(table(dia_P$Ph1_Satisfaccion_Global)))
dia_P_prop$Var1 <- as.integer(dia_P_prop$Var1)
dia_nps <- (sum(dia_P_prop$Freq[dia_P_prop$Var1>=9])
            - sum(dia_P_prop$Freq[dia_P_prop$Var1<=6]))

dia_P_prop$color <- ifelse(dia_P_prop$Var1 <= 6, 'Detractor'
                           ,ifelse(dia_P_prop$Var1 <= 8, 'Pasivo', 'Promotor'))
dia_P_prop$centro <- 
  paste0('Dia, NPS ',round(dia_nps*100,2), '%')

# Carrefour
carrefour_P_prop <-
  as.data.frame(prop.table(table(carrefour_P$Ph1_Satisfaccion_Global)))
carrefour_P_prop$Var1 <- as.integer(carrefour_P_prop$Var1)
carrefour_nps <- (sum(carrefour_P_prop$Freq[carrefour_P_prop$Var1>=9])
                  - sum(carrefour_P_prop$Freq[carrefour_P_prop$Var1<=6]))

carrefour_P_prop$color <- ifelse(carrefour_P_prop$Var1 <= 6, 'Detractor'
                                 ,ifelse(carrefour_P_prop$Var1 <= 8, 'Pasivo', 'Promotor'))
carrefour_P_prop$centro <-
  paste0('Carrefour, NPS ',round(carrefour_nps*100,2), '%')

# Mercadona
mercadona_P_prop <- 
  as.data.frame(prop.table(table(mercadona_P$Ph1_Satisfaccion_Global)))
mercadona_P_prop$Var1 <- as.integer(mercadona_P_prop$Var1)
mercadona_nps <- (sum(mercadona_P_prop$Freq[mercadona_P_prop$Var1>=9])
                  - sum(mercadona_P_prop$Freq[mercadona_P_prop$Var1<=6]))

mercadona_P_prop$color <- ifelse(mercadona_P_prop$Var1 <= 6, 'Detractor'
                                 ,ifelse(mercadona_P_prop$Var1 <= 8, 'Pasivo', 'Promotor'))

mercadona_P_prop$centro <- 
  paste0('Mercadona, NPS ',round(mercadona_nps*100,2), '%')

# resumen
resumen_prop <- dplyr::bind_rows(dia_P_prop, carrefour_P_prop, mercadona_P_prop)

# ordenamos la forma en la que aparecen los centros en el gráfico
resumen_prop$centro <- factor(resumen_prop$centro,
  levels = c(paste0('Dia, NPS ',round(dia_nps*100,2), '%'),
             paste0('Carrefour, NPS ',round(carrefour_nps*100,2), '%'),
             paste0('Mercadona, NPS ',round(mercadona_nps*100,2), '%')))

ggplot(resumen_prop, aes(as.factor(Var1), Freq, fill = color)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ centro, ncol = 1) +
  scale_fill_manual(
    values = c(Detractor='red', Pasivo = 'yellow', Promotor = 'green')) +
  scale_y_continuous(labels=percent) +
  theme(legend.position="bottom",legend.title=element_blank()) +
  labs(x = 'Valoración', y = 'Porcentaje')






# Gráfico de los 3 centros, sólo dejamos la leyenda de mercadona
gridExtra::grid.arrange((dia_NPS_plot +
                           theme(legend.position="none") +
                           theme(axis.title.x=element_blank())), 
                        (carrefour_NPS_plot +
                           theme(legend.position="none") +
                           theme(axis.title.x=element_blank())),
                        mercadona_NPS_plot, nrow = 3, ncol = 1)



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

medias_t <- data.frame(t(medias))
medias_t <- medias_t[-1,]
names(medias_t) <- c('carrefour','dia','mercadona')
medias_t <- medias_t %>% arrange(medias_t, mercadona)



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

# En una columna
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
#   ggtitle('Satisfacción global') +.
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

















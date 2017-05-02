library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(cluster)
library(factoextra)


survey <- read.delim("survey.csv",sep = ';')
#write.table(survey, 'survey.csv',sep=';')
summary(survey)

#survey[,colSums(is.na(survey)) < 41]

#Despues de ver los datos, elegimos eliminar las variables con más de 40 NA

survey <- survey %>%
  dplyr::select(-starts_with('P'), which(colSums(is.na(.)) < 41)) %>% 
  select(Establecimiento, starts_with('P'), starts_with('C'))

## Ajustar
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

## Cluster Jerárquico. Funciona muy bien.

dia_dist <- daisy(select(dia, select(dia, starts_with('P'))))       # daisy works with mixed data types
as.matrix(dia_dist)[1:4, 1:4]   # distances of first 4 observations
dia_segment <- hclust(dia_dist, method = 'ward.D2')
plot(dia_segment)
rect.hclust(dia_segment, k=4, border="red")

dia_segment <- hclust(dia_dist, method = 'ward.D2')
plot(dia_segment)

# Ward Hierarchical Clustering
d <- dist(select(dia, starts_with('P')), method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=4, border="red")




# Segmento
plot(cut(as.dendrogram(dia_segment), h=0.5)$lower[[1]])
# cophenetic correlation coefficient is a measure of how well 
# the clustering model reflects the distance matrix
cor(cophenetic(dia_segment), dia_dist)
# Asignamos 4 cluster en el gráfico
plot(dia_segment)
rect.hclust(dia_segment, k=4, border="red")

dia_cluster <- cutree(dia_segment, k=4)     # membership vector for 4 groups
table(dia_cluster)

aggregate(select(dia, -Establecimiento), by = list(dia_cluster), FUN=mean, na.rm=TRUE)


dia$cluster <- dia_cluster

plot(jitter(dia$Pc1_Actitud) ~ 
     jitter(dia$Ph1_Satisfaccion_Global), 
     col = dia$cluster, yaxt="n", xaxt="n")

plot(jitter(dia$Ph1_Relacion_Calidad_Precio) ~ 
       jitter(dia$Ph1_Satisfaccion_Global), 
     col = dia$cluster, yaxt="n", xaxt="n")

plot(jitter(dia$RC2_Sexo) ~ 
       jitter(dia$RC1_Edad), 
     col = dia$cluster, yaxt="n", xaxt="n")



# Kmeans

#### Cluster
set.seed(1234) # kmeans elije un centro aleatorio
dia_kmeans <- kmeans(select(dia, starts_with('P')),4)

# número de individuos por cluster
table(dia_kmeans$cluster)

set.seed(1234)
dia_kmeans$cluster <- factor(dia_kmeans$cluster, labels=c('Contentos','Fans', 'Descontentos','Detractores'))
# Gráfico con ggplot y % de varianza
factoextra::fviz_cluster(dia_kmeans, data = select(dia, starts_with('P')),
                         ellipse = TRUE) +
  theme_minimal()

# medias por respuesta
aggregate(select(dia, -Establecimiento, -starts_with('C')), 
          by = list(dia_kmeans$cluster), FUN=mean, na.rm=TRUE)

# gráficos
ggplot(dia,
       aes(Ph1_Relacion_Calidad_Precio, Ph1_Satisfaccion_Global,
           color = factor(dia_kmeans$cluster))) +
  geom_jitter() +
  theme(legend.position="bottom",legend.title=element_blank())


ggplot(dia,
       aes(C2_Sexo, Ph1_Satisfaccion_Global,
           color = factor(dia_kmeans$cluster))) +
  geom_jitter() +
  theme(legend.position="bottom",legend.title=element_blank())


ggplot(dia,
       aes(C4_Compra_Media, Ph1_Satisfaccion_Global,
           color = factor(dia_kmeans$cluster))) +
  geom_jitter() +
  theme(legend.position="bottom",legend.title=element_blank())


# Creamos levels en dia_kmeans$cluster




# Gráfico cluster sin ggplot
# cluster::clusplot(select(dia, starts_with('P')),
#                   dia_kmeans$cluster, color=TRUE, shade=TRUE, 
#                   labels=4, lines=0, main="K-means cluster plot")




# #### Cluster
# #Seleccionamos variables p y le quitamos los NA
# survey_clust <- survey %>% 
#   select(Establecimiento, starts_with('P')) %>% 
#   na.omit()
# 
# clust_result <- kmeans(survey_clust[,-1],3)
# 
# table(survey_clust$Establecimiento, clust_result$cluster)
# 
# plot(survey_clust[,34:35], col = clust_result$cluster)
# 
# plot(survey_clust[,34:35], col = survey_clust$Establecimiento)
# 
# ggplot(survey_clust,
#        aes(Ph1_Relacion_Calidad_Precio, Ph1_Satisfaccion_Global, color = factor(clust_result$cluster))) +
#   geom_jitter()
# 
# ggplot(survey_clust,
#        aes(Ph1_Relacion_Calidad_Precio, Ph1_Satisfaccion_Global, color = Establecimiento)) +
#   geom_jitter()
# 











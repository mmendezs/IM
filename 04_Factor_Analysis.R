# install.packages("FactoMineR")
# install.packages('factoextra')


library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(stringr)
library(FactoMineR)
library(factoextra)



survey <- read.delim("survey.csv",sep = ';')
#write.table(survey, 'survey.csv',sep=';')
summary(survey)

#survey[,colSums(is.na(survey)) < 41]

#Despues de ver los datos, elegimos eliminar las variables con más de 40 NA

survey <- survey %>%
  dplyr::select(-starts_with('P'), which(colSums(is.na(.)) < 41)) %>% 
  select(Establecimiento, starts_with('P'), starts_with('C'))

survey$Establecimiento <- 
  factor(survey$Establecimiento, labels = c('carrefour','dia','mercadona'))

# Separamos datos por supermercados
dia <- filter(survey, Establecimiento=='dia')
carrefour <- filter(survey, Establecimiento=='carrefour')
mercadona <- filter(survey, Establecimiento=='mercadona')


# PCA All
survey_pca <- PCA(na.omit(select(survey, starts_with('P'))))
print(survey_pca)
(survey_pca$eig)
fviz_screeplot(survey_pca)
survey_pca$var$coord

# PCA Día
dia_pca <- PCA(na.omit(select(dia, starts_with('P'))))
print(dia_pca)
(dia_pca$eig)
fviz_screeplot(dia_pca)
dia_pca$var$coord

# PCA Carrefour
carrefour_pca <- PCA(na.omit(select(carrefour, starts_with('P'))))
print(carrefour_pca)
(carrefour_pca$eig)
fviz_screeplot(carrefour_pca)
carrefour_pca$var$coord

# PCA Mercadona
mercadona_pca <- PCA(na.omit(select(mercadona, starts_with('P'))))
print(mercadona_pca)
(mercadona_pca$eig)
fviz_screeplot(mercadona_pca)
mercadona_pca$var$coord

# Quality Cos2
head(mercadona_pca$var$cos2)


ggplot(survey, aes(Ph1_Satisfaccion_Global, RC4_Compra_Media,
                   color = Establecimiento)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

ggplot(mercadona, aes(Ph1_Satisfaccion_Global, RC4_Compra_Media)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines


# # install.packages("devtools")
# devtools::install_github("kassambara/factoextra")



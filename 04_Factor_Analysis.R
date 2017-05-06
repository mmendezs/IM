
#library(scales)
#library(reshape2)
#library(stringr)
# library(FactoMineR)
# library(factoextra)
library(dplyr)
library(ggplot2)
library(psych)

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
survey_principal
# Elegimos 4 factores sin rotar
# PCA con todas las columnas para elegir cuantos factores y varianza explicada
survey_principal <- psych::principal(select(survey, starts_with('P')),
  nfactors = 4, rotate = 'none')
# Estudiamos las comunalidades
survey_principal$communality
# Extraemos las puntuaciones
survey_principal_cargas <- as.data.frame(unclass(survey_principal$loadings))
# Añadimos columnas con los nombres de las variables
survey_principal_cargas$variables <- rownames(survey_principal_cargas)
# Reordenamos las columnas
survey_principal_cargas <- select(survey_principal_cargas, variables, starts_with('P'))
# Reordenamos las puntuaciones en base a PC1
survey_principal_cargas <- arrange(survey_principal_cargas, -PC1)

print(survey_principal_cargas, digits = 2)

# Cogemos los scores de cada individuo
survey_principal_scores <- as.data.frame(unclass(survey_principal$scores))
# Les incluimos el establecimiento
survey_principal_scores$Establecimiento <- survey$Establecimiento
# Agrupamos los scores por establecimiento

survey_principal_scores <- survey_principal_scores %>% 
  group_by(Establecimiento)

ggplot(data = survey_principal_scores, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = factor(Establecimiento)), alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = survey_principal_cargas, 
            aes(PC1, PC2, label = variables),check_overlap = TRUE) +
  theme(legend.position="bottom",legend.direction="horizontal") +
  theme(legend.title = element_blank()) +
  ggtitle("Análisis de Correspondecias Múltiple, incluyendo individuos") +
  scale_colour_discrete(name = "Variable")

# ROTAMOS

survey_principal_rot <- psych::principal(select(survey, starts_with('P')),
                                     nfactors = 4, rotate = 'varimax')


survey_principal_cargas_rot <- as.data.frame(unclass(survey_principal_rot$loadings))
survey_principal_cargas_rot$variables <- rownames(survey_principal_cargas_rot)
survey_principal_cargas_rot <- select(survey_principal_cargas_rot, variables, starts_with('R'))
# Reordenamos en base a PC1
survey_principal_cargas_rot <- arrange(survey_principal_cargas_rot, -RC1)

##### Aquí nos quedamos....

a <- survey_principal_cargas_rot
a[a[]<0.4] <- 0


print(survey_principal_cargas_rot, digits = 2)


set.seed(1234)






## psych & Base R
survey_Base_prin <- princomp(select(survey, starts_with('P')),4, scores = TRUE)
survey_Base_pr <- prcomp(select(survey, starts_with('P')),4, scale = FALSE)
screeplot(survey_Base_pr) # Variance
screeplot(survey_Base_prin) # std deviation

psych::fa.sort(survey_principal)

 

nfactors(select(survey, starts_with('P')), n= 20, rotate = 'varimax')



## FactoMineR
survey_principal <- principal(select(survey, starts_with('P')),4 ,rotate = 'varimax',
                              scores = TRUE)
a <- cor(select(survey, starts_with('P')))
VSS.plot(a)
survey_principal$scores
# Para extraer varianza
a <- print(survey_principal)
round(a$Vaccounted,2)
cor(survey_principal$scores) 
## FactoMineR
# PCA All
survey_pca <- PCA(select(survey, starts_with('P')))
print(survey_pca)
prcomp(select(survey, starts_with('P')), rotate = TRUE)
(survey_pca$eig)
fviz_screeplot(survey_pca)
a <- survey_pca$var$coord
b <-t(apply(survey_pca$var$coord, 1, function(x) {x/sqrt(survey_pca$eig[,1])}))
head(sweep(survey_pca$var$coord, 2, sqrt(survey_pca$eig[,1]),'/'))

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


# Rotación
seed(123)
sim_data <-  as.data.frame(MASS::mvrnorm(
  n=100, mu=c(0, 0), Sigma=matrix(c(1, 0.8, 0.8, 1), nrow=2), empirical=TRUE))

names(sim_data) <- c('x', 'y')

line1 <- data.frame(x = seq(from = -2, to = 2, by = 1/100),
                    y = seq(from = -2, to = 2, by = 1/100))
line2 <- data.frame(x = seq(from = -1, to = 1, by = 1/100),
                    y = -seq(from = -1, to = 1, by = 1/100))

ggplot(sim_data, aes(x, y)) +
  geom_point() +
  geom_line(data = line1, aes(x, y)) +
  geom_line(data = line2, aes(x, y))

rotate <- function(df, degree) {
  dfr <- df
  degree <- pi * degree / 180
  l <- sqrt(df$start1^2 + df$start2^2)
  teta <- atan(df$start2 / df$start1)
  dfr$start1 <- round(l * cos(teta - degree))
  dfr$start2 <- round(l * sin(teta - degree))
  return(dfr)
}

sim_data1 <- rotate(sim_data, -90)

sim_data1 <- sim_data
degree <- 45
degree <- pi * degree / 180
l <- sqrt(sim_data1$x^2 + sim_data$y^2)
teta <- atan(sim_data$y / sim_data1$x)
sim_data1$x <- l * cos(teta - degree)
sim_data1$y <- l * sin(teta - degree)

ggplot(sim_data1, aes(x, y)) +
  geom_point() +
  geom_line(data = line1, aes(x, y)) +
  geom_line(data = line2, aes(x, y))

factor.rotate(sim_data, 45, col1=sim_data$x, col2=sim_data$y, plot=TRUE)

data(Harman23.cor)
f2 <- fa(Harman23.cor$cov,2,rotate="none")
op <- par(mfrow=c(1,2))
cluster.plot(f2,xlim=c(-1,1),ylim=c(-1,1),title="Unrotated ")
f2r <- factor.rotate(f2,-33,plot=TRUE,xlim=c(-1,1),ylim=c(-1,1),title="rotated -33 degrees")
op <- par(mfrow=c(1,1))




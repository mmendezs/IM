# https://www.tutorialspoint.com/r/r_chi_square_tests.htm
# Chi-Square test is a statistical method to determine if two categorical
# variables have a significant correlation between them
# Two random variables x and y are called independent if the probability distribution 
# of one variable is not affected by the presence of another.

# para pasar a factor, pero no es necesario para el chi
# library(magrittr)
# # # Pasar variables P factores
# survey %<>% 
#  mutate_each_(funs(factor(.)),starts_with('P'))

library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(stringr)
library(gridExtra)
library(corrplot)

survey <- read.delim("survey.csv",sep = ';')

# Recodificamos las variables de clasificación

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


# Separamos datos por supermercados
dia <- filter(survey, Establecimiento=='dia')
carrefour <- filter(survey, Establecimiento=='carrefour')
mercadona <- filter(survey, Establecimiento=='mercadona')
# Seleccionamos las preguntas sobre el establecimiento

tbl <- table(survey$Pc1_Actitud,survey$Establecimiento)
chisq.test(tbl)

av <- aov(Pc1_Actitud ~ Establecimiento, survey)
summary(av)

satisfaccion_av <- aov(Ph1_Satisfaccion_Global ~ Establecimiento, survey)
summary(satisfaccion_av)

asesoramiento_av <- aov(Pd1_Asesoramiento_Oferta ~ Establecimiento, survey)
summary(asesoramiento_av)


# Tukey comparación medias entre todas
calidad_prec_av <- aov(Ph1_Relacion_Calidad_Precio ~ Establecimiento, survey)
summary(calidad_prec_av)
aa <- TukeyHSD(calidad_prec_av, 'Establecimiento', ordered = TRUE)
plot(TukeyHSD(calidad_prec_av, 'Establecimiento'))

## Tukey con ggplot
aa <- data.frame(TukeyHSD(calidad_prec_av, 'Establecimiento')$Establecimiento)
aa$comparacion <- row.names(aa)

ggplot(aa, aes(comparacion, y = diff, ymin = lwr, ymax = upr, color = comparacion)) +
  geom_pointrange() +
  labs(title = 'Intervalo de confianza al 95%', y ='Diferencias', x = '' ) +
  theme(legend.position = 'none') +
  coord_flip()

## Día Edades Satisfacción
# Anova salen iguales
dia_edad_satisfacc_anova <- aov(Ph1_Satisfaccion_Global ~ C1_Edad, dia, ordered = TRUE)
summary(dia_edad_satisfacc_anova)

dia_edad_satisfacc_anova <- data.frame(TukeyHSD(dia_edad_satisfacc_anova, 'C1_Edad')$C1_Edad)
dia_edad_satisfacc_anova$comparacion <- row.names(dia_edad_satisfacc_anova)

ggplot(dia_edad_satisfacc_anova, aes(comparacion, y = diff, ymin = lwr, ymax = upr, color = comparacion)) +
  geom_pointrange() +
  labs(title = 'Intervalo de confianza al 95%', y ='Diferencias', x = '' ) +
  theme(legend.position = 'none') +
  coord_flip()

## Ingreso
dia_ingreso_satisfacc_anova <- aov(Ph1_Satisfaccion_Global ~ C5_Ingr_Mes, dia, ordered = TRUE)
summary(dia_ingreso_satisfacc_anova)

dia_ingreso_satisfacc_anova <- data.frame(TukeyHSD(dia_ingreso_satisfacc_anova, 'C5_Ingr_Mes')$C5_Ingr_Mes)
dia_ingreso_satisfacc_anova$comparacion <- row.names(dia_ingreso_satisfacc_anova)

ggplot(dia_ingreso_satisfacc_anova, aes(comparacion, y = diff, ymin = lwr, ymax = upr, color = comparacion)) +
  geom_pointrange() +
  labs(title = 'Intervalo de confianza al 95%', y ='Diferencias', x = '' ) +
  theme(legend.position = 'none') +
  coord_flip()


# Chi
tbl <- table(survey$Pc1_Actitud,survey$Establecimiento)
chisq.test(tbl)

## Correlation corrplot

col1 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","white", 
                           "cyan", "#007FFF", "blue","#00007F"))
# Survey
corrplot(cor(select(survey, starts_with('P'))),
         title = "Matriz de correlación", mar=c(0,0,1,0),
         method = "color", outline = T, addgrid.col = "darkgray",
         order = "hclust", addrect = 8, col=col1(100),
         tl.col='black', tl.cex=.75)
# Dia
corrplot(cor(select(dia, starts_with('P'))),
         title = "Matriz de correlación Dia", mar=c(0,0,1,0),
         method = "color", outline = T, addgrid.col = "darkgray",
         order = "hclust", addrect = 8, col=col1(100),
         tl.col='black', tl.cex=.75)
#dev.off()
# Carrefour
corrplot(cor(select(carrefour, starts_with('P'))),
         title = "Matriz de correlación Carrefour", mar=c(0,0,1,0),
         method = "color", outline = T, addgrid.col = "darkgray",
         order = "hclust", addrect = 8, col=col1(100),
         tl.col='black', tl.cex=.75)

# Mercadona
corrplot(cor(select(mercadona, starts_with('P'))),
         title = "Matriz de correlación Mercadona", mar=c(0,0,1,0),
         method = "color", outline = T, addgrid.col = "darkgray",
         order = "hclust", addrect = 8, col=col1(100),
         tl.col='black', tl.cex=.75)
dev.off() #dado que hemos tocado el mar() reseteamos graph device


### Matriz de Correlación a Mano #############################
## Correlation matrix
# survey_cor <- round(cor(select(survey, starts_with('P'))),2)
# survey_cor_melt <- melt(survey_cor)
# survey_cor_melt$value <- survey_cor_melt$value*100
# 
# ggplot(survey_cor_melt, aes(Var1, Var2, fill=value)) +
#   geom_tile(aes(fill = value), colour = "white") +
#   geom_text(aes(label = sprintf("%1.0f",value)), vjust = 0.5) +
#   scale_fill_gradient(low = "white", high = "dodgerblue4") +
#   theme(axis.text.x=element_text(angle=90, hjust=1)) +
#   theme(legend.position="none",legend.title=element_blank()) +
#   ggtitle('Matriz de correlación (%)') +
#   labs(x = '', y = "")
# 
# # Dia correlation
# dia_cor <- round(cor(select(dia, starts_with('P'))),2)
# dia_cor_melt <- melt(dia_cor)
# dia_cor_melt$value <- dia_cor_melt$value*100
# 
# ggplot(dia_cor_melt, aes(Var1, Var2, fill=value)) +
#   geom_tile(aes(fill = value), colour = "white") +
#   geom_text(aes(label = sprintf("%1.0f",value)), vjust = 0.5) +
#   scale_fill_gradient(low = "white", high = "dodgerblue4") +
#   theme(axis.text.x=element_text(angle=90, hjust=1)) +
#   theme(legend.position="none",legend.title=element_blank()) +
#   ggtitle('Matriz de correlación (%)') +
#   labs(x = '', y = "")
# 
# # carrefour correlation
# carrefour_cor <- round(cor(select(carrefour, starts_with('P'))),2)
# carrefour_cor_melt <- melt(carrefour_cor)
# carrefour_cor_melt$value <- carrefour_cor_melt$value*100
# 
# ggplot(carrefour_cor_melt, aes(Var1, Var2, fill=value)) +
#   geom_tile(aes(fill = value), colour = "white") +
#   geom_text(aes(label = sprintf("%1.0f",value)), vjust = 0.5) +
#   scale_fill_gradient(low = "white", high = "dodgerblue4") +
#   theme(axis.text.x=element_text(angle=90, hjust=1)) +
#   theme(legend.position="none",legend.title=element_blank()) +
#   ggtitle('Matriz de correlación (%)') +
#   labs(x = '', y = "")
# 
# # mercadona correlation
# mercadona_cor <- round(cor(select(mercadona, starts_with('P'))),2)
# mercadona_cor_melt <- melt(mercadona_cor)
# mercadona_cor_melt$value <- mercadona_cor_melt$value*100
# 
# ggplot(mercadona_cor_melt, aes(Var1, Var2, fill=value)) +
#   geom_tile(aes(fill = value), colour = "white") +
#   geom_text(aes(label = sprintf("%1.0f",value)), vjust = 0.5) +
#   scale_fill_gradient(low = "white", high = "dodgerblue4") +
#   theme(axis.text.x=element_text(angle=90, hjust=1)) +
#   theme(legend.position="none",legend.title=element_blank()) +
#   ggtitle('Matriz de correlación (%)') +
#   labs(x = '', y = "")



























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

tbl <- table(survey$Pc1_Actitud,survey$Establecimiento)
chisq.test(tbl)

av <- aov(Pc1_Actitud ~ Establecimiento, survey)
summary(av)

satisfaccion_av <- aov(Ph1_Satisfaccion_Global ~ Establecimiento, survey)
summary(satisfaccion_av)

asesoramiento_av <- aov(Pd1_Asesoramiento_Oferta ~ Establecimiento, survey)
summary(asesoramiento_av)

aspectos_av <- aov(Pd1_Aspectos_Positivos_Negativos ~ Establecimiento, survey)
summary(aspectos_av)

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
  coord_flip()

## Ingreso
dia_ingreso_satisfacc_anova <- aov(Ph1_Satisfaccion_Global ~ C5_Ingr_Mes, dia, ordered = TRUE)
summary(dia_ingreso_satisfacc_anova)

dia_ingreso_satisfacc_anova <- data.frame(TukeyHSD(dia_ingreso_satisfacc_anova, 'C5_Ingr_Mes')$C5_Ingr_Mes)
dia_ingreso_satisfacc_anova$comparacion <- row.names(dia_ingreso_satisfacc_anova)

ggplot(dia_ingreso_satisfacc_anova, aes(comparacion, y = diff, ymin = lwr, ymax = upr, color = comparacion)) +
  geom_pointrange() +
  labs(title = 'Intervalo de confianza al 95%', y ='Diferencias', x = '' ) +
  coord_flip()



# Chi
tbl <- table(survey$Pc1_Actitud,survey$Establecimiento)
chisq.test(tbl)


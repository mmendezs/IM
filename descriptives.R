library(readr)
library(ggplot2)
library(dplyr)
survey <- read_delim("survey.csv",
                     ";", escape_double = FALSE, trim_ws = TRUE)

survey$Establecimiento <- factor(survey$Establecimiento, labels = c('carrefour','dia','mercadona'))

survey$C1_Edad <- factor(survey$C1_Edad,
                         labels = c('18-24','25-35','35-44','45-54','55-64','>64'))
survey$C2_Sexo <- factor(survey$C2_Sexo,
                         labels = c('hombre','mujer'))
survey$C3_Estado_Civil <- factor(survey$C3_Estado_Civil,
                                 labels = c('soltero/a','casado/a','unido/a','separado/a','viudo/a'))
survey$C4_Importe_Medio_Compra <- factor(survey$C4_Importe_Medio_Compra,
                                         labels = c('<15','15-30','31-45','46-60',
                                         '61-75','76-90','91-120','>120'))
survey$C5_Ingresos_Mensuales_Totales <- factor(survey$C5_Ingresos_Mensuales_Totales,
                                               labels = c('<600','601-1000','1001-1500',
                                                          '1501-2000','2001-3000',
                                                          '3001-4500','>4500'))
par(mfrow=c(2,2))
boxplot(survey$Ph1_Satisfaccion_Global~survey$C1_Edad)
boxplot(survey$Ph1_Satisfaccion_Global~survey$C3_Estado_Civil)
boxplot(survey$Ph1_Satisfaccion_Global~survey$C4_Importe_Medio_Compra)
boxplot(survey$Ph1_Satisfaccion_Global~survey$C5_Ingresos_Mensuales_Totales)

par(mfrow=c(1,1))

boxplot((survey$Ph1_Satisfaccion_Global~survey$Establecimiento))

summarise(survey,Establecimiento,Ph1_Satisfaccion_Global)

table(survey$Ph1_Satisfaccion_Global,survey$Establecimiento)/colSums(survey$Ph1_Satisfaccion_Global)

dia <- filter(survey, Establecimiento=='dia')
carrefour <- filter(survey, Establecimiento=='carrefour')
mercadona <- filter(survey, Establecimiento=='mercadona')

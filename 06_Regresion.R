
library(MASS) # para step regression
library(dplyr)
library(ggplot2)
library(scales)
#library(caret) no vale sólo usa abs(t)
library(relaimpo)


survey <- read.delim("survey.csv",sep = ';')
survey <- survey %>%
  dplyr::select(-starts_with('P'), which(colSums(is.na(.)) < 41)) %>% 
  select(Establecimiento, starts_with('P'), starts_with('C'))
# Replace NA with median
survey[] <- lapply(survey, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
# Recode
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

# Todos los establecimientos
# Creamos la fórmula
f <- as.formula(
  paste('Ph1_Satisfaccion_Global ~',
        paste(colnames(select(survey, starts_with('P'), -Ph1_Satisfaccion_Global))
              , collapse='+')))

survey_fit <- lm(f, data= survey)



summary(survey_fit)
# Realizamos la regresión en pasos con el criterio de Aqaique
survey_fit_step <- MASS::stepAIC(survey_fit, direction = 'both')
survey_fit_step$anova

# Devuelve un modelo de corta pega y repetimos regresión
survey_fit_after_step <- lm(formula(survey_fit_step), data = survey)
# sale un modelo bastante bien
summary(survey_fit_after_step)
# Importancia relativa
a <- relaimpo::calc.relimp(survey_fit_after_step)

# Dia
# f <- as.formula(
#   paste('Ph1_Satisfaccion_Global ~',
#         paste(colnames(select(survey, starts_with('P'), -Ph1_Satisfaccion_Global))
#               , collapse='+')))

dia_fit <- lm( as.formula(
  paste('Ph1_Satisfaccion_Global ~',
        paste(colnames(select(survey, starts_with('P'), -Ph1_Satisfaccion_Global))
              , collapse='+'))) , data= dia)

summary(dia_fit)
# Realizamos la regresión en pasos con el criterio de Akaike
dia_fit_step <- MASS::stepAIC(survey_fit, direction = 'both')
dia_fit_step$anova
summary(dia_fit_step)

# Para extraer la formula del step fit

dia_fit_after_step <- lm(data = dia, formula(dia_fit_step))

summary(dia_fit_after_step)
a <- relaimpo::calc.relimp(dia_fit_after_step)
plot(a)

b <- data.frame(a$lmg)
b$variables <- rownames(b)

ggplot(b, aes(reorder(as.factor(variables), a.lmg), a.lmg)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()

#gráfico con predicción
dia$model <- predict(b, newdata = dia)

ggplot(dia) +
  geom_jitter(aes(x=Ph1_Satisfaccion_Global, y =model)) 


# Super bueno, explica la explicación del R2
c <- caret::varImp(dia_fit_step, scale=TRUE)
cumsum(c)
# dividimos por R2 pendiente entender que es varIMP
d <- c/sum(c)*100
cumsum(d)
par(mfrow = c(2, 2))
plot(dia_fit_step)

par(mfrow = c(1, 1))








































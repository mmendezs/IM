
library(MASS) # para step regression
library(dplyr)
library(ggplot2)
library(scales)
library(car)
library(relaimpo)

## Dejar warnings!!
## Por problemas de compatibilidad del comando de dplyr select con MASS se recomienda cargar primero este
## tambien en usar siempre ddplyr::select en vez de sólo select

survey <- read.delim("survey.csv",sep = ';')
# Separamos datos por supermercados
dia <- filter(survey, Establecimiento=='dia')
carrefour <- filter(survey, Establecimiento=='carrefour')
mercadona <- filter(survey, Establecimiento=='mercadona')

# Todos los establecimientos y Satisfacción Global (SG)
survey_fit_SG <- lm(as.formula(
  paste('Ph1_Satisfaccion_Global ~',
        paste(colnames(dplyr::select(survey, starts_with('P'), -Ph1_Satisfaccion_Global))
              , collapse='+'))), data= survey)
summary(survey_fit_SG)
# Realizamos la regresión en pasos con el criterio de Akaike
survey_fit_SG_step <- MASS::stepAIC(survey_fit_SG, direction = 'both')
summary(survey_fit_SG_step)
# Importancia relativa
survey_fit_SG_step_relaimp <- data.frame(relaimp = relaimpo::calc.relimp(survey_fit_SG_step, rela = TRUE, rank = TRUE)$lmg)
survey_fit_SG_step_relaimp$variables <- rownames(survey_fit_SG_step_relaimp)
dplyr::arrange(survey_fit_SG_step_relaimp,desc(relaimp))
# Gráfico Importancia Relativa
ggplot(survey_fit_SG_step_relaimp, aes(reorder(as.factor(variables), relaimp), relaimp)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Importancia relativa de cada variable',
       subtitle = 'Var. dep. Satisfacción Global' , x = '', y = '') +
  coord_flip()

## Aquí nos quedamos. Revisar modelo, valores negativos
# Sólo Recodificadas
survey_fit_IM <- lm(as.formula(
  paste('RC4_Compra_Media ~',
        paste(colnames(dplyr::select(survey, starts_with('R'),-RC4_Compra_Media))
              , collapse='+'))), data= survey)
summary(survey_fit_IM)

survey_fit_IM_relaimp <- data.frame(relaimp = relaimpo::calc.relimp(survey_fit_IM, rela = TRUE, rank = TRUE)$lmg)
survey_fit_IM_relaimp$variables <- rownames(survey_fit_IM_relaimp)
dplyr::arrange(survey_fit_IM_relaimp,desc(relaimp))
# Gráfico Importancia Relativa
ggplot(survey_fit_IM_relaimp, aes(reorder(as.factor(variables), relaimp), relaimp)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Importancia relativa de cada variable',
       subtitle = 'Var. dep. Satisfacción Global' , x = '', y = '') +
  coord_flip()


# Todos los establecimientos e Importe Medio de la Compra (IM)
survey_fit_IM <- lm(as.formula(
  paste('RC4_Compra_Media ~',
        paste(colnames(dplyr::select(survey, starts_with('P'), starts_with('R'),-RC4_Compra_Media))
              , collapse='+'))), data= survey)
summary(survey_fit_IM)
# Realizamos la regresión en pasos con el criterio de Akaike
survey_fit_IM_step <- MASS::stepAIC(survey_fit_IM, direction = 'both')
summary(survey_fit_IM_step)

survey_fit_IM_step_revisada <- lm(formula = RC4_Compra_Media ~ Pc1_Actitud_Servicio + Pd1_Asesoramiento_Oferta + 
                                    Pe1_Duracion_Fila_Cajas +  
                                    Pf1_Amplitud_Oferta_Productos + Pg1_N_Empleados_Adecuado + 
                                    Pg1_Decoracion_Establecimiento + Pg1_Sensacion_Orden_Limpieza + 
                                    Ph1_Satisfaccion_Global + RC1_Edad + 
                                    RC2_Sexo + RC5_Ingr_Mes, data = survey)
summary(survey_fit_IM_step_revisada)
# 2ª fase Sale bien
survey_fit_IM_step_revisada <- lm(formula = RC4_Compra_Media ~ Pe1_Duracion_Fila_Cajas +  
                                    Pf1_Amplitud_Oferta_Productos +  
                                    Pg1_Decoracion_Establecimiento + Pg1_Sensacion_Orden_Limpieza + 
                                    RC1_Edad + 
                                    RC2_Sexo + RC5_Ingr_Mes, data = survey)
summary(survey_fit_IM_step_revisada)

##VIF y Durbin
car::vif(survey_fit_IM_step_revisada)
car::durbinWatsonTest(survey_fit_IM_step_revisada)
# Importancia relativa revisada

#Importancia relativa, la aportación de cada variable la suma da la R2
survey_fit_IM_step_relaimp <- data.frame(relaimp = relaimpo::calc.relimp(survey_fit_IM_step_revisada)$lmg)
## mostrar dato relaimp
# Importancia relativa relativizada
survey_fit_IM_step_relaimp <- data.frame(relaimp = relaimpo::calc.relimp(survey_fit_IM_step_revisada, rela = TRUE, rank = TRUE)$lmg)

survey_fit_IM_step_relaimp$variables <- rownames(survey_fit_IM_step_relaimp)
dplyr::arrange(survey_fit_IM_step_relaimp,desc(relaimp))
# Gráfico Importancia Relativa
ggplot(survey_fit_IM_step_relaimp, aes(reorder(as.factor(variables), relaimp), relaimp)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  scale_y_continuous(labels = scales::percent) +
  labs(title = 'Importancia relativa de cada variable',
       subtitle = 'Var. dep. Satisfacción Global' , x = '', y = '') +
  coord_flip()




# Dia
dia_fit <- lm( as.formula(
  paste('Ph1_Satisfaccion_Global ~',
        paste(colnames(dplyr::select(survey, starts_with('P'), -Ph1_Satisfaccion_Global))
              , collapse='+'))) , data= dia)

summary(dia_fit)
# Realizamos la regresión en pasos con el criterio de Akaike
dia_fit_step <- MASS::stepAIC(dia_fit, direction = 'both')
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
  labs(title = 'Importancia relativa de cada variable',
       subtitle = 'Var. dep. Satisfacción Global' , x = '', y = '') +
  coord_flip()

#gráfico con predicción
dia$model <- predict(b, newdata = dia)

ggplot(dia) +
  geom_jitter(aes(x=Ph1_Satisfaccion_Global, y =model)) 









































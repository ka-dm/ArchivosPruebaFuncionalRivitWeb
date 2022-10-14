# Definiendo la carpeta de trabajo
setwd("/home/kevin/Documents/Trabajo de Grado 2022/PruebaFuncional")
# Revisando que se cambio la ruta de la carpeta
getwd()

################################################################################

# Instalar paquetes
install.packages("readr")
install.packages("dplyr")
install.packages("readxl")
install.packages("psych")
install.packages("ggplot2")

# Librerias
## Cargar datos .txt
library(readr)
## Para manipular (crear, transformar, filtrar variables)
library(dplyr)
## Para graficar
library(ggplot2)
## Para generar estadísticas descriptivas
library(psych)
## Para trabajar con variables categóricas (factores)
library(forcats)

# Datos ###############################################################################

DatosEvalFun <- read_delim("DatosEvalFun.csv", 
                           delim = ";", escape_double = FALSE, col_types = cols(Id = col_integer(), 
                                                                                Q1 = col_integer(), Q2 = col_integer(), 
                                                                                Q3 = col_integer(), ...10 = col_skip()), 
                           trim_ws = TRUE)
View(DatosEvalFun)

datos_tarea_1 = DatosEvalFun %>% 
 filter(`Tarea` == 'Tarea 1')

View(datos_tarea_1)

mean_t1_q1 = mean(datos_tarea_1$Q1) 
mean_t1_q2 = mean(datos_tarea_1$Q2) 
mean_t1_q3 = mean(datos_tarea_1$Q3) 

# Graficas ###############################################################################


df <- data.frame(dose=c("Q1", "Q2", "Q3"),
                 len=c(mean_t1_q1, mean_t1_q2, mean_t1_q3))

# Basic barplot
p<-ggplot(data=df, aes(x=dose, y=len, fill=dose)) +
  geom_bar(stat="identity", width=0.5)
p


# Ejemplos de graficas ###################################################################

# create a dataset
tareas <- c(rep("T1" , 5) , rep("T2" , 5) , rep("T3" , 5) , rep("T4" , 5) )
escala <- rep(c("r1" , "r2", "r3" , "r4" , "r5") , 4)
frecuencia <- abs(rnorm(20 , 0 , 20))
data_q1 <- data.frame(tareas,tareas,frecuencia)

# Grouped
ggplot(data_q1, aes(fill=escala, y=frecuencia, x=tareas)) + 
  geom_bar(position="dodge", stat="identity")


##

dt = DatosEvalFun %>% 
  group_by(Tarea, )

ggplot(DatosEvalFun, aes(x = Tarea, color = Q1)) + 
  geom_bar()


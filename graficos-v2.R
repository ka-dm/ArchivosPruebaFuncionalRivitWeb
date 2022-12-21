
# Instalar paquetes
install.packages("readr")
install.packages("dplyr")
install.packages("readxl")
install.packages("psych")
install.packages("ggplot2")
install.packages('forecast', dependencies = TRUE)

# Definiendo la carpeta de trabajo
setwd("/home/uriel/Documentos/R.-Studio-Workspace/ArchivosPruebaFuncionalRivitWeb")
# Revisando que se cambio la ruta de la carpeta
getwd()

################################################################################


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
library(tidyr)

# Datos ###############################################################################

DatosEvalFun <- read_delim("DatosEvalFun-10resp.csv", 
                           delim = ",", escape_double = FALSE, col_types = cols(Id = col_integer(), 
                                                                                Q1 = col_integer(), Q2 = col_integer(), 
                                                                                Q3 = col_integer(), ...10 = col_skip()), 
                           trim_ws = TRUE)
View(DatosEvalFun)

datos_tarea_1 = DatosEvalFun %>% 
 filter(`Tarea` == 'Tarea 1')

View(datos_tarea_1)

q1_sort = sort(datos_tarea_1$Q1)
q2_sort = sort(datos_tarea_1$Q2)
q3_sort = sort(datos_tarea_1$Q3)

mediana_t1_q1 = median(q1_sort)
mediana_t1_q2 = median(q2_sort)
mediana_t1_q3 = median(q3_sort)

datos_medianas = DatosEvalFun %>%
  select(Tarea, Q1, Q2, Q3) %>% 
  group_by(Tarea) %>% 
  summarise(mediana_q1 = median(sort(Q1)), mediana_q2 = median(sort(Q2)), mediana_q3 = median(sort(Q3)))
  
View(datos_medianas)

datos_mediana_medianas_q1 = median(sort(datos_medianas$mediana_q1)) 
datos_mediana_medianas_q1
  


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


## Plots preguntas prueba funcional

Q1 %>%  = DatosEvalFun %>% 
  group_by(Tarea, Q1 %>% ) %>% 
  summarise(conteo = n()) %>% 
  ungroup() %>% 
  rename(respuesta = Q1 %>% ) %>% 
  complete(nesting(Tarea), respuesta = seq(1,5))
  expand(nesting(Tarea, conteo), respuesta = seq(1,5))

  plotQ1 <- ggplot(Q1 %>% , aes(x = Tarea, y = conteo, fill = as.factor(respuesta))) + 
    geom_bar(position="dodge", stat = "identity") + 
    labs(fill = "Escala") +
    ylim(0, 10) +
    scale_fill_manual(values=c("#F24405", "#F2911B","#F2CF1D","#A3BF3F","#5A7340")) +
    labs(
      title = "Resultados de la pregunta 1:",
      subtitle = "\"En general, estoy satisfecho con la facilidad para completar esta tarea.\"",
      x = "Tareas",
      y = "Frecuencia"
    )
  
  plotQ1 + theme_classic() + theme(
    panel.grid.major = element_line(colour = "gray", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

  # Calculo de mediana para la pregunta 1
  Q1Count <- data.frame(
    Category  = c (Q1$respuesta), 
    Frequency= replace(Q1$conteo, is.na(Q1$conteo), 0)
  )
  
  respQ1 = aggregate(x= Q1Count$Frequency,              
                   by = list(Q1Count$Category),        
                   FUN = sum)  
  respQ1
  median(respQ1$x)
  # -------------------------------------
  
  
Q2 = DatosEvalFun %>% 
  group_by(Tarea, Q2) %>% 
  summarise(conteo = n()) %>% 
  ungroup() %>% 
  rename(respuesta = Q2) %>% 
  complete(nesting(Tarea), respuesta = seq(1,5))
  expand(nesting(Tarea, conteo), respuesta = seq(1,5))

plotQ2 <- ggplot(Q2, aes(x = Tarea, y = conteo, fill = as.factor(respuesta))) + 
  geom_bar(position="dodge", stat = "identity") + 
  labs(fill = "Escala") +
  ylim(0, 10) +
  scale_fill_manual(values=c("#F24405", "#F2911B","#F2CF1D","#A3BF3F","#5A7340")) +
  labs(
    title = "Resultados de la pregunta 2:",
    subtitle = "\"En general, estoy satisfecho con la cantidad de tiempo que \ntomó completar esta tarea.\"",
    x = "Tareas",
    y = "Frecuencia"
  )

plotQ2 + theme_classic() + theme(
  panel.grid.major = element_line(colour = "gray", linetype = "dotted"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)

# Calculo de mediana para la pregunta 2
Q2Count <- data.frame(
  Category  = c (Q2$respuesta), 
  Frequency= replace(Q2$conteo, is.na(Q2$conteo), 0)
)

respQ2 = aggregate(x= Q2Count$Frequency,              
                   by = list(Q2Count$Category),        
                   FUN = sum)  
respQ2
median(respQ2$x)

mean(respQ2$x)
# -------------------------------------

Q3 = DatosEvalFun %>% 
  group_by(Tarea, Q3) %>% 
  summarise(conteo = n()) %>% 
  ungroup() %>% 
  rename(respuesta = Q3) %>% 
  complete(nesting(Tarea), respuesta = seq(1,5))
expand(nesting(Tarea, conteo), respuesta = seq(1,5))

plotQ3 <- ggplot(Q3, aes(x = Tarea, y = conteo, fill = as.factor(respuesta))) + 
  geom_bar(position="dodge", stat = "identity") + 
  labs(fill = "Escala") +
  ylim(0, 10) +
  scale_fill_manual(values=c("#F24405", "#F2911B","#F2CF1D","#A3BF3F","#5A7340")) +
  labs(
    title = "Resultados de la pregunta 3:",
    subtitle = "\"En general, estoy satisfecho con la información de soporte \n(ayuda en línea, mensaje, documentación) al completar esta tarea.\"",
    x = "Tareas",
    y = "Frecuencia"
  )

plotQ3 + theme_classic() + theme(
  panel.grid.major = element_line(colour = "gray", linetype = "dotted"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)


# Completitud de tareas ########################################################

tarCom = DatosEvalFun %>% 
  group_by(Tarea, Completado) %>% 
  summarise(conteo = n(), fr = n()/10)

tareaCompletada = DatosEvalFun %>% 
  count(Tarea, Completado) %>% 
  ungroup() %>% 
  rename(respuesta = n) %>% 
  complete(nesting(Tarea), Completado)


# version 1
plotTareaComp <- ggplot(tareaCompletada, aes(x = Tarea, y = respuesta, fill = Completado)) + 
  geom_bar(position="dodge", stat = "identity") + 
  labs(fill = "¿Completada?") +
  ylim(0, 10) +
  scale_fill_manual(values=c("#F24405","#5A7340")) +
  labs(
    title = "Resultados de completitud de tareas",
    x = "Tareas",
    y = "Frecuencia"
  )

# version 2
plotTareaComp <- ggplot(tareaCompletada, 
                        aes(x = Tarea, y = respuesta, fill = Completado)) + 
  geom_bar(stat = "identity") + 
  labs(fill = "¿Completada?") +
  ylim(0, 10) +
  scale_fill_manual(values=c("#F24405","#5A7340")) +
  labs(
    title = "Resultados de completitud de tareas",
    x = "Tareas",
    y = "Frecuencia"
  )

plotTareaComp +
geom_text(aes(y = respuesta, label = respuesta), 
          position = position_stack(vjust = 0.5),
          color="white", 
          size=3.5) + 
  theme_classic() + theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)


# Graficos demograficos ########################################################

library(scales)
library(dplyr)

datosDem = DatosEvalFun %>% 
  group_by(Nombre, Sexo, Edad) %>% 
  summarise(conteo = n())

edadMedia = median(datosDem$Edad)
edadMedia

edadPromedio = mean(datosDem$Edad)
edadPromedio

edadMin = min(datosDem$Edad)
edadMin

edadMax = max(datosDem$Edad)
edadMax

countMujeres = datosDem %>% 
  group_by(Sexo) %>% 
  summarise(conteo = n(), porcentaje = scales::percent(conteo/10))


ggplot(countMujeres,aes(x="",y=conteo, fill=Sexo))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=porcentaje),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("#298BF2","#F241C6"))+
  theme_void()+
  labs(
    title = "Porcentaje de Participación",
  )







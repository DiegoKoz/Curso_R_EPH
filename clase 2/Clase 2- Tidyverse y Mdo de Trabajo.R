rm(list=ls())

dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
bases.dir      <-  paste0(dirname(dir),"/Fuentes/")
resultados.dir <- paste0(dirname(dir),"/Resultados/")


library(tidyverse)

INDICE  <- c(100,   100,   100,
             101.8, 101.2, 100.73,
             102.9, 102.4, 103.2)

FECHA  <-  c("Oct-16", "Oct-16", "Oct-16",
             "Nov-16", "Nov-16", "Nov-16",
             "Dic-16", "Dic-16", "Dic-16")


GRUPO  <-  c("Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado",
             "Privado_Registrado","Público","Privado_No_Registrado")

Datos <- data.frame(INDICE, FECHA, GRUPO)

Datos %>% 
  filter(INDICE>101 , GRUPO == "Privado_Registrado")

Datos %>% 
  filter(INDICE>101 | GRUPO == "Privado_Registrado")

Datos %>% 
  rename(Periodo = FECHA)

Datos <- Datos %>% 
  mutate(Doble=INDICE*2)

Datos <- Datos %>% 
  mutate(Caso_cuando = case_when(GRUPO == "Privado_Registrado"   ~ INDICE*2,
                                 GRUPO == "Público"              ~ INDICE*3,
                                 GRUPO == "Privado_No_Registrado"~ INDICE*5))

Datos2 <- Datos %>% 
  select(INDICE, FECHA, GRUPO)


Datos <- Datos %>% 
  select(-c(Doble,Caso_cuando))

Datos <- Datos %>% 
  arrange(GRUPO, INDICE)

Datos %>% 
  summarise(Indprom = mean(INDICE))

Datos %>% 
  group_by(FECHA) %>%
  summarise(Indprom = mean(INDICE))


Ponderadores <- data.frame(GRUPO = c("Privado_Registrado","Público","Privado_No_Registrado"),
                           PONDERADOR = c(50.16,29.91,19.93))

Datos_join <- Datos %>% 
  left_join(.,Ponderadores, by = "GRUPO")
Datos_join

Datos_Indice_Gral <- Datos_join %>% 
  group_by(FECHA) %>% 
  summarise(Indice_Gral = weighted.mean(INDICE,w = PONDERADOR))


## Tidyr

#__Gather__ es una función que nos permite pasar los datos de forma horizontal a una forma vertical. 
#__spread__ es una función que nos permite pasar los datos de forma vertical a una forma horizontal.
#Utilzamos un conjunto de datos que viene con la librería datasets
library(datasets)

data(iris)
iris <- iris %>% 
  mutate(id = 1:nrow(.)) %>%  #le agrego un ID
  select(id, everything()) # lo acomodo para que el id este primero. 

iris

iris_vertical <- iris %>% gather(., # el . llama a lo que esta atras del %>% 
                                 key   = Variables,
                                 value = Valores,
                                 2:5) #le indico que columnas juntar

iris_horizontal <- iris_vertical %>%
  spread(. ,
         key   = Variables, #la llave es la variable que va a dar los nombres de columna
         value = Valores) #los valores con que se llenan las celdas
iris_horizontal


####EPH####
library(readxl) # para leer archivos en excel
library(xlsx)   # para escribir archivos en excel

list.files(bases.dir)
Individual_t117 <-
  read.table(
    paste0(bases.dir, "usu_individual_t117.txt"),
    sep = ";",
    dec = ",",
    header = TRUE,
    fill = TRUE )


Aglom <- read_excel(paste0(bases.dir, "Aglomerados EPH.xlsx"))


###### Variables ###### 

# ESTADO: CONDICIÓN DE ACTIVIDAD 

# 0 = Entrevista individual no realizada ( no respuesta al Cuestionario Individual)
# 1 = Ocupado
# 2 = Desocupado
# 3 = Inactivo
# 4 = Menor de 10 años

# PONDERA: Ponderador 

# INTENSI: 

# 1 = Subocupado por insuficiencia horaria
# 2 = Ocupado pleno
# 3 = Sobreocupado
# 4 = Ocupado que no trabajó en la semana
# 9 = Ns./Nr.

# PP03J:

# Aparte de este/os trabajo/s,¿estuvo buscando algún
# empleo/ocupación/ actividad?
# 1 = Si
# 2 = No
# 9 = Ns./Nr.


#### 1. Principales Indicadores del Mdo de Trabajo #### 
####  1.1 Principales indicadores. Total 31 aglomerados urbanos
#####Opcion A####
Cuadro_1.1a <- Individual_t117 %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]),
            PEA               = Ocupados + Desocupados,
            Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            Subocupados       = Suboc_demandante + Suboc_no_demand ,
# También podemos llamar a las variables entre comillas, incluyendo nombres compuestos
# A su vez, podemos utilizar la variable recién creada en la definción de otra varible
            'Tasa Actividad'                  = PEA/Poblacion,
            'Tasa Empleo'                     = Ocupados/Poblacion,
            'Tasa Desocupacion'               = Desocupados/PEA,
            'Tasa ocupados demandantes'       = Ocupados_demand/PEA,
            'Tasa Subocupación'               = Subocupados/PEA,
            'Tasa Subocupación demandante'    = Suboc_demandante/PEA,
            'Tasa Subocupación no demandante' = Suboc_no_demand/PEA) %>% 
  select(-c(1:8)) %>% 
  gather(Tasas, Valor, 1:ncol(.))

#####Opcion B####
Cuadro_1.1b <- Individual_t117 %>% 
  #filter(ESTADO != 0) %>% 
  summarise(Actividad         = sum(PONDERA[ESTADO %in% c(1,2)])/sum(PONDERA),
            Empleo            = sum(PONDERA[ESTADO == 1]/sum(PONDERA)),
            Desocupacion      = sum(PONDERA[ESTADO == 2]/sum(PONDERA[ESTADO %in% c(1,2)])),
            Ocup_demand       = sum(PONDERA[ESTADO == 1 & PP03J ==1]/
                                      sum(PONDERA[ESTADO %in% c(1,2)])),
            Subocupación      = sum(PONDERA[ESTADO == 1 & INTENSI ==1]/
                                     sum(PONDERA[ESTADO %in% c(1,2)])),
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]/
                                      sum(PONDERA[ESTADO %in% c(1,2)])),
            Suboc_no_demandante= sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]/
                                       sum(PONDERA[ESTADO %in% c(1,2)]))
            ) %>% 
  gather(Tasas, Valor, 1:ncol(.)) 

#### 1.2 Principales Indicadores por áreas geográficas #### 
#    Primer trimestre de 2017

#####Opcion A####
Cuadro_1.2a <- Individual_t117 %>% 
  group_by(AGLOMERADO) %>% 
  summarise(Poblacion         = sum(PONDERA),
            Ocupados          = sum(PONDERA[ESTADO == 1]),
            Desocupados       = sum(PONDERA[ESTADO == 2]),
            PEA               = Ocupados + Desocupados,
            Ocupados_demand   = sum(PONDERA[ESTADO == 1 & PP03J ==1]),
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            Subocupados       = Suboc_demandante + Suboc_no_demand ,
            
            'Tasa Actividad'                  = PEA/Poblacion,
            'Tasa Empleo'                     = Ocupados/Poblacion,
            'Tasa Desocupacion'               = Desocupados/PEA,
            'Tasa ocupados demandantes'       = Ocupados_demand/PEA,
            'Tasa Subocupación'               = Subocupados/PEA,
            'Tasa Subocupación demandante'    = Suboc_demandante/PEA,
            'Tasa Subocupación no demandante' = Suboc_no_demand/PEA) %>% 
  select(-c(2:9)) %>% 
  left_join(.,Aglom) %>% 
  select(Nom_Aglo,everything(.),-AGLOMERADO) 


#####Opcion B####
Cuadro_1.2b <- Individual_t117 %>% 
  group_by(AGLOMERADO) %>% 
  summarise(Actividad         = sum(PONDERA[ESTADO %in% c(1,2)])/sum(PONDERA),
            Empleo            = sum(PONDERA[ESTADO == 1]/sum(PONDERA)),
            Desocupacion      = sum(PONDERA[ESTADO == 2]/sum(PONDERA[ESTADO %in% c(1,2)])),
            Ocup_demand       = sum(PONDERA[ESTADO == 1 & PP03J ==1]/
                                      sum(PONDERA[ESTADO %in% c(1,2)])),
            Subocupación      = sum(PONDERA[ESTADO == 1 & INTENSI ==1]/
                                      sum(PONDERA[ESTADO %in% c(1,2)])),
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]/
                                      sum(PONDERA[ESTADO %in% c(1,2)])),
            Suboc_no_demandante= sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]/
                                       sum(PONDERA[ESTADO %in% c(1,2)]))) %>% 
    left_join(.,Aglom) %>% 
    select(Nom_Aglo,everything(.),-AGLOMERADO) %>% 
  ####Procedimiento por si lo quiero exportar en porcentajes
    #mutate_each(funs(sprintf("%1.1f%%", 100*.)),2:ncol(.))
    mutate_at(2:ncol(.),funs(sprintf("%1.1f%%", 100*.)))



####Guardo en un Excel los Resultados#####

write.xlsx(as.data.frame(Cuadro_1.1a), paste0(resultados.dir,"Informe Mercado de Trabajo.xlsx"),
           sheetName = "Cuadro 1.1", append = FALSE, row.names = FALSE)
write.xlsx(as.data.frame(Cuadro_1.2a), paste0(resultados.dir,"Informe Mercado de Trabajo.xlsx"),
           sheetName = "Cuadro 1.2", append = TRUE, row.names = FALSE)

rm(list=ls())

#### Librerias#### 
library(tidyverse)
library(readxl)
library(questionr)
library(alluvial)

dir <- paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)),"/")
bases.dir      <-  paste0(dir,"/Fuentes/")
resultados.dir <- paste0(dir,"/Resultados/")
#### Levanto las bases#### 

individual.316 <- read.table(paste0(bases.dir, "usu_individual_t316.txt"), sep=";", dec=",", header = TRUE, fill = TRUE)
individual.416 <- read.table(paste0(bases.dir, "usu_individual_t416.txt"), sep=";", dec=",", header = TRUE, fill = TRUE)

#####Levanto fuentes secundarias#### 
Adequi <- read_excel(paste0(bases.dir,"ADEQUI.xls"))
CBA <- read_excel(paste0(bases.dir,"CANASTAS.xls"),sheet = "CBA")
CBT <- read_excel(paste0(bases.dir,"CANASTAS.xls"),sheet = "CBT")
dic.regiones <- read_excel(paste0(bases.dir,'Regiones.xlsx'))
###### Promedio trimestral de Canastas ######
CBA$Canasta <- 'CBA'
CBT$Canasta <- 'CBT'
Canasta <- bind_rows(CBA,CBT) %>% 
  gather(., Region, Valor, c(3:(ncol(.)-1) )) %>%
  mutate(Trimestre = case_when(Mes %in% c(1:3)   ~1,
                               Mes %in% c(4:6)   ~2,
                               Mes %in% c(7:9)   ~3,
                               Mes %in% c(10:12) ~4),
         Periodo = paste(Año, Trimestre, sep='.')) %>% 
  group_by(Canasta, Region, Periodo) %>% 
  summarise(Valor = mean(Valor)) %>% 
  spread(., Canasta,Valor) %>% 
  left_join(., dic.regiones, by = "Region") %>% 
  ungroup() %>% 
  select(-Region)

###### Calculo de Pobreza ######
var.ind <- c('CODUSU', 'ANO4','TRIMESTRE','NRO_HOGAR','COMPONENTE','REGION',
             'AGLOMERADO', 'PONDERA', 'CH04', 'CH06', 'ITF', 'PONDIH',"P21")

Pobreza_Individual <- bind_rows(individual.316 %>% 
                          select(var.ind),
                          individual.416 %>% 
                          select(var.ind)) %>% 
  mutate(Periodo = paste(ANO4, TRIMESTRE, sep='.')) %>% 
  group_by(CODUSU,NRO_HOGAR,COMPONENTE) %>% 
  mutate(CH06_Diff   = abs(lead(CH06) - CH06)<3,
         CH04_Diff   = lead(CH04)==CH04,
         Consistencia= ifelse((CH06_Diff & CH04_Diff)== FALSE,
                               "inconsistente","consistente")) %>%  
  ungroup() %>% 
  left_join(., Adequi, by = c("CH04", "CH06")) %>% 
  group_by(CODUSU, NRO_HOGAR, Periodo) %>% 
  mutate(Adequi_hogar = sum(adequi)) %>%
  ungroup() %>% 
  left_join(., Canasta, by = c("REGION", "Periodo")) %>% 
  mutate(CBA = CBA*Adequi_hogar,
         CBT = CBT*Adequi_hogar,
         Situacion = case_when(ITF<CBA            ~ 'Indigente',
                               ITF>=CBA & ITF<CBT ~ 'Pobre',
                               ITF>=CBT           ~ 'No.Pobre'))  

###### Panel ######
Panel_Ind <- Pobreza_Individual %>% 
  filter(PONDIH>0) %>%
#Si no saco estos me queda gente que en alguno de los dos periodos no respondio ingresos y distorsiona todo
  group_by(CODUSU, NRO_HOGAR, COMPONENTE) %>% 
  arrange(Periodo) %>% 
  mutate(Cod.Panel = as.factor(ifelse(!is.na(lead(Periodo)),1,0)),
         Situacion_t1 = lead(Situacion)) %>%
  filter(Cod.Panel == 1, Consistencia == "consistente")

####Calculo de Pobreza Individual- Base Completa - Panel ####
Pobreza_resumen <- Pobreza_Individual %>% 
  group_by(Periodo) %>% 
  summarise(Tasa_pobreza    = sum(PONDIH[Situacion %in% c('Pobre', 'Indigente')], na.rm = TRUE)/
                                sum(PONDIH,na.rm = TRUE),
            Tasa_indigencia = sum(PONDIH[Situacion == 'Indigente'], na.rm = TRUE)/
                                sum(PONDIH,na.rm = TRUE)) 

Pobreza.panel <- Panel_Ind %>% 
  group_by(Periodo) %>% 
  summarise(Tasa_pobreza    = sum(PONDIH[Situacion %in% c('Pobre', 'Indigente')], na.rm = TRUE)/
                                sum(PONDIH,na.rm = TRUE),
            Tasa_indigencia = sum(PONDIH[Situacion == 'Indigente'], na.rm = TRUE)/
                                sum(PONDIH,na.rm = TRUE)) 

datos.alluvial <- Panel_Ind %>% 
  group_by(Situacion, Situacion_t1, REGION) %>% 
  summarise(frecuencia = sum(PONDIH),
            n_muestral = n()) %>% 
  left_join(.,dic.regiones, by= "REGION") %>% 
  arrange(REGION)

datos.alluvial$Situacion <- factor(datos.alluvial$Situacion, levels =
                                         c("No.Pobre","Pobre","Indigente"))
datos.alluvial$Situacion_t1 <- factor(datos.alluvial$Situacion_t1, levels =
                                         c("No.Pobre","Pobre","Indigente"))
#wtd.table(datos.alluvial$Situacion, datos.alluvial$Cod.Panel, weights = datos.alluvial$PONDIH )

colores <- ifelse(datos.alluvial$Situacion == datos.alluvial$Situacion_t1, "gray60",
                  ifelse(datos.alluvial$Situacion_t1 == "Indigente","firebrick1",
                         ifelse(datos.alluvial$Situacion_t1 == 'Pobre','goldenrod1',
                                ifelse(datos.alluvial$Situacion_t1 == 'No.Pobre','chartreuse2','red'))))

transiciones <- alluvial(datos.alluvial[,c(1:2)], freq=datos.alluvial$frecuencia, border=NA, 
                         col = colores, cex=0.75, xw=.15)
#dev.off()

pdf(paste(resultados.dir, "Transiciones de Situacion - Regiones - t316-t416.pdf", sep = ""), onefile = T)
for(Reg in unique(datos.alluvial$REGION)){
  
  data.Reg <- datos.alluvial %>% filter(REGION == Reg)
  tmp.nom <- dic.regiones$Region[dic.regiones$REGION==Reg]
  colores <- ifelse(data.Reg$Situacion==data.Reg$Situacion_t1,"gray60",
                    ifelse(data.Reg$Situacion_t1=="Indigente","firebrick1",
                           ifelse(data.Reg$Situacion_t1 == 'Pobre','goldenrod1',
                                  ifelse(data.Reg$Situacion_t1 == 'No.Pobre','chartreuse2','red'))))
  
  transiciones <- alluvial(data.Reg[,c(1:2)], freq=data.Reg$frecuencia, border=NA, 
                           col = colores, cex=0.75, xw=.15)
  mtext(paste0('Region: ',tmp.nom), 3, line=3, font=2)
  paste(Sys.time(),tmp.nom)
}
dev.off()




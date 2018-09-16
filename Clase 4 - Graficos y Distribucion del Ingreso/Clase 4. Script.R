#RESTART R

# iris es un set de datos clásico, que ya viene incorporado en R
plot(iris)

data(iris)
plot(iris$Sepal.Length,type = "p")
plot(iris$Sepal.Length,type = "l")
plot(iris$Sepal.Length,type = "b")
hist(iris$Sepal.Length, col = "lightsalmon1", main = "Histograma")

archivo <- "Resultados/grafico1.PNG"
archivo
png(archivo)
plot(iris$Sepal.Length,type = "b")
dev.off()

library(ggplot2)
ggplot(data = iris, aes(x = Sepal.Length, fill = Species))+
  geom_histogram(alpha=0.75, binwidth = .5)+
  facet_wrap(~Species)+
  labs("Histograma por especie")+
  theme(legend.position = 'none')

library(tidyverse) # tiene ggplot, dplyr, tidyr, y otros
library(ggthemes)  # estilos de gráficos
library(ggrepel)   # etiquetas de texto más prolijas que las de ggplot
library(scales)    # tiene la función 'percent()'

####Levanto Bases####
Individual_t117 <- read.table("Fuentes/usu_individual_t117.txt",
                              sep=";", dec=",", header = TRUE, fill = TRUE)
Individual_t216 <- read.table("Fuentes/usu_individual_t216.txt",
                              sep=";", dec=",", header = TRUE, fill = TRUE) %>% 
  select(ANO4,TRIMESTRE, IPCF, PONDIH)

Individual_t316 <- read.table("Fuentes/usu_individual_t316.txt",
                              sep=";", dec=",", header = TRUE, fill = TRUE)%>% 
  select(ANO4,TRIMESTRE, IPCF, PONDIH)

Individual_t416 <- read.table("Fuentes/usu_individual_t416.txt",
                              sep=";", dec=",", header = TRUE, fill = TRUE)%>% 
  select(ANO4,TRIMESTRE, IPCF, PONDIH)


####GINI####
library(reldist) #para la función 'gini'


gini <- bind_rows(Individual_t216, 
                  Individual_t316,
                  Individual_t416,
                  Individual_t117) %>%
  select(ANO4,TRIMESTRE, IPCF, PONDIH) %>% 
  mutate(periodo = paste(ANO4, TRIMESTRE, sep = "\n")) %>% 
  group_by(periodo) %>% 
  summarise(gn = gini(IPCF, weights= PONDIH)) %>% 
  ungroup()

####GRAFICOS####
ggplot(data = gini, aes(x = periodo, y = gn)) + 
  geom_point()

ggplot(gini, aes(x = periodo, y = gn, group = 'gini', label= round(gn,3))) + 
  labs(x = "Trimestre", y = "Coeficiente de Gini", title = "Coeficiente de Gini", subtitle = "Según trimestre", caption = "Fuente: EPH")+
  geom_line( size= 1 )+
  geom_point(aes(shape = periodo, color = periodo),size= 3)+ #puedo definir aes() en cada tipo de gráfico
  geom_text_repel(nudge_x = .2)+
  theme_minimal()+
  theme(legend.position = "none")

ggsave(filename =  "Resultados/gini.png")

library(statar) # Para la función xtile

datagraf_2 <-Individual_t117 %>% 
  select(P47T,T_VI, TOT_P12, P21 , PONDII, CH04) %>% 
  filter(!is.na(P47T), P47T > 0 ) %>% 
  mutate(P47T_decil         = P47T+runif(nrow(.),min = -.01,max =.01), # Perturbo la variable
         DECINDR            = xtile(P47T_decil,n=10,w = PONDII),
         ingreso_laboral    = as.numeric(TOT_P12 + P21),
         ingreso_no_laboral = as.numeric(T_VI),
         ingreso_total      = ingreso_laboral + ingreso_no_laboral,
         CH04               = case_when(CH04 == 1 ~ "Varon",
                                        CH04 == 2 ~ "Mujer",
                                        FALSE     ~ "Otro") ) %>% 
  group_by(DECINDR, CH04) %>% 
  summarise('ingreso laboral'    = sum(ingreso_laboral*PONDII)/sum(ingreso_total*PONDII),
            'ingreso no laboral' = sum(ingreso_no_laboral * PONDII)/sum(ingreso_total*PONDII)) %>% 
  gather(tipo_ingreso, monto,3:4 ) 


ggplot(datagraf_2, aes(CH04, monto, fill = tipo_ingreso, 
                       label = sprintf("%1.1f%%", 100*monto)))+
  geom_col(position = "stack", alpha=0.6) + 
  geom_text(position = position_stack(vjust = 0.5), size=3)+
  labs(x="",y="Porcentaje")+
  theme_tufte()+
  scale_fill_fivethirtyeight()+
  scale_y_continuous(labels = percent)+
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        axis.text.x = element_text(angle=25))+
  facet_grid(.~DECINDR)

ggsave(filename = "Resultados/ingreso por decil.png",scale = 2)

class(Individual_t117$NIVEL_ED)
class(Individual_t117$CH04)


ggdata <- Individual_t117 %>% 
  filter(P21>0, !is.na(NIVEL_ED)) %>% 
  mutate(NIVEL_ED = as.factor(NIVEL_ED),
         CH04     = as.factor(CH04))

ggplot(ggdata, aes(x= CH04, y = P21, group = CH04, fill = CH04 )) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 40000))+
  facet_wrap(~ NIVEL_ED, labeller = "label_both")

datagraf <-Individual_t117 %>% 
  select(REGION,P47T,T_VI, TOT_P12, P21 , PONDII, CH04) %>% 
  filter(!is.na(P47T), P47T > 0 ) %>% 
  mutate(REGION             = case_when(REGION == 1    ~ 'GBA',
                                        REGION == 40   ~ 'NOA',
                                        REGION == 41   ~ 'NEA',
                                        REGION == 42   ~ 'Cuyo',
                                        REGION == 43   ~ 'Peampeana',
                                        REGION == 44   ~ 'Patagonia',
                                        FALSE          ~ 'otro'),
         ingreso_laboral    = as.numeric(TOT_P12 + P21),
         ingreso_no_laboral = as.numeric(T_VI),
         CH04               = case_when(CH04 == 1 ~ "Varon",
                                        CH04 == 2 ~ "Mujer",
                                        FALSE     ~ "Otro") ) %>% 
  gather(., key = Tipo_ingreso, Ingreso, c((ncol(.)-1):ncol(.)))


datagraf2 <- datagraf %>% filter( Ingreso !=0)


ggplot(datagraf2, aes(
  x = Ingreso,
  weights = PONDII,
  group = Tipo_ingreso,
  fill = Tipo_ingreso)) +
  geom_density(alpha=0.7,adjust =2)+
  labs(x="Distribución del ingreso", y="",
       title=" Total según tipo de ingreso y género", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,50000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "bottom",
        plot.title      = element_text(size=12))+
  facet_wrap(~ CH04, scales = "free")

ggsave(filename = "Resultados/Kernel_1.png",scale = 2)


ggplot(datagraf2, aes(
  x = Ingreso,
  weights = PONDII,
  group = CH04,
  fill = CH04)) +
  geom_density(alpha=0.7,adjust =2)+
  labs(x="Distribución del ingreso", y="",
       title=" Total según tipo de ingreso y género", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,50000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "bottom",
        plot.title      = element_text(size=12))+
  facet_wrap(~Tipo_ingreso, scales = "free")

ggsave(filename = "Resultados/Kernel_1.png",scale = 2)




ggplot(ggdata, aes(x = NIVEL_ED, y = P21, group = NIVEL_ED, fill = NIVEL_ED )) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 40000))

ggplot(ggdata, aes(x= NIVEL_ED, y = P21, group = NIVEL_ED, fill = NIVEL_ED )) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 40000))+
  facet_wrap(~ CH04, labeller = "label_both")


datagraf <-Individual_t117 %>% 
  select(REGION,P47T,T_VI, TOT_P12, P21 , PONDII, CH04) %>% 
  filter(!is.na(P47T), P47T > 0 ) %>% 
  mutate(REGION             = case_when(REGION == 1    ~ 'GBA',
                                        REGION == 40   ~ 'NOA',
                                        REGION == 41   ~ 'NEA',
                                        REGION == 42   ~ 'Cuyo',
                                        REGION == 43   ~ 'Peampeana',
                                        REGION == 44   ~ 'Patagonia',
                                        FALSE          ~ 'otro'),
         ingreso_laboral    = as.numeric(TOT_P12 + P21),
         ingreso_no_laboral = as.numeric(T_VI),
         CH04               = case_when(CH04 == 1 ~ "Varon",
                                        CH04 == 2 ~ "Mujer",
                                        FALSE     ~ "Otro") ) %>% 
  gather(., key = Tipo_ingreso, Ingreso, c((ncol(.)-1):ncol(.)))


datagraf2 <- datagraf %>% filter( Ingreso !=0)


ggplot(datagraf2, aes(
  x = Ingreso,
  weights = PONDII,
  group = Tipo_ingreso,
  fill = Tipo_ingreso)) +
  geom_density(alpha=0.7,adjust =2)+
  labs(x="Distribución del ingreso", y="",
       title=" Total según tipo de ingreso y género", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,50000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "bottom",
        plot.title      = element_text(size=12))+
  facet_wrap(~ CH04, scales = "free")

ggsave(filename =  "Resultados/Kernel_1.png",scale = 2)


ggplot(datagraf2, aes(
  x = Ingreso,
  weights = PONDII,
  group = CH04,
  fill = CH04)) +
  geom_density(alpha=0.7,adjust =2)+
  labs(x="Distribución del ingreso", y="",
       title=" Total según tipo de ingreso y género", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,50000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "bottom",
        plot.title      = element_text(size=12))+
  facet_wrap(~Tipo_ingreso, scales = "free")

ggsave(filename ="Resultados/Kernel_1.png",scale = 2)



library(ggjoy)  


datagraf3 <- datagraf2 %>% filter(Tipo_ingreso == "ingreso_laboral")

ggplot(datagraf3, aes(
  x = Ingreso,
  y = REGION,
  weights = PONDII,
  group = REGION,
  fill = REGION
)) +
  geom_joy(alpha=0.6, bandwidth = 1500)+
  labs(x="", y="",
       title="Distribución del ingreso laboral según región", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,35000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title   = element_text(size = 12))

ggsave(filename = "Resultados/Kernel_2.png",scale = 2)




ggdata <- Individual_t117 %>% 
  filter(P21>0,
         !is.na(NIVEL_ED),
         NIVEL_ED!=7, 
         PP04A !=3) %>% 
  mutate(NIVEL_ED = factor(case_when(NIVEL_ED == 1  ~ 'Primaria \n Incompleta', # '\n' significa carriage return, o enter
                                      NIVEL_ED == 2  ~ 'Primaria \n Completa',
                                      NIVEL_ED == 3  ~ 'Secundaria \nIncompleta',
                                      NIVEL_ED == 4  ~ 'Secundaria \nCompleta',
                                      NIVEL_ED == 5  ~ 'Superior \nUniversitaria \nIncompleta',
                                      NIVEL_ED == 6  ~ 'Superior \nUniversitaria \nCompleta',
                                      FALSE          ~ 'Otro'),
                            levels= c('Primaria \n Incompleta',
                                      'Primaria \n Completa',
                                      'Secundaria \nIncompleta',
                                      'Secundaria \nCompleta',
                                      'Superior \nUniversitaria \nIncompleta',
                                      'Superior \nUniversitaria \nCompleta')),
         Sexo     = case_when(CH04 == 1 ~ 'Varón',
                              CH04 == 2 ~ 'Mujer'),
         Establecimiento    = case_when(PP04A == 1 ~ 'Estatal',
                                        PP04A == 2 ~ 'Privado',
                                        FALSE      ~ 'Otro'))


ggplot(ggdata, aes(CH06, P21, colour = Sexo, shape = Sexo, alpha = P21))+
  geom_smooth() + 
  labs(
    x = 'Edad',
    y = 'ingreso',
    title = 'Ingreso por ocupación principal',
    subtitle = 'Según edad, nivel educativo y sexo') +
  theme_minimal()+
  scale_y_continuous(labels = comma)+
  scale_alpha(guide = FALSE)+
  facet_grid(.~NIVEL_ED)


ggplot(ggdata, aes(CH06, P21, colour = Sexo, weight = PONDIIO)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  labs(x = 'Edad',
       y = 'ingreso',
       title = 'Regresion cuadrática del Ingreso por ocupación principal respecto de la Edad',
       subtitle = 'Según Nivel educativo y sexo') +
  theme_minimal()+
  facet_grid(. ~ NIVEL_ED)


ggplot(ggdata, aes(CH06, P21, colour = Establecimiento, weight = PONDIIO)) +
  geom_smooth(method = "lm") +
  labs(
    x = 'Edad',
    y = 'ingreso',
    title = 'Tendencia del ingreso por ocupación principal',
    subtitle = 'Según edad, nivel educativo, sexo y tipo de establecimiento') +
  theme_minimal()+
  facet_grid(Sexo ~ NIVEL_ED)

ggsave(filename = "Resultados/regresion lineal.png",scale = 2)
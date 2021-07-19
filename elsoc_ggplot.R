##############################################################################################################################################
#########################################            TALLER DE VISUALIZACIÓN DE DATOS ELSOC          #########################################
#########################################        ESTUDIO LONGITUDINAL SOCIAL DE CHILE (ELSOC)        #########################################
#########################################               OLA Nº 1, 2, 3 y 4 (2016-2019)               #########################################
#########################################                                                            #########################################
#########################################          AUTORÍA:  PRACTICANTES ELSOC 2021                 #########################################
#########################################     Isidora Didier, Elisa Salas y Cristóbal Ortiz          #########################################
#########################################                     UPDATE: 16/07/2021                     #########################################
#########################################                                                            #########################################
##############################################################################################################################################


##### 1. Preparación de datos ####

###### a. Cargar librerías y otras configuraciones ######

#install.packages("pacman")
#library(pacman)
#pacman::p_load(car,dplyr,panelr,stringr,tidyverse,ggplot2,survey,ggrepel,na.tools)
library(car)
library(tidyverse) #ggplot, dplyer, stringr
library(planelr)
library(ggrepel)
library(na.tools)

remove(list = ls()) #limpieza del entorno de trabajo
options(scipen=999) #evita notación científica

###### b. Cargar base de datos ###### 
load(url("https://dataverse.harvard.edu/api/access/datafile/4606527")) #cargar bbdd desde url oficial alojado en dataverse

###### c. Selección de variables y filtrado de datos ###### 
sjmisc::frq(elsoc_wide_2016_2019$tipo_caso)

elsoc_wide <- elsoc_wide_2016_2019 %>% dplyr::filter(tipo_atricion==1 & tipo_caso !=2) #filtrar atrición entre 2016-19 y casos c/inconsistencias mayores

elsoc_wide <- elsoc_wide %>% dplyr::select(idencuesta, #identificador individual
                                           ponderador01_w01,ponderador01_w02,ponderador01_w03,ponderador01_w04,#ponderador población
                                           ponderador02_w01,ponderador02_w02,ponderador02_w03,ponderador02_w04,#ponderador sexo
                                           m0_sexo_w01,m0_sexo_w02,m0_sexo_w03,m0_sexo_w04, #sexo
                                           m0_edad_w01,m0_edad_w02,m0_edad_w03,m0_edad_w04, #edad
                                           m01_w01,m01_w02,m01_w03,m01_w04, #nivel de educación
                                           d01_01_w01,d01_01_w02,d01_01_w03,d01_01_w04, #estatus social subjetivo
                                           m02_w01,m02_w02,m02_w03,m02_w04, #situación ocupacional
                                           segmento_w01,segmento_w02,segmento_w03,segmento_w04, #manzana o bloque
                                           estrato_w01,estrato_w02,estrato_w03,estrato_w04, #ciudad
                                           region_w01,region_w02,region_w03,region_w04, #región
                                           c16_w01,c16_w02,c16_w03,c16_w04, #indentificación con partido político
                                           c17_w01,c17_w02,c17_w03,c17_w04, #intentificación con coalición política
                                           c28_w01,c28_w02,c28_w03,c28_w04, #de acuerdo con cambiar la constitución actual
                                           c05_08_w01,c05_08_w02,c05_08_w03,c05_08_w04, #confianza en el presidente
                                           c05_02_w01,c05_02_w02,c05_02_w03,c05_02_w04, #confianza en partidos políticos
                                           c05_03_w01,c05_03_w02,c05_03_w03,c05_03_w04) #confianza en carabineros

###### d. Transformar base de datos de wide a long ###### 
elsoc_long <- long_panel(data = elsoc_wide, #base de datos formato wide
                         prefix = "_w0", #caracteres antes de la etiqueta de cada ola
                         begin = 1, #etiqueta de la primera ola
                         end = 4, #etiqueta de la última ola
                         label_location = "end", #indica donde se localiza la etiqueta asociada a la ola 
                         id = "idencuesta", #indica identificador individual
                         wave = "ola") #nombre que tomará la variable que indica periodo. 

###### e. recodificación de NA ###### 
elsoc_long[elsoc_long==-999 | elsoc_long==-888] <- NA #recodificar No sabe y No responde en NA
sum(is.na(elsoc_long)) #indica cantidad de NA

###### f. Recodificación de variables ###### 

#Recode variable "ola" correspondiente a la ola de medición.
elsoc_long$ola <- factor(elsoc_long$ola,labels = c('2016', '2017', '2018', '2019'))
elsoc_long$ola <- sjlabelled::set_label(elsoc_long$ola, label = c("Ola de Medición"))#etiquetamos variable

#Recode variable "m0_sexo" / rename "sexo".
elsoc_long$sexo <- factor(elsoc_long$m0_sexo,labels = c('Hombre', 'Mujer'))
elsoc_long$sexo <- sjlabelled::set_label(elsoc_long$sexo, label = c("Tipo de sexo")) #etiquetamos variable

#Recode variable "m0_edad" / rename "edad"
elsoc_long$edad <- factor(car::recode(elsoc_long$m0_edad, "18:29=1;30:49=2;50:64=3;65:150=4"),
                          labels = c('18-29', '30-49', '50-64', '65 o más'))
elsoc_long$edad <- sjlabelled::set_label(elsoc_long$edad, label = c("Edad en Tramos")) #etiquetamos variable

#Recode variable "m01" nivel educacional / rename "educacion"
elsoc_long$educacion <- car::recode(elsoc_long$m01,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")
elsoc_long$educacion <- factor(elsoc_long$educacion,labels = c("Basica","Media","Tecnica","Universitaria"))
elsoc_long$educacion <- sjlabelled::set_label(elsoc_long$educacion, label = c("Nivel Educacional")) #etiquetamos variable

#Recode variable "d01_01" estatus social subjetivo / raneme "estatus".
elsoc_long$estatus<- factor(car::recode(elsoc_long$d01_01, "0:4=1;5=2;6:10=3"),
                            labels = c('Bajo','Medio','Alto'))
elsoc_long$estatus <- sjlabelled::set_label(elsoc_long$estatus, label = c("Estatus Social Subjetivo")) #etiquetamos variable

#Recode variable "m02" situación ocupacional / rename "socup".
elsoc_long$socup <- factor(car::recode(elsoc_long$m02,"c(1,2,3) = 1; 7 = 2; 6 = 3; 5 = 4; c(4, 8, 9) = 5"),
                           labels = c("Trabajo remunerado", "Trabajo doméstico no remunerado", 
                                      "Desempleado/a", "Jubilado/a o pensionado/a", 
                                      "Otras categorías"))
elsoc_long$socup <- na.replace(elsoc_long$socup, "NS/NR") #recode NA en categoría "NS/NR"
elsoc_long$socup <- sjlabelled::set_label(elsoc_long$socup, label = c("Situación Ocupacional")) #etiquetamos variable

#Recode variable "estrato" que refiere al tipo de ciudad / rename "tipo de ciudad"
elsoc_long$tipo_ciudad <- factor(elsoc_long$estrato, labels = c('Gran Santiago', 'Gran Valparaíso', 'Gran 
                                                                Concepción', 'Ciudades grandes', 'Ciudades 
                                                                medianas', 'Ciudades pequeñas'))
elsoc_long$tipo_ciudad <- sjlabelled::set_label(elsoc_long$estrato, label = c("Tipo de ciudad")) #etiquetamos variable

#Recode variable "region" correspondiente a regiones del país/ raname "zona" relativa a macrozona geográfica.
elsoc_long$zona  <- car::recode(elsoc_long$region,
                                "c('Tarapaca','Antofagasta','Atacama','Coquimbo','Arica')=1;
                                c('Valparaiso','Lib. Gral. B. Ohiggins','B. Ohiggins', 'Maule','Bio Bio')=2;
                                c('Araucania','Los Lagos','Aysen','Magallanes','Los Rios')=3 ;
                                'Metropolitana'=4")
elsoc_long$zona  <- factor(elsoc_long$zona,levels=c("1","2","3","4"), labels = 
                             c("Norte","Centro","Sur","Metropolitana"))
elsoc_long$zona <- sjlabelled::set_label(elsoc_long$zona, label = c("Zona Geográfica")) #etiquetamos variable

#Recode "c16" identificación con partidos políticos / rename "idpart".
elsoc_long$idpart <- factor(elsoc_long$c16,labels = 
                              c('PC','PH','RD','PRO','EVO','PPD','AMP','PDC','PRI','RN','UDI','PS','PRDS',
                                'Otro','Ninguno'))
elsoc_long$idpart <- sjlabelled::set_label(elsoc_long$idpart, label = c("Indentificación con Partido Político")) #etiquetamos variable

#Recode "c17" indentifiación con colación política /rename "idcoal".
elsoc_long$idcoal <- factor(elsoc_long$c17, labels = c('Chile Vamos','Nueva Mayoría','Frente Amplio', 
                                                       'Otra','Ninguna'))
elsoc_long$idcoal <- na.replace(elsoc_long$idcoal, "NS/NR") #recode NA en categoría "NS/NR"
elsoc_long$idcoal <- sjlabelled::set_label(elsoc_long$idcoal, label = 
                                             c("Indentificación con Coalición Política")) #etiquetamos variable

#Recode "c28" grado de acuerdo con cambiar constitución / rename "cambiar_consti".
elsoc_long$cambiar_consti <- factor(car::recode(elsoc_long$c28, "c(1,2)=1;c(3)=2;c(4,5)=3"))
elsoc_long$cambiar_consti <- factor(elsoc_long$cambiar_consti, 
                                    labels = c('En desacuendo o totalmente en desacuerdo', 'Ni de acuerdo ni en 
                                       desacuerdo', 'De acuerdo o totalmente de acuerdo'))
elsoc_long$cambiar_consti <- sjlabelled::set_label(elsoc_long$cambiar_consti, label = 
                                                     c("Grado de Acuerdo con Cambiar Constitución")) #etiquetamos variable

#Recode "c05_08" confianza en el presidente / rename "conf_presi".
elsoc_long$conf_presi <- factor(elsoc_long$c05_08,labels = c('Nada', 'Poco', 'Algo', 'Bastante', 'Mucho'))
elsoc_long$conf_presi <- sjlabelled::set_label(elsoc_long$conf_presi, label = c("Confianza en el 
                                                                                Presidente")) #etiquetamos variable

#Recode "c05_02" confianza en partidos políticos / rename "conf_part".
elsoc_long$conf_part <- factor(elsoc_long$c05_02,labels = c('Nada', 'Poco', 'Algo', 'Bastante', 'Mucho'))
elsoc_long$conf_part <- sjlabelled::set_label(elsoc_long$conf_part, label = 
                                                c("Confianza en Partidos Políticos")) #etiquetamos variable

#Recode "c05_03" confianza en carabineros / rename "conf_carb".
elsoc_long$conf_carb <- factor(elsoc_long$c05_03,labels = c('Nada', 'Poco', 'Algo', 'Bastante', 'Mucho'))
elsoc_long$conf_carb <- sjlabelled::set_label(elsoc_long$conf_carb, label = 
                                                c("Confianza en Carabineros"))  #etiquetamos variable

#Visualizar la bbdd procesada con las variables incorporadas
knitr::kable(elsoc_long, "pipe")


###### g. Guardar base de datos formato R.Data (opcional) ###### 

#save(elsoc_long, file "[ruta de carpeta local]/elsoc_long.RData")


#### Trabajando con una encuesta ####
#Reconocer diseño muestral con ponderadores
elsoc_diseno <- svydesign(ids = ~segmento, #muestreo por conglomerado a nivel de manzanas (segmento)
                          strata = ~estrato, #muestreo estratificado a nivel ciudad (estato)
                          weights = ~ponderador02, #ponderador de corte transversal
                          nest = TRUE,
                          data = elsoc_long)

#Crear una tabla para corroborar ponderadores
datos.tabla <- data.frame(prop.table((svytable(~cambiar_consti + #variable de intrés
                                                 ola, #variable de agrupación (puede ser más de una)
                                               elsoc_diseno, #diseño muestral
                                               round = F #redondear cifras
)))) 


###### PARTE 2: VISUALIZAR LOS DATOS ######
#### Gráficos de barra simples ####

#Nombrar el gráfico (c.1) y seleccionar datos (datos.grafico)
c.1 <-datos.grafico %>% 
  #indicar el contenido del gráfico: ejes y relleno (fill) por ola
  ggplot(aes(y = porcentaje, x = conf_presi, fill = ola, 
             label = as.character(scales::percent(porcentaje, accuracy = .1)))) +
  #fijar el fondo y el marco del gráfico uniforme
  theme_bw() + 
  #geom_col para usar variable y=porcentaje. 'dodge2' para formato side-to-side
  geom_col(position= 'dodge2') +
  #escala del eje y en porcentajes del 0 al 100%
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  #Nombres de los ejes se eliminan
  ylab(label = NULL) +
  xlab(label = NULL) +
  #colores oficiales por ola: degradé 'viridis'
  scale_fill_viridis_d(begin = 0, end = .85, direction = -1, option = 'viridis') +
  #etiquetas por sobre cada barra
  geom_text(vjust = -0.8,
            position = position_dodge(width = .9),
            size= 2.75) +
  #posicionamiento de leyenda arriba
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  #titulo del gráfico
  ggtitle('Confianza en el Presidente(a) de la República según año')

c.1

#Para seleccionar un sólo año a analizar, se realiza un subset de los datos a graficar
datos.subset <- droplevels(subset(datos.grafico, datos.grafico$ola == '2019'))

c.2 <- datos.subset %>% 
  ggplot(aes(y = porcentaje, x = conf_presi, fill = conf_presi, 
             label = as.character(scales::percent(porcentaje, accuracy = .1)))) +
  theme_bw() + 
  geom_col() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_fill_viridis_d(begin = 0, end = .85, direction = -1, option = 'viridis') +
  geom_text(vjust = -0.8,
            position = position_dodge(width = .9),
            size= 2.75) +
  theme(legend.position = 'none',   ## Eliminar leyenda al no ser necesaria
        legend.title = element_blank()) +
  ggtitle('Confianza en el Presidente(a) de la República el año 2019')

c.2

#### Gráfico de Barras con X = 'Ola' - Stack ####

#(1) recodificar la variable de interés en tres categorías
#agregar variable a bbdd original
elsoc_long$conf_presi_rec<- car::recode(elsoc_long$conf_presi, 
                                        "c('Nada','Poco')=1;
                                           c('Algo')=2;
                                           c('Bastante','Mucho')=3")
elsoc_long$conf_presi_rec <- factor(elsoc_long$conf_presi_rec,
                                    labels = c('Nada o Poco', 'Algo', 'Bastante o Mucho'))

#volver a realizar el código de la encuesta debido a la presencia de una nueva variable
elsoc_diseno <- svydesign(ids = ~segmento, strata = ~estrato, 
                          weights = ~ponderador02,nest = TRUE, 
                          data = elsoc_long)

#crear tabla para el gráfico
datos.grafico <- data.frame((svytable(~conf_presi_rec + ola, elsoc_diseno, 
                                      round = F))) %>% group_by(ola) %>% 
  mutate(porcentaje=Freq/sum(Freq))

#(2) crear gráfico apilado (Stack)

c.3 <-datos.grafico %>% 
  ggplot(aes(y = porcentaje, x = ola, fill = conf_presi_rec, 
             label = as.character(scales::percent(porcentaje, accuracy = .1)))) + 
  theme_bw() + 
  geom_col(position = 'Stack') +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_fill_viridis_d(begin = .33, end = .66, direction = -1, option = 'viridis') +
  #cambiar geom_text tal que: Stack y cambio colores
  geom_text(position = position_stack(vjust = .5),
            size= 2.75, color = rep.int(c('black', 'white', 'white'), 4)) + 
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  ggtitle('Confianza en el Presidente(a) de la República cada año')

c.3

#### Gráfico de Barras con dos categorías: edad y ola ####

datos.grafico <- data.frame((svytable(~conf_presi_rec 
                                      + ola + edad, elsoc_diseno, 
                                      round = F))) %>% group_by(ola, 
                                                                edad) %>% mutate(porcentaje=Freq/sum(Freq))


#Seleccionar una sóla respuesta, se realiza un subset de los datos a graficar
datos.subset <- droplevels(subset(datos.grafico, 
                                  datos.grafico$conf_presi_rec== 'Nada o Poco'))

c.4 <-datos.subset %>% 
  ggplot(aes(y = porcentaje, x = edad, fill = ola, 
             label = as.character(scales::percent(porcentaje, accuracy = .1)))) + 
  theme_bw() + 
  geom_col(position = 'Dodge') +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_fill_viridis_d(begin = 0, end = .85, direction = -1, option = 'viridis') +
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  geom_text(vjust = -0.8,
            position = position_dodge(width = .9),
            size= 2.75) +
  ggtitle('"Poco o Nada" de Confianza en el Presidente(a) de la República según edad y año')

c.4


#### Gráfico de Barras con más de una variable de interés ####
#agregar variables a bbdd original

#partidos politicos
elsoc_long$conf_part_rec<- car::recode(elsoc_long$conf_part, 
                                       "c('Nada','Poco')=1;
                                           c('Algo')=2;
                                           c('Bastante','Mucho')=3")
elsoc_long$conf_part_rec <- factor(elsoc_long$conf_part_rec,
                                   labels = c('Nada o Poco', 'Algo', 'Bastante o Mucho'))

#carabineros
elsoc_long$conf_carb_rec<- car::recode(elsoc_long$conf_carb, 
                                       "c('Nada','Poco')=1;
                                           c('Algo')=2;
                                           c('Bastante','Mucho')=3")
elsoc_long$conf_carb_rec <- factor(elsoc_long$conf_carb_rec,
                                   labels = c('Nada o Poco', 'Algo', 'Bastante o Mucho'))

#volver a realizar el código de la encuesta debido a la presencia de una nueva variable
elsoc_diseno <- svydesign(ids = ~segmento, strata = ~estrato, 
                          weights = ~ponderador02,nest = TRUE, data = elsoc_long)


#Luego se realiza las tablas de porcentajes y se trasponen por cada variable.

datos.presi <- data.frame((svytable(~conf_presi_rec + ola, elsoc_diseno, round = F))) %>% group_by(ola) %>% mutate(p_presi=Freq/sum(Freq))
datos.presi$ola <- NULL
datos.part <- data.frame((svytable(~conf_part_rec + ola, elsoc_diseno, round = F))) %>% group_by(ola) %>% mutate(p_part=Freq/sum(Freq))
datos.part$ola <- NULL
datos.carb <- data.frame((svytable(~conf_carb_rec + ola, elsoc_diseno, round = F))) %>% group_by(ola) %>% mutate(p_carb=Freq/sum(Freq))

datos.grafico1<- cbind(datos.presi, datos.part, datos.carb)

#trasponer según variable
datos.grafico <- datos.grafico1 %>% 
  pivot_longer(cols = starts_with('conf_'))  %>%
  mutate(variable = factor(name, labels = c('Presidente/a de la Republica', 'Partidos Políticos','Carabineros de Chile'))) %>% 
  drop_na()

datos.grafico$porcentaje <- with(datos.grafico, case_when(
  variable == 'Carabineros de Chile' ~ p_carb,
  variable == 'Presidente/a de la Republica' ~ p_presi,
  variable == 'Partidos Políticos' ~ p_part))

#graficamos

c.5 <-datos.grafico %>% 
  ggplot(aes(y = porcentaje, x = ola, fill = value, 
             label = as.character(scales::percent(porcentaje, accuracy = .1)))) +
  theme_bw() + 
  geom_col(width = 0.75, position = "Stack") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_fill_viridis_d(begin = .33, end = .66, direction = -1, option = 'viridis') +
  facet_grid(.~variable) + 
  geom_text(position = position_stack(vjust = .5),
            size= 1.75, color = rep(c('black', 'white', 'white'), 12))+
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  ggtitle('Confianza en instituciones por año')

c.5

#### TAREA: Gráfico de Barras que mide cambios en el tiempo ####
#Recolectar variables a utilizar
datos.grafico3 <- data.frame(cbind(elsoc_long$idencuesta, 
                                   elsoc_long$ola,elsoc_long$estatus,
                                   elsoc_long$ponderador02))

colnames(datos.grafico3) <- c("idencuesta", "ola", "estatus", "ponderador02")

#Pasar tabla de long a wide
datos.grafico3 <- panel_data(datos.grafico3, id = idencuesta, wave = ola)
datos.wide <- widen_panel(datos.grafico3, separator = "_")

#Recodificar cambios
datos.wide$cambio_estatus_4 <- factor(with(datos.wide, case_when(
  estatus_4 == estatus_3 & !is.na(estatus_4) & !is.na(estatus_3) ~ 2,
  estatus_4 > estatus_3  & !is.na(estatus_4) & !is.na(estatus_3) ~ 1,
  estatus_4 < estatus_3  & !is.na(estatus_4) & !is.na(estatus_3) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))
datos.wide$cambio_estatus_3 <- factor(with(datos.wide, case_when(
  estatus_3 == estatus_2 & !is.na(estatus_3) & !is.na(estatus_2) ~ 2,
  estatus_3 > estatus_2  & !is.na(estatus_3) & !is.na(estatus_2) ~ 1,
  estatus_3 < estatus_2  & !is.na(estatus_3) & !is.na(estatus_2) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))
datos.wide$cambio_estatus_2 <- factor(with(datos.wide, case_when(
  estatus_2 == estatus_1 & !is.na(estatus_2) & !is.na(estatus_1) ~ 2,
  estatus_2 > estatus_1  & !is.na(estatus_2) & !is.na(estatus_1) ~ 1,
  estatus_2 < estatus_1  & !is.na(estatus_2) & !is.na(estatus_1) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))

#Debido a la complejidad de agregar los ponderadores a partir del diseño encuesta, se realiza un ajuste manual con el ponderador02 del año 2016.
datos.grafico <- datos.wide %>% 
  pivot_longer(cols = c(cambio_estatus_2,cambio_estatus_3,cambio_estatus_4), 
               names_to = "cambio_estatus", 
               values_to = "respuesta") %>% 
  drop_na()%>%
  group_by(cambio_estatus, respuesta) %>% 
  summarise(n1=sum(ponderador02_1,na.rm=T)) %>% 
  mutate(n2 = sum(n1, na.rm = TRUE), porcentaje = n1/n2)%>% 
  ungroup()

#Recodificamos los periodos de cambio
datos.grafico$cambio_estatus<- car::recode(datos.grafico$cambio_estatus, 
                                           "'cambio_estatus_2'='2016-2017';'cambio_estatus_3'='2017-2018';
                           'cambio_estatus_4'='2018-2019'")

#Creamos el gráfico

c.5 <-ggplot(datos.grafico, aes(y=porcentaje, x=cambio_estatus,fill=respuesta ,label = 
                                  as.character(scales::percent(porcentaje, accuracy = .1))))+
  theme_bw()+
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_fill_viridis_d(begin = .33, end = .66, direction = -1, option = 'viridis') +
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  geom_text(position = position_stack(vjust = .5),
            size= 2.75,
            color = rep(c('black', 'white', 'white'), 3))+
  ggtitle('Cambio en el tiempo de percepción del estatus social')

c.5


#### Gráficos Alluvial para cuatro olas ####
#Paso 1: Crear una base de datos que agrupa la frecuencia de la variables de 
#interés por otra variable de agrupación (ej. `ola`).

datos.d.1 <- data.frame((svytable(~idcoal + ola + idencuesta, elsoc_diseno, round = F))) %>% dplyr::filter(Freq>0)  %>% group_by(ola) %>% mutate(porcentaje=Freq/sum(Freq)) %>% na.omit()

#Paso 2: Crear una tabla que agrupa frecuencias en función de las categorías de 
#respuesta tanto de la variable de interés como de la variable de agrupación (ej. `ola`)

etiquetas.d.1 <- data.frame((svytable(~idcoal + ola, elsoc_diseno, round = F))) %>% group_by(ola) %>% mutate(porcentaje=Freq/sum(Freq)) %>% na.omit() %>% 
  mutate(idencuesta = 1)


d.1 <- ggplot(datos.d.1, aes(x = ola, fill = idcoal, stratum = idcoal,
                             alluvium = idencuesta, y = porcentaje))+
  ggalluvial::geom_flow(alpha = .66) + 
  ggalluvial::geom_stratum(linetype = 0) +
  scale_y_continuous(labels = scales::percent) + 
  ylab(label = NULL) +
  xlab(label = NULL) + 
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  scale_fill_viridis_d(begin = 0, end = .95, direction = -1, option = 'viridis') +
  geom_text(data = etiquetas.d.1, 
            aes(label = ifelse(porcentaje > 0.03 , scales::percent(porcentaje, accuracy = .1),"")),
            position = position_stack(vjust = .5),
            show.legend = FALSE,
            size = 2,
            color = rep('white'))+
  ggtitle('Cambio de frecuencias de indentificación con coalición política según año')

d.1
#### Gráficos Alluvial para dos categorías #### 
#Paso 1.
datos.d.3 <- data.frame((svytable(~idcoal + 
                                    ola + idencuesta + socup + 
                                    sexo, elsoc_diseno, round = F))) %>% 
  dplyr::filter(Freq>0)  %>% group_by(ola,sexo) %>%
  mutate(porcentaje=Freq/sum(Freq)) %>% na.omit()

#Paso 1.1 crear un subset sólo para los años 2017 y 2019
subset.d.3 <- droplevels(subset(datos.d.3, datos.d.3$ola == '2017' | 
                                  datos.d.3$ola == '2019'))

#Paso 2
etiquetas.d.3 <- data.frame((svytable(~idcoal + ola + socup + sexo, elsoc_diseno, round = F))) %>% group_by(ola,sexo) %>% mutate(porcentaje=Freq/sum(Freq)) %>% na.omit() %>% 
  mutate(idencuesta = 1)

#Paso 2.2 crear un subset sólo para los años 2017 y 2019
etiquetas.d.3 <- droplevels(subset(etiquetas.d.3, etiquetas.d.3$ola == '2017' | etiquetas.d.3$ola == '2019'))


d.2 <- ggplot(subset.d.3, aes(x = ola, fill = socup, stratum = socup, 
                              alluvium = idencuesta, y = porcentaje)) +
  ggalluvial::geom_flow(alpha = .66) + 
  ggalluvial::geom_stratum(linetype = 0) +
  scale_y_continuous(labels = scales::percent) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  scale_fill_viridis_d(begin = 0, end = .9, direction = -1, option = 'viridis') +
  facet_wrap(.~sexo)+
  geom_text(data = etiquetas.d.3, 
            aes(label = ifelse(porcentaje > 0.1 , scales::percent(porcentaje, accuracy = .1),"")),
            position = position_stack(vjust = .5),
            show.legend = FALSE,
            size = 2.75,
            color = rep('white'))+
  ggtitle('Cambio de frecuencias en situación ocupacional para hombres y mujeres')

d.2

#### Gráficos de Lineas simples ####
datos.e.1 <- data.frame((svytable(~conf_presi + ola, elsoc_diseno, round = F))) %>% group_by(ola) %>% mutate(porcentaje=Freq/sum(Freq))


e.1 <- ggplot(datos.e.1,aes(y = porcentaje, x = ola, color = conf_presi, group = conf_presi,
                            label = as.character(scales::percent(porcentaje, accuracy = .1)))) +
  theme_bw() +   
  geom_line(size = 1) +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = 0, end = .95, direction = 1, option = 'viridis') +
  geom_text_repel(posilinetion = position_dodge(width = .9),
                  size= 2.25) + 
  theme(legend.position = 'top',
        legend.title = element_blank())

e.1

#### Gráficos de Lineas con más de una variable de interés ####

#Paso 1: Similar al código que gráfico de barras
datos.presi <- data.frame((svytable(~conf_presi_rec + ola + 
                                      zona, elsoc_diseno, round = F))) %>% 
  group_by(ola, zona) %>% mutate(p_presi=Freq/sum(Freq))
datos.presi$ola <- NULL
datos.presi$zona <- NULL

datos.part <- data.frame((svytable(~conf_part_rec + ola + 
                                     zona, elsoc_diseno, round = F))) %>% 
  group_by(ola, zona) %>% mutate(p_part=Freq/sum(Freq))
datos.part$ola <- NULL
datos.presi$zona <- NULL

datos.carb <- data.frame((svytable(~conf_carb_rec + ola + 
                                     zona, elsoc_diseno, round = F))) %>% 
  group_by(ola, zona) %>% mutate(p_carb=Freq/sum(Freq))

datos.grafico.e.2<- cbind(datos.presi, datos.part, datos.carb)

#trasponer según variable
datos.grafico <- datos.grafico.e.2 %>% 
  pivot_longer(cols = starts_with('conf_'))  %>%
  mutate(variable = factor(name,
                           labels = c('Presidente/a de la Republica', 'Partidos Políticos', 
                                      'Carabineros de Chile'))) %>% 
  drop_na()

datos.grafico$porcentaje <- with(datos.grafico, case_when(
  variable == 'Carabineros de Chile' ~ p_carb,
  variable == 'Presidente/a de la Republica' ~ p_presi,
  variable == 'Partidos Políticos' ~ p_part))

#Paso 2: filtrar el subset manteniendo "Nada o Poco"
subset.e.2 <- droplevels(subset(datos.grafico, value == 'Nada o Poco'))

e.2 <- ggplot(subset.e.2,aes(y = porcentaje, x = ola, color = variable, group = variable,
                             label = as.character(scales::percent(porcentaje, accuracy = .1)))) +
  theme_bw() +  
  geom_line(size = 1) +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_color_viridis_d(begin = .33, end = .66, direction = 1, option = 'viridis') +
  facet_wrap(.~zona)+
  geom_text(vjust = -0.8,
            posilinetion = position_dodge(width = .9),
            size= 1.75) +
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  ggtitle('Cambio de frecuencias en el grado de confianza "Nada" en algunas instituciones')

e.2


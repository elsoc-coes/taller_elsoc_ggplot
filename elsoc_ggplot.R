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
```

###### g. Guardar base de datos formato R.Data (opcional) ###### 

#save(elsoc_long, file "[ruta de carpeta local]/elsoc_long.RData")
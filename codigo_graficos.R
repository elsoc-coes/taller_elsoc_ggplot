
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


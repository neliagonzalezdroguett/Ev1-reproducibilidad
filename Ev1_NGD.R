##Evaluación Sumativa N°1 – Reproducibilidad y comunicación de resultados
## Nelia González Droguett


###############################CAPITULO 1: INTRODUCCION###############################

# El Instituto Nacional sobre el Abuso de Alcohol y Alcoholismo (NIAAA) define niveles y patrones de consumo de alcohol, 
# distinguiendo entre consumo de bajo riesgo, riesgoso y episodios intensivos, cada uno asociado a distintos daños potenciales. 
# Estas definiciones permiten comparar estudios, verificar perfiles sociodemográficos de consumo, monitorear tendencias y diseñar 
# intervenciones de salud basadas en evidencia.

# Para que esa evidencia sea sólida, es clave hacer investigación rigurosa sobre consumo de alcohol, con datos de calidad y procedimientos 
# transparentes.En este contexto, la reproducibilidad implica que otras personas puedan obtener los mismos resultados con los mismos datos 
# y código, mientras que la replicabilidad se refiere a que estudios independientes encuentren resultados consistentes con nuevos datos, 
# fortaleciendo la confianza en los hallazgos.

# En el presente análisis, se podrá ver la relación entre la cantidad (o frecuencia) de consumo y algunas variables sociodemográficas de 
# interés.



###############################CAPITULO 2: METODOLOGIA###############################

#Antes de partir con el ejercicio, serán instaladas y cargadas las librerías que serán utilizadas en la evaluación formativa.

#Cargando librerías necesarias

library(tidyr)
library (dplyr)
library(purrr)
library(ggplot2)
library(scales)
library(haven)
library(skimr)
library(stringr)
library(purrr)
library(srvyr)


##Antes de cargar la base de datos, se configurará la carpeta del proyecto. Lugar donde se encontrará toda la información con la que se trabajará.

setwd("C:/Users/nelia/OneDrive - Fundación Paz Ciudadana/Nelia personal/Diplomado Data Science para Ciencias Sociales/R/Módulo 4/Ev1")

#Confirmando que estamos en la carpeta correcta
getwd()

#Cargando la base de datos solicitada en las instrucciones. Correspondiente a la Encuesta Nacional de Drogas (SENDA)

ev_senda <- read_rds("SENDA2022.RDS")

#La instrucción señala que hay que buscar una variable de interés y tres variables secundarias.Para poder realizar esto es necesario leer el
#manual correspondiente a la encuesta, puesto que aquí está todo el detalle metodológico de la misma.
#Una vez revisada las variables, se procedió a revisar una breve búsqueda bibliográfica en torno al tema del consumo de alcohol. 
#Según el NIAAA (2007), el consumo excesivo de alcohol (heavy drinking) es definido:
#1) Para los hombres, consumir cinco o más bebidas alcohólicas en cualquier día o 15 o más bebidas alcohólicas a la semana;
#2) Para las mujeres, consumir de cuatro o más bebidas alcohólicas en cualquier día u ocho o más bebidas alcohólicas a la semana.

#A raíz de esta información, se decidió caracterizar la variable de bebidas alcohólicas consumidas cada vez.Por lo que las variables escogidas
#son las siguientes:

# OH_1 (esta es el filtro de la pregunta OH_9, por lo que se utilizará para el análisis): ¿Ha tomado Ud. alcohol alguna vez en su vida? 
# OH_9: ¿Cuántos tragos suele tomar usted en un día típico de consumo de alcohol? 
# SEXO 
# EDAD 
# REGION 

# A partir de esta selección, será generado un dt con las variables seleccionadas

senda_final <- ev_senda %>% 
  select(OH_1, OH_9, SEXO, EDAD, REGION, FACTOR_EXPANSION)

# Antes de comenzar a hacer al análisis propiamente tal y, al realizar la revisión del manual es posible dar cuenta que el análisis de la encuesta
#debe realizarse con un factor de expansión (variable incluida en la selección=. Éste será utilizado en forma adhoc, 
#en la medida que vayan analizándose los datos.


#A continuación, se dará comienzo al análisis solicitado en la pauta



############## SUBCAPÍTULO 2.1 Exploración inicial de los datos#################################


#a) Revisión de estructura
glimpse(senda_final) #revisando las características generales del data frame
str(senda_final) #complementando código anterior

#b) Tipo de variables
sapply(senda_final[, c("OH_1", "OH_9", "SEXO", "EDAD", "REGION")], class)
sapply(senda_final[, c("OH_1", "OH_9", "SEXO", "EDAD", "REGION")], typeof)

#c) Detección de valores perdidos 
skim(senda_final) #Para revisar si hay valores fuera de rango


#d) Distribución de variables 
senda_final %>% #Revisando frecuencias
  count(
    OH_1,
    OH_9,
        SEXO,
        EDAD,
    REGION)


############## SUBCAPÍTULO 2.2 Limpieza y preparación de los datos#################################


#a) Tratamiento de valores perdidos

#Según la exploración inicial de datos, la única variable que contiene valores NA es OH_1, es decir,"Consumo de alcohol".
senda_final$OH_1[senda_final$OH_1 == 88] <- NA # Reemplaza valores 88 por NA 
senda_final$OH_1[senda_final$OH_1 == 99] <- NA # Reemplaza valores 99 por NA 


#b) Recodificaciones: para un tratamiento más ordenado, se agruparán categorías para edad y regiones

#Agrupando edad en tramos, según las categorías usadas en el informe oficial de resultados de la encuesta
senda_final <- senda_final%>%  
  mutate(edad_recod =case_when(
    EDAD %in% c(12:18)~ 1,
    EDAD %in% c(19:25)~ 2,
    EDAD %in% c(26:34)~ 3,
    EDAD %in% c(35:44)~ 4,
    EDAD %in% c(45:70)~ 5
  )          )

#Agrupando regiones en macrozonas, según las categorías del Ministerio de Ciencia y Tecnología
#(https://ayuda.anid.gob.cl/hc/es/articles/360048066052--Cu%C3%A1les-son-las-Macrozonas-del-Ministerio-de-Ciencia-Tecnolog%C3%ADa-Conocimiento-e-Innovaci%C3%B3n)
# Las agrupaciones son: 
#Macrozona Norte: Arica y Parinacota, Antofagasta, Tarapacá, Atacama
#Macrozona Centro: Coquimbo, Valparaíso
#Macrozona Centro Sur: O'higgins, Maule, Ñuble, Biobio
#Macrozona Sur: La Araucanía, Los Ríos, Los Lagos
#Macrozona Austral: Aysén, Magallanes y la Antártica Chilena
#La Región Metropolitana conforma su propia macrozona.


senda_final <- senda_final %>%
  mutate(macrozonas =case_when(
    REGION %in% c(1:3, 15)~ 1,
    REGION %in% c(4,5)~ 2,
    REGION %in% c(6:8, 16)~ 3,
    REGION %in% c(9:10, 14)~ 4,
    REGION %in% c(11, 12)~ 5,
    REGION %in% c(13)~ 6
  )          )

#c) Categorizaciones:se renombrarán algunas variables y otras serán manejadas a modo de factor 

#Renombrando variable OH_1 y dejándola como factor
senda_final <- senda_final %>%
  rename(Consume_alc = OH_1)

senda_final <- senda_final %>%
  mutate(Consume_alc= case_when(
    Consume_alc ==1 ~ "Consume alcohol",
    Consume_alc ==2 ~ "No consume alcohol"
))

#Renombrando variable OH_9 y dejándola como factor
senda_final <- senda_final %>%
  rename(Freq_alc = OH_9)

senda_final <- senda_final %>% 
  mutate(
    Freq_alc = factor(
      Freq_alc,
      levels = c(0, 1, 2, 3, 4),
      labels = c("0 a 2 tragos", "3 a 4 tragos", "5 a 6 tragos", "7 a 9 tragos", "10 o más tragos")
    )
  )

#Dejando variable sexo como factor
senda_final <- senda_final %>% 
  mutate(
    SEXO = factor(
      SEXO,
      levels = c(1, 2),
      labels = c("Hombre", "Mujer")
    )
  )

#Categorizando la variable "edad" por tramos
senda_final <- senda_final %>%
  mutate(edad_recod= case_when(
    edad_recod ==1 ~ "12 a 18 años",
    edad_recod ==2 ~ "19 a 25 años",
    edad_recod ==3 ~ "26 a 34 años",
    edad_recod ==4 ~ "35 a 44 años",
    edad_recod ==5 ~ "45 años o más"
  ),
  edad_recod = factor(
    edad_recod,
    levels = c(
      "12 a 18 años",
      "19 a 25 años",
      "26 a 34 años",
      "35 a 44 años",
      "45 años o más"
    )
  )
  )


#Categorizando la variable "macrozonas"
senda_final <- senda_final %>%
  mutate(macrozonas= case_when(
    macrozonas ==1 ~ "Macrozona Norte",
    macrozonas ==2 ~ "Macrozona Centro",
    macrozonas ==3 ~ "Macrozona Centro Sur",
    macrozonas ==4 ~ "Macrozona Sur",
    macrozonas ==5 ~ "Macrozona Austral",
    macrozonas ==6 ~ "Región Metropolitana"
  ),
  macrozonas = factor(
    macrozonas,
    levels = c(
      "Macrozona Norte",
      "Macrozona Centro",
      "Macrozona Centro Sur",
      "Macrozona Sur",
      "Macrozona Austral",
      "Región Metropolitana"
    )
  )
  )
    
###############################CAPITULO 3: ANALISIS Y RESULTADOS ###############################



############## SUBCAPÍTULO 3.1 Análisis descriptivo #################################

#Como la frecuencia del consumo de alcohol (Freq_alc) se encuentra definida por quienes efectivamente consumen, es necesario definir
#una nueva base de trabajo, que considere sólo a los consumidores. Por lo tanto:
senda_consum <- senda_final %>% 
  filter(
    Consume_alc == "Consume alcohol",
    !is.na(Freq_alc)
  )

#Como la base de la encuesta debe ponderarse, aplicamos el factor de expansión (FACTOR_EXPANSION) variable por variable, al momento de armar
#las tablas de frecuencia

#Tabla i: Frecuencia de consumo
tabla_freq_alc <- senda_consum %>% 
  count(
    Freq_alc,
    wt   = FACTOR_EXPANSION,
    name = "n_pond"
  ) %>% 
  mutate(
    pct_pond = 100 * n_pond / sum(n_pond)
  )

tabla_freq_alc


#Tabla ii: sexo
tabla_sexo <- senda_consum %>% 
  count(
    SEXO,
    wt   = FACTOR_EXPANSION,
    name = "n_pond"
  ) %>% 
  mutate(
    pct_pond = 100 * n_pond / sum(n_pond)
  )

tabla_sexo

#Tabla iii: macrozonas
tabla_macrozonas <- senda_consum %>% 
  count(
    macrozonas,
    wt   = FACTOR_EXPANSION,
    name = "n_pond"
  ) %>% 
  mutate(
    pct_pond = 100 * n_pond / sum(n_pond)
  )

tabla_macrozonas


#Tabla iv: Tramos de edad
tabla_edad <- senda_consum %>% 
  count(
    edad_recod,
    wt   = FACTOR_EXPANSION,
    name = "n_pond"
  ) %>% 
  mutate(
    pct_pond = 100 * n_pond / sum(n_pond)
  )

tabla_edad


# Consolidando en una única tabla

# Creando un vector con los nombres de las variables a describir
lista_alc <- c("Freq_alc", "SEXO", "macrozonas", "edad_recod")

# Haciendo una tabla resumen según la estructura previamente definida
tabla_resumen <- map_dfr(lista_alc, function(v) {
  
  senda_consum %>% 
    count(.data[[v]], wt = FACTOR_EXPANSION, name = "n_pond") %>% 
    mutate(
      pct_pond = 100 * n_pond / sum(n_pond),
      variable  = v,
      categoria = as.character(.data[[v]])
    ) %>% 
    select(variable, categoria, pct_pond)
})


tabla_resumen


############## SUBCAPÍTULO 3.2 Análisis bivariado y gráficos asociados#################################

#En este caso, se considerará la siguiente estructura:
  #Variable principal: Frecuencia de consumo de alcohol
  #Variables secundarias: sexo, tramos de edad y macrozonas.


# Bivariado i: Frecuencia de consumo de alcohol y sexo

  #Tabla i:
tab_freq_sexo <- xtabs(
  FACTOR_EXPANSION ~ SEXO + Freq_alc,
  data = senda_consum
)

tab_freq_sexo <- prop.table(tab_freq_sexo, margin = 1) * 100

tab_freq_sexo

  #Gráfico i: se utilizará como base la tabla anterior, pero debe ser transformada a data.frame primero

df_freq_sexo <- as.data.frame(tab_freq_sexo)

#Armando el gráfico
ggplot(df_freq_sexo,
       aes(x = SEXO, y = Freq, fill = Freq_alc)) +
  geom_col() +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title= "Frecuencia de consumo de alcohol según sexo",
    x    = "Sexo",
    y    = " % ",
    fill = "Frecuencia de consumo de alcohol"
  ) +
  theme_minimal()

  #Interpretación i:
#Entre quienes declaran consumir alcohol, la distribución del número de tragos consumidos difiere según sexo. Entre las mujeres que consumen 
#alcohol, el consumo se concentra principalmente en cantidades bajas: un 74% reporta haber consumido entre 0 y 2 tragos. 
#En los hombres, esta proporción alcanza alrededor del 50%, mientras que casi la mitad (aproximadamente un 50%) declara haber consumido 
#3 o más tragos. Esto indica que, entre las personas que consumen alcohol, los hombres tienden a consumir mayores cantidades que las mujeres.




# Bivariado ii: Frecuencia de consumo de alcohol y tramos de edad

  #Tabla ii:
tab_freq_edad <- xtabs(
  FACTOR_EXPANSION ~ Freq_alc + edad_recod,
  data = senda_consum
)

tab_freq_edad <- prop.table(tab_freq_edad, margin = 1) * 100

tab_freq_edad

  #Gráfico ii: se utilizará como base la tabla anterior, pero debe ser transformada a data.frame primero

df_freq_edad <- as.data.frame(tab_freq_edad)

#Armando el gráfico

ggplot(df_freq_edad,
       aes(x = edad_recod,
           y = Freq,
           color = Freq_alc,
           group = Freq_alc)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Frecuencia de consumo de alcohol según edad",
    x     = "Grupo de edad",
    y     = "% (según frecuencia de consumo)",
    color = "Frecuencia de consumo"
  ) +
  theme_minimal()

  #Interpretación ii:
#Entre quienes consumen alcohol, la distribución por edad de las distintas cantidades de consumo muestra que los consumos medios y altos
#se concentran principalmente en personas adultas, desde 19 años. En todos los niveles de cantidad de tragos, las personas de 12 a 18 años representan una 
#proporción reducida. En cambio, los grupos de 26 a 34 y de 35 a 44 años concentran gran parte de quienes declaran consumos de 5 o más tragos, 
#mientras que, desde los 45 años o más comienza a volver el consumo más bajo (0 a 2 tragos), aunque también mantiene presencia en niveles altos.



# Bivariado iii: Frecuencia de consumo de alcohol y macrozona

  #Tabla iii:
tab_freq_macrozona <- xtabs(
  FACTOR_EXPANSION ~ Freq_alc + macrozonas,
  data = senda_consum
)

tab_freq_macrozona <- prop.table(tab_freq_macrozona, margin = 2) * 100

tab_freq_macrozona

  #Gráfico iii: se utilizará como base la tabla anterior, pero debe ser transformada a data.frame primero

df_freq_macrozona <- as.data.frame(tab_freq_macrozona)

#Armando el gráfico:
ggplot(df_freq_macrozona,
      aes(x = macrozonas, y = Freq, fill = Freq_alc)) +
  geom_col() +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  coord_cartesian(ylim = c(0, 100)) +  # recorta visualmente, sin borrar datos
  labs(
    title = "Frecuencia de consumo de alcohol según macrozona",
    x     = "Macrozona",
    y     = "%",
    fill  = "Frecuencia de consumo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(size = 5),
    axis.title.x = element_text(size = 10)
  )


  # Interpretación:
# En términos generales, entre quienes declaran comsumir alcohol, se observa una predominancia del bajo consumo, siendo mayoritaria 
# transversalmente a las distintsd macrozonas. En este caso, la Macrozona Centro es la que presenta la mayor proporción de consumos bajos (65%) 
# y la Macrozona Norte la menor (51%). Por otro lado, las diferencias aparecen con más claridad en los consumos altos (5 o más tragos): 
#en la Macrozona Norte cerca de un 21% declara haber consumido 5 o más tragos, proporción que duplica a la observada en la Macrozona Centro 
#y Centro Sur (alrededor de 8–9%) y que es superior a la de la Macrozona Sur y Austral (~10%). 
#La Región Metropolitana se ubica en una posición intermedia, con cerca de un 13% de consumos altos.




###############################CAPITULO 4: CONCLUSIONES ##################################

# En conjunto, los análisis muestran que, si bien la mayoría de la población se concentra en consumos de menor cantidad 
# (0 a 2 tragos en todas las macrozonas), existen diferencias sistemáticas según sexo, edad y territorio.

# Entre quienes consumen alcohol, las mujeres tienden a concentrarse más en cantidades bajas, mientras que los hombres 
# presentan una mayor proporción de consumos de 3 o más tragos, lo que se aproxima a los patrones de mayor riesgo definidos por la NIAAA.

# Por edad, los consumos más altos se observan principalmente en personas adultas de 26 a 44 años, mientras que las y los adolescentes 
# representan una fracción menor en todos los niveles de cantidad, y el grupo de 45 años o más se asocia más fuertemente a consumos bajos.

# Territorialmente, todas las macrozonas muestran predominio de consumos bajos, pero la Macrozona Norte y, en menor medida, 
# la Región Metropolitana concentran una mayor proporción de consumos altos (5 o más tragos), lo que sugiere contextos de mayor riesgo.

# Estos resultados ilustran la importancia de describir con precisión los patrones de consumo y de explicitar los criterios de medición, 
# de modo que otros equipos puedan reproducir los cálculos a partir de los mismos datos y código (reproducibilidad) y, a la vez, 
# contrastar estos hallazgos con nuevas encuestas o muestras independientes (replicabilidad), fortaleciendo la evidencia disponible 
# para el diseño de políticas y estrategias de prevención en materia de consumo de alcohol.




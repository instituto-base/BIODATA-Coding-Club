#DATA CLUB#
#Trabajando con dataset grandes
# https://ourcodingclub.github.io/tutorials/seecc_1/ puedes seguir el tutorial
#aquí y descargar los datos
#Este script fue modificado del original

#Preparación

setwd("C:/R/CLUB_large") #identificar carpeta

#Instalar paquetes
install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("broom")
install.packages("ggplot2")
install.packages("ggExtra")
install.packages("ggthemes")
install.packages("maps")
install.packages("RColorBrewer")
install.packages("rgbif")
install.packages("colourpicker")

#Cargar paquetes
library(readr)
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(ggExtra)
library(ggthemes)
library(maps)
library(RColorBrewer)
library(rgbif)
library(colourpicker)



#####Primera parte: formato de los datos#####
#Cargar datos
load("LPIdata_Feb2016.RData")

#Inspeccionar los datos
View(head(LPIdata_Feb2016))

#Transformar desde un formato wide a uno long
LPI_long <- gather(data = LPIdata_Feb2016, key = "year", value = "pop", 26:70 )

#Arreglar años
LPI_long$year <- parse_number(LPI_long$year)

#Renombrar la variable names para más consistencia
names(LPI_long)
names(LPI_long) <- gsub(".", "_", names(LPI_long), fixed = TRUE)
names(LPI_long) <- tolower(names(LPI_long))
names(LPI_long)

#Crear una nueva columna con el género y la especie juntos
LPI_long$genus_species_id <- paste(LPI_long$genus,
                                   LPI_long$species, LPI_long$id, 
                                   sep = "_")

#Comprobar los datos
View(LPI_long[c(1:5,500:505,1000:1005),])
# Se puden utilizar los [] para hacer un subconjunto de data frames 
#[filas, columnas]
# Si se quieren todas las filas/columnas,  se añade una coma en la posición de
#la fila/columna

#Eliminar caracteres especiales como "/"
LPI_long$country_list <- gsub(",", "", 
                              LPI_long$country_list, 
                              fixed = TRUE, useBytes = TRUE)
LPI_long$biome <- gsub("/", "", LPI_long$biome, fixed = TRUE)

# Examinar datos ordenados
View(head(LPI_long))

#####Parte 2: manupulación de los datos#####

#Eliminar filas duplicadas
LPI_long <- distinct(LPI_long)
#Eliminar datos daltantes o infinitos
LPI_long_fl <- filter(LPI_long, is.finite(pop))


#Mantener las especies con >5 años de datos y calcular la extención de monitoreo
LPI_long <- LPI_long_fl %>%
  group_by(genus_species_id) %>%  #agrupar filas para que cada grupo sea una población
  mutate(maxyear = max(year), minyear = min(year),  # Crear colummnas para los primeros años y los más recientes en lo que los datos fueron recolectados
         lengthyear = maxyear-minyear,  # Crear una columna para la duración de los datos
         scalepop = (pop-min(pop))/(max(pop)-min(pop))) %>%  # Escalar la tendecia de los datos poblacionales para que los valores sean entre 0 y 1re between 0 and 1
  filter(is.finite(scalepop),  # remover NAs
         lengthyear > 5) %>%  #mantener datos de mayor a cinco años
  ungroup()  # Remover grupos

#Calcular estadísticos primarios apra cada bioma
LPI_biome_summ <- LPI_long %>%
  group_by(biome) %>%  # agrupar por bioma
  summarise(populations = n(),   # Crear columnas, number of populations
            mean_study_length_years = mean(lengthyear),  # mean study length
            max_lat = max(decimal_latitude),  # max latitude
            min_lat = min(decimal_latitude),  # max longitude
            dominant_sampling_method = names(which.max(table(sampling_method))),  # método de muestreo modal
            dominant_units = names(which.max(table(units))))  # tipo de unidad modal
##Modelandola población en el tiempo

#Utilizar modelos lineales da la tendencia de las abundacias en el tiempo y extraer los coeficientes de los modelos

#Metodo pipes


LPI_models_pipes <- LPI_long %>%
  group_by(genus_species_id, lengthyear) %>%  #Agrupar por especie y años 
  do(mod = lm(scalepop ~ year, data = .)) %>%  # Crear modelo linear para cada grupo
  mutate(n = df.residual(mod),  # Crear columnas: degrees of freedom
         intercept = summary(mod)$coeff[1],  # intercept coefficient
         slope = summary(mod)$coeff[2],  # slope coefficient
         intercept_se = summary(mod)$coeff[3],  # standard error of intercept
         slope_se = summary(mod)$coeff[4],  # standard error of slope
         intercept_p = summary(mod)$coeff[7],  # p value of intercept
         slope_p = summary(mod)$coeff[8]) %>%  # p value of slope
  ungroup() %>%
  mutate(lengthyear = lengthyear) %>%  # añadir nuevamente la columna duración para que sea guardada en el objeto
  filter(n > 5) # Remover la fila donde los grados de libertad son <5
save(LPI_models_pipes, file = "LPI_models_pipes.RData") #guarda como archivo R

##Visualizar los resultados de los modelos

#Elegir colores con colorpicker
c("#746580", "#BEA2E0BD", "#914ECC")
#Configurar un función personalizada en ggplot2
theme_LPI <- function(){
  theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),
          axis.title.y = element_text(size = 14, face = "plain"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9))
}

#Crar histogramas con la pendiente estimada de cada bioma
biome.plots <- LPI_long %>%
  nest_by(genus_species_id, biome) %>% # Agrupar por especie y bioma
  mutate(mod =list(lm(scalepop ~ year, data = data)))%>% #Hacer el modelo lineal
  reframe(tidy(mod)) %>%  # Extraer los coeficientes del modelo, puede ser necesario utilizas summarise
  dplyr::select(genus_species_id, biome, term, estimate) %>%  # Seleccionar solo columnas necesarias
  spread(term, estimate)  %>% # Separar los estimados en dos columanas - una para intercept, una para year
  unnest(cols = c(genus_species_id,biome)) %>% # Eliminar grupo
  group_by(., biome) %>%
  do(ggsave(ggplot(., aes(x = year)) + 
           geom_histogram(colour="#8B5A00", fill="#CD8500") +
           theme()+ 
           xlab("Rate of population change (slopes)"), 
            filename = gsub("","", base::paste("Biome_LPI/", 
                                          unique(as.character(.$biome)),
                                          ".jpg", sep = "")), device = "jpg"))
#Gráfico del las pendientes estimadas y errores estándar para todas las poblaciones añadiendo histogramas a los márgenes

(all_slopes <- ggplot(LPI_models_pipes, aes(x = lengthyear, y = slope)) +
    geom_pointrange(aes(ymin = slope - slope_se, ymax = slope + slope_se)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_LPI() +
    ylab("Population change\n") +  # \n adds a blank line
    xlab("\nDuration (years)"))
#este es el resultado luego de utilizar ggextra
p1 <- (all_slopes <- ggplot(LPI_models_pipes, aes(x = lengthyear, y = slope)) +
    geom_pointrange(aes(ymin = slope - slope_se, ymax = slope + slope_se)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_LPI() +
    ylab("Population change\n") +  # \n añade una linea vacía
    xlab("\nDuration (years)"))

ggExtra::ggMarginal(
  p = p1,
  type = 'histogram',
  margins = 'both',
  size = 5,
  colour = 'black',
  fill = 'gray'
)

######Parte 3: Visualizar ocurrencia de especies obtenidas desde GBIF####
#utilizar puffin GBIF
load("puffin_GBIF.RData")#Cargar cuando se llegue a la sección
#Utilizar borders() para extraer mapa del paquete maps
map_world <- borders(database = "world", colour = "gray50", fill = "#383838")  # Se puede utiliazar `Colour Picker` para elegir los colores

(map_world_puffin <- ggplot() + map_world +  # gráfico del mapa
    geom_point(data = puffin_GBIF,  # especificar los datos para geom_point()
               aes(x = decimallongitude,  # especificar el eje x como longitud
                   y = decimallatitude,  #  especificar el eje y como latitud
                   colour = scientificname),  # Asignar color basado en el nombre de la especie
               alpha = 0.4,  # Opacidad del punto en  40%
               size = 1) +  # tamaño del punto 1
    scale_color_brewer(palette = "Set1") +   # SEspecificar la paleta de colores en los colores de los puntos
    theme_classic() +  # Remover la cuadrícula y la sombra en el gráfico
    ylab(expression("Latitude ("*degree*")" )) +  # añadir rótulo eje x
    xlab(expression("Longitude ("*degree*")" )) +  # añadir rótulo eje y
    theme(legend.position = "bottom",  # mover leyenda a la parte inferior del gráfico
          legend.title = element_blank()))  # Eliminar el título de la leyenda
install.packages("rgbif")

#Descargar y graficar ocurrencias de GBIF

UK_code <- isocodes[grep("United Kingdom", isocodes$name), "code"]

occur <- occ_search(scientificName = "Fratercula arctica", 
                    country = UK_code, 
                    hasCoordinate = TRUE, 
                    limit = 3000, 
                    year = '2006,2016', 
                    return = "data")
str(occur)
##Mapa de ocurrencias

(map <- ggplot(occur$data, aes(x = decimalLongitude, y = decimalLatitude)) + 
    # especificar solo presentar UK en el mapa
    # Cambair el color y tamaño del mapa 
    borders(database = "world", regions = "UK", colour = "gray40", size = 0.3) +  
    theme_map() + 
    # Cambiar el color y transparencia de los puntos de ocurrencia
    geom_point(alpha = 0.4, colour = "red")) 

#occ_search es una manera simple y rápida de descarga de datos, pero para realizar
#descargas para utilizar en publicaciones o estudios es necesario usar occ_download
#es necesario utilizar las credenciales de gbif y se obtiene la cita del recurso


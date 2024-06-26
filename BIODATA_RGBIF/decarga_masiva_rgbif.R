#Descarga masiva de ocurrencias desde GBIF
#Catalina Marín Cruz
#23-05-2024
#El siguiente script fue preparado para el club de programación BIODATA
#Esta basado en el siguiente post 
#https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/


#Setting----
setwd("C:/R/BIODATA_RGBIF") # selección de carpeta donde están los datos


#Instalacion de paquetes----
install.packages("rgbif")
install.packages("usethis")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("maps")

#Library----
library(rgbif)
library(dplyr)
library(ggplot2)
library(maps)

#Documentos----
liq <- read.csv("liquenes_clasificacion_chile.csv", encoding="UTF-8", sep = ";")
 
#documentos descargadis ahorrar tiempo en el taller 
oc_liq <- read.csv("oc_liq.csv", sep = "\t") # Ocurrencias ya descargadas para
keysNames <- read.csv("taxonKey_liquenes_clasificados.csv", sep = ";")# Tabla con taxonKeys y nombres paraahorrar tiempo en el taller

#Configuración cuenta GBIF----
#Para hacer descargas de más de 100.000 ocurrencias es necesario utilizar las 
#credenciales de nuestra cuenta en GBIF

usethis::edit_r_environ() #Se debe configurar solo una vez, abre otra pestaña 
#con una consola vacía donde tienes que escribir tus
#credenciales, a continuación se indica como
#En la consola que se abre debes copiar lo siguiente y completar con tus datos
GBIF_USER="nombredeusuario" #No correr
GBIF_PWD="contraseña"       #No correr
GBIF_EMAIL="correo@gbif.cl" #No correr

#Debes reiniciar R para que los cambios se guarden, si los datos son correctos
#cuando vuelvas a utilizar usethis::edit_r_environ() veras tus datos

#Primer paso: obtener taxonkeys----

# Cada especie (taxon) en GBIF tiene un número asociado, y para poder descargar
#de manera masiva estos datos es necesario hacer busquedas con el número
#registrado en GBIF, es decir, el taxonkey

taxonkeys_liq <- liq %>% # nombre del objeto donde esta la lista de especies
  pull("nombre_cientifico") %>% # nombre de la columna
  name_backbone_checklist() %>% # funcion a utilizar
  filter(!matchType == "NONE") %>% # obtener nombres coincidentes
  pull(usageKey) # obtener los taxonKey
#Es importante que los taxonKey se guarden en formato vector

#Si deseamos guardar el nombre de las especies y sus taxonKey podriamos utilizar
#la funcion mutate para crear un nuevo objeto
liq_completo<- liq %>% 
  mutate(taxonKey = taxonkeys_liq)
#Sin embargo se genera un error, porque hay una especie que no tiene un taxonKey
#No en todos los casos se encuentra este error, si el numero de especies es el 
#mismo que en el dataframe, no habra errores, pero en este caso hay que utilizar
#la siguientes funciones. Considerar que como se utiliza un loop, mientras más
#grande sea la lista de datos, más va a demorar

# Función para obtener el taxonKey
get_taxon_key <- function(species_name) {
  taxon_data <- name_backbone(species_name)
  if (!is.null(taxon_data$usageKey)) {
    return(taxon_data$usageKey)
  } else {
    return(NA)
  }
}

# Crear listas para almacenar nombres científicos y taxonKeys
scientific_names <- character(length(liq$nombre_cientifico)) # Columna con los nombres
taxon_keys <- numeric(length(liq$nombre_cientifico)) # Crear vector con el tamaño de nuestra lista 

# Iterar a través de las especies para obtener nombres científicos y taxonKeys
for (i in 1:length(liq$nombre_cientifico)) {
  species_name <- liq$nombre_cientifico[i]
  taxon_key <- get_taxon_key(species_name)
  
  scientific_names[i] <- species_name
  taxon_keys[i] <- taxon_key
}


# Crear un dataframe con los nombres científicos y taxonKeys
especie_taxonkey <- data.frame(
  ScientificName = scientific_names,
  TaxonKey = taxon_keys
)

#Guardar lista y taxonkey
write.csv2(especie_taxonkey, "taxonKey_liquenes_clasificados.csv") #mismo archivo dentro de la carpeta

#Esta función nos puede ser útil para conocer que especies no estarán dentro de
#nuestro análisis, si quieres ver cuales son, aquí esta el código

especies_faltantes <- especie_taxonkey %>%
  filter(rowSums(is.na(.)) > 0)

#Segundo paso: descarga de ocurrencias----


#Ahora que ya tenemos nuestro vector con los taxonKeys y conocemos cuales especies
#no estarán dentro de nuestra búsqueda comenzaremos a descargar las ocurrencias

#La función occ_download permite descargar más de 100.000 ocurrencias, pero 
#necesita de las credenciales, ya que la descarga se guarda en tu perfil de GBIF
#así se puede mantener un registro de descargas y las citas de los archivos que
#se generan

descarga_liquenes<- occ_download(
  pred_in("taxonKey", taxonkeys_liq), # selecionar los taxonkeys
  pred("hasCoordinate", TRUE), # puntos que tengan coordenadas
  pred("country","CL"), # solo en chile
  pred("hasGeospatialIssue", FALSE), # sin errores geoespaciales
  format = "SIMPLE_CSV") #formato simple

occ_download_wait(descarga_liquenes) # tiempo de descarga

occurrencia_liquenes<- occ_download_get(descarga_liquenes) %>%
  occ_download_import() #visualizar e importar los datos en un objeto

#Tercer Paso: conteo de registros por punto (cargar archivos descargados)----

conteo_liq <- oc_liq%>%
  group_by(taxonKey, decimalLatitude, decimalLongitude) %>% #Agrupar datos por punto
  summarise(conteo = n()) # Contar cuantos registros hay por cada punto
# Puedes ver que el numero de observaciones es menor que
# el archivo de ocurrencias

conteo_liq$nombres<- keysNames$ScientificName[match(conteo_liq$taxonKey, keysNames$TaxonKey)]
# agregar los nombres científicos iniciales a la tabla con el conteo

limpieza_conteo <- na.omit(conteo_liq) #Eliminar cualquier dato que sea NA

limpieza_conteo$categoria<- liq$categoria[match(limpieza_conteo$nombres, liq$nombre_cientifico)]
# agregar categorías de conservación

#Cuarto paso: conteo ocurrencias segun categoria de amenza o por especie----

conteo_amenza <- limpieza_conteo %>%
  group_by(categoria) %>%
  summarise(conteo = n ())

#podemos hacer un gráfico de barras para visualizar rapidamente los datos

grafico_amenzas <- ggplot(conteo_amenza,aes(x= categoria, y = conteo, fill = categoria))+
  geom_bar( stat='identity') +
  theme(axis.text.x=element_blank())
print(grafico_amenzas)

#podemos copiar el mismo código pero agrupar por especie para saber que especie tiene mas registros
conteo_especies <- limpieza_conteo %>%
  group_by(nombres) %>%
  summarise(conteo = n ())
#Quinto paso: mapa de ocurrencias----

mapa_especies <- ggplot(limpieza_conteo, 
                        aes(x = decimalLongitude, y = decimalLatitude, 
                            color = categoria)) + # los datos y respecto a que queremos el color
  borders(database = "world", regions = ("Chile"), size = 0.3) +  # el mapa de Chi
  # Change the colour and transparency of the plotted occurrence points 
  geom_point(alpha = 0.4)+ # agregamos puntos respecto a los datos y el alpha es transparencia
  labs(col= "Categorías de Riesgo") #  titulo de la leyenda


print(mapa_especies)


#Podemos hacer más personalizado el mapa y darle

#Primero debemos darle una escala a dependiendo el tipo clasificación
escala_categoria<- as.data.frame(unique(liq$categoria)) #  obtener las categorias
escala_categoria$escala_riesgo <- c(1, 2, 3, 5, 4, 6, 5, 5,1 ) # darle un valor numerico
nombre_colcategoria <- c("categoria", "escala_riesgo") # nombrar adecuadamente las cateforias
colnames(escala_categoria) <- nombre_colcategoria

especie_escala_categoria <- limpieza_conteo # hacer una copia de nuestra tabla para trabajar tranquilos
especie_escala_categoria$escala_riesgo <- escala_categoria$escala_riesgo[match( especie_escala_categoria$categoria,
                                                                                escala_categoria$categoria)]
# Le asignamos el valor respecto a la escala númeria que creamos        

map_colores_riesgo <- ggplot(especie_escala_categoria, 
                             aes(x = decimalLongitude, y = decimalLatitude, 
                                 color= as.factor(escala_riesgo))) + # agregar la columna donde esta la escala como factor
  borders(database = "world", regions = ("Chile"), size = 0.3) +  
  geom_point(aes(size= as.factor(escala_riesgo)))+ # Dar tamaño segun la categoria de amenaza
  scale_color_manual(values = c("pink", "darkgreen","chartreuse3", 
                                "yellow2",
                                "darkorange2","red"),
                     labels = c("Datos insuficientes", "Preocupación Menor",
                                "Casi Amenzada", "Vulnerable",
                                "En Peligro", "En Peligro Crítico")) + # dar colores específicos
  labs(col= "Categorías de Riesgo") +
  scale_size_manual( values = c(0.9, 1.3, 1.7, 2.1, 2.5, 2.9, 3.5)) + # dar tamaños especificos
  guides(size= "none") #no mostrar legenda de tamaño

print(map_colores_riesgo)

  

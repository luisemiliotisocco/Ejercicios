# En el siguiente script trabajaremos sobre la ciudad de Zárate

# Abrimos los paquetes que vamos a usar

library(tidyverse)
library(sf)
library(janitor)
library(readxl)

# Ahora, cargamos nuestros datasets

radios_censales <- st_read("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/Zarate/Shapes/Rad_882.shp")
ggplot() + geom_sf(data = radios_censales)
as_tibble(radios_censales)

#Nos vamos a quedar solo con los radios censales que correspongan a la ciudad de Zárate. De acuerdo con el INDEC, el código de la localidad es "040".

radios_censales <- filter(radios_censales, COD_LOC=="040")
ggplot() + geom_sf(data = radios_censales)

# Continuamos cargando el resto de los datasets

calles <- st_read("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/Zarate/Shapes/Ejes_882.shp")
ggplot() + geom_sf(data = calles)

hitos <- st_read("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/Zarate/Shapes/Hitos_882.shp")
ggplot() + geom_sf(data = hitos)

# Nos quedamos solo con las figuras de la Ciudad de Zárate

radios_censales <- st_transform(radios_censales, crs = 4326)

calles <- st_transform(calles, crs=st_crs(radios_censales))
calles <- st_intersection(calles, radios_censales)
hitos <- st_transform(hitos, crs= st_crs(radios_censales))
hitos <- st_intersection(hitos, radios_censales) 

ggplot() + 
  geom_sf(data = radios_censales) + 
  geom_sf(data = calles, color="red")+
  geom_sf(data = hitos, color="blue")

info_socioeconomica <- read_xlsx("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/Zarate/info sociodemográfica.xlsx")
as_tibble(info_socioeconomica)
info_socioeconomica <- mutate(info_socioeconomica, Radio_Censal = paste(0,Radio_Censal, sep = ""))

datos_demograficos <- st_read("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/PBA/Buenos_Aires_con_datos.shp")
ggplot()+
  geom_sf(data = datos_demograficos)
as_tibble(datos_demograficos)

datos_demograficos <- left_join(datos_demograficos, info_socioeconomica, by=c("link"= "Radio_Censal"))

datos_demograficos <- st_transform(datos_demograficos, crs = st_crs(radios_censales))
datos_demograficos <- st_intersection(datos_demograficos, radios_censales)
as_tibble(datos_demograficos)

datos_demograficos <- datos_demograficos %>%
  group_by(CLAVERA) %>%
  summarise(varon=sum(varon),
            mujer=sum(mujer),
            totalpobl=sum(totalpobl),
            hogares=sum(hogares),
            viviendasp=sum(viviendasp),
            viv_part_h=sum(viv_part_h),
            H_sin_agua_de_Red=mean(H_sin_agua_de_Red),
            H_NBI=mean(H_NBI),
            H_con_gas_de_red=mean(H_con_gas_de_red),
            V_casillas=mean(V_casillas),
            V_calidad_constructiva_insf=mean(V_calidad_constructiva_insf),
            V_conexiones_servicios_basicos_insf=mean(V_conexiones_servicios_basicos_insf),
            P_Tasa_alfabetizacion=mean(P_Tasa_alfabetizacion)) %>% 
  st_set_geometry(NULL) 
  
radios_censales <- left_join(radios_censales, datos_demograficos, by="CLAVERA")
as_tibble(radios_censales)


ggplot()+ geom_sf(data = radios_censales, aes(fill=totalpobl)) + scale_fill_viridis_c()
ggplot()+ geom_sf(data = radios_censales, aes(fill=totalpobl)) + scale_fill_viridis_c()
ggplot()+ geom_sf(data = radios_censales, aes(fill=radios_censales$H_NBI)) + scale_fill_viridis_c()



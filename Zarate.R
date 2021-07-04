# En el siguiente script trabajaremos sobre la ciudad de Zárate.

# 1. Abrimos las libererías que vamos a usar a lo largo del proyecto.
library(tidyverse)
library(sf)
library(janitor)
library(readxl)

# 2. Cargamos nuestros datasets
radios_censales <- st_read("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/Zarate/Shapes/Rad_882.shp")
ggplot() + geom_sf(data = radios_censales)
as_tibble(radios_censales)

#Nos vamos a quedar solo con los radios censales que correspongan a la ciudad de Zárate. De acuerdo con el INDEC, el código de la localidad es "040".
radios_censales <- filter(radios_censales, COD_LOC=="040")
ggplot() +
  geom_sf(data = radios_censales)

# Continuamos subiendo el resto de los datasets.
calles <- st_read("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/Zarate/Shapes/Ejes_882.shp")
ggplot() +
  geom_sf(data = calles)

hitos <- st_read("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/Zarate/Shapes/Hitos_882.shp")
ggplot() +
  geom_sf(data = hitos)

# 3. Nos quedamos solo con las figuras pertenecientes a la ciudad de Zárate
radios_censales <- st_transform(radios_censales, crs = 4326)
calles <- st_transform(calles, crs=st_crs(radios_censales))
calles <- st_intersection(calles, radios_censales)
hitos <- st_transform(hitos, crs= st_crs(radios_censales))
hitos <- st_intersection(hitos, radios_censales) 

ggplot() + 
  geom_sf(data = radios_censales, fill=NA) + 
  geom_sf(data = calles, color="red")+
  geom_sf(data = hitos, color="blue")+
  theme_void()

# 4. Cargamos información sociodemográfica.
info_socioeconomica <- read_xlsx("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/Zarate/info sociodemográfica.xlsx")
as_tibble(info_socioeconomica)

datos_demograficos <- st_read("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/PBA/Buenos_Aires_con_datos.shp")
ggplot()+
  geom_sf(data = datos_demograficos)
as_tibble(datos_demograficos)

# 5. Unimos los datasets sociodemográficos con el de radios censales.
info_socioeconomica <- mutate(info_socioeconomica, Radio_Censal = paste(0,Radio_Censal, sep = ""))
datos_demograficos <- left_join(datos_demograficos, info_socioeconomica, by=c("link"= "Radio_Censal"))
datos_demograficos <- st_transform(datos_demograficos, crs = st_crs(radios_censales))
datos_demograficos <- st_intersection(datos_demograficos, radios_censales)
datos_demograficos <- filter(datos_demograficos, !(is.na(H_NBI)))
as_tibble(datos_demograficos)
datos_demograficos <- datos_demograficos %>%
  group_by(CLAVERA) %>%
  summarise(varon=sum(varon),
            mujer=sum(mujer),
            totalpobl=sum(totalpobl),
            hogares=sum(hogares),
            viviendasp=sum(viviendasp),
            viv_part_h=sum(viv_part_h),
            H_sin_agua_de_Red=round(mean(H_sin_agua_de_Red),2),
            H_NBI=round(mean(H_NBI),2),
            H_con_gas_de_red=round(mean(H_con_gas_de_red),2),
            V_casillas=round(mean(V_casillas),2),
            V_calidad_constructiva_insf=round(mean(V_calidad_constructiva_insf),2),
            V_conexiones_servicios_basicos_insf=round(mean(V_conexiones_servicios_basicos_insf),2),
            P_Tasa_alfabetizacion=round(mean(P_Tasa_alfabetizacion),2)) %>% 
  st_set_geometry(NULL)
radios_censales <- left_join(radios_censales, datos_demograficos, by="CLAVERA")
as_tibble(radios_censales)

# 6. Graficamos.
ggplot()+
  geom_sf(data = radios_censales, aes(fill=totalpobl), alpha=0.7)+
  scale_fill_viridis_c()+
  labs(title="Población por radio censal",
       subtitle = "Zárate",
       fill="Habitantes",
       caption="Fuente: INDEC")+
  theme_void()

ggplot()+
  geom_sf(data = radios_censales, aes(fill=hogares), alpha=0.7) +
  scale_fill_viridis_c(breaks=c(0,1000, 2000, 3000, 4000, 5000),
                       limits=c(0,5000))+
  labs(title="Hogares por radio censal",
       subtitle = "Zárate",
       fill="Hogares",
       caption="Fuente: INDEC")+
  theme_void()

ggplot()+
  geom_sf(data = radios_censales, aes(fill=H_NBI), alpha=0.7) +
  scale_fill_viridis_c(breaks=c(0,10, 20, 30, 40),
                       limits=c(0,40))+
  labs(title="Hogares con al menos una necesidad básica insatisfecha",
       subtitle = "Zárate",
       fill="Hogares(%)",
       caption="Fuente: INDEC")+
  theme_void()

ggplot()+
  geom_sf(data = radios_censales, aes(fill=H_con_gas_de_red), alpha=0.7) +
  scale_fill_viridis_c(breaks=c(0,25,50,75,100),
                       limits=c(0,100))+
  labs(title="Hogares con conexión a la red de gas",
       subtitle = "Zárate",
       fill="Hogares(%)",
       caption="Fuente: INDEC")+
  theme_void()

ggplot()+
  geom_sf(data = radios_censales, aes(fill=H_sin_agua_de_Red), alpha=0.7) +
  scale_fill_viridis_c(breaks=c(0,25,50,75,100),
    limits=c(0,100))+
  labs(title="Hogares sin agua de red",
       subtitle = "Zárate",
       fill="Hogares(%)",
       caption="Fuente: INDEC")+
  theme_void()

ggplot()+
  geom_sf(data = radios_censales, aes(fill=V_conexiones_servicios_basicos_insf), alpha=0.7) +
  scale_fill_viridis_c(breaks=c(0,20,40,60),
                       limits=c(0,60))+
  labs(title="Viviendas con conexión insufieciente a servicios básicos",
       subtitle = "Zárate",
       fill="Viviendas(%)",
       caption="Fuente: INDEC")+
  theme_void()

ggplot()+
  geom_sf(data = radios_censales, aes(fill=V_calidad_constructiva_insf), alpha=0.7) +
  scale_fill_viridis_c(breaks=c(0,15,30,45),
                       limits=c(0,45))+
  labs(title="Viviendas con calidad constructiva insufieciente",
       subtitle = "Zárate",
       fill="Viviendas(%)",
       caption="Fuente: INDEC")+
  theme_void()

ggplot()+
  geom_sf(data = radios_censales, aes(fill=V_casillas), alpha=0.7) +
  scale_fill_viridis_c(breaks=c(0,10,20,30),
                       limits=c(0,30))+
  labs(title="Casillas",
       subtitle = "Sobre el total de viviendas",
       fill="Casillas (%)",
       caption="Fuente: INDEC")+
  theme_void()

ggplot()+
  geom_sf(data = radios_censales, aes(fill=P_Tasa_alfabetizacion), alpha=0.7) +
  scale_fill_viridis_c(direction = -1,
                       breaks=c(0,1,2,3,4),
                       limits=c(0,4))+
  labs(title="Analfabetismo",
       subtitle = "Zárate",
       fill="Tasa de Analfabetismo (%)",
       caption="Fuente: INDEC")+
  theme_void()

IVS <- st_read("C:/Users/Santi/Desktop/Santi/Ciencia de Datos/Instrumento de Analisis Urbano II/R/Ejercicios/Datasets/IVS/IVS.shp")
IVS <- st_transform(IVS3, crs = st_crs(radios_censales))
IVS <- st_intersection(IVS3,radios_censales)

ggplot()+
  geom_sf(data = IVS, aes(fill=VULNERABIL), alpha=0.7) +
  scale_fill_viridis_c(breaks=c(0.25,0.5,0.75,1),
                       limits=c(0,1))+
  labs(title="Indíce de Vulnerabilidad Social",
       subtitle = "Zárate",
       fill="IVS",
       caption="Fuente: Fundación Bunge & Born")+
  theme_void()

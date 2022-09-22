
library(sf)
library(raster)
library(ggspatial)
library(ggplot2)
SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
Per           <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
MDD           <- subset(Per, NAME_1  == "Madre de Dios")
Tambopata <- subset(Per, NAME_3 == "Las Piedras")

# Zona de Interes
Zona = st_read("shp/Manutata.geojson")
Zona_py <- st_transform(Zona ,
                        crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Viver = st_read("shp/Vivero.geojson")
Viver_py <- st_transform(Viver ,
                        crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru , fill="gray", color="black")+
  geom_sf(data = MDD, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")


MDD_GG=ggplot()+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.01)+
  geom_sf(data = Peru , fill=NA, color="black", size=0.01)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = MDD, fill=NA, color="black", size=0.5)+
  geom_sf(data = Tambopata, fill="gray", color="black", size=0.5)+
  geom_sf(data = Zona_py, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  coord_sf(xlim = c(-72.40404, -68.65311), ylim = c(-13.7 ,-9.879849)) +
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "b) Departamento de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -71, y = -13.4, hjust = 0, vjust = 1, 
           label = "Madre de Dios",size = 4, family="serif", color = 
             "black",  fontface="italic")


require(pacman)
pacman::p_load(tidyverse,sf,ggplot2, ggspatial,sp,osmdata,leaflet, ggmap )
available_tags("highway")

mad_map <- get_map(getbb("Madre de Dios"), maptype = "toner-background") # Localizamos madre de Dios
Via_Mal <- st_read ("SHP/Red_vial.geojson") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
Rio_Pol <- st_read ("SHP/Rio_Poli.geojson") # Caragmos un shp de puerto maldonado
Rio_Poli  <- st_transform(Rio_Pol ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
Puer_Mal <- st_read ("SHP/Area_puerto.shp") # Caragmos un shp de puerto maldonado
Puer_Mal <- st_transform(Puer_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

CP <- st_read ("SHP/CP.geojson") # Caragmos un shp de puerto maldonado
CetroPo  <- st_transform(CP ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion
CetroPo_xy <- cbind(CetroPo, st_coordinates(st_centroid(CetroPo$geometry)))
# Extrayendo información de OSM
Puer_Maldonado <- opq(Puer_Mal) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf() # Informacion de osm del area

Puer_Maldonado_rios <- opq(Puer_Mal)%>% add_osm_feature(key = "waterway", value = "river") %>%osmdata_sf()
Puer_Maldonado_secu <- opq(Puer_Mal)%>% add_osm_feature(key = "highway",
                                                        value = c("residential", "living_street",
                                                                  "unclassified",
                                                                  "service", "footway")) %>% osmdata_sf()
calles <- Puer_Maldonado$osm_lines
calles <- st_intersection(calles, Puer_Mal )
calles <- calles %>%
  mutate(maxspeed = as.numeric(maxspeed),
         lanes = ifelse(is.na(lanes), 1, as.numeric(lanes)))

ZonaGG= ggplot() +
  geom_sf(data = calles,inherit.aes = FALSE,aes(color = maxspeed), size = 1, alpha = .4) +
  scale_color_viridis_c() +
  geom_sf(data = filter(calles, str_detect(name, "Avenida")), color = "salmon") +
  geom_sf(data = Puer_Maldonado_secu$osm_lines,inherit.aes = FALSE,
          color = "gray", size = .5,alpha = .6) +
  geom_sf(data = Puer_Maldonado_rios$osm_lines,
          inherit.aes = FALSE, color = "blue",size = .5,alpha = .5) +
  #geom_point(data =Foco , aes(x = longitude, y = latitude),size=1.5, color="red", pch=21, fill="red")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.4)+
  geom_sf(data = Rio_Poli , color="#a2d2ff", size=0.3, fill="#a2d2ff")+
  geom_sf(data = Zona_py, fill="black", color="black")+
  geom_sf(data = Viver_py, fill="black", color="black")+
  
  coord_sf(xlim = c(-69.23, -69.07), ylim = c(-12.61 ,-12.40)) +
  labs(color = '',  x = 'Longitud', y = 'Latitud')+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -69.21, y = -12.41, hjust = 0, vjust = 1, 
           label = "C) Zona de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")
library(elevatr)
elev = get_elev_raster(Puer_Mal, z=12)

plot(elev)
Poligo_alt    <- crop(elev, Puer_Mal)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Puer_Mal)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")

library(ggnewscale) 
library(ggspatial)

D=ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0,80,100,120,180, 200, 250),
                       na.value = 'white',
                       labels = c("[0 - 79] ","[80 - 99]", "[100 - 119]",
                                  "[120 - 179]", "[180 - 199]", "[200 - 249]",
                                  "[250 - 300]"),
                       name='Elevacion \n(msnm)')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = Rio_Poli , color="blue", size=0.3, fill="#a2d2ff")+
  geom_sf(data =  CetroPo,size=1.5, color="black", pch=21, fill="black")+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.8)+
  geom_sf_label(data = CetroPo_xy , aes(label = NOMBCP ), 
                family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
                size = 2, face = "bold",color = 'black',
                point.padding = unit(0.9, "lines"))+
  geom_sf(data = Zona_py, fill=NA, color="black")+
  geom_sf(data = Viver_py, fill=NA, color="black")+
  
  coord_sf(xlim = c(-69.20, -69.085), ylim = c(-12.605 ,-12.43)) +
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_classic()+
  theme(legend.position = c(0.2, 0.3),
        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=11, family="serif"),
        legend.title = element_text(size=11, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Longitud', y = '')+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))

legend <- get_legend(D)

DD= D + theme(legend.position = "none")
DD

Alt_Vive= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0,80,100,120,180, 200, 250),
                       na.value = 'white',
                       labels = c("[0 - 79] ","[80 - 99]", "[100 - 119]",
                                  "[120 - 179]", "[180 - 199]", "[200 - 249]",
                                  "[250 - 300]"),
                       name='Elevacion \n(msnm)')+
  geom_sf(data = Viver_py, fill=NA, color="black")+
  
  coord_sf(xlim = c(-69.14034,-69.11236), ylim = c(-12.47958 ,-12.46336)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

Alt_Manutata= ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0,80,100,120,180, 200, 250),
                       na.value = 'white',
                       labels = c("[0 - 79] ","[80 - 99]", "[100 - 119]",
                                  "[120 - 179]", "[180 - 199]", "[200 - 249]",
                                  "[250 - 300]"),
                       name='Elevacion \n(msnm)')+
  geom_sf(data = Zona_py, fill=NA, color="black")+
  
  coord_sf(xlim = c(-69.16802,-69.14937), ylim = c(-12.51762 ,-12.50666)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))

recta1= cbind(c(-69.16802,-69.125 ), c(-12.475 ,-12.465))%>%
  st_linestring() %>%
  st_sfc(crs= projection(Poligo_alt))%>%
  st_sf()

recta= cbind(c(-69.140,-69.14937 ), c(-12.51762 ,-12.50666))%>%
  st_linestring() %>%
  st_sfc(crs= projection(Poligo_alt))%>%
  st_sf()

cortar= st_bbox(recta1)
fl_c= crop(Poligo_alt, c(cortar[1]-100, cortar[3]+100, cortar[2]-100, cortar[4]+100 ))
recta_r = st_transform(recta1, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
fl_c_r = projectRaster(fl_c, crs = "+proj=longlat +datum=WGS84 +no_defs")

library(rlang)
library(geosphere)
library(raster)
library(purr)
altura= raster::extract(fl_c_r, recta_r, along=T, cellnumbers=T)
alturas_df = purrr::map_df (altura, as.data.frame, id="ID")
coor_rec= xyFromCell(fl_c_r, alturas_df$cell)
par_dist = geosphere::distGeo((coor_rec))[-nrow(coor_rec)]
alturas_df$dist =c(0, cumsum(par_dist))
names(alturas_df)[3] = "altura"
colnames(alturas_df) <- c("cell","dist", "altura")

Manuta_reli= alturas_df
vive_reli= alturas_df
library(ggplot2)

Manuta_Reli_gg =ggplot()+
  geom_area(data= Manuta_reli, aes(x=altura, y =dist), size=1,color="blue",
            fill="lightblue", alpha=0.8)+
  coord_fixed(ratio = 4/1, ylim = c(75, 600), expand=F)+
  theme_classic()+
  theme(legend.position = c(0.2, 0.3),
        axis.text.x  = element_text(face="bold", color="black", size=7,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=7),
        axis.title = element_text(face="bold", color="black",size=7),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=7, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Distancia', y = 'Altitud (msnm)')+
  annotate(geom = "text", x = 300, y = 300, hjust = 0, vjust = 1, 
           label = "D) Perfil del elevacion Fundo el Bosque",size = 2, family="serif", color = 
             "black",  fontface="italic")
Manuta_Reli_gg

vive_reli_gg =ggplot()+
  geom_area(data= vive_reli, aes(x=altura, y =dist), size=1,color="blue",
            fill="lightblue", alpha=0.8)+
  ylim(0,300) +
  theme_classic()+
  theme(legend.position = c(0.2, 0.3),
        axis.text.x  = element_text(face="bold", color="black", size=7,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=7),
        axis.title = element_text(face="bold", color="black",size=7),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=7, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  labs(title = '', fill = 'Densidad \n(miles)',  x = 'Distancia', y = 'Altitud (msnm)')+
  annotate(geom = "text", x = 200, y = 100, hjust = 0, vjust = 1, 
           label = "E) Perfil del elevacion Fundo el Bosque",size = 2, family="serif", color = 
             "black",  fontface="italic")

vive_reli_gg

library(cowplot)
W =ggdraw() +
  coord_equal(xlim = c(0, 17), ylim = c(0, 25), expand = FALSE) +
  draw_plot(DD  , width = 19, height = 19,x = -4, y = 0)+
  
  draw_plot(ZonaGG, width = 7,       height = 7, x = 10.5, y = 18)+
  draw_plot(SurA, width = 7,       height = 7, x = -1.2, y = 18)+
  draw_plot(MDD_GG, width = 7.5,   height = 7, x = 4.2, y = 18)+
  
  draw_plot(vive_reli_gg , width = 4,       height = 5, x = 11.9, y = 0)+
  draw_plot(Manuta_Reli_gg , width = 4,       height = 5, x = 11.8, y = 3.4)+
  
  draw_plot(legend, width = 5,       height = 5, x = 13, y = 7.5)+
  
  draw_plot(Alt_Manutata, width = 5,       height = 5, x = 11.8, y = 9.2)+
  draw_plot(Alt_Vive, width = 5,       height = 5, x = 11.8, y = 13.2)+

  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = 12, y = 18, hjust = 0, vjust = 1,
           label = "Fundo el Bosque",face="bold",
           size = 5,family="serif", color = "black", face = "italic")+
  annotate(geom = "text", x = 12, y = 14.1, hjust = 0, vjust = 1,
           label = "Cachuela margen Izq.",face="bold",
           size = 4,family="serif", color = "black", face = "italic")

ggsave(plot = W ,"Mapas/01_Mapa de Ubicacion.png", units = "cm", width = 17,height = 25, dpi = 1200) 









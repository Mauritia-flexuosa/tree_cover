# Tree cover from Hansen for the Chapada dos Veadeiros National Park (PNCV as the acronym in Portuguese).
# Data dowloaded from <https://data.globalforestwatch.org/>

library(raster)
library(tidyverse)
library(patchwork)
library(RColorBrewer)

setwd("./Documentos/data/tree cover")

# Download data for the region of interest

# download.file("https://storage.googleapis.com/earthenginepartners-hansen/GFC-2019-v1.7/Hansen_GFC-2019-v1.7_treecover2000_10S_050W.tif")

======
# UCs do Brasil:
#url = "https://www.icmbio.gov.br/portal/images/stories/servicos/geoprocessamento/DCOL/dados_vetoriais/UC_fed_julho_2019.zip"
#download.file(url = url, "/home/marcio/Documentos/data/shp chapada/UCs Fed")

UCs <- sf::st_read("/home/marcio/Documentos/data/shp chapada/UCs Fed/UC_fed_julho_2019/UC_fed_julho_2019.shp") # O PNCV é o 302

======

# Store data into a raster object
tc_hansen <- raster("./Hansen_GFC-2019-v1.7_treecover2000_10S_050W.tif") %>% 
	crop(y = extent(UCs[302,]))
	
# Extract georreferenced tree cover values
tc_hansen_dataframe <- tc_hansen %>% rasterToPoints %>% as.data.frame %>% rename("tree_cover" = "Hansen_GFC.2019.v1.7_treecover2000_10S_050W")

# Plot tree cover density at the PNCV
a <- tc_hansen_dataframe %>% 
	ggplot() +
	    aes(x = tree_cover)+
	geom_density()+
	ggtitle("Including tree less")+
	theme(axis.text=element_text(size=14),
            axis.title=element_text(size=15),
            title=element_text(size=15))

# Filter tree covers higher than 5%
b <- tc_hansen_dataframe %>% filter(tree_cover > 5) %>% 
	ggplot() +
	    aes(x = tree_cover)+
	geom_density()+
	ggtitle("Without tree less envrs (Tree covers > 5%)")+
	theme(axis.text=element_text(size=14),
            axis.title=element_text(size=15),
            title=element_text(size=15))

#pdf("tree_cover_hansen_pncv", family = "sans", width = 10)
a|b
#dev.off()

# Add coordinates of the sample plots at the PNCV

x <- c(-47.75014, -47.74389, -47.76830, -47.72287, -47.73264, -47.73613, -47.68317,
       -47.66713, -47.69987, -47.70263, -47.76707, -47.71484, -47.71418, -47.67871,
       -47.67911, -47.68465, -47.63446, -47.63412, -47.63415, -47.69065, -46.97297,
       -46.98341, -46.98311, -46.98177, -47.84833, -47.84950, -47.84347, -46.97983,
       -46.98513, -46.98280, -47.63719, -47.63690, -47.63657, -47.63625, -47.63551,
       -47.63513, -47.63479, -47.63447, -47.63412, -47.63376, -47.63497, -47.63472,
       -47.63449, -47.63425, -47.63398, -47.63376, -47.63349, -47.63340, -47.63323,
       -47.63333)
y <- c(-14.14786, -14.14263, -14.13991, -14.13116, -14.12362, -14.12612, -14.12855,
       -14.12713, -14.12875, -14.12913, -14.13310, -14.13943, -14.13980, -14.11648,
       -14.11652, -14.11530, -14.09114, -14.09050, -14.08997, -14.12734, -13.92030,
       -13.88963, -13.88625, -13.88888, -14.20525, -14.20966, -14.20466, -13.89772,
       -13.88366, -13.88300, -14.10703, -14.10691, -14.10672, -14.10647, -14.10615,
       -14.10608, -14.10588, -14.10573, -14.10555, -14.10556, -14.09398, -14.09369,
       -14.09341, -14.09310, -14.09275, -14.09240, -14.09212, -14.09177, -14.09145,
       -14.09123)

xy <- cbind(x,y)

coordR <- SpatialPoints(xy, 
                        proj4string = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Extract tree covers from our sample plots
tc_hansen_our_sample <- raster::extract(tc_hansen, coordR) %>%
				as.data.frame %>%
				add_column(Vegetation_type = c(rep("savanna", 10),
					rep("gallery_for", 10),
					rep("dry_for",10),
					rep("grassland", 20)))


# Boxplot dos tree covers from sample plots by vegetation type

c <- tc_hansen_our_sample[1:30,] %>% ggplot(aes(y = ., x = factor(Vegetation_type)))+
				geom_boxplot(aes(fill = factor(Vegetation_type)), show.legend = FALSE)+
				ylab("Tree cover (%)")+
				xlab("Vegetation type")+
				ggtitle("Tree cover (Hansen) from our samples")+ 
			        scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
				theme(axis.text=element_text(size=13),
				    axis.title=element_text(size=14),
				    title=element_text(size=14))


`Vegetation type` <- c(rep("savanna", 10), rep("gallery forest", 10), rep("dry forest", 10)) %>% factor
parcelas <- ""
lat <- y[1:30]
lon <- x[1:30]

dat <- data.frame(parcelas, lat, lon, `Vegetation type`)


# Mapping tree cover at the entire PNCV
d <- ggplot(UCs[302,]) +
  geom_sf(fill = "transparent") +
  coord_sf()+
  geom_raster(data = tc_hansen_dataframe, aes(x = x, y = y, fill = tree_cover))+
  geom_sf(data = UCs[302,], fill = "transparent", alpha = 0.2)+
  coord_sf()+
  scale_fill_viridis_c(name = "mm/year", direction = -1)+
  geom_text(aes(x = -47.43 , y = -13.3), label = "Chapada dos Veadeiros National Park", show.legend = F, color = "white", size = 4) +
  geom_point(data = dat, aes(lon, lat, color = `Vegetation type`))+
  scale_color_manual(values=c("purple", "darkgrey", "orange2"))+
  theme_minimal()+
  xlab("lon")+
  ylab("lat")+
 scale_fill_gradient(low="blue", high="red",name = "tree cover(%)")+
				theme(axis.text=element_text(size=14),
				    axis.title=element_text(size=15),
				    title=element_text(size=15))
 
# Plot tree covers measured *in situ* by a concave densiometer versus tree covers from Hansen for the same plots. 
tc_field <- read.table("/home/marcio/treecover2021.txt", h = T)

e <- add_column(tc_hansen_our_sample[1:30,], tc_field) %>%
	ggplot()+
	  aes(x = ., y = wet)+
	geom_point(aes(color = factor(Vegetation_type)),show.legend = FALSE)+
	ylab("field measurement of tree cover (%)")+
	xlab("Tree cover from Hansen")+
	xlim(c(0,75))+
	ylim(c(0,100))+
	geom_smooth()+
	geom_text(aes(x = 7 , y = 70), label = "Forest", show.legend = F, color = "grey2", size = 4) +
	geom_text(aes(x = 7 , y = 50), label = "Savanna", show.legend = F, color = "grey2", size = 4) +
	geom_hline(aes(color = "red", "dashed"), yintercept = 60,
      	  na.rm = TRUE, show.legend = FALSE)+
	#geom_abline(slope = 1.491, intercept = -2.724)+
        scale_color_manual(values=c("purple", "darkgrey", "orange2"))+
	theme(axis.text=element_text(size=13),
	  axis.title=element_text(size=14))

#pdf("tree cover hansen nossas parcelas.pdf", family = "sans", width = 11)
#																																																																																																																																																																																										(a|b)/(c|e)
#dev.off()

#pdf("tree cover hansen nossas parcelas mais mapa.pdf", family = "sans", width = 12)
#(a|b)/(c|e)/d
#dev.off()

#pdf("pncv tree cover hansen mapa.pdf", family = "sans", width = 12)
d
#dev.off()

# Tentar isso na próxima vez!!
ggdraw() +
  draw_plot(d) +
  draw_plot(c, x = 47.88, y = 13.3, width = 0.27, height = 0.34)
  
  

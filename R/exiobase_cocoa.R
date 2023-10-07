# cocoa flows exiobase --------------------------

library(Matrix)
library(tidyverse)
library(data.table)
library(rworldmap)
library(sf)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(viridis)

# define functions --------------------------
is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

# load definitions --------------------------
load(file="/mnt/nfs_fineprint/tmp/exiobase/Y.codes.RData")
load(file="/mnt/nfs_fineprint/tmp/exiobase/Q.codes.RData")
load(file="/mnt/nfs_fineprint/tmp/exiobase/pxp/IO.codes.RData")
items <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/items.csv"))
regions <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/regions.csv"))

# make settings --------------------------
year <- 2016
country <- "PER"
code <- regions$area_code[regions$iso3c == country]
product <- "Cocoa Beans and products"

# read data --------------------------
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_L.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_x.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_E.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_F_hh.RData"))
E_fabio <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/E.rds"))
E_fabio <- E_fabio[[as.character(year)]]

# harmonize environmental data -------------------------------------------
conc_reg <- fread("inst/reg_fabio_exio.csv")
conc_items <- fread("inst/items_fabio_exio.csv")
E_fabio <- merge(E_fabio, conc_reg[, .(area_code, Country.Code = exiobase_code)], 
  by = "area_code", all.x = TRUE)
E_fabio <- merge(E_fabio, conc_items[, .(comm_code, Product.Code = exiobase_code)], 
                 by = "comm_code", all.x = TRUE)
extension <- E_fabio
extension[, 8:13] <- 0
extension[area_code == code & item == product, 8:13] <- E_fabio[area_code == code & item == product, 8:13]
extension <- extension[, .(landuse = sum(landuse, na.rm = TRUE), 
                           biomass = sum(biomass, na.rm = TRUE), 
                           green = sum(green, na.rm = TRUE), 
                           blue = sum(blue, na.rm = TRUE),
                           p_application = sum(p_application, na.rm = TRUE), 
                           n_application = sum(n_application, na.rm = TRUE)), 
                       by = c("Country.Code", "Product.Code")]
extension <- extension[Country.Code != "" & Product.Code != ""]
extension <- merge(IO.codes, extension[,.(Country.Code, Product.Code, landuse, biomass,
    green, blue, p_application, n_application)],
  by = c("Country.Code", "Product.Code"), all.x = TRUE)
extension[is.na(extension)] <- 0
extension <- as.data.table(extension[order(extension$Index), ])
extensions <- extension[, .(biomass, landuse, blue, green, p_application, n_application)]
ext_name = names(extensions)[1]


# ---------------- calculate footprints --------------------------

# prepare multipliers
ext <- extensions[, get(ext_name)] / as.vector(x)
ext[!is.finite(ext)] <- 0
MP <- ext * L

# prepare final demand
y <- Y
colnames(y) <- Y.codes$`Region Name`
y <- agg(y)

# derive footprints
FP <- MP %*% y
results <- FP %>%
  as.matrix() %>% 
  as_tibble() %>% 
  mutate(origin = paste0(IO.codes$Country.Code, "_", IO.codes$Product.Name)) %>% 
  gather(index, value, -origin) %>%
  mutate(country_origin = substr(origin,1,2)) %>% 
  mutate(item_origin = substr(origin,4,1000)) %>% 
  mutate(country_target = substr(index,1,2)) %>% 
  mutate(final_product = substr(index,4,1000)) %>% 
  select(-index, -origin) %>% 
  filter(value != 0)

results$iso_target <- conc_reg$iso3c[match(results$country_target, conc_reg$exiobase_code)]
results$iso_target[results$country_target %in% c("WA","WE","WF","WL","WM")] <- 
  results$country_target[results$country_target %in% c("WA","WE","WF","WL","WM")]
results$iso_target[results$country_target == "HR"] <- "HRV"
results$iso_target[results$country_target == "TW"] <- "TWN"
results$group_target <- IO.codes$Sector.Group[match(results$final_product, IO.codes$Product.Name)]
results$area_target <- conc_reg$exiobase_area[match(results$country_target, conc_reg$exiobase_code)]



# ---------------- Create Visualizations --------------------------
# load world map shape file
world_map <- getMap(resolution = "low") %>%
  st_as_sf() %>%
  filter(ADMIN != "Antarctica") %>%
  dplyr::select(ADMIN, ADM0_A3, ISO_A3, REGION, continent)

# create cocoa consumption map
indicator = "Cocoa beans"
unit = "tonnes"
title = ""
world_fp <- left_join(world_map, results[results$iso_target!="PER",], by = c("ISO_A3" = "iso_target"))

ggmap <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["value"]]), size = 0.05) +
  labs(fill=paste(indicator, " <br> in", unit), title = title) +
  scale_fill_viridis_c(direction = -1, na.value = "lightgrey") +
  coord_sf(crs = "+proj=robin") + # "+proj=moll"   "+proj=wintri"
  ggthemes::theme_map() +
  theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot", 
        legend.title = ggtext::element_markdown(size = 8)) #legend.position = "right"

if(title == "") ggmap <- ggmap + theme(plot.title = element_blank())

ggsave(paste0("output/cocoa_map.png"), ggmap, width = 15, height = 10, units = "cm")
fwrite(results, "output/cocoa_flows.csv")
fwrite(extension[biomass!=0], "output/cocoa_impacts_direct.csv")


# ---------------- calculate detailed footprints --------------------------

# prepare multipliers
ext <- extensions[, get(ext_name)] / as.vector(x)
ext[!is.finite(ext)] <- 0
MP <- ext * L

# prepare final demand
y <- Y
colnames(y) <- Y.codes$`Region Name`
y <- agg(y)

# derive footprints
FP <- y * colSums(MP)

results <- FP %>%
  as_tibble() %>% 
  mutate(product = paste0(IO.codes$Country.Code, "_", IO.codes$Product.Name)) %>% 
  gather(consumer_country, value, -product) %>%
  mutate(producer_country = substr(product,1,2)) %>% 
  mutate(product = substr(product,4,1000)) %>% 
  filter(value != 0)

results$iso_consumer <- conc_reg$iso3c[match(results$consumer_country, conc_reg$exiobase_code)]
results$iso_consumer[results$consumer_country %in% c("WA","WE","WF","WL","WM")] <- 
  results$consumer_country[results$consumer_country %in% c("WA","WE","WF","WL","WM")]
results$iso_producer <- conc_reg$iso3c[match(results$producer_country, conc_reg$exiobase_code)]
results$iso_producer[results$producer_country %in% c("WA","WE","WF","WL","WM")] <- 
  results$producer_country[results$producer_country %in% c("WA","WE","WF","WL","WM")]


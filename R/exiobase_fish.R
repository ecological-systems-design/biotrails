# fish flows exiobase --------------------------

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
conc_reg <- fread("inst/reg_fabio_exio.csv")

# make settings --------------------------
year <- 2016
# 19  Fish and other fishing products; services incidental of fishing p05
# 84  Greece  GRC EU  Greece  12  GR
country <- "GR"
product <- "Fish and other fishing products; services incidental of fishing"

# read data --------------------------
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_L.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Z.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_x.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_E.RData"))
load(file=paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_F_hh.RData"))
# convert GHG into GWP
E[Q.codes$GWP!=0, ] <- E[Q.codes$GWP!=0,] * Q.codes$GWP[Q.codes$GWP!=0]
Q.codes <- Q.codes %>% 
  tidyr::separate(Stressor, c("Stressor_short"), " - ", remove = FALSE) %>% 
  mutate(Unit = ifelse(GWP!=0, "kg", Unit)) %>% 
  mutate(Stressor_short = ifelse(GWP!=0, "CO2e", Stressor_short))


# ---------------- fish sector analysis --------------------------

data_z <- data.table(IO.codes, 
                     L = round(as.numeric(L[IO.codes$Product.Name==product & IO.codes$Country.Code==country,]),3),
                     Z = round(as.numeric(Z[IO.codes$Product.Name==product & IO.codes$Country.Code==country,])))
data_y <- data.table(Y.codes, Y = round(Y[IO.codes$Product.Name==product & IO.codes$Country.Code==country,]))
data_e <- data.table(Q.codes, E = round(E[,IO.codes$Product.Name==product & IO.codes$Country.Code==country]))


# ---------------- footprints of fish production --------------------------

ext <- t(E) / as.vector(x)
ext[!is.finite(ext)] <- 0
MP <- ext * as.numeric(L[,IO.codes$Product.Name==product & IO.codes$Country.Code==country])
# Fish production, Greece (1000 t) according to FAOSTAT: 
# Demersal Fish	121.66
# Freshwater Fish	2.9
# Marine Fish, Other	4.87
# Pelagic Fish	32
# Total: 161430 tonnes
# calculate footprint per tonne of fish
FP <- MP * x[IO.codes$Product.Name==product & IO.codes$Country.Code==country] / 161430
# aggregate and filter relevant stressors
colnames(FP) <- paste(Q.codes$Stressor_short, Q.codes$Compartment, Q.codes$Unit)
results <- agg(FP[, Q.codes$Unit %in% c("kg", "Mm3")])
# restructure results
rownames(results) <- paste0(IO.codes$Country.Code, "_", IO.codes$Product.Name)
results <- cbind(unique(Q.codes[Q.codes$Unit %in% c("kg", "Mm3"), 
                                c("Stressor_short", "Compartment", "Unit")]), t(results))
results <- results %>% 
  gather(target, value, -Stressor_short, -Compartment, -Unit) %>%
  mutate(country = substr(target,1,2)) %>% 
  mutate(product = substr(target,4,1000)) %>% 
  select(-target) %>% 
  filter(value != 0)

results$iso <- conc_reg$iso3c[match(results$country, conc_reg$exiobase_code)]
results$iso[results$country %in% c("WA","WE","WF","WL","WM")] <- 
  results$country[results$country %in% c("WA","WE","WF","WL","WM")]
results$iso[results$country == "HR"] <- "HRV"
results$iso[results$country == "TW"] <- "TWN"
results$group <- IO.codes$Sector.Group[match(results$product, IO.codes$Product.Name)]
results$area <- conc_reg$exiobase_area[match(results$country, conc_reg$exiobase_code)]

emissions <- results %>% 
  filter(Stressor_short %in% c("CO2e","NOx","SOx","PM10","PM2.5","Pb","As","Cd","NOX","Hg","Se","PCB")) %>% 
  mutate(value = round(value,6)) %>% 
  filter(value != 0) %>% 
  mutate(Compartment = paste0(Compartment, ", ", Unit)) %>% 
  mutate(Unit = NULL)
emissions_country <- emissions %>% 
  group_by(iso, area, Stressor_short) %>% 
  summarise(value = sum(value)) %>% 
  spread(Stressor_short, value, fill = 0)
fwrite(emissions_country, "output/fish_emissions_countries.csv")
emissions_product <- emissions %>% 
  group_by(group, product, Stressor_short) %>% 
  summarise(value = sum(value)) %>% 
  spread(Stressor_short, value, fill = 0)
fwrite(emissions_product, "output/fish_emissions_products.csv")

water <- results %>%
  filter(Compartment == "Blue.consumption") %>%
  mutate(value = round(value, 6)) %>%
  filter(value != 0) %>%
  mutate(Compartment = paste0(Compartment, ", ", Unit)) %>%
  mutate(Unit = NULL)
water_country <- water %>%
  group_by(iso, area, Stressor_short) %>%
  summarise(value = sum(value)) %>%
  spread(Stressor_short, value, fill = 0)
fwrite(water_country, "output/fish_water_countries.csv")
water_product <- water %>%
  group_by(group, product, Stressor_short) %>%
  summarise(value = sum(value)) %>%
  spread(Stressor_short, value, fill = 0)
fwrite(water_product, "output/fish_water_products.csv")



# ---------------- consumption of fish from GR --------------------------

# prepare final demand
y <- Y
colnames(y) <- Y.codes$`Region Name`
y <- agg(y)

# derive footprints
FP <- y * as.numeric(L[IO.codes$Product.Name==product & IO.codes$Country.Code==country,])

results <- FP %>%
  as.matrix() %>% 
  as_tibble() %>% 
  mutate(origin = paste0(IO.codes$Country.Code, "_", IO.codes$Product.Name)) %>% 
  gather(country_target, value, -origin) %>%
  mutate(country_final = substr(origin,1,2)) %>% 
  mutate(item_final = substr(origin,4,1000)) %>% 
  select(-origin) %>% 
  filter(value != 0)

results$iso_target <- conc_reg$iso3c[match(results$country_target, conc_reg$exiobase_code)]
results$iso_target[results$country_target %in% c("WA","WE","WF","WL","WM")] <- 
  results$country_target[results$country_target %in% c("WA","WE","WF","WL","WM")]
results$iso_target[results$country_target == "HR"] <- "HRV"
results$iso_target[results$country_target == "TW"] <- "TWN"
results$area_target <- conc_reg$exiobase_area[match(results$country_target, conc_reg$exiobase_code)]

# aggregate results
fish_country <- results %>%
  group_by(iso_target, area_target) %>%
  summarise(value = sum(value))
fwrite(fish_country, "output/fish_MEUR_countries.csv")
fish_product <- results %>%
  group_by(item_final) %>%
  summarise(value = sum(value))
fwrite(fish_product, "output/fish_MEUR_products.csv")


# ---------------- Create Visualizations --------------------------
# load world map shape file
world_map <- getMap(resolution = "low") %>%
  st_as_sf() %>%
  filter(ADMIN != "Antarctica") %>%
  dplyr::select(ADMIN, ADM0_A3, ISO_A3, REGION, continent)

# create cocoa consumption map
indicator = "Fish and seafood"
unit = "Million EUR"
title = ""
world_fp <- left_join(world_map, fish_country, by = c("ISO_A3" = "iso_target"))

ggmap <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["value"]]), size = 0.05) +
  labs(fill=paste(indicator, " <br> in", unit), title = title) +
  scale_fill_viridis_c(direction = -1, na.value = "lightgrey") +
  coord_sf(crs = "+proj=robin") + # "+proj=moll"   "+proj=wintri"
  ggthemes::theme_map() +
  theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot", 
        legend.title = ggtext::element_markdown(size = 8)) #legend.position = "right"

if(title == "") ggmap <- ggmap + theme(plot.title = element_blank())

ggsave(paste0("output/fish_MEUR_map.png"), ggmap, width = 15, height = 10, units = "cm")




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
  mutate(product = substr(product,4,100)) %>% 
  filter(value != 0)

results$iso_consumer <- conc_reg$iso3c[match(results$consumer_country, conc_reg$exiobase_code)]
results$iso_consumer[results$consumer_country %in% c("WA","WE","WF","WL","WM")] <- 
  results$consumer_country[results$consumer_country %in% c("WA","WE","WF","WL","WM")]
results$iso_producer <- conc_reg$iso3c[match(results$producer_country, conc_reg$exiobase_code)]
results$iso_producer[results$producer_country %in% c("WA","WE","WF","WL","WM")] <- 
  results$producer_country[results$producer_country %in% c("WA","WE","WF","WL","WM")]


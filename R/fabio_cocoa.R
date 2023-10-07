# cocoa flows

library(Matrix)
library(tidyverse)
library(data.table)
library(openxlsx)
library(sf)
library(rworldmap)
library(ggplot2)
library(ggtext)
library(ggthemes)
library(viridis)

# load footprint functions
source("R/fabio_functions.R")

# select fabio version
vers <- "1.2"
yr = 2020

# load FABIO data
Y <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/losses/Y.rds"))
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/losses/X.rds")) # total output

# load and prepare extensions
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E.rds")) # environmental extensions
if (vers == "1.1"){
  E_ghg <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/gwp.rds"))
  E_luh <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/luh.rds"))
  items_ghg <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/gwp_names.csv"))
  items_luh <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/luh_names.csv"))
  E_biodiv <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E_biodiv.rds"))
  items_biodiv <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/biodiv_codes.csv"))
} else if (vers == "1.2"){
  E_ghg <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E_gwp_value.rds"))
  E_luh <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E_luh_value.rds"))
  items_ghg <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/gwp_names.csv"))
  items_luh <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/luh_names.csv"))
  E_biodiv <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/E_biodiv.rds"))
  items_biodiv <- read.csv(paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/biodiv_codes.csv"))
}

# aggregate emission categories
E_ghg_agg <- lapply(E_ghg, colSums)
E_luh2_agg <- lapply(E_luh, function(x){colSums(x[grep("5 years", items_luh$Element),])})
E_ghg_pb <- lapply(E_ghg, function(x){colSums(x[!grepl("Energy use", items_ghg$Element),])})
E_ghg_energy <- lapply(E_ghg, function(x){colSums(x[grepl("Energy use", items_ghg$Element),])})
E_ghg_live <- lapply(E_ghg, function(x){colSums(x[grepl("Manure|Enteric", items_ghg$Element),])})
E_ghg_other <- lapply(E_ghg, function(x){colSums(x[!grepl("Energy use|Manure|Enteric", items_ghg$Element),])})

# convert potential species loss to E/MSY
items_biodiv <- items_biodiv[items_biodiv$land %in% c("cropland", "pasture"),]
E_biodiv <- lapply(E_biodiv, function(x){
  x <- x[, paste0(items_biodiv$species,"_", items_biodiv$land), with = FALSE]
  x <- t(t(x) / 100) 
  colnames(x) <- items_biodiv$land
  x <- agg(x)
})

E_all <- Map(function(e, e_biodiv, e_ghg, e_luh, e_energy, e_live, e_other, e_ghg_pb){
  cbind(e, "biodiv" = e_biodiv[,"cropland"]+e_biodiv[,"pasture"], 
        "ghg" = e_ghg*1000, "luh" = e_luh*1000, "ghg_energy" = e_energy*1000, "ghg_live" = e_live*1000, "ghg_other" = e_other*1000, "ghg_pb" = e_ghg_pb*1000, "ghg_all" = e_ghg*1000+e_luh*1000)
}, E, E_biodiv, E_ghg_agg, E_luh2_agg, E_ghg_energy, E_ghg_live, E_ghg_other, E_ghg_pb)

rm(E, E_biodiv, E_ghg_agg, E_luh2_agg, E_ghg_energy, E_ghg_pb, E_ghg, E_luh)

# read region classification
regions <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/regions.csv"))
if(!"continent" %in% names(regions)) regions <- fread(file="inst/regions.csv")

# read commodity classification
items <- fread(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",vers,"/items.csv"))
nrreg <- nrow(regions)
nrcom <- nrow(items)
# create index of all region-commodity combinations
index <- data.table(area_code = rep(regions$area_code, each = nrcom),
                          iso3c = rep(regions$iso3c, each = nrcom),
                          area = rep(regions$area, each = nrcom),
                          continent = rep(regions$continent, each = nrcom),
                          comm_code = rep(items$comm_code, nrreg),
                          item = rep(items$item, nrreg),
                          comm_group = rep(items$comm_group, nrreg))

Yi <- as.data.table(as.matrix(Y[[as.character(yr)]]))
Yi <- cbind(index, Yi)
setkey(Yi, area_code, comm_code)


#-------------------------------------------------------#
# ---------------- Calculate Footprints  ---------------
#-------------------------------------------------------#

cat("Calculate footprints for version", vers, "and year", yr, "\n")


# run calculations (see function library)
# fp_continents <- footprint_continent(product = "Cocoa Beans and products", allocation = "value", year = yr, y = Yi, X = X, E = E_all, 
#                                      index = index, ext = "biomass", v = vers, result.dir = "output", result.suffix = "cocoa")

fp <- footprint(source = "PER", product = "Cocoa Beans and products", consumption = "all", allocation = "value", 
                year = yr, y = Yi, X = X, E = E_all, index = index, v = vers, result.dir = "output", result.suffix = "cocoa")
fp$iso_consumer <- regions$iso3c[match(fp$consumer, regions$area)]

# write_csv(fp, "output/cocoa_flows.csv")


# ---------------- Create Visualizations ---------------
# load world map shape file
world_map <- getMap(resolution = "low") %>%
  st_as_sf() %>%
  filter(ADMIN != "Antarctica") %>%
  dplyr::select(ADMIN, ADM0_A3, ISO_A3, REGION, continent)

# create cocoa consumption map -------------------
indicator = "biomass"
unit = "tonnes"
title = "title"
world_fp <- left_join(world_map, fp[iso_consumer!="PER"], by = c("ISO_A3" = "iso_consumer"))

ggmap <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[[indicator]]), size = 0.05) +
  labs(fill=paste(indicator, " <br> in", unit), title = title) +
  scale_fill_viridis_c(direction = -1, na.value = "lightgrey") +
  coord_sf(crs = "+proj=robin") + # "+proj=moll"   "+proj=wintri"
  ggthemes::theme_map() +
  theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot", 
        legend.title = ggtext::element_markdown(size = 8)) #legend.position = "right"

if(title == "") ggmap <- ggmap + theme(plot.title = element_blank())


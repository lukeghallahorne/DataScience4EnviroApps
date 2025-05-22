

##### 
# OG CODE BELOW HERE - COPYING AND MODIFYING INTO SHINY SYNTAX AS I GO
# Transport Maps
## First Settlement Spots
### Load Data
site_id <- "FB"
release <- 477
behaviors <- c("none", "photo", "onto") 
sz_trim <- "_sz220"

loop_list <- list(); i <- 0
for(behav in behaviors) {
  i <- i + 1
  load(here("data_processed", "High_Res",
            "Larva_Tracks", paste0(site_id, "_Grid"),
            paste0(site_id,"_",release,"_",behav,sz_trim,
                   "_settled_dist.Rdata")))
  loop_list[[i]] <- get(paste0(site_id,"_",release,"_",behav,"_set_dist")) |>
    filter(setpoint == "first") |> 
    mutate(behavior = behav)
  rm(list = paste0(site_id,"_",release,"_",behav,"_set_dist"))
}

grid_set <- bind_rows(loop_list) |>
  mutate(behavior = factor(behavior, levels = behaviors))

# clean up environment
rm(loop_list, i, behav)

# Load larval tracks
loop_list <- list(); i <- 0
for(behav in behaviors) {
  i <- i + 1
  load(here("data_processed", "High_Res",
            "Larva_Tracks", paste0(site_id, "_Grid"),
            paste0(site_id,"_",release,"_",behav,sz_trim,
                   "_settled_tracks.Rdata")))
  loop_list[[i]] <- get(paste0(site_id,"_",release,"_",behav,"_set_tracks")) |>
    mutate(behavior = behav)
  rm(list = paste0(site_id,"_",release,"_",behav,"_set_tracks"))
}

grid_tracks <- bind_rows(loop_list) |>
  mutate(behavior = factor(behavior, levels = behaviors))

#### Data Prep
grid_clust <- grid_set |>
  select(behavior, x, y, h_smooth, site_track)


# 
#------------------ TRY 1 -----------------
loop_list <- list(); i <- 0
for(behav in behaviors) {
  i <- i + 1
  grid_clust_filt <- filter(grid_clust, behavior == behav)
  foo <- as.matrix(grid_clust_filt[,2:3])
  grid_clust_mat <- dist(foo)
  grid_clust_hclust <- hclust(grid_clust_mat, method = "ward.D2")
  
  plot(grid_clust_hclust)
  
  grid_clust_cut <- data.frame(grid_clust_filt,
                               cluster = cutree(grid_clust_hclust, 
                                                h = 25000)) |> tibble()
  
  loop_clust <- select(grid_clust_cut, c(behavior, site_track, cluster))
  if(i > 1) {
    loop_clust <- loop_clust |>
      mutate(cluster = cluster + max(loop_list[[i-1]]$cluster))
  }
  loop_list[[i]] <- loop_clust
}

grid_set <- grid_set |>
  inner_join(bind_rows(loop_list), 
             by = join_by(behavior, site_track)) |>
  mutate(cluster = factor(cluster))

# grid_clustID <- grid_set |>
#  group_by(behavior, cluster) |>
# summarize(n = length(x)) |>
#filter(n >= 4)

# grid_clust_polys <- st_as_sf(grid_set |> 
#                               filter(cluster %in% grid_clustID$cluster), 
#                            coords = c("x", "y"),
#                           crs = st_crs(26910)) |>
# group_by(behavior, cluster) |>
#summarize(geometry = st_combine(geometry)) |>
#st_cast("POLYGON")
#----------------------- TRY 1 DID NOT WORK---------------------
#--------------- TRY 2
# Load raw SSM data for day 1 to pull the full set of neles and nodes
day1 <- nc_open(here("data_raw", "Oly_Dispersal", "SSM2017", "ssm_0165.nc"))
# create a long dataframe with nele ID and each of the three surrounding nodes
ssm_nele2node <- data.frame(ncvar_get(day1, "nv")) |> 
  tibble() |>
  rename(node1 = X1, node2 = X2, node3 = X3) |>
  mutate(nele = 1:length(node1)) |>
  pivot_longer(cols = c(node1, node2, node3),
               values_to = "node", names_to = NULL) 
# pull the x and y coordinates for each node from day1
ssm_nodecoords <- data.frame(x = ncvar_get(day1, "x"),
                             y = ncvar_get(day1, "y")) |>
  tibble() |>
  mutate(node = 1:length(x))
#-------------------------
# clean-up environment
rm(day1)
#-------------------------
# Combine node coordinates with nele2node
# to be filtered by release site (nele) and then converted to sf polygon
ssm_grid_coords <- ssm_nele2node |>
  left_join(ssm_nodecoords, by = join_by(node))


loop_sf_list <- list(); loop_clust_list <- list(); i <- 0
for(behav in behaviors) {
  i <- i + 1
  loop_clust <- filter(grid_clust, behavior == behav)
  loop_mat <- as.matrix(loop_clust[,2:3])
  loop_dist <- dist(loop_mat)
  loop_hclust <- hclust(loop_dist, method = "ward.D2")
  loop_cut <- data.frame(loop_clust,
                         cluster = cutree(loop_hclust, 
                                          h = 25000)) |> 
    tibble() |> select(behavior, site_track, cluster)
  loop_clust <- loop_clust |>
    inner_join(loop_cut, by = join_by(behavior, site_track))
  
  k_cells <- 25
  loop_knn <- FNN::knn(train = WA_neles_coords[,2:3],
                       test = loop_clust[,2:3],
                       cl = WA_neles_coords$nele,
                       k = k_cells)
  loop_knnID <- attr(loop_knn, "nn.index")
  loop_neles <- tibble(data.frame(site_track = loop_clust$site_track,
                                  cluster = loop_clust$cluster,
                                  loop_knnID))
  loop_neles_long <- loop_neles |>
    pivot_longer(cols = (3:(k_cells+2)), names_to = NULL, values_to = "neleID")
  # loop_neles <- loop_neles[-which(duplicated(loop_neles)),]
  
  loop_nele_coords <- WA_neles_coords |> tibble() |>
    mutate(neleID = 1:length(x)) |>
    filter(neleID %in% loop_neles_long$neleID) |>
    inner_join(loop_neles_long, by = join_by(neleID)) |>
    group_by(cluster) |>
    mutate(behavior = behav) 
  
  loop_cells <- filter(ssm_grid_coords, nele %in% loop_nele_coords$nele) 
  
  loop_sf <- st_as_sf(loop_cells, coords = c("x", "y"),
                      crs = st_crs(26910)) |>
    group_by(nele) |>
    summarize(geometry = st_combine(geometry)) |>
    st_cast("POLYGON") |>
    inner_join(loop_nele_coords[,c("nele", "cluster")], 
               by = join_by("nele")) |>
    mutate(behavior = behav, 
           cluster = factor(cluster))
  
  # loop_sf <- do.call(st_sfc,
  #                   lapply(split(loop_nele_coords, loop_nele_coords$cluster),
  #                      function(d){st_cast(st_linestring(cbind(d$x, d$y)),
  #                                         "POLYGON")})) |>
  #st_set_crs(26910)
  loop_clust_list[[i]] <- loop_nele_coords
  loop_sf_list[[i]] <- loop_sf
}

grid_nele_coords <- bind_rows(loop_clust_list)
grid_sf <- bind_rows(loop_sf_list)

set.seed(1337)
grid_trackID <- grid_nele_coords |>
  select(behavior, site_track, cluster) |>
  group_by(behavior, cluster) |>
  slice_sample(n = 1)

grid_map_tracks <- grid_tracks |>
  filter(site_track %in% grid_trackID$site_track)


ggplot() +
  geom_sf(data = grid_sf)
geom_point(data = filter(WA_neles_coords, x > min(loop_clust$x) - 1000 &
                           x < max(loop_clust$x) + 1000 &
                           y > min(loop_clust$y) - 1000 &
                           y < max(loop_clust$y) + 1000),
           aes(x = x, y = y), alpha = 0.25, size = 0.25) +
  geom_point(data = loop_clust, aes(x = x, y = y, color = cluster)) +
  geom_point(data = loop_nele_coords, aes(x = x, y = y, color = cluster),
             shape = 14) 
#geom_sf(data = loop_sf)


### Settlement Map - Full Range
#### Basemaps
load(here("data_processed", "High_Res", "WA_grid_coords.Rdata"))

########### BASEMAPS #######################
# Use WA_coastline_4 (Major Shorelines) for Large-Scale Maps
WA_coastline_4 <- read_sf(here("data_raw", "MajorShorelines",
                               "MajorShorelines.shp"))
WA_coastline_4 <- st_transform(WA_coastline_4, crs = st_crs(26910))
WA_coastline_4 <- st_cast(WA_coastline_4, to = "MULTILINESTRING")

# Use WA_coastline_2 (Shorezone) for Small-Scale Maps
WA_coastline_2 <- read_sf(here("data_processed", "Shorezone", "ShoreZone_Inventory_-_Shoreline_type.shp"))
WA_coastline_2 <- st_transform(WA_coastline_2, crs = st_crs(26910))
WA_coastline_2 <- st_cast(WA_coastline_2, to = "MULTILINESTRING")

bm_xlim <- c(min(grid_set$x), max(grid_set$x))
bm_ylim <- c(min(grid_set$y), max(grid_set$y))

bm_xrange <- bm_xlim[2] - bm_xlim[1]
bm_yrange <- bm_ylim[2] - bm_ylim[1]
if(bm_xrange < bm_yrange) {
  bm_width <- bm_yrange + bm_yrange/2
  bm_height <- bm_width * 3/4
} else if (bm_xrange > bm_yrange) {
  bm_width <- bm_xrange + bm_xrange/2
  bm_height <- bm_width * 3/4
}
bm_xctr <- bm_xlim[1] + bm_xrange/2
bm_yctr <- bm_ylim[1] + bm_yrange/2

bm_xmin <- bm_xctr - bm_width/2
bm_xmax <- bm_xctr + bm_width/2
bm_ymin <- bm_yctr - bm_height/2
bm_ymax <- bm_yctr + bm_height/2

coast_box <- st_polygon(
  list(
    cbind(c(bm_xmin, bm_xmax, bm_xmax, bm_xmin, bm_xmin),
          c(bm_ymin, bm_ymin, bm_ymax, bm_ymax, bm_ymin))
  )
)
coast_poly <- st_sfc(coast_box, crs = st_crs(26910))

# Trim coastline polygon to WA_nodes_coords limits
WA_coastline <-  st_intersection(WA_coastline_4, coast_poly)
WA_geom <- st_bbox(WA_coastline)

grid_basemap_coast <- ggplot() +
  geom_sf(data = WA_coastline,
          alpha = 0.5) +
  scale_fill_continuous(na.value = "transparent") +
  coord_sf(expand = FALSE) +
  theme_bw()
grid_basemap_coast

grid_basemap_node <- ggplot() +
  geom_point(data = filter(WA_nodes_coords,
                           x >= bm_xmin & x <= bm_xmax &
                             y >= bm_ymin & y <= bm_ymax),
             aes(x = x, y = y),
             size = 0.25, alpha = 0.25, 
             color = "turquoise") +
  geom_sf(data = WA_coastline,
          alpha = 0.5) +
  scale_fill_continuous(na.value = "transparent") +
  coord_sf(expand = FALSE) +
  theme_bw()

grid_basemap_FVCOM <- ggplot() +
  geom_path(data = filter(WA_grid_coords,
                          x >= bm_xmin-100 & x <= bm_xmax+100 &
                            y >= bm_ymin-100 & y <= bm_ymax+100),
            aes(x = x, y = y, group = line),
            alpha = 0.25, 
            color = "turquoise4") +
  geom_sf(data = st_geometry(WA_coastline),
          alpha = 0.5) +
  scale_fill_continuous(na.value = "transparent") +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(bm_xmin, bm_xmax),
           ylim = c(bm_ymin, bm_ymax),
           expand = FALSE) +
  theme_bw() 


# Zoomed-In Basemap
grid_release <- grid_map_tracks |>
  filter(hour == release & sec == 1) |>
  summarize(x = mean(x), y = mean(y)) 
if(site_id %in% c("FB")) {
  grid_zoom_coords <- data.frame(x = c(grid_release$x - 10000,
                                       grid_release$x + 1000),
                                 y = c(grid_release$y - 12000,
                                       grid_release$y + 20000))
} else {
  grid_zoom_coords <- data.frame(x = c(grid_release$x - 10000,
                                       grid_release$x + 10000),
                                 y = c(grid_release$y - 10000,
                                       grid_release$y + 10000))
}
bm_xlim <- c(min(grid_zoom_coords$x), max(grid_zoom_coords$x))
bm_ylim <- c(min(grid_zoom_coords$y), max(grid_zoom_coords$y))

bm_xrange <- bm_xlim[2] - bm_xlim[1]
bm_yrange <- bm_ylim[2] - bm_ylim[1]
if(bm_xrange < bm_yrange) {
  bm_width <- bm_yrange + bm_yrange/2
  bm_height <- bm_width * 3/4
} else if (bm_xrange > bm_yrange | bm_xrange == bm_yrange) {
  bm_width <- bm_xrange + bm_xrange/2
  bm_height <- bm_width * 3/4
}
bm_xctr <- bm_xlim[1] + bm_xrange/2
bm_yctr <- bm_ylim[1] + bm_yrange/2

bm_xmin <- bm_xctr - bm_width/2
bm_xmax <- bm_xctr + bm_width/2
bm_ymin <- bm_yctr - bm_height/2
bm_ymax <- bm_yctr + bm_height/2

coast_box <- st_polygon(
  list(
    cbind(c(bm_xmin, bm_xmax, bm_xmax, bm_xmin, bm_xmin),
          c(bm_ymin, bm_ymin, bm_ymax, bm_ymax, bm_ymin))
  )
)
coast_poly <- st_sfc(coast_box, crs = st_crs(26910))
zoom_lims <- data.frame(xlim = c(bm_xmin, bm_xmax),
                        ylim = c(bm_ymin, bm_ymax))


# Trim coastline polygon to WA_nodes_coords limits
WA_coastline <-  st_intersection(WA_coastline_4, coast_poly)
WA_geom <- st_bbox(WA_coastline)

grid_basemap_coastZoom <- ggplot() +
  geom_sf(data = WA_coastline,
          alpha = 0.5) +
  scale_fill_continuous(na.value = "transparent") +
  coord_sf(expand = FALSE, xlim = zoom_lims$xlim,
           ylim = zoom_lims$ylim) +
  theme_bw() 
grid_basemap_coastZoom



#### Map - Points

pal1 <- pnw_palette("Bay", n = 12)
pal3 <- pal1[c(2,4,5)]
palClust <- pnw_palette("Bay", n = length(unique(grid_set$cluster)))
site_name <- as.character(filter(releasesites_WADFW, id == site_id)$site)

set_map <- grid_basemap_coast +
  geom_point(data = grid_set,
             aes(x = x, y = y, color = behavior, shape = behavior),
             alpha = 1, size = 1) +
  scale_color_manual(values = palClust) +
  scale_shape(labels = c("No Swimming", "Phototactic", "Ontogenetic")) +
  guides(color = "none") +
  labs(title = paste0(site_name, " Settlement Spots"),
       subtitle = paste0("First Settlement Locations after 220\u03bcm"),
       shape = "Behavior", 
       x = NULL, y = NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
set_map

ggsave(plot = set_map, filename = here("data_processed", "figures_final",
                                       "Paper1_Behavior", 
                                       paste0("P1_",site_id,"_Set1st_v1.png")),
       height = 3000, width = 6000, units = "px", create.dir = TRUE)

#### Map - Grid Cells

pal1 <- pnw_palette("Bay", n = 12)
pal3 <- pal1[c(2,4,5)]
palClust <- pnw_palette("Bay", n = length(unique(grid_set$cluster)))
site_name <- as.character(filter(releasesites_WADFW, id == site_id)$site)

set_map <- grid_basemap_coast +
  geom_point(data = grid_set_coords,
             aes(x = x, y = y, color = behavior, shape = behavior),
             alpha = 1, size = 1) +
  scale_color_manual(values = palClust) +
  scale_shape(labels = c("No Swimming", "Phototactic", "Ontogenetic")) +
  guides(color = "none") +
  labs(title = paste0(site_name, " Settlement Spots"),
       subtitle = paste0("First Settlement Locations after 220\u03bcm"),
       shape = "Behavior", 
       x = NULL, y = NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
set_map

ggsave(plot = set_map, filename = here("data_processed", "figures_final",
                                       "Paper1_Behavior", 
                                       paste0("P1_",site_id,"_Set1st_v2.png")),
       height = 3000, width = 6000, units = "px", create.dir = TRUE)



#### Map - Polygons

pal1 <- pnw_palette("Bay", n = 12)
pal3 <- pal1[c(2,4,5)]
palClust <- pnw_palette("Bay", n = length(unique(grid_clust_polys$cluster)))
site_name <- as.character(filter(releasesites_WADFW, id == site_id)$site)

set_map <- grid_basemap_coast +
  new_scale_fill() +
  geom_sf(data = grid_sf, 
          aes(fill = behavior, color = behavior),
          alpha = 0.5) +
  scale_fill_manual(values = pal3,
                    labels = c("No Swimming", "Phototactic", "Ontogenetic")) +
  scale_color_manual(values = pal3,
                     labels = c("No Swimming", "Phototactic", "Ontogenetic")) +
  #guides(fill = "none", color = "none") +
  labs(title = paste0(site_name, " Settlement Spots"),
       subtitle = paste0("First Settlement Locations after 220\u03bcm"),
       shape = "Behavior", 
       x = NULL, y = NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  coord_sf(expand = FALSE)
#set_map

ggsave(plot = set_map, filename = here("data_processed", "figures_final",
                                       "Paper1_Behavior", 
                                       paste0("P1_",site_id,"_Set1st_v3.png")),
       height = 3000, width = 6000, units = "px", create.dir = TRUE)

# Zoomed-In Map
set_mapZoom <- grid_basemap_coastZoom +
  new_scale_fill() +
  geom_sf(data = grid_sf, 
          aes(fill = behavior, color = behavior),
          alpha = 0.5) +
  scale_fill_manual(values = pal3,
                    labels = c("No Swimming", "Phototactic", "Ontogenetic")) +
  scale_color_manual(values = pal3,
                     labels = c("No Swimming", "Phototactic", "Ontogenetic")) +
  #guides(fill = "none", color = "none") +
  labs(title = paste0(site_name, " Settlement Spots"),
       subtitle = paste0("First Settlement Locations after 220\u03bcm"),
       shape = "Behavior", 
       x = NULL, y = NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  coord_sf(expand = FALSE, 
           xlim = zoom_lims$xlim,
           ylim = zoom_lims$ylim)
#set_mapZoom

ggsave(plot = set_map, filename = here("data_processed", "figures_final",
                                       "Paper1_Behavior", 
                                       paste0("P1_",site_id,"_Set1st_v3_Zoom.png")),
       height = 3000, width = 6000, units = "px", create.dir = TRUE)

#### Map - Tracks

pal1 <- pnw_palette("Bay", n = 12)
pal3 <- pal1[c(2,4,5)]
palClust <- pnw_palette("Bay", n = length(unique(grid_set$cluster)))
site_name <- as.character(filter(releasesites_WADFW, id == site_id)$site)

set_map <- grid_basemap_coast +
  geom_path(data = grid_map_tracks |> 
              mutate(behav_site_track = paste0(behavior,"_",site_track)),
            aes(x = x, y = y, group = behav_site_track, color = behavior),
            alpha = 0.5) +
  geom_point(data = grid_set,
             aes(x = x, y = y, color = behavior, shape = behavior),
             alpha = 1, size = 1) +
  scale_color_manual(values = pal3) +
  scale_shape(labels = c("No Swimming", "Phototactic", "Ontogenetic")) +
  guides(color = "none") +
  labs(title = paste0(site_name, " Settlement Spots"),
       subtitle = paste0("First Settlement Locations after 220\u03bcm"),
       shape = "Behavior", 
       x = NULL, y = NULL) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
#set_map

ggsave(plot = set_map, filename = here("data_processed", "figures_final",
                                       "Paper1_Behavior", 
                                       paste0("P1_",site_id,"_Set1st_v4.png")),
       height = 3000, width = 6000, units = "px", create.dir = TRUE)

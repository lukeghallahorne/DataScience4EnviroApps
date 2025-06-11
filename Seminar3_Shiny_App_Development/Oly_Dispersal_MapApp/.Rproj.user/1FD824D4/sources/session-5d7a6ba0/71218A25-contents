#####
# Filtering larval tracks to a reasonable size for uploading
### Convert Rdata files to CSV
library(here)
library(tidyverse)
library(sf)

#------------------------
# Saving as Rdata instead of csv - smaller files and loads faster


pb <- txtProgressBar(min = 0, max = 6*3, style = 3)
loop_dist <- list(); loop_tracks <- list(); i <- 0
for(site_id in c("FB", "PB", "KH", "SamB", "SimB", "UR")) {
  for(behav in c("none", "photo", "onto")) {
    if(site_id %in% c("FB", "PB", "KH", "SamB", "SimB")) {
      release <- 477
    } else if (site_id %in% c("UR")) {
      release <- 70
    }
    i <- i + 1
    # Larval Tracks
    ## load full dataset
    load(here("oly_data", "2017", paste0(site_id,"_larvatracks"),
              paste0(site_id,"_",release,"_",behav,"_sz220_settled_tracks.Rdata")))
    # re-lable for easy looping
    loop_tr <- get(paste0(site_id,"_",release,"_",behav,"_set_tracks"))
    # select 10 random tracks
    loop_IDs <- sample(unique(loop_tr$site_track),size = 10)
    # filter full dataset by 10 tracks, remove extra columns, and save to list
    loop_tracks[[i]] <- loop_tr |>
      filter(site_track %in% loop_IDs) |>
      select(x, y, site_track, behavior) |>
      mutate(bay = site_id, behavior = behav) 
    
    # Settlement sites
    ## load full dataset
    load(here("oly_data", "2017", paste0(site_id,"_larvatracks"),
              paste0(site_id,"_",release,"_",behav,"_sz220_settled_dist.Rdata")))
    # re-label for easy looping
    loop_df <- get(paste0(site_id,"_",release,"_",behav,"_set_dist"))
    # filter dataset by 10 tracks, remove extra columns, and save to list
    loop_dist[[i]] <- loop_df |>   
      filter(site_track %in% loop_IDs & setpoint == "first") |>
      select(site_track, size, length, Euclid) |>
      mutate(bay = site_id, behavior = behav)
    
    # clean up environment
    rm(list = c(paste0(site_id,"_",release,"_",behav,"_set_tracks"),
                paste0(site_id,"_",release,"_",behav,"_set_dist"),
                "loop_df", "loop_tr", "loop_IDs"))
    setTxtProgressBar(pb, i)
  }
}
oly_dist <- bind_rows(loop_dist) |> tibble() 
oly_tracks <- bind_rows(loop_tracks) |> tibble() 


save(oly_dist, file = here("data", "oly_dist.Rdata"))
save(oly_tracks, file = here("data", "oly_tracks.Rdata"))



rm(loop_dist, loop_tracks)

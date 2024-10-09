register_target(

  tar_target(
    name = gonio_data,
    command = {
      
      # Listing out the files since there's a change in the DeployID to deployid column that messes up
      # map_df
      # TODO - change paths here
      gonio_2019_file <-  "/Users/rob/Documents/_research/rrr/sattag_gonio_gps_match/2019_brs5_brs6/gonio_gpx_merge_2019.csv"
      
      # read the files in, in turn
      gonio_dat_2019 <- read_csv(gonio_2019_file, show_col_types = FALSE) %>% 
        janitor::clean_names() %>% 
        dplyr::rename(deployid = deploy_id)
      
      # Bind together
      gonio_dat <- gonio_dat_2019 %>% 
        dplyr::filter(deployid %in% exposed_ans) %>% 
        dplyr::mutate(strength_db = if_else(strength_db == 0, -30, strength_db)) %>% 
        dplyr::filter(strength_db != 1)
      
      gonio_dat$deployid <- stringr::str_remove(gonio_dat$deployid, "_DUML") 
      readr::write_csv(gonio_dat, file = here::here('data/raw_data',
                                           'gonio_locs.csv'))
      
      gonio_dat

      
    }
  )
)
register_target(
  
  # Here we do the summary of the distances to the ship
  
  tar_target(
    
    name = summarize_distances_rj,
    command = {
      
      
      # Container to hold output
      d2ship_rj <- vector(mode = 'list', length = length(calc_rl_rj))
      # loop over one animal/cee combination at a time
      for(i in 1:length(calc_rl_rj)){
        
        my_cee_id <- calc_rl_rj[[i]][["cee_id"]]
        my_deployid <- calc_rl_rj[[i]][["deployid"]]

        distances <- calc_rl_rj[[i]][["pts_rl"]] %>% 
          group_by(deployid, datetime) %>% 
          mutate(d2ship_km = as.numeric(d2ship_km)) %>% 
          do({x <- .$d2ship_km
          map_dfr(.x = c(.025, .5, .975),
                  .f = ~ tibble(Quantile = .x,
                                Value = quantile(x, probs = .x, na.rm = TRUE)))
          }) %>% 
          mutate(cee_id = my_cee_id,
                 crawlType = 'rejection_sampler')
        
        # Store items in List 
        d2ship_rj[[i]] <- distances
        
      } # end i loop over the list of each animal/cee combination
      
      
      d2ship_rj_df <- do.call('rbind', d2ship_rj) %>% 
        pivot_wider(names_from = Quantile, values_from = Value) %>% 
        group_by(deployid, cee_id) %>% 
        slice_head()

      write_csv(d2ship_rj_df, here::here('results/output_data/d2ship_rj.csv'))
      d2ship_rj_df
      
      })
             

)
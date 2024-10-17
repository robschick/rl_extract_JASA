register_target(
  
  # Here we do the summary of the distances to the ship
  # for animals that were regularly sampled, i.e.,
  # they did not use the rejection sampler approach
  
  tar_target(
    
    name = summarize_distances,
    command = {
      
      
      # Container to hold output
      d2ship <- vector(mode = 'list', length = length(calc_rl))
      # loop over one animal/cee combination at a time
      for(i in 1:length(calc_rl)){
        
        my_cee_id <- calc_rl[[i]][["cee_id"]]
        my_deployid <- calc_rl[[i]][["deployid"]]

        distances <- calc_rl[[i]][["pts_rl"]] %>% 
          group_by(deployid, datetime) %>% 
          mutate(d2ship_km = as.numeric(d2ship_km)) %>% 
          do({x <- .$d2ship_km
          map_dfr(.x = c(.025, .5, .975),
                  .f = ~ tibble(Quantile = .x,
                                Value = quantile(x, probs = .x, na.rm = TRUE)))
          }) %>% 
          mutate(cee_id = my_cee_id,
                 crawlType = 'regular')
        
        # Store items in List 
        d2ship[[i]] <- distances
        
      } # end i loop over the list of each animal/cee combination
      
      d2ship_df <- do.call('rbind', d2ship) %>% 
        pivot_wider(names_from = Quantile, values_from = Value) %>% 
        group_by(deployid, cee_id) %>% 
        slice_head()
      
      write_csv(d2ship_df, here::here('results/output_data/d2ship.csv'))
      d2ship_df
      
      })
             

)
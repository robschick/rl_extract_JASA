register_target(
  
  # Here we do the merge of the distances to the ship
  # calculated with the crawl-based rejection sampler
  # and also without. If there's ever a duplicate, I'll
  # keep the rejection sampler version
  
  tar_target(
    
    name = merge_distances,
    command = {
      
      sumd_rj <- summarize_distances_rj %>% 
        mutate(match_id = paste0(deployid, cee_id))
      
      sumd <- summarize_distances %>% 
        mutate(match_id = paste0(deployid, cee_id))
      
      sumidx <- !(sumd$match_id %in% sumd_rj$match_id)
      sumd <- sumd[sumidx, ]
      
      all_distances <- bind_rows(sumd_rj, sumd) %>% 
        dplyr::select(-match_id)
    
      write_csv(all_distances, here::here('results/output_data/',
                                          paste0(Sys.Date(),'_d2ship_combined.csv')))
      
    })
  
  
)
register_target(
  
  # Computer Directory Location for:
  # bathymetry file that will be used downstream
  # in the rejection sampler
  
  tar_target(
    
    name = locs_file_location,
    command = {
      
      here::here('data/raw_data/locations')
    
      })
             

)
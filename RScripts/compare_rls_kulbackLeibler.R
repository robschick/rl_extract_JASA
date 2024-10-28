library(tidyverse)
library(philentropy)
library(colorblindr)
library(gt)
library(ggrain)

accent_OkabeIto <- palette_OkabeIto[c(1, 2, 7, 4, 5, 3, 6)]
accent_OkabeIto[1:3] <- desaturate(lighten(accent_OkabeIto[1:3], .4), .8)
accent_OkabeIto[4:7] <- darken(accent_OkabeIto[4:7], .3)

# this script will loop over the animals in the RJ files
# and compare the data and calculate the KL differences
# which will ultimately be plotted across the animals

#######################################################################
# Directories
# With Gonio Data
data_dir_argos_gonio_rj <- "data/proc_data/rl_samples_rj"
data_dir_argos_gonio <- "data/proc_data/rl_samples"

# WithOUT Gonio Data
data_dir_argosonly_rj <- "~/Documents/_research/rrr/rl_extract_JASA-nogonio/data/proc_data/rl_samples_rj"
data_dir_argosonly <- "~/Documents/_research/rrr/rl_extract_JASA-nogonio/data/proc_data/rl_samples"


#######################################################################
# Files - Reference animals are those that have the RJ Employed
animal_files <- list.files(data_dir_argos_gonio_rj)
animal_files <- animal_files[animal_files!="ZcTag109_20_03_rl-samples-rj.csv"]
ref_animal_ids <- str_sub(animal_files, 0, 14)
manifest_argos_gonio_rj <- data.frame(deployid = str_sub(animal_files, 0, 8),
                       ceeid = str_sub(animal_files, 10, 14),
                       id_combo = str_sub(animal_files, 0, 14),
                       an_files = animal_files,
                       anc_data = 'argos_gonio_rj')

animal_files <- list.files(data_dir_argos_gonio)
idx <- which(str_sub(animal_files, 0, 14) %in% ref_animal_ids)
animal_files <- animal_files[idx]
manifest_argos_gonio <- data.frame(deployid = str_sub(animal_files, 0, 8),
                           ceeid = str_sub(animal_files, 10, 14),
                           id_combo = str_sub(animal_files, 0, 14),
                           an_files = animal_files,
                           anc_data = 'argos_gonio') 


animal_files <- list.files(data_dir_argosonly_rj)
idx <- which(str_sub(animal_files, 0, 14) %in% ref_animal_ids)
animal_files <- animal_files[idx]
manifest_argos_rj <- data.frame(deployid = str_sub(animal_files, 0, 8),
                                   ceeid = str_sub(animal_files, 10, 14),
                                id_combo = str_sub(animal_files, 0, 14),
                                an_files = animal_files,
                                   anc_data = 'argos_rj') 

animal_files <- list.files(data_dir_argosonly)
idx <- which(str_sub(animal_files, 0, 14) %in% ref_animal_ids)
animal_files <- animal_files[idx]
manifest_argos <- data.frame(deployid = str_sub(animal_files, 0, 8),
                                ceeid = str_sub(animal_files, 10, 14),
                             id_combo = str_sub(animal_files, 0, 14),
                             an_files = animal_files,
                                anc_data = 'argos') 

manifest_all <- bind_rows(manifest_argos_gonio_rj, 
                          manifest_argos_gonio,
                          manifest_argos_rj,
                          manifest_argos)

#######################################################################
# Function to read and prepare data
prepare_data <- function(my_id_combo) {
  
  # Subset data
  my_manifest <- manifest_all %>% 
    filter(id_combo == my_id_combo)
  # Read CSV files using the animal_tag to construct file paths
  # & Perform necessary data manipulations
  argos_gonio_rj <- read_csv(file.path(data_dir_argos_gonio_rj, my_manifest$an_files[1]),
                             show_col_types = FALSE)
  third_time <- unique(argos_gonio_rj$datetime)[3]
  argos_gonio_rj <- argos_gonio_rj %>% 
    filter(datetime == third_time) %>% 
    mutate(anc_data = 'argos_gonio_rj') %>% 
    select(-track_id) %>% 
    drop_na(raw)
  
  argos_gonio <- read_csv(file.path(data_dir_argos_gonio, my_manifest$an_files[2]),
                          show_col_types = FALSE) %>% 
    filter(datetime == third_time) %>% 
    mutate(anc_data = 'argos_gonio') %>% 
    select(-track_id) %>% 
    drop_na(raw)
  
  argos_rj <- read_csv(file.path(data_dir_argosonly_rj, my_manifest$an_files[3]),
                       show_col_types = FALSE) %>% 
    filter(datetime == third_time) %>% 
    mutate(anc_data = 'argos_rj') %>% 
    select(-track_id) %>% 
    drop_na(raw)
  
  argos <- read_csv(file.path(data_dir_argosonly, my_manifest$an_files[4]),
                    show_col_types = FALSE) %>% 
    filter(datetime == third_time) %>% 
    mutate(anc_data = 'argos') %>% 
    select(-track_id) %>% 
    drop_na(raw)
  
  # Return the combined data frame
  all_vals <- bind_rows(argos_gonio_rj, argos_gonio,
                        argos_rj, argos)
  
  all_vals
}

#######################################################################
# Function for analysis and plotting
analyze_and_plot <- function(data, animal_tag) {
  
  # Create and save the plot
  deployid <- unique(data$deployid)
  ceeid <- unique(data$cee_id)
  my_time <- unique(data$datetime)
  
  ggplot(data, aes(raw, color = anc_data))+
    geom_density(linewidth = 1)+
    theme_bw(base_size = 18)+
    scale_color_manual(values=c(accent_OkabeIto[5], 
                                accent_OkabeIto[1],
                                accent_OkabeIto[6],
                                accent_OkabeIto[4]))+
    labs(title = paste(deployid, ceeid, my_time, sep = ", "),
         x = 'Received Level [dB]',
         color = 'Ancillary Data')
  ggsave(filename = here::here('results/plots', 
                               paste(deployid, ceeid, 'rl-dist.png', sep = "_")),
         device = 'png',
         dpi = 'retina',
         width = 10, height = 6.1, units = 'in') 
  
  # Return the divergence values
  my_densities <- prepare_kl_data(data)
  
  # Calculate the values
  x1 <- rbind(my_densities$argos, my_densities$argos_gonio_rj)
  x2 <- rbind(my_densities$argos, my_densities$argos_gonio)
  x3 <- rbind(my_densities$argos, my_densities$argos_rj)
  
  out_df <- data.frame(animal = deployid,
                       cee_id = ceeid,
                       comparison = c('arg2ag_rj',
                                      'arg2ag',
                                      'arg2rj'),
                       kl_div = c(KL(x1), KL(x2), KL(x3)))
  out_df
  
}

prepare_kl_data <- function(data) {
  # Create a list to store densities for each distribution
  densities <- list()
  
  # Iterate over each unique value in anc_data
  for (dist in unique(data$anc_data)) {
    subset_data <- data %>% filter(anc_data == dist)
    density_obj <- density(subset_data$raw)
    
    # Discretize and normalize the density
    widths <- diff(density_obj$x)
    discrete_density <- density_obj$y[-length(density_obj$y)] * widths
    normalized_density <- discrete_density / sum(discrete_density)
    
    densities[[dist]] <- normalized_density
  }
  
  return(densities)
}

#######################################################################
# Main workflow
results <- vector('list', 
                  length = length(unique(manifest_all$id_combo)))

for(id_combo in unique(manifest_all$id_combo)) {
  print(id_combo)
  data <- prepare_data(id_combo)
  result <- analyze_and_plot(data, id_combo)
  results[[id_combo]] <- result
}

# Now results contains the analysis results for all animals
kl_df <- do.call(rbind, results) %>% 
  pivot_wider(id_cols = c(animal, cee_id),
              names_from = comparison,
              values_from = kl_div) %>% 
  arrange(desc(arg2ag_rj))

# Make a gt table
kl_df %>% 
  gt(rowname_col = 'animal') %>% 
  fmt_number(columns = c('arg2ag_rj', 'arg2ag', 'arg2rj'), 
             decimals = 2) %>% 
  tab_footnote(
    footnote = "Comparing ARGOS only to ARGOS + Gonio + Rejection Sampler",
    locations = cells_column_labels(columns = arg2ag_rj)) %>% 
  tab_footnote(
    footnote = "Bigger values correspond to more different distributions",
    locations = cells_column_labels(columns = arg2ag_rj)) %>% 
  tab_footnote(
        footnote = "Comparing ARGOS only to ARGOS + Gonio",
        locations = cells_column_labels(columns = arg2ag))%>%
  tab_footnote(
            footnote = "Comparing ARGOS only to ARGOS + Rejection Sampler",
            locations = cells_column_labels(columns = arg2rj)  )%>% 
  gtsave(file = here::here('results/kl_table.pdf'))

# Quick plot
kl_df_long <- do.call(rbind, results)
ggplot(data = kl_df_long) + 
  geom_density(aes(x = kl_div, color = comparison, fill = comparison), alpha = .2)+
  scale_color_brewer(palette = 'Set1')+
  scale_fill_brewer(palette = 'Set1')+
  theme_bw()

kl_df_long <- kl_df_long %>% 
  mutate(glabel = case_when(comparison == 'arg2ag_rj' ~ "ARGOS to\n ARGOS-Gonio-RJ",
                            comparison == 'arg2ag' ~ "ARGOS to\n ARGOS-Gonio",
                            comparison == 'arg2rj' ~ "ARGOS to\n Rej. Sampler"))
ggplot(kl_df_long, aes(glabel, kl_div, fill = glabel)) +
  geom_rain(alpha = .5) +
  theme_classic() +
  scale_fill_brewer(palette = 'Greys') +
  guides(fill = 'none', color = 'none') +
  coord_flip()+
  labs(y = 'Kullback-Leibler Divergence',
       x = '')
ggsave(filename = here::here('results/plots/2024-10-18_kl-dist.png'),
       device = 'png',
       dpi = 'retina',
       width = 6.1, height = 6.1, units = 'in') 


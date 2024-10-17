library(tidyverse)
library(readxl)
rl_compare <- read_excel("results/output_data/2024_10-10_rl-compare_error_methods.xlsx")
ggplot(rl_compare, aes(median_rl, deployid, group = Error_type, colour = Error_type))+
  geom_pointrange(aes(xmin = lower, xmax = upper), position=position_dodge(width=0.5))+
  # geom_jitter(aes(color = Error_type))+
  facet_grid(vars(cee_id), scales = 'free_y', space = 'free')+
  annotate("rect", xmin = -Inf, xmax = 100, ymin = -Inf, ymax = Inf,
           alpha = .4, fill = '#cccccc')+
  annotate("rect", xmin = 100, xmax = 120, ymin = -Inf, ymax = Inf,
           alpha = .4, fill = '#969696')+
  annotate("rect", xmin = 120, xmax = Inf, ymin = -Inf, ymax = Inf,
           alpha = .4, fill = '#636363')+
  scale_color_brewer(palette = 'Set2')+
  theme_bw()

ggsave(filename = here::here("results", '2024-10-10_error_compare.png'),
       device = 'png',
       dpi = 'retina',
       width = 10, height = 6.1, units = 'in') 

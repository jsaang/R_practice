library(ggplot2)

for(n in 1:10){
  gg = ggplot() +
    geom_point(aes(x = sample(1:10,
                              size = 10,
                              replace = T),
                   y = sample(1:10,
                              size = 10,
                              replace = T)))
  ggsave(filename = paste0('sample_ggplot_', n, '.png'),
         plot = gg)
}
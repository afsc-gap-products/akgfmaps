# Contributed by Sean Rohan on November 24, 2023

# eval_plot_breaks helps users select breaks for make_idw_map() and 
# make_idw_stack(). However, users are encouraged to select breaks 
# based on the properties of their data. This demo shows how to use 
# eval_plot_breaks() for exploring options for breaks.

library(akgfmaps)
library(tidyr)

# Example 1: Breaks with samples drawn from a normal distribution
dat <- data.frame(x = abs(rnorm(450, mean = 100, sd = 50)))

set.seed(1251)

algo_breaks <- akgfmaps::eval_plot_breaks(CPUE = dat$x, 
                                          n.breaks = 4)

# Breaks are spread out pretty evenly among bins. If these were spatial data.
# If these were spatial data, it's pretty likely jenks would work pretty well and we would see 
# clear distributional patterns. 
table(cut(dat$x, 
          breaks = c(0, algo_breaks[algo_breaks$style == "jenks" , 2:6]),
          right = TRUE))

brks <- algo_breaks |>
  tidyr::pivot_longer(cols = 2:6)

ggplot() +
  geom_density(data = dat,
               mapping = aes(x = x)) +
  geom_vline(data = brks,
             mapping = aes(xintercept = value, color = name)) +
  scale_x_log10() +
  facet_wrap(~style)

ggplot() +
  stat_ecdf(data = dat,
            mapping = aes(x = x)) +
  geom_vline(data = brks,
             mapping = aes(xintercept = value, color = name)) +
  scale_x_log10() +
  facet_wrap(~style)

# Example 2: Breaks with a multi-modal distribution where modes cover multiple orders of magnitude.

set.seed(1251)

dat <- data.frame(x = abs(c(rnorm(200, mean = 2, sd = 0.4),
                            rnorm(200, mean = 5, sd = 1),
                            rnorm(30, mean = 10, sd = 2),
                            rnorm(17, mean = 100, sd = 20),
                            rnorm(3, mean = 1e3, sd = 2e4))))

algo_breaks <- akgfmaps::eval_plot_breaks(CPUE = dat$x, 
                                          n.breaks = 4)

# Jenks breaks would result in >99% of data falling in one bin (1-163)
# If these were spatial data, the whole map would look the same except for a couple of hot spots.
table(cut(dat$x, 
          breaks = c(0, algo_breaks[algo_breaks$style == "jenks" , 2:6]),
          right = FALSE))

brks <- algo_breaks |>
  tidyr::pivot_longer(cols = 2:6)

# It's unclear which would be the best automatic option based on the distribution of the data, 
# although all of them perform pretty poorly. Manual break selection would be needed to provide
# a reasonsable representation of species distributions.
ggplot() +
  geom_density(data = dat,
               mapping = aes(x = x)) +
  geom_vline(data = brks,
             mapping = aes(xintercept = value, color = name)) +
  scale_x_log10() +
  facet_wrap(~style)

ggplot() +
  stat_ecdf(data = dat,
            mapping = aes(x = x)) +
  geom_vline(data = brks,
             mapping = aes(xintercept = value, color = name)) +
  scale_x_log10() +
  facet_wrap(~style)
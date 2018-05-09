# plot_norm
Vizualisation of one or multiple normal-distributions (plot of the distribution, shaded areas, mean, sd, test-value)

```r
# standard normal distribution (mean = 0, sd = 1)
plot_norm()

# standard normal distribution (mean = 100, sd = 10, alpha = 0.05)
plot_norm(mean = 100, sd = 10, alpha = 0.05)

# two normal distributions in one plot
plot_norm(mean = 100, sd = 10, alpha = 0.05)
plot_norm(mean = 115, sd = 15, color = "blue", show_sd = FALSE, add = TRUE)

# two normal distributions in one plot, custom canvas size
plot_empty(x_min = 60, x_max = 160, y_min = 0, y_max = 0.04, main = "normal distribution")
plot_norm(mean = 100, sd = 10, alpha = 0.05, add = TRUE)
plot_norm(mean = 115, sd = 15, color = "blue", show_sd = FALSE, add = TRUE)
```

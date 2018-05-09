################################################################################
# plot_norm (plots a normal distribution)
################################################################################

plot_norm <- function(mean = 0, sd = 1, test_x = NA, alpha = NA, side = "both", 
                      x_min = NA, x_max = NA, y_max = NA, 
                      show_sd = TRUE, show_text = TRUE, color = "black", add = FALSE)  {
  
  # define xlim, ylim
  xlim_left <- ifelse(is.na(x_min), mean - 4*sd, x_min)
  xlim_right <- ifelse(is.na(x_max), mean + 4*sd, x_max)
  x_all <- seq(xlim_left, xlim_right, length.out = 200)
  y_all <- dnorm(x_all, mean = mean, sd = sd)
  ylim_top <- ifelse(is.na(y_max), max(y_all), y_max)
  
  # if side both, split alpha in left and right
  if (!is.na(alpha) & side == "both") {
    alpha_side = alpha/2
  } else {
    alpha_side = alpha
  }
  
  # x confidence
  xconf_left <- qnorm(alpha_side, mean = mean, sd = sd)
  xconf_right <- qnorm(1 - alpha_side, mean = mean, sd = sd)
  
  # calculate x for plotting density of normal distribution
  if (is.na(alpha))  {
    x <- x_all
  } else if (!is.na(alpha) & side == "left")  {
    x <- seq(xconf_left, xlim_right, length.out = 100)
  } else if (!is.na(alpha) & side == "right")  {
    x <- seq(xlim_left, xconf_right, length.out = 100)
  } else if (!is.na(alpha) & side == "both")  {
    x <- seq(xconf_left, xconf_right, length.out = 100)
  } # if
  
  # plot density
  y <- dnorm(x, mean = mean, sd = sd)
  title = paste0("normal distribution: mean = ", round(mean,2), ", sd = ", round(sd,2))
  if (!is.na(alpha) & is.na(test_x)) {
    title = paste0(title, "\n", "alpha = ", alpha, ", side = ", side)
  } else if (!is.na(alpha) & !is.na(test_x)) {
    title = paste0(title, "\n", "test x = ", test_x, ", alpha = ", alpha, ", side = ", side)
  } 
  
  if (!add)  {
    plot(x,y, type = "l", lwd = 4,
         col = color,
         main = title, 
         ylab = "probability",
         xlim = c(xlim_left, xlim_right),
         ylim = c(0, ylim_top))
  } else  {
    lines(x,y, lwd = 4, col = color)
  }
  
  # plot mean, sd, test_x
  abline(v = mean, col = "black", lty="dotted", lwd = 1)
  
  if (show_sd)  {
    abline(v = mean - sd, col = "black", lty = "dotted", lwd = 1)
    abline(v = mean + sd, col = "black", lty = "dotted", lwd = 1)
  } 
  
  abline(h = 0, col = rgb(0.5, 0.5, 0.5, alpha = 0.3))
  
  if (!is.na(test_x)) {
    abline(v = test_x, col = "royalblue", lty = "solid", lwd = 2)
    if (show_text)  {
      text(test_x, max(y)*0.9, test_x, col = "blue")
    }
  }
  
  # plot alpha left side
  if (!is.na(alpha) & (side == "both" | side == "left")) {
    
    x_min = qnorm(alpha_side, mean = mean, sd = sd)
    abline(v = x_min, col = "tomato3", lty = "dotted", lwd = 1)
    if (show_text)  {
      text(x_min, max(y)*0.8, round(x_min,2), col = "red")
    }
    x_alpha <- seq(min(x_all), x_min, length.out = 50)
    y_alpha <- dnorm(x_alpha, mean = mean, sd = sd)
    x_poly <- c(min(x_alpha), x_alpha, max(x_alpha))
    y_poly <- c(0, y_alpha, 0)
    points(x_poly, y_poly, type = "n")
    polygon(x_poly, y_poly, col = "tomato3", border = "tomato3", density = 40, angle = 135)
    lines(x_alpha, y_alpha, col = "tomato3", lwd = 4)
  } # if side
  
  # plot alpha right side
  if (!is.na(alpha) & (side == "both" | side == "right")) {
    
    x_max = qnorm(1 - alpha_side, mean = mean, sd = sd)
    abline(v = x_max, col = "tomato", lty = "dotted", lwd = 1)
    if (show_text)  {
      text(x_max, max(y)*0.8, round(x_max,2), col = "red")
    }  
    x_alpha <- seq(x_max, max(x_all), length.out = 50)
    y_alpha <- dnorm(x_alpha, mean = mean, sd = sd)
    x_poly <- c(min(x_alpha), x_alpha, max(x_alpha))
    y_poly <- c(0, y_alpha, 0)
    points(x_poly, y_poly, type = "n")
    polygon(x_poly, y_poly, col = "tomato3", border = "tomato3", density = 40, angle = 135)
    lines(x_alpha, y_alpha, col = "tomato3", lwd = 4)
  } # if side
} # plot_norm

################################################################################
# plot_empty (plots an empty canvas)
################################################################################

plot_empty <- function(x_min = -4, x_max = 4, y_min = 0, y_max= 0.4, main = "")  {
  plot(x = c(x_min, x_max), 
       y = c(y_min, y_max), 
       type = "n", 
       xlab = "x", 
       ylab ="probability",
       main = main)
  
} # plot_empty


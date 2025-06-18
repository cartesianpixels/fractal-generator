library(ggplot2)
library(viridis)
library(dplyr)

random_params <- list(
  color_scheme = sample(c("plasma", "viridis", "magma", "inferno", "turbo", "cividis"), 1),
  max_iterations = sample(c(50, 80, 100, 150, 200), 1),
  resolution = sample(c(200, 300, 400), 1),
  zoom_center_real = runif(1, -2, 1),
  zoom_center_imag = runif(1, -1.5, 1.5),
  zoom_level = runif(1, 0.001, 0.5),
  julia_param = complex(real = runif(1, -1, 1), imaginary = runif(1, -1, 1))
)

mandelbrot <- function(c, max_iter = 100) {
  z <- 0
  for (i in 1:max_iter) {
    if (abs(z) > 2) return(i)
    z <- z^2 + c
  }
  return(max_iter)
}

create_mandelbrot_zoom <- function() {
  center_real <- -0.7453
  center_imag <- 0.1127
  zoom_levels <- seq(1, 0.001, length.out = 20)
  mandelbrot_data <- data.frame()
  
  for (frame in 1:length(zoom_levels)) {
    zoom <- zoom_levels[frame]
    real_seq <- seq(center_real - zoom, center_real + zoom, length.out = 200)
    imag_seq <- seq(center_imag - zoom, center_imag + zoom, length.out = 200)
    frame_data <- expand.grid(real = real_seq, imag = imag_seq)
    frame_data$c <- frame_data$real + 1i * frame_data$imag
    frame_data$iterations <- sapply(frame_data$c, mandelbrot, max_iter = 80)
    frame_data$frame <- frame
    frame_data$zoom_level <- zoom
    mandelbrot_data <- rbind(mandelbrot_data, frame_data)
  }
  return(mandelbrot_data)
}

create_static_mandelbrot <- function(width = 800, height = 800, max_iter = 100) {
  real_seq <- seq(-2.5, 1.5, length.out = width)
  imag_seq <- seq(-2, 2, length.out = height)
  mandelbrot_grid <- expand.grid(real = real_seq, imag = imag_seq)
  mandelbrot_grid$c <- mandelbrot_grid$real + 1i * mandelbrot_grid$imag
  mandelbrot_grid$iterations <- sapply(mandelbrot_grid$c, mandelbrot, max_iter = max_iter)
  return(mandelbrot_grid)
}

mandelbrot_static <- create_static_mandelbrot(width = random_params$resolution, 
                                              height = random_params$resolution, 
                                              max_iter = random_params$max_iterations)

static_plot <- ggplot(mandelbrot_static, aes(x = real, y = imag, fill = iterations)) +
  geom_raster() +
  scale_fill_viridis_c(name = "Iterations", option = random_params$color_scheme, trans = "sqrt") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.title = element_text(color = "white", hjust = 0.5, size = 16)
  ) +
  labs(title = paste("Mandelbrot Set -", random_params$color_scheme, "theme,", random_params$max_iterations, "iterations")) +
  coord_fixed()

print(static_plot)

explore_mandelbrot <- function(center_real = -0.5, center_imag = 0, zoom = 1, resolution = 300, 
                               max_iter = NULL, color_scheme = NULL) {
  if (is.null(max_iter)) max_iter <- sample(c(80, 100, 150), 1)
  if (is.null(color_scheme)) color_scheme <- sample(c("plasma", "viridis", "magma", "inferno", "turbo"), 1)
  
  real_range <- zoom * 2
  imag_range <- zoom * 2
  real_seq <- seq(center_real - real_range, center_real + real_range, length.out = resolution)
  imag_seq <- seq(center_imag - imag_range, center_imag + imag_range, length.out = resolution)
  mandelbrot_region <- expand.grid(real = real_seq, imag = imag_seq)
  mandelbrot_region$c <- mandelbrot_region$real + 1i * mandelbrot_region$imag
  mandelbrot_region$iterations <- sapply(mandelbrot_region$c, mandelbrot, max_iter = max_iter)
  
  ggplot(mandelbrot_region, aes(x = real, y = imag, fill = iterations)) +
    geom_raster() +
    scale_fill_viridis_c(name = "Iterations", option = color_scheme) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      legend.background = element_rect(fill = "black"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      plot.title = element_text(color = "white", hjust = 0.5, size = 14)
    ) +
    labs(title = paste("Mandelbrot -", color_scheme, "- Center:", round(center_real, 4), "+", round(center_imag, 4), "i")) +
    coord_fixed()
}

random_center_real <- runif(1, -2, 1)
random_center_imag <- runif(1, -1.5, 1.5)
random_zoom <- runif(1, 0.1, 1)
random_color <- sample(c("plasma", "viridis", "magma", "inferno", "turbo"), 1)

plot1 <- explore_mandelbrot(center_real = random_center_real, 
                            center_imag = random_center_imag, 
                            zoom = random_zoom, 
                            resolution = random_params$resolution,
                            color_scheme = random_color)
print(plot1)

interesting_points <- data.frame(
  real = c(-0.7453, -0.8, -0.12, -0.75, -1.25),
  imag = c(0.1127, 0.156, 0.77, 0.1, 0)
)
random_point <- interesting_points[sample(nrow(interesting_points), 1), ]
deep_zoom <- runif(1, 0.001, 0.01)
random_color2 <- sample(c("plasma", "viridis", "magma", "inferno", "turbo"), 1)

plot2 <- explore_mandelbrot(center_real = random_point$real, 
                            center_imag = random_point$imag, 
                            zoom = deep_zoom, 
                            resolution = random_params$resolution,
                            color_scheme = random_color2)
print(plot2)

ultra_zoom <- runif(1, 0.0001, 0.005)
random_iterations <- sample(c(100, 150, 200, 300), 1)
random_color3 <- sample(c("plasma", "viridis", "magma", "inferno", "turbo", "cividis"), 1)

plot3 <- explore_mandelbrot(center_real = -0.7453, 
                            center_imag = 0.1127, 
                            zoom = ultra_zoom, 
                            resolution = 250,
                            max_iter = random_iterations,
                            color_scheme = random_color3)
print(plot3)

julia_set <- function(c_param = NULL, max_iter = NULL, resolution = 400, color_scheme = NULL) {  
  if (is.null(c_param)) {
    interesting_julia <- c(-0.7 + 0.27015i, -0.4 + 0.6i, 0.285 + 0.01i, -0.8 + 0.156i, -0.12 + 0.77i)
    c_param <- sample(c(interesting_julia, runif(1, -1, 1) + 1i * runif(1, -1, 1)), 1)
  }
  if (is.null(max_iter)) max_iter <- sample(c(80, 100, 150, 200), 1)
  if (is.null(color_scheme)) color_scheme <- sample(c("plasma", "viridis", "magma", "inferno", "turbo", "cividis"), 1)
  
  real_seq <- seq(-2, 2, length.out = resolution)
  imag_seq <- seq(-2, 2, length.out = resolution)
  julia_grid <- expand.grid(real = real_seq, imag = imag_seq)
  julia_grid$z <- julia_grid$real + 1i * julia_grid$imag
  
  julia_grid$iterations <- sapply(julia_grid$z, function(z) {
    for (i in 1:max_iter) {
      if (abs(z) > 2) return(i)
      z <- z^2 + c_param
    }
    return(max_iter)
  })
  
  ggplot(julia_grid, aes(x = real, y = imag, fill = iterations)) +
    geom_raster() +
    scale_fill_viridis_c(name = "Iterations", option = color_scheme) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      legend.background = element_rect(fill = "black"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      plot.title = element_text(color = "white", hjust = 0.5, size = 14)
    ) +
    labs(title = paste("Julia Set (", color_scheme, ") - c =", round(Re(c_param), 3), "+", round(Im(c_param), 3), "i")) +
    coord_fixed()
}

for (i in 1:3) {
  julia_plot <- julia_set(resolution = sample(c(250, 300, 350), 1))
  print(julia_plot)
}
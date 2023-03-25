### define font
library("showtext")
font_add_google("Martel", "my_font")
showtext_auto()

### create image
library("ggplot2")
fun <- function(x) 0.3 * dnorm(x + 3) + 1.1 * dt(x - 4, df = 1) + 0.2
init_vec <- c(0, 7)
green <- "#66DE93"
red <- "#FF1E00"
p <- ggplot() +
  xlim(-6, 9) +
  ylim(0, 0.6) +
  stat_function(
    fun = fun,
    xlim = c(-6, 10),
    geom = "area",
    fill = "#ae988a"
  ) +
  geom_function(fun = fun, n = 200) +
  geom_point(
    data = data.frame(x = init_vec, y = sapply(init_vec, fun)),
    aes(x = x, y = y),
    color = c(red, green), size = 3
  ) +
  geom_curve(
    aes(x = 0, y = fun(0), xend = -2.7, yend = 1.02 * fun(-3)),
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = 0.4, angle = 120, color = red, linewidth = 1
  ) +
  geom_curve(
    aes(x = 7, y = fun(7), xend = 4.3, yend = 1.01 * fun(4)),
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = 0.3, angle = 120, color = green, linewidth = 1
  ) +
  theme_void()
plot(p)

### build sticker
library("hexSticker")
sticker_file <- sticker(
  ### image
  subplot = p,
  s_x = 1,
  s_y = 0.9,
  s_width = 2,
  s_height = 2,
  ### package name
  package = "ino",
  p_x = 0.7,
  p_y = 1.35,
  p_color = "black",
  p_family = "my_font",
  p_fontface = "plain",
  p_size = 30,
  ### sticker
  h_size = 1.2,
  h_fill = "#d4e8dc",
  h_color = "black",
  spotlight = TRUE,
  l_x = 0.9,
  l_y = 1.4,
  l_width = 2,
  l_height = 1,
  l_alpha = 0.8,
  white_around_sticker = TRUE,
  ### URL
  url = "loelschlaeger.de/ino",
  u_x = 1,
  u_y = 0.1,
  u_color = "black",
  u_family = "my_font",
  u_size = 7,
  u_angle = 30,
  ### save file
  filename = "sticker/ino_sticker.png",
  asp = 1,
  dpi = 300
)
plot(sticker_file)

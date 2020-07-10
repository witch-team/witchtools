# logo
library(hexSticker)

imgurl <- "data-raw/witchtools.png"
sticker(imgurl,
  package = "witchtools",
  p_size = 20,
  s_x = 0.955, s_y = 0.75, asp = 1,
  s_width = 0.7,
  h_fill = "#716fac", h_color = "#293189",
  spotlight = T, l_alpha = 0.5,
  filename = "man/figures/logo.png"
)

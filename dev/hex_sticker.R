library(hexSticker)

sticker("dev/Ecoli.png", package="ShinyBioHEAT", p_size=5, p_color = "#5A8EFF", p_y = 1.35,
        s_x=1, s_y=.75, s_width=.6,
        h_fill = "#FFF9D5", h_color = "#5A8EFF", h_size = 1.5,
        filename="inst/app/www/ShinyBioHEAT_hex.png")

file.rename("inst/app/www/ShinyBioHEAT_hex.png", to = "inst/app/www/favicon.ico")

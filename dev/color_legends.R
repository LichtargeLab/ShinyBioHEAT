library(tidyverse)

legend.df <- tibble(x = 1:100, y = 1,
                    ET_color = rev(SelectColor("ET")),
                    ET_rwb = rev(SelectColor("red_white_blue")),
                    ET_ryg = rev(SelectColor("red_yellow_green")),
                    pLDDT_color = SelectColor("alphafold"),
                    other_color = SelectColor("white_red"))

# ET rainbow color
legend.ET.plot <- ggplot(legend.df) +
  geom_col(aes(x, y, fill = ET_color), show.legend = FALSE, width = 1) +
  scale_fill_manual(values = sort(legend.df$ET_color)) +
  scale_x_continuous(breaks = c(12, 88),
                     labels = c("Less important positions", "More important positions")) +
  geom_polygon(data = tibble(x = c(0.5, 100.5, 100.5, 0.5), y = c(0,0,1,1)),
               aes(x = x, y = y), color = "black", fill = NA, size = 0.5) +
  cowplot::theme_cowplot() +
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  ggh4x::force_panelsizes(rows = unit(0.1, "in"),
                          cols = unit(5, "in"),
                          TRUE)
ggsave("inst/app/www/legend_ET.png",
       legend.ET.plot, width = 5, height = 0.5, units = "in")


# ET red-white-blue scale
legend.ET.rwb.plot <- ggplot(legend.df) +
  geom_col(aes(x, y, fill = ET_rwb), show.legend = FALSE, width = 1) +
  scale_fill_manual(values = sort(legend.df$ET_rwb)) +
  scale_x_continuous(breaks = c(12, 88),
                     labels = c("Less important positions", "More important positions")) +
  geom_polygon(data = tibble(x = c(0.5, 100.5, 100.5, 0.5), y = c(0,0,1,1)),
               aes(x = x, y = y), color = "black", fill = NA, size = 0.5) +
  cowplot::theme_cowplot() +
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  ggh4x::force_panelsizes(rows = unit(0.1, "in"),
                          cols = unit(5, "in"),
                          TRUE)
ggsave("inst/app/www/legend_ET_rwb.png",
       legend.ET.rwb.plot, width = 5, height = 0.5, units = "in")


# ET red-yellow-green scale
legend.ET.ryg.plot <- ggplot(legend.df) +
  geom_col(aes(x, y, fill = ET_ryg), show.legend = FALSE, width = 1) +
  scale_fill_manual(values = sort(legend.df$ET_ryg)) +
  scale_x_continuous(breaks = c(12, 88),
                     labels = c("Less important positions", "More important positions")) +
  geom_polygon(data = tibble(x = c(0.5, 100.5, 100.5, 0.5), y = c(0,0,1,1)),
               aes(x = x, y = y), color = "black", fill = NA, size = 0.5) +
  cowplot::theme_cowplot() +
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  ggh4x::force_panelsizes(rows = unit(0.1, "in"),
                          cols = unit(5, "in"),
                          TRUE)
ggsave("inst/app/www/legend_ET_ryg.png",
       legend.ET.ryg.plot, width = 5, height = 0.5, units = "in")


legend.other.plot <- ggplot(legend.df) +
  geom_col(aes(x, y, fill = other_color), show.legend = FALSE, width = 1) +
  geom_polygon(data = tibble(x = c(0.5, 100.5, 100.5, 0.5), y = c(0,0,1,1)),
               aes(x = x, y = y), color = "black", fill = NA, size = 0.5) +
  scale_fill_manual(values = sort(legend.df$other_color)) +
  scale_x_continuous(breaks = c(1, 95),
                     labels = c("0", "Max value")) +
  cowplot::theme_cowplot() +
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  ggh4x::force_panelsizes(rows = unit(0.1, "in"),
                          cols = unit(5, "in"),
                          TRUE)
ggsave("inst/app/www/legend_other.png",
       legend.other.plot, width = 5, height = 0.5, units = "in")




legend.pLDDT.plot <-
  ggplot(legend.df) +
  geom_col(aes(x, y, fill = pLDDT_color), show.legend = FALSE, width = 1) +
  geom_polygon(data = tibble(x = c(0.5, 100.5, 100.5, 0.5), y = c(0,0,1,1)),
               aes(x = x, y = y), color = "black", fill = NA, size = 0.5) +
  scale_fill_manual(values = sort(unique(legend.df$pLDDT_color))) +
  scale_x_continuous(breaks = c(25, 60, 80, 95),
                     labels = c("Very low", "Low", "Confident", "Very high")) +
  ggtitle("Model Confidence:") +
  cowplot::theme_cowplot() +
  theme(axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        title = element_text(size = 8),
        plot.title = element_text(size = 8, hjust = 0.05, face = "plain")
        ) +
  ggh4x::force_panelsizes(rows = unit(0.1, "in"),
                          cols = unit(5, "in"),
                          TRUE)
ggsave("inst/app/www/legend_pLDDT.png",
       legend.pLDDT.plot, width = 5, height = 0.8, units = "in")

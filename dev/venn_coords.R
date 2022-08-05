library(tidyverse)
# Code to produce the coordinates used in the venn diagram.
# Some code adapted from ggVenn library
radius <- 1.1

RotateCoord <- function(vec, ref_vec, theta) {
  scaled_vec <- vec - ref_vec
  rotate_mat <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)
  output <- rotate_mat %*% scaled_vec
  output <- output[,1]
  output <- output + ref_vec
  return(output)
}

center1 <- c(-2/3, sqrt(3)/3)
center2 <- c(2/3, sqrt(3)/3)
center3 <- c(0, -sqrt(3)/3)


point1 <- c(0, sqrt(3)/3 + sqrt((radius^2 - (2/3)^2)))
point2 <- c(0, sqrt(3)/3 - sqrt((radius^2 - (2/3)^2)))
point3 <- RotateCoord(point1, c(-2/3, sqrt(3)/3), -pi/3)
point4 <- RotateCoord(point2, c(-2/3, sqrt(3)/3), -pi/3)
point5 <- RotateCoord(point1, c(2/3, sqrt(3)/3), pi/3)
point6 <- RotateCoord(point2, c(2/3, sqrt(3)/3), pi/3)


GetCircle <- function(center_coord, p1_coord, p2_coord, radius, length.out = 1000,
                      side = c("upper", "lower"), rev = FALSE) {
  side <- match.arg(side)
  if (p1_coord[2] >= center_coord[2]) {
    theta_adj <- acos((p1_coord - center_coord)[1] / radius)
  } else {
    theta_adj <- - acos((p1_coord - center_coord)[1] / radius)
  }

  output <- tibble(theta = seq(0 + theta_adj, 2 * pi * (length.out-1)/length.out + theta_adj, length.out = length.out)) %>%
    mutate(x_raw = radius * cos(theta),
           y_raw = radius * sin(theta),
           x = center_coord[1] + x_raw,
           y = center_coord[2] + y_raw) %>%
    mutate(pos = 1:n())

  slope <- (p2_coord - p1_coord)[2] / (p2_coord - p1_coord)[1]
  if (is.infinite(slope)) {
    if (side == "upper") {
      output <- output %>%
        filter(x >= p1_coord[1])
    } else{
      output <- output %>%
        filter(x <= p1_coord[2])
    }
  } else {
    b <- p1_coord[2] - slope*p1_coord[1]
    if (side == "upper") {
      output <- output %>%
        filter(y >= (x*slope + b))
    } else{
      output <- output %>%
        filter(y <= (x*slope + b))
    }
  }

  if(rev == TRUE) {
    output <- arrange(output, desc(pos))
  } else {
    output <- arrange(output, pos)
  }
  return(output)
}

shape_coord <- list(
  "set1" = bind_rows(GetCircle(center1, p1_coord = point1, p2_coord = point4,
                               radius = radius, side = "upper", rev = FALSE),
                     GetCircle(center3, p1_coord = point5, p2_coord = point4,
                               radius = radius, side = "upper", rev = TRUE),
                     GetCircle(center2, p1_coord = point1, p2_coord = point5,
                               radius = radius, side = "upper", rev = TRUE)) %>%
    mutate(set = "set1"),

  "set2" = bind_rows(GetCircle(center2, p1_coord = point6, p2_coord = point1,
                               radius = radius, side = "upper", rev = FALSE),
                     GetCircle(center1, p1_coord = point3, p2_coord = point1,
                               radius = radius, side = "upper", rev = TRUE),
                     GetCircle(center3, p1_coord = point6, p2_coord = point3,
                               radius = radius, side = "upper", rev = TRUE)) %>%
    mutate(set = "set2"),

  "set3" = bind_rows(GetCircle(center3, p1_coord = point6, p2_coord = point4,
                               radius = radius, side = "lower", rev = TRUE),
                     GetCircle(center1, p1_coord = point4, p2_coord = point2,
                               radius = radius, side = "lower", rev = FALSE),
                     GetCircle(center2, p1_coord = point2, p2_coord = point6,
                               radius = radius, side = "lower", rev = FALSE)) %>%
    mutate(set = "set3"),

  "set1&2" = bind_rows(GetCircle(center1, p1_coord = point3, p2_coord = point1,
                                 radius = radius, side = "upper", rev = FALSE),
                       GetCircle(center2, p1_coord = point1, p2_coord = point5,
                                 radius = radius, side = "upper", rev = FALSE),
                       GetCircle(center3, p1_coord = point3, p2_coord = point5,
                                 radius = radius, side = "upper", rev = TRUE)) %>%
    mutate(set = "set1&2"),

  "set1&3" = bind_rows(GetCircle(center1, p1_coord = point4, p2_coord = point2,
                                 radius = radius, side = "lower", rev = FALSE),
                       GetCircle(center2, p1_coord = point5, p2_coord = point2,
                                 radius = radius, side = "lower", rev = TRUE),
                       GetCircle(center3, p1_coord = point5, p2_coord = point4,
                                 radius = radius, side = "upper", rev = FALSE)) %>%
    mutate(set = "set1&3"),

  "set2&3" = bind_rows(GetCircle(center2, p1_coord = point2, p2_coord = point6,
                                 radius = radius, side = "lower", rev = FALSE),
                       GetCircle(center3, p1_coord = point6, p2_coord = point3,
                                 radius = radius, side = "upper", rev = FALSE),
                       GetCircle(center1, p1_coord = point2, p2_coord = point3,
                                 radius = radius, side = "lower", rev = TRUE)) %>%
    mutate(set = "set2&3"),

  "set1&2&3" = bind_rows(GetCircle(center2, p1_coord = point5, p2_coord = point2,
                                   radius = radius, side = "lower", rev = FALSE),
                         GetCircle(center1, p1_coord = point2, p2_coord = point3,
                                   radius = radius, side = "lower", rev = FALSE),
                         GetCircle(center3, p1_coord = point3, p2_coord = point5,
                                   radius = radius, side = "upper", rev = FALSE)) %>%
    mutate(set = "set1&2&3")
)

shape_df <- reduce(shape_coord, bind_rows) %>%
  select(x, y, set)


text_pos <- tribble(~set,      ~x,    ~y,   ~hjust, ~vjust,
                    "set1",   -0.8,  0.65,      0.5,    0.5,
                    "set2",    0.8,  0.65,      0.5,    0.5,
                    "set3",      0, -0.70,      0.5,    0.5,
                    "set1&2",    0,  0.85,      0.5,    0.5,
                    "set1&3", -0.6,  -0.1,      0.5,    0.5,
                    "set2&3",  0.6,  -0.1,      0.5,    0.5,
                    "set1&2&3",  0,  0.22,      0.5,    0.5)

label_pos <- tribble(~name,       ~x,    ~y,  ~hjust, ~vjust,
                     "EA_KS",   -0.8,   1.8,     0.5,    0,
                     "EA_sum",   0.8,   1.8,     0.5,    0,
                     "Freq",       0,  -1.8,     0.5,    1)


venn_data <- list(
  radius = radius,
  center = list(center1, center2, center3),
  shape_df = shape_df,
  text_pos = text_pos,
  label_pos = label_pos
)

saveRDS(venn_data, file = "inst/app/www/venn_data.rds")




GenelistToSets <- function(gene_rankings, top) {
  top_list <- list("set1" = gene_rankings_df$locus_tag[gene_rankings_df$EAKS_rank <= top],
                   "set2" = gene_rankings_df$locus_tag[gene_rankings_df$EAsum_rank <= top],
                   "set3" = gene_rankings_df$locus_tag[gene_rankings_df$Freq_rank <= top])
  AnnotateSets <- function(input_list) {
    columns <- names(input_list)
    a2 <- unique(unlist(input_list[columns]))
    output <- lapply(input_list, `%in%`, x = a2) %>% as.data.frame() %>%
      mutate(element = a2) %>%
      rowwise() %>%
      mutate(vec = list(c(set1, set2, set3))) %>%
      ungroup() %>%
      mutate(vec = map(vec, ~c(1:3)[.])) %>%
      mutate(vec = map_chr(vec, ~paste0(., collapse = "&")),
             vec = paste0("set", vec)) %>%
      dplyr::rename(set = vec)
    return(output)
  }
  output <- AnnotateSets(top_list)
  return(output)
}



GetVenn <- function(sets_input, text_pos) {
  sets_count <- sets_input %>%
    group_by(set) %>%
    summarize(n = n()) %>%
    ungroup()

  df_element <- tibble(set = c("set1", "set2", "set3",
                               "set1&2", "set1&3", "set2&3",
                               "set1&2&3")) %>%
    mutate(set1 = str_detect(set, "1"),
           set2 = str_detect(set, "2"),
           set3 = str_detect(set, "3")) %>%
    left_join(sets_count) %>%
    replace_na(list(n = 0))

  fmt <- sprintf("%%d\n(%%.%df%%%%)", 1)
  output <- text_pos %>%
    left_join(df_element, by = "set") %>%
    mutate(text = sprintf(fmt, n, 100 * n / sum(n)))
  return(output)
}



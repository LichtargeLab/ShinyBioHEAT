#' ProcessGD
#'
#' @description Process .gd file from breseq output
#'
#' @return A tibble with 4 columns: POS, REF, ALT, and INFO. This file can be used
#' as the input of VCFtoEA
#'
#' @noRd
ProcessGD <- function(path, ref_seq) {
  gd.temp <- readr::read_lines(path)
  gd.snps <- readr::read_tsv(I(gd.temp[stringr::str_sub(gd.temp, 1,3) == "SNP"]),
                             col_names = c("type", "id", "parent_id",
                                           "CHROM", "POS", "ALT", "INFO"),
                             col_types = "ccccdcc") %>%
    dplyr::select(POS, ALT, contains("INFO"))

  gd.subs <- readr::read_tsv(I(gd.temp[stringr::str_sub(gd.temp, 1,3) == "SUB"]),
                             col_names = c("type", "id", "parent_id",
                                           "CHROM", "POS", "len", "ALT", "INFO"),
                             col_types = "ccccddcc") %>%
    dplyr::filter(len == nchar(ALT)) %>%
    dplyr::mutate(POS_vec = purrr::map2(POS, len, ~seq(.x, .x + .y - 1)),
                  ALT_vec = stringr::str_split(ALT, "")) %>%
    dplyr::select(POS = POS_vec, ALT = ALT_vec, contains("INFO")) %>%
    tidyr::unnest(cols = c(POS, ALT))
  output <- dplyr::bind_rows(gd.snps, gd.subs) %>%
    dplyr::mutate(REF = ref_seq[POS]) %>%
    dplyr::select(POS, REF, ALT, contains("INFO"))

  if (!"INFO" %in% names(output)) {
    output$INFO <- NA
  }
  return(output)
}




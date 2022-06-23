#' ReadVCF
#'
#' @description Read single sample VCF file. VCF head is removed.
#'
#' @return A tibble that contains information in the VCF file.
#'
#' @noRd
ReadVCF <- function(path) {
  vcf.temp <- readr::read_lines(path)
  skip.row <- grep(pattern = "#CHROM", vcf.temp) - 1 # remove info rows from the vcf header
  vcf <- readr::read_tsv(file = path, skip = skip.row,
                         col_types = readr::cols_only(`#CHROM` = "c",
                                                      POS = "d",
                                                      ID = "c",
                                                      REF = "c",
                                                      ALT = "c",
                                                      QUAL = "c",
                                                      FILTER = "c",
                                                      INFO = "c"))
  return(vcf)
}

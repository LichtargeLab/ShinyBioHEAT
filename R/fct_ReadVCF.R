#' ReadVCF
#'
#' @description Read the entries from the vcf files, headers are removed.
#' @author Chen Wang
#' @param path Path to the vcf file
#' @param header_max Max number of rows to guess VCF header lines.
#' @return A tibble containing all the entries in the VCF file.
#' @export
ReadVCF <- function(path, header_max = 100) {
  vcf.temp <- readr::read_lines(path, n_max = header_max)
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

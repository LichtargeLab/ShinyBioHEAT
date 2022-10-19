#' GetpLDDT
#'
#' @description Extract B factor from a pdb file. Can be used to extract pLDDT from
#' AF structures.
#'
#' @param pdb_lines PDB file that stored in a string vector. Output from readLines()
#' @param chain The chain ID to extract pLDDT from.
#' @return The return value, if any, from executing the function.
#'
#' @noRd
GetpLDDT <- function(pdb_lines, chain = "A") {
  pdb <- pdb_lines
  pdb_row_type <- stringr::str_sub(pdb, 1, 6)
  output <- pdb[stringr::str_detect(pdb_row_type, "ATOM  |HETATM")] %>%
    I() %>%
    readr::read_fwf(., col_positions = readr::fwf_widths(widths = c(6, 5, 1, 4, 1, 3, 1, 1, 4, 1, 3, 8, 8, 8, 6, 6, 6, 4, 2, 2)),
                    trim_ws = TRUE,
                    show_col_types = FALSE) %>%
    dplyr::filter(X8 %in% chain) %>%
    dplyr::filter(X6 %in% c("HIS", "PRO", "GLU", "THR", "LEU", "VAL", "LYS", "ASP", "ALA",
                     "GLN", "GLY", "ARG", "TYR", "ILE", "ASN", "SER", "PHE", "MET",
                     "CYS", "TRP", "MSE")) %>%
    dplyr::filter(X4 == "CA") %>%
    dplyr::select(chain = X8, POS = X9, pLDDT = X16) %>%
    dplyr::arrange(chain, POS)
  return(output)
}

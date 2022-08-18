#' PymolColorChainByResidue
#'
#' #' @param chain The chain in the pymol session that needs to be colored. Input can
#' be a vector with all the chains that need to be colored.
#' @param poistion A numeric vector specifying the residues that needs to be
#' colored
#' @param color A string vector with hex colors specifying the color. Should be
#' equal length of position. Color format "0xFFFFFF".
#'
#' @description Produce pymol commands to color residues in given chains.
#'
#' @return A string vector with pymol commands
#'
#' @noRd
PymolColorChainByResidue <- function(chain, position, color, object = NULL) {
  if(is.null(object)) {
    object_cmd <- ""
  } else {
    object_cmd <- paste0(object, " and ")
  }
  chain_cmd <- paste0("chain ", chain)
  chain_cmd <- paste0(chain_cmd, collapse = " or ")
  workdf <- tibble(position, color) %>%
    dplyr::filter(!is.na(color)) %>%
    dplyr::group_by(color) %>%
    dplyr::summarize(position = paste0(position, collapse = "+")) %>%
    dplyr::mutate(pymol_cmd = paste0("color ", color, ", ", object_cmd,
                                     "(", chain_cmd, ") and resi ", position))
  return(workdf$pymol_cmd)
}

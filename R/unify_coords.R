#' Unify coordinates in different format to decimal coordinates
#'
#' @param x a coordinate as string
#'
#' @return a unified, numeric decimal coordinate
#' @export
#' @importFrom stringr str_match
#'
#' @examples
#' unify_coords(c("49°40'46.148\"N", "E 48.232°"))
unify_coords <- Vectorize(
  function(x) {

  dd_regex <- "^[ENWOS]{0,1}\\s*([0-9]+\\.*[0-9]*)\\s*°{0,1}\\s*[ENWOS]{0,1}$"
  dms_regex <- "[ENWOS]{0,1}\\s*([0-9]+)\\s*°\\s*([0-9]+)\\s*[:QUOTATION_MARK:]\\s*([0-9]+\\.*[0-9]*)\\s*[:QUOTATION_MARK:]\\s*[ENWOS]{0,1}"

  match_dms <- stringr::str_match(x, dms_regex)
  match_dd <- stringr::str_match(x, dd_regex)

  if (!all(is.na(match_dms))) {
    dms_d_part <- as.numeric(match_dms[2])
    dms_h_part <- as.numeric(match_dms[3])
    dms_s_part <- as.numeric(match_dms[4])
    out <- dms_d_part + dms_h_part/60 + dms_s_part/3600
  } else {
    if (!(all(is.na(match_dd)))) {
      out <- as.numeric(match_dd[2])
    } else {
      out <- NA
    }
  }
  out
}, USE.NAMES = FALSE)

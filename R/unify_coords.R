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

    direction_       <- "[ENWOS]{0,1}"
    degree_          <- "°"
    optional_degree_ <- paste0(degree_, "{0,1}")
    sign_            <- "-{0,1}"
    integer_         <- "[0-9]+"
    double_          <- "[0-9]+[\\.,]*[0-9]*"
    minute_          <- "[[:QUOTATION_MARK:]′`´]"
    second_          <- paste0(minute_, "{1,2}")
    whitespace_      <- "\\s*"
    start_           <- "^"
    end_             <- "$"
    group_start_     <- "("
    group_end_       <- ")"

    # decimal degree coordinate
    dd_regex <- paste0(
      start_,
      whitespace_,
      direction_,
      whitespace_,
      group_start_,
      sign_,
      double_,
      group_end_,
      whitespace_,
      optional_degree_,
      whitespace_,
      direction_,
      whitespace_,
      end_
    )

    # degree-minute-second coordinate
    dms_regex <- paste0(
      start_,
      whitespace_,
      direction_,
      whitespace_,
      group_start_,
      sign_,
      integer_,
      group_end_,
      whitespace_,
      degree_,
      whitespace_,
      group_start_,
      integer_,
      group_end_,
      whitespace_,
      minute_,
      whitespace_,
      group_start_,
      double_,
      group_end_,
      whitespace_,
      second_,
      whitespace_,
      direction_,
      whitespace_,
      end_
    )

    # degree-decimal-minute coordinate
    dm_regex <- paste0(
      start_,
      whitespace_,
      direction_,
      whitespace_,
      group_start_,
      sign_,
      integer_,
      group_end_,
      whitespace_,
      degree_,
      whitespace_,
      group_start_,
      double_,
      group_end_,
      whitespace_,
      minute_,
      whitespace_,
      direction_,
      whitespace_,
      end_
    )

  match_dms <- stringr::str_match(x, dms_regex)
  match_dm <- stringr::str_match(x, dm_regex)
  match_dd <- stringr::str_match(x, dd_regex)

  if (!all(is.na(match_dms))) {
    dms_d_part <- as.numeric(match_dms[2])
    dms_m_part <- as.numeric(match_dms[3])
    dms_s_part <- as.numeric(sub(",", ".", match_dms[4]))
    out <- abs(dms_d_part) + dms_m_part/60 + dms_s_part/3600
    out <- out * sign(dms_d_part)
  } else {
    if (!(all(is.na(match_dm)))) {
      dm_d_part <- as.numeric(match_dm[2])
      dm_m_part <- as.numeric(sub(",", ".", match_dm[3]))
      out <- abs(dm_d_part) + dm_m_part/60
      out <- out * sign(dm_d_part)
    } else {
      if (!(all(is.na(match_dd)))) {
        out <- as.numeric(sub(",", ".", match_dd[2]))
      } else {
        out <- NA
      }
    }
  }
  out
}, USE.NAMES = FALSE)

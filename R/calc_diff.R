

#' Calculate difference from best
#'
#' @param d
#'
#' @return columns with raced rates that have differences from the best rate
#' @export
#'
#' @examples
#' calc_diff(d)
#'
calc_diff <- function(x) {

  # get geoid, raced rate and best columns
  rates <- dplyr::select(x, geoid, best, ends_with("_rate"), -starts_with("total_"), -ends_with("_no_rate"))

  # 2/21/23 added due to prior step resulting in dupes for overcrowding
  rates <- unique(rates)

  # pivot wide table to long on geoid & best cols
  diff_long <- pivot_longer(rates, 3:ncol(rates), names_to="measure_rate", values_to="rate") %>%
    # calc diff from best
    mutate(diff=abs(best-rate)) %>%
    # create new column names for diffs from best
    mutate(measure_diff=sub("_rate", "_diff", measure_rate))

  # pivot long table back to wide
  diff_wide <- diff_long %>% dplyr::select(geoid, measure_diff, diff) %>%
    pivot_wider(names_from=measure_diff, values_from=diff)

  # join new diff from best columns back to original table
  x <- x %>% left_join(diff_wide, by="geoid")

  return(x)
}

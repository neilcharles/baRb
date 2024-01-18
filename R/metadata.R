#' Get the list of available advertiser names
#'
#' @return A character vector of advertiser names
#' @export
#'
#' @examples
#' barb_get_advertisers()
barb_get_advertisers <- function(){

  raw_json <- barb_query_api(
    barb_url_advertisers()
  )

  raw_json$json |>
    tidyjson::spread_values(advertiser_name = tidyjson::jstring('advertiser_name')) |>
    dplyr::group_by(advertiser_name) |>
    dplyr::summarise()

}

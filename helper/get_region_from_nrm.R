get_region_from_nrm <- function(region_id, map_info){

  region_ids = c("TAS", "SWWA", "SEA", "EA", "NA", "R")
  if(!(region_id %in% region_ids))
     stop("region_id must be one of TAS, SWWA, SEA, EA, NA or R")

  if(region_id == "TAS"){
    region_info <- map_info %>%
      dplyr::filter(code == "SA") %>%
      dplyr::filter(lat < -40.5)
    main_piece <- dplyr::count(region_info,piece)$piece[1]
    region_info <- region_info %>%
      dplyr::filter(piece == main_piece)
  }

  if(region_id == "SEA"){
    region_info <- map_info %>%
      dplyr::filter(code == "SA") %>%
      dplyr::filter(lat > -40.5 & long > 130)
    main_piece <- dplyr::count(region_info,piece)$piece[1]
    region_info <- region_info %>%
      dplyr::filter(piece == main_piece)
  }

  if(region_id == "SWWA"){
    region_info <- map_info %>%
      dplyr::filter(code == "SA") %>%
      dplyr::filter(long < 130)
    main_piece <- dplyr::count(region_info,piece)$piece[1]
    region_info <- region_info %>%
      dplyr::filter(piece == main_piece)
  }

  if(region_id == "NA"){
    region_info <- map_info %>%
      dplyr::filter(code == "NA")
    main_piece <- dplyr::count(region_info,piece)$piece[1]
    region_info <- region_info %>%
      dplyr::filter(piece == main_piece)
  }

  if(region_id == "R"){
    region_info <- map_info %>%
      dplyr::filter(code == "R")
    main_piece <- dplyr::count(region_info,piece)$piece[1]
    region_info <- region_info %>%
      dplyr::filter(piece == main_piece)
  }

  if(region_id == "EA"){
    region_info <- map_info %>%
      dplyr::filter(code == "EA")
    main_piece <- dplyr::count(region_info,piece)$piece[1]
    region_info <- region_info %>%
      dplyr::filter(piece == main_piece)
  }

  return(region_info)

}

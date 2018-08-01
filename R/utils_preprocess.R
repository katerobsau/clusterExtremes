create_binary_class <- function(point_info, var_name, use_id){

  # select the relevant column
  var <- point_info %>%
    dplyr::select(var_name)

  # create a new class_id that is binary
  point_info <- point_info %>%
    dplyr::mutate(class_id = (var[,1] == use_id))

  return(point_info)

}

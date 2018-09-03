#' Returns a binary class
#'
#' Takes a data frame and finds the columns matching the variable name provided.
#' For the entries in that column, it creates a new variable, binary_class,
#' where the binary_class is given by var_name == use_id.
#' Function is useful for preprocessing data for fitting max-stable models.
#'
#' @param point_info dataframe which has at column name that matches the var_name
#' @param var_name a string, that gives the variable name for matching
#' @param use_id the criteria for matching that generates the binary class
#'
#' @return Returns point_info with a new column called binary_class
#'
#' @export
#' @examples
#' df = data.frame(x= 1:10, y = 1:10, z = rep(1:2, each = 5))
#' df_update = create_binary_class(df, "z", 2)
#'
create_binary_class <- function(point_info, var_name, use_id){

  # select the relevant column
  var <- point_info %>%
    dplyr::select(var_name)

  # create a new class_id that is binary
  point_info <- point_info %>%
    dplyr::mutate(binary_class = (var[,1] == use_id))

  return(point_info)

}

#' Returns a set of samples from a vector
#'
#' For a vector of size n, sub-samples can be taken randomly, or n can be
#' partitioned into sub-samples.
#'
#' @param n size of the vector we are sampling from
#' @param sample_type string of "random", "partition" or "percentage"
#' @param num_samples for random - number of sub-samples, default is 1
#' @param samp_size for random - size of the sub-sample to take
#' @param num_paritions for partition - number of ways to partition n and generate
#' sub-samples. If the num_partions does not divide n equally, the floor is taken.
#' @param percentage for percentage - percentage of n that should be used as the samp_size.
#' In this instance, samp_size is calculated internally so does not need to be provided.
#'
#' @return Returns a matrix where the columns give the sub-samples
#'
#' @export
#' @examples
#' get_samples(n = 10, sample_type = "random", samp_size = 5)
#' get_samples(n = 10, sample_type = "random", num_samples = 2, samp_size = 5)
#' get_samples(n = 20, sample_type = "partition", num_partitions = 2)
#' get_samples(n = 10, sample_type = "percentage", num_samples = 2, percentage = 70)
#'
get_samples <- function(n, sample_type, num_samples = 1, samp_size = NULL,
                        num_partitions = NULL, percentage = NULL){

  if(!(sample_type %in% c("random", "partition", "percentage")))
    stop("sample_type incorrectly specified, must be either random or partition")

  if(sample_type == "random")
    samples = replicate(num_samples, sample(1:n, samp_size, replace = FALSE))

  if(sample_type == "partition"){
    ord = sample(1:n , n, replace = FALSE)
    num_rows = floor(n/num_partitions)
    samples = matrix(ord, num_rows, num_partitions)
  }

  if(sample_type == "percentage"){
    if(percentage > 100 | percentage < 0)
      stop("Percentage value supplied must be greater than 0 and less than 100")
    samp_size = floor(n*percentage/100)
    samples = replicate(num_samples, sample(1:n, samp_size, replace = FALSE))
  }

  return(samples)
}

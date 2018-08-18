get_fmado_dist <- function(x){
  Nnb = ncol(x)
  Tnb = nrow(x)
  V = array(NaN, dim = c(Tnb,Nnb))
  for(p in 1:Nnb) {
    x.vec = as.vector(x[,p])
    if(all(is.na(x.vec))){
      V[,p] = x.vec
      next
    }
    Femp = ecdf(x.vec)(x.vec)
    V[,p] = Femp
  }
  DD_fmado = dist(t(V), method = "manhattan", diag = TRUE, upper = TRUE)/(2*Tnb)
  return(DD_fmado)
}

get_num_common_obs <- function(fmado_data){
  pair_count_mat <- count.pairwise(fmado_data)
  pair_count_dist <- pair_count_mat %>% as.dist(upper = TRUE, diag = FALSE)
  if(any(pair_count_dist > nrow(fmado_data) |
         pair_count_dist < 0) == 1)
    stop("ERROR: common count is incorrect")
  return(pair_count_dist)
}

apply_min_obs <- function(DD_fmado, DD_common, min_common_years = 20){
  na_ind <- which(is.na(DD_fmado))
  if(unique(DD_common[na_ind]) != 0)
    stop("DD_fmado and DD_common do not match")
  too_few_ind = which(DD_common < min_common_years)
  DD_fmado[too_few_ind] = NA
  return(DD_fmado)
}

cap_fmado_dist <- function(DD_fmado){
  DD_fmado_adj = pmin(DD_fmado, rep(1/6, length(DD_fmado)))
  if(any(DD_fmado_adj > 1/6, na.rm = TRUE) == 1)
    stop("ERROR: cap_fmado_dist didn't work")
  return(DD_fmado_adj)
}

range_infill_missing_fmado <- function(DD_euclid, DD_fmado, max_euclid){
  max_ind = which(is.na(DD_fmado) & DD_euclid >= max_euclid)
  DD_fmado[max_ind] = 1/6
  min_ind = which(is.na(DD_fmado) & DD_euclid == 0)
  DD_fmado[min_ind] = 0
  check = any(DD_fmado == 0 & DD_euclid > 0, na.rm = TRUE)
  if(check == TRUE){
    warning("A fmadogram distance was zero even though the euclidean distance not zero")
  }
  return(DD_fmado)
}

combine_dist <- function(DD_euclid, DD_fmado){

  region_size = attr(DD_euclid, "Size")

  pair_ind = expand.grid(p1 = 1:region_size, p2 = 1:region_size) %>%
    as.data.frame() %>%
    filter(p1 > p2)

  pair_dd <- pair_ind %>%
    mutate(euclid = DD_euclid, fmado = DD_fmado)

  return(pair_dd)

}

smart_infill_missing_fmado <- function(pair, pair_dd, radius = 0.1){

  # order of distance object pairs
  if(pair[1] > pair[2]){
    p1_i <- pair[1] %>% as.numeric()
    p2_i <- pair[2] %>% as.numeric()
  }else{
    p1_i <- pair[2] %>% as.numeric()
    p2_i <- pair[1] %>% as.numeric()
  }

  nbr1 = pair_dd %>%
    filter(p1 == p1_i | p2 == p1_i) %>%
    filter(euclid <= radius)

  nbr2 = pair_dd %>%
    mutate(euclid = DD_euclid, fmado = DD_fmado) %>%
    filter(p1 == p2_i | p2 == p2_i) %>%
    filter(euclid <= radius)

  nbr_grid = expand.grid(x = c(nbr1$p1, nbr1$p2), y = c(nbr2$p1, nbr2$p2)) %>%
    as.data.frame() %>%
    filter(x != y) %>%
    distinct() %>%
    mutate(p1 = ifelse(x > y, x, y)) %>%
    mutate(p2 = ifelse(x > y, y, x)) %>%
    select(p1, p2)

  pair_dist = pair_dd %>%
    filter(p1 == p1_i & p2 == p2_i) %>%
    select(euclid) %>%
    as.numeric()

  fill_pairs <- left_join(nbr_grid, pair_dd, by = c("p1", "p2")) %>%
    mutate(dist = abs(euclid - pair_dist)) %>%
    filter(!is.na(fmado))

  if(nrow(fill_pairs) == 0) return(NA)

  min_dist = which.min(fill_pairs$dist)

  fill_dist = fill_pairs$fmado[min_dist]

  return(fill_dist)

}

crude_infill_missing_fmado <- function(DD_euclid, DD_fmado, max_euclid){

  # subset the data
  i = which(DD_euclid > 0 & DD_euclid < max_euclid)
  x_data = DD_euclid[i]
  y_data = DD_fmado[i]
  fit_data = data.frame(euclid = x_data, fmado = y_data)

  # crudely fit a function to the Fmadogram
  # y < - lm(fmado ~ I(log(euclid)), data = fit_data)
  # y <- mgcv::gam(fmado ~ log(euclid) + 0, data = fit_data)
  y <- lm(fmado ~ log(euclid), data = fit_data)
  # y <- mgcv::gam(fmado ~ log(euclid), data = fit_data)

  # estimate remaining NA indexes
  na_ind = which(is.na(DD_fmado) & DD_euclid > 0 & DD_euclid < max_euclid)
  xx = DD_euclid[na_ind]
  yy = predict(y, data.frame(euclid = xx))
  DD_fmado[na_ind] = pmax(yy, 0)

  if(any(DD_fmado > 1/6 | DD_fmado < 0, na.rm = TRUE) == 1)
    warning("Observations outside theoretical range")

  return(DD_fmado)

}

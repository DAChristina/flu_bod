convert_bool <- function(x){
  x <- stringr::str_to_lower(trimws(as.character(x)))
  dplyr::case_when(
    is.na(x) ~ NA_integer_,
    x %in% c("0", "tidak", "t") ~ 0L,
    x %in% c("1", "ya", "y") ~ 1L,
    TRUE ~ NA_integer_
  )
}

keyword <- function(x) x %>% 
  stringr::str_to_lower() %>% 
  stringr::str_remove_all("[^a-z0-9]")

split_hint <- function(x){
  if (is.na(x)) 
    return(c(dist = NA_character_, 
             hint = NA_character_)
    )
  
  x <- x %>% 
    stringr::str_to_lower() %>% 
    stringr::str_remove_all("[^a-z0-9 ]") %>% 
    str_squish() %>%
    stringr::str_remove("^(kec|kecamatan) ")
  
  toks <- str_split(x, " ")[[1]]
  if (length(toks) > 1){
    for (i in 2:length(toks)){
      tail <- paste(toks[i:length(toks)], collapse = " ") %>%
        stringr::str_remove("^(kab|kabupaten|kota) ") %>% 
        keyword()
      
      if (tail %in% reg_keys)
        return(c(dist = keyword(paste(toks[1:(i - 1)],
                                      collapse = " ")), 
                 hint = tail
        )
        )
    }
  }
  
  c(dist = keyword(x), hint = NA_character_)
}

pick <- function(d){
  hp <- d$province[1]; hint <- d$hint[1]; step <- "unique"
  
  if (nrow(d) > 1 && !is.na(hint) && any(d$rk == hint)){ # regency hint
    d <- d[d$rk == hint, ]; step <- "regency_hint" }
  
  if (nrow(d) > 1 && any(d$standard_province == hp)){ # hospital's province
    d <- d[d$standard_province == hp, ]; step <- "hospital_province" }
  
  if (nrow(d) > 1){# biggest regency (most villages)
    d <- d[order(-d$n_villages, d$standard_regency)[1], ]; step <- "size_tiebreak" }
  
  tibble(row = d$row[1], est_province = d$standard_province[1],
         est_regency = d$standard_regency[1],
         name_match = d$name_match[1],
         fuzzy_dist = d$fuzzy_dist[1],
         fill_step = step
  )
}

find_candidates <- function(dk_in, hint, prov){
  out <- function(d, nm, dist = NA_real_)
    dplyr::mutate(dplyr::distinct(d,
                                  standard_province,
                                  standard_regency,
                                  .keep_all = TRUE),
                  name_match = nm,
                  fuzzy_dist = dist)
  
  c1 <- pairs_dk[pairs_dk$dk == dk_in, ] # exact district name
  if (nrow(c1))
    return(out(c1, "exact"))
  
  c2 <- pairs_rk[pairs_rk$rk == dk_in, ] # annoying "district" is actually a regency
  if (nrow(c2)) 
    return(out(c2, "regency_name"))
  
  pool <- dk_by_prov[[prov]]
  has_pool <- length(pool) > 0
  d_p <- if (has_pool) 
    stringdist::stringdist(dk_in, pool,
                           method = "jw", # Jaro-Winkler
                           p = 0.1) 
  else Inf
  
  best_p <- min(d_p)
  if (has_pool && best_p <= 0.08) # close spelling in hospital province
    return(out(pairs_dk[pairs_dk$dk == pool[which.min(d_p)], ],
               "fuzzy_province",
               best_p
    )
    )
  
  if (!is.na(hint)){ # regency written in the name
    c3 <- pairs_rk[pairs_rk$rk == hint, ]
    if (nrow(c3)) 
      return(out(c3,
                 "regency_hint_only"
      )
      )
  }
  
  # hopeless resort
  d_a <- stringdist::stringdist(dk_in,
                                std_dk,
                                method = "jw",
                                p = 0.1)
  
  if (!has_pool || min(d_a) + 0.03 < best_p)
    return(out(pairs_dk[pairs_dk$dk == std_dk[which.min(d_a)], ],
               "fuzzy_any_far",
               min(d_a)
    )
    )
  out(pairs_dk[pairs_dk$dk == pool[which.min(d_p)], ],
      "fuzzy_province_far",
      best_p)
}
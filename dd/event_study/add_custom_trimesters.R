add_custom_trimesters <- function(df, trim_dict, baseline) {

  df$trim <- map_int(.x = df$abertura_lances,
                     .f = ~ which.max(.x < trim_dict$abertura_lances))
  
  if (length(baseline) > 1) {
    df <- df %>% 
      mutate(trim = case_when(
        trim < min(baseline) ~ trim,
        trim %in% baseline ~ min(baseline),
        trim > max(baseline) ~ as.integer(trim - (length(baseline) - 1))
      ))
  }
  # 
  # df <- df %>% 
  #   mutate(trim_factor = as.factor(trim) %>% 
  #            fct_relevel(as.character(min(baseline))))
  
  df
        
}

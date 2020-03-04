add_post_interactions <- function(df, x) {
  
  for (i in x) {
    
    int_col <- df$comprasnet * (df$trim == i)
    
    col_name <- str_c('cnet_X_trim_post_',
                      str_pad(i, width = 2, side = 'left', pad = '0'))
    
    
    df[[col_name]] <- int_col
    
  }
  
  df
  
}
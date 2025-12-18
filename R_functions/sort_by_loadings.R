# function to sort structure matrix:
sort_by_loadings <- function(load_matrix, print.cut=0, drop=F, dec=2, drop.cut=0.3) {
  # print.cut = cutoff, which values to consider
  
  df <- as.data.frame(round(load_matrix, digits = dec))  # numeric only
  dominant <- apply(load_matrix, 1, function(x) colnames(load_matrix)[which.max(abs(x))])
  df$dominant_factor = dominant
  
  sorting_key = abs(load_matrix[cbind(1:nrow(load_matrix),
                                      match(dominant, colnames(load_matrix)))])
  
  o <- order(dominant, -sorting_key)
  
  df_sorted <- df[o, ]
  
  
  if(drop){
    df_sorted$trim = 'N'
    for(i in rownames(df_sorted)){
      domfac=df_sorted[i,]$dominant_factor
      
      if(abs(df_sorted[i,][[domfac]])<drop.cut){
        df_sorted[i,]$trim = 'Y'
      }
    }
    df_sorted=df_sorted[df_sorted$trim=='N', ]
    df_sorted$trim = NULL
  }
  
  df_sorted[df_sorted<print.cut & df_sorted>-print.cut] =''
  
  return(df_sorted) # return only the factor columns again
}

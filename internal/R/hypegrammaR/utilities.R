to_alphanumeric_lowercase <- function(x){tolower(gsub("[^a-zA-Z0-9]", "\\.", x))}

to_alphanumeric_lowercase_colnames_df <- function(df){
  names(df) <- to_alphanumeric_lowercase(names(df))
  return(df)
}


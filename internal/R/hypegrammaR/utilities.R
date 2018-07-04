to_alphanumeric_lowercase <- function(x){tolower(gsub("[^a-zA-Z0-9]", "\\.", x))}

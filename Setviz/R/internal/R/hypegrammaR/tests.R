# DONT DOCUMENT ME



# random_df<-function(n){
# 
# 
#   data.frame(
#     uniform = runif(n)*100,
#     normal = rnorm(n)*100,
#     exponential = rexp(n),
#     groupsN5  = sample(letters[1:5],n,T),
#     groupsN50 = sample(
#       expand.grid(LETTERS[1:10],letters[1:10]) %>% as.matrix %>% apply(1,function(x){paste0(x,collapse="")}),
#       n,T
#     )
# 
#   )
# 
# 
# }
# 
# 
# run_tests<-function(){
# data<-random_df(1000)
# return(
#   list(
# 
#     sanitise_group_difference(data,"uniform","groupsN5")$data %>% is.data.frame,
#     sanitise_group_difference(data,"uniform","groupsN50")$success== FALSE,
#   sanitise_group_difference(NULL,"a","b")$success==FALSE,
#   sanitise_group_difference(NA,"a","b")$success==FALSE,
#   sanitise_group_difference(runif(100),"a","b")$success==FALSE)) %>% unlist
# 
# 
# }
# 



folders<- c("./output/modified_data/",
            "./output/percent_aggregations_raw_csv",
            "./output/charts",
            "./output/log",
            "./output/composite_indicator_visualisation",
            "./output/tables",
            "./output/summary",
            "./output/viewer"
          )

# delete output folders (including content):
sapply(folders,unlink,recursive=T)
# (re)create output folders:
sapply(folders,dir.create,showWarnings=F)

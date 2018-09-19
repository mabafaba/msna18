# COMPOSITE INDICATORS:
logmessage(silver("making composite indicators.."))
composite_indicators_definitions_weighted_counts<-load_composite_indicator_definition_weighted_count()
if(nrow(composite_indicators_definitions_weighted_counts)>0){
  visualisation_composite_indicator_definition_graph(composite_indicators_definitions_weighted_counts)
  data<-add_variable_indicators_weighted_count(data,composite_indicators_definitions_weighted_counts)
  data %>% map_to_file("./output/modified_data/data_with_composite_indicators.csv")
  logmessage(green("data with composite indicators exported to ./output/modified_data/data_with_composite_indicators.csv"))
}else{
  .write_to_log("\nNo Composite Indicators Defined.\n")
}